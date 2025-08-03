#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <math.h>
#include <string.h>
#include <ctype.h>
#define CODESIZE 10000 //need to be not fixed
#define OUTPUTSIZE 15000 //need to be not fixed

int openfile(char * s, char * filename);

int preprocess(char * code, char * output, char * filename, int free);

int main(int argc, char ** argv)
{
	if (argc == 1) {perror("Need a file to preprocess, usage: ./mbf file.mbf output.bf [optional]\n"); return -1;}
	char code[CODESIZE] = {}, output[OUTPUTSIZE];
	if (openfile(code, argv[1])) return -2;
	if (preprocess(code, output, argv[1], 1)) {fprintf(stderr, "Incorrect macro syntax in %s\n", argv[1]); return -3;}		
	printf("code:\n%s\n", output);
	return 0;
}

int openfile(char * s, char * name)
{
	FILE * f = fopen(name, "r");
	if (!f) {fprintf(stderr, "Impossible to read the file %s\n", name); return -2;}
	fread(s, 1, CODESIZE - 1, f);
	fclose(f);
	return 0;
}

#define MAXNAME 100 //better if fixed
#define MAXMACROS 1000 //needs to be fixed

int hashstring(const char* s) {
    long hash = 0;
    const int len_s = strlen(s);
    for (int i = 0; i < len_s; i++) {
        hash += (long)pow(151, len_s - (i+1)) * s[i];
        hash = hash % MAXMACROS;
    }
    return (unsigned)hash;
}

struct macro {	
	char name[MAXNAME];
	char * bfi;
	unsigned * argsigns;
	unsigned narg;
};

struct listmacro {
	struct macro * macrolist;
	unsigned size; 
};


void freesubpointers(struct listmacro * p)
{
	unsigned i, j;
	for (i = 0; i < MAXMACROS; i++) {
		if (!p[i].macrolist)
			continue;
		if (p[i].size) printf("-----------------\n");
		for (j = 0; j < p[i].size; j++) {
			printf("listmacro[%d] and listmacro.macrolist[%d] name: %s\nnargs = %d\nbfi = %s\n", i, j, p[i].macrolist[j].name, p[i].macrolist[j].narg, 
			(p[i].macrolist[j].bfi) ? p[i].macrolist[j].bfi : "NULL");
			if (p[i].macrolist[j].bfi)
				free(p[i].macrolist[j].bfi);
			if (p[i].macrolist[j].argsigns)
				free(p[i].macrolist[j].argsigns);
		}
		free(p[i].macrolist);
	}
}

int findelement(struct listmacro list, char * name)
{
	unsigned i;
	for (i = 0; i < list.size; i++) {
		if (!strcmp(list.macrolist[i].name, name))
			return i + 1;	
	}
	return 0;
}

struct listmacro macros[MAXMACROS] = { };

struct pos {
	size_t line; 
	size_t collumn;
};

#define INITIALLIST 4
#define INITIALMBFI 100
#define INITIALARGSIGN 10

void * allocate(size_t size)
{
	void * ptr = malloc(size);
	if (!ptr) {fprintf(stderr, "Couldn't allocate\n"); freesubpointers(macros); exit(1);}
	return ptr;
}

void * reallocate(void * pr, size_t size)
{
	void * ptr = realloc(pr, size);
	if (!ptr) {fprintf(stderr, "Couldn't reallocate\n"); freesubpointers(macros); exit(1);}
	return ptr;
}

void initializemacrolist(struct macro * macrolist, int first, int last) //first and last included
{
	int i;
	for (i = first; i <= last; i++) {
		macrolist[i].bfi = NULL; 
		macrolist[i].argsigns = NULL;		
	}
}

int processmacros(char * code, char * output, char *name, unsigned *outputpos, unsigned *i, struct pos *currentpos);

int preprocess(char * s, char * output, char * name, int free)
{
	if (macros[0].macrolist == NULL)
		for (unsigned i = 0; i < MAXMACROS; i++) {
			macros[i].macrolist = NULL; //will have to malloc to create a list, in this list bfi should be null if there's no instruction, or malloc
			macros[i].size = 0;
		}
	unsigned i,  outputpos , outputwrite = 0;
	struct pos currentpos = {.line = 0, .collumn = 0};
	for (outputpos = i = 0; s[i]; i++, currentpos.collumn++)
		switch(s[i])
		{
		case '#':
			if (processmacros(s, output, name, &outputpos, &i, &currentpos))
				return -1;
			break;
		case '\n':
			currentpos.line++; currentpos.collumn = 0; 
			if (outputwrite) {
				output[outputpos++] = s[i]; outputwrite = 0;
			}
			break;
		default:
			output[outputpos++] = s[i]; outputwrite = 1;
			break;
		}
	if (free) freesubpointers(macros);
	output[outputpos] = '\0';
	return 0;	
}

#define KEYWORDCHECK(i, equalsign, comparison) (s[i] equalsign 'D' comparison s[i] \
equalsign 'U' comparison s[i] equalsign 'I' comparison s[i] equalsign '$' comparison s[i] equalsign '"' comparison s[i] equalsign '%')

#define ERROR(message, givenbackuppos, ret, additionalformat) {freesubpointers(macros);\
fprintf(stderr, message, additionalformat name, givenbackuppos.line + 1, givenbackuppos.collumn + 1);  return ret;} 

#define COMMA ,

int processmacros(char * s, char * output, char *name, unsigned *outputpos, unsigned *i, struct pos *currentpos)
{
	switch (s[(*i) + 1])
	{
	case 'D': case 'U': case 'I': case '$': case '"':  case '%':
		struct pos backuppos = (*currentpos);
		++(*i); currentpos->collumn++;
		unsigned oldi = (*i), j = 0, colon = 0, exclamationmark = 0, bfi = 0;
		char endc;
		char * argp = NULL, macroname[MAXNAME];
		int parenthese = 0;
		if (s[oldi] == '"') {
			endc = '"';
			if (s[oldi + 1] && s[oldi + 1] != '"')
				bfi = *i + 1;
		}
		else
			endc = '#';	
		if (s[oldi] == 'I' && s[oldi + 1] == 'N')
			(*i)++;
		while (s[++(*i)]) {
			currentpos->collumn++;
			if (s[(*i)] == endc) {
				if (endc == '"' && s[(*i) - 1] == '\\') 
					continue;
				if ((s[oldi] == 'D' || s[oldi] == 'I') && colon) {
					struct pos backuppos2 = (*currentpos);
					(*i)++;
					if (KEYWORDCHECK((*i), !=, &&)) {(*i)--; break;}
					(*i)++;
					int quotedstring = 0;
					if (s[(*i) - 1] == 'D' || s[(*i) - 1] == 'I') { //with : after
						int lastthing;
						int stack, colon2[MAXNAME] = { };
						for (lastthing = 1, stack = 1; s[(*i)] && stack; (*i)++, currentpos->collumn++) {
							if (s[(*i)] == '#') {
								if (KEYWORDCHECK((*i) + 1, ==, ||)) {
									if (lastthing && !colon2[stack - 1]) {colon2[--stack] = 0; continue;}
									else if (s[(*i) + 1] == '"' && !quotedstring) {
										quotedstring = 1; lastthing = 0; stack++; continue; //avoid fake lasthing
									}
									stack++;
									if (s[(*i) + 1] != 'I' && s[(*i) + 1] != 'D')
										lastthing = 0;
									else
										lastthing = 1;
								}
								else if (!quotedstring) {
									stack--;
									lastthing = 1;		
								}
							}
							else if (!colon2[stack - 1] && s[(*i)] == ':') colon2[stack - 1] = 1;
							else if (s[(*i)] == '\n' ) {currentpos->line++; currentpos->collumn = 0;}
							else if (s[(*i)] == '"'&& s[(*i) - 1] != '\\' && quotedstring) {
								quotedstring = 0; lastthing = 1; colon2[--stack] = 0;
							}
						}
						if (!stack) {(*i)--; currentpos->collumn--;}
					}
					else if (s[(*i) - 1] == 'U' || s[(*i) - 1] == '$' || s[(*i) - 1] == '%') //with nothing after
						for (; s[(*i)]; (*i)++, currentpos->collumn++) {
							if (s[(*i)] == '#') break;
							else if (s[(*i)] == '\n' ) {currentpos->line++; currentpos->collumn = 0;}
						}
					else if (s[(*i) - 1] == '"') { //different stuff
						quotedstring = 1;
						for (; s[(*i)]; (*i)++, currentpos->collumn++) {
							if (s[(*i)] == '"' && s[(*i) - 1] != '\\')
								break;
							else if (s[(*i)] == '\n' ) {currentpos->line++; currentpos->collumn = 0;}
						}
					}
					if (!s[(*i)]) ERROR("Not closed %c in %s line : %ld, collumn : %ld\n", backuppos2, -1, (quotedstring) ? '"' : '#' COMMA)
				}
				else
					break;
			}
			else if (s[(*i)] == '\n' ) {currentpos->line++; currentpos->collumn = 0;}
			else if (s[(*i)] == ':' && !colon) {
				colon = 1; 
				bfi = *i + 1;
			}
			else if (s[(*i)] == '!' && !exclamationmark) {
				exclamationmark = 1;
				continue; //avoid ! getting in macroname
			}
			if ((s[oldi] == 'D' || s[oldi] == 'I') && colon)
					continue;
			if (s[(*i)] == ')' && parenthese) {
				parenthese = 0; continue;
			}
			else if (s[oldi] == 'D' || s[oldi] == '$') {
				if (s[(*i)] == '(' && !parenthese) {
					argp = &s[(*i) + 1]; parenthese = 1;
				}
			}
			if(s[oldi] != '"' && !isspace(s[(*i)]) && !parenthese)
				macroname[j++] = s[(*i)];
		}
		if (!s[(*i)]) ERROR("Not closed %c in %s line : %ld, collumn : %ld\n", backuppos, -1, endc COMMA) 
		macroname[j] = '\0'; //j will now serve as a loop i
		unsigned hashedstring = hashstring(macroname), 
		foundelement = findelement(macros[hashedstring], macroname),
		pos = (foundelement) ? foundelement - 1 : macros[hashedstring].size;
		char passedoutput[CODESIZE];
		int retval;
		unsigned l, k, passedoutputpos = 0, encounteredchar = 0;;
		if (s[oldi] == 'D') {
			if (!foundelement) {
				if (!macros[hashedstring].macrolist) {
					macros[hashedstring].macrolist = allocate(sizeof(struct macro) * INITIALLIST); 
					initializemacrolist(macros[hashedstring].macrolist, 0, INITIALLIST - 1);
				}
				else if (pos > 0 && !(pos % INITIALLIST)) {
					macros[hashedstring].macrolist = reallocate(macros[hashedstring].macrolist, 
						sizeof(struct macro) * INITIALLIST * (pos / INITIALLIST + 1)); 
					initializemacrolist(macros[hashedstring].macrolist, INITIALLIST * (pos / INITIALLIST) 
						, INITIALLIST * (pos / INITIALLIST + 1) - 1);
				}
				strcpy(macros[hashedstring].macrolist[pos].name, macroname); 
				macros[hashedstring].size++;
			}
			if (bfi) {
				if (!macros[hashedstring].macrolist[pos].bfi)
					macros[hashedstring].macrolist[pos].bfi = allocate(INITIALMBFI);

				 //replace strings macros , $ and % macros in bfi
				for (j = bfi, l = 0; j < *i; j++, l++) {
					if (s[j] == '#' && (s[j + 1] == '"' || s[j + 1] == '$' || s[j + 1] == '%')) {
						retval =  processmacros(s, passedoutput, name, &passedoutputpos, &j, currentpos);
						if (retval) return retval;
						for (k = 0; k < passedoutputpos; k++, l++) {
							if ((l + 1) % INITIALMBFI) //for null char
								macros[hashedstring].macrolist[pos].bfi = reallocate(macros[hashedstring].macrolist[pos].bfi, 
								INITIALMBFI * ((l / INITIALMBFI) + 1));
							macros[hashedstring].macrolist[pos].bfi[l] = passedoutput[k];
						}
						if (k) l--;
						passedoutputpos = 0;
					}
					else {
						if (!encounteredchar && isspace(s[j])) {l--; continue;}
						if (s[j] == '\n') encounteredchar = 0;
						else encounteredchar = 1;
						if ((l + 1) % INITIALMBFI) //for null char
							macros[hashedstring].macrolist[pos].bfi = reallocate(macros[hashedstring].macrolist[pos].bfi, 
							INITIALMBFI * ((l / INITIALMBFI) + 1));
						macros[hashedstring].macrolist[pos].bfi[l] = s[j];
					}		
				}
				macros[hashedstring].macrolist[pos].bfi[l] = '\0';
			}
			else if (macros[hashedstring].macrolist[pos].bfi) {//if defined without ":" after
				free(macros[hashedstring].macrolist[pos].bfi); macros[hashedstring].macrolist[pos].bfi = NULL;
			}
			macros[hashedstring].macrolist[pos].narg = 0;
			if (argp) {
				int sign = 0;
				if (!macros[hashedstring].macrolist[pos].argsigns)
					macros[hashedstring].macrolist[pos].argsigns = allocate(sizeof(int) * INITIALARGSIGN);
				for (; *argp; argp++) {
					if (*argp == ',' || *argp == ')') {
						if (macros[hashedstring].macrolist[pos].narg > 0 && macros[hashedstring].macrolist[pos].narg  % INITIALARGSIGN == 0) 
						macros[hashedstring].macrolist[pos].argsigns = reallocate(macros[hashedstring].macrolist[pos].argsigns, 
						sizeof(int) * INITIALARGSIGN * (macros[hashedstring].macrolist[pos].narg  / INITIALARGSIGN + 1));

						macros[hashedstring].macrolist[pos].argsigns[ macros[hashedstring].macrolist[pos].narg ] = sign;
						sign = 0;
						macros[hashedstring].macrolist[pos].narg++;
						if (*argp == ')') break;
					}
					else if (*argp == '-')
						sign = 1;	
				}
			}
		}
		else if (s[oldi] == 'U') {
			if (!foundelement)  ERROR("Undefining an undefined macro \"%s\" in %s in line : %ld, collumn : %ld\n", backuppos, -1, macroname COMMA)
			if (macros[hashedstring].macrolist[pos].bfi) free(macros[hashedstring].macrolist[pos].bfi);
			if (macros[hashedstring].macrolist[pos].argsigns) free(macros[hashedstring].macrolist[pos].argsigns);
			for (j = pos; j < macros[hashedstring].size; j++)
				macros[hashedstring].macrolist[j] = macros[hashedstring].macrolist[j + 1];
			macros[hashedstring].size--;
		}
		else if (s[oldi] == '$') { 
			if (!foundelement) ERROR("Using an undefined macro \"%s\" in %s in line : %ld, collumn : %ld\n", backuppos, -1, macroname COMMA)
			if (!macros[hashedstring].macrolist[pos].bfi) 
				break;
			if (argp) {
				if (!macros[hashedstring].macrolist[pos].narg)
					ERROR("Adding arguments to a macro: \"%s\" who doesn't have argument in %s in line : %ld, collumn : %ld\n", backuppos, -1, macroname COMMA)
				else if (exclamationmark)
					ERROR("Adding arguments to a macro: \"%s\" who has '!' (see docs for further infos in %s in line) : %ld, collumn : %ld\n", backuppos, -1, macroname COMMA)
				unsigned currentarg, negative;
				int cellpos;
				for (cellpos = 0, currentarg = 0; *argp; argp++) { //condition is optional, but just in case
					if (*argp == ',' || *argp == ')') {
						if (cellpos <= (int) macros[hashedstring].macrolist[pos].narg && cellpos >= 0)	
							ERROR("The argument cannot be the copycell or args cell (for further infos check docs implementation section) :  \"%s\" in %s in line : %ld, collumn : %ld\n", backuppos, -1, macroname COMMA)
						if (cellpos < 0) {cellpos = -cellpos; negative = 1;}
						else negative = 0;
						for (k = 0; k < (unsigned) cellpos; k++)
							if (negative)
								output[(*outputpos)++] = '<';
							else
								output[(*outputpos)++] = '>';
						output[(*outputpos)++] = '[';
						if (macros[hashedstring].macrolist[pos].argsigns[currentarg])
							output[(*outputpos)++] = '+';
						else output[(*outputpos)++] = '-';
						for (k = 0; k < (unsigned) cellpos; k++)
							if (negative)
								output[(*outputpos)++] = '>';
							else
								output[(*outputpos)++] = '<';
						if (macros[hashedstring].macrolist[pos].argsigns[currentarg])
							output[(*outputpos)++] = '-';
						else output[(*outputpos)++] = '+';
						for (l = 0; l < currentarg + 1; l++)
							output[(*outputpos)++] = '>';
						if (macros[hashedstring].macrolist[pos].argsigns[currentarg])
							output[(*outputpos)++] = '-';
						else output[(*outputpos)++] = '+';
						for (l = 0; l < currentarg + 1; l++)
							output[(*outputpos)++] = '<';
						for (k = 0; k < (unsigned) cellpos; k++)
							if (negative)
								output[(*outputpos)++] = '<';
							else
								output[(*outputpos)++] = '>';
						output[(*outputpos)++] = ']';
						for (k = 0; k < (unsigned) cellpos; k++)
							if (negative)
								output[(*outputpos)++] = '>';
							else
								output[(*outputpos)++] = '<';
						output[(*outputpos)++] = '[';
						if (macros[hashedstring].macrolist[pos].argsigns[currentarg])
							output[(*outputpos)++] = '+';
						else output[(*outputpos)++] = '-';
						for (k = 0; k < (unsigned) cellpos; k++)
							if (negative)
								output[(*outputpos)++] = '<';
							else
								output[(*outputpos)++] = '>';
						if (macros[hashedstring].macrolist[pos].argsigns[currentarg])
							output[(*outputpos)++] = '-';
						else output[(*outputpos)++] = '+';
						for (k = 0; k < (unsigned) cellpos; k++)
							if (negative)
								output[(*outputpos)++] = '>';
							else
								output[(*outputpos)++] = '<';
						output[(*outputpos)++] = ']';
						currentarg++;
						cellpos = 0;
						if (*argp == ')')
							break;
					}
					else if (*argp == '<')
						cellpos--;
					else if (*argp == '>')
						cellpos++;	
				}
				if (currentarg != macros[hashedstring].macrolist[pos].narg)
					ERROR("%d arguments but : \"%s\" takes %d in %s in line : %ld, collumn : %ld\n", backuppos, -1, currentarg COMMA macroname 
					COMMA macros[hashedstring].macrolist[pos].narg COMMA)
			}
			else if (macros[hashedstring].macrolist[pos].narg && !exclamationmark)
				ERROR("Putting no arguments to a macro: \"%s\" who has argument(s), '!' to avoid this error note that you will have to know about macro-bf first,\n"
			"in %s in line : %ld, collumn : %ld\n", backuppos, -1, macroname COMMA)
			//get the content of the macro and put it in the file, macro execute the U,I,IN
			for (bfi = 0; macros[hashedstring].macrolist[pos].bfi[bfi]; bfi++) {
				if (macros[hashedstring].macrolist[pos].bfi[bfi] == '#' && 
				(macros[hashedstring].macrolist[pos].bfi[bfi + 1] == 'U' || macros[hashedstring].macrolist[pos].bfi[bfi + 1] == 'I' || 
				macros[hashedstring].macrolist[pos].bfi[bfi + 1] == 'D')) {
					retval = processmacros(macros[hashedstring].macrolist[pos].bfi, passedoutput, name, &passedoutputpos, &bfi, currentpos);
					if (retval) return retval;
					for (k = 0; k < passedoutputpos; k++)
						output[(*outputpos)++] = passedoutput[k];
					passedoutputpos = 0;
				}
				else
					output[(*outputpos)++] = macros[hashedstring].macrolist[pos].bfi[bfi];		
			}
		}
		else if (s[oldi] == '%') {
			char file[CODESIZE], macroreplaced[OUTPUTSIZE] = {};
			if (openfile(file, macroname)) ERROR("Impossible to read the file: \"%s\" in %s in line : %ld, collumn : %ld\n", backuppos, -2, macroname COMMA)
			if (preprocess(file, macroreplaced, macroname, 0)) return -1;
			for (j = 0; macroreplaced[j]; j++)
				output[(*outputpos)++] = macroreplaced[j];
		}
		else if (s[oldi] == '"') {
			char temp, inttos[4];
			int xctoi(char c), isodigit(char c), litescapeseq = -1;
			for (; s[bfi]!='"' || s[bfi - 1] == '\\'; bfi++) {
				if (s[bfi] == '\t' || s[bfi] == '\n') continue;
				if (s[bfi] == '\\') 
					switch (s[bfi + 1])
					{
					case 'a': s[++bfi] = '\a'; break;
					case 'b': s[++bfi] = '\b'; break;
					case 'e': s[++bfi] = '\e'; break;
					case 'f': s[++bfi] = '\f'; break;
					case 'n': s[++bfi] = '\n'; break;
					case 'r': s[++bfi] = '\r'; break;
					case 't': s[++bfi] = '\t'; break;
					case 'v': s[++bfi] = '\v'; break;
					case '\\': s[++bfi] = '\\'; break;
					case '\'': s[++bfi] = '\''; break;
					case '"': bfi++; break;
					case 'x': case 'X':
						if (!isxdigit(s[bfi + 2])) ERROR("No xdigit after \\x in %s in line : %ld, collumn : %ld\n", backuppos, -1, )
						litescapeseq = xctoi(s[bfi + 2]);
						bfi++;
						if (isxdigit(s[bfi + 2])) {
							litescapeseq *= 16;
							litescapeseq += xctoi(s[bfi + 2]);
							bfi++;
						}
						bfi++;
						break;
					default:
						if (isodigit(s[bfi + 1])) { //if it's octal
							litescapeseq = s[bfi + 1] - '0';
							bfi++;
							if (isodigit(s[bfi + 1])) {
								litescapeseq *= 8;
								litescapeseq += s[bfi + 1] - '0';
								bfi++;
								if (isodigit(s[bfi + 1])) {
									litescapeseq *= 8;
									litescapeseq += s[bfi + 1] - '0';
									bfi++;
								}
							}
							printf("%d and %d\n", litescapeseq, bfi);
						}
						else
							ERROR("Nothing valid after \\ in %s in line : %ld, collumn : %ld\n", backuppos, -1, )
						break;
					}
				for (temp = (litescapeseq == - 1) ? s[bfi] : litescapeseq; temp; temp--)
					output[(*outputpos)++] = '+';
				output[(*outputpos)++] = ' ';
				output[(*outputpos)++] = 'c';
				output[(*outputpos)++] = '=';
				sprintf(inttos, "%d", (litescapeseq == - 1) ? s[bfi] : litescapeseq);
				for (j = 0; inttos[j]; j++)
					output[(*outputpos)++] = inttos[j];
				output[(*outputpos)++] = '\n';	
				output[(*outputpos)++] = '>';
				litescapeseq = -1;
			}
		}
		else if(s[oldi] == 'I') {
			if (!bfi) break;
			if (s[oldi + 1] != 'N') {
				if (!foundelement) break;
			}
			else if (foundelement) break;
			for (j = bfi; j < *i; j++) {
				if (s[j] == '#' && KEYWORDCHECK(j + 1, ==, ||)) {
					retval =  processmacros(s, passedoutput, name, &passedoutputpos, &j, currentpos);
					if (retval) return retval;
					for (k = 0; k < passedoutputpos; k++, l++) {
						output[(*outputpos)++] = passedoutput[k];
					}
					if (k) l--;
					passedoutputpos = 0;
				}
				else {
					if (!encounteredchar && isspace(s[j])) {l--; continue;}
					if (s[j] == '\n') encounteredchar = 0;
					else encounteredchar = 1;
					output[(*outputpos)++] = s[j];
				}
			}
		}
		break;
	default:
		output[(*outputpos)++] = s[(*i)];
		break;
	}
	return 0;
}

int xctoi(char c)
{
	if (isdigit(c))
		c -= 48;
	else if (isxdigit(c))
		c = 10 + c - ((isalpha(c)) ? 'A' : 'a');
	return (int) c;
}

int isodigit(char c)
{
	if (c >= '0' && c <= '7')
		return 1;
	else
		return 0;
}
