#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <math.h>
#include <string.h>
#include <ctype.h>
#define CODESIZE 10000 //need to be not fixed
#define OUTPUTSIZE 15000 //need to be not fixed

void openfile(char * s, char * filename);

int preprocess(char * code, char * output, char * filename);

int main(int argc, char ** argv)
{
	if (argc == 1) {perror("Need a file to preprocess, usage: ./mbf file.mbf output.bf [optional]\n"); return -1;}
	char code[CODESIZE] = {}, output[OUTPUTSIZE];
	openfile(code, argv[1]);
	if (preprocess(code, output, argv[1])) {fprintf(stderr, "Incorrect macro syntax in %s\n", argv[1]); return -3;}		
	printf("code:\n%s\n", output);
	return 0;
}

void openfile(char * s, char * name)
{
	FILE * f = fopen(name, "r");
	if (!f) {fprintf(stderr, "Impossible to read the file %s\n", name); exit(-2);}
	fread(s, 1, CODESIZE - 1, f);
	fclose(f);
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
			putchar(p[i].macrolist[j].bfi[1]);
			printf("listmacro[%d] and listmacro.macrolist[%d] name: %s\nbfi = %s\n", i, j, p[i].macrolist[j].name, 
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

int preprocess(char * s, char * output, char * name)
{
	if (macros[0].macrolist == NULL)
		for (unsigned i = 0; i < MAXMACROS; i++) {
			macros[i].macrolist = NULL; //will have to malloc to create a list, in this list bfi should be null if there's no instruction, or malloc
			macros[i].size = 0;
		}
	unsigned i,  outputpos ,outputwrite = 0;
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
				output[outputpos++] = s[i]; outputwrite = 1;
			}
			break;
		default:
			output[outputpos++] = s[i]; outputwrite = 1;
			break;
		}
	freesubpointers(macros);
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
		unsigned oldi = (*i), j = 0, colon = 0;
		char endc;
		char * argp = NULL, macroname[MAXNAME];
		int parenthese = 0, bfi = -1;
		if (s[(*i)] == '"') {
			endc = '"';
			if (s[(*i) + 1] && s[(*i) + 1] != '"')
				bfi = *i + 1;
		}
		else
			endc = '#';	
		while (s[++(*i)]) {
			currentpos->collumn++;	
			if (s[(*i)] == endc) {
				if (endc == '"' && s[(*i) - 1] == '\\') 
					continue;
				else if ((s[oldi] == 'D' || s[oldi] == 'I') && colon) {
					struct pos backuppos2 = (*currentpos);
					(*i)++;
					if (KEYWORDCHECK((*i), !=, &&)) {(*i)--; break;}
					(*i)++;
					int quotedstring = 0;
					if (s[(*i) - 1] == 'D' || s[(*i) - 1] == 'I') { //with : after
						char lastthing;
						int stack, colon2;
						for (lastthing = s[(*i) - 1], stack = 1, colon2 = 0; s[(*i)] && stack; (*i)++, currentpos->collumn++) {
							if (s[(*i)] == '#') {
								if (KEYWORDCHECK((*i) + 1, ==, ||)) {
									if ((lastthing == 'D' || lastthing == 'I') && !colon2) {stack--; continue;}
									else if (lastthing == 'D' || lastthing == 'I') colon2 = 0;
									if (s[*i + 1] == '"')
										quotedstring = 1;
									else
										quotedstring = 0;
									stack++;
									lastthing = s[(*i) + 1];
								}
								else
									stack--;
							}
							else if (!colon2 && s[(*i)] == ':') colon2 = 1;
							else if (s[(*i)] == '\n' ) {currentpos->line++; currentpos->collumn = 0;}
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
			if ((s[oldi] == 'D' || s[oldi] == 'I') && colon)
					continue;
			if (s[(*i)] == ')' && parenthese)
				parenthese = 0;
			else if (s[oldi] == 'D' || s[oldi] == '$') {
				if (s[(*i)] == '(' && !parenthese) {
					argp = &s[(*i) + 1]; parenthese = 1;
				}
			}
			if(s[oldi] != '"')
				if (!isspace(s[(*i)]))
					macroname[j++] = s[(*i)];
		}
		if (!s[(*i)]) ERROR("Not closed %c in %s line : %ld, collumn : %ld\n", backuppos, -1, endc COMMA) 
		else if (parenthese) ERROR("Not closed parentheses in %s line : %ld, collumn : %ld\n", backuppos, -1, ) 
		macroname[j] = '\0'; //j will now serve as a loop i
		unsigned hashedstring = hashstring(macroname), 
		foundelement = findelement(macros[hashedstring], macroname),
		pos = (foundelement) ? foundelement - 1 : macros[hashedstring].size;
		char passedoutput[CODESIZE];
		int retval;
		unsigned k, passedoutputpos = 0;
		if (s[oldi] == 'D') {
			if (!foundelement) {
				if (!macros[hashedstring].macrolist) {
					macros[hashedstring].macrolist = allocate(sizeof(struct macro) * INITIALLIST); 
					initializemacrolist(macros[hashedstring].macrolist, 0, INITIALLIST - 1);
				}
				else if (pos - 1 > 0 && (pos - 1) % INITIALLIST) {
					macros[hashedstring].macrolist = reallocate(macros[hashedstring].macrolist, 
						sizeof(struct macro) * INITIALLIST * ((pos - 1) / 4)); 
					initializemacrolist(macros[hashedstring].macrolist, INITIALLIST * ((pos - 1) / 4) - INITIALLIST 
						, INITIALLIST * ((pos - 1) / 4) - 1);
				}
				strcpy(macros[hashedstring].macrolist[pos].name, macroname); 
				macros[hashedstring].size++;
			}
			if (bfi != -1) {
				if (!macros[hashedstring].macrolist[pos].bfi)
					macros[hashedstring].macrolist[pos].bfi = allocate(INITIALMBFI);

				 //replace strings macros , $ and % macros in bfi
				unsigned l, encounteredchar = 0;
				for (j = bfi, l = 0; j < *i; j++, l++) {
					if (!encounteredchar && isspace(s[j])) {l--; continue;}
					if (s[j] == '\n') encounteredchar = 0;
					else encounteredchar = 1;
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
				for (; *argp != ')'; argp++)
					if (*argp == ',')
						macros[hashedstring].macrolist[pos].narg++;
				macros[hashedstring].macrolist[pos].narg++;						
			}
		}
		else if (s[oldi] == 'U') {
			if (!foundelement)  ERROR("Undefining an undefined macro \"%s\" in %s in line : %ld, collumn : %ld\n", backuppos, -1, macroname COMMA)
			if (pos != macros[hashedstring].size) {
				free(macros[hashedstring].macrolist[pos].bfi);
				for (j = pos; j < macros[hashedstring].size; j++)
					macros[hashedstring].macrolist[j] = macros[hashedstring].macrolist[j + 1];
			}
			else
				free(macros[hashedstring].macrolist[pos - 1].bfi);
			macros[hashedstring].size--;
		}
		else if (s[oldi] == '$') { 
			if (!foundelement) ERROR("Using an undefined macro \"%s\" in %s in line : %ld, collumn : %ld\n", backuppos, -1, macroname COMMA)
			if (!macros[hashedstring].macrolist[pos].bfi) 
				break;
			if (argp) {
				if (!macros[hashedstring].macrolist[pos].narg)
					ERROR("Adding arguments to a macro: \"%s\" who doesn't have argument in %s in line : %ld, collumn : %ld\n", backuppos, -1, macroname COMMA)
				unsigned currentarg, negative;
				int cellpos;
				for (cellpos = 0, currentarg = 1; *argp != ')'; argp++) {
					if (*argp == ',') {
						if (cellpos < 0) {cellpos = -cellpos; negative = 1;}
						else negative = 0;
						for (k = 0; k < (unsigned) cellpos; k++)
							if (negative)
								output[(*outputpos)++] = '<';
							else
								output[(*outputpos)++] = '>';
						currentarg++;
					}
					else if (*argp == '<')
						cellpos--;
					else if (*argp == '>')
						cellpos++;	
				}
			}
			else if (macros[hashedstring].macrolist[pos].narg)
				ERROR("Putting no arguments to a macro: \"%s\" who has argument(s) in %s in line : %ld, collumn : %ld\n", backuppos, -1, macroname COMMA)
			//get the content of the macro and put it in the file, macro execute the U,I,IN
			for (bfi = 0; macros[hashedstring].macrolist[pos].bfi[bfi]; bfi++) {
				if (s[bfi] == '#' && (s[bfi + 1] == 'U' || s[bfi + 1] == 'I')) {
					retval = processmacros(s, passedoutput, name, &passedoutputpos, &j, currentpos);
					if (retval) return retval;
					for (k = 0; k < passedoutputpos; k++, (*outputpos)++)
						output[*outputpos] = passedoutput[k];
					if (k) (*outputpos)--;
					passedoutputpos = 0;
				}
				else
					output[(*outputpos)++] = macros[hashedstring].macrolist[pos].bfi[bfi];		
			}
		}
		else if (s[oldi] == '%') {
			char file[CODESIZE], macroreplaced[OUTPUTSIZE];
			printf("opened file %s\n", macroname);
		}
		else if (s[oldi] == '"') {
			char temp;
			for (; s[bfi]!='"' || s[bfi - 1] == '\\'; bfi++) {
				for (temp = s[bfi]; temp; temp--)
					output[(*outputpos)++] = '+';
				output[(*outputpos)++] = ' ';
				output[(*outputpos)++] = s[bfi];
				output[(*outputpos)++] = '\n';	
				output[(*outputpos)++] = '>';
			}
		}
		break;
	default:
		output[(*outputpos)++] = s[(*i)];
		break;
	}
	return 0;
}
