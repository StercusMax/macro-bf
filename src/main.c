#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>

int openfile(char ** s, char * filename, size_t * size);

int preprocess(char * code, char ** output, char * filename, int freeb, size_t size);

int main(int argc, char ** argv)
{
	if (argc == 1) {fprintf(stderr, "Need a file to preprocess, usage: ./mbf file.mbf output.bf [optional]\n"); return -1;}
	else if (argc > 3) {fprintf(stderr, "Too many arguments, usage: ./mbf file.mbf output.bf [optional]\n"); return -1;}
	char * code, * output;
	size_t initialsize;
	int retvalue = openfile(&code, argv[1], &initialsize);
	if (retvalue) return retvalue;
	output = malloc(initialsize + 1);
	if (!output) {
		free(code);
		fprintf(stderr, "Impossible to allocate memory in %s\n", argv[1]); 
		return -3;
	}
	memset(output, '\0', initialsize + 1);
	retvalue = preprocess(code, &output, argv[1], 1, initialsize + 1); 
	if (retvalue) {fprintf(stderr, "Incorrect macro syntax in %s\n", argv[1]); return retvalue;}
	char * name = (argc == 2) ? "output.bf" : argv[3];	
	FILE * f = fopen(name, "w");
	if (!f) {free(code); free(output); fprintf(stderr, "impossible to write in the file %s\n", name); return -2;}
	fprintf(f, "%s", output);
	fclose(f); free(code); free(output);
	return 0;
}

int openfile(char ** s, char * name, size_t * size)
{
	FILE * f = fopen(name, "r");
	if (!f) {fprintf(stderr, "Impossible to read the file %s\n", name); return -2;}
	fseek(f, 0, SEEK_END);
	*size = ftell (f);
	fseek(f, 0, SEEK_SET);
	*s = malloc(*size + 1);
	if (!(*s)) {
		fprintf(stderr, "Impossible to allocate memory in %s\n", name); 
		return -3;
	}
	fread((*s), 1, *size, f);
	(*s)[*size] = '\0';
	fclose(f);
	return 0;
}

#define MAXNAME 100
#define MAXMACROS 1000
#define MAXCIRC 150
#define MAXOCCURENCES 10

int hashstring(const char* s, unsigned length) {
    long hash = 0;
    const int len_s = strlen(s);
    for (int i = 0; i < len_s; i++) {
        hash += (long)pow(151, len_s - (i+1)) * s[i];
        hash = hash % length;
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

struct circdependencies {
	char name[MAXNAME]; //takes name
	struct circdependencies * node; //so we can store multiple one, the hash doesn't have an unique one
	unsigned occurence; //n occurences
	int type; //0 macro, 1 file
};

struct listmacro macros[MAXMACROS];
struct circdependencies macroscirc[MAXCIRC];


void freecirc()
{
	unsigned i;
	struct circdependencies * temp, * temp2;
	for (i = 0; i < MAXCIRC; i++) {
		if (macroscirc[i].type != -1) {
			macroscirc[i].occurence = 0;
			macroscirc[i].type = -1; //we use this to find out if it's used or not
		}
		for (temp = macroscirc[i].node; temp; temp = temp2) {
			temp2 = temp->node;
			free(temp);
		}
		macroscirc[i].node = NULL;
	}
}

void freesubpointers() //will have to free the macroscirc
{
	unsigned i, j;
	for (i = 0; i < MAXMACROS; i++) {
		if (!macros[i].macrolist)
			continue;
		//if (p[i].size) printf("-----------------\n");
		for (j = 0; j < macros[i].size; j++) {
			/*printf("listmacro[%d] and listmacro.macrolist[%d] name: %s\nnargs = %d\nbfi = %s\n", i, j, p[i].macrolist[j].name, p[i].macrolist[j].narg, 
			(p[i].macrolist[j].bfi) ? p[i].macrolist[j].bfi : "NULL");*/
			if (macros[i].macrolist[j].bfi)
				free(macros[i].macrolist[j].bfi);
			if (macros[i].macrolist[j].argsigns)
				free(macros[i].macrolist[j].argsigns);
		}
		free(macros[i].macrolist);
	}
	freecirc();
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

struct circdependencies * findcirc(struct circdependencies * list, char * name, int type)
{
	if (list->type == -1 || (!strcmp(name, list->name) && list->type == type)) return list; //if it's before the ones we want to allocate
	while (list->node) {
		list = list->node;	
		if (!strcmp(list->name, name) && list->type == type)
			return list;
	}
	return list;
}


struct pos {
	size_t line; 
	size_t collumn;
};

#define INITIALLIST 4
#define INITIALMBFI 100
#define INITIALARGSIGN 10

void * allocate(size_t size) //gotta free code as well
{
	void * ptr = malloc(size);
	if (!ptr) {fprintf(stderr, "Couldn't allocate\n"); freesubpointers();}
	return ptr;
}

void * reallocate(void * pr, size_t size)
{
	void * ptr = realloc(pr, size);
	if (!ptr) {fprintf(stderr, "Couldn't reallocate\n"); freesubpointers();}
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

int processmacros(char * code, char ** output, char *name, unsigned *outputpos, unsigned *i, struct pos *currentpos, size_t size, int freeb);

int outputequals(char ** output, unsigned *outputpos, char c, size_t size)
{
	if (*outputpos > 0 && !((*outputpos + 2) % size)) {
		*output = reallocate(*output, size * ((*outputpos + 2) / size + 1));
        if (!*output) {freesubpointers(); return -2;}
        memset(*output + *outputpos + 1, '\0', size);	
    }
    (*output)[(*outputpos)++] = c;
    return 0; 
}

int preprocess(char * s, char ** output, char * name, int freeb, size_t size)
{
	unsigned i;
	struct circdependencies {
		char name[MAXNAME]; //takes name
		struct macro * circdependencies; //so we can store multiple one, the hash doesn't have an unique one
		unsigned occurence; //n occurences
		int type; //0 macro, 1 file
	};
	if (freeb) {
		for (i = 0; i < MAXMACROS; i++) {
			macros[i].macrolist = NULL; //will have to malloc to create a list, in this list bfi should be null if there's no instruction, or malloc
			macros[i].size = 0;
		}
		for (i = 0; i < MAXCIRC; i++) {
			macroscirc[i].node = NULL;
			macroscirc[i].occurence = 0;
			macroscirc[i].type = -1; //we use this to find out if it's used or not
		}	
	}
	unsigned outputpos , outputwrite = 0;
	struct pos currentpos = {.line = 0, .collumn = 0};
	int retvalue;
	for (outputpos = i = 0; s[i]; i++, currentpos.collumn++)
		switch(s[i])
		{
		case '#':
			retvalue = processmacros(s, output, name, &outputpos, &i, &currentpos, size, freeb);
			if (retvalue) {
				free(s); free(*output); //we free here so upstream doesn't care
				return retvalue;
			}
			break;
		case ';':
			while (s[i] != '\n' && s[i])
				i++;
			i--; //so it process \n
			break;
		case '\n':
			currentpos.line++; currentpos.collumn = 0; 
			if (outputwrite) {
                retvalue = outputequals(output, &outputpos, s[i], size);
				if (retvalue) return retvalue;
                outputwrite = 0;
			}
			break;
		default:
            retvalue = outputequals(output, &outputpos, s[i], size); 
			if (retvalue) return retvalue; 
            outputwrite = 1;
			break;
		}
	if (freeb) freesubpointers();
	return 0;	
}

#define KEYWORDCHECK(i, equalsign, comparison) (i equalsign 'D' comparison i \
equalsign 'U' comparison i equalsign 'I' comparison i equalsign '$' comparison i equalsign '"' comparison i equalsign '%')

#define ERROR(message, givenbackuppos, ret, additionalformat) {freesubpointers();\
fprintf(stderr, message, additionalformat name, givenbackuppos.line + 1, givenbackuppos.collumn + 1);  return ret;} 

#define COMMA ,

#define INITOUTPUTGIVENSIZE 100

#define mallocate(ptr, size) ptr = allocate(size); if (!ptr) return -2
#define mallocatefree(ptr, size, ptrtofree) ptr = allocate(size); if (!ptr) {free(ptrtofree); return -2;}

#define mreallocate(ptr, size) ptr = reallocate(ptr, size); if (!ptr) return -2
#define mreallocatefree(ptr, ptrtofree, size) ptr = reallocate(ptr, size); if (!ptr) {free(ptrtofree); return -2;}

#define writetooutput(c) retval = outputequals(output, outputpos, c, size); if(retval) return retval
#define writetooutputfree(c, ptrtofree) retval = outputequals(output, outputpos, c, size); if(retval) {free(ptrtofree); return retval;}

#define GOTOARG(NEGATIVETO, POSITIVETO) \
for (k = 0; k < (unsigned) cellpos; k++) \
	if (negative) { \
        writetooutput(NEGATIVETO); \
    } \
    else { \
		writetooutput(POSITIVETO); \
    }
#define LOOPACT(POSITIVEACT,NEGATIVEACT) \
if (macros[hashedstring].macrolist[pos].argsigns[currentarg]) { \
	if (!changesign) {writetooutput(POSITIVEACT);} else {writetooutput(NEGATIVEACT);} \
} \
else {if (!changesign) {writetooutput(NEGATIVEACT);} else {writetooutput(POSITIVEACT);}}

#define CHECKCIRC(giventype) \
unsigned hashedstringcirc = hashstring(macroname, MAXCIRC); \
struct circdependencies * foundcirc = findcirc(&macroscirc[hashedstringcirc], macroname, giventype); \
if (macroscirc[hashedstringcirc].type != -1 && strcmp(macroname, foundcirc->name)) { \
	mallocatefree(foundcirc->node, sizeof(struct circdependencies), passedoutput); \
	strcpy(foundcirc->node->name, macroname); \
	foundcirc->node->node = NULL; \
	foundcirc->node->occurence = 0; \
	foundcirc->node->type = giventype; \
} \
else { \
	strcpy(foundcirc->name, macroname); \
	foundcirc->type = giventype; \
} \
foundcirc->occurence++;\
if (foundcirc->occurence == MAXOCCURENCES) {free(passedoutput); ERROR("Max depth is %d, circular dependencies suspected (to increase it you need to edit the source macro maxoccurences) in %s in line : %ld, collumn : %ld\n", backuppos, -20, MAXOCCURENCES COMMA)}

int processmacros(char * s, char ** output, char *name, unsigned *outputpos, unsigned *i, struct pos *currentpos, size_t size, int freeb)
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
					if (KEYWORDCHECK(s[(*i)], !=, &&)) {(*i)--; break;}
					(*i)++;
					int quotedstring = 0;
					if (s[(*i) - 1] == 'D' || s[(*i) - 1] == 'I') { //with : after
						int lastthing;
						int stack, colon2[MAXNAME] = { };
						for (lastthing = 1, stack = 1; s[(*i)] && stack; (*i)++, currentpos->collumn++) {
							if (s[(*i)] == '#') {
								if (KEYWORDCHECK(s[(*i) + 1], ==, ||)) {
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
					if (!s[(*i)]) ERROR("Not closed %c in %s line : %ld, collumn : %ld\n", backuppos2, -4, (quotedstring) ? '"' : '#' COMMA)
				}
				else
					break;
			}
			else if (s[(*i)] == '\n' ) {currentpos->line++; currentpos->collumn = 0;}
			else if (s[(*i)] == ':' && !colon && (s[*i + 1] != '#'))
				colon = 1; 
			if ((s[oldi] == 'D' || s[oldi] == 'I') && colon) {
				if (!bfi && !isspace(s[*i + 1]) && (s[*i + 1] != '#' || KEYWORDCHECK(s[*i + 2], ==, ||))) bfi = *i + 1;
				continue;
			}
			if (s[(*i)] == ')' && parenthese) {
				parenthese = 0; continue;
			}
			if (s[(*i)] == '!' && !exclamationmark && s[oldi] == '$') {
				exclamationmark = 1;
				continue; //avoid ! getting in macroname
			}
			else if (s[oldi] == 'D' || s[oldi] == '$') {
				if (s[(*i)] == '(' && !parenthese) {
					argp = &s[(*i) + 1]; parenthese = 1;
				}
			}
			if(s[oldi] != '"' && !isspace(s[(*i)]) && !parenthese) {
				if (j + 1 == MAXNAME - 1) ERROR("Name length exceeded %d chars in %s line : %ld, collumn : %ld\n", backuppos, -200, MAXNAME COMMA) 
				macroname[j++] = s[(*i)];
			}
		}
		if (!s[(*i)]) ERROR("Not closed %c in %s line : %ld, collumn : %ld\n", backuppos, -5, endc COMMA) 
		macroname[j] = '\0'; //j will now serve as a loop i
		unsigned hashedstring = hashstring(macroname, MAXMACROS),
		foundelement = findelement(macros[hashedstring], macroname),
		pos = (foundelement) ? foundelement - 1 : macros[hashedstring].size;
		char * passedoutput = NULL;
		int retval;
		unsigned l, k, passedoutputpos = 0, encounteredchar = 0;
		if (s[oldi] == 'D') {
			if (!foundelement) {
				if (!macros[hashedstring].macrolist) {
					mallocate(macros[hashedstring].macrolist, sizeof(struct macro) * INITIALLIST);
					initializemacrolist(macros[hashedstring].macrolist, 0, INITIALLIST - 1);
				}
				else if (pos > 0 && !((pos + 1) % INITIALLIST)) {
					mreallocate(macros[hashedstring].macrolist, 
						sizeof(struct macro) * INITIALLIST * ((pos + 1) / INITIALLIST + 1)); 
					initializemacrolist(macros[hashedstring].macrolist, INITIALLIST * ((pos + 1) / INITIALLIST) 
						, INITIALLIST * (pos / INITIALLIST + 1) - 1);
				}
				strcpy(macros[hashedstring].macrolist[pos].name, macroname); 
				macros[hashedstring].size++;
			}
			if (bfi) {
				if (!macros[hashedstring].macrolist[pos].bfi) {
					mallocate(macros[hashedstring].macrolist[pos].bfi, INITIALMBFI);
                 }
				 //replace strings macros , and let the rest to $
                mallocate(passedoutput, INITOUTPUTGIVENSIZE);
				memset(passedoutput, '\0', INITOUTPUTGIVENSIZE);
				for (j = bfi, l = 0; j < *i; j++, l++) {
					if (s[j] == '#' && s[j + 1] == '"') {
						retval =  processmacros(s, &passedoutput, name, &passedoutputpos, &j, currentpos, INITOUTPUTGIVENSIZE, 0);
						if (retval) {free(passedoutput); return retval;}
						for (k = 0; k < passedoutputpos; k++, l++) {
							if (!((l + 1) % INITIALMBFI)) {//for null char
                                mreallocatefree(macros[hashedstring].macrolist[pos].bfi, passedoutput, INITIALMBFI * (((l + 1) / INITIALMBFI) + 1));
                            }
							macros[hashedstring].macrolist[pos].bfi[l] = passedoutput[k];
						}
						if (k) l--;
						passedoutputpos = 0;
					}
					else {
						if (!encounteredchar && isspace(s[j])) {l--; continue;}
						if (s[j] == '\n') encounteredchar = 0;
						else encounteredchar = 1;
						if (!((l + 1) % INITIALMBFI)) {//for null char
                            mreallocatefree(macros[hashedstring].macrolist[pos].bfi, passedoutput, INITIALMBFI * (((l + 1) / INITIALMBFI) + 1));
                        }
						macros[hashedstring].macrolist[pos].bfi[l] = s[j];
					}		
				}
				free(passedoutput);
				macros[hashedstring].macrolist[pos].bfi[l] = '\0';
			}
			else if (macros[hashedstring].macrolist[pos].bfi) {//if defined without ":" after
				free(macros[hashedstring].macrolist[pos].bfi); macros[hashedstring].macrolist[pos].bfi = NULL;
			}
			macros[hashedstring].macrolist[pos].narg = 0;
			if (argp) {
				int sign = 0;
				if (!macros[hashedstring].macrolist[pos].argsigns) {
                    mallocate(macros[hashedstring].macrolist[pos].argsigns, INITIALARGSIGN);
                }				
                for (; *argp; argp++) {
					if (*argp == ',' || *argp == ')') {
						if (macros[hashedstring].macrolist[pos].narg > 0 && macros[hashedstring].macrolist[pos].narg  % INITIALARGSIGN == 0) {
                        mreallocate(macros[hashedstring].macrolist[pos].argsigns, sizeof(int) * INITIALARGSIGN *  
                        (macros[hashedstring].macrolist[pos].narg  / INITIALARGSIGN + 1));
						}
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
			if (!foundelement)  ERROR("Undefining an undefined macro \"%s\" in %s in line : %ld, collumn : %ld\n", backuppos, -6, macroname COMMA)
			if (macros[hashedstring].macrolist[pos].bfi) free(macros[hashedstring].macrolist[pos].bfi);
			if (macros[hashedstring].macrolist[pos].argsigns) free(macros[hashedstring].macrolist[pos].argsigns);
			for (j = pos; j < macros[hashedstring].size; j++)
				macros[hashedstring].macrolist[j] = macros[hashedstring].macrolist[j + 1];
			macros[hashedstring].size--;
		}
		else if (s[oldi] == '$') { 
			if (!foundelement) ERROR("Using an undefined macro \"%s\" in %s in line : %ld, collumn : %ld\n", backuppos, -7, macroname COMMA)
			if (!macros[hashedstring].macrolist[pos].bfi) 
				break;
			if (argp) {
				if (!macros[hashedstring].macrolist[pos].narg)
					ERROR("Adding arguments to a macro: \"%s\" who doesn't have argument in %s in line : %ld, collumn : %ld\n", backuppos, -8, macroname COMMA)
				else if (exclamationmark)
					ERROR("Adding no arguments to a macro: \"%s\" who has '!' (see docs for further infos in %s in line) : %ld, collumn : %ld\n", backuppos, -9, macroname COMMA)
				unsigned currentarg, negative, changesign = 0;
				int cellpos;
				for (cellpos = 0, currentarg = 0; *argp; argp++) { //condition is optional, but just in case
					if (*argp == ',' || *argp == ')') {
						if (cellpos <= (int) macros[hashedstring].macrolist[pos].narg && cellpos >= 0)	
							ERROR("The argument cannot be the copycell or args cell (for further infos check docs macros section) :  \"%s\" in %s in line : %ld, collumn : %ld\n", backuppos, -10, macroname COMMA)
						if (cellpos < 0) {cellpos = -cellpos; negative = 1;}
						else negative = 0;
						GOTOARG('<', '>');
						writetooutput('[');
						LOOPACT('+','-');
						GOTOARG('>', '<');
						LOOPACT('-','+');
						for (l = 0; l < currentarg + 1; l++) {
                            writetooutput('>');
                        }
						LOOPACT('-','+');
						for (l = 0; l < currentarg + 1; l++) {
                            writetooutput('<');
                        }
						GOTOARG('<', '>');
						writetooutput(']');
						GOTOARG('>', '<');
                        writetooutput('[');
						LOOPACT('+','-');
						GOTOARG('<', '>');
						LOOPACT('-','+');
						GOTOARG('>', '<');
						writetooutput(']');
						currentarg++;
						changesign = cellpos = 0;
						if (*argp == ')')
							break;
				}
				else if (*argp == '<')
					cellpos--;
				else if (*argp == '>')
					cellpos++;	
				else if(*argp == '-')
					changesign = 1;
				}
				if (currentarg != macros[hashedstring].macrolist[pos].narg)
					ERROR("%d arguments but : \"%s\" takes %d in %s in line : %ld, collumn : %ld\n", backuppos, -11, currentarg COMMA macroname 
					COMMA macros[hashedstring].macrolist[pos].narg COMMA)
			}
			else if (macros[hashedstring].macrolist[pos].narg && !exclamationmark)
				ERROR("Putting no arguments to a macro: \"%s\" who has argument(s), '!' to avoid this error note: check macro section in macro-bf,\n"
			"in %s in line : %ld, collumn : %ld\n", backuppos, -12, macroname COMMA)
			//get the content of the macro and put it in the file, macro execute the U,I,IN,% and $
			mallocate(passedoutput, INITOUTPUTGIVENSIZE);
			memset(passedoutput, '\0', INITOUTPUTGIVENSIZE);
			for (bfi = 0; macros[hashedstring].macrolist[pos].bfi[bfi]; bfi++) {
				if (macros[hashedstring].macrolist[pos].bfi[bfi] == '#' && KEYWORDCHECK(macros[hashedstring].macrolist[pos].bfi[bfi + 1], ==, ||)) { //no need to check if not string, since #D already deletes the strings
					if (macros[hashedstring].macrolist[pos].bfi[bfi + 1] == '$' || macros[hashedstring].macrolist[pos].bfi[bfi + 1] == '%') {
						CHECKCIRC(0);
					}
					retval = processmacros(macros[hashedstring].macrolist[pos].bfi, &passedoutput, name, &passedoutputpos, &bfi, currentpos, INITOUTPUTGIVENSIZE, 0);
					if (retval) {free(passedoutput); return retval;}
					if (freeb) freecirc();
					for (k = 0; k < passedoutputpos; k++) {
                        writetooutputfree(passedoutput[k], passedoutput);
                    }
					passedoutputpos = 0;
				}
				else {
                    writetooutputfree(macros[hashedstring].macrolist[pos].bfi[bfi], passedoutput);                
                }
			}
			free(passedoutput);
		}
		else if (s[oldi] == '%') {
			CHECKCIRC(1);
			char * file;
			size_t givensize;
			retval = openfile(&file, macroname, &givensize); 
			if (retval) ERROR("Impossible to read the file: \"%s\" in %s in line : %ld, collumn : %ld\n", backuppos, retval, macroname COMMA)
            mallocatefree(passedoutput, givensize + 1, file);
			memset(passedoutput, '\0', givensize);
			retval = preprocess(file, &passedoutput, macroname, 0, givensize); 
			if (retval)  return retval;
			free(file);
			freecirc();
			for (j = 0; passedoutput[j]; j++) {
		        writetooutputfree(passedoutput[j], passedoutput);                
            }			
            free(passedoutput);
		}
		else if (s[oldi] == '"') {
			char temp, inttos[4];
			int xctoi(char c), isodigit(char c), litescapeseq = -1;
			for (; s[bfi]!='"' || s[bfi - 1] == '\\'; bfi++) {
				if (s[bfi] == '\n') continue;
				if (s[bfi] == '\\') 
					switch (s[bfi + 1])
					{
					case 'a': s[++bfi] = '\a'; break;
					case 'b': s[++bfi] = '\b'; break;
					case 'e': s[++bfi] = 27; break;
					case 'f': s[++bfi] = '\f'; break;
					case 'n': s[++bfi] = '\n'; break;
					case 'r': s[++bfi] = '\r'; break;
					case 't': s[++bfi] = '\t'; break;
					case 'v': s[++bfi] = '\v'; break;
					case '\\': s[++bfi] = '\\'; break;
					case '\'': s[++bfi] = '\''; break;
					case '"': bfi++; break;
					case 'x': case 'X':
						if (!isxdigit(s[bfi + 2])) ERROR("No xdigit after \\x in %s in line : %ld, collumn : %ld\n", backuppos, -13, )
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
						}
						else
							ERROR("Nothing valid after \\ in %s in line : %ld, collumn : %ld\n", backuppos, -14, )
						break;
					}
				for (temp = (litescapeseq == - 1) ? s[bfi] : litescapeseq; temp; temp--) {
					writetooutput('+');
                }
				writetooutput(' ');
				writetooutput('c');
				writetooutput('=');
				sprintf(inttos, "%d", (litescapeseq == - 1) ? s[bfi] : litescapeseq);
				for (j = 0; inttos[j]; j++) {
					writetooutput(inttos[j]);
                }
				writetooutput('\n');
				writetooutput('>');
				litescapeseq = -1;
			}
		}
		else if(s[oldi] == 'I') {
			if (!bfi) break;
			if (s[oldi + 1] != 'N') {
				if (!foundelement) break;
			}
			else if (foundelement) break;
			mallocate(passedoutput,INITOUTPUTGIVENSIZE);
			memset(passedoutput, '\0', INITOUTPUTGIVENSIZE);
			for (j = bfi; j < *i; j++) {
				if (s[j] == '#' && KEYWORDCHECK(s[j + 1], ==, ||)) {
					retval =  processmacros(s, &passedoutput, name, &passedoutputpos, &j, currentpos, INITOUTPUTGIVENSIZE, 0);
					if (retval) {free(passedoutput); return retval;}
					for (k = 0; k < passedoutputpos; k++, l++) {
				        writetooutputfree(passedoutput[k], passedoutput);                
					}
					if (k) l--;
					passedoutputpos = 0;
				}
				else {
					if (s[j] == ';') {while (s[j] && s[j] != '\n') j++;} 
					if (!encounteredchar && isspace(s[j])) {l--; continue;}
					if (s[j] == '\n') encounteredchar = 0;
					else encounteredchar = 1;
					writetooutputfree(s[j], passedoutput);
				}
			}
			free(passedoutput);
		}
		break;
	default:
		writetooutput(s[(*i)]);
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
