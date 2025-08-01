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
			printf("listmacro[%d] and listmacro.macrolist[%d] name: %s\nbfi = %s\n", i, j, p[i].macrolist[j].name, 
			(p[i].macrolist[j].bfi) ? p[i].macrolist[j].bfi : "NULL");
			if (p[i].macrolist[j].bfi)
				free(p[i].macrolist[j].bfi);
		}
		free(p[i].macrolist);
		if (p[i].size) printf("-----------------\n");
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

#define KEYWORDCHECK(i, equalsign, comparison) (s[i] equalsign 'D' comparison s[i] \
equalsign 'U' comparison s[i] equalsign 'I' comparison s[i] equalsign '$' comparison s[i] equalsign '"' comparison s[i] equalsign '%')

#define ERROR(message, givenbackuppos, ret, additionalformat) {freesubpointers(macros);\
fprintf(stderr, message, additionalformat name, givenbackuppos.line + 1, givenbackuppos.collumn + 1);  return ret;} 

#define COMMA ,


struct listmacro macros[MAXMACROS] = { };

struct pos {
	size_t line; 
	size_t collumn;
};

#define INITIALLIST 4
#define INITIALMBFI 100

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
	for (i = first; i <= last; i++)
		macrolist[i].bfi = NULL; 
}

int processmacros(char * code, char * output, char *name, int *outputpos, unsigned *i, struct pos *currentpos);

int preprocess(char * s, char * output, char * name)
{
	if (macros[0].macrolist == NULL)
		for (unsigned i = 0; i < MAXMACROS; i++) {
			macros[i].macrolist = NULL; //will have to malloc to create a list, in this list bfi should be null if there's no instruction, or malloc
			macros[i].size = 0;
		}
	unsigned i;
	int outputpos ,outputwrite = 0;
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

int processmacros(char * s, char * output, char *name, int *outputpos, unsigned *i, struct pos *currentpos)
{
	switch (s[(*i) + 1])
	{
	case 'D': case 'U': case 'I': case '$': case '"':  case '%':
		struct pos backuppos = (*currentpos);
		++(*i); currentpos->collumn++;
		unsigned oldi = (*i), j = 0 ,colon = 0;
		unsigned char endc;
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
				else if (s[oldi] == 'D' || s[oldi] == 'I') {
					struct pos backuppos2 = (*currentpos);
					(*i)++;
					if (KEYWORDCHECK((*i), !=, &&)) {(*i)--; break;}
					(*i)++;
					if (s[(*i) - 1] == 'D' || s[(*i) - 1] == 'I') { //with : after
						int stack;
						for (stack = 1; s[(*i)] && stack; (*i)++, currentpos->collumn++) {
							if (s[(*i)] == '#') {
								if (KEYWORDCHECK((*i) + 1, ==, ||))
									stack++;
								else
									stack--;
							}
							else if (s[(*i)] == '\n' ) {currentpos->line++; currentpos->collumn = 0;}
						}
						if (!stack) {(*i)--; currentpos->collumn--;}
					}
					else if (s[(*i) - 1] == 'U' || s[(*i) - 1] == '$' || s[(*i) - 1] == '%') //with nothing after
						for (; s[(*i)]; (*i)++, currentpos->collumn++) {
							if (s[(*i)] == '#') break;
							else if (s[(*i)] == '\n' ) {currentpos->line++; currentpos->collumn = 0;}
						}
					else if (s[(*i) - 1] == '"') //different stuff
						for (; s[(*i)] && s[(*i)] != '"'; (*i)++, currentpos->collumn++) {
							if (s[(*i)] == '"' && s[(*i) - 1] != '\\')
								break;
							else if (s[(*i)] == '\n' ) {currentpos->line++; currentpos->collumn = 0;}
						}
					if (!s[(*i)]) ERROR("Not closed # in %s line : %ld, collumn : %ld\n", backuppos2, -1,)
				}
				else
					break;
			}
			else if (s[(*i)] == '\n' ) {currentpos->line++; currentpos->collumn = 0;}
			else if (s[(*i)] == ':' && !colon) {
				colon = 1; 
				bfi = *i + 1;
			}
			else if (s[(*i)] == ')' && parenthese) {
				parenthese = 0;
			}
			else
				 currentpos->collumn++;
			if (s[oldi] == 'D' || s[oldi] == 'I') {
				if (colon)
					continue;
			}
			if (s[oldi] == 'D' || s[oldi] == '$') {
				if (s[(*i)] == '(' && !parenthese) {
					if (!s[(*i) + 1] || s[(*i)+1] == '#') ERROR("Not closed parentheses in %s line : %ld, collumn : %ld\n", backuppos, -1, ) 
					argp = &s[(*i) + 1]; parenthese = 1;
				}
			}
			if(s[oldi] != '"') {
				if (!isspace(s[(*i)]))
					macroname[j++] = s[(*i)];	
			}
		}
		if (!s[(*i)]) ERROR("Not closed %c in %s line : %ld, collumn : %ld\n", backuppos, -1, endc COMMA) 
		else if (parenthese) ERROR("Not closed parentheses in %s line : %ld, collumn : %ld\n", backuppos, -1, ) 
		macroname[j] = '\0'; 
		unsigned hashedstring = hashstring(macroname), 
		foundelement = findelement(macros[hashedstring], macroname),
		pos = (foundelement) ? foundelement - 1 : macros[hashedstring].size;
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
				if (*i - bfi >= INITIALMBFI)
					macros[hashedstring].macrolist[pos].bfi = reallocate(macros[hashedstring].macrolist[pos].bfi, INITIALMBFI * ((*i - bfi) / INITIALMBFI + 1));
				strncpy(macros[hashedstring].macrolist[pos].bfi, s + bfi, *i - bfi);		
			}
			else {
				if (macros[hashedstring].macrolist[pos].bfi) {
					free(macros[hashedstring].macrolist[pos].bfi); macros[hashedstring].macrolist[pos].bfi = NULL;
				}
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
			/*if (!macros[hashedstring].macrolist[pos].bfi)
				break;
			if (argp) {
				unsigned args;
				for (; *argp != ')'; argp++)
					if (*argp == ',')
						args++;
				args++;
			}
			bfi = macros[hashedstring].macrolist[pos].bfi;
			for (; *bfi; bfi++)
				if (!isspace(*bfi))
					output[(*outputpos)++] = *bfi;*/
		}
		else if (s[oldi] == '%') {
			char file[CODESIZE], macroreplaced[OUTPUTSIZE];
			printf("opened file %s\n", macroname);
		}
		else if (s[oldi] == '"') {
			char temp;
			/*for (; s[bfi]!='"' && s[bfi - 1] == '\\'; bfi++) {
				for (temp = bfi; temp; temp--)
					output[(*outputpos)++] = '+';
				output[(*outputpos)++] = ' ';
				output[(*outputpos)++] = *bfi;
				output[(*outputpos)++] = '\n';	
				output[(*outputpos)++] = '>';
			}*/
		}
		break;
	default:
		output[(*outputpos)++] = s[(*i)];
		break;
	}
	return 0;
}
