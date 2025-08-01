#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <math.h>
#include <string.h>
#include <ctype.h>
#define CODESIZE 10000
#define OUTPUTSIZE 15000
#define MAXNAME 100
#define MAXARGUMENTS 100
#define MAXMACROS 1000
#define INITIALLIST 4

void openfile(char * s, char * filename);

int preprocess(char * code, char * output, char * filename);

int main(int argc, char ** argv)
{
	if (argc == 1) {perror("Need a file to preprocess, usage: ./mbf file.mbf output.bf [optional]\n"); return -1;}
	char code[CODESIZE], output[OUTPUTSIZE];
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
		for (j = 0; j < p[i].size; j++)
			printf("%d and %d : %s\n", i, j, p[i].macrolist[j].name);
		free(p[i].macrolist);
	}
}

int findelement(struct listmacro list, char * name)
{
	unsigned i;
	for (i = 0; i < list.size; i++)
		if (!strcmp(list.macrolist[i].name, name))
			return i + 1;
	return 0;
}

int preprocess(char * s, char * output, char * name)
{
	struct pos {size_t line; size_t collumn;};
	static struct listmacro macros[MAXMACROS];
	if (macros[0].macrolist == NULL)
		for (unsigned i = 0; i < MAXMACROS; i++) {
			macros[i].macrolist = malloc(sizeof(struct macro) * INITIALLIST);
			macros[i].size = 0;
		}
	struct macro vmacro;
	char includefile[OUTPUTSIZE], macroname[MAXNAME];
	unsigned i;;
	int outputpos ,outputwrite = 0;
	struct pos currentpos = {.line = 0, .collumn = 0}, backuppos;
	for (outputpos = i = 0; s[i]; i++, currentpos.collumn++)
		switch(s[i])
		{
		case '#':
			if (!s[i + 1])
				break;
			switch (s[i + 1])
			{
				case 'D': case 'U': case 'I': case '$': case '"':  case '%':
					backuppos = currentpos;
					++i; currentpos.collumn++;
					unsigned oldi = i, j = 0 ,colon = 0;
					unsigned char endc;
					char * bfi = NULL, * argp = NULL;
					int parenthese = 0;
					if (s[i] == '"') {
						endc = '"';
						if (s[i + 1] && s[i + 1] != '"')
							bfi = &s[i + 1];
					}
					else
						endc = '#';

					while (s[++i]) {
						currentpos.collumn++;	
						if (s[i] == endc) {
							if (endc == '"') {
								if (s[i - 1] == '\\')
									continue;	
							}
							else if (s[oldi] == 'D' || s[oldi] == 'I') {
								struct pos backuppos2 = currentpos;
								i++;
								if (!s[i] || (s[i] != 'D' && s[i] != 'I' && s[i] != 'U' && s[i] != '$' && s[i] != '%' && s[i] != '"')) {i--; break;}
								i++;
								if (s[i - 1] == 'D' || s[i - 1] == 'I') {//gotta check ""
									int stack;
									for (stack = 1; s[i] && stack; i++, currentpos.collumn++) {
										if (s[i] == '#') {
											if (s[i + 1] == 'D' || s[i + 1] == 'I' || s[i + 1] == 'U' || s[i + 1] == '$' || s[i + 1] == '%')
												stack++;
											else
												stack--;
										}
										else if (s[i] == '\n' ) {currentpos.line++; currentpos.collumn = 0;}
									}
									if (!stack) {i--; currentpos.collumn--;}
								}
								else if (s[i - 1] == 'U' || s[i - 1] == '$' || s[i - 1] == '%')
									for (; s[i]; i++, currentpos.collumn++) {
										if (s[i] == '#') break;
										else if (s[i] == '\n' ) {currentpos.line++; currentpos.collumn = 0;}
									}
								else if (s[i - 1] == '"')
									for (; s[i] && s[i] != '"'; i++, currentpos.collumn++) {
										if (s[i] == '"')
											break;
										else if (s[i] == '\n' ) {currentpos.line++; currentpos.collumn = 0;}
									}
								if (!s[i]) {freesubpointers(macros);fprintf(stderr, "Not closed # in %s line : %ld, collumn : %ld\n",  name, backuppos2.line + 1, backuppos2.collumn + 1); return -1;}; 		
							}
							else
								break;
						}
						else if (s[i] == '\n' ) {currentpos.line++; currentpos.collumn = 0;}
						else if (s[i] == ':' && !colon) {
							colon = 1; 
							if (s[i+1] && s[i+1] != '#') {
								bfi	= &s[i + 1];
							}
						}
						else if (s[i] == ')' && parenthese) {
							parenthese = 0;
						}
						if (s[oldi] == 'D' || s[oldi] == 'I') {
							if (colon)
								continue;
						}
						if (s[oldi] == 'D' || s[oldi] == '$') {
							if (s[i] == '(' && !parenthese) {
								if (!s[i + 1] || s[i+1] == '#') {freesubpointers(macros);fprintf(stderr, "Not closed parentheses in %s line : %ld, collumn : %ld\n",  name, backuppos.line + 1, backuppos.collumn + 1); return -1;}; 		
								argp = &s[i + 1]; parenthese = 1;
							}
						}
						if(s[oldi] != '"') {
							if (!isspace(s[i]))
								macroname[j++] = s[i];	
						}
					}
					if (!s[i]) {freesubpointers(macros);fprintf(stderr, "Not closed %c in %s line : %ld, collumn : %ld\n", endc, name, backuppos.line + 1, backuppos.collumn + 1); return -1;}				
					else if (parenthese) {freesubpointers(macros);fprintf(stderr, "Not closed parentheses in %s line : %ld, collumn : %ld\n",  name, backuppos.line + 1, backuppos.collumn + 1); return -1;}; 		
					macroname[j] = '\0'; 
					unsigned hashedstring = hashstring(macroname), 
					foundelement = findelement(macros[hashedstring], macroname),
					pos = (foundelement) ? foundelement - 1 : macros[hashedstring].size;
					if (s[oldi] == 'D') {
						if (!foundelement) {strcpy(macros[hashedstring].macrolist[pos].name, macroname); macros[hashedstring].size++;};
						macros[hashedstring].macrolist[pos].bfi = bfi;
						macros[hashedstring].macrolist[pos].narg = 0;
						if (argp) {
							for (; *argp != ')'; argp++)
								if (*argp == ',')
									macros[hashedstring].macrolist[pos].narg++;
							macros[hashedstring].macrolist[pos].narg++;						
						}
					}
					else if (s[oldi] == 'U') {
						if (!foundelement) {freesubpointers(macros);fprintf(stderr, "Undefining an undefined macro \"%s\" in %s in line : %ld, collumn : %ld\n", macroname, name, backuppos.line + 1, backuppos.collumn + 1); return -1;}
						if (pos != macros[hashedstring].size) {
							for (j = pos; j < macros[hashedstring].size; j++)
								macros[hashedstring].macrolist[j] = macros[hashedstring].macrolist[j + 1];
						}
						macros[hashedstring].size--;
					}
					else if (s[oldi] == '$') {
						if (!foundelement) {freesubpointers(macros);fprintf(stderr, "Using an undefined macro \"%s\" in %s in line : %ld, collumn : %ld\n", macroname, name, backuppos.line + 1, backuppos.collumn + 1); return -1;}
						if (!macros[hashedstring].macrolist[pos].bfi)
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
								output[outputpos++] = *bfi;
					}
					else if (s[oldi] == '%') {
						printf("opened file %s\n", macroname);
					}
					else if (s[oldi] == '"') {
						char temp;
						for (; *bfi!='"'; bfi++) {
							for (temp = *bfi; temp; temp--)
								output[outputpos++] = '+';
							output[outputpos++] = ' ';
							output[outputpos++] = *bfi;
							output[outputpos++] = '\n';	
							output[outputpos++] = '>';
						}
					}
					break;
				default:
					output[outputpos++] = s[i];
					break;
			}
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
