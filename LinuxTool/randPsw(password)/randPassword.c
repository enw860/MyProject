#include <stdio.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>
#include "randSupport.h"

int randNumb(int range){
	return rand()%range;
}

char randomSelect(char *pool,int range){
	int position = randNumb(range); 
	return pool[position];
}

char *getPhrase(char *s, int numb){
	char *phrase = malloc(numb+1);

	int n = numb;
	for(;n>0;n--){
		char randChar = randomSelect(s, strlen(s));
		strncat(phrase, &randChar, 1);
	}
	phrase[numb] = '\0';

	return phrase;
}

char *rawPassword(int numb, int cap, int oth, int nor){
	int length = numb + cap + nor + oth;
	char *password = malloc(length+1);

	char *temp1 = getPhrase(number, numb);
	char *temp2 = getPhrase(capital, cap);
	char *temp3 = getPhrase(normal, nor);
	char *temp4 = getPhrase(other, oth);

	strncat(password, temp1, numb);
	strncat(password, temp2, cap);
	strncat(password, temp3, nor);
	strncat(password, temp4, oth);
	password[length] = '\0';

	return password;
}

void swap(char *x, char *y){
    char temp;
    temp = *x;
    *x = *y;
    *y = temp;
}

char *shuffle(char *s){
	int length = strlen(s);

	int a,b,i;
	for(i=0; i<300; i++){
		a = randNumb(length);
		b = randNumb(length);
		swap(&s[a],&s[b]);
	}

	return s;
}

int isSameString(char *s1, char *s2){
	if(strlen(s1)!=strlen(s2)) return 0;
	
	int len;
	for(len=strlen(s1)-1;len>=0;len--){
		if(s1[len]!=s2[len]) return 0;
	}

	return 1;
}

int main(int argc, char **argv){
	srand(time(NULL));

	int passwordLength=0;
	int amount[4] = {-1,-1,-1,-1};

	if(argc==1){
		fprintf(stderr, "USAGE: ./password len [-n] numb [-c] cap [-o] other\n");
		fprintf(stderr, "NOTE: [-n] number of number\n");
		fprintf(stderr, "NOTE: [-c] number of capital\n");
		fprintf(stderr, "NOTE: [-o] number of other char\n");
		exit(1);
	}else{
		passwordLength = atol(argv[1]);

		int i;
		for(i=2; i<argc; i++){
			if(isSameString(argv[i],"-n")){
				amount[0] = atoi(argv[i+1]);
			}else if(isSameString(argv[i],"-c")){
				amount[1] = atoi(argv[i+1]);
			}else if(isSameString(argv[i],"-o")){
				amount[2] = atoi(argv[i+1]);
			}
		}
	}

	int remain=passwordLength,i;
	for(i=0; i<4; i++){
		if(amount[i]>=0){
			remain-=amount[i];
		}
	}

	for(i=0; i<3; i++){
		if(amount[i]<0){
			amount[i]=randNumb(remain);
			remain-=amount[i];
		}
	}
	amount[3] = remain;
	char *pass = rawPassword(amount[0],amount[1],amount[2],amount[3]);

	printf("%s\n", shuffle(pass));

	return 0;
}


