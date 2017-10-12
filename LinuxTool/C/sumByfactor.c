#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

struct PrimeSum{
	int prime;
	int sum;
	struct PrimeSum *next;
};

struct PrimeList{
	struct PrimeSum *head;
	int count;
};

typedef struct PrimeSum PS;
typedef struct PrimeList PL;

PS *creatPS(int prime, int sum){
	PS *ps = malloc(sizeof(PS));
	ps->prime = prime;
	ps->sum = sum;
	ps->next = NULL;
	return ps;
}

PL *creatPL(){
	PL *l = malloc(sizeof(PL*));
	l->head=NULL;
	l->count=0;
	return l;
}

void insertList(PL *l, int prime, int numb){
	if(l==NULL){
		fprintf(stderr, "Null Input\n");
		exit(1);
	}

	PS *temp = l->head;
	if((l->count==0)&&(l->head==NULL)){
		l->head = creatPS(prime,numb);
		l->count++;
		return;
	}

	if(temp->prime>prime){
		PS *n = creatPS(prime, numb);
		n->next = temp;	
		l->head = n;
		l->count++;
		return;
	}else if(temp->prime==prime){
		temp->sum+=numb;
		return;
	}

	while(temp->next!=NULL){
		if(temp->prime==prime){ 
			temp->sum+=numb;
			return;
		}

		if(prime<temp->next->prime){
			PS *n = creatPS(prime, numb);
			n->next = temp->next;
			temp->next = n;
			l->count++;
			return;
		}
		temp = temp->next;
	}

	if(temp->prime==prime){
		temp->sum+=numb;
		return;
	}

	PS *n = creatPS(prime,numb);
	temp->next = n;
	l->count++;
}

void primeFactors(PL *l,int lst){
	int org = lst;
	if(lst<0) lst*=-1;
	int count=0;
    while (lst%2 == 0){
    	count++;
        lst = lst/2;
    }
    if(count>0) insertList(l,2,org);

    for (int i=3; i<=sqrt(lst); i+=2){
    	count=0;
        while (lst%i == 0){
            count++;
            lst = lst/i;
        }
        if(count>0) insertList(l,i,org);

    }
 
    if (lst > 2) insertList(l,lst,org);
}

char *piece(int prime, int sum){
	char *temp = malloc(sizeof(char)*30);
	sprintf(temp, "(%d %d)", prime, sum);
	return temp;
}

char *sumOfDivided(int* lst, int l) {
	char *summary =  malloc(258);
	int i;
	PL *list = creatPL();
	for(i=0; i<l; i++){
		primeFactors(list,lst[i]);
	}

	PS *temp = list->head;
	while(temp!=NULL){
		printf("%d: %d\n", temp->prime, temp->sum);
		strcat(summary,piece(temp->prime, temp->sum));
		temp=temp->next;
	}
	return summary;
}

int main(int argc, char **argv){
	int arr[] = {-12,15,9,-162731725,-1253113,312,3,45678,-12312,23};
	printf("%s\n",sumOfDivided(arr,10));
	return 0;
}












