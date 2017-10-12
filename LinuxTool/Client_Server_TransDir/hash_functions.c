#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BLOCK_SIZE 8
/*
  compute a hash for a given file
*/
char *hash(FILE *f) {
  if(f==NULL){
    fprintf(stderr, "File record is NULL\n");
    exit(1);
  }

  char *hash_val = malloc(sizeof(char)*BLOCK_SIZE+1);
  char s = '\0';

  int i;
  for(i=0; i<BLOCK_SIZE+1; i++)
  {
    hash_val[i] = '\0';
  }

  //read every char in file one at a time
  int counter=0;
  while(fread(&s, sizeof(char), 1,f)==1){
    if(counter==BLOCK_SIZE)
    {
      counter=0;
    }
    char temp = s^ hash_val[counter];
    hash_val[counter] = temp;
    counter++;
  }
  return hash_val;
}

