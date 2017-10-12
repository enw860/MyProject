#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "hash.h"

/*
  this method receive a hash and print that hash code

  - hash: original hash code
  - reutrn: nothing
*/
void show_hash(char *hash){
  if(hash==NULL) return;

  char *hash_code = (char *)malloc(sizeof(char)*BLOCK_SIZE*2+1);
  char buffer[3];
  hash_code[0] = '\0';
  
  int i;
  for(i = 0; i < BLOCK_SIZE; i++) {
      snprintf(buffer,3,"%.2hhx",hash[i]);
      strncat(hash_code,buffer,2);
  }
  hash_code[BLOCK_SIZE*2] = '\0';
  printf("%s\n", hash_code);
}

/*
  compute a hash code base on file's content

  - f: a input file
  - return: return a hash code
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
  hash_val[BLOCK_SIZE] = '\0';
  
  return hash_val;
}