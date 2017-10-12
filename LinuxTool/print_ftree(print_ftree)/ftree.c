#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <dirent.h> 
#include <sys/stat.h>
#include <sys/types.h>
#include <string.h>
#include <unistd.h>
#include "ftree.h"
#include "hash.h"
#define MAX_PATH_SIZE 1024
#define MAX_NAME_SIZE 128

void show_hash(char *hash);

/*
  this function receive a path and return a file or directory name of that 
  path represent

  - path: a string represent path of a file
  - return: file or directory name of that path represent
*/
char *getname(const char *path){
  char p[MAX_PATH_SIZE] = {'\0'};
  strncpy(p,path,MAX_PATH_SIZE);
  strncat(p, "\0",1);

  char *name = (char *)malloc(sizeof(char)*MAX_NAME_SIZE);

  char *f;
  f = strtok(p,"/");
  while(f!=NULL){
      strncpy(name,f,MAX_NAME_SIZE);
      strncat(name, "\0",1);
      f = strtok (NULL, "/");
  }
  return name;
}

int isFirst_time = 1;

/*
  recursively going through a file and build tree base on files or dirs in 
  that file

  - fname: a name of a file or a dir that want to build a tree
  - return: an address of a generated tree
*/
struct TreeNode *generate_ftree(const char *fname) {
  char *name = getname(fname);
  
  //use following code if the first node have to be user's input
  /*char *name;
  if(isFirst_time){
    name = (char*)fname;
    isFirst_time = 0;
  }
  else{
    name = getname(fname);
  }*/

  struct stat fileInfo;
      
  //check if file or dir existed
  if(lstat(fname,&fileInfo)<0){
    perror("lstat");
    exit(1);
  }

  //create and initialize a new node
  struct TreeNode *node = (struct TreeNode *)malloc(sizeof(struct TreeNode));
  node->fname = (char *)malloc(sizeof(char)*MAX_NAME_SIZE);
  strncpy(node->fname,name, MAX_NAME_SIZE);
  node->fname[strlen(name)] = '\0';
  node->permissions = fileInfo.st_mode&0777;
  node->next=NULL;
  node->contents=NULL;
    
  //if input is file
  if(!S_ISDIR(fileInfo.st_mode)){
    FILE *myfile = fopen(fname,"rb");
    if(myfile == NULL){
      perror("fopen");
      exit(1);
    }
     
    node->hash = hash(myfile);

    if(fclose(myfile) != 0){
      perror("fclose");
      exit(1);
    }
    return node;
  }
    
  //if input is a directory
  DIR * d = opendir(fname);
  if(d==NULL){
    fprintf(stderr, "Cannot open directory\n");
    exit(1);
  }
  node->hash = NULL;
    
  //reursively going through that directory
  struct dirent * dir;
  while ((dir = readdir(d)) != NULL){  
    char path[MAX_PATH_SIZE];
    int len = snprintf(path, sizeof(path)-1, "%s/%s", fname, dir->d_name);
    path[len] = 0;
    
    //ignore all hidden file and . and .. dir
    if(dir->d_name[0]=='.') continue;
      
    struct TreeNode *temp = generate_ftree(path);
      
    if(node->contents == NULL){   //add head
        node->contents = temp;
    }
    else{                         //add tail
      struct TreeNode *walker = node->contents;

      while(walker->next != NULL){
        walker = walker->next;
      }
      
      walker->next = temp;
    }
  }
  closedir(d);
  return node;
}

/*
  print all element with in tree with different depth

  - root: root of a tree that want to print
  - return: nothing
*/
void print_ftree(struct TreeNode *root) {
  static int depth = 0;
  struct TreeNode *walker = root;
    
  while(walker!=NULL){
    if(walker->hash==NULL){
      printf("%*s===== %s (%o) =====\n",depth * 2,"", walker->fname, walker->permissions);
      //add depth if such node is a directory
      depth++;
      print_ftree(walker->contents);
      depth--;
    }
    else{
      printf("%*s%s (%o)\n",depth*2,"", walker->fname, walker->permissions);
      //show_hash(walker->hash);
    }
    walker = walker->next;
  }
}

