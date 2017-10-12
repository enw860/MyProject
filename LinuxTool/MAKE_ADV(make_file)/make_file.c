#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <errno.h>
#include <libgen.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netdb.h>

#define true 1
#define false 0
#define MAX_NAME 30
#define MAX_PATH 50

typedef int bool;

void print_usage(){
	fprintf(stderr, "USAGE: make_file [-o] dirctory name\n");
	fprintf(stderr, "Note:\t[-o] program name\n\n");
}

//see if command matches
bool match_char(char command, char *str){
	int length,i; 
	
	length= strlen(str);

	for(i=0; i<length; i++){
	 	if(str[i]==command){
	 		return true;
	 	}
	}
	return false;
}

int main(int argc, char **argv){
	if(argc<2){
		print_usage();
		return 1;
	}

	int args_init_position = 1;

	bool dft_name;
	dft_name = false;

	char *dft_prog_name = malloc(MAX_NAME);
	strncpy(dft_prog_name,"prog",strlen("prog"));
	dft_prog_name[strlen("prog")] = '\0';

	char *flags = malloc(MAX_NAME);
	strncpy(flags, "-Wall -g",strlen("-Wall -g"));
	flags[strlen("-Wall -g")] = '\0';

	//process commands
	if(argv[1][0]=='-'){
		args_init_position++;

		dft_name = match_char('o',argv[1]);

		if(dft_name){
			if((argc<3)||(match_char('.',argv[2]))){
				fprintf(stderr, "Not a valid program name\n\n");
				print_usage();
				return 1;
			}else if(argc<4){
				fprintf(stderr, "Missing arguement\n");
				print_usage();
				return 1;
			}
			
			strncpy(dft_prog_name,argv[2],strlen(argv[2]));
			dft_prog_name[strlen(argv[2])] = '\0';
			args_init_position++;
		}
	}

	char hfile[MAX_NAME] = {'\0'};
	char cfile[MAX_NAME] = {'\0'};
	char ofile[MAX_NAME] = {'\0'};

	//validate input directory
	char *dir_name = malloc(MAX_NAME);
	strncpy(dir_name,argv[args_init_position],strlen(argv[args_init_position]));

	struct stat dirstate;
	if(lstat(argv[args_init_position], &dirstate)){
		fprintf(stderr,"DIRECTORY not exist\n\n");
		print_usage();
		return 1;
	}

	if(!S_ISDIR(dirstate.st_mode)){
		fprintf(stderr, "Not a valid DIRECTORY\n\n");
		print_usage();
		return 1;
	}

	DIR * d = opendir(dir_name);
	if(d==NULL){
	    perror("opendir");
	    return 1;
	}

	//process file in directory
	struct dirent * dir;
	while ((dir = readdir(d)) != NULL){  
		if(dir->d_name[0]=='.') continue;
		    
		if(strstr(dir->d_name,".h")!=NULL){
	    	strncat(hfile, dir->d_name,strlen(dir->d_name));
			strncat(hfile, " ",2);
	    }else if(strstr(dir->d_name,".c")!=NULL){
	    	strncat(cfile, dir->d_name,strlen(dir->d_name));
			strncat(cfile, " ",2);

			char temp[50];
			strncpy(temp, dir->d_name,50);
			char *loc = strstr(temp,".")+1;
			*loc = 'o';
			strncat(ofile, temp,strlen(temp));
			strncat(ofile, " ",2);
	    }
		closedir(d);
	}
	
	//make a Makefile for such directory
	char *filename = malloc(MAX_PATH);
    int path_len = snprintf(filename, MAX_PATH-1, 
    	"%s/%s",argv[args_init_position], "Makefile");
    filename[path_len]='\0';

    FILE *target = fopen(filename, "wb");
	if(target == NULL){
		perror("fopen Makefile");
		exit(1);
	}

	fprintf(target, "FLAGS = %s\n\n", flags);
	fprintf(target, "%s : %s\n",dft_prog_name ,ofile);
	fprintf(target, "\tgcc ${FLAGS} -o $@ $^\n\n");
	fprintf(target, "%%.o : %%.c %s\n", hfile);
	fprintf(target, "\tgcc ${FLAGS} -c $<\n\n");
	fprintf(target, "clean :\n");
	fprintf(target, "\trm *.o %s\n",dft_prog_name);

	if(fclose(target)!=0){
		perror("fclose");
		exit(1);
	}

	printf("Done!!!\n");

	return 0;
}
