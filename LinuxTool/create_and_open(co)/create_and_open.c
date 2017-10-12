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
#include <time.h>
#define MAX_ARGS 10

/*
	create .c file and open it in sublime
*/

int match_command(char command, char *str){
	int length = strlen(str);

	int i;
	for(i=0; i<length; i++){
	 	if(str[i]==command){
	 		return 1;
	 	}
	}
	return 0;
}

void c_file_inital(FILE *temp){
	fprintf(temp, "#include <stdio.h>\n");
	fprintf(temp, "#include <string.h>\n");
	fprintf(temp, "#include <sys/stat.h>\n");
	fprintf(temp, "#include <stdlib.h>\n");
	fprintf(temp, "#include <dirent.h>\n");
	fprintf(temp, "#include <sys/types.h>\n");
	fprintf(temp, "#include <sys/wait.h>\n");
	fprintf(temp, "#include <unistd.h>\n");
	fprintf(temp, "#include <errno.h>\n");
	fprintf(temp, "#include <libgen.h>\n");
	fprintf(temp, "#include <arpa/inet.h>\n");
	fprintf(temp, "#include <netinet/in.h>\n");
	fprintf(temp, "#include <netdb.h>\n\n");
	fprintf(temp, "int main(int argc, char **argv){\n");
	fprintf(temp, "\tprintf(\"Hello,World\\n\");\n");
	fprintf(temp, "\treturn 0;\n");
	fprintf(temp, "}\n");
}

void regular_file_init(FILE *temp){
	time_t t = time(NULL);
	struct tm tm = *localtime(&t);

	fprintf(temp,"Date: %d-%d-%d Time: %d:%d:%d\n", tm.tm_year + 1900, tm.tm_mon + 1, 
		tm.tm_mday, tm.tm_hour, tm.tm_min, tm.tm_sec);
}

int main(int argc, char **argv){
	int c_file,default_open,argus_start,args_count;
	
	c_file = 0;
	default_open = 0;
	argus_start = 1;

	if(argc<2){
		fprintf(stderr, "USAGE: co [-c][-d] filenames\n");
		fprintf(stderr, "NOTE:\tdefault co create a regular file open with sublime\n");
		fprintf(stderr, "\t[-c]create c file mode\n");
		fprintf(stderr, "\t[-d]open in default mode\n");
		exit(1);
	}

	if(argv[1][0]=='-'){
		argus_start = 2;
		c_file = match_command('c', argv[1]);
		default_open = match_command('d', argv[1]);
	}

	//builds command line arguements
	char **args = malloc(sizeof(char*)*MAX_ARGS);
	
	//indicate using sublime to open file
	if(default_open){
		args_count = 2;
		args[0] = malloc(strlen("open")+1);
		strncpy(args[0], "open",strlen("open"));
		args[0][strlen("open")] = '\0';

		args[1] = malloc(strlen("-t")+1);
		strncpy(args[1], "-t",strlen("-t"));
		args[1][strlen("-t")] = '\0';
	}else{
		args_count = 1;
		args[0] = malloc(strlen("sublime")+1);
		strncpy(args[0], "sublime",strlen("sublime"));
		args[0][strlen("sublime")] = '\0';
	}
	

	int i;
	for(i=argus_start; i<argc; i++){
		//add command arguements to sublime arguements
		args[args_count] = malloc(strlen(argv[i])+1);
		strncpy(args[args_count], argv[i],strlen(argv[i]));
		args[args_count][strlen(argv[i])] = '\0';
		args_count++;

		FILE *temp;
		struct stat filestate;
		if(lstat(argv[i], &filestate)==0){
			continue;
		}else{
			temp = fopen(argv[i], "wb");
		}

		if(temp==NULL){
			perror("fopen");
			exit(1);
		}

		chmod(argv[i],0777);

		if(c_file){
			c_file_inital(temp);
		}else{
			regular_file_init(temp);
		}

		if(fclose(temp)!=0){
			perror("fclose");
			exit(1);
		}
	}

	args[args_count]=NULL;

	//open files in terminal
	if(default_open){
		execv("/usr/bin/open",args);
	}else{
		execv("/usr/local/bin/sublime", args);
	}

	return 0;
}