#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <dirent.h> 
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <libgen.h>

#include "ftree.h"
#include "hash.h"

#define MAX_PATH_SIZE 1024

/*function chech if two file have same permission, if not then change it*/
int check_permission(struct stat src_entry, struct stat dest_entry, const char *dest_path){
	if(src_entry.st_mode != dest_entry.st_mode){
		if(chmod(dest_path,src_entry.st_mode)<0){
			int errsv = errno;
		    //case when permission error raised
		    if(errsv==EACCES){
		      fprintf(stderr, "Unable change permission DIRECTORY(%s) : Permission Denied\n", basename((char*)dest_path));
		    }
		    return -1;
		}
	}
	return 0;
}

/* 
  Function for copying a file tree rooted at src to dest.
  Returns -1 on error. The magnitude of the return value
  is the number of processes involved in the copy and is
  at least 1.
*/
int copy_ftree(const char *src, const char *dest){
    //counting amount of processes used
    static int process=0;

    //check if src exist
	struct stat src_info;
  	if(lstat(src,&src_info)<0){
		perror("lstat");
	    return -1;
	}

	//check if dest exist
	struct stat dest_info;
	if(lstat(dest,&dest_info)<0){
		perror("lstat");
	    return -1;
	}

	//check if src is a link
	if(S_ISLNK(src_info.st_mode)){
	    fprintf(stderr, "REJRCT LINK %s\n",src);
	    return -1;
	}

	//check if dest is a link
	if(S_ISLNK(dest_info.st_mode)){
	    fprintf(stderr, "REJECT LINK %s\n",dest);
	    return -1;
	}

	DIR *object = opendir(src);
	if(object==NULL){
	  	int errsv = errno;
	    //case when permission error raised
	    if(errsv==EACCES){
	      	fprintf(stderr, "Cannot open DIRECTORY(%s) : Permission Denied\n", basename((char*)src));
	    }else if(errsv==ENOTDIR){ 
	    	//when src not a directory
	      	fprintf(stderr, "%s is not a directory\n", basename((char*)src));
	    }else{
	    	perror("opendir");
	    } 
	    return -1;
	}

	//make a new directory if not exist and make it permission same as src
	//if such directory exists then change it mode
	//otherwise raise error
	char path_dest[MAX_PATH_SIZE];
	int len = snprintf(path_dest, sizeof(path_dest)-1, "%s/%s", dest, basename((char*)src));
	path_dest[len] = 0;
	if(mkdir(path_dest,src_info.st_mode)!=0){
	    if(lstat(path_dest,&dest_info)<0){
	      	fprintf(stderr, "%s not a VALID directory\n",dest);
	      	return -1;
		}

		//if such directory exist change its permission
	    if(!S_ISDIR(dest_info.st_mode)){
	      	perror("mkdir");
	      	return -1;
	    }

	    if(check_permission(src_info, dest_info,path_dest)==-1){
		   	return -1;
		}
	}

	struct dirent *dir;
	while((dir = readdir(object))!= NULL){
		//path of sorce file
		char src_f_path[MAX_PATH_SIZE];
	    int len0 = snprintf(src_f_path, sizeof(src_f_path)-1, "%s/%s", src, dir->d_name);
	    src_f_path[len0] = 0;
	    
	    //path of where copy to
	    char dest_f_path[MAX_PATH_SIZE];  
	    int len1 = snprintf(dest_f_path, sizeof(path_dest)-1, "%s/%s", path_dest, dir->d_name);
	    dest_f_path[len1] = 0;

	    //skip hidden files
	    if(dir->d_name[0]=='.') continue;

		struct stat src_entry;
		if(lstat(src_f_path, &src_entry) == -1){
			fprintf(stderr, "Fail to load(%s): SKIP\n", src_f_path);
			continue;
		}
		
		if(S_ISREG(src_entry.st_mode)){
			//open source file and read
			FILE *src_file = fopen(src_f_path, "rb");
			if(src_file == NULL){
				int errsv = errno;
				//check if permission denied
		        if(errsv==EACCES){
		          fprintf(stderr, "Cannot open FILE(%s) : Permission Denied\n", basename(src_f_path));
		          continue;
		        } 
		        perror("fopen");
		        return -1;
			}
				
			// check if dest_file file existed->check size
			//->check hash,change mode
			FILE *dest_file = NULL;
				
			struct stat dest_entry;
			if(lstat(dest_f_path, &dest_entry) == 0){
				if((dest_file = fopen(dest_f_path, "rb"))==NULL){
					int errsv = errno;
				    if(errsv==EACCES){
				        fprintf(stderr, "Cannot open FILE(%s) : Permission Denied\n", basename(dest_f_path));
				        fclose(src_file);
				        continue;
				    } 
				    perror("fopen");
				    return -1;
				}

				if(dest_entry.st_size == src_entry.st_size){
					if(check_permission(src_entry, dest_entry, dest_f_path)<0){
						return -1;
					}

					if(strncmp(hash(src_file), hash(dest_file),8) == 0){
						fclose(src_file);
					    fclose(dest_file);
					    continue;
					}
					fclose(dest_file);
				}
			}
			
			//copy file and change permission
			dest_file = fopen(dest_f_path, "wb");
			if(dest_file == NULL){	
				int errsv = errno;
			    if(errsv==EACCES){
				    fprintf(stderr, "Cannot open FILE(%s) : Permission Denied\n", basename(dest_f_path));
				    fclose(src_file);
				    fclose(dest_file);
				    continue;
				}
			}

			if(check_permission(src_entry, dest_entry, dest_f_path)<0){
				return -1;
			}

			char c = '\0';
			while(fread(&c, sizeof(char), 1, src_file) == 1){
				fwrite(&c, sizeof(char), 1, dest_file);
			}

			if((fclose(src_file)!=0)||(fclose(dest_file)!=0)){
				perror("fclose");
      			return -1;
			}
		}

		//if read_file is a directory, create a process for each directory
		if(S_ISDIR(src_entry.st_mode)){
			//create a process
			int r = fork();

			//fork error
			if (r < 0) { 				
				perror("fork");
				return -1;
			}

			//child process
			if(r == 0){
				copy_ftree(src_f_path, path_dest);
				exit(process);
			}
			else{						
				//parent process
				int status;
				if(wait(&status) == -1){
					perror("wait");
					return -1;
				}
				if(WIFEXITED(status)){
					process= WEXITSTATUS(status);
				}
			}
		}
	}
	closedir(object);
	return ++process;
}

