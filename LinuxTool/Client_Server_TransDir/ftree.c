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
#include "ftree.h"
#include "hash.h"
#include <arpa/inet.h>
#include <netinet/in.h>    
#include <netdb.h>

#define MAX_BACKLOG 5
#define MAX_CONNECTIONS 12
#define BUF_SIZE 128

//define a struct that store every socket number and request for server side
struct File {
    int sock_fd;
    struct request receivefile;
    int step;
};

//////////////////////CLIENT SIDE////////////////////////////

//prefix of input file
static char dest_prefix[MAXPATH]={'\0'};
// total number of file sender client
int num_children = 0;

/*
  Return a connection port number by the given host name and port
*/
int make_connection(char *host, unsigned short port){
    struct hostent *hp;
    hp = gethostbyname(host);                
    if ( hp == NULL ) {  
        fprintf(stderr, "%s: unknown host\n", host);
        return -1;
    }

    int sock_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (sock_fd < 0) {
        perror("client: socket");
        return -1;
    }

    // Set the IP and port of the server to connect to.
    struct sockaddr_in server;
    server.sin_family = AF_INET;
    server.sin_port = htons(port);
    server.sin_addr = *((struct in_addr *)hp->h_addr);

    if (connect(sock_fd, (struct sockaddr *)&server, sizeof(server)) == -1) {
        perror("client: connect");
        return -1;
    }
    return sock_fd;
}

/*
  Fill in all the information for a single request, return 0 if finished.

*/
int make_request(struct request *sendfile,char *src,struct stat src_info, char *sendname){
    strncpy(sendfile->path,sendname ,MAXPATH);
    sendfile->path[strlen(sendname)] = '\0';
    sendfile->mode = src_info.st_mode;
    sendfile->size = src_info.st_size;

    //fill in additional information for a regular file
    if(S_ISREG(src_info.st_mode)){
        FILE *file = fopen(src,"rb");
        if(file==NULL){
            int errsv = errno;
            //skip the case when permission error raised
            if(errsv==EACCES){
                fprintf(stderr, "%s Permission Denied: SKIP\n", src);
                return 2;
            }
            else{
            	perror("fopen");
            	return 1;
            }
        }
        sendfile->type = REGFILE;
        strncpy(sendfile->hash,hash(file),BLOCKSIZE);
    }else if(S_ISDIR(src_info.st_mode)){
    	//fill in additional information for a regular directory
        sendfile->type = REGDIR;
    }
    return 0;
}

/*
  Sent a request to server, return 0 if finished, 1 other wise.
*/
int send_request(int sock_fd, struct request sendfile, char *src){
	int state,i, read_state;
	int file_type = htonl(sendfile.type);
	if(write(sock_fd, &file_type, sizeof(int)) != sizeof(int)) {
	    perror("write,type");
	    return 1;
	}

	for(i=1; i<5; i++){
		if(read(sock_fd, &read_state, sizeof(int))!=sizeof(int)){
			perror("read");
			close(sock_fd);
			return 1;
		}
		state = ntohl(read_state);

		if(state==AWAITING_PATH){
			if(write(sock_fd, sendfile.path, MAXPATH) != MAXPATH) {
	            perror("write,path");
	            return 1;
	        }
		}
		else if(state==AWAITING_SIZE){
			int convert_size = htonl(sendfile.size);
			if(write(sock_fd, &convert_size, sizeof(int)) != sizeof(int)) {
	            perror("write,size");
	       	    return 1;
	        }
		}
		else if(state==AWAITING_PERM){
			if(write(sock_fd, &(sendfile.mode), sizeof(mode_t)) != sizeof(mode_t)) {
	            perror("write,mode");
	            return 1;
	        }
		}
		else if(state==AWAITING_HASH){
			if(write(sock_fd, sendfile.hash, BLOCKSIZE) != BLOCKSIZE) {
	            perror("write,hash");
	            return 1;
	        }
		}
	}

	if(sendfile.type == TRANSFILE){
		if(read(sock_fd, &read_state, sizeof(int))!=sizeof(int)){
			perror("read");
			close(sock_fd);
			return 1;
		}
		state = ntohl(read_state);

		if(state==AWAITING_DATA){
			//we already checking for file's read premission and existence
			//it's safe to read it
	        FILE*f = fopen(src,"rb");
	       	if(f==NULL){
	       		return 1;
	        }

	        char c[MAXDATA];
    		int num_read;
    		while((num_read =fread(c,sizeof(char), MAXDATA,f)) == MAXDATA){
    			write(sock_fd,c,num_read);
    	    }
    		if (num_read){
    			write(sock_fd,c,num_read);
    		}
		
	        fclose(f);
	    }else{
	    	return 1;
	    }
	}
	return 0;
}

/*
  Helper function for copying client file tree to server, return 0 if finished, return 1 when a error occurs.
*/
int copy_tree(char *source, char *host, unsigned short port,int sock_fd,struct stat src_info){
	//file path in client side
	char origin_path[MAXPATH];
	int origin_path_len = snprintf(origin_path, sizeof(origin_path)-1, "%s/%s",dest_prefix, source);
    origin_path[origin_path_len] = '\0';

    //make and send a request to server 
	struct request sendfile; 
	int make_state = make_request(&sendfile,origin_path,src_info,source);
	if(make_state==1){
		return 1;
	}else if(make_state==2){
		//skip file due to premission denied
		return 0;
	}

	if(send_request(sock_fd,sendfile,origin_path)){
    	fprintf(stderr, "Error happen as sending request\n");
    	return 1;
    }

    //when input file is a regular file
	if(S_ISREG(src_info.st_mode)){
    	int status, read_status;
        if(read(sock_fd, &read_status,sizeof(int))!=sizeof(int)){
        	return 1;
        }
	    status = ntohl(read_status);
 
        //fork a child and make a new connection if server need copy file's data
        if(status==SENDFILE){
        	int r=fork();
		num_children += 1;
            if(r<0){
                perror("fork");
            }else if(r==0){
                sendfile.type = TRANSFILE;
                int private_fd = make_connection(host, port);

                if(send_request(private_fd,sendfile,origin_path)){
		    		fprintf(stderr, "send_request\n");
		    	}
                
                close(private_fd);
                exit(1);
            }

        }else if(status == OK){
            printf("%s already existed: SKIP\n",origin_path);
        }else{
        	//error happen in server side
        	fprintf(stderr, "%s: Premission Denied or Connection Error\n", origin_path);
        	return 0;
        }
    }	

    //when input is a dir
    if(S_ISDIR(src_info.st_mode)){
    	int status, read_status;
        read(sock_fd, &read_status,sizeof(status));
	status = ntohl(read_status);
        if(status==ERROR){
        	fprintf(stderr, "%s: Premission Denied or Connection Error\n", origin_path);
        	return 0;
        }

        //open a directory and read context in it
    	DIR *object = opendir(origin_path);
        if(object==NULL){
            int errsv = errno;
            //case when permission error raised
            if(errsv==EACCES){
                fprintf(stderr, "Cannot open DIRECTORY(%s) : Permission Denied\n", origin_path);
            }else{
                perror("opendir");
            } 
            return 1;
        }

        struct dirent *dir;
        while((dir = readdir(object))!= NULL){
            //skip hidden file
            if(dir->d_name[0]=='.') continue;

            //actual path and info of a file or directory in client side
            char actual_file_path[MAXPATH]={'\0'};
            int actual_file_path_len = snprintf(actual_file_path, sizeof(actual_file_path)-1, "%s/%s",origin_path, dir->d_name);
            actual_file_path[actual_file_path_len]='\0';
            
            struct stat actual_path_info;
            if(lstat(actual_file_path,&actual_path_info)<0){
                fprintf(stderr,"%s: No such file\n",actual_file_path);
                return 1;
            }

            if(S_ISLNK(actual_path_info.st_mode)){
                fprintf(stderr,"%s is a LINK: SKIP\n", actual_file_path);
                continue;
            }

            //path that send to server
            char file_path[MAXPATH]={'\0'};
            int file_path_len = snprintf(file_path, sizeof(file_path)-1, "%s/%s",source, dir->d_name);
            file_path[file_path_len]='\0';

            //recursively call to copy a ftree in server side
            if(copy_tree(file_path,host,port,sock_fd,actual_path_info)){
	            return 1;
	       	}
	    }   
    }
    return 0;
}

/*
  Copy files rooted at the given surce, to the server indicated by host and port.
*/
int rcopy_client(char *source, char *host, unsigned short port){
	//separate input path's prefix and filename
    char input_filename[MAXPATH]={'\0'};
    strncpy(input_filename,basename(source),strlen(basename(source)));
    int prefix_length = strlen(source)-strlen(input_filename);
    
    if(prefix_length>1){
        strncpy(dest_prefix,source,prefix_length-1);
        dest_prefix[prefix_length] = '\0';
    }else{
        dest_prefix[0] = '.';
        dest_prefix[1]= '\0';
    }

    //check existance of in put sorce file
    struct stat src_info;
    if(lstat(source,&src_info)<0){
        fprintf(stderr,"%s: No such file\n",source);
        return 1;
    }

    //reject the case when input path is a link
    if(S_ISLNK(src_info.st_mode)){
        fprintf(stderr, "REJECT LINK %s\n",source);
        return 1;
    }

    //make a connection to go through the tree
    int sock_fd = make_connection(host, port);
    if(sock_fd<0){
        fprintf(stderr, "Connection Error\n");
        return 1;
    }

    if(copy_tree(input_filename,host, port,sock_fd,src_info)){
    	//fprintf(stderr, "copy_tree\n");
    	return 1;
    }

    close(sock_fd);
    
    for (int i = 0; i < num_children; i ++){
	    int status;
	    if(wait(&status) == -1){
	    	perror("wait");
	        return 1;
	    }
    }
    return 0;
}

//////////////////////SERVER SIDE////////////////////////////

/*
  Check and return OK if the associated file of the given request exists on server, return sendfile if
  it does not exisit on server. 
*/ 
int checkexist(struct request *receivefile){
    char path[MAXPATH];
    int len0 = snprintf(path, sizeof(path)-1, "./%s", receivefile->path);
    path[len0] = '\0';

    struct stat localfile_stat;
    //check if such path exists in server side
    if(lstat(path,&localfile_stat)==0){
    	//check for both mode and size
        if((localfile_stat.st_mode == receivefile->mode) && (localfile_stat.st_size==receivefile->size)){
            //check for hash
            FILE *f = fopen(path, "rb");
            if(f==NULL){
                return ERROR;
            }

            char rehash[BLOCKSIZE] = {'\0'};
            strncpy(rehash,hash(f), BLOCKSIZE);

            int result=1;
            int i;
            for(i=0; i<BLOCKSIZE;i++){
                if(rehash[i]!=receivefile->hash[i]){
                    result = 0;
                }
            }

            if(result){
                fclose(f);
                return OK;
            }
            fclose(f);
        }else{
            if((localfile_stat.st_mode != receivefile->mode)){
                FILE *f = fopen(path, "rb");
                if(f==NULL){
                    return ERROR;
                }
            }
        }
    }
    return SENDFILE;
}

/*
  Accept a connction, and return a file descriptor that is created for communication with the client.
*/
int accept_connection(int fd, struct File *requests) {
    int index = 0;
    while (index < MAX_CONNECTIONS && requests[index].sock_fd != -1) {
        index++;
    }

    int client_fd = accept(fd, NULL, NULL);
    if (client_fd < 0) {
        perror("server: accept");
        close(fd);
        exit(1);
    }

    if (index == MAX_CONNECTIONS) {
        fprintf(stderr, "server: max concurrent connections\n");
        close(client_fd);
        return -1;
    }

    requests[index].sock_fd = client_fd;
    return client_fd;
}

/*
  Read a request step by step, terminated a connection when error occur or 
  client shut down the connection.
*/
int read_from(int client_index, struct File *requests) {
    int fd = requests[client_index].sock_fd;
    struct request *getfile = &(requests[client_index].receivefile);
    int current_step = requests[client_index].step;
    int read_in;

    //read transfer in data to different field base on current step
    if(current_step==AWAITING_TYPE){  
        if(read(fd, &read_in, sizeof(int)) != sizeof(int)) {
            requests[client_index].sock_fd = -1;
            return fd;
        }
	getfile->type = ntohl(read_in);
    }else if(current_step==AWAITING_PATH){
        if(read(fd, getfile->path, MAXPATH) != MAXPATH) {
            requests[client_index].sock_fd = -1;
            return fd;
        }
    }else if(current_step==AWAITING_SIZE){
        if(read(fd, &read_in, sizeof(int)) != sizeof(int)) {
        requests[client_index].sock_fd = -1;
            return fd;
        }
	getfile->size = ntohl(read_in);
    }else if(current_step==AWAITING_PERM){
        if(read(fd, &(getfile->mode), sizeof(mode_t)) != sizeof(mode_t)) {
            requests[client_index].sock_fd = -1;
            return fd;
        }
    }else if(current_step==AWAITING_HASH){
        if(read(fd, getfile->hash, BLOCKSIZE) != BLOCKSIZE) {
            requests[client_index].sock_fd = -1;
            return fd;
        }
    }else if(current_step==AWAITING_DATA){
        char buf[BUF_SIZE];

        char path[BUF_SIZE];
        int len0 = snprintf(path, sizeof(path)-1, "./%s", getfile->path);
        path[len0] = '\0';

        FILE* f = fopen(path,"wb");
        if(f==NULL){
            requests[client_index].sock_fd = -1;
            return fd;
        }

        while(read(fd, buf, 1)>0){
            fwrite(buf, sizeof(char), 1,f);
        }
        fclose(f);
        chmod(path,getfile->mode);
    }else{
        requests[client_index].sock_fd = -1;
        return fd;   
    }
    
    //move step after finish each read
    if(current_step<4){
        requests[client_index].step++;
        current_step++;
    }else if((getfile->type == TRANSFILE)&&(current_step==AWAITING_HASH)){
    	//case when need send file;s data
        requests[client_index].step++;
        current_step++;
    }else{
    	//after read all request's info, then initialize main client's step
        requests[client_index].step = AWAITING_TYPE;

        //check if file or directory exist
        int num = checkexist(&(requests[client_index].receivefile));
        
        //case when need making a new directory
        if((getfile->type==REGDIR)&&(num==SENDFILE)){
            char path[BUF_SIZE];
            int len0 = snprintf(path, sizeof(path)-1, "./%s", requests[client_index].receivefile.path);
            path[len0] = '\0';
            if(mkdir(path,requests[client_index].receivefile.mode)!=0){
                struct stat dir_info;
                if(lstat(path,&dir_info)<0){
                    fprintf(stderr, "%s not a VALID directory\n",path);
                    num = ERROR;
                }

                if(!S_ISDIR(dir_info.st_mode)){
                    perror("mkdir");
                    num = ERROR;
                }
            }
        }

        //return a state after get a complete request to client (OK, SENDFILE, ERROR)
        int write_to = htonl(num);
	if(write(fd, &write_to, sizeof(num)) != 4) {
            requests[client_index].sock_fd = -1;
            return fd;
        }

        return 0;
    }

    //return the step on which info in step is needed
    int write_to = htonl(current_step);
    if(write(fd, &write_to, sizeof(int))!=sizeof(int)){
        requests[client_index].sock_fd = -1;
        return fd;
    }
    return 0;
}

/*
  Create a server with the given port that can accepts mutiple client
*/
void rcopy_server(unsigned short port){
    //initialize all connection port
    struct File requests[MAX_CONNECTIONS];
    for (int index = 0; index < MAX_CONNECTIONS; index++) {
        requests[index].sock_fd = -1;
        requests[index].step = AWAITING_TYPE;
    }
    
    int sock_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (sock_fd < 0) {
        perror("server: socket");
        exit(1);
    }

    struct sockaddr_in server;
    server.sin_family = AF_INET;
    server.sin_port = htons(PORT);
    server.sin_addr.s_addr = INADDR_ANY;

    memset(&server.sin_zero, 0, 8);

    if (bind(sock_fd, (struct sockaddr *)&server, sizeof(server)) < 0) {
        perror("server: bind");
        close(sock_fd);
        exit(1);
    }

    if (listen(sock_fd, MAX_BACKLOG) < 0) {
        perror("server: listen");
        close(sock_fd);
        exit(1);
    }

    int max_fd = sock_fd;
    fd_set all_fds, listen_fds;
    FD_ZERO(&all_fds);
    FD_SET(sock_fd, &all_fds);

    //check for which connected client is ready and read from it
    while (1) {
        listen_fds = all_fds;
        int nready = select(max_fd + 1, &listen_fds, NULL, NULL, NULL);
        
        if (nready == -1) {
            perror("server: select");
            exit(1);
        }

        if (FD_ISSET(sock_fd, &listen_fds)) {
            int client_fd = accept_connection(sock_fd, requests);
            if (client_fd > max_fd) {
                max_fd = client_fd;
            }
            FD_SET(client_fd, &all_fds);
            printf("Accepted connection\n");
        }

        for (int index = 0; index < MAX_CONNECTIONS; index++) {
            if (requests[index].sock_fd > -1 && FD_ISSET(requests[index].sock_fd, &listen_fds)) {
                //read data from a single client
                int client_closed = read_from(index, requests);

                //case when client shut down connection
                if (client_closed > 0) {
                    FD_CLR(client_closed, &all_fds);
                    
                    if(requests[index].receivefile.type==TRANSFILE){
                        printf("Finish transferring file: ");
                    }else if(requests[index].receivefile.type==REGFILE){
                        printf("Finish checking FILE: ");
                    }else{
                        printf("Finish checking DIR: ");
                    }
                    printf("%s\n", requests[index].receivefile.path);
                }
            }
        }
    }
    // Should never get here!
    exit(1);
}
