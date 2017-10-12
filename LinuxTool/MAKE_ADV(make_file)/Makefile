FLAGS = -Wall -g

make_file : make_file.o 
	gcc ${FLAGS} -o $@ $^

%.o : %.c 
	gcc ${FLAGS} -c $<

clean :
	rm *.o make_file
