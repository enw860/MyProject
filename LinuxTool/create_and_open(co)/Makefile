FLAGS = -Wall -g

prog : create_and_open.o 
	gcc ${FLAGS} -o $@ $^

%.o : %.c 
	gcc ${FLAGS} -c $<

clean :
	rm *.o prog
