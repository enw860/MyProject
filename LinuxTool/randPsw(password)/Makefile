FLAGS = -Wall -g

passWord : randPassword.o
	gcc ${FLAGS} -o $@ $^

%.o : %.c 
	gcc ${FLAGS} -c $<

clean :
	rm *.o passWord
