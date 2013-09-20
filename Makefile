CFLAGS=-std=gnu99 -g -Wall

.PHONY: clean test

minilisp: minilisp.c

clean:
	rm -f minilisp *~

test: minilisp
	@./test.sh
