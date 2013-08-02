CFLAGS=-std=gnu99 -g

.PHONY: clean

minilisp: minilisp.c

clean:
	rm -f minilisp *~

test: minilisp
	@./test.sh
