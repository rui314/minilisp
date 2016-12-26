CFLAGS=-std=gnu99 -g -O2 -Wall
PREFIX=/usr/local

.PHONY: clean test

minilisp: minilisp.c

install: minilisp
	install minilisp $(PREFIX)/bin

clean:
	rm -f minilisp *~

test: minilisp
	@./test.sh
