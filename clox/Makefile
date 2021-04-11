all: main.c
	cppcheck --bug-hunting --enable=all --suppress=missingIncludeSystem -I . .
	gcc -Wall -g -o target/clox -I . main.c common.c memory.c chunk.c debug.c value.c

