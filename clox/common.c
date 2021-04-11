#include <stdlib.h>
#include <stdio.h>
#include "common.h"

void panic(char* message, int code) {
    printf("panic: %s", message);
    exit(code);
}
