#include <stdlib.h>
#include "common.h"
#include "memory.h"

void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
    if(newSize == 0) {
        free(pointer);
        return NULL;
    }

    void* result = realloc(pointer, newSize);
    if (result == NULL) panic("failed to allocate memory", 1);
    return result;
}
