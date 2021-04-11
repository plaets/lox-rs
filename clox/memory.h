#ifndef clox_memory_h
#define clox_memory_h

#include "common.h"

#define GROW_FACTOR 2

#define GROW_CAPACITY(capacity) \
    ((capacity) < 8 ? 8 : (capacity) * GROW_FACTOR)

#define GROW_ARRAY(type, pointer, oldCount, newCount) \
    (type*)reallocate(pointer, sizeof(type) * (oldCount), \
            sizeof(type) * (newCount))

#define FREE_ARRAY(type, pointer, oldCount) \
    reallocate(pointer, sizeof(type) * (oldCount), 0)

void* reallocate(void* pointer, size_t oldSize, size_t newSize);

#endif
