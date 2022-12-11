#ifndef MONKEY_H
#define MONKEY_H

#include <stdio.h>
#include <stddef.h>

#define UNUSED(v) (void)v

enum Operation {
    ADD,
    MUL,
    SQUARE
};

struct Monkey {
    int items[50];
    size_t itemCount;
    enum Operation op;
    int param;
    int testDiv;
    int trueTarget;
    int falseTarget;
};

struct Monkey *parseMonkey(FILE *file);
struct Monkey **parse(char *filename, size_t *size);

#endif