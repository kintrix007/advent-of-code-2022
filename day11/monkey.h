#ifndef MONKEY_H
#define MONKEY_H

#include <stdio.h>
#include <stddef.h>

#define UNUSED(v) ((void)v)

typedef signed long long SLL;

enum Operation {
    ADD,
    MUL,
    SQUARE
};

struct Monkey {
    SLL items[50];
    size_t size;
    enum Operation op;
    int param;
    int testDiv;
    int trueTarget;
    int falseTarget;
};

struct Monkey *parseMonkey(FILE *file);

struct Monkey **parse(char *filename, size_t *size);

int solve(struct Monkey **monkeys, size_t size, int iteration, int shouldDivide);

int inspectItem(struct Monkey *m, SLL *item, int shouldDivide);

#endif