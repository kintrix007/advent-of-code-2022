#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "monkey.h"

int main() {
    size_t size;
    struct Monkey **monkeys = parse("input", &size);
    
    int p1 = part1(monkeys);
    int p2 = part2(monkeys);
    printf("Part 1: %d\nPart 2: %d\n", p1, p2);

    for (size_t i = 0; i < size; i++) {
        free(monkeys[i]);
    }
    free(monkeys);
}

int part1(struct Monkey **monkeys) {
    UNUSED(monkeys);
    return -1;
}

int part2(struct Monkey **monkeys) {
    UNUSED(monkeys);
    return -1;
}

struct Monkey **parse(char *filename, size_t *size) {
    FILE *file = fopen(filename, "r");
    struct Monkey *m = NULL;
    struct Monkey *monkeyBuf[50];

    int n = 0;
    while (1) {
        m = parseMonkey(file);
        if (m == NULL) break;
        monkeyBuf[n] = m;
        n++;
    }

    struct Monkey **monkeys = malloc(n * sizeof(struct Monkey *));
    for (int i = 0; i < n; i++) monkeys[i] = monkeyBuf[i];
    *size = n;

    fclose(file);

    return monkeys;
}

struct Monkey *parseMonkey(FILE *file) {
    struct Monkey *monkey = NULL;
    int monkeyNum = -1;

    fscanf(file, "Monkey %d:\n", &monkeyNum);
    if (monkeyNum == -1) return NULL;
    fscanf(file, "  Starting items:");

    monkey = malloc(sizeof(struct Monkey));

    monkey->itemCount = 0;
    char term = ',';
    // printf("Items: ");
    for (int i = 0; term != '\n'; i++) {
        fscanf(file, " %d%c", monkey->items + i, &term);
        monkey->itemCount++;
        // printf("%d ", monkey->items[i]);
    }
    // printf("\nCount: %lu\n", monkey->itemCount);

    char op;
    char buf[10];
    fscanf(file, "  Operation: new = old %c %s\n", &op, buf);
    if (op == '*' && strcmp(buf, "old") == 0) {
        monkey->op = SQUARE;
        monkey->param = -1;
    } else {
        sscanf(buf, "%d", &monkey->param);
        switch (op) {
            case '+': monkey->op = ADD; break;
            case '*': monkey->op = MUL; break;
            default: printf("ERROR: unexpected operation '%c'", op);
        }
    }
    // printf("Op: %d, Param: %d\n", monkey->op, monkey->param);

    fscanf(file, "  Test: divisible by %d\n", &monkey->testDiv);
    // printf("Div: %d\n", monkey->testDiv);

    fscanf(file, "    If true: throw to monkey %d\n", &monkey->trueTarget);
    fscanf(file, "    If false: throw to monkey %d\n", &monkey->falseTarget);
    fscanf(file, "\n");
    // printf("true: %d, false: %d\n", monkey->trueTarget, monkey->falseTarget);
    // printf("--- MONKEY END ---\n");

    return monkey;
}
