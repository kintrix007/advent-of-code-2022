app = app.out
CFLAGS = -std=c11 -no-pie -ggdb3 -Wall -Wextra -Werror -Wpedantic -Wnull-dereference

.PHONY: run clean valgrind

$(app): main.c
	gcc $(CFLAGS) -o $@ $^

run:
	@make
	@echo "---"
	./$(app)

clean:
	-rm $(app)
