APP = app.out

.PHONY: run clean

$(APP): main.s
	gcc -no-pie -g -o $@ $^

run:
	@make && \
	./$(APP)

clean:
	rm $(APP)
