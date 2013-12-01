
main: snake.ml main.ml
	ocamlc -thread unix.cma threads.cma graphics.cma snake.ml main.ml -o main
