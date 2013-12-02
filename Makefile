
main: snake.ml main.ml
	ocamlc graphics.cma util.ml snake.ml main.ml -o main

clear:
	rm *.swp *.cmi *.cma *.cmx *.o
