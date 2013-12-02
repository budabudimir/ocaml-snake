
main: snake.ml main.ml
	ocamlc graphics.cma util.ml snake.ml main.ml -o main

clear:
	rm -f *.swp *.cmi *.cma *.cmx *.cmo *.o
