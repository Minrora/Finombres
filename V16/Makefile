.PHONY: all, clean

PERSO := Perso.ml
GRAGEN := graphgenerator.ml
PIXEL := pixelure.ml
WRIT := writer.ml
PLAN := plan.ml
MAIN := main.ml
GPHIC := graphicgenerator.ml
SCALE := scale.ml

NAME_EXE := code


fruit:
		ocamlfind ocamlopt -linkpkg -package graphics $(GPHIC) $(WRIT) $(PERSO)  $(PIXEL) $(PLAN) $(GRAGEN) $(SCALE) $(MAIN) -o $(NAME_EXE) 
		./$(NAME_EXE) fruit

kpalintiple: 
		ocamlfind ocamlopt -linkpkg -package graphics $(GPHIC) $(WRIT) $(PERSO)  $(PIXEL) $(PLAN) $(GRAGEN) $(SCALE) $(MAIN) -o $(NAME_EXE) 
		./$(NAME_EXE) kpalintiple 9

graphic:
		ocamlfind ocamlopt -linkpkg -package graphics $(GPHIC) $(WRIT) $(PERSO)  $(PIXEL) $(PLAN) $(GRAGEN) $(SCALE) $(MAIN) -o $(NAME_EXE) 
		./$(NAME_EXE) graphic

find_nature:
		ocamlfind ocamlopt -linkpkg -package graphics $(GPHIC) $(WRIT) $(PERSO)  $(PIXEL) $(PLAN) $(GRAGEN) $(SCALE) $(MAIN) -o $(NAME_EXE) 
		./$(NAME_EXE) find_nature 10000000


find_vol:
		ocamlfind ocamlopt -linkpkg -package graphics $(GPHIC) $(WRIT) $(PERSO)  $(PIXEL) $(PLAN) $(GRAGEN) $(SCALE) $(MAIN) -o $(NAME_EXE) 
		./$(NAME_EXE) find_vol 1000000

plan:
		ocamlfind ocamlopt -linkpkg -package graphics $(GPHIC) $(WRIT) $(PERSO)  $(PIXEL) $(PLAN) $(GRAGEN) $(SCALE) $(MAIN) -o $(NAME_EXE) 
		./$(NAME_EXE) plan 

grid_vol:
		ocamlfind ocamlopt -linkpkg -package graphics $(GPHIC) $(WRIT) $(PERSO)  $(PIXEL) $(PLAN) $(GRAGEN) $(SCALE) $(MAIN) -o $(NAME_EXE) 
		./$(NAME_EXE) grid_vol 800 800 
pixel:
		ocamlfind ocamlopt -linkpkg -package graphics $(GPHIC) $(WRIT) $(PERSO)  $(PIXEL) $(PLAN) $(GRAGEN) $(SCALE) $(MAIN) -o $(NAME_EXE) 
		./$(NAME_EXE) pixel 100000 
		
image_random:
		ocamlfind ocamlopt -linkpkg -package graphics $(GPHIC) $(WRIT) $(PERSO)  $(PIXEL) $(PLAN) $(GRAGEN) $(SCALE) $(MAIN) -o $(NAME_EXE) 
		./$(NAME_EXE) image_random 

show_them:
		ocamlfind ocamlopt -linkpkg -package graphics $(GPHIC) $(WRIT) $(PERSO)  $(PIXEL) $(PLAN) $(GRAGEN) $(SCALE) $(MAIN) -o $(NAME_EXE) 
		./$(NAME_EXE) show_them 

nature_finder_from:
		ocamlfind ocamlopt -linkpkg -package graphics $(GPHIC) $(WRIT) $(PERSO)  $(PIXEL) $(PLAN) $(GRAGEN) $(SCALE) $(MAIN) -o $(NAME_EXE) 
		./$(NAME_EXE) nature_finder_from 

clean:
		rm -f *.cmx *.cmi *.o nfa.cmo *.out $(MAIN_EXE)
