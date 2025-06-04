(*ocamlfind ocamlopt -o code -linkpkg -package graphics scale.ml *)

let create_window n1 n2 =
  (*      
    Entrée : Deux entiers n1 et n2 
    Sortie : Une fenêtre de taille 'n1'x'n2' d'un type bizarre;
    Effet de bord : Affiche dans le terminal la taille de la fenêtre créée 
  *) 

    let ssize = Printf.sprintf " %dx%d" n1 n2 in
    Printf.printf " Fenêtre de taille %dx%d pixels" n1 n2; Printf.printf "\n\n";
    Graphics.open_graph ssize

let wait_any_touch () =
  (*  
    Entrée : Rien
    Sortie : Rien
    Effet de bord : Attends qu'une touche soit pressée
  *)

  Graphics.wait_next_event [Key_pressed]


let scale_color min max x = match x with
  | y when x < min -> 0x000000
  | y when x > max -> 0xFFFFFF
  | y -> let a = 255*(y-min)/(max-min) in Graphics.rgb    (a+255) 0 (a)

let draw sequence zoom n1 n2 = 
  
  for i = 0 to (n1-1) do
    for j = 0 to (n2-1) do 
      print_int sequence.(i*n1 + j);
      Graphics.set_color (scale_color 0 1 sequence.(i*n1 + j));
      for k = 0 to (zoom-1) do
        for l = 0 to (zoom -1) do
          Graphics.plot ((j*zoom +l)) (n1*zoom - (i*zoom + k))
        done;
      done;
    
    done;
  done

let condition1 x y k = 
  if (x,y) = (0,0) then 0 else 
  (
  if Perso.suite_mem (9999*x+990*y) = Infinombre then -1 else 10
  )

let condition2 x y k = 
  if (x,y) = (0,0) then -1 else 
  (
    let u = Perso.suite_temps_vol (999*x+90*y) in u
  )

let condition3 x y k = match (x,y) with 
  | (a,b) when a=0 && b=0 -> 0
  | (a,b) when a=(-b) -> 1
  | (a,b) when b = 0 -> 2 
  | (a,b) -> if x+y mod 11 < 5 && x+y mod 11 > 0   then -1 else 10

let create_sequence x y k = 
  let sequence = Array.make (19*19) 0 in 
  for i=(-9) to 9 do
    for j=(-9) to 9 do
      sequence.(18*(i+9) + (j+9)) <- condition1 i j k
    done;
  done;
  sequence

let sequences_to_int s n = 
  let t =  Array.make n (0) in 
  for i=0 to (n-1) do 
    t.(i) <- ((int_of_char s.[i]) - 49)
  done;
  t

let create_larger_sequence r x y k = 
  let sequence = Array.make ((2*r+1)*(2*r+1)) 0 in 
  for i=(-r) to r do
    for j=(-r) to r do
      sequence.(2*r*(i+r) + (j+r)) <- condition1 i j k
    done;
  done;
  sequence

(*
let main () = 
  create_window 1000 1000;
  let sequence = [|1;0;1;1;1;0;0;1;0;1;0;1;1;1;1;1;0;0;0;0;0;0;0;0;0;0;0;1;1;1;0;0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;0;0;0;0;0;1;1;1;0;1;1;1;1;0;0;0;1;0;1;1;1;1;1;1;1;1;0;0;0;1;0;1;0;1;0;1;0;1;0;1;0;1;0;1;0;1;0;1;0;1;0;1;0;1;0;1;0;1;0;1;0;1;0;1;0;1;0;1;0;1;0;1;0;1;1;1;1;1;1;1;1;1;1;1;1;1;0;0;0;0;0|] in 
  for i=0 to 100 do
    sequence.(i) <- i mod 10
  done;

  let r = 20 in 
  let s2 = create_larger_sequence r 1 1 11 in 

  
  draw s2 15 (2*r) (2*r);
  wait_any_touch ();
  print_string ""


let () = main ()
*)