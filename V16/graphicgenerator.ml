
let get_max_array a = 
  Array.fold_left max 0 a
  
  
let change_background color n1 n2 = 
  let cinit = Graphics.foreground in
  Graphics.set_color color;
  Graphics.fill_rect 0 0 n1 n2;
  Graphics.set_color cinit

let create_window n1 n2 =
  (*      
    Entrée : Deux entiers n1 et n2 
    Sortie : Une fenêtre de taille 'n1'x'n2' d'un type bizarre
    Effet de bord : Affiche dans le terminal la taille de la fenêtre créée 
  *) 

  let size = Printf.sprintf " %dx%d" n1 n2 in
  Printf.printf " Fenêtre de taille %dx%d pixels" n1 n2; Printf.printf "\n\n";
  Graphics.open_graph size
  


  let wait_any_touch () =
    (*  
    Entrée : Rien
    Sortie : Rien
    Effet de bord : Attends qu'une touche soit pressée
    *)
    
    Graphics.wait_next_event [Key_pressed]
    
let close_window () = 
  wait_any_touch ();
  Graphics.close_graph ()

let generate sequence base n= 
  (*
    Entrée : Une chaine de caractère 'sequence', un entier 'base' et un entier 'n' 
    Sortie : Rien 
    Effet de bord : Dessine l'image des Finombres et des Infinombres de taille (base*n/base)

  Sommaire : 
    (0) On vérifie que la longueur de la séquence est plus grande que la taille 'n' donnée
    (1) On crée la fenêtre de taille x*y telle que la largeur x est de taille base et x*y = n (donc y ~ n/base)
    (2) On génère des couples (x,y) pour le nombre i = (i%base,i//base)
    (3) On change la couleur en fonction de la nature du nombre i (Finombre|Infinombre) -> (Blanc|Noir)
    (4) On affiche le nombre i à la position (x,y) avec la couleur choisi
  *)

  (*Printf.printf "%d,%d" n (String.length sequence);*)
  assert (n < String.length sequence); (*0*)
  create_window (base - 1) (n/base + 1); (*1*)
  for i=0 to (n-1) do 
    let (x,y) = (((i) mod base),((i) / base)) in (*2*)
    Graphics.set_color (if sequence.[i] = '0' then 0xFFFFFF else 0x000000); (*3*)
    Graphics.plot x y (*4*)
  done


let create_graphic tab_x tab_y color = 
  let x_max = get_max_array tab_x in 
  let y_max = get_max_array tab_y in 
  let n = Array.length tab_x in
  let size_x = 700 in 
  let size_y = 700 in 
  assert (n = Array.length tab_y);
  (*create_window (size_x) (size_y);*)

  for i=0 to (n-1) do 
    let (x,y) = (((tab_x.(i)*size_x)/x_max),(tab_y.(i)*size_y)/y_max) in 
    (*Printf.printf "(%d,%d) " x y;*)
    Graphics.set_color color;
    Graphics.plot y x;
  done
  (*wait_any_touch ();
  Graphics.close_graph ()*)


let create_origin size_x size_y = 
  create_window (size_x) (size_y);

  for i=0 to (size_x-1) do
    for j=0 to (size_y - 1)do 
    let (x,y) = (size_x-i-1,j) in 
    Graphics.set_color (0x0000EE*i*j);
    Graphics.plot y x;
    done;
    (*Printf.printf "(%d,%d) " x y;*)
  done;
  close_window ()

let create_origin2 size_x size_y = 
  create_window (size_x) (size_y);

  for i=0 to (100) do
    for j=0 to (200)do 
    let color = ref 0x000010 in
    let (x,y) = (size_x-i-1 + 100*(i/100),j) in 
    Graphics.set_color (!color*i*j);
    Graphics.plot y x;
    if(i mod 100 = 0) then color := !color + 2000;
    
    done;
    (*Printf.printf "(%d,%d) " x y;*)
  done;
  close_window ()
(*let test = 
  
  create_origin2 1000 1000*)
  (*
let ()) = create_graphic [|0;1;2;3;4;5;6|]  [|0;1;2;3;4;5;6|]*)