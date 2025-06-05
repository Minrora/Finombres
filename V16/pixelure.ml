(*ocamlfind ocamlopt -o code -linkpkg -package graphics pixelure.ml *)

(* Construit l'image représentant les Finombres et les Infinombres 
  Sommaire : 
  (*ocamlfind ocamlopt -o code -linkpkg -package graphics pixelure.ml *)

  Les fonctions : 
  
    sequences : 
      Entrée : un chemin 'source' vers un fichier
      Sortie : Une chaine de caractères 's' représentant la première ligne du fichier source 
      Commentaire : La ligne en sortie est la séquence des Finombres et Infinombres 
      
    create_window : 
      Entrée : Deux entiers n1 et n2 
      Sortie : Une fenêtre de taille 'n1'x'n2' d'un type bizarre
      Effet de bord : Affiche dans le terminal la taille de la fenêtre créée 

    wait_any_touch :
      Entrée : Rien
      Sortie : Rien
      Effet de bord : Attends qu'une touche soit pressée

    generate : 
      Entrée : Une chaine de caractère 'sequence', un entier 'base' et un entier 'n' 
      Sortie : Rien 
      Effet de bord : Dessine l'image des Finombres et des Infinombres de taille (base*n/base)

    generate_decal : 
      Entrée : Une chaine de caractère 'sequence', un entier 'base' et un entier 'n' 
      Sortie : Rien 
      Effet de bord : Dessine l'image des Finombres et des Infinombres de taille (base*n/base) avec un décalage arbitraire tous les 10000
      
    picture : 
      Entrée : Un triplet d'entier (number_of_pict,start_x,step_x)
      Sortie : Rien
      Effet de bord : Affiche successivement 'number_of_picture' images de taille n global des Finombres, (avec generate) 
                      en changeant la largeur de l'image, commençant d'une largeur 'start_x' et en y ajoutant 'step_x'

    picture_decal : 
      Entrée : Un triplet d'entier (number_of_pict,start_x,step_x)
      Sortie : Rien
      Effet de bord : Affiche successivement 'number_of_picture' images de taille n global 
                      des Finombres avec le décalage (avec generate_decal) 
                      en changeant la largeur de l'image, commençant d'une largeur 'start_x' et en y ajoutant 'step_x'
      
    affiche_exception :
      Entrée : Rien
      Sortie : Rien
      Effet de bord : Affiche tous les Finombres multiples de 11 entre 1000 et 10000 (renvoie Finombres/11)

    k_palintiples : 
      Entrée : Un entier k et un entier n
      Sortie : Rien
      Effet de bord : Affiche 10 images des les k-palintuples inferieur à n
  *)

let sequences source = 
  (*
    Entrée : un chemin 'source' vers un fichier
    Sortie : Une chaine de caractères 's' représentant la première ligne du fichier source 
    Commentaire : La ligne en sortie est la séquence des Finombres et Infinombres 
  *)
  let da = open_in source in 
  let s = input_line da in
  close_in da;
  s

let create_window n1 n2 =
  (*      
    Entrée : Deux entiers n1 et n2 
    Sortie : Une fenêtre de taille 'n1'x'n2' d'un type bizarre
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

  Printf.printf "%d,%d" n (String.length sequence);
  assert (n < String.length sequence); (*0*)
  create_window (base - 1) (n/base + 1); (*1*)
  for i=0 to (n-1) do 
    let (x,y) = (((i) mod base),n/base -((i) / base)) in (*2*)
    Graphics.set_color (if sequence.[i] = '0' then 0xFFFFFF else 0x000000); (*3*)
    Graphics.plot x y (*4*)
  done

let generate_decal sequence base n= 

  (*
    Entrée : Une chaine de caractère 'sequence', un entier 'base' et un entier 'n' 
    Sortie : Rien 
    Effet de bord : Dessine l'image des Finombres et des Infinombres de taille (base*n/base) avec un décalage arbitraire tous les 10000

  Sommaire : 
    (0) On vérifie que la longueur de la séquence est plus grande que la taille 'n' donnée
    (1) On crée la fenêtre de taille x*y telle que la largeur x est de taille base et x*y = n (donc y ~ n/base)
    (2) On génère des couples (x,y)
      a- Pour les 10000 premiers nombres, on fait simplement (i%base,i//base)
      b- Puis tous les 10000, on créé un décalage pour aligner les colonnes (mystérieux)
    (3) On change la couleur en fonction de la nature du nombre i (Finombre|Infinombre) -> (Blanc|Noir)
    (4) On affiche le nombre i à la position (x,y) avec la couleur choisi
  *)
  assert (n < String.length sequence); (*   (0)   *)
  create_window (base - 1) (n/base + 1); (*   (1)   *)

  (*   (2)   *)
  for i=0 to (n-1) do 
    let (x,y) = 
    if i < 10000 then 
      (*   (a)   *)
      ((i mod base),(i / base)) 
    else 
      (*   (b)   *)
      (let decal = ((i-10000) / 1000) in 
      let i2 = (i+100-10*(decal-1)) in
      (*Printf.printf "Decal : %d\n" decal;*)  
      (((i2) mod base),((i2) / base) + decal) ) 
    in
    Graphics.set_color (if sequence.[i] = '0' then 0xFFFFFF else 0x000000); (*    (3)    *)
    Graphics.plot x y (*   (4)   *)
  done

let data = "data.csv"
let step = 1
(* (start,step,max-step) *)
let triple = (50,50,1000)

(* start interessants : 
20,24,25,30,33,40,45,50,60,10,101,110,120,201,202,300,303,555,606,655,707,777,808,810,1010,1110, 2020
*)

let picture s n max_step start_x step_x=
  (*
    Entrée : La sequence s, l'entier n, Un triplet d'entier (number_of_pict,start_x,step_x)
    Sortie : Rien
    Effet de bord : Affiche successivement 'number_of_picture' images de taille n global des Finombres, (avec generate) 
                    en changeant la largeur de l'image, commençant d'une largeur 'start_x' et en y ajoutant 'step_x'
  *) 
  for image_i = 0 to max_step do
    let j = start_x + image_i*step_x in 
    Printf.printf "Pas de : %d\n" (j);
    flush stdout;
    generate s (j) (n-1); 
    wait_any_touch ();
    Graphics.close_graph ()
  done

let picture_decal n number_of_pict start_x step_x= 
  (*
    Entrée : Un triplet d'entier (number_of_pict,start_x,step_x)
    Sortie : Rien
    Effet de bord : Affiche successivement 'number_of_picture' images de taille n global 
                    des Finombres avec le décalage (avec generate_decal) 
                    en changeant la largeur de l'image, commençant d'une largeur 'start_x' et en y ajoutant 'step_x'
  *)
  let s = sequences data in 
  for i = 0 to number_of_pict do
    let j = start_x + i*step_x in 
    Printf.printf "Pas de : %d\n" (j);
    flush stdout;
    generate_decal s (j) n; 
    wait_any_touch ();
    Graphics.close_graph ()
  done

let affiche_exception () =  
  (*
    Entrée : Rien
    Sortie : Rien
    Effet de bord : Affiche tous les Finombres multiples de 11 entre 1000 et 10000 (renvoie Finombres/11)
  *)
  let s = sequences data in 
  for i = 0 to 819 do 
    if s.[1001 + 11*i] =  '0' then Printf.printf "%d," (i);
  done

let k_palintiples k n n_image base_x step_x = 
  (* 
    Entrée : Un entier k et un entier n, un entier n_image, un entier base_x, un entier stap_x
    Sortie : Rien
    Effet de bord : Affiche 10 images des les k-palintuples inferieur à n

  Principe : 
    - On crée une chaine de caractères de taille n composée uniquement de 1 (en Bytes pour l'aspet mutable)
    - On verifie que rev(i*k) = k*rev(i) (i est un k-palintiple)
    - 0 si oui, 1 sinon
    - On affiche 10 images des k-palintiples
  *)
  let s = Bytes.make n '1' in 
  for i=0 to (n-1) do 
    if (Perso.revV1(i*k)/k = Perso.revV1(i)) then Bytes.set s i '0';
  done;
  Printf.printf "%s\n%d\n" (Bytes.to_string s) (String.length (Bytes.to_string s)) ;
  picture (Bytes.to_string s) n (n_image) base_x 10

let repartition n b =
  let s = sequences data in 
  let repart =  Array.make n 0 in 
  let inf_fin = if b then '1' else '0' in 

  repart.(0) <- (if (inf_fin = s.[0]) then 1 else 0);
  for i=1 to (n-1)do
    repart.(i) <- repart.(i-1) + (if (inf_fin = s.[i]) then 1 else 0)
  done;
  (*Array.iter (fun x -> Printf.printf "%d,"x) repart;*)
  repart


let array_integer n = 
  Array.init n (fun x -> x)


let plan_number_n_digits n = 
  let square_size = 1000 in 
  let (a,b) = (((Perso.pow 10 n) - 1),(Perso.pow 10 (n-1) - (Perso.pow 10 1))) in
  for i=(-square_size) to square_size do
    for j=(-square_size) to square_size do
      let nature = Perso.suite_mem (i*a+j*b) in 
      let color = (if nature = Finombre then 0x00EE00 else 0x0000EE) in 
      Graphics.set_color color;
      Graphics.plot (i+square_size) (j+square_size)
    done; 
  done

let scale_color min max x = match x with
  | y when x < min -> 0x000000
  | y when x > max -> 0xFFFFFF
  | y -> let a = 255*(y-min)/(max-min) in Graphics.rgb  (255-a) a 0

let draw sequence zoom n1 n2 = 
  for i = 0 to (n1-1) do
    for j = 0 to (n2-1) do 
      (* Printf.printf "%d " (scale_color 1 9 (int_of_char(sequence.[i*n1 + j])-48)); *)
      Graphics.set_color (scale_color 1 9 (int_of_char(sequence.[i*n1 + j])-48));
      for k = 0 to (zoom-1) do
        for l = 0 to (zoom -1) do
          Graphics.plot  (n2*zoom - (j*zoom +l)) (n1*zoom - (i*zoom + k))
        done;
      done;
    
    done;
  done

let draw_grid_vol n1 n2 = 
  let sequence = sequences "data_vol.csv" in 
  create_window n1 n2;
  let a = draw sequence 2 200 200 in 
  wait_any_touch ();
  ()

let count_nature pas n = 
  (*
    Entrées : Deux entiers pas et n
    Sortie : Rien
    Effets de bord : Inscription de la nature des nombres de 0 à n dans le fichier data 
                      et affichage tous les 'pas' de la quantité de Finombres et d'Infinombres
    Commentaire : Le 'pas' permet de contrôler le nombre d'affichage

  Sommaire : 
    (0) Initialisation des compteurs de Finombres et d'Infinombres
    (1) Initialisation du tableau nature de taille 'pas'
    (2) Pour les nombres i de 0 à n
      -a On calcule la nature du nombre i
      -b Si c'est un Finombre, alors on augmente le compteur de Finombres
         Sinon on augment le compteur des Infinombre
         
      -c Si on a atteint le 'pas', on affiche le nombre de Finombres et d'Infinombres
  *)
  
  (*0*)
  let countFi = ref 0 in 
  let countInf = ref 0 in 

  (*1*)
  let seq = sequences data in  
  
  (*2*)
  for i = 0 to (n) do
    if seq.[i] = '0'
      (*a*)
      then 
        (incr countFi)
      (*b*)
      else 
        (incr countInf);
    (*c*)
    if (i mod pas = 0 && i <> 0 )
      then(
        Printf.printf "i : %d, Finombre : %d, Infinombre : %d\n" i !countFi !countInf; )
  done;
  print_newline ()

  
  (*
let main () = 
  k_palintiples 9 (n)


  
let () = main ()*)
