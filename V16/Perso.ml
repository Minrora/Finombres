(* ocamlopt -o code Perso.ml writer.ml *)

(* Voici le fichier des fonctions de bases utiles au TIPE sur les Finombres.
Sommaire : 

Les types : 
- Nature : Finombre | Infinombre


Les Variables globales : 

data : data.csv, le fichier contenant les données en binaires de la nature des nombres
temp : temp.csv, permet d'écrire dans data.csv sans écraser les données

Les fonctions : 
    
    
  revV1 :
    Entrées : Un entier x en base 10
    Sortie : L'entier renversé par la division euclidienne
    
  rev_subV1 : 
    Entrées : Un entier x en base 10
    Sortie : x - rev(x) (rev par la division euclidienne) 
    
  suite : 
    Entrées : Un entier x0 en base 10
    Sortie : Rien
    Effet de bord : Affiche les termes de la suite Un+1 = rev_sub(Un) avec U0 = x0
    Commentaire : S'arrête si on tombe sur 0 ou si on a fait 1000 itérations
    
  suite_dyn :
    Entrée : Un entier n0
    Sortie : ??? (Un tableau représentant la suite ?)
    Commentaire : Un parcours ?
    
  suite_mem :
    Entrée : Un entier n0 
    Sortie : La nature du nombre (Finombre | Infinombre)
    Commentaire : Par mémoïsation, on utilise une Hashtable pour retenir lesquels on a déjà vu
    
  int_to_array : 
    Entrées : Un entier n et un entier len 
    Sortie : Un tableau n1 de taille len, telle que n1.(i) est le i-ème chiffre en lisant n par la droite
    Commentaire : Divisions euclidiennes successives
    
  print_nature : 
    Entrée : Un élément de type nature
    Sortie : Rien
    Effet de bord : Affiche la nature dans le terminal
    
  pow : 
    Entrées : Un entier n et un entier n
    Sortie : L'entier n exposant p
    Commentaire : Exponentiation modulaire
    
  nature_finder : 
    Entrées : Deux entiers pas et n
    Sortie : Rien
    Effets de bord : Inscription de la nature des nombres de 0 à n dans le fichier data 
                     et affichage tous les 'pas' de la quantité de Finombres et d'Infinombres
    Commentaire : Le 'pas' permet de contrôler le nombre de mise à jour du fichier

  nature_counter_special :
    Entrées : Trois entiers n, a et b
    Sortie : Rien
    Effet de bord : Determine la nature des n premiers nombres de la forme a*k + b et l'affiche leur quantité
 
    *)
  
type nature = Finombre | Infinombre
let data = "data.csv"
let temp_file = "temp.csv"
  
let data_vol = "data_vol.csv"
let temp_vol_file = "temp_vol_file.csv"

(*
=============== Les Fonctions =================
*)


let revV1 n = 
  (*
  Entrée : Un entier n en base 10
  Sortie : L'entier renversé par la division euclidienne
  *)
  
  let renverse = ref 0 in 
  let temp = ref n in 

  while (!temp <> 0) do 
    let chiffre = !temp mod 10 in 
    renverse := !renverse * 10 + chiffre;
    temp := (!temp - chiffre)/10
  done;
  !renverse

let rev_b n b = 
  (*
  Entrée : Un entier n et un entier b (la base)
  Sortie : L'entier renversé par la division euclidienne
  *)
  
  let renverse = ref 0 in 
  let temp = ref n in 

  while (!temp <> 0) do 
    let chiffre = !temp mod b in 
    renverse := !renverse * b + chiffre;
    temp := (!temp - chiffre)/b
  done;
  !renverse

let rev_subV1 x = 
  (*
    Entrée : Un entier x en base 10
    Sortie : |x - rev(x)| (rev par la division euclidienne) 
  *)

  abs (x - revV1 x)

let suite x0 = 
  (*
    Entrée : Un entier x0 en base 10
    Sortie : Rien
    Effet de bord : Affiche les termes de la suite Un+1 = rev_sub(Un) avec U0 = x0
    Commentaire : S'arrête si on tombe sur 0 ou si on a fait 1000 itérations
  *)

  let s = ref x0 in 
  let nb_step = ref 0 in
  while (!s <> 0 && !nb_step < 1000) do 
    Printf.printf "%d " (!s);
    s := rev_subV1 (!s);
    nb_step := !nb_step + 1
  done;
  Printf.printf "%d " (!s)

(*
let suite_dyn n0  =
  (*
    Entrée : Un entier n0
    Sortie : ??? (Un tableau représentant la suite ?)
    Commentaire : Un parcours ?
  *)

  let deja_vu = Array.make (n0+1) false in 
  let tab_value = Array.make (n0+1) (-1) in 
  
  let rec aux n step= 
    if tab_value.(n) < 0 
      then 
        (if deja_vu.(n) 
          then 
            (tab_value.(n) <- Int.min_int; Int.min_int) 
          else 
            tab_value.(n) <- ((aux (rev_subV1 n) (step + 1)) + step )
        ) else (-1);

  in
  
  for i=0 to n0 do 
    aux i 0
  done;
  tab_value*)

let suite_mem n0 = 
  (* 
    Entrée : Un entier n0 
    Sortie : La nature du nombre (Finombre | Infinombre)
    Commentaire : Par mémoïsation, on utilise une Hashtable pour retenir lesquels on a déjà vu
  *)

  let deja_vu = Hashtbl.create 20 in 

  let rec aux n step = 
  try 
    let _ = Hashtbl.find deja_vu n in
    Infinombre
  with Not_found -> 
    Hashtbl.replace deja_vu n step;
    let next = rev_subV1 n in 
    if next = 0 then Finombre else aux next (step + 1)

in aux n0 0

let suite_temps_vol n0 = 
  (* 
    Entrée : Un entier n0 
    Sortie : Le nombre d'itération (maximum 9) avant qu'un nombre atteingne son cycle (0 si Infinombre)
    Commentaire : Par mémoïsation, on utilise une Hashtable pour retenir lesquels on a déjà vu
  *)

  let deja_vu = Hashtbl.create 20 in 

  let rec aux n step = 
  try 
    Hashtbl.find deja_vu n;
    0
  with Not_found -> 
    Hashtbl.replace deja_vu n step;
    let m = rev_subV1 n in 
    if m = 0 then (if step >= 9 then 9 else step) else aux m (step + 1)

in aux n0 1

let int_to_array n len = 
  (*
    Entrée : Un entier n et un entier len 
    Sortie : Un tableau n1 de taille len, telle que n1.(i) est le i-ème chiffre en lisant par la droite
    Commentaire : Divisions euclidiennes successives
  *)

  let n1 = Array.make len 0 in 
  let temp = ref n in 
  for i = 0 to (len-1) do
    n1.(len-1-i) <- !temp mod 10;
    temp := !temp / 10
  done;
  n1

let print_nature n = 
(*    Entrée : Un élément de type nature
      Sortie : Rien
      Effet de bord : Affiche la nature dans le terminal
*)
  if n = Finombre then print_string "Finombre" else print_string "Infinombre" 


let pow n p = 
  (*
    Entrée : Un entier n et un entier n
    Sortie : L'entier n exposant p
    Commentaire : Exponentiation modulaire
  *)

  let rec aux_pow p = match p with 
    | 0 -> 1
    | 1 -> n
    | m -> let b = aux_pow (m/2) in b*b*(if m mod 2 = 0 then 1 else n)
in aux_pow p



let nature_finder pas n =  
  (*
    Entrées : Deux entiers pas et n
    Sortie : Rien
    Effets de bord : Inscription de la nature des nombres de 0 à n dans le fichier data 
                      et affichage tous les 'pas' de la quantité de Finombres et d'Infinombres
    Commentaire : Le 'pas' permet de contrôler le nombre de mise à jour du fichier

  Sommaire : 
    (0) Initialisation des compteurs de Finombres et d'Infinombres
    (1) Initialisation du tableau nature de taille 'pas'
    (2) Pour les nombres i de 0 à n
      -a On calcule la nature du nombre i
      -b Si c'est un Finombre, alors on augmente le compteur de Finombres et on met la nature du tableau à 0
         Sinon on augment le compteur des Infinombre et on met la valeur 1 en nature dans la tableau
         
      -c Si on a atteint le 'pas', on affiche le nombre de Finombres et d'Infinombres puis on copie le tableau dans le fichier data à la bonne position
  *)
  
  (*0*)
  let countFi = ref 0 in 
  let countInf = ref 0 in 

  (*1*)
  let tab_nature = Array.make (pas) (-1) in  
  
  (*2*)
  for i = 0 to (n) do
    if suite_mem i = Finombre (*a*)
      (*n*)
      then 
        (incr countFi; tab_nature.(i mod pas) <- 0) 
      else 
        (incr countInf; tab_nature.(i mod pas) <- 1);
    (*c*)
    if (i mod pas = 0 && i <> 0 )
      then(
        Printf.printf "i : %d, Finombre : %d, Infinombre : %d\n" i !countFi !countInf; 
        Writer.write_file data temp_file (i-pas) i tab_nature;
        Writer.copy_file temp_file data);
  done;
  print_newline ()


  let vol_finder pas n =  
    (*
      Entrées : Deux entiers pas et n
      Sortie : Rien
      Effets de bord : Inscription du temps de vol des nombres de 0 à n dans le fichier data 
                        et affichage tous les 'pas' de la quantité de Finombres et d'Infinombres
      Commentaire : Le 'pas' permet de contrôler le nombre de mise à jour du fichier
  
    Sommaire : 
      (1) Initialisation du tableau nature de taille 'pas'
      (2) Pour les nombres i de 0 à n
        -a On calcule le temps de vol du nombre i (-1 si +infini)
        -b Si c'est un Finombre, alors on augmente le compteur de Finombres et on met la nature du tableau à 0
           Sinon on augment le compteur des Infinombre et on met la valeur 1 en nature dans la tableau
           
        -c Si on a atteint le 'pas', on affiche le nombre de Finombres et d'Infinombres puis on copie le tableau dans le fichier data à la bonne position
    *)
    
    (*1*)
    let tab_nature = Array.make (pas) (-1) in  
    
    (*2*)
    for i = 0 to (n) do
      tab_nature.(i mod pas) <- suite_temps_vol i ;
      (*c*)
      if (i mod pas = 0 && i <> 0 )
        then(
          Printf.printf "i : %d\n" i;
          Writer.write_file data_vol temp_vol_file (i-pas) i tab_nature;
          Writer.copy_file temp_vol_file data_vol);
    done;
    print_newline ()

let nature_counter_special n a b = 
  (* 
    Entrées : Trois entiers n, a et b
    Sortie : Rien
    Effet de bord : Determine la nature des n premiers nombres de la forme a*k + b et l'affiche leur quantité
  *)

  let countFi = ref 0 in 
  let countInf = ref 0 in

  for i = 0 to (n) do
    let fi = a*i + b in
    if suite_mem (fi) = Finombre 
      then 
        (incr countFi;Printf.printf "i : %d, Finombre : %d, Infinombre : %d\n" (i) !countFi !countInf) 
      else 
        (incr countInf)
  done

let nature_counter_special_11_times_10_pow_minus_1 () = 

  (* Calcul la nature des nombres de la forme 11*(10^i - 1) * j *)
  let countFi = ref 0 in 
  let countInf = ref 0 in

  
  for i = 2 to (16) do
    let oh = 11*((pow 10 i) - 1) in (*  11*(10^i - 1)  *)
    for j = 1 to 10 do 

      if j <> 5 
        then 
          (if suite_mem (oh*j) = Finombre 
            then 
              incr countFi 
            else 
              incr countInf);
      
      Printf.printf "i : %d, Finombre : %d, Infinombre : %d\n" (oh*j) !countFi !countInf;
    done;
  done;
  print_int (11*((pow 10 7) - 1)*1)


let nature_finder_from debut pas n =  
  (*
    Entrées : Deux entiers pas et n
    Sortie : Rien
    Effets de bord : Inscription de la nature des nombres de 0 à n dans le fichier data 
                      et affichage tous les 'pas' de la quantité de Finombres et d'Infinombres
    Commentaire : Le 'pas' permet de contrôler le nombre de mise à jour du fichier

  Sommaire : 
    (0) Initialisation des compteurs de Finombres et d'Infinombres
    (1) Initialisation du tableau nature de taille 'pas'
    (2) Pour les nombres i de 0 à n
      -a On calcule la nature du nombre i
      -b Si c'est un Finombre, alors on augmente le compteur de Finombres et on met la nature du tableau à 0
         Sinon on augment le compteur des Infinombre et on met la valeur 1 en nature dans la tableau
         
      -c Si on a atteint le 'pas', on affiche le nombre de Finombres et d'Infinombres puis on copie le tableau dans le fichier data à la bonne position
  *)
  
  (*0*)
  let countFi = ref 0 in 
  let countInf = ref 0 in 

  (*1*)
  let tab_nature = Array.make (pas) (-1) in  
  
  (*2*)
  for i = debut to (n+debut) do
    if suite_mem i = Finombre (*a*)
      (*n*)
      then 
        (incr countFi; tab_nature.(i mod pas) <- 0) 
      else 
        (incr countInf; tab_nature.(i mod pas) <- 1);
    (*c*)
    if ((i-debut) mod pas = 0 && (i-debut) <> 0 )
      then(
        Printf.printf "i : %d, Finombre : %d, Infinombre : %d\n" i !countFi !countInf; 
        Writer.write_file "data2.csv" temp_file (i-pas) i tab_nature;
        Writer.copy_file temp_file "data2.csv");
  done;
  print_newline ()
