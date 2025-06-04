(* Fonctions permettant le traitement de données par des fichier en Ocaml

  Les Fonctions : 
    
    copy_channel :
      Entrée : Un channel source en lecture 'so' un channel 'de' destination en écriture
      Sortie : Rien
      Effet de Bord : Copie tout le fichier d'entrée dans le fichier de sortie (Enfin au maximum 40320 lignes)
      Commentaire : Cette fonction est très utile pour continuer d'écrire dans un fichier sasn tout effacer. On met en général 'data.txt' et 'temp.txt' en so et de

    copy_first_line_channel : 
      Entrée : Un channel source en lecture 'so' un channel 'de' destination en écriture
      Sortie : Rien
      Effet de Bord : Copie uniquement la première ligne du fichier source dans celui de destination

    copy_file : 
      Entrée : Des strings 'source' et 'dest' chemin de fichiers
      Sortie : Rien
      Effet de bord : Copie tout le fichier source dans celui de destination
      Commentaire : On utilise principalement la fonction copy_channel mais en ouvrant les channels

    copy_first_line_file : 
      Entrée : Des strings 'source' et 'dest' chemin de fichiers
      Sortie : Rien
      Effet de bord : Copie la première ligne du fichier source dans celui de destination
      Commentaire : On utilise principalement la fonction copy_channel mais en ouvrant les channels
      *)

(* Copie totalement  un channel source so dans un autre de*)
let copy_channel so de =
  let running = ref true in  
  let r = input_line so in
  let countline = ref 0 in 
  Printf.fprintf de "%s" r;
  flush de;
  while !running && (!countline < 40320) do 
    try
      let r = input_line so in
      Printf.fprintf de "\n%s" r;
      flush de;
      countline := !countline + 1
    with
      End_of_file -> running := false
  done

(* Copie totalement  un channel source so dans un autre de*)
let copy_first_line_channel so de = 
  let r = input_line so in
  Printf.fprintf de "%s" r;
  flush de


(* Copie un fichier source dans un fichier dest*)
let copy_file source dest = 
  let da = open_in source in 
  let te = open_out dest in

  copy_channel da te;
  close_in da;
  close_out te

(* Copie la première ligne du fichier source dans un fichier dest*)
let copy_first_line_file source dest = 
  let da = open_in source in 
  let te = open_out dest in

  copy_first_line_channel da te;
  close_in da;
  close_out te

(* Ecrit la likste des Finombre (0) et infinombre (1) à partir de *)
let write_recur_channel n l te =
  ()

    
(* écrit les nombres de n à m en connaissant la liste l*)
let write_file data temp n m l = 
  (*
  Sommaire : 
    (0) ouverture des channels associés aux fichiers
    (1) Copie la première ligne 
    (2) Récupère le nombre max entre le nombre de caractères de la première ligne (écrit en deuxième ligne) et le nombre max m
    (3) On met le curseur en position n dans la première ligne
    (4) On écrit les natures des (m-n) nombres à leur position. La nature du nombre i dans [|n,m|] est codée par l.(i-n)
    (5) On écrit sur la ligne suivante le nombre de nombres analysées (par vraiment mais le nombre de caractères de la première ligne en tout cas)*)
    
  (*0*)
  let da = open_in data in 
  let te = open_out temp in

  copy_first_line_channel da te; (*1*)
  let maxi = max (int_of_string (input_line da)) m in (*2*)
  close_in da;
  seek_out te n; (*3*)

  (*4*)
  for i = n to (m-1) do 
    Printf.fprintf te "%d" l.(i-n);
    flush te
  done;

  (*5*)
  seek_out te maxi;
  Printf.fprintf te "\n%d" maxi;

  flush te;
  close_out te


let data = "data.csv"
let temp_file = "temp.csv"

(*
let main () = 
  
  Printf.printf "(%d,%d)" 5 11;
  write_file data temp_file 15 20 [|3;3;3;3;3;3;3;3;3;3;3;3|];
  copy_file temp_file data


  
let () = main ()*)