(* ocamlopt -o code graphgenerator.ml;
dot -Tsvg graph.dot
sfdp -Tsvg -Goverlap=prism -o graph.svg
*)


(* Construit le graphe des successeur de rev_sub 

    fwrite:
      Entrée : Un channel gfile, un entier k et un entier v
      Sortie : Rien
      Effet de bord : Ecrit dans le fichier associé à gfile l'arête k->v

    hashtable_rev_sub : 
      Entrée : Un entier n
      Sortie : Une hashtable (clef,valeur) telle que rev_sub(clef) = valeur
      Commentaire : Permet de générer le graphe associé à rev_sub

    graph_create_rev_sub : 
      Entrée : Une Hashtable htbl, Un channel gfile
      Sortie : Rien
      Effet de bord : Ecrit dans le fichier associé à gfile le graphe associé à la hashtable
    
    write_graph :
       Entrées : Un entier n et un chemin vers un fichier dot 'graphpath'
      Sortie : Rien
      Effet de bord : Construit le graphe et l'écrit dans le fichier données
 
      *)


type nature = Finombre | Infinombre

let graphpath = "graph.dot"
let temp_file = "temp.csv"


let fwrite gfile k v =
  (*
    Entrée : Un channel gfile, un entier k et un entier v
    Sortie : Rien
    Effet de bord : Ecrit dans le fichier associé à gfile l'arête k->v
  *) 
  Printf.fprintf gfile "  %d->%d;\n" k v 
 
let hashtable_rev_sub n mod_9 = 
  (*
    Entrée : Un entier n, un booléen mod_9
    Sortie : Une hashtable (clef,valeur) telle que rev_sub(clef) = valeur pour les valeurs de 0 à n
      - Si mod_9 = false, Alors pour toutes les valeurs de 0 à n
      - Si mod_9 = true, Alors uniquement pour les multiples de 9 entre 0 et n
    Commentaire : Permet de générer le graphe htbl_graph associé à rev_sub 
  *)

  let htbl_graph = Hashtbl.create n in 

  for i = 0 to n do 
    if (not mod_9) then
      Hashtbl.replace htbl_graph i (Perso.rev_subV1 i)
    else
      (if i mod 9 = 0 then Hashtbl.replace htbl_graph i (Perso.rev_subV1 i);)
  done;

  htbl_graph

let graph_create_rev_sub htbl gfile = 
  (*
    Entrée : Une Hashtable htbl, Un channel gfile
    Sortie : Rien
    Effet de bord : Ecrit dans le fichier associé à gfile le graphe associé à la hashtable
  *)
  Printf.fprintf gfile "digraph monGraph {\n";
  Hashtbl.iter (fun x y -> fwrite gfile x y) htbl;
  Printf.fprintf gfile "\n}\n"

let write_graph n graphpath = 
  (*
    Entrées : Un entier n et un chemin vers un fichier dot 'graphpath'
    Sortie : Rien
    Effet de bord : Construit le graphe des n dépendances des n premiers nombres et l'écrit dans le fichier données
  
  Sommaire: 
  (0)
    - Entier n :[|0;n|] intervalle concidéré
    - Hashtable graphh : graphe associé à rev_sub
    - Channel gfile : Channel associé au fichier .dot du graph

  (1)
    - Ecriture dans le fichier graph.dot pour former le graphe

  (2)
    - On Flush puis on ferme le channel gfile
  *)

  (*   (0)   *)
  let n = 9999 in 
  let graphh = hashtable_rev_sub n true in 
  let gfile = open_out graphpath in 

  (*   (1)   *)
  graph_create_rev_sub graphh gfile;

  (*   (2)   *)
  flush gfile;
  close_out gfile





(*
let main () = 
  let n = 9999 in 
  write_graph n graphpath


let () = main ()
*)