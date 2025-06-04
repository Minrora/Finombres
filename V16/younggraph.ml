(* ocamlopt -o code younggraph.ml;
dot -Tsvg graphyoung.dot;
sfdp -Tsvg -Goverlap=prism -o graphyouung.svg
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

let graphpath = "graphyoung.dot"
let temp_file = "temp.csv"

let print_list f l = List.iter f l

let print_tuple_list l = print_list (fun (a,b) -> Printf.printf "(%d,%d)," a b) l


let all_paires_without_sym b = 
  (*
  Entrée : un entier b représentant la base
  Sortie : Rien
  Effet de bord : Une liste contenant toutes les paires (a1,a2) avec a1 et a2 compris entre 0 et b-1 inclu où si (a1,a2) existe, (a2,a1) n'y est pas. *)

  let rec aux count1 count2 = match (count1,count2) with
    | (0,0) -> [(0,0)] 
    | (0,a2) -> (0,a2)::(aux 0 (a2-1))
    | (a1,0) -> (a1,0)::(aux (a1-1) (a1-1))
    | (a1,a2) -> (a1,a2)::(aux a1 (a2-1))
in aux (b-1) (b-1)

let all_paires b = 
  (*
  Entrée : un entier b représentant la base
  Sortie : Rien
  Effet de bord : Une liste contenant toutes les paires (a1,a2) avec a1 et a2 compris entre 0 et b-1 inclu*)

  let rec aux count1 count2 = match (count1,count2) with
    | (0,0) -> [(0,0)] 
    | (0,a2) -> (0,a2)::(aux 0 (a2-1))
    | (a1,0) -> (a1,0)::(aux (a1-1) (b-1))
    | (a1,a2) -> (a1,a2)::(aux a1 (a2-1))
in aux (b-1) (b-1)

let calcul_r_avec_bri b k (rn_1_i,ri_1) (an_1_i,ai) = 
    let rn_2_i = ai + rn_1_i*b - k*an_1_i in 
    let bri = k*ai + ri_1 - an_1_i in 
    let ri = bri/b in 
    (rn_2_i,bri,ri)

let calcul_r b k (rn_1_i,ri_1) (an_1_i,ai) = 
  let rn_2_i = ai + rn_1_i*b - k*an_1_i in 
  let bri = k*ai + ri_1 - an_1_i in 
  let ri = bri/b in 
  (rn_2_i,ri)

let add_edge gfile parent deja_vu (rn_1_i,ri_1) (an_1_i,ai) =
  (*
    Entrée : Un channel gfile, un entier k et un entier v
    Sortie : Rien
    Effet de bord : Ecrit dans le fichier associé à gfile l'arête k->v
  *) 

  let n1 = parent in 
  let n2 = Hashtbl.find deja_vu (rn_1_i,ri_1) in 
  (*Printf.fprintf gfile "  %d [label = \"[%d,%d]\"];\n" n2 rn_1_i ri_1;*)
  Printf.fprintf gfile "  %d->%d [label = \"(%d,%d)\"];\n" n1 n2 an_1_i ai
 
let add_vertex gfile deja_vu (rn_1_i,ri_1) = 
    let n = Hashtbl.find  deja_vu (rn_1_i,ri_1) in 
    Printf.fprintf gfile "  %d [label = \"[%d,%d]\"];\n" n rn_1_i ri_1
  
let test b k (rn_1_i,ri_1) (ai,an_1_i) =
  (* Vérifie que les restes sont possibles (entiers et entre 0 et b-1) *) 
  let (rn_2_i,bri,ri) = calcul_r_avec_bri b k (rn_1_i,ri_1) (ai,an_1_i) in  
  (rn_2_i >= 0) && (rn_2_i <= b-1) && (bri mod b = 0) && (ri >= 0) && (ri <= b-1) 



let fusion_list a0 b0= 
  let rec aux a b = match (a,b) with 
    |([],[]) -> []
    |(ha::ta,hb::tb) -> (ha,hb)::(aux ta tb)
    | _ -> failwith "Impossible de fusionner les deux listes, pas la même taille"
  in aux a0 b0


let youngrec b k gfile = 

  let deja_vu = Hashtbl.create 100 in 
  let deja_vu_edge = Hashtbl.create 1000 in 

  let rec recur parent (edge,(rn_1_i,ri_1)) = 
    try
      Hashtbl.find deja_vu (rn_1_i,ri_1);
      
      if not (Hashtbl.mem deja_vu_edge (parent,edge,(rn_1_i,ri_1))) then 
          add_edge gfile parent deja_vu (rn_1_i,ri_1) edge;
          Hashtbl.add deja_vu_edge (parent,edge,(rn_1_i,ri_1)) true;
      ()
    with 
    | Not_found ->
      (
      
      let candidats_possibles = List.filter (test b k (rn_1_i,ri_1)) (all_paires b) in (* Donne les couples (an_i, ai) possibles *)
      let reste_possibles = List.map (calcul_r b k (rn_1_i,ri_1)) candidats_possibles in (* Donne les couples (rn_i_i, ri_i) possibles associés aux couples précedents *)
      let fusion_ai_ri = fusion_list candidats_possibles reste_possibles in

      Printf.printf "\n\nSommet : [%d,%d]\n" rn_1_i ri_1 ;
      Printf.printf "arêtes possibles";
      print_tuple_list candidats_possibles;
      Printf.printf "restes possibles";
      print_tuple_list reste_possibles;

      let val_parent = (Hashtbl.length deja_vu) in
      Hashtbl.add deja_vu (rn_1_i,ri_1) val_parent;
      add_vertex gfile deja_vu (rn_1_i,ri_1);

      add_edge gfile parent deja_vu (rn_1_i,ri_1) edge;
      Hashtbl.add deja_vu_edge (parent,edge,(rn_1_i,ri_1)) true;

      List.iter (recur val_parent) fusion_ai_ri
      )
      
    in 
  
  
  Printf.fprintf gfile "digraph graphYoung {\n";

  (* Cas du premier noued à part *)
  let candidats_possibles = List.filter (test b k (0,0)) (all_paires b) in
  let reste_possibles = List.map (calcul_r b k (0,0)) candidats_possibles in

    Printf.printf "\n\nSommet : [[0,0]]\n" ;
    Printf.printf "arêtes possibles";
    print_tuple_list candidats_possibles;
    Printf.printf "restes possibles";
    print_tuple_list reste_possibles;
    
  Printf.fprintf gfile "  %d [label = \"[[0,0]]\"];\n" (-1);

  recur (-1) (List.hd candidats_possibles,List.hd reste_possibles);

  Printf.fprintf gfile "\n}\n"




let write_graph b k graphpath = 
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
  let gfile = open_out graphpath in 

  (*   (1)   *)
  youngrec b k gfile;

  (*   (2)   *)
  flush gfile;
  close_out gfile





let main () = 
  let (b,k) = (19,4) in 
  print_tuple_list (all_paires b);
  write_graph b k graphpath


let () = main ()
