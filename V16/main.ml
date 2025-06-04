let data = "data.csv"

let main () = 

  let argc = Array.length (Sys.argv) in
  Array.iter (fun x -> print_string x; print_string " ") Sys.argv;
  

  if (argc = 1) 
    then
      Printf.printf "\nAucun argument n'a été donné\n\n\n"


  else (
    match Sys.argv.(1) with 
    | key when key = "fruit" -> print_string "\nFruit\n"
    | key when key = "palintiple" -> Pixelure.k_palintiples 9 100 100 100 100
    | key when key = "kpalintiple" -> Pixelure.k_palintiples (int_of_string Sys.argv.(2)) 10000 100 100 100
    | key when key = "graphic" -> (
        Graphicgenerator.create_window 800 800;
        let n = 50000 in 
        Graphicgenerator.create_graphic (Pixelure.repartition n false) (Pixelure.array_integer n) 0x55AAEE;
        Graphicgenerator.create_graphic (Pixelure.repartition n true) (Pixelure.array_integer n) 0xAA55EE;
        Graphicgenerator.create_graphic  (Pixelure.array_integer n) (Pixelure.array_integer n) 0xEE0000;
        Graphicgenerator.close_window ())
    | key when key = "find_nature" -> Perso.nature_finder 10000 (int_of_string(Sys.argv.(2)))
    | key when key = "find_vol" -> Perso.vol_finder 10000 (int_of_string(Sys.argv.(2)))
    | key when key = "grid_vol" -> Pixelure.draw_grid_vol (int_of_string(Sys.argv.(2))) (int_of_string(Sys.argv.(3)))
    | key when key = "plan" -> (
        Graphicgenerator.create_window 2000 2000;
        let n = 3 in 
        Pixelure.plan_number_n_digits n;
        Graphicgenerator.close_window ())
    | key when key = "show_them" -> (
        Graphicgenerator.create_window 1000 1000;
        let r = 40 in 
        
        Scale.draw (Scale.sequences_to_int (Pixelure.sequences data) 1000000) 15 (2*r-8) (2*r-8);
        Graphicgenerator.close_window ()
      )
    | key when key = "image_random" -> Graphicgenerator.create_origin 500 500
    | key when key = "pixel" -> Pixelure.generate (Pixelure.sequences data) 100 (int_of_string Sys.argv.(2)); let a = Graphicgenerator.wait_any_touch () in ()
    | key when key = "nature_finder_from" -> Perso.nature_finder_from 0 1000 100000
    | _ -> print_string "Argument non reconnu (Message du main.ml)\n"
  );
  print_string "\n"

let () = main ()