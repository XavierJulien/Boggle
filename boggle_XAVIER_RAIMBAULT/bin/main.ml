open Boggle

let main () =
  let lexique = Lexicon.load_file "dict/dico_fr.txt" in
    match lexique with
      | None -> failwith "le fichier est inexistant"
      | Some dico ->
          if Array.length Sys.argv >= 2
            then begin
              print_string "J'ai reçu les paramètre ";
              print_endline Sys.argv.(1);
              let board = Board.from_string Sys.argv.(1) in
               match board with
                | None -> print_string "La longeur de votre string ne permet pas la création d'une grille"
                | Some board -> Board.print board;
                    print_string "donnez un chiffre entre 1 et 16 précisant la longueur minimum des mots que vous souhaitez recevoir : ";
                    let entree = read_line() in
                    let lexicon = Lexicon.filter_min_length ((int_of_string entree)+1) dico in
                    let solv =  Iter.to_rev_list (Path.iter_to_words board (Solver.find_all_paths board lexicon)) in
                    print_string "Trié par ordre alphabétique";print_newline();
                    List.iter (fun x -> print_string x;
                                        print_newline();)
                              (List.sort (fun x y -> (String.compare x y))
                                          solv);
                    print_string "Trié par ordre décroissant";print_newline();
                    List.iter (fun x -> print_string x;
                                        print_newline ())
                              (List.sort (fun x y -> if String.length x = String.length y
                                                     then 0
                                                     else if String.length x < String.length y
                                                      then 1
                                                      else -1)
                                          solv);

            end
            else begin
              print_endline "Je n'ai reçu aucun paramètre";
              let board = Board.make 4 (RandomLetter.picker RandomLetter.Distribution.fr) in
              Board.print board;
              print_string "donnez un chiffre entre 1 et 16 précisant la longueur minimum des mots que vous souhaitez recevoir : ";
              let entree = read_line() in
              let lexicon = Lexicon.filter_min_length ((int_of_string entree)+1) dico in
              let solv = Iter.to_rev_list (Path.iter_to_words board (Solver.find_all_paths board lexicon)) in
                print_string "Trié par ordre alphabétique";print_newline();
                List.iter (fun x -> print_string x;
                                    print_newline();)
                          (List.sort (fun x y -> (String.compare x y))
                                      solv);
                print_string "Trié par ordre décroissant";print_newline();
                List.iter (fun x -> print_string x;
                                    print_newline ())
                          (List.sort (fun x y -> if String.length x = String.length y
                                                 then 0
                                                 else if String.length x < String.length y
                                                  then 1
                                                  else -1)
                                      solv);
            end
let () = main ()
