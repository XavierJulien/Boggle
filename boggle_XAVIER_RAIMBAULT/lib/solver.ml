(* backtrack : Board.t -> Lexicon.t -> Path.t -> int * int -> Path.t Iter.t *)
let rec backtrack board lexicon path (i, j) =
  let path' = Path.add_tile board path (i,j) in
    match path' with
    | None -> Iter.empty
    | Some path' ->
      let lettre = Board.get_letter board i j in
      let dico = Lexicon.letter_suffixes lexicon lettre in
        if Lexicon.is_empty dico
          then Iter.empty
          else
            let res_voisins = Iter.flatten (Iter.fold
                                                (fun acc voisin -> Iter.cons (backtrack board dico path' voisin) acc)
                                                Iter.empty
                                                (Board.neighbours board (i, j))) in
                                                  if Lexicon.has_empty_word dico
                                                    then Iter.cons path' res_voisins
                                                    else res_voisins

let find_all_paths board lexicon =
  Iter.flatten (Iter.fold
                (fun acc (i,j) -> Iter.cons (backtrack board lexicon Path.empty (i, j)) acc)
                Iter.empty
                (Board.all_positions board))
