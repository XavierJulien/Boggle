type t = (int * int) list

let empty = []

let is_valid_pos board (i, j) =
  i < Board.dim board && i >= 0 && j >= 0 && j < Board.dim board

let add_tile board path (i, j) =
  let taille = List.length path in

      if (taille = 0) && (is_valid_pos board (i,j))
      then Some ((i, j)::path)
      else
        let pos = (List.nth path (taille-1)) in
          if (Board.are_neighbours board (i, j) pos) && (not (List.mem (i, j) path)) && (is_valid_pos board (i, j))
            then let res = path @ [(i,j)] in Some res
            else None

let rec to_string board path =
  match path with
    |[] -> ""
    |h::q -> (String.make 1 (Board.get_letter board (fst h) (snd h)))^(to_string board q)

let iter_to_words board all_paths =
  Iter.fold (fun acc x -> let word = (to_string board x) in
                            if (Iter.exists (String.equal word) acc) then
                              acc
                            else
                              Iter.cons word acc) Iter.empty all_paths
