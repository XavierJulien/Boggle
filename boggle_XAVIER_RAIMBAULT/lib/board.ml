type t = char array array
exception Invalid_argument ;;

let get_letter board i j =
  if(i < Array.length board && j < Array.length board.(i))
    then
      board.(i).(j)
    else
      raise Invalid_argument;;

let dim board =
  Array.length board;;

let all_positions board =
 let n = dim board in
  Iter.product (Iter.range 0 (n-1)) (Iter.range 0 (n-1))

let are_neighbours board (i, j) (i', j') =
      if i == i' && j == j'
        then
          false
        else
          abs (i-i') < 2 && abs (j-j') < 2

let is_valid_pos board (i, j) =
  i < dim board && j < dim board && i >= 0 && j >= 0;;

let neighbours board (i, j) =
    let res = all_positions board in
      Iter.filter (fun (x,y) -> are_neighbours board (x,y) (i,j)) res

let make dim  make_char =
  let res = Array.make_matrix dim dim 'e' in
    for i=0 to (dim-1) do
      for j=0 to (dim-1) do
       res.(i).(j) <- make_char();
      done;
    done;
    res

let is_carre n =
  let aux = sqrt (float_of_int n) in
    aux = (snd (modf aux))

let print board =
  let pos = all_positions board in
    Iter.iter (fun (x,y) -> if (y == ((dim board)-1)) then (print_char(get_letter board x y);print_newline()) else (print_char(get_letter board x y);print_char(' '))) pos

let from_string s =
    if is_carre (String.length s)
      then
        let dim = (int_of_float (sqrt (float_of_int (String.length s)))) in
        let grid = Array.make_matrix dim dim 'x' in
        let index = ref 0 in
          for i=0 to dim-1 do
            for j=0 to dim-1 do
                grid.(i).(j) <- (String.get s !index);
                index := !index + 1
            done;
          done;
        Some grid
      else
        None
