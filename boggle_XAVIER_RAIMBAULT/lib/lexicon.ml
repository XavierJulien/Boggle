module M = struct
  include Map.Make (Char)

let to_iter s =
  fun k ->
  iter (fun key item -> k (key, item)) s
end

type t = {
  eow : bool;
  words : t M.t;
}

let empty =
  (* Cette valeur vous est donnée, vous n'avez pas besoin de l'écrire
     vous-même. *)
  {
    eow = false;
    words = M.empty;
  }

let has_empty_word { eow; words } = (** [Valide]  *)
  eow

let rec is_empty { eow; words } = (** [Valide]  *)
  (eow == false) && (M.is_empty words || M.for_all (fun key value -> is_empty value) words)

let add word lexicon = (** [Valide]  *)
	let taille_word = String.length word in
		let rec aux cpt lex =
      if cpt == taille_word then
        {eow = true; words= lex.words}
      else
        begin
    			let lettre = String.get word cpt in
          let lex_next = M.find_opt lettre lex.words in
    			match lex_next with
    				| Some dico -> {eow = lex.eow; words = M.add lettre (aux (cpt+1) dico) lex.words}
    				| None ->      {eow = lex.eow; words = M.add lettre (aux (cpt+1) empty) lex.words}
        end
		in aux 0 lexicon

  let to_iter { eow; words } =
        let rec aux str lex =
            if lex.eow
            then
              begin
              Iter.append
                (Iter.singleton str)
                (M.fold
                  (fun key value acc -> Iter.append acc (aux (str^(String.make 1 key)) value))
                  lex.words
                  Iter.empty)
              end
            else
              Iter.append
                (Iter.empty)
                (M.fold
                  (fun key value acc -> Iter.append acc (aux (str^(String.make 1 key)) value))
                    lex.words
                    Iter.empty)
        in aux "" {eow; words}


let letter_suffixes { eow; words } letter = (** [Valide]  *)
  if M.mem letter words then
    M.find letter words
    else empty

let rec filter_min_length len { eow; words } = (** [A changer selon len]  *)
  let rec traverse cpt lex =
    if cpt == len-1
    then
      {eow = lex.eow; words = lex.words}
    else
      {eow=false; words= M.fold (fun key value acc -> M.add key (traverse (cpt+1) value) acc) lex.words M.empty}
  in traverse 0 {eow; words}


let load_file f =
  let rec load_channel channel acc =
    match input_line channel with
    | word -> load_channel channel (add word acc)
    | exception End_of_file -> acc
  in
  match open_in f with
  | channel -> Some (load_channel channel empty)
  | exception Sys_error _ -> None
