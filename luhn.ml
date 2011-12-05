let (|>) x f = f x

let min_ccn_length = 14
let max_ccn_length = 16

module IntSet = struct
  include Set.Make(struct
    type t = int
    let compare = Pervasives.compare
  end)

  (* returns the set without its minimum element *)
  let tail set =
    remove (min_elt set) set
end

let is_digit ch =
  '0' <= ch && ch <= '9'

let digits_at indices str =
  let parse_digit ch =
    if is_digit ch then
      int_of_char ch - int_of_char '0'
    else
      failwith (Printf.sprintf "'%c' is not a digit" ch)
  in
  indices |> IntSet.elements |> List.map (fun i -> parse_digit str.[i])

let chars_of_string str =
  let rec inner sofar = function
    | (-1) -> sofar
    | i -> inner (str.[i] :: sofar) (i-1)
  in
  inner [] (String.length str - 1)

let rec string_of_chars = function
  | [] -> ""
  | first :: rest -> Printf.sprintf "%c%s" first (string_of_chars rest)

(* Assumes: digits are all < 10 *)
let is_valid_ccn digits =
  let convert_double_digits_to_two_single_digits elements =
    let rec inner sofar = function
      | [] -> sofar
      | first :: rest ->
          if first >= 10 then
            inner (first / 10 :: first mod 10 :: sofar) rest
          else
            inner (first :: sofar) rest
    in
    inner [] elements
  in
  let map_every_second_reversed transform elements =
    let rec inner sofar is_alternate = function
      | [] -> sofar
      | first :: rest ->
          inner ((if is_alternate then transform first else first) :: sofar) (not is_alternate) rest
    in
    inner [] (List.length elements mod 2 = 0) elements
  in
  let sum = List.fold_left (+) 0 in
  (digits
    |> map_every_second_reversed (( * ) 2)
    |> convert_double_digits_to_two_single_digits
    |> sum) mod 10 = 0

let find_ccn_indices_in indices str =
  let rec inner sofar rest_indices =
    if IntSet.cardinal rest_indices < min_ccn_length then
      sofar
    else
      let digits = digits_at rest_indices str in
      if is_valid_ccn digits then
        IntSet.union sofar rest_indices
      else
        inner sofar (IntSet.tail rest_indices)
  in
  inner IntSet.empty indices

(* Converts a list of elements into a list of (index, element) pairs *)
let enumerate items =
  let rec indexed_from i sofar = function
    | [] -> List.rev sofar
    | first::rest -> indexed_from (i+1) ((i, first) :: sofar) rest
  in
  indexed_from 0 [] items

let mask_ccns str =
  let indexed_chars = str |> chars_of_string |> enumerate in
  let ccn_indices, _ =
    indexed_chars |> List.fold_left (fun (known, potential) (i, ch) ->
      match ch with
      | _ when is_digit ch -> (* this digit could contribute to the known indices *)
        let size = IntSet.cardinal potential in
        let potential = IntSet.add i (if size = max_ccn_length then IntSet.tail potential else potential) in
        let new_known = find_ccn_indices_in potential str in
        (IntSet.union known new_known, potential)
      | ' ' | '-' -> (* no new info *)
        (known, potential)
      | _ -> (* the preceding digits are no longer potentially ccn *)
        (known, IntSet.empty)
    ) (IntSet.empty, IntSet.empty)
  in
  indexed_chars
    |> List.map (fun (i, ch) -> if IntSet.mem i ccn_indices then 'X' else ch)
    |> string_of_chars


let rec loop () =
  try
    read_line () |> mask_ccns |> print_endline |> loop
  with End_of_file ->
    ()

let main = loop ()
