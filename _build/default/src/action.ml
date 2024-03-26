type writing = string list

type action =
  | Bet of writing
  | Hit
  | Stay
  | Split
  | Double_down
  | Surrender
  | Insurance
  | Start of writing
  | End of writing

exception Empty

exception Malformed

(**helper function for read. Takes a string and returns it filtered into
   seperate words*)
let white_space str =
  List.filter (fun x -> x <> "") (String.split_on_char ' ' str)

let rec read_helper (tut_msg : string) = 
  print_endline tut_msg; print_endline "Would you like to perform this action?";
  let input = (read_line ()) in  
  (match input with
    | "yes" | "y" -> true
    | "no" | "n" -> false
    | _ -> print_endline "Unknown input. Try again."; read_helper tut_msg )

(**read command line and return action*)
let rec read (is_tut:bool) (tut_msg:string array) str =
  match white_space (String.lowercase_ascii str) with
  | [] -> raise Empty
  | h :: t ->
      if h = "bet" && t <> [] then Bet t
      else if h = "hit" && t = [] then 
        if(is_tut) 
          then 
            (if read_helper tut_msg.(0) then Hit else( print_endline "Select a new action"; read is_tut tut_msg (read_line ()))) else Hit
      else if h = "stay" && t = [] then 
        if(is_tut) 
          then 
            (if read_helper tut_msg.(1) then Stay else( print_endline "Select a new action"; read is_tut tut_msg (read_line ()))) else Stay
      else if h = "split" && t = [] then 
        if(is_tut) 
          then 
            (if read_helper tut_msg.(4) then Split else( print_endline "Select a new action"; read is_tut tut_msg (read_line ()))) else Split
      else if h = "double" && t = [ "down" ] then 
        if(is_tut) 
          then 
            (if read_helper tut_msg.(2) then Double_down else( print_endline "Select a new action"; read is_tut tut_msg (read_line ()))) else Double_down
      else if h = "surrender" && t = [] then 
        if(is_tut) 
          then 
            (if read_helper "" then Surrender else( print_endline "Select a new action"; read is_tut tut_msg (read_line ()))) else Surrender
      else if h = "insurance" && t = [] then 
        if(is_tut) 
          then 
            (if read_helper tut_msg.(3) then Insurance else( print_endline "Select a new action"; read is_tut tut_msg (read_line ()))) else Insurance
      else if h = "start" && t <> [] then Start t
      else if h = "end" && t <> [] then End t
      else raise Malformed
