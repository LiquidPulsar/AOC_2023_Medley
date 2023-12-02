open List
open Core
open Re
open In_channel

type col = {
  r: int;
  g: int;
  b: int;
}

(* let string_of_col c = string_of_int c.r ^" "^ string_of_int c.g ^" "^ string_of_int c.b *)

let file = "input.txt"

let pat = Re.Perl.compile_pat {|(\d+) (red|green|blue)|}

let all_matches (s : string) : (int * string) list = 
  map (fun t -> (int_of_string (Group.get t 1), Group.get t 2)) (Re.all pat s)

let parse_line (s : string) : col = 
  let c = {r=0; g=0; b=0}
  and res = all_matches s
in 
(* iter (fun (x,y) -> print_endline (string_of_int x ^ " of " ^ y)) res; *)
fold_left (fun c (n,t) -> match t with
    "red"   -> {c with r = max c.r n}
  | "green" -> {c with g = max c.g n}
  | "blue"  -> {c with b = max c.b n}
  | _ -> failwith "impossible case") c res
(* in print_endline (string_of_col out); out *)


let () =
  try
    let ic = create file in
    let lines = 
      let rec read_lines () =
        match input_line ic with
        | Some line -> line :: read_lines ()
        | None      -> close ic; []
      in read_lines ()
    in let acc_1 (tot,i) c = ((tot + if c.r<=12 && c.g<=13 && c.b<=14 then i else 0),i+1)
    in let cols = map parse_line lines in
    let (p1,_) = fold_left acc_1 (0,1) cols in
    print_endline (string_of_int p1);
    print_endline (string_of_int (fold_left (fun tot c -> tot + c.r * c.g *c.b) 0 cols));
    (* iter print_endline (map string_of_col cols); *)
  with 
  | Sys_error e ->
    eprintf "Error: %s\n" e