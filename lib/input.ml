open Constants

(* Helpers *)
let rec l_compress = function
  | [] -> []
  | 0 :: t -> l_compress t
  | h :: t -> h :: l_compress t

let rec l_merge = function
  | a :: b :: t when a = b -> (a + b) :: l_merge t
  | a :: t -> a :: l_merge t
  | [] -> []

let l_move row =
  let compressed = l_compress row in
  let merged = l_merge compressed in
  let result = l_compress merged in
  result @ List.init (List.length row - List.length result) (fun _ -> 0)

let r_move row =
  let rev = List.rev row in
  let result = l_move rev in
  List.rev
    (result @ List.init (List.length row - List.length result) (fun _ -> 0))

(* let transpose board = *)
let calculate_next board dir =
  if dir = move_left then List.map l_move board
  else if dir = move_right then List.map r_move board
  else failwith "TODO"
(* else if dir = move_down then List.map d_move board else if dir = move_up then
   List.map u_move board *)
