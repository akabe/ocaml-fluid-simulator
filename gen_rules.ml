(* Simple fuild simulator by lattice gas automaton

   Copyright (C) 2015 Akinori ABE
   This program is distributed under MIT License.
   See LICENSE.txt for details. *)

#use "cell.ml";;

open Format

type cell = int
type mask = int

type 'a leaf =
  | Single of 'a
  | Double of 'a * 'a

let n_bits = 7

let flip f x y = f y x

let leaf_map f = function
  | Single x -> Single (f x)
  | Double (x, y) -> Double (f x, f y)

let make_cases rules =
  let add key value tbl =
    try ignore (List.assoc key tbl) ; tbl (* [tbl] has [key] *)
    with Not_found -> (key, value) :: tbl
  in
  let add_with_rot key value tbl =
    let rec loop k tbl =
      if k < 0 then tbl
      else loop (k - 1) (add (rotr key 1) (leaf_map (flip rotr 1) value) tbl)
    in
    loop (n_bits - 2) tbl
  in
  let aux tbl = function
    | [a; b] ->
      tbl
      |> add_with_rot a (Single b)
      |> add_with_rot b (Single a)
    | [a; b; c] ->
      tbl
      |> add_with_rot a (Double (b, c))
      |> add_with_rot b (Double (a, c))
      |> add_with_rot c (Double (a, b))
    | _ -> assert false
  in
  List.fold_left aux [] rules

(** Decision trees *)
type 'a tree =
  | Leaf of 'a leaf
  | If of mask * mask * 'a tree * 'a tree option

module List =
struct
  include List

  let init n f =
    let rec aux acc i = if i < 0 then acc else aux (f i :: acc) (i - 1) in
    aux [] (n - 1)

  let pickup f xs =
    let aux acc x =
      let y = f x in
      match acc with
      | None -> Some (x, y, [])
      | Some (max_x, max_y, others) ->
        if y < max_y then Some (max_x, max_y, x :: others)
        else Some (x, y, max_x :: others)
    in
    List.fold_left aux None xs
end

let calc_entropy cases mask =
  let aux (pos, neg) (key, _) =
    if key land mask <> 0 then (pos + 1, neg) else (pos, neg + 1)
  in
  let (pos, neg) = List.fold_left aux (0, 0) cases in
  if pos = 0 || neg = 0 then infinity else
  let p1 = float pos /. float (pos + neg) in
  let p2 = float neg /. float (pos + neg) in
  p1 *. log p1 +. p2 *. log p2

let hd = function
  | x :: xs -> Some (x, (), xs)
  | [] -> None

let make_tree cases =
  let rec aux cases masks =
    match hd masks (*List.pickup (calc_entropy cases) masks*) with
    | Some (mask0, _, masks) ->
      List.partition (fun (key, _) -> key land mask0 <> 0) cases
      |> (function
          | ([], []) -> assert false
          | (pos, []) -> If (mask0, mask0, aux pos masks, None)
          | ([], neg) -> If (mask0, 0, aux neg masks, None)
          | (pos, neg) -> If(mask0, mask0, aux pos masks, Some (aux neg masks)))
    | None -> (* [masks] is empty *)
      match cases with
      | [(_, leaf)] -> Leaf leaf
      | _ -> assert false
  in
  let masks = List.init n_bits (fun k -> 1 lsl k) in
  aux cases masks

let rec optimize = function
  (* [(x land mask1 = rhs1) && (x land mask2 = rhs2)]
     === [x land (mask1 lor mask2) = (rhs1 lor rhs2)] *)
  | If (mask1, rhs1, If (mask2, rhs2, left, None), None) ->
    optimize (If (mask1 lor mask2, rhs1 lor rhs2, left, None))
  (* otherwise: *)
  | Leaf leaf -> Leaf leaf
  | If (mask, rhs, left, None) -> If (mask, rhs, optimize left, None)
  | If (mask, rhs, left, Some right) ->
    If (mask, rhs, optimize left, Some (optimize right))

let pp_print_bint ppf n =
  let rec aux k =
    if k >= 0 then begin
      pp_print_char ppf (if n land (1 lsl k) = 0 then '0' else '1');
      aux (k - 1)
    end
  in
  if n = 0 then pp_print_char ppf '0' else begin
    pp_print_string ppf "0b";
    aux (n_bits - 1)
  end

let gen_code ppf tree =
  let pp_leaf ppf = function
    | Single x -> pp_print_bint ppf x
    | Double(x, y) -> fprintf ppf "select %a %a" pp_print_bint x pp_print_bint y
  in
  let rec aux ppf = function
    | None -> pp_print_string ppf "default ()"
    | Some (Leaf leaf) -> pp_leaf ppf leaf
    | Some (If (lhs, rhs, left, right)) ->
      fprintf ppf "if bits land %a = %a@ then @[%a@]@ else @[%a@]"
        pp_print_bint lhs pp_print_bint rhs aux (Some left) aux right
  in
  fprintf ppf "let f bits =@\n  @[\
               let select x y = if Random.bool () then x else y in@\n\
               let default () = Cell.rotr bits 3 in@\n\
               %a@]" aux (Some tree)

let _ =
  [
    (* 2-particle head-on collisions (FHP-I) *)
    [0b0_001001; 0b0_010010; 0b0_100100];
    (* 3-particle symmetric collisions (FHP-I) *)
    [0b0_010101; 0b0_101010];
    (* 2-particle asymmetric collisions (FHP-II) *)
    [0b0_010100; 0b1_000001];
    (* stopped- and 2-particle head-on collisions (FHP-II) *)
    [0b1_001001; 0b1_010010; 0b1_100100];
    (* stopped- and 3-particle symmetric collisions (FHP-II) *)
    [0b1_010101; 0b1_101010];
    (* 3-particle asymmetric collisions (FHP-III) *)
    [0b1_010100; 0b0_010011; 0b0_100101];
    (* stopped- and 3-particle asymmetric collisions (FHP-III) *)
    [0b1_101100; 0b1_010011; 0b0_101011];
    (* 4-particle symmetric collisions (FHP-III) *)
    [0b0_011011; 0b0_101101; 0b0_110110];
    (* stopped- and 4-particle symmetric collisions (FHP-III) *)
    [0b1_011011; 0b1_101101; 0b1_110110];
    (* 5-particle collisions (FHP-III) *)
    [0b0_111110; 0b1_101011];
  ]
  |> make_cases
  |> make_tree
  |> optimize
  |> gen_code std_formatter
