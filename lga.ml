(* Simple fuild simulator by lattice gas automaton

   Copyright (C) 2015 Akinori ABE
   This program is distributed under MIT License.
   See LICENSE.txt for details. *)

open Cell

type boundary =
  | Cell of int * int (** Not boundary, connected to a cell (y, x) *)
  | NoSlip (** No-slip boundary condition *)
  | Inflow of float (** Occurrence of particles with a probability *)
  | Outflow (** Disappearance of particles *)

(** [propagate boundaries src dest] computes propagation of particles in field
    [src] and destructively stores the result into [dest]. *)
let propagate boundaries src dest =
  let aux i j bits = function
    | None -> ()
    | Some boundary ->
      Mat.replace ((lor) (bits land 0b1000000)) dest i j; (* stopped particle *)
      Array.iteri (fun k ->
          let mask = 1 lsl k in
          let flag = bits land mask in
          function
          | Cell (i', j') -> Mat.replace ((lor) (rotr flag 3)) dest i' j'
          | NoSlip -> Mat.replace ((lor) flag) dest i j
          | Inflow p ->
             if Random.float 1.0 < p then Mat.replace ((lor) mask) dest i j
          | Outflow -> ())
        boundary
  in
  Mat.fill dest 0;
  Mat.iteri2 aux src boundaries

let collide boundaries field =
  Mat.replace_all2
    (fun bits -> function
       | None -> 0
       | Some _ -> Rules.f bits)
    field boundaries

(** {2 Smoothing} *)

let smoothing_in_block av_field field x y width height =
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      let bits = Mat.unsafe_get field (y + i) (x + j) in
      Array.iteri
        (fun k v ->
           if bits land (1 lsl k) <> 0
           then Array.unsafe_set av_field k (v +. 1.0)) av_field
    done
  done

let smoothing block_size counts field =
  let (n_rows, n_cols) = Mat.size counts in
  for i = 0 to n_rows - 1 do
    for j = 0 to n_cols - 1 do
      smoothing_in_block
        (Mat.unsafe_get counts i j)
        field (block_size * j) (block_size * i) block_size block_size
    done
  done

(** {2 Interface} *)

type t =
  {
    period : int;
    block_size : int;
    steps : int ref; (** # of steps that LGA is executed. *)
    fields : Cell.t Mat.t * Cell.t Mat.t;
    boundaries : boundary array option Mat.t;
    av_field : float array Mat.t;
  }

let create ~barrier ~boundary ~init ~block_size ~period width height =
  let make_boundary y x =
    if barrier x y then None
    else if y mod 2 = 1
    then Some [| boundary (x+1) y; boundary (x+1) (y-1); boundary x (y-1);
                 boundary (x-1) y; boundary x (y+1); boundary (x+1) (y+1) |]
    else Some [| boundary (x+1) y; boundary x (y-1); boundary (x-1) (y-1);
                 boundary (x-1) y; boundary (x-1) (y+1); boundary x (y+1) |]
  in
  let n_rows = height / block_size in
  let n_cols = width / block_size in
  let field0 = Mat.init height width (fun y x -> init x y) in
  {
    period; block_size; steps = ref 0;
    fields = (field0, Mat.make height width 0);
    boundaries = Mat.init height width make_boundary;
    av_field = Mat.init n_rows n_cols (fun _ _ -> Array.make 7 0.0);
  }

let exec1 lga =
  let swap (x, y) = (y, x) in
  let (src, dest) =
    if !(lga.steps) mod 2 = 0 then lga.fields else swap lga.fields in
  propagate lga.boundaries src dest;
  collide lga.boundaries dest;
  smoothing lga.block_size lga.av_field dest;
  incr lga.steps;
  dest

let exec lga =
  let rec repeat f n =
    if n = 1 then f () else begin ignore (f ()) ; repeat f (n-1) end
  in
  Mat.iter (fun arr -> Array.fill arr 0 (Array.length arr) 0.0) lga.av_field;
  let field = repeat (fun () -> exec1 lga) lga.period in
  let c = 1.0 /. float (lga.period * lga.block_size * lga.block_size) in
  Mat.replace_all (Array.map (( *. ) c)) lga.av_field; (* normalization *)
  field

let mkconv_av_field f ?dest lga =
  match dest with
  | None -> Mat.map f lga.av_field
  | Some dest -> Mat.replace_all2 (fun _ -> f) dest lga.av_field ; dest

let velocity ?dest lga =
  let velos =
    let c = sqrt 3.0 /. 2.0 in
    [|(1., 0.); (0.5, c); (-0.5, c); (-1., 0.); (-0.5, ~-.c); (0.5, ~-.c)|] in
  let aux cell =
    let x1, y1 = ref 0.0, ref 0.0 in
    for i = 0 to Array.length velos - 1 do (* 7-th item in [cell] is ignored. *)
      let n = Array.unsafe_get cell i in
      let (x2, y2) = Array.unsafe_get velos i in
      x1 := !x1 +. n *. x2;
      y1 := !y1 +. n *. y2
    done;
    (!x1, !y1)
  in
  mkconv_av_field aux ?dest lga

let density_per_cell cell =
  (* 6 for FHP-I, 7 for FHP-II and FHP-III *)
  Array.fold_left (+.) 0.0 cell /. 7.0

let density ?dest lga = mkconv_av_field density_per_cell ?dest lga

let reynolds ?dest lga =
  let aux cell =
    let cs = sqrt (3. /. 7.) in
    let d = density_per_cell cell in
    let g = (7. /. 12.) *. (1. -. 2. *. d) /. (1. -. d) in
    let nu = (1. /. 28.)
             *. (1. /. (d *. (1. -. d)))
             *. (1. /. (1. -. 8. *. d *. (1. -. d) /. 7.))
             -. 1. /. 8. in
    cs *. g /. nu
  in
  mkconv_av_field aux ?dest lga
