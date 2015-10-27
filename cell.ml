(* Simple fuild simulator by lattice gas automaton

   Copyright (C) 2015 Akinori ABE
   This program is distributed under MIT License.
   See LICENSE.txt for details. *)

(** Cells in Lattice Gas Automata

{v
      3   2
       \ /
   4 -- 7 -- 1      0b[7]_[6][5][4][3][2][1]
      /  \
     5    6
v} *)

type t = int

(** [rotr m n] rotate-shifts the lower 6 bits in [m] to the right by [n] bits.
    The top bit in [m] is preserved, e.g., [rotr 0b1_001010 2] is [0b1_100010].
    This behavior corresponds to clockwise rotation. *)
let rotr m n =
  assert(n >= 0 && n <= 6);
  let k = 6 - n in
  (m land 0b1000000) lor (* the top 7th bit *)
  ((m land (0b111111 lsr k)) lsl k) lor (* carry bits *)
  ((m land 0b111111) lsr n)

(** [rotl m n] rotate-shifts the lower 6 bits in [m] to the left by [n] bits.
    The top bit in [m] is preserved, e.g., [rotl 0b1_001010 2] is [0b1_101000].
    This behavior corresponds to counterclockwise rotation. *)
let rotl m n = rotr m (6 - n)
