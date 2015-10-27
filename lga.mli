(* Simple fuild simulator by lattice gas automaton

   Copyright (C) 2015 Akinori ABE
   This program is distributed under MIT License.
   See LICENSE.txt for details. *)

type boundary =
  | Cell of int * int (** Not boundary, connected to a cell (y, x) *)
  | NoSlip (** No-slip boundary condition *)
  | Inflow of float (** Occurrence of particles with a probability *)
  | Outflow (** Disappearance of particles *)

type t =
  {
    period : int;
    block_size : int;
    steps : int ref; (** # of steps that LGA is executed. *)
    fields : Cell.t Mat.t * Cell.t Mat.t;
    boundaries : boundary array option Mat.t;
    av_field : float array Mat.t; (** averaged field *)
  }

(** Create a LGA simulator.
    @param barrier [barrier x y] returns [true] if [(x, y)] is in barrier(s).
    @param boundary [boundary x y] returns boundary condition of a cell
    ([(x, y)] is NOT a coordinate of the cell).
    @param init [init x y] returns the initial state of cell [(x, y)]. *)
val create :
  barrier:(int -> int -> bool) ->
  boundary:(int -> int -> boundary) ->
  init:(int -> int -> Cell.t) ->
  block_size:int ->
  period:int -> int -> int -> t

val exec : t -> Cell.t Mat.t

(** Computes velocity vectors. *)
val velocity : ?dest:(float * float) Mat.t -> t -> (float * float) Mat.t

(** Computes density per cell. *)
val density : ?dest:float Mat.t -> t -> float Mat.t

(** Computs Reynolds coefficients (not Reynolds number). *)
val reynolds : ?dest:float Mat.t -> t -> float Mat.t
