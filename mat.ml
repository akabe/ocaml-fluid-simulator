(* Simple fuild simulator by lattice gas automaton

   Copyright (C) 2015 Akinori ABE
   This program is distributed under MIT License.
   See LICENSE.txt for details. *)

(** Two-dimensional arrays. *)

type 'a t = 'a array array

let make = Array.make_matrix

let init m n f = Array.init m (fun i -> Array.init n (f i))

let size x =
  let m = Array.length x in
  if m = 0 then (0, 0) else (m, Array.length (Array.unsafe_get x 0))

let unsafe_get x i j = Array.unsafe_get (Array.unsafe_get x i) j
let unsafe_set x i j y = Array.unsafe_set (Array.unsafe_get x i) j y

let fill x v =
  let _, n = size x in
  Array.iter (fun row -> Array.fill row 0 n v) x

let replace f x i j = unsafe_set x i j (f x.(i).(j))

let replace_all f x =
  let m, n = size x in
  for i = 0 to m - 1 do
    let rx = Array.unsafe_get x i in
    for j = 0 to n - 1 do Array.unsafe_set rx j (f (Array.unsafe_get rx j)) done
  done

let replace_all2 f x y =
  let m, n = size x in
  for i = 0 to m - 1 do
    let rx = Array.unsafe_get x i in
    let ry = Array.unsafe_get y i in
    for j = 0 to n - 1 do
      f (Array.unsafe_get rx j) (Array.unsafe_get ry j)
      |> Array.unsafe_set rx j
    done
  done

let map f x =
  let m, n = size x in
  init m n (fun i j -> f (unsafe_get x i j))

let fold_left f init x =
  let m, n = size x in
  let acc = ref init in
  for i = 0 to m - 1 do
    let rx = Array.unsafe_get x i in
    for j = 0 to n - 1 do
      acc := f !acc (Array.unsafe_get rx j)
    done
  done;
  !acc

let iteri f x =
  let m, n = size x in
  for i = 0 to m - 1 do
    let rx = Array.unsafe_get x i in
    for j = 0 to n - 1 do
      f i j (Array.unsafe_get rx j)
    done
  done

let iter f = iteri (fun _ _ -> f)

let iteri2 f x y =
  let m, n = size x in
  for i = 0 to m - 1 do
    let rx = Array.unsafe_get x i in
    let ry = Array.unsafe_get y i in
    for j = 0 to n - 1 do
      f i j (Array.unsafe_get rx j) (Array.unsafe_get ry j)
    done
  done
