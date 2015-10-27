(* Simple fuild simulator by lattice gas automaton

   Copyright (C) 2015 Akinori ABE
   This program is distributed under MIT License.
   See LICENSE.txt for details. *)

open Format

let margin = 10
let height = 400 (* the height of a field for LGA *)
let width = 800 (* the width of a field for LGA *)
let cells_per_block = 20
let pixels_per_block = 20
let period = 3

let barrier x y = x >= 100 && x <= 110 && y >= 140 && y <= 260

let lga =
  let boundary x y =
    if barrier x y then Lga.NoSlip
    else if x < 0 then Lga.Inflow 0.1
    else if x >= width then Lga.Outflow
    else Lga.Cell ((y + height) mod height, (x + width) mod width)
  in
  let init x y = (* particles in the initial field *)
    (if Random.float 1.0 < 0.3 then 0b1_000000 else 0) lor (* stopped *)
    (if Random.float 1.0 < 0.1 then 0b0_000001 else 0) lor (* left *)
    (if Random.float 1.0 < 0.1 then 0b0_000010 else 0) lor (* lower left *)
    (if Random.float 1.0 < 0.1 then 0b0_100000 else 0) (* upper left *)
  in
  Lga.create ~barrier ~boundary ~init
    ~period ~block_size:cells_per_block width height

let simulate draw =
  let open Graphics in
  open_graph "";
  let img =
    Renderer.make_barrier_image ~fgcolor:0x000000 ~barrier
      width height cells_per_block pixels_per_block in
  let img_width = width * pixels_per_block / cells_per_block in
  let img_height = height * pixels_per_block / cells_per_block in
  let rec loop () =
    let x, y = margin, size_y () - margin in
    ignore (Lga.exec lga);
    auto_synchronize false;
    clear_graph ();
    draw x y;
    draw_image img x (y - img_height);
    synchronize ();
    loop ()
  in
  resize_window (img_width + 2 * margin) (img_height + 2 * margin);
  loop ()

(** {2 Rendering engines} *)

let gen_draw_colormap_with_colobar lga =
  let (n_rows, n_cols) = Mat.size lga.Lga.av_field in
  let img_width = n_cols * pixels_per_block in
  let img_height = n_rows * pixels_per_block in
  fun mat x y ->
    Renderer.draw_colormap mat x (y - img_height) pixels_per_block;
    Renderer.draw_colorbar (x + img_width + pixels_per_block) (y - img_height)
      pixels_per_block img_height

let gen_draw_velocity_colormap lga =
  let draw = gen_draw_colormap_with_colobar lga in
  let mat1 = Mat.map (fun _ -> 0.0) lga.Lga.av_field in
  let mat2 = Mat.map (fun _ -> (0.0, 0.0)) lga.Lga.av_field in
  fun x y ->
    ignore (Lga.velocity ~dest:mat2 lga);
    Mat.replace_all2 (fun _ (x, y) -> sqrt (x *. x +. y *. y)) mat1 mat2;
    draw mat1 x y

let gen_draw_density_colormap lga =
  let draw = gen_draw_colormap_with_colobar lga in
  let mat = Mat.map (fun _ -> 0.0) lga.Lga.av_field in
  fun x y -> draw (Lga.density ~dest:mat lga) x y

let gen_draw_reynolds_colormap lga =
  let draw = gen_draw_colormap_with_colobar lga in
  let mat = Mat.map (fun _ -> 0.0) lga.Lga.av_field in
  fun x y -> draw (Lga.reynolds ~dest:mat lga) x y

let gen_draw_velocity_arrowmap lga =
  let (n_rows, _) = Mat.size lga.Lga.av_field in
  let ofsy = n_rows * pixels_per_block in
  let mat = Mat.map (fun _ -> (0.0, 0.0)) lga.Lga.av_field in
  fun x y ->
    ignore (Lga.velocity ~dest:mat lga);
    Renderer.draw_arrowmap mat x (y - ofsy) pixels_per_block

(* Execute a simulator: *)
(* let () = simulate (gen_draw_velocity_colormap lga) *)
(* let () = simulate (gen_draw_density_colormap lga) *)
(* let () = simulate (gen_draw_reynolds_colormap lga) *)
let () = simulate (gen_draw_velocity_arrowmap lga)
