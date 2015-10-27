(* Simple fuild simulator by lattice gas automaton

   Copyright (C) 2015 Akinori ABE
   This program is distributed under MIT License.
   See LICENSE.txt for details. *)

(** Returns an image of a given barrier. *)
let make_barrier_image
    ?(fgcolor = Graphics.foreground) ?(bgcolor = Graphics.transp)
    ~barrier width height cells_per_block pixels_per_block =
  let scale n = n * pixels_per_block / cells_per_block in
  let invscale n = n * cells_per_block / pixels_per_block in
  let pixel i j =
    if barrier (invscale j) (invscale i) then fgcolor else bgcolor
  in
  Mat.init (scale height) (scale width) pixel
  |> Graphics.make_image

(** {2 Arrow map} *)

(** [draw_arrow x1 y1 x2 y2 arrow_head_len] draws an arrow from [(x1, y1)] to
    [(x2, y2)]. *)
let draw_arrow =
  let pi = 3.14159265358979 in
  let rho = pi /. 6. in (* angle of arrows *)
  let sin_rho, cos_rho = sin rho, cos rho in
  fun x1 y1 x2 y2 head ->
    if abs_float (x1 -. x2) < 1e-6 && abs_float (y1 -. y2) < 1e-6
    then Graphics.plot (truncate x1) (truncate y1)
    else begin
      let theta = atan2 (y2 -. y1) (x2 -. x1) in
      let sin_theta, cos_theta = sin theta, cos theta in
      let ax1, ax2 = cos_theta *. cos_rho, sin_theta *. sin_rho in
      let ay1, ay2 = sin_theta *. cos_rho, cos_theta *. sin_rho in
      Graphics.moveto (truncate x1) (truncate y1);
      Graphics.lineto (truncate x2) (truncate y2);
      let dx = head *. (ax1 +. ax2) in
      let dy = head *. (ay1 -. ay2) in
      Graphics.moveto (truncate (x2 -. dx)) (truncate (y2 -. dy));
      Graphics.lineto (truncate x2) (truncate y2);
      let dx = head *. (ax1 -. ax2) in
      let dy = head *. (ay1 +. ay2) in
      Graphics.lineto (truncate (x2 -. dx)) (truncate (y2 -. dy))
    end

(** [draw_arrowmap mat x y pixels_per_block] draws a colormap with lower left
    corner at [(x, y)]. *)
let draw_arrowmap mat x0 y0 pixels_per_block =
  let x0, y0 = x0 + pixels_per_block / 2, y0 + pixels_per_block / 2 in
  let scale =
    let aux acc (vx, vy) = max (max acc (abs_float vx)) (abs_float vy) in
    let maxv = Mat.fold_left aux (-1.) mat in
    float pixels_per_block /. maxv in
  let aux i j (vx, vy) =
    let y1 = float (y0 + pixels_per_block * i) in
    let x1 = float (x0 + pixels_per_block * j) in
    let y2 = y1 +. vy *. scale in
    let x2 = x1 +. vx *. scale in
    draw_arrow x1 y1 x2 y2 (float pixels_per_block *. 0.3)
  in
  Mat.iteri aux mat

(** {2 Color map} *)

let mkcolor x =
  let f color_from color_range val_from val_range n base_color =
    let y = (x -. val_from) /. val_range *. color_range +. color_from in
    (truncate (floor (y +. 0.5)) lsl n) lor base_color
  in
  if x > 0.8 then f 255. (-127.) 0.8 0.2 16 0x000000 (* Maroon--Red *)
  else if x > 0.6 then f 255. (-255.) 0.6 0.2 8 0xff0000 (* Red--Yellow *)
  else if x > 0.5 then f 0. 255. 0.5 0.1 16 0x00ff00 (* Yellow--Green *)
  else if x > 0.3 then f 255. (-255.) 0.3 0.2 0 0x00ff00 (* Green-Cyan *)
  else if x > 0.1 then f 0. 255. 0.1 0.2 8 0x0000ff (* Cyan-Blue *)
  else f 128. 127. 0.0 0.1 0 0x000000 (* Blue-Navy *)

(** [draw_colorbar ?thickness x y width height] draws a colorbar with lower left
    corner at [(x, y)]. *)
let draw_colorbar ?(thickness = 1) x y width height =
  let n = height / thickness - 1 in
  for i = 0 to n do
    let y' = y + i * thickness in
    Graphics.set_color (mkcolor (float i /. float n));
    Graphics.fill_rect x y' width thickness
  done

(** [draw_colormap mat x y pixels_per_block] draws a colormap with lower left
    corner at [(x, y)]. Each value in [mat] is drawn as a square of size
    [pixels_per_block]. *)
let draw_colormap mat x0 y0 pixels_per_block =
  let max_val = Mat.fold_left max (~-. infinity) mat in
  let aux i j abs =
    let c = mkcolor (abs /. max_val) in
    let y = y0 + pixels_per_block * i in
    let x = x0 + pixels_per_block * j in
    Graphics.set_color c;
    Graphics.fill_rect x y pixels_per_block pixels_per_block
  in
  Mat.iteri aux mat
