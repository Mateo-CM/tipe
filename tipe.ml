open Graphics
open Voronoi
open Parameters
open Math
open Opti


let trace_diagramme (n : float ) v =
  let dcl = tw*gw/2 - int_of_float (0.5 *. n) in
  for i = 0 to v.na -1 do
    fill_circle (int_of_float (n*.((fst v.polygone_d.(i)))) + dcl) 
    (int_of_float (n*.(snd v.polygone_d.(i)) ) + dcl) 3;
    fill_circle (int_of_float (n*.((fst v.polygone_g.(i)))) + dcl) 
    (int_of_float (n*.(snd v.polygone_g.(i))) + dcl) 3
  done;
  for j = 0 to v.na - 1 do
    let epsilon = (0.001,0.001) in
    if not(epsilon_comp v.debut_a.(j) (1000.,1000.) epsilon) then begin
    moveto ((int_of_float (n *. (fst v.debut_a.(j)))) + dcl) 
    ((int_of_float (n *. (snd v.debut_a.(j))))+ dcl);
    lineto (int_of_float (n *. (fst v.fin_a.(j)))+ dcl) 
    ((int_of_float (n  *. (snd v.fin_a.(j)))) + dcl)
    end
  done

let main = 
  open_graph (Printf.sprintf " %dx%d" (tw*gw) (th*gh));
  set_window_title "TIPE";
    set_color (rgb 0 0 0);
    let n = float_of_int (tw*gw)*.0.04 in
    let v = (base_trig ()) in
    ajoute_point v (0.01,0.94);
    ajoute_point v (0.73,0.34);
    trace_diagramme n v;
    let wait = 1000. in
    let time = Sys.time () in
    while Sys.time () < time +. wait do
      ()
    done;
  close_graph ()

