open Parameters
open Math

let voronoi_copy v =
  {
    na = v.na;
    np = v.np;
    indice_a = Hashtbl.copy v.indice_a;
    indice_p = Hashtbl.copy v.indice_p;
    polygone_d = Array.copy v.polygone_d;
    polygone_g = Array.copy v.polygone_g;
    debut_a = Array.copy v.debut_a;
    fin_a = Array.copy v.fin_a;
    h_precedent = Array.copy v.h_precedent;
    ah_precedent = Array.copy v.ah_precedent;
    h_suivant = Array.copy v.h_suivant;
    ah_suivant = Array.copy v.ah_suivant;
    
    arete_autour_p = Array.copy v.arete_autour_p;
  }


let trouve_poly v s =
  let plus_proche_p = ref v.polygone_g.(0) in  
  let dist_min = ref (dist s !plus_proche_p) in
  if dist s v.polygone_d.(0) < !dist_min
  then begin 
    dist_min := dist s v.polygone_d.(0);
    plus_proche_p := v.polygone_d.(0)
  end;
  for i = 1 to v.na -1 do
    let a = dist s v.polygone_g.(i) in
    if a < !dist_min
    then begin
       dist_min := a; plus_proche_p := v.polygone_g.(i)
    end;
    let b = dist s v.polygone_d.(i) in
    if b < !dist_min
    then begin
      dist_min := b; plus_proche_p := v.polygone_d.(i)
    end
  done;
  (!plus_proche_p, (Hashtbl.find v.indice_p !plus_proche_p))

  let ajoute_point_struct v p a_l =
    let n_p = v.np + 1 in
    Hashtbl.add v.indice_p p (n_p -1);
    let t = ref [||] in
    let n = Array.length v.arete_autour_p in
    if n_p < n
    then t := Array.make n []
    else t := Array.make (2*n) [];
    for i = 0 to (n_p-2) do
        !t.(i) <- v.arete_autour_p.(i)
    done;
    !t.(n_p - 1) <- a_l;
    v.np <- n_p;
    v.arete_autour_p <- !t

let ajoute_arete_struct v a g d =
  let n_a = v.na +1 in
  Printf.printf "indice : %d\n" (n_a-1);flush stdout;
  Hashtbl.add v.indice_a a (n_a -1);
  let polygone_d_r = ref [||] in
  let polygone_g_r = ref [||] in 
  let debut_a_r = ref [||] in
  let fin_a_r = ref [||] in
  let h_precedent_r = ref [||] in
  let ah_precedent_r = ref [||] in
  let h_suivant_r = ref [||]  in
  let ah_suivant_r = ref [||] in
  if Array.length v.polygone_d < n_a 
  then begin
        let polygone_d = Array.make (2*n_a) (0.0,0.0) in
        let polygone_g = Array.make (2*n_a) (0.0,0.0) in 
        let debut_a = Array.make (2*n_a) (0.0,0.0) in
        let fin_a = Array.make (2*n_a) (0.0,0.0) in
        let h_precedent = Array.make (2*n_a) (Some ((0.0,0.0),(0.0,0.0))) in
        let ah_precedent = Array.make (2*n_a) (Some ((0.0,0.0),(0.0,0.0))) in
        let h_suivant = Array.make (2*n_a) (Some ((0.0,0.0),(0.0,0.0))) in
        let ah_suivant = Array.make (2*n_a) (Some ((0.0,0.0),(0.0,0.0))) in
        for i = 0 to (n_a -2 ) do
          polygone_d.(i) <- v.polygone_d.(i);
          polygone_g.(i) <- v.polygone_g.(i);
          debut_a.(i) <- v.debut_a.(i); 
          fin_a.(i) <- v.fin_a.(i);
          h_precedent.(i) <- v.h_precedent.(i);
          ah_precedent.(i) <- v.ah_precedent.(i);
          h_suivant.(i) <- v.h_suivant.(i);
          ah_suivant.(i)<- v.ah_suivant.(i);
      done ;
      polygone_d_r := polygone_d;
      polygone_g_r := polygone_g; 
      debut_a_r := debut_a;
      fin_a_r := fin_a;
      h_precedent_r := h_precedent;
      ah_precedent_r := ah_precedent;
      h_suivant_r := h_suivant;
      ah_suivant_r := ah_suivant;
      end
  else begin
      polygone_d_r := v.polygone_d;
      polygone_g_r := v.polygone_g; 
      debut_a_r := v.debut_a;
      fin_a_r := v.fin_a;
      h_precedent_r := v.h_precedent;
      ah_precedent_r := v.ah_precedent;
      h_suivant_r := v.h_suivant;
      ah_suivant_r := v.ah_suivant;
      end;
    !polygone_d_r.(n_a -1) <- d;
    !polygone_g_r.(n_a -1) <- g;
    !debut_a_r.(n_a - 1) <- fst a;
    !fin_a_r.(n_a -1) <- snd a;
  let epsilon = (0.001,0.001) in
  for i = 0 to n_a - 2 do
    if !fin_a_r.(i) = fst a then begin

      if ordre_triangle (fst a) (snd a) (!debut_a_r.(i)) then begin
        
        !ah_suivant_r.(i) <- Some a;
        !ah_precedent_r.(n_a-1) <- Some (!debut_a_r.(i),!fin_a_r.(i));
        !h_suivant_r.(n_a -1) <- Some (!debut_a_r.(i),!fin_a_r.(i));
        !h_precedent_r.(i) <- Some a
      end
      
      else begin

        !ah_suivant_r.(n_a -1) <- Some (!debut_a_r.(i),!fin_a_r.(i));
        !ah_precedent_r.(i) <- Some a;
        !h_suivant_r.(n_a -1) <- Some a;
        !h_precedent_r.(i) <- Some (!debut_a_r.(i),!fin_a_r.(i))
      end
      end
      
    else 
      if !debut_a_r.(i) = fst a then begin

        if ordre_triangle (fst a) (snd a) (!fin_a_r.(i)) then begin
          
          !ah_suivant_r.(i) <- Some a;
          !ah_precedent_r.(n_a-1) <- Some (!debut_a_r.(i),!fin_a_r.(i));
          !h_suivant_r.(n_a -1) <- Some (!debut_a_r.(i),!fin_a_r.(i));
          !h_precedent_r.(i) <- Some a
        end
        
        else begin
  
          !ah_suivant_r.(n_a -1) <- Some (!debut_a_r.(i),!fin_a_r.(i));
          !ah_precedent_r.(i) <- Some a;
          !h_suivant_r.(n_a -1) <- Some a;
          !h_precedent_r.(i) <- Some (!debut_a_r.(i),!fin_a_r.(i))
        end
        end
    
    else
      if !fin_a_r.(i) = snd a then begin

        if ordre_triangle (fst a) (snd a) (!debut_a_r.(i)) then begin
          
          !ah_suivant_r.(n_a -1) <- Some (!debut_a_r.(i),!fin_a_r.(i));
          !ah_precedent_r.(i) <- Some a;
          !h_suivant_r.(n_a -1) <- Some a;
          !h_precedent_r.(i) <- Some (!debut_a_r.(i),!fin_a_r.(i))
        end
        
        else begin
  
          !ah_suivant_r.(i) <- Some a;
          !ah_precedent_r.(n_a-1) <- Some (!debut_a_r.(i),!fin_a_r.(i));
          !h_suivant_r.(n_a -1) <- Some (!debut_a_r.(i),!fin_a_r.(i));
          !h_precedent_r.(i) <- Some a
        end
        end
      
    else
      if !debut_a_r.(i) = snd a then begin

        if ordre_triangle (fst a) (snd a) (!fin_a_r.(i)) then begin
          
          !ah_suivant_r.(n_a -1) <- Some (!debut_a_r.(i),!fin_a_r.(i));
          !ah_precedent_r.(i) <- Some a;
          !h_suivant_r.(n_a -1) <- Some a;
          !h_precedent_r.(i) <- Some (!debut_a_r.(i),!fin_a_r.(i))
        end
        
        else begin
  
          !ah_suivant_r.(i) <- Some a;
          !ah_precedent_r.(n_a-1) <- Some (!debut_a_r.(i),!fin_a_r.(i));
          !h_suivant_r.(n_a -1) <- Some (!debut_a_r.(i),!fin_a_r.(i));
          !h_precedent_r.(i) <- Some a
        end
        end
  done;
  v.na <- n_a;
  v.polygone_d <- !polygone_d_r;
  v.polygone_g <- !polygone_g_r;
  v.debut_a <- !debut_a_r;
  v.fin_a <- !fin_a_r;
  v.h_precedent <- !h_precedent_r;
  v.ah_precedent <- !ah_precedent_r;
  v.h_suivant <- !h_suivant_r;
  v.ah_suivant <- !ah_suivant_r;
  v.arete_autour_p <- v.arete_autour_p
 
let erase v a =
  match a with
  |None -> ()
  |Some x ->
  let epsilon = (0.0001,0.0001) in 
  if not ((epsilon_comp (fst x) (0.,0.) epsilon) && (epsilon_comp (snd x) (0.,0.) epsilon))
  then begin
  try
  let temp = Hashtbl.find v.indice_a x in
  v.debut_a.(temp) <- (1000.,1000.);
  v.fin_a.(temp) <- (1000.,1000.)
  with Not_found -> (
  let temp = Hashtbl.find v.indice_a ((snd x),(fst x)) in
  v.debut_a.(temp) <- (1000.,1000.);
  v.fin_a.(temp) <- (1000.,1000.))
  end

let coupe_bas wj a_couperj newj =
  Printf.printf "bas\n"; flush stdout;
  if snd (fst (snd wj)) > snd (snd (snd wj))
    then begin
      a_couperj := (snd (snd wj),fst wj);
      newj := (fst wj,(fst (snd wj)))
    end
    else begin
      newj := (fst wj,snd (snd wj));
      a_couperj := (fst wj,(fst (snd wj)))
    end

let coupe_haut wj a_couperj newj =
  Printf.printf "haut\n"; flush stdout;
  if snd (fst (snd wj)) > snd (snd (snd wj))
    then begin
      a_couperj := (fst (snd wj),fst wj);
      newj := ((snd (snd wj)),fst wj)
    end
    else begin
      newj := (fst (snd wj),fst wj);
      a_couperj := (fst wj,(snd (snd wj)))
    end

let coupe_droite wj a_couperj newj =
  Printf.printf "droite\n"; flush stdout;
  if fst (fst (snd wj)) > fst (snd(snd wj))
  then begin
    a_couperj := (fst (snd wj),fst wj);
    newj := ((snd (snd wj)),fst wj)
  end
  else begin
    newj := (fst (snd wj),fst wj);
    a_couperj := (fst wj,(snd (snd wj)))
  end

let coupe_gauche wj a_couperj newj =
  Printf.printf "gauche\n"; flush stdout;
  if fst (fst (snd wj)) > fst (snd(snd wj))
  then begin
    a_couperj := (snd (snd wj),fst wj);
    newj := (fst wj,(fst (snd wj)))
  end
  else begin
    newj := (fst wj,snd (snd wj));
    a_couperj := (fst wj,(fst (snd wj)))
  end


let pente wh wi =
  let a1,_ = equation (wh,wi) in
  a1

let haut wi wj =
  snd (fst wi) < snd(fst wj)

let bas wi wj =
  snd (fst wi) > snd (fst wj)


let tronque wh wi wj ac n  =
  
  if haut wh wi && haut wi wj 
  then coupe_gauche wi ac n 
  
  else if haut wh wi && bas wi wj
  then coupe_bas wi ac n 

  else if bas wh wi && haut wi wj 
  then coupe_haut wi ac n

  else if bas wh wi && bas wi wj 
  then coupe_droite wi ac n



let efface_arete v wh wi wj a p =
  let epsilon = (0.001,0.001) in
  let a_couperj = ref ((0.,0.),(0.,0.)) in
  let newj = ref (snd wi) in
  if epsilon_comp (0.,snd (fst wi)) (0.,snd (fst wj)) epsilon && fst (fst wi) > fst(fst wj)
  then begin
    if fst (fst wh) > fst(fst wi)
    then coupe_bas wi a_couperj newj
    else coupe_haut wi a_couperj newj
  end;
  let x,y = (pente (fst wh) (fst wi)),(pente (fst wi) (fst wj)) in
  if Float.is_nan(pente (fst (snd wi)) (snd (snd wi)) ) 
    && (fst(fst wh) < fst(fst wi) && fst(fst wj) > fst(fst wi) 
  || fst(fst wh) > fst(fst wi) && fst(fst wj) < fst(fst wi)) then
  tronque wh wi wj a_couperj newj
  else
  if x > 0. && y < 0. || x < 0. && y > 0.
  then begin if (snd(fst wh) < snd(fst wi) && snd(fst wj) < snd(fst wi) 
    || snd(fst wh) > snd(fst wi) && snd(fst wj) > snd(fst wi)) 
  then begin if((Float.min x y) > (pente (fst (snd wi)) (snd (snd wi))) 
    || (Float.max x y) < (pente (fst (snd wi)) (snd(snd wi))))
  then begin 
    tronque wh wi wj a_couperj newj;
    Printf.printf "teeeeest2\n";flush stdout
  end
  end
  else if ((Float.min x y) < (pente (fst (snd wi)) (snd (snd wi))) 
    && (Float.max x y) > (pente (fst (snd wi)) (snd(snd wi)))) 
    then tronque wh wi wj a_couperj newj
  end
  else if x > 0. && y > 0. || x < 0. && y < 0. then begin
    if not(est_aigu wh wi wj) 
    then begin
    if ((Float.min x y) > (pente (fst (snd wi)) (snd (snd wi))) 
      || (Float.max x y) < (pente (fst (snd wi)) (snd(snd wi)))) 
       then begin
       tronque wh wi wj a_couperj newj;
       Printf.printf "teeeeest4\n";flush stdout
       end
    end
    else if ((Float.min x y) < (pente (fst (snd wi)) (snd (snd wi))) 
      && (Float.max x y) > (pente (fst (snd wi)) (snd(snd wi)))) 
      then begin
        tronque wh wi wj a_couperj newj;
        Printf.printf "teeeeest4\n";flush stdout
        end

    end;
  if x > 0. && y < 0. || x < 0. && y > 0.
  then if (fst(fst wh) < fst(fst wi) && fst(fst wj) < fst(fst wi) 
    || fst(fst wh) > fst(fst wi) && fst(fst wj) > fst(fst wi)) then
    tronque wh wi wj a_couperj newj;
  
    
  v.arete_autour_p.(Hashtbl.find v.indice_p a) <- 
  List.filter (fun i -> i <> snd wi) v.arete_autour_p.(Hashtbl.find v.indice_p a);
  let flag = ref false in
  for i = 0 to v.na - 1 do
    if v.ah_precedent.(i) = Some (snd wi)
    then begin
      v.ah_precedent.(i) <- None;
    end;
    if v.h_precedent.(i) = Some (snd wi) 
    then begin
      flag := true;
      if not( epsilon_comp v.debut_a.(i) v.debut_a.(Hashtbl.find v.indice_a (snd wj)) epsilon 
      && epsilon_comp v.fin_a.(i) v.fin_a.(Hashtbl.find v.indice_a (snd wj)) epsilon)
      then erase v (Some (v.debut_a.(i),v.fin_a.(i)))
      else flag := false
    end;
    if v.h_precedent.(i) = Some (snd wi)
    then begin
      v.h_precedent.(i) <- None;
    end;
    if v.ah_suivant.(i) = Some (snd wi)
    then  begin
      v.ah_suivant.(i) <- None;
    end;
    if v.h_suivant.(i) = Some (snd wj)
    then begin
      flag := false
    end;
    if !flag 
    then erase v v.h_suivant.(i) 
  done;
  erase v (Some (snd wi));
  (try
  ajoute_arete_struct v !newj 
  v.polygone_g.(Hashtbl.find v.indice_a (snd wi)) v.polygone_d.(Hashtbl.find v.indice_a (snd wi));
  with Not_found -> ajoute_arete_struct v !newj 
  v.polygone_g.(Hashtbl.find v.indice_a ((snd (snd wi)),fst(snd wi))) 
  v.polygone_d.(Hashtbl.find v.indice_a ((snd (snd wi)),fst(snd wi))););
  v.arete_autour_p.(Hashtbl.find v.indice_p a) <- 
  !newj::v.arete_autour_p.(Hashtbl.find v.indice_p a)
  


let affichewi wi : unit =
  Printf.printf "\t((%f,%f),  ((%f,%f), (%f,%f)))\n" (fst (fst wi)) (snd (fst wi)) 
  (fst (fst (snd wi))) (snd (fst (snd wi))) (fst (snd (snd wi))) (snd (snd (snd wi)));
  flush stdout;
  ()

let rec affichelist l = 
  match l with 
  | [] -> Printf.printf "\n"
  | t::q -> 
      Printf.printf "((%f,%f), (%f,%f))\n" (fst (fst t)) (snd (fst t)) (fst (snd t)) (snd (snd t));
      flush stdout;
      affichelist q
