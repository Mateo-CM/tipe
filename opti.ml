open Parameters
open Math
open Donnee
open Voronoi

let rec near_neigh v p pi =
  let l = ref [] in
  List.iter(fun i -> let a = v.polygone_d.(Hashtbl.find v.indice_a i) in 
                      if a <> pi then l := a::!l
                       else l := v.polygone_d.(Hashtbl.find v.indice_a i)::!l ) 
                       v.arete_autour_p.(Hashtbl.find v.indice_p pi);
  let d = ref (dist p pi) in
  let p_near = ref pi in
  List.iter (fun i -> if dist p i < !d then begin d:= dist p i; p_near := i end) !l;
  if !p_near = pi 
  then pi
  else near_neigh v p !p_near

let ajoute_point_2 v p pi =
  let p' = near_neigh v p pi in
  let la = v.arete_autour_p.(Hashtbl.find v.indice_p p')  in
  let a_l = ref [] in
  let med = mediatrice p p' in
  let mil = milieu p p' in
  let w1,w2 = prochain_point_candidat la med mil in
  let p_actu = ref !w2 in
  let goal = ref !w1 in
  let deux = ref !w2 in 
  let sens = ref false in
  let p_prec = ref p' in
  let v' = voronoi_copy v in
  let wh = ref !w1 in
  if snd p' > snd p
  then begin 
        p_actu :=!w1;
        deux := !w1;
        goal := !w2;
        wh := !w2;
        sens := true;
        ajoute_arete_struct v' (fst !w2,fst !w1) p p';
        a_l := (fst !w2,fst !w1)::!a_l;
        v'.arete_autour_p.(Hashtbl.find v.indice_p p') <- 
        (fst !w1, fst !w2)::(v'.arete_autour_p.(Hashtbl.find v.indice_p p'))
        end
  else begin
        ajoute_arete_struct v' (fst !w1,fst !w2) p p';
        v'.arete_autour_p.(Hashtbl.find v.indice_p p') <- 
        (fst !w2, fst !w1)::(v'.arete_autour_p.(Hashtbl.find v.indice_p p'));
        a_l := (fst !w1,fst !w2)::!a_l
  end;
  let deux' = ref !w1 in
  let a = ref p' in
  let epsilon = (0.001,0.001) in
  while not (epsilon_comp (fst !p_actu) (fst !goal) epsilon)  do
      if !wh = !deux
      then deux' := !p_actu;
      let temp = !a in
      a:= v.polygone_d.(Hashtbl.find v.indice_a (snd !p_actu));
      if epsilon_comp temp !a epsilon then 
      a := v.polygone_g.(Hashtbl.find v.indice_a (snd !p_actu));
      p_prec := temp;  
      let med' = mediatrice p !a in
      let x,y = !a in
      Printf.printf "a : %f,%f\n" x y;flush stdout;
      let la' = v.arete_autour_p.(Hashtbl.find v.indice_p !a) in
      let mil' = milieu p !a in
      let (wi,wj)= prochain_point_candidat la' med' mil' in
      if (fst (fst !wi) < fst ( fst !p_actu) && fst (fst !wj) < fst ( fst !p_actu)) 
          || (fst (fst !wi) > fst ( fst !p_actu) && fst (fst !wj) > fst ( fst !p_actu))
      then begin 
            if dist (fst !wj) (fst !p_actu) < dist (fst !wi) (fst !p_actu)
            then begin 
                let temp = !wj in 
                wj := !wi;
                wi := temp
            end;
          end; 
      if epsilon_comp (fst !wi) (200.,200.) epsilon
      then wi := !wj;
      if epsilon_comp (fst !wj) (200.,200.) epsilon
      then wj := !wi;
      if ordre_triangle p (fst !p_actu) (fst !wi)  || epsilon_comp (fst !goal) (fst !wi) epsilon 
      then begin 
          affichewi !wi;
          efface_arete v' !wh !p_actu !wi temp p;
          ajoute_arete_struct v' (fst !p_actu,fst !wi) p !a;
          v'.ah_suivant.(Hashtbl.find v'.indice_a (fst !p_actu,fst !wi )) <- Some (fst !wh,fst !p_actu);
          v'.h_suivant.(Hashtbl.find v'.indice_a (fst !wh,fst !p_actu )) <- Some (fst !p_actu,fst !wi );
          wh:= !p_actu ; 
          a_l:=(fst !p_actu, fst !wi)::!a_l;
          v'.arete_autour_p.(Hashtbl.find v'.indice_p !a) <- 
          (fst !p_actu, fst !wi)::(v'.arete_autour_p.(Hashtbl.find v'.indice_p !a));
          p_actu := !wi;
          end
      else
          begin
          affichewi !wj;
          efface_arete v' !wh !p_actu !wj temp p;
          ajoute_arete_struct v' (fst !p_actu,fst !wj) !a p;
          v'.ah_suivant.(Hashtbl.find v'.indice_a (fst !p_actu,fst !wj )) <- Some (fst !wh,fst !p_actu);
          v'.h_suivant.(Hashtbl.find v'.indice_a (fst !wh,fst !p_actu )) <- Some (fst !p_actu,fst !wj );
          wh := !p_actu;
          let test' = fst !p_actu in
          a_l:=(fst !p_actu,fst !wj)::!a_l;
          v'.arete_autour_p.(Hashtbl.find v'.indice_p !a) <- 
          (fst !p_actu, fst !wj)::(v'.arete_autour_p.(Hashtbl.find v'.indice_p !a));
          p_actu := !wj;
          end
      done;
  efface_arete v' !wh !p_actu !deux' !a p;
  ajoute_point_struct v' p !a_l;
  v.na <- v'.na;
  v.np <- v'.np;
  v.indice_a <- v'.indice_a;
  v.indice_p <- v'.indice_p;
  v.polygone_d <- v'.polygone_d;
  v.polygone_g <- v'.polygone_g;
  v.debut_a <- v'.debut_a;
  v.fin_a <- v'.fin_a;
  v.h_precedent <- v'.h_precedent;
  v.h_suivant <- v'.h_suivant;
  v.ah_precedent <- v'.ah_precedent;
  v.ah_suivant <- v'.ah_suivant;
  v.arete_autour_p <- v'.arete_autour_p

let rec search_pow_4 powk n k  =
  if abs(powk - n) < abs(4*powk - n)
  then powk
  else search_pow_4 (4*powk) n (k+1)

let cherche_haut_gauche l h =
  let p = ref (List.hd l) in
  List.iter (fun i -> if (fst i) < (fst !p ) && Float.abs((fst !p) -. (fst i)) > h then p := i;
                      if (snd i) > (snd !p ) && Float.abs((snd i) -. (snd !p)) > h then p := i) l;
  !p
let rec create_tree v_p pow_4 org =
  let h = 1./.float_of_int(pow_4) in
  if v_p <> []
  then begin
    let a = cherche_haut_gauche v_p h in
    let h_g = List.filter(fun i -> (fst i) < ((fst org) +. h*.float_of_int(pow_4/4)) 
    && (snd i) >  ((snd org) +. h*.float_of_int(pow_4/4))) v_p in
    let h_d = List.filter(fun i -> (fst i) > ((fst org) +. h*.float_of_int(pow_4/4)) 
    && (snd i) >  ((snd org) +. h*.float_of_int(pow_4/4))) v_p in
    let b_g = List.filter(fun i -> (fst i) < ((fst org) +. h*.float_of_int(pow_4/4)) 
    && (snd i) <  ((snd org) +. h*.float_of_int(pow_4/4))) v_p in
    let b_d = List.filter(fun i -> (fst i) > ((fst org) +. h*.float_of_int(pow_4/4)) 
    && (snd i) <  ((snd org) +. h*.float_of_int(pow_4/4))) v_p in
    Noeud (a,create_tree h_g (pow_4/4) ((fst org),((snd org) +. h*.float_of_int(pow_4/4))),
    create_tree h_d (pow_4/4) (((fst org) +. h*.float_of_int(pow_4/4)),((snd org) +. h*.float_of_int(pow_4/4)))
    ,create_tree b_g (pow_4/4) org,
    create_tree b_d (pow_4/4) ((fst org) +. h*.float_of_int(pow_4/4),(snd org)))
  end
  else Nil 

let rec parcours v arbre =
  match arbre with
  |Nil -> ()
  |Noeud(p,eg,g,d,ed) -> match eg with
                        |Nil -> ()
                        |Noeud(p1,_,_,_,_) -> (ajoute_point_2 v p1 p; parcours v eg);
                        match g with 
                        |Nil -> ()
                        |Noeud(p2,_,_,_,_) -> (ajoute_point_2 v p2 p; parcours v g;);
                        match d with
                        |Nil -> ()
                        |Noeud(p3,_,_,_,_) -> (ajoute_point_2 v p3 p; parcours v d;);
                        match ed with 
                        |Nil -> ()
                        |Noeud(p4,_,_,_,_) -> (ajoute_point_2 v p4 p;parcours v ed)