open Math
open Donnee
open Parameters

let base_trig () =
    let p = Hashtbl.create 3 in
    Hashtbl.add p p1 0;
    Hashtbl.add p p2 1;
    Hashtbl.add p p3 2;
    let s = Hashtbl.create 4 in
    Hashtbl.add s (0.0,0.0) 0;
    Hashtbl.add s (milieu p1 p2)  1;
    Hashtbl.add s (milieu p1 p3)  2;
    Hashtbl.add s (milieu p2 p3)  3;
    let a = Hashtbl.create 3 in
    
    let n = 10. in

    let a1',b1 = mediatrice p1 p2 in
    let a2',b2 = mediatrice p1 p3 in

    let a1 = ((0.5,a1'*.0.5+.b1),(-.10.+.0.5,a1'*.(-.10.+.0.5)+.b1)) in
    let a2 = ((0.5,a2'*.0.5+.b2),(10.+.0.5,a2'*.(10.+.0.5)+.b2)) in
    let a3 = ((0.5,a2'*.0.5+.b2), (n*.(fst (milieu p2 p3) -. 0.5) 
    +. 0.5, n*.(snd (milieu p2 p3) -. 0.5) +. 0.5)) in

    Hashtbl.add a a1 0;
    Hashtbl.add a a2 1;
    Hashtbl.add a a3 2;
    {
    na = 3;
    np = 3;
    indice_a = a;
    indice_p = p;  
    
    
    polygone_d = [|p1;p3;p2|];
    polygone_g = [|p2;p1;p3|];
    debut_a = [|fst a1;fst a2;fst a3|];
    fin_a =[|snd a1;snd a2;snd a3|];
    h_precedent = [|None;Some a3;Some a1|];
    ah_precedent = [|Some a3;None; Some a2|];
    h_suivant = [|Some a3;None;None|];
    ah_suivant = [|None;Some a3;None|];
    
    arete_autour_p = [|[a1;a2];[a1;a3];[a2;a3]|];
    }   


let prochain_point_candidat la med mil  =
    let wi = ref ((200.,200.),((4000.,4000.),(3200.,3200.))) in
    let wj = ref ((200.,200.),((4000.,4000.),(3200.,3200.))) in
    let epsilon = (0.001,0.001) in
    List.iter(fun i -> match (intersection i med) with
                        |None -> ()
                        |Some temp -> 
                       if (fst temp) <= fst mil &&  dist temp mil 
                        < dist (fst !wi) mil && not(Float.is_nan (fst temp)) 
                       && not(Float.is_nan (snd temp))
                       then wi := (temp,i)
                       else if dist temp mil < dist (fst !wj) mil 
                        && not(epsilon_comp temp (fst !wi) epsilon) 
                        && not(Float.is_nan (fst temp)) 
                        && not(Float.is_nan (snd temp))
                             then wj := (temp,i)
              ) la;
    (wi,wj)

let ajoute_point v p =
    let p',_= trouve_poly v p in
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
    

