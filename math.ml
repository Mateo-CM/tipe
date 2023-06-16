 open Parameters

let milieu x1 x2 =
  ((fst x1 +. fst x2)/.2.,(snd x1 +. snd x2)/.2.)
  

  let identifiable a b =
    let v = a -. b in
    let va = if v < 0. then -.v else v in
    va < 1e-5

let equation ar =
  let (x1,y1),(x2,y2) = ar in
  if identifiable x1 x2
  then (Float.nan, x1)
  else let a = (y2 -. y1)/.(x2 -. x1) in
      let b = y1 -. a*.x1 in
      (a,b)

let dist p1 p2 =
  int_of_float (10000000.*.Float.sqrt ((fst p1 -. fst p2)*.(fst p1 -. fst p2) 
  +. (snd p1 -. snd p2)*.(snd p1 -. snd p2)))

let intersection ar1 eq =
  let a1,b1 = equation ar1 in
  let a2,b2 = eq in
  if a1 -. a2 = 0.0
  then None
  else
      match Float.is_nan a1, Float.is_nan a2 with
      | true, true ->
            if identifiable b1 b2
            then Some (b1, 0.)
            else None
        | true, false -> Some (b1, a2 *. b1 +. b2)
        | false, true -> Some (b2, a1 *. b2 +. b1)
        | _ -> let x = (b2 -. b1)/.(a1 -. a2) in
              let y = (a1*.x +.b1) in
                    Some (x,y)
        
let mediatrice p1 p2 =
    let a,_ = equation (p1,p2) in
    let a' = -.1.0/.a in
    let x,y = milieu p1 p2 in
    let b' = y -. a'*.x in
    (a',b')


let ordre_triangle i j k  =
    let a = ((snd k) -. (snd i))*.((fst j) -. (fst i)) -. ((fst k) -. (fst i))*.((snd j) -. (snd i)) in
    (a>=1.0)

let epsilon_comp a1 a2 eps =
  fst a1 -. fst eps <= fst a2 && fst a1 +. fst eps >= fst a2
  && snd a1 -. snd eps <= snd a2 && snd a1 +. snd eps >= snd a2

let est_aigu wh wi wj =
  ((fst(fst wh)) -.(fst (fst wi)))*.((fst(fst wj)) -. (fst(fst wi))) 
  +. ((snd(fst wh)) -.(snd (fst wi)))*.((snd(fst wj)) -. (snd(fst wi))) > 0.
    
  