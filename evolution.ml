open Grid
open Parameters

exception Taillemax 
let flood_fill i j grid c_cible c_rep =
  let p = ref [] in
  if grid.(i).(j) = c_cible
  then p:= (i,j)::!p;
    let case_mod = ref 0 in
    while !p <> [] && !case_mod < 40 do
      (match !p with
      |[] -> failwith "Impossible"
      |(a,b)::q -> p:= q;
              if grid.(a).(b) = c_cible
              then begin
                    let w = ref b in
                    let e = ref b in
                    try
                      while grid.(a).(!w) = c_cible do
                        if !e - !w = length_sub_max
                        then raise Taillemax
                        else decr w
                      done;
                      while grid.(a).(!e) = c_cible do
                        if !e - !w = length_sub_max
                        then raise Taillemax
                        else incr e
                      done;
                    with Taillemax -> ();
                  for k = !w+1 to !e-1 do
                    grid.(a).(k) <- c_rep;
                    if grid.(a+1).(k) = c_cible 
                    then p:=(a+1,k)::!p;
                    if grid.(a-1).(k) = c_cible
                    then p:= (a-1,k)::!p
                  done
                  end)
      done
                         
  

let pop_growth_ev grid grid_2 n m = 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  for i = 1 to n-2 do
    for j = 1 to m-2 do
        for a = -1 to 1 do
          for b = -1 to 1 do
            if grid_2.(i).(j) = Black
            then begin
              if grid_2.(i+a).(j+b) <> Blue && grid_2.(i+a).(j+b) <> Green
              then grid.(i+a).(j+b) <- Black 
              end;
            if grid_2.(i).(j) = Green 
            then let r = Random.int 100 in
              if r <= dev_green 
              then grid.(i+a).(j+b) <- Green
          done
        done
    done
  done

