let lowest t n =
    let j = ref 0 in
    for i = 1 to n-1 do
        if snd(t.(i)) < snd(t.(!j))
        then j:= i
        else if snd(t.(i)) = snd(t.(!j))
             then if fst(t.(i)) < fst(t.(!j))
                  then j:= i
    done;
    !j
    
let orient t i j k =
    let a = (snd(t.(k)) - snd(t.(i)))*(fst(t.(j)) - fst(t.(i))) - (fst(t.(k)) - fst(t.(i)))*(snd(t.(j)) - snd(t.(i))) in
    if a > 0
    then 1
    else if a < 0 
    then -1
    else 0 

let next_point t n i =
    let j = ref 0 in
    for k = 0 to n-1 do
        if k <> i 
        then if orient t i j k <= 0
            then j:= k
    done;
    !j

let jarvis t n hull =
    let a = lowest t n in
    let temp = ref a in
    hull := [a];
    let count_p = ref 1 in 
    let i = ref 1 in
    while !i < n && !temp <> a do
        let np = next_point t n !temp in
        hull := np::!hull;
        temp := np;
        incr count_p;
    done;
    if !temp = a 
    then decr count_p;
    !count_p


        



 