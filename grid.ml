open Parameters

type colors = White | Black | Blue | Green | White_bis

let grid = Array.make_matrix (fst dim) (snd dim) White

let fill_source grid1 grid2 =
  for i = 0 to Array.length grid1 - 1 do
    for j = 0 to Array.length grid1.(0) - 1 do
      grid2.(i).(j) <- grid1.(i).(j)
    done
  done

let copy_matrix grid1 = 
  let grid_2 = Array.make_matrix (fst dim) (snd dim) White in
  for i = 0 to fst(dim)-1 do
    for j = 0 to snd(dim)-1 do
      grid_2.(i).(j) <- grid1.(i).(j)
    done
  done;
  grid_2

let genere_grid seed =
  let _ = Random.init seed in
  for i = 0 to fst dim - 1 do
    for j = 0 to snd dim - 1 do
      let r = Random.int 100 in
      if r < freq_black
      then grid.(i).(j) <- Black
    done
  done;


for j = 0 to 79 do
  for i = 7 to 9 do
    grid.(i).(j) <- Blue
  done
done;

for k = 56 to 72 do
  for l = 47 to 69 do
    grid.(l).(k) <- Green
  done
done