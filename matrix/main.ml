open Util

let ( *^ ) x y =
  let ans = ref 0. in
  for i = 0 to (Array.length x - 1) do
    ans := !ans +. x.(i) *. y.(i)
  done;
  !ans

let ( *~ ) a x =
  Array.map (( *. ) a) x

let ( +^ ) x y =
  let n = Array.length x in
  let ans = Array.make n 0. in
  for i = 0 to (n - 1) do
    ans.(i) <- x.(i) +. y.(i)
  done;
  ans

let ( -^ ) x y =
  let n = Array.length x in
  let ans = Array.make n 0. in
  for i = 0 to (n - 1) do
    ans.(i) <- x.(i) -. y.(i)
  done;
  ans

let norm x = sqrt(x *^ x)

module SparseMatrix = struct
  type t = float array * int array * int array
  let make mat =
    let m = Array.length mat
    and n = Array.length mat.(0) in
    let rec extract i j num row col =
      if j < 0 then extract (i - 1) (n - 1) num row col
      else if i < 0 then (num, row, col)
      else if mat.(i).(j) = 0. then extract i (j - 1) num row col
      else extract i (j - 1) (mat.(i).(j) :: num) (i :: row) (j :: col)
    and shorten row =
      let srow = Array.make m 0 in
      let rec aux pred i = function
	  [] ->
	    Array.to_list srow
	| hd :: tl ->
	    if hd = pred then aux hd (i + 1) tl
	    else (srow.(hd) <- i; aux hd (i + 1) tl)
      in
      aux (-1) 0 row
    in
    let (num, row, col) = extract (m - 1) (n - 1) [] [] [] in
    (num, shorten row, col)
  let mut_mul_vec (num, row, col) vec =
    let m = List.length row
    and elm_num = List.length num in
    let ans = Array.make m 0. in
    let rec mul i num row col =
      let rec add i k num col =
	if k = 0 then
	  (num, col)
	else
	  (ans.(i) <- ans.(i) +. (List.hd num) *. vec.(List.hd col);
	  add i (k - 1) (List.tl num) (List.tl col))
      in
      match row with
	[] -> ans
      | [last] ->
	  let (num, col) = add i (elm_num - last) num col in
	  mul (i + 1) num [] col
      | hd :: tl ->
	  let (num, col) = add i (List.hd tl - hd) num col in
	  mul (i + 1) num tl col
    in
    mul 0 num row col
  let cg a b x0 =
    let rec aux x p r =
      if norm r < 1e-9 *. norm b then x
      else
	let ap = mut_mul_vec a p in
	let alpha = (r *^ p) /. (p *^ ap) in
	let x = x +^ alpha *~ p in
	let r = r -^ alpha *~ ap in
	let beta = (-1.) *. (r *^ ap) /. (p *^ ap) in
	let p = r +^ beta *~ p in
	aux x p r
    and r0 = b -^ mut_mul_vec a x0 in
    aux x0 r0 r0
end
