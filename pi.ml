open Printf

let rec pi_fraction n m i =
  if i <= 0 then 0.0
  else n +. (m *. m) /. (pi_fraction (n +. 2.) (m +. 1.) (i-1) );;

let pi n =
  4. /. (pi_fraction 1. 1. n);;

let rec range a b =
  if a>b then []
  else a :: range (a+1) b;;

List.iter (fun i -> printf "%2d: %f\n" i (pi i)) (range 1 (int_of_string Sys.argv.(1)));;