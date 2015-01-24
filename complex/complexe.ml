type t = {re : float ; im : float}

let cree_complexe a b = {re = a ; im = b}

let zero = cree_complexe 0. 0.

let un = cree_complexe 1.  0.

let i = cree_complexe 0. 1.

let re z = z.re

let im z = z.im

let norme2 z = (re z) *. (re z) +. (im z) *. (im z)

let norme z = sqrt (norme2 z)

let neg z = cree_complexe (-. (re z))  (-. (im z)) 

let conj z = cree_complexe (re z) (-. (im z))

let add z1 z2 = cree_complexe
  ((re z1) +. (re z2))
  ((im z1) +. (im z2))

let sub z1 z2 = cree_complexe
  ((re z1) -. (re z2))
  ((im z1) -. (im z2))

let mul z1 z2 = cree_complexe
  ((re z1) *. (re z2) -. (im z1) *. (im z2))
  ((re z1) *. (im z2) +. (im z1) *. (re z2))

let inv z =
  if z = zero then
    failwith "inv : division par zero"
  else
    let n2 = norme2 z in
      cree_complexe ((re z) /. n2)  (-. (im z) /. n2)

let div z1 z2 = 
  try
    mul z1 (inv z2)
  with
      Failure _ -> failwith "div : division par zero"

let imprimer z = 
  Printf.printf "%f + %fi" (re z) (im z)
