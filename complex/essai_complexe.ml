(** 
    Programme d'essai du module Complexe.

    Programme destiné à la compilation. Ligne de commande de compilation
    - [ocamlc -I <chemin d'acces au module complexe> complexe.cmo essai_complexe.ml -o essai_complexe]
    
    @author FIL - IEEA - Univ. Lille 1
*)
open Complexe 

(** 
    [usage ()] procédure appelée lorsque l'exécution du programme a été comandée avec un nombre incorrect de paramètres sur la ligne de commande ou avec des paramètres incorrects.
*)
let usage () =
  Printf.printf "%s x y\n" Sys.argv.(0) ;
  Printf.printf "\tx = partie reelle\n" ;
  Printf.printf "\ty = partie imaginaire\n" ;
  exit 1 

(**
   [principal ()] procédure chargée de vérifier la validité de la ligne de commande appelant l'exécution du programme, puis de 
*)
let principal () =
  if Array.length Sys.argv <> 3 then 
    usage ()
  else
    let x = 
      try 
	float_of_string Sys.argv.(1)
      with
	  Failure _ -> usage ()
    and y = 
      try 
	float_of_string Sys.argv.(2)
      with
	  Failure _ -> usage () in
    let z = cree_complexe x y in
      Printf.printf "Nombre complexe z = " ;
      imprimer z ;
      Printf.printf "\n" ;
      Printf.printf "Partie reelle de z = %f\n" (re z) ;
      Printf.printf "Partie imaginaire de z = %f\n" (im z) ;
      Printf.printf "Norme de z = %f\n" (norme z) ;
      Printf.printf "Conjugue de z = " ;
      imprimer (conj z) ;
      Printf.printf "\n" ;
      if z <> zero then begin
	Printf.printf "Inverse de z = " ;
	imprimer (inv z) ;
	Printf.printf "\n" ;
      end ;
      if z <> un then begin
	Printf.printf "(z + 1) / (z - 1) = " ;
	imprimer (div (add z un) (sub z un)) ;
	Printf.printf "\n" ;
      end ;
      exit 0

(* appel à la procédure principal *)
let _ = principal ()
