(**
   Programme principal du problème des tours de Hanoi.


   Nécessite deux modules : 
   - Hanoi_utils
   - Resolution_hanoi

   Programme destiné à la compilation :
   - [ocamlc -o hanoi graphics.cma hanoi_utils.cmo resolution_hanoi.cmo hanoi.ml].

   Exécution du programme depuis un terminal
   - [./hanoi <nb disques> <vitesse>]
   
   Une fois les tours initialisées, le programme attend l'appui sur une touche pour commencer. De même, une fois le problème résolu, le programme attend l'appui d'une touche pour s'arrêter.
   
   @author FIL - IEEA - LILLE 1 (2010)
*)

open Hanoi_utils 
open Resolution_hanoi 


(** {2 Une procédure pour indiquer l'usage du programme} *)

let usage () =
  Printf.printf "Usage : %s n d\n" Sys.argv.(0) ;
  Printf.printf "\tn = nbre de disques (0 < n < 11)\n" ;
  Printf.printf "\td = délai d'attente après chaque déplacement de disque (en secondes)\n" ;
  exit 1 

(** {2 La procédure principale} *)

(** [principal ()] procédure chargée de vérifier la validité des paramètres passés sur la ligne de commande, puis 
    - d'initialiser la fenêtre graphique, 
    - initialiser la tour [A],
    - régler la vitesse des déplacements de disques,
    - résoudre le problème des tours.
*)
let principal () =
  if Array.length Sys.argv <> 3 then
    usage ()
  else
    let nb_disques = 
      try
	int_of_string Sys.argv.(1)
      with
	  Failure _ -> usage ()
    and vitesse =
      try
	float_of_string Sys.argv.(2)
      with
	  Failure _ -> usage ()
    in
      initialiser () ;
      initialiser_tours nb_disques ;
      ignore (Graphics.read_key ()) ;
      fixer_delai vitesse ;
      hanoi nb_disques A C ;
      ignore (Graphics.read_key ()) ;
      terminer () ;
      exit 0 

(** La commande déclenchant l'exécution du programme.*)
let _ = principal ()
