(*
  Auteur : MOEVI Alexandre
  Date   : 11 octobre 2013
  Objet  : TP n°3 - Tours de Hanoï
*)

open Hanoi_utils ;;
open Resolution_hanoi ;;

let usage () =
  Printf.printf "Usage : %s n\n" Sys.argv.(0) ;
  Printf.printf "\tn = nbre de disques (0 < n < 11)\n" ;
  exit 1 

let tour_char tour =
	 match tour with
		 |A -> 'A';
		 |B -> 'B';
		 |C -> 'C'
  
 let rec hanoi n depart arrivee =
	let inter = 
		match (depart, arrivee) with
		|(A, B) |(B, A) -> C ;
		|(C, B) |(B, C) -> A ;
		|(A, C) |(C, A) -> B ;
		|(_, _) -> failwith"Les disques sont deja sur la bonne tour."
	in 
		if n > 0 then
			begin
				hanoi (n-1) depart inter; 
				deplacer_disque depart arrivee;
				Printf.printf "deplacer_disque %c %c;\n" (tour_char depart) (tour_char arrivee); 
				flush stdout;
				hanoi (n-1) inter arrivee 
		end;;
  
  
let principal () =
  if Array.length Sys.argv <> 2 then
    usage ()
  else
    let nb_disques = 
      try
	int_of_string Sys.argv.(1)
      with
	  Failure _ -> usage ()
    in
		Printf.printf "open Hanoi_utils\n";
		Printf.printf "\n";
		Printf.printf "let _ =\n";
        Printf.printf "initialiser () ;\n";
        print_string("initialiser_tours "^Sys.argv.(1)^" ;\n");
		Printf.printf "fixer_delai 0.25 ;\n";
		Printf.printf "ignore (Graphics.read_key ()) ;\n";
		initialiser () ;
        initialiser_tours nb_disques ;
		hanoi nb_disques A C;
		terminer () ;
		Printf.printf"ignore (Graphics.read_key ()) ;\n";;
	  
let _ = principal ();;  