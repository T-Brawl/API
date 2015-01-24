(*
  Auteur : MOEVI Alexandre
  Date   : 11 octobre 2013
  Objet  : TP n°3 - Tours de Hanoï
*)


open Hanoi_utils ;; 

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
				Printf.printf "%c -> %c\n" (tour_char depart) (tour_char arrivee); 
				flush stdout;
				hanoi (n-1) inter arrivee 
		end;;

(* Le déplacement optimal pour n disques comporte (2 puissance n) - 1 mouvements.
En lisant le programme, on peut remarquer que pour n disques, on doit déplacer les n-1 disques deux fois (2x) et déplacer le disque restant (2x + 1).
Mais pour déplacer les n-1 disques, on doit faire la même chose avec les n-2 (2y + 1).
Par récurrence on descend à l'étape 1 disque, ce qui correspond à un mouvement (2*0 + 1).
On remonte ainsi, pour 2 disques, 1 + 1 + 1 = 3 déplacements, pour 3 disques = 3 + 1 + 3 = 7, pour 4, 7 + 1 + 7 = 15 et pour n = 2 puissance (n - 1) - 1 + 1 + 2 puissance (n - 1) - 1 *)