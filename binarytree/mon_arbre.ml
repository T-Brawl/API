open Arbre_binaire ;;

open Exemples_arbres ;;

open Big_int ;;

let a1 = cree 3 vide vide ;;

let a2 = cree 3 (cree 1 vide vide) vide ;;

let a3 = cree 3 (cree 1 vide vide) (cree 4 (cree 1 (cree 9 vide vide) vide) (cree 5 vide (cree 2 vide vide))) ;;

let taille_max () =
	let l = ref (taille tab_arbres.(0)) in
		for i = 1 to 99 do
			if taille tab_arbres.(i) > !l then
				l := taille tab_arbres.(i)
		done;
	!l;;

let taille_min () =
	let l = ref (taille tab_arbres.(0)) in
		for i = 1 to 99 do
			if taille tab_arbres.(i) < !l then
				l := taille tab_arbres.(i)
		done;
	!l;;

let hauteur_max () =
	let l = ref (hauteur tab_arbres.(0)) in
		for i = 1 to 99 do
			if hauteur tab_arbres.(i) > !l then
				l := hauteur tab_arbres.(i)
		done;
	!l;;

let hauteur_min () =
	let l = ref (hauteur tab_arbres.(0)) in
		for i = 1 to 99 do
			if hauteur tab_arbres.(i) < !l then
				l := hauteur tab_arbres.(i)
		done;
	!l;;

let question_15 () =
	let pile = Stack.create () in
		for i = 0 to 99 do
			if float_of_int(taille tab_arbres.(i)) = 2. ** float_of_int(hauteur tab_arbres.(i) + 1) -. 1. then
				Stack.push i pile
			done;
		print_string "Les arbres qui, pour leur hauteur, ont la taille maximale sont :\n[ ";
		while not(Stack.is_empty pile) do 
			print_int (Stack.pop pile); 
			print_string "; "
		done;
		print_string "]\n" ;;


let rec nbre_arbres_recursif n =
	if n = 0 then 
		1
	else 
		begin
			let somme = ref 0 in
				for i = 0 to n-1 do
					somme := !somme + (nbre_arbres_recursif i) * (nbre_arbres_recursif (n-i-1))
				done;
			!somme
	end;;

let nbre_arbres_iteratif n =
	if n = 0 then 1 else
	let tableau = Array.make (n+1) 1 in
		for i = 1 to n do
			let somme = ref 0 in
			begin
				for j = 0 to i-1 do
					somme := !somme + (tableau.(j)) * (tableau.(i-1-j))
				done
			end;
			tableau.(i) <- !somme
		done;
    tableau.(n);;	

(* # nbre_arbres_iteratif 19 ;;
- : int = -380220458

En utilisant nbre_arbres 19 avec les entiers natifs de OCaml, on obtient -380220458.
C'est un resultat incoherent puisque le nombre d'arbres croit avec le nombre de noeuds et n'est jamais negatif. *)		
	
let nbre_arbres n =
	if eq_big_int (big_int_of_int(n)) (zero_big_int) then "1" else
	let tableau = Array.make (n+1) unit_big_int in 
		for i = 1 to n do
			let somme = ref zero_big_int in
			begin
				for k = 0 to i-1 do
					somme := add_big_int (!somme) (mult_big_int (tableau.(k)) (tableau.(i-1-k)))
				done
			end;
			tableau.(i) <- !somme
		done;
	string_of_big_int(tableau.(n));; 
		
