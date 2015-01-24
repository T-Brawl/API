(**
   Implémentation du module Arbre_binaire.

   @author FIL - IEEA - Univ. Lille1 (oct 2010)
*)

type 'a t = Vide | Noeud of 'a * 'a t * 'a t

exception Arbre_vide

let vide = Vide

let cree e g d =
  Noeud (e, g, d)

let racine a =
  match a with
    | Noeud (a,_,_) -> a
    | _ -> raise Arbre_vide

let gauche a =
  match a with
    | Noeud (_,g,_) -> g
    | _ -> raise Arbre_vide

let droit a =
  match a with
    | Noeud (_,_,d) -> d
    | _ -> raise Arbre_vide

let est_vide a =
  a = Vide


let rec taille a =
  match a with
    | Noeud (_,g,d) -> 1 + taille g + taille d
    | Vide -> 0

let rec hauteur a =
  match a with
    | Noeud (_,g,d) -> 
	let hg = hauteur g
	and hd = hauteur d in
	  1 + max hg hd
    | Vide -> -1

let rec imprimer imp a = 
  match a with
    | Noeud (e,g,d) ->
	Printf.printf "(" ;
	imp e ;
	Printf.printf "," ;
	imprimer imp g ;
	Printf.printf "," ;
	imprimer imp d ;
	Printf.printf ")"
    | Vide -> Printf.printf "()"


