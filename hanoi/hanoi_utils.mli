(**
   Module utilitaire pour le problème des tours de Hanoï.


   L'utilisation de ce module en mode interprété nécessite l'appel à l'interpréteur avec les options :

   - [ocaml unix.cma graphics.cma hanoi_utils.cmo]


   La production d'un exécutable utilisant ce module doit se faire avec la commande :

   - [ocamlc -o <nom_executable> unix.cma graphics.cma hanoi_utils.cmo  <source_a_compiler>]

   @author FIL - IEEA - LILLE 1 (2010)
*)

(** {2 Le type [tour]} *)

(**
   Les trois valeurs du type [tour].
*)
type tour = A | B | C

(**
   [tour_en_caractere tour] convertit la tour [tour] en le caractère équivalent. 
*)
val tour_en_caractere : tour -> char


(** {2 Initialisations} *)

(**
   exception déclenchée par la procédure [initialiser_tours].
*)
exception Initialisation_incorrecte

(**
   [initialiser ()] ouvre une fenêtre graphique et dessine les bases des trois tours.
*)
val initialiser : unit -> unit


(**
   [initialiser_tours n] initialise la tour A avec [n] disques, et rend vide les deux autres.

   Nécessite qu'une fenêtre graphique soit ouverte (avec la procédure [initialiser]. Sinon exception [Initialisation_incorrecte] déclenchée.
*)
val initialiser_tours : int -> unit

(** {2 Fin de session} *)

(**
   [terminer ()] ferme la fenêtre graphique.
*)
val terminer : unit -> unit


(** {2 Déplacement de disque} *)


(**
   exception déclenchée par la procédure [deplacer_disque]
*)
exception Deplacement_impossible

(**
   [deplacer_disque depart arrivee] déplace le disque au sommet de la tour [depart] vers la tour [arrivee].

   S'il n'y a pas de disque sur la tour [depart], ou si ce disque est plus grand que celui situé au sommet de la tour [arrivee] (s'il y en a), exception [Deplacement_impossible] déclenchée.
*)
val deplacer_disque : tour -> tour -> unit


(** {2 Divers} *)

(**
   [fixer_delai s] fixe une pose de [s] secondes après chaque déplacement de déplacements de disque.  
*)
val fixer_delai : float -> unit



