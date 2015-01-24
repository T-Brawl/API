(**
   Module définissant un type de données pour les arbres binaires, ainsi que les opérations primitives les accompagnant.

   @author FIL - IEEA - Univ. Lille1 (oct 2010)
*)

(** {2 Le type} *)

(**
   Le type des arbres binaires étiquetés par des éléments de type ['a].
*)
type 'a t

(** Exception déclenchée par les trois sélecteurs. *)
exception Arbre_vide

(** {2 Les constructeurs} *)

(** La constante arbre binaire vide. *)
val vide : 'a t

(**
   [cree a g d] renvoie un arbre binaire dont le noeud racine a la valeur [a] et les sous-arbres gauche et droit sont les arbres binaires [g] et [d].
*)
val cree : 'a -> 'a t -> 'a t -> 'a t

(** {2 Les sélecteurs} *)

(**
   [racine a] renvoie la valeur du noeud racine de l'arbre [a].

   @raise Arbre_vide si [a] est vide.
*)
val racine : 'a t -> 'a

(**
   [gauche a] renvoie le sous-arbre gauche de l'arbre [a].

   @raise Arbre_vide si [a] est vide.
*)
val gauche : 'a t -> 'a t

(**
   [droit a] renvoie le sous-arbre droit de l'arbre [a].

   @raise Arbre_vide si [a] est vide.
*)
val droit : 'a t -> 'a t

(** {2 Prédicat} *)

(**
   [est_vide a] =
   - Vrai si [a] est vide
   - Faux sinon.
*)
val est_vide : 'a t -> bool


(** {2 Opérations non primitives} *)

(**
   [taille a] renvoie la taille de l'arbre [a].
*)
val taille : 'a t -> int


(**
   [hauteur a] renvoie la hauteur de l'arbre [a].
   
   Par convention, la hauteur de l'arbre vide est -1.
*)
val hauteur : 'a t -> int


(**
   [imprimer imp a] imprime l'arbre [a] sous forme préfixée complètement parenthésée en utilisant l'imprimeur [imp] pour imprimer les étiquettes des noeuds de l'arbre. 

   L'arbre vide est imprimé par le couple de parenthèses [()].
*)
val imprimer : ('a -> unit) -> 'a t -> unit

 
