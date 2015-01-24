(**
   Module Complexe.

   Déclaration du type complexe et des opérations qui l'accompagnent.

   @author FIL - IEEA - Univ. Lille 1
*)


(** {2 Le type complexe} *)

(**
   Les nombres complexes sont représentés par un couple de nombres réels : parties réelle et imaginaire. 
*)
type t = {re : float ; im : float}

(** {2 Quelques constantes complexes prédéfinies} *)

(** Le complexe [0]. *)
val zero : t

(** Le complexe [1]. *)
val un : t

(** Le complexe [i] (dont le carré vaut -1). *)
val i : t

(** {2 Constructeur de nombres complexes} *)

(**
   [cree_complexe a b] = le nombre complexe a + ib.
*)
val cree_complexe : float -> float -> t

(** {2 Accès aux parties réelles et imaginaires} *)

(** 
    [re z] = la partie réelle du nombre complexe [z]. 
*)
val re : t -> float

(** 
    [im z] = la partie imaginaire du nombre complexe [z] 
*)
val im : t -> float

(**  {2 Norme d'un nombre complexe} *)

(**
   [norme2 z] = [a^2 + b^2] si [a] et [b] sont les parties réelle et imaginaire du complexe [z] (norme (ou module) au carré).
*)
val norme2: t -> float


(**
   [norme z] = [(a^2 + b^2)^(1/2)] si [a] et [b] sont les parties réelle et imaginaire du complexe [z] (norme (ou module)).
*)
val norme: t -> float


(** {2 Opérations arithmétiques} *)

(**
   [neg z] = le nombre complexe [-z] (négation unaire).
*)
val neg: t -> t

(**
   [conj z] = le conjugué du nombre complexe [z].
*)
val conj: t -> t

(**
   [add z1 z2] = [z1 + z2] (addition).
*)
val add: t -> t -> t

(**
   [sub z1 z2] = [z1 - z2] (soustraction).
*)
val sub: t -> t -> t

(**
   [mul z1 z2] = [z1 * z2] (multiplication).
*)
val mul: t -> t -> t

(**
   [inv z] = [1 /z] (inversion).

   {b CU :} [z] doit être non nul. Déclenche l'exception [Failure "inv : division par zero"] si contrainte d'utilisation non respectée.
*)
val inv: t -> t


(**
   [div z1 z2] = [z1 / z2] (division).

   {b CU :} [z2] doit être non nul. Déclenche l'exception [Failure "div : division par zero"] si contrainte d'utilisation non respectée.
*)
val div: t -> t -> t

(** {2 Impression} *)

(**
   [imprimer z] imprime sur la sortie standard le nombre complexe [z] sous la forme  [a + bi] où [a] et [b] sont les parties réelles et imaginaires de [z].
*)
val imprimer: t -> unit 

