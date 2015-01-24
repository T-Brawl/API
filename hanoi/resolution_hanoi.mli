(**
   Module de résolution du problème des tours de Hanoï.

   Nécessite l'utilisation du module [Hanoi_utils].


   L'utilisation de ce module en mode interprété nécessite l'appel à l'interprèteur avec les options :
   
   - [ocaml unix.cma graphics.cma hanoi_utils.cmo resolution_hanoi.cmo]

   La production d'un exécutable utilisant ce module doit se faire avec la commande :

   - [ocamlc -o <nom_executable> unix.cma graphics.cma hanoi_utils.cmo resolution_hanoi.cmo <source_a_compiler>]

   @author FIL - IEEA - LILLE 1 (2010)
*)



(**
   [hanoi n depart arrivee] résoud le problème des tours de Hanoï en déplaçant [n] disques de la tour depart vers la tour arrivee. 

   {b CU :} nécessite que les tours aient été initialisées correctement.
*)
val hanoi : int -> Hanoi_utils.tour -> Hanoi_utils.tour -> unit
