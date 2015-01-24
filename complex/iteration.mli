(**
   Module Iteration.

   Itération d'une fonction polynomiale [f(z) = z^2 + c] sur les complexes.
   Pour l'étude des suites de la forme
   - [z(0) = 0]
   - [z(n+1) = f(z(n))]

   @author FIL - IEEA - Univ. Lille 1
*)


(**
   [iteration c m] donne 
   - le plus petit entier [k] tel que [|z(k)| > 2] s'il en existe un inférieur ou égal à [m] ;
   - l'entier [m] sinon
   Le complexe [c] détermine la fonction polynomiale [f(z) = z^2 + c] qui définit la suite de nombres complexes [z(n)]. 
*)
val iteration : Complexe.t -> int -> int
