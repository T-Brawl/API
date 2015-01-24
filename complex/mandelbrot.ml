(**
   Programme de dessin de l'ensemble de Mandelbrot.

   Nécessite deux modules :
   - le module Complexe
   - le module Iteration
   
   Ligne de commande de compilation
   - ocamlc -I <chemin d'acces aux modules> graphics.cma complexe.cmo iteration.cmo mandelbrot.ml -o mandelbrot


   @author FIL - IEEA - Univ. Lille 1
*)
open Graphics 
open Complexe 
open Iteration

(** 
    Quelques constantes fixant les dimensions de la fenêtre graphique, ainsi que le nombre maximal d'itérations
*)
let larg = 400
and haut = 400  (* dimensions fenêtre graphique *)
and max = 64    (* nbre max d'iteration *)

(**
   [complexe_of_pixel (i,j)] = nombre complexe associé au pixel de coordonnées [(i,j)] en tenant compte que le coin supérieur gauche de la fenêtre (pixel de coordonnées [(0,haut-1)]) correspond approximativement au nombre complexe -9/4 + 3/2 i et le coin inférieur droit (pixel de coordonées [(larg-1,0)]) au nombre complexe 3/4 - 3/2 i. 

   La transformation du rectancle [[0,larg]x[O,haut]] en le rectangle [[-9/4, 3/4]x[-3/2, 3/2]] est affine.
*) 
let complexe_of_pixel (i,j) = 
  let agrand = 3./. float_of_int(larg)
  in
    cree_complexe 
      (float_of_int(i - 3*larg / 4) *. agrand)
      (float_of_int(haut / 2 - j) *. agrand)

(** 
    [couleur n] associe une couleur (ici nuances de bleu) à l'entier [n]
*)
let couleur n =
  if n = max then
    rgb 0 0 0
  else 
    rgb (256*n / max)  (256*n / max) (128 + 128*n / max)


(** Programme principal 
    [principal ()] 
    - ouvre une fenêtre graphique
    - puis pour chacun des pixels de cette fenêtre colorie ce pixel d'une couleur déterminée par le nombre d'itérations calculée
    - enfin attend l'appui d'une touche pour fermer la fenêtre et terminer.
*)
let principal () = 
  open_graph (" "^string_of_int(larg)^"x"^string_of_int(haut));
  set_window_title "Ensemble de Mandelbrot";
  for j = haut - 1 downto 0 do
    for i = 0 to larg - 1 do
      let n = iteration (complexe_of_pixel (i,j)) max
      in
	set_color (couleur n);
	plot i j ;
    done
  done;
  print_string ("Pour terminer appuyer sur une touche.");
  flush (stdout);
  ignore(read_key ()) ;
  close_graph ()


let _ = principal ()
