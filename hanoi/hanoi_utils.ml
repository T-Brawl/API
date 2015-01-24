(*
  auteur : FIL - IEEA - Lille 1
  date   : 2010
  objet  : fonctions de gestion des tours, deplacements de disque 
    et d'interface graphique pour les tours de Hanoi
*)

open Graphics



let couleur_base = rgb 0 0 0 
and couleur_disque = rgb 0 0 255

type tour = A | B | C

(*
  fonction tour_en_caractere : tour -> char
  valeur renvoyee : char = caractere designant la tour
    passee en parametre
  CU : aucune
*)
let tour_en_caractere tour =
  match tour with
    | A -> 'A'
    | B -> 'B'
    | C -> 'C'

(* les 3 piles pour chaque tour *)
let tour_A = Stack.create ()
and tour_B = Stack.create () 
and tour_C = Stack.create ()


(* dimensions fenetre graphique *)
let largeur = 800
and hauteur = 600

(* dimensions bases des tours *)
let largeur_base = 3 * largeur / 10 
and hauteur_base = 9 * hauteur / 110

(* espaces entre les bases *)
let espace_horizontal = largeur / 40

(* espaces entre disques *)
let espace_vertical = hauteur / 120

(* abscisses des axes des tours *)
let axe_A = espace_horizontal + largeur_base / 2
and axe_B = 2 * espace_horizontal + 3 * largeur_base / 2
and axe_C = 3 * espace_horizontal + 5 * largeur_base / 2


(* dimensions des disques *)
let epaisseur_disque = hauteur_base * 6 / 10
and largeur_disque n = 9 * n * largeur_base / 100

(* visibilite des disques *)
let visible = true
and invisible = false


(* 
   variable fixant le delai entre chaque deplacement de disque
*)
let delai = ref 0.

(* 
   procedure fixer_delai float -> unit
   action : fixe la valeur de la variable delai 
   CU : aucune
*)
let fixer_delai t =
  delai := t

(*
  procedure attendre : unit -> unit
  action : pause apres un deplacement de disque
  duree de la pause = valeur variable delai (en seconde)
                        si cette valeur est positive ou nulle
                      jusqu'a appui d'une touche si valeur negative
                          
*)
let attendre () = 
  if !delai < 0. then
    ignore (Graphics.read_key ())
  else
    ignore (Unix.select [] [] [] !delai)
(*    ignore (Sys.command ("sleep "^(string_of_float !delai))) ;;*)


(*
  procedure dessiner_base : tour -> unit
  action : dessine la base de la tour passee en parametre
  CU : aucune
*)
let dessiner_base tour =
  let nom = tour_en_caractere tour
  in
  let x = 
    match tour with
      | A -> espace_horizontal
      | B -> 2 * espace_horizontal + largeur_base
      | C -> 3 * espace_horizontal + 2 * largeur_base
  and y = espace_vertical
  in
    set_color couleur_base ;
    fill_rect x y largeur_base hauteur_base ;
    set_color background ;
    moveto (x + largeur_base / 2) (y + hauteur_base / 2) ;
    draw_char nom

let dessiner_disque n h v tour = 
  let couleur = 
    if v then
      couleur_disque
    else
      background
  and largeur = largeur_disque n
  and hauteur = 
    hauteur_base + 
      (h - 1) * epaisseur_disque +
      (h + 1) * espace_vertical 
  and axe = 
    match tour with
      | A -> axe_A
      | B -> axe_B
      | C -> axe_C
  in
    set_color couleur ;
    fill_rect 
      (axe - largeur / 2) 
      hauteur
      largeur
      epaisseur_disque


(*
  procedure initialiser : unit -> unit
  action : ouvre une fenetre graphique et dessine les bases des trois tours
  CU : aucune
*)
let initialiser () = 
  open_graph (" "^(string_of_int largeur)^"x"^(string_of_int hauteur)) ;
  set_window_title "Les tours de Hanoi" ; 
  dessiner_base A ;
  dessiner_base B ;
  dessiner_base C



exception Initialisation_incorrecte

(*
  procedure initialiser_tour : int -> unit
  action : initialise la tour A avec n disques
        et les deux autres avec 0 disque,
        et dessine les disques
  CU : 0<= n <= 10, et suppose une fenetre graphique ouverte
*)
let initialiser_tours n =
  if (n < 0) || (n > 10) then
    raise Initialisation_incorrecte
  else begin
    clear_graph () ;
    dessiner_base A ;
    dessiner_base B ;
    dessiner_base C ;
    Stack.clear tour_A ;
    Stack.clear tour_B ;
    Stack.clear tour_C ;
    for i = n downto 1 do
      Stack.push i tour_A ; 
      dessiner_disque i (n - i + 1) true A
    done ;
  end

exception Deplacement_impossible

let deplacer_disque depart arrivee = 
  let tour_depart =
    match depart with
      | A -> tour_A
      | B -> tour_B
      | C -> tour_C
  and tour_arrivee = 
    match arrivee with
      | A -> tour_A
      | B -> tour_B
      | C -> tour_C
  in
  let nb_disques_depart = Stack.length tour_depart
  and nb_disques_arrivee = Stack.length tour_arrivee
  and taille_disque_depart = 
    try
      Stack.top tour_depart
    with
	Stack.Empty -> raise Deplacement_impossible
  in
    if (nb_disques_arrivee = 0) ||
      (taille_disque_depart < (Stack.top tour_arrivee)) then begin
	ignore (Stack.pop tour_depart) ;
	Stack.push taille_disque_depart tour_arrivee ;
	dessiner_disque 
	  taille_disque_depart 
	  nb_disques_depart 
	  invisible 
	  depart ;
	dessiner_disque
	  taille_disque_depart 
	  (nb_disques_arrivee + 1)
	  visible
	  arrivee ;
	attendre ()
    end else 
      raise Deplacement_impossible


let terminer () =
  close_graph () 


