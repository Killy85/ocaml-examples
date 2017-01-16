(* ---------------------------------------------------------------------- *)
(* ENTIERS NATURELS ----------------------------------------------------- *)
(* ---------------------------------------------------------------------- *)

(* TYPE naturel *)

(*

type naturel
-- represente l'ensemble des entiers naturels

zero : unit -> naturel
-- pour representer l'entier naturel nul

suc : naturel -> naturel
-- pour representer le successeur d'un entier naturel

est_zero : naturel -> bool
-- pour tester la nullite d'un entier naturel

pre : naturel -> naturel
-- ne s'applique qu'a un entier naturel non nul
-- pour calculer le predecesseur d'un entier naturel non nul

affiche_naturel : Format.formatter -> naturel -> unit
-- apres evaluation de la directive suivante :
-- #install_printer affiche_naturel
-- affiche une representation externe des entiers naturels

*)

type naturel = Z | S of naturel

(* ----- *)

(* zero : unit -> naturel *)

let zero () = Z

(* suc : naturel -> naturel *)

let suc (n) = S (n)

(* est_zero : naturel -> bool *)

let est_zero (n) = match n with
                     Z -> true |
                     _ -> false

(* pre : naturel -> naturel *)
 
let pre (n) = match n with
                S (m) -> m |
                _ -> raise (Failure ("pas de predecesseur"))

(* ---------- *)

(* hors interface *)

(* en_batons : naturel -> string *)

let rec en_batons (n) = match n with
                          Z -> "" |
                          S (m) -> "|" ^ en_batons (m)

(* ----- *)

(* dans l'interface *)

(* affiche_naturel : Format.formatter -> naturel -> unit *)

let affiche_naturel form n =
  Format.pp_open_box form 0 ;
  Format.pp_print_string form (en_batons n) ;
  Format.pp_close_box form ()

(* ---------- *)

(* #install_printer affiche_naturel *)

(* ---------------------------------------------------------------------- *)
(* ---------------------------------------------------------------------- *)
