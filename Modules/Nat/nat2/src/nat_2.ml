(* ---------------------------------------------------------------------- *)
(* ENTIERS NATURELS ----------------------------------------------------- *)
(* ---------------------------------------------------------------------- *)

(* TYPE naturel *)

(*

type naturel
-- represente l'ensemble des entiers naturels

zero : unit -> naturel
-- pour representer l'entier naturel 0

un : unit -> naturel
-- pour representer l'entier naturel 1

sucsuc : naturel -> naturel
-- pour representer le successeur du successeur d'un entier naturel

est_zero : naturel -> bool
-- pour tester la nullite d'un entier naturel

est_un : naturel -> bool
-- pour tester l'egalite d'un entier naturel a 1

prepre : naturel -> naturel
-- ne s'applique qu'a un entier naturel >= 2
-- pour calculer le predecesseur du predecesseur d'un entier naturel >= 2

affiche_naturel : Format.formatter -> naturel -> unit
-- apres evaluation de la directive suivante :
-- #install_printer affiche_naturel
-- affiche une representation externe des entiers naturels

*)

type naturel = Z | U | SS of naturel

let zero = function () -> Z

let un = function () -> U

let sucsuc = function n -> SS (n)

let est_zero = function Z -> true | _ -> false

let est_un = function U -> true | _ -> false

let prepre = function
 SS (n) -> n |
 _ -> raise (Failure ("pas de predecesseur de predecesseur"))

(* ---------- *)

(* hors interface *)

(* en_batons : naturel -> string *)

let rec en_batons (n) = match n with
                          Z -> "" |
                          U -> "|" |
                          SS (m) -> "||" ^ en_batons (m)

(* ----- *)

(* dans l'interface *)

(* val affiche_naturel : Format.formatter -> naturel -> unit *)

let affiche_naturel form n =
  Format.pp_open_box form 0 ;
  Format.pp_print_string form (en_batons n) ;
  Format.pp_close_box form ()

(* ---------- *)

(* #install_printer affiche_naturel *)

(* ---------------------------------------------------------------------- *)
(* ---------------------------------------------------------------------- *)
