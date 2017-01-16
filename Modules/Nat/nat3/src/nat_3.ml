(* ---------------------------------------------------------------------- *)
(* ENTIERS NATURELS ----------------------------------------------------- *)
(* ---------------------------------------------------------------------- *)

(* TYPE naturel *)

(*

type naturel
-- represente l'ensemble des entiers naturels

zero : unit -> naturel
-- pour representer l'entier naturel nul

d0 : naturel -> naturel
-- pour representer le double d'un entier naturel non nul

d1 : naturel -> naturel
-- pour representer le successeur du double d'un entier naturel

est_pair_et_non_nul : naturel -> bool
-- pour déterminer si un entier naturel est pair et non nul

est_impair : naturel -> bool
-- pour déterminer si un entier naturel est impair

inv_d0 : naturel -> naturel
-- ne s'applique qu'a un entier naturel pair et non nul
-- pour calculer la moitie entiere d'un entier naturel pair et non nul

inv_d1 : naturel -> naturel
-- ne s'applique qu'a un entier naturel impair
-- pour calculer la moitie entiere d'un entier naturel impair

affiche_naturel : Format.formatter -> naturel -> unit
-- apres evaluation de la directive suivante :
-- #install_printer affiche_naturel
-- affiche une representation externe des entiers naturels

*)

type naturel = Z | D0 of naturel | D1 of naturel

let zero = function () -> Z

let d0 = function 
  Z -> raise (Failure ("d0 : s'applique a un entier naturel non nul")) |
  n -> D0 (n)

let d1 = function n -> D1 (n)

let est_pair_et_non_nul = function D0 _ -> true | _ -> false

let est_impair = function D1 _ -> true | _ -> false

let inv_d0 = function
  D0 (n) -> n |
  _ -> raise (Failure ("inv_d0 : ne s'applique qu'a un entier pair et non nul"))

let inv_d1 = function
  D1 (n) -> n |
  _ -> raise (Failure ("inv_d1 : ne s'applique qu'a un entier impair"))

(* ---------- *)

(* hors interface *)

(* en_batons : naturel -> string *)

let rec en_batons (n) = match n with
                          Z -> "" |
                          D0 (m) -> en_batons (m) ^ en_batons (m) |
                          D1 (m) -> "|" ^ en_batons (m) ^ en_batons (m)

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