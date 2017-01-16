open Nat_1;;

(*
  naturel_of_int
  int_of_naturel
  est_pair
  plus
  fois
  moins
  supegal
  quotient_et_reste


  est_puissance_de_deux
  log_base_deux

  installation de l'affichage :

  #install_printer affiche_naturel

  *)
let rec naturel_of_int x =
      if x=0
      then
        zero()
      else
        suc(naturel_of_int (x - 1));;

let rec int_of_naturel x =
      if est_zero(x)
      then
          0
      else
          1+ int_of_naturel(pre(x));;


          let rec est_pair_term(n) =
            if est_zero(n)
              then
                true
              else
                if est_zero(pre(n))
                then
                  false
                else
                  est_pair_term(pre(pre(n)));;

                  let rec plus_term(x,y) =
                       if est_zero(y)
                       then x
                       else plus_term (suc(x),pre(y)) ;;


      let rec fois(x,y) =
            if est_zero(y)
            then zero()
            else plus_term(fois(x, pre(y)),x);;


      let rec moins(x , y) =
          if est_zero y
          then x
          else moins(pre(x),pre(y)) ;;

      let rec supegal(x,y) =
          if est_zero(y)
          then true
          else if est_zero(x)
              then false
              else supegal(pre(x),pre(y));;

      let rec quotient_et_reste(x,y)=
          if not(supegal(x,y))
          then  zero(),x
          else let q,r = quotient_et_reste(moins(x,y),y)
                in suc(q),r;;

      let rec est_puissance_de_deux x =
        if x = suc(zero())
        then true
        else if est_pair_term(x)
            then let (a,b) = quotient_et_reste(x, suc(suc(zero())))
            in est_puissance_de_deux(a)
            else false;;


      let rec log_base_2 j =
        if est_puissance_de_deux j
        then
          if j  = suc(suc(zero()))
          then suc(zero())
          else
          let q, r = quotient_et_reste(j, suc(suc(zero())))
          in plus_term(suc(zero()), log_base_2 q )

        else
          log_base_2(moins(j, suc(zero())));;
(* est_pair_term
  quotient_et_reste(
        naturel_of_int(8),
        suc(suc(zero()))
        )
        ;;*)
