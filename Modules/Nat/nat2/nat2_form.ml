open Nat_2;;
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
          if x=1
          then
            un()
          else
          sucsuc(naturel_of_int (x - 2));;

          let rec int_of_naturel x =
                if est_zero(x)
                then
                    0
                else
                  if est_un(x)
                  then
                      1
                  else
                    2 + int_of_naturel(prepre(x));;

                    let rec est_pair_term(n) =
                      if est_zero(n)
                        then
                          true
                        else
                          if est_un(n)
                          then
                            false
                          else
                            est_pair_term(prepre(n));;


                        let rec plus_term(x,y) =
                            if est_zero(y)
                            then x
                            else
                            if est_un(y)
                            then x
                            else
                            plus_term (suc(x),pre(y)) ;;
