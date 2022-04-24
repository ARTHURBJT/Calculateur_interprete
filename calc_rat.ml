type ope = Plus | Moins | Mult | Div | Puiss;;
type parenthese = Ouverte | Fermee;;

type rationnel = I of int | F of int * int;;
(*on souhaite utiliser des liste d'ope ET d'entiers à la fois*)
type ope_int = L1 of int | L2 of ope | L3 of parenthese | L4 of ope_int list | L5 of rationnel;;


let rec (^^) a n = 
  if n = 0 then 1 
  else a * (a ^^ (n-1));;


let rec pgcd a b = 
  if a = 0 then b
  else if b = 0 then a
  else pgcd b (a mod b);;


let reduit r = match r with
  | I(i) -> I(i)
  | F(i1,i2) -> 
    let p = pgcd i1 i2 in
      if i2/p = 1 then I(i1/p)
      else
        F(i1/p,i2/p);;


let (++) r1 r2 = match (r1,r2) with
  | (I(i1),I(i2)) -> I(i1 + i2)
  | (I(i1),F(i2,i3)) -> reduit (F(i1*i3+i2, i3))
  | (F(i1,i2),I(i3)) -> reduit (F(i1+i3*i2, i2))
  | (F(i1,i2),F(i3,i4)) -> reduit (F(i1*i4+i3*i2, i2*i4));;


let (--) r1 r2 = match (r1,r2) with
| (I(i1),I(i2)) -> I(i1 - i2)
| (I(i1),F(i2,i3)) -> reduit (F(i1*i3-i2, i3))
| (F(i1,i2),I(i3)) -> reduit (F(i1-i3*i2, i2))
| (F(i1,i2),F(i3,i4)) -> reduit (F(i1*i4-i3*i2, i2*i4));;


let ($) r1 r2 = match (r1,r2) with (*la multiplication*)
  | (I(i1),I(i2)) -> I(i1 * i2)
  | (I(i1),F(i2,i3)) -> reduit (F(i1*i2, i3))
  | (F(i1,i2),I(i3)) -> reduit (F(i1*i3, i2))
  | (F(i1,i2),F(i3,i4)) -> reduit (F(i1*i3, i2*i4));;


let (//) r1 r2 = match (r1,r2) with
  | (I(i1),I(i2)) -> F(i1, i2)
  | (I(i1),F(i2,i3)) -> reduit (F(i1*i3, i2))
  | (F(i1,i2),I(i3)) -> reduit (F(i1, i2*i3))
  | (F(i1,i2),F(i3,i4)) -> reduit (F(i1*i4, i2*i3));;


let ($$) r n = match (r,n) with (*la puissance n*)
  | (I(i1),I(i2)) -> I(i1^^i2)
  | (F(i1,i2),I(i3)) -> reduit (F(i1^^i3, i2^^i3))
  ;;



let bij_int c = (int_of_char c) - 48;;


let rec reconnait_nb l = 

  let rec aux l1 = match l1 with 
    | L1(p) :: q ->
      let (l2,n,q1) = aux q in (L1(p) :: l2,n+1,q1)
    | _ -> ([],0,l1)
    in

    let rec fusionne l1 i = match l1 with
      | [] -> 0
      | L1(p) :: q -> p * (10 ^^ i) + (fusionne q (i-1))
      in
  
  
      match l with
      | [] -> []
      | L1(p) :: q -> 
        let (l1,n,q1) = aux q in
          if n = 0 then L1(p) :: (reconnait_nb q1)
          else L1(fusionne (L1(p) :: l1) n) :: (reconnait_nb q1)  (*n pour n-1 car on ajt un élément*)
      | L4(l) :: q -> L4(reconnait_nb l) :: (reconnait_nb q)
      | p :: q -> p :: (reconnait_nb q);;



let rec parentheses l = 
  
  let rec aux l1 lf compteur = match l1 with
    | [] -> failwith "pb pb"
    | L1(p) :: q -> aux q (L1(p) :: lf) compteur
    | L2(p) :: q -> aux q (L2(p) :: lf) compteur
    | L3(p) :: q -> 
      if p = Ouverte then aux q (L3(p) :: lf) (compteur + 1)
      else
        if compteur = 1 then (List.rev lf, q)
        else aux q (L3(p) :: lf) (compteur - 1)
      in
  
    match l with
      | [] -> []
      | L1(p) :: q -> L1(p) :: (parentheses q)
      | L2(p) :: q -> L2(p) :: (parentheses q)
      | L3(p) :: q ->
        if p = Ouverte then 
          let (lf,q1) = (aux q [] 1) in (L4(List.rev (parentheses lf))) :: (parentheses q1) (*on inverse pour l'ordre de prio*)
        else failwith "pb";;


let list_of_str s =
  let n = String.length s in
  
  let rec aux i l =
    if i = -1 then l
    else 
      let j = bij_int (s.[i]) in
        if j <= 9 && j >= 0 then aux (i-1) (L1(j) :: l)

        else if j = -6 then aux (i-1) (L2(Mult) :: l)
        else if j = -5 then aux (i-1) (L2(Plus) :: l)
        else if j = -3 then aux (i-1) (L2(Moins) :: l)
        else if j = -1 then aux (i-1) (L2(Div) :: l)
        else if j = 46 then aux (i-1) (L2(Puiss) :: l)

        else if j = -8 then aux (i-1) (L3(Ouverte) :: l)
        else if j = -7 then aux (i-1) (L3(Fermee) :: l)

        else aux (i-1) l
    in

    parentheses (reconnait_nb (aux (n-1) []));;


(*il y a un ordre de priorité des opérations, celui ci est vérifié grâce à l'ordre des fct aux*)
let rec calc l =

  let rec aux_puiss l0 l1 = match l0 with
    | [] -> failwith "pb entree"
    | L1(p) :: q -> aux_puiss q (L1(p) :: l1)
    | L2(p) :: q -> 
      if p = Puiss then (calc q) $$ (calc (List.rev l1)) (*inversion*)
      else aux_puiss q (L2(p) :: l1)
      | L4(l) :: q -> aux_puiss q (L4(l) :: l1)
      in

      let rec aux_mult l0 l1 = match l0 with
        | [] -> aux_puiss (List.rev l1) []
        | L1(p) :: q -> aux_mult q (L1(p) :: l1)
        | L2(p) :: q -> 
          if p = Mult then (calc (List.rev l1)) $ (calc q)
          else if p = Div then (calc q) // (calc (List.rev l1)) (*inversion*)
          else aux_mult q (L2(p) :: l1)
        | L4(l) :: q -> aux_mult q (L4(l) :: l1)
        in 

          let rec aux_plus l0 l1 = match l0 with
            | [] -> aux_mult (List.rev l1) []
            | L1(p) :: q -> aux_plus q (L1(p) :: l1)
            | L2(p) :: q -> 
              if p = Plus then (calc (List.rev l1)) ++ (calc q)
              else if p = Moins then (calc q) -- (calc (List.rev l1)) (*réinversion*)
              else aux_plus q (L2(p) :: l1)
            | L4(l) :: q -> aux_plus q (L4(l) :: l1)
              in 

            match l with
              | [] -> I(0)
              | [L1(i)] -> I(i)
              | [L2(o)] -> failwith "le calcul demande est invalide"
              | [L4(l)] -> calc l
              | _ -> 
                reduit (aux_plus l []);;


(*les opérations sont dans l'ordre +,- < *,/ < ^ pour la priorité (si il n'y a pas de (), 
si ambiguité, dans l'ordre de lecture.
ATTENTION : 
1) a/b est le quotient dans la div. eucl. de a par b
2) il ne faut pas dépasser 10 ^ 18*)
let calcul s =
  let l = list_of_str s in 
    match calc (List.rev l) with
      | I(i) -> string_of_int i
      | F(i1,i2) -> string_of_int i1 ^ "/" ^ string_of_int i2;;

let l = "3 * 2 ^ 3 /5+1/3 * 34";;
calcul l;;

