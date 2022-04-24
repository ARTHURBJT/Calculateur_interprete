type ope = Plus | Moins | Mult | Div | Puiss ;;
type parenthese = Ouverte | Fermee;;
(*on souhaite utiliser des liste d'ope ET d'entiers à la fois*)
type ope_int = L1 of int | L2 of ope | L3 of parenthese | L4 of ope_int list | Point | L5 of float;;


let bij_int c = (int_of_char c) - 48;;


let rec (^^) a n = 
  if n = 0 then 1 
  else a * (a ^^ (n-1));;


let rec reconnait_float l = 

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
        begin
        let (l1,n1,q1) = aux q in match q1 with
          | Point :: q2 -> 
            let (l2,n2,q2) = aux q2 in
              let a = (float_of_int (fusionne (L1(p) :: l1) n1) +. float_of_int (fusionne (l2) (n2-1)) *. 10. ** (-. (float_of_int n2))) in
                L5(a) :: (reconnait_float q2) (*n pour n-1 car on ajt un élément*)
        end
      | L4(l) :: q -> L4(reconnait_float l) :: (reconnait_float q)
      | L2(e) :: q -> L2(e) :: (reconnait_float q)
      | L3(p) :: q -> L3(p) :: (reconnait_float q)
      | L5(f) :: q -> L5(f) :: (reconnait_float q);;


let rec parentheses l = 
  
  let rec aux l1 lf compteur = match l1 with
    | [] -> failwith "pb pb"
    | L3(p) :: q -> 
      if p = Ouverte then aux q (L3(p) :: lf) (compteur + 1)
      else
        if compteur = 1 then (List.rev lf, q)
        else aux q (L3(p) :: lf) (compteur - 1)
    | p :: q -> aux q (p :: lf) compteur
      in
  
    match l with
      | [] -> []
      | L3(p) :: q ->
        if p = Ouverte then 
          let (lf,q1) = (aux q [] 1) in (L4(List.rev (parentheses lf))) :: (parentheses q1) (*on inverse pour l'ordre de prio*)
        else failwith "pb"
      | p :: q -> p :: (parentheses q);;


let list_of_str s =
  let n = String.length s in

  let rec aux_int i l c = 
    if i = -1 then 
      if c = 0 then (l @ [Point],-1)
      else (l,-1)
    else 
      let j1 = bij_int (s.[i]) in
        if c = 0 then 
          if j1 <= 9 && j1 >= 0 then 
            aux_int (i-1) (L1(j1) :: l) 0 (*on ne change rien*)
          else if j1 = -2 then 
            aux_int (i-1) (Point :: l) 1 (*on rajoute un point, le compteur indique qu'on l'a bien mis*)
          else
            (l @ [Point],i)
        else 
          if j1 <= 9 && j1 >= 0 then 
            aux_int (i-1) (L1(j1) :: l) 1 (*on ne change rien*)
          else (l,i)
          in
      
    let rec aux i l =
      if i = -1 then l
      else 
        let j = bij_int (s.[i]) in

          if j <= 9 && j >= 0 then 
            let (l1,i1) = aux_int (i-1) [L1(j)] 0 in 
              aux i1 (l1 @ l)

          else if j = -6 then aux (i-1) (L2(Mult) :: l)
          else if j = -5 then aux (i-1) (L2(Plus) :: l)
          else if j = -3 then aux (i-1) (L2(Moins) :: l)
          else if j = -1 then aux (i-1) (L2(Div) :: l)
          else if j = 46 then aux (i-1) (L2(Puiss) :: l)

          else if j = -2 then
            let (l1,i1) = aux_int (i-1) [] 1 in 
              aux i1 (l1 @ Point :: l)

          else if j = -8 then aux (i-1) (L3(Ouverte) :: l)
          else if j = -7 then aux (i-1) (L3(Fermee) :: l)

          else aux (i-1) l
      in

      parentheses (reconnait_float (aux (n-1) []));;


(*il y a un ordre de priorité des opérations, celui ci est vérifié grâce à l'ordre des fct aux*)
let rec calc l =

  let rec aux_puiss l0 l1 = match l0 with
    | [] -> failwith "pb entree"
    | L5(p) :: q -> aux_puiss q (L5(p) :: l1)
    | L2(p) :: q -> 
      if p = Puiss then (calc q) ** (calc (List.rev l1)) (*inversion*)
      else aux_puiss q (L2(p) :: l1)
      | L4(l) :: q -> aux_puiss q (L4(l) :: l1)
      in

      let rec aux_mult l0 l1 = match l0 with
        | [] -> aux_puiss (List.rev l1) []
        | L5(p) :: q -> aux_mult q (L5(p) :: l1)
        | L2(p) :: q -> 
          if p = Mult then (calc (List.rev l1)) *. (calc q)
          else if p = Div then (calc q) /. (calc (List.rev l1)) (*inversion*)
          else aux_mult q (L2(p) :: l1)
        | L4(l) :: q -> aux_mult q (L4(l) :: l1)
        in 

          let rec aux_plus l0 l1 = match l0 with
            | [] -> aux_mult (List.rev l1) []
            | L5(p) :: q -> aux_plus q (L5(p) :: l1)
            | L2(p) :: q -> 
              if p = Plus then (calc (List.rev l1)) +. (calc q)
              else if p = Moins then (calc q) -. (calc (List.rev l1)) (*réinversion*)
              else aux_plus q (L2(p) :: l1)
            | L4(l) :: q -> aux_plus q (L4(l) :: l1)
              in 

            match l with
              | [] -> 0.
              | [L5(i)] -> i
              | [L2(o)] -> failwith "le calcul demande est invalide"
              | [L4(l)] -> calc l
              | _ -> 
                aux_plus l [];;


(*les opérations sont dans l'ordre +,- < *,/ < ^ pour la priorité (si il n'y a pas de (), 
si ambiguité, dans l'ordre de lecture.
ATTENTION : 
Il ne faut pas dépasser 10 ^ 18*)
let calcul s =
  let l = list_of_str s in calc (List.rev l);;

let l = "789 / 7 + (3-283) * 3.1415";;
list_of_str l;;
calcul l;;