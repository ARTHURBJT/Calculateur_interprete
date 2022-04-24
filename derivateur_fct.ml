type fonction_usuelle = Exp | Ln | Cos | Sin | Tan | Arccos | Arcsin | Arctan | Sinh | Cosh | Tanh | S of int;; (*Quoi d'autre ?*)

type 'a automate = {transitions : ('a * char * 'a) list ; initial : 'a ; finaux : 'a list};; (*l'alphabet est l'ensemble des chars*)


type ope = Plus | Moins | Mult | Div | Puiss ;;
type parenthese = Ouverte | Fermee;;
(*on souhaite utiliser des liste d'ope ET d'entiers à la fois*)
type expression = L1 of int | L2 of ope | L3 of parenthese | L4 of expression list | Point | L5 of float | 
  X | L6 of char | L7 of fonction_usuelle;;




let char_list_of_fct f = match f with
  | S(a) -> failwith "vous n'avez pas rentré une fonction usuelle"
  | Exp -> ['e';'x';'p']
  | Ln -> ['l';'n']
  | Cos -> ['c';'o';'s']
  | Sin -> ['s';'i';'n']
  | Tan -> ['t';'a';'n']
  | Arccos -> ['a';'r';'c';'c';'o';'s']
  | Arcsin -> ['a';'r';'c';'s';'i';'n']
  | Arctan -> ['a';'r';'c';'t';'a';'n']
  | Sinh -> ['s';'i';'n';'h']
  | Cosh -> ['c';'o';'s';'h']
  | Tanh -> ['t';'a';'n';'h']



let voisin c transition = match c with
  | (S(n0),p) ->

    let rec aux t = match t with
      | [] -> S(0)                       (*on choisit 0 car aucune transition ne retourne au sommet 0*)
      | (S(n1),p1,S(n2)) :: q -> 
        if n1 = n0 && p1 = p then S(n2)
        else aux q
      | (S(n1),p1,fct) :: q ->
        if n1 = n0 && p1 = p then fct
        else aux q
      | (fct,p1,_) :: q -> aux q
        in
  
        aux transition
  | (fct,p) ->

    let rec aux t = match t with
      | [] -> S(0)                       (*on choisit 0 car aucune transition ne retourne au sommet 0*)
      | (S(n1),p1,_) :: q -> aux q
      | (fct1,p1,S(n)) :: q -> 
        if fct1 = fct && p1 = p then S(n)
        else aux q
      | (fct1,p1,fct2) :: q -> 
        if fct1 = fct && p1 = p then fct2
        else aux q
        in
        
        aux transition;;




let automate_fct_usuelles = 

  let ajt_fct auto fct n = 
    let mot = char_list_of_fct fct in

    let rec ajt mot0 s0 n0 transitions = match mot0 with (*m0 le sommet de départ, n0+1 celui d'arrivée*)
      | [] -> (transitions, n0)
      | [a] -> ((s0, a, fct) :: transitions, n0)
      | p :: q -> ajt q (S(n0+1)) (n0+1) ((s0, p, S(n0+1)) :: transitions)
      in

      let rec reconnait mot1 s1 = match (mot1,s1) with
        | ([],_) -> (auto, n)
        | (p :: q,S(m1)) ->  
          begin
          match voisin (S(m1),p) auto.transitions with
            | S(m2) ->
              if m2 = 0 then 
                let transitions1, n1 = ajt mot1 (S(m1)) n auto.transitions in
                  ({transitions = transitions1 ; initial = auto.initial ; finaux = auto.finaux}, n1)
              else 
                reconnait q (S(m2))
            | fct -> reconnait q fct
          end
        | (p :: q,fct0) ->  match voisin (fct0,p) auto.transitions with
          | S(m2) ->
            if m2 = 0 then 
              let transitions1, n1 = ajt mot1 fct0 n auto.transitions in
                ({transitions = transitions1 ; initial = auto.initial ; finaux = auto.finaux}, n1)
            else 
              reconnait q (S(m2))
          | fct -> reconnait q fct
            in

            reconnait mot (S(0))
            in

            let rec construit_auto l auto n = match l with
              | [] -> auto
              | p :: q -> 
                let auto1,n1 = ajt_fct auto p n in
                  construit_auto q auto1 n1
                  in

                  construit_auto [Exp ; Ln ; Cos ; Sin ; Tan ; Arcsin ; Arccos ; Arctan ; Sinh ; Cosh] {transitions = [] ; initial = S(0) ; finaux = [Exp ; Ln ; Cos ; Sin ; Tan ; Arcsin ; Arccos ; Arctan ; Sinh ; Cosh]} 0;;



let lecture mot auto = 

  let rec aux mot0 s = match mot0 with
    | [] -> S(0)
    | [L6(a)] -> voisin (s,a) auto.transitions 
    | (L6(p)) :: q -> match voisin (s,p) auto.transitions with
      | S(n) -> 
        if n = 0 then S(0)
        else aux q (S(n))
      | fct -> aux q fct
      in

      aux mot (S(0));;

    
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
      | p :: q -> p :: (reconnait_float q);;


let rec reconnait_fct l = match l with
  | [] -> []
  | p :: q -> 
    match p with
      | L4([L6(a)]) -> X :: (reconnait_fct q)
      | L4(L6(t) :: q1) -> 
        begin
        match lecture (L6(t) :: q1) automate_fct_usuelles with
          | S(n) -> failwith "il y a un problème d'entrée"
          | fct -> L7(fct) :: (reconnait_fct q)
        end
      | _ -> p :: (reconnait_fct q);;


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

          let rec aux_char i l = 
            if i = -1 then (l,-1)
            else 
              let j1 = int_of_char (s.[i]) in
                if j1 <= 122 && j1 >= 97 then 
                  aux_char (i-1) (L6(s.[i]) :: l)
                else (l,i)
                  in
      
                  let rec aux i l =
                    if i = -1 then l
                    else 
                      let j = bij_int (s.[i]) in

                        if j <= 9 && j >= 0 then 
                          let (l1,i1) = aux_int (i-1) [L1(j)] 0 in 
                            aux i1 (l1 @ l)

                        else if j <= 74 && j >= 49 then 
                          let (l1,i1) = aux_char (i-1) [L6(s.[i])] in 
                            aux i1 (L4(l1) :: l)

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

                    parentheses (reconnait_fct (reconnait_float (aux (n-1) [])));;



(*il y a un ordre de priorité des opérations, celui ci est vérifié grâce à l'ordre des fct aux*)
let rec derive l =

  let rec aux_fct l0 = match l0 with
    | [] -> failwith "pb entree"
    | L7(fct) :: [L4(l)] -> 
      begin
        match fct with
          | Exp -> [L4(l);L7(Exp);L2(Mult);L4(derive l)] (*inversion*)
          | Ln -> [L4(l);L2(Div);L4(derive l)] (*inversion*)
          | Cos -> [L4(l);L7(Sin);L2(Mult);L5(-.1.);L2(Mult);L4(derive l)] (*inversion*)
          | Sin -> [L4(l);L7(Cos);L2(Mult);L4(derive l)] (*inversion*)
          | Tan -> [L4 [L5(2.);L2(Puiss);L4(l);L7(Tan);L2(Plus);L5(1.)];L2(Mult);L4(derive l)] (*inversion*)
          | Arccos -> [L5(0.5);L2(Puiss);L4[L5(2.);L2(Puiss);L4(l);L2(Moins);L5(1.)];L2(Div);L5(-.1.);L2(Mult);L4(derive l)] (*inversion*)
          | Arcsin -> [L5(0.5);L2(Puiss);L4[L5(2.);L2(Puiss);L4(l);L2(Moins);L5(1.)];L2(Div);L4(derive l)] (*inversion*)
          | Arctan -> [L4[L5(2.);L2(Puiss);L4(l);L2(Plus);L5(1.)];L2(Div);L4(derive l)] (*inversion*)
          | Cosh -> [L4(l);L7(Sinh);L2(Mult);L4(derive l)] (*inversion*)
          | Sinh -> [L4(l);L7(Cosh);L2(Mult);L4(derive l)] (*inversion*)
          | Tanh -> List.rev [L4[L5(2.);L2(Puiss);L4(l);L7(Tanh);L2(Moins);L5(1.)];L2(Mult);L4(derive l)] (*inversion*)
          | _ -> failwith "pb entree1"
      end
    | _ -> failwith "pb entree2"
      in

  let rec aux_puiss l0 l1 = match l0 with
    | [] -> aux_fct l1
    | L2(p) :: q -> 
      if p = Puiss then [L4(List.rev l1);L2(Puiss);L4(q);L2(Mult);L4(derive [L4(q);L7(Ln);L2(Mult);L4(List.rev l1)])]  (*inversion*)
      else aux_puiss q (L2(p) :: l1)
    | p :: q -> aux_puiss q (p :: l1)
      in

      let rec aux_mult l0 l1 = match l0 with
        | [] -> aux_puiss (List.rev l1) []
        | L2(p) :: q -> 
          if p = Mult then [L4(q);L2(Mult);L4(derive (List.rev l1));L2(Plus);L4(List.rev l1);L2(Mult);L4(derive q)]
          else if p = Div then [L5(2.);L2 Puiss;L4(List.rev l1);L2(Div);L4[L4(q);L2(Mult);L4(derive (List.rev l1));L2(Moins);L4(l1);L2(Mult);L4(derive q)]] (*inversion*)
          else aux_mult q (L2(p) :: l1)
        | p :: q -> aux_mult q (p :: l1)
        in 

          let rec aux_plus l0 l1 = match l0 with
            | [] -> aux_mult (List.rev l1) []
            | L2(p) :: q -> 
              if p = Plus then (derive q) @ [L2(Plus)] @ (derive (List.rev l1))
              else if p = Moins then (derive (List.rev l1)) @ (L2(Moins) :: (derive q)) (*réinversion*)
              else aux_plus q (L2(p) :: l1)
            | p :: q -> aux_plus q (p :: l1)
              in 

            match l with
              | [] -> [L5(0.)]
              | [L5(i)] -> [L5(0.)]
              | [L7(e)] -> failwith "pb d'entree"
              | [L2(o)] -> failwith "le calcul demande est invalide"
              | [L4(l)] -> derive l
              | [X] -> [L5(1.)]
              | _ -> 
                aux_plus l [];;


let reverse l = 
  let rec aux l0 l1 = match l0 with
    | [] -> l1
    | L4(l2) :: q -> aux q (L4(aux l2 []) :: l1)
    | p :: q -> aux q (p :: l1) in
    aux l [];;


(*les opérations sont dans l'ordre +,- < *,/ < ^ pour la priorité (si il n'y a pas de (), 
si ambiguité, dans l'ordre de lecture.
ATTENTION : 
Il ne faut pas dépasser 10 ^ 18
Et on demande de toujours mettre l'argument d'une fonction entre () (voir exemple)*)
let derivation s =
  let l = list_of_str s in reverse (derive (List.rev l));;

  list_of_str "3^2";;

let e = derivation "x*arctan(x) / 12 * x";;

(*)

let rec simplifie expr = 

  let rec aux_fct l0 = match l0 with
    | [] -> failwith "pb entree"
    | L7(fct) :: [L4(l)] -> 
      begin
        let l2 = simplifie l in
          match l2 with
            | L5(a) -> 
              begin
                match fct with
                  | Exp -> L5(exp(a))
                  | Ln -> L5(log(a))
                  | Cos -> L5(cos(a))
                  | Sin -> L5(sin(a))
                  | Tan -> L5(tan(a))
                  | Arccos -> L5(acos(a))
                  | Arcsin -> L5(asin(a))
                  | Arctan -> L5(atan(a))
                  | Cosh -> L5(cosh(a))
                  | Sinh -> L5(sinh(a))
                  | Tanh -> L5(tanh(a))
                  | _ -> failwith "pb entree1"
              end
            | _ -> L4[L7(fct);l2]
      end
    | _ -> failwith "pb entree3"
      in

  let rec aux_puiss l0 l1 = match l0 with
    | [] -> aux_fct l1
    | L2(p) :: q ->
      if p = Puiss then  
        let l2 = simplifie q in
          let l3 = simplifie (List.rev l1) in
              match (l2,l3) with
                | (L5(a),L5(b)) -> L5(a**b)
                | (L5(a),l3) when a = 0. -> L5(0.)
                | (l2,L5(b)) when b = 0. -> L5(1.)
                | _ -> L4[l2;L2(Puiss);l3]
      else aux_puiss q (L2(p) :: l1)
    | p :: q -> aux_puiss q (p :: l1)
      in

      let rec aux_mult l0 l1 = match l0 with
        | [] -> aux_puiss (List.rev l1) []
        | L2(p) :: q -> 
          if p = Mult then 
            let l2 = simplifie q in
              let l3 = simplifie (List.rev l1) in
                match (l2,l3) with
                  | (L5(a),L5(b)) -> L5(a*.b)
                  | (L5(a),l3) when a = 0. -> L5(0.)
                  | (l2,L5(b)) when b = 0. -> L5(0.)
                  | _ -> L4[l2;L2(Mult);l3]
          else if p = Div then                             (*ORDRE????*)
            let l2 = simplifie q in
              let l3 = simplifie (List.rev l1) in
                match (l2,l3) with
                  | (L5(a),L5(b)) -> L5(a/.b)
                  | (L5(a),l3) when a = 0. -> L5(0.)
                  | (l2,L5(b)) when b = 0. -> failwith " division par zero"
                  | _ -> L4[l2;L2(Div);l3]
          else aux_mult q (L2(p) :: l1)
        | p :: q -> aux_mult q (p :: l1)
        in 

          let rec aux_plus l0 l1 = match l0 with
            | [] -> aux_mult (List.rev l1) []
            | L2(p) :: q -> 
              if p = Plus then 
                let l2 = simplifie q in
                  let l3 = simplifie (List.rev l1) in
                    match (l2,l3) with
                      | (L5(a),L5(b)) -> L5(a+.b)
                      | (L5(a),l3) when a = 0. -> l3
                      | (l2,L5(b)) when b = 0. -> l2
                      | _ -> L4[l3;L2(Plus);l2]
              else if p = Moins then 
                let l2 = simplifie q in
                  let l3 = simplifie (List.rev l1) in
                    match (l2,l3) with
                      | (L5(a),L5(b)) -> L5(a-.b)
                      | (l2,L5(b)) when b = 0. -> l2
                      | _ -> L4[l3;L2(Moins);l2]
              else aux_plus q (L2(p) :: l1)
            | p :: q -> aux_plus q (p :: l1)
              in 

            match expr with
              | [L5(i)] -> L5(i)
              | [L2(o)] -> failwith "pb d'entree"
              | [L7(e)] -> failwith "pb d'entree"
              | [L4(l)] -> simplifie l
              | [X] -> X
              | _ -> 
                aux_plus expr [];;


simplifie e;;

(*exemple : *)