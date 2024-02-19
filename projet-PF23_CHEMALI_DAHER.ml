(* BINOME : Maissa CHEMALI 28722554 Version utilsee
            Sarah DAHER 21100791
   
   SUR PAPIER
   
   1- : TUCK DUP ROT SWAP ;
     9 12 15 TUCK -> etat de la pile :
     9 12 15
       9 12 15 15
         9 15 12 15
           9 15 15 12 -> duplique et insere le premier element de la pile apres
             le second ; donc a b c TUCK donne a c b c
    
   2- : CUBE DUP CARRE * ;
      6 CUBE -> etat de la pile :
        6 6 6 * *
         6 36 *
          216
*)
       

(* *********** Question 1.a *********** *)

(* definition des types *)

type op_pile = DUP | DROP | SWAP | ROT
type op_arithm = PLUS | FOIS | DIVISE | MOINS 
type op_cond = IF | THEN | ELSE | ENDIF
type op_comp = PLUSGRAND | PLUSPETIT | EGAL | DIFF
               
type element = OP_PILE  of  op_pile
             | OP_COMP of op_comp
             | OP_ARITH of op_arithm 
             | CST_BOOL of bool 
             | CST_INT of int 
             |OP_COND of op_cond 
             |CST_STRING of string ;;


(* *********** Question 1.b *********** *)

(* la fonction to_string transforme un element en une chaine de caracteres *)

let to_string (x:element) : string = 
  match x with
  |OP_PILE p->
      (match p with
       |DUP -> "DUP"
       |DROP -> "DROP"
       |SWAP -> "SWAP"
       |ROT -> "ROT" )
  |OP_ARITH a->
      (match a with
       |PLUS -> "+"
       |FOIS -> "*"
       |DIVISE -> "/"
       |MOINS -> "-"
      )
  |CST_INT i -> string_of_int(i)
  |CST_BOOL b-> 
      (match b with
       |true -> "TRUE"
       |false -> "FALSE"
      )
  |OP_COMP c -> 
      (match c with
       |EGAL -> "="
       |PLUSGRAND -> ">"
       |PLUSPETIT ->"<" 
       |DIFF -> "<>"
      )
  |OP_COND cnd ->
      (match cnd with
       |IF -> "IF"
       |THEN -> "THEN"
       |ELSE -> "ELSE"
       |ENDIF -> "ENDIF"
      )
  |CST_STRING s -> s ;;


(* la fonction of_string fait l'operation inverse, 
donc transforme une chaine de caracteres en un element*)

let of_string (s:string) : element =
  match s with
  |"DUP" -> OP_PILE DUP
  |"DROP" -> OP_PILE DROP
  |"SWAP" -> OP_PILE SWAP
  |"ROT" -> OP_PILE ROT
  |"+" -> OP_ARITH PLUS
  |"*"-> OP_ARITH FOIS
  |"-" -> OP_ARITH MOINS
  |"/"-> OP_ARITH DIVISE
  |"FALSE" -> CST_BOOL false
  |"TRUE" -> CST_BOOL true
  |"=" -> OP_COMP EGAL
  |">" -> OP_COMP PLUSGRAND
  |"<" -> OP_COMP PLUSPETIT
  |"<>" -> OP_COMP DIFF
  |"IF" -> OP_COND IF
  |"THEN" -> OP_COND THEN
  |"ELSE" -> OP_COND ELSE 
  |"ENDIF"-> OP_COND ENDIF
  |a-> (match int_of_string_opt(a) with 
      |None -> CST_STRING a
      |Some i ->  CST_INT i) (* si la fonction int_of_string_opt : string -> int option 
                                retourne None alors la chaine de caractères 
                                ne correspond pas à un entier, sinon c'en est un *)
;;



(* *********** Question 1.c *********** *)

(* fonction utilitaire : 
    [split s] découpe le texte [s] en une suite de mot. 
*)
let split (s:string) : string list =
  (* traite les tabulations et les sauts de lignes comme des espaces *)
  let normalize_s = String.map (function '\n' | '\t' | '\r' -> ' ' | c -> c) s in
  let split_s = String.split_on_char ' ' normalize_s in
  (* ignore les espaces successifs *)
  List.filter ((<>) "") split_s ;;

assert (split "  \t  \n " = []) ;;
assert (split " A     \n B\t C  " = ["A";"B";"C"]) ;;

(* transforme un texte (représentant un programme ou une pile)
    en une suite de symboles du langage (e.g., "42" et "DUP") 
*)
let parse (s:string) : element list = 
  let l = split s in 
  List.map(of_string)(l);;



(* operation inverse : transforme un suite de symbole du langage (représentant un programme 
    ou une pile) en un texte équivalent (les elements sont separes par un espace). 
    Par exemple : [text (parse "1 2 +")) = "1 2 +"].
*) 

(* ^ pour concatener les chaines de caracteres
(trouve dans la bibliotheque ocaml) *)

let text (p:element list) : string =
  let rec text_aux p acc =
    match p with
    |[] -> acc
    |hd :: tl ->
        text_aux tl (if acc = "" then to_string(hd) else acc^" "^to_string(hd))
  in text_aux p "" ;; 

(* autre version (non recursive terminale) :
   
  let rec text (p:element list) : string = 
    match p with
    |[] -> ""
    |hd :: tl ->( match tl with 
        |[] ->to_string(hd)
        |_ -> to_string(hd) ^ " " ^ text(tl) );;
*) 

(* *********** Question 2 *********** *)

type prog = element list
type stack = element list

(* fonction auxiliaire : évaluation d'un opérateur binaire *) 
let eval_binop op (e1: element) (e2: element) : element =
  match op with 
  |OP_ARITH a ->
      (match a with
       | PLUS ->
           (match (e1, e2) with
            | (CST_INT n1, CST_INT n2) -> CST_INT (n1 + n2)
            | _ -> raise (Invalid_argument "eval_binop +"))
       | FOIS ->
           (match (e1, e2) with
            | (CST_INT n1, CST_INT n2) -> CST_INT (n1 * n2)
            | _ -> raise (Invalid_argument "eval_binop *"))
       | DIVISE ->
           (match (e1, e2) with 
            | (CST_INT 0, _) -> raise (Invalid_argument "eval_binop: division par zero??")
            | (CST_INT n1, CST_INT n2) -> CST_INT (n2 / n1)
            | _ -> raise (Invalid_argument "eval_binop /"))
       | MOINS ->
           (match (e1, e2) with
            | (CST_INT n1, CST_INT n2) -> CST_INT (n2 - n1)
            | _ -> raise (Invalid_argument "eval_binop -")))
  |OP_COMP c ->
      (match c with
       | EGAL ->
           (match (e1, e2) with
            | (CST_INT n1, CST_INT n2) -> CST_BOOL (n2 = n1)
            | _ -> raise (Invalid_argument "eval_binop ="))
       | PLUSGRAND ->
           (match (e1, e2) with
            | (CST_INT n1, CST_INT n2) -> CST_BOOL (n2 > n1)
            | _ -> raise (Invalid_argument "eval_binop >"))
       | PLUSPETIT ->
           (match (e1, e2) with
            | (CST_INT n1, CST_INT n2) -> CST_BOOL (n2 < n1)
            | _ -> raise (Invalid_argument "eval_binop <"))
       | DIFF ->
           (match (e1, e2) with
            | (CST_INT n1, CST_INT n2) -> CST_BOOL (n2 <> n1)
            | _ -> raise (Invalid_argument "eval_binop <>"))
      )
  | _ -> raise (Invalid_argument "eval_binop") ;;

(* fonction auxiliaire : évaluation d'un opérateur de pile *)
let eval_stackop (stk: stack) (op: op_pile) : stack =
  match op with
  | DUP ->
      (match stk with
       | hd :: tl -> hd :: hd :: tl
       | _ -> raise (Invalid_argument "eval_stackop dup"))
  | DROP ->
      (match stk with
       | _ :: tl -> tl
       | _ -> raise (Invalid_argument "eval_stackop drop"))
  | SWAP ->
      (match stk with
       | x :: y :: tl -> y :: x :: tl
       | _ -> raise (Invalid_argument "eval_stackop swap"))
  | ROT ->
      (match stk with
       | x :: y :: z :: tl -> y :: z :: x :: tl
       | _ -> raise (Invalid_argument "eval_stackop rot"));;

(* [step stk e] exécute l'élément [e] dans la pile [stk] 
   et retourne la pile résultante *)

let step (stk: stack) (e: element) : stack =
  match e with
  | CST_INT _ | CST_BOOL _ -> e :: stk
  | OP_PILE op -> eval_stackop stk op
  | OP_ARITH _ | OP_COMP _ ->
      (match stk with
       | e1 :: e2 :: tl -> eval_binop (e) (e1) (e2) :: tl
       | _ -> raise (Invalid_argument "step")) 
  | _ -> raise (Invalid_argument "step");;
  

(* *********** Question 3 *********** *)

(* prend une pile et un programme (ne contenant ni operateurs conditionnels ni declarations)
   et renvoie la pile obtenue a la fin de l'execution du programme *)

let rec calc (stk:stack) (p:prog) : stack =
  match p with
  |[]->stk
  |hd::tl-> calc (step stk hd) tl ;; 

(* *********** Question 4 *********** *)

(* premiere version, moins optimale (mais qui fonctionne bien!)
   utilisant des listes de couples 

type name = string  
type dico = (name*prog) list
let empty : dico = []

let add_bof (x:name) (def:prog) (dico:dico) : dico =
  (x,def)::dico ;;

let rec add (x:name) (def:prog) (dico:dico) : dico =
  match dico with
  |[]->(x,def)::dico
  |(n,p)::q-> if n=x then (n,def)::q else (n,p)::(add x def q)
;;

let rec lookup (x:name) (dico:dico) : prog =
  match dico with
  |[] -> raise Not_found (* cas dico = empty *)
  |(n,p)::dic-> if n=x then p else lookup x dic ;;

*)
   
(* *********** Question 4 *********** *)

(* version plus optimale manipulant un arbre binaire de recherche *)

type name = string
type dico =
  | Empty
  | Node of name * prog * dico * dico
let empty : dico = Empty ;;

(* ajoute un mot et sa definition dans le dictionnaire, si le mot est 
deja present on met a jour sa definition *)  
let rec add (x:name) (def:prog) (dico:dico) : dico =
  match dico with
  |Empty -> Node(x, def, Empty, Empty)
  |Node (n, p, dg, dd) ->
      if n = x then Node (n, def, dg, dd)
      else if x < n then Node (n, p, add x def dg, dd)
      else Node (n, p, dg, add x def dd) ;;

(* recherche un mot dans le dictionnaire et renvoie sa definition si il y est *)
let rec lookup (x:name) (dico:dico) : prog =
  match dico with
  | Empty -> raise Not_found
  | Node (n, def, dg, dd) -> if n = x then def else 
      if n > x then lookup x dg
      else lookup x dd

(* *********** Question 5 *********** *) 

(* eval_declar_aux est appelee par la fonction eval_declar *)
let rec eval_declar_aux (dico:dico) (p:prog) (acc:prog) (n:name) : dico*prog =
  match p with 
  | h :: t -> 
      if to_string (h) = ";" then (add n (List.rev acc) dico, t) (* toute declaration se termine par ';', on a donc recupere toute la definition et on ajoute alors le mot au dictionnaire ; on renvoie ce nouveau dictionnaire ainsi que la suite du programme *)
      else if to_string (h) = ":" then (* si on rencontre une autre declaration *)
        match t with
        |[] -> raise (Invalid_argument "eval_declar_aux") (* pas de mot associe -> erreur *)
        |x::tl ->
            let (dico2,q) = eval_declar_aux dico tl [] (to_string x) in (* on recupere le dictionnaire contenant ce nouveau mot et la suite des instructions *)
            eval_declar_aux dico2 q acc n (* on rappelle la fonction avec le mot d'origine, le nouveau dictionnaire et la suite du programme mis a jour *)
      else eval_declar_aux dico t (h::acc) n (* sinon on rajoute l'element dans notre definition et on rappelle la fonction sur notre mot avec la suite du programme *)
  | _ -> raise (Invalid_argument "eval_declar_aux") (* la declaration ne termine pas par un ';' -> erreur*)
;; 
  
(* eval_declar est appelee par la fonction eval quand on rencontre un ':' signifiant le debut d'une declaration 
elle retourne un dictionnaire mis a jour ainsi que la suite du programme *)
let eval_declar (dico:dico) (p:prog) : dico*prog =
  match p with
  |(CST_STRING nom)::t->
      (match t with
       |_::_ -> eval_declar_aux dico t [] nom
       |_->failwith "eval_declar" ) (* chaque declaration se termine par ';' ! *)
  |_-> failwith "eval_declar"  (* pas de nom ??? -> erreur *)
;;                           

(* la fonction eval_mot prend un dictionnaire, un mot et un programme et renvoie 
le programme mis a jour en lui ayant ajoute au debut la suite d'instructions 
correspondant au mot  
Exemple : eval_mot dico CARRE [8 ; *] -> [DUP ; * ; 8 ; *] *)
let eval_mot (dico:dico) (mot:string) (p:prog) : prog =
  (lookup mot dico)@p ;; 


(*prend une liste d'instructions en parametre et un mot et renvoie cette liste 
  modifiee en ayant supprime tous les elements jusqu'a atteindre ce mot *)
let rec supp_until (mot : string) (p : prog) : prog =
  match p with
  |[]->failwith "supp_until"
  |x::q-> if to_string(x)=mot then q else supp_until mot q ;;

(* Concernant la fonction supp_until2, nous avions realise une 
premiere version simple mais qui ne traitait pas le cas des IF imbriques,
en effet on supprimait entre le if et le premier else ou then rencontre,
mais ce n'est pas forcement celui associe au if !
nous en avons donc ecrit ensuite une deuxieme*)

(* let rec supp_until2_v1 (mot1 : string) (mot2 : string) (p : prog) : prog =
     match p with
     |[]->failwith "supp_until2"
     |x::q-> if (to_string(x)=mot1) || (to_string(x)=mot2) then q
         else supp_until2 mot1 mot2 q ;;
*)

(* seconde version permettant de traiter tous les cas *)

(* supp_until2_aux est appelee par supp_until2 *)
let rec supp_until2_aux (mot1 : string) (mot2 : string) (count : int) (p : prog) : prog =
  match p with
  |[] -> failwith "supp_until2_aux" (* on doit forcement trouver un mot d'arret (mot1 ou mot2), sinon -> erreur*)
  |x::q ->
      if (to_string x) = "IF" then supp_until2_aux mot1 mot2 (count + 1) q (* si on rencontre un if imbrique, on rappelle la fonction en incrementant le compteur de 1 *)
      else if ((to_string x) = mot2)||((to_string x) = mot1) then (* sinon si on tombe sur un des deux mots en parametre alors *)
        if count = 0 then q (* si le compteur est a 0, c'est qu'on a traite tous nos if imbriques, on retourne donc la suite du programme*)
        else supp_until2_aux mot1 mot2 (count - 1) q (* sinon, c'est qu'on a termine de traite un if mais qu'il en reste d'autre(s), on rappelle donc la fonction en decrementant le compteur de 1 *)
      else supp_until2_aux mot1 mot2 count q (* sinon on continue jusqu'a atteindre un des deux mots passes en parametre *)
;;

(* supp_until2 est appelee par eval_if dans le cas où le booleen precedent le if
est false ; alors, on veut ignorer toutes les instructions entre ce if et le else 
ou le then associe.
Cette fonction sera appelee avec les mots ELSE (mot1) et THEN (mot2) *)
let supp_until2 (mot1 : string) (mot2 : string) (p : prog) : prog =
  match p with
  |[] -> failwith "supp_until2" (* on doit forcement trouver un mot d'arret (mot1 ou mot2), sinon -> erreur*)
  |x::q ->
      if (to_string x) = "IF" then (* si on rencontre un if imbrique *)
        supp_until2_aux mot1 mot2 1 q (* alors on appelle la fonction auxiliaire en mettant le compteur a 1*)
      else if ((to_string x) = mot2)||((to_string x) = mot1) then q (* sinon si on rencontre un des deux mots d'arret, alors on retourne la suite du programme a partir de ce mot (exclu) *)
      else supp_until2_aux mot1 mot2 0 q (* sinon on appelle la fonction auxiliaire en mettant le compteur a 0 *)
;;

(* eval_if est appelee par la fonction eval lorsque un if est rencontre,
et renvoie l'etat de la pile (donc sans le booleen precedant le if) ainsi que
le programme a traiter *)
let eval_if (stk:stack) (p:prog) : stack * prog =
  match stk with
  |(CST_BOOL b)::q ->
      if to_string(CST_BOOL b)="TRUE" then (q,p) (* si le booleen precedant le if est true, alors la suite du programme est juste apres le if *)
      else (q, supp_until2 "ELSE" "THEN" p) (* sinon si il est false, la suite du programme est juste apres le else associé (cas IF ELSE ENDIF) ou le then associé (cas IF THEN), on appelle pour cela la fonction supp_until2 *)
  |_->failwith "eval_if" (* il faut forcement un booleen dans stk ! *)
;;

(* prend une pile, un programme et un dictionnaire et renvoie la pile obtenue 
a la fin de l'execution du programme *)
let rec eval (dico:dico) (stk:stack) (p:prog) : stack =
  match p with
  |[]->stk (* pas d'instruction -> la pile reste la meme*)
  |(CST_STRING h)::q -> (* si la premiere instruction est CST_STRING alors*)
      let mot = to_string(CST_STRING h) in
      if mot=":" then (* si c'est ':' , on a donc une declaration et on l'ajoute au dictionnaire *)
        let (nvdico,t) = eval_declar(dico)(q) in 
        eval nvdico stk t (* on evalue ensuite la suite du programme *)
      else eval dico stk (eval_mot dico mot q) (* sinon, on cherche le mot dans le dictionnaire et on met a jour la suite du programme (fonction eval_mot) qu'on evalue *)
  |(OP_COND op)::q -> (* si la premiere instruction est une operation conditionnelle alors*)
      let nom_op = to_string(OP_COND op) in
      if nom_op  = "IF" then let (st,t) = eval_if stk q in eval dico st t (* si c'est un if on met a jour la pile et le programme puis on l'evalue *)
      else if (nom_op = "THEN")||(nom_op = "ENDIF") then eval dico stk q (* si c'est un then ou un endif on l'ignore et on evalue la suite *)
      else eval dico stk (supp_until "ENDIF" q) (* sinon, c'est un else : dans ce cas le booleen avant le if etait true, on ignore donc les instructions entre le else et le endif suivant, puis on evalue ce nouveau programme (donc a partir du endif exclu)  *)
  |hd::tl-> eval dico (step stk hd) tl (*si c'est un autre element on l'execute dans la pile stk (fonction step) puis on evalue la suite du programme *)
;;


    
let jeux_de_test = [ (": fact dup 1 > if dup 1 - fact * then ; 6 fact", "720") ]


(* *********** Question 6 *********** *)

let carre n = 
  Printf.sprintf ": carre dup * ; %d carre" n ;;

let fib n = 
  Printf.sprintf ": fib DUP 1 > IF DUP 1 - fib SWAP 2 - fib + THEN ; %d fib" n ;;

(* *********** Question 7 *********** *)

let jeux_de_test = [(": fact DUP 1 > IF DUP 1 - fact * THEN ; 6 fact", "720") ;
                    (": carre DUP * ; 8 carre", "64") ; 
                    ("2 3 * 4 +", "10")  ; 
                    ("4 5 6 ROT + * ", "54") ;
                    ("7 10 * 8 + DUP 42", "42 78 78") ;
                    ("4 5 6 SWAP + * ", "44") ; 
                    (" 2 3 4 * + 2 /","7") ;
                    ("1 DUP +", "2") ; 
                    ("1 2 + 10 *","30") ; 
                    ("1 2 + 5 <","TRUE") ;
                    (": carre DUP + ; : carre DUP * ; : cube DUP carre * ; 1 1 + cube","8") ;
                    (": carre DUP * ; : cube DUP carre * ; 1 1 + cube","8") ;
                    (": tuck DUP ROT SWAP ; 9 12 15 tuck ","12 15 15 9") ; 
                    (": pair DUP 0 = IF DROP TRUE ELSE DUP 1 = IF DROP FALSE ELSE 2 - pair ENDIF ENDIF ; 0 pair 60 pair 79 pair 1 pair ", "FALSE FALSE TRUE TRUE") ;
                    ("5 4 DUP ROT","4 5 4");
                    ("5 4 DUP ROT SWAP DUP","5 5 4 4");
                    ("5 4 DUP ROT SWAP DUP ROT","5 4 5 4");
                    (": max DUP ROT SWAP DUP ROT > IF DROP ELSE SWAP DROP ENDIF ;  9 11 max", "11");
                    (": abs DUP 0 < IF 0 SWAP - THEN ; -3 abs 0 abs 4 abs" , "4 0 3");
                    ("3 4 <>", "TRUE") ] ;;