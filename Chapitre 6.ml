Exercice 1 :
------------


1)

fonction `a list -> `a


let deuxieme = fun

| (_ :: second :: _) ->
	second

| _ -> failwith "La liste n'a pas de second élément."
;;


deuxieme ([1 ; 2 ; 3]) ;;
-> 2

deuxieme ([[1 ; 2] ; [3] ; []]) ;;
-> [3]

deuxieme ([1]) ;;
-> Erreur


2)

fonction `a list -> bool


let auMoinsTrois = fun

(_ :: _ :: _ :: _) ->
	true

| _ -> false
;;


auMoinsTrois ([1 ; 2]) ;;
-> false

auMoinsTrois ([[1 ; 2] ; [3] ; []]) ;;
-> true

auMoinsTrois ([]) ;;
-> false


3)

fonction int list -> list


let sommeTrois = fun

(un :: deux :: trois :: _) ->
	un + deux + trois

| _ -> failwith "La liste a moins de 3 éléments."
;;


sommeTrois ([1 ; 2 ; 3 ; 4]) ;;
-> 6

sommeTrois ([1 ; 2]) ;;
-> Erreur


4)

fonction `a list -> bool


let troisEstPair = fun

(_ :: _ :: troisieme :: _) ->
	troisieme mod 2 = 0

| _ -> failwith "La liste n'a pas de 3e élément."
;;

troisEstPair ([1 ; 2]) ;;
-> Erreur

troisEstPair ([1 ; 2 ; 3 ; 4]) ;;
-> false

troisEstPair ([1 ; 2 ; 6 ; 4]) ;;
-> true

troisEstPair ([]) ;;
-> Erreur


5)

fonction `a * `a list -> `a list


let ajoutDeuxFois = fun

(element, liste) ->
	element :: element :: liste
;;


ajoutDeuxFois (9, [1 ; 2 ; 3]) ;;
-> [9 ; 9 ; 1 ; 2 ; 3]

ajoutDeuxFois ([4 ; 5], [[1 ; 2] ; [3] ; []]) ;;
-> [[4 ; 5] ; [4 ; 5] ; [1 ; 2] ; [3] ; []]

ajoutDeuxFois (0, []) ;;
-> [0 ; 0]


6)

fonction `a list -> `a list


let permute = fun

(premier :: second :: queue) ->
	second :: premier :: queue

| _ -> failwith "La liste a moins de 2 éléments."
;;


permute ([1 ; 2 ; 3]) ;;
-> [2 ; 1 ; 3]

permute ([[1 ; 2] ; [3] ; []]) ;;
-> [[3] ; [1 ; 2] ; []]

permute ([1]) ;;
-> Erreur



Exercice 2 :
------------


1)

1.1)

fonction int * int list -> int list


let rec etendListe = fun

(entier, (tete :: queue)) ->
	if tete = entier
	then tete :: queue
	else etendListe (entier, tete + 1 :: tete :: queue)

| _ -> failwith "La liste n'a pas d'élément de départ."
;;


1.2)

fonction int -> int list


let construitListe = fun

entier ->
	if entier < 0
	then failwith "Entier strictement négatif."
	else etendListe (entier, [0])
;;


construitListe (7) ;;
-> [7 ; 6 ; 5 ; 4 ; 3 ; 2 ; 1 ; 0]

construitListe (0) ;;
-> [0]


2)

fontion `a list -> int


let rec longueur = fun

[] -> 0

(_ :: queue) ->
	longueur (queue) + 1
;;


longueur ([1 ; 2 ; 3]) ;;
-> 3

longueur ([[1 ; 2] ; [3] ; []]) ;;
-> 3

longueur ([]) ;;
-> 0


3)

fonction `a list -> `a


let rec dernier = fun

(_ :: second :: queue) ->
	dernier (second :: queue)

| [element] -> element

| _ -> failwith "La liste est vide."
;;


dernier ([1 ; 2 ; 3]) ;;
-> 3

dernier ([[1 ; 2] ; [3] ; []]) ;;
-> []

dernier ([]) ;;
-> Erreur


4)

4.1)

fonction int list -> int


let rec sommeNonVide = fun

(tete :: queue) ->
	tete + sommeNonVide (queue)

| _ -> 0
;;


4.2)

fonction int list -> int


let somme = fun

[] -> failwith "La liste est vide."

| liste -> sommeNonVide (liste)
;;


somme ([1 ; 2 ; 4]) ;;
-> 7

somme ([0]) ;;
-> 0

somme ([]) ;;
-> Erreur


5)

fonction `a list -> bool


let rec taillePaire = fun

(_ :: queue) ->
	not taillePaire (queue)

| _ -> true
;;


taillePaire ([1 ; 2 ; 3 ; 4]) ;;
-> true

taillePaire ([[1 ; 2] ; [3] ; []]) ;;
-> false

taillePaire ([]) ;;
-> true


6)

6.1)

fonction bool * `a list * `a list -> bool * `a list * `a list


let rec rangImpairTuple = fun

(impair, (teteDepart :: queueDepart), sortie) ->
	if impair
	then rangImpairTuple (false, queueDepart, teteDepart :: sortie)
	else rangImpairTuple (true, queueDepart, sortie)

| (_, _, sortie) -> (false, [], sortie)
;;


6.2)

fonction `a list * `a list -> `a list * `a list


let rec reconstruit = fun

((teteDepart :: queueDepart), sortie) ->
	reconstruit (queueDepart, teteDepart :: sortie)

| (_, sortie) -> ([], sortie)
;;


6.3)

fonction `a list -> `a list


let rangImpair = fun

liste ->
	let (_, _, rangImpairInverse) = rangImpairTuple (true, liste, []) in
		snd (reconstruit (rangImpairInverse, []))
;;


rangImpair ([1 ; 2 ; 3 ; 4]) ;;
-> [1 ; 3]

rangImpair ([[1 ; 2] ; [3] ; []]) ;;
-> [[1 ; 2] ; []]

rangImpair ([]) ;;
-> []



Exercice 3 :
------------


1)

fonction  '_a * '_a list -> bool


let rec appartient = fun

(element, tete :: queue) ->
	element = tete
	or appartient (element, queue)

| _ -> false
;;


appartient (3, [1 ; 2 ; 3 ; 4]) ;;
-> true

appartient ([3], [[1 ; 2] ; [3] ; []]) ;;
-> true

appartient (5, [1 ; 2 ; 3 ; 4]) ;;
-> false

appartient ([3 ; 4], [[1 ; 2] ; [3] ; []]) ;;
-> false

appartient (1, []) ;;
-> false


2)

2.1)

fonction '_a * '_a list -> '_a


let rec maximumCouple = fun

(maxi, tete :: queue) ->
	if tete > maxi
	then maximumCouple (tete, queue)
	else maximumCouple (maxi, queue)

| (maxi, _) -> maxi
;;


2.2)

fonction '_a list -> '_a


let maximum = fun

(tete :: queue) ->
    maximumCouple (tete, queue)

| _ -> failwith "La liste est vide."
;;


maximum ([1 ; 2 ; 4 ; -3]) ;;
-> 4

maximum ([0]) ;;
-> 0

maximum ([]) ;;
-> Erreur


3)

fonction '_a * '_a list -> int


let rec occurences = fun

(* Cas récursif *)
(element, tete :: queue) ->
    if element = tete
    then occurences (element, queue) + 1
    else occurences (element, queue)

(* Cas de base *)
| _ -> 0
;;

occurences (46, [1 ; -2 ; 46 ; 4 ; -3 ; 46]) ;;
-> 2

occurences (2, [0 ; -1]) ;;
-> 0

occurences (0, []) ;;
-> 0


4)

fonction int list -> int list


let rec fois2 = fun

(* Cas récursif *)
(tete :: queue) ->
    tete * 2 :: fois2 (queue)

(* Cas de base *)
| _ -> []
;;


fois2 ([1 ; -2 ; 46 ; 4 ; -3 ; 46]) ;;
-> [2 ; -4 ; 92 ; 8 ; -6 ; 92]

fois2 ([0]) ;;
-> [0]

fois2 ([]) ;;
-> []


5)

fonction int * int list -> int list


let rec insere = fun

(entier, tete :: queue) ->
    if entier > tete
    (* Cas récursif *)
    then tete :: insere (entier, queue)
    (* Cas de base *)
    else entier :: tete :: queue

(* Cas élémentaire *)
| (entier, _) -> [entier]
;;


insere (-32, [-46 ; -3 ; -2 ; 0 ; 1 ; 4 ; 46]) ;;
-> [-46 ; -32 ; -3 ; -2 ; 0 ; 1 ; 4 ; 46]

insere (1, [0]) ;;
-> [0 ; 1]

insere (0, []) ;;
-> [0]



Exercice 4 :
------------


1)

1.1)

fonction int * '_a list -> '_a


let rec iemePositif = fun

(* Cas de base*)
(0, tete :: queue) -> tete

(* Cas récursif *)
| (indice, tete :: queue) ->
    iemePositif (indice - 1, queue)

| _ -> failwith "Indice en dehors des limites de la liste."
;;


1.2)

fonction int * '_a list -> '_a


let ieme = fun

(indice, liste) ->
    if indice < 0
    then failwith "Indice négatif."
    else iemePositif (indice, liste)
;;


ieme (3, [1 ; -2 ; 46 ; 4 ; -3 ; 46]) ;;
-> 4

ieme (6, [1 ; -2 ; 46 ; 4 ; -3 ; 46]) ;;
-> Erreur

ieme (-1, [1 ; -2 ; 46 ; 4 ; -3 ; 46]) ;;
-> Erreur

ieme (1, [[1 ; 2] ; [3] ; []]) ;;
-> [3]

ieme (0, [0]) ;;
-> 0

ieme (0, []) ;;
-> Erreur


2)

2.1)

fonction int * '_a list -> '_a list


let rec prendrePositif = fun

(* Cas de base *)
(0, _) -> []

(* Cas récursif *)
| (nbElements, tete :: queue) ->
    tete :: prendrePositif (nbElements - 1, queue)

| _ -> failwith "Indice en dehors des limites de la liste."
;;


2.2)

fonction int * '_a list -> '_a list


let prendre = fun

(nbElements, liste) ->
    if nbElements < 0
    then failwith "Nombre négatif d'éléments sélectionnés."
    else prendrePositif (nbElements, liste)
;;


prendre (4, [1 ; -2 ; 46 ; 4 ; -3 ; 46]) ;;
-> [1 ; -2 ; 46 ; 4]

prendre (7, [1 ; -2 ; 46 ; 4 ; -3 ; 46]) ;;
-> Erreur

prendre (-1, [1 ; -2 ; 46 ; 4 ; -3 ; 46]) ;;
-> Erreur

prendre (2, [[1 ; 2] ; [3] ; []]) ;;
-> [[1 ; 2] ; [3]]

prendre (1, [0]) ;;
-> [0]

prendre (0, []) ;;
-> []


3)

3.1)

fonction int * '_a list -> '_a list


let rec enlevePositif = fun

(* Cas élémentaire*)
(_, []) -> []

(* Cas de base *)
| (0, liste) -> liste

(* Cas récursif *)
| (nbElements, tete :: queue) ->
    enlevePositif (nbElements - 1, queue)
;;


3.2)

fonction int * '_a list -> '_a list


let enleve = fun

(nbElements, liste) ->
    if nbElements < 0
    then failwith "Nombre négatif d'éléments sélectionnés."
    else enlevePositif (nbElements, liste)
;;


enleve (4, [1 ; -2 ; 46 ; 4 ; -3 ; 46]) ;;
-> [-3 ; 46]

enleve (7, [1 ; -2 ; 46 ; 4 ; -3 ; 46]) ;;
-> []

enleve (-1, [1 ; -2 ; 46 ; 4 ; -3 ; 46]) ;;
-> Erreur

enleve (1, [[1 ; 2] ; [3] ; []]) ;;
-> [[3] ; []]

enleve (0, [0]) ;;
-> [0]

enleve (0, []) ;;
-> []


4)

fonction '_a list * '_b list -> ('_a * '_b) list


let rec melange = fun

(* Cas récursif *)
(teteA :: queueA, teteB :: queueB) ->
    (teteA, teteB) :: melange (queueA, queueB)

(* Cas de base *)
| _ -> []
;;


melange ([1 ; -2 ; 46 ; 4 ; -3 ; 46], ["bonsoir" ; "bonjour" ; "au revoir"]) ;;
-> [(1, "bonsoir") ; (-2, "bonjour") ; (46, "au revoir")]

melange ([0.5], [[]]) ;;
-> [(0.5, [])]

melange ([5 ; -4], []) ;;
-> []

melange ([], [5 ; -4]) ;;
-> []

melange ([], []) ;;
-> []



Exercice 5 :
------------


5)

5.1)

fonction int * int list -> int list


let rec fibonacciListe = fun

(* Cas de base *)
(0, liste) -> liste

(* Cas récursif *)
| (compteur, entier_b :: entier_a :: queue) ->
	let entier_c = entier_b + entier_a in
		fibonacciListe (compteur - 1, entier_c :: entier_b :: entier_a :: queue)

| _ -> failwith "Erreur de fonction principale."
;;


5.2)

fonction '_a list * '_a list -> '_a list * '_a list


let rec reconstruit = fun

((teteDepart :: queueDepart), sortie) ->
	reconstruit (queueDepart, teteDepart :: sortie)

| (_, sortie) -> ([], sortie)
;;


5.3)

fonction '_a list -> '_a list


let reverse = fun

liste -> snd (reconstruit (liste, []))
;;


5.4)

fonction int -> int list


let fibonacci = fun

(* Cas élémentaires *)
0 -> []
| 1 -> [1]

| nbTermes ->
	if nbTermes < 0
	then failwith "Nombre de termes négatif."
	else reverse (fibonacciListe (nbTermes - 2, [1 ; 1]))
;;

fibonacci (0) ;;
-> []

fibonacci (8) ;;
-> [1; 1; 2; 3; 5; 8; 13; 21]

fibonacci (1000 * 1000) ;;
-> [1; 1; 2; 3; 5; 8; 13; 21; 34; 55; 89; 144; 233; 377; 610; 987; 1597; 2584;
  4181; 6765; 10946; 17711; 28657; 46368; 75025; 121393; 196418; 317811;
  514229; 832040; 1346269; 2178309; 3524578; 5702887; 9227465; 14930352;
  24157817; 39088169; 63245986; 102334155; 165580141; 267914296; 433494437;
  701408733; -1012580478; -311171745; 823731425; 512559680; -811192543;
  -298632863; 1037658242; 739025379; -370800027; 368225352; -2574675;
  365650677; 363076002; 728726679; -1055680967; -326954288; 764848393;
  437894105; -944741150; -506847045; 695895453; 189048408; 884943861;
  -1073491379; -188547518; 885444751; 696897233; -565141664; 131755569;
  -433386095; -301630526; -735016621; -1036647147; 375819880; -660827267;
  -285007387; -945834654; 916641607; -29193047; 887448560; 858255513;
  -401779575; 456475938; 54696363; 511172301; 565868664; -1070442683;
  -504574019; 572466946; 67892927; 640359873; 708252800; -798870975;
  -90618175; -889489150; ...]



Exercice 6 :
------------


1)

fonction '_a list -> bool


let rec estCroissante = fun

(* Cas récursif *)
(tete :: cou :: queue) ->
	tete <= cou & estCroissante (cou :: queue)

(* Cas de base ou élémentaire *)
| _ -> true
;;


estCroissante ([-5 ; -2 ; 0 ; 36]) ;;
-> true

estCroissante ([-5 ; -2 ; 0 ; 36 ; 35]) ;;
-> false

estCroissante ([42]) ;;
-> true

estCroissante ([]) ;;
-> true


2)

2.1)

fonction '_a list * '_a list -> '_a list


let rec fusionRec = fun

(* Cas de base *)
(liste_a, []) -> liste_a
| ([], liste_b) -> liste_b

(* Cas récursif *)
| (tete_a :: queue_a, tete_b :: queue_b) ->
	if tete_a < tete_b
	then tete_a :: fusionRec (queue_a, tete_b :: queue_b)
	else tete_b :: fusionRec (tete_a :: queue_a, queue_b)
;;


2.2)

fonction '_a list * '_a list -> '_a list


let fusion = fun

(liste_a, liste_b) ->
	if estCroissante (liste_a)
		& estCroissante (liste_b)
	then fusionRec (liste_a, liste_b)
	else failwith "Au moins une des listes n'est pas croissante."
;;


fusion ([-4 ; -3 ; 2], [-5 ; -2 ; 0 ; 36]) ;;
-> [-5; -4; -3; -2; 0; 2; 36]

fusion ([-4 ; -3 ; 2], [-5 ; -2 ; 0 ; 36 ; 35]) ;;
-> Erreur

fusion ([-4 ; -3 ; 2], []) ;;
-> [-4 ; -3 ; 2]

fusion ([], [-5 ; -2 ; 0 ; 36]) ;;
-> [-5 ; -2 ; 0 ; 36]

fusion ([], []) ;;
-> []



Exercice 7 :
------------


1)

1.1)

fonction int * int -> int list


let rec genererBorne = fun

(compteur, borne) ->
	if compteur = borne
	(* Cas de base *)
	then [borne]
	(* Cas récursif *)
	else compteur :: genererBorne (compteur + 1, borne)
;;


1.2)

fonction int -> int list


let generer = fun

borne ->
	if borne < 2
	then failwith "Borne trop petite"
	else genererBorne (2, borne)
;;

generer (8) ;;
-> [2 ; 3 ; 4 ; 5 ; 6 ; 7 ; 8]

generer (2) ;;
-> [2]

generer (1) ;;
-> Erreur


2)

2.1)

fonction int * int list -> int list


let rec eliminerNonNul = fun

(* Cas récursif *)
(diviseur, tete :: queue) ->
	let appel = eliminerNonNul (diviseur, queue) in
	
		if tete mod diviseur = 0
		then appel
		else tete :: appel
		
(* Cas de base *)
| _ -> []
;;


2.2)

fonction int * int list -> int list


let eliminer = fun

(0, _) -> failwith "Diviseur nul"

| (diviseur, liste) ->
	eliminerNonNul (diviseur, liste)
;;


eliminer (-2, [2 ; 5 ; 4 ; -5 ; -2 ; -4 ; 8 ; 3]) ;;
-> [5 ; -5 ; 3]

eliminer (0, [2 ; 5 ; 4 ; -5 ; -2 ; -4 ; 8 ; 3]) ;;
-> Erreur

eliminer (1, [2 ; 5 ; 4 ; -5 ; -2 ; -4 ; 8 ; 3]) ;;
-> [2 ; 5 ; 4 ; -5 ; -2 ; -4 ; 8 ; 3]


3)

3.1)

fonction int list -> int list


let rec eratosRec = fun

(* Cas récursif *)
(tete :: queue) ->
	tete :: eratosRec (eliminer (tete, queue))
	
(* Cas de base *)
| _ -> []
;;


3.2)

fonction int -> int list


let eratos = fun

entier -> 
	let depart = generer (entier) in
		eratosRec (depart)
;;


eratos (42) ;;
-> [2 ; 3 ; 5 ; 7 ; 11 ; 13 ; 17 ; 19 ; 23 ; 29 ; 31 ; 37 ; 41]

eratos (2) ;;
-> [2]

eratos (1) ;;
-> Erreur


4)

4.1)

fonction int list -> (int * int) list


let rec jumeauxRec = fun

(* Cas récursif *)
(tete :: cou :: queue) ->
	let appel = jumeauxRec (cou :: queue) in
	
		if cou - tete = 2
		then (tete, cou) :: appel
		else appel
		
(* Cas de base *)
| _ -> []
;;


4.2)

int list -> (int * int) list


let jumeaux = fun

(tete :: cou :: queue) ->
	let liste = tete :: cou :: queue in
	
		if not estCroissante (liste)
		then failwith "Liste d'entrée non triée"
		else
		
		(* Cas élémentaire (2, 3) *)
			if tete = 2
			& cou = 3
			then (2, 3) :: jumeauxRec (queue)
			else jumeauxRec (liste)

(* Cas élémentaire où la liste a 0 ou 1 élément *)
| _ -> []
;;


jumeaux ([2 ; 3 ; 5 ; 7 ; 11 ; 13 ; 17 ; 19 ; 23 ; 29 ; 31 ; 37 ; 41]) ;;
-> [(2, 3) ; (5, 7) ; (11, 13) ; (17, 19) ; (29, 31)]

jumeaux ([2 ; 3 ; 5]) ;;
-> [(2, 3)]

jumeaux ([2]) ;;
-> []

jumeaux ([]) ;;
-> []


5)

fonction int -> (int * int) list


let listeJumeaux = fun

borne ->
	jumeaux (eratos (borne))
;;


listeJumeaux (42) ;;
-> [(2, 3) ; (5, 7) ; (11, 13) ; (17, 19) ; (29, 31)]

listeJumeaux (3) ;;
-> [(2, 3)]

listeJumeaux (2) ;;
-> []

listeJumeaux (1) ;;
-> Erreur



Exercice 8 :
------------


1)

1.1)

fonction bool * '_a * '_a list -> bool


let rec appartientRec = fun

(* Cas de base vrai *)
(true, _, _) -> true

(* Cas récursif *)
| (_, element, tete :: queue) ->
	appartientRec (element = tete, element, queue)

(* Cas de base faux *)
| _ -> false
;;


1.2)

fonction '_a * '_a list -> bool


(* Complexité linéaire *)
let appartient = fun

(element, ens) ->
	appartientRec (false, element, ens)
;;


appartient (3, [-5 ; 0 ; 3 ; -45]) ;;
-> true

appartient (2, [-5 ; 0 ; 3 ; -45]) ;;
-> false

appartient (0, [0]) ;;
-> true

appartient (0, []) ;;
-> false


2)

2.1)

fonction '_a list * '_a list -> '_a list


let rec union = fun

(* Cas récursif *)
(tete :: queue, ensBase) ->
	let appel = union (queue, ensBase) in
	
		if appartient (tete, ensBase)
		then appel
		else tete :: appel

(* Cas de base *)
| (_, ens) -> ens
;;


union ([-5 ; 0 ; 3 ; -45 ; 56], [3 ; 56 ; -4]) ;;
-> {-45, -5, -4, 0, 3, 56}

union ([-5 ; 0 ; 3 ; -45 ; 56], []) ;;
-> {-45, -5, 0, 3, 56}

union ([], [3 ; 56 ; -4]) ;;
-> {-4, 3, 56}

union ([0], [0]) ;;
-> {0}

union ([], []) ;;
-> {}


2.2)

fonction '_a list * '_a list -> '_a list


let rec intersection = fun

(* Cas récursif *)
(tete :: queue, ensBase) ->
	let appel = intersection (queue, ensBase) in
	
		if appartient (tete, ensBase)
		then tete :: appel
		else appel

(* Cas de base *)
| _ -> []
;;


intersection ([-5 ; 0 ; 3 ; -45 ; 56], [3 ; 56 ; -4]) ;;
-> {3, 56}

intersection ([-5 ; 0 ; 3 ; -45 ; 56], []) ;;
-> {}

intersection ([], [3 ; 56 ; -4]) ;;
-> {}

intersection ([0], [0]) ;;
-> {0}

intersection ([], []) ;;
-> {}


3)

fonction '_a list * '_a list -> bool


let inclus = fun

(ens_a, ens_b) ->
	let petit = intersection (ens_a, ens_b) in

		petit = ens_a
		or petit = ens_b
;;


inclus ([3 ; 56 ; -4], [-5 ; 0 ; 3 ; -45 ; 56]) ;;
-> false

inclus ([3 ; 56 ; -4], [-4 ; -5 ; 0 ; 3 ; -45 ; 56]) ;;
-> true

inclus ([], [3 ; 56 ; -4]) ;;
-> true

inclus ([-5 ; 0 ; 3 ; -45 ; 56], [3 ; 56 ; -4]) ;;
-> false

inclus ([0], [0]) ;;
-> true

inclus ([], []) ;;
-> true


4)

fonction '_a list * '_a list -> bool


let disjoint = fun

(ens_a, ens_b) ->
	intersection (ens_a, ens_b) = []
;;


disjoint ([-5 ; 0 ; 3 ; -45 ; 56], [4 ; 57 ; -4]) ;;
-> true

disjoint ([-5 ; 0 ; 3 ; -45 ; 56], [3 ; 56 ; -4]) ;;
-> false

disjoint ([-5 ; 0 ; 3 ; -45 ; 56], []) ;;
-> true

disjoint ([], [3 ; 56 ; -4]) ;;
-> true

disjoint ([0], [0]) ;;
-> false

disjoint ([], []) ;;
-> true


5)

fonction '_a list * '_a list -> bool


let egaux = fun

(ens_a, ens_b) ->
	union (ens_a, ens_b)
	= intersection (ens_a, ens_b)
;;


egaux ([1 ; 2 ; 3], [2 ; 3 ; 1]) ;;
-> true

egaux ([1 ; -2 ; 3], [2 ; 3 ; 1]) ;;
-> false

egaux ([1 ; 3], [2 ; 3 ; 1]) ;;
-> false

egaux ([1 ; 2 ; 3], []) ;;
-> false

egaux ([], [2 ; 3 ; 1]) ;;
-> false

egaux ([0], [0]) ;;
-> true

egaux ([], []) ;;
-> true


6)

'_a list * '_a list -> '_a list


let rec complement = fun

(* Cas récursif *)
(tete :: queue, ensBase) ->
	let appel = complement (queue, ensBase) in
	
		if appartient (tete, ensBase)
		then appel
		else tete :: appel

(* Cas de base *)
| _ -> []
;;


complement ([1 ; 2 ; 3], [2 ; 3 ; 1]) ;;
-> {}

complement ([1 ; -2 ; 3], [2 ; 3 ; 1]) ;;
-> {-2}

complement ([1 ; 3], [2 ; 3 ; 1]) ;;
-> {}

complement ([1 ; 2 ; 3], []) ;;
-> {1, 2, 3}

complement ([], [2 ; 3 ; 1]) ;;
-> {}

complement ([0], [0]) ;;
-> {}

complement ([], []) ;;
-> {}


7)

7.1)

fonction '_a list * '_a list -> '_a list


let rec ensembleRec = fun

(* Cas récursif *)
(tete :: queue, ens) ->
	if appartient (tete, ens)
	then ensembleRec (queue, ens)
	else ensembleRec (queue, tete :: ens)
	
(* Cas de base *)
| (_, ens) -> ens
;;


7.2)

fonction '_a list -> '_a list


let ensemble = fun

liste ->
	ensembleRec (liste, [])
;;


ensemble ([2 ; 3 ; 1]) ;;
-> {1, 2, 3}

ensemble ([2 ; -2 ; 1 ; 2 ; 1 ; -2 ; 1 ; 3]) ;;
-> {-2, 1, 2, 3}

ensemble ([]) ;;
-> {}


8)

8.1)

fonction '_a * '_a list list -> '_a list list


let rec ajouts = fun

(* Cas récursif *)
(element, listeTete :: tableauQueue) ->
	(element :: listeTete) :: ajouts (element, tableauQueue)

(* Cas de base *)
| _ -> [[]]
;;


8.2)

fonction '_a list -> '_a list list


let rec partiesRec = fun

(* Cas récursif *)
(tete :: queue) ->
	let appel = partiesRec (queue) in
	ajouts (tete, appel)
	@ appel

(* Cas de base *)
| _ -> [[]]
;;


8.3)

fonction '_a list -> '_a list list


let parties = fun

listeBase ->
	let tableauParties = partiesRec (listeBase) in
		ensemble (tableauParties)
;;

parties ([2 ; 3 ; 1]) ;;
-> {{}, {1}, {2}, {3}, {1, 2}, {1, 3}, {2, 3}, {1, 2, 3}}

parties ([0]) ;;
-> {{}, {0}}

parties ([]) ;;
-> {{}}
