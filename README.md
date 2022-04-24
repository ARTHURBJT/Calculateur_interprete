# Calculateur_interprete

Un document explicatif arrive prochainement.

Le but est de réaliser un interprète et calculateur d'expression donnée sous la forme de chaine de caractère. L'utilisation est donc la même qu'avec une calculatrice standard, le programme renvoyant le resultat du calcul donné en argument. Tout l'intérêt est dans la construction de l'interprète, comment transformer une chaine de caractère pour que la machine comprenne le calcul et renvoie un resultat.

J'ai ajouté beaucoup de document Ocaml à ce projet pour expliciter la démarche :

-J'ai commencé par réaliser un interprète et calculateur d'expression arithmétique (addition, multiplication, soustration, division euclidienne...) ne contenant que des chiffres, ensuite je l'ai étendu aux entiers (int), puis aux expression de calcul avec des réels (float) (division, puissance, ...). Et enfin j'ai étendu ce dernier programme à quasiment toute les expressions que l'on peut calculer avec une calculatrice en ajoutant les fonctions usuelles (exp, ln, cos, arcsin, tanh, ...).

-J'ai d'autre part étendu le programme de calcul avec des réels, (sans les fonctions usuelles) au calcul d'expresion complexe (cplx) avec notamment l'addition, la multiplication, la division, la puissance entière... Le programme renvoie toujours une expression algébrique.

-Enfin le projet contient un programme de calcul rationnel (rat), qui respecte ce type (addition, soustraction, multiplication, division). Le programme renvoie toujours une fraction irreductible.
