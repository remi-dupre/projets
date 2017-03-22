Mini projet - Langages formels
===

(Je met des éléments de réponse à postériori, suivant ce dont je me rappel, j'espère ne pas être trop imprécis)

### Question 1.
Les rêgles ne sont pas dans leur ordre de priorité et peuvent en écraser d'autres car l'expression régulière est trop générale. Par exemple le matching de "and" est après celui des variables. Donc "and" sera compris comme une déclaration de variable.

### Question 7.
J'ai implémenté les commentaires bien parenthésés à deux petites restrictions près :
 - le programme ne doit pas commencer par un commentaire (j'avais un petit conflict que je ne voyais pas comment résoudre sinon)
 - Les instructions '(*' et '*)' doivent être entourées d'espaces (ca peut être résolu par une simple expression régulière mais je trouve ça un peu moche)
La gestion des commentaires se fait quasiment entièrement dans l'analyseur gramatical.

### Question 8.
Le lexeur interprète le '=' comme un terminal EQUALS dont on a défini le comportement uniquement dans le contexte d'un "let .. = .. in ...". Il faut donc rajouter des rêgles pour adapter son comportement, en mettant les mêmes rêgles que BIN_COMP.
Le comportement n'est pas encore celui désiré, il manque la gestion de priorité des opérateurs.

### Question 9.
J'ai uniquement utilisé %left pour hiérarchiser les opérateurs selon leur priorité. (et un peu nonassos par la suite)

### Question 11.
Je ne me rappel plus quel était le conflict à cette question, peut être par rapport au signe '=' sur les entrées du type "let v = x=y in v".

### Question 13.
Les priorités sur le moins unaire ne s'adaptent pas tout de suite à ce que l'on désire : par example -x + y est interprété comme -(x+y).

