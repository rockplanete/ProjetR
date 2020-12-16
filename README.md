# ProjetR

Projet2

Ligne à rentrer pour récuperer le package : 

```{r}
devtools::install_github("rockplanete/ProjetR")
library(ProjetR)
```


Le programme s'utilise de cette façon : 

Il faut utiliser la fonction OneSimu() en rentrant plusieurs paramètres à l'intérieur 

```{r}
OneSimu(largeur,hauteur,evolve)
```

les paramètres:

largeur : La largeur de la matrice (int)

hauteur : la hauteur de la matrice (int)

evolve : Les deux modes possibles de la simulation (bool) 
--> TRUE : Mode avec le nombre de 0 initial evolutif, 
           à la fin on aura une courbe de l'évolution 
           du nombre de triangle en fonction du nombre de 0 
           dans la génération intiale.  
           (Attention : le programme dure environ 7 minutes
           à s'éxecuter)
        
--> FALSE : Mode avec une seule génération de triangle et 
            le nombre de 0 et de 1 généré avec une probabilité
            égale à 1/2.
