# Gcode Manuel

Le gcode est un langage qui est utilisé dans la plupart des impressions 3D. C'est le code qui va ordonner toutes les démarches de l'impression allant aux mouvements, de la température (buse/plateau), jusqu'à la gestion du ventilateur.

Par avance, les prochaines informations concernent que les imprimantes 3D par filament plastique (couche par couche). Donc les imprimantes à résines, les fraiseuses CNC, ou encore les imprimantes 3D à autres matériaux telles que des extraits de bois ou autres ne sont pas concernés...

Les commentaires sont exprimés par ```[...] ;``` _(en n'oubliant pas l'espace)_.

Je ne prends pas en compte les commandes de récupération et d'affichage de données en cours.
____
Avant tout pour mieux comprendre, on instaure des notations (toutes attachées) qui permettent d'indiquer nos valeurs :
* ```X@``` => X + la coordonnée x
* ```Y@``` => Y + la coordonnée y
* ```Z@``` => Z + la coordonnée z
* ```I@``` => X + la coordonnée relative de x _(d'après le point de rotation)_
* ```J@``` => Y + la coordonnée relative de y _(d'après le point de rotation)_
* ```F@``` => F + la vitesse en mm/min de la tête d'impression pour les mouvements
* ```E@``` => E + la longueur du filament en millimètre extruder par l'extrudeur
* ```T@``` => T + la buse choisit 
* ```@t``` => constante
* _**P'tite astuce** (pour F@ & T@) :_ il peut être utilisé seul dans une ligne pour appliquer à toutes les commandes prochaines.
___


### Plan des commandes
* Gestion de fichiers
* Les contrôles générales nécessaires et d'urgences
* Les mouvements
* L'interprétation des positions
* L'extrudeur
* La température
* L'étalonnage et le nivellement du plateau
* Le ventilateur
* Quelques commentaires de démarrage


<br/>

## Gestion de fichiers et de sauvegardes
* ```M21``` -> **monter la carte mémoire** (s'il est mise)
* ```M22``` -> **démonter la carte mémoire**
* ```M23 [fichier.gcode]``` -> **sélectionner un fichier donné** de la carte mémoire
* ```M500``` -> **sauvegarder les paramètres personnalisés actuels dans l'EEPROM**
  * L'avantage de l'EEPROM est que les données seront **non volatile** (concervées même après une coupure de courant) et **modifiable**


<br/><br/>

## Les contrôles générales nécessaires et d'urgences
* ```M25``` -> **mettre en pause l'impression** depuis la carte sonde
* ```M24``` -> **reprendre l'impression** depuis la carte sonde
* ```M112``` -> **arrêt d'urgence**, à n'utiliser qu'en cas de nécessité absolue (collisions, dangers potentiels, etc)
* _Communication ponctuelle avec l'utilisateur_ :<br/>
  ```M300 S@ P@``` -> **déclencher un son** en Hz _(```P@```)_ pendant un temps donné en secondes _(```S@```)_


<br/><br/>

## Les mouvements
* ```G28 [axes potentielles]``` -> **retour position d'origine** _(home position)_
    * Si un ou plusieurs axes sont spécifiés (ex. X, Y, Z), seuls ces axes sont déplacés
#### [En ligne droite][source de déplacement en ligne droite]
* ```GO X@ Y@``` -> déplacement **rapide**
    * _Attention :_ ce déplacement n'est pas contrôlé, la machine va juste se déplacer à la destination
    <br/> => mouvement brutal
    * _Application :_ déplacement sans extrusion
* ```G1 F@ X@ Y@``` -> déplacement **contrôlé**
    * _Application :_ déplacement avec extrusion

#### [Circulaire][source de déplacement circulaire]
* ```G2 F@ X@ Y@ I@ J@``` -> déplacement dans le _sens d'une aiguille d'une montre_
    * _Attention :_ il faut bien connaître la notion de position relative par rapport au point de rotation
* ```G3 F@ X@ Y@ I@ J@``` -> déplacement dans le _sens **contraire** d'une aiguille d'une montre_
    * _Exemple :_ il y a un bon exemple à la fin de cette [page][source de déplacement circulaire]

#### Accélération
```M204 P@ R@ T@``` -> **contôler** l'accélération des mouvements
  *  ```P@``` => P + accélération utilisée pendant l'extrusion en mm/s**2
  *  ```T@``` => P + accélération utilisée pendant les mouvements sans extrusion en mm/s**2
  *  ```R@``` => R + accélération utilisée pendant les mouvements de rétractation de filament en mm/s**2


<br/><br/>

## L'interprétation des positions
* ```G90``` -> mode de positionnement **absolu**
    * _Aide :_ différencier l'absolu et le relatif [ici][source sur les positions]
* ```G91``` -> mode de positionnement **relative**
* ```G92 X@ Y@ Z@ E@``` -> définir la position actuelle comme **position par défaut** _(la nouvelle origine)_
    * _Application :_ Réinitialiser à 0 uniquement l'extrudeur


<br/><br/>

## L'extrudeur
Pour extruder du filament, on doit utiliser le paramètre E@ sur les mouvements quelques conques (de préférence contrôlés). Ce paramètre n'agit que sur l'extrudeur en extrudant une longueur donnée de filament en millimètre.

* ```M82``` -> mode d'extrusions **absolu** 
  <br/> => quantité totale depuis le début de l'impressions
* ```G83``` -> mode d'extrusion en **relative**
    <br/> => quantité à ajouter à l'extrudeur au moment donné
  * _Application :_ c'est celui le plus pratique pour la tortue 

*  _Quelques exemples :_
   * **Précharger la buse** (conseiller en début d'impression) : 
      ```gcode
      G1 E10 F100
      ```
   * **Retirer du filament** (donc en négatif) : 
      ```gcode
      G1 E-5 F100
      ```
   * **Réinitialiser uniquement l'extrudeur**
      ```gcode
      G92 E0
      ```
   * **Les méthodes de priming** _(purger)_
    
    Ce sont des méthodes permettant d'éliminer les résidus de filaments, prévenir les bulles d'air et assurer une extrusion de qualité fluide)
     * avant de commencer l'impression
     * après un changement de filament
     * faire un Skirt _(entourer la pièce au début de l'impression)_ pour une buse prête en adhérence sur le plateau
     * Naprès une pause/arrêt


<br/><br/>

## La température
Les _Ultimakers_ préfèrent attendre à chaque fois que la température soit attente par le plateau puis l'extrudeur (il ne fait pas les deux en même temps).

Pour les prochaines commandes, 
* ```S@``` => S + la température **minimum** en degrée
* ```R@``` => R + la température **maximum** en degrée


#### Le plateau (lit chauffant)
* ```M140 S@ R@``` -> définir la température du plateau **sans attendre**
* ```M190 S@ R@``` -> définir la température du plateau et **attendre qu’elle soit atteinte**.
  * _P'tite info :_ il faut faire attention sur la buse attribuée, il y en a une commande qui est destinée à la première buse et l'autre à la seconde...

#### L'extrudeur _(T@ optionnel)_
* ```M104 S@ R@ T@``` -> définir la température de l’extrudeur **sans attendre**
* ```M109 S@ R@ T@``` -> définir la température de l’extrudeur et **attendre qu’elle soit atteinte**.


<br/><br/>

## L'étalonnage et le nivellement du plateau _(ajuster la hauteur de l'axe Z)_
Avant tout, il faut retourner sur la position HOME avec ```G28```,
* ```M17``` -> **activer les moteurs** _(pour empêcher le déplacement manuelle, ainsi avoir une stabilité) 
* ```M18``` -> **désactiver les moteurs** _(pour reprendre le déplacement manuelle des axes)_ 
  * ```M84 S@``` -> similaire à ```M18``` en allant petit à petit, moteurs par moteurs dans un temps donné en secondes _(```S@```)_, soit plus écolo et contrôlé

* **modifier manuellement** le décalage de l'axe Z entre **la sonde de nivellement** (capteur) ET **la buse** 
  ```gcode
  M851 Z@
  M500
  ```

* ```G29``` -> **démarrer le nivellement automatique**
  
* **activer la compensation de la carte de nivellement** acquis par le nivellement automatique _(dépend selon les firmwares)_
  ```gcode
  M420 S1
  M500
  ```
* **désactiver la compensation** _(soit fais abstraction du nivellement automatique)_
  ```gcode
  M420 S0
  M500
  ```

<br/><br/>

## Le ventilateur
* ```M106 S@ P@ F@``` -> **régler la vitesse** du ventilateur _(en allumant)_
  *  ```S@``` => S + la vitesse du ventilateur de ```0``` à ```255``` _(signal activé en continu soit 100%)_ en unités PWM _(Pulse Width Modulation)_
  *  ```P@``` => P + le ventilateur concerné _(sans ce paramètre, tout les ventilateurs sont concernés)_
  *  ```F@``` => F + la fréquence PWM _(optionnelle mais efficace pour réduire le bruit)_
*  ```M107``` -> **désactiver** le ventilateur 

<br/><br/>

## Quelques commentaires de démarrage

#### Ceux auquels l'imprimante a besoin pour démarrer
* **début des commentaires**
  ```
  ;START_OF_HEADER
  ;HEADER_VERSION:@t
  ```
* ```;FLAVOR:Griffin``` -> la version spécifique du G-code utilisé _(par exemple, celui de Ultimaker est ```Griffin```)_

<br/>

____________________________

  Dans notre cas, on s'est basé sur la manière de coder du slicer Cura pour déclencher proprement l'impression. 
  * Pour prévenir l'imprimante, il faut y spécifier **la source et la version du générateur** (ainsi que la date auquel le générateur a généré le fichier) :
    ```
    ;GENERATOR.NAME:@t
    ;GENERATOR.VERSION:@t
    ;GENERATOR.BUILD_DATE:@t
    ```
    * _P'tit problématique :_ le slicer Cura ne met pas à jour la date de l'exportation gcode, car elle correspond à chaque fois à l'installation de la dernière version installée du logiciel
    * _Application :_ dans notre projet, on utilisera ce commentaire à la date de la génération du gcode

* ```;TARGET_MACHINE.NAME:@t``` -> **définir le nom de l'imprimante** _(marque et modèle)_

* **les paramètres spécialement pour la tête d'impression**
  * ```;EXTRUDER_TRAIN.@t1.INITIAL_TEMPERATURE:@t2``` -> indique **la température @t2** de la _buse @t1_ en début d'impression 
  * ```;EXTRUDER_TRAIN.@t1.MATERIAL.VOLUME_USED:@t2``` -> **la quantité estimée @t2 de matériau** pour l'impression pour la _buse @t1_
  * ```;EXTRUDER_TRAIN.@t1.MATERIAL.GUID:@t2``` -> **le matériau @t2** utilisé pour la _buse @t1_
  * ```;EXTRUDER_TRAIN.@t1.NOZZLE.DIAMETER:@t2``` -> **le diamètre @t2** de la _buse @t1_
  * ```;EXTRUDER_TRAIN.@t1.NOZZLE.NAME:@t2``` -> indique **le type ou/et la taille @t2** de la _buse utilisée @t1_

* ```;BUILD_PLATE.INITIAL_TEMPERATURE:@t``` -> **initialisation de la température** de départ du **plateau**
  
* ```;BUILD_VOLUME.TEMPERATURE:@t``` -> **définir la température ambiante dans l'enceinte de l'imprimante** _(utile dans certaines imprimantes comme Ultimaker ou encore pour certains matériaux)_

* **définir les estimations des dimensions de l'impression**
  ```
  ;PRINT.SIZE.MIN.X:@t
  ;PRINT.SIZE.MIN.Y:@t
  ;PRINT.SIZE.MIN.Z:@t
  ;PRINT.SIZE.MAX.X:@t
  ;PRINT.SIZE.MAX.Y:@t
  ;PRINT.SIZE.MAX.Z:@t
  ```

* ```;PRINT.TIME:@t``` -> **définir le temps à l'imprimante** pour avoir un système de minuteur

* **fin des commentaires**
  ```
  ;Generated with @[nom_generateur] @[version_generateur] 
  ;END_OF_HEADER
  ```
______________
#### Les extras bonus
* ```;PRINT.GROUPS:@t``` -> **gérer plus facilement les groupes d'objets**, une manière de structurer pour les professionnels
* ```;SLICE_UUID:@t``` -> **d'identifier l'origine exacte du fichier G-code** _(pratique pour reprendre le G-Code dans un slicer comme Cura)_


[source de déplacement en ligne droite]: https://www.e-techno-tutos.com/2018/06/10/gcode-g00-g01/ "explication en détails schématique"

[source de déplacement circulaire]: https://www.e-techno-tutos.com/2018/06/10/gcode-g02-g03/ "explication en détails schématique, surtout sur les coordonnées relatives I et J"

[source sur les positions]: https://www.e-techno-tutos.com/2018/06/10/gcode-g90-g91/ "comprendre la différence dans le gcode entre position relative et absolue"

