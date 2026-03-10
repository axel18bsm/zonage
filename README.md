Currently we have 2 systems to click  a numerical 2D paper map  

1st System :
On prend une carte de ce style
<img width="1920" height="1080" alt="image" src="https://github.com/user-attachments/assets/abed858e-c45e-47e2-b97d-fe932b1c3fc2" />

On appuie sur le bouton analyse du programme, on obtiendra le resultat suivant :
<img width="1920" height="1080" alt="image" src="https://github.com/user-attachments/assets/64b4865c-d215-4ca2-83b1-36fb75b375da" />

le tour de magie se fait par un programme qui analyse un calque coloré que je crée moi meme et qui est superposé à la carte principale

<img width="1920" height="1080" alt="image" src="https://github.com/user-attachments/assets/34dc5adf-0ef0-4cb4-b6a5-7c4959152bf3" />

L'analyse s effectue sur la carte colorée, les lignes noires sont des frontieres fermées, la couleur des pixels qui se trouvent à l interieur de la frontiere va  determiner un numero de zone
Je vais sauvegarder chaque point de la zone avec ce numero de zone. il suffit de cliquer sur un point de la carte pour connaitre la zone cliquée. Ce qui est le but.

Maintenant le cout memoire. Programme lancé et les 2 maps chargés avant analyse : 54 Mo. Apres analyse, on a 191 Mo.  Je pense que c 'est raisonnable pour une image d'une
taille de 4800x3400 pixels.

la sauvegarde, la restauration du un projet est faite. Le voisinage se fait automatiquement et est sauvegardé automatiquement.
Apres analyse et sauvegarde, il est possible de reprendre  et recharger le projet.

Programme terminé: installation tout mettre dans un répertoire, respecter les chemins. Lancez zonage.exe.

2nd system  the new one :

1)create a qgis project, load a 2d map and create another couch file over the mmap.

<img width="960" height="515" alt="image" src="https://github.com/user-attachments/assets/cbb593a2-dac9-4621-a2ac-5ba802106c49" />

2)use the tools to create new regions ( in violet color) 
3) export the data of the tiled couch in json system.poc_qgz.geoson.

4)Put your map2d and the geoson file in the same directory.
5)Launch zonehongrie.lpr
6) at the first launch, the program is converting the geoson file in csv file with vector to use by raylib and display the map. You can click on the map to find the zone.
7)At the second launch, the program load csv file and display the map, you can click on it.

you can seee an sample with the zone 47 clicked !
<img width="1365" height="940" alt="image" src="https://github.com/user-attachments/assets/33b1316f-7191-4aed-b944-87b811d4e470" />




