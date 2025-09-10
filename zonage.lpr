program zonage;

{$mode objfpc}{$H+}

uses
  uGlobales, uChargement, uAffichage, uNavigation, uAnalyse, uInterface,
  SysUtils, raylib, raygui;

begin
  // Initialisation Raylib
  InitWindow(SCREEN_WIDTH, SCREEN_HEIGHT, 'Visualiseur Carte Hongrie - Structure Modulaire');
  SetTargetFPS(60);

  // Messages d'accueil
  AjouterMessage('=== DEMARRAGE DU VISUALISEUR ===');
  AjouterMessage('Touches: Fleches=Navigation, R=Reset, C=Clear');

  // Chargement initial
  ChargerImages;

  // Boucle principale
  while not WindowShouldClose do
  begin
    // Gestion des entrées
    GererEntrees;

    // Sortie avec ESC
    if IsKeyPressed(KEY_ESCAPE) then
      break;

    // Rendu
    RenduComplet;
  end;

  // Nettoyage
  LibererRessources;
  CloseWindow;
end.
