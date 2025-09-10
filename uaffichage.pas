unit uAffichage;

{$mode objfpc}{$H+}

interface

uses
  raylib, uGlobales, uInterface, SysUtils;

procedure DessinerInterface;
procedure DessinerCartes;
procedure DessinerTextes;
procedure RenduComplet;

implementation

procedure DessinerInterface;
begin
  // Zone de visualisation (fond gris foncé)
  DrawRectangle(0, 0, VIEW_WIDTH, VIEW_HEIGHT, DARKGRAY);

  // Panneau de commandes (fond noir)
  DrawRectangle(VIEW_WIDTH, 0, PANEL_WIDTH, SCREEN_HEIGHT, BLACK);

  // Zone de messages (fond noir)
  DrawRectangle(0, VIEW_HEIGHT, VIEW_WIDTH, MESSAGE_HEIGHT, BLACK);

  // Bordures
  DrawLine(VIEW_WIDTH, 0, VIEW_WIDTH, SCREEN_HEIGHT, WHITE);
  DrawLine(0, VIEW_HEIGHT, SCREEN_WIDTH, VIEW_HEIGHT, WHITE);
end;

procedure DessinerCartes;
var
  carteADessiner: TTexture2D;
begin
  // Définir la zone de dessin avec clipping
  BeginScissorMode(0, 0, VIEW_WIDTH, VIEW_HEIGHT);

  // Déterminer quelle carte afficher
  if (carteVisible = 0) and carteReelleLoaded then
    carteADessiner := carteReelle
  else if (carteVisible = 1) and carteCouleurLoaded then
    carteADessiner := carteCouleur
  else
  begin
    DrawText('Aucune carte disponible', 50, 50, 24, RED);
    EndScissorMode;
    Exit;
  end;

  // Dessiner la carte avec zoom
  DrawTextureEx(carteADessiner,
                Vector2Create(cameraOffset.x, cameraOffset.y),
                0.0,
                facteurZoom,
                WHITE);

  EndScissorMode;
end;

procedure DessinerTextes;
var
  i: Integer;
  y: Integer;
begin
  // Titre dans le panneau
  DrawText('CONTROLES', VIEW_WIDTH + 10, 20, 20, WHITE);
  DrawText('Fleches: Navigation', VIEW_WIDTH + 10, 60, 16, LIGHTGRAY);
  DrawText('Molette: Zoom', VIEW_WIDTH + 10, 80, 16, LIGHTGRAY);
  DrawText('Clic droit: Changer carte', VIEW_WIDTH + 10, 100, 16, LIGHTGRAY);
  DrawText('ESC: Quitter', VIEW_WIDTH + 10, 120, 16, LIGHTGRAY);

  // État des cartes
  DrawText('ETAT CARTES:', VIEW_WIDTH + 10, 160, 18, WHITE);
  if carteReelleLoaded then
    DrawText('Carte reelle: OK', VIEW_WIDTH + 10, 190, 14, GREEN)
  else
    DrawText('Carte reelle: MANQUANTE', VIEW_WIDTH + 10, 190, 14, RED);

  if carteCouleurLoaded then
    DrawText('Carte couleur: OK', VIEW_WIDTH + 10, 210, 14, GREEN)
  else
    DrawText('Carte couleur: MANQUANTE', VIEW_WIDTH + 10, 210, 14, RED);

  // Position de la caméra
  DrawText('POSITION CAMERA:', VIEW_WIDTH + 10, 250, 18, WHITE);
  DrawText(PChar('X: ' + FormatFloat('0', cameraOffset.x)), VIEW_WIDTH + 10, 280, 14, LIGHTGRAY);
  DrawText(PChar('Y: ' + FormatFloat('0', cameraOffset.y)), VIEW_WIDTH + 10, 300, 14, LIGHTGRAY);

  // Informations supplémentaires
  if carteReelleLoaded then
  begin
    DrawText('TAILLE CARTE:', VIEW_WIDTH + 10, 340, 18, WHITE);
    DrawText(PChar('L: ' + IntToStr(carteReelle.width)), VIEW_WIDTH + 10, 370, 14, LIGHTGRAY);
    DrawText(PChar('H: ' + IntToStr(carteReelle.height)), VIEW_WIDTH + 10, 390, 14, LIGHTGRAY);
  end;

  // Affichage zoom et carte active
  DrawText('ZOOM ET CARTE:', VIEW_WIDTH + 10, 430, 18, WHITE);
  DrawText(PChar('Zoom: ' + FormatFloat('0.0', facteurZoom) + 'x'), VIEW_WIDTH + 10, 460, 14, LIGHTGRAY);
  if carteVisible = 0 then
    DrawText('Active: Carte reelle', VIEW_WIDTH + 10, 480, 14, YELLOW)
  else
    DrawText('Active: Carte couleur', VIEW_WIDTH + 10, 480, 14, YELLOW);

  // Affichage du statut d'analyse
  if analyseTerminee then
  begin
    DrawText('ANALYSE:', VIEW_WIDTH + 10, 510, 18, WHITE);
    DrawText(PChar('Regions: ' + IntToStr(nombreRegions)), VIEW_WIDTH + 10, 530, 14, GREEN);
  end;

  // Messages en bas
  DrawText('MESSAGES:', 10, VIEW_HEIGHT + 10, 18, WHITE);
  y := VIEW_HEIGHT + 40;
  for i := 0 to messageCount - 1 do
  begin
    DrawText(PChar(messages[i]), 10, y, 14, LIGHTGRAY);
    Inc(y, 20);
  end;
end;

procedure RenduComplet;
begin
  BeginDrawing;
  ClearBackground(BLACK);

  DessinerInterface;
  DessinerCartes;
  DessinerTextes;

  // Dessiner les boutons de l'interface
  DessinerBoutons;

  EndDrawing;
end;

end.

