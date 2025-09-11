unit uInterface;

{$mode objfpc}{$H+}

interface

uses
  raylib, raygui, uGlobales, uAnalyse, uChargement, SysUtils;

procedure DessinerBoutons;
procedure GererClicsBoutons;

implementation

const
  BOUTON_LARGEUR = 250;
  BOUTON_HAUTEUR = 30;
  BOUTON_X = VIEW_WIDTH + 25;

procedure DessinerBoutons;
var
  boutonRect: TRectangle;
  yPos: Integer;
begin
  yPos := 550;

  // Bouton "Analyser Régions"
  boutonRect := RectangleCreate(BOUTON_X, yPos, BOUTON_LARGEUR, BOUTON_HAUTEUR);
  if carteCouleurLoaded and not analyseTerminee then
    GuiButton(boutonRect, 'Analyser Regions')
  else
    GuiButton(boutonRect, 'Analyser Regions (Non disponible)');

  Inc(yPos, 40);

  // Bouton "Sauvegarder Projet"
  boutonRect := RectangleCreate(BOUTON_X, yPos, BOUTON_LARGEUR, BOUTON_HAUTEUR);
  if analyseTerminee then
    GuiButton(boutonRect, 'Sauvegarder Projet')
  else
    GuiButton(boutonRect, 'Sauvegarder (Non disponible)');

  Inc(yPos, 40);

  // Bouton "Charger Projet"
  boutonRect := RectangleCreate(BOUTON_X, yPos, BOUTON_LARGEUR, BOUTON_HAUTEUR);
  GuiButton(boutonRect, 'Charger Projet');

  Inc(yPos, 50);

  // Affichage du statut
  if analyseTerminee then
  begin
    DrawText(PChar('Regions analysees: ' + IntToStr(nombreRegions)),
             BOUTON_X, yPos, 14, GREEN);
    Inc(yPos, 25);
    DrawText('Cliquez sur la carte pour', BOUTON_X, yPos, 12, LIGHTGRAY);
    Inc(yPos, 15);
    DrawText('obtenir le numero de region', BOUTON_X, yPos, 12, LIGHTGRAY);
  end;
end;

procedure GererClicsBoutons;
var
  boutonRect: TRectangle;
  mousePos: TVector2;
  nomProjet: string;
begin
  mousePos := GetMousePosition();

  if IsMouseButtonPressed(MOUSE_BUTTON_LEFT) then
  begin
    // Bouton "Analyser Régions" (Y=550)
    boutonRect := RectangleCreate(BOUTON_X, 550, BOUTON_LARGEUR, BOUTON_HAUTEUR);
    if CheckCollisionPointRec(mousePos, boutonRect) then
    begin
      if carteCouleurLoaded and not analyseTerminee then
      begin
        AnalyserRegions;
      end
      else if not carteCouleurLoaded then
      begin
        AjouterMessage('Carte couleur requise pour analyse');
      end
      else if analyseTerminee then
      begin
        AjouterMessage('Analyse deja effectuee');
      end;
    end;

    // Bouton "Sauvegarder Projet" (Y=590)
    boutonRect := RectangleCreate(BOUTON_X, 590, BOUTON_LARGEUR, BOUTON_HAUTEUR);
    if CheckCollisionPointRec(mousePos, boutonRect) then
    begin
      if analyseTerminee then
      begin
        nomProjet := ObtenirNomCarte();
        if nomProjet = '' then nomProjet := 'carte_sans_nom';
        SauvegarderProjet(nomProjet);
      end
      else
      begin
        AjouterMessage('Aucune analyse a sauvegarder');
      end;
    end;

    // Bouton "Charger Projet" (Y=630)
    boutonRect := RectangleCreate(BOUTON_X, 630, BOUTON_LARGEUR, BOUTON_HAUTEUR);
    if CheckCollisionPointRec(mousePos, boutonRect) then
    begin
      // Pour l'instant, charger le projet par défaut
      nomProjet := 'hongrie2vrai';
      ChargerProjet(nomProjet);
    end;
  end;
end;

end.

