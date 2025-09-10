unit uNavigation;

{$mode objfpc}{$H+}

interface

uses
  raylib, uGlobales, uChargement, uInterface, uAnalyse, SysUtils;

procedure GererNavigation;
procedure GererClics;
procedure GererEntrees;
function ConvertirCoordonnees(screenX, screenY: Integer): TVector2D;

implementation

procedure GererNavigation;
begin
  if IsKeyDown(KEY_RIGHT) then
    cameraOffset.x := cameraOffset.x - VITESSE_DEPLACEMENT;
  if IsKeyDown(KEY_LEFT) then
    cameraOffset.x := cameraOffset.x + VITESSE_DEPLACEMENT;
  if IsKeyDown(KEY_DOWN) then
    cameraOffset.y := cameraOffset.y - VITESSE_DEPLACEMENT;
  if IsKeyDown(KEY_UP) then
    cameraOffset.y := cameraOffset.y + VITESSE_DEPLACEMENT;

  // Navigation plus rapide avec Shift
  if IsKeyDown(KEY_LEFT_SHIFT) or IsKeyDown(KEY_RIGHT_SHIFT) then
  begin
    if IsKeyDown(KEY_RIGHT) then
      cameraOffset.x := cameraOffset.x - VITESSE_DEPLACEMENT * 3;
    if IsKeyDown(KEY_LEFT) then
      cameraOffset.x := cameraOffset.x + VITESSE_DEPLACEMENT * 3;
    if IsKeyDown(KEY_DOWN) then
      cameraOffset.y := cameraOffset.y - VITESSE_DEPLACEMENT * 3;
    if IsKeyDown(KEY_UP) then
      cameraOffset.y := cameraOffset.y + VITESSE_DEPLACEMENT * 3;
  end;
end;

procedure LimiterPosition;
var
  carteAUtiliser: TTexture2D;
  largeurCarte, hauteurCarte: Single;
  minX, maxX, minY, maxY: Single;
begin
  // Déterminer quelle carte utiliser pour les calculs
  if (carteVisible = 0) and carteReelleLoaded then
    carteAUtiliser := carteReelle
  else if (carteVisible = 1) and carteCouleurLoaded then
    carteAUtiliser := carteCouleur
  else
    Exit; // Pas de carte chargée

  // Calculer les dimensions avec zoom
  largeurCarte := carteAUtiliser.width * facteurZoom;
  hauteurCarte := carteAUtiliser.height * facteurZoom;

  // Limiter horizontalement
  if largeurCarte <= VIEW_WIDTH then
    cameraOffset.x := (VIEW_WIDTH - largeurCarte) / 2
  else
  begin
    minX := VIEW_WIDTH - largeurCarte;
    maxX := 0;
    if cameraOffset.x < minX then cameraOffset.x := minX;
    if cameraOffset.x > maxX then cameraOffset.x := maxX;
  end;

  // Limiter verticalement
  if hauteurCarte <= VIEW_HEIGHT then
    cameraOffset.y := (VIEW_HEIGHT - hauteurCarte) / 2
  else
  begin
    minY := VIEW_HEIGHT - hauteurCarte;
    maxY := 0;
    if cameraOffset.y < minY then cameraOffset.y := minY;
    if cameraOffset.y > maxY then cameraOffset.y := maxY;
  end;
end;

function ConvertirCoordonnees(screenX, screenY: Integer): TVector2D;
begin
  Result.x := (screenX - cameraOffset.x) / facteurZoom;
  Result.y := (screenY - cameraOffset.y) / facteurZoom;
end;

procedure GererClics;
var
  mousePos: TVector2;
  cartePos: TVector2D;
  message: string;
  carteAUtiliser: TTexture2D;
  numeroRegion: Integer;
begin
  if IsMouseButtonPressed(MOUSE_BUTTON_LEFT) then
  begin
    mousePos := GetMousePosition();

    // Vérifier si le clic est dans la zone de visualisation
    if (mousePos.x >= 0) and (mousePos.x < VIEW_WIDTH) and
       (mousePos.y >= 0) and (mousePos.y < VIEW_HEIGHT) then
    begin
      cartePos := ConvertirCoordonnees(Round(mousePos.x), Round(mousePos.y));

      // Déterminer quelle carte utiliser
      if (carteVisible = 0) and carteReelleLoaded then
        carteAUtiliser := carteReelle
      else if (carteVisible = 1) and carteCouleurLoaded then
        carteAUtiliser := carteCouleur
      else
        Exit;

      // Vérifier si les coordonnées sont dans les limites de la carte
      if (cartePos.x >= 0) and (cartePos.x < carteAUtiliser.width) and
         (cartePos.y >= 0) and (cartePos.y < carteAUtiliser.height) then
      begin
        // Si l'analyse est terminée, obtenir le numéro de région
        if analyseTerminee then
        begin
          numeroRegion := ObtenirNumeroRegion(Round(cartePos.x), Round(cartePos.y));
          if numeroRegion > 0 then
            message := 'Region: ' + IntToStr(numeroRegion) + ' | Carte(' +
                      IntToStr(Round(cartePos.x)) + ',' + IntToStr(Round(cartePos.y)) + ')'
          else
            message := 'Zone noire ou limite | Carte(' +
                      IntToStr(Round(cartePos.x)) + ',' + IntToStr(Round(cartePos.y)) + ')';
        end
        else
        begin
          message := 'Clic: Ecran(' + IntToStr(Round(mousePos.x)) + ',' + IntToStr(Round(mousePos.y)) +
                     ') -> Carte(' + IntToStr(Round(cartePos.x)) + ',' + IntToStr(Round(cartePos.y)) + ')';
        end;

        AjouterMessage(message);
      end
      else
      begin
        AjouterMessage('Clic hors limites de la carte');
      end;
    end;
  end;
end;

procedure GererEntrees;
var
  wheel: Single;
  ancienZoom: Single;
begin
  // 1. NAVIGATION avec flèches
  GererNavigation;

  // 2. ZOOM avec molette de souris
  wheel := GetMouseWheelMove();
  if wheel <> 0 then
  begin
    ancienZoom := facteurZoom;
    facteurZoom := facteurZoom + (wheel * 0.1);
    if facteurZoom < 0.1 then facteurZoom := 0.1;
    if facteurZoom > 5.0 then facteurZoom := 5.0;

    if ancienZoom <> facteurZoom then
      AjouterMessage('Zoom: ' + FormatFloat('0.0', facteurZoom) + 'x');
  end;

  // 3. BASCULEMENT entre cartes (clic droit)
  if IsMouseButtonPressed(MOUSE_BUTTON_RIGHT) then
  begin
    if carteReelleLoaded and carteCouleurLoaded then
    begin
      carteVisible := 1 - carteVisible; // Bascule entre 0 et 1
      if carteVisible = 0 then
        AjouterMessage('Affichage: Carte reelle')
      else
        AjouterMessage('Affichage: Carte couleur');
    end
    else
      AjouterMessage('Impossible: une seule carte chargee');
  end;

  // 4. LIMITATION des déplacements (après navigation et zoom)
  LimiterPosition;

  // 5. GESTION des clics sur boutons
  GererClicsBoutons;

  // 6. GESTION des clics gauches sur carte
  GererClics;

  // 7. TOUCHES spéciales
  if IsKeyPressed(KEY_R) then
  begin
    cameraOffset.x := 0;
    cameraOffset.y := 0;
    facteurZoom := 1.0;
    LimiterPosition; // Recalculer après reset
    AjouterMessage('Position et zoom reinitialises');
  end;

  if IsKeyPressed(KEY_C) then
  begin
    messageCount := 0;
    AjouterMessage('Messages effaces');
  end;
end;

end.
