unit uAnalyse;

{$mode objfpc}{$H+}

interface

uses
  raylib, uGlobales, uChargement, SysUtils;

procedure AnalyserRegions;
function ObtenirNumeroRegion(x, y: Integer): Integer;

implementation

type
  TPoint = record
    x, y: Integer;
  end;

  TStack = record
    items: array of TPoint;
    count: Integer;
  end;

procedure StackPush(var stack: TStack; point: TPoint);
begin
  if stack.count >= Length(stack.items) then
    SetLength(stack.items, Length(stack.items) + 1000);
  stack.items[stack.count] := point;
  Inc(stack.count);
end;

function StackPop(var stack: TStack): TPoint;
begin
  if stack.count > 0 then
  begin
    Dec(stack.count);
    Result := stack.items[stack.count];
  end
  else
  begin
    Result.x := -1;
    Result.y := -1;
  end;
end;

function StackEmpty(const stack: TStack): Boolean;
begin
  Result := stack.count = 0;
end;

function EstPixelNoir(image: TImage; x, y: Integer): Boolean;
var
  color: TColorB;
begin
  if (x < 0) or (y < 0) or (x >= image.width) or (y >= image.height) then
  begin
    Result := True; // Considérer les bords comme noirs
    Exit;
  end;

  color := GetImageColor(image, x, y);
  // Un pixel est considéré comme noir si toutes ses composantes sont très sombres
  Result := (color.r < 50) and (color.g < 50) and (color.b < 50);
end;

procedure FloodFill(image: TImage; startX, startY, numeroRegion: Integer);
var
  stack: TStack;
  point, nouveauPoint: TPoint;
begin
  stack.count := 0;
  SetLength(stack.items, 1000);

  point.x := startX;
  point.y := startY;
  StackPush(stack, point);

  while not StackEmpty(stack) do
  begin
    point := StackPop(stack);

    // Vérifier si le point est valide et non traité
    if (point.x >= 0) and (point.y >= 0) and
       (point.x < image.width) and (point.y < image.height) and
       (regionMatrix[point.x][point.y] = 0) and
       (not EstPixelNoir(image, point.x, point.y)) then
    begin
      // Marquer ce pixel
      regionMatrix[point.x][point.y] := numeroRegion;

      // Ajouter les voisins (connectivité 4)
      nouveauPoint.x := point.x + 1; nouveauPoint.y := point.y;
      StackPush(stack, nouveauPoint);

      nouveauPoint.x := point.x - 1; nouveauPoint.y := point.y;
      StackPush(stack, nouveauPoint);

      nouveauPoint.x := point.x; nouveauPoint.y := point.y + 1;
      StackPush(stack, nouveauPoint);

      nouveauPoint.x := point.x; nouveauPoint.y := point.y - 1;
      StackPush(stack, nouveauPoint);
    end;
  end;
end;

procedure AnalyserRegions;
var
  image: TImage;
  x, y: Integer;
  numeroRegion: Integer;
begin
  if not carteCouleurLoaded then
  begin
    AjouterMessage('ERREUR: Carte couleur non chargee');
    Exit;
  end;

  AjouterMessage('Debut analyse des regions...');

  // Charger l'image depuis la texture
  image := LoadImageFromTexture(carteCouleur);

  // Initialiser la matrice des régions
  SetLength(regionMatrix, image.width);
  for x := 0 to image.width - 1 do
  begin
    SetLength(regionMatrix[x], image.height);
    for y := 0 to image.height - 1 do
      regionMatrix[x][y] := 0;
  end;

  numeroRegion := 1;

  // Parcourir chaque pixel pour détecter les régions
  for y := 0 to image.height - 1 do
  begin
    for x := 0 to image.width - 1 do
    begin
      // Si pixel non traité et non noir, démarrer une nouvelle région
      if (regionMatrix[x][y] = 0) and (not EstPixelNoir(image, x, y)) then
      begin
        FloodFill(image, x, y, numeroRegion);
        Inc(numeroRegion);
      end;
    end;
  end;

  nombreRegions := numeroRegion - 1;
  analyseTerminee := True;

  AjouterMessage('Analyse terminee: ' + IntToStr(nombreRegions) + ' regions');

  // Libérer l'image temporaire
  UnloadImage(image);
end;

function ObtenirNumeroRegion(x, y: Integer): Integer;
begin
  Result := 0;

  if not analyseTerminee then
  begin
    AjouterMessage('Aucune analyse effectuee');
    Exit;
  end;

  if (x < 0) or (y < 0) or (x >= Length(regionMatrix)) or
     (y >= Length(regionMatrix[0])) then
  begin
    AjouterMessage('Coordonnees hors limites');
    Exit;
  end;

  Result := regionMatrix[x][y];
end;

end.

