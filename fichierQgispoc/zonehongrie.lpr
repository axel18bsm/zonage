program ZoneHongrie;

{$mode objfpc}{$H+}

uses
  Raylib, SysUtils, Math;

{ ============================================================
  CONSTANTES
  ============================================================ }
const
  IMG_W     = 4885;
  IMG_H     = 3414;
  WIN_W     = 1400;
  WIN_H     = 900;
  MAX_ZONES = 100;
  MAX_PTS   = 15000;
  ZOOM_MIN  = 0.05;
  ZOOM_MAX  = 8.0;
  ZOOM_STEP = 1.12;
  TIF_W     = 5297.77;
  TIF_H     = 3702.33;

{ ============================================================
  VARIABLES GLOBALES
  ============================================================ }
var
  { donnees zones }
  ZoneFid      : array[0..MAX_ZONES-1] of Integer;
  ZonePtCount  : array[0..MAX_ZONES-1] of Integer;
  ZoneStart    : array[0..MAX_ZONES-1] of Integer;
  ZoneCount    : Integer;
  ZonePts      : array[0..MAX_PTS-1]   of TVector2;
  TotalPts     : Integer;
  { affichage }
  MapTex       : TTexture2D;
  HoveredIdx   : Integer;
  SelectedIdx  : Integer;
  HasSelection : Boolean;
  OffX, OffY   : Single;
  Zoom         : Single;
  IsDragging   : Boolean;
  DragStartX   : Single;
  DragStartY   : Single;
  DragOffX     : Single;
  DragOffY     : Single;
  WinW, WinH   : Integer;
  { boucle principale }
  Zi           : Integer;
  Pi2          : Integer;
  Cx2, Cy2     : Single;
  Sx3, Sy3     : Single;
  Lbl          : string;
  MapX, MapY   : Single;
  Mx0, My0     : Single;
  Wheel        : Single;
  { timer }
  T0, T1       : Double;
  { format virgule -> point pour StrToFloat }
  FS           : TFormatSettings;

{ ============================================================
  TIMER
  ============================================================ }
procedure TimerStart;
begin
  T0 := GetTime;
end;

procedure TimerStop(const AMsg : string);
begin
  T1 := GetTime;
  WriteLn(Format('[TIMER] %s : %.4f ms', [AMsg, (T1 - T0) * 1000.0]));
end;

{ ============================================================
  INIT OFFSETS ZONES
  ============================================================ }
procedure InitZoneStarts;
var
  i   : Integer;
  acc : Integer;
begin
  acc := 0;
  for i := 0 to ZoneCount - 1 do
  begin
    ZoneStart[i] := acc;
    Inc(acc, ZonePtCount[i]);
  end;
end;

{ ============================================================
  RESET VUE
  ============================================================ }
procedure ResetView;
var
  ZX : Single;
  ZY : Single;
begin
  ZX := WinW / IMG_W;
  ZY := WinH / IMG_H;
  if ZX < ZY then
    Zoom := ZX
  else
    Zoom := ZY;
  OffX := (WinW - IMG_W * Zoom) / 2;
  OffY := (WinH - IMG_H * Zoom) / 2;
end;

{ ============================================================
  CONVERSIONS ECRAN <-> IMAGE
  ============================================================ }
procedure MapToScreen(mx, my : Single; out sx, sy : Single);
begin
  sx := mx * Zoom + OffX;
  sy := my * Zoom + OffY;
end;

procedure ScreenToMap(sx, sy : Single; out mx, my : Single);
begin
  mx := (sx - OffX) / Zoom;
  my := (sy - OffY) / Zoom;
end;

{ ============================================================
  UTILITAIRES PARSER
  ============================================================ }

{ Avance i en sautant espaces, tabulations, CR, LF, virgules }
procedure SkipWS(const S : string; var i : Integer);
begin
  while (i <= Length(S)) and
        ((S[i] = ' ') or (S[i] = #9) or (S[i] = #10) or
         (S[i] = #13) or (S[i] = ',')) do
    Inc(i);
end;

{ Cherche la sous-chaine P dans S a partir de FromPos (1-base).
  Retourne la position 1-base ou -1. }
function FindStr(const S, P : string; FromPos : Integer) : Integer;
var
  i    : Integer;
  j    : Integer;
  slen : Integer;
  plen : Integer;
  ok   : Boolean;
begin
  Result := -1;
  plen   := Length(P);
  slen   := Length(S);
  i      := FromPos;
  while i <= slen - plen + 1 do
  begin
    ok := True;
    j  := 1;
    while (j <= plen) and ok do
    begin
      if S[i + j - 1] <> P[j] then
        ok := False;
      Inc(j);
    end;
    if ok then
    begin
      Result := i;
      Exit;
    end;
    Inc(i);
  end;
end;

{ Lit un entier (eventuellement negatif) a partir de i (1-base).
  Met a jour i apres le dernier chiffre. }
function ReadInt(const S : string; var i : Integer) : Integer;
var
  neg : Boolean;
  num : string;
begin
  while (i <= Length(S)) and (S[i] = ' ') do
    Inc(i);
  neg := False;
  if (i <= Length(S)) and (S[i] = '-') then
  begin
    neg := True;
    Inc(i);
  end;
  num := '';
  while (i <= Length(S)) and (S[i] >= '0') and (S[i] <= '9') do
  begin
    num := num + S[i];
    Inc(i);
  end;
  if num = '' then
    Result := 0
  else
  begin
    Result := StrToInt(num);
    if neg then
      Result := -Result;
  end;
end;

{ Lit un double (eventuellement negatif) a partir de i (1-base).
  Met a jour i apres le dernier caractere du nombre. }
function ReadDouble(const S : string; var i : Integer) : Double;
var
  neg : Boolean;
  num : string;
  c   : Char;
begin
  while (i <= Length(S)) and (S[i] = ' ') do
    Inc(i);
  neg := False;
  if (i <= Length(S)) and (S[i] = '-') then
  begin
    neg := True;
    Inc(i);
  end;
  num := '';
  while i <= Length(S) do
  begin
    c := S[i];
    if ((c >= '0') and (c <= '9')) or
       (c = '.') or (c = 'e') or (c = 'E') or (c = '+') then
    begin
      num := num + c;
      Inc(i);
    end
    else
      Break;
  end;
  if num = '' then
    Result := 0
  else
  begin
    Result := StrToFloat(num, FS);
    if neg then
      Result := -Result;
  end;
end;

{ ============================================================
  CONVERSION GEOJSON -> TABLEAUX STATIQUES
  Structure : "coordinates": [ [ [ x,y ], [ x,y ], ... ] ] ]
  Apres les 3 brackets on est DANS le premier point : x,y ]
  ============================================================ }
function ConvertGeoJSON(const AFile : string) : Boolean;
var
  f        : File;
  content  : string;
  scaleX   : Double;
  scaleY   : Double;
  pos1     : Integer;
  pos2     : Integer;
  fid      : Integer;
  gx, gy   : Double;
  ptCount  : Integer;
  zIdx     : Integer;
  i        : Integer;
  brackets : Integer;
begin
  Result := False;
  WriteLn('[ConvertGeoJSON] Lecture : ', AFile);
  TimerStart;

  { Lecture binaire complete pour eviter la limite ReadLn (lignes > 16000 chars) }
  AssignFile(f, AFile);
  {$I-}
  Reset(f, 1);
  {$I+}
  if IOResult <> 0 then
  begin
    WriteLn('[ERREUR] Impossible d''ouvrir : ', AFile);
    Exit;
  end;
  SetLength(content, FileSize(f));
  BlockRead(f, content[1], Length(content));
  CloseFile(f);
  WriteLn(Format('[ConvertGeoJSON] %d octets lus', [Length(content)]));
  TimerStop('Lecture GeoJSON binaire');

  scaleX := IMG_W / TIF_W;
  scaleY := IMG_H / TIF_H;
  WriteLn(Format('[ConvertGeoJSON] scaleX=%.8f  scaleY=%.8f', [scaleX, scaleY]));

  ZoneCount := 0;
  TotalPts  := 0;
  zIdx      := 0;
  pos1      := 1;

  TimerStart;

  { Parcourir chaque feature }
  while True do
  begin
    { Chercher "fid": }
    pos1 := FindStr(content, '"fid":', pos1);
    if pos1 < 0 then
      Break;
    Inc(pos1, 6);
    fid  := ReadInt(content, pos1);

    if ZoneCount >= MAX_ZONES then
    begin
      WriteLn('[WARN] MAX_ZONES atteint, fid=', fid, ' ignore');
      Continue;
    end;

    ZoneFid[zIdx]     := fid;
    ZonePtCount[zIdx] := 0;
    ptCount           := 0;

    { Chercher "coordinates": apres ce fid }
    pos2 := FindStr(content, '"coordinates":', pos1);
    if pos2 < 0 then
      Break;
    pos1 := pos2 + 14;

    { Avancer jusqu apres les 3 crochets ouvrants [ [ [ }
    i        := pos1;
    brackets := 0;
    while (i <= Length(content)) and (brackets < 3) do
    begin
      if content[i] = '[' then
        Inc(brackets);
      Inc(i);
    end;
    { i est maintenant juste apres le 3eme '[', soit le debut du premier x }

    { Boucle de lecture des points.
      Structure reelle apres le 3eme '[' :
        x,y ], [ x,y ], [ x,y ] ] ]
      On lit : x (double) , y (double) ] puis cherche '[' ou ']' de fin.  }
    while i <= Length(content) do
    begin
      { Sauter espaces/virgules/CR/LF }
      while (i <= Length(content)) and
            ((content[i] = ' ') or (content[i] = #9) or
             (content[i] = #10) or (content[i] = #13) or
             (content[i] = ',')) do
        Inc(i);

      if i > Length(content) then
        Break;

      if content[i] = ']' then
        Break;   { fin du ring exterior -> arreter }

      if content[i] = '[' then
      begin
        Inc(i); { consommer '[' debut de point }
        Continue;
      end;

      { On est sur le debut d un x (chiffre ou '-') }
      if ((content[i] >= '0') and (content[i] <= '9')) or
         (content[i] = '-') or (content[i] = '.') then
      begin
        gx := ReadDouble(content, i);
        { Sauter la virgule separatrice }
        while (i <= Length(content)) and
              ((content[i] = ' ') or (content[i] = ',')) do
          Inc(i);
        gy := ReadDouble(content, i);
        { Consommer ']' de fin de point }
        while (i <= Length(content)) and
              ((content[i] = ' ') or (content[i] = #9) or
               (content[i] = #10) or (content[i] = #13)) do
          Inc(i);
        if (i <= Length(content)) and (content[i] = ']') then
          Inc(i);
        { Stocker le point converti }
        if TotalPts < MAX_PTS then
        begin
          ZonePts[TotalPts] := Vector2Create(gx * scaleX, -gy * scaleY);
          Inc(TotalPts);
          Inc(ptCount);
        end;
      end
      else
        Inc(i);
    end;

    ZonePtCount[zIdx] := ptCount;
    WriteLn(Format('  fid=%d  points=%d', [fid, ptCount]));
    Inc(ZoneCount);
    Inc(zIdx);
    pos1 := i;
  end;

  TimerStop(Format('Parsing GeoJSON (%d zones, %d pts)', [ZoneCount, TotalPts]));
  InitZoneStarts;
  Result := TotalPts > 0;
end;

{ ============================================================
  SAUVEGARDE CSV
  Format : fid;nb_pts;px0;py0;px1;py1;...
  ============================================================ }
procedure SaveCSV(const AFile : string);
var
  f    : TextFile;
  i, j : Integer;
  line : string;
begin
  WriteLn('[SaveCSV] Ecriture : ', AFile);
  TimerStart;
  AssignFile(f, AFile);
  Rewrite(f);
  WriteLn(f, '# zones.csv - genere par ZoneHongrie');
  WriteLn(f, '# format: fid;nb_pts;px0;py0;px1;py1;...');
  for i := 0 to ZoneCount - 1 do
  begin
    line := IntToStr(ZoneFid[i]) + ';' + IntToStr(ZonePtCount[i]);
    for j := ZoneStart[i] to ZoneStart[i] + ZonePtCount[i] - 1 do
      line := line + ';' + IntToStr(Round(ZonePts[j].x)) + ';' + IntToStr(Round(ZonePts[j].y));
    WriteLn(f, line);
  end;
  CloseFile(f);
  TimerStop(Format('SaveCSV (%d zones)', [ZoneCount]));
end;

{ ============================================================
  CHARGEMENT CSV
  ============================================================ }
function LoadCSV(const AFile : string) : Boolean;
var
  f       : TextFile;
  line    : string;
  parts   : array of string;
  nParts  : Integer;
  i       : Integer;
  j       : Integer;
  zIdx    : Integer;
  ptCount : Integer;
  p       : Integer;
  token   : string;
begin
  Result := False;
  WriteLn('[LoadCSV] Lecture : ', AFile);
  TimerStart;
  AssignFile(f, AFile);
  {$I-}
  Reset(f);
  {$I+}
  if IOResult <> 0 then
  begin
    WriteLn('[LoadCSV] Fichier introuvable : ', AFile);
    Exit;
  end;
  ZoneCount := 0;
  TotalPts  := 0;
  zIdx      := 0;
  while not Eof(f) do
  begin
    ReadLn(f, line);
    if (line = '') or (line[1] = '#') then
      Continue;
    { Split par ';' }
    SetLength(parts, 0);
    nParts := 0;
    token  := '';
    for i := 1 to Length(line) do
    begin
      if line[i] = ';' then
      begin
        SetLength(parts, nParts + 1);
        parts[nParts] := token;
        Inc(nParts);
        token := '';
      end
      else
        token := token + line[i];
    end;
    SetLength(parts, nParts + 1);
    parts[nParts] := token;
    Inc(nParts);
    if nParts < 2 then
      Continue;
    ZoneFid[zIdx]     := StrToInt(parts[0]);
    ptCount           := StrToInt(parts[1]);
    ZonePtCount[zIdx] := ptCount;
    p := 2;
    for j := 0 to ptCount - 1 do
    begin
      if (p + 1 < nParts) and (TotalPts < MAX_PTS) then
      begin
        ZonePts[TotalPts] := Vector2Create(
          StrToInt(parts[p]), StrToInt(parts[p + 1]));
        Inc(TotalPts);
        Inc(p, 2);
      end;
    end;
    Inc(ZoneCount);
    Inc(zIdx);
    if ZoneCount >= MAX_ZONES then
      Break;
  end;
  CloseFile(f);
  TimerStop(Format('LoadCSV (%d zones, %d pts)', [ZoneCount, TotalPts]));
  { CSV invalide si aucun point }
  if TotalPts = 0 then
  begin
    WriteLn('[LoadCSV] CSV invalide (0 pts) -> reconversion forcee');
    ZoneCount := 0;
    Exit;
  end;
  InitZoneStarts;
  Result := ZoneCount > 0;
end;

{ ============================================================
  POINT IN POLYGON  (ray-casting)
  ============================================================ }
{ CheckCollisionPointPoly utilisable directement car ZonePts est en TVector2 }
function FindZoneAt(px, py : Single) : Integer;
var
  i     : Integer;
  mouse : TVector2;
begin
  Result := -1;
  mouse  := Vector2Create(px, py);
  i      := ZoneCount - 1;
  while (i >= 0) and (Result < 0) do
  begin
    if CheckCollisionPointPoly(mouse,
                               @ZonePts[ZoneStart[i]],
                               ZonePtCount[i]) then
      Result := i;
    Dec(i);
  end;
end;

{ PolygonAreaSign supprimee : DrawTriangleFan gere le winding automatiquement }

{ ============================================================
  DESSIN POLYGONE  (tessellation centroide + winding auto)
  ============================================================ }
{ Tampon global pour DrawTriangleFan/DrawLineStrip en coordonnees ecran }
{ pts est global pour eviter stack overflow (MAX_PTS * 8 bytes) }
var
  DrawPts : array[0..MAX_PTS+1] of TVector2;

procedure DrawZonePoly(zoneIdx   : Integer;
                       startIdx  : Integer;
                       fillCol   : TColorB;
                       borderCol : TColorB);
{ ZonePts contient les coordonnees IMAGE (Single).
  On applique MapToScreen pour obtenir les coords ecran dans DrawPts.
  [0] = centroide  [1..n] = sommets  [n+1] = fermeture }
var
  i        : Integer;
  n        : Integer;
  cx, cy   : Single;
  scx, scy : Single;
  sx, sy   : Single;
begin
  n  := ZonePtCount[zoneIdx];
  cx := 0;
  cy := 0;
  for i := startIdx to startIdx + n - 1 do
  begin
    cx := cx + ZonePts[i].x;
    cy := cy + ZonePts[i].y;
  end;
  cx := cx / n;
  cy := cy / n;

  { [0] = centroide }
  MapToScreen(cx, cy, scx, scy);
  DrawPts[0] := Vector2Create(scx, scy);

  { [1..n] = sommets convertis en coords ecran }
  for i := 0 to n - 1 do
  begin
    MapToScreen(ZonePts[startIdx + i].x, ZonePts[startIdx + i].y, sx, sy);
    DrawPts[i + 1] := Vector2Create(sx, sy);
  end;

  { [n+1] = fermeture }
  DrawPts[n + 1] := DrawPts[1];

  { 1 appel remplissage + 1 appel contour }
  DrawTriangleFan(@DrawPts[0], n + 2, fillCol);
  DrawLineStrip(@DrawPts[1], n + 1, borderCol);
end;

{ ============================================================
  COULEURS SELON ETAT
  ============================================================ }
function GetFillColor(idx : Integer) : TColorB;
begin
  if HasSelection and (idx = SelectedIdx) then
    Result := ColorAlpha(YELLOW, 0.45)
  else if idx = HoveredIdx then
    Result := ColorAlpha(WHITE, 0.30)
  else
    Result := ColorAlpha(SKYBLUE, 0.18);
end;

function GetBorderColor(idx : Integer) : TColorB;
begin
  if HasSelection and (idx = SelectedIdx) then
    Result := ORANGE
  else if idx = HoveredIdx then
    Result := WHITE
  else
    Result := ColorAlpha(WHITE, 0.55);
end;

{ ============================================================
  HUD
  ============================================================ }
procedure DrawHUD;
var
  s : string;
begin
  DrawRectangle(0, 0, WinW, 26, ColorAlpha(BLACK, 0.70));
  DrawText(
    'Molette=zoom  Clic D+glisser=pan  R=reset  Clic G=select  Echap=quitter',
    8, 6, 13, RAYWHITE);
  s := Format('Zoom %.0f%%  |  %d zones', [Zoom * 100, ZoneCount]);
  DrawText(PChar(s), WinW - 200, 6, 13, RAYWHITE);
  DrawRectangle(0, WinH - 26, WinW, 26, ColorAlpha(BLACK, 0.70));
  if HoveredIdx >= 0 then
    s := Format('Zone fid=%d   (%d sommets)',
                [ZoneFid[HoveredIdx], ZonePtCount[HoveredIdx]])
  else
    s := 'Aucune zone';
  DrawText(PChar(s), 8, WinH - 18, 13, LIGHTGRAY);
  if HasSelection then
  begin
    s := Format('Selectionnee : fid=%d', [ZoneFid[SelectedIdx]]);
    DrawText(PChar(s), WinW - 280, WinH - 18, 13, YELLOW);
  end;
end;

{ ============================================================
  PROGRAMME PRINCIPAL
  ============================================================ }
begin
  { Forcer le point comme separateur decimal (locale independant) }
  FS                  := DefaultFormatSettings;
  FS.DecimalSeparator := '.';

  WriteLn('=== ZoneHongrie 1526 - Demarrage ===');
  ZoneCount := 0;
  TotalPts  := 0;

  { Chargement : CSV en priorite, sinon conversion GeoJSON }
  if not LoadCSV('zones.csv') then
  begin
    WriteLn('[INIT] Reconversion depuis GeoJSON...');
    if ConvertGeoJSON('poc_qgz.geojson') then
      SaveCSV('zones.csv')
    else
    begin
      WriteLn('[ERREUR] Donnees introuvables. Arret.');
      Halt(1);
    end;
  end;

  WriteLn(Format('[INIT] %d zones, %d points charges', [ZoneCount, TotalPts]));

  { Init Raylib }
  TimerStart;
  SetConfigFlags(FLAG_WINDOW_RESIZABLE);
  InitWindow(WIN_W, WIN_H, 'ZoneHongrie 1526');
  SetTargetFPS(60);
  MapTex := LoadTexture('hongrie4000.png');
  SetTextureFilter(MapTex, TEXTURE_FILTER_BILINEAR);
  TimerStop('InitWindow + LoadTexture');

  HoveredIdx   := -1;
  SelectedIdx  := -1;
  HasSelection := False;
  IsDragging   := False;
  WinW         := GetScreenWidth;
  WinH         := GetScreenHeight;
  ResetView;

  WriteLn('[INIT] Entree boucle principale');

  while not WindowShouldClose do
  begin
    WinW  := GetScreenWidth;
    WinH  := GetScreenHeight;
    Wheel := GetMouseWheelMove;

    { zoom molette centre sur curseur }
    if Wheel <> 0 then
    begin
      ScreenToMap(GetMouseX, GetMouseY, Mx0, My0);
      Zoom := Zoom * Power(ZOOM_STEP, Wheel);
      if Zoom < ZOOM_MIN then
        Zoom := ZOOM_MIN;
      if Zoom > ZOOM_MAX then
        Zoom := ZOOM_MAX;
      OffX := GetMouseX - Mx0 * Zoom;
      OffY := GetMouseY - My0 * Zoom;
    end;

    { pan clic droit }
    if IsMouseButtonPressed(MOUSE_BUTTON_RIGHT) then
    begin
      IsDragging := True;
      DragStartX := GetMouseX;
      DragStartY := GetMouseY;
      DragOffX   := OffX;
      DragOffY   := OffY;
    end;
    if IsMouseButtonReleased(MOUSE_BUTTON_RIGHT) then
      IsDragging := False;
    if IsDragging then
    begin
      OffX := DragOffX + (GetMouseX - DragStartX);
      OffY := DragOffY + (GetMouseY - DragStartY);
    end;

    { reset vue }
    if IsKeyPressed(KEY_R) then
      ResetView;

    { detection zone sous curseur }
    ScreenToMap(GetMouseX, GetMouseY, MapX, MapY);
    HoveredIdx := FindZoneAt(MapX, MapY);

    { selection clic gauche }
    if IsMouseButtonPressed(MOUSE_BUTTON_LEFT) then
    begin
      if HoveredIdx >= 0 then
      begin
        SelectedIdx  := HoveredIdx;
        HasSelection := True;
        WriteLn(Format('[SELECT] fid=%d  pts=%d',
                       [ZoneFid[SelectedIdx], ZonePtCount[SelectedIdx]]));
      end
      else
      begin
        HasSelection := False;
        SelectedIdx  := -1;
      end;
    end;

    BeginDrawing;
      ClearBackground(BLACK);
      DrawTextureEx(MapTex, Vector2Create(OffX, OffY), 0, Zoom, WHITE);

      { zones }
      for Zi := 0 to ZoneCount - 1 do
        DrawZonePoly(Zi, ZoneStart[Zi], GetFillColor(Zi), GetBorderColor(Zi));

      { etiquettes au centroide }
      for Zi := 0 to ZoneCount - 1 do
      begin
        Cx2 := 0;
        Cy2 := 0;
        for Pi2 := ZoneStart[Zi] to ZoneStart[Zi] + ZonePtCount[Zi] - 1 do
        begin
          Cx2 := Cx2 + ZonePts[Pi2].x;
          Cy2 := Cy2 + ZonePts[Pi2].y;
        end;
        Cx2 := Cx2 / ZonePtCount[Zi];
        Cy2 := Cy2 / ZonePtCount[Zi];
        MapToScreen(Cx2, Cy2, Sx3, Sy3);
        Lbl := IntToStr(ZoneFid[Zi]);
        DrawText(PChar(Lbl), Round(Sx3) - 9, Round(Sy3) - 9, 18, BLACK);
        DrawText(PChar(Lbl), Round(Sx3) - 8, Round(Sy3) - 8, 18, RAYWHITE);
      end;

      DrawHUD;
    EndDrawing;
  end;

  WriteLn('[FIN] Fermeture.');
  UnloadTexture(MapTex);
  CloseWindow;
end.
