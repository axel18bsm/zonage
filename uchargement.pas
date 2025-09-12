unit uChargement;

{$mode objfpc}{$H+}

interface

uses
  raylib, uGlobales, SysUtils,classes;

procedure AjouterMessage(const msg: string);
procedure ChargerImages;
procedure LibererRessources;
procedure SauvegarderProjet(const nomProjet: string);
procedure ChargerProjet(const nomProjet: string);
function ListerProjets: TStringList;
function ObtenirNomCarte: string;
procedure SauvegarderVoisinage(const nomFichier: string);
procedure ChargerVoisinage(const nomFichier: string);

implementation
procedure ChargerVoisinage(const nomFichier: string);
var
  fichier: TextFile;
  ligne, versionStr, partieVoisins: string;
  i, j, regionId, voisinId, nbRegionsVoisinage: Integer;
  nomFichierVoisinage: string;
  colonPos, virgulePos, startPos: Integer;
  voisinsTrouves: array of Integer;
  nbVoisins: Integer;
begin
  // Créer le nom du fichier voisinage
  nomFichierVoisinage := ChangeFileExt(nomFichier, '.voi');

  if not FileExists(nomFichierVoisinage) then
  begin
    AjouterMessage('Fichier voisinage inexistant: ' + ExtractFileName(nomFichierVoisinage));
    Exit;
  end;

  try
    AssignFile(fichier, nomFichierVoisinage);
    Reset(fichier);

    // Lire l'en-tête
    ReadLn(fichier, versionStr);
    if versionStr <> 'VOISINAGE_V1' then
    begin
      AjouterMessage('Version fichier voisinage non supportée');
      CloseFile(fichier);
      Exit;
    end;

    ReadLn(fichier, nbRegionsVoisinage);

    // Vérifier la cohérence avec les données chargées
    if nbRegionsVoisinage <> nombreRegions then
    begin
      AjouterMessage('Incohérence: ' + IntToStr(nbRegionsVoisinage) + ' regions voisinage vs ' + IntToStr(nombreRegions) + ' regions données');
      CloseFile(fichier);
      Exit;
    end;

    // Initialiser le tableau voisinage
    SetLength(voisinageRegions, nombreRegions + 1);
    for i := 1 to nombreRegions do
      SetLength(voisinageRegions[i], 0);

    SetLength(voisinsTrouves, 100); // Buffer temporaire

    // Lire chaque ligne de voisinage
    while not Eof(fichier) do
    begin
      ReadLn(fichier, ligne);

      // Parser la ligne "regionId:voisin1,voisin2,voisin3"
      colonPos := Pos(':', ligne);
      if colonPos > 0 then
      begin
        regionId := StrToIntDef(Copy(ligne, 1, colonPos - 1), 0);

        if (regionId > 0) and (regionId <= nombreRegions) then
        begin
          // Extraire la liste des voisins après le ':'
          partieVoisins := Copy(ligne, colonPos + 1, Length(ligne) - colonPos);

          if Length(partieVoisins) > 0 then
          begin
            nbVoisins := 0;
            startPos := 1;

            // Parser manuellement les voisins séparés par des virgules
            repeat
              virgulePos := Pos(',', Copy(partieVoisins, startPos, Length(partieVoisins)));

              if virgulePos > 0 then
              begin
                voisinId := StrToIntDef(Copy(partieVoisins, startPos, virgulePos - 1), 0);
                virgulePos := startPos + virgulePos - 1;
              end
              else
              begin
                voisinId := StrToIntDef(Copy(partieVoisins, startPos, Length(partieVoisins)), 0);
              end;

              if voisinId > 0 then
              begin
                if nbVoisins >= Length(voisinsTrouves) then
                  SetLength(voisinsTrouves, Length(voisinsTrouves) + 50);
                voisinsTrouves[nbVoisins] := voisinId;
                Inc(nbVoisins);
              end;

              if virgulePos > 0 then
                startPos := virgulePos + 1
              else
                Break;

            until startPos > Length(partieVoisins);

            // Copier dans le tableau final
            SetLength(voisinageRegions[regionId], nbVoisins);
            for j := 0 to nbVoisins - 1 do
              voisinageRegions[regionId][j] := voisinsTrouves[j];
          end;
        end;
      end;
    end;

    SetLength(voisinsTrouves, 0);
    CloseFile(fichier);
    AjouterMessage('Voisinage charge: ' + IntToStr(nombreRegions) + ' regions');

  except
    on E: Exception do
    begin
      AjouterMessage('ERREUR chargement voisinage: ' + E.Message);
      if TTextRec(fichier).Mode = fmInput then
        CloseFile(fichier);
    end;
  end;
end;

procedure SauvegarderVoisinage(const nomFichier: string);
var
  fichier: TextFile;
  i, j: Integer;
  nomFichierVoisinage: string;
begin
  // Créer le nom du fichier voisinage (.voi)
  nomFichierVoisinage := ChangeFileExt(nomFichier, '.voi');

  try
    AssignFile(fichier, nomFichierVoisinage);
    Rewrite(fichier);

    // Écrire l'en-tête
    WriteLn(fichier, 'VOISINAGE_V1');
    WriteLn(fichier, nombreRegions);

    // Écrire le voisinage de chaque région
    for i := 1 to nombreRegions do
    begin
      Write(fichier, i, ':');
      for j := 0 to Length(voisinageRegions[i]) - 1 do
      begin
        Write(fichier, voisinageRegions[i][j]);
        if j < Length(voisinageRegions[i]) - 1 then
          Write(fichier, ',');
      end;
      WriteLn(fichier);
    end;

    CloseFile(fichier);
    AjouterMessage('Voisinage sauvegarde: ' + ExtractFileName(nomFichierVoisinage));

  except
    on E: Exception do
    begin
      AjouterMessage('ERREUR sauvegarde voisinage: ' + E.Message);
    end;
  end;
end;


procedure CreerRepertoire(const chemin: string);
begin
  if not DirectoryExists(chemin) then
  begin
    if not CreateDir(chemin) then
      AjouterMessage('ERREUR: Impossible de creer ' + chemin);
  end;
end;

function ObtenirNomCarte: string;
begin
  Result := '';
  if carteReelleLoaded then
  begin
    // Extraire le nom sans extension ni chemin
    Result := ExtractFileName(ChangeFileExt('hongrie2vrai', ''));
  end;
end;

procedure CopierFichierImage(const source, destination: string);
var
  sourceStream, destStream: TFileStream;
  buffer: array[0..4095] of Byte;
  bytesRead: Integer;
begin
  try
    if FileExists(source) then
    begin
      sourceStream := TFileStream.Create(source, fmOpenRead);
      try
        destStream := TFileStream.Create(destination, fmCreate);
        try
          repeat
            bytesRead := sourceStream.Read(buffer, SizeOf(buffer));
            if bytesRead > 0 then
              destStream.Write(buffer, bytesRead);
          until bytesRead = 0;
          AjouterMessage('Copie: ' + ExtractFileName(destination));
        finally
          destStream.Free;
        end;
      finally
        sourceStream.Free;
      end;
    end;
  except
    AjouterMessage('ERREUR: Copie de ' + ExtractFileName(source));
  end;
end;

procedure SauvegarderDonneesRegions(const cheminFichier: string);
var
  fichier: TextFile;
begin
  try
    AssignFile(fichier, cheminFichier);
    Rewrite(fichier);

    WriteLn(fichier, '[DONNEES_REGIONS]');
    WriteLn(fichier, 'NombreRegions=' + IntToStr(nombreRegions));
    WriteLn(fichier, 'LargeurImage=' + IntToStr(Length(regionMatrix)));
    if Length(regionMatrix) > 0 then
      WriteLn(fichier, 'HauteurImage=' + IntToStr(Length(regionMatrix[0])));
    WriteLn(fichier, 'DateAnalyse=' + DateTimeToStr(Now));
    WriteLn(fichier, 'VersionFormat=1.0');

    CloseFile(fichier);
    AjouterMessage('Donnees regions sauvees');
  except
    AjouterMessage('ERREUR: Sauvegarde donnees regions');
  end;
end;

procedure SauvegarderMatriceRegions(const cheminFichier: string);
var
  fichier: TFileStream;
  largeur, hauteur, x, y: Integer;
begin
  try
    largeur := Length(regionMatrix);
    hauteur := Length(regionMatrix[0]);

    fichier := TFileStream.Create(cheminFichier, fmCreate);
    try
      // Ecrire dimensions
      fichier.Write(largeur, SizeOf(Integer));
      fichier.Write(hauteur, SizeOf(Integer));

      // Ecrire matrice ligne par ligne
      for y := 0 to hauteur - 1 do
        for x := 0 to largeur - 1 do
          fichier.Write(regionMatrix[x][y], SizeOf(Integer));

      AjouterMessage('Matrice regions sauvee');
    finally
      fichier.Free;
    end;
  except
    AjouterMessage('ERREUR: Sauvegarde matrice regions');
  end;
end;

procedure SauvegarderProjet(const nomProjet: string);
var
  cheminBase, cheminProjet: string;
  nomCarteReelle, nomCarteCouleur: string;
begin
  if not analyseTerminee then
  begin
    AjouterMessage('ERREUR: Aucune analyse a sauvegarder');
    Exit;
  end;

  cheminBase := 'Save';
  cheminProjet := cheminBase + DirectorySeparator + nomProjet;

  // Creer repertoires
  CreerRepertoire(cheminBase);
  CreerRepertoire(cheminProjet);

  // Determiner noms fichiers cartes
  if FileExists('ressources/hongrie2vrai.png') then
    nomCarteReelle := 'hongrie2vrai.png'
  else
    nomCarteReelle := 'hongrie2vrai.jpg';

  if FileExists('ressources/hongrie2color.png') then
    nomCarteCouleur := 'hongrie2color.png'
  else
    nomCarteCouleur := 'hongrie2color.jpg';

  // Copier les cartes
  CopierFichierImage('ressources/' + nomCarteReelle,
                     cheminProjet + DirectorySeparator + nomCarteReelle);
  CopierFichierImage('ressources/' + nomCarteCouleur,
                     cheminProjet + DirectorySeparator + nomCarteCouleur);

  // Sauvegarder donnees analyse
  SauvegarderDonneesRegions(cheminProjet + DirectorySeparator + 'regions.dat');
  SauvegarderMatriceRegions(cheminProjet + DirectorySeparator + 'regions.matrix');

  // AJOUT: Sauvegarder le voisinage si disponible
  if Length(voisinageRegions) > 0 then
    SauvegarderVoisinage(cheminProjet + DirectorySeparator + 'regions.dat');

  AjouterMessage('Projet sauve: ' + nomProjet);
end;

function ChargerDonneesRegions(const cheminFichier: string): Boolean;
var
  fichier: TextFile;
  ligne, cle, valeur: string;
  positionEgal: Integer;
begin
  Result := False;
  if not FileExists(cheminFichier) then Exit;

  try
    AssignFile(fichier, cheminFichier);
    Reset(fichier);

    while not EOF(fichier) do
    begin
      ReadLn(fichier, ligne);
      positionEgal := Pos('=', ligne);
      if positionEgal > 0 then
      begin
        cle := Copy(ligne, 1, positionEgal - 1);
        valeur := Copy(ligne, positionEgal + 1, Length(ligne));

        if cle = 'NombreRegions' then
          nombreRegions := StrToIntDef(valeur, 0);
      end;
    end;

    CloseFile(fichier);
    Result := True;
    AjouterMessage('Donnees regions chargees');
  except
    AjouterMessage('ERREUR: Chargement donnees regions');
  end;
end;

function ChargerMatriceRegions(const cheminFichier: string): Boolean;
var
  fichier: TFileStream;
  largeur, hauteur, x, y: Integer;
begin
  Result := False;
  if not FileExists(cheminFichier) then Exit;

  try
    fichier := TFileStream.Create(cheminFichier, fmOpenRead);
    try
      // Lire dimensions
      fichier.Read(largeur, SizeOf(Integer));
      fichier.Read(hauteur, SizeOf(Integer));

      // Recreer matrice
      SetLength(regionMatrix, largeur);
      for x := 0 to largeur - 1 do
        SetLength(regionMatrix[x], hauteur);

      // Lire matrice
      for y := 0 to hauteur - 1 do
        for x := 0 to largeur - 1 do
          fichier.Read(regionMatrix[x][y], SizeOf(Integer));

      Result := True;
      AjouterMessage('Matrice regions chargee');
    finally
      fichier.Free;
    end;
  except
    AjouterMessage('ERREUR: Chargement matrice regions');
  end;
end;

procedure ChargerProjet(const nomProjet: string);
var
  cheminProjet: string;
  nomCarteReelle, nomCarteCouleur: string;
  cheminCarteReelle, cheminCarteCouleur: string;
begin
  cheminProjet := 'Save' + DirectorySeparator + nomProjet;

  if not DirectoryExists(cheminProjet) then
  begin
    AjouterMessage('ERREUR: Projet inexistant: ' + nomProjet);
    Exit;
  end;

  // Liberer ressources actuelles
  LibererRessources;
  carteReelleLoaded := False;
  carteCouleurLoaded := False;
  analyseTerminee := False;

  // Determiner noms fichiers
  if FileExists(cheminProjet + DirectorySeparator + 'hongrie2vrai.png') then
    nomCarteReelle := 'hongrie2vrai.png'
  else
    nomCarteReelle := 'hongrie2vrai.jpg';

  if FileExists(cheminProjet + DirectorySeparator + 'hongrie2color.png') then
    nomCarteCouleur := 'hongrie2color.png'
  else
    nomCarteCouleur := 'hongrie2color.jpg';

  // Construire chemins complets
  cheminCarteReelle := cheminProjet + DirectorySeparator + nomCarteReelle;
  cheminCarteCouleur := cheminProjet + DirectorySeparator + nomCarteCouleur;

  // Charger cartes avec PChar
  carteReelle := LoadTexture(PChar(cheminCarteReelle));
  carteReelleLoaded := True;
  AjouterMessage('Carte reelle: ' + IntToStr(carteReelle.width) + 'x' + IntToStr(carteReelle.height));

  carteCouleur := LoadTexture(PChar(cheminCarteCouleur));
  carteCouleurLoaded := True;
  AjouterMessage('Carte couleur: ' + IntToStr(carteCouleur.width) + 'x' + IntToStr(carteCouleur.height));

  // Charger donnees analyse
  if ChargerDonneesRegions(cheminProjet + DirectorySeparator + 'regions.dat') and
     ChargerMatriceRegions(cheminProjet + DirectorySeparator + 'regions.matrix') then
  begin
    analyseTerminee := True;

    // AJOUT: Charger le voisinage si disponible
    ChargerVoisinage(cheminProjet + DirectorySeparator + 'regions.dat');

    AjouterMessage('Projet charge: ' + nomProjet + ' (' + IntToStr(nombreRegions) + ' regions)');
  end
  else
  begin
    AjouterMessage('Projet charge mais sans analyse');
  end;
end;

function ListerProjets: TStringList;
var
  searchRec: TSearchRec;
  cheminBase: string;
begin
  Result := TStringList.Create;
  cheminBase := 'Save';

  if DirectoryExists(cheminBase) then
  begin
    if FindFirst(cheminBase + DirectorySeparator + '*', faDirectory, searchRec) = 0 then
    begin
      repeat
        if (searchRec.Attr and faDirectory <> 0) and
           (searchRec.Name <> '.') and (searchRec.Name <> '..') then
          Result.Add(searchRec.Name);
      until FindNext(searchRec) <> 0;
      FindClose(searchRec);
    end;
  end;
end;

procedure AjouterMessage(const msg: string);
var
  i: Integer;
begin
  if messageCount < MAX_MESSAGES then
  begin
    messages[messageCount] := msg;
    Inc(messageCount);
  end
  else
  begin
    // Décaler les messages vers le haut
    for i := 0 to MAX_MESSAGES-2 do
      messages[i] := messages[i+1];
    messages[MAX_MESSAGES-1] := msg;
  end;
end;

procedure ChargerImages;
begin
  AjouterMessage('Chargement des images...');

  // Tenter de charger la carte réelle
  if FileExists('ressources/hongrie2vrai.png') then
  begin
    carteReelle := LoadTexture('ressources/hongrie2vrai.png');
    carteReelleLoaded := True;
    AjouterMessage('Carte reelle: ' + IntToStr(carteReelle.width) + 'x' + IntToStr(carteReelle.height));
  end
  else if FileExists('ressources/hongrie2vrai.jpg') then
  begin
    carteReelle := LoadTexture('ressources/hongrie2vrai.jpg');
    carteReelleLoaded := True;
    AjouterMessage('Carte reelle: ' + IntToStr(carteReelle.width) + 'x' + IntToStr(carteReelle.height));
  end
  else
    AjouterMessage('ERREUR: hongrie2vrai.png/jpg manquant');

  // Tenter de charger la carte couleur
  if FileExists('ressources/hongrie2color.png') then
  begin
    carteCouleur := LoadTexture('ressources/hongrie2color.png');
    carteCouleurLoaded := True;
    AjouterMessage('Carte couleur: ' + IntToStr(carteCouleur.width) + 'x' + IntToStr(carteCouleur.height));
  end
  else if FileExists('ressources/hongrie2color.jpg') then
  begin
    carteCouleur := LoadTexture('ressources/hongrie2color.jpg');
    carteCouleurLoaded := True;
    AjouterMessage('Carte couleur: ' + IntToStr(carteCouleur.width) + 'x' + IntToStr(carteCouleur.height));
  end
  else
    AjouterMessage('ERREUR: cartecouleur.png/jpg manquant');

  if carteReelleLoaded and carteCouleurLoaded then
    AjouterMessage('Toutes les cartes chargees avec succes')
  else
    AjouterMessage('Certaines cartes sont manquantes');
end;

procedure LibererRessources;
begin
  if carteReelleLoaded then
  begin
    UnloadTexture(carteReelle);
    carteReelleLoaded := False;
  end;
  if carteCouleurLoaded then
  begin
    UnloadTexture(carteCouleur);
    carteCouleurLoaded := False;
  end;
end;

end.
