unit uChargement;

{$mode objfpc}{$H+}

interface

uses
  raylib, uGlobales, SysUtils;

procedure AjouterMessage(const msg: string);
procedure ChargerImages;
procedure LibererRessources;

implementation

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
