unit uGlobales;

{$mode objfpc}{$H+}

interface

uses
  raylib, SysUtils;

const
  SCREEN_WIDTH = 1920;
  SCREEN_HEIGHT = 1080;
  PANEL_WIDTH = 300;
  MESSAGE_HEIGHT = 300;
  VIEW_WIDTH = SCREEN_WIDTH - PANEL_WIDTH;
  VIEW_HEIGHT = SCREEN_HEIGHT - MESSAGE_HEIGHT;
  MAX_MESSAGES = 4;
  VITESSE_DEPLACEMENT = 5.0;

type
  TVector2D = record
    x, y: Single;
  end;

var
  // Images
  carteReelle: TTexture2D;
  carteCouleur: TTexture2D;
  carteReelleLoaded: Boolean;
  carteCouleurLoaded: Boolean;

  // Navigation et affichage
  cameraOffset: TVector2D;
  facteurZoom: Single;
  carteVisible: Integer; // 0 = carte réelle, 1 = carte couleur

  // Analyse des régions
  regionMatrix: array of array of Integer;
  nombreRegions: Integer;
  analyseTerminee: Boolean;

  // Messages
  messages: array[0..MAX_MESSAGES-1] of string;
  messageCount: Integer;

implementation

initialization
  carteReelleLoaded := False;
  carteCouleurLoaded := False;
  cameraOffset.x := 0;
  cameraOffset.y := 0;
  facteurZoom := 1.0;
  carteVisible := 0;
  nombreRegions := 0;
  analyseTerminee := False;
  messageCount := 0;

end.
