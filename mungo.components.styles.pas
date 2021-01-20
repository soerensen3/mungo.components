unit mungo.components.styles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fgl, Generics.Collections,

  p3d.math,

  mungo.components.types,
  mungo.components.renderer,
  mungo.components.geometry;

type
  { TRenderStyle }

  //TRenderStyleCache = class;

  TStyleID = String;

  TRenderStyle = class ( specialize TFPGObjectList < TRenderer >)
    procedure Render( AGeometry: TGeometry );
    procedure Render( ACanvas: TObject; APosition: TVec2; ACaption: String );
    procedure CalculateParams( ACanvas: TObject; AGeometry: TGeometry );
    //procedure Render( ACanvas: TObject; ACache: TRenderStyleCache );
  end;

  TStyleSheet = class abstract ( specialize TDictionary < TStyleID, TRenderStyle >)
    public
      constructor Create; override;
      destructor Destroy; override;

      function CreateWidgetContext( ARect: TRectF; ACaption: String ): TWidgetContext; virtual; abstract;
  end;

var
  DefaultStyleSheet: TStyleSheet = nil;

implementation

{ TRenderStyle }

procedure TRenderStyle.Render(AGeometry: TGeometry);
var
  Renderer: TRenderer;
begin
  for Renderer in Self do
    Renderer.RenderGeometry( AGeometry );
end;

procedure TRenderStyle.Render(ACanvas: TObject; APosition: TVec2;
  ACaption: String);
var
  Renderer: TRenderer;
begin
  for Renderer in Self do
    if ( Renderer is TRendererText ) then
      TRendererText( Renderer ).RenderText( ACanvas, APosition, ACaption );
end;

procedure TRenderStyle.CalculateParams(ACanvas: TObject; AGeometry: TGeometry);
begin

end;

{procedure TRenderStyle.Render(ACanvas: TObject; ACache: TRenderStyleCache);
begin

end;}

constructor TStyleSheet.Create;
begin
  inherited Create;

  if ( DefaultStyleSheet = nil ) then
    DefaultStyleSheet:= Self;
end;

destructor TStyleSheet.Destroy;
begin
  if ( Self = DefaultStyleSheet ) then
    DefaultStyleSheet:= nil;
  inherited Destroy;
end;

end.

