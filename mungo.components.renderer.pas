unit mungo.components.renderer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  mungo.components.geometry,
  mungo.components.types,
  mungo.components.colors,

  fgl,
  p3d.math;

type
  TRenderer = class abstract
    procedure RenderPoints( ACanvas: TObject; Points: TVec2List ); virtual; abstract;
    procedure RenderBezier( ACanvas: TObject; Points: TVec2List ); virtual; abstract;
    procedure RenderCircle( ACanvas: TObject; Position: TVec2; Radius: Float ); virtual; abstract;
    procedure RenderEllipse( ACanvas: TObject; TopLeft: TVec2; BottomRight:TVec2 ); virtual; abstract;
    procedure RenderRectangle( ACanvas: TObject; TopLeft: TVec2; BottomRight:TVec2 ); virtual; abstract;
    procedure RenderText( ACanvas: TObject; APosition: TVec2; ACaption: String ); virtual; abstract;
    procedure RenderGeometry( AGeometry: TGeometry ); virtual; abstract;
  end;

  TRendererFill = class abstract ( TRenderer )
  end;

  TRendererStroke = class abstract ( TRenderer )
  end;

  { TRendererText }

  TRendererText = class abstract ( TRenderer )
    private
      FFontName: String;
      FSize: Integer;
      FStyle: TFontStyles;

    public
      constructor Create;

      procedure RenderText( ACanvas: TObject; APosition: TVec2; ACaption: String ); virtual; abstract;

      property FontName: String read FFontName write FFontName;
      property Size: Integer read FSize write FSize;
      property Style: TFontStyles read FStyle write FStyle;
  end;

  { TRendererTextFillColor }

  TRendererTextFillColor = class abstract ( TRendererText )
    private
      FColor: TColorRGBA;

    public
      constructor Create;

      property Color: TColorRGBA read FColor write FColor;
  end;

  { TRendererFillColor }

  TRendererFillColor = class abstract ( TRendererFill )
    private
      FColor: TColorRGBA;

    public
      constructor Create;

      property Color: TColorRGBA read FColor write FColor;
  end;

  { TRendererStrokeColor }

  TRendererStrokeColor = class abstract ( TRendererFill )
    private
      FColor: TColorRGBA;
      FDensity: Float;

    public
      constructor Create;

      property Color: TColorRGBA read FColor write FColor;
      property Density: Float read FDensity write FDensity;
  end;


implementation

{ TRendererTextFillColor }

constructor TRendererTextFillColor.Create;
begin
  inherited Create;

  Color:= Black;
end;

{ TRendererText }

constructor TRendererText.Create;
begin
  FontName:= 'Arial';
  Size:= 10;
  Style:= [];
end;


{ TRendererFillColor }

constructor TRendererFillColor.Create;
begin
  Color:= vec4( 1 );
end;

{ TRendererStrokeColor }

constructor TRendererStrokeColor.Create;
begin
  Density:= 1;
  Color:= vec4( 0, 0, 0, 1 );
end;

end.

