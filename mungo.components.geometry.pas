unit mungo.components.geometry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  maki.node,

  mungo.components.types,

  p3d.math;

type
  TWidgetContext = class;

  { TGeometry }

  TGeometry = class abstract ( TNode )
    private
      FContext: TWidgetContext;

    public
      constructor Create( AContext: TWidgetContext ); virtual;
      procedure PassGeometry; virtual; abstract;

      property Context: TWidgetContext read FContext;
  end;

  { TGeometryRect }

  TGeometryRect = class abstract ( TGeometry )
    private
      FHeight: Float;
      FLeft: Float;
      FTop: Float;
      FWidth: Float;

    public
      constructor Create( AContext: TWidgetContext ); virtual;

      property Left: Float read FLeft write FLeft;
      property Top: Float read FTop write FTop;
      property Width: Float read FWidth write FWidth;
      property Height: Float read FHeight write FHeight;
  end;

  { TGeometryEllipse }

  TGeometryEllipse = class abstract ( TGeometry )
    private
      FHeight: Float;
      FLeft: Float;
      FTop: Float;
      FWidth: Float;

    public
      constructor Create( AContext: TWidgetContext ); virtual;

      property Left: Float read FLeft write FLeft;
      property Top: Float read FTop write FTop;
      property Width: Float read FWidth write FWidth;
      property Height: Float read FHeight write FHeight;
  end;

  { TGeometryCircle }

  TGeometryCircle = class abstract ( TGeometry )
    private
      FCenterX: Float;
      FCenterY: Float;
      FRadius: Float;

    public
      constructor Create( AContext: TWidgetContext ); virtual;

      property CenterX: Float read FCenterX write FCenterX;
      property CenterY: Float read FCenterY write FCenterY;
      property Radius: Float read FRadius write FRadius;
  end;

  { TGeometryPolygon }

  TGeometryPath = class abstract ( TGeometry )
    private
      FCenterX: Float;
      FCenterY: Float;

    public
      procedure AddPoint( APoint: TVec2 ); virtual; abstract;

      property CenterX: Float read FCenterX write FCenterX;
      property CenterY: Float read FCenterY write FCenterY;
  end;

  { TGeometryText }

  TGeometryText = class abstract ( TGeometry )
    protected
      FCaption: String;
      FFontName: String;
      FFontSize: Float;
      FFontStyle: TFontStyles;
      FHeight: Float;
      FLeft: Float;
      FTop: Float;
      FWidth: Float;

      procedure SetCaption(AValue: String); virtual;
      procedure SetFontName(AValue: String); virtual;
      procedure SetFontSize(AValue: Float); virtual;
      procedure SetFontStyle(AValue: TFontStyles); virtual;
      procedure UpdateGeometry; virtual;

    public
      constructor Create( AContext: TWidgetContext ); override;

      property Caption: String read FCaption write SetCaption;

      property Left: Float read FLeft write FLeft;
      property Top: Float read FTop write FTop;
      property Width: Float read FWidth;
      property Height: Float read FHeight;

      property FontName: String read FFontName write SetFontName;
      property FontSize: Float read FFontSize write SetFontSize;
      property FontStyle: TFontStyles read FFontStyle write SetFontStyle;
  end;

  { TWidgetContext }

  TWidgetContext = class abstract ( TInterfacedPersistent )
    private
      FCaption: String;
      FRect: TRectF;

      procedure SetCaption(AValue: String);

    public
      procedure UpdateRect( ARect: TRectF ); virtual;

      function CreateRect( ARect: TRectF ): TGeometryRect; virtual; abstract;
      function CreateEllipse( ARect: TRectF ): TGeometryEllipse; virtual; abstract;
      function CreateCircle( ACenter: TVec2; ARadius: Float ): TGeometryCircle; virtual; abstract;
      function CreateText( ACaption: String ): TGeometryText; virtual; abstract;

      property Rect: TRectF read FRect;
      property Caption: String read FCaption write SetCaption;
  end;



implementation

{ TWidgetContext }

procedure TWidgetContext.SetCaption(AValue: String);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
end;

procedure TWidgetContext.UpdateRect(ARect: TRectF);
begin
  FRect:= ARect;
end;

{ TGeometry }

constructor TGeometry.Create(AContext: TWidgetContext);
begin
  FContext:= AContext;
end;

{ TGeometryRect }

constructor TGeometryRect.Create(AContext: TWidgetContext);
begin
  inherited Create( AContext );
  Width:= 200;
  Height:= 50;
end;

{ TGeometryText }

procedure TGeometryText.SetCaption(AValue: String);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  UpdateGeometry;
end;

procedure TGeometryText.SetFontName(AValue: String);
begin
  if FFontName=AValue then Exit;
  FFontName:=AValue;
  UpdateGeometry;
end;

procedure TGeometryText.SetFontSize(AValue: Float);
begin
  if FFontSize=AValue then Exit;
  FFontSize:=AValue;
  UpdateGeometry;
end;

procedure TGeometryText.SetFontStyle(AValue: TFontStyles);
begin
  if FFontStyle=AValue then Exit;
  FFontStyle:=AValue;
  UpdateGeometry;
end;

procedure TGeometryText.UpdateGeometry;
begin

end;

constructor TGeometryText.Create(AContext: TWidgetContext);
begin
  inherited Create( AContext );
  FontName:= 'Arial';
  FontSize:= 12;
  FontStyle:= [];
end;

{ TGeometryCircle }

constructor TGeometryCircle.Create(AContext: TWidgetContext);
begin
  inherited Create( AContext );
  FRadius:= 50;
end;

{ TGeometryEllipse }

constructor TGeometryEllipse.Create(AContext: TWidgetContext);
begin
  inherited Create( AContext );
  Width:= 50;
  Height:= 50;
end;


end.

