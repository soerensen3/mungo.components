unit mungo.components.widget;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  Math,

  mungo.components.types,
  mungo.components.geometry,
  mungo.components.styles,

  p3d.math;

type

  { TWidget }

  TWidget = class abstract ( TPersistent )
    protected
      FContext: TWidgetContext;
      FCaption: String;
      FId: String;
      FNeedsUpdate: Boolean;
      FRect: TRectF;
      FStyleSheet: TStyleSheet;
      FVisible: Boolean;
      FActiveStyle: TRenderStyle;
      FGeometry: TGeometryRect;
      FGeometryText: TGeometryText;

      procedure SetCaption(AValue: String);
      procedure SetStyleSheet(AValue: TStyleSheet);
      procedure SetVisible(AValue: Boolean);
      procedure SetId(AValue: String); virtual;

      property NeedsUpdate: Boolean read FNeedsUpdate;
      procedure UpdateTheme; virtual; abstract;
      procedure UpdateState; virtual; abstract;

      property Rect: TRectF read FRect;

    public
      constructor Create;
      destructor Destroy; override;

      procedure Render;
      procedure UpdateRect( ARect: TRectF );

    published
      property StyleSheet: TStyleSheet read FStyleSheet write SetStyleSheet;
      property Id: String read FId write SetId;
      property Visible: Boolean read FVisible write SetVisible;
      property Caption: String read FCaption write SetCaption;
      property Context: TWidgetContext read FContext;
  end;

  { TCustomWidget }

  generic TCustomWidget < TWidgetState, TWidgetStates> = class abstract ( TWidget )
    protected
      FWidgetState: TWidgetStates;
      FStyles: array [ TWidgetState ] of TRenderStyle;

      procedure SetWidgetState(AValue: TWidgetStates);
      function GetDim(AIndex: Integer): Float;
      procedure SetDim(AIndex: Integer; AValue: Float);

      function StateToString( AState: TWidgetState ): String; virtual; abstract;
      procedure UpdateTheme; override;
//      procedure UpdateState; override;

    public
      property WidgetState: TWidgetStates read FWidgetState write SetWidgetState;
      property Left: Float index 0 read GetDim write SetDim;
      property Top: Float index 1 read GetDim write SetDim;
      property Width: Float index 2 read GetDim write SetDim;
      property Height: Float index 3 read GetDim write SetDim;
  end;

implementation

{ TWidget }

procedure TWidget.SetCaption(AValue: String);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  Context.Caption:= FCaption;
  UpdateState;
end;

procedure TWidget.SetStyleSheet(AValue: TStyleSheet);
begin
  if FStyleSheet=AValue then Exit;
  FStyleSheet:=AValue;
  FreeAndNil( FContext );
  if ( Assigned( StyleSheet )) then
  FContext:= StyleSheet.CreateWidgetContext( Rect, Caption );
  UpdateTheme;
end;

procedure TWidget.SetVisible(AValue: Boolean);
begin
  if FVisible=AValue then Exit;
  FVisible:=AValue;
end;

procedure TWidget.SetId(AValue: String);
begin
  //if ( FId = AValue ) then
  //  exit;
  FId:= AValue;
  UpdateTheme;
end;

constructor TWidget.Create;
begin
  inherited Create;

  StyleSheet:= DefaultStyleSheet;
end;

destructor TWidget.Destroy;
begin
  FreeAndNil( FContext );
  inherited Destroy;
end;

procedure TWidget.Render;
var
  Geom: TGeometryRect;
  GeomText: TGeometryText;
  TP: TVec2;
begin
  if ( Assigned( FActiveStyle )) then begin
    Geom:= Context.CreateRect( Rect );
    FActiveStyle.Render( Geom );
    Geom.Free;

    GeomText:= Context.CreateText( Caption );
    TP:= Rect.AlignInRect( vec2( GeomText.Width, GeomText.Height ), haCenter, vaCenter );
    GeomText.Left:= TP.X;
    GeomText.Top:= TP.Y;
    FActiveStyle.Render( GeomText );
    GeomText.Free;
  end;
end;

procedure TWidget.UpdateRect(ARect: TRectF);
begin
  FRect:= ARect;
  FContext.UpdateRect( ARect );
end;

{ TCustomWidget }

function TCustomWidget.GetDim(AIndex: Integer): Float;
begin
  case AIndex of
    0: Result:= Rect.Left;
    1: Result:= Rect.Top;
    2: Result:= Rect.Width;
    3: Result:= Rect.Height;
  end;
end;

procedure TCustomWidget.SetDim(AIndex: Integer; AValue: Float);
begin
  case AIndex of
    0: Rect.Left:= AValue;
    1: Rect.Top:= AValue;
    2: Rect.Width:= AValue;
    3: Rect.Height:= AValue;
  end;
end;

procedure TCustomWidget.SetWidgetState(AValue: TWidgetStates);
begin
  if ( FWidgetState = AValue ) then
    exit;

  FWidgetState:= AValue;
  UpdateState;
end;


procedure TCustomWidget.UpdateTheme;
var
  State: TWidgetState;
begin
  if ( not Assigned( StyleSheet )) then
    FillChar( FStyles, SizeOf( FStyles ), 0 )
  else
    for State:= low( TWidgetState ) to high( TWidgetState ) do
      if ( StyleSheet.ContainsKey( Id + ':' + StateToString( State ))) then
        FStyles[ State ]:= StyleSheet[ Id + ':' + StateToString( State )];
  UpdateState;
end;

{procedure TCustomWidget.UpdateState;
var
  State: TWidgetState;
begin
{  State:= High( TWidgetState );
  FActiveStyle:= nil;
  repeat
    if ( State in WidgetState ) then
      FActiveStyle:= FStyles[ State ];
    Dec( State );
  until ( State < low( TWidgetState )) or Assigned( FActiveStyle );}
  State:= high( WidgetState );
  FActiveStyle:= FStyles[ State ];
end;
}

end.

