unit mungo.components.widget;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  Math,

  fgl,

  maki.node,

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

  TWidgetAlignBox = ( waFill, waStart, waCenter, waEnd );

  { TWidgetSlot }

  TWidgetSlot = class abstract
    private
      FWidget: TWidget;

      procedure SetWidget(AValue: TWidget);

    public
      destructor Destroy; override;

      procedure UpdateWidgetDimensions( var ASlotSize: TRectF ); virtual; abstract;

    published
      property Widget: TWidget read FWidget write SetWidget;
  end;

  { TWidgetSlotBox }

  TWidgetSlotBox = class ( TWidgetSlot )
    private
      FAlign: TWidgetAlignBox;
      FMarginEnd: Float;
      FMarginStart: Float;

      procedure SetAlign(AValue: TWidgetAlignBox);
      procedure SetMarginEnd(AValue: Float);
      procedure SetMarginStart(AValue: Float);

    public
      procedure UpdateWidgetDimensions( var ASlotSize: TRectF ); override;

      property Align: TWidgetAlignBox read FAlign write SetAlign;
      property MarginStart: Float read FMarginStart write SetMarginStart;
      property MarginEnd: Float read FMarginEnd write SetMarginEnd;
  end;

  { TWidgetSlotList }

  generic TWidgetSlotList < TWidgetSlotType: TWidgetSlot > = class ( specialize TFPGObjectList < TWidgetSlotType >)
    public
      function AddWidget( AWidget: TWidget ): Integer;
      procedure RemoveWidget( AWidget: TWidget );
  end;

  { TWidgetContainer }

  generic TWidgetContainer < TWidgetSlotType: TWidgetSlot > = class abstract
    private type
      TCustomWidgetSlotList = specialize TWidgetSlotList<TWidgetSlotType>;

    private
      FRect: TRectF;
      FSlots: TCustomWidgetSlotList;

    public
      constructor Create;
      destructor Destroy; override;

      procedure UpdateRect( ARect: TRectF );
      procedure Realign; virtual; abstract;

      property Rect: TRectF read FRect;

    published
      property Slots: TCustomWidgetSlotList read FSlots;
  end;

  TWidgetDirection = ( wdVertical, wdHorizontal );

  { TWidgetBox }

  TWidgetBox = class ( specialize TWidgetContainer < TWidgetSlotBox >)
    private
      FDirection: TWidgetDirection;

      procedure SetDirection(AValue: TWidgetDirection);

    public
      procedure Realign; override;

      property Direction: TWidgetDirection read FDirection write SetDirection;
  end;

implementation

{ TWidgetBox }

procedure TWidgetBox.SetDirection(AValue: TWidgetDirection);
begin
  if FDirection=AValue then Exit;
  FDirection:=AValue;
  Realign;
end;

procedure TWidgetBox.Realign;
var
  RemainingSpace, WidgetRect: TRectF;
  i, C: Integer;
begin
  RemainingSpace:= Rect;

  C:= Slots.Count;
  for i:= 0 to C - 1 do begin
    WidgetRect:= RemainingSpace;
    WidgetRect.Height:= WidgetRect.Height / C;
    Slots[ i ].UpdateWidgetDimensions( WidgetRect );
    RemainingSpace:= RectFEx( RemainingSpace.Left, WidgetRect.Bottom, RemainingSpace.Right, RemainingSpace.Bottom );
  end;
end;

{ TWidgetSlotBox }

procedure TWidgetSlotBox.SetAlign(AValue: TWidgetAlignBox);
begin
  if FAlign=AValue then Exit;
  FAlign:=AValue;
end;

procedure TWidgetSlotBox.SetMarginEnd(AValue: Float);
begin
  if FMarginEnd=AValue then Exit;
  FMarginEnd:=AValue;
end;

procedure TWidgetSlotBox.SetMarginStart(AValue: Float);
begin
  if FMarginStart=AValue then Exit;
  FMarginStart:=AValue;
end;

procedure TWidgetSlotBox.UpdateWidgetDimensions(var ASlotSize: TRectF);
begin

end;

{ TWidgetSlotList }

function TWidgetSlotList.AddWidget(AWidget: TWidget): Integer;
var
  Slot: TWidgetSlotType;
begin
  Slot:= TWidgetSlotType.Create;

  Result:= Add( Slot );
end;

procedure TWidgetSlotList.RemoveWidget(AWidget: TWidget);
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    if ( Items[ i ].Widget = AWidget ) then begin
      Delete( i );
    end;
end;

{ TWidgetSlot }

procedure TWidgetSlot.SetWidget(AValue: TWidget);
begin
  if FWidget=AValue then Exit;
  if ( Assigned( FWidget )) then
    FWidget.Free;
  FWidget:=AValue;
end;

destructor TWidgetSlot.Destroy;
begin
  Widget:= nil;
  inherited Destroy;
end;

{ TWidgetContainer }

constructor TWidgetContainer.Create;
begin
  FSlots:= TCustomWidgetSlotList.Create;
//  FSlots.OnChange:= ;
end;

destructor TWidgetContainer.Destroy;
begin
  FreeAndNil( FSlots );
  inherited Destroy;
end;

procedure TWidgetContainer.UpdateRect(ARect: TRectF);
begin
  FRect:= ARect;
  Realign;
end;

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
    GeomText.Top:= TP.Y + GeomText.Height;
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

