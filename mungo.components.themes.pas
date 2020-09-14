unit mungo.components.themes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Types, Math, ImgList,
  mungo.components.colors;

type
  TTabState = ( tsDefault, tsActive, tsHover );
  TTabStates = set of TTabState;
  TButtonState = ( bsDefault, bsPressed, bsHover, bsAccent );
  TButtonStates = set of TButtonState;

  TPositionData = record
    Positon: TPoint;
    HAlign: TAlignment;
    VAlign: TTextLayout;
  end;

  TRectData = record
    Rect: TRect;
    HAlign: TAlignment;
    VAlign: TTextLayout;
  end;

  TFontData = record
    Name: String;
    Size: Integer;
    Color: TColor;
    Style: TFontStyles;
  end;

  TBorderSide = ( bsLeft, bsTop, bsRight, bsBottom );

  TBorderStrokeData = record
    Strength: Integer;
    Color: TColor;
    Style: TPenStyle;
  end;

  TFillData = record
    Color: TColor;
    Style: TBrushStyle;
  end;

  TBorderData = array [ TBorderSide ] of TBorderStrokeData;

  TStateData = record
    Font: TFontData;
    Fill: TFillData;
    Border: TBorderData;
  end;
  PStateData = ^TStateData;

  TControlTheme = record
    Margin: array [ TBorderSide ] of Integer;
  end;
  PControlTheme = ^TControlTheme;

  TButtonTheme = record
    Margin: array [ TBorderSide ] of Integer;
    Regular: array [ bsDefault..bsHover ] of TStateData;
    Accent: array [ bsDefault..bsHover ] of TStateData;
  end;
  PButtonTheme = ^TButtonTheme;

  TTabButtonTheme = record
    Margin: array [ TBorderSide ] of Integer;
    States: array [ tsDefault..tsHover ] of TStateData;
  end;
  PTabButtonTheme = ^TTabButtonTheme;

  TGlyphInstanceData = record
    Index: Integer;
    Pos: TPositionData;
    Visible: Boolean;
  end;

  TCaptionInstanceData = record
    Text: String;
    Visible: Boolean;
    Pos: TPositionData;
  end;

  { TControlInstanceData }

  TControlInstanceData = class
    ControlRect: TRect;
    ClientRect: TRect;
    ActiveStyle: PStateData;
    ActiveTheme: PControlTheme;
    procedure CalcMetrics( ATargetRect: TRect );
    function GetActiveState: PStateData; virtual; abstract;
    procedure CalcStateMetrics( ACanvas: TCanvas ); virtual; abstract;
  end;

  { TCustomButtonInstanceData }

  TCustomButtonInstanceData = class ( TControlInstanceData )
    Caption: TCaptionInstanceData;
    Glyph: TGlyphInstanceData;
    Images: TCustomImageList;
    procedure CalcStateMetrics( ACanvas: TCanvas ); override;
  end;

  { TButtonInstanceData }

  TButtonInstanceData = class ( TCustomButtonInstanceData )
    State: TButtonStates;
    function GetActiveState: PStateData; override;
  end;

  { TTabButtonInstanceData }

  TTabButtonInstanceData = class ( TCustomButtonInstanceData )
    State: TTabStates;
    ShowCloseBtn: Boolean;
    function GetActiveState: PStateData; override;
  end;


  { TThemeBase }

  TThemeBase = class ( TPersistent )
    public
//      procedure RenderMenuItem( ACanvas: TCanvas; ARect: TRect; ACaption: TCaption; AStates: TOwnerDrawState ); virtual; abstract;
      procedure RenderBtn( ACanvas: TCanvas; AButton: TCustomButtonInstanceData ); virtual; abstract;

//      procedure RenderTabBtn( ACanvas: TCanvas; AButton: TTabButtonInstanceData ); virtual; abstract;
      procedure RenderTabBg( ACanvas: TCanvas ); virtual; abstract;
      procedure RenderHeaderBg( ACanvas: TCanvas; ACaption: TCaption ); virtual; abstract;
      procedure RenderToolbarBg( ACanvas: TCanvas ); virtual; abstract;
      procedure RenderSpacer( ACanvas: TCanvas ); virtual; abstract;

      function GetBtnSize( ACanvas: TCanvas; AButton: TButtonInstanceData ): TSize; virtual; abstract;
      function GetTabWidth( ACanvas: TCanvas; AButton: TTabButtonInstanceData ): Integer; virtual; abstract;
      function GetThemeForCtrl(ACtrl: TControlClass; const AId: String = '' ): PControlTheme; virtual; abstract;
  end;

  { TThemeDefault }

  TThemeDefault = class ( TThemeBase )
    public const
      DefaultButtonTheme: TButtonTheme = (
        Margin: (
          5, 2, 5, 2
        );
        Regular: (
          ( //default
            Font: ( Name: 'Roboto Condensed'; Size: 10; Color: Black; Style: []);
            Fill: ( Color: Gray100; Style: bsSolid );
            Border: (
              ( Strength: 1; Color: Black; Style: psSolid ), // left
              ( Strength: 1; Color: Black; Style: psSolid ), // top
              ( Strength: 1; Color: Black; Style: psSolid ), // right
              ( Strength: 1; Color: Black; Style: psSolid ) // bottom
            )
          ),
          ( //pressed
            Font: ( Name: 'Roboto Condensed'; Size: 10; Color: White; Style: []);
            Fill: ( Color: Gray900; Style: bsSolid );
            Border: (
              ( Strength: 1; Color: Black; Style: psSolid ), // left
              ( Strength: 1; Color: Black; Style: psSolid ), // top
              ( Strength: 1; Color: Black; Style: psSolid ), // right
              ( Strength: 1; Color: Black; Style: psSolid ) // bottom
            )
          ),
          ( //hover
            Font: ( Name: 'Roboto Condensed'; Size: 10; Color: Black; Style: []);
            Fill: ( Color: Gray200; Style: bsSolid );
            Border: (
              ( Strength: 1; Color: Black; Style: psSolid ), // left
              ( Strength: 1; Color: Black; Style: psSolid ), // top
              ( Strength: 1; Color: Black; Style: psSolid ), // right
              ( Strength: 1; Color: Black; Style: psSolid ) // bottom
            )
          )
        );
        Accent: (
          ( //default
            Font: ( Name: 'Roboto Condensed'; Size: 10; Color: Black; Style: []);
            Fill: ( Color: Gray100; Style: bsSolid );
            Border: (
              ( Strength: 1; Color: Black; Style: psSolid ), // left
              ( Strength: 1; Color: Black; Style: psSolid ), // top
              ( Strength: 1; Color: Black; Style: psSolid ), // right
              ( Strength: 1; Color: Black; Style: psSolid ) // bottom
            )
          ),
          ( //pressed
            Font: ( Name: 'Roboto Condensed'; Size: 10; Color: White; Style: []);
            Fill: ( Color: Red400; Style: bsSolid );
            Border: (
              ( Strength: 1; Color: Black; Style: psSolid ), // left
              ( Strength: 1; Color: Black; Style: psSolid ), // top
              ( Strength: 1; Color: Black; Style: psSolid ), // right
              ( Strength: 1; Color: Black; Style: psSolid ) // bottom
            )
          ),
          ( //hover
            Font: ( Name: 'Roboto Condensed'; Size: 10; Color: White; Style: []);
            Fill: ( Color: Red500; Style: bsSolid );
            Border: (
              ( Strength: 1; Color: Black; Style: psSolid ), // left
              ( Strength: 1; Color: Black; Style: psSolid ), // top
              ( Strength: 1; Color: Black; Style: psSolid ), // right
              ( Strength: 1; Color: Black; Style: psSolid ) // bottom
            )
          )
        )
      );
      DefaultCloseButtonTheme: TButtonTheme = (
        Margin: (
          5, 2, 5, 2
        );
        Regular: (  // passive tab
          ( //default
            Font: ( Name: 'Roboto Condensed'; Size: 10; Color: Black; Style: []);
            Fill: ( Color: Gray200; Style: bsSolid );
            Border: (
              ( Strength: 1; Color: Black; Style: psClear ), // left
              ( Strength: 1; Color: Black; Style: psClear ), // top
              ( Strength: 1; Color: Black; Style: psClear ), // right
              ( Strength: 1; Color: Black; Style: psClear ) // bottom
            )
          ),
          ( //pressed
            Font: ( Name: 'Roboto Condensed'; Size: 10; Color: White; Style: []);
            Fill: ( Color: Red400; Style: bsSolid );
            Border: (
              ( Strength: 1; Color: Black; Style: psSolid ), // left
              ( Strength: 1; Color: Black; Style: psSolid ), // top
              ( Strength: 1; Color: Black; Style: psSolid ), // right
              ( Strength: 1; Color: Black; Style: psSolid ) // bottom
            )
          ),
          ( //hover
            Font: ( Name: 'Roboto Condensed'; Size: 10; Color: Black; Style: []);
            Fill: ( Color: Red500; Style: bsSolid );
            Border: (
              ( Strength: 1; Color: Black; Style: psSolid ), // left
              ( Strength: 1; Color: Black; Style: psSolid ), // top
              ( Strength: 1; Color: Black; Style: psSolid ), // right
              ( Strength: 1; Color: Black; Style: psSolid ) // bottom
            )
          )
        );
        Accent: (   // active tab
          ( //default
            Font: ( Name: 'Roboto Condensed'; Size: 10; Color: Black; Style: []);
            Fill: ( Color: White; Style: bsClear );
            Border: (
              ( Strength: 1; Color: Black; Style: psSolid ), // left
              ( Strength: 1; Color: Black; Style: psSolid ), // top
              ( Strength: 1; Color: Black; Style: psSolid ), // right
              ( Strength: 1; Color: Black; Style: psSolid ) // bottom
            )
          ),
          ( //pressed
            Font: ( Name: 'Roboto Condensed'; Size: 10; Color: White; Style: []);
            Fill: ( Color: Red400; Style: bsSolid );
            Border: (
              ( Strength: 1; Color: Black; Style: psSolid ), // left
              ( Strength: 1; Color: Black; Style: psSolid ), // top
              ( Strength: 1; Color: Black; Style: psSolid ), // right
              ( Strength: 1; Color: Black; Style: psSolid ) // bottom
            )
          ),
          ( //hover
            Font: ( Name: 'Roboto Condensed'; Size: 10; Color: White; Style: []);
            Fill: ( Color: Red500; Style: bsSolid );
            Border: (
              ( Strength: 1; Color: Black; Style: psSolid ), // left
              ( Strength: 1; Color: Black; Style: psSolid ), // top
              ( Strength: 1; Color: Black; Style: psSolid ), // right
              ( Strength: 1; Color: Black; Style: psSolid ) // bottom
            )
          )
        )
      );

      DefaultTabButtonTheme: TTabButtonTheme = (
        Margin: (
          5, 2, 40, 2
        );
        States: (
          ( //default
            Font: ( Name: 'Roboto Condensed'; Size: 10; Color: Black; Style: []);
            Fill: ( Color: Gray200; Style: bsSolid );
            Border: (
              ( Strength: 1; Color: Black; Style: psSolid ), // left
              ( Strength: 1; Color: Black; Style: psSolid ), // top
              ( Strength: 1; Color: Black; Style: psSolid ), // right
              ( Strength: 1; Color: Black; Style: psSolid ) // bottom
            )
          ),
          ( //active
            Font: ( Name: 'Roboto Condensed'; Size: 10; Color: Black; Style: []);
            Fill: ( Color: White; Style: bsSolid );
            Border: (
              ( Strength: 1; Color: Black; Style: psSolid ), // left
              ( Strength: 1; Color: Black; Style: psSolid ), // top
              ( Strength: 1; Color: Black; Style: psSolid ), // right
              ( Strength: 1; Color: Black; Style: psSolid ) // bottom
            )
          ),
          ( //hover
            Font: ( Name: 'Roboto Condensed'; Size: 10; Color: Black; Style: []);
            Fill: ( Color: Gray100; Style: bsSolid );
            Border: (
              ( Strength: 1; Color: Black; Style: psSolid ), // left
              ( Strength: 1; Color: Black; Style: psSolid ), // top
              ( Strength: 1; Color: Black; Style: psSolid ), // right
              ( Strength: 1; Color: Black; Style: psSolid ) // bottom
            )
          )
        )
      );

      DefaultTabsSpecialButtonTheme: TButtonTheme = (
        Margin: (
          5, 2, 5, 2
        );
        Regular: (
          ( //default
            Font: ( Name: 'Roboto Condensed'; Size: 10; Color: Gray600; Style: []);
            Fill: ( Color: Gray300; Style: bsSolid );
            Border: (
              ( Strength: 1; Color: Black; Style: psClear ), // left
              ( Strength: 1; Color: Black; Style: psClear ), // top
              ( Strength: 1; Color: Black; Style: psClear ), // right
              ( Strength: 1; Color: Black; Style: psClear ) // bottom
            )
          ),
          ( //pressed
            Font: ( Name: 'Roboto Condensed'; Size: 10; Color: Black; Style: [ fsBold ]);
            Fill: ( Color: Gray300; Style: bsSolid );
            Border: (
              ( Strength: 1; Color: Black; Style: psSolid ), // left
              ( Strength: 1; Color: Black; Style: psSolid ), // top
              ( Strength: 1; Color: Black; Style: psSolid ), // right
              ( Strength: 1; Color: Black; Style: psSolid ) // bottom
            )
          ),
          ( //hover
            Font: ( Name: 'Roboto Condensed'; Size: 10; Color: Black; Style: [ fsBold ]);
            Fill: ( Color: Gray300; Style: bsSolid );
            Border: (
              ( Strength: 1; Color: Black; Style: psSolid ), // left
              ( Strength: 1; Color: Black; Style: psSolid ), // top
              ( Strength: 1; Color: Black; Style: psSolid ), // right
              ( Strength: 1; Color: Black; Style: psSolid ) // bottom
            )
          )
        );
        Accent: (
          ( //default
            Font: ( Name: 'Roboto Condensed'; Size: 10; Color: Black; Style: []);
            Fill: ( Color: White; Style: bsClear );
            Border: (
              ( Strength: 1; Color: Black; Style: psSolid ), // left
              ( Strength: 1; Color: Black; Style: psSolid ), // top
              ( Strength: 1; Color: Black; Style: psSolid ), // right
              ( Strength: 1; Color: Black; Style: psSolid ) // bottom
            )
          ),
          ( //pressed
            Font: ( Name: 'Roboto Condensed'; Size: 10; Color: White; Style: [fsBold]);
            Fill: ( Color: Red400; Style: bsSolid );
            Border: (
              ( Strength: 1; Color: Black; Style: psSolid ), // left
              ( Strength: 1; Color: Black; Style: psSolid ), // top
              ( Strength: 1; Color: Black; Style: psSolid ), // right
              ( Strength: 1; Color: Black; Style: psSolid ) // bottom
            )
          ),
          ( //hover
            Font: ( Name: 'Roboto Condensed'; Size: 10; Color: White; Style: [fsBold]);
            Fill: ( Color: Red500; Style: bsSolid );
            Border: (
              ( Strength: 1; Color: Black; Style: psSolid ), // left
              ( Strength: 1; Color: Black; Style: psSolid ), // top
              ( Strength: 1; Color: Black; Style: psSolid ), // right
              ( Strength: 1; Color: Black; Style: psSolid ) // bottom
            )
          )
        )
      );

    private
      function InternalGetThemeForCtrl(ACtrl: TControlClass): PControlTheme;

    public
      ButtonTheme: TButtonTheme;
      CloseButtonTheme: TButtonTheme;
      TabSpecialButtonTheme: TButtonTheme;
      TabButtonTheme: TTabButtonTheme;

      constructor Create;

//      procedure RenderMenuItem( ACanvas: TCanvas; ARect: TRect; ACaption: TCaption; AStates: TOwnerDrawState ); override;
      procedure RenderBtn( ACanvas: TCanvas; AButton: TCustomButtonInstanceData ); override;
//      procedure RenderTabBtn( ACanvas: TCanvas; AButton: TTabButtonInstanceData ); override;
      procedure RenderTabBg( ACanvas: TCanvas ); override;
      procedure RenderHeaderBg( ACanvas: TCanvas; ACaption: TCaption ); override;
      procedure RenderToolbarBg( ACanvas: TCanvas ); override;
      procedure RenderSpacer( ACanvas: TCanvas ); override;
      function GetTabWidth(ACanvas: TCanvas; AButton: TTabButtonInstanceData): Integer; override;
      function GetBtnSize( ACanvas: TCanvas; AButton: TButtonInstanceData ): TSize; override;
      function GetThemeForCtrl(ACtrl: TControlClass; const AId: String = '' ): PControlTheme; override;
  end;

implementation

uses
  mungo.components.base;

function CalcAlignment( AWidthHeight: TSize; ADestRect: TRect; AHAlign: TAlignment; AVAlign: TTextLayout ): TPoint;
begin
  case AHAlign of
    taLeftJustify:
      Result.X:= ADestRect.Left;
    taCenter:
      Result.X:= ADestRect.Left + ( ADestRect.Width - AWidthHeight.Width ) div 2;
    taRightJustify:
      Result.X:= ADestRect.Left + ADestRect.Width - AWidthHeight.Width;
  end;
  case AVAlign of
    tlTop:
      Result.Y:= ADestRect.Top;
    tlCenter:
      Result.Y:= ADestRect.Top + ( ADestRect.Height - AWidthHeight.Height ) div 2;
    tlBottom:
      Result.Y:= ADestRect.Top + ADestRect.Height - AWidthHeight.Height;
  end;
end;

procedure TControlInstanceData.CalcMetrics(ATargetRect: TRect );
begin
  ControlRect:= ATargetRect;
  if ( Assigned( ActiveTheme )) then
    with ( ActiveTheme^ ) do
      ClientRect:= Rect( ControlRect.Left + Margin[ bsLeft ], ControlRect.Top + Margin[ bsTop ], ControlRect.Right - Margin[ bsRight ], ControlRect.Bottom - Margin[ bsBottom ]);
end;

{ TCustomButtonInstanceData }

procedure TCustomButtonInstanceData.CalcStateMetrics(ACanvas: TCanvas );
var
  S: TSize;
  P: TPoint;
begin
  if ( Assigned( ActiveTheme )) then
    with ( ACanvas ) do begin
      Font.Size:= ActiveStyle^.Font.Size;
      Font.Color:= ActiveStyle^.Font.Color;
      Font.Name:= ActiveStyle^.Font.Name;
      Font.Style:= ActiveStyle^.Font.Style;

      if ( Caption.Visible ) then
        if ( Glyph.Visible and Assigned( Images )) then begin
          S:= ACanvas.TextExtent( Caption.Text );
          S.Width:= S.Width + 3 + Images.Width;
          S.Height:= Max( S.Height, Images.Height );
          P:= CalcAlignment( S, ClientRect, Caption.Pos.HAlign, Caption.Pos.VAlign );
          case ( Glyph.Pos.HAlign ) of
            taLeftJustify, taCenter: begin
                Glyph.Pos.Positon:= P;
                Caption.Pos.Positon:= Point( P.X + Images.Width + 3, P.Y );
              end;
            taRightJustify: begin
                Caption.Pos.Positon:= P;
                Glyph.Pos.Positon:= Point( P.X + S.Width - Images.Width, P.Y );
              end;
          end;
        end else begin
          S:= ACanvas.TextExtent( Caption.Text );
          P:= CalcAlignment( S, ClientRect, Caption.Pos.HAlign, Caption.Pos.VAlign );
          Caption.Pos.Positon:= P;
        end
      else if ( Glyph.Visible and Assigned( Images )) then begin
        S:= Size( Images.Width, Images.Height );
        P:= CalcAlignment( S, ClientRect, Glyph.Pos.HAlign, Glyph.Pos.VAlign );
        Glyph.Pos.Positon:= Point( P.X, P.Y );
      end else if ( Glyph.Visible ) then
        WriteLn( 'Error' );
    end;
end;

{ TTabButtonInstanceData }

function TTabButtonInstanceData.GetActiveState: PStateData;
begin
  if ( tsActive in State ) then
    Result:= @PTabButtonTheme( ActiveTheme )^.States[ tsActive ]
  else if ( tsHover in State ) then
    Result:= @PTabButtonTheme( ActiveTheme )^.States[ tsHover ]
  else
    Result:= @PTabButtonTheme( ActiveTheme )^.States[ tsDefault ];
end;

{ TButtonInstanceData }

function TButtonInstanceData.GetActiveState: PStateData;
begin
  if ( bsAccent in State ) then
    if ( bsPressed in State ) then
      Result:= @PButtonTheme( ActiveTheme )^.Accent[ bsPressed ]
    else if ( bsHover in State ) then
      Result:= @PButtonTheme( ActiveTheme )^.Accent[ bsHover ]
    else
      Result:= @PButtonTheme( ActiveTheme )^.Accent[ bsDefault ]
  else
    if ( bsPressed in State ) then
      Result:= @PButtonTheme( ActiveTheme )^.Regular[ bsPressed ]
    else if ( bsHover in State ) then
      Result:= @PButtonTheme( ActiveTheme )^.Regular[ bsHover ]
    else
      Result:= @PButtonTheme( ActiveTheme )^.Regular[ bsDefault ];
end;

{ TThemeDefault }

procedure TThemeDefault.RenderBtn(ACanvas: TCanvas; AButton: TCustomButtonInstanceData);
begin
  if ( Assigned( AButton.ActiveStyle )) then
    with ( AButton ) do begin
      ACanvas.Brush.Color:= ActiveStyle^.Fill.Color;
      ACanvas.Brush.Style:= ActiveStyle^.Fill.Style;
      if ( ACanvas.Brush.Style <> bsClear ) then
        ACanvas.FillRect( AButton.ControlRect );

      // TODO: Border

      if ( AButton.Caption.Visible ) then begin
        ACanvas.Font.Size:= ActiveStyle^.Font.Size;
        ACanvas.Font.Color:= ActiveStyle^.Font.Color;
        ACanvas.Font.Name:= ActiveStyle^.Font.Name;
        ACanvas.Font.Style:= ActiveStyle^.Font.Style;
        ACanvas.Brush.Style:= bsClear;

        ACanvas.TextOut( AButton.Caption.Pos.Positon.X, AButton.Caption.Pos.Positon.Y, AButton.Caption.Text );
      end;

      if ( AButton.Glyph.Visible and Assigned( AButton.Images )) then
        AButton.Images.Draw( ACanvas, AButton.Glyph.Pos.Positon.X, AButton.Glyph.Pos.Positon.Y, AButton.Glyph.Index );
    end;
end;

constructor TThemeDefault.Create;
begin
  ButtonTheme:= DefaultButtonTheme;
  TabButtonTheme:= DefaultTabButtonTheme;
  CloseButtonTheme:= DefaultCloseButtonTheme;
  TabSpecialButtonTheme:= DefaultTabsSpecialButtonTheme;
end;

{procedure TThemeDefault.RenderMenuItem(ACanvas: TCanvas; ARect: TRect;
  ACaption: TCaption; AStates: TOwnerDrawState);
var
  Style: TTextStyle;
begin
  with ( ACanvas ) do begin
{    if ( bsPressed in AStates ) then begin
      Brush.Color:= Gray900;
      Font.Color:= White;
    end else }if ( odSelected in AStates ) then begin
      Brush.Color:= Gray700;
      Font.Color:= White;
    end else begin
      Brush.Color:= Gray800;
      Font.Color:= White;
    end;

    FillRect( ARect );
    Style:= TextStyle;
    Style.Alignment:= taCenter;
    Style.Layout:= tlCenter;
    TextRect( ARect, 0, 0, ACaption, Style );
  end;
end;

procedure TThemeDefault.RenderTabBtn(ACanvas: TCanvas; AButton: TCustomButtonInstanceData);
begin
  with ( AButton ) do begin
    ACanvas.Brush.Color:= ActiveStyle^.Fill.Color;
    ACanvas.Brush.Style:= ActiveStyle^.Fill.Style;
    ACanvas.FillRect( AButton.ControlRect );

    // TODO: Border

    if ( AButton.Caption.Visible ) then begin
      Font.Size:= ActiveStyle^.Font.Size;
      Font.Color:= ActiveStyle^.Font.Color;
      Font.Name:= ActiveStyle^.Font.Name;
      Font.Style:= ActiveStyle^.Font.Style;

      TextOut( AButton.Caption.Pos.Positon.X, AButton.Caption.Pos.Positon.Y, AButton.Caption.Text );
    end;

    if ( AButton.Glyph.Visible and Assigned( AButton.Images )) then
      AButton.Images.Draw( ACanvas, AButton.Glyph.Pos.Positon.X, AButton.Glyph.Pos.Positon.Y, AButton.Glyph.Index );
  end;
end;
}
procedure TThemeDefault.RenderHeaderBg(ACanvas: TCanvas; ACaption: TCaption);
var
  ClientRect: TRect;
  Style: TTextStyle;
begin
  ClientRect:= Rect( 0, 0, ACanvas.Width, ACanvas.Height );
  with ( ACanvas ) do begin
    Brush.Color:= Gray800;
    FillRect( ClientRect );
    Font.Color:= White;
    Font.Style:= [ fsBold ];
    Style:= TextStyle;
    Style.Alignment:= taCenter;
    Style.Layout:= tlCenter;
    TextRect( ClientRect, 0, 0, ACaption, Style );
  end;
end;

procedure TThemeDefault.RenderToolbarBg(ACanvas: TCanvas);
var
  ClientRect: TRect;
begin
  ClientRect:= Rect( 0, 0, ACanvas.Width, ACanvas.Height );
  with ( ACanvas ) do begin
    Brush.Color:= Gray100;
    FillRect( ClientRect );
    Pen.Color:= Gray100;
    Pen.Width:= 2;
    //Line( 0, ACanvas.Height - 2, ACanvas.Width, ACanvas.Height - 2 );
  end;
end;

procedure TThemeDefault.RenderSpacer(ACanvas: TCanvas);
var
  ClientRect: TRect;
const
  SpacerHalfWidth = 1;
begin
  ClientRect:= Rect( ACanvas.Width div 2 - SpacerHalfWidth, 0, ACanvas.Width div 2 + SpacerHalfWidth, ACanvas.Height );
  ACanvas.Brush.Color:= Gray400;
  ACanvas.FillRect( ClientRect );
end;

function TThemeDefault.GetTabWidth(ACanvas: TCanvas; AButton: TTabButtonInstanceData): Integer;
begin
  with ( TabButtonTheme.States[ tsDefault ]) do begin
    ACanvas.Font.Size:= Font.Size;
    ACanvas.Font.Color:= Font.Color;
    ACanvas.Font.Name:= Font.Name;
    ACanvas.Font.Style:= Font.Style;
  end;
  Result:= TabButtonTheme.Margin[ bsLeft ] + TabButtonTheme.Margin[ bsRight ];
  if ( AButton.Glyph.Visible and Assigned( AButton.Images )) then
    if ( AButton.Caption.Visible ) then
      Result:= Result + AButton.Images.Width + ACanvas.TextExtent( AButton.Caption.Text ).Width
    else
      Result:= Result + AButton.Images.Width
  else
    Result:= Result + ACanvas.TextExtent( AButton.Caption.Text ).Width;
end;

function TThemeDefault.GetBtnSize(ACanvas: TCanvas; AButton: TButtonInstanceData): TSize;
begin
  with ( ButtonTheme.Regular[ bsDefault ]) do begin
    ACanvas.Font.Size:= Font.Size;
    ACanvas.Font.Color:= Font.Color;
    ACanvas.Font.Name:= Font.Name;
    ACanvas.Font.Style:= Font.Style;
  end;

  Result.Width:= ButtonTheme.Margin[ bsLeft ] + ButtonTheme.Margin[ bsRight ];
  Result.Height:= ButtonTheme.Margin[ bsTop ] + ButtonTheme.Margin[ bsBottom ];
  if ( AButton.Glyph.Visible and Assigned( AButton.Images )) then
    if ( AButton.Caption.Visible ) then
      with ( ACanvas.TextExtent( AButton.Caption.Text )) do
        Result:= Result.Add( Size( Max( AButton.Images.Width, Width ), Max( AButton.Images.Height, Height )))
    else
      Result:= Result.Add( Size( AButton.Images.Width, AButton.Images.Height ))
  else
    Result.Add( ACanvas.TextExtent( AButton.Caption.Text ));
end;

function TThemeDefault.InternalGetThemeForCtrl(ACtrl: TControlClass): PControlTheme;
begin
  if ( ACtrl.InheritsFrom( TButton )) then
    Result:= @ButtonTheme
  else if ( ACtrl.InheritsFrom( TTabButton )) then
    Result:= @TabButtonTheme
  else
    Result:= nil;
end;

function TThemeDefault.GetThemeForCtrl(ACtrl: TControlClass; const AId: String): PControlTheme;
begin
  if ( AId = '' ) then
    Result:= InternalGetThemeForCtrl( ACtrl )
  else if (( ACtrl.InheritsFrom( TButton )) and ( AId = 'closebutton' )) then
    Result:= @CloseButtonTheme
  else if (( ACtrl.InheritsFrom( TButton )) and ( AId = 'tabspecialbutton' )) then
    Result:= @TabSpecialButtonTheme
  else if ( ACtrl.InheritsFrom( TButton )) then
    Result:= @ButtonTheme
  else
    Result:= nil;
end;

procedure TThemeDefault.RenderTabBg(ACanvas: TCanvas);
var
  ClientRect: TRect;
begin
  ClientRect:= Rect( 0, 0, ACanvas.Width, ACanvas.Height );
  ACanvas.Brush.Color:= Gray300;
  ACanvas.FillRect( ClientRect );
end;


end.

