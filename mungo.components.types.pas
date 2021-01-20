unit mungo.components.types;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPCanvas, FPImage,

  mungo.components.colors,

  Math,
  p3d.math;

type

  THorizontalAlign = ( haLeft, haCenter, haRight );
  TVerticalAlign  = ( vaTop, vaCenter, vaBottom );
  TFontStyle = (fsBold, fsItalic, fsUnderline, fsStrikeOut);
  TFontStyles = set of TFontStyle;
  TBorderSide = ( bsLeft, bsTop, bsRight, bsBottom );
  TPenStyle = TFPPenStyle;
  TBrushStyle = TFPBrushStyle;

  { TRectF }

  TRectF = packed object
    private
      FLeft: Float;
      FTop: Float;
      FWidth: Float;
      FHeight: Float;

      function GetBottom: Float;
      function GetBottomI: Integer;
      function GetBottomRight: TVec2;
      function GetHeightI: Integer;
      function GetLeftI: Integer;
      function GetRight: Float;
      function GetRightI: Integer;
      function GetTopI: Integer;
      function GetTopLeft: TVec2;
      function GetWidthHeight: TVec2;
      function GetWidthI: Integer;
      procedure SetBottom(AValue: Float);
      procedure SetBottomRight(AValue: TVec2);
      procedure SetRight(AValue: Float);
      procedure SetTopLeft(AValue: TVec2);
      procedure SetWidthHeight(AValue: TVec2);

    public
      function PtInRect( P: TVec2 ): Boolean;
      function RectCollision( R: TRectF ): Boolean;
      function Clip( R: TRectF ): TRectF;
      function Inset( ATopLeft, ABottomRight: TVec2 ): TRectF;
      function ToString: String;
      function Transform( Matrix: TMat4 ): TRectF;
      function AlignInRect( AWidthHeight: TVec2; AHAlign: THorizontalAlign; AVAlign: TVerticalAlign ): TVec2;

      property TopLeft: TVec2 read GetTopLeft write SetTopLeft;
      property BottomRight: TVec2 read GetBottomRight write SetBottomRight;
      property WidthHeight: TVec2 read GetWidthHeight write SetWidthHeight;
      property Left: Float read FLeft write FLeft;
      property Top: Float read FTop write FTop;
      property Width: Float read FWidth write FWidth;
      property Height: Float read FHeight write FHeight;
      property Bottom: Float read GetBottom write SetBottom;
      property Right: Float read GetRight write SetRight;
      property LeftI: Integer read GetLeftI;
      property TopI: Integer read GetTopI;
      property WidthI: Integer read GetWidthI;
      property HeightI: Integer read GetHeightI;
      property RightI: Integer read GetRightI;
      property BottomI: Integer read GetBottomI;
    end;

  TColor = Cardinal; // TFPColor

  TPositioningData = record
    Positon: TVec2;
    HAlign: THorizontalAlign;
    VAlign: TVerticalAlign;
  end;

  TRectPositioningData = record
    Rect: TRectF;
    HAlign: THorizontalAlign;
    VAlign: TVerticalAlign;
  end;

  TFontData = record
    Name: String;
    Size: Integer;
    Color: TColorRGBA;
    Style: TFontStyles;
  end;

  TCaptionInstanceData = record
    Text: String;
    Visible: Boolean;
    Pos: TPositioningData;
  end;


  function RectF( Left, Top, Width, Height: Float ): TRectF;
  function RectF(TopLeft, WidthHeight: TVec2): TRectF;
  function RectFEx(Left, Top, Right, Bottom: Float): TRectF;
  function RectFEx( TopLeft, BottomRight: TVec2 ): TRectF;

  operator + ( R: TRectF; v: TVec2 ): TRectF;
  operator - ( R: TRectF; v: TVec2 ): TRectF;
  operator * ( m: TMat4; R: TRectF ): TRectF;
  operator * ( R: TRectF; S: Float ): TRectF;
  operator / ( R: TRectF; S: Float ): TRectF;
  operator = ( R1, R2: TRectF ): Boolean;


implementation

{ TRectF }

function TRectF.GetBottomRight: TVec2;
begin
  Result:= TopLeft + vec2( Width, Height );
end;

function TRectF.GetHeightI: Integer;
begin
  Result:= round( Height );
end;

function TRectF.GetLeftI: Integer;
begin
  Result:= round( Left );
end;

function TRectF.GetBottom: Float;
begin
  Result:= TopLeft.y + Height;
end;

function TRectF.GetBottomI: Integer;
begin
  Result:= round( Bottom );
end;

function TRectF.GetRight: Float;
begin
  Result:= TopLeft.x + Width;
end;

function TRectF.GetRightI: Integer;
begin
  Result:= round( Right );
end;

function TRectF.GetTopI: Integer;
begin
  Result:= round( Top );
end;

function TRectF.GetTopLeft: TVec2;
begin
  Result:= vec2( Left, Top );
end;

function TRectF.GetWidthHeight: TVec2;
begin
  Result:= vec2( Width, Height );
end;

function TRectF.GetWidthI: Integer;
begin
  Result:= round( Width );
end;

procedure TRectF.SetBottom(AValue: Float);
begin
  Height:= Max( 0, AValue - Top );
end;


procedure TRectF.SetBottomRight(AValue: TVec2);
begin
  Width:= Max( 0, AValue.x - Left );
  Height:= Max( 0, AValue.y - Top );
end;


procedure TRectF.SetRight(AValue: Float);
begin
  Width:= Max( 0, AValue - Left );
end;


procedure TRectF.SetTopLeft(AValue: TVec2);
begin
  Left:= AValue.X;
  Top:= AValue.Y;
end;

procedure TRectF.SetWidthHeight(AValue: TVec2);
begin
  Width:= AValue.X;
  Height:= AValue.Y;
end;


function TRectF.PtInRect(P: TVec2): Boolean;
begin
  Result:= ( P.x >= Left ) and ( P.x < Right )
       and ( P.Y >= Top )  and ( P.y < Bottom );
end;

function TRectF.RectCollision(R: TRectF): Boolean;
begin
  Result:= ( Left <= R.Right ) and
           ( Right >= R.Left ) and
           ( Top <= R.Bottom ) and
           ( Bottom >= R.Top );
end;

function TRectF.Clip(R: TRectF): TRectF;
begin
  Result.TopLeft:= Max( R.TopLeft, TopLeft );
  Result.BottomRight:= Max( Result.TopLeft, Min( R.BottomRight, BottomRight ));
end;

function TRectF.Inset(ATopLeft, ABottomRight: TVec2): TRectF;
begin
  Result.TopLeft:= TopLeft + ATopLeft;
  Result.BottomRight:= BottomRight - ABottomRight;
end;

function TRectF.ToString: String;
begin
  Result:= TopLeft.ToString() + ', ' + BottomRight.ToString();
end;

function TRectF.Transform(Matrix: TMat4): TRectF;
var
  _TopLeft, _TopRight, _BottomLeft, _BottomRight: TVec2;
begin
  _TopLeft:= ( Matrix * vec4( TopLeft, 0, 1 )).xy;
  _TopRight:= ( Matrix * vec4( Right, Top, 0, 1 )).xy;
  _BottomLeft:= ( Matrix * vec4( Left, Bottom, 0, 1 )).xy;
  _BottomRight:= ( Matrix * vec4( BottomRight, 0, 1 )).xy;
  Result.TopLeft:= min( _TopLeft, min( _TopRight, min( _BottomLeft, _BottomRight )));
  Result.BottomRight:= max( _TopLeft, max( _TopRight, max( _BottomLeft, _BottomRight )));
end;

function TRectF.AlignInRect(AWidthHeight: TVec2; AHAlign: THorizontalAlign;
  AVAlign: TVerticalAlign): TVec2;
begin
  case AHAlign of
    haLeft:
      Result.X:= Left;
    haCenter:
      Result.X:= Left + ( Width - AWidthHeight.X ) / 2;
    haRight:
      Result.X:= Left + Width - AWidthHeight.X;
  end;
  case AVAlign of
    vaTop:
      Result.Y:= Top;
    vaCenter:
      Result.Y:= Top + ( Height - AWidthHeight.Y ) / 2;
    vaBottom:
      Result.Y:= Top + Height - AWidthHeight.Y;
  end;
end;

operator+(R: TRectF; v: TVec2): TRectF;
begin
  Result:= RectF( R.TopLeft + v, R.WidthHeight );
end;

operator-(R: TRectF; v: TVec2): TRectF;
begin
  Result:= RectF( R.TopLeft - v, R.WidthHeight );
end;

operator*(m: TMat4; R: TRectF): TRectF;
begin
  Result:= R.Transform( m );
end;

operator*(R: TRectF; S: Float): TRectF;
begin
  Result.TopLeft:= R.TopLeft * S;
  Result.BottomRight:= R.BottomRight * S;
end;

operator/(R: TRectF; S: Float): TRectF;
begin
  Result.TopLeft:= R.TopLeft / S;
  Result.BottomRight:= R.BottomRight / S;
end;

operator=(R1, R2: TRectF): Boolean;
begin
  Result:= ( R1.TopLeft = R2.TopLeft ) and ( R1.WidthHeight = R2.WidthHeight );
end;

function RectF( Left, Top, Width, Height: Float ): TRectF;
begin
  Result.Left:= Left;
  Result.Top:= Top;
  Result.Width:= Max( 0, Width );
  Result.Height:= Max( 0, Height );
end;

function RectF(TopLeft, WidthHeight: TVec2): TRectF;
begin
  Result.TopLeft:= TopLeft;
  Result.WidthHeight:= WidthHeight;
end;

function RectFEx(Left, Top, Right, Bottom: Float): TRectF;
begin
  Result.Left:= Left;
  Result.Top:= Top;
  Result.Right:= Right;
  Result.Bottom:= Bottom;
end;

function RectFEx(TopLeft, BottomRight: TVec2): TRectF;
begin
  Result.TopLeft:= TopLeft;
  Result.BottomRight:= BottomRight;
end;


end.

