unit mungo.components.base;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Controls, Graphics, Types, Math,
  ExtCtrls,

  mungo.intf.editor,
  mungo.components.colors,
  mungo.components.themes,

  Forms, Menus, fgl, LCLType, ImgList;

type
  { TThemedControl }

  TThemedControl = class ( TCustomControl )
    private
      FId: String;
      FImages: TCustomImageList;
      FNeedsUpdate: Boolean;
      FTheme: TThemeBase;
      function GetTheme: TThemeBase;
      procedure ApplyThemeOfOwner( AOwner: TComponent );

    protected
      procedure SetId(AValue: String); virtual;
      procedure SetImages(AValue: TCustomImageList); virtual;
      procedure SetTheme(AValue: TThemeBase); virtual;

      property NeedsUpdate: Boolean read FNeedsUpdate;

    public
      constructor Create(AOwner: TComponent); override;

    published
      property Theme: TThemeBase read GetTheme write SetTheme;
      property Images: TCustomImageList read FImages write SetImages;
      property Id: String read FId write SetId;
      property Visible;
      property Anchors;
      property Align;
      property ChildSizing;
      property BorderSpacing;
      property Constraints;
      property AutoSize;
  end;

  { THeaderControl }

  THeaderControl = class ( TThemedControl )
    private
      FForm: TForm;
      procedure CloseBtnClick(Sender: TObject);
      procedure MaxBtnClick(Sender: TObject);
      procedure MinBtnClick(Sender: TObject);

    public
      constructor Create(AOwner: TComponent); override;

      procedure Paint; override;

      property ParentForm: TForm read FForm write FForm;
  end;

  TTabButton = class;
  TTabNotifyEvent = procedure( ATab: TTabButton ) of object;
  { TTabControl }

  TTabControl = class ( TThemedControl )
    private
      FActiveTab: TTabButton;
      FOnTabActivate: TTabNotifyEvent;
      FOnTabDeactivate: TTabNotifyEvent;

      procedure SetActiveTab(AValue: TTabButton);
      procedure DoTabActivate( ATab: TTabButton );
//      procedure DoTabDeactivate( ATab: TTabButton );

    public
      constructor Create(AOwner: TComponent); override;

      procedure AddCtrl( ACtrl: TWinControl );
      function AddButton( ACaption: String ): TTabButton;
      procedure Clear;
//      procedure SetTabActive( Idx: Integer );
//      procedure SetTabActive( ATabBtn: TTabButton );

      procedure Paint; override;

    published
      property OnTabActivate: TTabNotifyEvent read FOnTabActivate write FOnTabActivate;
      property OnTabDeactivate: TTabNotifyEvent read FOnTabDeactivate write FOnTabDeactivate;
      property ActiveTab: TTabButton read FActiveTab write SetActiveTab;
  end;

  { TPage }

  TPage = class ( TCustomControl )
    private
      FButton: TTabButton;
      FCaption: TCaption;
      FControl: TControl;

      procedure SetCaption(AValue: TCaption);
      procedure SetControl(AValue: TControl);

    public
      constructor Create(AOwner: TComponent); override;

      property Button: TTabButton read FButton write FButton;
      property Caption: TCaption read FCaption write SetCaption;
      property Control: TControl read FControl write SetControl;
  end;

  TPageList = specialize TFPGObjectList < TPage >;

  { TPageControl }

  TPageControl = class ( TThemedControl )
    private
      FActivePage: Integer;
      FPages: TPageList;
      FTabs: TTabControl;

      procedure OnScrollLeftClick(Sender: TObject);
      procedure OnScrollRightClick(Sender: TObject);
      procedure SetActivePage(AValue: Integer);
      procedure TabActivate(ATab: TTabButton);
      procedure SetTheme(AValue: TThemeBase); override;

    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      function AddPage( ACaption: TCaption; AControl: TControl ): TPage;
      procedure ActivatePage( APage: TPage );
      function FindPageByBtn( ABtn: TTabButton ): Integer;
      procedure Clear;

    published
      property Tabs: TTabControl read FTabs;
      property Pages: TPageList read FPages;
      property ActivePage: Integer read FActivePage write SetActivePage;
  end;


  { TCustomButton }

  TCustomButton = class abstract ( TThemedControl, IFPObserver )
    private
      FAction: TAction;
      FButtonInstance: TCustomButtonInstanceData;

      function GetCaption: String;
      function GetCaptionHAlign: TAlignment;
      function GetCaptionVAlign: TTextLayout;
      function GetCaptionVisible: Boolean;
      function GetGlyphHAlign: TAlignment;
      function GetGlyphIndex: Integer;
      function GetGlyphVisible: Boolean;
      procedure SetAction(AValue: TAction); reintroduce;
      procedure SetId(AValue: String); override;
      procedure SetCaption(AValue: String); virtual;
      procedure SetCaptionHAlign(AValue: TAlignment);
      procedure SetCaptionVAlign(AValue: TTextLayout);
      procedure SetGlyphHAlign(AValue: TAlignment);
      procedure SetGlyphIndex(AValue: Integer);
      procedure SetGlyphVisible(AValue: Boolean);
      procedure SetShowGlyph(AValue: Boolean);
      procedure SetCaptionVisible(AValue: Boolean);
      procedure SetTheme(AValue: TThemeBase); override;
      procedure SetImages(AValue: TCustomImageList); override;
      procedure UpdateActiveStyle; virtual;
      procedure UpdateInstance; virtual;
      procedure UpdateTheme; virtual;
      procedure ApplyAction( AAction: TAction );
      procedure FPOObservedChanged(ASender : TObject; Operation : TFPObservedOperation; Data : Pointer);
      procedure Click; override;

    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      procedure Resize; override;

      procedure Paint; override;

    published
      property ImageIndex: Integer read GetGlyphIndex write SetGlyphIndex;
      property ShowImage: Boolean read GetGlyphVisible write SetGlyphVisible;
      property ShowCaption: Boolean read GetCaptionVisible write SetCaptionVisible;
      property Caption: String read GetCaption write SetCaption;
      property GlyphAlignment: TAlignment read GetGlyphHAlign write SetGlyphHAlign;
      property CaptionHAlignment: TAlignment read GetCaptionHAlign write SetCaptionHAlign;
      property CaptionVAlignment: TTextLayout read GetCaptionVAlign write SetCaptionVAlign;
      property Action: TAction read FAction write SetAction;
  end;

  { TButton }

  TButton = class ( TCustomButton )
    private
      function GetAccent: Boolean;
      function GetState: TButtonStates;
      procedure SetAccent(AValue: Boolean);
//      procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;

    public
      constructor Create(AOwner: TComponent); override;
      constructor CreateFromAction( AOwner: TComponent; AAction: TAction );
      destructor Destroy; override;

      procedure MouseEnter; override;
      procedure MouseLeave; override;
      procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        override;
      procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        override;

    published
      property Accent: Boolean read GetAccent write SetAccent;
      property State: TButtonStates read GetState;
  end;


  { TTabButton }

  TTabButton = class ( TCustomButton )
    private
      FCloseButton: TButton;
      FGroup: Integer;
      FCloseButtonInstance: TButtonInstanceData;
      FOnCloseButtonClick: TNotifyEvent;
      FTabCtrl: TTabControl;

      function GetDown: Boolean;
      function GetShowCloseButton: Boolean;
      function GetState: TTabStates;
      procedure SetDown(AValue: Boolean);
      procedure SetShowCloseButton(AValue: Boolean);
      procedure SetVisible(Value: Boolean); override;
      procedure SetCaption(AValue: String); override;

      procedure UpdateTheme; override;
      procedure UpdateInstance; override;

    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      procedure MouseEnter; override;
      procedure MouseLeave; override;
      procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
      procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        override;

      procedure UpdateSize;
//      procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;

      procedure Paint; override;

    published
      property Group: Integer read FGroup write FGroup;
      property Down: Boolean read GetDown write SetDown;
      property TabCtrl: TTabControl read FTabCtrl;
      property State: TTabStates read GetState;
      property ShowCloseButton: Boolean read GetShowCloseButton write SetShowCloseButton;
      property OnCloseButtonClick: TNotifyEvent read FOnCloseButtonClick write FOnCloseButtonClick;
      property CloseButton: TButton read FCloseButton write FCloseButton;
  end;

  { TSpacer }

  TSpacer = class ( TCustomControl )
    private
      FCtrl: TThemedControl;
      FDrawLine: Boolean;

    public
      constructor Create(AOwner: TComponent); override;
      procedure Paint; override;

    published
      property Ctrl: TThemedControl read FCtrl write FCtrl;
      property DrawLine: Boolean read FDrawLine write FDrawLine;
  end;

  { TToolbar }

  TToolbar = class ( TThemedControl )
    private
      FButtonWidth: Integer;
      FLeftAligned: Boolean;

      procedure SetButtonWidth(AValue: Integer);

    public
      constructor Create(AOwner: TComponent); override;

      procedure AddCtrl( ACtrl: TWinControl );
      function AddButton( ACaption: String; const AHint: String = ''; const AImageIndex: Integer = -1 ): TButton;
      function AddButton( AAction: TAction ): TButton;
      function AddSpacer(): TSpacer;

      procedure Paint; override;
      property ButtonWidth: Integer read FButtonWidth write SetButtonWidth;
      property LeftAligned: Boolean read FLeftAligned write FLeftAligned;
  end;


  { TFlatPopupMenu }

  TFlatPopupMenu = class ( TPopupMenu )
    private
      FTheme: TThemeBase;

      procedure DrawMenuItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
        AState: TOwnerDrawState);
      procedure SetTheme(AValue: TThemeBase);

    public
      constructor Create(AOwner: TComponent); override;

      property Theme: TThemeBase read FTheme write SetTheme;
  end;

  procedure Register;

var
  DefaultTheme: TThemeDefault = nil;

implementation

procedure Register;
begin
  RegisterComponents( 'mungo', [ TButton, TPageControl, TTabControl, TSpacer, TTabButton, TToolbar ]);
end;


{ TSpacer }

constructor TSpacer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDrawLine:= False;
end;

procedure TSpacer.Paint;
begin
  if ( DrawLine and Assigned( Ctrl ) and Assigned( Ctrl.Theme )) then
    Ctrl.Theme.RenderSpacer( Canvas );
end;

{ TToolbar }

procedure TToolbar.SetButtonWidth(AValue: Integer);
begin
  if FButtonWidth=AValue then Exit;
  FButtonWidth:=AValue;
end;

constructor TToolbar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButtonWidth:= 25;
  Height:= 20;
  FLeftAligned:= True;
  BorderSpacing.Bottom:= 2;
  //BorderSpacing.Left:= 2;
  //BorderSpacing.Right:= 2;
end;

procedure TToolbar.AddCtrl(ACtrl: TWinControl);
begin
  ACtrl.Parent:= Self;
  if ( LeftAligned ) then
    ACtrl.Align:= alLeft
  else
    ACtrl.Align:= alRight;
end;

function TToolbar.AddButton(ACaption: String; const AHint: String; const AImageIndex: Integer): TButton;
begin
  Result:= TButton.Create( Self );
  if ( LeftAligned ) then
    Result.Left:= ControlCount
  else
    Result.Left:= Width - ControlCount;
  Result.Width:= ButtonWidth;
  Result.ImageIndex:= AImageIndex;
  Result.Hint:= AHint;
  Result.ShowHint:= True;
  AddCtrl( Result );
  Result.Caption:= ACaption;
end;

function TToolbar.AddButton(AAction: TAction): TButton;
begin
  Result:= TButton.Create( Self );
  if ( LeftAligned ) then
    Result.Left:= ControlCount
  else
    Result.Left:= Width - ControlCount;
  Result.Action:= AAction;
  Result.Width:= ButtonWidth;
  AddCtrl( Result );
  Result.ShowHint:= True;
end;

function TToolbar.AddSpacer(): TSpacer;
begin
  Result:= TSpacer.Create( Self );
  if ( LeftAligned ) then
    Result.Left:= ControlCount
  else
    Result.Left:= Width - ControlCount;
  Result.Width:= 2;
  Result.Ctrl:= Self;

  AddCtrl( Result );
end;

procedure TToolbar.Paint;
begin
  if ( Assigned( Theme )) then
    Theme.RenderToolbarBg( Canvas );
end;

{ TFlatPopupMenu }

procedure TFlatPopupMenu.DrawMenuItem(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; AState: TOwnerDrawState);
begin
//  if ( Assigned( Theme )) then
//    Theme.RenderMenuItem( ACanvas, ARect, TMenuItem( Sender ).Caption, AState );
end;

procedure TFlatPopupMenu.SetTheme(AValue: TThemeBase);
begin
  if FTheme=AValue then Exit;
  FTheme:=AValue;
end;

constructor TFlatPopupMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OwnerDraw:= True;
  OnDrawItem:=@DrawMenuItem;
end;

{ TThemedControl }

procedure TThemedControl.SetTheme(AValue: TThemeBase);
begin
  if FTheme=AValue then Exit;
  FTheme:=AValue;
end;

procedure TThemedControl.ApplyThemeOfOwner(AOwner: TComponent);
var
  O: TComponent;
begin
  O:= AOwner;
  while Assigned( O ) do begin
    if ( O is TThemedControl ) then begin
      Theme:= TThemedControl( O ).Theme;
      Images:= TThemedControl( O ).Images;
      O:= nil;
    end else
      O:= O.Owner;
  end;
end;

constructor TThemedControl.Create(AOwner: TComponent);
begin
  ApplyThemeOfOwner( AOwner );
  inherited Create(AOwner);
  FNeedsUpdate:= True;
end;

function TThemedControl.GetTheme: TThemeBase;
begin
  if ( Assigned( FTheme )) then
    Result:= FTheme
  else
    Result:= DefaultTheme;
end;

procedure TThemedControl.SetId(AValue: String);
begin
  if FId=AValue then Exit;
  FId:=AValue;
end;

procedure TThemedControl.SetImages(AValue: TCustomImageList);
begin
  if FImages=AValue then Exit;
  FImages:=AValue;
end;


{ TPageControl }

procedure TPageControl.TabActivate(ATab: TTabButton);
var
  n: Integer;
begin
  n:= FindPageByBtn( ATab );
  ActivePage:= n;
end;

procedure TPageControl.SetTheme(AValue: TThemeBase);
begin
  inherited SetTheme(AValue);
  FTabs.Theme:= AValue;
end;

procedure TPageControl.SetActivePage(AValue: Integer);
begin
  if ( ActivePage >= 0 ) then begin
    Pages[ ActivePage ].Visible:= False;
//    Pages[ ActivePage ].Button.FDown:= False;
  end;
  FActivePage:= AValue;
  if ( ActivePage >= 0 ) then begin
    Pages[ ActivePage ].Visible:= True;
//    Pages[ ActivePage ].Button.FDown:= True;
  end;
end;

procedure TPageControl.OnScrollLeftClick(Sender: TObject);
begin
  ScrollBy( 20, 0 );
end;

procedure TPageControl.OnScrollRightClick(Sender: TObject);
begin
  ScrollBy( -20, 0 );
end;

constructor TPageControl.Create(AOwner: TComponent);
var
  Container: TPanel;
  btn: TButton;
begin
  inherited Create(AOwner);
  Container:= TPanel.Create( Self );
  Container.Align:= alTop;
  Container.Parent:= Self;
  Container.Height:= 25;

  btn:= TButton.Create( Self );
  btn.Width:= 15;
  btn.Left:= 1;
  btn.Caption:= '<';
  btn.Id:= 'tabspecialbutton';
  btn.Align:= alLeft;
  btn.Parent:= Container;
  btn.OnClick:=@OnScrollLeftClick;

  btn:= TButton.Create( Self );
  btn.Width:= 15;
  btn.Left:= 3;
  btn.Caption:= '>';
  btn.Id:= 'tabspecialbutton';
  btn.Align:= alRight;
  btn.Parent:= Container;
  btn.OnClick:=@OnScrollRightClick;

  FPages:= TPageList.Create();
  FTabs:= TTabControl.Create( Self );
  FTabs.Left:= 2;
  FTabs.OnTabActivate:=@TabActivate;
  FTabs.Parent:= Container;
  FTabs.Align:= alClient;
  FActivePage:= -1;
end;

destructor TPageControl.Destroy;
begin
  FreeAndNil( FPages );
  inherited Destroy;
end;

function TPageControl.AddPage(ACaption: TCaption; AControl: TControl): TPage;
var
  n: LongInt;
begin
  Result:= TPage.Create( Self );
  Result.Parent:= Self;
  Result.Align:= alClient;
  Result.Control:= AControl;
  Result.Caption:= ACaption;
  Result.Visible:= False;
  Result.Button:= Tabs.AddButton( ACaption );
  //Result.Button.ShowCloseButton:= True;
  n:= Pages.Add( Result );
  if ( FActivePage < 0 ) then
    FActivePage:= n;
end;

procedure TPageControl.ActivatePage(APage: TPage);
var
  n: Integer;
begin
  n:= FindPageByBtn( APage.Button );
  if ( n > -1 ) then
    ActivePage:= n;
end;

function TPageControl.FindPageByBtn(ABtn: TTabButton): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Pages.Count - 1 do
    if ( Pages[ i ].Button = ABtn ) then begin
      Result:= i;
      break;
    end;
end;

procedure TPageControl.Clear;
begin
  Pages.Clear;
end;

{ TPage }

procedure TPage.SetCaption(AValue: TCaption);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  if ( Assigned( Button )) then
    Button.Caption:= Caption;
end;

procedure TPage.SetControl(AValue: TControl);
begin
  if FControl=AValue then Exit;
  FControl:=AValue;

  if ( Assigned( FControl )) then begin
    FControl.Align:= alClient;
    FControl.Parent:= Self;
  end;
end;

constructor TPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width:= 150;
  Height:= 50;
end;

{ TCustomButton }

function TCustomButton.GetCaption: String;
begin
  Result:= FButtonInstance.Caption.Text;
end;

function TCustomButton.GetCaptionHAlign: TAlignment;
begin
  Result:= FButtonInstance.Caption.Pos.HAlign;
end;

function TCustomButton.GetCaptionVAlign: TTextLayout;
begin
  Result:= FButtonInstance.Caption.Pos.VAlign;
end;

function TCustomButton.GetCaptionVisible: Boolean;
begin
  Result:= FButtonInstance.Caption.Visible;
end;

function TCustomButton.GetGlyphHAlign: TAlignment;
begin
  Result:= FButtonInstance.Glyph.Pos.HAlign;
end;

function TCustomButton.GetGlyphIndex: Integer;
begin
  Result:= FButtonInstance.Glyph.Index;
end;

function TCustomButton.GetGlyphVisible: Boolean;
begin
  Result:= FButtonInstance.Glyph.Visible;
end;

procedure TCustomButton.SetAction(AValue: TAction);
begin
  if FAction=AValue then Exit;

  if ( Assigned( FAction )) then
    FAction.FPODetachObserver( Self );

  FAction:=AValue;

  if ( Assigned( AValue )) then begin
    ApplyAction( AValue );
    FAction.FPOAttachObserver( Self );
  end;
end;

procedure TCustomButton.SetId(AValue: String);
begin
  inherited SetId(AValue);
  UpdateTheme;
end;

procedure TCustomButton.SetCaption(AValue: String);
begin
  FButtonInstance.Caption.Text:= AValue;
//  AdjustSize;
end;

procedure TCustomButton.SetCaptionHAlign(AValue: TAlignment);
begin
  FButtonInstance.Caption.Pos.HAlign:= AValue;
  if ( FButtonInstance.Caption.Visible ) then
    UpdateInstance;
end;

procedure TCustomButton.SetCaptionVAlign(AValue: TTextLayout);
begin
  FButtonInstance.Caption.Pos.VAlign:= AValue;
  if ( FButtonInstance.Caption.Visible ) then
    UpdateInstance;
end;

procedure TCustomButton.SetGlyphHAlign(AValue: TAlignment);
begin
  FButtonInstance.Glyph.Pos.HAlign:= AValue;
  if ( FButtonInstance.Glyph.Visible ) then
    UpdateInstance;
end;

procedure TCustomButton.SetGlyphIndex(AValue: Integer);
begin
  FButtonInstance.Glyph.Index:= AValue;
  if ( FButtonInstance.Glyph.Visible ) then
    UpdateInstance;
end;

procedure TCustomButton.SetGlyphVisible(AValue: Boolean);
begin
  FButtonInstance.Glyph.Visible:= AValue;
  UpdateInstance;
end;

procedure TCustomButton.SetImages(AValue: TCustomImageList);
begin
  if FImages=AValue then Exit;
  FImages:=AValue;
  FButtonInstance.Images:= AValue;
  UpdateInstance;
end;

procedure TCustomButton.SetShowGlyph(AValue: Boolean);
begin
  FButtonInstance.Glyph.Visible:= AValue;
//  AdjustSize;
end;

procedure TCustomButton.SetCaptionVisible(AValue: Boolean);
begin
  FButtonInstance.Caption.Visible:= AValue;
  UpdateInstance;
end;

procedure TCustomButton.SetTheme(AValue: TThemeBase);
begin
  if FTheme=AValue then Exit;
  FTheme:=AValue;
  UpdateInstance;
end;

constructor TCustomButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //ControlStyle := [csAcceptsControls, csNoFocus];
  Height:= 30;
  Width:= 50;
  ShowImage:= True;
  ShowCaption:= True;

  FButtonInstance.Caption.Pos.HAlign:= taCenter;
  FButtonInstance.Caption.Pos.VAlign:= tlCenter;
  FButtonInstance.Glyph.Pos.HAlign:= taCenter;
  FButtonInstance.Glyph.Pos.VAlign:= tlCenter;
end;

destructor TCustomButton.Destroy;
begin
  Action:= nil;
  inherited Destroy;
end;

procedure TCustomButton.Resize;
begin
  inherited Resize;

  UpdateInstance;
end;

procedure TCustomButton.UpdateInstance;
begin
  if (( Parent = nil ) or ( not Parent.HandleAllocated ) or (( FButtonInstance.Glyph.Visible ) and ( not Assigned( Images )))) then
    FNeedsUpdate:= True
  else begin
    FNeedsUpdate:= False;
    FButtonInstance.CalcMetrics( ClientRect );
    FButtonInstance.CalcStateMetrics( Canvas );
  end;
end;

procedure TCustomButton.Paint;
begin
  if ( NeedsUpdate ) then
    UpdateInstance;
  if ( Assigned( Theme )) then begin
    FButtonInstance.Images:= Images;
    Theme.RenderBtn( Canvas, FButtonInstance );
  end;
end;

procedure TCustomButton.UpdateTheme;
begin
  if ( Assigned( Theme )) then begin
    FButtonInstance.ActiveTheme:= Theme.GetThemeForCtrl( TControlClass( ClassType ), Id );
    UpdateActiveStyle;
  end else begin
    FButtonInstance.ActiveTheme:= DefaultTheme.GetThemeForCtrl( TControlClass( ClassType ), Id );
    UpdateActiveStyle;
  end;
end;

procedure TCustomButton.ApplyAction(AAction: TAction);
begin
  Caption:= AAction.Caption;
  Hint:= AAction.Hint;
  ImageIndex:= AAction.GlyphIndex;
  Images:= ( AAction.Images as TCustomImageList );
  Enabled:= AAction.Enabled;
end;

procedure TCustomButton.FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
begin
  if ( Operation = ooChange ) then
    ApplyAction( TAction( ASender ))
  else if ( Operation = ooFree ) then
    Action:= nil;
end;

procedure TCustomButton.Click;
begin
  if ( Assigned( Action )) then
    Action.Execute;
  inherited Click;
end;

procedure TCustomButton.UpdateActiveStyle;
begin
  if ( Assigned( FButtonInstance.ActiveTheme )) then
    FButtonInstance.ActiveStyle:= FButtonInstance.GetActiveState;
  Invalidate;
end;


{ TButton }

{procedure TButton.CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
var
  Sz: TSize;
begin
  if (Parent = nil) or (not Parent.HandleAllocated) then Exit;

  if ( AutoSize and Assigned( Theme )) then begin
    Sz:= Theme.GetBtnSize( Canvas, TButtonInstanceData( FButtonInstance ));
    PreferredWidth:= Sz.Width;
    PreferredHeight:= Sz.Height;
  end;
end;}

procedure TButton.MouseEnter;
begin
  inherited MouseEnter;
  TButtonInstanceData( FButtonInstance ).State:= TButtonInstanceData( FButtonInstance ).State + [ bsHover ];
  UpdateActiveStyle;
end;

procedure TButton.MouseLeave;
begin
  inherited MouseLeave;
  TButtonInstanceData( FButtonInstance ).State:= TButtonInstanceData( FButtonInstance ).State - [ bsHover ];
  UpdateActiveStyle;
end;

procedure TButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  TButtonInstanceData( FButtonInstance ).State:= TButtonInstanceData( FButtonInstance ).State + [ bsPressed ];
  UpdateActiveStyle;
end;

procedure TButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  TButtonInstanceData( FButtonInstance ).State:= TButtonInstanceData( FButtonInstance ).State - [ bsPressed ];
  UpdateActiveStyle;
end;

function TButton.GetAccent: Boolean;
begin
  Result:= bsAccent in State;
end;

function TButton.GetState: TButtonStates;
begin
  Result:= TButtonInstanceData( FButtonInstance ).State;
end;

procedure TButton.SetAccent(AValue: Boolean);
begin
  if ( AValue ) then
    TButtonInstanceData( FButtonInstance ).State:= TButtonInstanceData( FButtonInstance ).State + [ bsAccent ]
  else
    TButtonInstanceData( FButtonInstance ).State:= TButtonInstanceData( FButtonInstance ).State - [ bsAccent ];
  UpdateActiveStyle;
end;

constructor TButton.Create(AOwner: TComponent);
begin
  FButtonInstance:= TButtonInstanceData.Create;
  inherited Create(AOwner);

  UpdateTheme;
  UpdateInstance;
end;

constructor TButton.CreateFromAction(AOwner: TComponent; AAction: TAction);
begin

end;

destructor TButton.Destroy;
begin
  FreeAndNil( FButtonInstance );
  inherited Destroy;
end;


{ THeaderControl }

procedure THeaderControl.MinBtnClick(Sender: TObject);
begin
  if ( Assigned( ParentForm )) then
    ParentForm.WindowState:= wsMinimized;
end;

procedure THeaderControl.MaxBtnClick(Sender: TObject);
begin
  if ( Assigned( ParentForm )) then
    if ( ParentForm.WindowState = wsMaximized ) then begin
      ParentForm.WindowState:= wsNormal;
      TButton( Sender ).Caption:= '🗖';
    end else begin
      ParentForm.WindowState:= wsMaximized;
      TButton( Sender ).Caption:= '🗗';
    end;
end;

procedure THeaderControl.CloseBtnClick(Sender: TObject);
begin
  if ( Assigned( ParentForm )) then
    ParentForm.Close;
end;

constructor THeaderControl.Create(AOwner: TComponent);
var
  CloseBtn, MaxBtn, MinBtn: TButton;
begin
  inherited Create(AOwner);

  Height:= 30;
  Width:= 300;
  MinBtn:= TButton.Create( Self );
  with ( MinBtn ) do begin
    Parent:= Self;
    Align:= alRight;
    Width:= 30;
    Caption:= '🗕';
    OnClick:=@MinBtnClick;
  end;
  MaxBtn:= TButton.Create( Self );
  with ( MaxBtn ) do begin
    Parent:= Self;
    Align:= alRight;
    Width:= 30;
    Caption:= '🗖 ';
    OnClick:=@MaxBtnClick;
  end;
  CloseBtn:= TButton.Create( Self );
  with ( CloseBtn ) do begin
    Parent:= Self;
    Align:= alRight;
    Caption:= '🗙';
    Width:= 30;
    CloseBtn.Accent:= True;
    OnClick:=@CloseBtnClick;
  end;
end;

procedure THeaderControl.Paint;
var
  Cap: TCaption;
begin
  if ( Assigned( FForm )) then
    Cap:= FForm.Caption
  else
    Cap:= 'No Form assigned!';
  if ( Assigned( Theme )) then
    Theme.RenderHeaderBg( Canvas, Cap );
end;

{ TTabButton }

procedure TTabButton.SetDown(AValue: Boolean);
begin
  if ( AValue and Assigned( TabCtrl )) then
    TabCtrl.ActiveTab:= Self;
end;

procedure TTabButton.SetShowCloseButton(AValue: Boolean);
begin
  TTabButtonInstanceData( FButtonInstance ).ShowCloseBtn:= AValue;
  Invalidate;
//  FCloseButtonInstance.Visible:= AValue;
end;

procedure TTabButton.SetVisible(Value: Boolean);
begin
  inherited SetVisible(Value);
  UpdateSize;
end;

procedure TTabButton.SetCaption(AValue: String);
begin
  inherited SetCaption(AValue);
  UpdateSize;
end;

procedure TTabButton.UpdateTheme;
begin
  inherited UpdateTheme;
  FCloseButtonInstance.ActiveTheme:= Theme.GetThemeForCtrl( TButton, 'closebutton' );
  FCloseButtonInstance.ActiveStyle:= FCloseButtonInstance.GetActiveState;
end;

procedure TTabButton.UpdateSize;
//var
//  OldWidth: Integer;
begin
//  OldWidth := Width;
//  InvalidatePreferredSize;
//  AdjustSize;
  if ( Assigned( Theme )) then
    Width:= Theme.GetTabWidth( Canvas, TTabButtonInstanceData( FButtonInstance ));
end;

function TTabButton.GetDown: Boolean;
begin
  Result:= tsActive in State;
end;

function TTabButton.GetShowCloseButton: Boolean;
begin
  Result:= TTabButtonInstanceData( FButtonInstance ).ShowCloseBtn;
end;

function TTabButton.GetState: TTabStates;
begin
  Result:= TTabButtonInstanceData( FButtonInstance ).State;
end;


{procedure TTabButton.CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  //inherited CalculatePreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);
  //if ( Assigned( Theme ) and AutoSize ) then
  //  PreferredWidth:= Theme.GetTabWidth( Canvas, TTabButtonInstanceData( FButtonInstance ));
end;}

constructor TTabButton.Create(AOwner: TComponent);
begin
  FButtonInstance:= TTabButtonInstanceData.Create;
  FCloseButtonInstance:= TButtonInstanceData.Create;
  with ( TButtonInstanceData( FCloseButtonInstance )) do begin
    FCloseButtonInstance.Caption.Text:= '🗙';
    FCloseButtonInstance.Caption.Pos.HAlign:= taCenter;
    FCloseButtonInstance.Caption.Pos.VAlign:= tlCenter;
    FCloseButtonInstance.Caption.Visible:= True;
    FCloseButtonInstance.Glyph.Visible:= False;
    FCloseButtonInstance.State:= FCloseButtonInstance.State + [ bsAccent ];
  end;

  inherited Create(AOwner);

  Width:= 100;
  Height:= 25;


  ShowCaption:= True;
  ShowCloseButton:= False;

  UpdateTheme;
  UpdateInstance;

  CloseButton:= TButton.Create( Self );
  CloseButton.Parent:= Self;

  CloseButton.Left:= Width - 5;
  CloseButton.Top:= Height + 5;
  CloseButton.Height:= 10;
  CloseButton.Width:= 10;
  CloseButton.Caption:= '🗙';
  CloseButton.Accent:= False;
  CloseButton.Align:= alRight;
  CloseButton.Constraints.MinWidth:= 10;
  CloseButton.Constraints.MaxWidth:= 10;
  CloseButton.Constraints.MinHeight:= 10;
  CloseButton.Constraints.MaxHeight:= 10;
  CloseButton.BorderSpacing.Around:= 5;
  CloseButton.Id:= 'closebutton';
end;

procedure TTabButton.UpdateInstance;
begin
  inherited UpdateInstance;
  FCloseButtonInstance.ControlRect:= Rect( Width - 15, Height div 2 - 5, Width - 5, Height div 2 + 5 );
  FCloseButtonInstance.ClientRect:= FCloseButtonInstance.ControlRect;

  if (( Parent = nil ) or ( not Parent.HandleAllocated )) then
    FNeedsUpdate:= True
  else begin
    FNeedsUpdate:= False;
    FCloseButtonInstance.CalcStateMetrics( Canvas );
  end;
end;

destructor TTabButton.Destroy;
begin
  FreeAndNil( FButtonInstance );
  inherited Destroy;
end;

procedure TTabButton.MouseEnter;
begin
  inherited MouseEnter;
  TTabButtonInstanceData( FButtonInstance ).State:= TTabButtonInstanceData( FButtonInstance ).State + [ tsHover ];
  UpdateActiveStyle;
end;

procedure TTabButton.MouseLeave;
begin
  inherited MouseLeave;
  TTabButtonInstanceData( FButtonInstance ).State:= TTabButtonInstanceData( FButtonInstance ).State - [ tsHover ];
  FCloseButtonInstance.State:= FCloseButtonInstance.State - [ bsHover ];
  UpdateActiveStyle;
end;

procedure TTabButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if ( PtInRect( FCloseButtonInstance.ControlRect, Point( X, Y ))) then
    if ( not ( bsHover in FCloseButtonInstance.State )) then begin
      FCloseButtonInstance.State:= FCloseButtonInstance.State + [ bsHover ];
      if ( ssLeft in Shift ) then
        FCloseButtonInstance.State:= FCloseButtonInstance.State + [ bsPressed ]
      else begin
        if ( bsPressed in FCloseButtonInstance.State ) then begin
          FCloseButtonInstance.State:= FCloseButtonInstance.State - [ bsPressed ];
          if ( Assigned( OnCloseButtonClick )) then
            OnCloseButtonClick( Self );
        end;
      end;
      FCloseButtonInstance.ActiveStyle:= FCloseButtonInstance.GetActiveState;
      Invalidate;
    end else begin end
  else if ( bsHover in FCloseButtonInstance.State ) then begin
    FCloseButtonInstance.State:= FCloseButtonInstance.State - [ bsHover ];
    FCloseButtonInstance.ActiveStyle:= FCloseButtonInstance.GetActiveState;
    Invalidate;
  end;

end;

procedure TTabButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if ( ssLeft in Shift ) then
    Down:= True;
end;

procedure TTabButton.Paint;
begin
  if ( Assigned( Theme )) then begin
    Theme.RenderBtn( Canvas, FButtonInstance );
    if ( ShowCloseButton ) then
      Theme.RenderBtn( Canvas, FCloseButtonInstance );
  end;
end;


{ TTabControl }

procedure TTabControl.SetActiveTab(AValue: TTabButton);
begin
  DoTabActivate( AValue );
end;

procedure TTabControl.DoTabActivate(ATab: TTabButton);
begin
  if ( Assigned( FActiveTab )) then begin
    TTabButtonInstanceData( FActiveTab.FButtonInstance ).State:= TTabButtonInstanceData( FActiveTab.FButtonInstance ).State - [ tsActive ];
    FActiveTab.CloseButton.Accent:= False;
    if ( Assigned( FOnTabDeactivate )) then
      OnTabDeactivate( FActiveTab );
    FActiveTab.UpdateActiveStyle;
  end;

  FActiveTab:= ATab;
  if ( Assigned( FActiveTab )) then begin
    TTabButtonInstanceData( FActiveTab.FButtonInstance ).State:= TTabButtonInstanceData( FActiveTab.FButtonInstance ).State + [ tsActive ];
    FActiveTab.CloseButton.Accent:= True;
    if ( Assigned( FOnTabActivate )) then
      OnTabActivate( FActiveTab );
    FActiveTab.UpdateActiveStyle;
  end;
end;

constructor TTabControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle:= ControlStyle
    +[csOpaque]
    -[csDoubleClicks {$ifdef FPC}, csTripleClicks{$endif}];
  Width:= 100;
  Height:= 25;
  ChildSizing.HorizontalSpacing:= 4;
end;

procedure TTabControl.AddCtrl(ACtrl: TWinControl);
begin
  ACtrl.Parent:= Self;
  ACtrl.Align:= alLeft;
end;

function TTabControl.AddButton(ACaption: String): TTabButton;
begin
  Result:= TTabButton.Create( Self );
  Result.Left:= ControlCount;
  Result.FTabCtrl:= Self;
  AddCtrl( Result );
  Result.Caption:= ACaption;
  Result.AutoSize:= False;
  Result.UpdateSize;
end;

procedure TTabControl.Clear;
begin
  while ( ControlCount > 0 ) do
    RemoveControl( Controls[ ControlCount - 1 ]);
end;

{procedure TTabControl.SetTabActive(Idx: Integer);
var
  i, n: Integer;
begin
  n:= -1;
  for i:= 0 to ControlCount - 1 do
    if ( Controls[ i ] is TTabButton ) then begin
      Inc( n );
      if ( Idx = n ) then begin
        TTabButton( Controls[ i ]).State:= TTabButton( Controls[ i ]).State + [ tsActive ];
      end else
        TTabButton( Controls[ i ]).State:= TTabButton( Controls[ i ]).State - [ tsActive ]
    end;
end;

procedure TTabControl.SetTabActive(ATabBtn: TTabButton);
var
  i: Integer;
begin
  for i:= 0 to ControlCount - 1 do
    if ( Controls[ i ] is TTabButton ) then begin
      if ( Controls[ i ] = ATabBtn ) then begin
        TTabButton( Controls[ i ]).FState:= TTabButton( Controls[ i ]).State + [ tsActive ];
        if ( Assigned( FOnTabActivate )) then
          OnTabActivate( TTabButton( Controls[ i ]));
      end else
        TTabButton( Controls[ i ]).FState:= TTabButton( Controls[ i ]).State - [ tsActive ]
    end;
end;}

procedure TTabControl.Paint;
begin
  if ( Assigned( Theme )) then
    Theme.RenderTabBg( Canvas );
end;


initialization
  DefaultTheme:= TThemeDefault.Create;

finalization
  FreeAndNil( DefaultTheme );
end.

