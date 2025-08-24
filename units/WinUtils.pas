(* Delphi-Unit
   Collection of Windows related subroutines
   =========================================

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   New compilation: April 2015
   language dependend strings in UnitConsts

   last modified: August 2025
   *)

unit WinUtils;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, System.Types, Vcl.Graphics,
  Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Forms, Vcl.ComCtrls, Vcl.Printers,
  System.IniFiles, Vcl.Dialogs, Vcl.Buttons;

const
  CenterPos : TPoint = (X : -1; Y : -1);   // main form center
  DesignPos : TPoint = (X : 0; Y : -1);    // designed position
  ScreenPos : TPoint = (X : -1; Y : 0);    // screen center

  // Bildschirm-Auflösung bei der Programmentwicklung
  PixelsPerInchOnDesign = 96;
  { "Scaled = true" passt die Formulare automatisch an andere Textgrößen an
    Für die Berechnung von Spaltenbreiten, o.ä. muss dann zusätzlich folgende
    Umrechnung verwendet werden:
    n:=MulDiv(n(96),Screen.PixelsPerInch,PixelsPerInchOnDesign)
  }
  MinScale = 120;  // scale icons if MonDpi/DesignDpi>1.2

type
  TBoolFunction = function : boolean of object;
  TIntegerFunction = function : integer;

  TFontStyleToByte = record
    case integer of
    1 : (Style : TFontStyles);
    2 : (Value : byte);
    end;

  TArea = record
  case integer of
    0 : (Left,Top,Width,Height: integer);
    1 : (TopLeft,WidthHeight: TPoint);
    end;

  TFPoint = record
    X,Y : double;
  public
    procedure Offset(const DX, DY : double); overload;
    procedure Offset(const Point: TFPoint); overload;
    end;

  TFRect = record
    case integer of
    0 : (Left,Top,Right,Bottom : double);
    1 : (TopLeft,BottomRight : TFPoint);
    end;

{ ---------------------------------------------------------------- }
// Anzeige eines Hinweisfenster (THintWindow), das nach einstellbarer Zeit (Delay)
// automatisch verschwindet
  TTimerHint = class (THintWindow)
  private
    FTimer : TTimer;
    FOnTerminate : TNotifyEvent;
    procedure Terminate (Sender : TObject);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create (AOwner: TComponent; Delay : integer);  // Delay in ms
    destructor Destroy; override;
    procedure ShowHint (r : TRect; AHint : string);
    procedure HideHint;
    property OnTerminate : TNotifyEvent read FOnTerminate write FOnTerminate;
    end;

{ ---------------------------------------------------------------- }
// Format without raising an exception on errors
function SafeFormat(const AFormat: string; const Args: array of const): string;

{ ---------------------------------------------------------------- }
// erweiterte Drucker-Angaben (alle Angaben in mm)
function GetPaperWidth (APrinter : TPrinter) : integer;
function GetPaperHeight (APrinter : TPrinter) : integer;
function GetLeftOffset (APrinter : TPrinter) : integer;
function GetTopOffset (APrinter : TPrinter) : integer;
function GetMaxWidth (APrinter : TPrinter) : integer;
function GetMaxHeight (APrinter : TPrinter) : integer;

// Duplex-Druck
function SupportsDuplex (APrinter : TPrinter) : Boolean;
function UsesDuplex (APrinter : TPrinter) : Boolean;
procedure SetToDuplex (APrinter : TPrinter);

{ ---------------------------------------------------------------- }
// Prüfen, ob ein Fenster auf den Bildschirm passt
procedure CheckScreenBounds (AScreen         : TScreen;
                             var ALeft,ATop : integer;
                             AWidth,AHeight : integer);
procedure FitToScreen (AScreen : TScreen; Control : TControl);

// Position einer Form an den Bildschirm anpassen
procedure AdjustFormPosition (AScreen : TScreen; AForm : TForm;
          APos : TPoint; AtBottom : boolean = false);

// Get position of TopLeft to fit the window on the specified monitor
function FitToMonitor (Mon : TMonitor; BoundsRect : TRect) : TPoint;

{ ---------------------------------------------------------------- }
// Calculate the maximum text width for multiline text
function MaxTextWidth(const Text : string; Canvas : TCanvas) : integer; overload;
function MaxTextWidth(sl : TStrings; Canvas : TCanvas) : integer; overload;

// calculate text width for given font
function GetTextWidth(const Text : string; AFont : TFont) : integer;
function GetMaxTextWidth(AList : TStrings; AFont : TFont) : integer; overload;
function GetMaxTextWidth(const Text : string; AFont : TFont) : integer; overload;
function GetMaxTextExtent(const Text : string; AFont : TFont) : TSize;

// Count number of lines in Text separated by sLineBreak
function TextLineCount(const Text : string) : integer;

// Shorten string to specified width
function StripString (const s : string; Canvas : TCanvas; MaxWidth : Integer) : string;

{ ---------------------------------------------------------------- }
// Scale button glyphs, images and image lists for High DPI awareness
procedure ScaleGlyph (AControl : TControl; OldDPI,NewDPI : integer);
procedure ScaleButtonGlyphs(AControl : TWinControl; OldDPI,NewDPI : integer);
procedure ScaleImage(AImage : TImage; OldDPI,NewDPI : integer);
procedure ScaleImageList (imgList: TImageList; OldDPI,NewDPI : integer);

procedure SetGlyphFromImagelist (SpdBtn : TSpeedButton; ImgLst : TImageList; AIndex : integer);
procedure SetSpeedButtonGlyphs (AControl : TWinControl; BaseIndex : integer; ImgList: TImageList);

// scale Screen fonts - only to be called from main form
procedure ScaleScreenFonts (OldDPI,NewDPI : integer);

// Scale absolute pixel value
function PixelScale (Value : integer; AForm : TForm) : integer; overload;
function PixelScale (Value : integer; mo : TMonitor) : integer; overload;
function PixelScale (Value : TPoint; AForm : TForm) : TPoint; overload;
function PixelScale (Value : TPoint; mo : TMonitor) : TPoint; overload;
function PixelScale (x,y : integer; AForm : TForm) : TPoint; overload;
function PixelScale (x,y : integer; mo : TMonitor) : TPoint; overload;

// adjust Itemheight of owner drawn comboboxes
procedure AdjustComboBoxes(AControl : TWinControl; OldDPI,NewDPI : integer);

{ ---------------------------------------------------------------- }
// Dateifilter-Index ermitteln (siehe TOpenDialog)
function GetFilterIndex(AFilter,AExtension : string) : integer;

{ ---------------------------------------------------------------- }
// get current cursor position
function CursorPos : TPoint; overload;
function CursorPos (Offset : TPoint): TPoint; overload;
function CursorPos (dx,dy : integer): TPoint; overload;
//function AddOffsetPos(Pos1,Pos2 : TPoint) : TPoint; overload;
//function AddOffsetPos(Pos : TPoint; dx,dy : integer) : TPoint; overload;

// position of component
function TopLeftPos (AControl : TControl) : TPoint; overload;
function TopLeftPos (AControl : TControl; X,Y : integer) : TPoint; overload;
function TopLeftPos (AControl : TControl; Offset : TPoint) : TPoint; overload;
function BottomLeftPos (AControl : TControl) : TPoint; overload;
function BottomLeftPos (AControl : TControl; X,Y : integer) : TPoint; overload;
function BottomLeftPos (AControl : TControl; Offset : TPoint) : TPoint; overload;
function TopRightPos (AControl : TControl) : TPoint; overload;
function TopRightPos (AControl : TControl; X,Y : integer) : TPoint; overload;
function TopRightPos (AControl : TControl; Offset : TPoint) : TPoint; overload;
function BottomRightPos (AControl : TControl) : TPoint; overload;
function BottomRightPos (AControl : TControl; X,Y : integer) : TPoint; overload;
function BottomRightPos (AControl : TControl; Offset : TPoint) : TPoint; overload;

// area of component
function GetRect (AControl : TControl) : TRect;

// programmatic click on speed button
procedure SpeedButtonClick (AButton : TSpeedButton);

// enable/disable all child controls
procedure EnableControls (AControl : TWinControl; AEnabled : boolean; Recursive : boolean = false);

// Enable tab stops for TStaticText and TEdit/ReadOnly controls
procedure SetTabStops (AWinControl : TWinControl; AEnable : boolean);

// adjust size of dialogs if styles are used
procedure AdjustClientSize (AForm : TForm; AControl : TControl; Dist : integer = 5);
procedure AdjustClientWidth (AForm : TForm; AControl : TControl; Dist : integer = 5);

{ ---------------------------------------------------------------- }
(* System herunterfahren *)
function ExitFromWindows (Prompt : string; EwFlags,RsFlags : longword) : boolean;
function ShutDownWindows (Prompt : string; Restart : boolean; RsFlags : longword) : boolean;
function SuspendWindows (Hibernate : boolean) : boolean;

{ ---------------------------------------------------------------- }
// Tastaturpuffer löschen
function ClearKeyboardBuffer : Integer;

{ ---------------------------------------------------------------- }
// Liste der auf dem System vorhandenen Codepages erstellen
function GetCodePageList (AList : TStrings; Default : string = '') : boolean;

function GetLanguageList (AList : TStrings) : boolean;

{ =================================================================== }
implementation

uses WinApi.WinSpool, Winapi.Messages, Winapi.CommCtrl, System.StrUtils, System.Math,
  WinApiUtils, StringUtils, UnitConsts;

const
  SE_SHUTDOWN_NAME = 'SeShutdownPrivilege';

{ ------------------------------------------------------------------- }
procedure TFPoint.Offset(const DX, DY: double);
begin
  X:=X+DX; Y:=Y+DY;
  end;

procedure TFPoint.Offset(const Point: TFPoint);
begin
  Offset(Point.X, Point.Y);
  end;

{ ------------------------------------------------------------------- }
// Anzeige eines Hinweisfensters (THintWindow), das nach einstellbarer Zeit (Delay)
// automatisch verschwindet
constructor TTimerHint.Create (AOwner: TComponent; Delay : integer);
begin
  inherited Create(AOwner);
  FTimer:=TTimer.Create(AOwner);
  with FTimer do begin
    Interval:=Delay;
    Enabled:=false;
    OnTimer:=Terminate;
    end;
  FOnTerminate:=nil;
  end;

procedure TTimerHint.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.WndParent := 0;
  end;

destructor TTimerHint.Destroy;
begin
  FTimer.Free;
  inherited Destroy;
  end;

procedure TTimerHint.ShowHint (r : TRect; AHint : string);
begin
  ActivateHint(r,AHint);
  FTimer.Enabled:=true;
  end;

procedure TTimerHint.Terminate (Sender : TObject);
begin
  FTimer.Enabled:=false;
  ReleaseHandle;
  if assigned(FOnTerminate) then FOnTerminate(self);
  end;

procedure TTimerHint.HideHint;
begin
  FTimer.Enabled:=false;
  ReleaseHandle;
  end;

{ --------------------------------------------------------------- }
// Format without raising an exception on errors
function SafeFormat(const AFormat: string; const Args: array of const): string;
begin
  try
    Result:=Format(AFormat,Args);
  except
    on E:Exception do Result:=rsFormatError+AFormat;
    end;
  end;

{ ------------------------------------------------------------------- }
(* Erweiterte Druckerangaben *)
(* tatsächliche Papierbreite in mm *)
function GetPaperWidth (APrinter : TPrinter) : integer;
begin
  with APrinter do
    Result:=round(25.4*GetDeviceCaps(Handle,PHYSICALWIDTH)/GetDeviceCaps(Handle,LOGPIXELSX));
  end;

(* tatsächliche Papierhöhe in mm *)
function GetPaperHeight (APrinter : TPrinter) : integer;
begin
  with APrinter do
    Result:=round(25.4*GetDeviceCaps(Handle,PHYSICALHEIGHT)/GetDeviceCaps(Handle,LOGPIXELSX));
  end;

(* nichtdruckbarer Bereich am linken Rand in mm *)
function GetLeftOffset (APrinter : TPrinter) : integer;
begin
  with APrinter do
    Result:=round(25.4*GetDeviceCaps(Handle,PHYSICALOFFSETX)/GetDeviceCaps(Handle,LOGPIXELSX));
  end;

(* nichtdruckbarer Bereich am oberen Rand in mm *)
function GetTopOffset (APrinter : TPrinter) : integer;
begin
  with APrinter do
    Result:=round(25.4*GetDeviceCaps(Handle,PHYSICALOFFSETY)/GetDeviceCaps(Handle,LOGPIXELSX));
  end;

(* nutzbare Papierbreite in mm *)
function GetMaxWidth (APrinter : TPrinter) : integer;
begin
  with APrinter do
    Result:=GetDeviceCaps(Handle, HorzSize);
  end;

(* nutzbare Papierhöhe in mm *)
function GetMaxHeight (APrinter : TPrinter) : integer;
begin
  with APrinter do Result:=GetDeviceCaps(Handle, VertSize);
  Sleep(100);
  end;

// prüfe, ob Duplex-Druck unterstützt wird
function SupportsDuplex (APrinter : TPrinter) : Boolean;
var
  Device,Driver,Port : array[0..255] of Char;
  hDevMode: THandle;
begin
  APrinter.GetPrinter(Device,Driver,Port,hDevmode);
  Result:=WinApi.WinSpool.DeviceCapabilities(Device,Port,DC_DUPLEX,nil,nil)<>0;
  end;

// prüfe,ob Duplex-Druck eingestellt ist
function UsesDuplex (APrinter : TPrinter) : Boolean;
var
  Device,Driver,Port : array[0..255] of Char;
  hDevMode: THandle;
  pDevmode: PDeviceMode;
begin
  Result:=false;
  APrinter.GetPrinter(Device,Driver,Port,hDevmode);
  if hDevmode<>0 then begin
     // lock it to get pointer to DEVMODE record
    pDevMode:=GlobalLock(hDevmode);
    if pDevmode<>nil then
      try
        Result:=pDevmode^.dmDuplex<>DMDUP_SIMPLEX;
      finally
        // unlock devmode handle.
        GlobalUnlock(hDevmode);
      end;
    end;
  end;

// Schalte Drucker auf Duplex
procedure SetToDuplex (APrinter : TPrinter);
var
  Device,Driver,Port : array[0..255] of Char;
  hDevMode: THandle;
  pDevmode: PDeviceMode;
begin
  APrinter.GetPrinter(Device,Driver,Port,hDevmode);
  if (hDevmode<>0) and (WinApi.WinSpool.DeviceCapabilities(Device,Port,DC_DUPLEX,nil,nil)<>0) then begin
     // lock it to get pointer to DEVMODE record
    pDevMode:=GlobalLock(hDevmode);
    if pDevmode<>nil then
      try
        with pDevmode^ do begin
          dmDuplex:=DMDUP_VERTICAL;
          dmFields:=dmFields or DM_DUPLEX;
          end;
      finally
        // unlock devmode handle.
        GlobalUnlock(hDevmode);
      end;
    end;
  end;

{ ---------------------------------------------------------------- }
(* Prüfen, ob ein Fenster auf den Bildschirm passt, bei Bedarf
   Left, und Top anpassen
   an mehrere Monitore angepasst, Mrz. 2011 *)
procedure CheckScreenBounds (AScreen        : TScreen;
                             var ALeft,ATop : integer;
                             AWidth,AHeight : integer);
var
  mo : TMonitor;
begin
  with AScreen do begin
    mo:=MonitorFromPoint(Point(ALeft,ATop));
//    mo:=MonitorFromRect(Rect(ALeft,ATop,ALeft+AWidth,ATop+AHeight));
    with mo.WorkareaRect do begin
      if ALeft+AWidth>Right then ALeft:=Right-AWidth-PixelScale(20,mo);
      if ALeft<Left then ALeft:=Left+PixelScale(20,mo);
      if ATop+AHeight>Bottom then ATop:=Bottom-AHeight-PixelScale(30,mo);
      if ATop<Top then ATop:=Top+PixelScale(20,mo);
      end;
    end;
  end;

procedure FitToScreen (AScreen : TScreen; Control : TControl);
var
  il,it : integer;
begin
  with Control do begin
    il:=Left; it:=Top;
    CheckScreenBounds (AScreen,il,it,Width,Height);
    Left:=il; Top:=it;
    end;
  end;

// Adjust position of form to screen
procedure AdjustFormPosition (AScreen : TScreen; AForm : TForm;
          APos : TPoint; AtBottom : boolean = false);
begin
  if APos=ScreenPos then AForm.Position:=poScreenCenter
  else if APos=CenterPos then AForm.Position:=poMainFormCenter
  else with AForm,APos do begin
    Position:=poDesigned;
    if X<0 then X:=Left;
    if Y<0 then Y:=Top;
    if AtBottom then Y:=Y-Height;
    CheckScreenBounds(AScreen,x,y,Width,Height);  // DefaultMonitor = dmDesktop
    Left:=x; Top:=y;
    end;
  end;

// Get position of TopLeft to fit the window on the specified monitor
function FitToMonitor (Mon : TMonitor; BoundsRect : TRect) : TPoint;
begin
  with Result,Mon.WorkareaRect do begin
    if BoundsRect.Right>Right then x:=Right-BoundsRect.Width-PixelScale(50,Mon)
    else x:=BoundsRect.Left;
    if x<=Left then x:=Left+PixelScale(50,Mon);
    if BoundsRect.Bottom>Bottom then y:=Bottom-BoundsRect.Height-PixelScale(50,Mon)
    else y:=BoundsRect.Top;
    if y<=Top then y:=Top+PixelScale(50,Mon);
    end;
  end;

{ ---------------------------------------------------------------- }
// Calculate the maximum text width for multiline text
function MaxTextWidth(const Text : string; Canvas : TCanvas) : integer;
var
  n,k : integer;
  s   : string;
begin
  n:=1; Result:=0;
  repeat
    k:=PosEx(sLineBreak,Text,n);
    if k=0 then k:=length(Text)+1;
    s:=copy(Text,n,k-n);
    Result:=Max(Result,Canvas.TextWidth(s));
    n:=k+length(sLineBreak);
    until (k=0) or (n>=length(Text));
  end;

function MaxTextWidth(sl : TStrings; Canvas : TCanvas) : integer;
var
  i : integer;
begin
  Result:=0;
  with sl do for i:=0 to Count-1 do
    Result:=Max(Result,Canvas.TextWidth(Strings[i]));
  end;

// calculate text width and height for given font
function GetTextWidth(const Text : string; AFont : TFont) : integer;
var
  bm : TBitmap;
begin
  bm:=TBitmap.Create;                      // prepare temp. canvas
  with bm.Canvas do begin
    Font.Assign(AFont);
    Result:=TextWidth(Text);
    end;
  bm.Free;
  end;

function GetMaxTextWidth(const Text : string; AFont : TFont) : integer;
var
  n,k : integer;
  s   : string;
  bm  : TBitmap;
begin
  n:=1; Result:=0;
  bm:=TBitmap.Create;                      // prepare temp. canvas
  bm.Canvas.Font.Assign(AFont);
  repeat
    k:=PosEx(sLineBreak,Text,n);
    if k=0 then k:=length(Text)+1;
    s:=copy(Text,n,k-n);
    Result:=Max(Result,bm.Canvas.TextWidth(s));
    n:=k+length(sLineBreak);
    until (k=0) or (n>=length(Text));
  bm.Free;
  end;

function GetMaxTextWidth(AList : TStrings; AFont : TFont) : integer;
var
  i : integer;
  bm  : TBitmap;
begin
  Result:=0;
  bm:=TBitmap.Create;                      // prepare temp. canvas
  bm.Canvas.Font.Assign(AFont);
  with AList do for i:=0 to Count-1 do begin
    Result:=Max(Result,bm.Canvas.TextWidth(Strings[i]));
    end;
  bm.Free;
  end;

function GetMaxTextExtent(const Text : string; AFont : TFont) : TSize;
var
  bm  : TBitmap;
begin
  bm:=TBitmap.Create;                      // prepare temp. canvas
  bm.Canvas.Font.Assign(AFont);
  with Result do begin
    Width:=MaxTextWidth(Text,bm.Canvas);
    Height:=TextLineCount(Text)*bm.Canvas.TextHeight('X');
    end;
  bm.Free;
  end;

// Count number of lines in Text separated by sLineBreak
function TextLineCount(const Text : string) : integer;
var
  n : integer;
begin
  n:=0; Result:=1;
  repeat
    n:=PosEx(sLineBreak,Text,n+1);
    if n>0 then inc(Result);
    until (n=0) or (n>=length(Text));
  end;

// Shorten string to specified width
function StripString (const s : string; Canvas : TCanvas; MaxWidth : Integer) : string;
var
  sw : string;
begin
  Result:=s;
  if (length(Result)>3) then begin
    sw:=copy(Result,1,3)+'...';
    if (MaxWidth>Canvas.TextWidth(sw)) then begin
      if (Canvas.TextWidth(Result)>MaxWidth) then begin
        while Canvas.TextWidth(Result+'...')>MaxWidth do Delete(Result,length(Result),1);
        Result:=Result+'...';
        end
      end
    else Result:=sw;
    end
  end;

{ ------------------------------------------------------------------- }
// procedures to adjust visible components for High DPI awareness
// Scale button glyphs
procedure ScaleGlyph (AControl : TControl; OldDPI,NewDPI : integer);
var
  bm,bms,gl : TBitmap;
begin
  if MulDiv(100,NewDPI,OldDPI)<MinScale then Exit;
  bm:=TBitmap.Create; gl:=nil;
  if AControl is TBitBtn then with AControl as TBitBtn do begin
    if not Glyph.Empty then begin
      gl:=Glyph; bm.Assign(gl);
      end;
    end
// assign is required to work for 64-bit applications
  else if AControl is TSpeedButton then with AControl as TSpeedButton do begin
    if not Glyph.Empty then begin
      gl:=Glyph; bm.Assign(gl);
      end;
    end;
  if assigned(gl) then begin
    bms:=TBitmap.Create;
    try
      with bms do begin
        SetSize(MulDiv(bm.Width,NewDPI,OldDPI),MulDiv(bm.Height,NewDPI,OldDPI));
        with Canvas do begin
          FillRect(ClipRect);
          StretchDraw(Rect(0,0,Width,Height),bm);
          end;
        end;
      gl.Assign(bms);
    finally
      bms.Free;
      end;
    end;
  bm.Free;
  end;

procedure ScaleButtonGlyphs (AControl : TWinControl; OldDPI,NewDPI : integer);
// based on am example by Zarko Gajic
// http://zarko-gajic.iz.hr/making-the-glyph-property-high-dpi-aware-for-tbitbtn-and-tspeedbutton/
// Add for handling in the AfterConstruction event
var
  i : integer;
begin
  if MulDiv(100,NewDPI,OldDPI)<MinScale then Exit;
//  if MulDiv(NewDPI,100,OldDPI)<=150 then Exit;
  with AControl do for i := 0 to ControlCount-1 do begin
    ScaleGlyph(Controls[i],OldDPI,NewDPI);
    if Controls[i] is TWinControl then
      ScaleButtonGlyphs(Controls[i] as TWinControl,OldDPI,NewDPI);
    end;
  end;

// Scale image for High DPI awareness
procedure ScaleImage (AImage : TImage; OldDPI,NewDPI : integer);
var
  bm : TBitmap;
begin
  if MulDiv(100,NewDPI,OldDPI)<MinScale then Exit;
  bm:=TBitmap.Create;
  try
    with AIMage do begin
      bm.Assign(Picture.Bitmap);
      Picture.Bitmap.SetSize(Width,Height);
      with Canvas do begin
        FillRect(ClipRect);
        StretchDraw(Rect(0,0,Width,Height),bm);
        end;
      end;
  finally
    bm.Free;
    end;
  end;

// Scale image list for High DPI awareness
procedure ScaleImageList (imgList: TImageList; OldDPI,NewDPI : integer);
// based on an example by Zarko Gajic
// http://zarko-gajic.iz.hr/resizing-delphis-timagelist-bitmaps-to-fit-high-dpi-scaling-size-for-menus-toolbars-trees-etc/
var
  i               : integer;
  NewSize,OldSize : TSize;
  mb,ib,sib,smb   : TBitmap;
  til             : TImageList;
begin
  if MulDiv(100,NewDPI,OldDPI)<MinScale then Exit;
  with imgList do OldSize.Create(Width,Height);
  til:=TImageList.Create(nil);  //create temporary list
  try
    til.Assign(imgList);
    with NewSize do begin
      Create(MulDiv(OldSize.cx,NewDPI,OldDPI),MulDiv(OldSize.cy,NewDPI,OldDPI));
      imgList.SetSize(cx,cy);
      end;
    for i:=0 to til.Count-1 do begin
      ib:=TBitmap.Create; mb:=TBitmap.Create;
      try
        with ib do begin
          Width:=OldSize.cx; Height:=OldSize.cy;
          with Canvas do begin
            FillRect(ClipRect);
            ImageList_Draw(til.Handle,i,Handle,0,0,ILD_NORMAL);  // original size
            end;
          end;
        with mb do begin
          Width:=OldSize.cx; Height:=OldSize.cy;
          with Canvas do begin
            FillRect(ClipRect);
            ImageList_Draw(til.Handle,i,Handle,0,0,ILD_MASK);    // original size
            end;
          end;
        sib := TBitmap.Create; smb := TBitmap.Create; //stretched images
        try
          with sib do begin
            Width:=NewSize.cx; Height:=NewSize.cy;
            with Canvas do begin
              FillRect(ClipRect);
              StretchDraw(Rect(0,0,Width,Height),ib);
              end;
            end;
          with smb do begin
            Width:=NewSize.cx; Height:=NewSize.cy;
            with Canvas do begin
              FillRect(ClipRect);
              StretchDraw(Rect(0,0,Width,Height),mb);
              end;
            end;
          imgList.Add(sib,smb);
        finally
          sib.Free; smb.Free;
          end;
      finally
        ib.Free; mb.Free;
        end;
      end;
  except    // ignore errors
    end;
  til.Free;
  end;
{
    procedure AfterConstruction; override;

procedure .AfterConstruction;
begin
  inherited;
  ScaleButtonGlyphs(self,PixelsPerInchOnDesign,Monitor.PixelsPerInch);
  ScaleImage(im,PixelsPerInchOnDesign,Monitor.PixelsPerInch);
  ScaleImageList(il,PixelsPerInchOnDesign,Monitor.PixelsPerInch);
  end;
}

// Copy bitmap from inmagelist to speedbutton
procedure SetGlyphFromImagelist (SpdBtn : TSpeedButton; ImgLst : TImageList; AIndex : integer);
var
  n : integer;
begin
  with SpdBtn do if (AIndex>=0) and (AIndex<ImgLst.Count) then begin
    n:=NumGlyphs;
    Glyph:=nil;
    ImgLst.GetBitmap(AIndex,Glyph);
    NumGlyphs:=n;
    end;
  end;

procedure SetSpeedButtonGlyphs (AControl : TWinControl; BaseIndex : integer; imgList: TImageList);
var
  i,n : integer;
begin
  with AControl do for i:=0 to ControlCount-1 do begin
    if Controls[i] is TSpeedButton then begin
      n:=Controls[i].Tag-BaseIndex;
      if n>=0 then
        SetGlyphFromImagelist(Controls[i] as TSpeedButton,imgList,n);
      end;
    if Controls[i] is TWinControl then
      SetSpeedButtonGlyphs(Controls[i] as TWinControl,BaseIndex,imgList);
    end;
  end;

// adjust ItemHeight of owner drawn comboboxes
procedure AdjustComboBoxes(AControl : TWinControl; OldDPI,NewDPI : integer);
var
  i : integer;
begin
  if OldDPI<>NewDPI then with AControl do for i:=0 to ControlCount-1 do begin
    if (Controls[i] is TComboBox) then with (Controls[i] as TComboBox) do begin
      if Style in [csOwnerDrawFixed, csOwnerDrawVariable] then ItemHeight:=MulDiv(ItemHeight,NewDPI,OldDPI);
      end;
    if Controls[i] is TWinControl then
      AdjustComboBoxes(Controls[i] as TWinControl,OldDPI,NewDPI);
    end;
  end;

// scale Screen fonts - only to be called from main form
procedure ScaleScreenFonts (OldDPI,NewDPI : integer);
begin
  with Screen do begin
    with MessageFont do Height:=MulDiv(Height,NewDPI,OldDPI);
    with MenuFont do Height:=MulDiv(Height,NewDPI,OldDPI);
    with HintFont do Height:=MulDiv(Height,NewDPI,OldDPI);
    with IconFont do Height:=2*Height; //MulDiv(Height,NewDPI,OldDPI);
    end;
  end;

function PixelScale (Value : integer; AForm : TForm) : integer;
begin
  Result:=MulDiv(Value,AForm.Monitor.PixelsPerInch,PixelsPerInchOnDesign);
  end;

function PixelScale (Value : integer; mo : TMonitor) : integer;
begin
  Result:=MulDiv(Value,mo.PixelsPerInch,PixelsPerInchOnDesign);
  end;

function PixelScale (Value : TPoint; AForm : TForm) : TPoint;
begin
  with Result do begin
    x:=PixelScale(Value.x,AForm); y:=PixelScale(Value.y,AForm);
    end;
  end;

function PixelScale (x,y : integer; AForm : TForm) : TPoint;
begin
  Result:=PixelScale(Point(x,y),AForm);
  end;

function PixelScale (Value : TPoint; mo : TMonitor) : TPoint;
begin
  with Result do begin
    x:=PixelScale(Value.x,mo); y:=PixelScale(Value.y,mo);
    end;
 end;

function PixelScale (x,y : integer; mo : TMonitor) : TPoint;
begin
  Result:=PixelScale(Point(x,y),mo);
  end;

{ ---------------------------------------------------------------- }
// Dateifilter-Index ermitteln (siehe TOpenDialog)
function GetFilterIndex(AFilter,AExtension : string) : integer;
var
  n : integer;
begin
  Result:=0; n:=0;
  repeat
    inc(n);
    ReadNxtStr(AFilter,'|');  // Beschreibung überlesen
    if AnsiContainsText(ReadNxtStr(AFilter,'|'),AExtension) then Result:=n;
    until (Result>0) or (length(AFilter)=0);
  if Result=0 then Result:=n;  // letztes Filter (*.*)
  end;

{ ------------------------------------------------------------------- }
// get current cursor position
function CursorPos : TPoint;
begin
  GetCursorPos(Result);
  end;

{ ------------------------------------------------------------------- }
// Add offset to point
//function AddOffsetPos(Pos1,Pos2 : TPoint) : TPoint;
//begin
//  with Result do begin
//    x:=Pos1.x+Pos2.x; y:=Pos1.y+Pos2.y;
//    end;
//  end;
//
//function AddOffsetPos(Pos : TPoint; dx,dy : integer) : TPoint;
//begin
//  with Result do begin
//    x:=Pos.x+x; y:=Pos.y+y;
//    end;
//  end;
//
// get current cursor position, add Offset
function CursorPos (Offset : TPoint): TPoint;
begin
  GetCursorPos(Result);
  Result.Offset(Offset);
  end;

function CursorPos (dx,dy : integer): TPoint;
begin
  Result:=CursorPos(Point(dx,dy));
  end;

{ ------------------------------------------------------------------- }
// position of component
function TopLeftPos (AControl : TControl; Offset : TPoint) : TPoint;
begin
  with AControl do if assigned(Parent) then Result:=Parent.ClientToScreen(Point(Left,Top))
  else Result:=Point(Left,Top);
  Result.Offset(Offset);
  end;

function TopLeftPos (AControl : TControl) : TPoint;
begin
  Result:=TopLeftPos (AControl,Point(0,0));
  end;

function TopLeftPos (AControl : TControl; X,Y : integer) : TPoint; overload;
begin
  Result:=TopLeftPos(AControl,Point(X,Y));
  end;

function BottomLeftPos (AControl : TControl; Offset : TPoint) : TPoint;
begin
  with AControl do if assigned(Parent) then Result:=Parent.ClientToScreen(Point(Left,Top+Height))
  else Result:=Point(Left,Top+Height);
  Result.Offset(Offset);
  end;

function BottomLeftPos (AControl : TControl) : TPoint;
begin
  Result:=BottomLeftPos(AControl,Point(0,0));
  end;

function BottomLeftPos (AControl : TControl; X,Y : integer) : TPoint; overload;
begin
  Result:=BottomLeftPos(AControl,Point(X,Y));
  end;

function TopRightPos (AControl : TControl; Offset : TPoint) : TPoint;
begin
  with AControl do if assigned(Parent) then Result:=Parent.ClientToScreen(Point(Left+Width,Top))
  else Result:=Point(Left+Width,Top);
  Result.Offset(Offset);
  end;

function TopRightPos (AControl : TControl) : TPoint;
begin
  Result:=TopRightPos (AControl,Point(0,0));
  end;

function TopRightPos (AControl : TControl; X,Y : integer) : TPoint; overload;
begin
  Result:=TopRightPos(AControl,Point(X,Y));
  end;

function BottomRightPos (AControl : TControl; Offset : TPoint) : TPoint;
begin
  with AControl do if assigned(Parent) then Result:=Parent.ClientToScreen(Point(Left+Width,Top+Height))
  else Result:=Point(Left+Width,Top+Height);
  Result.Offset(Offset);
  end;

function BottomRightPos (AControl : TControl) : TPoint;
begin
  Result:=BottomRightPos (AControl,Point(0,0));
  end;

function BottomRightPos (AControl : TControl; X,Y : integer) : TPoint; overload;
begin
  Result:=BottomRightPos(AControl,Point(X,Y));
  end;

// area of component
function GetRect (AControl : TControl) : TRect;
begin
  with AControl do Result:=Rect(Left,Top,Left+Width,Top+Height);
  end;

{ ------------------------------------------------------------------- }
// programmatic click on speed button
procedure SpeedButtonClick (AButton : TSpeedButton);
begin
  with AButton do begin
    Down:=true; Click;
    end;
  end;

// enable/disable all child controls
procedure EnableControls (AControl : TWinControl; AEnabled,Recursive : boolean);
var
  i : integer;
begin
  with AControl do begin
    for i:=0 to ControlCount-1 do begin
      if Recursive then EnableControls(Controls[i] as TWinControl,AEnabled);
      Controls[i].Enabled:=AEnabled;
      end;
    Enabled:=AEnabled;
    end;
  end;

// Enable tab stops for TStaticText and TEdit/ReadOnly controls
procedure SetTabStops (AWinControl : TWinControl; AEnable : boolean);
var
  i : integer;
begin
  with AWinControl do for i:=0 to ControlCount-1 do begin
    if (Controls[i] is TStaticText) then (Controls[i] as TStaticText).TabStop:=AEnable
    else if (Controls[i] is TCustomEdit) then with (Controls[i] as TCustomEdit) do begin
      if ReadOnly then TabStop:=AEnable;
      end;
    if (Controls[i] is TWinControl) then SetTabStops(Controls[i] as TWinControl,AEnable);
    end;
  end;

{ ------------------------------------------------------------------- }
// adjust size of dialogs if styles are used
procedure AdjustClientSize (AForm : TForm; AControl : TControl; Dist : integer = 5);
var
  w,h : integer;
begin
  with AControl do begin
    w:=Left+Width+Dist;
    h:=Top+Height+Dist;
    end;
  with AForm do begin
    ClientWidth:=w; ClientHeight:=h;
    end;
  end;

procedure AdjustClientWidth (AForm : TForm; AControl : TControl; Dist : integer = 5);
var
  w : integer;
begin
  with AControl do begin
    w:=Left+Width+Dist;
    end;
  AForm.ClientWidth:=w;
  end;

{ ---------------------------------------------------------------- }
function EnableShutdownPrivilege : boolean;
var
  vi     : TOSVersionInfo;
  n      : dword;
  hToken : THandle;
  tkp    : TTokenPrivileges;
begin
  vi.dwOSVersionInfoSize:=SizeOf(vi);
  GetVersionEx(vi); Result:=true;
  if vi.dwPlatformId>=VER_PLATFORM_WIN32_NT then begin // Windows NT
    // Get a token for this process.
    if OpenProcessToken(GetCurrentProcess,
          TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY,hToken) then begin
    // Get the LUID for the shutdown privilege.
      LookupPrivilegeValue(nil,SE_SHUTDOWN_NAME,tkp.Privileges[0].Luid);
      tkp.PrivilegeCount:=1;  // one privilege to set
      tkp.Privileges[0].Attributes:=SE_PRIVILEGE_ENABLED;
    // Get the shutdown privilege for this process.
      Result:=AdjustTokenPrivileges(hToken,FALSE,tkp,0,nil,n);
      end;
    end;
  end;

{ ---------------------------------------------------------------- }
(* System herunterfahren *)
function ExitFromWindows (Prompt : string; EwFlags,RsFlags : longword) : boolean;
begin
  Result:=false;
  if (length(Prompt)>0) and (MessageDlg(Prompt,mtConfirmation,[mbYes,mbNo],0)=mrNo) then exit;
  Result:=EnableShutdownPrivilege and ExitWindowsEx (EwFlags,RsFlags);
  end;

function ShutDownWindows (Prompt : string; Restart : boolean; RsFlags : longword) : boolean;
var
  vi     : TOSVersionInfo;
  n      : dword;
  hToken : THandle;
  tkp    : TTokenPrivileges;
begin
  Result:=false;
  if (length(Prompt)>0) and (MessageDlg(Prompt,mtConfirmation,[mbYes,mbNo],0)=mrNo) then exit;
  Result:=EnableShutdownPrivilege and InitiateSystemShutdownEx(nil,nil,0,true,Restart,RsFlags);
  end;

function HasS3Support : boolean;
// Check if computer supports S3 - disabled on new computers using S0 (modern standby)
var
  pwc : TSystemPowerCapabilities;
  n   : int64;
begin
  if GetPowerCapabilities(pwc) then Result:=true //pwc.SystemS3
  else Result:=true;
  end;

function SuspendWindows (Hibernate : boolean) : boolean;
// Hibernate = true  : S4 (Ruhezustand)
//           = false : S3 (Standby) if available see "powercg /a"
// Remark: S0 (modern standby) is not supported
begin
  Result:=EnableShutdownPrivilege;
  if Result then begin
    if Hibernate or HasS3Support then SetSuspendState(Hibernate,false,false)
    else PostMessage(HWND_BROADCAST,WM_SYSCOMMAND,SC_MONITORPOWER,2);  // monitor off
    end;
//  Result:=EnableShutdownPrivilege and SetSystemPowerState(not Hibernate,false);
  end;

//-----------------------------------------------------------------------------
// Tastaturpuffer löschen
function ClearKeyboardBuffer : Integer;
var
   Msg: TMsg;
begin
  Result := 0;
  while PeekMessage(Msg,0,WM_KEYFIRST,WM_KEYLAST,PM_REMOVE) do inc(Result);
  end;

{------------------------------------------------------------------}
// Liste der auf dem System vorhandenen Codepages erstellen
var
  CodePageList : TStringList;

function CpEnumProc (CodePage : PChar) : Cardinal; stdcall;
var
   CpInfoEx : TCPInfoEx;
   s : string;
   Cp : cardinal;
begin
  Cp := StrToIntDef(CodePage,0);
  if IsValidCodePage(Cp) then begin
    GetCPInfoEx(Cp, 0, CpInfoEx);
    s:=CpInfoEx.CodePageName;
    ReadNxtStr(s,' ');
    s:=Trim(s);
    s:=RemChar(CutChar(s,')'),'(');
    CodePageList.AddObject(Format('%s - (%u)',[s,CpInfoEx.Codepage]),TObject(Cp));
    end;
  Result := 1;
  end;

function GetCodePageList (AList : TStrings; Default : string) : boolean;
begin
  CodePageList:=TStringList.Create;
  CodePageList.Sorted:=true;
  if length(Default)>0 then CodePageList.AddObject(Space+Default,nil);
  Result:=false;
  try
    Result:=EnumSystemCodePages(@CpEnumProc,CP_SUPPORTED);
    if Result then AList.Assign(CodePageList);
  finally
    CodePageList.Free;
    end;
  end;

function LangEnumProc (lpName : PChar; lParam : long_ptr) : boolean; stdcall;
begin
  CodePageList.AddObject(Format('%s - (%u)',[lpName,lParam]), TObject(lParam));
  Result:=true;
  end;

function GetLanguageList (AList : TStrings) : boolean;
begin
  CodePageList:=TStringList.Create;
  CodePageList.Sorted:=true;
  Result:=false;
  try
    Result:=EnumUILanguages(@LangEnumProc,MUI_LANGUAGE_NAME,0);
    if Result then AList.Assign(CodePageList);
  finally
    CodePageList.Free;
    end;
  end;

end.
