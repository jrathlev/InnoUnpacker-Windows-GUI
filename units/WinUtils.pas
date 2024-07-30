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
   last modified: July 2023
   *)

unit WinUtils;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, System.Types, Vcl.Graphics,
  Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Forms, Vcl.ComCtrls, Vcl.Printers,
  System.IniFiles, Vcl.Dialogs, Vcl.Buttons;

const
  CenterPos : TPoint = (X : -1; Y : -1);
  DesignPos : TPoint = (X : 0; Y : -1);
  defMaxHist : integer = 50;

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
function MaxTextWidth(const Text : string; Canvas : TCanvas) : integer;

// calculate text width for given font
function GetTextWidth(const Text : string; AFont : TFont) : integer;
function GetMaxTextWidth(sl : TStrings; AFont : TFont) : integer; overload;
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
// History list management
procedure LoadHistory (IniFile : TCustomIniFile; const Section,Ident : string;
                       History : TStrings; MaxCount : integer; CvQuote : boolean = false); overload;
procedure LoadHistory (IniFile : TCustomIniFile; const Section,Ident : string;
                       History : TStrings; CvQuote : boolean = false); overload;
procedure LoadHistory (IniFile : TCustomIniFile; const Section : string;
                       History : TStrings; CvQuote : boolean = false); overload;
procedure LoadHistory (const IniName,Section,Ident : string;
                       History : TStrings; MaxCount : integer; CvQuote : boolean = false); overload;
procedure LoadHistory (const IniName,Section,Ident : string;
                       History : TStrings; CvQuote : boolean = false); overload;
procedure LoadHistory (const IniName,Section : string;
                       Combo : TComboBox; MaxHist : integer = 0; CvQuote : boolean = false); overload;
procedure LoadHistory (IniFile : TCustomIniFile; const Section : string;
                       Combo : TComboBox; MaxHist : integer = 0; CvQuote : boolean = false); overload;

procedure SaveHistory (IniFile : TCustomIniFile; const Section,Ident : string;
                       Erase : boolean; History : TStrings; MaxCount : integer; CvQuote : boolean = false); overload;
procedure SaveHistory (IniFile : TCustomIniFile; const Section,Ident : string;
                       Erase : boolean; History : TStrings; CvQuote : boolean = false); overload;
procedure SaveHistory (IniFile : TCustomIniFile; const Section : string;
                       Erase : boolean; History : TStrings; CvQuote : boolean = false); overload;
procedure SaveHistory (IniFile : TCustomIniFile; const Section : string;
                       History : TStrings; CvQuote : boolean = false); overload;
procedure SaveHistory (const IniName,Section,Ident : string;
                       Erase : boolean; History : TStrings; MaxCount : integer; CvQuote : boolean = false); overload;
procedure SaveHistory (const IniName,Section,Ident : string;
                       Erase : boolean; History : TStrings; CvQuote : boolean = false); overload;
procedure SaveHistory (IniFile : TCustomIniFile; const Section : string; Erase : boolean;
                       Combo : TComboBox; MaxHist : integer = 0; CvQuote : boolean = false); overload;
procedure SaveHistory (IniFile : TCustomIniFile; const Section : string;
                       Combo : TComboBox; MaxHist : integer = 0; CvQuote : boolean = false); overload;
procedure SaveHistory (const IniName,Section : string; Erase : boolean;
                       Combo : TComboBox; MaxHist : integer = 0; CvQuote : boolean = false); overload;

procedure AddToHistory (History : TStrings; const hs : string; MaxCount : integer); overload;
procedure AddToHistory (History : TStrings; const hs : string); overload;
procedure AddToHistory (Combo : TComboBox; const hs : string); overload;
procedure AddToHistory (Combo : TComboBox); overload;
procedure RemoveFromHistory (History : TStrings; const hs : string);

{ ---------------------------------------------------------------- }
// Entferne alle Objekte einer String-Liste oder einer ListView-Liste aus dem Speicher
procedure FreeListObjects (Liste : TStrings);
procedure FreeListViewData (Liste : TListItems);

{ ---------------------------------------------------------------- }
// Ausgewählten Eintrag in einer ListBox
function GetSelectedItem (ListBox : TListBox) : string;

{ ---------------------------------------------------------------- }
// Listview-Index aus Caption ermitteln (wie IndexOf bei TListBox)
function GetListViewIndex (lv : TListView; const ACaption : string): integer;

// Subitem-Index aus der Mausposition ermitteln (nur vsReport)
function GetColumnIndexAt (ListView : TListView; Pos : integer) : integer;

// TopItem auf Index setzen (nur vsReport)
procedure SetListViewTopItem (lv : TListView; AIndex : integer; Select : boolean);

{ ---------------------------------------------------------------- }
(* System herunterfahren *)
function ExitFromWindows (Prompt : string; EwFlags,RsFlags : longword) : boolean;
function ShutDownWindows (Prompt : string; Restart : boolean; RsFlags : longword) : boolean;

{ ---------------------------------------------------------------- }
// Tastaturpuffer löschen
function ClearKeyboardBuffer : Integer;

{ ---------------------------------------------------------------- }
// Liste der auf dem System vorhandenen Codepages erstellen
function GetCodePageList (sl : TStrings; Default : string = '') : boolean;

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
  with AForm,APos do begin
    if (Y < 0) or (X < 0) then Position:=poMainFormCenter // poScreenCenter
    else begin
      Position:=poDesigned;
      if X<0 then X:=Left;
      if Y<0 then Y:=Top;
      if AtBottom then Y:=Y-Height;
      CheckScreenBounds(AScreen,x,y,Width,Height);  // DefaultMonitor = dmDesktop
      Left:=x; Top:=y;
      end;
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

function GetMaxTextWidth(sl : TStrings; AFont : TFont) : integer;
var
  i : integer;
  bm  : TBitmap;
begin
  Result:=0;
  bm:=TBitmap.Create;                      // prepare temp. canvas
  bm.Canvas.Font.Assign(AFont);
  with sl do for i:=0 to Count-1 do begin
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
  bm:=TBitmap.Create;
  if AControl is TBitBtn then begin
    gl:=(AControl as TBitBtn).Glyph; bm.Assign(gl);
    end
// assign is required to work for 64-bit applications
  else if AControl is TSpeedButton then begin
    gl:=(AControl as TSpeedButton).Glyph; bm.Assign(gl);
    end
  else Exit;
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
    bm.Free; bms.Free;
    end;
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
  with AControl do for i:=0 to ControlCount-1 do begin
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

{ --------------------------------------------------------------- }
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

{ ------------------------------------------------------------------- }
// History list management
const
  iniHist = 'History';

procedure LoadHistory (IniFile : TCustomIniFile; const Section,Ident : string;
                       History : TStrings; MaxCount : integer; CvQuote : boolean);
var
  i : integer;
  s,si : string;
begin
  with IniFile do begin
    if SectionExists(Section) then begin
      if length(Ident)=0 then si:=iniHist else si:=Ident;
      History.Clear;
      for i:=0 to MaxCount-1 do begin
        s:=ReadString(Section,si+IntToStr(i),'');
        if length(s)>0 then begin
          if CvQuote then s:=ReplChars(s,'#',Quote);
          History.Add(s);
          end;
        end;
      end;
    end;
  end;

procedure LoadHistory (IniFile : TCustomIniFile; const Section,Ident : string;
                       History : TStrings; CvQuote : boolean);
begin
  LoadHistory(IniFile,Section,Ident,History,defMaxHist,CvQuote);
  end;

procedure LoadHistory (IniFile : TCustomIniFile; const Section : string;
                       History : TStrings; CvQuote : boolean);
begin
  LoadHistory(IniFile,Section,'',History,defMaxHist,CvQuote);
  end;

procedure LoadHistory (const IniName,Section,Ident : string;
                       History : TStrings; MaxCount : integer; CvQuote : boolean);
var
  IniFile : TMemIniFile;
begin
  IniFile:=TMemIniFile.Create(IniName);
  LoadHistory(IniFile,Section,Ident,History,MaxCount,CvQuote);
  IniFile.Free;
  end;

procedure LoadHistory (const IniName,Section,Ident : string;
                       History : TStrings; CvQuote : boolean);
begin
  LoadHistory(IniName,Section,Ident,History,defMaxHist,CvQuote);
  end;

procedure LoadHistory (IniFile : TCustomIniFile; const Section : string;
                       Combo : TComboBox; MaxHist : integer = 0; CvQuote : boolean = false);
var
  n : integer;
begin
  with Combo do begin
    if MaxHist=0 then n:=DropDownCount else n:=MaxHist;
    LoadHistory(IniFile,Section,'',Items,n,CvQuote);
    if Items.Count>0 then ItemIndex:=0;
    if (Items.Count<=1) then Style:=csSimple else Style:=csDropDown;
    end;
  end;

procedure LoadHistory (const IniName,Section : string;
                       Combo : TComboBox; MaxHist : integer; CvQuote : boolean);
var
  n : integer;
begin
  with Combo do begin
    if MaxHist=0 then n:=DropDownCount else n:=MaxHist;
    LoadHistory(IniName,Section,'',Items,n,CvQuote);
    if Items.Count>0 then ItemIndex:=0;
    if (Items.Count<=1) then Style:=csSimple else Style:=csDropDown;
    end;
  end;

procedure SaveHistory (IniFile : TCustomIniFile; const Section,Ident : string;
                       Erase : boolean; History : TStrings; MaxCount : integer; CvQuote : boolean);
var
  i,n : integer;
  s,si : string;
begin
  with IniFile do begin
    if length(Ident)=0 then si:=iniHist else si:=Ident;
    if Erase then EraseSection (Section);
    with History do begin
      if Count>MaxCount then n:=MaxCount else n:=Count;
      for i:=0 to n-1 do begin
        s:=Strings[i];
        if CvQuote then s:=ReplChars(s,Quote,'#');
        WriteString(Section,si+IntToStr(i),s);
        end;
      end;
    end;
  end;

procedure SaveHistory (IniFile : TCustomIniFile; const Section,Ident : string;
                       Erase : boolean; History : TStrings; CvQuote : boolean);
begin
  SaveHistory(IniFile,Section,Ident,Erase,History,defMaxHist,CvQuote);
  end;

procedure SaveHistory (IniFile : TCustomIniFile; const Section : string;
                       Erase : boolean; History : TStrings; CvQuote : boolean);
begin
  SaveHistory(IniFile,Section,'',Erase,History,defMaxHist,CvQuote);
  end;

procedure SaveHistory (IniFile : TCustomIniFile; const Section : string;
                       History : TStrings; CvQuote : boolean = false);
begin
  SaveHistory(IniFile,Section,'',true,History,defMaxHist,CvQuote);
  end;

procedure SaveHistory (const IniName,Section,Ident : string;
                       Erase : boolean; History : TStrings; MaxCount : integer; CvQuote : boolean);
var
  IniFile : TMemIniFile;
begin
  IniFile:=TMemIniFile.Create(IniName);
  SaveHistory(IniFile,Section,Ident,Erase,History,defMaxHist,CvQuote);
  try
    IniFile.UpdateFile;
  finally
    IniFile.Free;
    end;
  end;

procedure SaveHistory (const IniName,Section,Ident : string;
                       Erase : boolean; History : TStrings; CvQuote : boolean);
begin
  SaveHistory(IniName,Section,Ident,Erase,History,defMaxHist,CvQuote);
  end;

procedure SaveHistory (IniFile : TCustomIniFile; const Section : string; Erase : boolean;
                       Combo : TComboBox; MaxHist : integer = 0; CvQuote : boolean = false);
var
  n : integer;
begin
  with Combo do begin
    if MaxHist=0 then n:=DropDownCount else n:=MaxHist;
    SaveHistory(IniFile,Section,'',Erase,Items,n,CvQuote);
    end;
  end;

procedure SaveHistory (IniFile : TCustomIniFile; const Section : string;
                       Combo : TComboBox; MaxHist : integer = 0; CvQuote : boolean = false); overload;
begin
  SaveHistory (IniFile,Section,true,Combo,MaxHist,CvQuote);
  end;

procedure SaveHistory (const IniName,Section : string; Erase : boolean;
                       Combo : TComboBox; MaxHist : integer; CvQuote : boolean);
var
  n : integer;
begin
  with Combo do begin
    if MaxHist=0 then n:=DropDownCount else n:=MaxHist;
    SaveHistory(IniName,Section,'',Erase,Items,n,CvQuote);
    end;
  end;

// move or add item "hs" to begin of history list
procedure AddToHistory (History : TStrings; const hs : string; MaxCount : integer);
var
  n : integer;
begin
  if length(hs)>0 then with History do begin
    n:=IndexOf(hs);
    if n<0 then begin
      if Count>=MaxCount then Delete (Count-1);
      Insert (0,hs);
      end
    else begin
      if n>0 then Move (n,0);
      Strings[0]:=hs;  // update string anyway, e.g. if case was changed
      end;
    end;
  end;

procedure AddToHistory (History : TStrings; const hs : string);
begin
  AddToHistory (History,hs,defMaxHist);
  end;

procedure AddToHistory (Combo : TComboBox; const hs : string);
begin
  with Combo do begin
    AddToHistory (Items,hs,DropDownCount);
    if Items.Count>0 then ItemIndex:=0;
    if (Items.Count<=1) then Style:=csSimple else Style:=csDropDown;
    end;
  end;

procedure AddToHistory (Combo : TComboBox);
begin
  AddToHistory(Combo,Combo.Text);
  end;

procedure RemoveFromHistory (History : TStrings; const hs : string);
var
  n : integer;
begin
  if length(hs)>0 then with History do begin
    n:=IndexOf(hs);
    if n>=0 then Delete(n);
    end;
  end;

//-----------------------------------------------------------------------------
procedure FreeListObjects (Liste : TStrings);
var
  i : integer;
begin
  with Liste do begin
    for i:=0 to Count-1 do if assigned(Objects[i]) then begin
      try Objects[i].Free; except end;
      Objects[i]:=nil;
      end;
    end;
  end;

procedure FreeListViewData (Liste : TListItems);
var
  i : integer;
begin
  with Liste do for i:=0 to Count-1 do with Item[i] do if Data<>nil then begin
    TObject(Data).Free; Data:=nil;
    end;
  end;

{ ---------------------------------------------------------------- }
// Ausgewählten Eintrag in einer ListBox
function GetSelectedItem (ListBox : TListBox) : string;
begin
  with ListBox do if ItemIndex>=0 then Result:=Items[ItemIndex]
  else Result:='';
  end;

//-----------------------------------------------------------------------------
// Listview-Index aus Caption ermitteln (wie IndexOf bei TListBox)
function GetListViewIndex (lv : TListView; const ACaption : string): integer;
begin
  with lv.Items do for Result:=0 to Count-1 do
    if AnsiSameText(Item[Result].Caption,ACaption) then Exit;
  Result:=-1;
  end;

// Subitem-Index aus der Mausposition ermitteln (nur vsReport)
function GetColumnIndexAt (ListView : TListView; Pos : integer) : integer;
var
  x : integer;
begin
  with ListView.Columns do begin
    x:=0;
    for Result:=0 to Count-1 do with Items[Result] do begin
      if (Pos>=x) and (Pos<x+Width) then Exit;
      x:=x+Width;
      end;
    end;
  Result:=-1;
  end;

// TopItem auf Index setzen (nur vsReport)
procedure SetListViewTopItem (lv : TListView; AIndex : integer; Select : boolean);
var
  n : integer;
begin
  with lv do if (AIndex>=0) and (Items.Count>0) and (AIndex<Items.Count) then begin
    with TopItem.DisplayRect(drBounds)do n:=Top-Bottom;
    Scroll(0,n*(TopItem.Index-AIndex));
    if Select then ItemIndex:=AIndex;
    end;
  end;

{ ---------------------------------------------------------------- }
(* System herunterfahren *)
function ExitFromWindows (Prompt : string; EwFlags,RsFlags : longword) : boolean;
var
  vi     : TOSVersionInfo;
  n      : dword;
  hToken : THandle;
  tkp    : TTokenPrivileges;
begin
  Result:=false;
  if (length(Prompt)>0) and (MessageDlg(Prompt,mtConfirmation,[mbYes,mbNo],0)=mrNo) then exit;
  vi.dwOSVersionInfoSize:=SizeOf(vi);
  GetVersionEx(vi);
  if vi.dwPlatformId>=VER_PLATFORM_WIN32_NT then begin // Windows NT
    // Get a token for this process.
    if OpenProcessToken(GetCurrentProcess,
          TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY,hToken) then begin
    // Get the LUID for the shutdown privilege.
      LookupPrivilegeValue(nil,SE_SHUTDOWN_NAME,tkp.Privileges[0].Luid);
      tkp.PrivilegeCount:=1;  // one privilege to set
      tkp.Privileges[0].Attributes:=SE_PRIVILEGE_ENABLED;
    // Get the shutdown privilege for this process.
      AdjustTokenPrivileges(hToken,FALSE,tkp,0,nil,n);
      end;
    end;
  Result:=ExitWindowsEx (EwFlags,RsFlags);
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
  vi.dwOSVersionInfoSize:=SizeOf(vi);
  GetVersionEx(vi);
  if vi.dwPlatformId>=VER_PLATFORM_WIN32_NT then begin // Windows NT
    // Get a token for this process.
    if OpenProcessToken(GetCurrentProcess,
          TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY,hToken) then begin
    // Get the LUID for the shutdown privilege.
      LookupPrivilegeValue(nil,SE_SHUTDOWN_NAME,tkp.Privileges[0].Luid);
      tkp.PrivilegeCount:=1;  // one privilege to set
      tkp.Privileges[0].Attributes:=SE_PRIVILEGE_ENABLED;
    // Get the shutdown privilege for this process.
      AdjustTokenPrivileges(hToken,FALSE,tkp,0,nil,n);
      end;
    end;
  Result:=InitiateSystemShutdownEx(nil,nil,0,true,Restart,RsFlags);
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

function CpEnumProc(CodePage : PChar) : Cardinal ; stdcall;
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
    CodePageList.AddObject(Format('%s - (%u)', [s,CpInfoEx.Codepage]), TObject(Cp));
    end;
  Result := 1;
  end;

function GetCodePageList (sl : TStrings; Default : string) : boolean;
begin
  CodePageList:=TStringList.Create;
  CodePageList.Sorted:=true;
  if length(Default)>0 then CodePageList.AddObject(Space+Default,nil);
  Result:=false;
  try
    Result:=EnumSystemCodePages(@CpEnumProc, CP_SUPPORTED);
    if Result then sl.Assign(CodePageList);
  finally
    CodePageList.Free;
    end;
  end;

end.
