(* Delphi Dialog
   Dialog to enter a string
   ========================

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Vers. 1 - Sep. 2002 
   last modified: July 2025
   *)

unit InputString;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls;

type
  TInputStringDialog = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    CharTabBtn: TBitBtn;
    Descriptor: TStaticText;
    TextFeld: TEdit;
    SuffixText: TStaticText;
    procedure FormCreate(Sender: TObject);
    procedure TextFeldKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    defWidth : integer;
  public
    { Public declarations }
{$IFDEF HDPI}   // scale glyphs and images for High DPI
    procedure AfterConstruction; override;
{$EndIf}
    function Execute (APos       : TPoint;
                      const Titel,Desc,Suffix : string;
                      var AText  : string) : boolean;
  end;

(* Text eingeben, Ergebnis: "true" bei "ok" *)
function InputText(APos       : TPoint;
                   const Titel,Desc,Suffix : string;
                   var AText  : string) : boolean; overload;

function InputText(APos       : TPoint;
                   const Titel,Desc : string;
                   var AText  : string) : boolean; overload;

var
  InputStringDialog: TInputStringDialog;

implementation

{$R *.DFM}

uses System.IniFiles, GnuGetText, WinUtils
  {$IFDEF ACCESSIBLE}, ShowMessageDlg{$ENDIF};

{ ------------------------------------------------------------------- }
procedure TInputStringDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent (self,'dialogs');
  defWidth:=ClientWidth;
  end;

{$IFDEF HDPI}   // scale glyphs and images for High DPI
procedure TInputStringDialog.AfterConstruction;
begin
  inherited;
  if Application.Tag=0 then
    ScaleButtonGlyphs(self,PixelsPerInchOnDesign,Monitor.PixelsPerInch);
  end;
{$EndIf}

procedure TInputStringDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
{$IFDEF ACCESSIBLE}
  if (Key=VK_F11) then begin
    with ActiveControl do if length(Hint)>0 then ShowHintInfo(Hint);
    end;
{$ENDIF}
  end;

procedure TInputStringDialog.FormShow(Sender: TObject);
begin
  FitToScreen(Screen,self);
  end;

{ ------------------------------------------------------------------- }
procedure TInputStringDialog.TextFeldKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key=#$1B then Modalresult:=mrCancel
  else if Key=#$D then Modalresult:=mrOK;
  end;

{ ------------------------------------------------------------------- }
function TInputStringDialog.Execute (APos       : TPoint;
                                     const Titel,Desc,Suffix : string;
                                     var AText  : string) : boolean;
var
  w : integer;
begin
  AdjustFormPosition(Screen,self,APos);
  if length(Titel)>0 then Caption:=Titel;
  Descriptor.Caption:=Desc;
  SuffixText.Caption:=Suffix;
  if length(Suffix)>=0 then
  ActiveControl:=TextFeld;
  with TextFeld do begin
    Text:=AText;
    Hint:=Desc;
    end;
  w:=GetTextWidth(AText,TextFeld.Font)+PixelScale(20,self);
  if w>defWidth then ClientWidth:=w else ClientWidth:=defWidth;
  if ShowModal=mrOK then begin
    AText:=TextFeld.Text;
    Result:=true;
    end
  else Result:=false;
  end;

{ ------------------------------------------------------------------- }
(* Txt eingeben, Ergebnis: "true" bei "ok" *)
function InputText(APos       : TPoint;
                   const Titel,Desc,Suffix : string;
                   var AText  : string) : boolean;
begin
  if not assigned(InputStringDialog) then InputStringDialog:=TInputStringDialog.Create(Application);
  Result:=InputStringDialog.Execute(APos,Titel,Desc,Suffix,AText);
  FreeAndNil(InputStringDialog);
  end;

function InputText (APos       : TPoint;
                    const Titel,Desc : string;
                    var AText  : string) : boolean;
begin
  Result:=InputText(APos,Titel,Desc,'',AText);
  end;

end.
