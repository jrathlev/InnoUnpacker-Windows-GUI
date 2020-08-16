(* Delphi Dialog
   Texteingabe mit opt. Hilfe durch Zeichentabelle für Sonderzeichen
   =================================================================

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Vers. 1 - Sep. 2002 
   *)

unit InpText;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls;

type
  TInputTextDialog = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    Descriptor: TLabel;
    CharTabBtn: TBitBtn;
    TextFeld: TComboBox;
    procedure CharTabBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TextFeldKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    FIniName,FIniSection : string;
  public
    { Public declarations }
    HistCount : integer;
  (* History-Liste laden (optional) *)
  procedure LoadFromIni(AIniName,AIniSection : string);
  function Execute (Pos        : TPoint;
                    Titel,Desc : string;
                    ShowTable  : boolean;
                    AFontName  : TFontName;
                    var AText  : string) : boolean;
  end;

(* Text eingeben, Ergebnis: "true" bei "ok" *)
function InputText(Pos        : TPoint;
                   Titel,Desc : string;
                   ShowTable  : boolean;
                   AFontName  : TFontName;
                   AHistory   : TStrings;
                   ASortHist  : boolean;
                   AHistCount : integer;
                   var AText  : string) : boolean; overload;

function InputText(Pos        : TPoint;
                   Titel,Desc : string;
                   ShowTable  : boolean;
                   var AText  : string) : boolean; overload;

var
  InputTextDialog: TInputTextDialog;

implementation

{$R *.DFM}

uses CharTableDlg, System.IniFiles, WinUtils, GnuGetText;

const
  iniHistory = 'History';

{ ------------------------------------------------------------------- }
procedure TInputTextDialog.LoadFromIni(AIniName,AIniSection : string);
var
  IniFile : TIniFile;
begin
  FIniName:=AIniName; FIniSection:=AIniSection;
  if (length(FIniName)>0) and (length(FIniSection)>0) then begin
    IniFile:=TIniFile.Create(FIniName);
    LoadHistory(IniFile,FIniSection,iniHistory,TextFeld.Items);
    IniFile.Free;
    with TextFeld do begin
      if Items.Count=0 then Style:=csSimple else Style:=csDropDown;
      AutoComplete:=true;
      end;
    end;
  end;

procedure TInputTextDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent (self,'dialogs');
  FIniName:=''; FIniSection:='';
  HistCount:=MaxHist;
  TextFeld.Style:=csSimple;
  end;

procedure TInputTextDialog.FormDestroy(Sender: TObject);
var
  IniFile : TIniFile;
begin
  if (length(FIniName)>0) and (length(FIniSection)>0) then begin
    IniFile:=TIniFile.Create(FIniName);
    SaveHistory(IniFile,FIniSection,iniHistory,true,TextFeld.Items);
    IniFile.Free;
    end;
  end;

{ ------------------------------------------------------------------- }
procedure TInputTextDialog.TextFeldKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key=#$1B then Modalresult:=mrCancel
  else if Key=#$D then Modalresult:=mrOK;
  end;

{ ------------------------------------------------------------------- }
procedure TInputTextDialog.CharTabBtnClick(Sender: TObject);
var
  c : char;
begin
  with TextFeld do begin
    c:=CharFromTable (TopRightPos(CharTabBtn),Font.Name);
    if c>#0 then Text:=Text+c;
    SetFocus; SelStart:=SelLength;
    end;
  end;

{ ------------------------------------------------------------------- }
function TInputTextDialog.Execute (Pos        : TPoint;
                                   Titel,Desc : string;
                                   ShowTable  : boolean;
                                   AFontName  : TFontName;
                                   var AText  : string) : boolean;
var
  w : integer;
begin
  with Pos do begin
    if (Y < 0) or (X < 0) then Position:=poScreenCenter
    else begin
      Position:=poDesigned;
      if X<0 then X:=Left;
      if Y<0 then Y:=Top;
      CheckScreenBounds(Screen,x,y,Width,Height);
      Left:=x; Top:=y;
      end;
    end;
  if length(Titel)>0 then Caption:=Titel;
  Descriptor.Caption:=Desc;
  CharTabBtn.Visible:=ShowTable;
  ActiveControl:=TextFeld;
  with TextFeld do begin
    if length(AFontName)>0 then Font.Name:=AFontName;
    if Items.Count=0 then Style:=csSimple else Style:=csDropDown;
    AutoComplete:=true;
    Text:=AText;
    end;
  w:=TextFeld.Canvas.TextWidth(AText)+20;
  if w>311 then ClientWidth:=w else ClientWidth:=311;
  if ShowModal=mrOK then begin
    AText:=TextFeld.Text;
    AddToHistory(TextFeld.Items,AText,HistCount);
    Result:=true;
    end
  else Result:=false;
  end;

{ ------------------------------------------------------------------- }
(* Txt eingeben, Ergebnis: "true" bei "ok" *)
function InputText(Pos        : TPoint;
                   Titel,Desc : string;
                   ShowTable  : boolean;
                   AFontName  : TFontName;
                   AHistory   : TStrings;
                   ASortHist  : boolean;
                   AHistCount : integer;
                   var AText  : string) : boolean;
begin
  if not assigned(InputTextDialog) then InputTextDialog:=TInputTextDialog.Create(Application);
  with InputTextDialog do begin
    if assigned(AHistory) then with TextFeld do begin
      Clear; Sorted:=ASortHist;
      Items.Assign(AHistory);
      HistCount:=AHistCount;
      end;
    Result:=Execute(Pos,Titel,Desc,ShowTable,AFontName,AText);
    if Result and (AHistCount>0) and assigned(AHistory) then AHistory.Assign(TextFeld.Items);
    Release;
    end;
  InputTextDialog:=nil;
  end;

function InputText(Pos        : TPoint;
                   Titel,Desc : string;
                   ShowTable  : boolean;
                   var AText  : string) : boolean; overload;
begin
  Result:=InputText(Pos,Titel,Desc,ShowTable,'',nil,false,0,AText);
  end;

end.
