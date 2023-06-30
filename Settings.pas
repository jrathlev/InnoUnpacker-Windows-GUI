(* DosPanel - Windows GUI for DOSBox
   =================================
   Global settings
   ---------------

   © J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   J. Rathlev, Dec. 2011
   last modified: Dec. 2017
   *)

unit Settings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  Vcl.ExtCtrls;

const
  DosBoxName = 'DOSBox.exe';

type
  TCodePageInfo = record
    CodePageName : string;
    CodePageNr   : integer;
    end;

const
  CodePages : array [0..8] of TCodePageInfo = (
//    (CodePageName : ''; CodePageNr : ),
    (CodePageName : 'US (437)'; CodePageNr : 437),
    (CodePageName : 'Latin-1 (850)'; CodePageNr : 850),
    (CodePageName : 'Latin-2 (852)'; CodePageNr : 852),
    (CodePageName : 'Latin-1 + Euro (858)'; CodePageNr : 858),
    (CodePageName : 'Portuguese (860)'; CodePageNr : 860),
    (CodePageName : 'Nordic (865)'; CodePageNr : 865),
    (CodePageName : 'Cyrillic (866)'; CodePageNr : 866),
    (CodePageName : 'Greek (869)'; CodePageNr : 869),
    (CodePageName : 'Baltic (775)'; CodePageNr : 775)
    );

type
  TBasicSettings = record
    DosBoxPath,
    RootPath,
    ConfFile,
    LangFile,
    MapperFile,
    KeyLayout     : string;
    Codepage      : integer;
    AutoStart,
    HideCon       : boolean;
    end;

  TDosBoxSetDialog = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    edPath: TLabeledEdit;
    btPath: TSpeedButton;
    cbConsole: TCheckBox;
    edRootPath: TLabeledEdit;
    btRootPath: TSpeedButton;
    edConfFile: TLabeledEdit;
    edLangFile: TLabeledEdit;
    btConf: TSpeedButton;
    btLang: TSpeedButton;
    OpenDialog: TOpenDialog;
    edMapperFile: TLabeledEdit;
    btMapper: TSpeedButton;
    gbKeyLayout: TGroupBox;
    rbAutoKey: TRadioButton;
    rbKeyLayout: TRadioButton;
    cbKeyLayout: TComboBox;
    gbCodePage: TGroupBox;
    cbCodePages: TComboBox;
    gbDosBox: TGroupBox;
    gbDosPanel: TGroupBox;
    cbAutoStart: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btPathClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btRootPathClick(Sender: TObject);
    procedure btConfClick(Sender: TObject);
    procedure btLangClick(Sender: TObject);
    procedure btMapperClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    function Execute (var ASettings : TBasicSettings) : boolean;
  end;

function GetKeyboardLayoutDosName : string;
function GetCodePage : integer;

var
  DosBoxSetDialog: TDosBoxSetDialog;

implementation

{$R *.dfm}

uses DosPanelMain, GnuGetText, PathUtils, WinUtils, MsgDialogs, ShellDirDlg;

{ ------------------------------------------------------------------- }
function GetKeyboardLayoutDosName : string;
var
  kbId : cardinal;
begin
  kbId:=GetKeyboardLayout(0) and $3FF;
  case kbId of
  $02 : Result:='BG'; //Bulgarien - 866
  $05 : Result:='CZ243'; //Tschechien - 852
  $0c : Result:='FR'; //Frankreich - 858
  $08 : Result:='GK'; //Griechenland - 869
  $07 : Result:='GR'; //Deutschland/Österreich - 858
  $1a : Result:='HR'; //Kroatien - 852
  $0e : Result:='HU'; //Ungarn - 852
  $10 : Result:='IT'; //Italien - 858
  $13 : Result:='NL'; //Niederlande - 858
  $14 : Result:='NO'; //Norwegen - 865
  $15 : Result:='PL'; //Polen - 852
  $19 : Result:='RU'; //Russland - 866
  $1b : Result:='SK'; //Slowakei - 852
  $0a : Result:='SP'; //Spanien - 858
  $0b : Result:='SU'; //Finnland - 858
  $1d : Result:='SV'; //Schweden - 850
  else Result:='';
    end;
  end;

function GetCodePage : integer;
var
  kbId : cardinal;
begin
  kbId:=GetKeyboardLayout(0) and $3FF;
  case kbId of
  $02 : Result:=866; //Bulgarien - 866
  $05 : Result:=852; //Tschechien - 852
  $0c : Result:=858; //Frankreich - 858
  $08 : Result:=869; //Griechenland - 869
  $07 : Result:=858; //Deutschland/Österreich - 858
  $1a : Result:=852; //Kroatien - 852
  $0e : Result:=852; //Ungarn - 852
  $10 : Result:=858; //Italien - 858
  $13 : Result:=858; //Niederlande - 858
  $14 : Result:=865; //Norwegen - 865
  $15 : Result:=852; //Polen - 852
  $19 : Result:=866; //Russland - 866
  $1b : Result:=852; //Slowakei - 852
  $0a : Result:=858; //Spanien - 858
  $0b : Result:=858; //Finnland - 858
  $1d : Result:=850; //Schweden - 850
  else Result:=437;
    end;
  end;

procedure TDosBoxSetDialog.FormCreate(Sender: TObject);
var
  i : integer;
begin
  TranslateComponent(self);
  with cbCodePages do begin
    Items.Clear;
    for i:=0 to High(CodePages) do Items.Add(CodePages[i].CodePageName);
    ItemIndex:=0;
    end;
  end;

procedure TDosBoxSetDialog.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if (Modalresult=mrOK) then begin
    if not FileExists(SetDirName(edPath.Text)+DosBoxName) then
    CanClose:=not ConfirmDialog(_('DosBox.exe not found in the specified directory!'+
      sLineBreak+'Adjust setting?'));
    if not DirectoryExists(SetDirName(edRootPath.Text)) then
    CanClose:=not ConfirmDialog(_('Root path to DOS applications not found!'+
      sLineBreak+'Adjust setting?'));
    end;
  end;

procedure TDosBoxSetDialog.btConfClick(Sender: TObject);
begin
  with OpenDialog do begin
    if length(edConfFile.Text)>0 then InitialDir:=ExtractFilePath(edConfFile.Text)
    else InitialDir:=frmMain.LocPath;
    DefaultExt:='conf';
    Filename:='';
    Filter:=_('DOSBox configuration files')+'|*.conf|'+_('All')+'|*.*';
    Title:=_('Select basic configuration file');
    if Execute then edConfFile.Text:=Filename;
    end;
  end;

procedure TDosBoxSetDialog.btLangClick(Sender: TObject);
begin
  with OpenDialog do begin
    if length(edLangFile.Text)>0 then InitialDir:=ExtractFilePath(edLangFile.Text)
    else InitialDir:=frmMain.LocPath;
    DefaultExt:='lang';
    Filename:='';
    Filter:=_('DOSBox language files')+'|*.lang|'+_('All')+'|*.*';
    Title:=_('Select language file');
    if Execute then edLangFile.Text:=Filename;
    end;
  end;

procedure TDosBoxSetDialog.btMapperClick(Sender: TObject);
begin
  with OpenDialog do begin
    if length(edMapperFile.Text)>0 then InitialDir:=ExtractFilePath(edMapperFile.Text)
    else InitialDir:=frmMain.LocPath;
    DefaultExt:='map';
    Filename:='';
    Filter:=_('DOSBox key mapper files')+'|*.map|'+_('All')+'|*.*';
    Title:=_('Select key mapper file');
    if Execute then edMapperFile.Text:=Filename;
    end;
  end;

procedure TDosBoxSetDialog.btPathClick(Sender: TObject);
var
  s : string;
begin
  s:=edPath.Text;
  if length(s)=0 then s:=frmMain.ProgPath;
  if ShellDirDialog.Execute (_('Select DOSBox program directory'),
      false,true,false,frmMain.ProgPath,s) then edPath.Text:=s;
  end;

procedure TDosBoxSetDialog.btRootPathClick(Sender: TObject);
var
  s : string;
begin
  s:=edRootPath.Text;
  if length(s)= 0 then s:=frmMain.UserPath;
  if ShellDirDialog.Execute (_('Select root path to DOS applications'),
      false,true,false,frmMain.ProgPath,s) then edRootPath.Text:=s;
  end;

function TDosBoxSetDialog.Execute (var ASettings : TBasicSettings) : boolean;
var
  i : integer;
begin
  with ASettings do begin
    edRootPath.Text:=RootPath;
    edPath.Text:=DosBoxPath;
    edConfFile.Text:=ConfFile;
    edLangFile.Text:=LangFile;
    edMapperFile.Text:=MapperFile;
    if length(KeyLayout)=0 then begin
      rbAutoKey.Checked:=true;
      cbKeyLayout.Text:=GetKeyboardLayoutDosName;
      end
    else begin
      rbKeyLayout.Checked:=true;
      cbKeyLayout.Text:=KeyLayout;
      end;
    for i:=0 to High(CodePages) do if CodePages[i].CodePageNr=CodePage then Break;
    if i>High(CodePages) then i:=0;
    cbCodePages.ItemIndex:=i;
    cbConsole.Checked:=HideCon;
    cbAutoStart.Checked:=AutoStart;
    Result:=ShowModal=mrOK;
    if Result then begin
      RootPath:=edRootPath.Text;
      DosBoxPath:=edPath.Text;
      ConfFile:=edConfFile.Text;
      LangFile:=edLangFile.Text;
      MapperFile:=edMapperFile.Text;
      if rbAutoKey.Checked then KeyLayout:=''
      else KeyLayout:=cbKeyLayout.Text;
      CodePage:=CodePages[cbCodePages.ItemIndex].CodePageNr;
      HideCon:=cbConsole.Checked;
      AutoStart:=cbAutoStart.Checked;
      end;
    end;
  end;
end.
