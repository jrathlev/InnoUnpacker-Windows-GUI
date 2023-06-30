(* DosPanel - Windows GUI for DOSBox
   =================================
   Convert Unicode text to other code page
   ---------------------------------------

   © J. Rathlev, D-24222 Schwentinental (info(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   J. Rathlev, Sep. 2014
   last modified: Dec. 2017
   *)

unit TxtConvertDlg;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  Vcl.ExtCtrls;

type
  TTxtConvertDialog = class(TForm)
    btbCancel: TBitBtn;
    btbConvert: TBitBtn;
    rgCodePages: TRadioGroup;
    edTextfile: TLabeledEdit;
    btTextFile: TSpeedButton;
    SaveDialog: TSaveDialog;
    OpenDialog: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure btTextFileClick(Sender: TObject);
    procedure btbConvertClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FRootPath,
    LastFile : string;
    procedure Convert(const InFile,OutFile : string; CodePage : integer);
  public
    { Public-Deklarationen }
    procedure Execute (const ARootPath : string; ACodePage : integer);
  end;

var
  TxtConvertDialog: TTxtConvertDialog;

implementation

{$R *.dfm}

uses GnuGetText, WinUtils, MsgDialogs, NumberUtils, PathUtils, Settings;

procedure TTxtConvertDialog.FormCreate(Sender: TObject);
var
  i : integer;
begin
  TranslateComponent(self);
  with rgCodePages do begin
    Items.Clear;
    for i:=0 to High(CodePages) do Items.Add(CodePages[i].CodePageName);
    end;
  LastFile:='';
  end;

procedure TTxtConvertDialog.btTextFileClick(Sender: TObject);
begin
  with OpenDialog do begin
    if length(LastFile)>0 then InitialDir:=ExtractFilePath(LastFile)
    else InitialDir:=FRootPath;
    DefaultExt:='txt';
    Filename:='';
    Filter:=_('All')+'|*.*';
    Title:=_('Select text file to be converted');
    if Execute then edTextfile.Text:=Filename;
    end;
  end;

procedure TTxtConvertDialog.Convert(const InFile,OutFile : string; CodePage : integer);
var
  sl      : TStringList;
begin
  sl:=TStringList.Create;
  with sl do begin
    LoadFromFile(InFile);
    SaveToFile(OutFile,TEncoding.GetEncoding(CodePage));
    Free;
    end;
  end;

procedure TTxtConvertDialog.btbConvertClick(Sender: TObject);
var
  s : string;
begin
  if FileExists(edTextfile.Text) then with SaveDialog do begin
    LastFile:=edTextfile.Text;
    InitialDir:=ExtractFilePath(edTextfile.Text);
    Filename:=InsertNameSuffix(ExtractFilename(edTextfile.Text),
                 '-'+ZStrint(CodePages[rgCodePages.ItemIndex].CodePageNr,3));
    DefaultExt:='txt';
    Filter:=_('All')+'|*.*';
    Title:=_('Save converted text as');
    if Execute then begin
      Convert(edTextfile.Text,Filename,CodePages[rgCodePages.ItemIndex].CodePageNr);
      end;
    end
  else ErrorDialog(Format(_('File not found: %s'),[edTextfile.Text]));
  end;

procedure TTxtConvertDialog.Execute (const ARootPath : string; ACodePage : integer);
var
  i : integer;
begin
  FRootPath:=ARootPath;
  edTextFile.Text:='';
  for i:=0 to High(CodePages) do if CodePages[i].CodePageNr=ACodePage then Break;
  if i>High(CodePages) then i:=0;
  rgCodePages.ItemIndex:=i;
  ShowModal;
  end;

end.
