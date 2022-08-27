(* Unpack Inoo setup files
   =======================
   GUI for "innounp.exe"
   see: https://sourceforge.net/projects/innounp/files/

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   J. Rathlev, Jan. 2008
   Vers. 1.6 (August 2020): added filter to extract single files
   last modified: October 2021
   *)

unit UnpackMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls;

const
  ProgName = 'InnoUnpacker';
  Vers = ' 1.8.3';
  CopRgt = '© 2014-2022 Dr. J. Rathlev, D-24222 Schwentinental';
  EmailAdr = 'kontakt(a)rathlev-home.de';

  defPipeSize = 64*1024;

type
  TMainForm = class(TForm)
    pnTop: TPanel;
    Label2: TLabel;
    cbFile: TComboBox;
    bbOptions: TBitBtn;
    InfoBtn: TSpeedButton;
    bbExit: TBitBtn;
    mmDos: TMemo;
    bbList: TBitBtn;
    OpenDialog: TOpenDialog;
    bbExtract: TBitBtn;
    bbVerify: TBitBtn;
    pnExtract: TPanel;
    cbDir: TComboBox;
    Label1: TLabel;
    bbStart: TBitBtn;
    cxDupl: TCheckBox;
    cxOverwrite: TCheckBox;
    cxStrip: TCheckBox;
    bbCopyResult: TBitBtn;
    Label3: TLabel;
    cbFilter: TComboBox;
    bbFilter: TBitBtn;
    bbDir: TBitBtn;
    bbFile: TBitBtn;
    cxEmbedded: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure InfoBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure bbOptionsClick(Sender: TObject);
    procedure bbExitClick(Sender: TObject);
    procedure bbListClick(Sender: TObject);
    procedure bbFileClick(Sender: TObject);
    procedure bbVerifyClick(Sender: TObject);
    procedure bbExtractClick(Sender: TObject);
    procedure bbDirClick(Sender: TObject);
    procedure cbDirExit(Sender: TObject);
    procedure bbStartClick(Sender: TObject);
    procedure cbFileCloseUp(Sender: TObject);
    procedure cbDirCloseUp(Sender: TObject);
    procedure bbCopyResultClick(Sender: TObject);
    procedure bbFilterClick(Sender: TObject);
    procedure cbFilterCloseUp(Sender: TObject);
  private
    { Private-Deklarationen }
    AppPath,UserPath,
    IniName,ProgPath,
    UnpProg          : string;
    function LoadUnpacker : boolean;
    procedure Execute (const Command,FileName,Filter,Comment : string);
    procedure WMDROPFILES (var Msg: TMessage); message WM_DROPFILES;
  public
    { Public-Deklarationen }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses System.IniFiles, System.StrUtils, Winapi.ShellApi, GnuGetText, WinUtils, MsgDialogs,
  FileUtils, InitProg, StringUtils, WinApiUtils, ShellDirDlg, SelectFromListDlg;

const
  mList = 20;
  IniExt = 'ini';
  InnoUnp = 'innounp.exe';

  (* INI-Sektionen *)
  CfGSekt = 'Config';
  FileSekt = 'Files';
  DirSekt  = 'Directories';
  FilterSekt = 'Filter';

  (* INI-Variablen *)
  iniLeft = 'Left';
  iniTop  = 'Top';
  iniWdt  = 'Width';
  iniHgt  = 'Height';
  iniUnp = 'Unpacker';
  iniFName = 'Name';
//  iniPpSz = 'BufferSize';

procedure TMainForm.FormCreate(Sender: TObject);
var
  i : integer;
begin
  TranslateComponent(self);
  DragAcceptFiles(MainForm.Handle, true);
  InitPaths(AppPath,UserPath,ProgPath);
  IniName:=Erweiter(AppPath,PrgName,IniExt);
  with TUnicodeIniFile.CreateForRead(IniName) do begin
    Left:=ReadInteger(CfgSekt,iniLeft,Left);
    Top:=ReadInteger(CfgSekt,iniTop,Top);
    ClientWidth:=ReadInteger(CfgSekt,iniWdt,ClientWidth);
    ClientHeight:=ReadInteger(CfgSekt,iniHgt,ClientHeight);
    UnpProg:=ReadString(CfgSekt,iniUnp,'');
    Free;
    end;
  LoadHistory(Ininame,FilterSekt,'',cbFilter.Items,mList);
  with cbFilter do begin
    if Items.Count=0 then AddItem('*.*',nil);
    ItemIndex:=0;
    end;
  LoadHistory(Ininame,FileSekt,iniFName,cbFile.Items,mList);
  with cbFile do begin
    with Items do for i:=Count-1 downto 0 do
      if not FileExists(Strings[i]) then Delete(i);
    if Items.Count>0 then ItemIndex:=0;
    end;
  LoadHistory(Ininame,DirSekt,iniFName,cbDir.Items,mList);
  with cbDir do if Items.Count>0 then ItemIndex:=0;
  pnExtract.Visible:=false;
  Caption:=ProgName+Vers+' - '+_('Inspect and unpack InnoSetup files');
  end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  with TUnicodeIniFile.CreateForWrite(IniName) do begin
    WriteInteger(CfgSekt,iniLeft,Left);
    WriteInteger(CfgSekt,iniTop,Top);
    WriteInteger(CfgSekt,iniWdt,ClientWidth);
    WriteInteger(CfgSekt,iniHgt,ClientHeight);
    WriteString(CfgSekt,iniUnp,UnpProg);
    Free;
    end;
  SaveHistory(Ininame,FilterSekt,'',true,cbFilter.Items,mList);
  SaveHistory(Ininame,FileSekt,iniFName,true,cbFile.Items,mList);
  SaveHistory(Ininame,DirSekt,iniFName,true,cbDir.Items,mList);
  end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if not FileExists(UnpProg) then UnpProg:=SetDirName(PrgPath)+InnoUnp;
  if not FileExists(UnpProg) then begin
    if not LoadUnpacker then Close;
    end;
  if FileExists(cbFile.Text) then bbListClick(Sender)
  else bbFileClick(Sender);
  end;

procedure TMainForm.bbExitClick(Sender: TObject);
begin
  Close;
  end;

procedure TMainForm.WMDROPFILES (var Msg: TMessage);
var
   n,size: integer;
   Filename: PChar;
begin
  inherited;
  Filename:=nil;
  n:= DragQueryFile(Msg.WParam, $FFFFFFFF, Filename, 255);
  if n>0 then begin
    size := DragQueryFile(Msg.WParam, 0 , nil, 0) + 1;
    Filename:= StrAlloc(size);
    DragQueryFile(Msg.WParam,0 , Filename, size);
    if AnsiSameText(GetExt(Filename),'exe') then begin
      AddToHistory(cbFile.Items,Filename,mList);
      cbFile.Text:=Filename;
      Application.BringToFront;
      bbListClick(self);
      end
    else ErrorDialog(_('This application only allows dropping of exe files!'));
    StrDispose(Filename);
  end;
  DragFinish(Msg.WParam);
end;

procedure TMainForm.cbFileCloseUp(Sender: TObject);
begin
  with cbFile do begin
    AddToHistory(Items,Items[ItemIndex],mList);
    ItemIndex:=0;
    bbListClick(Sender);
    end;
  end;

procedure TMainForm.cbFilterCloseUp(Sender: TObject);
begin
  with cbFilter do begin
    AddToHistory(Items,Items[ItemIndex],mList);
    ItemIndex:=0;
    end;
  end;

procedure TMainForm.bbFileClick(Sender: TObject);
begin
  with OpenDialog do begin
    if length(cbFile.Text)>0 then InitialDir:=ExtractFilePath(cbFile.Text)
    else InitialDir:=UserPath;
    Filename:='';
    Filter:=_('Programs|*.exe|All files|*.*');
    Title:=_('Select InnoSetup archive');
    if Execute then begin
      AddToHistory(cbFile.Items,Filename,mList);
      cbFile.Text:=Filename;
      bbListClick(Sender);
      end;
    end;
  end;

procedure TMainForm.cbDirCloseUp(Sender: TObject);
begin
  with cbDir do begin
    AddToHistory(Items,Items[ItemIndex],mList);
    ItemIndex:=0;
    end;
  end;

procedure TMainForm.cbDirExit(Sender: TObject);
begin
  with cbDir do AddToHistory(Items,Text,mList);
  end;

function TMainForm.LoadUnpacker : boolean;
begin
  with OpenDialog do begin
    if length(UnpProg)>0 then InitialDir:=ExtractFilePath(UnpProg)
    else InitialDir:=ProgPath;
    Filename:=ExtractFilename(UnpProg);
    Filter:=_('Programs|*.exe|All files|*.*');
    Title:=_('Search for "innounp.exe"');
    Result:=Execute;
    if Result then UnpProg:=Filename;
    end;
  end;

procedure TMainForm.bbCopyResultClick(Sender: TObject);
begin
  with mmDos do begin
    SelectAll;
    CopyToClipBoard;
    SelLength:=0;
    end;
  end;

procedure TMainForm.bbDirClick(Sender: TObject);
var
  s : string;
begin
  s:=cbDir.Text;
  if length(s)=0 then s:=ExtractFilePath(cbFile.Text);
  if ShellDirDialog.Execute(_('Select directory for extracted files'),
                true,true,false,UserPath,s) then begin
    AddToHistory(cbDir.Items,s,mList);
    cbDir.Text:=s;
    end;
  end;

procedure TMainForm.bbFilterClick(Sender: TObject);
var
  s  : string;
  ml : TStringList;
begin
  s:='';
  ml:=TStringList.Create;
  with ml do begin
    Delimiter:=';';
    DelimitedText:=cbFilter.Text;
    end;
  if SelectFromListDialog.Execute(BottomRightPos(bbFilter,5,5),
              Caption,_('File filter:'),'',
              [soEdit,soOrder],0,tcLower,'*.*',ml,s)=mrOK then begin
    with cbFilter do begin
      Text:=ml.DelimitedText; AddItem(Text,nil);
      AddToHistory(Items,Text,mList);
      end;
    ml.Free;
    end;
  end;

procedure TMainForm.bbExtractClick(Sender: TObject);
begin
  with pnExtract do Visible:=not Visible;
  end;

procedure TMainForm.bbListClick(Sender: TObject);
var
  s : string;
begin
  pnExtract.Visible:=false;
  s:=MakeQuotedStr(UnpProg)+' -v';
  if cxEmbedded.Checked then s:=s+' -m';
  Execute(s,cbFile.Text,'','');
  end;

procedure TMainForm.bbVerifyClick(Sender: TObject);
var
  s : string;
begin
  pnExtract.Visible:=false;
  s:=MakeQuotedStr(UnpProg)+' -t';
  if cxEmbedded.Checked then s:=s+' -m';
  Execute(s,cbFile.Text,'','');
  end;

procedure TMainForm.bbOptionsClick(Sender: TObject);
begin
  LoadUnpacker;
  end;

procedure TMainForm.bbStartClick(Sender: TObject);
var
  s,sd,sf,cmd : string;
begin
  sd:=cbDir.Text;
  if not ContainsFullPath(sd) then begin
    cmd:=ExtractFilePath(cbFile.Text);
    if length(cmd)=0 then cmd:=PrgPath;
    sd:=MakeQuotedStr(cmd)+sd;
    end
  else sd:=MakeQuotedStr(sd);
  s:=cbFilter.Text; sf:='';
  repeat
    sf:=sf+MakeQuotedStr(ReadNxtStr(s,';'))+Space;
    until (length(s)=0);
  sf:=Trim(sf);
  if AnsiSameText(sf,'*.*') then sf:='';
  cmd:=MakeQuotedStr(UnpProg)+' -b ';
  if cxStrip.Checked then cmd:=cmd+'-e ' else cmd:=cmd+'-x ';
  if cxEmbedded.Checked then cmd:=cmd+'-m ';
  if cxOverwrite.Checked then cmd:=cmd+'-y ';
  if cxDupl.Checked then cmd:=cmd+'-a ';
  Execute(cmd+'-d'+sd,cbFile.Text,sf,'*** '
    +Format(_('Extracting setup file ...'+sLineBreak+'Destination directory: %s'),[sd]));
  end;

procedure TMainForm.InfoBtnClick(Sender: TObject);
begin
  InfoDialog ('',Caption+' ('+Vers+')'+sLineBreak+CopRgt
           +sLineBreak+'E-Mail: '+EmailAdr);
  end;

procedure TMainForm.Execute (const Command,FileName,Filter,Comment : string);
const
  BUFSIZE = 4096;
var
  si        : TStartupInfo;
  pi        : TProcessInformation;
  saAttr    : TSecurityAttributes;
  hChildStdoutRd,
  hChildStdoutWr  : THandle;
  chBuf           : array [0..BUFSIZE] of AnsiChar;
  dwRead,ec,wc    : DWord;
  s           : string;
  sa          : RawByteString;
  vi          : TFileVersionInfo;

  function RawByteToUnicode(sa : RawByteString; CodePage : integer = 1252) : string;
  var
    ta,tu : TBytes;
  begin
    if length(sa)=0 then Result:=''
    else begin
      SetLength(ta,length(sa));
      Move(sa[1],ta[0],Length(ta));
      SetLength(tu,length(sa)*sizeof(Char));
      tu:=TEncoding.Convert(TEncoding.GetEncoding(CodePage),TEncoding.Unicode,ta);
      SetLength(Result,length(sa));
      Move(tu[0],Result[1],Length(tu));
      ta:=nil; tu:=nil;
      end;
    end;

begin
  with mmDos,Lines do begin
    Clear;
    Add(_('Filename: ')+FileName);
    Add('');
    end;
  if not FileExists(FileName) then begin
    s:=SysErrorMessage(ERROR_FILE_NOT_FOUND);
    mmDos.Lines.Add(_('Error: ')+s);
    ErrorDialog(s);
    Exit;
    end;
  s:=Command+' -u '+MakeQuotedStr(Erweiter(PrgPath,Filename,''));
  if length(Filter)>0 then s:=s+Space+Filter;
  with mmDos,Lines do begin
    if GetFileVersion (Filename,vi) then begin
      Add(_('Name: ')+vi.Description);
      Add(_('Version: ')+vi.Version);
      Add(_('Copyright: ')+Trim(vi.Copyright));
      Add(_('Company: ')+vi.Company);
      Add(_('Comment: ')+vi.Comments);
      end;
    Add('');
    if length(Comment)>0 then Add(Comment);
    end;
  Application.ProcessMessages;
  Screen.Cursor:=crHourglass;
// Set the bInheritHandle flag so pipe handles are inherited.
  with saAttr do begin
    nLength:=sizeof(SECURITY_ATTRIBUTES);
    bInheritHandle:=TRUE;
    lpSecurityDescriptor:=nil;
    end;
// Create a pipe for the child process's STDOUT.
  CreatePipe(hChildStdoutRd,hChildStdoutWr,@saAttr,defPipeSize);
  SetHandleInformation(hChildStdoutRd, HANDLE_FLAG_INHERIT,0);
// Create process to start compiler
  FillChar(si, SizeOf(TStartupInfo), 0);
  with si do begin
    cb := Sizeof(TStartupInfo);
    dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
    wShowWindow:=SW_HIDE;
    hStdOutput:=hChildStdoutWr;
//    hStdError:=hChildStdoutWr;
    end;
  try
     if CreateProcess(nil,                // Anwendungsname
                     pchar(s),
                     nil,                // Security
                     nil,                // Security
                     true,               // use InheritHandles
                     NORMAL_PRIORITY_CLASS, // Priorität
                     nil,                   // Environment
                     nil,                   // Verzeichnis
                     si,pi) then begin
      wc:=WaitForSingleObject(pi.hProcess,10000); //=WAIT_TIMEOUT; // wait 10 s
      GetExitCodeProcess(pi.hProcess,ec); // exit code from called program
      CloseHandle(pi.hProcess);
// Close the write end of the pipe before reading from the
// read end of the pipe.
      if CloseHandle(hChildStdoutWr) then begin
  // Read output from the child process, and write to parent's STDOUT.
        while ReadFile(hChildStdoutRd,chBuf[0],BUFSIZE,dwRead,nil)
              and (dwRead=BUFSIZE) do begin
          sa:=sa+chBuf;
          end;
        if dwRead>0 then begin
          chBuf[dwread]:=#0;
          sa:=sa+chBuf;
          end;
        s:=UTF8ToString(sa);
        s:=ReplaceStr(s,CrLf,Lf); // Convert Unix style output
        s:=ReplaceStr(s,Lf,CrLf);
        mmDos.SetSelTextBuf(PChar(s));
        end;
      CloseHandle(hChildStdoutRd);
      // DOS-Ausgabe anzeigen
      with mmDos do begin
        SelLength:=0;
        Perform(WM_VSCROLL,SB_BOTTOM,0);
        end;
      if wc<>WAIT_OBJECT_0 then mmDos.Lines.Add('*** '+_('Error: ')+SysErrorMessage(wc));
      end
    else ErrorDialog(_('Error: ')+SysErrorMessage(GetLastError));
  finally
    Screen.Cursor:=crDefault;
    end;
  end;

end.

