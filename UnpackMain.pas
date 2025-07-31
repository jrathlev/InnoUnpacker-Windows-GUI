(* Unpack Inno setup files
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
   Vers. 1.6 (August 2020):     added filter to extract single files
   Vers. 1.7 (October 2021):    console output uses UTF8
   Vers. 1.8 (June 2022):       "embedded option" added
   Vers. 1.9.1 (August 2022):   command line options added
   Vers. 1.9.4 (August 2024):   innounp updated to version 1.72
                                timeout on calling innounp.exe with confirmation
   Vers. 1.9.6 (October 2024):  using innounp up to version 0.50 and 1.75
   Vers. 2.0   (December 2024): new layout,
                                colored display of innounp (v1.77 and up) output

   last modified: January 2025

   Command line options: [<setupname>] [options]
     <setupname>  : name of setup file to be unpacked
     /d:<destdir> : destination directory for unpacked files
     /f:<filter>  : file filter
     /e:<pwd>     : encryption password
     /c           : only files from {app} path
     /m           : process internal embedded files
     /s           : extract files without paths
     /a           : process all copies of duplicate files
     /o           : overwrite files
     /p           : run as portable program
   *)

unit UnpackMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  Vcl.ExtCtrls, WinApiUtils, Vcl.ComCtrls;

const
  ProgName = 'InnoUnpacker';
  ProgVers = ' 2.0.3';
  CopRgt = '© 2014-2025 Dr. J. Rathlev, D-24222 Schwentinental';
  EmailAdr = 'kontakt(a)rathlev-home.de';

  defPipeSize = 1024*1024;
  defTimeOut  = 10000;  // 10 s

type
  TMainForm = class(TForm)
    pnTop: TPanel;
    Label2: TLabel;
    cbFile: TComboBox;
    bbOptions: TBitBtn;
    bbExit: TBitBtn;
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
    bbDown: TBitBtn;
    bbUp: TBitBtn;
    bbInfo: TBitBtn;
    cxEncrypted: TCheckBox;
    edPassword: TLabeledEdit;
    pnBottom: TPanel;
    pnTools: TPanel;
    bbVersion: TBitBtn;
    bbCopyPath: TBitBtn;
    bbLang: TBitBtn;
    bbScript: TBitBtn;
    bbSetupInfo: TBitBtn;
    paShowText: TPanel;
    pbShowText: TPaintBox;
    sbVert: TScrollBar;
    sbHorz: TScrollBar;
    cxOnlyApp: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure bbInfoClick(Sender: TObject);
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
    procedure bbUpClick(Sender: TObject);
    procedure bbDownClick(Sender: TObject);
    procedure cxEncryptedClick(Sender: TObject);
    procedure bbVersionClick(Sender: TObject);
    procedure bbCopyPathClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure bbLangClick(Sender: TObject);
    procedure bbScriptClick(Sender: TObject);
    procedure bbSetupInfoClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pbShowTextPaint(Sender: TObject);
    procedure sbVertScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure edPasswordKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private-Deklarationen }
    AppPath,UserPath,
    IniName,ProgPath,
    ProgVersName,ProgVersDate,
    UnpProg               : string;
    ConsoleText           : TStringList;
    NewUnp                : boolean;
    LineHeight,VisibleLines,
    TextWdt               : integer;
    function StripColCtrls (const AText : string) : string;
    function LoadUnpacker : boolean;
    function CheckUnpackVersion : boolean;
//    procedure AddText(const AText : string);
    procedure Execute (const Command,FileName,Filter,Comment : string; GoBottom : boolean = false);
    procedure WMDROPFILES (var Msg: TMessage); message WM_DROPFILES;
  public
    { Public-Deklarationen }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses System.IniFiles, System.StrUtils, Winapi.ShellApi, System.UITypes, Vcl.ClipBrd,
  System.Math, GnuGetText, InitProg,WinUtils, MsgDialogs,IniFileUtils, PathUtils,
  ListUtils, WinDevUtils, StringUtils, ShellDirDlg, SelectFromListDlg;

{ ------------------------------------------------------------------- }
resourcestring
  rsInfo = 'Command line options: [<name>] [options]'+sLineBreak+
     #9'<name>'#9#9': name of setup file to be unpacked'+sLineBreak+
     #9'/d:<ddir>'#9': destination directory for unpacked files'+sLineBreak+
     #9'/f:<filter>'#9': file filter'+sLineBreak+
     #9'/e:<pwd>'#9': encryption password'+sLineBreak+
     #9'/l:<lg>'#9#9': language selection (en, de, fr, it or hu)'+sLineBreak+
     #9'/m'#9#9': process internal embedded files'+sLineBreak+
     #9'/s'#9#9': extract files without paths'+sLineBreak+
     #9'/a'#9#9': process all copies of duplicate files'+sLineBreak+
     #9'/o'#9#9': overwrite files'+sLineBreak+
     #9'/p'#9#9': run as portable program';

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

procedure TMainForm.FormCreate(Sender: TObject);
var
  i : integer;
  port : boolean;
  s,sa,sd,sf : string;
begin
  TranslateComponent(self);
  DragAcceptFiles(MainForm.Handle, true);
  InitPaths(AppPath,UserPath,ProgPath);
  InitVersion(ProgName,ProgVers,CopRgt,3,3,ProgVersName,ProgVersDate);
  port:=false; sd:=''; sf:=''; sa:='';
  if ParamCount>0 then for i:=1 to ParamCount do begin
    s:=ParamStr(i);
    if (s[1]='/') or (s[1]='-') then begin
      Delete(s,1,1);
      if CompareOption(s,'m') then cxEmbedded.Checked:=true
      else if CompareOption(s,'c') then cxOnlyApp.Checked:=true
      else if CompareOption(s,'s') then cxStrip.Checked:=true
      else if CompareOption(s,'a') then cxDupl.Checked:=true
      else if CompareOption(s,'o') then cxOverwrite.Checked:=true
      else if CompareOption(s,'p') then port:=true
      else if ReadOptionValue(s,'w') then edPassword.Text:=s
      else if ReadOptionValue(s,'d') then sd:=s
      else if ReadOptionValue(s,'f') then sf:=s;
      cxEncrypted.Checked:=length(edPassword.Text)>0;
      end
    else sa:=s;
    end;
  if port or IsEmptyStr(AppPath) or IsRemovableDrive(PrgPath) then s:=PrgPath else s:=AppPath;
  IniName:=Erweiter(s,PrgName,IniExt);
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
  AddToHistory(cbFilter,sf);
  LoadHistory(Ininame,FileSekt,iniFName,cbFile.Items,mList);
  with cbFile do begin
    with Items do for i:=Count-1 downto 0 do
      if not FileExists(Strings[i]) then Delete(i);
    if Items.Count>0 then ItemIndex:=0;
    end;
  AddToHistory(cbFile,sa);
  LoadHistory(Ininame,DirSekt,iniFName,cbDir.Items,mList);
  with cbDir do if Items.Count>0 then ItemIndex:=0;
  AddToHistory(cbDir,sd);
  pnExtract.Visible:=false; NewUnp:=true;
  Caption:=ProgVersName+' - '+_('Inspect and unpack InnoSetup files');
  ConsoleText:=TStringList.Create;
  with pbShowText do Canvas.Font:=Font;
  LineHeight:=MulDiv(abs(pbShowText.Font.Height),12,10);
  end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ConsoleText.Free;
  end;

procedure TMainForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
with pbShowText do if sbVert.Visible and ClientRect.Contains(ScreenToClient(MousePos)) then begin
    if WheelDelta<>0 then with sbVert do begin
      if WheelDelta<0 then begin
        if Position<Max then Position:=Position+1
        end
      else begin
        if Position>Min then Position:=Position-1;
        end;
      end;
    pbShowText.Invalidate;
    Handled:=true;
    end;
  end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  if Visible then begin
    if TextWdt>0 then begin
      sbHorz.Visible:=TextWdt>pbShowText.Width;
      with sbHorz do if Visible then Max:=TextWdt-pbShowText.Width;
      end;
    if ConsoleText.Count>0 then begin
      VisibleLines:=pbShowText.Height div LineHeight;
      sbVert.Visible:=ConsoleText.Count>VisibleLines;
      with sbVert do if Visible then begin
        Max:=ConsoleText.Count-VisibleLines;
        if Position>Max then Position:=Max;
        end;
      pbShowText.Invalidate;
      end;
    end;
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

function TMainForm.CheckUnpackVersion : boolean;
var
  UnpVers : TVersion;
begin
  Result:=GetFileVersionAsNumber(UnpProg,UnpVers);
  if Result then with UnpVers do begin
    Result:=(Major>1) or ((Major>=1) and (Minor>=70));
    end;
  end;

procedure TMainForm.FormShow(Sender: TObject);
var
  s : string;
begin
  if not FileExists(UnpProg) then UnpProg:=SetDirName(PrgPath)+InnoUnp;
  if not FileExists(UnpProg) then begin
    if not LoadUnpacker then Close;
    end;
  NewUnp:=CheckUnpackVersion;
  if FileExists(cbFile.Text) then bbSetupInfoClick(Sender)
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
      bbSetupInfoClick(self);
      end
    else ErrorDialog(_('This application only allows dropping of exe files!'));
    StrDispose(Filename);
  end;
  DragFinish(Msg.WParam);
end;

procedure TMainForm.sbVertScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  pbShowText.Invalidate;
  end;

{ ------------------------------------------------------------------- }
function TMainForm.LoadUnpacker : boolean;
begin
  with OpenDialog do begin
    if length(UnpProg)>0 then InitialDir:=ExtractFilePath(UnpProg)
    else InitialDir:=ProgPath;
    Filename:=ExtractFilename(UnpProg);
    Filter:=_('Programs|*.exe|All files|*.*');
    Title:=_('Search for "innounp.exe"');
    Result:=Execute;
    if Result then begin
      UnpProg:=Filename;
      NewUnp:=CheckUnpackVersion;
      end;
    end;
  end;

{ ------------------------------------------------------------------- }
procedure TMainForm.cbFileCloseUp(Sender: TObject);
begin
  with cbFile do begin
    AddToHistory(Items,Items[ItemIndex],mList);
    ItemIndex:=0;
    bbSetupInfoClick(Sender);
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
      bbSetupInfoClick(Sender);
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
  with cbDir do begin
    AddToHistory(Items,Text,mList);
    ItemIndex:=0;
    end;
  end;

procedure TMainForm.cxEncryptedClick(Sender: TObject);
begin
  edPassword.Visible:=cxEncrypted.Checked;
  end;

procedure TMainForm.edPasswordKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_RETURN then bbVerifyClick(Sender);
  end;

procedure TMainForm.bbCopyPathClick(Sender: TObject);
begin
  AddToHistory(cbDir,DelExt(cbFile.Text));
  end;

function TMainForm.StripColCtrls (const AText : string) : string;
var
  n0,n1,n2 : integer;
begin
  Result:=''; n0:=1;
  repeat
    n1:=PosEx('<',AText,n0);
    if n1>0 then begin
      Result:=Result+copy(AText,n0,n1-n0);
      n2:=PosEx('>',AText,n1+1);
      if (n2=n1+2) then n0:=n2+1;
      end;
    until n1=0;
  Result:=Result+copy(AText,n0,length(AText)-n0+1);
  end;

procedure TMainForm.bbCopyResultClick(Sender: TObject);
begin
  ClipBoard.AsText:=_('Filename: ')+cbFile.Text+sLineBreak+sLineBreak+StripColCtrls(ConsoleText.Text);
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
              ProgVersName,_('Extract files matching:'),'',
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
  if Visible then begin
    pnExtract.Visible:=false;
    s:=MakeQuotedStr(UnpProg)+' -b -v -h';
    if cxEmbedded.Checked then s:=s+' -m';
    if cxEncrypted.Checked then s:=s+' -p'+edPassword.Text;
    Execute(s,cbFile.Text,'','');
    end;
  end;

procedure TMainForm.bbVerifyClick(Sender: TObject);
var
  s : string;
begin
  pnExtract.Visible:=false;
  s:=MakeQuotedStr(UnpProg)+' -b -t -h';
  if cxEmbedded.Checked then s:=s+' -m';
  if cxEncrypted.Checked then s:=s+' -p'+edPassword.Text;
  Execute(s,cbFile.Text,'','');
  end;

procedure TMainForm.bbLangClick(Sender: TObject);
begin
  Execute(MakeQuotedStr(UnpProg)+' -l -h',cbFile.Text,'','');
  end;

procedure TMainForm.bbVersionClick(Sender: TObject);
begin
  Execute(MakeQuotedStr(UnpProg)+' -i','','','');
  end;

procedure TMainForm.bbSetupInfoClick(Sender: TObject);
begin
  Execute(MakeQuotedStr(UnpProg)+' -h',cbFile.Text,'','');
  end;

procedure TMainForm.bbOptionsClick(Sender: TObject);
begin
  if LoadUnpacker then bbSetupInfoClick(Sender);
  end;

procedure TMainForm.bbScriptClick(Sender: TObject);
begin
  cbFilter.Text:='install_script.iss';
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
  cmd:=MakeQuotedStr(UnpProg)+' -b';
  if cxStrip.Checked then cmd:=cmd+' -e' else cmd:=cmd+' -x';
  if cxOnlyApp.Checked then cmd:=cmd+' -c{app}';
  if cxEmbedded.Checked then cmd:=cmd+' -m';
  if cxEncrypted.Checked then cmd:=cmd+' -p'+edPassword.Text;
  if cxOverwrite.Checked then cmd:=cmd+' -y';
  if cxDupl.Checked then cmd:=cmd+' -a';
  Execute(cmd+' -d'+sd,cbFile.Text,sf,'<0>*** '+_('Extract files from setup ...')+sLineBreak
    +_('Destination directory: ')+'<2>'+sd,true);
  end;

procedure TMainForm.bbUpClick(Sender: TObject);
begin
  with sbVert do Position:=Min;
  pbShowText.Invalidate;
  end;

procedure TMainForm.bbDownClick(Sender: TObject);
begin
  with sbVert do Position:=Max;
  pbShowText.Invalidate;
  end;

procedure TMainForm.bbInfoClick(Sender: TObject);
begin
  InfoDialog (ProgVersName+' - '+ProgVersDate
    +sLineBreak+_('Inspect and unpack InnoSetup files')
    +sLineBreak+VersInfo.CopyRight
    +sLineBreak+'E-Mail: '+EmailAdr+sLineBreak+sLineBreak+rsInfo);
  end;

{ ------------------------------------------------------------------- }
// To display colors and all unicode characters, TPaintBox is used.
// It replaces a TRichEdit component because this does not support
// all Unicode characters, e.g. nepali and some chinese
procedure TMainForm.pbShowTextPaint(Sender: TObject);
var
  i,n,n0,n1,n2,w,k : integer;
  s,sw : string;
const
  MaxCol = 5;
  ColArray : array[0..5] of TColor = (clBlack,clred,clGreen,clBlue,clMaroon,clPurple);
begin
  with pbShowText do begin
    Canvas.Brush.Color:=clWhite;
    Canvas.FillRect(Rect(0,0,Width-1,Height-1));
    with ConsoleText do if Count<VisibleLines then n:=Count else n:=VisibleLines;
    for i:=0 to n-1 do begin
      k:=sbVert.Position+i;
      if k<ConsoleText.Count then s:=ConsoleText[k] else s:='';
      n0:=1; w:=5-sbHorz.Position;
      repeat
        n1:=PosEx('<',s,n0);
        if n1>0 then begin
          sw:=copy(s,n0,n1-n0);
          Canvas.TextOut(w,5+LineHeight*i,sw);
          w:=w+Canvas.TextWidth(sw);
          n2:=PosEx('>',s,n1+1);
          if (n2=n1+2) and TryStrToInt(copy(s,n1+1,1),k) and (k<>MaxCol) then begin
            Canvas.Font.Color:=ColArray[k];
            n0:=n2+1;
            end
          else begin
            if n2=0 then n2:=n1;
            n0:=n2+1;
            end;
          end;
        until n1=0;
      Canvas.TextOut(w,5+LineHeight*i,copy(s,n0,length(s)-n0+1));
      end;
    end;
  end;

{ ------------------------------------------------------------------- }
procedure TMainForm.Execute (const Command,FileName,Filter,Comment : string; GoBottom : boolean);
const
  BUFSIZE = 4096;
  TextAlign = 30;
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
  cancel      : boolean;

  function GetMaxTextWidth (AList : TStrings) : integer;
  var
    i : integer;
  begin
    Result:=0;
    with AList do for i:=0 to Count-1 do begin
      Result:=Max(Result,pbShowText.Canvas.TextWidth(StripColCtrls(Strings[i])));
      end;
    end;

  procedure InitShowText;
  begin
    // DOS-Ausgabe anzeigen
    TextWdt:=GetMaxTextWidth(ConsoleText);
    sbHorz.Visible:=TextWdt>pbShowText.Width;
    with sbHorz do if Visible then Max:=TextWdt-pbShowText.Width;
    VisibleLines:=pbShowText.Height div LineHeight;
    sbVert.Visible:=ConsoleText.Count>VisibleLines;
    with sbVert do begin
      if Visible then Max:=ConsoleText.Count-VisibleLines else Max:=0;
      if GoBottom then Position:=Max else Position:=Min;
      end;
    sbHorz.Position:=0;
    pbShowText.Invalidate;
    end;

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

  function AddColString (const Text1,Text2 : string) : string;
  begin
    Result:='<0>'+ExtSp(Text1,TextAlign)+'<2>'+Text2;
    end;

begin
  ConsoleText.Clear;
  InitShowText;
  if (length(Filename)>0) and not FileExists(FileName) then begin
    s:=SysErrorMessage(ERROR_FILE_NOT_FOUND);
    ErrorDialog(Filename+': '+s);
    Exit;
    end;
//  with ConsoleText do begin
//    if length(Filename)>0 then begin
//      Add(_('Filename: ')+FileName);
//      Add('');
//      end;
//    end;
  if NewUnp then s:=Command+' -u -z'
  else s:=Command;
  if length(Filename)>0 then begin
    s:=s+Space+MakeQuotedStr(Erweiter(PrgPath,Filename,''));
    with ConsoleText do begin
      if GetFileVersion (Filename,vi) then begin
        Add(AddColString(_('Name: '),vi.Description));
        Add(AddColString(_('Version: '),vi.Version));
        Add(AddColString(_('Copyright: '),Trim(vi.Copyright)));
        Add(AddColString(_('Company: '),vi.Company));
        Add(AddColString(_('Comment: '),vi.Comments));
        end;
      if length(Comment)>0 then begin
        Add('');
        Add(Comment);
        end;
      end;
    end;
  InitShowText;
  if length(Filter)>0 then s:=s+Space+Filter;
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
      repeat
        wc:=WaitForSingleObject(pi.hProcess,defTimeOut); // wait 10 s
        if wc<>WAIT_OBJECT_0 then Cancel:=not ConfirmDialog(_('Timeout occured - continue anyway?'));
        until (wc=WAIT_OBJECT_0) or Cancel;
      if Cancel then TerminateProcess(pi.hProcess,3);
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
        with ConsoleText do Text:=Text+s;
        end;
      CloseHandle(hChildStdoutRd);
      if wc<>WAIT_OBJECT_0 then ConsoleText.Add('*** '+_('Error: ')+SysErrorMessage(wc));
      ConsoleText.Add('');
      InitShowText;
      end
    else ErrorDialog(_('Error: ')+SysErrorMessage(GetLastError));
  finally
    Screen.Cursor:=crDefault;
    end;
  end;

end.

