(* Delphi Dialog
   Verzeichniswahldialog mit ShellTreeView
   =======================================
   Design ähnlich wie Windows-Datei-Dialog
   
   © Dr. J. Rathlev 24222 Schwentinental
     Web:  www.rathlev-home.de
     Mail: kontakt(a)rathlev-home.de

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.
    
   Vers. 1 - August 2003
   Vers. 1.1 - Sep. 2005 : changes for use with XpManifest (TPanel is transp.)
   Vers. 2.0 - Jun. 2006 : optional file window
   Vers. 2.1 - Jan. 2007 : starts on MyFiles if no directory specified
   Vers. 2.2 - Apr. 2008 : changes in ShellCtrls - see
                           http://www.kutinsoft.com/Hints/DelphiHints.php
   Vers. 2.3 - Sep. 2009 : history list for selected directories
   Vers. 2.4 - Mar. 2010 : adjustable window sizes
   Vers. 3.0 - Apr. 2012 : Delphi XE2
   Vers. 3.1 - Nov. 2015 : Delphi 10, adaption to new shell control components
   Vers. 3.2 - July 2022 : define compiler switch "ACCESSIBLE" to make dialog
                           messages accessible to screenreaders
   Vers. 3.3 - Mar. 2024 : changed to TMemIniFile

   last modified: April 2025
   *)

unit ShellDirDlg;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.ComCtrls,
  Vcl.Shell.ShellCtrls, Vcl.Menus, System.ImageList, Vcl.ImgList;

const
  NetLink = 'target.lnk';

type
  TShellDirDialog = class(TForm)
    btbOK: TBitBtn;
    btbCancel: TBitBtn;
    ShellTreeView: TShellTreeView;
    spbNew: TSpeedButton;
    cbxSelectedDir: TComboBox;
    Label1: TLabel;
    spbUp: TSpeedButton;
    spbHome: TSpeedButton;
    panRoot: TPanel;
    spbNetwork: TSpeedButton;
    spbComputer: TSpeedButton;
    spbDesktop: TSpeedButton;
    spbMyFiles: TSpeedButton;
    cbxFiles: TCheckBox;
    ShellListView: TShellListView;
    PanelLeft: TPanel;
    PanelRight: TPanel;
    Panel1: TPanel;
    PopupMenu: TPopupMenu;
    itmDelete: TMenuItem;
    N1: TMenuItem;
    cancel1: TMenuItem;
    itmCreate: TMenuItem;
    itmUpdate: TMenuItem;
    Splitter: TSplitter;
    laVolHint: TStaticText;
    Shape1: TShape;
    procedure ShellTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure spbDesktopClick(Sender: TObject);
    procedure spbMyFilesClick(Sender: TObject);
    procedure spbComputerClick(Sender: TObject);
    procedure spbNetworkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure spbUpClick(Sender: TObject);
    procedure spbHomeClick(Sender: TObject);
    procedure spbNewClick(Sender: TObject);
    procedure btbOKClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ShellTreeViewClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cbxFilesClick(Sender: TObject);
    procedure itmDeleteClick(Sender: TObject);
    procedure itmUpdateClick(Sender: TObject);
    procedure ShellTreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PopupMenuPopup(Sender: TObject);
    procedure cbxSelectedDirCloseUp(Sender: TObject);
    procedure cbxSelectedDirChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FDefaultDir,FIniName,FIniSection : string;
    DirList   : TStringList;
    procedure SaveToIni;
    function GetDiskInfo (const APath : string) : string;
    procedure ShowFiles (AShow : boolean);
    procedure AddHistory (const ADir : string);
    procedure DeleteHistory (const ADir : string);
    procedure SelectDir (const ADir : string);
  public
    { Public declarations }
{$IFDEF HDPI}   // scale glyphs and images for High DPI
    procedure AfterConstruction; override;
{$EndIf}
    procedure LoadFromIni(const IniName, Section : string);
    procedure ResetPosition;
    function Execute (const ATitle  : string;
                      Hidden,FileView,ZipAsFiles : boolean;
                      const HomeDir : string;
                      var Dir : string) : boolean;
  end;

procedure InitDirectoryDialog (const AIniName,ASection : string);
function DirectoryDialog (const ATitle  : string; Hidden,FileView  : boolean;
                          const HomeDir : string; var Dir : string) : boolean; overload;
function DirectoryDialog (const ATitle  : string; Hidden,FileView,ZipAsFiles  : boolean;
                          const HomeDir : string; var Dir : string) : boolean; overload;

var
  ShellDirDialog: TShellDirDialog;

implementation

{$R *.dfm}

uses System.IniFiles, Vcl.Dialogs, System.StrUtils, Winapi.ShlObj, Winapi.Shellapi,
  Winapi.ActiveX, WinShell, WinUtils, {$IFDEF ACCESSIBLE} ShowMessageDlg {$ELSE} MsgDialogs {$ENDIF},
  PathUtils, NumberUtils, GnuGetText, SelectDlg
{$IFDEF Trace}
  , FileUtils
{$EndIf}
  ;

const
  FMaxLen = 15;

var
  IniFileName,SectionName   : string;

{ ------------------------------------------------------------------- }
procedure TShellDirDialog.FormCreate(Sender: TObject);
begin
{$IFDEF Trace}
  WriteDebugLog('Create ShellDirDlg');
{$EndIf}
  TranslateComponent (self,'dialogs');
  FIniName:=''; FIniSection:='';
  FDefaultDir:='';
  DirList:=TStringList.Create;
  cbxSelectedDir.DropDownCount:=FMaxLen;
  panRoot.ParentBackground:=false;
  Top:=(Screen.Height-Height) div 2;
  Left:=(Screen.Width-Width) div 2;
//  if (Win32Platform=VER_PLATFORM_WIN32_NT) and (Win32MajorVersion>=10) then // Windows 10
//    spbNetwork.Visible:=Smb1Installed;
  end;

{$IFDEF HDPI}   // scale glyphs and images for High DPI
procedure TShellDirDialog.AfterConstruction;
begin
  inherited;
  if Application.Tag=0 then begin
    ScaleButtonGlyphs(self,PixelsPerInchOnDesign,Monitor.PixelsPerInch);
    end;
  end;
{$EndIf}

{ ------------------------------------------------------------------- }
(* Initialize *)
procedure TShellDirDialog.FormShow(Sender: TObject);
begin
  FitToScreen(Screen,self);
  with spbHome do begin
    Visible:=FDefaultDir<>'';
    Hint:=dgettext('dialogs','Default: ')+FDefaultDir;
    end;
  with cbxSelectedDir do begin
    Items.Assign(DirList);
    if Items.Count>0 then Style:=csDropDown else Style:=csSimple;
    if DirectoryExists(Text) then ShellTreeView.Path:=Text;
    end;
  end;


procedure TShellDirDialog.FormActivate(Sender: TObject);
begin
  with ShellTreeView do begin
//      Path:=cbxSelectedDir.Text;
    try
      Selected.MakeVisible;
    except
      end;
    SetFocus;
    end;
  end;

procedure TShellDirDialog.FormResize(Sender: TObject);
begin
  if Visible and cbxFiles.Checked then
    with ShellListView do SetColWidths([GetWidth-48,12,15,17]) // Anzahl Zeichen pro Spalte
  end;

procedure TShellDirDialog.ResetPosition;
begin
  Top:=50; Left:=50;
  end;

(* save position and history list *)
procedure TShellDirDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try SaveToIni; except end;
  end;

procedure TShellDirDialog.FormDestroy(Sender: TObject);
begin
  DirList.Free;
  end;

{ ------------------------------------------------------------------- }
const
  iniHistory = 'History';
  iniFileView = 'Fileview';
  iniTop = 'Top';
  iniLeft = 'Left';
  iniHeight = 'Height';
  iniLWidth = 'DirWidth';
  iniRWidth= 'FileWidth';

(* load posiition and history list *)
procedure TShellDirDialog.LoadFromIni(const IniName, Section : string);
var
  i       : integer;
  IniFile : TMemIniFile;
  s       : string;
begin
  FIniName:=IniName; FIniSection:=Section;
  if FileExists(FIniName) and (length(FIniSection)>0) then begin
    DirList.Clear;
    IniFile:=TMemIniFile.Create(IniName);
    for i:=0 to FMaxLen-1 do begin
      s:=IniFile.ReadString(FIniSection,iniHistory+IntToStr(i),'');
      if s<>'' then DirList.Add(s);
      end;
    with IniFile do begin
      Top:=ReadInteger(FIniSection,iniTop,0);
      Left:=ReadInteger(FIniSection,iniLeft,Left);
      Height:=ReadInteger(FIniSection,iniHeight,Height);
      with PanelLeft do Width:=ReadInteger(FIniSection,iniLWidth,Width);
      with PanelRight do Width:=ReadInteger(FIniSection,iniRWidth,Width);
      Splitter.Left:=PanelRight.Left;
      cbxFiles.Checked:=ReadBool(FIniSection,iniFileView,false);
      if Top=0 then begin
        Top:=(Screen.Height-Height) div 2;
        Left:=(Screen.Width-Width) div 2;
        end;
      Free;
      end;
    end;
  end;

procedure TShellDirDialog.SaveToIni;
var
  i : integer;
begin
  if (length(FIniName)>0) and (length(FIniSection)>0) then begin
    with TMemIniFile.Create(FIniName) do begin
      try
        EraseSection(FIniSection);
        with DirList do for i:=0 to Count-1 do
          WriteString(FIniSection,iniHistory+IntToStr(i),Strings[i]);
        WriteInteger(FIniSection,iniTop,Top);
        WriteInteger(FIniSection,iniLeft,Left);
        WriteInteger(FIniSection,iniHeight,Height);
        WriteInteger(FIniSection,iniLWidth,PanelLeft.Width);
        WriteInteger(FIniSection,iniRWidth,PanelRight.Width);
        WriteBool(FIniSection,iniFileView,cbxFiles.Checked);
        UpdateFile;
      finally
        Free;
        end;
      end;
//    EraseSectionFromIniFile(FIniName,FIniSection);
//    with cbxSelectedDir.Items do for i:=0 to Count-1 do
//      WriteStringToIniFile(FIniName,FIniSection,iniHistory+IntToStr(i),Strings[i]);
//    WriteIntegerToIniFile(FIniName,FIniSection,iniTop,Top);
//    WriteIntegerToIniFile(FIniName,FIniSection,iniLeft,Left);
//    WriteIntegerToIniFile(FIniName,FIniSection,iniHeight,Height);
//    WriteIntegerToIniFile(FIniName,FIniSection,iniLWidth,PanelLeft.Width);
//    WriteIntegerToIniFile(FIniName,FIniSection,iniRWidth,PanelRight.Width);
//    WriteBoolToIniFile(FIniName,FIniSection,iniFileView,cbxFiles.Checked);
//    UpdateIniFile(FIniName);
    end;
  end;

{ ------------------------------------------------------------------- }
(* add directory to history list *)
procedure TShellDirDialog.AddHistory (const ADir : string);
var
  n : integer;
begin
  if length(ADir)>0 then with DirList do begin
    n:=IndexOf(ADir);
    if n<0 then begin
      if Count>=FMaxLen then Delete (Count-1);
      Insert (0,ADir);
      end
    else begin
      if n>0 then Move (n,0);
      Strings[0]:=ADir;  // update string anyway, e.g. if case was changed
      end;
    cbxSelectedDir.Items.Assign(DirList);
    end;
  end;

(* delete directory from history list *)
procedure TShellDirDialog.DeleteHistory (const ADir : string);
var
  n : integer;
begin
  with DirList do begin
    n:=IndexOf(ADir);
    if n>=0 then Delete(n);
    end;
  with cbxSelectedDir do begin
    Items.Assign(DirList);
    ItemIndex:=0;
    end;
  end;

{------------------------------------------------------------------- }
(* go to parent directory *)
procedure TShellDirDialog.spbUpClick(Sender: TObject);
var
  s : string;
begin
  with ShellTreeView do begin
//    s:=SelectedFolder.Parent.PathName;
    if (Selected.Parent=nil) then begin
      Root:='rfMyComputer'; // Path:='';
      spbComputer.Down:=true;
      end
    else if Root='rfPersonal' then begin
      s:=ExtractParentPath(Path);
      Root:='rfMyComputer'; // Path:='';
      spbComputer.Down:=true;
      Path:=s;
      end
    else if (Selected.Parent.Level>0) then Path:=SelectedFolder.Parent.PathName;
    if assigned(Selected) then Selected.MakeVisible;
    SetFocus;
    end;
{  s:=cbxSelectedDir.Text;
  if (copy(s,1,2)<>'\\') or (PosEx('\',s,3)>0) then begin
    while s[length(s)]<>'\' do delete (s,length(s),1);
    delete (s,length(s),1);
    if length(s)>0 then begin
      if (length(s)=2) and (copy(s,2,1)=':') then begin
        ShellTreeView.Root:='rfMyComputer'; s:=s+'\';
        end;
      with ShellTreeView do begin
        Path:=s; Selected.MakeVisible;
        end;
      end;
    end;
  ShellTreeView.SetFocus;   }
  end;

(* go to home directory *)
procedure TShellDirDialog.spbHomeClick(Sender: TObject);
begin
  if not SetCurrentDir(FDefaultDir) then begin
    ErrorDialog(SafeFormat(dgettext('dialogs','Directory not found:'+sLineBreak+'%s!'),[FDefaultDir]));
    DeleteHistory(FDefaultDir);
    end
  else with ShellTreeView do begin
    if (Root='rfNetwork') or ((length(FDefaultDir)=3) and (copy(FDefaultDir,2,2)=':\')) then Root:=FDefaultDir
    else begin
      Root:='rfMyComputer';
      spbComputer.Down:=true;
      try Path:=FDefaultDir; except end;
      Selected.MakeVisible;
      end;
    end;
  ShellTreeView.SetFocus;
  end;

(* create new directory *)
procedure TShellDirDialog.spbNewClick(Sender: TObject);
var
  s : string;
begin
  s:='';
  if InputQuery (ShellTreeView.Path,dgettext('dialogs','New subdirectory:'),s) then begin
    s:=IncludeTrailingPathDelimiter(ShellTreeView.Path)+s;
    if not ForceDirectories(s) then
      ErrorDialog(SafeFormat(dgettext('dialogs','Could not create directory:'+sLineBreak+'%s!'),[s]))
    else with ShellTreeView do begin
      Root:='rfMyComputer';
      spbComputer.Down:=true;
      try Path:=s; except end;
      Selected.MakeVisible;
      end;
    end;
  ShellTreeView.SetFocus;
  end;

procedure TShellDirDialog.itmDeleteClick(Sender: TObject);
var
  s : string;
  fc,dc,ec : cardinal;
  n   : integer;
  err : boolean;

  // Delete a directory including all subdirectories and files
  procedure DeleteDirectory (const Base,Dir           : string;
                             DeleteRoot               : boolean;
                             var DCount,FCount,ECount : cardinal);
  // DCount: number of deleted directories
  // FCount: number of deleted files
  // ECount: number of errors
  var
    DirInfo    : TSearchRec;
    fc,dc,
    Findresult : integer;
    s,sd       : string;
  begin
    if length(Dir)>0 then sd:=SetDirName(Base)+Dir else sd:=Base;
    if DirectoryExists(sd) then begin
      FindResult:=FindFirst(SetDirName(sd)+'*.*',faAnyFile,DirInfo);
      while FindResult=0 do with DirInfo do begin
        if NotSpecialDir(Name) and ((Attr and faDirectory)<>0) then
          DeleteDirectory(Base,SetDirName(Dir)+DirInfo.Name,DeleteRoot,DCount,FCount,ECount);
        FindResult:=FindNext(DirInfo);
        end;
      FindClose(DirInfo);
      fc:=0; dc:=0;
      FindResult:=FindFirst(SetDirName(sd)+'*.*',faArchive+faReadOnly+faHidden+faSysfile+faNormal,DirInfo);
      while FindResult=0 do with DirInfo do begin
        if NotSpecialDir(Name) then begin
          inc(fc);
          (* Dateien löschen *)
          s:=SetDirName(sd)+Name;
          FileSetAttr(s,faArchive);
          if DeleteFile(s) then begin
            inc(FCount); inc(dc);
            end
          else inc(ECount);  // Fehler
          end;
        FindResult:=FindNext(DirInfo);
        end;
      FindClose(DirInfo);
      if (fc=dc) and (DeleteRoot or (length(Dir)>0)) then begin   // Verzeichnis leer ==> löschen
        FileSetAttr(sd,0);    // Attribute zum Löschen entfernen
        if RemoveDir(sd) then inc(DCount) else inc(ECount);
        end;
      end;
    end;

begin
  s:=ShellTreeView.Path;
  n:=SelectOption(dgettext('dialogs','Delete directory'),
    SafeFormat(dgettext('dialogs','Delete "%s"'),[s]),mtConfirmation,[fsBold],
    [dgettext('dialogs','Definitely'),dgettext('dialogs','To recycle bin')]);
  if n>=0 then begin
    spbUpClick(Sender);
    if n=0 then begin
      fc:=0; dc:=0; ec:=0;
      DeleteDirectory(s,'',true,dc,fc,ec);
      err:=ec>0;
      if not err then InfoDialog(SafeFormat(dgettext('dialogs','%u directories with %u files deleted!'),[dc,fc]));
      end
    else begin
      if (Win32Platform=VER_PLATFORM_WIN32_NT) and (Win32MajorVersion>=6) then // IsVista or newer
        err:=IShellDeleteDir (Application.Handle,SetDirName(s),true,false)<>NO_ERROR
      else err:=ShellDeleteAll (Application.Handle,SetDirName(s),'',true,false)<>NO_ERROR;
      if not err then InfoDialog(SafeFormat(dgettext('dialogs','%s moved to Recycle Bin!'),[s]));
      end;
    if err then ErrorDialog(SafeFormat(dgettext('dialogs','Error deleting directory:'+sLineBreak+'%s!'),[s]));
    itmUpdateClick(Sender);
    end;
  end;

procedure TShellDirDialog.itmUpdateClick(Sender: TObject);
begin
  with ShellTreeView do Refresh(Selected);
  end;

{------------------------------------------------------------------- }
procedure TShellDirDialog.btbOKClick(Sender: TObject);
begin
  AddHistory(cbxSelectedDir.Text);
  end;

procedure TShellDirDialog.PopupMenuPopup(Sender: TObject);
begin
  itmUpdateClick(Sender);
  end;

procedure TShellDirDialog.ShellTreeViewClick(Sender: TObject);
begin
{  with ShellTreeView do if assigned(Selected) then begin
    Refresh(Selected);
    Selected.MakeVisible;
    end;      }
  end;

procedure TShellDirDialog.ShellTreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
//  if Button=mbRight then with ShellTreeView do begin
//    n:=GetItemAt(x,y).Index;
  end;

procedure TShellDirDialog.ShellTreeViewChange(Sender: TObject;
  Node: TTreeNode);
var
  s,s1,s2,s3,s4,sh : string;
  sf : TShellFolder;
begin
  if Active then with ShellTreeView do begin
    sh:='';
    sf:=SelectedFolder;
    if assigned(sf) then with sf do begin
      btbOk.Enabled:=fpFileSystem in Properties;
      cbxSelectedDir.Text:=PathName;
      if fpIsLink in Properties then begin
        s:=IncludeTrailingPathDelimiter(PathName)+NetLink;
        if FileExists(s) then begin
          GetLink(s,s1,s2,s3,s4);
          cbxSelectedDir.Text:=s1;
          end
        end;
      sh:=GetDiskInfo(cbxSelectedDir.Text);
      end;
    laVolHint.Caption:=sh;
    end;
  end;

procedure TShellDirDialog.spbDesktopClick(Sender: TObject);
begin
  ShellTreeView.Root:='rfDesktop';
  end;

procedure TShellDirDialog.spbMyFilesClick(Sender: TObject);
begin
  ShellTreeView.Root:='rfPersonal';
  end;

procedure TShellDirDialog.spbComputerClick(Sender: TObject);
begin
  ShellTreeView.Root:='rfMyComputer';
  end;

procedure TShellDirDialog.spbNetworkClick(Sender: TObject);
begin
  ShellTreeView.Root:='rfNetwork';
  end;

procedure TShellDirDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Shift=[]) and (Key=VK_F5) then begin
    with ShellTreeView do if assigned(Selected) then Refresh(Selected)
    end
{$IFDEF ACCESSIBLE}
  else if (Key=VK_F11) then begin
    with ActiveControl do if length(Hint)>0 then ShowHintInfo(Hint);
    end
  else if ssCtrl in Shift then begin
    case Key of
    ord('T'),ord('t') : SpeedButtonClick(spbDesktop);
    ord('D'),ord('d') : SpeedButtonClick(spbMyFiles);
    ord('C'),ord('c') : SpeedButtonClick(spbComputer);
    ord('W'),ord('w') : SpeedButtonClick(spbNetwork);
    ord('N'),ord('n') : SpeedButtonClick(spbNew);
    ord('U'),ord('u') : SpeedButtonClick(spbUp);
    ord('H'),ord('h') : SpeedButtonClick(spbHome);
      end;
    end
{$ENDIF}
  else if Key=VK_Return then ModalResult:=mrOK;
  end;

{ ------------------------------------------------------------------- }
function TShellDirDialog.GetDiskInfo (const APath : string) : string;
var
  na,nf : int64;
begin
  if GetDiskFreeSpaceEx(pchar(IncludeTrailingPathDelimiter(APath)),na,nf,nil) then
    Result:=SizeToStr(na)+dgettext('dialogs',' free of ')+SizeToStr(nf)
  else Result:='';
  end;

procedure TShellDirDialog.ShowFiles (AShow : boolean);
var
  d : integer;
begin
  if AShow then with PanelRight do begin
//    Width:=301;
    d:=Left+Width;
    Visible:=true;
    ShellTreeView.ShellListView:=ShellListView;
    with cbxSelectedDir do if (length(Text)>0) and DirectoryExists(Text) then ShellTreeView.Path:=Text;
    end
  else with PanelLeft do begin
    d:=Width;
    PanelRight.Visible:=false;
    ShellTreeView.ShellListView:=nil;
    end;
  ClientWidth:=d;
  cbxSelectedDir.Text:=ShellTreeView.Path;
  laVolHint.Caption:=GetDiskInfo(cbxSelectedDir.Text);
  btbOk.Enabled:=DirectoryExists(cbxSelectedDir.Text);
  end;

{ ------------------------------------------------------------------- }
procedure TShellDirDialog.cbxFilesClick(Sender: TObject);
begin
  if Visible then ShowFiles(cbxFiles.Checked);
  end;

procedure TShellDirDialog.cbxSelectedDirChange(Sender: TObject);
begin
  SelectDir(cbxSelectedDir.Text);
  btbOk.Enabled:=DirectoryExists(cbxSelectedDir.Text);
  end;

procedure TShellDirDialog.cbxSelectedDirCloseUp(Sender: TObject);
begin
  with cbxSelectedDir do SelectDir(Items[ItemIndex]);
  end;

procedure TShellDirDialog.SelectDir (const ADir : string);
var
  s,r : string;
begin
  s:=ADir;
  if length(s)=0 then s:=FDefaultDir;
  if length(s)=0 then begin  // no default
    spbDesktop.Down:=true;
    r:='rfDesktop';
    end
  else begin
    spbComputer.Down:=true;
    if (length(s)=0) or not DirectoryExists(s)then begin
      s:=GetPersonalFolder;
      r:='rfMyComputer';
  //    r:='rfPersonal';
      if length(s)=0 then s:=GetCurrentDir;
      end
    else begin
      if copy(s,1,2)='\\' then begin
        r:='rfNetwork';
  //      r:='rfDesktop';
        spbNetwork.Down:=true;
        end
      else r:='rfMyComputer';
      end;
    end;
  with ShellTreeView do begin
    Root:=r;
    sleep(500);    // new Feb. 2021
    if length(s)>0 then Path:=s
    else Selected:=nil;
    if assigned(Selected) then begin
      try
        Selected.Expand(false);
        Selected.MakeVisible;
      except
        end;
      end;
    end;
  end;

{------------------------------------------------------------------- }
(* Dialog an Position anzeigen *)
function TShellDirDialog.Execute (const ATitle  : string;
                                  Hidden,FileView,ZipAsFiles  : boolean;
                                  const HomeDir : string;
                                  var Dir : string) : boolean;
begin
  Caption:=ATitle; FDefaultDir:=HomeDir;
  with ShellTreeView do begin
    if Hidden then ObjectTypes:=ObjectTypes+[otHidden,otHiddenSystem]
    else ObjectTypes:=ObjectTypes-[otHidden,otHiddenSystem];
    ShowZip:=not ZipAsFiles;
    end;
//  if (copy(Dir,1,2)='\\') and not spbNetwork.Visible then begin
//    ErrorDialog(_('Browsing the network is not supported on this system!'));
//    SelectDir(HomeDir);
//    Exit;
//    end
  with ShellListView do begin
    if Hidden then ObjectTypes:=ObjectTypes+[otHidden,otHiddenSystem]
    else ObjectTypes:=ObjectTypes-[otHidden,otHiddenSystem];
    ShowZip:=ZipAsFiles;
    end;
  SelectDir(Dir);
  cbxSelectedDir.Text:=ShellTreeView.Path;
  laVolHint.Caption:=GetDiskInfo(cbxSelectedDir.Text);
  cbxFiles.Visible:=FileView;
  if FileView then ShowFiles(cbxFiles.Checked)
  else ShowFiles(false);
  Result:=ShowModal=mrOK;
  if Result then Dir:=SetDirName(cbxSelectedDir.Text);
  end;

procedure InitDirectoryDialog (const AIniName,ASection : string);
begin
  IniFileName:=AIniName; SectionName:=ASection;
  end;

function DirectoryDialog (const ATitle  : string; Hidden,FileView,ZipAsFiles  : boolean;
                          const HomeDir : string; var Dir : string) : boolean; overload;
begin
  if not assigned(ShellDirDialog)then begin
    ShellDirDialog:=TShellDirDialog.Create(Application);
    ShellDirDialog.LoadFromIni(IniFileName,SectionName);
    end;
  Result:=ShellDirDialog.Execute(ATitle,Hidden,FileView,ZipAsFiles,HomeDir,Dir);
  end;

function DirectoryDialog (const ATitle  : string; Hidden,FileView  : boolean;
                          const HomeDir : string; var Dir : string) : boolean;
begin
  Result:=DirectoryDialog(ATitle,Hidden,FileView,true,HomeDir,Dir);
  end;

initialization
  IniFileName:=''; SectionName:='';
finalization
end.
