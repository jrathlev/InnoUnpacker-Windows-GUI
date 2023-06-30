(* DosPanel - Windows GUI for DOSBox
   =================================
   Application specific settings
   -----------------------------

   © J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   J. Rathlev, Dec. 2011
   last modified: Dec. 2023
   *)

unit AppSettings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Imaging.pngimage;

const
  defCycles = 4000;
  maxCycles = 1000000;

type
  TImgType = (imNone,imIco,imBmp,imPng);

const
  ImageExt : array[TImgType] of string = ('','ico','bmp','png');

type
  TAppIcons = class(TPicture)
  private
    FImgType : TImgType;
    DefLImg,DefSImg   : TPicture;
    function GetSmallImage (Img : TPicture): TBitmap; overload;
    function GetSmallImage (Img : TIcon): TBitmap; overload;
  public
    Small    : TBitmap;
    constructor Create (AImgType : TImgType; DefLargeImg,DefSmallImg : TPicture);
    constructor CreateFrom (Icons : TAppIcons);
    destructor Destroy; override;
    procedure Assign (Icons : TAppIcons);
    procedure AssignIcon (Icon : TIcon);
    procedure LoadFromFile (const Filename : string);
    property ImgType : TImgType read FImgType write FImgType;
    end;

  TDosBoxApp = class(TObject)
  private
    DefLImg,DefSImg   : TPicture;
    procedure InitIcons;
  public
    AppName,Category,
    AppPath,CdPath,
    AppFile,Parameters,
    Commands,AppMapper,
    IconFile,ManFile,
    Description       : string;
    HardDrv,CdDrv     : Char;
    IsoImage,
    FullScreen,
    AutoEnd,
    UseDefault        : boolean;
    ImgIndex,
    MemSize,
    Speed             : integer;
    Icons             : TAppIcons;
    constructor Create (DefLargeImg,DefSmallImg : TPicture);
    destructor Destroy; override;
    procedure Assign (ADosBoxApp : TDosBoxApp);
    procedure LoadIcons(const Filename : string);
    end;

  TAppSettingsDialog = class(TForm)
    btbCancel: TBitBtn;
    btbOK: TBitBtn;
    Label1: TLabel;
    cbCategory: TComboBox;
    edAppName: TLabeledEdit;
    edIconFile: TLabeledEdit;
    edManFile: TLabeledEdit;
    OpenDialog: TOpenDialog;
    btExeFile: TSpeedButton;
    btIconFile: TSpeedButton;
    btManFile: TSpeedButton;
    imgIcon: TImage;
    edDescription: TLabeledEdit;
    cxFullScreen: TCheckBox;
    edParam: TLabeledEdit;
    gbOptions: TGroupBox;
    cxAutoEnd: TCheckBox;
    gbSpeed: TGroupBox;
    rbAuto: TRadioButton;
    rbCycles: TRadioButton;
    edExeFile: TLabeledEdit;
    btPath: TSpeedButton;
    edCommands: TLabeledEdit;
    btCommands: TSpeedButton;
    cbHardDrive: TComboBox;
    cbCdRomDrive: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    edAppPath: TEdit;
    edIsoFile: TEdit;
    Label6: TLabel;
    cbMemSize: TComboBox;
    rbDrive: TRadioButton;
    rbIsoIMage: TRadioButton;
    btIsoFile: TSpeedButton;
    cbDrive: TComboBox;
    gbHardDrive: TGroupBox;
    gbCdDrive: TGroupBox;
    pnIsoFile: TPanel;
    edMapperFile: TLabeledEdit;
    btMapper: TSpeedButton;
    rbMax: TRadioButton;
    reCycles: TEdit;
    udCycles: TUpDown;
    cxDefault: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btPathClick(Sender: TObject);
    procedure btExeFileClick(Sender: TObject);
    procedure btIsoFileClick(Sender: TObject);
    procedure btIconFileClick(Sender: TObject);
    procedure btManFileClick(Sender: TObject);
    procedure edAppPathChange(Sender: TObject);
    procedure btCommandsClick(Sender: TObject);
    procedure cbHardDriveCloseUp(Sender: TObject);
    procedure rbDriveClick(Sender: TObject);
    procedure rbIsoIMageClick(Sender: TObject);
    procedure btMapperClick(Sender: TObject);
    procedure cxDefaultClick(Sender: TObject);
  private
    { Private-Deklarationen }
    NewIcons   : TAppIcons;
    LastDrive  : string;
    procedure ChangeCdRom (IsIso : boolean);
    procedure ShowOptions;
  public
    { Public-Deklarationen }
    function Execute (Categories : TStrings;
                      var DosBoxApp : TDosBoxApp) : boolean;
  end;

function MiscName : string;
function ScaleBitmap(ABitmap: TBitmap; AWidth,AHeight : integer) : TBitMap;

var
  AppSettingsDialog: TAppSettingsDialog;

implementation

{$R *.dfm}

uses Winapi.ShellApi, System.StrUtils, DosPanelMain, GnuGetText, WinUtils, MsgDialogs,
  PathUtils, ShellDirDlg, SelectFromListDlg, WinDevUtils, StringUtils;

const
  MemSizeList : array [0..4] of word = (8,16,32,48,64);

{ ------------------------------------------------------------------- }
function MiscName : string;
begin
  Result:=_('Miscellaneous');
  end;

function ScaleBitmap(ABitmap: TBitmap; AWidth,AHeight : integer) : TBitMap;
begin
  ABitmap.Transparent:=true;
  Result:=TBitmap.Create;
  with Result do begin
    SetSize(AWidth,AHeight);
    TransparentMode:=tmAuto;
    Canvas.StretchDraw(Rect(0,0,AWidth,AHeight),ABitmap);  // scale
    Transparent:=true;
    end;
  end;

{ ------------------------------------------------------------------- }
constructor TAppIcons.Create (AImgType : TImgType; DefLargeImg,DefSmallImg : TPicture);
begin
  inherited Create;
  FImgType:=AImgType;
  DefLImg:=DefLargeImg; DefSImg:=DefSmallImg;
  AssignIcon(DefLargeImg.Icon);
  Small:=GetSmallImage(DefSmallImg);
  end;

constructor TAppIcons.CreateFrom (Icons : TAppIcons);
begin
  inherited Create;
  Assign(Icons);
  end;

procedure TAppIcons.Assign (Icons : TAppIcons);
begin
  FImgType:=Icons.ImgType;
  DefLImg:=Icons.DefLImg;
  DefSImg:=Icons.DefSImg;
  inherited Assign(Icons.Graphic);
  Small:=TBitmap.Create;
  Small.Assign(Icons.Small);
  end;

procedure TAppIcons.AssignIcon (Icon : TIcon);
begin
  ImgType:=imIco;
  inherited Assign(Icon);
  end;

destructor TAppIcons.Destroy;
begin
  if assigned(Small) then Small.Free;
  inherited Destroy;
  end;

function TAppIcons.GetSmallImage (Img : TPicture): TBitmap;
begin
  Result:=TBitmap.Create;
  with Result do begin
    SetSize(Img.Width,Img.Height);
    TransparentMode:=tmAuto; Transparent:=true;
    Canvas.Draw(0,0,Img.Graphic);
    end;
  end;

function TAppIcons.GetSmallImage (Img : TIcon): TBitmap;
begin
  Result:=TBitmap.Create;
  with Result do begin
    SetSize(Img.Width,Img.Height);
    TransparentMode:=tmAuto; Transparent:=true;
    Canvas.Draw(0,0,Img);
    end;
  end;

procedure TAppIcons.LoadFromFile (const Filename : string);
var
  ic : TIcon;
  bj : TPicture;

  function GetFileIcon(const FileName: string; const Small: Boolean): TIcon;
  var
    FI: TSHFileInfo;
    Attributes: DWORD;
    Flags: Word;
  begin
    Attributes := 0;
    Flags := SHGFI_ICON;
    if Small then Flags:=Flags or SHGFI_SMALLICON else Flags:=Flags or SHGFI_LARGEICON;
    if SHGetFileInfo(PChar(FileName), Attributes, FI, SizeOf(FI), Flags) <> 0 then begin
      Result := TIcon.Create;
      Result.Handle := FI.hIcon;
      end
    else Result:=nil;
    end;

  function ScalePng(APng: TGraphic; AWidth,AHeight : integer) : TBitMap;
  var
    bm : TBitMap;
  begin
    bm:=TBitmap.Create;
    bm.PixelFormat:=pf16Bit;
    (APng as TPngImage).AssignTo(bm);
    Result:=ScaleBitmap(bm,AWidth,AHeight);
    bm.Free;
    end;

  function ScaleIcon (AIcon : TIcon; AWidth,AHeight : integer) : TBitMap;
  var
    bm : TBitMap;
  begin
    bm:=TBitmap.Create;
    with bm do begin
      SetSize(AIcon.Width,AIcon.Height);
      TransparentMode:=tmFixed;
      TransparentColor:=clWhite;
      Transparent:=true;
      with Canvas do begin
        Brush.Color:=clWhite;          // background
        Draw(0,0,AIcon);               // copy to bitmap
        end;
      Transparent:=TransparentColor=clWhite;
      end;
    Result:=TBitmap.Create;
    with Result do begin
      SetSize(AWidth,AHeight);
      TransparentMode:=tmFixed;
      Canvas.StretchDraw(Rect(0,0,AWidth,AHeight),bm);  // scale
      Transparent:=bm.Transparent;
      end;
    bm.Free;
    end;

  function GeTImgType (const Filename : string) : TImgType;
  var
    ext : string;
    it  : TImgType;
  begin
    ext:=GetExt(Filename);
    Result:=imNone;
    for it:=imIco to High(TImgType) do if AnsiSameText(ext,ImageExt[it]) then begin
      Result:=it; Break;
      end;
    end;

begin
  if FileExists(Filename) then begin
    ImgType:=GetImgType(Filename);
    if ImgType=imNone then begin
      ic:=GetFileIcon(Filename,false);
      if assigned(ic) then begin
        AssignIcon(ic); ic.Free;
        ic:=GetFileIcon(Filename,true);
        if assigned(ic) then begin
          Small:=GetSmallImage(ic); ic.Free;
          end
        else Small:=ScaleIcon(Icon,16,16);
        end
      else begin
        AssignIcon(DefLImg.Icon);
        Small:=GetSmallImage(DefSImg);
        end;
      end
    else if ImgType=imPng then begin
      bj:=TPicture.Create;
      bj.LoadFromFile(Filename);
      Bitmap.PixelFormat:=pf16Bit;
      (bj.Graphic as TPngImage).AssignTo(Bitmap);
      Small:=ScaleBitmap(Bitmap,16,16);
      bj.Free;
      end
    else begin
      inherited LoadFromFile(Filename);
      if ImgType=imIco then Small:=ScaleIcon(Icon,16,16)
      else Small:=ScaleBitmap(Bitmap,16,16)
      end;
    end
  else begin
    ImgType:=imIco;
    AssignIcon(DefLImg.Icon);
    Small:=GetSmallImage(DefSImg);
    end;
  end;

{ ------------------------------------------------------------------- }
constructor TDosBoxApp.Create(DefLargeImg,DefSmallImg : TPicture);
begin
  inherited Create;
  AppName:=''; Category:='';
  AppPath:=''; CdPath:='';
  HardDrv:='C'; CdDrv:='D';
  AppFile:=''; Parameters:='';
  IconFile:=''; ManFile:='';
  AppMapper:='';
  Commands:=HardDrv+':;"CD \"';
  Description:='';
  IsoImage:=true;
  FullScreen:=false;
  AutoEnd:=true;
  UseDefault:=false;
  Speed:=0; ImgIndex:=-1;
  DefLImg:=DefLargeImg; DefSImg:=DefSmallImg;
  InitIcons;
  end;

destructor TDosBoxApp.Destroy;
begin
  if assigned(Icons) then Icons.Free;
  inherited Destroy;
  end;

procedure TDosBoxApp.Assign (ADosBoxApp : TDosBoxApp);
begin
  AppName:=ADosBoxApp.AppName;
  Category:=ADosBoxApp.Category;
  AppPath:=ADosBoxApp.AppPath;
  CdPath:=ADosBoxApp.CdPath;
  HardDrv:=ADosBoxApp.HardDrv;
  CdDrv:=ADosBoxApp.CdDrv;
  AppFile:=ADosBoxApp.AppFile;
  Parameters:=ADosBoxApp.Parameters;
  IconFile:=ADosBoxApp.IconFile;
  ManFile:=ADosBoxApp.ManFile;
  AppMapper:=ADosBoxApp.AppMapper;
  Commands:=ADosBoxApp.Commands;
  Description:=ADosBoxApp.Description;
  IsoImage:=ADosBoxApp.IsoImage;
  FullScreen:=ADosBoxApp.FullScreen;
  AutoEnd:=ADosBoxApp.AutoEnd;
  Speed:=ADosBoxApp.Speed;
  ImgIndex:=ADosBoxApp.ImgIndex;
  DefLImg:=ADosBoxApp.DefLImg;
  DefSImg:=ADosBoxApp.DefSImg;
  Icons.Assign(ADosBoxApp.Icons);
  end;

procedure TDosBoxApp.InitIcons;
begin
  Icons:=TAppIcons.Create(imIco,DefLImg,DefSImg);
  end;

procedure TDosBoxApp.LoadIcons(const Filename : string);
begin
  Icons.LoadFromFile(Filename);
  end;

{ ------------------------------------------------------------------- }
procedure TAppSettingsDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent(self);
  with cbDrive do begin
    BuildDriveList(Items,[dtCdRom]);
    if Items.Count>0 then ItemIndex:=0;
    end;
  end;

procedure TAppSettingsDialog.ChangeCdRom (IsIso : boolean);
begin
  if not IsIso then begin
    if (cbDrive.Items.Count>0) then begin
      cbDrive.Visible:=true;
      pnIsoFile.Visible:=false;
      rbDrive.Checked:=true;
      end
    else begin
      ErrorDialog(_('No physical CD drives available!'));
      IsIso:=true;
      end;
    end;
  if IsIso then begin
    cbDrive.Visible:=false;
    pnIsoFile.Visible:=true;
    rbIsoIMage.Checked:=true;
    end;
  end;

procedure TAppSettingsDialog.cxDefaultClick(Sender: TObject);
begin
  ShowOptions;
  end;

procedure TAppSettingsDialog.ShowOptions;
var
  i : integer;
begin
  with gbOptions do for i:=0 to ControlCount-1 do Controls[i].Enabled:=not cxDefault.Checked;
  with gbSpeed do for i:=0 to ControlCount-1 do Controls[i].Enabled:=not cxDefault.Checked;
  end;

procedure TAppSettingsDialog.rbDriveClick(Sender: TObject);
begin
  if Visible then ChangeCdRom(false);
  end;

procedure TAppSettingsDialog.rbIsoIMageClick(Sender: TObject);
begin
  if Visible then ChangeCdRom(true);
  end;

procedure TAppSettingsDialog.btIconFileClick(Sender: TObject);
begin
  with OpenDialog do begin
    if length(edIconFile.Text)>0 then InitialDir:=ExtractFilePath(edIconFile.Text)
    else InitialDir:=edAppPath.Text;
    DefaultExt:='ico';
    Filename:='';
    Filter:=_('Image')+'|*.ico;*.bmp;*.png|'+_('Executables')+'|*.exe;*.dll|'+_('All')+'|*.*';
    Title:=_('Select file with icon for this application');
    if Execute then begin
      edIconFile.Text:=Filename;
      NewIcons.LoadFromFile(Filename);
      imgIcon.Picture.Assign(NewIcons);
      end;
    end;
  end;

procedure TAppSettingsDialog.btIsoFileClick(Sender: TObject);
begin
  with OpenDialog do begin
    if length(edIsoFile.Text)>0 then InitialDir:=ExtractFilePath(edIsoFile.Text)
    else InitialDir:=edAppPath.Text;
    DefaultExt:='iso';
    Filename:='';
    Filter:=_('ISO images')+'|*.iso';
    Title:=SafeFormat(_('Select iso image to be mounted as drive %s'),[cbCdRomDrive.Text]);
    if Execute then edIsoFile.Text:=Filename;
    end;
  end;

procedure TAppSettingsDialog.btManFileClick(Sender: TObject);
begin
  with OpenDialog do begin
    if length(edManFile.Text)>0 then InitialDir:=ExtractFilePath(edManFile.Text)
    else InitialDir:=edAppPath.Text;
    DefaultExt:='txt';
    Filename:='';
    Filter:=_('Text files')+'|*.txt|'+_('HTML files')+'|*.htm;*.html|'+
            _('PDF files')+'|*.pdf|'+_('All')+'|*.*';
    Title:=_('Select manual for this application');
    if Execute then edManFile.Text:=Filename;
    end;
  end;

procedure TAppSettingsDialog.btMapperClick(Sender: TObject);
begin
  with OpenDialog do begin
    if length(edMapperFile.Text)>0 then InitialDir:=ExtractFilePath(edMapperFile.Text)
    else InitialDir:=edAppPath.Text;
    DefaultExt:='map';
    Filename:='';
    Filter:=_('DOSBox key mapper files')+'|*.map|'+_('All')+'|*.*';
    Title:=_('Select key mapper file');
    if Execute then edMapperFile.Text:=Filename;
    end;
  end;

procedure TAppSettingsDialog.btCommandsClick(Sender: TObject);
var
  sl : TStringList;
  s  : string;
begin
  sl:=TStringList.Create;
  with sl do begin
    QuoteChar:=Quote; Delimiter:=Semicolon;
    DelimitedText:=edCommands.Text;
    end;
  if SelectFromListDialog.Execute(CursorPos,_('Edit startup commands'),_('List of commands'),'',
              [soEdit,soOrder],1,tcNone,'',sl,s)=mrOK then edCommands.Text:=sl.DelimitedText;
  sl.Free;
  end;

procedure TAppSettingsDialog.btExeFileClick(Sender: TObject);
begin
  with OpenDialog do begin
    if length(edExeFile.Text)>0 then
      InitialDir:=SetDirName(edAppPath.Text)+ExtractFilePath(edExeFile.Text)
    else InitialDir:=edAppPath.Text;
    DefaultExt:='exe';
    Filename:='';
    Filter:=_('Executables')+'|*.exe;*.com|'+_('Batch files')+'|*.bat|'+_('All')+'|*.*';
    Title:=_('Select executable for startup');
    if Execute then begin
      if IsSubPath(edAppPath.Text,Filename) then
        edExeFile.Text:=MakeRelativePath(SetDirName(edAppPath.Text),Filename)
      else ErrorDialog(SafeFormat(_('The executable file must be located beneath the root path:'+
        sLineBreak+'%s'+sLineBreak+'Please try again!'),[edAppPath.Text]));
      end;
    end;
  end;

procedure TAppSettingsDialog.btPathClick(Sender: TObject);
var
  s : string;
begin
  s:=edAppPath.Text;
  if length(s)=0 then s:=frmMain.BasicSettings.RootPath;
  if ShellDirDialog.Execute (SafeFormat(_('Select path to be mounted as drive %s'),[cbHardDrive.Text]),
      false,true,false,frmMain.BasicSettings.RootPath,s) then edAppPath.Text:=s;
  end;

procedure TAppSettingsDialog.cbHardDriveCloseUp(Sender: TObject);
begin
  with edCommands do Text:=AnsiReplaceText(Text,LastDrive+':',cbHardDrive.Text[1]+':');
  LastDrive:=cbHardDrive.Text;
  end;

procedure TAppSettingsDialog.edAppPathChange(Sender: TObject);
var
  ok : boolean;
begin
  ok:=DirectoryExists(edAppPath.Text);
  edExeFile.Enabled:=ok;
  btExeFile.Enabled:=ok;
  edParam.Enabled:=ok;
  edIconFile.Enabled:=ok;
  btIconFile.Enabled:=ok;
  edMapperFile.Enabled:=ok;
  end;

function TAppSettingsDialog.Execute (Categories : TStrings;
                                     var DosBoxApp : TDosBoxApp) : boolean;
var
  i,n : integer;
begin
  with cbCategory do begin
    Clear;
    for i:=0 to Categories.Count-1 do Items.AddObject(Categories[i],pointer(i));
    end;
  with DosBoxApp do begin
    edAppname.Text:=AppName;
    with cbCategory do ItemIndex:=Items.IndexOf(Category);
    edAppPath.Text:=AppPath;
    with cbHardDrive do begin
      n:=Items.IndexOf(HardDrv);
      if n>=0 then ItemIndex:=n else ItemIndex:=2;
      end;
    LastDrive:=cbHardDrive.Text;
    with cbCdRomDrive do begin
      n:=Items.IndexOf(CdDrv);
      if n>=0 then ItemIndex:=n else ItemIndex:=3;
      end;
    ChangeCdRom(IsoImage);
    if IsoImage then edIsoFile.Text:=CdPath
    else with cbDrive do ItemIndex:=Items.IndexOf(CdPath);
    edExeFile.Text:=AppFile;
    edParam.Text:=Parameters;
    edCommands.Text:=Commands;
    edIconFile.Text:=IconFile;
    edMapperFile.Text:=AppMapper;
    edManFile.Text:=ManFile;
    edDescription.Text:=Description;
    cxFullScreen.Checked:=FullScreen;
    cxAutoEnd.Checked:=AutoEnd;
    for i:=0 to High(MemSizeList) do if MemSize<=MemSizeList[i] then Break;
    with cbMemSize do if i>High(MemSizeList) then Itemindex:=1 else Itemindex:=i;
    with udCycles do begin
      Position:=defCycles;
      Max:=maxCycles-1;
      end;
    if Speed=0 then rbAuto.Checked:=true
    else if Speed>=maxCycles then rbMax.Checked:=true
    else begin
      rbCycles.Checked:=true;
      udCycles.Position:=Speed;
      end;
    cxDefault.Checked:=UseDefault;
    imgIcon.Picture.Assign(Icons);
    NewIcons:=TAppIcons.CreateFrom(Icons);
    Result:=ShowModal=mrOK;
    if Result then begin
      AppName:=edAppname.Text;
      Category:=cbCategory.Text;
      if AnsiSameText(Category,MiscName) then Category:='';
      if length(Category)>0 then
        with cbCategory.Items do if IndexOf(Category)<0 then Add(Category);
      AppPath:=edAppPath.Text;
      HardDrv:=cbHardDrive.Text[1];
      CdDrv:=cbCdRomDrive.Text[1];
      IsoImage:=rbIsoImage.Checked;
      if IsoImage then CdPath:=edIsoFile.Text else CdPath:=cbDrive.Text;
      AppFile:=edExeFile.Text;
      Parameters:=edParam.Text;
      Commands:=edCommands.Text;
      IconFile:=edIconFile.Text;
      AppMapper:=edMapperFile.Text;
      ManFile:=edManFile.Text;
      Description:=edDescription.Text;
      FullScreen:=cxFullScreen.Checked;
      AutoEnd:=cxAutoEnd.Checked;
      MemSize:=MemSizeList[cbMemSize.ItemIndex];
      if rbAuto.Checked then Speed:=0
      else if rbMax.Checked then Speed:=maxCycles
      else Speed:=udCycles.Position;
      UseDefault:=cxDefault.Checked;
      Icons.Assign(NewIcons);
      end;
    NewIcons.Free;
    end;
  end;

end.
