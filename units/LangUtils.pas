(* Delphi-Unit
  Unterroutinen und Komponenten für Mehrsprachenunterstützung
  ===========================================================

  © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

  The contents of this file may be used under the terms of the
  Mozilla Public License ("MPL") or
  GNU Lesser General Public License Version 2 or later (the "LGPL")

  Software distributed under this License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  Version 2.0 - Nov. 2011
          2.1 - Sept. 2024: languages added
          2.2 - June 2025 : optional loading of language list from resource
          2.3 - April 2026: language codes expanded in accordance with ISO 639-1 <language>-<region>

  last modified: April 2026

  Hinweise zur Verwendung:
  ========================
  1. in der Projekt-Datei (*.dpr):
     -----------------------------
    ...
    uses
      GnuGetText in '<relative path>\Units\GnuGetText.pas',
      LangUtils in '<relative path>Bibliotheken\Units\LangUtils.pas',
      ...
    begin
      TP_GlobalIgnoreClass(TFont);
      TP_GlobalIgnoreClassProperty(TMdiForm,'Caption');
      // ... mögliche weitere "ignores"
      // Subdirectory in AppData for user configuration files and supported languages
      InitTranslation('CfgDir','CfgName',['Delphi10','Indy10','more domains..']);
      ...

    dabei sind:  CfgDir  - Unterverzeichnis für die Konfigurations-Dateien (ini,cfg)
                           in "Anwendungsdaten" oder absolutes Verzeichnis
                 CfgName - Name der Cfg-Datei zum Speichern der Spracheinstellung
                           (leer: Name der Anwendung)
  2. im Haupt-Formular:
     ------------------
    interface
    uses ..., LangUtils, ...
    ...
    type
      TMainForm = class(TForm)
        ...
      private
        ...
        Languages        : TLanguageList;
        procedure SetLanguageClick(Sender : TObject; Language : TLangCodeString);
        ...
      end;
      ...
    implementation
    ...
    procedure TMainForm.FormCreate(Sender: TObject);
    begin
      ...
      InitPaths (AppPath,UserPath);
      InitVersion(Progname,Vers,CopRgt,3,3,ProgVersName,ProgVersDate);
      ...
      Languages:=TLanguageList.Create(PrgPath,LangName);
      with Languages do begin
        Menu:=itmLang;
        LoadLanguageNames(SelectedLanguage);
        OnLanguageItemClick:=SetLanguageClick;
        end;
      ...

    { ------------------------------------------------------------------- }
    procedure TMainForm.SetLanguageClick(Sender : TObject; Language : TLangCodeString);
    begin
      if not AnsiSameStr(SelectedLanguage,Language) then begin
        Languages.LanguageCode:=Language;
        ChangeLanguage(Language);
        Languages.LoadLanguageNames(SelectedLanguage);
        InfoDialog('',GetLanguageHint,CursorPos);
        ...
        end;
      end;
    ...

    dabei sind: TLangCodeString - Länder-Code (2 Buchstaben)
                itmLang         - Menü-Item im Hauptmenü
                                  Die Sprachauswahl wird automatisch als
                                  Untermenü angehängt
  *)
(* @abstract(Subroutines and components for multi languge support with GnuGetText)
   @author(© Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de))
   @created(November 2011)
   @lastmod(April 2026)
*)

unit LangUtils;

interface

uses System.Classes, System.SysUtils, Vcl.Menus, Vcl.Graphics;

const
  LangName = 'Language.cfg';  // Sprachen

  // Spracheinstellung in <PrgName>.cfg
  CfgExt = 'cfg';
  LangSekt  = 'Language';
  LangID = 'LangID';

// Befehlszeile
  siLangOption = 'lang';      // Spracheinstellung
  siAltIni = 'ini';           // Ort für alternative Ini-Datei
  siPortable = 'portable';    // starte als portables Programm

type
  TLangCodeString = string;

  TLanguageMenuEvent = procedure(Sender : TObject; const LangCode : TLangCodeString) of object;

  TLanguageList = class (TStringList)
  private
    FMenu : TMenuItem;
    MenuSizeBefore : integer;
    FLangCode : TLangCodeString;
    FOnLangItemClick : TLanguageMenuEvent;
    FOnLangMeasureItem :TMenuMeasureItemEvent;
    FCurrentLanguage,
    FPath,FLangName : string;
    procedure AddMenuItems;
    procedure RemoveMenuItems;
    function GetLangCode (Index : integer) : TLangCodeString;
    procedure SetMenu(Menu : TMenuItem);
    function LoadDefaultNames : boolean;
  protected
    procedure DoLangItemClick(Sender : TObject); virtual;
    procedure DoLangMeasureItem (Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
    function GetLangIndex (const Value: TLangCodeString) : integer;
    procedure SetLangCode (const Value: TLangCodeString);
  public
    constructor Create (const APath : string; const Filename : string = '');
    destructor Destroy; override;
    function LoadLanguageNames (LangCode : TLangCodeString) : boolean;
    property CurrentLanguage : string read FCurrentLanguage;
    property SelectedLanguageCode : TLangCodeString read FLangCode write SetLangCode;
    property LanguageCode[Index : integer] : TLangCodeString read GetLangCode;
    property Menu : TMenuItem read FMenu write SetMenu;
    property OnLanguageItemClick : TLanguageMenuEvent read FOnLangItemClick write FOnLangItemClick;
    property OnLanguageMeasureItem: TMenuMeasureItemEvent read FOnLangMeasureItem write FOnLangMeasureItem;
    end;

// InitTranslation muss in der Projekt-Datei vor "Application.Initialize" stehen
// und definiert das zu verwendende Anwendungsdaten-Verzeichnis
// AppDir ist entweder ein relativer Pfad zu "AppData" oder ein beliebiger absoluter Pfad
// ConfigName gibt den Dateinamen für die Spracheinstellungen
// Domains ist eine Liste der vom Progamm benötigten Bibliotheks-Übersetzungen
// Aufruf z.B.: InitTranslation('MySoftware','Language.cfg',['delphi10','indy10']);
procedure InitTranslation (const Domains : array of string); overload;
procedure InitTranslation (const CfgDir : string; const Domains : array of string); overload;
procedure InitTranslation (const CfgDir,ConfigName : string; const Domains : array of string); overload;
procedure SaveLanguage (NewLangCode : TLangCodeString);
function ChangeLanguage (const NewLangCode : TLangCodeString) : TLangCodeString;
function GetLanguageHint : string;

var
  SelectedLanguage   : TLangCodeString;
  UserLangID         : TLocaleID;
  LangFromCfg        : boolean;

implementation

uses
  Winapi.Windows, Vcl.Forms, Winapi.ShlObj, System.IniFiles, System.StrUtils,
  System.IOUtils, GnuGetText, UnitConsts, StringUtils, InitProg;

type
  TLangEntry = record
    ShortName : TLangCodeString;
    Id        : TLocaleID;
    end;

const
  IsVista : boolean = true;

// Table to get LangId from language short name
// Used for older systems not supporting GetLocaleInfoEx (XP and older)
  LangCount = 82;
  LangTable : array [0..LangCount-1] of TLangEntry = (
    (ShortName : 'af'; Id : $0436),
    (ShortName : 'ar'; Id : $0401),
    (ShortName : 'be'; Id : $0423),
    (ShortName : 'bg'; Id : $0402),
    (ShortName : 'bo'; Id : $0451),
    (ShortName : 'ca'; Id : $0403),
    (ShortName : 'co'; Id : $0483),
    (ShortName : 'cs'; Id : $0405),
    (ShortName : 'cy'; Id : $0452),
    (ShortName : 'da'; Id : $0406),
    (ShortName : 'de'; Id : $0407),
    (ShortName : 'el'; Id : $0408),
    (ShortName : 'en'; Id : $0409),
    (ShortName : 'es'; Id : $0C0A),
    (ShortName : 'et'; Id : $0425),
    (ShortName : 'fa'; Id : $0429),
    (ShortName : 'fi'; Id : $040B),
    (ShortName : 'fo'; Id : $0438),
    (ShortName : 'fr'; Id : $040C),
    (ShortName : 'fy'; Id : $0462),
    (ShortName : 'ga'; Id : $083C),
    (ShortName : 'gd'; Id : $0491),
    (ShortName : 'gl'; Id : $0456),
    (ShortName : 'hi'; Id : $0439),
    (ShortName : 'hr'; Id : $041A),
    (ShortName : 'hu'; Id : $040E),
    (ShortName : 'hy'; Id : $042B),
    (ShortName : 'id'; Id : $0421),
    (ShortName : 'ii'; Id : $0478),
    (ShortName : 'is'; Id : $040F),
    (ShortName : 'it'; Id : $0410),
    (ShortName : 'ja'; Id : $0411),
    (ShortName : 'ka'; Id : $0437),
    (ShortName : 'kk'; Id : $043F),
    (ShortName : 'ko'; Id : $0412),
    (ShortName : 'ku'; Id : $0492),
    (ShortName : 'ky'; Id : $0440),
    (ShortName : 'lb'; Id : $046E),
    (ShortName : 'lo'; Id : $0454),
    (ShortName : 'lt'; Id : $0427),
    (ShortName : 'lv'; Id : $0426),
    (ShortName : 'mi'; Id : $0481),
    (ShortName : 'mk'; Id : $042F),
    (ShortName : 'mn'; Id : $0450),
    (ShortName : 'mt'; Id : $043A),
    (ShortName : 'ne'; Id : $0461),
    (ShortName : 'nl'; Id : $0413),
    (ShortName : 'no'; Id : $0414),
    (ShortName : 'om'; Id : $0472),
    (ShortName : 'or'; Id : $0448),
    (ShortName : 'pa'; Id : $0446),
    (ShortName : 'pl'; Id : $0415),
    (ShortName : 'ps'; Id : $0463),
    (ShortName : 'pt'; Id : $0416),
    (ShortName : 'rm'; Id : $0417),
    (ShortName : 'ro'; Id : $0418),
    (ShortName : 'ru'; Id : $0419),
    (ShortName : 'sa'; Id : $044F),
    (ShortName : 'se'; Id : $043B),
    (ShortName : 'sk'; Id : $041B),
    (ShortName : 'sl'; Id : $0424),
    (ShortName : 'so'; Id : $0477),
    (ShortName : 'sq'; Id : $041C),
    (ShortName : 'st'; Id : $0430),
    (ShortName : 'sv'; Id : $041D),
    (ShortName : 'sw'; Id : $0441),
    (ShortName : 'ta'; Id : $0449),
    (ShortName : 'th'; Id : $041E),
    (ShortName : 'tk'; Id : $0442),
    (ShortName : 'tr'; Id : $041F),
    (ShortName : 'tt'; Id : $0444),
    (ShortName : 'ug'; Id : $0480),
    (ShortName : 'uk'; Id : $0422),
    (ShortName : 'ur'; Id : $0420),
    (ShortName : 'uz'; Id : $0443),
    (ShortName : 've'; Id : $0433),
    (ShortName : 'vi'; Id : $042A),
    (ShortName : 'wo'; Id : $0488),
    (ShortName : 'xh'; Id : $0434),
    (ShortName : 'yo'; Id : $046A),
    (ShortName : 'zh'; Id : $0804),
    (ShortName : 'zu'; Id : $0435));

{ ------------------------------------------------------------------- }
(* Name enthält vollständigen Pfad *)
function ContainsFullPath (const Name : string) : boolean;
begin
  if length(Name)>0 then Result:=(Name[1]=PathDelim) or (pos(DriveDelim,Name)>0)
  else Result:=false;
  end;

{ ------------------------------------------------------------------- }
function LangCodeToId (const LangName : TLangCodeString) : TLocaleID;
var
  i : integer;
begin
  for i:=0 to LangCount-1 do with LangTable[i] do if AnsiStartsText(ShortName,LangName) then  begin
    Result:=Id; Exit;
    end;
  Result:=0;
  end;

function LangIdToCode (id : TLocaleID) : TLangCodeString;
begin
  case id and $3FF of
  $01 : Result:='ar';
  $02 : Result:='bg';
  $03 : Result:='ca';
  $04 : Result:='zh';
  $05 : Result:='cs';
  $06 : Result:='da';
  $07 : Result:='de';
  $08 : Result:='el';
  $09 : Result:='en';
  $0a : Result:='es';
  $0b : Result:='fi';
  $0c : Result:='fr';
  $0e : Result:='hu';
  $0f : Result:='is';
  $10 : Result:='it';
  $11 : Result:='ja';
  $12 : Result:='ko';
  $13 : Result:='nl';
  $14 : Result:='no';
  $15 : Result:='pl';
  $16 : Result:='pt';
  $17 : Result:='rm';
  $18 : Result:='ro';
  $19 : Result:='ru';
  $1a : Result:='hr';
  $1b : Result:='sk';
  $1c : Result:='sq';
  $1d : Result:='sv';
  $1e : Result:='th';
  $1f : Result:='tr';
  $20 : Result:='ur';
  $21 : Result:='id';
  $22 : Result:='uk';
  $23 : Result:='be';
  $24 : Result:='sl';
  $25 : Result:='et';
  $26 : Result:='lv';
  $27 : Result:='lt';
  $2a : Result:='vi';
  $2b : Result:='hy';
  $2f : Result:='mk';
  $29 : Result:='fa';
  $30 : Result:='st';
  $33 : Result:='ve';
  $34 : Result:='xh';
  $35 : Result:='zu';
  $36 : Result:='af';
  $37 : Result:='ka';
  $38 : Result:='fo';
  $39 : Result:='hi';
  $3a : Result:='mt';
  $3b : Result:='se';
  $3c : Result:='ga';
  $3f : Result:='kk';
  $40 : Result:='ky';
  $41 : Result:='sw';
  $42 : Result:='tk';
  $43 : Result:='uz';
  $44 : Result:='tt';
  $46 : Result:='pa';
  $48 : Result:='or';
  $49 : Result:='ta';
  $4f : Result:='sa';
  $51 : Result:='bo';
  $54 : Result:='lo';
  $50 : Result:='mn';
  $52 : Result:='cy';
  $56 : Result:='gl';
  $61 : Result:='ne';
  $62 : Result:='fy';
  $63 : Result:='ps';
  $6a : Result:='yo';
  $6e : Result:='lb';
  $72 : Result:='om';
  $77 : Result:='so';
  $78 : Result:='ii';
  $80 : Result:='ug';
  $81 : Result:='mi';
  $83 : Result:='co';
  $88 : Result:='wo';
  $91 : Result:='gd';
  $92 : Result:='ku';
  else Result:='en';
    end;
  end;

{ ------------------------------------------------------------------- }
function IdToShortLanguageName (LangId : TLocaleID) : string;
var
  nc : cardinal;
  buf : array of Char;
  lct : LCTYPE;
begin
  Result:=''; nc:=0;
  if IsVista then lct:=LOCALE_SNAME else lct:=LOCALE_SISO639LANGNAME;
  nc:=GetLocaleInfo(LangId,lct,nil,nc);
  if nc>0 then begin
    SetLength(buf,nc);
    if GetLocaleInfo(LangId,lct,@buf[0],nc)>0 then
      Result:=PChar(@buf[0]);
    buf:=nil;
    end;
  if not IsVista then Result:=copy(Result,1,2);
  end;

function ShortLanguageNameToID (const LangName : TLangCodeString) : TLocaleID;
var
  nc : cardinal;
begin
  Result:=0; nc:=0;
  if IsVista then begin
    if GetLocaleInfoEx(pchar(LangName),LOCALE_RETURN_NUMBER or LOCALE_ILANGUAGE,@nc,4)>0 then Result:=nc;
    end
  else begin
    if length(LangName)<2 then Result:=0
    else Result:=LangCodeToId(LangName);
    end;
  end;

{ ------------------------------------------------------------------- }
constructor TLanguageList.Create (const APath,Filename : string);
begin
  inherited Create;
//  Sorted:=true;
  MenuSizeBefore:=0; FLangCode:='';
  FPath:=IncludeTrailingPathDelimiter(APath); FLangName:=Filename;
  LoadDefaultNames;  // load default language table
  end;

destructor TLanguageList.Destroy;
begin
  inherited Destroy;
  end;

{ ------------------------------------------------------------------- }
function TLanguageList.GetLangCode (Index : integer) : TLangCodeString;
begin
  Result:=IdToShortLanguageName(TLocaleID(Objects[Index]));
  end;

function TLanguageList.LoadDefaultNames : boolean;
var
  sl      : TStringList;
  s,sn    : string;
  rs      : TResourceStream;
  ss      : TLangCodeString;
  i       : integer;
begin
  sl:=TStringList.Create; Result:=false;
  if FLangName.IsEmpty then begin // load from resource
    Result:=FindResource(HInstance,'IDR_LANGUAGES',RT_RCDATA)<>0;
    if Result then begin
    // read list of supported languages from resource
      rs:=TResourceStream.Create(HInstance,'IDR_LANGUAGES',RT_RCDATA);
      sl.LoadFromStream(rs);
      rs.Free;
      end;
    end
  else begin
    s:=FPath+FLangName;
    Result:=FileExists(s);
    if Result then sl.LoadFromFile(s);
    end;
  if Result then begin
    Clear;
    AddObject(rsSystemDefault,nil);  // system language
    for i:=0 to sl.Count-1 do begin
      s:=Trim(sl[i]);
      if (length(s)>0) and (s[1]<>'#') then begin
        ss:=ReadNxtStr(s,'=');
        sn:=Trim(ReadNxtStr(s,'#'));
        if (length(sn)>0) and (length(ss)>0) then
          AddObject(sn,pointer(ShortLanguageNameToId(ss)));
        end;
      end;
    Sort;
    if Assigned(FMenu) then SetMenu(FMenu);
    end;
  sl.Free;
  end;

procedure TLanguageList.RemoveMenuItems;
begin
  { alle Menüeinträge ab der gespeicherten Position
    MenuSizeBefore wieder entfernen: }
  if Assigned(FMenu) then with FMenu do
    while Count>MenuSizeBefore do Items[Count-1].Free;
  end;

procedure TLanguageList.AddMenuItems;
{ Gegenstück zu RemoveMenuItems }
var
  i  : integer;
  mi : TMenuItem;
begin
  if Assigned(FMenu) then begin
    (* Änderung - keine Linie, wenn Menü leer *)
    if MenuSizeBefore>0 then
      FMenu.Add(NewLine); { mit einem Separator abtrennen }
    for i:=0 to Count-1 do begin
      mi:=NewItem(Strings[i],0,false,True,DoLangItemClick,0,'');
      with mi do begin
        RadioItem:=true;
        GroupIndex:=123;
        Tag:=i+1;
        Checked:=false;
        OnMeasureItem:=DoLangMeasureItem;
        end;
      FMenu.Add(mi);
      end;
    end;
  end;

procedure TLanguageList.SetMenu(Menu : TMenuItem);
begin
  if Assigned(FMenu) then RemoveMenuItems;
  FMenu:=Menu; { Property-zugehörige Variable setzen }
  MenuSizeBefore:=Menu.Count; { bisherige Menügröße speichern }
  AddMenuItems; { ab sofort bleibt das Menü aktuell }
  end;

function TLanguageList.GetLangIndex (const Value: TLangCodeString) : integer;
begin
  for Result:=0 to Count-1 do
     if ShortLanguageNameToId(Value)=TLocaleID(Objects[Result]) then Exit;
  Result:=-1;
  end;

procedure TLanguageList.SetLangCode (const Value: TLangCodeString);
var
  n : integer;
begin
  n:=GetLangIndex(Value);
  if n>=0 then begin
    if assigned(FMenu) then FMenu.Items[MenuSizeBefore+n].Checked:=true;
    FLangCode:=Value;
    FCurrentLanguage:=Strings[n];
    end;
  end;

procedure TLanguageList.DoLangItemClick(Sender : TObject);
begin
  if Assigned (FOnLangItemClick) then begin
    with (Sender as TMenuItem) do begin
      Checked:=true;
      if Tag>0 then FOnLangItemClick(FMenu,IdToShortLanguageName(TLocaleID(Objects[Tag-1])));
      end;
    end;
  end;

procedure TLanguageList.DoLangMeasureItem (Sender: TObject; ACanvas: TCanvas;
    var Width, Height: Integer);
begin
  if Assigned (FOnLangMeasureItem) then FOnLangMeasureItem(Sender,ACanvas,Width,Height);
  end;

function TLanguageList.LoadLanguageNames (LangCode : TLangCodeString) : boolean;
var
  sl      : TStringList;
  s,sn    : string;
  ss      : TLangCodeString;
  i,n     : integer;
begin
  if length(LangCode)>0 then begin
    LoadDefaultNames;
    if FLangName.IsEmpty then begin  // get supported languages from resource
      for i:=1 to Count-1 do Strings[i]:=dgettext('languages',Strings[i]); // translate
      Sort;
      if Assigned(FMenu) then SetMenu(FMenu);
      end
    else begin
      s:=FPath+'locale\'+LangCode+'\LC_MESSAGES\'+FLangName;
      if not FileExists(s) then s:=FPath+'locale\'+copy(LangCode,1,2)+'\LC_MESSAGES\'+FLangName;
      Result:=FileExists(s);  // localized language table found
      if Result then begin
        sl:=TStringList.Create;
        sl.LoadFromFile(s);
        for i:=0 to sl.Count-1 do begin
          s:=Trim(sl[i]);
          if (length(s)>0) and (s[1]<>'#') then begin
            ss:=ReadNxtStr(s,'=');
            n:=GetLangIndex(ss);
            sn:=Trim(ReadNxtStr(s,'#'));
            if (length(sn)>0) and (n>=0) then Strings[n]:=sn;
            end;
          end;
        Sort;
        if Assigned(FMenu) then SetMenu(FMenu);
        sl.Free;
        end;
      end;
    SetLangCode(LangCode);
    end
  else Result:=false;
  end;

{ ------------------------------------------------------------------- }
// GnuGetText ermittelt als System-Standard nicht die Sprache sondern die lokale
// Bsp: engl. System mit lokaler Einstellung "German" gibt:
//    GetSystemDefaultUILanguage                ==> 1033 = $409 = "English (US)"
//    GetUserDefaultLangID                      ==> 1031 from regional user settings
//    GetUserDefaultUILanguage                  ==> 1031 = $407 = "German (DE)"
function GetUserLang : TLangCodeString;
var
  pli : TLocaleID;
begin
  pli:=GetUserDefaultUILanguage;       // not available with Win98
  if pli=0 then pli:=GetUserDefaultLangID;
  Result:=IdToShortLanguageName(pli);
//  Result:=LangIdToCode(pli);
  end;

// Spracheinstellung laden und speichern
function ReadLanguageCode : TLangCodeString;
var
  s,si  : string;
  j     : integer;
  po    : boolean;

  // replace environment variable
  function ReplacePathPlaceHolder (const ps : string) : string;
  var
    n,k : integer;
    se,sv : string;
  begin
    Result:=ps;
    n:=1;
    repeat
      n:=PosEx('%',ps,n);
      if n>0 then begin
        k:=PosEx('%',ps,n+1);
        if k>0 then begin
          sv:=copy(ps,n+1,k-n-1);
          se:=GetEnvironmentVariable(sv);
          Result:=AnsiReplaceText(Result,'%'+sv+'%',se);
          n:=k+1;
          end;
        end;
      until n=0;
    end;

begin
  po:=false; si:=''; Result:='';
  for j:=1 to ParamCount do begin   // prüfe Befehlszeile
    s:=ParamStr(j);
    if (s[1]='/') or (s[1]='-') then begin
      delete (s,1,1);
      if ReadOptionValue(s,siLangOption) then Result:=s  // Sprache
      else if ReadOptionValue(s,siAltIni) then si:=ReplacePathPlaceHolder(s)  // anderer Ort für Ini-Datei
      else if CompareOption(s,siPortable) then begin
        po:=true;
        if length(si)=0 then si:=ExtractFilePath(Application.ExeName) // portable environment
        end;
      end;
    end;
  if length(si)>0 then begin
    if AnsiEndsText(PathDelim,si) then CfgName:=si+ExtractFilename(CfgName) // is path
    else begin
      s:=ExtractFilename(si);
      if po then s:=ChangeFileExt(s,'.'+CfgExt)
      else s:=ExtractFileName(CfgName);
      if ContainsFullPath(si) then si:=ExtractFilePath(si)
      else if po then begin
        if Pos(PathDelim,si)>0 then
          si:=IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+si
        else si:=ExtractFilePath(Application.ExeName);
        end
      else begin
        if Pos(PathDelim,si)>0 then si:=ExtractFilePath(ExpandFileName(si))
        else si:=ExtractFilePath(CfgName);
        end;
      CfgName:=IncludeTrailingPathDelimiter(si)+s;
      end
    end;
  LangFromCfg:=length(Result)=0;
  if LangFromCfg then begin    //aus Konfigurations-Datei
    with TMemIniFile.Create(CfgName) do begin
      Result:=ReadString(LangSekt,LangID,'');
      Free;
      end;
    end;
  end;

function GetLanguage : TLangCodeString;
begin
  Result:=ReadLanguageCode;     // from cfg file or command line
  if length(Result)=0 then Result:=GetUserLang;  // from system setting
  UserLangID:=ShortLanguageNameToID(Result);
  SelectedLanguage:=Result;
  end;

procedure SaveLanguage (NewLangCode : TLangCodeString);
begin
  if LangFromCfg then begin
    with TMemIniFile.Create(CfgName) do begin
      WriteString(LangSekt,LangID,NewLangCode);
      try
        UpdateFile;
      finally
        Free;
        end;
      end;
    end;
  end;

function GetLanguageHint : string;
begin
  Result:=rsLangRestart;
  end;

{ ------------------------------------------------------------------- }
// Spracheinstellung eines laufenden Programms ändern
function ChangeLanguage (const NewLangCode : TLangCodeString) : TLangCodeString;
var
  i : integer;
begin
  SelectedLanguage:=NewLangCode;
{$IFDEF RELEASE}
  SaveLanguage(NewLangCode);
{$EndIf}
  UseLanguage(NewLangCode);
  if length(NewLangCode)=0 then Result:=copy(GetCurrentLanguage,1,2)  // system default
  else Result:=NewLangCode;
  UserLangID:=ShortLanguageNameToID(Result);
  with Application do for i:=0 to ComponentCount-1 do if (Components[i] is TForm) then begin
    try ReTranslateComponent(Components[i]); except; end;
    end;
  end;

{ ------------------------------------------------------------------- }
// InitTranslation muss in der Projekt-Datei vor "Application.Initialize" stehen
procedure InitTranslation (const CfgDir,ConfigName : string; const Domains : array of string);
var
  i  : integer;
  sc : string;
begin
  if length(ConfigName)>0 then CfgName:=ConfigName
  else CfgName:=ChangeFileExt(PrgName,'.'+CfgExt);
  if IsRelativePath(CfgDir) then begin
    sc:=IncludeTrailingPathDelimiter(GetAppPath)+CfgDir;
    AppSubDir:=CfgDir;
    end
  else sc:=CfgDir;
  CfgName:=IncludeTrailingPathDelimiter(sc)+CfgName;
  for i:=0 to High(Domains) do AddDomainForResourceString(Domains[i]);
  UseLanguage(GetLanguage);
  end;

procedure InitTranslation (const Domains : array of string);
begin
  InitTranslation ('','',Domains);
  end;

procedure InitTranslation (const CfgDir : string; const Domains : array of string);
begin
  InitTranslation (CfgDir,'',Domains);
  end;

initialization
  IsVista:=(Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 6);
  SelectedLanguage:=''; // default: system language

end.
