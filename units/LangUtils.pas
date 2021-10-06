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
          last changed: Jan. 2020

  Hinweise zur Verwendung:
  ========================
  1. in der Projekt-Datei (*.dpr):
     -----------------------------
    ...
    uses
      GnuGetText in '..\..\(..\)Bibliotheken\Units\GnuGetText.pas',
      LangUtils in '..\..\(..\)Bibliotheken\Units\LangUtils.pas',
      ...
    begin
      TP_GlobalIgnoreClass(TFont);
      TP_GlobalIgnoreClassProperty(TMdiForm,'Caption');
      // Subdirectory in AppData for user configuration files and supported languages
      InitTranslation('CfgDir','CfgName',['Delphi10','Indy10','more domains..']);
      ...
    dabei sind:  CfgDir  - Unterverzeichnis für die Konfigurations-Dateien (ini,cfg)
                           in "Anwendungsdaten  oder abdolutes Verzeichnis
                 CfgName - Name der Cfg-Datei zum Speichern der Spracheinstellung
                           (leer: Name der Anwendung)
  2. im Haupt-Formular:
     ------------------
    uses ..., LangUtils, ...
    ...
    private
      ...
      Languages        : TLanguageList;
      procedure SetLanguageClick(Sender : TObject; Language : TLangCodeString);
      ...
    begin
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
          SaveLanguage(Language);
          InfoDialog('',GetLanguageHint,CursorPos);
          end;
        end;
      ...

    dabei sind: TLangCodeString - Länder-Code (2 Buchstaben)
                itmLang         - Menü-Item im Hauptmenü
                                  Die Sprachauswahl wird automatisch als
                                  Untermenü angehängt
  *)

unit LangUtils;

interface

uses System.Classes, System.SysUtils, Vcl.Menus;

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
  TLangCodeString = string[2];

  TLanguageMenuEvent = procedure(Sender : TObject; LangCode : TLangCodeString) of object;

  TLanguageList = class (TStringList)
  private
    FMenu : TMenuItem;
    MenuSizeBefore : integer;
    FLangCode : TLangCodeString;
    FOnLangItemClick : TLanguageMenuEvent;
    FCurrentLanguage,
    FPath,FName : string;
    procedure AddMenuItems;
    procedure RemoveMenuItems;
    function GetLangCode (Index : integer) : TLangCodeString;
    procedure SetMenu(Menu : TMenuItem);
    function LangCodeStringToCardinal (LangCode : TLangCodeString) : pointer;
    function CardinalToLangCodeString (Value : cardinal) : TLangCodeString;
    function LoadDefaultNames : boolean;
  protected
    procedure DoLangItemClick(Sender : TObject); virtual;
    function GetLangIndex (Value: TLangCodeString) : integer;
    procedure SetLangCode (Value: TLangCodeString);
  public
    constructor Create (const APath,Filename : string);
    destructor Destroy; override;
    function LoadLanguageNames (LangCode : TLangCodeString) : boolean;
    property CurrentLanguage : string read FCurrentLanguage;
    property SelectedLanguageCode : TLangCodeString read FLangCode write SetLangCode;
    property LanguageCode[Index : integer] : TLangCodeString read GetLangCode;
    property Menu : TMenuItem read FMenu write SetMenu;
    property OnLanguageItemClick : TLanguageMenuEvent read FOnLangItemClick write FOnLangItemClick;
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
procedure ChangeLanguage (NewLangCode : TLangCodeString);
function GetLanguageHint : string;
function LangIdToCode(id : integer) : TLangCodeString;

var
  CfgName            : string;
  SelectedLanguage   : TLangCodeString;
  LangFromCfg        : boolean;

implementation

uses
  Winapi.Windows, Vcl.Forms, Winapi.ShlObj, System.IniFiles, GnuGetText, IniFileUtils,
  UnitConsts, StringUtils, WinShell, InitProg, Placeholders;

{ ------------------------------------------------------------------- }
(* Name enthält vollständigen Pfad *)
function ContainsFullPath (const Name : string) : boolean;
begin
  if length(Name)>0 then Result:=(Name[1]=PathDelim) or (pos(DriveDelim,Name)>0)
  else Result:=false;
  end;

{ ------------------------------------------------------------------- }
constructor TLanguageList.Create (const APath,Filename : string);
begin
  inherited Create;
//  Sorted:=true;
  MenuSizeBefore:=0; FLangCode:='';
  FPath:=IncludeTrailingPathDelimiter(APath); FName:=Filename;
  LoadDefaultNames;  // load default language table
  end;

destructor TLanguageList.Destroy;
begin
  inherited Destroy;
  end;

function TLanguageList.LangCodeStringToCardinal (LangCode : TLangCodeString) : pointer;
begin
  if length(LangCode)<2 then Result:=nil
  else Result:=pointer(ord(LangCode[2]) shl 8 + ord(LangCode[1]));
//  BytesToCardinal(0,0,ord(LangCode[2]),ord(LangCode[1])));
  end;

function TLanguageList.CardinalToLangCodeString (Value : cardinal) : TLangCodeString;
begin
  if Value=0 then Result:=''
  else Result:=AnsiChar(LongRec(Value).Bytes[0])+AnsiChar(LongRec(Value).Bytes[1]);
  end;

function TLanguageList.GetLangCode (Index : integer) : TLangCodeString;
begin
  Result:=CardinalToLangCodeString(cardinal(Objects[Index]));
  end;

function TLanguageList.LoadDefaultNames : boolean;
var
  sl      : TStringList;
  s,sn    : string;
  ss      : TLangCodeString;
  i       : integer;
begin
  s:=FPath+FName;
  Result:=FileExists(s);
  if Result then begin
    sl:=TStringList.Create;
    sl.LoadFromFile(s);
    Clear;
    AddObject(rsSystemDefault,LangCodeStringToCardinal(''));  // system language
    for i:=0 to sl.Count-1 do begin
      s:=Trim(sl[i]);
      if (length(s)>0) and (s[1]<>'#') then begin
        ss:=ReadNxtStr(s,'=');
        sn:=Trim(ReadNxtStr(s,'#'));
        if (length(sn)>0) and (length(ss)>0) then
          AddObject(sn,pointer(LangCodeStringToCardinal(ss)));
        end;
      end;
    sl.Free;
    Sort;
    if Assigned(FMenu) then SetMenu(FMenu);
    end;
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
        Checked:=AnsiSameStr(FLangCode,CardinalToLangCodeString(cardinal(Objects[i])));
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

function TLanguageList.GetLangIndex (Value: TLangCodeString) : integer;
begin
  for Result:=0 to Count-1 do
     if LangCodeStringToCardinal(Value)=Objects[Result] then Exit;
  Result:=-1;
  end;

procedure TLanguageList.SetLangCode (Value: TLangCodeString);
var
  n : integer;
begin
  n:=GetLangIndex(Value);
  //    if AnsiSameStr(Value,CardinalToLangCodeString(Objects[i])) then Break;
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
      if Tag>0 then FOnLangItemClick(FMenu,CardinalToLangCodeString(cardinal(Objects[Tag-1])));
      end;
    end;
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
    s:=FPath+'locale\'+LangCode+'\LC_MESSAGES\'+FName;
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
      sl.Free;
      Sort;
      if Assigned(FMenu) then SetMenu(FMenu);
      end;
    SetLangCode(LangCode);
    end
  else Result:=false;
  end;

{ ------------------------------------------------------------------- }
// GnuGetText ermittelt als System-Standard nicht die Sprache sondern die lokale
// Einstellung (GetLocaleInfo)
// Bsp: engl. System mit lokaler Einstellung "German" gibt:
//    GetLocaleInfo oder GetUserDefaultLangID   ==> de_DE
//    GetUserDefaultUILanguage                  ==> 1033 = $409 = "English (US)"
// Die nachfolgenden Routinen umgehen diesen Fehler
function GetUserLang : TLangCodeString;
var
  pli : integer;
begin
  pli:=GetUserDefaultUILanguage;       // not available with Win98
  if pli=0 then pli:=GetUserDefaultLangID;
  Result:=LangIdToCode(pli);
  end;

function LangIdToCode(id : integer) : TLangCodeString;
begin
  case id and $3FF of
  $02 : Result:='bg';
  $05 : Result:='cs';
  $06 : Result:='da';
  $07 : Result:='de';
  $08 : Result:='el';
  $09 : Result:='en';
  $0a : Result:='es';
  $0b : Result:='fi';
  $0c : Result:='fr';
  $0e : Result:='hu';
  $10 : Result:='it';
  $13 : Result:='nl';
  $14 : Result:='no';
  $15 : Result:='pl';
  $16 : Result:='pt';
  $18 : Result:='ro';
  $19 : Result:='ru';
  $1a : Result:='hr';
  $1b : Result:='sk';
  $1c : Result:='sq';
  $1d : Result:='sv';
  $1f : Result:='tr';
  $22 : Result:='uk';
  $24 : Result:='sl';
  $25 : Result:='et';
  $26 : Result:='lv';
  $27 : Result:='lt';
  else Result:='';
    end;
  end;

// Spracheinstellung laden und speichern
function ReadLanguageCode : TLangCodeString;
var
  s,si  : string;
  j     : integer;
  ok    : boolean;
begin
  ok:=false; si:=''; Result:='';
  for j:=1 to ParamCount do if not ok then begin   // prüfe Befehlszeile
    s:=ParamStr(j);
    if (s[1]='/') or (s[1]='-') then begin
      delete (s,1,1);
      if ReadOptionValue(s,siLangOption) then Result:=s  // Sprache
      else if ReadOptionValue(s,siAltIni) then si:=ReplacePathPlaceHolder(s)  // anderer Ort für Ini-Datei
      else if CompareOption(s,siPortable) then si:=ExtractFilePath(Application.ExeName) // portable environment
      end;
    end;
  if length(si)>0 then begin
    if ContainsFullPath(si) then begin
  //    si:=ExpandFileName(si);
      if (ExtractFileExt(si)<>'') then si:=ExtractFilePath(si);
      end
    else if Pos(PathDelim,si)>0 then si:=ExtractFilePath(ExpandFileName(si))
    else si:='';  // nur alternative Ini-Datei ohne Pfad
    if length(si)>0 then CfgName:=IncludeTrailingPathDelimiter(si)+ExtractFilename(CfgName);
    end;
  LangFromCfg:=length(Result)=0;
  if LangFromCfg then begin    //aus Konfigurations-Datei
    with TIniFile.Create(CfgName) do begin
      Result:=ReadString(LangSekt,LangID,'');
      Free;
      end;
    end;
  end;

function GetLanguage : TLangCodeString;
begin
  Result:=ReadLanguageCode;
  if length(Result)=0 then Result:=GetUserLang;
  SelectedLanguage:=Result;
  end;

procedure SaveLanguage (NewLangCode : TLangCodeString);
begin
  SelectedLanguage:=NewLangCode;
  if LangFromCfg then begin
    WriteStringToIniFile(CfgName,LangSekt,LangID,SelectedLanguage);
    UpdateIniFile(CfgName);
    end;
  end;

function GetLanguageHint : string;
begin
  Result:=rsLangRestart;
  end;

{ ------------------------------------------------------------------- }
// Spracheinstellung eines laufenden Programms ändern
procedure ChangeLanguage (NewLangCode : TLangCodeString);
var
  i : integer;
begin
  SelectedLanguage:=NewLangCode;
  if LangFromCfg then begin
    WriteStringToIniFile(CfgName,LangSekt,LangID,SelectedLanguage);
    UpdateIniFile(CfgName);
    end;
  UseLanguage(SelectedLanguage);
  with Application do for i:=0 to ComponentCount-1 do if (Components[i] is TForm) then
    ReTranslateComponent(Components[i]);
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
  CfgName:='';
  SelectedLanguage:=''; // default: system language

end.
