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
          2.2 - June 2025: optional loading of language list from resource

          last modified: June 2025

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
  TLangCodeString = string[2];

  TLanguageMenuEvent = procedure(Sender : TObject; LangCode : TLangCodeString) of object;

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
    function LangCodeStringToCardinal (LangCode : TLangCodeString) : pointer;
    function CardinalToLangCodeString (Value : cardinal) : TLangCodeString;
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
procedure ChangeLanguage (NewLangCode : TLangCodeString);
function GetLanguageHint : string;
function LangIdToCode(id : integer) : TLangCodeString;

var
  SelectedLanguage   : TLangCodeString;
  LangFromCfg        : boolean;

implementation

uses
  Winapi.Windows, Vcl.Forms, Winapi.ShlObj, System.IniFiles, System.StrUtils,
  System.IOUtils, GnuGetText, UnitConsts, StringUtils, InitProg;

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
  FPath:=IncludeTrailingPathDelimiter(APath); FLangName:=Filename;
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
        Checked:=AnsiSameStr(FLangCode,CardinalToLangCodeString(cardinal(Objects[i])));
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
     if LangCodeStringToCardinal(Value)=Objects[Result] then Exit;
  Result:=-1;
  end;

procedure TLanguageList.SetLangCode (const Value: TLangCodeString);
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
// Einstellung (GetLocaleInfo) (korrigiert Sept. 2023)
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
  Result:=ReadLanguageCode;
  if length(Result)=0 then Result:=GetUserLang;
  SelectedLanguage:=Result;
  end;

procedure SaveLanguage (NewLangCode : TLangCodeString);
begin
  SelectedLanguage:=NewLangCode;
  if LangFromCfg then begin
    with TMemIniFile.Create(CfgName) do begin
      WriteString(LangSekt,LangID,SelectedLanguage);
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
procedure ChangeLanguage (NewLangCode : TLangCodeString);
var
  i : integer;
begin
  SaveLanguage(NewLangCode);
  UseLanguage(NewLangCode);
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
  SelectedLanguage:=''; // default: system language

end.
