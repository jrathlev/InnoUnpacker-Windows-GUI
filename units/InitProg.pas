(* Delphi-Unit
  Subroutines to initialize an application  (paths, version, etc.)
  ================================================================

  © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

  The contents of this file may be used under the terms of the
  Mozilla Public License ("MPL") or
  GNU Lesser General Public License Version 2 or later (the "LGPL")

  Software distributed under this License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  Version 1.0 - Nov. 2011
  last modified: Jan. 2020
  *)
  
unit InitProg;

interface

uses System.Classes, System.SysUtils, Vcl.ComCtrls, WinApiUtils;

// Ermittelt die Standardpfade: Anwendungsdaten, Eigene Dateien, Programm
function GetAppPath : string;
function GetAppSubPath (const SubDir : string; CreateAppDir : boolean = true) : string;
function GetUserPath : string;
function GetProgPath : string;

procedure InitPaths (var AppPath,UserPath,ProgPath : string; CreateAppDir : boolean = true); overload;
procedure InitPaths (var AppPath,UserPath : string); overload;
procedure InitPaths (var AppPath : string); overload;

// Ermittelt den Namen und Version/Erstellungsdatum aus den in der Exe-Datei des Prorgamms
// gespeicherten Angaben. Wenn diese fehlen, werden die Angaben ProgName,Vers,CopRgt
// verwendet
procedure InitVersion (const ProgName,Vers,CopRgt : string; VersLevel,ShowLevel : integer;
                       var ProgVersName,ProgVersDate : string); overload;
procedure InitVersion (const ProgName,Vers,CopRgt : string; VersLevel,ShowLevel : integer;
                       var ProgVersName,ProgVers,ProgVersDate : string); overload;

function GetVersion (ShowLevel : integer) : string;
function GetCopyRight (const Year : string) : string;
function GetLongCopyRight (const Year : string) : string;

procedure FixToolBar(tb: TToolBar);

var
  PrgPath,PrgName,
  AppSubDir        : string;
  VersInfo         : TFileVersionInfo;       // Programm- und Versionsinfo

implementation

uses Vcl.Forms, WinShell, Winapi.ShlObj, UnitConsts;

{ ------------------------------------------------------------------- }
// Standardpfade
function GetAppPath : string;
begin
  Result:=GetDesktopFolder(CSIDL_APPDATA);
  end;

function GetAppSubPath (const SubDir : string; CreateAppDir : boolean) : string;
begin
  Result:=GetAppPath;
  if length(Result)>0 then begin
    Result:=IncludeTrailingPathDelimiter(Result)+SubDir;  // Pfad zu Anwendungsdaten
    if CreateAppDir then begin
      if not ForceDirectories(Result) then Result:=PrgPath;
      end;
    end
  else Result:=PrgPath;
  end;

function GetUserPath : string;
begin
  Result:=GetDesktopFolder(CSIDL_PERSONAL);
  end;

function GetProgPath : string;
begin
  Result:=GetDesktopFolder(CSIDL_PROGRAM_FILES);
  end;

procedure InitPaths (var AppPath,UserPath,ProgPath : string; CreateAppDir : boolean);
begin
  AppPath:=GetAppSubPath(AppSubDir,CreateAppDir);
  UserPath:=IncludeTrailingPathDelimiter(GetUserPath);
  ProgPath:=IncludeTrailingPathDelimiter(GetDesktopFolder(CSIDL_PROGRAM_FILES));
  end;

procedure InitPaths (var AppPath,UserPath : string);
var
  s : string;
begin
  InitPaths (AppPath,UserPath,s);
  end;

procedure InitPaths (var AppPath : string);
var
  s,t : string;
begin
  InitPaths (AppPath,t,s);
  end;

function GetVersion (ShowLevel : integer) : string;
var
  i : integer;
begin
  Result:=VersInfo.Version;
  // Versionsnr. anpassen
  for i:=3 downto ShowLevel do Result:=ChangeFileExt(Result,'');
  Result:=rsVersion+' '+Result;
  end;

procedure InitVersion (const ProgName,Vers,CopRgt : string; VersLevel,ShowLevel : integer;
                       var ProgVersName,ProgVers,ProgVersDate : string); overload;
begin
// Versions-Info
  with VersInfo do begin
    if GetFileVersion (Application.ExeName,VersInfo) then begin
      if length(InternalName)=0 then InternalName:=ProgName;
      end
    else begin
      InternalName:=ProgName; Version:=Vers; Copyright:=CopRgt;
      end;
    Comments:=GetVersion(ShowLevel);
    ProgVersName:=InternalName;
    ProgVers:=' ('+Comments+')';
    end;
  ProgVersDate:=DateToStr(FileDateToDateTime(FileAge(Application.ExeName)));
  end;

procedure InitVersion (const ProgName,Vers,CopRgt : string; VersLevel,ShowLevel : integer;
                       var ProgVersName,ProgVersDate : string);
var
  sn,sv : string;
begin
  InitVersion(ProgName,Vers,CopRgt,VersLevel,ShowLevel,sn,sv,ProgVersDate);
  ProgVersName:=sn+sv;
  end;

{ ------------------------------------------------------------------- }
// Workaround for corrupted dropdown toolbuttons when using GnuGetText
// see: http://sourceforge.net/tracker/index.php?func=detail&aid=902470&group_id=74086&atid=539908
procedure FixToolBar(tb: TToolBar);
var
  i : integer;
begin
  with tb do for i:=0 to ButtonCount-1 do with Buttons[i] do begin
    if (Style = tbsDropDown) then begin
      Style := tbsButton;
      Style := tbsDropDown;
      end;
    end;
  end;

function GetCopyRight (const Year : string) : string;
begin
  Result:='© '+Format(rsCopyRight,[Year]);
  end;

function GetLongCopyRight (const Year : string) : string;
begin
  Result:='© '+Format(rsLongCopyRight,[Year]);
  end;

initialization
  PrgPath:=ExtractFilePath(Application.ExeName);
  PrgName:=ExtractFileName(ChangeFileExt(Application.ExeName,''));
  AppSubDir:='';
end.

