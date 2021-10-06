(* Delphi 10 Unit
   several subroutines for Windows Desktop and Shell

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Vers. 1 - Sep. 2002
   Vers. 2 - April 2016: IFileOperation added
   last updated: June 2020
   *)

unit WinShell;

interface

uses Winapi.Windows, System.SysUtils, Winapi.ShlObj, System.Classes;

const
{ Werte für nFolder in SHGetSpecialFolderLocation
  # = für Windows 9x, NT und 2000
  * = vermutlich nur Windows 98 und 2000
  + = vermutlich nur Windows NT und 2000
  ? = vermutlich nur Windows 2000 }
{ Deklarationen: siehe ShlObj
  CSIDL_DESKTOP                       = $0000;    (* # User\Desktop *)
  CSIDL_PROGRAMS                      = $0002;    (* # User\Startmenü\Programme *)
  CSIDL_CONTROLS                      = $0003;
  CSIDL_PRINTERS                      = $0004;
  CSIDL_PERSONAL                      = $0005;    (* # pers. Verz. *)
  CSIDL_FAVORITES                     = $0006;    (* # User\Favoriten *)
  CSIDL_STARTUP                       = $0007;    (* # User\Startmenü\Programme\Autostart *)
  CSIDL_RECENT                        = $0008;    (* # User\Recent *)
  CSIDL_SENDTO                        = $0009;    (* # User\SendTo *)
  CSIDL_BITBUCKET                     = $000A;
  CSIDL_STARTMENU                     = $000B;    (* # User\Startmenü *)
  CSIDL_DESKTOPDIRECTORY              = $0010;    (* # User\Desktop *)
  CSIDL_DRIVES	                      = $0011;
  CSIDL_NETWORK                       = $0012;
  CSIDL_NETHOOD                       = $0013;    (* # User\Netzwerkumgebung *)
  CSIDL_FONTS                         = $0014;    (* # Windows\Fonts *)
  CSIDL_TEMPLATES                     = $0015;    (* # User\Vorlagen *)
  CSIDL_COMMON_STARTMENU              = $0016;    (* + All Users\Startmenü *)
  CSIDL_COMMON_PROGRAMS               = $0017;    (* + All Users\Programme *)
  CSIDL_COMMON_STARTUP                = $0018;    (* + All Users\Autostart *)
  CSIDL_COMMON_DESKTOPDIRECTORY       = $0019;    (* + All Users\Desktop *)
  CSIDL_APPDATA                       = $001A;    (* # User\Anwendungsdaten *)
  CSIDL_PRINTHOOD                     = $001B;    (* # User\Druckumgebung *)
  CSIDL_LOCALAPPDATA                  = $001C;    (* * User\Lokale Einstellungen\Anwendungsdaten *)
  CSIDL_COMMON_FAVORITES              = $001F;    (* ? All Users\Favoriten *)
  CSIDL_CACHE                         = $0020;    (* + User\Lokale Einstellungen\Temporary Internet Files *)
  CSIDL_COOKIES                       = $0021;    (* + User\Cookies *)
  CSIDL_HISTORY                       = $0022;    (* + User\Lokale Einstellungen\Verlauf *)
  CSIDL_COMMON_APPDATA                = $0023;    (* ? All Users\Anwendungsdaten *)
  CSIDL_WINDIR	                      = $0024;    (* ? Windows-Verzeichnis *)
  CSIDL_SYSTEMDIR                     = $0025;    (* ? Windows-Systemverzeichnis *)
  CSIDL_MYPICTURES                    = $0027;    (* ? pers. Verz.\Eigene Bilder *)
  CSIDL_USERPROFILE                   = $0028;    (* ? User *)
  CSIDL_SHAREFILESDIR                 = $002B;    (* ? Programm-Verzeichnis *)
  CSIDL_COMMON_TEMPLATES              = $002D;    (* ? All Users\Vorlagen *)

// neue Parameter aus ShFolder.h  (SDK für Win2003), andere Werte siehe ShlObj.pas
  CSIDL_LOCAL_APPDATA             = $001C;     // non roaming, user\Local Settings\Application Data
  CSIDL_COMMON_APPDATA            = $0023;     // All Users\Application Data
  CSIDL_WINDOWS                   = $0024;     // GetWindowsDirectory()
  CSIDL_SYSTEM                    = $0025;     // GetSystemDirectory()
  CSIDL_PROGRAM_FILES             = $0026;     // C:\Program Files
  CSIDL_MYPICTURES                = $0027;     // My Pictures, new for Win2K
  CSIDL_PROGRAM_FILES_COMMON      = $002b;     // C:\Program Files\Common
  CSIDL_COMMON_DOCUMENTS          = $002e;     // All Users\Documents
  CSIDL_RESOURCES                 = $0038;     // %windir%\Resources\, For theme and other windows resources.
  CSIDL_RESOURCES_LOCALIZED       = $0039;     // %windir%\Resources\<LangID>, for theme and other windows specific resources.
  }
  {ulFlags in TBrowseInfo}
  {$EXTERNALSYM BIF_NEWDIALOGSTYLE}
  BIF_NEWDIALOGSTYLE          =   $0040;   // Use the new dialog layout with the ability to resize
  {$EXTERNALSYM BIF_NONEWFOLDERBUTTON}
  BIF_NONEWFOLDERBUTTON       =   $0200;   // Do not add the "New Folder" button to

  // fehlt in ShlObj - siehe ActiveX
  IID_IPersistFile: TGUID = '{0000010b-0000-0000-C000-000000000046}';

type
  TTaskbarPos = record
    Edge : integer;
    Rect : TRect;
    end;

// see function GetProgramFolder
  TProgramFolder = (pfProgramFiles,pfCommonProgramFiles,pfProgramFiles86,pfCommonProgramFiles86,
    pfProgramFiles64,pfCommonProgramFiles64);

// exe file types
  TFileExeType = (etError, etLink, etMsDos, etWin16, etWin32Gui, etWin32Con);

(*   IID_ITaskbarList: TGUID = '{56FDF342-FD6D-11d0-958A-006097C9A090}';
  IID_ITaskbarList2: TGUID = '{602D4995-B13A-429b-A66E-1935E44F4317}';
  IID_ITaskbarList3: TGUID = '{ea1afb91-9e28-4b86-90e9-9e9f8a5eefaf}';

type
 TBPF = (TBPF_NOPROGRESS = 0,
          TBPF_INDETERMINATE = 1,
          TBPF_NORMAL = 2,
          TBPF_ERROR = 4,
          TBPF_PAUSED = 8);
  TBATF = (TBATF_USEMDITHUMBNAIL = 1,
          TBATF_USEMDILIVEPREVIEW = 2);

  ITaskbarList = interface(IUnknown)
    [IID_ITaskbarList]
    function HrInit : HResult; stdcall;
    function AddTab(hWndOwner : HWND) : HResult; stdcall;
    function DeleteTab(hWndOwner : HWND) : HResult; stdcall;
    function ActivateTab(hWndOwner : HWND) : HResult; stdcall;
    function SetActiveAlt(hWndOwner : HWND) : HResult; stdcall;
    end; { ITaskbarList }

  ITaskbarList2 = interface(ITaskbarList)
    [IID_ITaskbarList2]
    function MarkFullscreenWindow(wnd : HWND; fFullscreen : bool) : HResult; stdcall;
    end;

  ITaskbarList3 = interface (ITaskbarList2)
   [IID_ITaskbarList3]
   function SetProgressValue (hWnd: HWND; ullCompleted: int64; ullTotal: int64): HResult; stdcall;
   function SetProgressState (hWnd: HWND; tbpFlags: TBPF): HResult; stdcall;
   function RegisterTab (hwndTab: HWND; hwndMDI: HWND): HResult; stdcall;
   function UnregisterTab (hwndTab: HWND): HResult; stdcall;
   function SetTabOrder (hwndTab: HWND; hwndInsertBefore: HWND): HResult; stdcall;
   function SetTabActive (hwndTab: HWND; hwndMDI: HWND; tbatFlags: TBATF): HResult; stdcall;
   function ThumbBarAddButtons (hWnd: HWND; cButtons: integer; pButtons: pointer): HResult; stdcall;
   function ThumbBarUpdateButtons (hWnd: HWND; cButtons: cardinal; pButtons: pointer): HResult; stdcall;
   function ThumbBarSetImageList (hWnd: HWND; himl: pointer): HResult; stdcall;
   function SetOverlayIcon (hWnd: HWND; hIcon: HICON; pszDescription: PWideChar): HResult; stdcall;
   function SetThumbnailTooltip (hWnd: HWND; pszTip: PWideChar): HResult; stdcall;
   function SetThumbnailClip (hWnd: HWND; prcClip: PRect): HResult; stdcall;
   end;      *)

{ ---------------------------------------------------------------- }
(* Verzeichnisse auf Desktop oder im Startmenü erstellen *)
function NewDesktopFolder (Foldername : string;
                           Typ        : integer): boolean;

(* Verzeichnisnamen aus Desktop oder Startmenü *)
function GetDesktopFolder (Typ : integer) : string;
function GetKnownFolder (rfId : TGUID) : string;
function GetProgramFolder (pfType : TProgramFolder) : string;

function GetPersonalFolder : string;
function GetAppDataFolder : string;

procedure RefreshDesktop;

{ Dialog Verzeichnisauswahl }
function GetDirectory (Title      : string;
                       RootTyp    : integer;
                       var Dir    : string;
                       var ImgNdx : integer) : boolean;

function MakeLink (const LinkObj,ProgName,IconLocation,Arg,WorkDir,Desc: string;
                   IconIndex : integer; RunAs : boolean = false) : HResult; overload;
function MakeLink (const LinkObj,ProgName,IconLocation,Arg,WorkDir,Desc: string;
                   RunAs : boolean = false) : HResult; overload;
function MakeLink (const LinkObj,ProgName,Arg,WorkDir,Desc: string;
                   RunAs : boolean = false) : HResult; overload;

function GetLink (const LinkObj : string; var ProgName,Arg,WorkDir,Desc: string;
                  var RunAs : boolean) : HResult; overload;
function GetLink (const LinkObj : string; var ProgName,Arg,WorkDir,Desc: string) : boolean; overload;

{ ---------------------------------------------------------------- }
// Icon in Taskbar aufnehmen
function TaskBarAddIcon (WinHandle  : HWnd;
                         UserId,MessageID  : integer;
                         IconHandle : HIcon;
                         const Title,Text,Tip : string;
                         PlaySound  : boolean = true) : boolean;

// Standard-Tip ändern
function TaskBarChangeTip (WinHandle  : HWnd;
                           UserId     : integer;
                           const Tip : string;
                           PlaySound  : boolean = true) : boolean;

// Balloon-Tipp löschen
function TaskBarChangeBalloonTip (WinHandle  : HWnd;
                                  UserId     : integer;
                                  const Title,Text : string;
                                  PlaySound  : boolean = true) : boolean;

// Icon in Taskbar ändern
function TaskBarChangeIcon (WinHandle  : HWnd;
                            UserId     : integer;
                            IconHandle : HIcon;
                            const Tip  : string;
                            PlaySound  : boolean = true) : boolean; overload;

function TaskBarChangeIcon (WinHandle  : HWnd;
                            UserId     : integer;
                            IconHandle : HIcon;
                            PlaySound  : boolean = true) : boolean; overload;

// Icon aus Taskbar entfernen
function TaskBarRemoveIcon (WinHandle  : HWnd;
                            UserId     : integer) : boolean;

// Position der Taskbar ermitteln
function TaskBarGetPos : TTaskbarPos;

//  Returns focus to the taskbar notification area
function TaskBarSetFocus : boolean;

{ ---------------------------------------------------------------- }
// Shell file operations using IFileOperation
function IShellCopyFiles (WinHandle         : HWnd;
                          const Source,Dest : string;
                          Silent            : boolean = false) : HResult;

function IShellMoveFiles (WinHandle         : HWnd;
                          const Source,Dest : string;
                          Silent            : boolean = false) : HResult;

function IShellDeleteFiles (WinHandle    : HWnd;
                            const Source : string;
                            Recycle      : boolean;
                            NoPrompt     : boolean = false) : HResult;

function IShellDeleteDir (WinHandle    : HWnd;
                          const Source : string;
                          Recycle      : boolean;
                          NoPrompt     : boolean = false) : HResult;

{ ---------------------------------------------------------------- }
// Shell file operations using SHFileOperation
function ShellCopyFiles (WinHandle        : HWnd;
                         const Source,Dest,Hint : string;
                         Silent           : boolean = false) : integer;

function ShellMoveFiles (WinHandle        : HWnd;
                         const Source,Dest,Hint : string;
                         Silent           : boolean = false) : integer;

function ShellDeleteFiles (WinHandle   : HWnd;
                           const Source,Hint : string;
                           Recycle     : boolean = true;
                           NoPrompt    : boolean = false) : integer;

function ShellDeleteAll (WinHandle   : HWnd;
                         const Source,Hint : string;
                         Recycle     : boolean = true;
                         NoPrompt    : boolean = false) : integer;

{ ---------------------------------------------------------------- }
// Get Desktop system parameters
function GetClientArea : TRect;

{ ---------------------------------------------------------------- }
// Get file information
function IsConsoleApp (const Filename : string) : boolean;

{ ---------------------------------------------------------------- }
// Windows 7 Bibliotheken
function GetShellLibraryforLibrary(const LibraryName: String; grfMode: DWORD; var ppv: IShellLibrary): Boolean;
function GetLibraryFileSystemFolders(const LibraryName: String; Folders: TStrings): Boolean;

{ ---------------------------------------------------------------- }
implementation

uses Winapi.ActiveX, Winapi.Shellapi, Winapi.KnownFolders;

{ ------------------------------------------------------------------- }
{ ---------------------------------------------------------------- }
(* Windows-Desktop *)
(* Verzeichnisse auf Desktop oder im Startmenü erstellen *)
function NewDesktopFolder (Foldername : string;
                           Typ        : integer): boolean;
var
  pidl          : PItemIDList;
  ProgramPath   : PChar;
begin
  result := False;
  if SUCCEEDED(SHGetSpecialFolderLocation(0,Typ,pidl)) then begin
    ProgramPath := StrAlloc(max_path);
    SHGetPathFromIDList(pidl, ProgramPath);
    SetLastError(0);
    CreateDirectory(PChar(ProgramPath + '\\' + Foldername), nil );
    if (GetLastError()=0) or (GetLastError()=ERROR_ALREADY_EXISTS) then result := True;
    StrDispose(ProgramPath);
    end;
  end;

(* Verzeichnisnamen aus Desktop oder Startmenü *)
function GetDesktopFolder (Typ : integer) : string;
var
  pidl          : PItemIDList;
  FolderPath    : PChar;
  pMalloc       : IMalloc;
begin
  pidl:=nil;
  SHGetMalloc(pMalloc);
  if SUCCEEDED(SHGetSpecialFolderLocation(0,Typ,pidl)) then begin
    FolderPath := StrAlloc(max_path);
    SHGetPathFromIDList(pidl,FolderPath);
    SetLastError(0);
    Result:=FolderPath;
    StrDispose(FolderPath);
    end
  else Result:='';
  if pidl<>nil then pMalloc.Free(pidl);
  pMalloc._Release;
  end;

function GetKnownFolder (rfId : TGUID) : string;  // available since Vista
var
  ppszPath : PWideChar;
begin
  Result:='';
  if (Win32Platform=VER_PLATFORM_WIN32_NT) and (Win32MajorVersion>=6) and
      SUCCEEDED(SHGetKnownFolderPath(rfId,0,0,ppszPath)) then begin
    try
      Result:=ppszPath;
    finally
      CoTaskMemFree(ppszPath);
      end;
    end
  end;

function GetProgramFolder (pfType : TProgramFolder) : string;
begin
  case pfType of
  pfProgramFiles86 : begin
    Result:=GetEnvironmentVariable('ProgramFiles(x86)'); // get from environment
    if length(Result)=0 then Result:=GetDesktopFolder(CSIDL_PROGRAM_FILES);
    end;
  pfProgramFiles64 : begin
    Result:=GetEnvironmentVariable('ProgramW6432'); // get from environment
    if length(Result)=0 then Result:=GetDesktopFolder(CSIDL_PROGRAM_FILES);
    end;
  pfCommonProgramFiles86 : begin
    Result:=GetEnvironmentVariable('CommonProgramFiles(x86)'); // get from environment
    if length(Result)=0 then Result:=GetDesktopFolder(CSIDL_PROGRAM_FILES_COMMON);
    end;
  pfCommonProgramFiles64 : begin
    Result:=GetEnvironmentVariable('CommonProgramW6432'); // get from environment
    if length(Result)=0 then Result:=GetDesktopFolder(CSIDL_PROGRAM_FILES_COMMON);
    end;
  pfCommonProgramFiles : Result:=GetDesktopFolder(CSIDL_PROGRAM_FILES_COMMON);
  else Result:=GetDesktopFolder(CSIDL_PROGRAM_FILES);
    end;
  end;

function GetPersonalFolder : string;
begin
  Result:=GetDesktopFolder(CSIDL_PERSONAL);
  end;

function GetAppDataFolder : string;
begin
  Result:=GetDesktopFolder(CSIDL_APPDATA);
  end;

{ ---------------------------------------------------------------- }
procedure RefreshDesktop;
var
  pidl          : PItemIDList;
  pMalloc       : IMalloc;
begin
  pidl:=nil;
  SHGetMalloc(pMalloc);
  SHGetSpecialFolderLocation(0,CSIDL_DESKTOP,pidl);
  SHChangeNotify(SHCNE_ALLEVENTS,SHCNF_IDLIST,pidl,nil);
  if pidl<>nil then pMalloc.Free(pidl);
  pMalloc._Release;
  end;

{ ---------------------------------------------------------------- }
{ Dialog Verzeichnisauswahl }
function GetDirectory (Title      : string;
                       RootTyp    : integer;
                       var Dir    : string;
                       var ImgNdx : integer) : boolean;
var
  bi            : TBROWSEINFO;
  lpBuffer      : PChar;
  pidlPrograms,
  pidlBrowse    : PItemIDList;
  ok            : boolean;
  pMalloc       : IMalloc;
begin
  pidlPrograms:=nil; pidlBrowse:=nil;
  SHGetMalloc(pMalloc);
  Result:=false;
  ok:=(SUCCEEDED(SHGetSpecialFolderLocation(getactivewindow,RootTyp,pidlPrograms)));
  if not ok then ok:=(SUCCEEDED(SHGetSpecialFolderLocation(getactivewindow,CSIDL_DESKTOP,pidlPrograms)));
  if ok then begin
   lpBuffer := StrAlloc(max_path);
    with bi do begin
      hwndOwner := getactivewindow;
      pidlRoot := pidlPrograms;
      pszDisplayName := lpBuffer;
      lpszTitle := pChar(Title);
      ulFlags := BIF_RETURNONLYFSDIRS or BIF_NEWDIALOGSTYLE;
      lpfn := nil;
      lParam := 0;
      end;
    pidlBrowse := SHBrowseForFolder(bi);
    if (pidlBrowse <> nil) then begin
      if (SHGetPathFromIDList(pidlBrowse,lpBuffer)) then begin
        Dir:=lpBuffer;
        ImgNdx:=bi.iImage;
        Result:=true;
        end;
      end;
    StrDispose(lpBuffer);
    end;
  if pidlPrograms<>nil then pMalloc.Free(pidlPrograms);
  if pidlBrowse<>nil then pMalloc.Free(pidlBrowse);
  pMalloc._Release;
  end;


{ ---------------------------------------------------------------- }
function MakeLink (const LinkObj,ProgName,IconLocation,Arg,WorkDir,Desc: string;
                   IconIndex : integer; RunAs : boolean = false) : HResult;
var
  psl : IShellLink;
  ppf : IPersistFile;
  pdl : IShellLinkDataList;
  dwFlags : DWORD;
begin
  Result:=CoCreateInstance(CLSID_ShellLink,     // ID von ShellLink (Typ TGUID)
                      nil,CLSCTX_INPROC_SERVER,
                      IID_IShellLinkW,          // Referenz auf die Funktion
                      psl);
  if SUCCEEDED(Result) then with psl do begin
    Result:=SetPath(pChar(ProgName));
    if SUCCEEDED(Result) then begin
      SetArguments(PChar(Arg));
      if length(ProgName)>0 then SetIconLocation(PChar(IconLocation),IconIndex);
      if length(WorkDir)>0 then SetWorkingDirectory(PChar(WorkDir));
      if length(Desc)>0 then SetDescription(PChar(Desc));
      Result:=QueryInterface(IID_IPersistFile,ppf);
      if SUCCEEDED(Result) then begin
        if RunAs then begin
          Result:=QueryInterface(IID_IShellLinkDataList,pdl);
          if SUCCEEDED(Result) then begin
            Result:=pdl.GetFlags(dwFlags);
            if SUCCEEDED(Result) and ((SLDF_RUNAS_USER and dwFlags)=0) then
                Result:=pdl.SetFlags(SLDF_RUNAS_USER or dwFlags);
            end
          end
        else Result:=S_OK;
        if SUCCEEDED(Result) then Result:=ppf.Save(PChar(LinkObj),true);
        if SUCCEEDED(Result) then ppf.SaveCompleted(nil);
        end;
//      ppf:=nil;
      end;
//    psl:=nil;
    end;
  end;

function MakeLink (const LinkObj,ProgName,IconLocation,Arg,WorkDir,Desc: string;
                   RunAs : boolean = false) : HResult;
begin
  Result:=MakeLink(LinkObj,ProgName,ProgName,Arg,WorkDir,Desc,0,RunAs);
  end;

function MakeLink(const LinkObj,ProgName,Arg,WorkDir,Desc: string; RunAs : boolean = false) : HResult;
begin
  Result:=MakeLink(LinkObj,ProgName,ProgName,Arg,WorkDir,Desc,RunAs);
  end;

function GetLink (const LinkObj : string; var ProgName,Arg,WorkDir,Desc: string;
                  var RunAs : boolean) : HResult;
var
  psl : IShellLink;
  ppf : IPersistFile;
  sb  : PChar;
  pdl : IShellLinkDataList;
  pfd : TWin32FindData;
  dwFlags : DWORD;
begin
  RunAs:=false; ProgName:=''; Arg:=''; WorkDir:=''; Desc:='';
  if FileExists(LinkObj) then begin
    Result:=CoCreateInstance(CLSID_ShellLink,     // ID von ShellLink (Typ TGUID)
                      nil,CLSCTX_INPROC_SERVER,
                      IID_IShellLinkW,     // Referenz auf die Funktion
                      psl);
    if SUCCEEDED(Result) then with psl do begin
      Result:=QueryInterface(IID_IPersistFile,ppf);
      if SUCCEEDED(Result) then begin
        ppf.Load(PChar(LinkObj),STGM_READ);
        Result:=QueryInterface(IID_IShellLinkDataList,pdl);
        if SUCCEEDED(Result) then begin
          Result:=pdl.GetFlags(dwFlags);
          if SUCCEEDED(Result) then RunAs:=(SLDF_RUNAS_USER and dwFlags)<>0;
          end;
        if SUCCEEDED(Result) then begin
          sb:=StrAlloc(MAX_PATH+1);
          Result:=GetPath(sb,MAX_PATH,pfd,SLGP_UNCPRIORITY);
          if succeeded(Result) then begin
            ProgName:=sb;
            if succeeded(GetArguments(sb,MAX_PATH)) then Arg:=sb;
            if succeeded(GetWorkingDirectory(sb,MAX_PATH)) then WorkDir:=sb;
            if succeeded(GetDescription(sb,MAX_PATH)) then Desc:=sb;
            end;
          StrDispose(sb);
//          ppf:=nil;
          end;
        end;
      end;
//    psl:=nil;
    end
  else Result:=ERROR_FILE_NOT_FOUND;
  end;

function GetLink (const LinkObj : string; var ProgName,Arg,WorkDir,Desc: string) : boolean;
var
  ra : boolean;
begin
  Result:=SUCCEEDED(GetLink (LinkObj,ProgName,Arg,WorkDir,Desc,ra));
  end;

{ ---------------------------------------------------------------- }
(* Windows-Taskbar *)
(* Icon in Taskleiste aufnehmen *)
function TaskBarAddIcon (WinHandle  : HWnd;
                         UserId,MessageID  : integer;
                         IconHandle : HIcon;
                         const Title,Text,Tip : string;
                         PlaySound : boolean = true) : boolean;
var
  tnid : TNOTIFYICONDATA;
begin
  FillChar(tnid,sizeof(tnid),0);
  with tnid do begin
    cbSize := system.sizeof(tnid);     // Größenangabe der Struktur
    Wnd := WinHandle;                  // Handle des Message-Empfängers
    uID := UserId;                     // ID beliebig
    uFlags := 0;                       // siehe Tabelle
    if MessageID>0 then uFlags:=NIF_MESSAGE;
    uCallbackMessage := MessageID;     // Message Identifier
    hIcon := Iconhandle;               // Iconhandle
    if IconHandle<>0 then begin
      uFlags:=uFlags or NIF_ICON;
      dwInfoFlags:=NIIF_USER;
      end
    else dwInfoFlags:=NIIF_INFO;
    if not PlaySound then dwInfoFlags:=dwInfoFlags or NIIF_NOSOUND;
    if length(Title)>0 then begin         // Balloon tooltip
      uFlags:=uFlags or NIF_INFO;
      strpcopy(szInfoTitle,pchar(Title)); // Title max 64 Zeichen
      strpcopy(szInfo,pchar(Text));       // Text max 256 Zeichen
      end;
    if length(Tip)>0 then begin           // Standard tooltip
      uFlags:=uFlags or NIF_TIP;
      strpcopy(szTip,pchar(Tip));         // Tooltiptext max 127 Zeichen
      end;
    end;
  Result:=Shell_NotifyIcon(NIM_ADD,@tnid);       // Registrieren ...
  end;

function TaskBarChangeTip (WinHandle  : HWnd;
                           UserId     : integer;
                           const Tip : string;
                           PlaySound : boolean = true) : boolean;
var
  tnid : TNOTIFYICONDATA;
begin
  FillChar(tnid,sizeof(tnid),0);
  with tnid do begin
    cbSize := system.sizeof(tnid);        // Größenangabe der Struktur
    Wnd := WinHandle;                     // Handle des Message-Empfängers
    uID := UserId;                        // ID beliebig
    uFlags := 0;                          // siehe Tabelle
    if length(Tip)>0 then begin           // standard tooltip
      uFlags:=uFlags or NIF_TIP;
      strpcopy(szTip,pchar(Tip));         // Tooltiptext max 127 Zeichen
      end;
    dwInfoFlags:=NIIF_USER;
    if not PlaySound then dwInfoFlags:=dwInfoFlags or NIIF_NOSOUND;
    end;
  Result:=Shell_NotifyIcon(NIM_MODIFY,@tnid);  // Registrieren ...
  end;

function TaskBarChangeBalloonTip (WinHandle  : HWnd;
                                  UserId     : integer;
                                  const Title,Text : string;
                                  PlaySound : boolean = true) : boolean;
var
  tnid : TNOTIFYICONDATA;
begin
  FillChar(tnid,sizeof(tnid),0);
  with tnid do begin
    cbSize := system.sizeof(tnid);        // Größenangabe der Struktur
    Wnd := WinHandle;                     // Handle des Message-Empfängers
    uID := UserId;                        // ID beliebig
    uFlags := 0;                          // siehe Tabelle
    if length(Title)>0 then begin         // Balloon tooltip
      uFlags:=uFlags or NIF_INFO;
      strpcopy(szInfoTitle,pchar(Title)); // Title max 64 Zeichen
      strpcopy(szInfo,pchar(Text));       // Text max 256 Zeichen
      end;
    dwInfoFlags:=NIIF_USER;
    if not PlaySound then dwInfoFlags:=dwInfoFlags or NIIF_NOSOUND;
    end;
  Result:=Shell_NotifyIcon(NIM_MODIFY,@tnid);  // Registrieren ...
  end;

(* Icon in Taskbar ändern *)
function TaskBarChangeIcon (WinHandle  : HWnd;
                            UserId     : integer;
                            IconHandle : HIcon;
                            const Tip  : string;
                            PlaySound  : boolean = true) : boolean;
var
  tnid : TNOTIFYICONDATA ;
begin
  FillChar(tnid,sizeof(tnid),0);
  with tnid do begin
    cbSize := system.sizeof(tnid);        // Größenangabe der Struktur
    Wnd := WinHandle;
    uID := UserId;
    uFlags := 0;                          // siehe Tabelle
    if length(Tip)>0 then begin           // standard tooltip
      uFlags:=uFlags or NIF_TIP;
      strpcopy(szTip,pchar(Tip));         // Tooltiptext max 127 Zeichen
      end;
    if Assigned(pointer(IconHandle)) then uFlags:=uFlags or NIF_ICON;
    hIcon := Iconhandle;                  // Iconhandle
    if not PlaySound then dwInfoFlags:=NIIF_NOSOUND;
    end;
  Result:=Shell_NotifyIcon(NIM_MODIFY,@tnid);
  end;

function TaskBarChangeIcon (WinHandle  : HWnd;
                            UserId     : integer;
                            IconHandle : HIcon;
                            PlaySound  : boolean = true) : boolean;
begin
  Result:=TaskBarChangeIcon (WinHandle,UserId,IconHandle,'',PlaySound);
  end;

(* Icon aus Taskbar entfernen *)
function TaskBarRemoveIcon (WinHandle  : HWnd;
                            UserId     : integer) : boolean;
var
  tnid : TNOTIFYICONDATA ;
begin
  FillChar(tnid,sizeof(tnid),0);
  tnid.cbSize := system.sizeof(tnid);
  tnid.Wnd := WinHandle;
  tnid.uID := UserId;
  Result:=Shell_NotifyIcon(NIM_DELETE, @tnid);
  end;

(* Position der Taskbar ermitteln *)
function TaskBarGetPos : TTaskbarPos;
var
  Data: TAppBarData;
begin
with Data do begin
    cbSize := SizeOf(TAppBarData);
    hWnd := FindWindow('Shell_TrayWnd', nil);
    if hWnd <> 0 then begin
      SHAppBarMessage(ABM_GETTASKBARPOS,Data);
      with Result do begin
        Edge:=uEdge;
        Rect:=rc;
        end;
      end
    else Result.Edge:=-1;
    end;
  end;

(*  Returns focus to the taskbar notification area *)
function TaskBarSetFocus : boolean;
begin
  Result:=Shell_NotifyIcon(NIM_SETFOCUS, nil);
  end;

{ ---------------------------------------------------------------- }
function DoIFileOperation(WinHandle : HWnd; const srcFile,destFile : string; Action,Flags : integer) : HResult;
//works on Windows >= Vista and 2008 server
var
  fileOp: IFileOperation;
  siSrcFile: IShellItem;
  siDestFolder: IShellItem;
  destFileName : string;
begin
  //init com
  Result:=CoInitializeEx(nil, COINIT_APARTMENTTHREADED or COINIT_DISABLE_OLE1DDE);
  if Succeeded(Result) then begin
    //create IFileOperation interface
    Result:=CoCreateInstance(CLSID_FileOperation, nil, CLSCTX_ALL, IFileOperation, fileOp);
    if Succeeded(Result) then begin
      //set operations flags
      Result:=fileOp.SetOperationFlags(Flags);
      if Succeeded(Result) then begin
        Result:=fileOp.SetOwnerWindow(WinHandle);
        if Succeeded(Result) then begin
          destFileName:=ExtractFileName(destFile);
          //get source shell item
          Result:=SHCreateItemFromParsingName(PChar(srcFile), nil, IShellItem, siSrcFile);
          if Succeeded(Result) then begin
            if Action<>FO_DELETE then begin   // copy or move
              //get destination folder shell item
              Result:=SHCreateItemFromParsingName(PChar(ExtractFileDir(destFile)), nil, IShellItem, siDestFolder);
              if Succeeded(Result) then begin
                if Action=FO_COPY then //add copy operation
                  Result:=fileOp.CopyItem(siSrcFile, siDestFolder, pchar(destFileName), nil)
                else
                  Result:=fileOp.MoveItem(siSrcFile, siDestFolder, pchar(destFileName), nil);
                end
              end
            else // delete
              Result:=fileOp.DeleteItem(siSrcFile, nil);
            end;
          //execute
          if Succeeded(Result) then Result:=fileOp.PerformOperations;
          end;
        end;
      end;
    CoUninitialize;
    end;
  end;

function IShellCopyFiles (WinHandle : HWnd; const Source,Dest : string; Silent : boolean) : HResult;
var
  f : integer;
begin
  if Silent then f:=FOF_SILENT else f:=0;
  Result:=DoIFileOperation(WinHandle,Source,Dest,FO_COPY,
    f+FOF_NOERRORUI+FOF_FILESONLY+FOF_NOCONFIRMATION+FOF_NOCONFIRMMKDIR+FOF_NO_CONNECTED_ELEMENTS);
  end;

function IShellMoveFiles (WinHandle : HWnd; const Source,Dest : string; Silent : boolean) : HResult;
var
  f : integer;
begin
  if Silent then f:=FOF_SILENT else f:=0;
  Result:=DoIFileOperation(WinHandle,Source,Dest,FO_MOVE,
    f+FOF_NOERRORUI+FOF_FILESONLY+FOF_NOCONFIRMATION+FOF_NOCONFIRMMKDIR+FOF_NO_CONNECTED_ELEMENTS);
  end;

function IShellDeleteFiles (WinHandle : HWnd; const Source : string; Recycle,NoPrompt : boolean) : HResult;
var
  f : integer;
begin
  if Recycle then f:=FOF_ALLOWUNDO else f:=0;
  if NoPrompt then f:=f+FOF_NOERRORUI;
  Result:=DoIFileOperation(WinHandle,Source,'',FO_DELETE,
    f+FOF_FILESONLY+FOF_NOCONFIRMATION+FOF_NO_CONNECTED_ELEMENTS);
  end;

function IShellDeleteDir (WinHandle : HWnd; const Source : string; Recycle,NoPrompt : boolean) : HResult;
var
  f : integer;
begin
  if Recycle then f:=FOF_ALLOWUNDO else f:=0;
  if NoPrompt then f:=f+FOF_NOERRORUI;
  Result:=DoIFileOperation(WinHandle,IncludeTrailingPathDelimiter(Source),'',FO_DELETE,
    f+FOF_NOCONFIRMATION+FOF_NO_CONNECTED_ELEMENTS);
  end;

{ ---------------------------------------------------------------- }
// Shell file operations
function DoFileOperation(WinHandle  : HWnd; Source,Dest,Caption : string; Action,Flags : integer) : integer;
var
  FileOpStruct : TSHFileOpStruct;
begin
  with FileOpStruct do begin
    wnd:=WinHandle;
    wFunc:=Action;
    pFrom:=PChar(Source+#0);
    if length(Dest)>0 then pTo:=PChar(Dest+#0)
    else pTo:=nil;
    lpszProgressTitle:=pchar(Caption);
    fFlags:=Flags;
    if length(Caption)>0 then fFlags:=fFlags+FOF_SIMPLEPROGRESS;
    end;
  Result:=SHFileOperation(FileOpStruct);
  end;

function ShellCopyFiles (WinHandle : HWnd; const Source,Dest,Hint : string; Silent : boolean) : integer;
var
  f : integer;
begin
  if Silent then f:=FOF_SILENT else f:=0;
  Result:=DoFileOperation(WinHandle,Source,Dest,Hint,FO_COPY,
    f+FOF_NOERRORUI+FOF_FILESONLY+FOF_MULTIDESTFILES+FOF_NOCONFIRMATION+FOF_NOCONFIRMMKDIR+FOF_NO_CONNECTED_ELEMENTS);
  end;

function ShellMoveFiles (WinHandle        : HWnd;
                         const Source,Dest,Hint : string;
                         Silent           : boolean) : integer;
var
  f : integer;
begin
  if Silent then f:=FOF_SILENT else f:=0;
  Result:=DoFileOperation(WinHandle,Source,Dest,Hint,FO_MOVE,
    f+FOF_NOERRORUI+FOF_FILESONLY+FOF_MULTIDESTFILES+FOF_NOCONFIRMATION+FOF_NOCONFIRMMKDIR+FOF_NO_CONNECTED_ELEMENTS);
  end;

function ShellDeleteFiles (WinHandle   : HWnd;
                           const Source,Hint : string;
                           Recycle     : boolean;
                           NoPrompt    : boolean) : integer;
var
  f : integer;
begin
  if Recycle then f:=FOF_ALLOWUNDO else f:=0;
  if NoPrompt then f:=f+FOF_NOERRORUI;
  Result:=DoFileOperation(WinHandle,Source,'',Hint,FO_DELETE,
    f+FOF_FILESONLY+FOF_MULTIDESTFILES+FOF_NOCONFIRMATION+FOF_NO_CONNECTED_ELEMENTS);
  end;

function ShellDeleteAll (WinHandle   : HWnd;
                         const Source,Hint : string;
                         Recycle     : boolean;
                         NoPrompt    : boolean) : integer;
var
  f : integer;
begin
  if Recycle then f:=FOF_ALLOWUNDO else f:=0;
  if NoPrompt then f:=f+FOF_NOERRORUI;
  Result:=DoFileOperation(WinHandle,Source,'',Hint,FO_DELETE,
    f+FOF_MULTIDESTFILES+FOF_NOCONFIRMATION+FOF_NO_CONNECTED_ELEMENTS);
  end;

{ ---------------------------------------------------------------- }
function GetClientArea : TRect;
begin
  SystemParametersInfo(SPI_GETWORKAREA, 0, @Result, 0);
  end;

{ ---------------------------------------------------------------- }
// Get file information
function GetFileExeType (const FileName: string): TFileExeType;
var
  FileInfo: TSHFileInfo;
  rv : DWORD;
begin
  Result:=etError;
  FileInfo.dwAttributes := 0;
  rv:=SHGetFileInfo(PChar(FileName),0,FileInfo,SizeOf(FileInfo),SHGFI_EXETYPE);
  if rv=0 then begin
    if (SHGetFileInfo(PChar(FileName),0,FileInfo,SizeOf(FileInfo),SHGFI_ATTRIBUTES)<>0)
      and (fileInfo.dwAttributes and SFGAO_LINK <>0) then Result:=etLink;
    end
  else case LoWord(rv) of
    IMAGE_DOS_SIGNATURE: Result:=etMsDos;        // MZ
    IMAGE_OS2_SIGNATURE: Result:=etWin16;        // NE
    Word(IMAGE_NT_SIGNATURE):                    // PE
      if HiWord(rv)=0 then Result:=etWin32Con
      else Result:=etWin32Gui;
      end;
  end;

function IsConsoleApp (const Filename : string) : boolean;
begin
  Result:=GetFileExeType(Filename)=etWin32Con;
  end;

{ ---------------------------------------------------------------- }
// SHLoadLibraryFromItem() is defined wrong in ShlObj.pas!!! See QC #109306
function GetSHLoadLibraryFromItem(const psiLibrary: IShellItem; grfMode: DWORD; const riid: TIID; out ppv): HResult;
var
  plib: IShellLibrary;
begin
  Result:=CoCreateInstance(CLSID_ShellLibrary, nil, CLSCTX_INPROC_SERVER, IID_IShellLibrary, plib);
  if Succeeded(Result) then begin
    Result:=plib.LoadLibraryFromItem(psiLibrary, grfMode);
    if Succeeded(Result) then Result:=plib.QueryInterface(riid, ppv);
    end;
  end;

// Windows 7 Bibliotheken
function GetShellLibraryforLibrary(const LibraryName: String; grfMode: DWORD; var ppv: IShellLibrary): Boolean;
var
  Enum: IEnumShellItems;
  Item: IShellItem;
  DisplayName: LPWSTR;
  hr: HRESULT;
begin
  Result:=False;
  ppv:=nil;

  if FAILED(SHGetKnownFolderItem(FOLDERID_Libraries, 0, 0, IShellItem, PPointer(@Item)^)) then Exit;

  hr:=Item.BindToHandler(nil, BHID_EnumItems, IEnumShellItems, Enum);
  if FAILED(hr) then Exit;

  Item:=nil;
  while Enum.Next(1, Item, nil) = S_OK do begin
    if FAILED(Item.GetDisplayName(SIGDN_NORMALDISPLAY, DisplayName)) then Exit;
    try
      if AnsiSameText(DisplayName, LibraryName) then
      begin
        Result:=SUCCEEDED(GetSHLoadLibraryFromItem(Item, grfMode, IShellLibrary, ppv));
        Break;
        end;
    finally
      CoTaskMemFree(DisplayName);
      end;
    Item:=nil;
    end;
  end;

function GetLibraryFileSystemFolders(const LibraryName: String; Folders: TStrings): Boolean;
var
  SL: IShellLibrary;
  Arr: IShellItemArray;
  Enum: IEnumShellItems;
  Item: IShellItem;
  Path: LPWSTR;
begin
  Result:=False;

  if not GetShellLibraryforLibrary(LibraryName, STGM_READ, SL) then Exit;
  if FAILED(SL.GetFolders(LFF_FORCEFILESYSTEM, IShellItemArray, Arr)) then Exit;
  if FAILED(Arr.EnumItems(Enum)) then Exit;

  while Enum.Next(1, Item, nil) = S_OK do begin
    if FAILED(Item.GetDisplayName(SIGDN_FILESYSPATH, Path)) then Exit;
    try
      Folders.Add(Path);
    finally
      CoTaskMemFree(Path);
      end;
    Item:=nil;
    end;
  Result:=True;
  end;

end.
