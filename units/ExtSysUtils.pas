(* Delphi unit
   Extensions to system routines
   =============================

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Vers. 1.0 - May 2018
   last modified:
   *)

unit ExtSysUtils;

interface

uses System.SysUtils, Winapi.Windows;

const
  // errors from SHgetFileOperation
  FACILITY_PreWin32 = 128;
  FACILITY_ShellExec = 129;

{ ---------------------------------------------------------------- }
// Format without raising an exception on errors
function TryFormat(const AFormat: string; const Args: array of const): string;

{ ---------------------------------------------------------------- }
// path conversion
function UnixPathToDosPath(const Path: string): string;
function DosPathToUnixPath(const Path: string): string;

{ ---------------------------------------------------------------- }
// system error messages
function SystemErrorMessage(ASysError : cardinal) : string;
function NoError(ASysError : cardinal) : boolean;
function ThisError(ASysError,ThisError : cardinal) : boolean;
function IsSysError(ASysError : cardinal) : boolean;

{ ---------------------------------------------------------------- }
// read key from keyboard
function ReadKey : Word;
procedure WaitForAnyKey;

implementation

uses UnitConsts;

{------------------------------------------------------------------}
// create system error message including hex error code
// refer to: Win-SDK - Structure of COM Error Codes
function SystemErrorMessage(ASysError : cardinal) : string;
begin
  if Win32MajorVersion<6 then begin
    case LongRec(ASysError).Hi and $7FF of
    FACILITY_NULL,
    FACILITY_WIN32: Result:=SysErrorMessage(ASysError and $FFFF);
    FACILITY_WINDOWS: Result:=rsWindowsError;
    FACILITY_STORAGE: Result:=rsStorageError;
    FACILITY_RPC: Result:=rsRpcError;
  //  FACILITY_ITF: Result:=rsInterfaceError;
    FACILITY_DISPATCH: Result:=rsDispatchError;
    FACILITY_PreWin32: Result:=rsPreWin32Error;
    FACILITY_ShellExec: Result:=rsShellExec;
    else Result:=rsUnknownError;
      end;
    Result:=Result+Format(' (0x%.8x)',[ASysError]);
    end
  else Result:=SysErrorMessage(ASysError)+Format(' (0x%.8x)',[ASysError]);
  end;

function NoError(ASysError : cardinal) : boolean;
begin
  Result:=ASysError and $FFFF =NO_ERROR;
  end;

function ThisError(ASysError,ThisError : cardinal) : boolean;
begin
  Result:=(ASysError and $FFFF) = ThisError;
  end;

function IsSysError(ASysError : cardinal) : boolean;
begin
  Result:=ASysError and $FFFF <>NO_ERROR;
  end;

{ --------------------------------------------------------------- }
// Format without raising an exception on errors
function TryFormat(const AFormat: string; const Args: array of const): string;
begin
  try
    Result:=Format(AFormat,Args);
  except
    on E:Exception do Result:=rsFormatError+AFormat;
    end;
  end;

{ --------------------------------------------------------------- }
function UnixPathToDosPath(const Path: string): string;
begin
  Result:=Path.Replace('/', '\');
end;

function DosPathToUnixPath(const Path: string): string;
begin
  Result:=Path.Replace('\', '/');
end;

{ --------------------------------------------------------------- }
// read key from keyboard
function ReadKey : Word;
var
  nRead : Cardinal;
  Hdl   : THandle;
  Rec   : TInputRecord;
begin
  FlushConsoleInputBuffer(STD_INPUT_HANDLE);
  Hdl := GetStdHandle(STD_INPUT_HANDLE);
  repeat
    ReadConsoleInput(Hdl,Rec,1,nRead);
    until (Rec.EventType=KEY_EVENT) and (nRead=1) and (Rec.Event.KeyEvent.bKeyDown);
  Result := Rec.Event.KeyEvent.wVirtualKeyCode;
  end;

procedure WaitForAnyKey;
begin
  write(rsAnyKey); readkey; writeln;
  end;

end.
