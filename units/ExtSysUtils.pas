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

uses System.SysUtils;

function TryFormat(const AFormat: string; const Args: array of const): string;
function UnixPathToDosPath(const Path: string): string;
function DosPathToUnixPath(const Path: string): string;

implementation

uses UnitConsts;

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
  Result := Path.Replace('/', '\');
end;

function DosPathToUnixPath(const Path: string): string;
begin
  Result := Path.Replace('\', '/');
end;


end.
