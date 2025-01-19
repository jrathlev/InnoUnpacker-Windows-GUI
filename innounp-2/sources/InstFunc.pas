unit InstFunc;

{
  Inno Setup
  Copyright (C) 1997-2004 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Misc. installation functions

  $jrsoftware: issrc/Projects/InstFunc.pas,v 1.30 2004/02/16 02:58:30 jr Exp $
}

interface

function GenerateUniqueName(Path: String; const Extension: String): String;
procedure Win32ErrorMsg(const FunctionName: String);

implementation

uses
  Winapi.Windows, System.SysUtils, PathFunc, CmnFunc2, Msgs, MsgIDs;

procedure Win32ErrorMsg(const FunctionName: String);
var
  LastError: DWORD;
begin
  LastError := GetLastError;
  raise Exception.Create(FmtSetupMessage(msgErrorFunctionFailedWithMessage,
    [FunctionName, IntToStr(LastError), SysErrorMessage(LastError)]));
end;

function GenerateUniqueName(Path: String; const Extension: String): String;
  function IntToBase32(Number: Longint): String;
  const
    Table: array[0..31] of Char = '0123456789ABCDEFGHIJKLMNOPQRSTUV';
  var
    I: Integer;
  begin
    Result := '';
    for I := 0 to 4 do begin
      Insert(Table[Number and 31], Result, 1);
      Number := Number shr 5;
    end;
  end;
var
  Rand, RandOrig: Longint;
begin
  Path := AddBackslash(Path);
  RandOrig := Random($2000000);
  Rand := RandOrig;
  repeat
    Inc(Rand);
    if Rand > $1FFFFFF then Rand := 0;
    if Rand = RandOrig then
      { practically impossible to go through 33 million possibilities,
        but check "just in case"... }
      raise Exception.Create(FmtSetupMessage1(msgErrorTooManyFilesInDir,
        RemoveBackslashUnlessRoot(Path)));
    { Generate a random name }
    Result := Path + 'is-' + IntToBase32(Rand) + Extension;
  until not FileOrDirExists(Result);
end;

end.
