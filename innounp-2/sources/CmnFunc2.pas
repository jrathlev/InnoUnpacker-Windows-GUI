unit CmnFunc2;

{
  Inno Setup
  Copyright (C) 1997-2005 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Common non-VCL functions

  $jrsoftware: issrc/Projects/CmnFunc2.pas,v 1.68 2005/02/19 10:23:38 mlaan Exp $
}

{$B-,R-}

interface

function NewFileExists(const Name: String): Boolean;
function DirExists(const Name: String): Boolean;
function FileOrDirExists(const Name: String): Boolean;
function AddQuotes(const S: String): String;
function RemoveQuotes(const S: String): String;
function IsWildcard(const Pattern: String): Boolean;
function WildcardMatch(const Text, Pattern: PChar): Boolean;
function Win32ErrorString(ErrorCode: Integer): String;

implementation

uses
  Winapi.Windows, System.SysUtils, PathFunc;

function InternalGetFileAttr(const Name: String): Integer;
begin
  Result := GetFileAttributes(PChar(RemoveBackslashUnlessRoot(Name)));
end;

function NewFileExists(const Name: String): Boolean;
{ Returns True if the specified file exists.
  This function is better than Delphi's FileExists function because it works
  on files in directories that don't have "list" permission. There is, however,
  one other difference: FileExists allows wildcards, but this function does
  not. }
var
  Attr: Integer;
begin
  Attr := InternalGetFileAttr(Name);
  Result := (Attr <> -1) and (Attr and faDirectory = 0);
end;

function DirExists(const Name: String): Boolean;
{ Returns True if the specified directory name exists. The specified name
  may include a trailing backslash.
  NOTE: Delphi's FileCtrl unit has a similar function called DirectoryExists.
  However, the implementation is different between Delphi 1 and 2. (Delphi 1
  does not count hidden or system directories as existing.) }
var
  Attr: Integer;
begin
  Attr := InternalGetFileAttr(Name);
  Result := (Attr <> -1) and (Attr and faDirectory <> 0);
end;

function FileOrDirExists(const Name: String): Boolean;
{ Returns True if the specified directory or file name exists. The specified
  name may include a trailing backslash. }
begin
  Result := InternalGetFileAttr(Name) <> -1;
end;

function AddQuotes(const S: String): String;
{ Adds a quote (") character to the left and right sides of the string if
  the string contains a space and it didn't have quotes already. This is
  primarily used when spawning another process with a long filename as one of
  the parameters. }
begin
  Result := Trim(S);
  if (PathPos(' ', Result) <> 0) and
     ((Result[1] <> '"') or (PathLastChar(Result)^ <> '"')) then
    Result := '"' + Result + '"';
end;

function RemoveQuotes(const S: String): String;
{ Opposite of AddQuotes; removes any quotes around the string. }
begin
  Result := S;
  while (Result <> '') and (Result[1] = '"') do
    Delete(Result, 1, 1);
  while (Result <> '') and (PathLastChar(Result)^ = '"') do
    SetLength(Result, Length(Result)-1);
end;

function IsWildcard(const Pattern: String): Boolean;
begin
  Result := (Pos('*', Pattern) <> 0) or (Pos('?', Pattern) <> 0);
end;

function WildcardMatch(const Text, Pattern: PChar): Boolean;
type
  TWildcardMatchResult = (wmFalse, wmTrue, wmAbort);

  function InternalWildcardMatch(T, P: PChar): TWildcardMatchResult;
  begin
    while P^ <> #0 do begin
      case P^ of
        '?': ;  { Match any character }
        '*': begin
               Inc(P);
               while P^ = '*' do begin
                 { Consecutive stars act just like one }
                 Inc(P);
               end;
               if P^ = #0 then begin
                 { Trailing star matches everything }
                 Result := wmTrue;
                 Exit;
               end;
               while T^ <> #0 do begin
                 Result := InternalWildcardMatch(T, P);
                 if Result <> wmFalse then
                   Exit;
                 T := CharNext(T);
               end;
               Result := wmAbort;
               Exit;
             end;
      else
        if not PathCharCompare(T, P) then begin
          Result := wmFalse;
          Exit;
        end;
      end;
      T := CharNext(T);
      P := CharNext(P);
    end;
    if T^ = #0 then
      Result := wmTrue
    else
      Result := wmFalse;
  end;

begin
  Result := (InternalWildcardMatch(Text, Pattern) = wmTrue);
end;

function Win32ErrorString(ErrorCode: Integer): String;
{ Like SysErrorMessage but also passes the FORMAT_MESSAGE_IGNORE_INSERTS flag
  which allows the function to succeed on errors like 129 }
var
  Len: Integer;
  Buffer: array[0..1023] of Char;
begin
  Len := FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or
    FORMAT_MESSAGE_IGNORE_INSERTS or FORMAT_MESSAGE_ARGUMENT_ARRAY, nil,
    ErrorCode, 0, Buffer, SizeOf(Buffer), nil);
  while (Len > 0) and (Buffer[Len - 1] in [#0..#32, '.']) do
    Dec(Len);
  SetString(Result, Buffer, Len);
end;

end.
