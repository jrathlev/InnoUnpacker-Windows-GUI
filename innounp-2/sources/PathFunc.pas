unit PathFunc;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  This unit provides some path-related functions.

  The string comparison functions (including PathCompare, PathEndsWith, PathHasSubstringAt,
  PathSame, PathStartsWith, PathStrCompare, PathStrFind) all ignore case by
  default, and use a locale-independent "ordinal" comparison, which is important
  when comparing filenames/paths. Despite the "Path" prefix, however, the
  functions can be used to compare any kind of text, not just filenames/paths.

  About super paths (extended-length paths):

  PathHasInvalidCharacters considers all super paths as invalid.

  PathSame and Path(Str)Compare do not consider a super path and its normal
  version to be the same.

  modified for "innounp" and compilation with Delphi 10 by J. Rathlev, January 2025
}

interface

uses
  Windows, SysUtils;

type
  EPathFuncError = class(Exception);

function AddBackslash(const S: String): String;
function PathChangeExt(const Filename, Extension: String): String;
function PathCharCompare(const S1, S2: PChar): Boolean;
function PathCompare(const S1, S2: String; const IgnoreCase: Boolean = True): Integer;
function PathDrivePartLength(const Filename: String): Integer;
function PathDrivePartLengthEx(const Filename: String;
  const IncludeSignificantSlash: Boolean): Integer;
function PathCharIsSlash(const C: Char): Boolean;
function PathExtensionPos(const Filename: String): Integer;
function PathExtractDir(const Filename: String): String;
function PathExtractDrive(const Filename: String): String;
function PathExtractExt(const Filename: String): String;
function PathExtractName(const Filename: String): String;
function PathExtractPath(const Filename: String): String;
function PathHasSubstringAt(const S, Substring: String; const Offset: Integer;
  const IgnoreCase: Boolean = True): Boolean;
function PathPathPartLength(const Filename: String;
  const IncludeSlashesAfterPath: Boolean): Integer;
function PathLastChar(const S: String): PChar;
function PathLastDelimiter(const Delimiters, S: string): Integer;
function PathLowercase(const S: String): String;
function PathNormalizeSlashes(S: String): String;
function PathPos(const C: Char; const S: String;
  const Offset: Integer = 1): Integer;
function PathSame(const S1, S2: String): Boolean;
function PathStartsWith(const S, AStartsWith: String;
  const IgnoreCase: Boolean = True): Boolean;
function PathStrCompare(const S1: PChar; const S1Length: Integer;
  const S2: PChar; const S2Length: Integer;
  const IgnoreCase: Boolean = True): Integer;
function PathStrFind(const SSource: PChar; const SSourceLength: Integer;
  const SValue: PChar; const SValueLength: Integer;
  const IgnoreCase: Boolean = True): Integer;
function PathStrNextChar(const S: PChar): PChar;
function PathStrScan(const S: PChar; const C: Char): PChar;
function RemoveBackslash(const S: String): String;
function RemoveBackslashUnlessRoot(const S: String): String;

implementation

{$ZEROBASEDSTRINGS OFF}

function AddBackslash(const S: String): String;
{ Returns S plus a trailing backslash, unless S is an empty string or already
  ends in a backslash/slash. }
begin
  if (S <> '') and not PathCharIsSlash(PathLastChar(S)^) then
    Result := S + '\'
  else
    Result := S;
end;

function PathCharIsSlash(const C: Char): Boolean;
{ Returns True if C is a backslash or slash. }
begin
  Result := (C = '\') or (C = '/');
end;

function PathCharCompare(const S1, S2: PChar): Boolean;
{ Compares two first characters, and returns True if they are equal. }
var
  N, I: Integer;
begin
  N := CharNext(S1) - S1;
  if N = CharNext(S2) - S2 then begin
    for I := 0 to N-1 do begin
      if S1[I] <> S2[I] then begin
        Result := False;
        Exit;
      end;
    end;
    Result := True;
  end else
    Result := False;
end;

function PathChangeExt(const Filename, Extension: String): String;
{ Takes Filename, removes any existing extension, then adds the extension
  specified by Extension and returns the resulting string. }
var
  I: Integer;
begin
  I := PathExtensionPos(Filename);
  if I = 0 then
    Result := Filename + Extension
  else
    Result := Copy(Filename, 1, I - 1) + Extension;
end;

function PathCompare(const S1, S2: String; const IgnoreCase: Boolean = True): Integer;
{ Compares two strings (typically filenames, but they don't have to be) and
  returns 0 for "equal", <0 for "less than", or >0 for "greater than".
  An ordinal comparison is used, ignoring case by default. }
begin
  Result := PathStrCompare(PChar(S1), Length(S1), PChar(S2), Length(S2),
    IgnoreCase);
end;

function PathDrivePartLength(const Filename: String): Integer;
begin
  Result := PathDrivePartLengthEx(Filename, False);
end;

function PathDrivePartLengthEx(const Filename: String;
  const IncludeSignificantSlash: Boolean): Integer;
{ Returns length of the drive portion of Filename, or 0 if there is no drive
  portion.
  If IncludeSignificantSlash is True, the drive portion can include a trailing
  slash if it is significant to the meaning of the path (i.e. 'x:' and 'x:\'
  are not equivalent, nor are '\' and '').
  If IncludeSignificantSlash is False, the function works as follows:
    'x:file'              -> 2  ('x:')
    'x:\file'             -> 2  ('x:')
    '\\server\share\file' -> 14 ('\\server\share')
    '\file'               -> 0  ('')
  If IncludeSignificantSlash is True, the function works as follows:
    'x:file'              -> 2  ('x:')
    'x:\file'             -> 3  ('x:\')
    '\\server\share\file' -> 14 ('\\server\share')
    '\file'               -> 1  ('\')
}
var
  Len,I,MaxComponents,CurComponent,ComponentStartIndex,ComponentLen : Integer;
  FirstComponentIsNamespace,SecondComponentIsDrive : boolean;
begin
  Len := Length(Filename);

  { Standard UNC path:
      \\server\share
    Super/device-namespace drives:
      \\?\C:
      \\.\C:
      \\?\C:\   (trailing slash kept when IncludeSignificantSlash is True)
      \\.\C:\
    Super/device-namespace UNC paths (4 components):
      \\?\UNC\server\share
      \\.\UNC\server\share }
  if (Len >= 2) and PathCharIsSlash(Filename[1]) and PathCharIsSlash(Filename[2]) then begin
    { For consistency with the behavior of Windows' GetFullPathName function,
      we don't collapse 3 leading slashes into 2. Instead, a leading "\\\" is
      treated like "\\<empty computer name>\".
      PathNormalizeSlashes works the same way. }
    FirstComponentIsNamespace := False;
    SecondComponentIsDrive := False;
    MaxComponents := 2;
    CurComponent := 0;
    ComponentStartIndex := 3;
    I := ComponentStartIndex;
    while I <= Len do begin
      if PathCharIsSlash(Filename[I]) then begin
        ComponentLen := I - ComponentStartIndex;
        case CurComponent of
          0: begin
               if (ComponentLen = 1) and
                  CharInSet(Filename[ComponentStartIndex], ['?', '.']) then
                 FirstComponentIsNamespace := True;
             end;
          1: if FirstComponentIsNamespace then begin
               if ComponentLen = 2 then begin
                 if Filename[ComponentStartIndex+1] = ':' then
                   SecondComponentIsDrive := True;
               end
               else if ComponentLen = 3 then begin
                 if PathHasSubstringAt(Filename, 'UNC', ComponentStartIndex - Low(Filename)) then
                   Inc(MaxComponents, 2);
               end;
             end;
        end;
        Inc(CurComponent);
        if CurComponent >= MaxComponents then begin
          if SecondComponentIsDrive and IncludeSignificantSlash then
            Inc(I);
          Break;
        end;
        repeat
          Inc(I);
          { And skip any additional consecutive slashes: }
        until (I > Len) or not PathCharIsSlash(Filename[I]);
        ComponentStartIndex := I;
      end
      else
        Inc(I);
    end;
    Result := I - 1;
    Exit;
  end;

  { \ }
  { Note: Test this before 'x:' since '\:stream' means access stream 'stream'
    on the root directory of the current drive, not access drive '\:' }
  if (Len >= 1) and PathCharIsSlash(Filename[1]) then begin
    if IncludeSignificantSlash then
      Result := 1
    else
      Result := 0;
    Exit;
  end;

  { x: }
  if (Len >= 2) and (Filename[2] = ':') then begin
    { We intentionally omit an A-Z check here because Windows doesn't have
      one. For example, you can define a drive '@:' using DefineDosDevice, and
      then successfully pass '@:\file' to APIs like GetFileAttributes. }
    if IncludeSignificantSlash and (Len >= 3) and PathCharIsSlash(Filename[3]) then
      Result := 3
    else
      Result := 2;
    Exit;
  end;

  Result := 0;
end;

function PathExtensionPos(const Filename: String): Integer;
{ Returns index of the last '.' character in the filename portion of Filename,
  or 0 if there is no '.' in the filename portion.
  Note: Filename is assumed to NOT include an NTFS alternate data stream name
  (i.e. 'filename:stream'). }
var
  NamePartStartIndex,i : integer;
begin
  NamePartStartIndex := PathPathPartLength(Filename, True) + 1;
  for I := Length(Filename) downto NamePartStartIndex do
    if Filename[I] = '.' then
      Exit(I);

  Result := 0;
end;

function PathExtractDir(const Filename: String): String;
{ Like PathExtractPath, but strips any trailing slashes, unless the resulting
  path is the root directory of a drive (i.e. 'C:\' or '\'). }
var
  I: Integer;
begin
  I := PathPathPartLength(Filename, False);
  Result := Copy(Filename, 1, I);
end;

function PathExtractDrive(const Filename: String): String;
{ Returns the drive portion of Filename (either 'x:' or '\\server\share'),
  or an empty string if there is no drive portion. }
var
  L: Integer;
begin
  L := PathDrivePartLength(Filename);
  if L = 0 then
    Result := ''
  else
    Result := Copy(Filename, 1, L);
end;

function PathExtractExt(const Filename: String): String;
{ Returns the extension portion of the last component of Filename (e.g. '.txt')
  or an empty string if there is no extension. }
var
  I: Integer;
begin
  I := PathExtensionPos(Filename);
  if I = 0 then
    Result := ''
  else
    Result := Copy(Filename, I, Maxint);
end;

function PathExtractName(const Filename: String): String;
{ Returns the filename portion of Filename (e.g. 'filename.txt'). If Filename
  ends in a slash or consists only of a drive part or is empty, the result will
  be an empty string.
  This function is essentially the opposite of PathExtractPath. }
var
  I: Integer;
begin
  I := PathPathPartLength(Filename, True);
  Result := Copy(Filename, I + 1, Maxint);
end;

function PathExtractPath(const Filename: String): String;
{ Returns the path portion of Filename (e.g. 'c:\dir\'). If Filename contains
  no drive part or slash, the result will be an empty string.
  This function is essentially the opposite of PathExtractName. }
var
  I: Integer;
begin
  I := PathPathPartLength(Filename, True);
  Result := Copy(Filename, 1, I);
end;

function PathHasSubstringAt(const S, Substring: String; const Offset: Integer;
  const IgnoreCase: Boolean = True): Boolean;
{ Returns True if Substring exists in S at the specified zero-based offset
  from the beginning of S.
  An ordinal comparison is used, ignoring case by default.
  Passing an out-of-range Offset value is allowed/safe. False is returned if
  Offset is negative or if checking for Substring at Offset would go beyond
  the end of S (partially or fully).
  If Substring is empty and Offset = Length(S), then True is returned because
  that is not considered going *beyond* the end of S. }
var
  SubstringLen : integer;
begin
  if Offset < 0 then
    Exit(False);
  SubstringLen := Length(Substring);
  if Offset > Length(S) - SubstringLen then
    Exit(False);

  Result := (PathStrCompare(PChar(S) + Offset, SubstringLen, PChar(Substring),
    SubstringLen, IgnoreCase) = 0);
end;

function PathPathPartLength(const Filename: String;
  const IncludeSlashesAfterPath: Boolean): Integer;
{ Returns length of the path portion of Filename, or 0 if there is no path
  portion.
  Note these differences from Delphi's ExtractFilePath function:
  - The result will never be less than what PathDrivePartLength returns.
    If you pass a UNC root path, e.g. '\\server\share', it will return the
    length of the entire string, NOT the length of '\\server\'.
  - If you pass in a filename with a reference to an NTFS alternate data
    stream, e.g. 'abc:def', it will return 0, NOT the length of 'abc:'. }
var
  DrivePartLen : integer;
  FoundSlash : boolean;
begin
  DrivePartLen := PathDrivePartLengthEx(Filename, True);
  FoundSlash := False;
  Result := Length(Filename);
  while Result > DrivePartLen do begin
    if PathCharIsSlash(Filename[Result]) then begin
      if IncludeSlashesAfterPath then
        Break;
      FoundSlash := True;
    end else begin
      if FoundSlash then
        Break;
    end;
    Dec(Result);
  end;
end;

function PathLastChar(const S: String): PChar;
{ Returns pointer to last character in the string. Returns nil if the string is
  empty. }
begin
  if S = '' then
    Result := nil
  else
    Result := @S[High(S)];
end;

function PathLastDelimiter(const Delimiters, S: string): Integer;
{ Returns the index of the last occurrence in S of one of the characters in
  Delimiters, or 0 if none were found.
  Both strings are allowed to contain #0 characters; they are treated the same
  as any other character. (Delphi's LastDelimiter function doesn't allow #0
  in Delimiters.) }
var
  i : integer;
begin
  for I := Length(S) downto 1 do
    if PathPos(S[I], Delimiters) <> 0 then
      Exit(I);

  Result := 0;
end;

function PathLowercase(const S: String): String;
{ Converts the specified path name to lowercase }
begin
  Result := AnsiLowerCase(S);
end;

function PathPos(const C: Char; const S: String;
  const Offset: Integer = 1): Integer;
{ Like Delphi's Pos function, but searches for a single character instead of a
  string, making it somewhat faster.
  Returns 1-based index of the first occurrence of C in S, or 0 if not found.
  The optional Offset parameter (1-based) specifies where the search should
  start. If Offset is less than 1, then 0 is returned (same as Delphi's Pos
  function). }
var
  i : integer;
begin
  if Offset >= 1 then
    for I := Offset to Length(S) do
      if S[I] = C then
        Exit(I);

  Result := 0;
end;

function PathNormalizeSlashes(S: String): String;
{ Returns S minus any superfluous slashes, and with any forward slashes
  converted to backslashes. For example, if S is 'C:\\\some//path', it returns
  'C:\some\path'.
  If the string starts with two slashes ('\\') then those two characters are
  ignored when collapsing repeated slashes. So:
    \\server\share   -> \\server\share   (unchanged)
    \\\server\share  -> \\\server\share  (unchanged)
    \\\\server\share -> \\\server\share  (one backslash removed)
  Note that paths with 3+ leading slashes don't actually work. But Windows'
  GetFullPathName function, used by PathExpand, collapses slashes the same
  way. Best to be consistent. }
var
  i,len,EndIndex : integer;
begin
  Len := Length(S);
  for I := 1 to Len do
    if S[I] = '/' then
      S[I] := '\';

  EndIndex := 2;
  if (Len >= 2) and (S[1] = '\') and (S[2] = '\') then
    Inc(EndIndex, 2);

  for I := Len downto EndIndex do
    if (S[I] = '\') and (S[I-1] = '\') then
      Delete(S, I, 1);

  Result := S;
end;

function PathSame(const S1, S2: String): Boolean;
{ Returns True if the specified strings (typically filenames) are equal.
  An ordinal comparison is used, ignoring case.
  Like PathCompare, but faster for checking equality as it returns False
  immediately if the strings are different lengths. }
begin
  Result := (Length(S1) = Length(S2)) and (PathCompare(S1, S2) = 0);
end;

function PathStartsWith(const S, AStartsWith: String;
  const IgnoreCase: Boolean = True): Boolean;
{ Returns True if S starts with (or is equal to) AStartsWith.
  An ordinal comparison is used, ignoring case by default. }
begin
  Result := PathHasSubstringAt(S, AStartsWith, 0, IgnoreCase);
end;

{ Use our own CompareStringOrdinal declaration. The one in the Windows unit is
  "delayload" (yuck), and the bIgnoreCase parameter type differs between
  Delphi 11 and 12 (BOOL vs. DWORD). }
function CompareStringOrdinal_static(lpString1: LPCWSTR; cchCount1: Integer;
  lpString2: LPCWSTR; cchCount2: Integer; bIgnoreCase: BOOL): Integer; stdcall;
  external kernel32 name 'CompareStringOrdinal';

function PathStrCompare(const S1: PChar; const S1Length: Integer;
  const S2: PChar; const S2Length: Integer;
  const IgnoreCase: Boolean = True): Integer;
{ Compares two strings and returns 0 for "equal", <0 for "less than", or
  >0 for "greater than".
  An ordinal comparison is used, ignoring case by default.
  A length of -1 may be passed if a string is null-terminated; in that case,
  the length is determined automatically. }
var
  CompareResult : integer;
begin
  { As documented, CompareStringOrdinal only allows 1 for TRUE in the
    bIgnoreCase parameter. "BOOL(Byte(IgnoreCase))" ensures we pass 1, not the
    usual -1 Delphi passes when a Boolean is implicitly converted to BOOL. }
  CompareResult := CompareStringOrdinal_static(S1, S1Length, S2, S2Length,
    BOOL(Byte(IgnoreCase)));
  case CompareResult of
    0: raise EPathFuncError.CreateFmt('PathStrCompare: CompareStringOrdinal failed (%u)',
         [GetLastError]);
    1..3: ;
  else
    raise EPathFuncError.CreateFmt('PathStrCompare: CompareStringOrdinal result invalid (%d)',
      [CompareResult]);
  end;
  Result := CompareResult - 2;
end;

{ Use our own FindStringOrdinal declaration. The one in the Windows unit is
  "delayload" (yuck). }
function FindStringOrdinal_static(dwFindStringOrdinalFlags: DWORD;
  lpStringSource: LPCWSTR; cchSource: Integer;
  lpStringValue: LPCWSTR; cchValue: Integer; bIgnoreCase: BOOL): Integer; stdcall;
  external kernel32 name 'FindStringOrdinal';

function PathStrFind(const SSource: PChar; const SSourceLength: Integer;
  const SValue: PChar; const SValueLength: Integer;
  const IgnoreCase: Boolean = True): Integer;
{ Locates a value in a string, starting with the first character of the string.
  Returns a 0-based index if found, and -1 otherwise.
  An ordinal comparison is used, ignoring case by default.
  A length of -1 may be passed if a string is null-terminated; in that case,
  the length is determined automatically. }
var
  CompareResult,LastError : integer;
begin
  { This is not documented for FindStringOrdinal, but like CompareStringOrdinal
    it only allows 1 for TRUE in the bIgnoreCase parameter. See above. }
  CompareResult := FindStringOrdinal_static(FIND_FROMSTART, SSource, SSourceLength,
    SValue, SValueLength, BOOL(Byte(IgnoreCase)));
  if CompareResult = -1 then begin
    LastError := GetLastError;
    if LastError <> ERROR_SUCCESS then
      raise EPathFuncError.CreateFmt('PathStrFind: FindStringOrdinal failed (%u)',
        [LastError]);
  end else if not ((CompareResult >= 0) and ((SSourceLength = -1) or (CompareResult < SSourceLength))) then
    raise EPathFuncError.CreateFmt('PathStrFind: FindStringOrdinal result invalid (%d)',
      [CompareResult]);
  Result := CompareResult;
end;

function PathStrNextChar(const S: PChar): PChar;
{ Returns pointer to the character after S, unless S points to a null (#0). }
begin
  Result := S;
  if Result^ <> #0 then
    Inc(Result);
end;

function PathStrScan(const S: PChar; const C: Char): PChar;
{ Returns pointer to first occurrence of C in S, or nil if there are no
  occurrences. As with StrScan, specifying #0 for the search character is legal. }
begin
  Result := S;
  while Result^ <> C do begin
    if Result^ = #0 then begin
      Result := nil;
      Break;
    end;
    Result := PathStrNextChar(Result);
  end;
end;

function RemoveBackslash(const S: String): String;
{ Returns S minus any trailing slashes. Use of this function is discouraged;
  use RemoveBackslashUnlessRoot instead when working with file system paths. }
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and PathCharIsSlash(S[I]) do
    Dec(I);
  if I = Length(S) then
    Result := S
  else
    Result := Copy(S, 1, I);
end;

function RemoveBackslashUnlessRoot(const S: String): String;
{ Returns S minus any trailing slashes, unless S specifies the root directory
  of a drive (i.e. 'C:\' or '\'), in which case it leaves 1 slash. }
var
  DrivePartLen, I: Integer;
begin
  DrivePartLen := PathDrivePartLengthEx(S, True);
  I := Length(S);
  while (I > DrivePartLen) and PathCharIsSlash(S[I]) do
    Dec(I);
  if I = Length(S) then
    Result := S
  else
    Result := Copy(S, 1, I);
end;

end.
