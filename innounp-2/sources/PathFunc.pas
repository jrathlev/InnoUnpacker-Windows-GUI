unit PathFunc;

{
  Inno Setup
  Copyright (C) 1997-2005 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  This unit provides some path-related, MBCS-aware functions.

  These functions should always be used in lieu of their SysUtils counterparts
  since they aren't MBCS-aware on Delphi 2, and sometimes not MBCS-aware on
  Delphi 6 and 7 either (see QC#5096).

  $jrsoftware: issrc/Components/PathFunc.pas,v 1.34 2005/02/12 22:16:24 jr Exp $
}

interface

function AddBackslash(const S: String): String;
function PathChangeExt(const Filename, Extension: String): String;
function PathCharCompare(const S1, S2: PChar): Boolean;
function PathCharLength(const S: String; const Index: Integer): Integer;
function PathCombine(const Dir, Filename: String): String;
function PathCompare(const S1, S2: String): Integer;
function PathDrivePartLength(const Filename: String): Integer;
function PathDrivePartLengthEx(const Filename: String;
  const IncludeSignificantSlash: Boolean): Integer;
function PathExpand(const Filename: String): String;
function PathExtensionPos(const Filename: String): Integer;
function PathExtractDir(const Filename: String): String;
function PathExtractDrive(const Filename: String): String;
function PathExtractExt(const Filename: String): String;
function PathExtractName(const Filename: String): String;
function PathExtractPath(const Filename: String): String;
function PathLastChar(const S: String): PChar;
function PathLastDelimiter(const Delimiters, S: string): Integer;
function PathLowercase(const S: String): String;
function PathNormalizeSlashes(const S: String): String;
function PathPathPartLength(const Filename: String;
  const IncludeSlashesAfterPath: Boolean): Integer;
function PathPos(Ch: Char; const S: String): Integer;
function RemoveBackslash(const S: String): String;
function RemoveBackslashUnlessRoot(const S: String): String;

const
  PathSlash = ['\', '/'];

implementation

uses
  Winapi.Windows, System.SysUtils;

function AddBackslash(const S: String): String;
{ Returns S plus a trailing backslash, unless S is an empty string or already
  ends in a backslash/slash. }
begin
  if (S <> '') and not(PathLastChar(S)^ in PathSlash) then
    Result := S + '\'
  else
    Result := S;
end;

function PathCharLength(const S: String; const Index: Integer): Integer;
{ Returns the length in bytes of the character at Index in S.
  Notes:
  1. If Index specifies the last character in S, 1 will always be returned,
     even if the last character is a lead byte.
  2. If a lead byte is followed by a null character (e.g. #131#0), 2 will be
     returned. This mimics the behavior of MultiByteToWideChar and CharPrev,
     but not CharNext(P)-P, which would stop on the null. }
begin
  if IsDBCSLeadByte(Ord(S[Index])) and (Index < Length(S)) then
    Result := 2
  else
    Result := 1;
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

function PathCombine(const Dir, Filename: String): String;
{ Combines a directory and filename into a path.
  If Dir is empty, it just returns Filename.
  If Dir specifies only a drive letter and colon ('c:'), it returns
  Dir + Filename.
  Otherwise, it returns the equivalent of AddBackslash(Dir) + Filename. }
var
  I: Integer;
begin
  if Dir <> '' then begin
    I := PathCharLength(Dir, 1) + 1;
    if ((I = Length(Dir)) and (Dir[I] = ':')) or
       (PathLastChar(Dir)^ in PathSlash) then
      Result := Dir + Filename
    else
      Result := Dir + '\' + Filename;
  end
  else
    Result := Filename;
end;

function PathCompare(const S1, S2: String): Integer;
{ Compares two filenames, and returns 0 if they are equal. }
begin
  Result := AnsiCompareStr(PathLowercase(S1), PathLowercase(S2));
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
  Note: This is MBCS-safe, unlike the Delphi's ExtractFileDrive function.
  (Computer and share names can include multi-byte characters!) }
var
  Len, I, C: Integer;
begin
  Len := Length(Filename);

  { \\server\share }
  if (Len >= 2) and (Filename[1] in PathSlash) and (Filename[2] in PathSlash) then begin
    I := 3;
    C := 0;
    while I <= Len do begin
      if Filename[I] in PathSlash then begin
        Inc(C);
        if C >= 2 then
          Break;
        repeat
          Inc(I);
          { And skip any additional consecutive slashes: }
        until (I > Len) or not(Filename[I] in PathSlash);
      end
      else
        Inc(I, PathCharLength(Filename, I));
    end;
    Result := I - 1;
    Exit;
  end;

  { \ }
  { Note: Test this before 'x:' since '\:stream' means access stream 'stream'
    on the root directory of the current drive, not access drive '\:' }
  if (Len >= 1) and (Filename[1] in PathSlash) then begin
    if IncludeSignificantSlash then
      Result := 1
    else
      Result := 0;
    Exit;
  end;

  { x: }
  if Len > 0 then begin
    I := PathCharLength(Filename, 1) + 1;
    if (I <= Len) and (Filename[I] = ':') then begin
      if IncludeSignificantSlash and (I < Len) and (Filename[I+1] in PathSlash) then
        Result := I+1
      else
        Result := I;
      Exit;
    end;
  end;

  Result := 0;
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
    stream, e.g. 'abc:def', it will return the length of the entire string,
    NOT the length of 'abc:'. }
var
  LastCharToKeep, Len, I: Integer;
begin
  Result := PathDrivePartLengthEx(Filename, True);
  LastCharToKeep := Result;
  Len := Length(Filename);
  I := Result + 1;
  while I <= Len do begin
    if Filename[I] in PathSlash then begin
      if IncludeSlashesAfterPath then
        Result := I
      else
        Result := LastCharToKeep;
      Inc(I);
    end
    else begin
      Inc(I, PathCharLength(Filename, I));
      LastCharToKeep := I-1;
    end;
  end;
end;

function PathExpand(const Filename: String): String;
{ Like Delphi's ExpandFileName, but does proper error checking. }
var
  Res: Integer;
  FilePart: PChar;
  Buf: array[0..4095] of Char;
begin
  DWORD(Res) := GetFullPathName(PChar(Filename), SizeOf(Buf), Buf, FilePart);
  if (Res > 0) and (Res < SizeOf(Buf)) then
    SetString(Result, Buf, Res)
  else
    Result := Filename;
end;

function PathExtensionPos(const Filename: String): Integer;
{ Returns index of the last '.' character in the filename portion of Filename,
  or 0 if there is no '.' in the filename portion.
  Note: Filename is assumed to NOT include an NTFS alternate data stream name
  (i.e. 'filename:stream'). }
var
  Len, I: Integer;
begin
  Result := 0;
  Len := Length(Filename);
  I := PathPathPartLength(Filename, True) + 1;
  while I <= Len do begin
    if Filename[I] = '.' then begin
      Result := I;
      Inc(I);
    end
    else
      Inc(I, PathCharLength(Filename, I));
  end;
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
  ends in a slash or consists only of a drive part, the result will be an empty
  string.
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

function PathLastChar(const S: String): PChar;
{ Returns pointer to last character in the string. Is MBCS-aware. Returns nil
  if the string is empty. }
begin
  if S = '' then
    Result := nil
  else
    Result := CharPrev(Pointer(S), @S[Length(S)+1]);
end;

function PathLastDelimiter(const Delimiters, S: string): Integer;
{ Returns the index of the last occurrence in S of one of the characters in
  Delimiters, or 0 if none were found.
  Note: S is allowed to contain null characters. }
var
  P, E: PChar;
begin
  Result := 0;
  if (S = '') or (Delimiters = '') then
    Exit;
  P := Pointer(S);
  E := @P[Length(S)];
  while P < E do begin
    if P^ <> #0 then begin
      if StrScan(Pointer(Delimiters), P^) <> nil then
        Result := (P - Pointer(S)) + 1;
      P := CharNext(P);
    end
    else
      Inc(P);
  end;
end;

function PathLowercase(const S: String): String;
{ Converts the specified path name to lowercase }
var
  I, L: Integer;
begin
  if (Win32Platform <> VER_PLATFORM_WIN32_NT) and
     (GetSystemMetrics(SM_DBCSENABLED) <> 0) then begin
    { Japanese Windows 98's handling of double-byte Roman characters in
      filenames is case sensitive, so we can't change the case of double-byte
      characters. (Japanese Windows NT/2000 is case insensitive, on both FAT
      and NTFS, in my tests.) Based on code from AnsiLowerCaseFileName. }
    Result := S;
    L := Length(Result);
    I := 1;
    while I <= L do begin
      if Result[I] in ['A'..'Z'] then begin
//        Inc(Byte(Result[I]), 32);
        inc(Result[I], Ord('a')-Ord('A'));
        Inc(I);
      end
      else
        Inc(I, PathCharLength(Result, I));
    end;
  end
  else
    Result := AnsiLowerCase(S);
end;

function PathPos(Ch: Char; const S: String): Integer;
{ This is an MBCS-aware Pos function. }
var
  Len, I: Integer;
begin
  Len := Length(S);
  I := 1;
  while I <= Len do begin
    if S[I] = Ch then begin
      Result := I;
      Exit;
    end;
    Inc(I, PathCharLength(S, I));
  end;
  Result := 0;
end;

function PathNormalizeSlashes(const S: String): String;
{ Returns S minus any superfluous slashes, and with any forward slashes
  converted to backslashes. For example, if S is 'C:\\\some//path', it returns
  'C:\some\path'. Does not remove a double backslash at the beginning of the
  string, since that signifies a UNC path. }
var
  Len, I: Integer;
begin
  Result := S;
  Len := Length(Result);
  I := 1;
  while I <= Len do begin
    if Result[I] = '/' then
      Result[I] := '\';
    Inc(I, PathCharLength(Result, I));
  end;
  I := 1;
  while I < Length(Result) do begin
    if (Result[I] = '\') and (Result[I+1] = '\') and (I > 1) then
      Delete(Result, I+1, 1)
    else
      Inc(I, PathCharLength(Result, I));
  end;
end;

function RemoveBackslash(const S: String): String;
{ Returns S minus any trailing slashes. Use of this function is discouraged;
  use RemoveBackslashUnlessRoot instead when working with file system paths. }
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and (CharPrev(Pointer(S), @S[I+1])^ in PathSlash) do
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
  while (I > DrivePartLen) and (CharPrev(Pointer(S), @S[I+1])^ in PathSlash) do
    Dec(I);
  if I = Length(S) then
    Result := S
  else
    Result := Copy(S, 1, I);
end;

end.
