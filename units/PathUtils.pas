(* Delphi-Unit
   collection of routines for path string processing
   =================================================

   separated from former FileUtils

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   February 2023
   last modified: February 2023
   *)

unit PathUtils;

interface

const
  Punkt = '.';
  DPunkt = ':';
  BSlash = '\';
  IllegalFilenameChars = [#0..#31,'*','?','<','>',':','"','/','\','|'];
  IllegalFileCharStr = '* ? < > : " / \ |';

{ ---------------------------------------------------------------- }
// Add trailing path delimiter if path is not empty
function SetDirName (const Dir : string) : string;

// Compare directory names including trailing path delimiter
function SameDirName (const S1, S2: string): Boolean;

{ ---------------------------------------------------------------- }
// Get file extension without leading period
function GetExt (const Name : string) : string;

// Delete file extension
function DelExt (Name : string) : string;

// Change file extension
function NewExt (Name,Ext  : string) : string;

// Add a file extension
function AddExt (const Name,Ext : string; ReplaceMode : integer = 0) : string;

// Check extension
function HasExt (const Name,Ext : string) : boolean;

{ ---------------------------------------------------------------- }
// Add a path to a filename
function AddPath (Path,Name : string) : string;

// Expand filename to full path and add extension (if not exists)
function ExpandToPath (const Pfad,Name,Ext : string) : string;
function Erweiter (const Pfad,Name,Ext : string) : string;

// Add suffix and extension to filename
function AddNameSuffix (FName,Suffix,Ext : string) : string;

// Insert suffix between filename and extension
function InsertNameSuffix (FName,Suffix : string) : string;

// Remove suffix of length CharCount from filename with extension
function RemoveNameSuffix (FName : string; CharCount : integer) : string;

// Check if FName has prefix matching to Pref
function HasPrefix(const Pref,FName : string) : boolean;

// Strip a path to a maximum length of Len characters
function StripPath (const APath : string; Len  : integer) : string;

// Extract parent path
function ExtractParentPath (Dir : string) : string;

// Remove the first subdirectory from path
function RemoveFirstDir (const Dir : string) : string;

// Extract last subdirectory from path
function ExtractLastDir (const Path : string) : string;

// Extract first subdirectory from path and remove from Path
function ExtractFirstDir (var Path : string) : string;

// Check if Path2 is a subpath of Path1 (both are full paths)
function IsSubPath (const Path1,Path2 : string) : boolean;

// Check if Path is a root path (e.g. "C:\..")
function IsRootPath (const Path : string) : boolean;

// Check if Path2 is identical to Path1 (both are full paths)
function IsSamePath (const Path1,Path2 : string) : boolean;

// Check if Path is an absolute path (starting with \ or drive)
function ContainsFullPath (const Path : string) : boolean;

// Convert absolute path to relative and and vice versa
function MakeRelativePath (const BaseName,DestName: string) : string;
function MakeAbsolutePath (const BaseName,DestName: string) : string;

// Expand relative path "DestName" to absolute path based on "BaseName"
function ExpandRelativePath (const BaseName,DestName: string) : string;

// Expand relative path to absolute path
function ExpandPath (const DestName: string) : string;

// Check for illegal characters in path
function CheckPathChars (APath : string) : boolean;

// Check for illegal characters in filename (without path)
function CheckFilename (const Filename : string) : boolean;

// Replace illegal characters by AChar
function ReplaceIllegalChars (const Filename : string; AChar : char) : string;

// Extract identical leading path fragments
function ExtractSamePath(const Path1,Path2 : string) : string;

// Get last existing parent path, set DefPath if not found
function GetExistingParentPath (const Path,DefPath : string) : string;

// Replace drive letter in path
function ReplaceDrive(const Path,Drive : string) : string;

// Remove drive
function RemoveDrive (const Path : string) : string;

{ ---------------------------------------------------------------- }
// Check for special directories ('.'=self and '..'=one up )
// or if directory starts with specified character
function NotSpecialDir (const Name : string; StartChars : array of char) : boolean; overload;
function NotSpecialDir (const Name : string) : boolean; overload;

implementation

uses System.SysUtils, System.StrUtils, System.Masks, Winapi.Windows, Winapi.Shlwapi;

{ ------------------------------------------------------------------- }
// Add trailing path delimiter if path is not empty
function SetDirName (const Dir : string) : string;
begin
  if length(Dir)>0 then Result:=IncludeTrailingPathDelimiter(Dir)
  else Result:='';
  end;

// Compare directory names including trailing path delimiter
function SameDirName (const S1,S2: string): Boolean;
begin
  Result:=SameFileName(SetDirName(S1),SetDirName(S2));
  end;

{ --------------------------------------------------------------- }
// Get file extension without leading period
function GetExt (const Name : string) : string;
var
  i,j : integer;
begin
  Result:='';
  j:=length(Name);
  if j>0 then begin
    i:=j;
    while (i>0) and (Name[i]<>Punkt) and (not IsPathDelimiter(Name,i)) do dec(i);
    if (i>0) and (Name[i]=Punkt) then Result:=copy(name,i+1,j-i)
    end;
  end;

{ --------------------------------------------------------------- }
// Delete file extension (from end to last period)
function DelExt (Name : string) : string;
var
  i,j : integer;
begin
  j:=length(Name);
  if j>0 then begin
    i:=j;
    while (i>0) and (Name[i]<>Punkt) and (not IsPathDelimiter(Name,i)) do dec(i);
    if (i>0) and (Name[i]=Punkt) then delete (Name,i,j-i+1);
    end;
  Result:=Name;
  end;

{ --------------------------------------------------------------- }
// Change file extension (from end to last period)
function NewExt (Name,Ext  : string) : string;
begin
  if length(Name)>0 then begin
    Name:=DelExt(Name);
    if (length(Ext)>0) and (Ext[1]=Punkt) then delete(Ext,1,1);
    if length(Ext)>0 then Result:=Name+Punkt+Ext
    else Result:=Name;
    end
  else Result:='';
  end;

{ --------------------------------------------------------------- }
// Add a file extension
// ReplaceMode = 2 - add extension in any case
//             = 1 - replace existing extension
//             = 0 - no action if same extension already exists (default)
function AddExt (const Name,Ext  : string; ReplaceMode : integer) : string;
var
  ok : boolean;
  se : string;
begin
  Result:=Name;
  if (length(Name)=0) or (length(Ext)=0) then Exit
  else begin
    if ReplaceMode=0 then begin
      se:=GetExt(Name);
      ok:=(length(se)=0) or not AnsiSameText(se,Ext);
      end
    else ok:=true;
    if ok then begin
      if ReplaceMode=1 then Result:=NewExt(Name,Ext)
      else begin
        if Result[length(Result)]=Punkt then Delete(Result,length(Result),1);
        if (Ext[1]=Punkt) then Result:=Result+Ext else Result:=Result+Punkt+Ext;
        end;
      end
    end;
  end;

// Check extension
function HasExt (const Name,Ext : string) : boolean;
begin
  Result:=AnsiSameText(GetExt(Name),Ext);
  end;

{ --------------------------------------------------------------- }
// Add a path to a filename
function AddPath (Path,Name : string) : string;
begin
  if length(Name)>0 then begin
    if length(Path)>0 then Result:=SetDirName(Path)+Name else Result:=Name;
    end
  else Result:=Path;
  end;

{ --------------------------------------------------------------- }
// Expand filename to full path and add extension (if not exists)
function ExpandToPath (const Pfad,Name,Ext : string) : string;
var
  sn,se : string;
begin
  se:=Ext; sn:=Name;
  if (length(se)>0) and (se[1]=Punkt) then delete(se,1,1);
  if (pos(Punkt,sn)=0) and (length(se)<>0) then sn:=sn+Punkt+se;
  if (pos (DPunkt,sn)<>0) or (pos(BSlash,sn)=1)
     or (length(Pfad)=0) then Result:=sn
  else if Pfad[length(Pfad)]=BSlash then Result:=Pfad+sn
         else Result:=Pfad+BSlash+sn;
  end;

function Erweiter (const Pfad,Name,Ext  : string) : string;
begin
  Result:=ExpandToPath (Pfad,Name,Ext);
  end;

{ ---------------------------------------------------------------- }
// Add suffix and extension to filename
function AddNameSuffix (FName,Suffix,Ext : string) : string;
begin
  if (length(Ext)>0) and (Ext[1]<>Punkt) then Ext:=Punkt+Ext;
  Result:=DelExt(FName)+Suffix+Ext;
  end;

{ ---------------------------------------------------------------- }
// Insert suffix between filename and extension
function InsertNameSuffix (FName,Suffix : string) : string;
begin
  Result:=AddNameSuffix(FName,Suffix,GetExt(FName));
  end;

{ ---------------------------------------------------------------- }
// Remove suffix of length CharCount from filename with extension
function RemoveNameSuffix (FName : string; CharCount : integer) : string;
var
  s : string;
begin
  s:=DelExt(FName);
  Result:=AnsiLeftStr(s,length(s)-CharCount)+ExtractFileExt(FName);
  end;

{ ---------------------------------------------------------------- }
// Check if FName has prefix matching to Pref
function HasPrefix(const Pref,FName : string) : boolean;
begin
  Result:=(length(Pref)>0) and MatchesMask(FName,Pref);
  end;

{ ---------------------------------------------------------------- }
// Strip a path to a maximum length of Len characters
// similar to MinimizeName in FileCtrl but character related
function StripPath (const APath : string; Len  : integer) : string;
const
  Punkte = '...';
var
  i,j,nl : integer;
  ok     : boolean;
begin
  Result:=APath;
  nl:=length(Result);
  if (Len>3) and (nl>=Len) then begin
    i:=nl; ok:=true;
    if IsPathDelimiter(Result,i) then dec(i);
    while not IsPathDelimiter(Result,i) and (i>0) do dec(i);
    if i=0 then Result:=Punkte+copy(Result,nl-Len+3,nl)
    else begin
      dec(i); j:=i;
      repeat
        while not IsPathDelimiter(Result,j) and (j>0) do dec(j);
        dec(j);
        if j<0 then Result:=Punkte+copy(Result,nl-Len+3,nl)
        else begin
          ok:=nl-i+j+4<=Len;
          end;
        until ok or (j<0);
      if ok then begin
        inc(j,2);
        delete (Result,j,succ(i-j)); insert (Punkte,Result,j);
        end;
      end;
    end;
  end;

{ ------------------------------------------------------------------- }
// Extract parent path
function ExtractParentPath (Dir : string) : string;
var
  i : integer;
begin
  Dir:=ExcludeTrailingPathDelimiter(Dir);
  i:=length(Dir);
  while not IsPathDelimiter(Dir,i) and (i>0) do dec(i);
  if i<=1 then Result:=Dir
  else Result:=copy(Dir,1,i-1);
  end;

// Extract last subdirectory from path
function ExtractLastDir (const Path : string) : string;
begin
  Result:=ExtractFileName(ExcludeTrailingPathDelimiter(Path));
  end;

// Extract first subdirectory from path and remove from Path
function ExtractFirstDir (var Path : string) : string;
var
  n : integer;
begin
  Result:='';
  if not ContainsFullPath(Path) then begin
    n:=Pos(PathDelim,Path);
    if n>0 then begin
      Result:=copy(Path,1,n-1); delete(Path,1,n);
      end
    else begin
      Result:=Path; Path:='';
      end;
    end;
  end;

function RemoveFirstDir (const Dir : string) : string;
var
  n : integer;
begin
  Result:=Dir;
  if not ContainsFullPath(Dir) then begin
    n:=Pos(PathDelim,Dir);
    if n>0 then delete(Result,1,n);
    end;
  end;

{ ------------------------------------------------------------------- }
// Check if Path2 is a subpath of Path1 (both are full paths)
function IsSubPath (const Path1,Path2 : string) : boolean;
begin
  Result:=(length(Path1)>0) and AnsiStartsText(SetDirName(Path1),SetDirName(Path2));
  end;

// Check if Path is a root path (e.g. "C:\..")
function IsRootPath (const Path : string) : boolean;
begin
  Result:=AnsiSameText(ExtractFileDrive(Path),ExcludeTrailingPathDelimiter(Path));
  end;

// Check if Path2 is identical to Path1 (both are full paths)
function IsSamePath (const Path1,Path2 : string) : boolean;
begin
  Result:=(length(Path1)>0) and AnsiSameText(SetDirName(Path1),SetDirName(Path2));
  end;

// Check if Path is an absolute path (starting with \ or drive)
function ContainsFullPath (const Path : string) : boolean;
begin
  if length(Path)>0 then Result:=(Path[1]=BSlash) or (pos(DPunkt,Path)>0)
  else Result:=false;
  end;

// Convert absolute path to relative and and vice versa
function MakeRelativePath (const BaseName,DestName: string) : string;
begin
  if AnsiSameText(SetDirName(Basename),SetDirName(DestName)) then Result:=''
  else if IsSubPath(Basename,DestName) then Result:=ExtractRelativePath(SetDirName(BaseName),DestName)
  else Result:=DestName
  end;

function MakeAbsolutePath (const BaseName,DestName: string) : string;
begin
  if ContainsFullPath(DestName) then Result:=DestName
  else Result:=SetDirName(BaseName)+DestName;
  end;

// Expand relative path "DestName" to absolute path based on "BaseName"
function ExpandRelativePath (const BaseName,DestName: string) : string;
var
  Dst: array[0..MAX_PATH-1] of char;
begin
  if ContainsFullPath(DestName) then Result:=DestName
  else begin
    if PathCanonicalize(@Dst[0],PChar(IncludeTrailingBackslash(BaseName)+DestName))
      then Result:=Dst
    else Result:=DestName;
    end;
  end;

// Expand relative path "DestName" to absolute path based on "CurrentDir"
function ExpandPath (const DestName: string) : string;
begin
  if ContainsFullPath(DestName) then Result:=DestName
  else Result:=ExpandFileName(Destname);
  end;

// Extract identical leading path fragments
function ExtractSamePath(const Path1,Path2 : string) : string;
var
  i,n1,n2     : integer;
  s1,s2 : string;
  ok    : boolean;
begin
  s1:=AnsiLowerCase(IncludeTrailingPathDelimiter(Path1));
  s2:=AnsiLowerCase(IncludeTrailingPathDelimiter(Path2));
  n2:=0;
  repeat
    n1:=PosEx('\',s1,n2+1);
    ok:=(n1>0) and (n1=PosEx('\',s2,n2+1));
    if ok then for i:=n2+1 to n1-1 do ok:=ok and (s1[i]=s2[i]);
    if ok then n2:=n1;
    until not ok or (n1>length(s1)) or (n1>length(s2));
  if n2>0 then Result:=copy(Path1,1,n2) else Result:='';
  end;

// Get last existing parent path, set DefPath if not found
function GetExistingParentPath (const Path,DefPath : string) : string;
var
  sd : string;
begin
  if (length(Path)=0) or not ContainsFullPath(Path) then Result:=DefPath
  else begin
    Result:=Path; sd:=ExtractFileDrive(Path);
    if not DirectoryExists(Result) then begin
      if (copy(Path,1,2)='\\') then Result:=''
      else while (length(Result)>0) and not DirectoryExists(Result) do begin
        if AnsiSameText(Result,sd) then Result:='' else Result:=ExtractParentPath(Result);
        end;
      if length(Result)=0 then Result:=DefPath;
      end;
    end;
  end;

// Replace drive letter in path
function ReplaceDrive(const Path,Drive : string) : string;
begin
  Result:=Path;
  if (length(Path)>0) and (length(Drive)>0) then Result[1]:=Drive[1];
  end;

// Remove drive
function RemoveDrive (const Path : string) : string;
var
  dr : string;
begin
  dr:=ExtractFileDrive(Path);
  if length(dr)>0 then Result:=MakeRelativePath(dr,Path)
  else Result:=Path;
  end;

{ ---------------------------------------------------------------- }
// Check for illegal characters in path
function CheckPathChars (APath : string) : boolean;
var
  s : string;

  function ReadNext (var s   : String) : string;
  var
    i : integer;
  begin
    if length(s)>0 then begin
      i:=pos (BSlash,s);
      if i=0 then i:=succ(length(s));
      Result:=copy(s,1,pred(i));
      delete(s,1,i);
      end
    else Result:='';
    end;

begin
  Result:=true;
  if length(APath)>0 then begin
    if (copy(APath,1,2)='\\') then delete(APath,1,2);
    APath:=ExcludeTrailingPathDelimiter(APath);
    while Result and (length(APath)>0) do begin
      s:=ReadNext(APath);
      if (Length(s)>2) or not MatchesMask(s,'?:') then Result:=CheckFilename(s);
      end;
    end;
  end;

// Check for illegal characters in filename (without path)
function CheckFilename (const Filename : string) : boolean;
var
  i : integer;
begin
  Result:=true; i:=1;
  if length(Filename)>0 then while Result and (i<=length(Filename)) do begin
    Result:=(Filename[i]>#255) or not CharInSet(AnsiChar(Filename[i]),IllegalFilenameChars);
    inc(i);
    end;
  end;

// Replace illegal characters by AChar
function ReplaceIllegalChars (const Filename : string; AChar : char) : string;
var
  i : integer;
begin
  Result:=Filename;
  for i:=1 to length(Filename) do if Result[i]<=#255 then begin
    if CharInSet(AnsiChar(Result[i]),IllegalFilenameChars) then Result[i]:=AChar;
    end
  end;

{ ------------------------------------------------------------------- }
// Check for special directories ('.'=self and '..'=one up )
// or if directory starts with specified character
function NotSpecialDir (const Name : string; StartChars : array of char) : boolean;
var
  i : integer;
begin
  Result:=(Name<>'.') and (Name<>'..');
  if Result and (length(StartChars)>0) then begin
    for i:=Low(StartChars) to High(StartChars) do Result:=Result and (Pos(StartChars[i],Name)<>1);
    end;
  end;

function NotSpecialDir (const Name : string) : boolean;
begin
  Result:=NotSpecialDir(Name,[]);
  end;

end.
