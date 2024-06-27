unit Msgs;

{
  Inno Setup
  Copyright (C) 1997-2004 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Message file handling functions

  $jrsoftware: issrc/Projects/Msgs.pas,v 1.10 2004/02/28 20:48:18 jr Exp $
}

interface

uses
  MsgIDs, Struct;

const
  SNewLine = #13#10;  { line break }
  SNewLine2 = #13#10#13#10;  { double line break }

var
  SetupMessages: array[TSetupMessageID] of String;

function FmtMessage(S: PChar; const Args: array of String): String;
function FmtSetupMessage(const ID: TSetupMessageID; const Args: array of String): String;
function FmtSetupMessage1(const ID: TSetupMessageID; const Arg1: String): String;

const
  { You don't have to translate these. The only time they are used is when an
    error occurs before or while the messages file is loaded. Otherwise, it
    uses the corresponding messages in the messages file. }
  SSetupFileMissing = 'The file %1 is missing from the installation directory. ' +
    'Please correct the problem or obtain a new copy of the program.';
  SSetupFileCorrupt = 'Failed to recognize the file(s) as a valid Inno Setup installer. ' +
    'The setup files may be corrupted or made by an incompatible version.';
  SSetupFileCorruptOrWrongVer = 'The setup files are corrupted, or are ' +
    'incompatible with this version of Setup. Please correct the problem or ' +
    'obtain a new copy of the program.';
  SMsgsFileMissing = 'Messages file "%s" is missing. Please correct ' +
    'the problem or obtain a new copy of the program.';

implementation

uses
  Winapi.Windows, System.SysUtils, Compress, CmnFunc2, FileClass;

const
  SMsgsFileTooLarge = 'Internal error: Messages file is too large';

function FmtMessage(S: PChar; const Args: array of String): String;
var
  P: PChar;
  Z: String;
begin
  Result := '';
  if S = nil then Exit;
  while True do begin
    P := StrScan(S, '%');
    if P = nil then begin
      Result := Result + S;
      Break;
    end;
    if P <> S then begin
      SetString(Z, S, P - S);
      Result := Result + Z;
      S := P;
    end;
    Inc(P);
    if (P^ >= '1') and (Ord(P^) <= Ord('1') + High(Args)) then begin
      Result := Result + Args[Ord(P^) - Ord('1')];
      Inc(S, 2);
    end
    else begin
      Result := Result + '%';
      Inc(S);
      if P^ = '%' then
        Inc(S);
    end;
  end;
end;

function FmtSetupMessage(const ID: TSetupMessageID; const Args: array of String): String;
begin
  Result := FmtMessage(PChar(SetupMessages[ID]), Args);
end;

function FmtSetupMessage1(const ID: TSetupMessageID; const Arg1: String): String;
begin
  Result := FmtSetupMessage(ID, [Arg1]);
end;

procedure FreeSetupMessages;
var
  I: TSetupMessageID;
begin
  for I := Low(SetupMessages) to High(SetupMessages) do
    SetupMessages[I] := '';
end;


initialization
  SetupMessages[msgErrorReadingSource]:='An error occurred while trying to read the source file:';
  SetupMessages[msgErrorCopying]:='An error occurred while trying to copy a file:';
  SetupMessages[msgErrorCreatingDir]:='Setup was unable to create the directory "%1"';
  SetupMessages[msgLastErrorMessage]:='%1.%n%nError %2: %3';
  SetupMessages[msgSetupFileCorrupt]:='The setup files are corrupted. Please obtain a new copy of the program.';
  SetupMessages[msgSetupFileCorruptOrWrongVer]:='The setup files are corrupted, or are incompatible with this version of Setup. Please correct the problem or obtain a new copy of the program.';
  SetupMessages[msgErrorRegisterServerMissingExport]:='DllRegisterServer export not found';
  SetupMessages[msgErrorTitle]:='Error';
  SetupMessages[msgErrorRestartingComputer]:='Setup was unable to restart the computer. Please do this manually.';
  SetupMessages[msgErrorRegOpenKey]:='Error opening registry key:%n%1\%2';
  SetupMessages[msgErrorTooManyFilesInDir]:='Unable to create a file in the directory "%1" because it contains too many files';
  SetupMessages[msgErrorFunctionFailed]:='%1 failed; code %2';
  SetupMessages[msgErrorFunctionFailedWithMessage]:='%1 failed; code %2.%n%3';
  SetupMessages[msgSourceIsCorrupted]:='The source file is corrupted';
  SetupMessages[msgIncorrectPassword]:='Incorrect password specified';

finalization
  FreeSetupMessages;
end.
