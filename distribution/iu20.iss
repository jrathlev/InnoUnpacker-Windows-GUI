; Setup script for InnoUnpacker (32 bit)
; ======================================
;  © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

;  The contents of this file may be used under the terms of the
;  Mozilla Public License ("MPL") or
;  GNU Lesser General Public License Version 2 or later (the "LGPL")

;  Software distributed under this License is distributed on an "AS IS" basis,
;  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
;  the specific language governing rights and limitations under the License.

;  Compiles with Inno Setup 6.7.0

#define DeleteLastDigitFromVersion(version) Copy(version, 1, (RPos(".", version)) - 1)
#define ApplicationVersion DeleteLastDigitFromVersion(GetVersionNumbersString('..\Release\Win32\InnoUnpack.exe'))
#define Year GetDateTimeString('yyyy','','')
#define ProgramName "InnoUnpacker"
#define ProgramAuthor "Dr. J. Rathlev"
#define ProgramWebURL "http://www.rathlev-home.de/?tools/progtools.html"
#define OutputFile "InnoUnpacker-setup"

[Setup]
PrivilegesRequired=lowest
PrivilegesRequiredOverridesAllowed=dialog
UsePreviousPrivileges=no
AppName={#ProgramName}
AppVersion={#ApplicationVersion}
AppVerName={#ProgramName} {#ApplicationVersion}
AppPublisher={#ProgramAuthor}
AppPublisherURL={#ProgramWebURL}
AppSupportURL={#ProgramWebURL}
AppUpdatesURL={#ProgramWebURL}
AppCopyright=2014-{#Year} {#ProgramAuthor}
VersionInfoVersion={#ApplicationVersion}
VersionInfoDescription={#ProgramName} Setup
DefaultDirName={autopf}\{#ProgramName}
DefaultGroupName={#ProgramName}
OutputDir=.
OutputBaseFilename={#OutputFile}
SetupIconFile=..\inno-unpacker-2.ico
UninstallDisplayName={#ProgramName}
UninstallDisplayIcon={app}\inno-unpacker-u-2.ico
WizardImageFile=Wizard-IU.png
WizardSmallImageFile=Wizard-small.png
Compression=lzma
SolidCompression=yes
ChangesAssociations=yes
ShowLanguageDialog=yes
DisableDirPage=auto
DisableProgramGroupPage=auto
WizardStyle=modern dynamic 

[Languages]
Name: "en"; MessagesFile: compiler:Default.isl;           LicenseFile:"license-en.rtf";
#include "iu-20-lang.inc"

[CustomMessages]
en.FileAssoc=File associations:
en.DescContext=Add "InnoUnpacker" to context menu of exe files
en.InnoUnpack=Open &Inno Setup installer with InnoUnpacker
#include "iu-20-cmsg.inc"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked
Name: "fileassoc"; Description: "{cm:DescContext}"; GroupDescription: "{cm:FileAssoc}"; 

[Registry]
Root: HKA; Subkey: "SOFTWARE\Classes\exefile\shell\InnoUnpack"; ValueType: string; ValueName: ""; ValueData: "{cm:InnoUnpack}"; Tasks: fileassoc; Flags: uninsdeletekey
Root: HKA; Subkey: "SOFTWARE\Classes\exefile\shell\InnoUnpack\Command"; ValueType: string; ValueName: ""; ValueData: """{app}\InnoUnpack.exe"" ""%1"""; Tasks: fileassoc; Flags: uninsdeletekey

[Files]
Source: "..\Release\Win32\InnoUnpack.exe"; DestDir: "{app}"; Flags: ignoreversion restartreplace
Source: "..\innounp-2\Release\innounp.exe"; DestDir: "{app}"; Flags: ignoreversion restartreplace
Source: "..\innounp-2\docs\innounp.htm"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\innounp-2\docs\innounp.css"; DestDir: "{app}"; Flags: ignoreversion
;Source: "..\Release\Win32\locale\*.mo"; DestDir: "{app}\locale"; Flags: recursesubdirs ignoreversion restartreplace
;Source: "..\Release\Win32\language.cfg"; DestDir: "{app}"; Flags: recursesubdirs ignoreversion restartreplace
Source: "..\Docs\InnoUnpack.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Docs\\changelog.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\..\Common\license-*.rtf"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\inno-unpacker-2.ico"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\inno-unpacker-u-2.ico"; DestDir: "{app}"; Flags: ignoreversion
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{group}\{#ProgramName}"; Filename: "{app}\InnoUnpack.exe"
Name: "{group}\{cm:UninstallProgram,{#ProgramName}}"; Filename: "{uninstallexe}"
Name: "{commondesktop}\{#ProgramName}"; Filename: "{app}\InnoUnpack.exe"; Tasks: desktopicon

[Run]
Filename: "{app}\InnoUnpack.exe"; Description: "{cm:LaunchProgram,{#ProgramName}}"; Flags: nowait postinstall runasoriginaluser


