; Setup script for InnoUnpack (32 bit)
; ====================================
;  © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

;  The contents of this file may be used under the terms of the
;  Mozilla Public License ("MPL") or
;  GNU Lesser General Public License Version 2 or later (the "LGPL")

;  Software distributed under this License is distributed on an "AS IS" basis,
;  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
;  the specific language governing rights and limitations under the License.

;  Compiles with Inno Setup 6.1.2

#define ApplicationVersion GetVersionNumbersString('..\Release\Win32\InnoUnpack.exe')
#define Year GetDateTimeString('yyyy','','')
#define ProgramName "Unpack InnoSetup"
#define ProgramAuthor "Dr. J. Rathlev"
#define ProgramWebURL "http://www.rathlev-home.de/?tools/progtools.html"
#define OutputFile "InnoUnpack-setup"

[Setup]
PrivilegesRequired=admin
AppName={#ProgramName}
AppVerName={#ProgramName} {#ApplicationVersion}
AppVersion={#ApplicationVersion}
AppPublisher={#ProgramAuthor}
AppPublisherURL={#ProgramWebURL}
AppSupportURL={#ProgramWebURL}
AppUpdatesURL={#ProgramWebURL}
AppCopyright=© 2014-{#Year} {#ProgramAuthor}
VersionInfoVersion={#ApplicationVersion}
DefaultDirName={commonpf}\{#ProgramName}
DefaultGroupName={#ProgramName}
OutputDir=.
OutputBaseFilename={#OutputFile}
SetupIconFile=..\Unpack-View.ico
UninstallDisplayIcon={app}\Unpack-View.ico
WizardImageFile=..\..\..\Common\WizImage-JR.bmp
WizardSmallImageFile=..\..\..\Common\WizSmallImage-JR.bmp
Compression=lzma
SolidCompression=yes
ChangesAssociations=yes
ShowLanguageDialog=yes
DisableDirPage=auto
DisableProgramGroupPage=auto

[Languages]
Name: "en"; MessagesFile: compiler:Default.isl;           LicenseFile:"..\..\..\Common\license-en.rtf";
Name: "de"; MessagesFile: compiler:Languages\German.isl;  LicenseFile:"..\..\..\Common\license-de.rtf";
Name: "fr"; MessagesFile: compiler:Languages\French.isl;  LicenseFile:"..\..\..\Common\license-en.rtf";
Name: "it"; MessagesFile: compiler:Languages\Italian.isl; LicenseFile:"..\..\..\Common\license-it.rtf";

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "..\Release\Win32\InnoUnpack.exe"; DestDir: "{app}"; Flags: ignoreversion restartreplace
Source: "..\Release\Win32\innounp.exe"; DestDir: "{app}"; Flags: ignoreversion restartreplace
Source: "..\Release\Win32\innounp.htm"; DestDir: "{app}"; Flags: ignoreversion restartreplace
Source: "..\Release\Win32\locale\*.mo"; DestDir: "{app}\locale"; Flags: recursesubdirs ignoreversion restartreplace
Source: "..\Release\Win32\language.cfg"; DestDir: "{app}"; Flags: recursesubdirs ignoreversion restartreplace
Source: "..\..\..\Common\license-*.rtf"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Unpack-View.ico"; DestDir: "{app}"; Flags: ignoreversion
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{group}\{#ProgramName}"; Filename: "{app}\InnoUnpack.exe"
Name: "{group}\{cm:UninstallProgram,{#ProgramName}}"; Filename: "{uninstallexe}"
Name: "{commondesktop}\{#ProgramName}"; Filename: "{app}\InnoUnpack.exe"; Tasks: desktopicon

[Run]
Filename: "{app}\InnoUnpack.exe"; Description: "{cm:LaunchProgram,Unpack InnoSetup}"; Flags: nowait postinstall runasoriginaluser

[Code]
function ReadVersionNumber (const FName : string) : string;
var
  sv : string;
begin
  if GetVersionNumbersString(FName,sv) then Result:=sv else Result:='1.0';
  end;


