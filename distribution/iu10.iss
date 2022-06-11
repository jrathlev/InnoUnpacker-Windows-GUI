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

[Setup]
PrivilegesRequired=admin
AppName=InnoUnpack
AppVerName=InnoUnpack {#ApplicationVersion}
AppVersion={#ApplicationVersion}
AppPublisher=Dr. J. Rathlev
AppPublisherURL=http://www.rathlev-home.de/?tools/progtools.html
AppSupportURL=http://www.rathlev-home.de/?tools/progtools.html
AppUpdatesURL=http://www.rathlev-home.de/?tools/progtools.html
AppCopyright=Copyright © 2014-{#Year} Dr. J. Rathlev
VersionInfoVersion={#ApplicationVersion}
DefaultDirName={commonpf}\Unpack InnoSetup
DefaultGroupName=Unpack InnoSetup
OutputDir=.
OutputBaseFilename=InnoUnpack-setup
SetupIconFile=..\InnoUnpack_Icon.ico
UninstallDisplayIcon=..\InnoUnpack_Icon.ico
WizardImageFile=..\..\..\Common\WizImage-JR.bmp
WizardSmallImageFile=..\..\..\Common\WizSmallImage-JR.bmp
Compression=lzma
SolidCompression=yes
ChangesAssociations=yes
ShowLanguageDialog=auto
DisableDirPage=auto
DisableProgramGroupPage=auto

[Languages]
Name: "en"; MessagesFile: compiler:Default.isl; LicenseFile:"..\..\..\Common\license-en.rtf";
Name: "de"; MessagesFile: compiler:Languages\German.isl; LicenseFile:"..\..\..\Common\license-de.rtf";
Name: "it"; MessagesFile: compiler:Languages\French.isl; LicenseFile:"..\..\..\Common\license-en.rtf";
Name: "fr"; MessagesFile: compiler:Languages\Italian.isl; LicenseFile:"..\..\..\Common\license-en.rtf";

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "..\Release\Win32\InnoUnpack.exe"; DestDir: "{app}"; Flags: ignoreversion restartreplace
Source: "..\Release\Win32\innounp.exe"; DestDir: "{app}"; Flags: ignoreversion restartreplace
Source: "..\Release\Win32\innounp.htm"; DestDir: "{app}"; Flags: ignoreversion restartreplace
Source: "..\..\..\Common\license-*.rtf"; DestDir: "{app}"; Flags: ignoreversion
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{group}\Unpack InnoSetup"; Filename: "{app}\InnoUnpack.exe"
Name: "{group}\{cm:UninstallProgram,Unpack InnoSetup}"; Filename: "{uninstallexe}"
Name: "{commondesktop}\Unpack InnoSetup"; Filename: "{app}\InnoUnpack.exe"; Tasks: desktopicon

[Run]
Filename: "{app}\InnoUnpack.exe"; Description: "{cm:LaunchProgram,Unpack InnoSetup}"; Flags: nowait postinstall runasoriginaluser

[Code]
function ReadVersionNumber (const FName : string) : string;
var
  sv : string;
begin
  if GetVersionNumbersString(FName,sv) then Result:=sv else Result:='1.0';
  end;


