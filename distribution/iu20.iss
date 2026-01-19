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
UninstallDisplayIcon={app}\inno-unpacker-u-2.ico
WizardImageFile=WizImage-JR.bmp
WizardSmallImageFile=WizSmallImage-JR.bmp
Compression=lzma
SolidCompression=yes
ChangesAssociations=yes
ShowLanguageDialog=yes
DisableDirPage=auto
DisableProgramGroupPage=auto
WizardStyle=modern dynamic 

[Languages]
Name: "en"; MessagesFile: compiler:Default.isl;           LicenseFile:"license-en.rtf";
Name: "de"; MessagesFile: compiler:Languages\German.isl;  LicenseFile:"license-de.rtf";
Name: "fr"; MessagesFile: compiler:Languages\French.isl;  LicenseFile:"license-fr.rtf";
Name: "it"; MessagesFile: compiler:Languages\Italian.isl; LicenseFile:"license-it.rtf";
Name: "es"; MessagesFile: compiler:Languages\Spanish.isl; LicenseFile:"license-es.rtf";
Name: "zh"; MessagesFile: compiler:Languages\ChineseSimplified.isl; LicenseFile:"license-en.rtf";

[CustomMessages]
en.FileAssoc=File associations:
en.DescContext=Add "InnoUnpacker" to context menu of exe files
en.InnoUnpack=Open &Inno Setup installer with InnoUnpacker

fr.FileAssoc=Associations de fichiers:
fr.DescContext=Ajouter "InnoUnpacker" au menu contextuel des fichiers exe
fr.InnoUnpack=Ouvrez le programme d'installation &Inno Setup avec InnoUnpacker

de.FileAssoc=Dateizuordnungen:
de.DescContext=Füge "InnoUnpacker" zum Kontext-Menü für exe-Dateien hinzu
de.InnoUnpack=Öffne &Inno Setup Installer mit InnoUnpacker

it.FileAssoc=Associazione file:
it.DescContext=Aggiungi "InnoUnpacker" al menu contestuale dei file exe
it.InnoUnpack=Apri installer &Inno Setup con InnoUnpacker

es.FileAssoc=Asociaciones de fichero:
es.DescContext=Añadir "InnoUnpacker" al menú contextual de ficheros exe
es.InnoUnpack=Abre el instalador de &Inno Setup con InnoUnpacker

zh.FileAssoc=文件关联:
zh.DescContext=将 "InnoUnpacker" ”添加到exe文件的右键菜单中
zh.InnoUnpack=使用InnoUnpacker打开Inno Setup安装程序

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked
Name: "fileassoc"; Description: "{cm:DescContext}"; GroupDescription: "{cm:FileAssoc}"; 

[Registry]
Root: HKA; Subkey: "SOFTWARE\Classes\exefile\shell\InnoUnpack"; ValueType: string; ValueName: ""; ValueData: "{cm:InnoUnpack}"; Tasks: fileassoc; Flags: deletevalue
Root: HKA; Subkey: "SOFTWARE\Classes\exefile\shell\InnoUnpack\Command"; ValueType: string; ValueName: ""; ValueData: """{app}\InnoUnpack.exe"" ""%1"""; Tasks: fileassoc; Flags: deletevalue

[Files]
Source: "..\Release\Win32\InnoUnpack.exe"; DestDir: "{app}"; Flags: ignoreversion restartreplace
Source: "..\innounp-2\Release\innounp.exe"; DestDir: "{app}"; Flags: ignoreversion restartreplace
Source: "..\innounp-2\docs\innounp.htm"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\innounp-2\docs\innounp.css"; DestDir: "{app}"; Flags: ignoreversion
;Source: "..\Release\Win32\locale\*.mo"; DestDir: "{app}\locale"; Flags: recursesubdirs ignoreversion restartreplace
;Source: "..\Release\Win32\language.cfg"; DestDir: "{app}"; Flags: recursesubdirs ignoreversion restartreplace
Source: "..\InnoUnpack.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\changelog.txt"; DestDir: "{app}"; Flags: ignoreversion
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


