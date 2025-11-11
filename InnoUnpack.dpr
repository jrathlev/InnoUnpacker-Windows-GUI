program InnoUnpack;

{$R 'languages.res' 'languages.rc'}

uses
  GnuGetText in '..\..\Bibliotheken\Units\GnuGetText.pas',
  LangUtils in '..\..\Bibliotheken\Units\LangUtils.pas',
  Vcl.Forms,
  Vcl.Graphics,
  UnpackMain in 'UnpackMain.pas' {MainForm},
  ShellDirDlg in '..\..\Bibliotheken\dialogs\ShellDirDlg.pas' {ShellDirDialog},
  SelectFromListDlg in '..\..\Bibliotheken\Dialogs\SelectFromListDlg.pas' {SelectFromListDialog},
  SelectListItems in '..\..\Bibliotheken\Dialogs\SelectListItems.pas' {SelectListItemsDialog};

{$R *.res}

begin
  TP_GlobalIgnoreClass(TFont);
  // Subdirectory in AppData for user configuration files and supported languages
  InitTranslation(['delphi10','units']);

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TShellDirDialog, ShellDirDialog);
  Application.CreateForm(TSelectFromListDialog, SelectFromListDialog);
  Application.CreateForm(TSelectListItemsDialog, SelectListItemsDialog);
  Application.Run;
end.

