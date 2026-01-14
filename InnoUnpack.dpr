program InnoUnpack;

{$R 'languages.res' 'languages.rc'}

uses
  GnuGetText in 'units\GnuGetText.pas',
  LangUtils in 'units\LangUtils.pas',
  Vcl.Forms,
  Vcl.Graphics,
  UnpackMain in 'UnpackMain.pas' {MainForm},
  ShellDirDlg in 'units\ShellDirDlg.pas' {ShellDirDialog},
  SelectFromListDlg in 'units\SelectFromListDlg.pas' {SelectFromListDialog},
  SelectListItems in 'units\SelectListItems.pas' {SelectListItemsDialog},
  Vcl.Themes,
  Vcl.Styles;

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

