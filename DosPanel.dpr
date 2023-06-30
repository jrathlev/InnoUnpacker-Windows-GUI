program DosPanel;

uses
  GnuGetText in 'units\GnuGetText.pas',
  LangUtils in 'units\LangUtils.pas',
  Forms,
  Graphics,
  DosPanelMain in 'DosPanelMain.pas' {frmMain},
  Settings in 'Settings.pas' {DosBoxSetDialog},
  ShellDirDlg in 'units\ShellDirDlg.pas' {ShellDirDialog},
  AppSettings in 'AppSettings.pas' {AppSettingsDialog},
  SelectFromListDlg in 'units\SelectFromListDlg.pas' {SelectFromListDialog},
  ShowMemo in 'units\ShowMemo.pas' {ShowTextDialog},
  TxtConvertDlg in 'TxtConvertDlg.pas' {TxtConvertDialog},
  FileCopy in 'units\FileCopy.pas';

{$R *.res}

begin
  TP_GlobalIgnoreClass(TFont);
  InitTranslation('','',['delphi10','units']);
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TDosBoxSetDialog, DosBoxSetDialog);
  Application.CreateForm(TShellDirDialog, ShellDirDialog);
  Application.CreateForm(TAppSettingsDialog, AppSettingsDialog);
  Application.CreateForm(TSelectFromListDialog, SelectFromListDialog);
  Application.CreateForm(TShowTextDialog, ShowTextDialog);
  Application.CreateForm(TTxtConvertDialog, TxtConvertDialog);
  Application.Run;
end.
