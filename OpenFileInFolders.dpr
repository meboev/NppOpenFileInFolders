library OpenFileInFolders;

uses
  SysUtils,
  Classes,
  Types,
  Windows,
  Messages,
  nppplugin in 'lib\nppplugin.pas',
  scisupport in 'lib\SciSupport.pas',
  NppForms in 'lib\NppForms.pas' {NppForm},
  OpenFileInFoldersPlugin in 'OpenFileInFoldersPlugin.pas',
  AboutForms in 'AboutForms.pas' {AboutForm},
  ShowForms in 'ShowForms.pas' {ShowForm};

{$R *.res}

{$Include 'lib\NppPluginInclude.pas'}

begin
  { First, assign the procedure to the DLLProc variable }
  DllProc := @DLLEntryPoint;
  { Now invoke the procedure to reflect that the DLL is attaching to the process }
  DLLEntryPoint(DLL_PROCESS_ATTACH);
end.
