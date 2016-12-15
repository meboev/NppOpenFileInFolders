unit OpenFileInFoldersPlugin;

interface

uses
  NppPlugin, SysUtils, Windows, SciSupport, AboutForms, ShowForms;

type
  TOpenFileInFoldersPlugin = class(TNppPlugin)
  public
    constructor Create;
    procedure FuncShow;
    procedure FuncAbout;
  end;

procedure _FuncShow; cdecl;
procedure _FuncAbout; cdecl;

var
  Npp: TOpenFileInFoldersPlugin;

implementation

{ TOpenFileInFoldersPlugin }

constructor TOpenFileInFoldersPlugin.Create;
var
  sk: TShortcutKey;
begin
  inherited;
  self.PluginName := 'Open File In Folders';

  sk.IsCtrl := true; sk.IsAlt := false; sk.IsShift := true;
  sk.Key := #82; // CTRL SHIFT R
  self.AddFuncItem('Show Open File In Folders', _FuncShow, sk);
  self.AddFuncItem('About', _FuncAbout);
end;

procedure _FuncShow; cdecl;
begin
  Npp.FuncShow;
end;

procedure _FuncAbout; cdecl;
begin
  Npp.FuncAbout;
end;

procedure TOpenFileInFoldersPlugin.FuncShow;
begin
  if (not Assigned(ShowForm)) then ShowForm := TShowForms.Create(self);
  ShowForm.Show;
end;

procedure TOpenFileInFoldersPlugin.FuncAbout;
var
  a: TAboutForm;
begin
  a := TAboutForm.Create(self);
  a.ShowModal;
  a.Free;
end;

initialization
  Npp := TOpenFileInFoldersPlugin.Create;
end.
