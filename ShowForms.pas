unit ShowForms;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, NppForms, NppPlugin, FileCtrl, StrUtils, ShellCtrls, ExtCtrls,
  ComCtrls, Mask, ShellAPI, ShlObj;

type
  TShowForms = class(TNppForm)
    GetFilesButton: TButton;
    FoldersEdit: TEdit;
    FoldersLabel: TLabel;
    OpenFolderButton: TButton;
    FilesMemo: TMemo;
    SearchLabel: TLabel;
    SearchListBox: TListBox;
    ExcludesEdit: TEdit;
    ExcludesLabel: TLabel;
    MaxResultsLabel: TLabel;
    MaxResultsComboBox: TComboBox;
    StatusBar: TStatusBar;
    SearchMemo: TMemo;
    FilesLabel: TLabel;
    procedure OpenFolderButtonClick(Sender: TObject);
    procedure GetFilesButtonClick(Sender: TObject);
    procedure SearchListBoxClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SearchListBoxDblClick(Sender: TObject);
    procedure SearchMemoChange(Sender: TObject);
    procedure SearchMemoKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  end;

  TGetFilesThread = class(TThread)
  private
    FDirsStringList: TStringList;
    FExcludesStringList: TStringList;
    FFile: string;
    procedure AddAllFilesInDir(const Dir: string);
    procedure UpdateMainThread;
    procedure BeginUpdateMainThread;
    procedure EndUpdateMainThread;
  protected
    procedure Execute; override;
  public
    constructor Create(Dirs: string; Excludes: string);
    destructor Destroy; override;
  end;


var
  ShowForm: TShowForms;
  GetFilesThread: TGetFilesThread;
  GetFilesThreadTerminated: Boolean;

{$IFDEF UNICODE}
  function ILCreateFromPath(pszPath: PChar): PItemIDList stdcall; external shell32
    name 'ILCreateFromPathW';
{$ELSE}
  function ILCreateFromPath(pszPath: PChar): PItemIDList stdcall; external shell32
    name 'ILCreateFromPathA';
{$ENDIF}

procedure ILFree(pidl: PItemIDList) stdcall; external shell32;
function SHOpenFolderAndSelectItems(pidlFolder: PItemIDList; cidl: Cardinal;
  apidl: pointer; dwFlags: DWORD): HRESULT; stdcall; external shell32;

implementation

{$R *.dfm}

function OpenFolderAndSelectFile(const FileName: String): Boolean;
var
  IIDL: PItemIDList;
begin
  Result := false;
  IIDL := ILCreateFromPath(PChar(FileName));
  if IIDL <> nil then
    try
      Result := SHOpenFolderAndSelectItems(IIDL, 0, nil, 0) = S_OK;
    finally
      ILFree(IIDL);
    end;
end;

{ TGetFilesThread }

constructor TGetFilesThread.Create(Dirs: string; Excludes: string);
var
  i: Integer;
  tmp: String;
begin
  inherited Create(True);

  Dirs := StringReplace(Dirs, ' ', '*', [rfReplaceAll]);
  FDirsStringList := TStringList.Create;
  FDirsStringList.Delimiter := '|';
  FDirsStringList.DelimitedText := Dirs;
  for i := 0 to FDirsStringList.Count - 1 do
    FDirsStringList[i] := Trim(StringReplace(FDirsStringList[i], '*', ' ', [rfReplaceAll]));

  Excludes := StringReplace(Excludes, ' ', '*', [rfReplaceAll]);
  FExcludesStringList := TStringList.Create;
  FExcludesStringList.Delimiter := '|';
  FExcludesStringList.DelimitedText := Excludes;
  for i := 0 to FExcludesStringList.Count - 1 do
    FExcludesStringList[i] := Trim(StringReplace(FExcludesStringList[i], '*', ' ', [rfReplaceAll]));

  FreeOnTerminate := True;
end;

destructor TGetFilesThread.Destroy;
begin
  FExcludesStringList.Free;
  FDirsStringList.Free;
  
  GetFilesThreadTerminated := true;
  inherited;
end;

procedure TGetFilesThread.Execute;
var
  i: Integer;
begin
//  Synchronize(BeginUpdateMainThread);
  BeginUpdateMainThread;
  for i := 0 to FDirsStringList.Count - 1 do
    AddAllFilesInDir(FDirsStringList[i]);
  EndUpdateMainThread;
//  Synchronize(EndUpdateMainThread);
end;

procedure TGetFilesThread.AddAllFilesInDir(const Dir: string);
var
  SR: TSearchRec;
  Excluded: Boolean;
  FullPath: String;
  i: Integer;
begin
  if FindFirst(IncludeTrailingBackslash(Dir) + '*.*', faAnyFile or faDirectory, SR) = 0 then
    try
      repeat
        if Terminated then Exit;
        FullPath := IncludeTrailingBackslash(Dir) + SR.Name;
        Excluded := false;
        for i := 0 to FExcludesStringList.Count - 1 do
          if (SR.Name = '.') or (SR.Name = '..') or
            AnsiContainsText(FullPath, FExcludesStringList[i]) then
            begin
              Excluded := true;
              Break;
            end;
        if Excluded then Continue;

        if (SR.Attr and faDirectory) = 0 then
          begin
            FFile := FullPath;
            //Synchronize(UpdateMainThread);
            UpdateMainThread;
            Continue;
          end;

        AddAllFilesInDir(FullPath);
     until FindNext(Sr) <> 0;
   finally
     FindClose(SR);
   end;
end;

procedure TGetFilesThread.UpdateMainThread;
begin
  ShowForm.FilesMemo.Lines.Add(FFile);
  ShowForm.StatusBar.SimpleText := 'Loading files: ' +
    IntToStr(ShowForm.FilesMemo.Lines.Count) + '...';
end;

procedure TGetFilesThread.BeginUpdateMainThread;
begin
  ShowForm.FilesMemo.Lines.Clear;
  ShowForm.FilesMemo.Update;
  ShowForm.FilesMemo.Lines.BeginUpdate;
end;

procedure TGetFilesThread.EndUpdateMainThread;
begin
  ShowForm.StatusBar.SimpleText := 'Loaded files: ' +
    IntToStr(ShowForm.FilesMemo.Lines.Count);
  ShowForm.FilesMemo.Lines.EndUpdate;
end;


{ TShowForms }

procedure TShowForms.OpenFolderButtonClick(Sender: TObject);
var d: String;
begin
  if SelectDirectory('Select a directory', '', d) then begin
    FoldersEdit.Text := Trim(FoldersEdit.Text);
    if (FoldersEdit.Text = '') then
      FoldersEdit.Text := d
    else
      FoldersEdit.Text := FoldersEdit.Text + ' | ' + d;
  end;
end;

procedure TShowForms.GetFilesButtonClick(Sender: TObject);
begin
  while not GetFilesThreadTerminated do begin
    if GetFilesThread <> nil then GetFilesThread.Terminate;
    Application.ProcessMessages;
  end;

  GetFilesThreadTerminated := false;
  GetFilesThread := TGetFilesThread.Create(FoldersEdit.Text, ExcludesEdit.Text);
  GetFilesThread.Resume;
end;

procedure TShowForms.SearchListBoxClick(Sender: TObject);
begin
  SearchMemo.SetFocus;
end;

procedure TShowForms.FormShow(Sender: TObject);
begin
  GetFilesButtonClick(nil);

  SearchMemo.SetFocus;
  SearchMemo.SelectAll;
end;

procedure TShowForms.FormCreate(Sender: TObject);
begin
  GetFilesThreadTerminated := true;

  ExcludesEdit.DoubleBuffered := true;
  FoldersEdit.DoubleBuffered := true;
  MaxResultsComboBox.DoubleBuffered := true;
  SearchMemo.DoubleBuffered := true;
  FilesMemo.DoubleBuffered := true;
//  SearchListBox.DoubleBuffered := true;
  StatusBar.DoubleBuffered := true;
end;

procedure TShowForms.SearchListBoxDblClick(Sender: TObject);
var
  ret: Word;
begin
  ret := 13;
  SearchMemoKeyDown(nil, ret, []);
end;

procedure TShowForms.SearchMemoChange(Sender: TObject);
var
  i: Integer;
  max: Integer;
  s: String;
begin
  s := SearchMemo.Lines.Text;
  s := StringReplace(s, #13, '', [rfReplaceAll]);
  s := StringReplace(s, #10, '', [rfReplaceAll]);
  SearchMemo.Lines.Text := s;

  SearchListBox.Items.BeginUpdate;
  SearchListBox.Items.Clear;
  max := StrToIntDef(MaxResultsComboBox.Text, -1);
  for i := 0 to FilesMemo.Lines.Count - 1 do
    if AnsiContainsText(FilesMemo.Lines[i], SearchMemo.Lines.Text) then
      begin
        SearchListBox.Items.Add(FilesMemo.Lines[i]);
        if (max <> -1) and (SearchListBox.Items.Count >= max) then Break;
      end;
  SearchListBox.ItemIndex := 0;
  SearchListBox.Items.EndUpdate;
end;

procedure TShowForms.SearchMemoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  filename: nppString;
begin
  inherited;

  if SearchListBox.ItemIndex < 0 then SearchListBox.ItemIndex := 0;
  case Key of
    VK_DOWN:
      begin
        SearchListBox.ItemIndex := SearchListBox.ItemIndex + 1;
        Key := 0;
      end;
    VK_UP:
      begin
        if SearchListBox.ItemIndex <> 0 then
          SearchListBox.ItemIndex := SearchListBox.ItemIndex - 1;
        Key := 0;
      end;
    VK_PRIOR: // Page Up
      begin
        if SearchListBox.ItemIndex - 5 >= 0 then
          SearchListBox.ItemIndex := SearchListBox.ItemIndex - 5
        else
          SearchListBox.ItemIndex := 0;
        Key := 0;
      end;
    VK_NEXT: // Page Down
      begin
        if SearchListBox.ItemIndex + 5 < SearchListBox.Items.Count - 1  then
          SearchListBox.ItemIndex := SearchListBox.ItemIndex + 5
        else
          SearchListBox.ItemIndex := SearchListBox.Items.Count - 1;
        Key := 0;
      end;
    VK_RETURN:
      begin
        Key := 0;
        if SearchListBox.ItemIndex >= 0 then
          begin
            filename := SearchListBox.Items[SearchListBox.ItemIndex];
            if (Shift = [ssCtrl]) then
              begin
                if not OpenFolderAndSelectFile(filename) then
                  SendMessage(self.Npp.NppData.NppHandle, WM_DOOPEN, 0, LPARAM(PChar(filename)));
              end
            else
              SendMessage(self.Npp.NppData.NppHandle, WM_DOOPEN, 0, LPARAM(PChar(filename)));
            SearchListBox.Clear;
            SearchMemo.Clear;
            Close;
          end;
      end;
  end;
end;

procedure TShowForms.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  while not GetFilesThreadTerminated do begin
    if GetFilesThread <> nil then GetFilesThread.Terminate;
    Application.ProcessMessages;
  end;
end;

end.
