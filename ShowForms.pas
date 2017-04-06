unit ShowForms;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, NppForms, NppPlugin, SciSupport, FileCtrl, StrUtils, ShellCtrls, ExtCtrls,
  ComCtrls, Mask, ShellAPI, ShlObj, Registry;

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
    StopButton: TButton;
    GetFilesTimer: TTimer;
    procedure SaveSetting;
    procedure LoadSetting;
    function GetSelectedText: String;
    procedure OpenFolderButtonClick(Sender: TObject);
    procedure GetFilesButtonClick(Sender: TObject);
    procedure SearchListBoxClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SearchListBoxDblClick(Sender: TObject);
    procedure SearchMemoChange(Sender: TObject);
    procedure SearchMemoKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MaxResultsComboBoxClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure GetFilesTimerTimer(Sender: TObject);
    procedure FilesMemoKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FPrevSearch: String;
    FExcludesStringList: TStringList;
    procedure GetFiles;
    procedure AddAllFilesInDir(const Dir: string);
  end;

var
  ShowForm: TShowForms;
  SearchFromRegistry: String;
  isGetFilesStopped: Boolean;
  stopGetFiles: Boolean;
  closeAfterStopGetFiles: Boolean;


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

{ TShowForms }

procedure TShowForms.GetFiles;
var
  i: Integer;
  Dirs: String;
  Excludes: String;
  FDirsStringList: TStringList;
begin
  isGetFilesStopped := false;
  stopGetFiles := false;

  Dirs := FoldersEdit.Text;
  Excludes := ExcludesEdit.Text;

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

  FilesMemo.Lines.Clear;
  FilesMemo.Update;
  FilesMemo.Lines.BeginUpdate;

  for i := 0 to FDirsStringList.Count - 1 do
    AddAllFilesInDir(FDirsStringList[i]);

  FilesMemo.Lines.EndUpdate;

  StatusBar.SimpleText := 'Loaded files: ' +
    IntToStr(FilesMemo.Lines.Count);

  FPrevSearch := '';
  SearchMemoChange(nil);
  SearchMemo.SetFocus;

  FExcludesStringList.Free;
  FDirsStringList.Free;

  // Set the values as in the Form Show
  isGetFilesStopped := true;
  stopGetFiles := false;

  if closeAfterStopGetFiles then ShowForm.Close;
end;

procedure TShowForms.AddAllFilesInDir(const Dir: string);
var
  SR: TSearchRec;
  Excluded: Boolean;
  FullPath: String;
  i: Integer;
begin
  if stopGetFiles or Application.Terminated then Exit;
  if FindFirst(IncludeTrailingBackslash(Dir) + '*.*', faAnyFile or faDirectory, SR) = 0 then
    try
      repeat
        if stopGetFiles or Application.Terminated then Exit;
        Application.ProcessMessages;

        if (SR.Name = '.') or (SR.Name = '..') then Continue;

        Excluded := false;
        FullPath := IncludeTrailingBackslash(Dir) + SR.Name;
        for i := 0 to FExcludesStringList.Count - 1 do
          if AnsiContainsText(FullPath, FExcludesStringList[i]) then begin
            Excluded := true;
            Break;
          end;
        if Excluded then Continue;

        if (SR.Attr and faDirectory) = 0 then begin
          FilesMemo.Lines.Add(FullPath);
          StatusBar.SimpleText := 'Loading files: ' +
            IntToStr(FilesMemo.Lines.Count) + '...';
          Continue;
        end;

        AddAllFilesInDir(FullPath);
        if stopGetFiles or Application.Terminated then Exit;
     until FindNext(Sr) <> 0;
   finally
     FindClose(SR);
   end;
end;

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
  stopGetFiles := true;
  while not isGetFilesStopped do Application.ProcessMessages;
  stopGetFiles := false;

  SearchMemo.SetFocus;
  GetFiles;
end;

procedure TShowForms.FormCreate(Sender: TObject);
begin
  ExcludesEdit.DoubleBuffered := true;
  FoldersEdit.DoubleBuffered := true;
  MaxResultsComboBox.DoubleBuffered := true;
  SearchMemo.DoubleBuffered := true;
  FilesMemo.DoubleBuffered := true;
  StatusBar.DoubleBuffered := true;
end;

procedure TShowForms.SearchMemoChange(Sender: TObject);
var
  i: Integer;
  max: Integer;
  s, sWin, sLin: String;
  newSearch: TStringList;
begin
  if SearchMemo.Lines.Text = '' then begin
    SearchListBox.Items.Clear;
    FPrevSearch := '';
    Exit;
  end;

  s := SearchMemo.Lines.Text;
  s := StringReplace(s, #13, '', [rfReplaceAll]);
  s := StringReplace(s, #10, '', [rfReplaceAll]);
  sWin := StringReplace(s, '/', '\', [rfReplaceAll]);
  sLin := StringReplace(s, '\', '/', [rfReplaceAll]);
  SearchMemo.Lines.Text := s;

  if SearchMemo.Lines.Text = FPrevSearch then Exit;
  FPrevSearch := s;

  newSearch := TStringList.Create;
  max := StrToIntDef(MaxResultsComboBox.Text, -1);
  for i := 0 to FilesMemo.Lines.Count - 1 do
    if AnsiContainsText(FilesMemo.Lines[i], s) or
       AnsiContainsText(FilesMemo.Lines[i], sWin) or
       AnsiContainsText(FilesMemo.Lines[i], sLin) then
      begin
        newSearch.Add( FilesMemo.Lines[i] );
        if (max <> -1) and (newSearch.Count >= max) then Break;
      end;

  if (SearchListBox.Items.Text <> newSearch.Text) then
    begin
      SearchListBox.Items.Text := newSearch.Text;
      if SearchListBox.Items.Count > 0 then SearchListBox.Selected[0] := true;
    end;

  newSearch.Free;
end;

procedure TShowForms.SearchMemoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  filename: nppString;
  i, topSelected, newTopSelected, bottomSelected, newBottomSelected: Integer;
  shouldClose, shouldGoToEnd: Boolean;
begin
  topSelected := -1;
  newTopSelected := -1;
  bottomSelected := -1;
  newBottomSelected := -1;

  if (SearchListBox.Count > 0) and (
    (Key = VK_DOWN) or (Key = VK_UP) or
    (Key = VK_PRIOR) or (Key = VK_NEXT) or
    (Key = VK_RETURN) or (Key = VK_ESCAPE) ) then
    begin
      for i := 0 to SearchListBox.Count - 1 do
        if SearchListBox.Selected[i] then
          begin
            if topSelected = -1 then topSelected := i;
            bottomSelected := i;
          end;

      if topSelected = -1 then
        begin
          topSelected := 0;
          bottomSelected := 0;
          SearchListBox.Selected[0] := true;
        end;
    end;

  case Key of
    VK_DOWN:
      begin
        newBottomSelected := bottomSelected;
        if bottomSelected < SearchListBox.Count - 1 then
          newBottomSelected := bottomSelected + 1;

        if not (ssShift in Shift) and
          not (ssCtrl in Shift) then
            SearchListBox.ClearSelection;

        SearchListBox.Selected[ newBottomSelected ] := true;
        Key := 0;
      end;
    VK_UP:
      begin
        newTopSelected := topSelected;
        if topSelected > 0 then
          newTopSelected := topSelected - 1;

        if not (ssShift in Shift) and
          not (ssCtrl in Shift) then
            SearchListBox.ClearSelection;

        SearchListBox.Selected[ newTopSelected ] := true;
        Key := 0;
      end;
    VK_NEXT: // Page Down
      begin
        if SearchListBox.Items.Count > 0 then
          if bottomSelected + 5 < SearchListBox.Items.Count - 1  then
            newBottomSelected := bottomSelected + 5
          else
            newBottomSelected := SearchListBox.Items.Count - 1;

        if not (ssShift in Shift) and
          not (ssCtrl in Shift) then
            begin
              SearchListBox.ClearSelection;
              SearchListBox.Selected[ newBottomSelected ] := true;
            end
        else for i := bottomSelected to newBottomSelected do
          SearchListBox.Selected[ i ] := true;
        Key := 0;
      end;
    VK_PRIOR: // Page Up
      begin
        if SearchListBox.Items.Count > 0 then
          if topSelected - 5 > 0 then
            newTopSelected := topSelected - 5
          else
            newTopSelected := 0;

        if not (ssShift in Shift) and
          not (ssCtrl in Shift) then
            begin
              SearchListBox.ClearSelection;
              SearchListBox.Selected[ newTopSelected ] := true;
            end
        else for i := topSelected downto newTopSelected do
          SearchListBox.Selected[ i ] := true;
        Key := 0;
      end;
    VK_RETURN:
      begin
        Key := 0;
        shouldClose := false;
        shouldGoToEnd := false;
        if topSelected <> -1 then
          begin
            for i := 0 to SearchListBox.Count - 1 do
              if SearchListBox.Selected[i] then
                begin
                  filename := SearchListBox.Items[ i ];
                  if (Shift = [ssCtrl]) then
                    begin
                      if not OpenFolderAndSelectFile(filename) then begin
                        SendMessage(self.Npp.NppData.NppHandle, WM_DOOPEN, 0, LPARAM(PChar(filename)));
                        shouldClose := true;
                      end else shouldGoToEnd := true;
                    end
                  else begin
                    SendMessage(self.Npp.NppData.NppHandle, WM_DOOPEN, 0, LPARAM(PChar(filename)));
                    shouldClose := true;
                  end;
                end;
            if shouldClose then Close;
            if shouldGoToEnd then SearchMemo.SelStart := Length(SearchMemo.Text);
          end;
      end;
    VK_ESCAPE:
      begin
        Key := 0;
        if isGetFilesStopped then
          ShowForm.Close
        else begin
          stopGetFiles := true;
          closeAfterStopGetFiles := true;
        end;
      end;
  end;
end;

procedure TShowForms.FormShow(Sender: TObject);
var
  selectedText: String;
begin
  isGetFilesStopped := true;
  stopGetFiles := false;
  closeAfterStopGetFiles := false;

  LoadSetting;

  selectedText := GetSelectedText;
  if (selectedText <> '') then
    SearchMemo.Lines.Text := selectedText
  else if (SearchFromRegistry <> '') then
    SearchMemo.Lines.Text := SearchFromRegistry;

  SearchMemo.SetFocus;
  SearchMemo.SelectAll;


  GetFilesTimer.Enabled := true;
end;

procedure TShowForms.SaveSetting;
var
  Reg: TRegistry;
begin
  Reg:= TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('SOFTWARE\NPPOpenFileInFoldersPlugin', true) then begin
      Reg.WriteString('Excludes', ExcludesEdit.Text);
      Reg.WriteString('Folders', FoldersEdit.Text);
      Reg.WriteInteger('MaxResultsItemIndex', MaxResultsComboBox.ItemIndex);
      Reg.WriteString('Search', SearchMemo.Lines.Text);
      Reg.WriteInteger('Height', ShowForm.Height);
      Reg.WriteInteger('Width', ShowForm.Width);
    end;
  finally
    Reg.Free;
  end;
end;

procedure TShowForms.LoadSetting;
var
  Reg: TRegistry;
begin
  ExcludesEdit.Text := '';
  FoldersEdit.Text := '';
  MaxResultsComboBox.ItemIndex := 0;

  Reg:= TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('SOFTWARE\NPPOpenFileInFoldersPlugin', false) then begin
      try ExcludesEdit.Text := Reg.ReadString('Excludes'); except end;
      try FoldersEdit.Text := Reg.ReadString('Folders'); except end;
      try MaxResultsComboBox.ItemIndex := Reg.ReadInteger('MaxResultsItemIndex'); except end;
      try SearchFromRegistry := Reg.ReadString('Search'); except end;
      try ShowForm.Height := Reg.ReadInteger('Height'); except end;
      try ShowForm.Width := Reg.ReadInteger('Width'); except end;
    end;
  finally
    Reg.Free;
  end;
end;

function TShowForms.GetSelectedText: String;
var
  len: Integer;
begin
  SetLength(Result, 1024);
  SendMessage(self.Npp.NppData.ScintillaMainHandle, SciSupport.SCI_GETSELTEXT, 0, LPARAM(PChar(Result)));
  len := StrLen(PChar(Result));
  if len > 1024 then len := 1024;
  SetString(Result, PChar(Result), len);
end;

procedure TShowForms.MaxResultsComboBoxClick(Sender: TObject);
begin
  SearchMemo.SetFocus;
  //SearchMemo.SelectAll;
  SearchMemo.SelStart := Length(SearchMemo.Text);

  FPrevSearch := '';
  SearchMemoChange(nil);
  SearchMemo.SetFocus;
end;

procedure TShowForms.SearchListBoxClick(Sender: TObject);
begin
  SearchMemo.SetFocus;
  //SearchMemo.SelectAll;
  SearchMemo.SelStart := Length(SearchMemo.Text);
end;

procedure TShowForms.SearchListBoxDblClick(Sender: TObject);
var
  ret: Word;
begin
  ret := 13;
  SearchMemoKeyDown(nil, ret, []);
end;

procedure TShowForms.StopButtonClick(Sender: TObject);
begin
  stopGetFiles := true;
  SearchMemo.SetFocus;
end;

procedure TShowForms.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  SaveSetting;

  if isGetFilesStopped then begin
    CanClose := true;
  end else begin
    CanClose := false;
    stopGetFiles := true;
    closeAfterStopGetFiles := true;
  end;
end;

procedure TShowForms.GetFilesTimerTimer(Sender: TObject);
begin
  GetFilesTimer.Enabled := false;
  GetFilesButtonClick(nil);
end;

procedure TShowForms.FilesMemoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      begin
        Key := 0;
        if isGetFilesStopped then
          ShowForm.Close
        else begin
          stopGetFiles := true;
          closeAfterStopGetFiles := true;
        end;
      end;
  end;
end;

end.
