inherited ShowForms: TShowForms
  Left = 917
  Top = 245
  Width = 400
  Height = 560
  Caption = 'Open File In Folders'
  Constraints.MinHeight = 560
  Constraints.MinWidth = 400
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnKeyDown = FilesMemoKeyDown
  OnShow = FormShow
  DesignSize = (
    384
    521)
  PixelsPerInch = 96
  TextHeight = 13
  object FoldersLabel: TLabel
    Left = 16
    Top = 56
    Width = 34
    Height = 13
    Caption = 'Folders'
  end
  object SearchLabel: TLabel
    Left = 16
    Top = 296
    Width = 354
    Height = 13
    Caption = 
      'Search - Up/Down to move; Enter to Open; Ctrl + Enter to Open in' +
      ' Explorer'
  end
  object ExcludesLabel: TLabel
    Left = 16
    Top = 8
    Width = 163
    Height = 13
    Caption = 'Excludes - \node_modules\ | \.git\'
  end
  object MaxResultsLabel: TLabel
    Left = 16
    Top = 248
    Width = 58
    Height = 13
    Caption = 'Max Results'
  end
  object FilesLabel: TLabel
    Left = 16
    Top = 128
    Width = 21
    Height = 13
    Caption = 'Files'
  end
  object GetFilesButton: TButton
    Left = 16
    Top = 96
    Width = 121
    Height = 25
    Caption = 'Get files'
    TabOrder = 5
    OnClick = GetFilesButtonClick
    OnKeyDown = FilesMemoKeyDown
  end
  object FoldersEdit: TEdit
    Left = 16
    Top = 72
    Width = 305
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    OnKeyDown = FilesMemoKeyDown
  end
  object OpenFolderButton: TButton
    Left = 328
    Top = 70
    Width = 41
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Add'
    TabOrder = 4
    OnClick = OpenFolderButtonClick
    OnKeyDown = FilesMemoKeyDown
  end
  object FilesMemo: TMemo
    Left = 16
    Top = 144
    Width = 353
    Height = 89
    TabStop = False
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 7
    OnKeyDown = FilesMemoKeyDown
  end
  object SearchListBox: TListBox
    Left = 16
    Top = 344
    Width = 353
    Height = 147
    TabStop = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
    OnClick = SearchListBoxClick
    OnDblClick = SearchListBoxDblClick
    OnKeyDown = FilesMemoKeyDown
  end
  object ExcludesEdit: TEdit
    Left = 16
    Top = 24
    Width = 353
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnKeyDown = FilesMemoKeyDown
  end
  object MaxResultsComboBox: TComboBox
    Left = 16
    Top = 264
    Width = 353
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 8
    Text = '20'
    OnClick = MaxResultsComboBoxClick
    OnCloseUp = MaxResultsComboBoxClick
    OnKeyDown = FilesMemoKeyDown
    Items.Strings = (
      '20'
      '50'
      '100'
      '200'
      '500'
      'All')
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 502
    Width = 384
    Height = 19
    Panels = <>
    SimplePanel = True
    SimpleText = 'Loaded Files: 0'
  end
  object SearchMemo: TMemo
    Left = 16
    Top = 312
    Width = 353
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = SearchMemoChange
    OnKeyDown = SearchMemoKeyDown
  end
  object StopButton: TButton
    Left = 144
    Top = 96
    Width = 121
    Height = 25
    Caption = 'Stop getting files'
    TabOrder = 6
    OnClick = StopButtonClick
    OnKeyDown = FilesMemoKeyDown
  end
  object GetFilesTimer: TTimer
    Enabled = False
    Interval = 1
    OnTimer = GetFilesTimerTimer
    Left = 288
    Top = 104
  end
end
