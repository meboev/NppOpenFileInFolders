inherited ShowForms: TShowForms
  Left = 431
  Top = 269
  Width = 686
  Height = 560
  Caption = 'Open File In Folders'
  Constraints.MinHeight = 560
  Constraints.MinWidth = 400
  OldCreateOrder = True
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    670
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
    Width = 348
    Height = 13
    Caption = 
      'Search - Up/Down to move; Enter to Open; Ctrl+Enter to Open in E' +
      'xplorer'
  end
  object ExcludesLabel: TLabel
    Left = 16
    Top = 8
    Width = 43
    Height = 13
    Caption = 'Excludes'
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
    Width = 97
    Height = 25
    Caption = 'Get files'
    TabOrder = 5
    OnClick = GetFilesButtonClick
  end
  object FoldersEdit: TEdit
    Left = 16
    Top = 72
    Width = 591
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    Text = 'C:\dev\react-ui-toolkit'
  end
  object OpenFolderButton: TButton
    Left = 614
    Top = 72
    Width = 41
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'add'
    TabOrder = 4
    OnClick = OpenFolderButtonClick
  end
  object FilesMemo: TMemo
    Left = 16
    Top = 144
    Width = 639
    Height = 89
    TabStop = False
    Anchors = [akLeft, akTop, akRight]
    HideSelection = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 6
  end
  object SearchListBox: TListBox
    Left = 16
    Top = 336
    Width = 639
    Height = 155
    TabStop = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
    OnClick = SearchListBoxClick
    OnDblClick = SearchListBoxDblClick
  end
  object ExcludesEdit: TEdit
    Left = 16
    Top = 24
    Width = 639
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    Text = '\node_modules\ | \.git\ | \dist\'
  end
  object MaxResultsComboBox: TComboBox
    Left = 16
    Top = 264
    Width = 639
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 7
    Text = '20'
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
    Width = 670
    Height = 19
    Panels = <>
    SimplePanel = True
    SimpleText = 'Loaded Files: 0'
  end
  object SearchMemo: TMemo
    Left = 16
    Top = 312
    Width = 639
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ScrollBars = ssHorizontal
    TabOrder = 0
    OnChange = SearchMemoChange
    OnKeyDown = SearchMemoKeyDown
  end
end
