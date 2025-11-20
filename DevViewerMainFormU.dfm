object DeviceViewerForm: TDeviceViewerForm
  Left = 503
  Top = 323
  Width = 515
  Height = 466
  ActiveControl = Devices_TV
  Caption = 'Device Viewer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = True
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 16
  object Devices_TV: TTreeView
    Left = 0
    Top = 0
    Width = 499
    Height = 407
    Align = alClient
    HideSelection = False
    Images = ImageList
    Indent = 19
    PopupMenu = PopupMenu
    ReadOnly = True
    TabOrder = 0
    OnDblClick = Devices_TV_DblClick
    OnKeyPress = Devices_TV_KeyPress
  end
  object MainMenu: TMainMenu
    Left = 24
    Top = 16
    object FileMenu: TMenuItem
      Caption = '&File'
      object FileLoadSavedListMenu: TMenuItem
        Caption = '&Load saved list ...'
        ShortCut = 16463
        OnClick = FileLoadSavedListMenuClick
      end
      object FileSaveMenu: TMenuItem
        Caption = '&Save list to file ...'
        ShortCut = 16467
        OnClick = FileSaveMenuClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object FileExitMenu: TMenuItem
        Caption = '&Exit'
        OnClick = FileExitMenuClick
      end
    end
    object EditMenu: TMenuItem
      Caption = '&Edit'
      object EditFindMenu: TMenuItem
        Caption = '&Find ...'
        ShortCut = 16454
        OnClick = EditFindMenuClick
      end
      object EditFindNextMenu: TMenuItem
        Caption = 'Find &next'
        ShortCut = 114
        OnClick = EditFindNextMenuClick
      end
    end
    object ViewMenu: TMenuItem
      Caption = '&View'
      object ViewRefreshMenu: TMenuItem
        Caption = 'Refresh'
        ShortCut = 116
        OnClick = ViewRefreshMenuClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object ViewShowHiddenMenu: TMenuItem
        Caption = '&Show hidden devices'
        OnClick = ViewShowHiddenMenuClick
      end
      object ViewShowNonPresentMenu: TMenuItem
        Caption = 'S&how nonpresent devices'
        OnClick = ViewShowNonPresentMenuClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object ViewDisplayBiosInfoMenu: TMenuItem
        Caption = '&Display BIOS information ...'
        OnClick = ViewDisplayBiosInfoMenuClick
      end
    end
    object DebugMenu: TMenuItem
      Caption = '&Debug'
      object SetupDiGetDevicePropertyW_Menu: TMenuItem
        Caption = '&SetupDiGetDevicePropertyW (Vista)'
        OnClick = SetupDiGetDevicePropertyW_MenuClick
      end
    end
    object HelpMenu: TMenuItem
      Caption = '&Help'
      object HelpAboutMenu: TMenuItem
        Caption = '&About ...'
        OnClick = HelpAboutMenuClick
      end
    end
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.txt'
    Filter = 'Text files (*.txt)|*.txt|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 156
    Top = 16
  end
  object ImageList: TImageList
    Left = 224
    Top = 16
  end
  object PopupMenu: TPopupMenu
    Left = 296
    Top = 16
    object PopupShowDetailsMenu: TMenuItem
      Caption = '&Show details ...'
      Default = True
      OnClick = PopupShowDetailsMenuClick
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.txt'
    Filter = 'Text files (*.txt)|*.txt|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 92
    Top = 16
  end
end
