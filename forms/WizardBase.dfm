object frmWizardBase: TfrmWizardBase
  Left = 0
  Top = 0
  Caption = 'Setup wizard'
  ClientHeight = 594
  ClientWidth = 809
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlLanguage: TPanel
    Left = 4
    Top = 48
    Width = 393
    Height = 213
    Align = alCustom
    BevelOuter = bvNone
    TabOrder = 0
    object lblLanguage: TLabel
      Left = 8
      Top = 4
      Width = 381
      Height = 49
      AutoSize = False
      Caption = '-'
      Color = clBtnFace
      ParentColor = False
      WordWrap = True
    end
    object lblLanguageList: TLabel
      Left = 8
      Top = 64
      Width = 51
      Height = 13
      Caption = 'Language:'
    end
    object lstLanguages: TComboBoxEx
      Left = 8
      Top = 80
      Width = 165
      Height = 22
      ItemsEx = <>
      Style = csExDropDownList
      TabOrder = 0
      OnSelect = lstLanguagesSelect
    end
  end
  object pnlNav: TPanel
    Left = 0
    Top = 545
    Width = 809
    Height = 49
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 4
    Padding.Top = 4
    Padding.Right = 4
    Padding.Bottom = 4
    TabOrder = 1
    object Bevel2: TBevel
      Left = 4
      Top = 4
      Width = 801
      Height = 5
      Align = alTop
      Shape = bsTopLine
      ExplicitLeft = -7
      ExplicitWidth = 396
    end
    object btnBack: TBitBtn
      Left = 4
      Top = 9
      Width = 97
      Height = 36
      Align = alLeft
      Caption = '&Cancel'
      DoubleBuffered = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000FF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FF000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0000008CFF8C000000FF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00
        000000C64200DE008CFF8C000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FF00000000C64200C64200C64200DE008CFF8C0000
        00FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00000000B53900
        B53900B53900C64200C642000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FF00000000A53900B53900B53900B53900C642000000FF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00000000A53900B53900B53900
        B53900B539000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        000000009C3100A53900B53900B53900B539000000FF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000009C3100A53900B53900
        B5398CFF8C000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FF000000009C3100B53900B53900C6428CFF8C000000FF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000009C3100
        B53900B53900C6428CFF8C000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FF000000009C3100B53900B53900C6428CFF8C0000
        00FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00
        0000009C3100A53900A539000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000009C31000000FF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FF000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
      ParentDoubleBuffered = False
      TabOrder = 1
      OnClick = btnBackClick
    end
    object btnNext: TBitBtn
      Left = 708
      Top = 9
      Width = 97
      Height = 36
      Align = alRight
      Caption = '&Next'
      Default = True
      DoubleBuffered = True
      Glyph.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000000000000000000000000000000000000FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF0000000000009C310000000000FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF000000000000A5390000A53900009C310000000000FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00000000008CFF8C0000C6420000B5390000B53900009C310000000000FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00000000008CFF8C0000C6420000B5390000B53900009C31000000
        0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00000000008CFF8C0000C6420000B5390000B53900009C
        310000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00000000008CFF8C0000B5390000B5390000A5
        3900009C310000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000B5390000B5390000B5
        390000A53900009C310000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF000000000000B5390000B5390000B5390000B5
        390000A5390000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF000000000000C6420000B5390000B5390000B5390000A5
        390000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF000000000000C6420000C6420000B5390000B5390000B539000000
        0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00000000008CFF8C0000DE000000C6420000C6420000C6420000000000FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00000000008CFF8C0000DE000000C6420000000000FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00000000008CFF8C0000000000FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
      Layout = blGlyphRight
      ParentDoubleBuffered = False
      TabOrder = 0
      OnClick = btnNextClick
    end
  end
  object pnlStorage: TPanel
    Left = 408
    Top = 48
    Width = 393
    Height = 241
    BevelOuter = bvNone
    TabOrder = 2
    object lblData: TLabel
      Left = 8
      Top = 4
      Width = 381
      Height = 37
      AutoSize = False
      Caption = '-'
      Color = clBtnFace
      ParentColor = False
      WordWrap = True
    end
    object lblAppData: TLabel
      Left = 24
      Top = 60
      Width = 361
      Height = 53
      AutoSize = False
      Caption = '-'
      WordWrap = True
    end
    object lblPortable: TLabel
      Left = 24
      Top = 144
      Width = 361
      Height = 53
      AutoSize = False
      Caption = '-'
      WordWrap = True
    end
    object optAppData: TRadioButton
      Left = 8
      Top = 36
      Width = 353
      Height = 21
      Caption = 'Save data to registry/application data folder'
      TabOrder = 0
    end
    object optPortable: TRadioButton
      Left = 8
      Top = 120
      Width = 353
      Height = 21
      Caption = 'Save data to application path'
      TabOrder = 1
    end
  end
  object pnlUpdates: TPanel
    Left = 4
    Top = 272
    Width = 393
    Height = 213
    BevelOuter = bvNone
    TabOrder = 3
    object lblUpdates: TLabel
      Left = 8
      Top = 4
      Width = 381
      Height = 53
      AutoSize = False
      Caption = '-'
      Color = clBtnFace
      ParentColor = False
      WordWrap = True
    end
    object chkAutoUpdate: TCheckBox
      Left = 8
      Top = 64
      Width = 341
      Height = 21
      Caption = 'Enable automatic search for updates'
      TabOrder = 0
    end
  end
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 809
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    Padding.Left = 4
    Padding.Top = 4
    Padding.Right = 4
    Padding.Bottom = 4
    TabOrder = 4
    object Shape1: TShape
      Left = 4
      Top = 4
      Width = 801
      Height = 33
      Align = alClient
      Brush.Color = clActiveCaption
      Pen.Color = clActiveBorder
      ExplicitLeft = -276
      ExplicitHeight = 29
    end
    object lblTop: TLabel
      AlignWithMargins = True
      Left = 10
      Top = 9
      Width = 792
      Height = 25
      Margins.Left = 6
      Margins.Top = 5
      Align = alClient
      AutoSize = False
      Caption = 'Welcome'
      Color = clActiveCaption
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      GlowSize = 1
      ParentColor = False
      ParentFont = False
      Layout = tlCenter
      ExplicitLeft = 76
      ExplicitTop = 4
      ExplicitWidth = 190
      ExplicitHeight = 30
    end
  end
end
