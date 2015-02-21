object frmSettingsBase: TfrmSettingsBase
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Settings'
  ClientHeight = 470
  ClientWidth = 620
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    000001002000000000000000000000000000000000000000000000000000874A
    2056874A20FF874A20FF874A20FF874A20440000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000874A
    20FFBEA592FFB89C86FFB89C86FF874A20FF874A204400000000000000000000
    00000000000000000000858A8881858A88D3858A88810000000000000000874A
    20FFCCBBADFFA7917DFFB5957AFFAE8B72FF874A20FF874A2044000000000000
    000000000000858A884C858A88FF9EA1A0FF858A88FF0000000000000000874A
    20FFCCBBADFFA48D78FFA28A74FFB4957AFFB3937CFF874A20FF000000000000
    0000858A884C858A88FFD2D3D4FFC2C3C4FF858A88FF0000000000000000874A
    2030874A20FFD4C8BDFFA48D78FFA48D78FFBEA592FF874A20FF00000000858A
    884C858A88FFE2E3E4FFC2C4C6FF858A88FF858A888100000000000000000000
    0000874A2019874A20FFCCBBADFFCCBBADFFB3937CFF874A20FF858A8881858A
    88FFD3D3D4FFBDBEBFFF858A88FF858A884C0000000200000000000000000000
    000000000000874A2019874A20FF874A20FF874A20FF85837DFFAAADADFFC8C9
    CAFFBDBEBFFF858A88FF858A884C000000000000000000000000000000000000
    00000000000000000000000000000000000000000000B5B7B8FF858A88FFB7B8
    B9FF858A88FF858A884C00000002000000000000000000000000000000000000
    0000858A884C858A88FF858A88FF858A88FF858A88FFD0D1D2FFA3A4A4FF858A
    88FF858A8881000000000000000000000000000000000000000000000000858A
    884C858A88FFF3F3F3FEEFF0F0FFEDEEEEFEEAECECFEB6B9BAFF858A88FF858A
    884C858A88FF858A884C0000000000000000000000000000000000000000858A
    88FFF5F6F6FEA9ACABFF858A88FFF7F7F7FFE2E3E5FFAAADADFFECEEEE810000
    0000858A884C858A88FF858A884C00000000000000000000000000000000858A
    884C858A88FF00000000858A884C858A88FFFAFAFAFF858A88FF000000000000
    000000000000858A884C858A88FF878C8AFF0000004C00000000000000000000
    0000000000000000000000000000858A88FFEEF0F0FF858A88FF000000000000
    00000000000000000000858A88FFF0F0F0FF858A88FF0000004C000000000000
    00000000000000000000858A88FFE9EBECFF858A88FF858A884C000000000000
    000000000000000000000000004C858A88FFEEEFEFFC858A88FF000000000000
    00000000000000000000858A884C858A88FF858A884C00000000000000000000
    00000000000000000000000000000000004C858A88FF858A884C000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000008FFF
    000007E3000003E3000001C3000081830000C00F0000E01F0000FC3F0000C03F
    000080BF000000DF0000B1E70000F1E30000E3F10000F7FB0000FFFF0000}
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 620
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    Padding.Left = 4
    Padding.Top = 2
    Padding.Right = 4
    Padding.Bottom = 2
    TabOrder = 0
    object Shape1: TShape
      Left = 4
      Top = 2
      Width = 612
      Height = 29
      Align = alClient
      Brush.Color = clActiveCaption
      Pen.Color = clActiveBorder
      ExplicitLeft = -276
      ExplicitTop = 4
      ExplicitWidth = 801
    end
    object lblTop: TLabel
      AlignWithMargins = True
      Left = 7
      Top = 5
      Width = 606
      Height = 23
      Align = alClient
      AutoSize = False
      Caption = 'Settings'
      Color = clActiveCaption
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Tahoma'
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
  object pnlNav: TPanel
    Left = 0
    Top = 430
    Width = 620
    Height = 40
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
      Width = 612
      Height = 5
      Align = alTop
      Shape = bsTopLine
      ExplicitLeft = -7
      ExplicitWidth = 396
    end
    object btnOK: TBitBtn
      Left = 519
      Top = 9
      Width = 97
      Height = 27
      Align = alRight
      Caption = '&OK'
      Default = True
      DoubleBuffered = False
      Layout = blGlyphRight
      ParentDoubleBuffered = False
      TabOrder = 0
      OnClick = btnOKClick
    end
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 33
    Width = 93
    Height = 397
    Align = alLeft
    BevelOuter = bvNone
    Padding.Left = 4
    Padding.Right = 8
    TabOrder = 2
  end
  object pnlGeneral: TPanel
    Left = 100
    Top = 52
    Width = 337
    Height = 357
    TabOrder = 3
    Visible = False
    DesignSize = (
      337
      357)
    object lblLanguage: TLabel
      Left = 4
      Top = 0
      Width = 51
      Height = 13
      Caption = 'Language:'
    end
    object lblPortable: TLabel
      Left = 4
      Top = 236
      Width = 321
      Height = 57
      Anchors = [akLeft, akBottom]
      AutoSize = False
      Caption = '-'
      WordWrap = True
    end
    object lstLanguages: TComboBoxEx
      Left = 4
      Top = 16
      Width = 165
      Height = 22
      ItemsEx = <>
      Style = csExDropDownList
      TabOrder = 0
      OnChange = lstLanguagesChange
    end
    object chkAutoUpdateCheck: TCheckBox
      Left = 4
      Top = 44
      Width = 301
      Height = 21
      Caption = 'Enable automatic search for updates'
      TabOrder = 1
    end
    object btnCopyProfile: TButton
      Left = 4
      Top = 296
      Width = 129
      Height = 27
      Anchors = [akLeft, akBottom]
      Caption = '&Copy profile'
      TabOrder = 2
      OnClick = btnCopyProfileClick
    end
    object txtPort: TLabeledEdit
      Left = 20
      Top = 176
      Width = 121
      Height = 21
      EditLabel.Width = 24
      EditLabel.Height = 13
      EditLabel.Caption = 'Port:'
      NumbersOnly = True
      TabOrder = 3
    end
    object txtHost: TLabeledEdit
      Left = 20
      Top = 132
      Width = 121
      Height = 21
      EditLabel.Width = 26
      EditLabel.Height = 13
      EditLabel.Caption = 'Host:'
      TabOrder = 4
    end
    object chkProxy: TCheckBox
      Left = 4
      Top = 92
      Width = 245
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Use HTTP proxy'
      TabOrder = 5
      OnClick = chkProxyClick
    end
    object btnDeleteProfile: TButton
      Left = 4
      Top = 328
      Width = 129
      Height = 27
      Anchors = [akLeft, akBottom]
      Caption = '&Delete profile'
      TabOrder = 6
      OnClick = btnDeleteProfileClick
    end
    object btnExportProfile: TButton
      Left = 140
      Top = 296
      Width = 129
      Height = 27
      Anchors = [akLeft, akBottom]
      Caption = '&Export profile'
      TabOrder = 7
      OnClick = btnExportProfileClick
    end
    object btnImportProfile: TButton
      Left = 140
      Top = 328
      Width = 129
      Height = 27
      Anchors = [akLeft, akBottom]
      Caption = '&Import profile'
      TabOrder = 8
      OnClick = btnImportProfileClick
    end
    object chkCheckCertificate: TCheckBox
      Left = 4
      Top = 68
      Width = 284
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Cancel connections on invalid certificate'
      TabOrder = 9
    end
  end
end
