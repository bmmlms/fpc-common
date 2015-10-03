object frmProfileSettings: TfrmProfileSettings
  Left = 0
  Top = 0
  Anchors = [akLeft, akTop, akRight]
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Manage profiles'
  ClientHeight = 290
  ClientWidth = 329
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    329
    290)
  PixelsPerInch = 96
  TextHeight = 13
  object lblProfiles: TLabel
    Left = 4
    Top = 40
    Width = 321
    Height = 85
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = '-'
    WordWrap = True
  end
  object pnlNav: TPanel
    Left = 0
    Top = 250
    Width = 329
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 4
    Padding.Top = 4
    Padding.Right = 4
    Padding.Bottom = 4
    TabOrder = 6
    object Bevel2: TBevel
      Left = 4
      Top = 4
      Width = 321
      Height = 5
      Align = alTop
      Shape = bsTopLine
      ExplicitLeft = -7
      ExplicitWidth = 396
    end
    object btnOk: TPngBitBtn
      Left = 228
      Top = 9
      Width = 97
      Height = 27
      Align = alRight
      Caption = '&OK'
      Default = True
      DoubleBuffered = True
      Layout = blGlyphRight
      ParentDoubleBuffered = False
      TabOrder = 0
      OnClick = btnOkClick
      PngImage.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        610000001974455874536F6674776172650041646F626520496D616765526561
        647971C9653C000001534944415478DA63FCFFFF3F03258091EA0634AE79F9E5
        DF3F86C98D61E295641AF0FAFF9F7FFFFEFFF8F9ABB33B5696A021180654AD78
        FEDF449193E1D68B9F0C379F7EED9F9FA554449201F90B1FFFB752E761F8FDFB
        3FC3A547DF182E3DF83C7F47B56612D10624CF78F4DF5E8B9BE1E3B73F0CEC2C
        8C0C27EE7C61387DFBC3D2CBBD4631441910D27FE7BF9B2E3FC3F3F73F191819
        191838D918190E5EFFC470FED687C74FE75ACA1134C0A5F5FA7F5F43418627EF
        7E3230030DE06267623874E523C3B55B9FB73C5B66E94BD000B38ACBFF832C84
        185E7FFACDC0C5C1C470F8E27B869B373FACFDFBE97BC4AB5D1E7F081AA0927F
        EE7FB49508C3EF7FFF198E5CFCC070EBE6DB552FD63B85131D88D2C9A7FE473B
        8A311C0769BEF66EF1CBAD4E712445A364D4D1FFCA121C0CB7AEBC5DF86A975B
        020301806180B8FFC1FFFF3E7F9FF37A9F472A21CD580D2015506C0000335FC0
        E114135B270000000049454E44AE426082}
    end
  end
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 329
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    Padding.Left = 4
    Padding.Top = 2
    Padding.Right = 4
    Padding.Bottom = 2
    TabOrder = 7
    object Shape1: TShape
      Left = 4
      Top = 2
      Width = 321
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
      Width = 315
      Height = 23
      Align = alClient
      AutoSize = False
      Caption = 'Select profile'
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
  object rbInstalled: TRadioButton
    Left = 4
    Top = 132
    Width = 321
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Use settings from registry/application data folder'
    Checked = True
    TabOrder = 0
    TabStop = True
  end
  object rbPortable: TRadioButton
    Left = 4
    Top = 196
    Width = 321
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Use settings from application path (portable)'
    TabOrder = 3
  end
  object btnDelete: TButton
    Left = 20
    Top = 156
    Width = 81
    Height = 27
    Caption = 'Delete'
    TabOrder = 1
    OnClick = btnDeleteClick
  end
  object btnDelete2: TButton
    Left = 20
    Top = 220
    Width = 81
    Height = 27
    Caption = 'Delete'
    TabOrder = 4
    OnClick = btnDeleteClick
  end
  object btnCopy: TButton
    Left = 108
    Top = 156
    Width = 81
    Height = 27
    Caption = 'Copy'
    TabOrder = 2
    OnClick = btnCopyClick
  end
  object btnCopy2: TButton
    Left = 108
    Top = 220
    Width = 81
    Height = 27
    Caption = 'Copy'
    TabOrder = 5
    OnClick = btnCopyClick
  end
end
