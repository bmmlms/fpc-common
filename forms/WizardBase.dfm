object frmWizardBase: TfrmWizardBase
  Left = 0
  Top = 0
  Caption = 'Setup wizard'
  ClientHeight = 300
  ClientWidth = 430
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
    Top = 96
    Width = 393
    Height = 213
    Align = alCustom
    BevelOuter = bvNone
    TabOrder = 0
    object lblLanguageList: TLabel
      Left = 8
      Top = 4
      Width = 51
      Height = 13
      Caption = 'Language:'
    end
    object lstLanguages: TComboBoxEx
      Left = 8
      Top = 20
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
    Top = 260
    Width = 430
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
      Width = 422
      Height = 5
      Align = alTop
      Shape = bsTopLine
      ExplicitLeft = -7
      ExplicitWidth = 396
    end
    object btnNext: TPngBitBtn
      Left = 329
      Top = 9
      Width = 97
      Height = 27
      Align = alRight
      Caption = '&Next'
      Default = True
      DoubleBuffered = True
      Layout = blGlyphRight
      ParentDoubleBuffered = False
      TabOrder = 0
      OnClick = btnNextClick
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
    object btnBack: TPngBitBtn
      Left = 4
      Top = 9
      Width = 97
      Height = 27
      Align = alLeft
      Caption = '&Cancel'
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 1
      OnClick = btnBackClick
      PngImage.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        610000001974455874536F6674776172650041646F626520496D616765526561
        647971C9653C000001454944415478DA63FCFFFF3F032580912E06E4CC79D4FE
        EB1F43EEAC34391E920D489BF5A0FDEF3FA6F2EF3FFF302ECB576224C980C469
        F7FAA404390A15C45919369C7ACFB0B5428D7803A227DE99272DC291A822CEC6
        F0E7DF7F86A587DF301C6DD626CE8090BE5B4BE484D9A335A43919DE7EF9C3C0
        CECCC830F7C04B86ABBD06840D08ECBAF5485A845D564B9A83E1CDE7DF0C1FBF
        FD6110E4616198B1E319C3935966840D706ABCBE594E94CD47478E93E1CDC7DF
        0C1F800608F3B032CCDFF694E1D9624BC20668179E610186FA0A4571CE601315
        6E8657EF7F31707332332CDBF888E1C55A7BE203512AF6E84A15699E30430D5E
        86DF7FFE33AC59F390E1D50E27D2A251DC7BDF2245459E586D2D41864DABEF33
        BCDEEF469A0120206AB57581AC9A40FCE35B9F195E1FF320DD00101032DC389B
        919925E5ED196FF20CC0072836000013EF9BE18A8403640000000049454E44AE
        426082}
    end
  end
  object pnlStorage: TPanel
    Left = 408
    Top = 96
    Width = 393
    Height = 241
    BevelOuter = bvNone
    TabOrder = 2
    object lblAppData: TLabel
      Left = 24
      Top = 20
      Width = 361
      Height = 53
      AutoSize = False
      Caption = '-'
      WordWrap = True
      OnClick = lblAppDataClick
    end
    object lblPortable: TLabel
      Left = 24
      Top = 100
      Width = 361
      Height = 53
      AutoSize = False
      Caption = '-'
      WordWrap = True
      OnClick = lblPortableClick
    end
    object optAppData: TRadioButton
      Left = 8
      Top = 0
      Width = 353
      Height = 21
      Caption = 'Save data to registry/application data folder'
      TabOrder = 0
    end
    object optPortable: TRadioButton
      Left = 8
      Top = 80
      Width = 353
      Height = 21
      Caption = 'Save data to application path'
      TabOrder = 1
    end
  end
  object pnlUpdates: TPanel
    Left = 4
    Top = 316
    Width = 393
    Height = 213
    BevelOuter = bvNone
    TabOrder = 3
    object chkAutoUpdate: TCheckBox
      Left = 8
      Top = 0
      Width = 341
      Height = 21
      Caption = 'Enable automatic search for updates'
      TabOrder = 0
    end
  end
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 430
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    Padding.Left = 2
    Padding.Top = 2
    Padding.Right = 2
    Padding.Bottom = 2
    TabOrder = 4
    object Shape1: TShape
      Left = 2
      Top = 2
      Width = 426
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
      Left = 5
      Top = 5
      Width = 420
      Height = 23
      Align = alClient
      AutoSize = False
      Caption = 'Welcome'
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
  object pnlDesc: TPanel
    Left = 0
    Top = 33
    Width = 430
    Height = 48
    Align = alTop
    BevelOuter = bvNone
    Padding.Left = 8
    Padding.Top = 4
    Padding.Right = 8
    Padding.Bottom = 4
    TabOrder = 5
    DesignSize = (
      430
      48)
    object lblDesc: TLabel
      Left = 8
      Top = 4
      Width = 413
      Height = 45
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = False
    end
  end
end
