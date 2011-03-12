object frmAbout: TfrmAbout
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'About'
  ClientHeight = 269
  ClientWidth = 421
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    000001002000000000000000000000000000000000000000000000000000FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00874A2024874A2085884B21C5874A20F2874A
    20F2884B21C4874A2084874A2024FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF008A4D22028B4D22758B4F24F494592EFC9A5F33FF9E6439FF9E64
    39FF9A5F33FF95592EFB8B4F23F58A4D22748A4D2202FFFFFF00FFFFFF00FFFF
    FF008C4F24028D5025C493572CF4A56B40FFA5693CFFA26437FFA06333FFA063
    33FFA26537FFA56A3DFFA46B3FFF93572BF48D5025C48C4F2402FFFFFF00FFFF
    FF0090522574975B2FF4AD7549FFA76A3AFFA46534FFA46635FFA46635FFA466
    35FFA46635FFA46635FFA76A3AFFAE764AFF975B2FF490522574FFFFFF009153
    272493552AF5B17C52FFAC7143FFA96C3CFFAA6D3CFFF5EEE9FFF7F1ECFFF6F0
    EAFFF3EAE4FFAA6D3DFFAA6D3CFFAC7244FFB27D53FF93552AF5915327249456
    2984A77045FBB68157FFAE7343FFAF7344FFAF7444FFAF7545FFF7F0EBFFF8F3
    EFFFB07545FFAF7545FFAF7444FFAF7444FFB78258FFA77046FB94562984985A
    2CC4B5825AFFB88155FFB37A4BFFB47B4BFFB57C4CFFB57C4DFFF7F2EDFFF8F3
    EFFFB57C4DFFB57C4DFFB57C4CFFB47B4CFFB98256FFB5835BFF985A2CC49A5B
    2DF2C39772FFBA8356FFB98152FFBA8253FFBA8354FFBB8455FFF9F3EFFFF9F4
    F0FFBB8455FFBB8455FFBA8354FFBA8253FFBB8558FFC49872FF9A5B2DF29C5E
    2FF2C99F7BFFBF8B5EFFBE895AFFBF8A5BFFC08A5CFFF7F0EAFFF9F3EEFFF6EE
    E7FFC08B5DFFC08B5DFFC08B5CFFBF8A5BFFC08D5FFFCAA07CFF9C5E2FF2A062
    32C5C49872FFC8986EFFC39061FFC49163FFC59264FFC69265FFC69365FFC693
    65FFC69365FFC69365FFC59264FFC49163FFC99A70FFC59874FFA06232C5A162
    3285BD8C64FCD2AA85FFC99669FFCA986AFFCA996CFFCB9A6CFFCFA177FFD0A2
    77FFCB9A6DFFCB9A6DFFCB996CFFCA986BFFD4AB86FFBE8D66FCA1623285A263
    3324A6693AF3D8B596FFD2A67DFFCF9F72FFCF9F73FFD2A77EFFFDFAF8FFFDFA
    F8FFD3A87EFFCFA074FFCFA073FFD3A87FFFD9B696FFA6693AF4A2633324FFFF
    FF00A4663575B68155F4E0C0A4FFD5AC84FFD1A479FFD4AA83FFFDFAF8FFFDFA
    F8FFD5AB84FFD2A57AFFD5AD86FFE0C2A4FFB57F54F5A4663575FFFFFF00FFFF
    FF00A3643302A66837C4B88358F4DFC1A6FFDEBD9EFFD9B490FFD9B38FFFD9B4
    90FFD9B491FFDFBE9FFFDEC1A6FFB68156F5A66837C4A3643302FFFFFF00FFFF
    FF00FFFFFF00A3643302A5663675A86C3CF3C89E7AFDD6B497FFE2C8AFFFE2C8
    B0FFD7B598FFC89E7AFCA76B3BF5A4663574A3643302FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00A3643324A4653485A76939C5A56736F2A567
    36F2A76A39C4A4653584A3643324FFFFFF00FFFFFF00FFFFFF00FFFFFF00F81F
    0000F00F0000C0030000C0030000800100000000000000000000000000000000
    0000000000000000000080010000C0030000C0030000F00F0000F81F0000}
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnKeyDown = FormKeyDown
  DesignSize = (
    421
    269)
  PixelsPerInch = 96
  TextHeight = 13
  object pagAbout: TPageControl
    Left = 4
    Top = 4
    Width = 413
    Height = 213
    ActivePage = tabAbout
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    OnChange = pagAboutChange
    ExplicitWidth = 461
    object tabAbout: TTabSheet
      Caption = 'About'
      ExplicitWidth = 453
      DesignSize = (
        405
        185)
      object lblAbout: TLabel
        Left = 4
        Top = 2
        Width = 7
        Height = 19
        Caption = '-'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object imgLogo: TImage
        Left = 365
        Top = 2
        Width = 32
        Height = 32
        Anchors = [akTop, akRight]
        AutoSize = True
      end
      object lblForumLink: TLabel
        Left = 4
        Top = 168
        Width = 4
        Height = 13
        Cursor = crHandPoint
        Anchors = [akLeft, akBottom]
        Caption = '-'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsUnderline]
        ParentColor = False
        ParentFont = False
        OnClick = lblForumLinkClick
        ExplicitTop = 218
      end
      object lblProjectLink: TLabel
        Left = 4
        Top = 148
        Width = 4
        Height = 13
        Cursor = crHandPoint
        Anchors = [akLeft, akBottom]
        Caption = '-'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsUnderline]
        ParentColor = False
        ParentFont = False
        OnClick = lblProjectLinkClick
      end
      object lblHelpLink: TLabel
        Left = 4
        Top = 128
        Width = 4
        Height = 13
        Cursor = crHandPoint
        Anchors = [akLeft, akBottom]
        Caption = '-'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsUnderline]
        ParentColor = False
        ParentFont = False
        OnClick = lblHelpLinkClick
        ExplicitTop = 178
      end
      object lblVersion: TLabel
        Left = 3
        Top = 24
        Width = 4
        Height = 13
        Caption = '-'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblCopyright: TLabel
        Left = 4
        Top = 56
        Width = 228
        Height = 13
        Caption = 'Copyright (c) 2010-2011 Alexander Nottelmann'
      end
      object lblHomepage: TLabel
        Left = 4
        Top = 76
        Width = 89
        Height = 13
        Cursor = crHandPoint
        Caption = 'http://mistake.ws/'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = lblHomepageClick
      end
      object btnDonateDe: TImage
        Left = 312
        Top = 52
        Width = 86
        Height = 21
        Cursor = crHandPoint
        Anchors = [akTop, akRight]
        AutoSize = True
        Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D49484452000000560000
          00150803000000EB44E907000000017352474200AECE1CE900000180504C5445
          000000FFF6ECFEB747FFAF33FEDB997F8983506A7B607580506A7D405E76FEDE
          A460625350595320496E305473304A5B20425EFFE2B7FEC76C103B62103E6A20
          4970FEE1AAFFAC2DFEE0A4FEDE9FFFBA4DBF9853FFEACCFFEEDAFFB13AFFE7CD
          405E74FFE9CA807759405F79FFECD0405158FFFCF6CEBF987F8B8BAF863FFFB4
          40807049EED5A0DF9A2ECF93337F8D8FAEAC9960747DEED8ABBF8E3BAEA98E70
          7F85CEC3A4BEB79CBEB4929EA194EFA431DF9D349F7E429E9E8AFFB2399F7D3E
          FFF4E7706E5B9E9F90CF9941DECBA1EED39BFEB84A9F8756FED9988E968FFFFA
          F5FFBF59FFF0D870828CFECD79FFC2609F8A61BF9144FFCD82AF9055DFBB80FF
          AF3ADFA13EEEDDB8FFD89FFFA534FEB74BEFC482605F4E70674AFFA62DBFA77C
          40607CFED07F7F8E9260665D8F815DDFA950FFA82CFFA934FEB94DAEAFA2BEB9
          A3FFDCA1CFA152FFD9A0DECA9C8E9386FFF5E7FEE3B0FFA926FFEBCEFEE6B7FE
          E8BDFFDDAAFFB138FFCE84FFF8EAFFBF5EFFF4DEFFEFD2FFECC7003366FF9933
          CCCB4A920000000174524E530040E6D86600000001624B47440088051D480000
          00097048597300000B1300000B1301009A9C180000000774494D4507DA0A0515
          321D96842449000002C34944415478DA636480807A06EA804608C5083574A98C
          0154621B83174E4D6B19189860EC7F3006271043B55CB8930C31186C6CFD595D
          2A399681E1B27123D4D8FAB3FA54331508581AC1C6D62F8AA6A6A90C0CCC8D40
          63EBBB8BA96B2AC33B914690B1D9E8E2378E674D0FE1C1ADEF112793301E63A7
          963632D61F67D24113DEEDDFA5FE2C73871D4E7DDF4436BAE231F6CA3F4BC6FA
          DB3BE2D18437457F6638F646CCE2C47B8ECF427FED196EDC7AAAE1C870E23DDF
          25062DFB83DF7EB33284EDB4DA7F435A4D83E1D847BE8B32C2D6A8FAEF498A33
          D6373308B0A00A7F2CEFD6BB0FA433183AFF57AC7AF7B17CFBCD82639732A67D
          FD5BD1C9FFA162FB4DB6AC63FF6C3A182A8E5CF95C3A41DDB39F1345FB9F0F0C
          B58CF5AB1918547F5E401617B95BCAD02DF7913F7CF37391A06E392025E3D59E
          29D02FFF3BBC4311C853709BC6F2E72737508EFF6BE1CA8F7F18909D65C00C74
          512863FDB593408EA1D062512439F91771FD3C2A8EFBEF30A42E7A5AD92FFCAE
          A05DDFAB5D44DAAB3D8B7FDB5379B745126ED37818E216A95A2EFAA903C4307D
          AFBDFE5E06D1898CF52D5021EB3B5086EEED1F0C0C5FF3E7A8DBCEFD6FA97DF8
          61CCB3EDC0DCB93766EE7F8E98B9EC404AD77CC9CFE4A73B181802D7272FF9C1
          08C4509D2A47A18C1AC6FAFB3BD162EC4BF15A0ECEEB429FF82297F3DD61D160
          705ECE779359FB75E49EBB7C917B989CFA645F65AF7EF7934192E985F4DFE0D5
          EF448018CD047F4960BAFD8C9EBE1EE533304C9477E199E8A73849CE85E1ECC5
          3C206FE1DF2406107EA53489216FBDEB9E87F993F48D6715AD7705613413781B
          19BD4C6FC9A127BCFBBF3F99309CB1EECD61200B7C9068049509BFB14A5ED37B
          26469EB1AC8D9012ECB21A16C93BDC7F64C93295BD1156DEDE9423CB006CE0BB
          10ACBC05D50E939260E21B19FC71EA5909F4218C0D0F3A0E208669999787543B
          30786DAFA392631B19FF8328002037FA8F9326B09A0000000049454E44AE4260
          82}
        Visible = False
        OnClick = btnDonateClick
      end
      object btnDonateEn: TImage
        Left = 324
        Top = 52
        Width = 74
        Height = 21
        Cursor = crHandPoint
        Anchors = [akTop, akRight]
        AutoSize = True
        Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D494844520000004A0000
          00150803000000D6D68861000000017352474200AECE1CE9000001DD504C5445
          CE0702FFAC2DFEE2ABFEE0A5FEE4B1FEDE9FFFBA4DFFAF33BF8B36FFB13AEEDB
          B2405E76103E6BFFB440DF9A2E8F7F58706E5B103E6A809097405F79707E7F60
          6151CF93327F8983BF9853BEB3919F87569EA3997F8D8FDED0ADEFA22A8E9793
          9EA091EFB24FFEB7477F8B89305474EED39B505D5E405F77EED9AC7F8A86CF95
          38DECCA260747E40607AAEA88DFEC76C60737BBEB9A3CEC19E9EA195FFECD0CE
          C3A3FED185BEB69A304A5B103B62305473305472FEB84A6076829F7E42CEC4A8
          FFFCF63053718E9489FFA534405E74DF9D34807049AEB0A2FFCD82D110058E95
          8DBF8E3BFEDA98FFBE5DFFB74650697AFED48CFFD38BAF9055BFBEAFCFA152CE
          BF99FFF4E4FFC260D10E05CEBE94DECA9DFEDEA4FFA82C7F8C8C9E9F8D506B7E
          60665DFFF4E6FFEBC9FEB546FFD9A020496EFFE2B770684D8E9690FFE9CBFFD8
          9F20425FFFAF3A405560FFBF59FEB74BBEBBA8FED68CFEB94DFFF0D9FFC566EF
          E0C2FFEED3EFA431506C82607580FEE1AA607887FFEECEFEDD9EFFF0D8708084
          FED07FAEAC99FFA62DFFDBA0FEBF59304C61FEC972FFAE33FFF2DB20486DAEAE
          9EFFECCFFFE2B6FFA934FFB23FFEE6B7FFA926FFF5E7FFEBCEFEE9BEFFECC8FF
          DDAAFFF8EAFFF4DFFFCE84FFF0D3FFBF5EFFB138003366CC0000FF993331CCAF
          EF0000000174524E530040E6D86600000001624B47440088051D480000000970
          48597300000B1300000B1301009A9C180000000774494D4507DA0A0515322B59
          3EB1D0000002614944415478DA63648080790CE4832408C50835E8ED3503A8C4
          7A8640920C3A2A6C0A310C6CD4BCABCA14388A81E1814612D4A8790F24293289
          81E1A35812D8A87947CC29348981E1B15212D0A8794F05D1C4EB80D8D111A7B6
          87F25804F7FA26018DFA8A267A570744DE97C061D21643696C46F92531CE9B27
          6F822A3AA1B6BEA45A397F7100C3A2D7CCFF8A18FA446E0A32BE6B65A81664FC
          FF9EA14168E287A20D77995FB7A2685ACE0634EAA2781DAA5196F133CF33D44A
          1F5C6E19DFF18241F37CB46DB96045F94795270CEC9DE5A1407BA73364022564
          EE20E929E37AA7C338EFE87E913328465924A5313098A6B6FFAD79DCAC5978B7
          D322A9E5117F779AC147FE0BAAA5698A95B3BF1476DF96AB59B703498FEE6786
          1AC6791FEA1994FCDA908467E6FF6060B08F3AF4D177DF4A1067E6FC13202AE8
          FAFD28F3F9273826A6DB47E5CB3117F4DE82EBF0549ACFC0309171DEB9B90C0C
          2CE97FA7C0C4459A2B3E3228A47488C847643284384F7909E403C504DF18BC65
          C8ABF8C8637044A875CA730606A637500DD5D73781A8A98CF35E364044FE4365
          387B4B38AC8CC58BBF73F5D4B1FBE8D77CEFAD7BC3D95BF7AFA54490B5ACCECA
          A3EB81504BC937CEC0834FC1CA3D7642F54D679CF7A41E35D445DA81C45CDEDD
          0C65075318E6B21E1669EFBB0EC2E6290CE70DD7FF0A67A87C9366CAC0B0E830
          AAB6682760BAFA842AD606E2774099624540A2038C192AD8045EB135D5FD626B
          62A8FB059440057CA024BA34828172F04930099407D7FB526E144B12A4645815
          40A9496C49B0F2EA8C3645069DB7829557A052F4840E4C7C11431C6906FD1242
          2A4519A852B603001DD0D26863F81EFD0000000049454E44AE426082}
        Visible = False
        OnClick = btnDonateClick
      end
    end
    object tabLicense: TTabSheet
      Caption = 'License'
      ImageIndex = 1
      ExplicitWidth = 421
      DesignSize = (
        405
        185)
      object lblGPL: TLabel
        Left = 4
        Top = 4
        Width = 4
        Height = 13
        Cursor = crHandPoint
        Caption = '-'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = lblGPLClick
      end
      object txtAbout: TMemo
        Left = 4
        Top = 24
        Width = 397
        Height = 157
        Anchors = [akLeft, akTop, akRight, akBottom]
        Color = clBtnFace
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
        ExplicitWidth = 445
      end
    end
    object tabThanks: TTabSheet
      Caption = 'Thanks'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
  end
  object pnlNav: TPanel
    Left = 0
    Top = 220
    Width = 421
    Height = 49
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 4
    Padding.Top = 4
    Padding.Right = 4
    Padding.Bottom = 4
    TabOrder = 1
    ExplicitWidth = 469
    object Bevel2: TBevel
      Left = 4
      Top = 4
      Width = 413
      Height = 5
      Align = alTop
      Shape = bsTopLine
      ExplicitLeft = -7
      ExplicitWidth = 396
    end
    object btnClose: TBitBtn
      Left = 320
      Top = 9
      Width = 97
      Height = 36
      Align = alRight
      Caption = '&Close'
      Default = True
      DoubleBuffered = True
      Layout = blGlyphRight
      ParentDoubleBuffered = False
      TabOrder = 0
      OnClick = btnCloseClick
      ExplicitLeft = 368
    end
  end
end
