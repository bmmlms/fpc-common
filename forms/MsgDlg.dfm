object frmMsgDlg: TfrmMsgDlg
  Left = 488
  Top = 454
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Info'
  ClientHeight = 170
  ClientWidth = 381
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  
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
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  DesignSize = (
    381
    170)
  PixelsPerInch = 96
  TextHeight = 13
  object chkNotShowAgain: TCheckBox
    Left = 4
    Top = 105
    Width = 277
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = '&Don'#39't show this message again'
    TabOrder = 0
  end
  object txtText: TMemo
    Left = 4
    Top = 4
    Width = 373
    Height = 99
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = bsNone
    Color = clBtnFace
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
    ExplicitHeight = 97
  end
  object pnlNav: TPanel
    Left = 0
    Top = 130
    Width = 381
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 4
    Padding.Top = 4
    Padding.Right = 4
    Padding.Bottom = 4
    TabOrder = 2
    ExplicitTop = 128
    object Bevel2: TBevel
      Left = 4
      Top = 4
      Width = 373
      Height = 5
      Align = alTop
      Shape = bsTopLine
      ExplicitLeft = -7
      ExplicitWidth = 396
    end
    object cmdCancel: TButton
      Left = 280
      Top = 9
      Width = 97
      Height = 27
      Align = alRight
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 0
      OnClick = cmdCancelClick
      ExplicitHeight = 25
    end
    object cmdOK: TButton
      Left = 4
      Top = 9
      Width = 97
      Height = 27
      Align = alLeft
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
      OnClick = cmdOKClick
      ExplicitHeight = 25
    end
  end
end
