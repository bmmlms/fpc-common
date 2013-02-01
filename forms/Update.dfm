object frmUpdate: TfrmUpdate
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Update'
  ClientHeight = 153
  ClientWidth = 269
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  
  Font.Style = []
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    000001002000000000000000000000000000000000000000000000000000FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000C7949150F7B
    469A0F7A46DE0D7844FB0F7A46DD0F7A46990C794915FFFFFF00FFFFFF000960
    9500035A900002598F0002598F0002598F0002598F00107C4C40127F4CEF2491
    64F8329F78FF37A57FFF329F78FF249164F8127F4CEF107C4C40FFFFFF000960
    9590035A90FC02598FFF02598FFF02598FFF035C8AFF148056FF3BA47EFF43AE
    8AFF62BB9EFF6CBFA4FF62BB9EFF43AE8AFF3BA47EFF168353EF18865515035A
    8FFD88C3E7FF90C6EAFF8FC7E9FF8FC6EAFF49A193FF369F79FF4DB593FF40B0
    8BFFD9EFE8FFF9FDFBFFD9EFE8FF40B08BFF4DB593FF349E76F81B895A990259
    8FFF8EC5E9FF4FA6DCFF4EA4DBFF4CA3DAFF249171FF5AB999FF78CAAFFF74C8
    ADFFE5F5F0FFFFFFFFFFE5F5F0FF74C8ADFF78CAAFFF5AB999FF1F8F62DD0259
    8FFF8CC4E7FF4DA5DBFF4AA1D9FF479FD7FF229067FF65C4A3FFC6EADEFFFEFF
    FEFFF7FCFAFFFFFFFFFFF7FCFAFFFEFFFEFFC6EADEFF65C4A3FF229066FB0259
    8FFF8CC4E8FF4DA3DAFF48A0D8FF449ED7FF2A987BFF65C3A3FF5FC7A3FFE3F5
    EFFFFCFEFDFFFFFFFFFFFCFEFDFFE3F5EFFF5FC7A3FF65C3A3FF28986EDE0259
    8FFF90C7EAFF58ABDFFF5AACE0FF5FAEE1FF44A4A0FF53B595FF72D0B1FF67CC
    ABFFF2FBF8FFFFFFFFFFF2FBF8FF67CCABFF72D0B1FF53B694F92D9C769A0259
    8FFF93C9ECFF55A0CFFF4E91BCFF5388AAFF577985FF37A280FF72CDAEFF7AD5
    B7FF82D8BBFFF4FBF9FF82D8BBFF7AD5B7FF72CDAEFF2F9F7AEF319E79150259
    8FFFA0D0EFFF8BC5EAFFB5DAF1FFE8F3FBFFDDEEF8FF3A8CA6FF2E9B7CFF5AB9
    9BFF7BD0B4FF8FDAC1FF7BD0B4FF5AB99BFF2F9F7AEF2C9B7840FFFFFF000259
    8FFFD8E6EFFFCDE1ECFFCBE0ECFFCADFECFF538FB5FF6BA1C2FF9AC4D5FF5FB2
    A3FF3DA588FF2E9D79FF36A288FF1D8382FF319E7915FFFFFF00FFFFFF000259
    8FFFEEF0F0FFECEEEEFFECEEEEFFECEEEEFF055B90FFF5F7F9FFDBEAF2FFBBDB
    EEFF92C7E8FF69B4E3FF86C2E9FF02598FFFFFFFFF00FFFFFF00FFFFFF00045A
    8FFDE4E9ECFFF5F6F6FFFCFCFCFFFCFCFCFF02598FFFF9FAFAFFC8DDEAFFA2CB
    E5FF90C4E5FF93C7E9FFABD3EDFF035A8FFFFFFFFF00FFFFFF00FFFFFF001263
    9590055A90FC02598FFF02598FFF02598FFF02598FFFF9FAFAFFE7EFF4FFE1EE
    F6FFC3DEEEFF89BAD9FF4589B4FF085F93DCFFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00035A91F6BCD5E4FF7DACCAFA216F
    A0F6045B92ED065D93AF03588F5D00808002FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0003588F4B035C919B03598E61005A
    8F22FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF07
    0000FE0300000001000000000000000000000000000000000000000000000001
    00000003000000070000000700000007000000070000F81F0000FDFF0000}
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  DesignSize = (
    269
    153)
  PixelsPerInch = 96
  TextHeight = 13
  object lblState: TLabel
    Left = 8
    Top = 49
    Width = 253
    Height = 20
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Searching for new version...'
  end
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 87
    Height = 13
    Caption = 'Installed version:'
  end
  object Label2: TLabel
    Left = 8
    Top = 28
    Width = 81
    Height = 13
    Caption = 'Newest version:'
  end
  object lblVersion: TLabel
    Left = 257
    Top = 8
    Width = 4
    Height = 13
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    Caption = '-'
  end
  object lblNewestVersion: TLabel
    Left = 257
    Top = 28
    Width = 4
    Height = 13
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    Caption = '-'
  end
  object lblChangeLog: TLabel
    Left = 112
    Top = 72
    Width = 51
    Height = 13
    Cursor = crHandPoint
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Changelog'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentFont = False
    Visible = False
    OnClick = lblChangeLogClick
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 92
    Width = 253
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object pnlNav: TPanel
    Left = 0
    Top = 113
    Width = 269
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 4
    Padding.Top = 4
    Padding.Right = 4
    Padding.Bottom = 4
    TabOrder = 1
    ExplicitLeft = -340
    ExplicitTop = 230
    ExplicitWidth = 609
    object Bevel2: TBevel
      Left = 4
      Top = 4
      Width = 261
      Height = 5
      Align = alTop
      Shape = bsTopLine
      ExplicitLeft = -7
      ExplicitWidth = 396
    end
    object cmdOK: TButton
      Left = 168
      Top = 9
      Width = 97
      Height = 27
      Align = alRight
      Caption = '&Download'
      Enabled = False
      TabOrder = 0
      OnClick = cmdOKClick
      ExplicitLeft = 160
    end
    object cmdCancel: TButton
      Left = 4
      Top = 9
      Width = 97
      Height = 27
      Align = alLeft
      Caption = '&Close'
      TabOrder = 1
      OnClick = cmdCancelClick
    end
  end
end
