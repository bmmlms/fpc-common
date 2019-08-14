object frmMsgDlg: TfrmMsgDlg
  Left = 488
  Top = 454
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Info'
  ClientHeight = 170
  ClientWidth = 421
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  DesignSize = (
    421
    170)
  PixelsPerInch = 96
  TextHeight = 13
  object imgIcon: TImage
    Left = 8
    Top = 4
    Width = 32
    Height = 32
  end
  object chkNotShowAgain: TCheckBox
    Left = 4
    Top = 109
    Width = 277
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = '&Don'#39't show this message again'
    TabOrder = 1
  end
  object txtText: TMemo
    Left = 48
    Top = 4
    Width = 369
    Height = 101
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = bsNone
    Color = clBtnFace
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object pnlNav: TPanel
    Left = 0
    Top = 130
    Width = 421
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 4
    Padding.Top = 4
    Padding.Right = 4
    Padding.Bottom = 4
    TabOrder = 2
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
    object cmdNoCancel: TButton
      Left = 4
      Top = 9
      Width = 97
      Height = 27
      Align = alLeft
      Caption = '&Cancel'
      TabOrder = 1
      OnClick = cmdNoCancelClick
    end
    object cmdYesOK: TButton
      Left = 320
      Top = 9
      Width = 97
      Height = 27
      Align = alRight
      Caption = '&OK'
      TabOrder = 0
      OnClick = cmdYesOKClick
    end
  end
end
