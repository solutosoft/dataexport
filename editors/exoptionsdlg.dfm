object OptionsDlg: TOptionsDlg
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Options'
  ClientHeight = 243
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Grid: TStringGrid
    Left = 0
    Top = 0
    Width = 554
    Height = 208
    Align = alClient
    ColCount = 2
    DefaultColWidth = 260
    DefaultRowHeight = 21
    RowCount = 1
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    TabOrder = 0
    OnSelectCell = GridSelectCell
  end
  object PanBtn: TPanel
    Left = 0
    Top = 208
    Width = 554
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      554
      35)
    object BtnClose: TButton
      Left = 476
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Close'
      ModalResult = 2
      TabOrder = 0
    end
    object BtnOk: TButton
      Left = 395
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Ok'
      ModalResult = 1
      TabOrder = 1
    end
  end
  object CbxValue: TComboBox
    Left = 16
    Top = 48
    Width = 105
    Height = 21
    TabOrder = 2
    Visible = False
    OnChange = CbxValueChange
    OnExit = CbxValueExit
  end
end
