object ComboItemsDlg: TComboItemsDlg
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Items'
  ClientHeight = 218
  ClientWidth = 552
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Grid: TStringGrid
    Left = 0
    Top = 0
    Width = 463
    Height = 218
    Align = alClient
    ColCount = 3
    DefaultColWidth = 100
    DefaultRowHeight = 21
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    TabOrder = 0
    OnSelectCell = GridSelectCell
    ExplicitWidth = 465
  end
  object CbxType: TComboBox
    Left = 312
    Top = 8
    Width = 105
    Height = 21
    TabOrder = 1
    Visible = False
    OnChange = CbxTypeChange
    OnExit = CbxTypeExit
  end
  object PanBtn: TPanel
    Left = 463
    Top = 0
    Width = 89
    Height = 218
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitLeft = 465
    DesignSize = (
      89
      218)
    object BtnOk: TButton
      Left = 7
      Top = 155
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Ok'
      ModalResult = 1
      TabOrder = 2
    end
    object BtnClose: TButton
      Left = 8
      Top = 186
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Close'
      ModalResult = 2
      TabOrder = 3
    end
    object BtnAdd: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Append'
      TabOrder = 0
      OnClick = BtnAddClick
    end
    object BtnDel: TButton
      Left = 8
      Top = 40
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Delete'
      TabOrder = 1
      OnClick = BtnDelClick
    end
  end
end
