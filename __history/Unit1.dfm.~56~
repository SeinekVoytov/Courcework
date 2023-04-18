object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 577
  ClientWidth = 851
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GraphPanel: TPanel
    Left = 274
    Top = 0
    Width = 577
    Height = 577
    Align = alClient
    TabOrder = 0
    object GraphPaintBox: TPaintBox
      Left = 1
      Top = 1
      Width = 575
      Height = 575
      Align = alClient
      OnPaint = GraphPaintBoxPaint
      ExplicitLeft = 6
      ExplicitTop = 2
      ExplicitWidth = 598
    end
  end
  object EditPanel: TPanel
    Left = 0
    Top = 0
    Width = 274
    Height = 577
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object InputEdit: TEdit
      Left = 16
      Top = 36
      Width = 209
      Height = 20
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnChange = InputEditChange
      OnKeyDown = InputEditKeyDown
    end
    object MathInputButton: TButton
      AlignWithMargins = True
      Left = 223
      Top = 36
      Width = 21
      Height = 21
      Hint = #1052#1072#1090#1077#1084#1072#1090#1080#1095#1077#1089#1082#1080#1081' '#1074#1074#1086#1076
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = MathInputButtonClick
    end
    object MathInputPanel: TPanel
      Left = 5
      Top = 259
      Width = 269
      Height = 95
      BevelOuter = bvNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      object SinButton: TButton
        Left = 42
        Top = 8
        Width = 30
        Height = 25
        Caption = 'Sin'
        TabOrder = 0
        OnClick = SinButtonClick
      end
      object CosButton: TButton
        Left = 73
        Top = 8
        Width = 30
        Height = 25
        Caption = 'Cos'
        TabOrder = 1
        OnClick = CosButtonClick
      end
      object TgButton: TButton
        Left = 104
        Top = 8
        Width = 30
        Height = 25
        Caption = 'Tg'
        TabOrder = 2
        OnClick = TgButtonClick
      end
      object CtgButton: TButton
        Left = 135
        Top = 8
        Width = 30
        Height = 25
        Caption = 'Ctg'
        TabOrder = 3
        OnClick = CtgButtonClick
      end
      object ASinButton: TButton
        Left = 42
        Top = 34
        Width = 30
        Height = 25
        Caption = 'ASin'
        TabOrder = 4
        OnClick = ASinButtonClick
      end
      object ACosButton: TButton
        Left = 73
        Top = 34
        Width = 30
        Height = 25
        Caption = 'ACos'
        TabOrder = 5
        TabStop = False
        OnClick = ACosButtonClick
      end
      object ATgButton: TButton
        Left = 104
        Top = 34
        Width = 30
        Height = 25
        Caption = 'ATg'
        TabOrder = 6
        OnClick = ATgButtonClick
      end
      object ACtgButton: TButton
        Left = 135
        Top = 34
        Width = 30
        Height = 25
        Caption = 'ACtg'
        TabOrder = 7
        OnClick = ACtgButtonClick
      end
      object SqrtButton: TButton
        Left = 42
        Top = 60
        Width = 30
        Height = 25
        Caption = #8730
        DisabledImageIndex = 42
        TabOrder = 8
        OnClick = SqrtButtonClick
      end
      object LogButton: TButton
        Left = 73
        Top = 60
        Width = 30
        Height = 25
        Caption = 'Log'#8321#8320
        TabOrder = 9
        OnClick = LogButtonClick
      end
      object LnButton: TButton
        Left = 104
        Top = 60
        Width = 30
        Height = 25
        Caption = 'Ln'
        TabOrder = 10
        OnClick = LnButtonClick
      end
      object AbsButton: TButton
        Left = 134
        Top = 60
        Width = 30
        Height = 25
        Caption = '|  |'
        TabOrder = 11
        OnClick = AbsButtonClick
      end
    end
    object RangeAndBuildPanel: TPanel
      Left = -1
      Top = 65
      Width = 269
      Height = 188
      BevelOuter = bvNone
      TabOrder = 3
      object RangeLabel: TLabel
        Left = 17
        Top = 9
        Width = 110
        Height = 13
        Caption = #1043#1088#1072#1085#1080#1094#1099' '#1087#1086#1089#1090#1088#1086#1077#1085#1080#1103':'
      end
      object ToLabel: TLabel
        Left = 82
        Top = 31
        Width = 13
        Height = 13
        Caption = #1076#1086
      end
      object FromLabel: TLabel
        Left = 20
        Top = 31
        Width = 14
        Height = 13
        Caption = #1054#1090
      end
      object RangeFromEdit: TEdit
        Left = 37
        Top = 28
        Width = 41
        Height = 21
        TabOrder = 0
        Text = '-10'
        OnChange = RangeFromEditChange
        OnExit = RangeFromEditExit
      end
      object RangeToEdit: TEdit
        Left = 98
        Top = 28
        Width = 45
        Height = 21
        TabOrder = 1
        Text = '10'
        OnChange = RangeToEditChange
        OnExit = RangeToEditExit
      end
      object ShowGraphButton: TButton
        Left = 34
        Top = 124
        Width = 106
        Height = 25
        Caption = #1055#1086#1089#1090#1088#1086#1080#1090#1100' '#1075#1088#1072#1092#1080#1082
        TabOrder = 2
        OnClick = ShowGraphButtonClick
      end
      object ColorBox: TColorBox
        Left = 17
        Top = 69
        Width = 145
        Height = 22
        TabOrder = 3
      end
      object PenWidthComboBox: TComboBox
        Left = 17
        Top = 97
        Width = 145
        Height = 21
        TabOrder = 4
      end
      object ClearGraphButton: TButton
        Left = 168
        Top = 124
        Width = 91
        Height = 25
        Caption = #1057#1090#1077#1088#1077#1090#1100' '#1075#1088#1072#1092#1080#1082
        TabOrder = 5
        OnClick = ClearGraphButtonClick
      end
      object ClearAllButton: TButton
        Left = 170
        Top = 155
        Width = 89
        Height = 25
        Caption = 'O'#1095#1080#1089#1090#1080#1090#1100' '
        TabOrder = 6
        OnClick = ClearAllButtonClick
      end
    end
  end
end
