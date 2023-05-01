﻿unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Math,
  Checker, Calculator, Converter, List, Graph;

Type
  TMainForm = class(TForm)
    GraphPanel: TPanel;
    EditPanel: TPanel;
    InputEdit: TEdit;
    ShowGraphButton: TButton;
    GraphPaintBox: TPaintBox;
    RangeFromEdit: TEdit;
    RangeToEdit: TEdit;
    RangeLabel: TLabel;
    FromLabel: TLabel;
    ToLabel: TLabel;
    MathInputButton: TButton;
    MathInputPanel: TPanel;
    SinButton: TButton;
    CosButton: TButton;
    TgButton: TButton;
    CtgButton: TButton;
    ASinButton: TButton;
    ACosButton: TButton;
    ATgButton: TButton;
    ACtgButton: TButton;
    SqrtButton: TButton;
    RangeAndBuildPanel: TPanel;
    LogButton: TButton;
    LnButton: TButton;
    AbsButton: TButton;
    ColorBox: TColorBox;
    PenWidthComboBox: TComboBox;
    ClearGraphButton: TButton;
    ClearAllButton: TButton;
    ClearInputButton: TButton;
    MinusScaleButton: TButton;
    PlusScaleButton: TButton;
    SquareButton: TButton;
    XSquareButton: TButton;
    PiButton: TButton;
    ScaleLabel: TLabel;
    InfoScaleLabel: TLabel;
    ExtremaCheckBox: TCheckBox;
    procedure InputEditChange(Sender: TObject);
    procedure GraphPaintBoxPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RangeFromEditChange(Sender: TObject);
    procedure RangeToEditChange(Sender: TObject);
    procedure ShowGraphButtonClick(Sender: TObject);
    procedure MathInputButtonClick(Sender: TObject);
    procedure ClearInputButtonClick(Sender: TObject);
    procedure SinButtonClick(Sender: TObject);
    procedure CosButtonClick(Sender: TObject);
    procedure TgButtonClick(Sender: TObject);
    procedure CtgButtonClick(Sender: TObject);
    procedure ASinButtonClick(Sender: TObject);
    procedure ACosButtonClick(Sender: TObject);
    procedure ATgButtonClick(Sender: TObject);
    procedure ACtgButtonClick(Sender: TObject);
    procedure SqrtButtonClick(Sender: TObject);
    procedure LogButtonClick(Sender: TObject);
    procedure LnButtonClick(Sender: TObject);
    procedure AbsButtonClick(Sender: TObject);
    {procedure InputEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);}
    procedure ClearGraphButtonClick(Sender: TObject);
    procedure ClearAllButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    Procedure PaintYAxis(const X: Integer);
    Procedure PaintXAxis(const Y: Integer);
    Procedure ClearPaintBox();
    Function GetSelectedWidth(): Byte;
    Procedure SetEditEnabled(const Value: Boolean);
    Procedure SetClearButtonEnabled(const Value: Boolean);
    Procedure PaintAllGraphs();
    procedure InputEditKeyPress(Sender: TObject; var Key: Char);
    procedure PlusScaleButtonClick(Sender: TObject);
    procedure MinusScaleButtonClick(Sender: TObject);
    procedure SquareButtonClick(Sender: TObject);
    procedure XSquareButtonClick(Sender: TObject);
    procedure PiButtonClick(Sender: TObject);

  private
    CurrXAxisPos, CurrYAxisPos: Integer;
    GraphsArray: array [1..3] of TGraph;
    XFrom, XTo, YFrom, YTo: Integer;
    GraphAmount: Byte;
    YOffset: Integer;
    Scale: Integer;
    Range, Step: Real;
    MathInput: Boolean;
    LBorder, RBorder: Integer;
    IterationsPerUnit: Integer;
    PrevWidth, PrevHeight: Integer;
  public
    GraphPicture: TBitmap;
  end;

const
  ITERATION_COUNT = 10000;
  MAX_GRAPH_AMOUNT = 3;
  STANDART_PEN_WIDTH = 3;

var
  MainForm: TMainForm;
  ZoomFactor: Byte;

implementation

{$R *.dfm}

Procedure TMainForm.PaintYAxis(const X: Integer);
const
  ARROW_WIDTH = 10;
  STICK_WIDTH = 4;
var
  Width: Integer;
  Color: TColor;
  Y, I: Integer;
begin
  Width := Self.GraphPicture.Canvas.Pen.Width;
  Color := Self.GraphPicture.Canvas.Pen.Color;
  with Self.GraphPicture.Canvas do
    begin
      Pen.Width := STANDART_PEN_WIDTH;
      Pen.Color := clBlack;
      MoveTo(X, 0);
      LineTo(X, Self.GraphPaintBox.Height);       // axis painting
      Y := Self.Scale;
      for I := Self.YTo - 1 downto Self.YFrom + 1  do      // sticks painting
        Begin
          if (I <> 0) then
            Begin
              MoveTo(Self.CurrYAxisPos - STICK_WIDTH, Y);
              LineTo(Self.CurrYAxisPos + STICK_WIDTH, Y);
              TextOut(Self.CurrYAxisPos + STICK_WIDTH, Y, IntToStr(I));
            End;
          Y := Y + Self.Scale;
        End;
      MoveTo(Self.CurrYAxisPos - ARROW_WIDTH, ARROW_WIDTH);
      LineTo(Self.CurrYAxisPos, 0);
      LineTo(Self.CurrYAxisPos + ARROW_WIDTH, ARROW_WIDTH);
      Pen.Color := Color;
      Pen.Width := Width;
    end;
end;

procedure EditScaleLabel(var ScaleLabel: TLabel; const Delta: Short);
var
  CurrScale: Byte;
begin
  CurrScale := StrToInt(Copy(ScaleLabel.Caption, 1, Length(ScaleLabel.Caption) - 1));
  Inc(CurrScale, Delta);
  ScaleLabel.Caption := IntToStr(CurrScale) + '%';
end;

procedure TMainForm.PlusScaleButtonClick(Sender: TObject);
begin
  EditScaleLabel(ScaleLabel, 10);
  Inc(XFrom);
  Dec(XTo);
  Inc(YFrom);
  Dec(YTo);
  Inc(LBorder, IterationsPerUnit);
  Dec(RBorder, IterationsPerUnit);
  Step := GraphPaintBox.Width / (RBorder - LBorder);
  Scale := Trunc(GraphPaintBox.Width / (XTo - XFrom));
  CurrXAxisPos := YTo * Scale;
  CurrYAxisPos := -XFrom * Scale;
  ClearPaintBox();
  PaintXAxis(CurrXAxisPos);
  PaintYAxis(CurrYAxisPos);
  YOffset := CurrXAxisPos;
  PaintAllGraphs();
  GraphPaintBox.Canvas.Draw(0, 0, GraphPicture);
  MinusScaleButton.Enabled := True;
  if (XTo - XFrom = 4) then
    PlusScaleButton.Enabled := False;

end;

procedure TMainForm.MinusScaleButtonClick(Sender: TObject);
begin
  EditScaleLabel(ScaleLabel, -10);
  if (LBorder = 0) then
    Begin
      Inc(XTo, 2);
      Inc(RBorder, 2*IterationsPerUnit);
    End
  else if (RBorder = ITERATION_COUNT) then
    Begin
      Dec(XFrom, 2);
      Dec(LBorder, 2*IterationsPerUnit);
    End
  else
    Begin
      Dec(XFrom);
      Dec(LBorder, IterationsPerUnit);
      Inc(XTo);
      Inc(RBorder, IterationsPerUnit);
    End;
  Dec(YFrom);
  Inc(YTo);
  Step := GraphPaintBox.Width / (RBorder - LBorder);
  Scale := Trunc(GraphPaintBox.Width / (XTo - XFrom));
  CurrXAxisPos := YTo * Scale;
  CurrYAxisPos := -XFrom * Scale;
  ClearPaintBox();
  PaintXAxis(CurrXAxisPos);
  PaintYAxis(CurrYAxisPos);
  YOffset := CurrXAxisPos;
  PaintAllGraphs();
  GraphPaintBox.Canvas.Draw(0, 0, GraphPicture);
  PlusScaleButton.Enabled := True;
  if (StrToInt(RangeToEdit.Text) - StrToInt(RangeFromEdit.Text) = XTo - XFrom) then
    MinusScaleButton.Enabled := False;
end;

Procedure TMainForm.PaintXAxis(const Y: Integer);
const
  ARROW_WIDTH = 10;
  STICK_WIDTH = 4;
var
  Width: Integer;
  Color: TColor;
  X, I: Integer;
begin
  with Self.GraphPicture.Canvas do
    begin
      Width := Pen.Width;
      Color := Pen.Color;
      Pen.Width := STANDART_PEN_WIDTH;
      Pen.Color := clBlack;
      MoveTo(0, Y);
      LineTo(Self.GraphPaintBox.Width, Y);     // axis painting
      X := Scale;
      for I := Self.XFrom + 1 to Self.XTo - 1 do      // sticks painting
        Begin
          MoveTo(X, Self.CurrXAxisPos - STICK_WIDTH);
          LineTo(X, Self.CurrXAxisPos + STICK_WIDTH);
          TextOut(X, Self.CurrXAxisPos + STICK_WIDTH, IntToStr(I));
          X := X + Self.Scale;
        End;
      MoveTo(Self.GraphPaintBox.Width - ARROW_WIDTH, Self.CurrXAxisPos - ARROW_WIDTH);
      LineTo(Self.GraphPaintBox.Width, Self.CurrXAxisPos);
      LineTo(Self.GraphPaintBox.Width - ARROW_WIDTH, Self.CurrXAxisPos + ARROW_WIDTH);
      Pen.Color := Color;
      Pen.Width := Width;
    end;
end;

Procedure TMainForm.PaintAllGraphs();
Begin
  for var I := 1 to GraphAmount do
    Begin
      GraphsArray[I].Paint(Self.GraphPicture, Self.Step, Self.Scale, Self.YOffset, Self.LBorder, Self.RBorder);
      if GraphsArray[I].MaxExtrList <> nil then
        GraphsArray[I].PaintExtremaDots(Self.GraphPicture, Self.Scale, Self.XFrom, Self.YTo);
    End;
End;

Function TMainForm.GetSelectedWidth(): Byte;
var
  SelectedWidth: String;
begin
  SelectedWidth := Self.PenWidthComboBox.Items[Self.PenWidthComboBox.ItemIndex];
    with Self.GraphPicture.Canvas.Pen do
    Begin
      if (SelectedWidth = 'mid') then
        Result := 3
      else if (SelectedWidth = 'low') then
        Result := 1
      else
        Result := 5;
    End;
end;

Procedure TMainForm.SetEditEnabled(const Value: Boolean);
begin
  with Self do
    Begin
      RangeToEdit.Enabled := Value;
      RangeFromEdit.Enabled := Value;
    End;
end;

Procedure TMainForm.SetClearButtonEnabled(const Value: Boolean);
begin
  with Self do
    Begin
      ClearAllButton.Enabled := Value;
      ClearGraphButton.Enabled := Value;
    End;
end;

Procedure TMainForm.ClearPaintBox();
var
  Color: TColor;
begin
  with Self.GraphPicture.Canvas do
    Begin
      Color := Pen.Color;
      Pen.Color := clWhite;
      Rectangle(0, 0, Self.GraphPaintBox.Width, Self.GraphPaintBox.Height);
      Pen.Color := Color;
    End;
end;

Procedure TMainForm.FormCreate(Sender: TObject);

  Procedure SetFormMaxHeight();
    Begin
      var ScreenHeight := GetSystemMetrics(SM_CYSCREEN);
      Self.Constraints.MaxHeight := ScreenHeight - (Self.Height - Self.ClientHeight);;
    End;

  Procedure InitPenColorComboBox();
    Const
      ColorNames: array[0..6] of String = ('Черный', 'Красный', 'Зеленый', 'Синий', 'Желтый', 'Оранжевый', 'Розовый');
      ColorValues: array[0..6] of string = ('$000000', '$0000FF', '$00FF00', '$FF0000', '$00CCFF', '$00A5FF', '$FF00FF');
    Var
      I: Integer;
    begin
      Self.ColorBox.Clear;
      for I := Low(ColorValues) to High(ColorValues) do
        Self.ColorBox.Items.AddObject(ColorNames[i], TObject(StringToColor(ColorValues[i])));
      ColorBox.Selected := clBlack;
    end;

    Procedure InitPenWidthComboBox();
      Begin
        with Self.PenWidthComboBox do
          Begin
            Items.Add('low');
            Items.Add('mid');
            Items.Add('high');
            ItemIndex := 1;
          End;
      End;
  begin
    XFrom := -10;
    XTo := 10;
    YFrom := -10;
    YTo := 10;
    GraphAmount := 0;
    Step := GraphPaintBox.Width / ITERATION_COUNT;
    Range := (XTo - XFrom) / ITERATION_COUNT;
    Scale := GraphPaintBox.Width div (XTo - XFrom);
    CurrXAxisPos := GraphPaintBox.Height div 2;
    CurrYAxisPos := GraphPaintBox.Width div 2;
    YOffset := CurrXAxisPos;
    LBorder := 0;
    RBorder := ITERATION_COUNT;
    PrevWidth := ClientWidth;
    PrevHeight := ClientHeight;
    IterationsPerUnit := ITERATION_COUNT div (XTo - XFrom);
    MathInput := False;
    SetClearButtonEnabled(False);
    SetFormMaxHeight();
    GraphPicture := TBitmap.Create;
    GraphPicture.Canvas.Pen.Width := 3;
    MathInputPanel.Visible := False;
    ShowGraphButton.Enabled := False;
    MinusScaleButton.Enabled := False;
    InitPenWidthComboBox();
    InitPenColorComboBox();
    GraphPicture.SetSize(GraphPaintBox.Width, GraphPaintBox.Height);
    PaintYAxis(CurrXAxisPos);
    PaintXAxis(CurrYAxisPos);
    GraphPaintBox.Canvas.Draw(0, 0, GraphPicture);
  end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  X: Real;
begin
  case Key of
    VK_UP, VK_DOWN:
      Begin
//        if (not InputEdit.Focused) then
          with GraphPicture.Canvas.Pen do
            begin
              KeyPreview := True;
              ClearPaintBox();
              if (Key = VK_UP) then
                begin
                  Inc(YTo);
                  Inc(YFrom);
                  CurrXAxisPos := CurrXAxisPos + Scale;
                  YOffset := YOffset + Scale;
                end
              else
                begin
                  Dec(YTo);
                  Dec(YFrom);
                  CurrXAxisPos := CurrXAxisPos - Scale;
                  YOffset := YOffset - Scale;
                end;

              PaintXAxis(CurrXAxisPos);
              PaintYAxis(CurrYAxisPos);
              PaintAllGraphs();
            end;
          GraphPaintBox.Canvas.Draw(0, 0, GraphPicture);
      End;
    VK_RIGHT, VK_LEFT:
      begin
//        if (not InputEdit.Focused) then
        with GraphPicture.Canvas.Pen do
          begin
            KeyPreview := True;
            ClearPaintBox();
            if (Key = VK_LEFT) then
              begin
                CurrYAxisPos := CurrYAxisPos + Scale;
                if (LBorder > 0) then
                  Begin
                    Dec(RBorder, IterationsPerUnit);
                    Dec(LBorder, IterationsPerUnit);
                  End
                else
                  for var I := 1 to GraphAmount do
                    Begin
                      GraphsArray[I].ShiftArrayOfDotsRight(Self.XFrom, Self.IterationsPerUnit, Self.Range);
                      GraphsArray[I].FindExtrema(Self.XFrom - 1, Self.Range);
                    End;
                Dec(XTo);
                Dec(XFrom);
              end
            else
              begin
                CurrYAxisPos := CurrYAxisPos - Scale;
                 if (RBorder < ITERATION_COUNT) then
                   Begin
                     Inc(LBorder, IterationsPerUnit);
                     Inc(RBorder, IterationsPerUnit);
                   End
                 else
                   for var I := 1 to GraphAmount do
                     Begin
                       GraphsArray[I].ShiftArrayOfDotsLeft(Self.XTo, Self.IterationsPerUnit, Self.Range);
                       GraphsArray[I].FindExtrema(Self.XFrom + 1, Self.Range);
                     End;
                Inc(XTo);
                Inc(XFrom);
              end;
            PaintXAxis(CurrXAxisPos);
            PaintYAxis(CurrYAxisPos);

            PaintAllGraphs();
          end;
        GraphPaintBox.Canvas.Draw(0, 0, GraphPicture);
      end;
    VK_RETURN:
      Begin
        KeyPreview := True;
        if (ShowGraphButton.Enabled) then
          ShowGraphButtonClick(Sender);
      End;
  end;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  if (ClientWidth <> PrevWidth) and (Height < Self.Constraints.MaxHeight) then
    Begin
      Self.ClientHeight := ClientWidth - EditPanel.Width;
      PrevWidth := ClientWidth;
    End
  else if (ClientHeight <> PrevHeight) then
    Begin
      Width := ClientHeight + EditPanel.Width;
      PrevHeight := ClientHeight;
    End;
  ClearPaintBox();
  Scale := GraphPaintBox.Width div (XTo - XFrom);
  CurrXAxisPos := YTo * Scale;
  CurrYAxisPos := -XFrom * Scale;
  PaintXAxis(CurrXAxisPos);
  PaintYAxis(CurrYAxisPos);
  YOffset := CurrXAxisPos;
  PaintAllGraphs();
  GraphPicture.SetSize(GraphPaintBox.Width, GraphPaintBox.Height);
  GraphPaintBox.Canvas.Draw(0, 0, GraphPicture);

end;

procedure TMainForm.GraphPaintBoxPaint(Sender: TObject);
begin
  GraphPaintBox.Canvas.Draw(0, 0, GraphPicture);
end;

procedure TMainForm.InputEditChange(Sender: TObject);
Var
    Text: String;
begin
  Text := Lowercase(InputEdit.Text);
  if (not IsMathExprValid(Text)) then
    ShowGraphButton.Enabled := False
  else
    ShowGraphButton.Enabled := True;
end;

procedure TMainForm.InputEditKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then
    KeyPreview := True;
end;

//procedure TMainForm.InputEditKeyDown(Sender: TObject; var Key: Word;
//  Shift: TShiftState);
//begin
//  if (Key = VK_RETURN) and (ShowGraphButton.Enabled) then
//    Begin
//      ShowGraphButtonClick(Sender);
//      Key := 0;
//  End;
//end;

procedure TMainForm.ClearInputButtonClick(Sender: TObject);
begin
  InputEdit.Text := '';
  InputEdit.SetFocus;
end;

procedure TMainForm.MathInputButtonClick(Sender: TObject);
begin
  if (not MathInput) then
    Begin
      MathInputPanel.Visible := True;
      MathInput := True;
      MathInputPanel.Top := RangeAndBuildPanel.Top;
      RangeAndBuildPanel.Top := MathInputPanel.Top + MathInputPanel.Height;
    End
  else
    Begin
      MathInputPanel.Visible := False;
      MathInput := False;
      RangeAndBuildPanel.Top := MathInputPanel.Top;
      MathInputPanel.Top := RangeAndBuildPanel.Top + RangeAndBuildPanel.Height;
    End
end;

procedure TMainForm.RangeFromEditChange(Sender: TObject);
begin
  if (not CheckInput(RangeFromEdit.Text) or (XTo <= StrToInt(RangeFromEdit.Text))) then
    Begin
      // покраснение рамки edit и блокировка кнопки
      ShowGraphButton.Enabled := False;
    End
  else
    Begin
      ShowGraphButton.Enabled := True;
      ClearPaintBox();
      XFrom := StrToInt(RangeFromEdit.Text);
      Scale := Trunc(GraphPaintBox.Width / (XTo - XFrom));
      YTo := Abs((XTo - XFrom) div 2);
      if ((XTo - XFrom) mod 2 = 1) then
        YFrom := -YTo - 1
      else
        YFrom := -YTo;
      CurrXAxisPos := Abs(YTo) * Scale;
      CurrYAxisPos := -XFrom * Scale;
      YOffset := CurrXAxisPos;
      PaintXAxis(CurrXAxisPos);
      PaintYAxis(CurrYAxisPos);
      Range := (XTo - XFrom) / ITERATION_COUNT;
      IterationsPerUnit := ITERATION_COUNT div (XTo - XFrom);
      GraphPaintBox.Canvas.Draw(0, 0, GraphPicture);
    End;
end;

procedure TMainForm.RangeToEditChange(Sender: TObject);
begin
  if (not CheckInput(RangeToEdit.Text) or (XFrom >= StrToInt(RangeToEdit.Text))) then
    Begin
      // покраснение рамки edit и блокировка кнопки
      ShowGraphButton.Enabled := False;
    End
  else
    Begin
      ShowGraphButton.Enabled := True;
      ClearPaintBox();
      XTo := StrToInt(RangeToEdit.Text);
      Scale := Trunc(GraphPaintBox.Width / (XTo - XFrom));
      YTo := Abs((XTo - XFrom) div 2);
      if ((XTo - XFrom) mod 2 = 1) then
        YFrom := -YTo - 1
      else
        YFrom := -YTo;
      CurrXAxisPos := Abs(YTo) * Scale;
      CurrYAxisPos := -XFrom * Scale;
      YOffset := CurrXAxisPos;
      PaintXAxis(CurrXAxisPos);
      PaintYAxis(CurrYAxisPos);
      Range := (XTo - XFrom) / ITERATION_COUNT;
      IterationsPerUnit := ITERATION_COUNT div (XTo - XFrom);
      GraphPaintBox.Canvas.Draw(0, 0, GraphPicture);
    End;
end;

procedure TMainForm.ShowGraphButtonClick(Sender: TObject);
  Var
    CurrX, CurrY: Real;
    I: Integer;

begin
  Inc(GraphAmount);
  SetEditEnabled(False);
  SetClearButtonEnabled(True);
  CurrX := XFrom - LBorder div IterationsPerUnit;

  GraphsArray[GraphAmount] := TGraph.Create(ConvertToPolishNotation(InputEdit.Text), ColorBox.Selected, GetSelectedWidth(), Self.Range, CurrX);

  if (GraphAmount = 1) and not ((GraphsArray[GraphAmount].MaxY <= YFrom) and (GraphsArray[GraphAmount].MinY >= YTo)) then
    Begin
      var Delta := (XTo - XFrom) div 2;
      if (GraphsArray[GraphAmount].MaxY <= YFrom) then
        Begin
          YTo := Trunc(GraphsArray[GraphAmount].MaxY) + Delta;
          YFrom := Trunc(GraphsArray[GraphAmount].MaxY) - Delta;
        End;

      if (GraphsArray[GraphAmount].MinY >= YTo) then
        Begin
          YTo := Trunc(GraphsArray[GraphAmount].MinY) + Delta;
          YFrom := Trunc(GraphsArray[GraphAmount].MinY) - Delta;
        End;
      ClearPaintBox();
      CurrXAxisPos := YTo * Scale;
      YOffset := CurrXAxisPos;
      PaintXAxis(CurrXAxisPos);
      PaintYAxis(CurrYAxisPos);
    End;

  if (ExtremaCheckBox.Checked) then
    Begin
      GraphsArray[GraphAmount].FindExtrema(Self.XFrom, Self.Range);
      GraphsArray[GraphAmount].PaintExtremaDots(Self.GraphPicture, Self.Scale, Self.XFrom, Self.YTo);
    End;

  GraphsArray[GraphAmount].Paint(Self.GraphPicture, Self.Step, Self.Scale, Self.YOffset, Self.LBorder, Self.RBorder);

  GraphPaintBox.Canvas.Draw(0, 0, GraphPicture);
  if (GraphAmount = MAX_GRAPH_AMOUNT) then
    Begin
      MathInputPanel.Enabled := False;
      InputEdit.Enabled := False;
      ColorBox.Enabled := False;
      PenWidthComboBox.Enabled := False;
      ShowGraphButton.Enabled := False;
      PenWidthComboBox.Enabled := False;
      ColorBox.Enabled := False;
      ClearInputButton.Enabled := False;
    End;
end;

procedure TMainForm.ClearAllButtonClick(Sender: TObject);
begin
  GraphAmount := 0;
  ClearPaintBox();
  XFrom := -10;
  XTo := 10;
  YTo := 10;
  YFrom := -10;
  LBorder := 0;
  RBorder := ITERATION_COUNT;
  CurrXAxisPos := YTo * Scale;
  CurrYAxisPos := -XFrom * Scale;
  YOffset := CurrXAxisPos;
  PaintYAxis(CurrXAxisPos);
  PaintXAxis(CurrYAxisPos);
  GraphPaintBox.Canvas.Draw(0,0,GraphPicture);
  RangeFromEdit.Text := IntToStr(XFrom);
  RangeToEdit.Text := IntToStr(XTo);
  SetEditEnabled(True);
  SetClearButtonEnabled(False);
  InputEdit.Enabled := True;
  InputEdit.Clear;
  ColorBox.Enabled := True;
  PenWidthComboBox.Enabled := True;
  MathInputPanel.Enabled := True;
  PenWidthComboBox.ItemIndex := 1;
  ShowGraphButton.Enabled := True;
  PenWidthComboBox.Enabled := True;
  ColorBox.Enabled := True;
  ClearInputButton.Enabled := True;
end;

procedure TMainForm.ClearGraphButtonClick(Sender: TObject);
begin
  Dec(GraphAmount);
  if (GraphAmount = 0) then
    ClearGraphButton.Enabled := False;

  ClearPaintBox();
  PaintYAxis(CurrYAxisPos);
  PaintXAxis(CurrXAxisPos);
  PaintAllGraphs();
  GraphPaintBox.Canvas.Draw(0,0,GraphPicture);
  InputEdit.Enabled := True;
  ColorBox.Enabled := True;
  PenWidthComboBox.Enabled := True;
  MathInputPanel.Enabled := True;
  ShowGraphButton.Enabled := True;
end;

procedure MathInputEditor(var InputEdit: TEdit; const Text: String);
var
  CurrInput: String;
  CurrCursorPos: Integer;
begin
  CurrInput := InputEdit.Text;
  CurrCursorPos := InputEdit.SelStart;
  Insert(Text, CurrInput, CurrCursorPos + 1);
  InputEdit.Text := CurrInput;
  InputEdit.SetFocus;
  InputEdit.SelStart := CurrCursorPos + Pos('(', Text);
end;

procedure TMainForm.SinButtonClick(Sender: TObject);
begin
  MathInputEditor(Self.InputEdit, 'sin()');
end;

procedure TMainForm.CosButtonClick(Sender: TObject);
begin
  MathInputEditor(Self.InputEdit, 'cos()');
end;

procedure TMainForm.TgButtonClick(Sender: TObject);
begin
  MathInputEditor(Self.InputEdit, 'tg()');
end;

procedure TMainForm.CtgButtonClick(Sender: TObject);
begin
  MathInputEditor(Self.InputEdit, 'ctg()');
end;

procedure TMainForm.ASinButtonClick(Sender: TObject);
begin
  MathInputEditor(Self.InputEdit, 'arcsin()');
end;

procedure TMainForm.ACosButtonClick(Sender: TObject);
begin
  MathInputEditor(Self.InputEdit, 'arccos()');
end;

procedure TMainForm.ATgButtonClick(Sender: TObject);
begin
  MathInputEditor(Self.InputEdit, 'arctg()');
end;

procedure TMainForm.ACtgButtonClick(Sender: TObject);
begin
  MathInputEditor(Self.InputEdit, 'arcctg()');
end;

procedure TMainForm.SqrtButtonClick(Sender: TObject);
begin
  MathInputEditor(Self.InputEdit, 'sqrt()');
end;

procedure TMainForm.LogButtonClick(Sender: TObject);
begin
  MathInputEditor(Self.InputEdit, 'log10()');
end;

procedure TMainForm.LnButtonClick(Sender: TObject);
begin
  MathInputEditor(Self.InputEdit, 'ln()');
end;

procedure TMainForm.AbsButtonClick(Sender: TObject);
begin
  MathInputEditor(Self.InputEdit, 'abs()');
end;

procedure TMainForm.SquareButtonClick(Sender: TObject);
begin
  MathInputEditor(Self.InputEdit, '()^2');
end;

procedure TMainForm.XSquareButtonClick(Sender: TObject);
var
  CurrInput: String;
  CurrCursorPos: Integer;
begin
  CurrInput := InputEdit.Text;
  CurrCursorPos := InputEdit.SelStart;
  Insert('x^2', CurrInput, CurrCursorPos + 1);
  InputEdit.Text := CurrInput;
  InputEdit.SetFocus;
  InputEdit.SelStart := CurrCursorPos + 3;
end;

procedure TMainForm.PiButtonClick(Sender: TObject);
var
  CurrInput: String;
  CurrCursorPos: Integer;
begin
  CurrInput := InputEdit.Text;
  CurrCursorPos := InputEdit.SelStart;
  Insert('Pi', CurrInput, CurrCursorPos + 1);
  InputEdit.Text := CurrInput;
  InputEdit.SetFocus;
  InputEdit.SelStart := CurrCursorPos + 2;
end;

end.
