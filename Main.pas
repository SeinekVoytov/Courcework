unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Math, Checker, Calculator, Converter;

Type
  TDotArray = array [1..10000] of Real;
type
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
    procedure InputEditChange(Sender: TObject);
    procedure GraphPaintBoxPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RangeFromEditChange(Sender: TObject);
    procedure RangeToEditChange(Sender: TObject);
    procedure ShowGraphButtonClick(Sender: TObject);
    procedure MathInputButtonClick(Sender: TObject);
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
    procedure InputEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RangeFromEditExit(Sender: TObject);
    procedure RangeToEditExit(Sender: TObject);
    procedure ClearGraphButtonClick(Sender: TObject);
    procedure ClearAllButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  private
    CurrXAxisPos, CurrYAxisPos: Integer;
    DotArrays: array [1..3] of TDotArray;
    RangeFrom, RangeTo: Integer;
    PolNotExprs: array [1..3] of String;
    GraphNumber: Byte;
    XOffset, YOffset: Real;
    Step, Range: Real;
    ColorsArray: array [1..3] of TColor;
    WidthArray: array [1..3] of Byte;
    MathInput: Boolean;
  public
    GraphPicture: TBitmap;
  end;

const
  IterationCount = 10000;
  ScaleY = 28;
  ScaleX = 28;
  ColorNames: array[0..6] of String = ('Черный', 'Красный', 'Зеленый', 'Синий', 'Желтый', 'Оранжевый', 'Розовый');
  ColorValues: array[0..6] of string = ('$000000', '$0000FF', '$00FF00', '$FF0000', '$00FFFF', '$00A5FF', '$FF00FF');
var
  MainForm: TMainForm;

implementation

{$R *.dfm}

Function CheckInput(Const s: String): Boolean;
  Begin
    try
      StrToInt(s);
      Result := True;
    except
      Result := False;
    end;
  End;

Procedure PaintYAxis(const X: Integer);
var
  Temp: Integer;
begin
  Temp := MainForm.GraphPicture.Canvas.Pen.Width;
  with MainForm.GraphPicture.Canvas do
    begin
      Pen.Width := 3;
      MoveTo(X, 0);
      LineTo(X, MainForm.GraphPaintBox.Height);
      Pen.Width := Temp;
    end;
end;

Procedure PaintXAxis(const Y: Integer);
var
  Temp: Integer;
begin
  Temp := MainForm.GraphPicture.Canvas.Pen.Width;
  with MainForm.GraphPicture.Canvas do
    begin
      Pen.Width := 3;
      MoveTo(0, Y);
      LineTo(MainForm.GraphPaintBox.Width, Y);
      Pen.Width := Temp;
    end;
end;

Procedure RewriteYAxis(var CurrYAxisPos: Integer;
                       const RangeTo, RangeFrom, CurrXAxisPos: Integer
                       );
begin
  with MainForm.GraphPicture.Canvas do
        Begin
          Pen.Color := clWhite;
          Pen.Width := 3;
          PaintYAxis(CurrYAxisPos);

          CurrYAxisPos := Trunc(Abs(RangeFrom) * MainForm.GraphPaintBox.Width / (RangeTo - RangeFrom));

          Pen.Color := clBlack;
          PaintYAxis(CurrYAxisPos);
          PaintXAxis(CurrXAxisPos);
        End;

  MainForm.GraphPaintBox.Invalidate;
end;


Procedure SetSelectedWidth();
var
  SelectedWidth: String;
begin
  SelectedWidth := MainForm.PenWidthComboBox.Items[MainForm.PenWidthComboBox.ItemIndex];
    with MainForm.GraphPicture.Canvas.Pen do
    Begin
      if (SelectedWidth = 'mid') then
          Width := 3
      else if (SelectedWidth = 'low') then
        Width := 1
      else
        Width := 5;
      MainForm.WidthArray[MainForm.GraphNumber] := Width;;
    End;
end;

Procedure PaintGraph(Const DotArray: TDotArray; Const Step, XOffset, YOffset: Real);
var
  I: Integer;
  WasNan: Boolean;
  CurrX: Real;
begin
   WasNan := True;
   CurrX := 0;
   for I := 1 to High(DotArray) do
    Begin
      if (FloatToStr(DotArray[I]) = 'NAN') then
        WasNaN := True
      else if (WasNan) then
        Begin
          MainForm.GraphPicture.Canvas.MoveTo(Trunc(CurrX + XOffset), Trunc(DotArray[I] + YOffset));
          WasNan := False;
        End
      else
        MainForm.GraphPicture.Canvas.LineTo(Trunc(CurrX + XOffset), Trunc(DotArray[I] + YOffset));

      CurrX := CurrX + Step;
    End;

    MainForm.GraphPaintBox.Invalidate;
end;

Procedure PaintXAxisPoints(const ScaleX: Real);
begin

end;

Procedure PaintYAxisPoints(const ScaleY: Real);
var
  TempWidth: Byte;
begin
  TempWidth := MainForm.GraphPicture.Canvas.Pen.Width;
  with MainForm.GraphPicture.Canvas do
    Begin
      Pen.Width := 3;
      Pen.Color := clBlack;
      MoveTo(MainForm.GraphPaintBox.Width div 2 - 100, Trunc(ScaleY));
      LineTo(MainForm.GraphPaintBox.Width div 2 + 100, Trunc(ScaleY));
      Pen.Width := TempWidth;
    End;
end;

Procedure SetEditEnabled(const Value: Boolean);
begin
  with MainForm do
    Begin
      RangeToEdit.Enabled := Value;
      RangeFromEdit.Enabled := Value;
    End;
end;

Procedure SetButtonEnabled(const Value: Boolean);
begin
  with MainForm do
    Begin
      ClearAllButton.Enabled := Value;
      ClearGraphButton.Enabled := Value;
    End;
end;

Procedure ClearPaintBox();
var
  Color: TColor;
  Width: Byte;
begin
  with MainForm.GraphPicture.Canvas do
    Begin
      Color := Pen.Color;
      Width := Pen.Width;
      Pen.Color := clWhite;
      Rectangle(0,0,MainForm.GraphPaintBox.Width,MainForm.GraphPaintBox.Height);
      Pen.Width := 3;
      Pen.Color := Color;
      Pen.Width := Width;
    End;
end;

procedure TMainForm.FormCreate(Sender: TObject);
  var
    I: Integer;
  begin
    RangeFrom := -10;
    RangeTo := 10;
    GraphNumber := 0;
    Step := GraphPaintBox.Width / IterationCount;
    Range := 20 / IterationCount;
    XOffset := 0;
    YOffset := GraphPaintBox.Height / 2;
    CurrXAxisPos := GraphPaintBox.Height div 2;
    CurrYAxisPos := GraphPaintBox.Width div 2;
    MathInput := False;
    SetButtonEnabled(False);
    GraphPicture := TBitmap.Create;
    GraphPicture.Canvas.Pen.Width := 3;
    MathInputPanel.Visible := False;
    with PenWidthComboBox do
      Begin
        Items.Add('low');
        Items.Add('mid');
        Items.Add('high');
        ItemIndex := 1;
      End;

    ColorBox.Clear;
    for I := 0 to High(ColorValues) do
      ColorBox.Items.AddObject(ColorNames[i], TObject(StringToColor(ColorValues[i])));

    ColorBox.Selected := clBlack;
    GraphPicture.SetSize(GraphPaintBox.Width, GraphPaintBox.Height);
    PaintYAxis(CurrXAxisPos);
    PaintXAxis(CurrYAxisPos);
  end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  I: Integer;
  X: Real;
begin
  case Key of
    VK_UP, VK_DOWN:
      Begin
        with GraphPicture.Canvas.Pen do
          begin
            KeyPreview := True;
            ClearPaintBox();

            if (Key = VK_UP) then
              begin
                CurrXAxisPos := CurrXAxisPos + ScaleY;
                YOffset := YOffset + 2 * ScaleY;
              end
            else
              begin
                CurrXAxisPos := CurrXAxisPos - ScaleY;
                YOffset := YOffset - 2 * ScaleY;
              end;

            PaintXAxis(CurrXAxisPos);
            PaintYAxis(CurrYAxisPos);
            for I := 1 to GraphNumber do
              PaintGraph(DotArrays[I], Step, XOffset, YOffset);
          end;
        GraphPaintBox.Canvas.Draw(0, 0, GraphPicture);
      End;
    VK_RIGHT, VK_LEFT:
      begin
        with GraphPicture.Canvas.Pen do
          begin
            KeyPreview := True;
            ClearPaintBox();

            if (Key = VK_LEFT) then
              begin
                X := RangeFrom;
                CurrYAxisPos := CurrYAxisPos + 2 * ScaleX;
                for I := 9500 downto 1 do
                  DotArrays[GraphNumber][I + 500] := DotArrays[GraphNumber][I];
                for I := 500 downto 1 do
                  Begin
                    DotArrays[GraphNumber][I] := -TCalculate.Calculate(PolNotExprs[GraphNumber], X) * 0.05 * GraphPaintBox.Height;
                    X := X - Range;
                  End;
                Dec(RangeTo);
                Dec(RangeFrom);
              end
            else
              begin
                X := RangeTo;
                CurrYAxisPos := CurrYAxisPos - 2 * ScaleX;
                for I := 501 to 10000 do
                  DotArrays[GraphNumber][I - 500] := DotArrays[GraphNumber][I];
                for I := 9501 to 10000 do
                  Begin
                    DotArrays[GraphNumber][I] := -TCalculate.Calculate(PolNotExprs[GraphNumber], X) * 0.05 * GraphPaintBox.Height;
                    X := X + Range;
                  End;
                Inc(RangeTo);
                Inc(RangeFrom);
              end;

            PaintXAxis(CurrXAxisPos);
            PaintYAxis(CurrYAxisPos);
            for I := 1 to GraphNumber do
              PaintGraph(DotArrays[I], Step, XOffset, YOffset);
          end;
        GraphPaintBox.Canvas.Draw(0, 0, GraphPicture);
      end;
  end;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  GraphPicture.SetSize(GraphPaintBox.Width, GraphPaintBox.Height);
  GraphPaintBox.Invalidate;
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
  if (not TChecker.IsMathExprValid(Text)) then
    ShowGraphButton.Enabled := False
  else
    ShowGraphButton.Enabled := True;
end;

procedure TMainForm.InputEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (ShowGraphButton.Enabled) then
    Begin
      KeyPreview := True;
      Key := 0;
      ShowGraphButtonClick(Sender);
  End;
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
  if (not CheckInput(RangeFromEdit.Text) or (RangeTo <= RangeFrom)) then
    Begin
      // покраснение рамки edit и блокировка кнопки
    End
  else
    RangeFrom := StrToInt(RangeFromEdit.Text);
end;

procedure TMainForm.RangeToEditChange(Sender: TObject);
begin
  if (not CheckInput(RangeToEdit.Text) or (RangeTo <= RangeFrom)) then
    Begin
      // покраснение рамки edit и блокировка кнопки
    End
  else
    RangeTo :=  StrToInt(RangeToEdit.Text);
end;

procedure TMainForm.RangeToEditExit(Sender: TObject);
begin
  RewriteYAxis(CurrYAxisPos, RangeTo, RangeFrom, CurrXAxisPos);
end;

procedure TMainForm.RangeFromEditExit(Sender: TObject);
begin
  RewriteYAxis(CurrYAxisPos, RangeTo, RangeFrom, CurrXAxisPos);
end;

procedure TMainForm.ShowGraphButtonClick(Sender: TObject);
  Var
    CurrX,  ScaleX, ScaleY: Real;
    I: Integer;
    CurrExpr: String;
begin
      Inc(GraphNumber);
      SetEditEnabled(False);
      SetButtonEnabled(True);
      CurrExpr := TConverter.ConvertToPolishNotation(InputEdit.Text);
      PolNotExprs[GraphNumber] := CurrExpr;
      CurrX := RangeFrom;
      ScaleY := 0.05 * GraphPaintBox.Height;

      for I := 1 to IterationCount do
        Begin
          DotArrays[GraphNumber][I] := ScaleY * (-TCalculate.Calculate(PolNotExprs[GraphNumber], CurrX));
          CurrX := CurrX + Range;
        End;

      GraphPicture.Canvas.Pen.Color := ColorBox.Selected;
      ColorsArray[GraphNumber] := ColorBox.Selected;

      SetSelectedWidth();

      PaintGraph(DotArrays[GraphNumber], Step, XOffset, YOffset);

      if (GraphNumber = 3) then
        Begin
          MathInputPanel.Enabled := False;
          InputEdit.Enabled := False;
          ColorBox.Enabled := False;
          PenWidthComboBox.Enabled := False;
          ShowGraphButton.Enabled := False;
        End;
end;

procedure TMainForm.ClearAllButtonClick(Sender: TObject);
begin
  GraphNumber := 0;
  GraphPicture.Canvas.Pen.Color := clWhite;
  GraphPicture.Canvas.Rectangle(0,0,GraphPaintBox.Width,GraphPaintBox.Height);

  CurrXAxisPos := GraphPaintBox.Height div 2;
  CurrYAxisPos := GraphPaintBox.Width div 2;
  GraphPicture.Canvas.Pen.Color := clBlack;
  PaintYAxis(CurrXAxisPos);
  PaintXAxis(CurrYAxisPos);
  GraphPaintBox.Invalidate;
  RangeFrom := -10;
  RangeTo := 10;
  RangeFromEdit.Text := IntToStr(RangeFrom);
  RangeToEdit.Text := IntToStr(RangeTo);
  SetEditEnabled(True);
  SetButtonEnabled(False);
  InputEdit.Enabled := True;
  InputEdit.Clear;
  ColorBox.Enabled := True;
  PenWidthComboBox.Enabled := True;
  MathInputPanel.Enabled := True;
  PenWidthComboBox.ItemIndex := 1;
  ShowGraphButton.Enabled := True;
end;

procedure TMainForm.ClearGraphButtonClick(Sender: TObject);
var
  I: Integer;
begin
      Dec(GraphNumber);
      if (GraphNumber = 0) then
        ClearGraphButton.Enabled := False;
      with GraphPicture.Canvas do
        Begin
          Pen.Color := clWhite;
          Rectangle(0,0,GraphPaintBox.Width,GraphPaintBox.Height);
          Pen.Width := 3;
          Pen.Color := clBlack;
        End;

      PaintYAxis(CurrYAxisPos);
      PaintXAxis(CurrXAxisPos);
      for I := 1 to GraphNumber do
        Begin
          GraphPicture.Canvas.Pen.Color := ColorsArray[I];
          GraphPicture.Canvas.Pen.Width := WidthArray[I];
          PaintGraph(DotArrays[I], Step, XOffset, YOffset);
        End;
      GraphPaintBox.Invalidate;
      InputEdit.Enabled := True;
      ColorBox.Enabled := True;
      PenWidthComboBox.Enabled := True;
      MathInputPanel.Enabled := True;
      ShowGraphButton.Enabled := True;

end;

procedure TMainForm.SinButtonClick(Sender: TObject);
var
  CurrInput: String;
  CurrCursorPos: Integer;
begin
  CurrInput := InputEdit.Text;
  CurrCursorPos := InputEdit.SelStart;
  Insert('sin()', CurrInput, CurrCursorPos + 1);
  InputEdit.Text := CurrInput;
  InputEdit.SetFocus;
  InputEdit.SelStart := CurrCursorPos + 4;
end;

procedure TMainForm.CosButtonClick(Sender: TObject);
var
  CurrInput: String;
  CurrCursorPos: Integer;
begin
  CurrInput := InputEdit.Text;
  CurrCursorPos := InputEdit.SelStart;
  Insert('cos()', CurrInput, CurrCursorPos + 1);
  InputEdit.Text := CurrInput;
  InputEdit.SetFocus;
  InputEdit.SelStart := CurrCursorPos + 4;
end;

procedure TMainForm.TgButtonClick(Sender: TObject);
var
  CurrInput: String;
  CurrCursorPos: Integer;
begin
  CurrInput := InputEdit.Text;
  CurrCursorPos := InputEdit.SelStart;
  Insert('tg()', CurrInput, CurrCursorPos + 1);
  InputEdit.Text := CurrInput;
  InputEdit.SetFocus;
  InputEdit.SelStart := CurrCursorPos + 3;
end;

procedure TMainForm.CtgButtonClick(Sender: TObject);
var
  CurrInput: String;
  CurrCursorPos: Integer;
begin
  CurrInput := InputEdit.Text;
  CurrCursorPos := InputEdit.SelStart;
  Insert('ctg()', CurrInput, CurrCursorPos + 1);
  InputEdit.Text := CurrInput;
  InputEdit.SetFocus;
  InputEdit.SelStart := CurrCursorPos + 4;
end;

procedure TMainForm.ASinButtonClick(Sender: TObject);
var
  CurrInput: String;
  CurrCursorPos: Integer;
begin
  CurrInput := InputEdit.Text;
  CurrCursorPos := InputEdit.SelStart;
  Insert('arcsin()', CurrInput, CurrCursorPos + 1);
  InputEdit.Text := CurrInput;
  InputEdit.SetFocus;
  InputEdit.SelStart := CurrCursorPos + 7;
end;

procedure TMainForm.ACosButtonClick(Sender: TObject);
var
  CurrInput: String;
  CurrCursorPos: Integer;
begin
  CurrInput := InputEdit.Text;
  CurrCursorPos := InputEdit.SelStart;
  Insert('arccos()', CurrInput, CurrCursorPos + 1);
  InputEdit.Text := CurrInput;
  InputEdit.SetFocus;
  InputEdit.SelStart := CurrCursorPos + 7;
end;

procedure TMainForm.ATgButtonClick(Sender: TObject);
var
  CurrInput: String;
  CurrCursorPos: Integer;
begin
  CurrInput := InputEdit.Text;
  CurrCursorPos := InputEdit.SelStart;
  Insert('arctg()', CurrInput, CurrCursorPos + 1);
  InputEdit.Text := CurrInput;
  InputEdit.SetFocus;
  InputEdit.SelStart := CurrCursorPos + 6;
end;

procedure TMainForm.ACtgButtonClick(Sender: TObject);
var
  CurrInput: String;
  CurrCursorPos: Integer;
begin
  CurrInput := InputEdit.Text;
  CurrCursorPos := InputEdit.SelStart;
  Insert('arcctg()', CurrInput, CurrCursorPos + 1);
  InputEdit.Text := CurrInput;
  InputEdit.SetFocus;
  InputEdit.SelStart := CurrCursorPos + 7;
end;

procedure TMainForm.SqrtButtonClick(Sender: TObject);
var
  CurrInput: String;
  CurrCursorPos: Integer;
begin
  CurrInput := InputEdit.Text;
  CurrCursorPos := InputEdit.SelStart;
  Insert('sqrt()', CurrInput, CurrCursorPos + 1);
  InputEdit.Text := CurrInput;
  InputEdit.SetFocus;
  InputEdit.SelStart := CurrCursorPos + 5;
end;

procedure TMainForm.LogButtonClick(Sender: TObject);
var
  CurrInput: String;
  CurrCursorPos: Integer;
begin
  CurrInput := InputEdit.Text;
  CurrCursorPos := InputEdit.SelStart;
  Insert('log10()', CurrInput, CurrCursorPos + 1);
  InputEdit.Text := CurrInput;
  InputEdit.SetFocus;
  InputEdit.SelStart := CurrCursorPos + 6;
end;

procedure TMainForm.LnButtonClick(Sender: TObject);
var
  CurrInput: String;
  CurrCursorPos: Integer;
begin
  CurrInput := InputEdit.Text;
  CurrCursorPos := InputEdit.SelStart;
  Insert('ln()', CurrInput, CurrCursorPos + 1);
  InputEdit.Text := CurrInput;
  InputEdit.SetFocus;
  InputEdit.SelStart := CurrCursorPos + 3;
end;

procedure TMainForm.AbsButtonClick(Sender: TObject);
var
  CurrInput: String;
  CurrCursorPos: Integer;
begin
  CurrInput := InputEdit.Text;
  CurrCursorPos := InputEdit.SelStart;
  Insert('abs()', CurrInput, CurrCursorPos + 1);
  InputEdit.Text := CurrInput;
  InputEdit.SetFocus;
  InputEdit.SelStart := CurrCursorPos + 4;
end;
end.
