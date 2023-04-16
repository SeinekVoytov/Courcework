unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Math, Checker, Calculate, Converter;

type
  TForm1 = class(TForm)
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

  private
    CurrXAxisPos, CurrYAxisPos: Integer;
    DotArray: array of Extended;
    RangeFrom, RangeTo: Integer;
    PolNotExpr: String;
    IsGrappBuilt: Boolean;
  public
    GraphPicture: TBitmap;
  end;

const
  IterationCount = 10000;
  ColorNames: array[0..6] of String = ('Черный', 'Красный', 'Зеленый', 'Синий', 'Желтый', 'Оранжевый', 'Розовый');
  ColorValues: array[0..6] of string = ('$000000', '$0000FF', '$00FF00', '$FF0000', '$00FFFF', '$00A5FF', '$FF00FF');
var
  Form1: TForm1;

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
begin
  with Form1.GraphPicture.Canvas do
  begin
    MoveTo(X, 0);
    LineTo(X, Form1.GraphPaintBox.Height);
  end;
end;

Procedure PaintXAxis(const Y: Integer);
begin
  with Form1.GraphPicture.Canvas do
    begin
      MoveTo(0, Y);
      LineTo(Form1.GraphPaintBox.Width, Y);
    end;
end;

Procedure RewriteYAxis(var CurrYAxisPos: Integer;
                       const RangeTo, RangeFrom, CurrXAxisPos: Integer
                       );
var
  Temp: Integer;
begin
  Temp := Form1.GraphPicture.Canvas.Pen.Width;
  with Form1.GraphPicture.Canvas do
        Begin
          Pen.Color := clWhite;
          Pen.Width := 3;
          PaintYAxis(CurrYAxisPos);

          CurrYAxisPos := Trunc(Abs(RangeFrom) * Form1.GraphPaintBox.Width / (RangeTo - RangeFrom));

          MoveTo(CurrYAxisPos, 0);
          Pen.Color := clBlack;
          LineTo(CurrYAxisPos, Form1.GraphPaintBox.Height);
          PaintXAxis(CurrXAxisPos);
          Pen.Width := Temp;
        End;

  Form1.GraphPaintBox.Invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
  var
    I: Integer;
  begin
    RangeFrom := -10;
    RangeTo := 10;
    CurrXAxisPos := GraphPaintBox.Height div 2;
    CurrYAxisPos := GraphPaintBox.Width div 2;
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

procedure TForm1.GraphPaintBoxPaint(Sender: TObject);
  begin

    //GraphPaintBox.Invalidate;
    GraphPaintBox.Canvas.Draw(0, 0, GraphPicture);
  end;

procedure TForm1.InputEditChange(Sender: TObject);
Var
    Text: String;
begin
  Text := Lowercase(InputEdit.Text);
  if (not TChecker.IsMathExprValid(Text)) then
    ShowGraphButton.Enabled := False
  else
    ShowGraphButton.Enabled := True;
end;

procedure TForm1.InputEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (ShowGraphButton.Enabled) then
    ShowGraphButtonClick(Sender);
end;

procedure TForm1.MathInputButtonClick(Sender: TObject);
Var
  Temp: Integer;
begin
  Temp := MathInputPanel.Top;
  MathInputPanel.Top := RangeAndBuildPanel.Top;
  RangeAndBuildPanel.Top := Temp;
  MathInputPanel.Visible := True;
end;

procedure TForm1.RangeFromEditChange(Sender: TObject);
begin
  if (not CheckInput(RangeFromEdit.Text) or (RangeTo <= RangeFrom)) then
    Begin
      // покраснение рамки edit и блокировка кнопки
    End
  else
    RangeFrom := StrToInt(RangeFromEdit.Text);
end;

procedure TForm1.RangeToEditChange(Sender: TObject);
begin
  if (not CheckInput(RangeToEdit.Text) or (RangeTo <= RangeFrom)) then
    Begin
      // покраснение рамки edit и блокировка кнопки
    End
  else
    RangeTo :=  StrToInt(RangeToEdit.Text);
end;

procedure TForm1.RangeToEditExit(Sender: TObject);
begin
  RewriteYAxis(CurrYAxisPos, RangeTo, RangeFrom, CurrXAxisPos);
end;

procedure TForm1.RangeFromEditExit(Sender: TObject);
begin
  RewriteYAxis(CurrYAxisPos, RangeTo, RangeFrom, CurrXAxisPos);
end;

procedure TForm1.ShowGraphButtonClick(Sender: TObject);
  Var
    CurrX, CurrY, Step, XOffset, YOffset, MaxY, MinY, ScaleX, ScaleY: Real;
    DotNumber, I: Integer;
    WasNaN: Boolean;
    SelectedValue: String;
begin
//  RangeFrom := -10;
//  RangeTo := 10;
  PolNotExpr := TConverter.ConvertToPolishNotation(InputEdit.Text);
  Step := (RangeTo - RangeFrom) / IterationCount;
  DotNumber := IterationCount;
  SetLength(DotArray, DotNumber);
  CurrX := RangeFrom;
  MaxY := Math.NegInfinity;
  MinY := Math.Infinity;
  ScaleY := GraphPaintBox.Height / IterationCount;


  for I := 0 to DotNumber - 1 do
    Begin
        DotArray[I] := ScaleY * (-TCalculate.Calculate(PolNotExpr, CurrX));
        if (FloatToStr(DotArray[I]) <> 'NAN') then
          Begin
            if (DotArray[I] > MaxY) then
              MaxY := DotArray[I];

            if (DotArray[I] < MinY) then
              MinY := DotArray[I];
          End;
      CurrX := CurrX + Step;
    End;

  CurrX := 0;
  XOffset := GraphPaintBox.Width / DotNumber;
  YOffset := GraphPaintBox.Height / 2;
  //YOffset := (MaxY - MinY) * GraphPaintBox.Height / 4;
  WasNaN := True;
//  ScaleX :=  GraphPaintBox.Width / (RangeTo - RangeFrom);
//  ScaleY :=  GraphPaintBox.Height / (MaxY - MinY);
  GraphPicture.Canvas.MoveTo(Trunc(CurrX), Trunc(DotArray[0] * 10 + YOffset));
//  for I := 1 to DotNumber - 1 do
//    Begin
//      if (FloatToStr(DotArray[I]) = 'NAN') then
//        WasNaN := True
//      else if (WasNan) then
//        Begin
//          GraphPicture.Canvas.MoveTo(Trunc(CurrX), Trunc(DotArray[I] * ScaleY + YOffset));
//          WasNan := False;
//        End
//      else
//        GraphPicture.Canvas.LineTo(Trunc(CurrX), Trunc(DotArray[I] * ScaleY + YOffset));
//
//      CurrX := CurrX + XOffset;
//    End;
  GraphPicture.Canvas.Pen.Color := ColorBox.Selected;

  SelectedValue := PenWidthComboBox.Items[PenWidthComboBox.ItemIndex];
  if (SelectedValue = 'mid') then
    GraphPicture.Canvas.Pen.Width := 3
  else if (SelectedValue = 'low') then
    GraphPicture.Canvas.Pen.Width := 1
  else
    GraphPicture.Canvas.Pen.Width := 5;

  for I := 1 to DotNumber - 1 do
    Begin
      if (FloatToStr(DotArray[I]) = 'NAN') then
        WasNaN := True
      else if (WasNan) then
        Begin
          GraphPicture.Canvas.MoveTo(Trunc(CurrX), Trunc(DotArray[I] * 500 + YOffset));
          WasNan := False;
        End
      else
        GraphPicture.Canvas.LineTo(Trunc(CurrX), Trunc(DotArray[I] * 500 + YOffset));

      CurrX := CurrX + XOffset;
    End;

  GraphPaintBox.Invalidate;
end;

procedure TForm1.SinButtonClick(Sender: TObject);
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

procedure TForm1.CosButtonClick(Sender: TObject);
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

procedure TForm1.TgButtonClick(Sender: TObject);
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

procedure TForm1.CtgButtonClick(Sender: TObject);
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

procedure TForm1.ASinButtonClick(Sender: TObject);
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

procedure TForm1.ACosButtonClick(Sender: TObject);
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

procedure TForm1.ATgButtonClick(Sender: TObject);
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

procedure TForm1.ACtgButtonClick(Sender: TObject);
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

procedure TForm1.SqrtButtonClick(Sender: TObject);
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

procedure TForm1.LogButtonClick(Sender: TObject);
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

procedure TForm1.LnButtonClick(Sender: TObject);
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

procedure TForm1.AbsButtonClick(Sender: TObject);
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
