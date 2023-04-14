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
    procedure InputEditChange(Sender: TObject);
    procedure GraphPaintBoxPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RangeFromEditChange(Sender: TObject);
    procedure RangeToEditChange(Sender: TObject);
    procedure ShowGraphButtonClick(Sender: TObject);

  private
    { Private declarations }
  public
    GraphPicture: TBitmap;
    DotArray: array of Extended;
    RangeFrom, RangeTo: Integer;
    PolNotExpr: String;
  end;

const
  IterationCount = 10000;
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

procedure TForm1.FormCreate(Sender: TObject);
  begin
    GraphPicture := TBitmap.Create;
    GraphPicture.SetSize(GraphPaintBox.Width, GraphPaintBox.Height);
    GraphPicture.Canvas.MoveTo(GraphPaintBox.Width div 2, 0);
    GraphPicture.Canvas.LineTo(GraphPaintBox.Width div 2, GraphPaintBox.Height);
    GraphPicture.Canvas.MoveTo(0, GraphPaintBox.Height div 2);
    GraphPicture.Canvas.LineTo(GraphPaintBox.Width, GraphPaintBox.Height div 2);
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


procedure TForm1.RangeFromEditChange(Sender: TObject);
begin
  if (not CheckInput(RangeFromEdit.Text)) then
    Begin
      // покраснение рамки edit и блокировка кнопки
    End
  else
    RangeFrom := StrToInt(RangeFromEdit.Text);
end;

procedure TForm1.RangeToEditChange(Sender: TObject);
begin
  if (not CheckInput(RangeToEdit.Text)) then
    Begin
      // покраснение рамки edit и блокировка кнопки
    End
  else
    RangeTo :=  StrToInt(RangeToEdit.Text);
end;

procedure TForm1.ShowGraphButtonClick(Sender: TObject);
  Var
    CurrX, CurrY, Step, XOffset, YOffset, MaxY, MinY, ScaleX, ScaleY: Real;
    DotNumber, I: Integer;
    WasNaN: Boolean;
begin
  RangeFrom := -10;
  RangeTo := 10;
  PolNotExpr := TConverter.ConvertToPolishNotation(InputEdit.Text);
  Step := (RangeTo - RangeFrom) / IterationCount;
  DotNumber := IterationCount;
  SetLength(DotArray, DotNumber);
  CurrX := RangeFrom;
  MaxY := Math.NegInfinity;
  MinY := Math.Infinity;

  for I := 0 to DotNumber - 1 do
    Begin
        DotArray[I] := (-TCalculate.Calculate(PolNotExpr, CurrX));
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
//  CurrY := Trunc(DotArray[0] * 100);
  XOffset := GraphPaintBox.Width / DotNumber;
  YOffset := GraphPaintBox.Height / 2;
//  GraphPicture.Canvas.MoveTo(Trunc(CurrX), Trunc(DotArray[0] * 100+ YOffset));
//  for I := 1 to DotNumber - 1 do
//    Begin
//      CurrX := CurrX + XOffset;
//      GraphPicture.Canvas.LineTo(Trunc(CurrX), Trunc(DotArray[I] * 100 + YOffset));
//    End;
  WasNaN := False;
  ScaleX :=  GraphPaintBox.Width / (RangeTo - RangeFrom);
  ScaleY :=  GraphPaintBox.Height / (MaxY - MinY);
  GraphPicture.Canvas.MoveTo(Trunc(CurrX), Trunc(DotArray[0] * ScaleY + YOffset));
  for I := 1 to DotNumber - 1 do
    Begin
      if (FloatToStr(DotArray[I]) = 'NAN') then
        WasNaN := True
      else if (WasNan) then
        Begin
          GraphPicture.Canvas.MoveTo(Trunc(CurrX), Trunc(DotArray[I] * ScaleY + YOffset));
          WasNan := False;
        End
      else
        GraphPicture.Canvas.LineTo(Trunc(CurrX), Trunc(DotArray[I] * ScaleY + YOffset));

      CurrX := CurrX + XOffset;
    End;

  GraphPaintBox.Invalidate;
end;

end.
