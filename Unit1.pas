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
    procedure InputEditChange(Sender: TObject);
    procedure GraphPaintBoxPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private
    { Private declarations }
  public
    GraphPicture: TBitmap;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

Procedure DeleteSpaces(var Expr: String);
  Begin
    Expr := StringReplace(Expr, ' ', '', [rfReplaceAll]);
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

end.
