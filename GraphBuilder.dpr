program GraphBuilder;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {Form1},
  Converter in 'Converter.pas',
  Checker in 'Checker.pas',
  Stack in 'Stack.pas',
  Calculate in 'Calculate.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
