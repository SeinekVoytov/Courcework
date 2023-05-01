program GraphBuilder;

uses
  Vcl.Forms,
  Main in 'Main.pas' {MainForm},
  Converter in 'Converter.pas',
  Checker in 'Checker.pas',
  TStack in 'TStack.pas',
  Calculator in 'Calculator.pas',
  ExtremaFinder in 'ExtremaFinder.pas',
  List in 'List.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
