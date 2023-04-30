unit ExtremaChecker;

interface
  Function IsExtrema(Y, YPlusDelta, Delta, Accuracy: Real): Boolean;

implementation
  Function IsExtrema(Y, YPlusDelta, Delta, Accuracy: Real): Boolean;
  var
    Determinant: Real;
  Begin
    Determinant := (YPlusDelta - Y) / Delta;
    Result := Abs(Determinant) < Accuracy;
  End;
end.
