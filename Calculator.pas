unit Calculator;


interface

uses System.SysUtils, TStack, Math;

  Function Calculate(const Expr: String; X: Extended): Extended;

implementation

  Function Calculate(const Expr: String; X: Extended): Extended;

  Function FindOperandReversed(Const Expr: String; var Index: Integer): Extended;
  Var
    I: Integer;
  Begin
    I := Index;
    while (I > 0) and (Expr[I] >= '0') and (Expr[I] <= '9') or (Expr[I] = '.') do
      Begin
        Dec(I);
      End;
    Result := StrToFloat(Copy(Expr, I + 1, Index - I));
    Index := I + 1;
  End;

  Function IsFunction(Item: Char): Boolean;
    Begin
      Result := CharInSet(Item, ['!', '@', '#', '$', '%', '&', '_', '{', '}', '"', '?', '|']);
    End;

  Function IsOperand(Item: Char): Boolean;
    Begin
      Result := ((Item >= '0') and (Item <= '9')) or (Item = 'x');
    End;

  Const HalfPi: Extended = Pi/2;

  Var
    OperandStack: TStack<Real>;
    Index: Integer;
    CurrSymbol: Char;
    Operand1, Operand2, TempValue: Extended;
  Begin
    OperandStack := TStack<Real>.Create(Length(Expr));
    Index := Length(Expr);
    while (Index > 0) do
      Begin
          CurrSymbol := Expr[Index];
          if (CurrSymbol <> ' ') then
          Begin
            if (IsOperand(Expr[Index])) or (Expr[Index] = '~') or (Expr[Index] = 'e') then
              Begin
                if (CurrSymbol = 'x') then
                  Begin
                    Operand1 := X;
                  End
                else if (CurrSymbol = '~') then
                  Begin
                    Operand1 := Pi;
                  End
                else if (CurrSymbol = 'e') then
                  Begin
                    Operand1 := Exp(1);
                  end
                else
                  Begin
                    Operand1 := FindOperandReversed(Expr, Index);
                  End;
                OperandStack.Push(Operand1);
              End
            else if (IsFunction(CurrSymbol)) then
              Begin
                  Operand1 := OperandStack.Pop();
                  case CurrSymbol of
                    '!':
                      OperandStack.Push(System.Sin(Operand1));
                    '@':
                      OperandStack.Push(System.Cos(Operand1));
                    '#':
                      Begin
                        TempValue := Operand1 / Pi;
                        if (Abs(Frac(TempValue)) > 0.49) and (Abs(Frac(TempValue)) < 0.51) then
                          OperandStack.Push(Math.NaN)
                        else
                          OperandStack.Push(System.Sin(Operand1) / System.Cos(Operand1));
                      End;
                    '$':
                      Begin
                        TempValue := Operand1 / Pi;
                        if (Abs(Frac(TempValue)) > 0.99) or (Abs(Frac(TempValue)) < 0.01) then
                            OperandStack.Push(Math.NaN)
                        else
                          OperandStack.Push(System.Cos(Operand1) / System.Sin(Operand1));
                      End;
                    '%':
                      if (Operand1 < 0) then
                        OperandStack.Push(System.Math.NaN)
                      else
                        OperandStack.Push(System.Ln(Operand1));
                    '?':
                      if (Operand1 < 0) then
                        OperandStack.Push(System.Math.NaN)
                      else
                        OperandStack.Push(System.Math.Log10(Operand1));
                    '"':
                      if (Operand1 < 0) then
                        OperandStack.Push(System.Math.NaN)
                      else
                        OperandStack.Push(System.Sqrt(Operand1));
                    '|':
                      OperandStack.Push(System.Math.RoundTo(System.Abs(Operand1), -3));
                    '&':
                      if (Operand1 < -1) or (Operand1 > 1) then
                        OperandStack.Push(Math.NaN)
                      else
                        OperandStack.Push(System.Math.ArcSin(Operand1));
                    '_':
                      if (Operand1 < -1) or (Operand1 > 1) then
                        OperandStack.Push(Math.NaN)
                      else
                        OperandStack.Push(System.Math.ArcCos(Operand1));
                    '{':
                      OperandStack.Push(System.ArcTan(Operand1));
                    '}':
                      OperandStack.Push(Pi/2 - System.ArcTan(Operand1));
                  End;
              End
            else
              Begin
                  Operand1 := OperandStack.Pop();
                  Operand2 := OperandStack.Pop();
                  case CurrSymbol of
                    '+':
                      OperandStack.Push(Operand1 + Operand2);
                    '-':
                      OperandStack.Push(Operand1 - Operand2);
                    '*':
                      OperandStack.Push(Operand1 * Operand2);
                    '/':
                      if (Operand2 > -0.1) and (Operand2 < 0.1) then
                        OperandStack.Push(Math.NaN)
                      else
                        OperandStack.Push(Operand1 / Operand2);
                    '^':
                      OperandStack.Push(Math.Power(Operand1, Operand2));
                  End;
              End;
          End;
        Dec(Index);
      End;

     Result := OperandStack.Peek();
  End;
end.
