unit Calculate;


interface

uses System.SysUtils, Stack, Math;

  Type
    TCalculate = class
      class Function Calculate(const Expr: String; X: Extended): Extended;
    end;

implementation

  class Function TCalculate.Calculate(const Expr: String; X: Extended): Extended;

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
      Result := CharInSet(Item, ['!', '@', '#', '$', '%', '&', '_', '{', '}']);
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
            if (IsOperand(Expr[Index])) or (Expr[Index] = '~') then
              Begin
                if (CurrSymbol = 'x') then
                  Begin
                    Operand1 := X;
                  End
                else if (CurrSymbol = '~') then
                  Begin
                    Operand1 := Pi;
                  End
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
                      OperandStack.Push(System.Math.RoundTo(System.Sin(Operand1), -3));
                    '@':
                      OperandStack.Push(System.Math.RoundTo(System.Cos(Operand1), -3));
                    '#':
                      Begin
                        TempValue := Operand1 / Pi;
                        if (Abs(Frac(TempValue)) > 0.4) and (Abs(Frac(TempValue)) < 0.6) then
                          OperandStack.Push(Math.NaN)
                        else
                          OperandStack.Push(System.Math.RoundTo(System.Sin(Operand1) / System.Cos(Operand1), -3));
                      End;
                    '$':
                      Begin
                        TempValue := Operand1 / Pi;
                        if (Abs(Frac(TempValue)) > 0.9) or (Abs(Frac(TempValue)) < 0.1) then
                            OperandStack.Push(Math.NaN)
                        else
                          OperandStack.Push(System.Math.RoundTo(System.Cos(Operand1) / System.Sin(Operand1), -3));
                      End;
                    '%':
                      OperandStack.Push(System.Math.RoundTo(System.Ln(Operand1), -3));
                    '&':
                      if (Operand1 < -1) or (Operand1 > 1) then
                        OperandStack.Push(Math.NaN)
                      else
                        OperandStack.Push(System.Math.RoundTo(System.Math.ArcSin(Operand1), -3));
                    '_':
                      if (Operand1 < -1) or (Operand1 > 1) then
                        OperandStack.Push(Math.NaN)
                      else
                        OperandStack.Push(System.Math.RoundTo(System.Math.ArcCos(Operand1), -3));
                    '{':
                      OperandStack.Push(System.Math.RoundTo(System.ArcTan(Operand1), -3));
                    '}':
                      OperandStack.Push(System.Math.RoundTo(Pi/2 - System.ArcTan(Operand1), -3));
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