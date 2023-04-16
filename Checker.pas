unit Checker;

interface

uses System.SysUtils;

  Type
    TChecker = class
      class Function IsMathExprValid(Const Expr: String): Boolean;
    end;

implementation

  class Function TChecker.IsMathExprValid(Const Expr: String): Boolean;

  Function IsCharValid(Const Symbol: Char): Boolean;
    Begin
      Result := CharInSet(Symbol, ['0'..'9', '+', '-', '*', '/', '^', '(', '.',
                                   ')', 's', 'c', 't', 'l', 'x', ' ', 'a', 'p',
                                   'g', 'q', 'r']);
    End;

  Function IsNumber(Const Item: Char): Boolean;
    Begin
      Result := CharInSet(Item, ['0'..'9', '.']);
    End;

  Function IsSign(Const Item: Char): Boolean;
    Begin
      Result := CharInSet(Item, ['+', '-', '*', '/', '^']);
    End;

  Function CheckNumber(Var Index: Integer): Boolean;
    Var
      WasDot: Boolean;
    Begin
      Result := True;
      if (Expr[Index] = '.') or ((Expr[Index] = '0') and (Index < Length(Expr)) and (CharInSet(Expr[Index + 1], ['0'..'9']))) then
        Begin
          Result := False;
        End
      else
        Begin
          WasDot := False;
          while (Index <= Length(Expr)) and (IsNumber(Expr[Index])) do
            Begin
              if (Expr[Index] = '.') then
                Begin
                  if (WasDot) or (Index = Length(Expr)) or (not CharInSet(Expr[Index + 1], ['0'..'9'])) then
                    Begin
                      Result := False;
                    End
                  else
                    Begin
                      WasDot := True;
                    End;
                End;
              Inc(Index);
            End;
        End;
      Dec(Index);
    End;

  Function CheckParentheses(): Boolean;
    Var
      I, ParenthesisCounter: Integer;
      CurrSymbol: Char;
    Begin
      ParenthesisCounter := 0;
      I := 1;
      Result := True;
      while (I <= Length(Expr)) and (Result) do
        Begin
          CurrSymbol := Expr[I];
          if (CurrSymbol = '(') then
            Begin
              Inc(ParenthesisCounter);
            End
          else if (CurrSymbol = ')') then
            Begin
              if (ParenthesisCounter = 0) then
                Begin
                  Result := False;
                End
              else
                Begin
                  Dec(ParenthesisCounter);
                End;
            End;
          Inc(I);
        End;
      Result := Result and (ParenthesisCounter = 0);
    End;

  Var
    I, Len, TempIndex: Integer;
    CurrSymbol: Char;
    Temp: String;
  Begin
    I := 1;
    Len := Length(Expr);
    Result := CheckParentheses();
    while (I <= Len) and (Result) do
      Begin
        CurrSymbol := Expr[I];
          Begin
            if (IsCharValid(CurrSymbol)) then
              Begin
                if (IsSign(CurrSymbol)) then
                  Begin
                    Result := Result and ((I > 1) or (Length(Expr) > 1));
                    TempIndex := I;
                    Inc(I);
                    while (I < Length(Expr)) and (Expr[I] = ' ') do
                      Begin
                        Inc(I);
                      End;
                    Result := Result and (IsNumber(Expr[I]) or (Expr[I] = '(') or (Expr[I] = 'x') or (CharInSet(Expr[I], ['s', 'c', 't', 'l', 'a', 'p'])));
                    while (TempIndex > 1) and (Expr[TempIndex] = ' ') do
                      Begin
                        Dec(TempIndex);
                      End;
                    Result := Result and (IsNumber(Expr[I]) or (Expr[I] = 'x') or (Expr[I] = '(') or (CharInSet(Expr[I], ['s', 'c', 't', 'l', 'a', 'p'])));
                    Dec(I);
                  End
                else if IsNumber(CurrSymbol) then
                  Begin
                    Result := Result and CheckNumber(I);
                  End
                else if (CurrSymbol = 's') then
                  Begin
                    if (I < Len - 4) then
                      Begin
                        Temp := Copy(Expr, I, 4);
                        Result := (Temp = 'sin(') and (Expr[I + 4] <> ')');
                        if (I < Len - 5) and (not Result) then
                          Begin
                            Temp := Copy(Expr, I, 5);
                            Result := (Temp = 'sqrt(') and (Expr[I + 5] <> ')');
                            Inc(I, 4);
                          End
                        else
                          Inc(I, 3);
                      End
                    else
                      Result := False;
                  End
                else if (CurrSymbol = 'c') then
                  Begin
                    if (I < Len - 4) then
                      Begin
                        Temp := Copy(Expr, I, 4);
                        Result := ((Temp = 'cos(') or (Temp = 'ctg(')) and (Expr[I + 4] <> ')');
                        Inc(I, 3);
                      End
                    else
                      Result := False;
                  End
                else if (CurrSymbol = 'l') then
                  Begin
                    if (I < Len - 3) then
                      Begin
                        Temp := Copy(Expr, I, 3);
                        Result := (Temp = 'ln(') and (Expr[I + 3] <> ')');
                        if (I < Len - 6) and (not Result) then
                          Begin
                            Temp := Copy(Expr, I, 6);
                            Result := (Temp = 'log10(') and (Expr[I + 6] <> ')');
                            Inc(I, 5);
                          End
                        else
                          Inc(I, 2);
                      End
                    else
                      Result := False;
                  End
                else if (CurrSymbol = 't') then
                  Begin
                    if (I < Len - 3) then
                      Begin
                        Temp := Copy(Expr, I, 3);
                        Result := (Temp = 'tg(') and (Expr[I + 3] <> ')');
                        Inc(I, 2);
                      End
                    else
                      Result := False;
                  End
                else if (CurrSymbol = 'a') then
                  Begin
                    if (I < Len - 4) then
                      Begin
                        Temp := Copy(Expr, I, 3);
                        Result := (Temp = 'arc') and (CharInSet(Expr[I + 3], ['s', 'c', 't']));
                        if (not Result) then
                          Result := (Temp = 'abs') and (Expr[I + 3] = '(') and (Expr[I + 4] <> ')');
                        Inc(I, 2);
                      End
                    else
                      Result := False;
                  End
                else if (CurrSymbol = 'p') then
                  Begin
                    if (I < Len) then
                      Begin
                        Result := (Copy(Expr, I, 2) = 'pi');
                        Inc(I, 1);
                      End
                    else
                      Result := False;
                  End;
              End
            else
              Begin
                Result := False;
              End;
          End;
        Inc(I);
      End;
  End;
end.
