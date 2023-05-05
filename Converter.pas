unit Converter;

interface
  uses TStack, System.SysUtils;

 Function ConvertToPolishNotation(Expr: String): String;

implementation
  Function ConvertToPolishNotation(Expr: String): String;

  Type
    TSign = (Uno_Minus, Uno_Plus, Sin, Cos, Tg, Ctg, ArcSin, ArcCos, ArcTg, ArcCtg, Ln, Log10, Abs, Sqrt, Caret, Minus, Plus, Star, Slash, Open, Close);

  Type
    TPriority = array [TSign] of Integer;

  Function IsFunction(Item: Char): Boolean;
    Begin
      Result := CharInSet(Item, ['!', '@', '#', '$', '%', '&', '?', '_', '{', '}', '"', '|']);
    End;

  Function IsOperand(Item: Char): Boolean;
    Begin
      Result := ((Item >= '0') and (Item <= '9')) or (Item = 'x');
    End;

  Function IsSign(Item: Char): Boolean;
    Begin
      Result := CharInSet(Item, ['+', '-', '*', '/', '^', '!', '@', '#', '$', '&', '_', '{', '}', '~', '?', '"', '|']);
    End;

  Procedure ReplaceFunctions(Var Expr: String);
    Begin
      Expr := StringReplace(Expr, 'arcsin', '&', [rfReplaceAll]);
      Expr := StringReplace(Expr, 'arccos', '_', [rfReplaceAll]);
      Expr := StringReplace(Expr, 'arctg', '{', [rfReplaceAll]);
      Expr := StringReplace(Expr, 'arcctg', '}', [rfReplaceAll]);
      Expr := StringReplace(Expr, 'log10', '?', [rfReplaceAll]);
      Expr := StringReplace(Expr, 'sqrt', '"', [rfReplaceAll]);
      Expr := StringReplace(Expr, 'abs', '|', [rfReplaceAll]);
      Expr := StringReplace(Expr, 'ctg', '$', [rfReplaceAll]);
      Expr := StringReplace(Expr, 'sin', '!', [rfReplaceAll]);
      Expr := StringReplace(Expr, 'cos', '@', [rfReplaceAll]);
      Expr := StringReplace(Expr, 'tg', '#', [rfReplaceAll]);
      Expr := StringReplace(Expr, 'ln', '%', [rfReplaceAll]);
    End;

  Procedure InitPriority(Var Priority: TPriority);
    Begin
      Priority[Open] := 0;
      Priority[Close] := 0;
      Priority[Plus] := 1;
      Priority[Minus] := 1;
      Priority[Slash] := 2;
      Priority[Star] := 2;
      Priority[Caret] := 3;
      Priority[ArcSin] := 4;
      Priority[ArcCos] := 4;
      Priority[ArcTg] := 4;
      Priority[ArcCtg] := 4;
      Priority[Sin] := 4;
      Priority[Cos] := 4;
      Priority[Tg] := 4;
      Priority[Ctg] := 4;
      Priority[Ln] := 4;
      Priority[Log10] := 4;
      Priority[Sqrt] := 4;
      Priority[Abs] := 4;
      Priority[Uno_Minus] := 5;
      Priority[Uno_Plus] := 5;
    End;

  Function FindOperand(Const Expr: String; var Index: Integer): String;
    Var
      I: Integer;
    Begin
      I := Index;
      while (I <= Length(Expr)) and (Expr[I] >= '0') and (Expr[I] <= '9') or (Expr[I] = '.') do
        Begin
          Inc(I);
        End;
      Result := Copy(Expr, Index, I - Index);
      Index := I - 1;
    End;

  Procedure DeleteSpaces(var Expr: String);
    Begin
      Expr := StringReplace(Expr, ' ', '', [rfReplaceAll]);
    End;

  Procedure AddStars(var Expr: String; FromIndex, ToIndex: Integer);

    Procedure EvaluateParentheses(var Expr: String; var Index: Integer);
      Begin
        Inc(Index);
        var ParenthesesCounter := 1;
        var StartIndex := Index;

        while (ParenthesesCounter > 0) do
          Begin
            if (Expr[Index] = '(') then
              Inc(ParenthesesCounter)
            else if (Expr[Index] = ')') then
              Dec(ParenthesesCounter);
            Inc(Index);
          End;
        Dec(Index);
        AddStars(Expr, StartIndex, Index - 1);
      End;

    Procedure InsertStar(var Expr: String; var Index, ToIndex: Integer);
      Begin
        Insert('*', Expr, Index);
        Inc(Index);
        Inc(ToIndex);
      End;

    Procedure CheckInsertConditions(var Cond1, Cond2, Cond3, Cond4, Cond5, Cond6: Boolean; var Expr: String; var Index, ToIndex: Integer);
      Begin
        if (Cond1) or (Cond2) or (Cond3) or (Cond4) or (Cond5) or (Cond6) then
          Begin
            InsertStar(Expr, Index, ToIndex);
            Cond1 := False;
            Cond2 := False;
            Cond3 := False;
            Cond4 := False;
            Cond5 := False;
            Cond6 := False;
          End;
      End;
    var
      WasNumber, WasFunction, WasX, WasPi, WasParentheses, WasExp: Boolean;
      CurrSymbol: Char;
      I: Integer;
    Begin
      I := FromIndex;
      WasNumber := False;
      WasX := False;
      WasFunction := False;
      WasPi := False;
      WasParentheses := False;
      WasExp := False;
      while (I <= ToIndex) do
        Begin
          CurrSymbol := Expr[I];
          if (CharInSet(CurrSymbol, ['+', '-', '*', '/', '^'])) then
            Begin
              WasNumber := False;
              WasX := False;
              WasFunction := False;
              WasPi := False;
              WasParentheses := False;
              WasExp := False;
            End
          else if (CharInSet(CurrSymbol, ['0'..'9'])) then
            Begin
              CheckInsertConditions(WasParentheses, WasFunction, WasX, WasX, WasPi, WasExp, Expr, I, ToIndex);
              WasNumber := True;
              Inc(I);

              while (I <= ToIndex) and (CharInSet(Expr[I], ['0'..'9', '.'])) do
                Inc(I);
              Dec(I);
            End
          else if (CurrSymbol = 'p') then
            Begin
              CheckInsertConditions(WasParentheses, WasFunction, WasNumber, WasX, WasPi, WasExp, Expr, I, ToIndex);
              WasPi := True;
              Inc(I);
            End
          else if (IsFunction(CurrSymbol)) then
            Begin
              CheckInsertConditions(WasParentheses, WasFunction, WasNumber, WasX, WasPi, WasExp,  Expr, I, ToIndex);
              WasFunction := True;
            End
          else if (CurrSymbol = 'x') then
            Begin
              CheckInsertConditions(WasParentheses, WasFunction, WasNumber, WasX, WasPi, WasExp,  Expr, I, ToIndex);
              WasX := True;
            End
          else if (CurrSymbol = '(') then
            Begin
              CheckInsertConditions(WasParentheses, WasNumber, WasNumber, WasX, WasPi, WasExp, Expr, I, ToIndex);
              WasFunction := False;
              EvaluateParentheses(Expr, I);
              WasParentheses := True;
            End
          else if (CurrSymbol = 'e') then
            Begin
              CheckInsertConditions(WasParentheses, WasFunction, WasNumber, WasX, WasPi, WasExp,  Expr, I, ToIndex);
              WasExp := True;
            End;
          Inc(I);
        End;
    End;

  Procedure Convert(Var SignStack: TStack<TSign>; Var OperandStack: TStack<String>; Var Sign: TSign);
  Var
    LeftPart, RightPart: String;
  Begin
    Sign := SignStack.Pop();
    if (Sign = Uno_Minus) or (Sign = Uno_Plus) or (Sign = Sin) or
       (Sign = Cos) or (Sign = Tg) or (Sign = Ctg) or (Sign = Ln) or
       (Sign = Sqrt) or (Sign = Log10) or (Sign = Abs) or
       (Sign = ArcSin) or (Sign = ArcCos) or (Sign = ArcTg) or (Sign = ArcCtg) then
      Begin
        LeftPart := OperandStack.Pop();
          case Sign of
            Uno_Minus:
              LeftPart := '-0 ' + LeftPart;
            Sin:
              LeftPart := '!' + LeftPart;
            Cos:
              LeftPart := '@' + LeftPart;
            Tg:
              LeftPart := '#' + LeftPart;
            Ctg:
              LeftPart := '$' + LeftPart;
            Ln:
              LeftPart := '%' + LeftPart;
            ArcSin:
              LeftPart := '&' + LeftPart;
            ArcCos:
              LeftPart := '_' + LeftPart;
            ArcTg:
              LeftPart := '{' + LeftPart;
            ArcCtg:
              LeftPart := '}' + LeftPart;
            Sqrt:
              LeftPart := '"' + LeftPart;
            Log10:
              LeftPart := '?' + LeftPart;
            Abs:
              LeftPart := '|' + LeftPart;
          end;
      End
    else
      Begin
        RightPart := OperandStack.Pop();
        LeftPart := OperandStack.Pop();
          case Sign of
            Caret:
              LeftPart := '^' + LeftPart + ' ' + RightPart;
            Minus:
              LeftPart := '-' + LeftPart + ' ' + RightPart;
            Plus:
              LeftPart := '+' + LeftPart + ' ' + RightPart;
            Star:
              LeftPart := '*' + LeftPart + ' ' + RightPart;
            Slash:
              LeftPart := '/' + LeftPart + ' ' + RightPart;
          end;
      End;

    OperandStack.Push(LeftPart);
  End;

  Var
    OperandStack: TStack<String>;
    SignStack: TStack<TSign>;
    Priority: TPriority;
    Operand: String;
    I, Len: Integer;
    CurrSign, Oper: TSign;
    CurrSymbol: Char;
  Begin
    InitPriority(Priority);
    DeleteSpaces(Expr);
    Expr := Lowercase(Expr);
    ReplaceFunctions(Expr);
    AddStars(Expr, 1, Length(Expr));
    Expr := '(' + Expr + ')';
    Len := Length(Expr);
    OperandStack := TStack<String>.Create(Len);
    SignStack := TStack<TSign>.Create(Len);
    I := 1;
    while (I <= Len) do
      Begin
        CurrSymbol := Expr[I];
        if (IsOperand(CurrSymbol)) or (Expr[I] = 'p') or (Expr[I] = 'e') then
          Begin
            if (CurrSymbol = 'x') then
              Operand := CurrSymbol
            else if (CurrSymbol = 'p') then
              Begin
                Operand := '~';
                Inc(I);
              End
            else if (CurrSymbol = 'e') then
              Operand := 'e'
            else
              Operand := FindOperand(Expr, I);

            OperandStack.Push(Operand);
          End
        else if (IsFunction(CurrSymbol)) then
          Begin
              case CurrSymbol of
                '!':
                  SignStack.Push(Sin);
                '@':
                  SignStack.Push(Cos);
                '#':
                  SignStack.Push(Tg);
                '$':
                  SignStack.Push(Ctg);
                '%':
                  SignStack.Push(Ln);
                '&':
                  SignStack.Push(ArcSin);
                '_':
                  SignStack.Push(ArcCos);
                '{':
                  SignStack.Push(ArcTg);
                '}':
                  SignStack.Push(ArcCtg);
                '"':
                  SignStack.Push(Sqrt);
                '?':
                  SignStack.Push(Log10);
                '|':
                  SignStack.Push(Abs);
              end;
          End
        else
          Begin
              case CurrSymbol of
                '+':
                  Begin
                    if (I = 1) or (Expr[I - 1] = '(') then
                      Begin
                        CurrSign := Uno_Plus;
                      End
                    else
                      Begin
                        CurrSign := Plus;
                      End;
                  End;
                '-':
                  if (CurrSymbol = '-') then
                  Begin
                    if (I = 1) or (Expr[I - 1] = '(') then
                      Begin
                        CurrSign := Uno_Minus;
                      End
                    else
                      Begin
                        CurrSign := Minus;
                      End;
                  End;
                '^':
                  CurrSign := Caret;
                '*':
                  CurrSign := Star;
                '/':
                  CurrSign := Slash;
                '(':
                  CurrSign := Open;
                ')':
                  CurrSign := Close;
              end;

            if (CurrSign = Uno_Minus) or (CurrSign = Uno_Plus) then
              Begin
                SignStack.Push(CurrSign);
              End
            else if (IsSign(Expr[I])) then
              Begin
                while (not SignStack.IsEmpty()) and (Priority[SignStack.Peek()] >= Priority[CurrSign]) do
                  Begin
                    Convert(SignStack, OperandStack, Oper);
                  End;
                SignStack.Push(CurrSign);
              End
            else
              Begin
                if (CurrSign = Open) then
                  Begin
                    SignStack.Push(CurrSign);
                  End
                else
                  Begin
                    while (not SignStack.IsEmpty()) and (SignStack.Peek() <> Open) do
                      Begin
                        Convert(SignStack, OperandStack, Oper);
                      End;
                    SignStack.Pop();
                  End;
              End;
          End;
        Inc(I);
      End;

    while (not SignStack.IsEmpty()) do
      Begin
        Convert(SignStack, OperandStack, Oper);
      End;

    Result := OperandStack.Peek();

    OperandStack.Destroy();
    SignStack.Destroy();
  End;

end.
