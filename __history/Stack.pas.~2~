unit Stack;

interface

Type
  TStack<T> = class
    Stack: array of T;
    Size: Integer;
    public Function Pop(): T;
    public Function Peek(): T;
    public Procedure Push(Item: T);
    public Function IsEmpty(): Boolean;
    public constructor Create(Const InintialCapacity: Integer);
    public destructor Destroy();
  end;

implementation
  constructor TStack<T>.Create(Const InintialCapacity: Integer);
    Begin
      SetLength(Stack, InintialCapacity);
      Size := 0;
    End;

  destructor TStack<T>.Destroy();
    Begin

      inherited;
    End;

  Function TStack<T>.Pop(): T;
    Begin
      Result := Stack[Size - 1];
      Dec(Size);
    End;

  Function TStack<T>.Peek(): T;
    Begin
      Result := Stack[Size - 1];
    End;

  Function TStack<T>.IsEmpty(): Boolean;
    Begin
      Result := (Size = 0);
    End;

  Procedure TStack<T>.Push(Item: T);
    Begin
      Stack[Size] := Item;
      Inc(Size);
    End;
end.
