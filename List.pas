unit List;

interface

Type
  TDotArray = array [1..2] of array [1..10000] of Real;

Type
  PNode = ^TListNode;

  TListNode = record
    Index: Integer;
    Next: PNode;
  end;

Type
  TList = class
    private
      Head: PNode;
    public
      function GetHead(): PNode;
      procedure Add(Index: Integer);
      function IsEmpty(): Boolean;
      constructor Create();
      destructor Destroy();
  end;

implementation

  function TList.GetHead(): PNode;
    Begin
      Result := Head;
    End;

  procedure TList.Add(Index: Integer);
    var
      CurrNode: PNode;
    Begin
      CurrNode := Head;
      while (CurrNode.Next <> nil) do
        CurrNode := CurrNode.Next;
      New(CurrNode.Next);
      CurrNode := CurrNode.Next;
      CurrNode^.Index := Index;
      CurrNode^.Next := nil;
    End;

  function TList.IsEmpty(): Boolean;
    Begin
      Result := Head.Next = nil;
    End;

  constructor TList.Create();
    Begin
      New(Head);
      Head.Next := nil;
    End;

  destructor TList.Destroy();
    var
      CurrNode, TempNode: PNode;
    Begin
      CurrNode := Head.Next;
      Dispose(Head);
      while (CurrNode <> nil) do
        Begin
          TempNode := CurrNode;
          CurrNode := CurrNode.Next;
          Dispose(TempNode);
        End;
    End;

end.
