unit List;

interface

Type
  TDotArray = array [1..10000] of Real;

Type
  PNode = ^TListNode;

  TListNode = record
    Data: Real;
    Next: PNode;
  end;

Type
  TList = class
    private
      Head: PNode;
    public
      procedure Add(Data: Real);
      function IsEmpty(): Boolean;
      constructor Create();
      destructor Destroy();
  end;

implementation

  procedure TList.Add(Data: Real);
    var
      CurrNode: PNode;
    Begin
      CurrNode := Head;
      while (CurrNode.Next <> nil) do
        CurrNode := CurrNode.Next;
      New(CurrNode.Next);
      CurrNode := CurrNode.Next;
      CurrNode^.Data := Data;
      CurrNode^.Next := nil;
    End;

  function TList.IsEmpty(): Boolean;
    Begin
      Result := Head.Next = nil;
    End;

  constructor TList.Create();
    Begin
      New(Head);
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
