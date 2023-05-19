unit Graph;

interface

uses List, Graphics, SysUtils, Calculator, Math;

Type
  TGraph = class
  public
    ArrayOfDots: TDotArray;
    Expression: String;
    MinY, MaxY: Real;
    MinExtrList: TList;
    MaxExtrList: TList;
    Color: TColor;
    Width: Byte;
    IsExtremaFound: Boolean;
    LeftBound: Integer;
    constructor Create(Expression: String; Color: TColor; Width: Byte; XStep, CurrX: Real; XFrom: Integer; IsExtremaFound: Boolean);
    destructor Destroy();
    function Paint(var Bitmap: TBitmap; XStep, Scale: Real; YOffset, LBorder, RBorder: Integer): Boolean;
    procedure ShiftArrayOfDotsRight(const XFrom, ShiftingSize: Integer; XStep: Real);
    procedure ShiftArrayOfDotsLeft(const XTo, ShiftingSize: Integer; XStep: Real);
    procedure FindExtrema(XFrom: Integer; Range: Real);
  end;

const
  ITERATION_COUNT = 10000;

implementation
  constructor TGraph.Create(Expression: String; Color: TColor; Width: Byte; XStep, CurrX: Real; XFrom: Integer; IsExtremaFound: Boolean);
    procedure InitArrayOfDots();
      var
        CurrY: Real;
      Begin
        for var I := 1 to ITERATION_COUNT do
          Begin
            CurrY := Calculate(Expression, CurrX);
            if (CurrY > MaxY) then
              MaxY := CurrY;
            if (CurrY < MinY) then
              MinY := CurrY;
            ArrayOfDots[2][I] := CurrY;
            ArrayOfDots[1][I] := CurrX;

            CurrX := CurrX + XStep;
          End;
      End;

    Begin
      Self.Expression := Expression;
      MaxY := Single.MinValue;
      Self.IsExtremaFound := IsExtremaFound;
      MinY := Single.MaxValue;
      InitArrayOfDots();
      Self.Color := Color;
      Self.Width := Width;
      LeftBound := XFrom;
    End;

  destructor TGraph.Destroy();
    Begin
      if IsExtremaFound then
      Begin
        MinExtrList.Destroy;
        MaxExtrList.Destroy;
      End;
      inherited;
    End;

  Function TGraph.Paint(var Bitmap: TBitmap; XStep, Scale: Real; YOffset, LBorder, RBorder: Integer): Boolean;
  const
    CIRCLE_RADIUS = 5;
  var
    WasNan: Boolean;
    CurrX: Real;
    CurrY: LongInt;
    I: Integer;
    Begin
      WasNan := True;
      Result := False;
      CurrX := 0;
      Bitmap.Canvas.Pen.Color := Self.Color;
      Bitmap.Canvas.Pen.Width := Self.Width;
      if (IsExtremaFound) then
      begin
        var CurrMinListNode, CurrMaxListNode: PNode;
        CurrMinListNode := MinExtrList.GetHead.Next;
        CurrMaxListNode := MaxExtrList.GetHead.Next;
        for I := LBorder + 1 to RBorder do
          Begin
            CurrY := Round(-Scale * Self.ArrayOfDots[2][I]) + YOffset;
            if (FloatToStr(Self.ArrayOfDots[2][I]) = 'NAN') or
               (Self.ArrayOfDots[2][I] > 4375000) or
               (Self.ArrayOfDots[2][I] < -4375000) then
              WasNaN := True
            else if (WasNan) then
              begin
                Bitmap.Canvas.MoveTo(Round(CurrX), CurrY);
                WasNan := False;
                Result := True;
              End
            else
              Begin
                Bitmap.Canvas.LineTo(Round(CurrX), CurrY);
                Result := True;
              End;

            if (CurrMinListNode <> nil) and (CurrMinListNode.Index = I) then
              begin
                Bitmap.Canvas.Ellipse(Round(CurrX) - CIRCLE_RADIUS, CurrY - CIRCLE_RADIUS,
                                    Round(CurrX) + CIRCLE_RADIUS, CurrY + CIRCLE_RADIUS);
                CurrMinListNode := CurrMinListNode.Next;
              end;

            if (CurrMaxListNode <> nil) and (CurrMaxListNode.Index = I) then
              begin
                Bitmap.Canvas.Ellipse(Round(CurrX) - CIRCLE_RADIUS, CurrY - CIRCLE_RADIUS,
                                    Round(CurrX) + CIRCLE_RADIUS, CurrY + CIRCLE_RADIUS);
                CurrMaxListNode := CurrMaxListNode.Next;
              end;

            CurrX := CurrX + XStep;
          End;
      end
      else
      begin
        for I := LBorder + 1 to RBorder do
          Begin
            CurrY := Round(-Scale * Self.ArrayOfDots[2][I]) + YOffset;
            if (FloatToStr(Self.ArrayOfDots[2][I]) = 'NAN') or
               (Self.ArrayOfDots[2][I] > 4375000) or
               (Self.ArrayOfDots[2][I] < -4375000) then
              WasNaN := True
            else if (WasNan) then
              begin
                Bitmap.Canvas.MoveTo(Round(CurrX), CurrY);
                WasNan := False;
                Result := True;
              End
            else
              Begin
                Bitmap.Canvas.LineTo(Round(CurrX), CurrY);
                Result := True;
              End;

            CurrX := CurrX + XStep;
          End;
      end;
    End;

  procedure TGraph.ShiftArrayOfDotsRight(const XFrom, ShiftingSize: Integer; XStep: Real);
    var
      X: Real;
    Begin
      Dec(LeftBound);
      for var I := High(ArrayOfDots[2]) - ShiftingSize downto Low(ArrayOfDots[2]) do
      begin
        ArrayOfDots[1][I + ShiftingSize] := ArrayOfDots[1][I];
        ArrayOfDots[2][I + ShiftingSize] := ArrayOfDots[2][I];
      end;

      X := XFrom;
      for var I := ShiftingSize downto Low(ArrayOfDots[2]) do
        Begin
          ArrayOfDots[1][I] := X;
          ArrayOfDots[2][I] := Calculate(Expression, X);
          X := X - XStep;
        End;
    End;

  procedure TGraph.ShiftArrayOfDotsLeft(const XTo, ShiftingSize: Integer; XStep: Real);
  var
    X: Real;
    Begin
      Inc(LeftBound);
      for var I := ShiftingSize + 1 to High(ArrayOfDots[2]) do
      begin
        ArrayOfDots[1][I - ShiftingSize] := ArrayOfDots[1][I];
        ArrayOfDots[2][I - ShiftingSize] := ArrayOfDots[2][I];
      end;

      X := XTo;
      for var I := ITERATION_COUNT - ShiftingSize + 1 to ITERATION_COUNT do
        Begin
          ArrayOfDots[1][I] := X;
          ArrayOfDots[2][I] := Calculate(Expression, X);
          X := X + XStep;
        End;
    End;

  procedure TGraph.FindExtrema(XFrom: Integer; Range: Real);
    Begin
      if (MinExtrList = nil) or (MaxExtrList = nil) then
        Begin
          MinExtrList := TList.Create;
          MaxExtrList := TList.Create;
        End
      else
        Begin
          MinExtrList.Destroy;
          MaxExtrList.Destroy;
          MinExtrList := TList.Create;
          MaxExtrList := TList.Create;
        End;
      for var I := Low(ArrayOfDots[2]) + 1 to  High(ArrayOfDots[2]) - 1 do
        Begin
          if (FloatToStr(ArrayOfDots[2][I]) <> 'NAN') and
             (FloatToStr(ArrayOfDots[2][I - 1]) <> 'NAN') and
             (FloatToStr(ArrayOfDots[2][I + 1]) <> 'NAN') then
            Begin
              if (ArrayOfDots[2][I] > ArrayOfDots[2][I + 1]) and
                 (ArrayOfDots[2][I] > ArrayOfDots[2][I - 1]) then
                MaxExtrList.Add(I);

              if (ArrayOfDots[2][I] < ArrayOfDots[2][I + 1]) and
                 (ArrayOfDots[2][I] < ArrayOfDots[2][I - 1]) then
                MinExtrList.Add(I);
            End;
        End;
    End;

end.
