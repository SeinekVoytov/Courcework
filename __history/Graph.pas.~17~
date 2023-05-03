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
    procedure Paint(var Bitmap: TBitmap; XStep: Real; Scale, YOffset, LBorder, RBorder: Integer);
    procedure ShiftArrayOfDotsRight(const XFrom, ShiftingSize: Integer; XStep: Real);
    procedure ShiftArrayOfDotsLeft(const XTo, ShiftingSize: Integer; XStep: Real);
    procedure PaintExtremaDots(var Bitmap: TBitmap; Scale, XFrom, XTo, YTo: Integer);
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
            ArrayOfDots[I] := CurrY;

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
      MinExtrList.Destroy;
      MaxExtrList.Destroy;
      inherited;
    End;

  procedure TGraph.Paint(var Bitmap: TBitmap; XStep: Real; Scale, YOffset, LBorder, RBorder: Integer);
  var
    WasNan: Boolean;
    CurrX: Real;
    CurrY: LongInt;
    I: Integer;
    Begin
      WasNan := True;
      CurrX := 0;
      Bitmap.Canvas.Pen.Color := Self.Color;
      for I := LBorder + 1 to RBorder do
        Begin
          CurrY := Trunc(-Scale * Self.ArrayOfDots[I]) + YOffset;
          if (FloatToStr(Self.ArrayOfDots[I]) = 'NAN') or
             (Self.ArrayOfDots[I] > 4375000) or
             (Self.ArrayOfDots[I] < -4375000) then
            WasNaN := True
          else if (WasNan) then
            begin
              Bitmap.Canvas.MoveTo(Trunc(CurrX), CurrY);
              WasNan := False;
            End
          else
            Bitmap.Canvas.LineTo(Trunc(CurrX), CurrY);

          CurrX := CurrX + XStep;
        End;
    End;

  procedure TGraph.ShiftArrayOfDotsRight(const XFrom, ShiftingSize: Integer; XStep: Real);
    var
      X: Real;
    Begin
      Dec(LeftBound);
      for var I := High(ArrayOfDots) - ShiftingSize downto Low(ArrayOfDots) do
        ArrayOfDots[I + ShiftingSize] := ArrayOfDots[I];

      X := XFrom;
      for var I := ShiftingSize downto Low(ArrayOfDots) do
        Begin
          ArrayOfDots[I] := Calculate(Expression, X);
          X := X - XStep;
        End;
    End;

  procedure TGraph.ShiftArrayOfDotsLeft(const XTo, ShiftingSize: Integer; XStep: Real);
  var
    X: Real;
    Begin
      Inc(LeftBound);
      for var I := ShiftingSize + 1 to High(ArrayOfDots) do
        ArrayOfDots[I - ShiftingSize] := ArrayOfDots[I];

      X := XTo;
      for var I := ITERATION_COUNT - ShiftingSize + 1 to ITERATION_COUNT do
        Begin
          ArrayOfDots[I] := Calculate(Expression, X);
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
      for var I := Low(ArrayOfDots) + 1 to  High(ArrayOfDots) - 1 do
        Begin
          if (FloatToStr(ArrayOfDots[I]) <> 'NAN') and
             (FloatToStr(ArrayOfDots[I - 1]) <> 'NAN') and
             (FloatToStr(ArrayOfDots[I + 1]) <> 'NAN') then
            Begin
              if (ArrayOfDots[I] > ArrayOfDots[I + 1]) and
                 (ArrayOfDots[I] > ArrayOfDots[I - 1]) then
                MaxExtrList.Add(Math.RoundTo(LeftBound + Range * (I - 1), -3), Math.RoundTo(ArrayOfDots[I], -3));

              if (ArrayOfDots[I] < ArrayOfDots[I + 1]) and
                 (ArrayOfDots[I] < ArrayOfDots[I - 1]) then
                MinExtrList.Add(Math.RoundTo(LeftBound + Range * (I - 1), -3), Math.RoundTo(ArrayOfDots[I], -3));
            End;
        End;
    End;

  procedure TGraph.PaintExtremaDots(var Bitmap: TBitmap; Scale, XFrom, XTo, YTo: Integer);
  const
    CIRCLE_RADIUS = 5;

  procedure PaintExtrema(ExtrList: TList);
    var
      CurrNode: PNode;
      X, Y: Integer;
    Begin
      CurrNode := ExtrList.GetHead().Next;
      while (CurrNode <> nil) do
        Begin
          if (CurrNode.X >= XFrom)  and (CurrNode.X <= XTo) then
            Begin
              X := Trunc((CurrNode.X - XFrom) * Scale);
              Y := Trunc((YTo - CurrNode.Y) * Scale);
              Bitmap.Canvas.Ellipse(X - CIRCLE_RADIUS, Y - CIRCLE_RADIUS,
                                    X + CIRCLE_RADIUS, Y + CIRCLE_RADIUS);
            End;
          CurrNode := CurrNode.Next;
        End;
    End;

  var
    PenColor, BrushColor: TColor;
    Begin
      PenColor := Bitmap.Canvas.Pen.Color;
      BrushColor :=  Bitmap.Canvas.Brush.Color;

      with Bitmap.Canvas do
      if (PenColor = clRed) then
        Begin
          Pen.Color := clBlack;
          Brush.Color := clBlack;
        End
      else
        Begin
          Pen.Color := clRed;
          Brush.Color := clRed;
        End;

      PaintExtrema(MinExtrList);
      PaintExtrema(MaxExtrList);

      with Bitmap.Canvas do
        Begin
          Pen.Color := PenColor;
          Brush.Color := BrushColor;
        End;
    End;

end.
