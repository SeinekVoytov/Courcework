unit ExtremaFinder;

interface
  uses List, Calculator, Math;

  Procedure FindExtrema(DotArray: TDotArray;
                          var MinExtrList, MaxExtrList: TList;
                          XFrom: Integer; Range: Real);

implementation
  Procedure FindExtrema(DotArray: TDotArray;
            var MinExtrList, MaxExtrList: TList;
            XFrom: Integer; Range: Real);
    Begin
      for var I := Low(DotArray) + 1 to High(DotArray) - 1 do
        Begin
          if (DotArray[I] > DotArray[I + 1]) and
             (DotArray[I] > DotArray[I - 1]) then
            MaxExtrList.Add(Math.RoundTo(XFrom + Range * (I - 1), -3), Math.RoundTo(DotArray[I], -3));

          if (DotArray[I] < DotArray[I + 1]) and
             (DotArray[I] < DotArray[I - 1]) then
            MinExtrList.Add(Math.RoundTo(XFrom + Range * (I - 1), -3), Math.RoundTo(DotArray[I], -3));
        End;
    End;
end.
