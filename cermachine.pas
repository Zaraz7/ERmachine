program CERmachine;
uses ERunit, CRT;

var
  operators: array of ERoperator;
  mainRegl: registers;
  i: longint;

procedure writeRegl;
var i: integer;
begin
  for i:=0 to high(mainRegl) do
      writeln(i, mainRegl[i]:8);
end;

begin
  initReg(mainRegl);
  mainRegl[0]:=100;
  writeRegl;
  writeln;
  i:=1;
  incReg([1],i, mainRegl);
  copyReg([0, 2],i, mainRegl);
  writeRegl;
  jumpTo([0, 2, 1000],i, mainRegl);
  writeln(i);
end.
