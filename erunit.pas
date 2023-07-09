unit ERunit;
{$mode ObjFPC}{$H+}

interface
uses
  Classes, SysUtils;

type
  Uregisters = array of LongWord;
  Fparameters = array of integer; // r1, r2, jumpLine
  ERfunction = procedure(param: Fparameters; var curLine: integer; regl: Uregisters);
  ERoperator = record
    func: ERfunction;
    parameters: Fparameters;
  end;
  ERcode = array of ERoperator;


const
  minRegHigh = 16;

procedure initReg(var regl: Uregisters);
{F+}
procedure zeroReg(param: Fparameters; var curLine: integer; regl: Uregisters);
procedure incReg(param: Fparameters; var curLine: integer; regl: Uregisters);
procedure copyReg(param: Fparameters; var curLine: integer; regl: Uregisters);
procedure jumpTo(param: Fparameters; var curLine: integer; regl: Uregisters);
{F-}

implementation

procedure initReg(var regl: Uregisters);
begin
  setLength(regl, minRegHigh);
end;

{F+}
procedure zeroReg(param: Fparameters; var curLine: integer; regl: Uregisters);
begin
  regl[param[0]] := 0;
  inc(curLine);
end;

procedure incReg(param: Fparameters; var curLine: integer; regl: Uregisters);
begin
  regl[param[0]]:= regl[param[0]]+1;
  inc(curLine);
end;

procedure copyReg(param: Fparameters; var curLine: integer; regl: Uregisters);
begin
  regl[param[1]] := regl[param[0]];
  inc(curLine);
end;

procedure jumpTo(param: Fparameters; var curLine: integer; regl: Uregisters);
begin
  if regl[param[0]] = regl[param[1]] then
     curLine := param[2]
  else
     inc(curLine);
end;
{F-}

end.

