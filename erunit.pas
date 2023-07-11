unit ERunit;
{$mode ObjFPC}{$H+}

interface
uses
  Classes, SysUtils;

type
  Uregisters = array of LongWord;
  Fparameters = array of integer; // r1, r2, jumpLine
  ERfunction = procedure(param: Fparameters) of object;
  ERoperator = record
    func: ERfunction;
    parameters: Fparameters;
  end;
  ERcode = array of ERoperator;
  ControlUnit = class
  private
    curLine: Integer;
    registers: Uregisters;
  public
    constructor Create;
    procedure setRegister(id: Integer; newValue: LongWord);
    function getRegister(id: Integer): LongWord;
    function CountReg: Integer;
    {F+}
    procedure zeroReg(param: Fparameters);
    procedure incReg(param: Fparameters);
    procedure copyReg(param: Fparameters);
    procedure jumpTo(param: Fparameters);
    {F-}
  end;

const
  minRegHigh = 16;

implementation

{ ControlUnit }
constructor ControlUnit.Create;
begin
  setLength(registers, minRegHigh);
end;

procedure ControlUnit.setRegister(id: Integer; newValue: LongWord);
begin
  if high(registers) < id then
     setLength(registers, id+1);
  registers[id]:= newValue;
end;

function ControlUnit.getRegister(id: Integer): LongWord;
begin
  if high(registers) < id then
     Result:= 0
  else
     Result:= registers[id];
end;

function ControlUnit.CountReg: Integer;
begin
  Result:= length(registers);
end;

{F+}
procedure ControlUnit.zeroReg(param: Fparameters);
begin
  if high(registers) >= param[0] then
     registers[param[0]] := 0;
  inc(curLine);
end;

procedure ControlUnit.incReg(param: Fparameters);
begin
  if high(registers) < param[0] then
    setLength(registers, param[0]+1);
  registers[param[0]]:= registers[param[0]]+1;
  inc(curLine);
end;

procedure ControlUnit.copyReg(param: Fparameters);
begin
  if high(registers) < param[1] then
    setLength(registers, param[1]+1);
  registers[param[1]] := getRegister(param[0]);
  inc(curLine);
end;

procedure ControlUnit.jumpTo(param: Fparameters);
begin
  if getRegister(param[0]) = getRegister(param[1]) then
     curLine := param[2]
  else
     inc(curLine);
end;
{F-}
//  //
end.
