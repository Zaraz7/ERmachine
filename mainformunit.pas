unit mainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  StdCtrls, PairSplitter, ComCtrls, ValEdit, SynHighlighterPas, SynEdit,
  SynHighlighterAny, ERunit, ERparser, SynEditMarks, SynGutterBase;

type

  { TMainForm }

  TMainForm = class(TForm)
    NextPageButton: TButton;
    PrevPageButton: TButton;
    RegistersButton: TPanel;
    RegisterBox: TGroupBox;
    IconList: TImageList;
    LogListBox: TListBox;
    MainMenu: TMainMenu;
    FileItem: TMenuItem;
    InfoItem: TMenuItem;
    ERSyn: TSynAnySyn;
    MainTextEdit: TSynEdit;
    RunTimer: TTimer;
    UpdaterTimer: TTimer;
    ToolBar: TToolBar;
    RunTool: TToolButton;
    SeparatorTool: TToolButton;
    PauseTool: TToolButton;
    StopTool: TToolButton;
    DebugTool: TToolButton;
    StepTool: TToolButton;
    RegisterListEditor: TValueListEditor;
    procedure FormActivate(Sender: TObject);
    procedure LogListBoxClick(Sender: TObject);
    procedure MainTextEditGutterClick(Sender: TObject; X, Y, Line: integer;
      mark: TSynEditMark);
    procedure NextPageButtonClick(Sender: TObject);
    procedure PrevPageButtonClick(Sender: TObject);
    procedure RegisterListEditorEditingDone(Sender: TObject);
    procedure RegisterListEditorKeyPress(Sender: TObject; var Key: char);
    procedure RunTimerTimer(Sender: TObject);
    procedure RunToolClick(Sender: TObject);
    procedure StopToolClick(Sender: TObject);
    procedure UpdaterTimerTimer(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

var
  page: Integer = 0;
  notStop: Boolean;
  firstReg, lastReg: Integer;

{$R *.lfm}

{ TMainForm }

procedure UpdateViewRegisters;
var i: Integer;
begin
   for i:=0 to 15 do begin
     MainForm.RegisterListEditor.Cells[0,i+1]:= intToStr(firstReg+i);
     if firstReg+i <= cu.CountReg-1 then
        MainForm.RegisterListEditor.Cells[1,i+1]:= intToStr(cu.getRegister(firstReg+i))
     else
        MainForm.RegisterListEditor.Cells[1,i+1]:= '0';
   end;
end;

procedure UpdateRegister(id: Integer);
begin
   if (id div 16) = page then
      MainForm.RegisterListEditor.Cells[1,id mod 16+1]:= intToStr(cu.getRegister(id));
end;

procedure UpdatePageRegisters;
begin
   firstReg:=page*16;
   lastReg:=firstReg+15;
   MainForm.RegistersButton.Caption:=intToStr(firstReg)+' - '+intToStr(lastReg);
   if firstReg = 0 then
      MainForm.PrevPageButton.Enabled:=False
   else
     MainForm.PrevPageButton.Enabled:=True;
   UpdateViewRegisters;
end;

procedure RunCode;
begin
  cu.Line:=0;
  with MainForm do begin
    UpdaterTimer.Enabled:=True;
    RunTimer.Enabled:=True;

    RunTool.Enabled:=False;
    DebugTool.Enabled:=False;
    StopTool.Enabled:=True;
    PauseTool.Enabled:=True;
  end;
end;

procedure StopCode;
begin
  with MainForm do begin
    UpdaterTimer.Enabled:=False;
    RunTimer.Enabled:=False;

    RunTool.Enabled:=True;
    DebugTool.Enabled:=True;
    StopTool.Enabled:=False;
    PauseTool.Enabled:=False;
  end;
  UpdateViewRegisters;
end;

procedure TMainForm.RegisterListEditorKeyPress(Sender: TObject; var Key: char);
begin
  case Key of
  '0'..'9': key:=key;
  #8: key:=key;
  else key:=#0;
  end;
end;

procedure TMainForm.RunTimerTimer(Sender: TObject);
begin
  if cu.Line < length(code) then
     code[cu.Line].func(code[cu.Line].parameters)
  else begin
    StopCode;
  end;
end;

procedure TMainForm.RunToolClick(Sender: TObject);
begin
  Parse(MainForm.MainTextEdit.Lines);
  RunCode;
end;

procedure TMainForm.StopToolClick(Sender: TObject);
begin
  StopCode;
end;

procedure TMainForm.UpdaterTimerTimer(Sender: TObject);
begin
  UpdateViewRegisters;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  UpdatePageRegisters;
end;

procedure TMainForm.LogListBoxClick(Sender: TObject);
begin

end;

procedure TMainForm.MainTextEditGutterClick(Sender: TObject; X, Y,
  Line: integer; mark: TSynEditMark);
var m: TSynEditMark;
  i: integer;
begin
  for i:=0 to MainTextEdit.Marks.Count-1 do
      if MainTextEdit.Marks.Items[i].Line = Line then begin
      MainTextEdit.Marks.Items[i].Visible:=False;
      MainTextEdit.Marks.Delete(i);
          exit;
      end;


  m:=TSynEditMark.Create(MainTextEdit);
  m.Line:=Line;
  m.ImageList := IconList;
  m.ImageIndex := 3;
  m.Visible := true;
  MainTextEdit.Marks.Add(m);
  //showMessage(IntToStr(MainTextEdit.Marks.Count)+' '+IntToStr(MainTextEdit.Marks.Items[0].Line));
end;



procedure TMainForm.NextPageButtonClick(Sender: TObject);
begin
  inc(page);
  UpdatePageRegisters;
end;

procedure TMainForm.PrevPageButtonClick(Sender: TObject);
begin
  dec(page);
  UpdatePageRegisters;
end;

procedure TMainForm.RegisterListEditorEditingDone(Sender: TObject);
begin
  with Sender as TValueListEditor do begin
    if Cells[Col,Row] = '' then
       Cells[Col,Row]:= '0';
    cu.setRegister(firstReg+Row-1, strToInt(Cells[Col,Row]));
  end;
end;

end.

