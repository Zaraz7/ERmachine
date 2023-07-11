unit mainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  StdCtrls, PairSplitter, ComCtrls, ValEdit, SynHighlighterPas, SynEdit,
  SynHighlighterAny, ERunit, ERparser;

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
    ToolBar: TToolBar;
    RunTool: TToolButton;
    SeparatorTool: TToolButton;
    PauseTool: TToolButton;
    StopTool: TToolButton;
    DebugTool: TToolButton;
    StepTool: TToolButton;
    RegisterListEditor: TValueListEditor;
    procedure FormActivate(Sender: TObject);
    procedure NextPageButtonClick(Sender: TObject);
    procedure PrevPageButtonClick(Sender: TObject);
    procedure RegisterListEditorEditingDone(Sender: TObject);
    procedure RegisterListEditorKeyPress(Sender: TObject; var Key: char);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

var
  page: Integer = 0;
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
      MainForm.RegisterListEditor.Cells[1,id mod page + 1]:= intToStr(cu.getRegister(id));
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

procedure TMainForm.RegisterListEditorKeyPress(Sender: TObject; var Key: char);
begin
  case Key of
  '0'..'9': key:=key;
  #8: key:=key;
  else key:=#0;
  end;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  UpdatePageRegisters;
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

