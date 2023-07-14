unit mainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  StdCtrls, PairSplitter, ComCtrls, ValEdit, SynHighlighterPas, SynEdit,
  SynHighlighterAny, ERunit, ERparser, SynEditMarks, SynGutterBase, blankUnit;

type

  { TMainForm }

  TMainForm = class(TForm)
    ExitItem: TMenuItem;
    SaveItem: TMenuItem;
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
    DebugTimer: TTimer;
    UpdaterTimer: TTimer;
    ToolBar: TToolBar;
    RunTool: TToolButton;
    SeparatorTool: TToolButton;
    PauseTool: TToolButton;
    StopTool: TToolButton;
    DebugTool: TToolButton;
    StepTool: TToolButton;
    RegisterListEditor: TValueListEditor;
    procedure DebugTimerTimer(Sender: TObject);
    procedure ExitItemClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure InfoItemClick(Sender: TObject);
    procedure MainTextEditChange(Sender: TObject);
    procedure MainTextEditGutterClick(Sender: TObject; X, Y, Line: integer;
      mark: TSynEditMark);
    procedure NextPageButtonClick(Sender: TObject);
    procedure PrevPageButtonClick(Sender: TObject);
    procedure RegisterListEditorEditingDone(Sender: TObject);
    procedure RegisterListEditorKeyPress(Sender: TObject; var Key: char);
    procedure RunTimerTimer(Sender: TObject);
    procedure RunToolClick(Sender: TObject);
    procedure SaveItemClick(Sender: TObject);
    procedure StopToolClick(Sender: TObject);
    procedure UpdaterTimerTimer(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

const
  TemplateDir = 'templates';
  TemplateFileName = 'test.er';
  RegistersFileName = 'registers';

var
  page: Integer = 0;
  Pause, ChangedFile: Boolean;
  firstReg, lastReg: Integer;
  ProjectDir: String = TemplateDir;
  FileName: String = TemplateFileName;
  registersFile: File of LongWord;

{$R *.lfm}

function FullFileName: String;
begin
   Result:= ProjectDir+PathDelim+FileName;
end;

function FullRegistersFileName: String;
begin
   Result:= ProjectDir+PathDelim+RegistersFileName;
end;

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

procedure SaveRegisters;
var i: Integer;
begin
   cu.Optimize;
   assign(registersFile, FullRegistersFileName);
   rewrite(registersFile);
   for i:=0 to cu.CountReg-1 do
       write(registersFile, cu.getRegister(i));
   close(registersFile);
end;

procedure OpenRegisters;
var i: Integer;
    inp: LongWord;
begin
  if not FileExists(FullRegistersFileName) then begin
      exit;
   end;
  assign(registersFile, FullRegistersFileName);
   reset(registersFile);
   cu.SetLen(System.FileSize(registersFile));
   for i:=0 to cu.CountReg-1 do begin
       read(registersFile, inp);
       cu.setRegister(i, inp);
   end;
   close(registersFile);
   UpdatePageRegisters;
end;

function SaveFile: boolean;
begin
  try
    MainForm.MainTextEdit.Lines.SaveToFile(FullFileName);
    ChangedFile := False;
    SaveRegisters;
  except
    on E : Exception do begin
      ShowMessage('Ошибка сохранения файла!'+LineEnding+E.Message);
      SaveFile := False;
    end;
  end;
end;

procedure OpenFile;
begin
  MainForm.MainTextEdit.Lines.LoadFromFile(FullFileName);
  ChangedFile := False;
  OpenRegisters;
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
    RegisterBox.Enabled:=False;
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
    RegisterBox.Enabled:=True;
    cu.Line:=0;
    LogListBox.Items.Add('Машина остановлена.');
  end;
  UpdateViewRegisters;
end;

function StepCode: Integer;
begin
  Result:= code[cu.Line].func(code[cu.Line].parameters);
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
     StepCode
  else begin
    StopCode;
  end;
end;

procedure TMainForm.DebugTimerTimer(Sender: TObject);
begin
  if Pause then
    with MainForm do begin
      DebugTimer.Enabled:=False;
      DebugTool.Enabled:=True;
      StepTool.Enabled:=True;
      exit;
    end;

  if cu.Line < length(code) then
     StepCode
  else begin
    StopCode;
  end;
end;

procedure TMainForm.ExitItemClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.RunToolClick(Sender: TObject);
begin
  try
    Parse(MainForm.MainTextEdit.Lines);
    MainForm.LogListBox.Items.Add('Компиляция прошла успешно.');
    RunCode;
  except
    on E : Exception do MainForm.LogListBox.Items.Add('Компиляция прервана: '+E.Message);
  end;

end;

procedure TMainForm.SaveItemClick(Sender: TObject);
begin
  SaveFile;
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

procedure TMainForm.FormShow(Sender: TObject);
begin
  with MainForm do begin
    DebugTool.Visible:=False;
    PauseTool.Visible:=False;
  end;
  OpenFile;
end;

procedure TMainForm.InfoItemClick(Sender: TObject);
begin
  BlankFrame.Show;
end;

procedure TMainForm.MainTextEditChange(Sender: TObject);
begin
  ChangedFile:=True;
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
    ChangedFile := True;
  end;
end;

end.

