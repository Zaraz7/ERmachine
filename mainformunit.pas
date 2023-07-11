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
    procedure RegisterListEditorKeyPress(Sender: TObject; var Key: char);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }



procedure TMainForm.RegisterListEditorKeyPress(Sender: TObject; var Key: char);
begin
  case Key of
  '0'..'9': key:=key;
  #8: key:=key;
  else key:=#0;
  end;
end;

end.

