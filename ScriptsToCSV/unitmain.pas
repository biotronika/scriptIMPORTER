unit unitMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, FileUtil;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonReadFiles: TButton;
    EditPath: TEdit;
    Memo: TMemo;
    MemoScript: TMemo;
    Panel1: TPanel;
    procedure ButtonReadFilesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private

  public

    function FileToStr( FileName : string) : string;

  end;

var
  FormMain: TFormMain;
  ScriptFiles: TStringList;


implementation

{$R *.lfm}

{ TFormMain }

function RemoveZeros (s: string): string;
var
  TagBegin, TagEnd, TagLength: integer;
begin

  TagBegin := Pos( '.00', s);

  while (TagBegin > 0) do begin
    Delete(s, TagBegin, 3);
    TagBegin := Pos( '.00', s);
  end;

  result := s;

end;

function RemoveSemicolon (s: string): string;
var
  TagBegin, TagEnd, TagLength: integer;
begin

  TagBegin := Pos( ';', s);

  while (TagBegin > 0) do begin
    Delete(s, TagBegin, 3);
    TagBegin := Pos( ';', s);
  end;

  result := s;

end;

procedure TFormMain.ButtonReadFilesClick(Sender: TObject);
var i : integer;

begin
    ScriptFiles.Clear;

    FindAllFiles(ScriptFiles, EditPath.Text, '*.txt', true); //find e.g. all pascal sourcefiles
    Memo.Lines.Add(Format('Found %d source files', [ScriptFiles.Count]));

    for i:= 0 to (*ScriptFiles.Count - 1*) 10 do begin
      MemoScript.Lines.Add( FileToStr( ScriptFiles[i]) );
    end;



end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ScriptFiles := TStringList.Create;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  ScriptFiles.Free;
end;

function TFormMain.FileToStr( FileName : string) : string;
var
  f: TextFile;
  s, script: string;
begin

    AssignFile(f, FileName);
    script := '';
    try
      {$I-}
      Reset(f);
      {$I+}
      while not eof(f) do begin

        Readln(f, s);
        script := script + RemoveSemicolon(s) + #13#10;

      end;

    finally
      CloseFile(f);
    end;

    if script <>'' then script := script + 'off'#13#10'@';

    result := RemoveZeros(script);

end;


end.

