unit unitMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, LazFileUtils ,FileUtil;

const
  SCR_NL = #13#10; //';';
  BODY   = '"Script imported from github. See: <a href=''https://github.com/biotronika/scriptIMPORTER'' target=''_blank''>https://github.com/biotronika/scriptIMPORTER</a>"';
  FIRST_LINE = 'title,langcode,status,title,promote,body,field_biozap_buttons,field_dni_do_potwierdzenia_skute,field_skrypt,field_urzadzenie';
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
  scrFiles: TStringList;


implementation

{$R *.lfm}

{ TFormMain }

function UpCaseFirstChar(const S: string): string;
begin
  if Length(S) = 0 then
    Result := S
  else begin
    Result := LowerCase(S);
    Result[1] := UpCase(Result[1]);
  end;
end;

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
    s : string;
    f: TextFile;

begin
    scrFiles.Clear;

    AssignFile(f, ExtractFilePath(Application.ExeName) + 'out.csv');

    try
      {$I-}
      Rewrite(f);
      {$I+}


    FindAllFiles(scrFiles, EditPath.Text, '*.txt', true); //find e.g. all pascal sourcefiles
    Memo.Lines.Add(Format('Found %d source files', [scrFiles.Count]));
    MemoScript.Lines.Add( FIRST_LINE  );
    writeln(f,FIRST_LINE);

    for i:= 0 to scrFiles.Count - 1 do begin

      s:='"'+ UpCaseFirstChar(  trim(ExtractFileNameOnly( scrFiles[i] )));
      s:= s+'","en",true,"' + UpCaseFirstChar( trim( ExtractFileNameOnly( scrFiles[i] ))) + '",';   //title, langcode, status, title
      s:= s + 'false,'+ BODY +','; //promote, body
      s:= s + '"","30",'; // buttons, dni
      s:= s + '"' + FileToStr( scrFiles[i] ) + '","multiZAP"'; //script, device

      //MemoScript.Lines.Add( s );

      MemoScript.Lines.Add( scrFiles[i]  );
      Application.ProcessMessages;
      writeln(f,s);
    end;

    finally
       CloseFile(f);
    end;
      Memo.Lines.Add( 'done.');
      Memo.Lines.Add( 'See: '+ ExtractFilePath(Application.ExeName) + 'out.csv');


end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  scrFiles := TStringList.Create;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  scrFiles.Free;
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
        script := script + RemoveSemicolon(s) + SCR_NL;

      end;

    finally
      CloseFile(f);
    end;

    if script <>'' then script := script + 'off'+SCR_NL+'@';

    result := RemoveZeros(script);

end;


end.

