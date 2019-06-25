{ This unit facilitates the bridging between darkteal and Gnuplot }

unit DTGNUPlot;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, DTCore;

var
  s: ansistring;

type

  { @abstract(A class to hold multiple series) }
  TSeriesList = class
    ListSize: integer;
  public
    constructor Create;
    procedure AddSeries(Series: TDTMatrix; title: string);
  private
    SeriesList: array of TDTMatrix;
    TitleList: array of string;
  end;

procedure DTGNUPlotInit(GNUplotPath: string);

procedure PlotSeriesList(SeriesList: TSeriesList; Title: string);
procedure Plot(x: TDTMatrix; Title: string); overload;
procedure Plot(x, y: TDTMatrix; Title: string); overload;

implementation

var
  _GNUPlotInitialized: boolean = False;
  _GNUPlotPath: string;
  _GNUPlotTerminal: string;

function IsDTGNUPlotReady: boolean;
begin
  Result := True;
  if not _GNUPlotInitialized then
  begin
    WriteLn('GNU Plot has not been configured properly.');
    Result := False;
  end;
end;

procedure DTGNUPlotInit(GNUplotPath: string);
begin
  _GNUPlotPath := GNUplotPath;
  _GNUPlotTerminal := 'qt';
  if FileExists(GNUplotPath) then
    _GNUPlotInitialized := True
  else
    WriteLn('GNU Plot executable is not found.');
end;

procedure PlotSeriesList(SeriesList: TSeriesList; Title: string);
var
  script, fn: string;
  sl: TStringList;
  s: string = '';
  i: integer;
begin
  if IsDTGNUPlotReady then
  begin
    sl := TStringList.Create;
    s := s + 'set terminal %s title ''%s'';';
    s := s + 'do for [i=1:64] {set style line i linewidth 2};';
    s := s + 'plot ';

    { Create series file list }
    for i := 0 to SeriesList.ListSize - 1 do
    begin
      sl := TStringList.Create;
      sl.Text := SeriesList.SeriesList[i].ToStringTable;
      fn := Format('s%d.dtgnuplotseries', [i]);
      sl.SaveToFile(fn);
    end;

    { Generate gnuplot script }
    for i := 0 to SeriesList.ListSize - 1 do
    begin
      s := s + Format('''s%d.dtgnuplotseries'' title ''%s'' with lines linestyle %d',
        [i, SeriesList.TitleList[i], i + 1]);
      if i < SeriesList.ListSize - 1 then
        s := s + ', ';
    end;
    script := Format(s, [_GNUPlotTerminal, Title, fn]);

    { The actual plotting }
    ExecuteProcess(Utf8ToAnsi(Format('%s --persist -e "%s" ', [_GNUPlotPath, script])),
      '', []);

    { Clean series file list }
    for i := 0 to SeriesList.ListSize - 1 do
    begin
      fn := Format('s%d.dtgnuplotseries', [i]);
      if FileExists(fn) then
        DeleteFile(fn);
    end;
  end;
end;

procedure Plot(x: TDTMatrix; Title: string);
var
  script, fn: string;
  sl: TStringList;
  s: string = '';
begin
  if IsDTGNUPlotReady then
  begin
    // generate temporary file
    sl := TStringList.Create;
    sl.Text := x.ToStringTable;
    fn := FormatDateTime('YYYYMMDDhhmmsszzz', Now);
    sl.SaveToFile(fn);

    s := s + 'set terminal %s title ''%s'';';
    s := s + 'do for [i=1:64] {set style line i linewidth 2};';
    s := s + 'plot ''anu.txt'' with lines linestyle 1, ''anu2.txt'' with lines linestyle 2';
    script := Format(s, [_GNUPlotTerminal, Title, fn]);

    ExecuteProcess(Utf8ToAnsi(Format('%s --persist -e "%s" ', [_GNUPlotPath, script])),
      '', []);

    // remove temporary file
    if FileExists(fn) then
      DeleteFile(fn);
  end;
end;

procedure Plot(x, y: TDTMatrix; Title: string);
begin
  Plot(AppendColumns(x, y), Title);
end;

constructor TSeriesList.Create;
begin
  ListSize := 0;
  SetLength(SeriesList, 0);
  SetLength(TitleList, 0);
end;

procedure TSeriesList.AddSeries(Series: TDTMatrix; title: string);
var
  s: TDTMatrix;
begin
  if (Series.Width = 1) or (Series.Height = 1) then
  begin
    Inc(ListSize);
    SetLength(SeriesList, Length(SeriesList) + 1);
    SetLength(TitleList, Length(TitleList) + 1);

    s := CopyMatrix(Series);
    { convert to row vector }
    if Series.Width > 1 then
      s := s.T;

    SeriesList[High(SeriesList)] := s;
    TitleList[High(TitleList)] := title;
  end
  else
    WriteLn('The shape of TDTMatrix for series must be "1 by n" or "n by 1".');
end;


end.
