{ This unit facilitates the bridging between darkteal and Gnuplot }

unit DTGNUPlot;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, DTCore;

var
  s: ansistring;

type

  TPlotType = (ptLines, ptDots, ptHistogram);

  { @abstract(A class that holds information of data points to plot, including its style) }
  TPlot = class(TObject)
    PlotLabel: string;
    PlotType: TPlotType;
    OverrideDefaultStyle: boolean;
  public
    constructor Create;
    function GenerateScript: string; // Move this to private later
    procedure SetDataPoints(x: TDTMatrix); overload;
    procedure SetDataPoints(x, y: TDTMatrix); overload;
  private
    values: TDTMatrix;
  end;

  { @abstract(A class to hold multiple series) }
  TSeriesList = class(TObject)
    ListSize: integer;
    XLabel: string;
    YLabel: string;
  public
    constructor Create;
    procedure AddSeries(Series: TDTMatrix; title: string);
    function GenerateScript: string; // Move this to private later
  private
    SeriesList: array of TDTMatrix;
    TitleList: array of string;
  end;

  { @abstract(A class that holds information of a single figure) }
  TFigure = class(TObject)
    Title: string;
    XLabel: string;
    YLabel: string;
    PlotList: TList;
  public
    constructor Create;
    function GenerateScript: string; // Move this to private later
    procedure AddPlot(Plot: TPlot);
    procedure Show;
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


procedure MatrixStringTableToFile(X: TDTMatrix; fn: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Text := X.ToStringTable;
  sl.SaveToFile(fn);
end;

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
    { Create series file list }
    for i := 0 to SeriesList.ListSize - 1 do
    begin
      sl := TStringList.Create;
      sl.Text := SeriesList.SeriesList[i].ToStringTable;
      fn := Format('s%d.dtgnuplotseries', [i]);
      sl.SaveToFile(fn);
    end;

    { Generate gnuplot script }
    sl := TStringList.Create;
    s := s + 'set terminal %s title ''%s'';';
    s := s + 'set xlabel ''' + SeriesList.XLabel + ''';';
    s := s + 'set ylabel ''' + SeriesList.YLabel + ''';';
    s := s + 'do for [i=1:64] {set style line i linewidth 2};';
    s := s + 'plot ';
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
    { tunggu aba-aba }

    //// generate temporary file
    //sl := TStringList.Create;
    //sl.Text := x.ToStringTable;
    //fn := FormatDateTime('YYYYMMDDhhmmsszzz', Now);
    //sl.SaveToFile(fn);

    //s := s + 'set terminal %s title ''%s'' show title;';
    //s := s + 'do for [i=1:64] {set style line i linewidth 2};';
    //s := s + 'plot ''anu.txt'' with lines linestyle 1, ''anu2.txt'' with lines linestyle 2';
    //script := Format(s, [_GNUPlotTerminal, Title, fn]);

    //ExecuteProcess(Utf8ToAnsi(Format('%s --persist -e "%s" ', [_GNUPlotPath, script])),
    //  '', []);

    //// remove temporary file
    //if FileExists(fn) then
    //  DeleteFile(fn);
  end;
end;

procedure Plot(x, y: TDTMatrix; Title: string);
begin
  Plot(AppendColumns(x, y), Title);
end;

{ TFigure }
constructor TFigure.Create;
begin
  PlotList := TList.Create;
end;

function TFigure.GenerateScript: string;
var
  s, script: string;
  i: integer;
begin
  s := '';
  s := s + 'set terminal %s title ''%s'';';
  s := s + 'set xlabel ''' + self.XLabel + ''';';
  s := s + 'set ylabel ''' + self.YLabel + ''';';
  s := s + 'do for [i=1:64] {set style line i linewidth 2};';

  s := s + 'plot ';
  for i := 0 to PlotList.Count - 1 do
    s := s + TPlot(PlotList.items[i]).GenerateScript;
  script := Format(s, [_GNUPlotTerminal, Title]);
  Result := script;
end;

procedure TFigure.AddPlot(Plot: TPlot);
begin
  PlotList.Add(Plot);
end;

procedure TFigure.Show;
begin
  ExecuteProcess(Utf8ToAnsi(Format('%s --persist -e "%s" ',
    [_GNUPlotPath, self.GenerateScript])),
    '', []);

  writeln(Utf8ToAnsi(Format('"%s" --persist -e "%s" ',
    [_GNUPlotPath, self.GenerateScript])));

  // do cleanup (temp files removal)

end;

constructor TSeriesList.Create;
begin
  ListSize := 0;
  SetLength(SeriesList, 0);
  SetLength(TitleList, 0);
  XLabel := '';
  YLabel := '';
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

function TSeriesList.GenerateScript: string;
var
  s: string;
begin
  Result := '';
end;

constructor TPlot.Create;
begin
  OverrideDefaultStyle := False;
  PlotType := ptDots; // 'histogram', 'lines', 'dots'
end;

function TPlot.GenerateScript: string;
var
  s, PlotTypeStr: string;
begin
  case PlotType of
    ptLines: PlotTypeStr := 'lines';
    ptDots: PlotTypeStr := 'dots';
    ptHistogram: PlotTypeStr := 'histogram';
  end;
  s := Format('''%s'' with %s', ['anu2.txt', PlotTypeStr]);
  Result := s;
end;

procedure TPlot.SetDataPoints(x: TDTMatrix);
begin
  if (x.Width = 1) or (x.Height = 1) then
  begin
    self.values := x;
  end;
end;

procedure TPlot.SetDataPoints(x, y: TDTMatrix);
var
  x_, y_: TDTMatrix;
begin
  if ((x_.Width = 1) or (x_.Height = 1)) and ((y_.Width = 1) or (y_.Height = 1)) then
  begin
    x_ := CopyMatrix(x);
    y_ := CopyMatrix(y);
    if x.Width > 1 then
      x_ := x_.T;
    if y.Width > 1 then
      y_ := y_.T;
    self.values := AppendColumns(x, y);
  end;
end;


end.
