{ This unit facilitates the bridging between darkteal and Gnuplot }

unit DTGNUPlot;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, DTCore;

var
  s: ansistring;

type

  TPlotType = (ptLines, ptPoints, ptHistogram);

  { @abstract(A class that holds information of data points to plot, including its style) }
  TPlot = class(TObject)
    PlotLabel: string;
    Title: string;
    PlotType: TPlotType;
    OverrideDefaultStyle: boolean;
  public
    constructor Create;
    procedure SetDataPoints(x: TDTMatrix); overload;
    procedure SetDataPoints(x, y: TDTMatrix); overload;
  private
    values: TDTMatrix;
    function GenerateScript: string;
  end;

  { @abstract(A class that holds information of a single figure) }
  TFigure = class(TObject)
    Title: string;
    XLabel: string;
    YLabel: string;
    PlotList: TList;
  public
    constructor Create;
    procedure AddPlot(Plot: TPlot);
    procedure Show;
  private
    function GenerateScript: string;
  end;



procedure DTGNUPlotInit(GNUplotPath: string);
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


procedure Plot(x: TDTMatrix; Title: string);
var
  script, fn: string;
  sl: TStringList;
  s: string = '';
begin
  if IsDTGNUPlotReady then
  begin

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
  begin
    s := s + TPlot(PlotList.items[i]).GenerateScript;
    if i < PlotList.Count - 1 then
      s := s + ',';
  end;
  script := Format(s, [_GNUPlotTerminal, Title]);
  Result := script;
end;

procedure TFigure.AddPlot(Plot: TPlot);
begin
  PlotList.Add(Plot);
end;

procedure TFigure.Show;
begin
  if IsDTGNUPlotReady then
  begin
    ExecuteProcess(Utf8ToAnsi(Format('%s --persist -e "%s" ',
      [_GNUPlotPath, self.GenerateScript])),
      '', []);

  end;

  //writeln(Utf8ToAnsi(Format('"%s" --persist -e "%s" ',
  //  [_GNUPlotPath, self.GenerateScript])));

  // do cleanup (temp files removal)

end;

constructor TPlot.Create;
begin
  OverrideDefaultStyle := False;
  PlotType := ptPoints; // 'histogram', 'lines', 'dots'
end;

function TPlot.GenerateScript: string;
var
  s, PlotTypeStr: string;
begin
  case PlotType of
    ptLines: PlotTypeStr := 'lines';
    ptPoints: PlotTypeStr := 'points';
    ptHistogram: PlotTypeStr := 'histogram';
  end;
  s := Format('''%s'' title ''%s''with %s', ['anu2.txt', Title, PlotTypeStr]);
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
