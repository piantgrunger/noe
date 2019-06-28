{ This unit facilitates the plotting functionality for darkteal.
  Gnuplot is used as the backend. }

unit DTPlot;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, DTCore;

var
  s: ansistring;
  { Hold global count of created plots }
  GlobalPlotCount: integer;

type

  TPlotType = (ptBoxes, ptLines, ptPoints, ptHistogram);

  { @abstract(A record containing plot style) }
  TPlotStyle = record
    LineType: integer;
    LineColor: string;
    LineWidth: integer;
    PointType: integer;
    PointSize: integer;
  end;

  { @abstract(A class that holds information of data points to plot, including its style) }
  TPlot = class(TObject)
    PlotLabel: string;
    PlotStyle: TPlotStyle;
    Title: string;
    PlotType: TPlotType;
    OverrideDefaultStyle: boolean;
  public
    constructor Create;
    { Set the data points to plot
      @param(x only accepts TDTMatrix with size of 1 by m or m by 1) }
    procedure SetDataPoints(x: TDTMatrix); overload;
    { Set the data points to plot (x axis against y axis) }
    procedure SetDataPoints(x, y: TDTMatrix); overload;
  private
    Values: TDTMatrix;
    FileName: string;
    procedure WriteDataStringTableToFile;
    procedure RemoveDataStringTableFile;
    function GenerateScript: string;
  end;

  { @abstract(A class that holds information of a single figure) }
  TFigure = class(TObject)
    Title: string;
    XLabel: string;
    YLabel: string;
  public
    constructor Create;
    procedure AddPlot(Plot: TPlot);
    procedure Show;
  private
    PlotList: TList;
    procedure CleanDataFile;
    procedure GenerateDataFile;
    function GenerateScript: string;
  end;


{ Initialize plotting functionality by passing gnuplot executable path }
procedure DTPlotInit(GNUplotPath: string);

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

function IsDTPlotReady: boolean;
begin
  Result := True;
  if not _GNUPlotInitialized then
  begin
    WriteLn('GNU Plot has not been configured properly.');
    Result := False;
  end;
end;

procedure DTPlotInit(GNUplotPath: string);
begin
  _GNUPlotPath := GNUplotPath;
  _GNUPlotTerminal := 'qt';
  if FileExists(GNUplotPath) then
    _GNUPlotInitialized := True
  else
    WriteLn('GNU Plot executable is not found.');
end;

constructor TFigure.Create;
begin
  PlotList := TList.Create;
end;

procedure TFigure.CleanDataFile;
begin

end;

procedure TFigure.GenerateDataFile;
var
  i: integer;
begin

end;

function TFigure.GenerateScript: string;
var
  s, script: string;
  i: integer;
begin
  s := '' + sLineBreak;
  s := s + 'set terminal %s title ''%s'';' + sLineBreak;
  s := s + 'set xlabel ''' + self.XLabel + ''';' + sLineBreak;
  s := s + 'set ylabel ''' + self.YLabel + ''';' + sLineBreak;
  s := s + 'do for [i=1:64] {set style line i linewidth 2};' + sLineBreak;

  s := s + 'plot ';
  for i := 0 to PlotList.Count - 1 do
  begin
    s := s + TPlot(PlotList.items[i]).GenerateScript;
    if i < PlotList.Count - 1 then
      s := s + ',';
  end;
  s := s + ';';
  script := Format(s, [_GNUPlotTerminal, Title]);
  Result := script;
end;

procedure TFigure.AddPlot(Plot: TPlot);
begin
  PlotList.Add(Plot);
end;

procedure TFigure.Show;
var
  i: integer;
begin
  { Generate temp files for each plot }
  for i := 0 to PlotList.Count - 1 do
    TPlot(PlotList.Items[i]).WriteDataStringTableToFile;

  if IsDTPlotReady then
  begin
    ExecuteProcess(Utf8ToAnsi(Format('%s --persist -e "%s" ',
      [_GNUPlotPath, self.GenerateScript])),
      '', []);

  end;

  { do cleanup (temp files removal) }
  for i := 0 to PlotList.Count - 1 do
    TPlot(PlotList.Items[i]).RemoveDataStringTableFile;

end;

constructor TPlot.Create;
begin
  OverrideDefaultStyle := False;
  PlotType := ptPoints; // 'histogram', 'lines', 'dots'
  Inc(GlobalPlotCount);
  FileName := Format('_DTPLOT_TMP_%d.tmp', [GlobalPlotCount]);

  { default style (for overriding) }
  PlotStyle.LineType := 1;
  PlotStyle.LineColor := '#000000';
  PlotStyle.LineWidth := 2;
  PlotStyle.PointType := 7;
  PlotStyle.PointSize := 1;
end;

procedure TPlot.RemoveDataStringTableFile;
begin
  if FileExists(self.FileName) then
    DeleteFile(self.FileName);
end;

procedure TPlot.WriteDataStringTableToFile;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Text := self.Values.ToStringTable;
  sl.SaveToFile(self.FileName);
end;

function TPlot.GenerateScript: string;
var
  s, style, PlotTypeStr: string;
begin
  case PlotType of
    ptLines: PlotTypeStr := 'lines';
    ptPoints: PlotTypeStr := 'points';
    ptHistogram: PlotTypeStr := 'histogram';
    ptBoxes: PlotTypeStr := 'boxes';
  end;

  if not OverrideDefaultStyle then
    style := ''
  else
  begin
    style := Format('linetype %d linecolor ''%s'' linewidth %d pointtype %d pointsize %d',
      [PlotStyle.LineType, PlotStyle.LineColor, PlotStyle.LineWidth,
      PlotStyle.PointType, PlotStyle.PointSize]);
  end;
  s := Format('''%s'' title ''%s''with %s %s', [FileName, Title, PlotTypeStr, style]);
  Result := s;
end;

procedure TPlot.SetDataPoints(x: TDTMatrix);
var
  x_: TDTMatrix;
begin
  if (x.Width = 1) or (x.Height = 1) then
  begin
    x_ := CopyMatrix(x);
    if x.Width > 1 then
      x_ := x_.T;
    self.Values := x_;
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
    self.Values := AppendColumns(x, y);
  end;
end;

initialization
  GlobalPlotCount := 0;

end.
