{ The mandatory include unit for darkteal }
unit DTCore;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, dynlibs;

const
  {$IFDEF MSWINDOWS}
  { @exclude }
  BLAS_FILENAME = 'libopenblas.dll';
  {$ENDIF}
  {$IFDEF UNIX}
    {$IFDEF LINUX}
      BLAS_FILENAME = 'libopenblas.so';
    {$ENDIF}
    {$IFDEF DARWIN}
      BLAS_FILENAME = 'libopenblas.dylib';
    {$ENDIF}
  {$ENDIF}

type
  { Primitive type wrapper }
  TFloatVector = array of double;

  { Callback function type wrapper }
  TCallbackDouble = function(x: double): double;

  { @abstract(Darkteal matrix representation) }
  TDTMatrix = record
    val: TFloatVector;
    Width: longint;
    Height: longint;
    class operator Implicit(A: TFloatVector): TDTMatrix;
    class operator Explicit(A: TFloatVector): TDTMatrix;
    class operator Add(A, B: TDTMatrix): TDTMatrix;
    class operator Subtract(A, B: TDTMatrix): TDTMatrix; overload;
    class operator Subtract(A: TDTMatrix; x: double): TDTMatrix; overload;
    class operator Multiply(A: TDTMatrix; x: double): TDTMatrix; overload;
    class operator Multiply(x: double; A: TDTMatrix): TDTMatrix; overload;
    class operator Multiply(A, B: TDTMatrix): TDTMatrix; overload;
    class operator Divide(A: TDTMatrix; x: double): TDTMatrix; overload;
    class operator Divide(A, B: TDTMatrix): TDTMatrix; overload;
    function T: TDTMatrix;
    function GetRow(idx: integer): TDTMatrix;
    function GetColumn(idx: integer): TDTMatrix;
    function GetRange(row, col, Height, Width: longint): TDTMatrix;
    function Dot(A: TDTMatrix): TDTMatrix;
    function Apply(func: TCallbackDouble): TDTMatrix;
    function Sum(axis: integer = -1): TDTMatrix;
  end;


  { @exclude }
  CBLAS_ORDER = (CblasRowMajor = 101, CblasColMajor = 102);
  { @exclude }
  CBLAS_TRANSPOSE = (CblasNoTrans = 111, CblasTrans = 112, CblasConjTrans = 113);
  { @exclude }
  CBLAS_UPLO = (CblasUpper = 121, CblasLower = 122);
  { @exclude }
  CBLAS_DIAG = (CblasNonUnit = 131, CblasUnit = 132);
  { @exclude }
  LAPACK_ORDER = (LAPACKRowMajor = 101, LAPACKColMajor = 102);

  { @exclude }
  _dcopy = procedure(N: longint; X: TFloatVector; incX: longint;
    Y: TFloatVector; incY: longint); cdecl;
  { @exclude }
  _daxpy = procedure(N: longint; alpha: double; X: TFloatVector;
    incX: longint; Y: TFloatVector; incY: longint); cdecl;
  { @exclude }
  _dscal = procedure(N: longint; alpha: double; X: TFloatVector;
    incX: longint); cdecl;
  { @exclude }
  _dgemm = procedure(Order: CBLAS_ORDER; TransA: CBLAS_TRANSPOSE;
    TransB: CBLAS_TRANSPOSE; M: longint; N: longint; K: longint;
    alpha: double; A: TFloatVector; lda: longint; B: TFloatVector;
    ldb: longint; beta: double; C: TFloatVector; ldc: longint); cdecl;
  { @exclude }
  _dtbmv = procedure(order: CBLAS_ORDER; Uplo: CBLAS_UPLO;
    TransA: CBLAS_TRANSPOSE; Diag: CBLAS_DIAG; N: longint; K: longint;
    A: TFloatVector; lda: longint; X: TFloatVector; incX: longint); cdecl;
  { @exclude }
  _ddot = function(N: longint; X: TFloatVector; incX: longint;
    Y: TFloatVector; incY: longint): double; cdecl;
  { @exclude }
  _dasum = function(N: longint; X: TFloatVector; incX: longint): double; cdecl;

  // LAPACK interface

  { @exclude }
  _dgesvd = function(layout: LAPACK_ORDER; jobu: char; jobvt: char;
    m: longint; n: longint; A: TFloatVector; lda: longint; S: TFloatVector;
    U: TFloatVector; ldu: longint; VT: TFloatVector; ldvt: longint;
    superb: TFloatVector): longint; cdecl;
  { @exclude }
  _dgeev = function(layout: LAPACK_ORDER; jobvl: char; jobvr: char;
    n: longint; A: TFloatVector; lda: longint; wr: TFloatVector;
    wi: TFloatVector; vl: TFloatVector; ldvl: longint; vr: TFloatVector;
    ldvr: longint): longint; cdecl;

{ initialize the engine }
procedure DarkTealInit;

{ free the engine }
procedure DarkTealRelease;

{ helper function to print a matrix using stdout }
procedure PrintMatrix(M: TDTMatrix);

{ @exclude}
function CreateVector(size: integer; x: double): TFloatVector;

{ create a matrix filled with x

  @param(x A double value to fill the matrix) }
function CreateMatrix(row, col: integer; x: double): TDTMatrix; overload;

{ create a matrix with random values }
function CreateMatrix(row, col: integer): TDTMatrix; overload;

{ create a matrix filled with one }
function Ones(row, col: integer): TDTMatrix;

{ copying a matrix }
function CopyMatrix(M: TDTMatrix): TDTMatrix;

{ compute covariance matrix }
function Cov(X, Y: TDTMatrix): TDTMatrix;

{ Delete element in A.val with position pos.
  Should @bold(NOT) be used directly on a TDTMatrix. }
function DeleteElement(var A: TDTMatrix; pos: integer): TDTMatrix;

{ Get column of A from index idx.
  @param(A is a m by n TDTMatrix)
  @param(idx is an integer indicating the designated column index)
  @returns(m by 1 TDTMatrix)}
function GetColumn(A: TDTMatrix; idx: integer): TDTMatrix;

{ Get row of A from index idx.
  @param(A is a m by n TDTMatrix)
  @param(idx is an integer indicating the designated row index)
  @returns(1 by n TDTMatrix)}
function GetRow(A: TDTMatrix; idx: integer): TDTMatrix;
function Dot(A, B: TDTMatrix): TDTMatrix;
function Abs(x: double): double; overload;
function Abs(A: TDTMatrix): TDTMatrix; overload;
function Add(A, B: TDTMatrix): TDTMatrix;
function AppendColumns(A, B: TDTMatrix): TDTMatrix;
function AppendRows(A, B: TDTMatrix): TDTMatrix;
function InsertRowsAt(A, B: TDTMatrix; pos: integer): TDTMatrix;

{ Insert @code(B) into @code(A) at column index @code(pos).
  A and B must have the same height. }
function InsertColumnsAt(A, B: TDTMatrix; pos: integer): TDTMatrix;

function Subtract(A, B: TDTMatrix): TDTMatrix; overload;
function Subtract(A: TDTMatrix; x: double): TDTMatrix; overload;
function Multiply(A: TDTMatrix; x: double): TDTMatrix; overload;
function Multiply(A, B: TDTMatrix): TDTMatrix; overload;
function Diag(A: TDTMatrix): TDTMatrix; overload;
function Divide(A: TDTMatrix; x: double): TDTMatrix; overload;
function Divide(A, B: TDTMatrix): TDTMatrix; overload;
function Sum(A: TDTMatrix): double; overload;
function Sum(A: TDTMatrix; axis: integer): TDTMatrix; overload;
function IndexMax(A: TDTMatrix): double; overload;
function IndexMax(A: TDTMatrix; axis: integer): TDTMatrix; overload;
function Max(A: TDTMatrix): double; overload;
function Max(A: TDTMatrix; axis: integer): TDTMatrix; overload;
function Mean(A: TDTMatrix): double; overload;
function Mean(A: TDTMatrix; axis: integer): TDTMatrix; overload;
function Min(A: TDTMatrix): double; overload;
function Min(A: TDTMatrix; axis: integer): TDTMatrix; overload;
function PopRow(var A: TDTMatrix; pos: integer): TDTMatrix;
function Power(A: TDTMatrix; exponent: double): TDTMatrix; overload;
function Sqrt(x: double): double; overload;
function Exp(x: double): double; overload;
function Std(A: TDTMatrix; ddof: integer): double; overload;
function Std(A: TDTMatrix; axis: integer; ddof: integer): TDTMatrix; overload;
function TileDown(A: TDTMatrix; size: integer): TDTMatrix; overload;
function TileRight(A: TDTMatrix; size: integer): TDTMatrix; overload;

{ Get row of A from index idx.
  @param(A is a m by n TDTMatrix)
  @param(ddof is an integer indicating the degree of freedom.
         Setting ddof=1 will give unbiased estimator.)
  @returns(a double-valued scalar containing variance of A) }
function Variance(A: TDTMatrix; ddof: integer): double; overload;

{ Get row of A from index idx.
  @param(A is a m by n TDTMatrix.)
  @param(axis is the axis along which the variance is calculated.)
  @param(ddof is an integer indicating the degree of freedom.
         Setting ddof=1 will give unbiased estimator.)
  @returns(a 1 by m TDTMatrix (if axis=0) or an n by 1 TDTMatrix (if axis=1)
           containing list of variances with respect to colums (or rows.)) }
function Variance(A: TDTMatrix; axis: integer; ddof: integer): TDTMatrix; overload;

{ Transform each element in @code(A) using a real-valued function @code(func) }
function Apply(func: TCallbackDouble; A: TDTMatrix): TDTMatrix;

{ Create a matrix from a CSV file.

  Please note that @name can only load a CSV containing numeric values only.
  The values are stored as floating point numbers. }
function TDTMatrixFromCSV(f: string): TDTMatrix;

{ Delete several elements in A.val with position pos.
  Should @bold(NOT) be used directly on a TDTMatrix. }
procedure DeleteElements(var A: TDTMatrix; pos, amount: integer);

{ Set the values in TDTMatrix A at column idx with the values of B }
procedure SetColumn(var A: TDTMatrix; B: TDTMatrix; idx: integer);

{ Swap values in column idx1 with the values in column idx2 }
procedure SwapColumns(var A: TDTMatrix; idx1, idx2: integer);

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

var
  { @exclude }
  blas_dcopy: _dcopy;
  { @exclude }
  blas_daxpy: _daxpy;
  { @exclude }
  blas_ddot: _ddot;
  { @exclude }
  blas_dscal: _dscal;
  { @exclude }
  blas_dgemm: _dgemm;
  { @exclude }
  blas_dtbmv: _dtbmv;
  { @exclude }
  blas_dasum: _dasum;
  { @exclude }
  LAPACKE_dgesvd: _dgesvd;
  { @exclude }
  LAPACKE_dgeev: _dgeev;
  { @exclude }
  libHandle: TLibHandle;

implementation

uses Math;

procedure DarkTealInit;
begin
  Assert(FileExists(BLAS_FILENAME), BLAS_FILENAME + ' cannot be found.');
  libHandle := LoadLibrary(BLAS_FILENAME);
  Pointer(@blas_dcopy) := GetProcedureAddress(libHandle, 'cblas_dcopy');
  Pointer(@blas_daxpy) := GetProcedureAddress(libHandle, 'cblas_daxpy');
  Pointer(@blas_ddot) := GetProcedureAddress(libHandle, 'cblas_ddot');
  Pointer(@blas_dscal) := GetProcedureAddress(libHandle, 'cblas_dscal');
  Pointer(@blas_dgemm) := GetProcedureAddress(libHandle, 'cblas_dgemm');
  Pointer(@blas_dtbmv) := GetProcedureAddress(libHandle, 'cblas_dtbmv');
  Pointer(@blas_dasum) := GetProcedureAddress(libHandle, 'cblas_dasum');
  Pointer(@LAPACKE_dgesvd) := GetProcedureAddress(libHandle, 'LAPACKE_dgesvd');
  Pointer(@LAPACKE_dgeev) := GetProcedureAddress(libHandle, 'LAPACKE_dgeev');
end;

procedure DarkTealRelease;
begin
  UnloadLibrary(libHandle);
  blas_dcopy := nil;
  blas_daxpy := nil;
  blas_ddot := nil;
  blas_dscal := nil;
  blas_dgemm := nil;
  blas_dtbmv := nil;
  blas_dasum := nil;
  LAPACKE_dgesvd := nil;
end;

class operator TDTMatrix.Explicit(A: TFloatVector): TDTMatrix;
begin
  Result.val := A;
end;

class operator TDTMatrix.Implicit(A: TFloatVector): TDTMatrix;
begin
  Result.val := A;
end;

class operator TDTMatrix.Add(A, B: TDTMatrix): TDTMatrix;
begin
  Result := DTCore.Add(A, B);
end;

class operator TDTMatrix.Subtract(A, B: TDTMatrix): TDTMatrix;
begin
  Result := DTCore.Subtract(A, B);
end;

class operator TDTMatrix.Subtract(A: TDTMatrix; x: double): TDTMatrix;
begin
  Result := DTCore.Subtract(A, x);
end;

class operator TDTMatrix.Multiply(A: TDTMatrix; x: double): TDTMatrix;
begin
  Result := DTCore.Multiply(A, x);
end;

class operator TDTMatrix.Multiply(x: double; A: TDTMatrix): TDTMatrix;
begin
  Result := DTCore.Multiply(A, x);
end;

class operator TDTMatrix.Multiply(A, B: TDTMatrix): TDTMatrix;
begin
  Result := DTCore.Multiply(A, B);
end;

class operator TDTMatrix.Divide(A: TDTMatrix; x: double): TDTMatrix;
begin
  Result := DTCore.Divide(A, x);
end;

class operator TDTMatrix.Divide(A, B: TDTMatrix): TDTMatrix;
begin
  Result := DTCore.Divide(A, B);
end;

function TDTMatrix.T: TDTMatrix;
var
  i, j, idx: longint;
begin
  Result := CopyMatrix(self);
  Result.Width := self.Height;
  Result.Height := self.Width;
  idx := 0;


  for i := 0 to self.Width - 1 do
    for j := 0 to self.Height - 1 do
    begin
      Result.val[idx] := self.val[j * self.Width + i];
      Inc(idx);
    end;

end;

function TDTMatrix.GetColumn(idx: integer): TDTMatrix;
begin
  Result := DTCore.GetColumn(self, idx);
end;

function TDTMatrix.GetRow(idx: integer): TDTMatrix;
begin
  Result := DTCore.GetRow(self, idx);
end;

function TDTMatrix.GetRange(row, col, Height, Width: longint): TDTMatrix;
var
  i, j, idx: integer;
begin
  Result.Width := Width;
  Result.Height := Height;
  SetLength(Result.val, Width * Height);
  idx := 0;
  for i := row to row + Height - 1 do
    for j := col to col + Width - 1 do
    begin
      Result.val[idx] := self.val[(i) * self.Width + (j)];
      Inc(idx);
    end;
end;

function TDTMatrix.Dot(A: TDTMatrix): TDTMatrix;
begin
  Result := DTCore.Dot(Self, A);
end;

function TDTMatrix.Apply(func: TCallbackDouble): TDTMatrix;
var
  i: longint;
begin
  Result := DTCore.CopyMatrix(self);
  for i := 0 to Length(Result.val) - 1 do
    Result.val[i] := func(self.val[i]);
end;

function TDTMatrix.Sum(axis: integer = -1): TDTMatrix;
begin
  Result := DTCore.Sum(self, axis);
end;

procedure PrintMatrix(M: TDTMatrix);
var
  i, j: integer;
begin
  for i := 0 to M.Height - 1 do
  begin
    Write('  ');
    for j := 0 to M.Width - 1 do
    begin
      Write(M.val[i * M.Width + j]: 5: 3, ' ');
    end;
    WriteLn;
  end;
  WriteLn;
end;

function CreateVector(size: integer; x: double): TFloatVector;
var
  i: integer;
  res: TFloatVector;
begin
  SetLength(res, size);
  for i := 0 to size - 1 do
    res[i] := x;
  Result := res;
end;

function CreateMatrix(row, col: integer; x: double): TDTMatrix;
var
  i: longint;
begin
  Result.Width := col;
  Result.Height := row;
  SetLength(Result.val, row * col);
  for i := 0 to (row * col) - 1 do
    Result.val[i] := x;
end;

function CreateMatrix(row, col: integer): TDTMatrix;
var
  i: longint;
begin
  Result.Width := col;
  Result.Height := row;
  SetLength(Result.val, row * col);
  for i := 0 to (row * col) - 1 do
    Result.val[i] := Random();
end;

function Ones(row, col: integer): TDTMatrix;
begin
  Result := createMatrix(row, col, 1);
end;

function Cov(X, Y: TDTMatrix): TDTMatrix;
var
  X_, Xc, Yc, C: TDTMatrix;
begin
  X_ := CopyMatrix(X);
  Xc := X_ - Mean(X_, 1);
  Result := Xc.Dot(Xc.T) / (X.Width - 1);
end;

function CopyMatrix(M: TDTMatrix): TDTMatrix;
begin
  SetLength(Result.val, M.Width * M.Height);
  Result.Width := M.Width;
  Result.Height := M.Height;
  blas_dcopy(M.Width * M.Height, M.val, 1, Result.val, 1);
end;

function Dot(A, B: TDTMatrix): TDTMatrix;
begin
  Result := nil;
  SetLength(Result.val, A.Height * B.Width);
  blas_dgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans,
    A.Height, B.Width, B.Height, // m, n, k
    1, // alpha
    A.val, B.Height,
    B.val, B.Width,
    1, // beta
    Result.val, B.Width
    );
  Result.Height := A.Height;
  Result.Width := B.Width;
end;

function Multiply(A: TDTMatrix; x: double): TDTMatrix;
begin
  Result := DTCore.CopyMatrix(A);
  blas_dscal(A.Height * A.Width, x, Result.val, 1);
end;

function Multiply(A, B: TDTMatrix): TDTMatrix;
begin
  Result := CopyMatrix(B);
  blas_dtbmv(CblasColMajor, CblasUpper, CblasNoTrans, CblasNonUnit,
    Length(A.val), 0, A.val, 1, Result.val, 1);
end;

function Diag(A: TDTMatrix): TDTMatrix;
var
  i, j: integer;
  c: integer;
begin
  Result := CreateMatrix(Length(A.val), Length(A.val), 0);
  c := 0;
  for i := 0 to Result.Height - 1 do
    Result.val[i * Result.Width + i] := A.val[i];
end;

{ Should NOT be used directly on a TDTMatrix }
function DeleteElement(var A: TDTMatrix; pos: integer): TDTMatrix;
var
  l, j: integer;
begin
  l := length(A.val);
  if pos > l - 1 then
    exit
  else if pos = l - 1 then
  begin
    Setlength(A.val, l - 1);
    exit;
  end;
  for j := pos to l - 2 do
    A.val[j] := A.val[j + 1];
  SetLength(A.val, l - 1);
end;

procedure DeleteElements(var A: TDTMatrix; pos, amount: integer);
var
  i: integer;
begin
  for i := pos + amount to High(A.val) do
  begin
    A.val[i - amount] := A.val[i];
  end;
  SetLength(A.val, Length(A.val) - amount);
end;

function Divide(A: TDTMatrix; x: double): TDTMatrix;
begin
  Result := DTCore.CopyMatrix(A);
  blas_dscal(A.Height * A.Width, 1 / x, Result.val, 1);
end;

function Divide(A, B: TDTMatrix): TDTMatrix;
var
  i: integer;
begin
  Result.Width := A.Width;
  Result.Height := A.Height;
  SetLength(Result.val, length(A.val));
  for i := 0 to length(Result.val) - 1 do
    Result.val[i] := A.val[i] / B.val[i];
end;

function GetColumn(A: TDTMatrix; idx: integer): TDTMatrix;
var
  i: integer;
begin
  SetLength(Result.val, A.Height);
  Result.Height := A.Height;
  Result.Width := 1;
  for i := 0 to Length(Result.val) - 1 do
    Result.val[i] := A.val[i * A.Width + idx];
end;

function GetRow(A: TDTMatrix; idx: integer): TDTMatrix;
var
  i: integer;
begin
  SetLength(Result.val, A.Width);
  Result.Height := 1;
  Result.Width := A.Width;
  for i := 0 to Length(Result.val) - 1 do
    Result.val[i] := A.val[idx * A.Width + i];
end;

function Sum(A: TDTMatrix): double;
begin
  //Result.Width := 1;
  //Result.Height := 1;
  //SetLength(Result.val, 1);
  Result := blas_dasum(A.Width * A.Height, A.val, 1);
end;

function Sum(A: TDTMatrix; axis: integer): TDTMatrix;
var
  i: integer;
begin
  if axis = 0 then
  begin
    SetLength(Result.val, A.Width);
    Result.Height := 1;
    Result.Width := A.Width;
    for i := 0 to A.Width - 1 do
      Result.val[i] := DTCore.Sum(GetColumn(A, i));
  end
  else if axis = 1 then
  begin
    SetLength(Result.val, A.Height);
    Result.Height := A.Height;
    Result.Width := 1;
    for i := 0 to A.Height - 1 do
      Result.val[i] := DTCore.Sum(GetRow(A, i));
  end;
end;

function IndexMax(A: TDTMatrix): double;
var
  i: integer;
  CurMax, CurIdxMax: double;
begin
  CurMax := -1.0 / 0.0;
  CurIdxMax := 0;
  for i := 0 to Length(A.val) - 1 do
    if A.val[i] > CurMax then
    begin
      CurMax := A.val[i];
      CurIdxMax := i;
    end;
  Result := CurIdxMax;
end;

function IndexMax(A: TDTMatrix; axis: integer): TDTMatrix;
var
  i: integer;
begin
  if axis = 0 then
  begin
    SetLength(Result.val, A.Width);
    Result.Height := 1;
    Result.Width := A.Width;
    for i := 0 to A.Width - 1 do
      Result.val[i] := DTCore.IndexMax(GetColumn(A, i));
  end
  else
  begin
    SetLength(Result.val, A.Height);
    Result.Height := A.Height;
    Result.Width := 1;
    for i := 0 to A.Height - 1 do
      Result.val[i] := DTCore.IndexMax(GetRow(A, i));
  end;
end;

function Max(A: TDTMatrix): double;
var
  i: integer;
  CurMax: double;
begin
  CurMax := -1.0 / 0.0;
  for i := 0 to Length(A.val) - 1 do
    if A.val[i] > CurMax then
      CurMax := A.val[i];
  Result := CurMax;
end;

function Max(A: TDTMatrix; axis: integer): TDTMatrix;
var
  i: integer;
begin
  if axis = 0 then
  begin
    SetLength(Result.val, A.Width);
    Result.Height := 1;
    Result.Width := A.Width;
    for i := 0 to A.Width - 1 do
      Result.val[i] := DTCore.Max(GetColumn(A, i));
  end
  else
  begin
    SetLength(Result.val, A.Height);
    Result.Height := A.Height;
    Result.Width := 1;
    for i := 0 to A.Height - 1 do
      Result.val[i] := DTCore.Max(GetRow(A, i));
  end;
end;

function Mean(A: TDTMatrix): double;
var
  i: integer;
  tot: double;
begin
  tot := 0;
  for i := 0 to Length(A.val) - 1 do
    tot := tot + A.val[i];
  Result := tot / Length(A.val);
end;

function Mean(A: TDTMatrix; axis: integer): TDTMatrix;
var
  i: integer;
begin
  if axis = 0 then
  begin
    SetLength(Result.val, A.Width);
    Result.Height := 1;
    Result.Width := A.Width;
    for i := 0 to A.Width - 1 do
      Result.val[i] := DTCore.Mean(GetColumn(A, i));
  end
  else
  begin
    SetLength(Result.val, A.Height);
    Result.Height := A.Height;
    Result.Width := 1;
    for i := 0 to A.Height - 1 do
      Result.val[i] := DTCore.Mean(GetRow(A, i));
  end;
end;

function Min(A: TDTMatrix): double;
var
  i: integer;
  CurMin: double;
begin
  CurMin := 1.0 / 0.0;
  for i := 0 to Length(A.val) - 1 do
    if A.val[i] < CurMin then
      CurMin := A.val[i];
  Result := CurMin;
end;

function Min(A: TDTMatrix; axis: integer): TDTMatrix;
var
  i: integer;
begin
  if axis = 0 then
  begin
    SetLength(Result.val, A.Width);
    Result.Height := 1;
    Result.Width := A.Width;
    for i := 0 to A.Width - 1 do
      Result.val[i] := DTCore.Min(GetColumn(A, i));
  end
  else
  begin
    SetLength(Result.val, A.Height);
    Result.Height := A.Height;
    Result.Width := 1;
    for i := 0 to A.Height - 1 do
      Result.val[i] := DTCore.Min(GetRow(A, i));
  end;
end;

{ Removing pos-th row of an array. Take the row as the returned value.
  The implementation could be better. }
function PopRow(var A: TDTMatrix; pos: integer): TDTMatrix;
var
  i: integer;
begin
  Result := A.GetRow(pos);
  DeleteElements(A, pos * A.Width, A.Width);
  A.Height := A.Height - 1;
end;

function Power(A: TDTMatrix; exponent: double): TDTMatrix;
var
  i: integer;
begin
  Result := CopyMatrix(A);
  for i := 0 to High(A.val) do
    Result.val[i] := Math.power(Result.val[i], exponent);
end;

function Exp(x: double): double;
begin
  Result := system.exp(x);
end;

procedure SetColumn(var A: TDTMatrix; B: TDTMatrix; idx: integer);
var
  i: integer;
begin
  for i := 0 to B.Height - 1 do
    A.val[i * A.Width + idx] := B.val[i];
end;

procedure SwapColumns(var A: TDTMatrix; idx1, idx2: integer);
var
  tmp: TDTMatrix;
begin
  tmp := A.GetColumn(idx1);
  SetColumn(A, A.GetColumn(idx2), idx1);
  SetColumn(A, tmp, idx2);
end;

function Sqrt(x: double): double;
begin
  Result := system.sqrt(x);
end;

function Std(A: TDTMatrix; ddof: integer): double;
begin
  Result := sqrt(Variance(A, ddof));
end;

function Std(A: TDTMatrix; axis: integer; ddof: integer): TDTMatrix;
begin
  Result := Apply(@sqrt, variance(A, axis, ddof));
end;

function TileDown(A: TDTMatrix; size: integer): TDTMatrix; overload;
var
  i, j: integer;
begin
  assert(A.Height = 1, 'Only matrix with height equals to 1 can be tiled down');
  Result.Width := A.Width;
  Result.Height := size;
  SetLength(Result.val, A.Width * A.Height * size);
  for i := 0 to size - 1 do
  begin
    for j := 0 to A.Width - 1 do
      Result.val[i * Result.Width + j] := A.val[j];
  end;
end;

function TileRight(A: TDTMatrix; size: integer): TDTMatrix; overload;
begin
  assert(A.Width = 1, 'Only matrix with width equals to 1 can be tiled down');
  Result := TileDown(A.T, size).T;
end;

function Variance(A: TDTMatrix; ddof: integer): double;
begin
  Result := Sum(Power(A - Mean(A), 2)) / (Length(A.val) - ddof);
end;

function Variance(A: TDTMatrix; axis: integer; ddof: integer): TDTMatrix;
var
  i: integer;
begin
  if axis = 0 then
  begin
    SetLength(Result.val, A.Width);
    Result.Height := 1;
    Result.Width := A.Width;
    for i := 0 to A.Width - 1 do
      Result.val[i] := DTCore.Variance(GetColumn(A, i), ddof);
    //Variance(GetColumn(A, i), ddof);
  end
  else
  begin
    SetLength(Result.val, A.Height);
    Result.Height := A.Height;
    Result.Width := 1;
    for i := 0 to A.Height - 1 do
      Result.val[i] := DTCore.Variance(GetRow(A, i), ddof);
  end;
end;

function Apply(func: TCallbackDouble; A: TDTMatrix): TDTMatrix;
var
  i: longint;
begin
  Result := DTCore.CopyMatrix(A);
  for i := 0 to Length(Result.val) - 1 do
    Result.val[i] := func(A.val[i]);
end;

function Abs(x: double): double;
begin
  if x < 0 then
    Result := x * -1
  else
    Result := x;
end;

function Abs(A: TDTMatrix): TDTMatrix;
begin
  Result := Apply(@Abs, A);
end;

function Add(A, B: TDTMatrix): TDTMatrix;
begin
  Result := CopyMatrix(B);
  // handle broadcasting better next time :(
  if B.Height = 1 then
    Result := TileDown(B, A.Height);
  if B.Width = 1 then
    Result := TileRight(B, A.Width);
  blas_daxpy(Length(A.val), 1, A.val, 1, Result.val, 1);
end;

function AppendColumns(A, B: TDTMatrix): TDTMatrix;
begin
  Result := InsertColumnsAt(A, B, A.Width);
end;

function AppendRows(A, B: TDTMatrix): TDTMatrix;
begin
  Result := InsertRowsAt(A, B, A.Height);
end;

function InsertRowsAt(A, B: TDTMatrix; pos: integer): TDTMatrix;
var
  i, w: integer;
begin
  if A.Height > 0 then
  begin
    Result := CopyMatrix(A);
    w := B.Width * B.Height;
    SetLength(Result.val, Length(Result.val) + w);
    { shift elements }
    for i := High(Result.val) downto High(Result.val) - w + 1 do
    begin
      Result.val[i] := Result.val[i - w];
    end;
    { fill the empty space with the new array }
    for i := 0 to Length(B.val) - 1 do
      Result.val[i + pos * A.Width] := B.val[i];
    Result.Height := Result.Height + B.Height;
  end
  else
  begin
    Result := CopyMatrix(B);
    Result.Height := B.Height;
  end;
end;

function InsertColumnsAt(A, B: TDTMatrix; pos: integer): TDTMatrix;
begin
  Result := InsertRowsAt(A.T, B.T, pos).T;
end;

function Subtract(A, B: TDTMatrix): TDTMatrix;
var
  i: longint;
  B_: TDTMatrix;
begin
  Result := CopyMatrix(A);
  B_ := CopyMatrix(B);
  if B.Height = 1 then
    B_ := TileDown(B, A.Height);
  if B.Width = 1 then
    B_ := TileRight(B, A.Width);
  for i := 0 to Length(Result.val) - 1 do
    Result.val[i] := A.val[i] - B_.val[i];
end;

function Subtract(A: TDTMatrix; x: double): TDTMatrix;
begin
  Result := Subtract(A, CreateMatrix(A.Height, A.Width, x));
end;

function TDTMatrixFromCSV(f: string): TDTMatrix;
var
  tfIn: TextFile;
  s, row: string;
  i, idx, cntRow, cntCol: integer;
  isRowSet: boolean = False;
begin
  assignfile(tfIn, f);
  try
    Reset(tfIn); // open file
    cntRow := 0;
    idx := 0;
    s := '';
    SetLength(Result.val, 1);
    while not EOF(tfIn) do
    begin
      Readln(tfIn, row);

      Inc(cntRow);
      cntCol := 0;
      for i := 1 to Length(row) do
      begin
        if (row[i] <> ',') then
          s := s + row[i]
        else
        begin
          Inc(cntCol);
          Result.val[idx] := StrToFloat(s);

          s := '';
          Inc(idx);
          setLength(Result.val, idx + 1);
        end;
        if i = Length(row) then
        begin
          Inc(cntCol);
          Result.val[idx] := StrToFloat(s);
          Inc(idx);
          setLength(Result.val, idx + 1);
        end;
      end;
      s := '';
    end;
    CloseFile(tfIn); // close file
    setLength(Result.val, Length(Result.val) - 1);
    // idk, there is always trailing value :(
  except
    on E: EInOutError do
      writeln('File handling error occurred. Details: ', E.Message);
  end;
  Result.Width := cntCol;
  Result.Height := cntRow;
end;

initialization
  DarkTealInit;

finalization
  DarkTealRelease;

end.
