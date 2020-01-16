{
 This file is part of "noe" library.

 Noe library. Copyright (C) 2020 Aria Ghora Prabono.

 This unit provides an interface to OpenBLAS library.
}

unit noe.backend.blas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dynlibs, noe.core;

type
  CBLAS_ORDER = (CblasRowMajor = 101, CblasColMajor = 102);
  CBLAS_TRANSPOSE = (CblasNoTrans = 111, CblasTrans = 112, CblasConjTrans = 113);
  CBLAS_UPLO = (CblasUpper = 121, CblasLower = 122);
  CBLAS_DIAG = (CblasNonUnit = 131, CblasUnit = 132);
  LAPACK_ORDER = (LAPACKRowMajor = 101, LAPACKColMajor = 102);

  TFuncDgemm = procedure(Order: CBLAS_ORDER; TransA: CBLAS_TRANSPOSE;
    TransB: CBLAS_TRANSPOSE; M: longint; N: longint; K: longint;
    alpha: double; A: TFloatVector; lda: longint; B: TFloatVector;
    ldb: longint; beta: double; C: TFloatVector; ldc: longint); cdecl;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

var
  blas_dgemm: TFuncDgemm;

  libHandle: THandle = dynlibs.NilHandle;

function __MatMul(A, B: TTensor): TTensor;

implementation

function __MatMul(A, B: TTensor): TTensor;
begin
  Result := TTensor.Create;
  SetLength(Result.val, A.Shape[0] * B.Shape[1]);
  blas_dgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans,
    A.Shape[0], B.Shape[1], B.Shape[0], // m, n, k
    1, // alpha
    A.val, B.Shape[0],
    B.val, B.Shape[1],
    1, // beta
    Result.val, B.Shape[1]
    );
  Result.Reshape([A.Shape[0], B.Shape[1]]);
end;

initialization
  Assert(FileExists('libopenblas.dll'), 'Cannot load libopenblas.dll');
  libHandle := LoadLibrary('libopenblas.dll');

  Assert(libHandle <> dynlibs.NilHandle, 'Failed loading libopenblas.dll');
  blas_dgemm := TFuncDgemm(GetProcedureAddress(libHandle, 'cblas_dgemm'));

  {$IFDEF BLAS}
  writeln('blasss');
  {$ENDIF}

finalization
  blas_dgemm := nil;

end.

