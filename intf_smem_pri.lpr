library intf_smem_pri;

{$mode objfpc}{$H+}

uses
  classes,
  smem,
  sysutils;

type
  TCallback = procedure(hndl: Int32; data: Pointer); stdcall;

  TDataThread = class(TThread)
  public
    constructor Create;
  protected
    procedure Execute; override;
  end;

  TState = record
    smem: TSharedMemory;
    thrd: TDataThread;
    call: TCallback;
    hndl: Int32;
    rate: Int32;
    freq: array [0..7] of Int32;
  end;

  TInfo = record
    name: PChar;
    size: Int32;
    rate: array [0..2] of Single;
  end;

  PInfo = ^TInfo;

  TSettings = record
    hndl: Int32;
    size: Int32;
    rate: Int32;
    niu0: LongBool;
    call: TCallback;
    niu1: Pointer;
    niu2: Pointer;
    niu3: Pointer;
    niu4: Pointer;
  end;

  PSettings = ^TSettings;

var
  s: TState;
  p: array [0..7] of Pointer;
  data: TDataMemory;

constructor TDataThread.Create;
begin
  FreeOnTerminate := True;
  inherited Create(false);
end;

procedure TDataThread.Execute;
var
  i, j, l: Int32;
begin
  repeat
    if not s.smem.Wait then Continue;
    l := (512 shl s.rate) - 1;
    for i := 0 to 7 do
    begin
      for j := 0 to l do
      begin
        data[i, j, 0] := s.smem.data^[i, j, 0] * 1e5;
        data[i, j, 1] := s.smem.data^[i, j, 1] * 1e5;
      end;
    end;
    s.call(s.hndl, @p);
  until Terminated;
end;

procedure GetSdrInfo(info: PInfo); stdcall;
begin
  with info^ do
  begin
    name := PChar('SMEM Primary');
    size := 8;
    rate[0] := 48e3;
    rate[1] := 96e3;
    rate[2] := 192e3;
  end;
end;

procedure StartRx(settings: PSettings); stdcall;
var
  i: Int32;
begin
  s.call := settings^.call;
  s.hndl := settings^.hndl;
  s.rate := settings^.rate and 3;
  for i := 0 to 7 do p[i] := @data[i];
  with s.smem do
  begin
    Open(0);
    ctrl^.rate := s.rate;
    ctrl^.freq := s.freq;
  end;
  s.thrd := TDataThread.Create;
end;

procedure StopRx(); stdcall;
begin
  s.thrd.Terminate;
  Sleep(200);
  s.smem.Close;
end;

procedure SetRxFrequency(freq, i: Int32); stdcall;
begin
  s.freq[i] := freq;
  if s.smem.ctrl <> nil then s.smem.ctrl^.freq[i] := freq;
end;

procedure SetCtrlBits(bits: UInt8); stdcall;
begin

end;

function ReadPort(port: Int32): Int32; stdcall;
begin
  Result := 0;
end;

exports
  GetSdrInfo,
  StartRx,
  StopRx,
  SetRxFrequency,
  SetCtrlBits,
  ReadPort;

begin

end.
