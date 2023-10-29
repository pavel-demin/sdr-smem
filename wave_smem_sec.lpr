program wave_smem_sec;

{$mode objfpc}{$H+}

uses
  mmsystem,
  smem,
  sysutils,
  windows;

var
  s: TSharedMemory;
  caps: TWaveOutCapsA;
  wfx: TWaveFormatEx;
  hndl: THandle;
  open: Boolean;
  device, status: Int32;
  i, l: Int32;
  chan, rate: Int32;
  hsig: array [0..31] of TWaveHdr;
  signal: array [0..31, 0..4095, 0..1] of Single;

function OpenDevice: Boolean;
var
  i, size: Int32;
begin
  wfx.wFormatTag := 3;
  wfx.nChannels := 2;
  wfx.nSamplesPerSec := 48000 shl rate;
  wfx.nAvgBytesPerSec := 384000 shl rate;
  wfx.nBlockAlign := 8;
  wfx.wBitsPerSample := 32;
  wfx.cbSize := 0;
  try
    status := waveOutOpen(@hndl, device, @wfx, 0, 0, CALLBACK_NULL);
    WinCheck(status = MMSYSERR_NOERROR);
  except
    Result := False;
    Exit;
  end;
  size := 512 shl rate;
  for i := 0 to High(hsig) do
  begin
    hsig[i].lpData := @signal[i];
    hsig[i].dwBufferLength := size * 8;
    waveOutPrepareHeader(hndl, @hsig[i], SizeOf(TWaveHdr));
    waveOutWrite(hndl, @hsig[i], sizeof(TWaveHdr));
  end;
  open := True;
  Result := True;
end;

procedure CloseDevice;
var
  i: Int32;
begin
  if not open then Exit;
  for i := 0 to High(hsig) do
  begin
    waveOutUnprepareHeader(hndl, @hsig[i], SizeOf(TWaveHdr));
  end;
  waveOutClose(hndl);
  open := False;
end;

begin
  if ParamCount() <> 2 then
  begin
    WriteLn('Usage: wave_smem_sec.exe device channel');
    WriteLn('Available devices:');
    l := waveOutGetNumDevs - 1;
    for i := 0 to l do
    begin
      if waveOutGetDevCaps(i, @caps, SizeOf(Caps)) = MMSYSERR_NOERROR then
      begin
        WriteLn(i:3, ' - ', caps.szPname);
      end;
    end;
    Exit;
  end;

  try
    device := StrToInt(ParamStr(1));
  except
    WriteLn('Error: unable to convert device');
    Exit;
  end;

  try
    chan := StrToInt(ParamStr(2));
  except
    WriteLn('Error: unable to convert channel');
    Exit;
  end;

  s.Open(0);

  repeat
    if (not open) or (rate <> s.ctrl^.rate) then
    begin
      Sleep(1000);
      CloseDevice;
      rate := s.ctrl^.rate;
      if not OpenDevice then
      begin
        WriteLn('Error: unable to open device');
        Exit;
      end;
      i := 0;
    end;
    if not s.Wait then Continue;
    Move(s.data^[chan], signal[i], hsig[i].dwBufferLength);
    waveOutWrite(hndl, @hsig[i], sizeof(TWaveHdr));
    Inc(i);
    if i > High(hsig) then i := 0;
  until False;
end.
