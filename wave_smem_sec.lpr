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
  i, j, l: Int32;
  chan, rate, size: Int32;
  hsig: array [0..74] of TWaveHdr;
  hsil: array [0..7] of TWaveHdr;
  signal: array [0..74, 0..4095, 0..1] of Single;
  silence: array [0..7, 0..38399, 0..1] of Single;

function OpenDevice: Boolean;
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
  for i := 0 to High(hsig) do
  begin
    hsig[i].lpData := @signal[i];
    hsig[i].dwBufferLength := SizeOf(signal[i]);
    waveOutPrepareHeader(hndl, @hsig[i], SizeOf(TWaveHdr));
  end;
  for j := 0 to High(hsil) do
  begin
    hsil[j].lpData := @silence[j];
    hsil[j].dwBufferLength := SizeOf(silence[j]);
    waveOutPrepareHeader(hndl, @hsil[j], SizeOf(TWaveHdr));
    waveOutWrite(hndl, @hsil[j], sizeof(TWaveHdr));
  end;
  open := True;
  Result := True;
end;

procedure CloseDevice;
begin
  if not open then Exit;
  for i := 0 to High(hsig) do
  begin
    waveOutUnprepareHeader(hndl, @hsig[i], SizeOf(TWaveHdr));
  end;
  for j := 0 to High(hsil) do
  begin
    waveOutUnprepareHeader(hndl, @hsil[j], SizeOf(TWaveHdr));
  end;
  waveOutClose(hndl);
  open := False;
end;

begin
  if ParamCount() <> 2 then
  begin
    WriteLn('Usage: wave_smem_sec.exe device chan');
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
      rate := s.ctrl^.rate;
      Sleep(1000);
      CloseDevice;
      if not OpenDevice then
      begin
        WriteLn('Error: unable to open device');
        Exit;
      end;
      i := 0;
      j := 0;
    end;
    if s.Wait then
    begin
      size := 512 shl rate;
      hsig[i].dwBufferLength := size * 8;
      Move(s.data^[chan], signal[i], size * 8);
      waveOutWrite(hndl, @hsig[i], sizeof(TWaveHdr));
      Inc(i);
      if i > High(hsig) then i := 0;
    end
    else
    begin
      size := 4800 shl rate;
      hsil[j].dwBufferLength := size * 8;
      waveOutWrite(hndl, @hsil[j], sizeof(TWaveHdr));
      Inc(j);
      if j > High(hsil) then j := 0;
    end;
  until False;
end.
