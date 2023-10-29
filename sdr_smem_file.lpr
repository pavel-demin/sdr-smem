program sdr_smem_file;

{$mode objfpc}{$H+}

uses
  classes,
  mmsystem,
  smem,
  sysutils,
  windows;

var
  s: TSharedMemory;
  strm: TFileStream;
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
  if ParamCount() <> 3 then
  begin
    WriteLn('Usage: sdr_smem_file.exe file device channel');
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
    strm := TFileStream.Create(ParamStr(1), fmOpenRead);
  except
    WriteLn('Error: unable to open file');
    Exit;
  end;

  try
    device := StrToInt(ParamStr(2));
  except
    WriteLn('Error: unable to convert device');
    Exit;
  end;

  try
    chan := StrToInt(ParamStr(3));
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
    while (hsig[i].dwFlags and WHDR_DONE) = 0 do Sleep(1);
    try
      strm.ReadBuffer(signal[i], hsig[i].dwBufferLength);
      Move(signal[i], s.data^[chan], hsig[i].dwBufferLength);
      waveOutWrite(hndl, @hsig[i], sizeof(TWaveHdr));
      s.Notify;
      Inc(i);
      if i > High(hsig) then i := 0;
    except
      Exit;
    end;
  until False;
end.
