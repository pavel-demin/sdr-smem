program cat_smem;

{$mode objfpc}{$H+}

uses
  smem,
  ssockets,
  sysutils,
  windows;

type
  TState = record
    hndl: THandle;
    smem: TSharedMemory;
    sock: array [0..1] of TInetSocket;
    port: array [0..1] of Int32;
    chan: array [0..1] of Int32;
    freq: array [0..3] of Int32;
  end;

const
  bands: array [0..3, 0..10] of Int32 = (
    (1831250, 3521250, 5371250, 7021250, 10121250, 14021250, 18089250, 21021250, 24911250, 28021250, 50081250),
    (1852500, 3542500, 5392500, 7042500, 10142500, 14042500, 18110500, 21042500, 24932500, 28042500, 50102500),
    (1895000, 3585000, 5435000, 7085000, 10185000, 14085000, 18153000, 21085000, 24975000, 28085000, 50145000),
    (1980000, 3670000, 5520000, 7170000, 10270000, 14170000, 18238000, 21170000, 25060000, 28170000, 50230000)
  );

var
  s: TState;
  cs: TDCB;
  ct: TCommTimeouts;
  i, rate, status: Int32;
  size: UInt32;
  call, buffer: String;
  freq: array [0..3] of Int32;
  drain: array [0..1023] of Byte;

function BinarySearch(constref a: array of Int32; v: Int32): Int32;
var
  imin, imax, imid: Int32;
begin
  imin := 0;
  imax := High(a);
  while imin <= imax do
  begin
    imid := (imin + imax) shr 1;
    if a[imid] < v then
      imin := imid + 1
    else if a[imid] > v then
      imax := imid - 1
    else
      Exit(imid);
  end;
  Result := -imin;
end;

function FindNearest(constref a: array of Int32; v: Int32): Int32;
var
  h, i: Int32;
begin
  h := High(a);
  if v <= a[0] then Exit(a[0]);
  if v >= a[h] then Exit(a[h]);
  i := BinarySearch(a, v);
  if i >= 0 then Exit(a[i]);
  i := -i;
  if (a[i] - v) < (v - a[i - 1]) then
    Exit(a[i])
  else
    Exit(a[i - 1]);
end;

begin
  if ParamCount() <> 6 then
  begin
    WriteLn('Usage: cat_smem.exe port chan chan call port port');
    Exit;
  end;

  buffer := '\\.\COM' + ParamStr(1);

  try
    s.hndl := CreateFile(PChar(buffer), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, 0, 0);
    WinCheck(s.hndl <> INVALID_HANDLE_VALUE);
  except
    WriteLn('Error: unable to open serial port');
    Exit;
  end;

  try
    s.chan[0] := StrToInt(ParamStr(2)) and 7;
    s.chan[1] := StrToInt(ParamStr(3)) and 7;
  except
    WriteLn('Error: unable to convert channels');
    Exit;
  end;

  call := ParamStr(4);

  try
    s.port[0] := StrToInt(ParamStr(5));
    s.port[1] := StrToInt(ParamStr(6));
  except
    WriteLn('Error: unable to convert ports');
    Exit;
  end;

  GetCommState(s.hndl, cs);
  cs.BaudRate := CBR_115200;
  cs.ByteSize := 8;
  cs.Parity := NOPARITY;
  cs.StopBits := ONESTOPBIT;
  SetCommState(s.hndl, cs);

  GetCommTimeouts(s.hndl, ct);
  ct.ReadTotalTimeoutConstant := 100;
  SetCommTimeouts(s.hndl, ct);

  s.smem.Open;

  repeat
    Sleep(100);

    PurgeComm(s.hndl, PURGE_RXABORT or PURGE_RXCLEAR or PURGE_TXABORT or PURGE_TXCLEAR);

    buffer := 'fa;fb;';
    WriteFile(s.hndl, buffer[1], 6, size, nil);
    if size <> 6 then
    begin
      WriteLn('Error: unable to send command');
      Exit;
    end;

    FlushFileBuffers(s.hndl);

    SetLength(buffer, 28);
    ReadFile(s.hndl, buffer[1], 28, size, nil);
    if size <> 28 then
    begin
      Continue;
    end;

    try
      freq[0] := StrToInt(buffer.SubString(2, 11));
      freq[1] := StrToInt(buffer.SubString(16, 11));
    except
      Continue;
    end;

    rate := s.smem.ctrl^.rate;

    for i := 0 to 1 do
    begin
      freq[i + 2] := FindNearest(bands[rate], freq[i]);
      if (s.freq[i] = freq[i]) and (s.freq[i + 2] = freq[i + 2]) then Continue;

      s.smem.ctrl^.freq[s.chan[i]] := freq[i + 2];

      if s.sock[i] = nil then
      begin
        buffer := call + #10;
        try
          s.sock[i] := TInetSocket.Create('127.0.0.1', s.port[i], 50);
        except
          FreeAndNil(s.sock[i]);
          Continue;
        end;
        status := s.sock[i].Write(buffer[1], Length(buffer));
        if status <> Length(buffer) then
        begin
          FreeAndNil(s.sock[i]);
          Continue;
        end;
      end;

      buffer := 'skimmer/lo_freq ' + IntToStr(freq[i + 2]) + #10;
      status := s.sock[i].Write(buffer[1], Length(buffer));
      if status <> Length(buffer) then
      begin
        FreeAndNil(s.sock[i]);
        Continue;
      end;

      s.freq[i + 2] := freq[i + 2];

      buffer := 'skimmer/qsy ' + FloatToStr(freq[i] / 1000) + #10;
      status := s.sock[i].Write(buffer[1], Length(buffer));
      if status <> Length(buffer) then
      begin
        FreeAndNil(s.sock[i]);
        Continue;
      end;

      s.freq[i] := freq[i];

      status := s.sock[i].Read(drain, SizeOf(drain));
      if status < 0 then
      begin
        FreeAndNil(s.sock[i]);
        Continue;
      end;
    end;
  until False;
end.
