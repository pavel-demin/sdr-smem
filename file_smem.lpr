program file_smem;

{$mode objfpc}{$H+}

uses
  classes,
  smem,
  sysutils,
  wave;

var
  s: TSharedMemory;
  w: TWaveOutDevice;
  strm: TFileStream;
  device, chan, rate: Int32;
  buffer: array [0..4095, 0..1] of Single;

begin
  if ParamCount() <> 3 then
  begin
    WriteLn('Usage: file_smem.exe file device channel');
    w.Help;
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

  s.Open;

  rate := -1;

  repeat
    if rate <> s.ctrl^.rate then
    begin
      w.Close;
      rate := s.ctrl^.rate;
      try
        w.Open(device, rate)
      except
        WriteLn('Error: unable to open device');
        Exit;
      end;
    end;
    try
      strm.ReadBuffer(buffer, w.size);
      w.Play(buffer);
      Move(buffer, s.data^[chan], w.size);
      s.Notify;
    except
      Exit;
    end;
  until False;
end.
