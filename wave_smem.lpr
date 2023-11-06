program wave_smem;

{$mode objfpc}{$H+}

uses
  smem,
  sysutils,
  wave;

var
  s: TSharedMemory;
  w: TWaveOutDevice;
  device, chan, rate: Int32;

begin
  if ParamCount() <> 2 then
  begin
    WriteLn('Usage: wave_smem.exe device channel');
    w.Help;
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
    if not s.Wait then Continue;
    w.Play(s.data^[chan]);
  until False;
end.
