program cat_smem_serial;

{$mode objfpc}{$H+}

uses
  serial,
  smem,
  sysutils;

type
  TState = record
    port: TSerialHandle;
    smem: TSharedMemory;
    chan: array [0..1] of Int32;
    freq: array [0..1] of Int32;
  end;

const
  command: String = 'fa;fb;';

var
  s: TState;
  status: Int32;
  buffer: String;

begin
  if ParamCount() <> 3 then
  begin
    WriteLn('Usage: cat_smem_serial.exe port chan chan');
    Exit;
  end;

  s.port := SerOpen('\\.\COM' + ParamStr(1));
  if s.port = 0 then
  begin
    WriteLn('Error: unable to open port');
    Exit;
  end;

  try
    s.chan[0] := StrToInt(ParamStr(2)) and 7;
    s.chan[1] := StrToInt(ParamStr(3)) and 7;
  except
    WriteLn('Error: unable to convert channels');
    Exit;
  end;

  s.smem.Open(0);

  SetLength(buffer, 28);

  repeat
    SerSetParams(s.port, 115200, 8, NoneParity, 1, []);

    status := SerWrite(s.port, command[1], Length(command));
    if status <> Length(command) then
    begin
      WriteLn('Error: unable to send command');
      Exit;
    end;

    SerSync(s.port);
    Sleep(100);

    status := SerRead(s.port, buffer[1], 28);
    if status <> 28 then
    begin
      WriteLn('Error: unable to read frequencies');
      Exit;
    end;

    try
      s.freq[0] := StrToInt(buffer.SubString(2, 11));
      s.freq[1] := StrToInt(buffer.SubString(16, 11));
    except
      WriteLn('Error: unable to convert frequencies');
      Exit;
    end;

    s.smem.ctrl^.freq[s.chan[0]] := s.freq[0];
    s.smem.ctrl^.freq[s.chan[1]] := s.freq[1];
  until False;
end.
