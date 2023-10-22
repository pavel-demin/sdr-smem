program cat_smem_telnet;

{$mode objfpc}{$H+}

uses
  smem,
  ssockets,
  sysutils;

type
  TState = record
    sock: TInetSocket;
    smem: TSharedMemory;
    port: Int32;
    chan: Int32;
    freq: Int32;
  end;

const
  command: String = 'skimmer/lo_freq ';

var
  s: TState;
  freq: Int32;
  status: Int32;
  buffer: String;

begin
  if ParamCount() <> 3 then
  begin
    WriteLn('Usage: cat_smem_telnet.exe call port chan');
    Exit;
  end;

  try
    s.port := StrToInt(ParamStr(2));
  except
    WriteLn('Error: unable to convert port');
    Exit;
  end;

  try
    s.chan := StrToInt(ParamStr(3)) and 7;
  except
    WriteLn('Error: unable to convert channel');
    Exit;
  end;

  try
    s.sock := TInetSocket.Create('127.0.0.1', s.port, 1000);
  except
    WriteLn('Error: unable to connect');
    Exit;
  end;

  s.smem.Open(0);

  Sleep(100);

  buffer := ParamStr(1) + #10;
  status := s.sock.Write(buffer[1], Length(buffer));
  if status <> Length(buffer) then
  begin
    WriteLn('Error: unable to send callsign');
    Exit;
  end;

  Sleep(100);

  repeat
    freq := s.smem.ctrl^.freq[s.chan];
    if s.freq <> freq then
    begin
      s.freq := freq;
      buffer := command + IntToStr(freq) + #10;
      status := s.sock.Write(buffer[1], Length(buffer));
      if status <> Length(buffer) then
      begin
        WriteLn('Error: unable to send command');
        Exit;
      end;
    end;
    Sleep(100);
  until False;
end.
