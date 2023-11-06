unit smem;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  registry;

type
  TCtrlMemory = record
    inps, rate: Int32;
    freq: array [0..7] of Int32;
  end;

  TDataMemory = array [0..7, 0..4095, 0..1] of Single;

  TSharedMemory = record
  public
    ctrl: ^TCtrlMemory;
    data: ^TDataMemory;
    rgst: TRegistry;
    name: array [0..1023] of Char;
    function Open: TRegistry;
    procedure Close;
    procedure Notify;
    function Wait: Boolean;
  private
    mapping: THandle;
    event: THandle;
  end;

implementation

uses
  sysutils,
  windows;

function TSharedMemory.Open: TRegistry;
var
  n, s: Int32;
  p: PChar;
begin
  n := 0;
  s := GetModuleFileName(MainInstance, name, SizeOf(name));
  if s <> 0 then
  begin
    p := StrRScan(name, '.');
    if p <> nil then p^ := #0;
    p := StrRScan(name, '_');
    if p <> nil then n := StrToIntDef(p + 1, 0);
  end;
  try
    name := Format('SDR_SMEM_%d_MEM', [n]);
    mapping := CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0,
      SizeOf(TCtrlMemory) + SizeOf(TDataMemory), name);
    WinCheck(mapping <> 0);
    name := Format('SDR_SMEM_%d_EVT', [n]);
    event := CreateEvent(nil, True, False, name);
    WinCheck(event <> 0);
    ctrl := MapViewOfFile(mapping, FILE_MAP_WRITE, 0, 0, 0);
    WinCheck(ctrl <> nil);
    name := Format('\Software\SDR_SMEM_%d', [n]);
    rgst := TRegistry.Create;
    rgst.OpenKey(name, True);
    name := Format('SMEM %d', [n]);
  except
    Close;
    Result := nil;
    Exit;
  end;
  data := Pointer(ctrl) + SizeOf(TCtrlMemory);
  Result := rgst;
end;

procedure TSharedMemory.Close;
begin
  rgst.Free;
  if ctrl <> nil then UnmapViewOfFile(ctrl);
  if event <> 0 then CloseHandle(event);
  if mapping <> 0 then CloseHandle(mapping);
  ctrl := nil;
  data := nil;
end;

function TSharedMemory.Wait: Boolean;
begin
  if event = 0 then Exit(False);
  ResetEvent(event);
  Result := WaitForSingleObject(event, 100) <> WAIT_TIMEOUT;
end;

procedure TSharedMemory.Notify;
begin
  if event = 0 then Exit;
  SetEvent(event);
end;

end.
