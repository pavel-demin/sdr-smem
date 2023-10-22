unit smem;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

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
    procedure Open(i: Int32);
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

procedure TSharedMemory.Open(i: Int32);
var
  name: String;
begin
  try
    name := 'SDR_SMEM_' + IntToStr(i) + '_MEM';
    mapping := CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0,
      SizeOf(TCtrlMemory) + SizeOf(TDataMemory), PChar(name));
    WinCheck(mapping <> 0);
    name := 'SDR_SMEM_' + IntToStr(i) + '_EVT';
    event := CreateEvent(nil, True, False, PChar(name));
    WinCheck(event <> 0);
    ctrl := MapViewOfFile(mapping, FILE_MAP_WRITE, 0, 0, 0);
    WinCheck(ctrl <> nil);
  except
    Close;
    Exit;
  end;
  data := Pointer(ctrl) + SizeOf(TCtrlMemory);
end;

procedure TSharedMemory.Close;
begin
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
