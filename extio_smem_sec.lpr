library extio_smem_sec;

{$mode objfpc}{$H+}

uses
  classes,
  controls,
  forms,
  interfaces,
  registry,
  smem,
  spin,
  ssockets,
  stdctrls,
  sysutils;

type
  TCallback = procedure(size, code: Int32; offset: Single; data: Pointer); cdecl;

  TDataThread = class(TThread)
  public
    constructor Create;
  protected
    procedure Execute; override;
  end;

  TState = class
    rgst: TRegistry;
    smem: TSharedMemory;
    thrd: TDataThread;
    call: TCallback;
    form: TForm;
    chan: TComboBox;
    rate: Int32;
    freq: Int32;
    procedure ExitComboBox(Sender: TObject);
  end;

const

  path: String = '\Software\SDR_SMEM_0';

  rates: array of Int32 = (48, 96, 192, 384);

var
  s: TState;
  b: array [0..4095, 0..1] of Single;

function SetHWLO(freq: Int32): Int32; stdcall; forward;

constructor TDataThread.Create;
begin
  FreeOnTerminate := True;
  inherited Create(false);
end;

procedure TDataThread.Execute;
var
  chan, freq, rate, size, limit, offset: Int32;
begin
  chan := s.chan.ItemIndex;
  freq := s.smem.ctrl^.freq[chan];
  rate := s.smem.ctrl^.rate;
  size := 512 shl rate;
  limit := 7 shr rate;
  offset := 0;
  repeat
    if not s.smem.Wait then Continue;
    Move(s.smem.data^[chan], b[offset * size], size * 8);
    Inc(offset);
    if offset > limit then
    begin
      if s.call <> nil then s.call(4096, 0, 0.0, @b);
      chan := s.chan.ItemIndex;
      freq := s.smem.ctrl^.freq[chan];
      if s.freq <> freq then
      begin
        s.freq := freq;
        if s.call <> nil then s.call(-1, 101, 0.0, nil);
      end;
      rate := s.smem.ctrl^.rate;
      if s.rate <> rate then
      begin
        s.rate := rate;
        if s.call <> nil then s.call(-1, 100, 0.0, nil);
      end;
      size := 512 shl rate;
      limit := 7 shr rate;
      offset := 0;
    end;
  until Terminated;
end;

procedure TState.ExitComboBox(Sender: TObject);
begin
  (Sender as TComboBox).SelLength := 0;
end;

procedure SetupControl(c: TWinControl; p: TForm; e: TNotifyEvent; y: Int32);
begin
  with c do
  begin
    Parent := p;
    OnExit := e;
    Width := 112;
    Left := 136;
    Top := y;
  end;
end;

procedure CreateLabel(f: TForm; c: String; s: TControl);
var
  l: TLabel;
begin
  l := TLabel.create(f);
  with l do
  begin
    Parent := f;
    Caption := c;
    Left := 8;
    AnchorVerticalCenterTo(s);
  end;
end;

procedure Init;
var
  c, chan: Int32;
begin
  chan := 0;
  s := TState.Create;
  s.rgst := TRegistry.Create;
  with s.rgst do
  begin
    OpenKey(path, True);
    if ValueExists('Chan') then chan := ReadInteger('Chan') else WriteInteger('Chan', chan);
  end;
  if chan < 0 then chan := 0;
  if chan > 7 then chan := 7;

  Application.Scaled := True;
  Application.Initialize;

  s.form := TForm.Create(nil);
  with s.form do
  begin
    BorderStyle := bsSingle;
    Caption := 'Settings';
    PixelsPerInch := 96;
    Height := 36;
    Width := 256;
  end;

  s.chan := TComboBox.Create(s.form);
  SetupControl(s.chan, s.form, @s.ExitComboBox, 8);
  with s.chan do
  begin
    ReadOnly := True;
    for c := 0 to 7 do Items.Add(IntToStr(c));
    ItemIndex := chan;
  end;

  CreateLabel(s.form, 'RX channel', s.chan);
end;

procedure Free;
begin
  s.rgst.WriteInteger('Chan', s.chan.ItemIndex);
  s.form.Free;
  s.rgst.Free;
  s.Free;
end;

function InitHW(name, model: PChar; var format: Int32): Boolean; stdcall;
begin
  format := 7;
  StrPCopy(name, 'SMEM Secondary');
  StrPCopy(model, '');
  Result := True;
end;

function OpenHW: Boolean; stdcall;
begin
  Result := True;
end;

function StartHW(freq: Int32): Int32; stdcall;
begin
  SetHWLO(freq);
  s.smem.Open(0);
  s.thrd := TDataThread.Create;
  Result := 4096;
end;

procedure StopHW; stdcall;
begin
  s.thrd.Terminate;
  Sleep(200);
  s.smem.Close;
end;

procedure CloseHW; stdcall;
begin

end;

procedure SetCallback(call: TCallback); stdcall;
begin
  s.call := call;
end;

function SetHWLO(freq: Int32): Int32; stdcall;
begin
  Result := 0;
  if (s.freq <> freq) and (s.call <> nil) then s.call(-1, 101, 0.0, nil);
end;

function GetHWLO: Int32; stdcall;
begin
  Result := s.freq;
end;

function GetHWSR: Int32; stdcall;
begin
  Result := rates[s.rate] * 1000;
end;

function GetStatus: Int32; stdcall;
begin
  Result := 0;
end;

procedure ShowGUI; stdcall;
begin
  if not s.form.Visible then s.form.Show;
end;

procedure HideGUI; stdcall;
begin
  if s.form.Visible then s.form.Hide;
end;

exports
  InitHW,
  OpenHW,
  StartHW,
  StopHW,
  CloseHW,
  SetCallback,
  SetHWLO,
  GetHWLO,
  GetHWSR,
  GetStatus,
  ShowGUI,
  HideGUI;

begin
  Init;
  ExitProc := @Free;
end.
