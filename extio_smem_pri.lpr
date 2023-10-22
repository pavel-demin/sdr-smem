library extio_smem_pri;

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
    rate: TComboBox;
    freq: Int32;
    procedure ChangeRate(Sender: TObject);
    procedure ExitComboBox(Sender: TObject);
  end;

const

  path: String = '\Software\SDR_SMEM_0';

  fmax: Int32 = 490000000;

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
  chan, rate, size, limit, offset: Int32;
begin
  chan := s.chan.ItemIndex;
  s.smem.ctrl^.freq[chan] := s.freq;
  rate := s.rate.ItemIndex;
  s.smem.ctrl^.rate := rate;
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
      s.smem.ctrl^.freq[chan] := s.freq;
      rate := s.rate.ItemIndex;
      s.smem.ctrl^.rate := rate;
      size := 512 shl rate;
      limit := 7 shr rate;
      offset := 0;
    end;
  until Terminated;
end;

procedure TState.ChangeRate(Sender: TObject);
begin
  if s.call <> nil then s.call(-1, 100, 0.0, nil);
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
  c, chan, r, rate, rmax: Int32;
begin
  chan := 0;
  rate := 2;
  s := TState.Create;
  s.rgst := TRegistry.Create;
  with s.rgst do
  begin
    OpenKey(path, True);
    if ValueExists('Chan') then chan := ReadInteger('Chan') else WriteInteger('Chan', chan);
    if ValueExists('Rate') then rate := ReadInteger('Rate') else WriteInteger('Rate', rate);
  end;
  if chan < 0 then chan := 0;
  if chan > 7 then chan := 7;
  rmax := High(rates);
  if rate < 0 then rate := 0;
  if rate > rmax then rate := rmax;

  Application.Scaled := True;
  Application.Initialize;

  s.form := TForm.Create(nil);
  with s.form do
  begin
    BorderStyle := bsSingle;
    Caption := 'Settings';
    PixelsPerInch := 96;
    Height := 68;
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

  s.rate := TComboBox.Create(s.form);
  SetupControl(s.rate, s.form, @s.ExitComboBox, 40);
  with s.rate do
  begin
    ReadOnly := True;
    OnChange := @s.ChangeRate;
    for r in rates do Items.Add(IntToStr(r) + ' kSPS');
    ItemIndex := rate;
  end;

  CreateLabel(s.form, 'Sample rate', s.rate);
end;

procedure Free;
begin
  s.rgst.WriteInteger('Chan', s.chan.ItemIndex);
  s.rgst.WriteInteger('Rate', s.rate.ItemIndex);
  s.form.Free;
  s.rgst.Free;
  s.Free;
end;

function InitHW(name, model: PChar; var format: Int32): Boolean; stdcall;
begin
  format := 7;
  StrPCopy(name, 'SMEM Primary');
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
  if freq > fmax then
  begin
    s.freq := fmax;
    Result := fmax;
  end
  else
  begin
    s.freq := freq;
    Result := 0;
  end;
  if (s.freq <> freq) and (s.call <> nil) then s.call(-1, 101, 0.0, nil);
end;

function GetHWLO: Int32; stdcall;
begin
  Result := s.freq;
end;

function GetHWSR: Int32; stdcall;
begin
  Result := rates[s.rate.ItemIndex] * 1000;
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
