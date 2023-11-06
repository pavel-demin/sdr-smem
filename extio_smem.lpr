library extio_smem;

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
    prim: TCheckBox;
    chan: TComboBox;
    rate: TComboBox;
    freq: Int32;
    procedure ChangePrim(Sender: TObject);
    procedure ChangeRate(Sender: TObject);
    procedure ExitComboBox(Sender: TObject);
  end;

const
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
  chan, freq, rate, size, limit, offset: Int32;
begin
  offset := 0;
  repeat
    if offset = 0 then
    begin
      chan := s.chan.ItemIndex;
      if s.prim.Checked then
      begin
        s.smem.ctrl^.freq[chan] := s.freq;
        rate := s.rate.ItemIndex;
        s.smem.ctrl^.rate := rate;
      end
      else
      begin
        freq := s.smem.ctrl^.freq[chan];
        if s.freq <> freq then
        begin
          s.freq := freq;
          if s.call <> nil then s.call(-1, 101, 0.0, nil);
        end;
        rate := s.smem.ctrl^.rate;
        if s.rate.ItemIndex <> rate then
        begin
          s.rate.ItemIndex := rate;
          if s.call <> nil then s.call(-1, 100, 0.0, nil);
        end;
      end;
      size := 512 shl rate;
      limit := 7 shr rate;
    end;
    if not s.smem.Wait then Continue;
    Move(s.smem.data^[chan], b[offset * size], size * 8);
    Inc(offset);
    if offset > limit then
    begin
      if s.call <> nil then s.call(4096, 0, 0.0, @b);
      offset := 0;
    end;
  until Terminated;
end;

procedure TState.ChangePrim(Sender: TObject);
begin
  s.rate.Enabled := s.prim.Checked;
end;

procedure TState.ChangeRate(Sender: TObject);
begin
  if s.call <> nil then s.call(-1, 100, 0.0, nil);
end;

procedure TState.ExitComboBox(Sender: TObject);
begin
  (Sender as TComboBox).SelLength := 0;
end;

procedure SetupControl(c: TWinControl; p: TForm; y: Int32);
begin
  with c do
  begin
    Parent := p;
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
  s := TState.Create;
  s.rgst := s.smem.Open;
  try
    chan := s.rgst.ReadInteger('Chan');
    rate := s.rgst.ReadInteger('Rate');
  except
    chan := 0;
    rate := 2;
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
    Height := 100;
    Width := 256;
  end;

  s.prim := TCheckBox.Create(s.form);
  SetupControl(s.prim, s.form, 8);
  with s.prim do
  begin
    Checked := True;
    OnChange := @s.ChangePrim;
  end;

  CreateLabel(s.form, 'Primary', s.prim);

  s.chan := TComboBox.Create(s.form);
  SetupControl(s.chan, s.form, 40);
  with s.chan do
  begin
    ReadOnly := True;
    OnExit := @s.ExitComboBox;
    for c := 0 to 7 do Items.Add(IntToStr(c));
    ItemIndex := chan;
  end;

  CreateLabel(s.form, 'RX channel', s.chan);

  s.rate := TComboBox.Create(s.form);
  SetupControl(s.rate, s.form, 72);
  with s.rate do
  begin
    Enabled := True;
    ReadOnly := True;
    OnChange := @s.ChangeRate;
    OnExit := @s.ExitComboBox;
    for r in rates do Items.Add(IntToStr(r) + ' kSPS');
    ItemIndex := rate;
  end;

  CreateLabel(s.form, 'Sample rate', s.rate);
end;

procedure Free;
begin
  try
    s.rgst.WriteInteger('Chan', s.chan.ItemIndex);
    s.rgst.WriteInteger('Rate', s.rate.ItemIndex);
  except
  end;
  s.smem.Close;
  s.form.Free;
  s.Free;
end;

function InitHW(name, model: PChar; var format: Int32): Boolean; stdcall;
begin
  format := 7;
  StrPCopy(name, s.smem.name);
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
  s.thrd := TDataThread.Create;
  Result := 4096;
end;

procedure StopHW; stdcall;
begin
  s.thrd.Terminate;
  Sleep(200);
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
  if s.prim.Checked then
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
  end
  else
  begin
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
