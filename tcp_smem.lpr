program tcp_smem;

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
  TSingleBuffer = array [0..32767, 0..1] of Single;
  TByteBuffer = array [0..262143] of Byte;

  TDataThread = class(TThread)
  public
    constructor Create;
  protected
    procedure Execute; override;
  private
    procedure Update(rate: Int32);
  end;

  TState = class
    rgst: TRegistry;
    sock: TInetSocket;
    smem: TSharedMemory;
    thrd: TDataThread;
    form: TForm;
    conn: TButton;
    addr: TEdit;
    corr: TFloatSpinEdit;
    inps: array [0..7] of TComboBox;
    info: array [0..8] of TEdit;
    rate: Int32;
    freq: array [0..7] of Int32;
    procedure ClickConn(Sender: TObject);
    procedure ChangeInps(Sender: TObject);
    procedure ExitComboBox(Sender: TObject);
  end;

const
  rates: array of Int32 = (48, 96, 192, 384);
  inputs: array of String = (
    'IN1',
    'IN2'
  );

var
  s: TState;
  b: TSingleBuffer;

constructor TDataThread.Create;
begin
  inherited Create(false);
end;

procedure TDataThread.Execute;
var
  rate, size, limit, offset: Int32;
begin
  rate := s.smem.ctrl^.rate;
  limit := 32768 shl rate;
  offset := 0;
  repeat
    size := limit - offset;
    size := s.sock.Read(TByteBuffer(b)[offset], size);
    if size < 0 then Break;
    offset := offset + size;
    if offset = limit then
    begin
      Update(rate);
      rate := s.smem.ctrl^.rate;
      limit := 32768 shl rate;
      offset := 0;
    end;
  until Terminated;
end;

procedure TDataThread.Update(rate: Int32);
var
  ctrl: TCtrlMemory;
  corr: Double;
  i, j, l: Int32;
begin
  l := (512 shl rate) - 1;
  for i := 0 to 7 do
    for j := 0 to l do
      s.smem.data^[i, j] := b[j * 8 + i];
  s.smem.Notify;
  ctrl := s.smem.ctrl^;
  corr := s.corr.Value;
  for i := 0 to 7 do ctrl.freq[i] := Round(ctrl.freq[i] * corr);
  s.sock.Write(ctrl, SizeOf(TCtrlMemory));
  if s.rate <> s.smem.ctrl^.rate then
  begin
    s.rate := s.smem.ctrl^.rate;
    s.info[0].Text := IntToStr(rates[s.rate]) + ' kSPS';
  end;
  if not CompareMem(@s.freq, @s.smem.ctrl^.freq, SizeOf(s.freq)) then
  begin
    s.freq := s.smem.ctrl^.freq;
    for i := 0 to 7 do s.info[i + 1].Text := IntToStr(s.freq[i]);
  end;
end;

procedure UpdateInps;
var
  i, c: Int32;
begin
  c := 0;
  for i := 0 to 7 do c := c or (s.inps[i].ItemIndex shl i);
  s.smem.ctrl^.inps := c;
end;

procedure Connect;
begin
  FreeAndNil(s.thrd);
  try
    FreeAndNil(s.sock);
    s.sock := TInetSocket.Create(s.addr.Text, 1001, 1000);
  except
    FreeAndNil(s.sock);
    Exit;
  end;
  s.thrd := TDataThread.Create;
end;

procedure TState.ClickConn(Sender: TObject);
begin
  Connect;
end;

procedure TState.ExitComboBox(Sender: TObject);
begin
  (Sender as TComboBox).SelLength := 0;
end;

procedure TState.ChangeInps(Sender: TObject);
begin
  UpdateInps;
end;

procedure SetupControl(c: TControl; p: TForm; y: Int32);
begin
  with c do
  begin
    Parent := p;
    Width := 112;
    Left := 136;
    Top := y;
  end;
end;

procedure CreateLabel(p: TForm; c: String; s: TControl);
var
  l: TLabel;
begin
  l := TLabel.create(p);
  with l do
  begin
    Parent := p;
    Caption := c;
    Left := 8;
    AnchorVerticalCenterTo(s);
  end;
end;

procedure Init;
var
  addr: String;
  corr: Double;
  inps: Int32;
  i: Int32;
begin
  s := TState.Create;
  s.rgst := s.smem.Open;
  try
    addr := s.rgst.ReadString('Addr');
    corr := s.rgst.ReadFloat('Corr');
    inps := s.rgst.ReadInteger('Inps');
  except
    addr := '192.168.1.100';
    corr := 1.0;
    inps := 0;
  end;
  if corr < 0.9999 then corr := 0.9999;
  if corr > 1.0001 then corr := 1.0001;

  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TForm, s.form);

  with s.form do
  begin
    BorderStyle := bsSingle;
    Caption := s.smem.name;
    PixelsPerInch := 96;
    Height := 356;
    Width := 256;
  end;

  s.addr := TEdit.create(s.form);
  SetupControl(s.addr, s.form, 8);
  s.addr.Text := addr;

  s.conn := TButton.create(s.form);
  with s.conn do
  begin
    Parent := s.form;
    Caption := 'Connect';
    Height := 24;
    Width := 112;
    Left := 8;
    AnchorVerticalCenterTo(s.addr);
    OnClick := @s.ClickConn;
  end;

  s.corr := TFloatSpinEdit.create(s.form);
  SetupControl(s.corr, s.form, 40);
  with s.corr do
  begin
    Alignment := taRightJustify;
    MinValue := 0.9999;
    MaxValue := 1.0001;
    Increment := 1E-7;
    DecimalPlaces := 7;
    Value := corr;
  end;

  CreateLabel(s.form, 'Frequency correction', s.corr);

  for i := 0 to 8 do
  begin
    s.info[i] := TEdit.create(s.form);
    SetupControl(s.info[i], s.form, 72 + 32 * i);
    with s.info[i] do
    begin
      Alignment := taRightJustify;
      Enabled := False;
      ReadOnly := True;
    end;
  end;

  s.info[0].Text := IntToStr(rates[s.rate]) + ' kSPS';

  CreateLabel(s.form, 'Sample rate', s.info[0]);

  for i := 0 to 7 do
  begin
    CreateLabel(s.form, 'RX' + IntToStr(i), s.info[i + 1]);

    s.inps[i] := TComboBox.create(s.form);
    with s.inps[i] do
    begin
      Parent := s.form;
      OnExit := @s.ExitComboBox;
      Width := 72;
      Left := 48;
      AnchorVerticalCenterTo(s.info[i + 1]);
      OnChange := @s.ChangeInps;
      Items.SetStrings(inputs);
      ItemIndex := (inps shr i) and 1;
    end;
  end;

  UpdateInps;
end;

procedure Free;
begin
  s.thrd.Free;
  s.sock.Free;
  try
    s.rgst.WriteString('Addr', s.addr.Text);
    s.rgst.WriteFloat('Corr', s.corr.Value);
    s.rgst.WriteInteger('Inps', s.smem.ctrl^.inps);
  except
  end;
  s.smem.Close;
  s.Free;
end;

begin
  Init;
  Connect;
  Application.Run;
  Free;
end.
