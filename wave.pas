unit wave;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  mmsystem;

type
  TWaveOutDevice = record
  public
    size: Int32;
    procedure Open(device, rate: Int32);
    procedure Close;
    procedure Play(var data);
    procedure Help;
  private
    wfx: TWaveFormatEx;
    hndl: THandle;
    index: Int32;
    hsig: array [0..31] of TWaveHdr;
    signal: array [0..31, 0..4095, 0..1] of Single;
  end;

implementation

uses
  sysutils;

procedure TWaveOutDevice.Open(device, rate: Int32);
var
  i, status: Int32;
begin
  wfx.wFormatTag := 3;
  wfx.nChannels := 2;
  wfx.nSamplesPerSec := 48000 shl rate;
  wfx.nAvgBytesPerSec := 384000 shl rate;
  wfx.nBlockAlign := 8;
  wfx.wBitsPerSample := 32;
  wfx.cbSize := 0;
  status := waveOutOpen(@hndl, device, @wfx, 0, 0, CALLBACK_NULL);
  WinCheck(status = MMSYSERR_NOERROR);
  size := 4096 shl rate;
  for i := 0 to High(hsig) do
  begin
    hsig[i].lpData := @signal[i];
    hsig[i].dwBufferLength := size;
    waveOutPrepareHeader(hndl, @hsig[i], SizeOf(TWaveHdr));
    waveOutWrite(hndl, @hsig[i], SizeOf(TWaveHdr));
  end;
  index := 0;
end;

procedure TWaveOutDevice.Close;
var
  i: Int32;
begin
  Sleep(1000);
  for i := 0 to High(hsig) do
  begin
    waveOutUnprepareHeader(hndl, @hsig[i], SizeOf(TWaveHdr));
  end;
  waveOutClose(hndl);
end;

procedure TWaveOutDevice.Play(var data);
begin
  while (hsig[index].dwFlags and WHDR_DONE) = 0 do Sleep(1);
  Move(data, signal[index], hsig[index].dwBufferLength);
  waveOutWrite(hndl, @hsig[index], SizeOf(TWaveHdr));
  Inc(index);
  if index > High(hsig) then index := 0;
end;

procedure TWaveOutDevice.Help;
var
  i, l: Int32;
  caps: TWaveOutCapsA;
begin
  WriteLn('Available devices:');
  l := waveOutGetNumDevs - 1;
  for i := 0 to l do
  begin
    if waveOutGetDevCaps(i, @caps, SizeOf(Caps)) = MMSYSERR_NOERROR then
    begin
      WriteLn(i:3, ' - ', caps.szPname);
    end;
  end;
end;

end.
