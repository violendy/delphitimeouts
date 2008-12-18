// DelphiTimeouts version 1.1
// Copyright (c) 2007-2008 Szymon Jachim
//
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use,
// copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following
// conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
// WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
// OTHER DEALINGS IN THE SOFTWARE.
unit Timeouts;


{
Let's you limit execution time of a procedure/function. Timeout will be called off automatically with the execution of a last statement of the procedure/function in which it was created. The thread which didn't finish it's work, won't be killed abruptly! It will magically raise ETimeoutException, so you have the ability to release resources and free memory during stack unwinding... And you can use it from GUI/main thread.

procedure TForm1.Button1Click?(Sender: TObject); begin

    try

        timeout(4000); // this is where "magic" happens... while True do begin

            Sleep(1000); // some real processing here

        end;

    except

        on E: Exception do

            ShowMessage?(E.ClassName?);

    end;

end;
}

interface

uses
  SysUtils, Windows, Classes;

type
  ETimeoutException = class(Exception);

  ITimeout = interface(IUnknown)
    ['{34EC5325-C67A-4D74-B868-3DCF312A3E4A}']
    procedure CallOff;
  end;

function Timeout(Time: Cardinal): ITimeout;

implementation

type
  TTimeoutThread = class(TThread)
  private
    procedure TimeoutThread;
  public
    ThreadHandle: Cardinal;
    Timeout: Cardinal;
    procedure Execute; override;
  end;

  TTimeout = class(TInterfacedObject, ITimeout)
  private
    FTimeoutThread: TTimeoutThread;
  public
    procedure CallOff;
    constructor Create(ATimeout: Cardinal; AThreadHandle: THandle);
    destructor Destroy; override;
  end;

function Timeout(Time: Cardinal): ITimeout;
var
  ThisThreadHandle: THandle;
begin
  DuplicateHandle(GetCurrentProcess, GetCurrentThread, GetCurrentProcess, @ThisThreadHandle, 0, True, DUPLICATE_SAME_ACCESS);
  Result := TTimeout.Create(Time, ThisThreadHandle);
end;

procedure RaiseTimeOutException;
begin
  raise ETimeoutException.Create('Timedout!'); // tutaj klase trzeba zmienic na ETimeOut albo cos takiego
end;

procedure TTimeoutThread.TimeoutThread;
var
  Ctx: _CONTEXT;
begin
  SuspendThread(ThreadHandle);
  Ctx.ContextFlags := CONTEXT_FULL;
  GetThreadContext(ThreadHandle, Ctx);
  Ctx.Eip := Cardinal(@RaiseTimeOutException);
  SetThreadContext(ThreadHandle, Ctx);
  ResumeThread(ThreadHandle);
end;

{ TTimeout }

procedure TTimeout.CallOff;
begin
  FTimeoutThread.Terminate;
end;

constructor TTimeout.Create(ATimeout: Cardinal; AThreadHandle: THandle);
begin
  FTimeoutThread := TTimeoutThread.Create(True);
  FTimeoutThread.FreeOnTerminate := True;
  FTimeoutThread.ThreadHandle := AThreadHandle;
  FTimeoutThread.Timeout := ATimeout;
  FTimeoutThread.Resume;
end;

destructor TTimeout.Destroy;
begin
  CallOf;
  inherited;
end;

{ TTimeoutThread }

procedure TTimeoutThread.Execute;
var
  I: Integer;
begin
  inherited;
  for I := Timeout downto 0 do
  begin
    Sleep(1);
    if Terminated then
      Break;
  end;
  if not Terminated then
    TimeoutThread
end;

end.
