Let's you limit execution time of a procedure/function.
Timeout will be called off automatically with the execution of a last statement of the procedure/function in which it was created.
The thread which didn't finish it's work, won't be killed abruptly! It will magically raise ETimeoutException, so you have the ability to release resources and free memory during stack unwinding... And you can use it from GUI/main thread.

```
procedure TForm1.Button1Click(Sender: TObject);
begin
  try
    timeout(4000); // this is where "magic" happens...
    while True do
    begin
      Sleep(10000); // some real processing here
    end;
  except
    on E: Exception do
      ShowMessage(E.ClassName);
  end;
end;
```