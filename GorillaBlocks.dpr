program GorillaBlocks;

uses
  System.StartUpCopy,
  FMX.Forms,
{$IFDEF LINUX}
  Gorilla.Context.GLES.Linux,
{$ENDIF}
  MainForm in 'MainForm.pas' {Form2};

{$R *.res}

begin
{$IFDEF LINUX}
  Gorilla.Context.GLES.Linux.TGorillaContextLinux.AUTO_SELECT_GPU := 'NVIDIA';
{$ENDIF}

  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
