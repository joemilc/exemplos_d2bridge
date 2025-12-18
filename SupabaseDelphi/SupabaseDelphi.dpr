{$IFDEF D2DOCKER}library{$ELSE}program{$ENDIF} SupabaseDelphi;

{$IFDEF D2BRIDGE}
 {$APPTYPE CONSOLE}
{$ENDIF}



uses
  Vcl.Forms,
  D2Bridge.Instance,
  D2Bridge.ServerControllerBase in 'D:\DelphiAlexandria\D2Bridge\D2Bridge Framework\D2Bridge.ServerControllerBase.pas' {D2BridgeServerControllerBase: TDataModule},
  Prism.SessionBase in 'D:\DelphiAlexandria\D2Bridge\D2Bridge Framework\Prism\Prism.SessionBase.pas' {PrismSessionBase: TPrismSessionBase},
  SupabaseDelphiWebApp in 'SupabaseDelphiWebApp.pas' {SupabaseDelphiWebAppGlobal},
  SupabaseDelphi_Session in 'SupabaseDelphi_Session.pas' {SupabaseDelphiSession},
  D2BridgeFormTemplate in 'D2BridgeFormTemplate.pas',
  Unit_D2Bridge_Server_Console in 'Unit_D2Bridge_Server_Console.pas',
  Unit1 in 'Unit1.pas' {Form1},
  Unit2 in 'Unit2.pas' {Form2},
  SupaBaseConnector in 'SupaBaseConnector.pas',
  CriaRESTClient in 'CriaRESTClient.pas',
  CadastrosService in 'CadastrosService.pas';

{$R *.res}

{$IFNDEF D2BRIDGE}
var
  Unit1: TForm1;
{$ENDIF}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar:= False;
  {$IFNDEF D2BRIDGE}
  Application.CreateForm(TForm1, Unit1);
  D2BridgeInstance.AddInstace(Unit1);
  Application.Run;
  {$ELSE}
  TD2BridgeServerConsole.Run
  
  {$ENDIF}
end.
