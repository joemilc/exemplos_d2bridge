{$IFDEF D2DOCKER}library{$ELSE}program{$ENDIF} GooglCharts;

{$IFDEF D2BRIDGE}
 {$APPTYPE CONSOLE}
{$ENDIF}



uses
  Vcl.Forms,
  D2Bridge.ServerControllerBase in 'D:\DelphiAlexandria\D2Bridge\D2Bridge Framework\D2Bridge.ServerControllerBase.pas' {D2BridgeServerControllerBase: TDataModule},
  Prism.SessionBase in 'D:\DelphiAlexandria\D2Bridge\D2Bridge Framework\Prism\Prism.SessionBase.pas' {PrismSessionBase: TPrismSessionBase},
  GooglChartsWebApp in 'GooglChartsWebApp.pas' {GooglChartsWebAppGlobal},
  GooglCharts_Session in 'GooglCharts_Session.pas' {GooglChartsSession},
  D2BridgeFormTemplate in 'D2BridgeFormTemplate.pas',
    Unit_D2Bridge_Server_Console in 'Unit_D2Bridge_Server_Console.pas',

  
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar:= False;
  TD2BridgeServerConsole.Run
  
end.
