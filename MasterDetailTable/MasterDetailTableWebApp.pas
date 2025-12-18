unit MasterDetailTableWebApp;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

interface

Uses
 Classes, SysUtils,
 System.UITypes,
 D2Bridge.ServerControllerBase, D2Bridge.Types, D2Bridge.JSON,
 Prism.Session, Prism.Server.HTTP.Commom, Prism.Types, Prism.Interfaces,
 MasterDetailTable_Session;

type
 IPrismSession = Prism.Interfaces.IPrismSession;
 TSessionChangeType = Prism.Types.TSessionChangeType;
 TD2BridgeLang = D2Bridge.Types.TD2BridgeLang;


type
 TMasterDetailTableWebAppGlobal = class(TD2BridgeServerControllerBase)
  private
   procedure OnNewSession(const Request: TPrismHTTPRequest; Response: TPrismHTTPResponse; Session: TPrismSession);
   procedure OnCloseSession(Session: TPrismSession);
   procedure OnDisconnectSession(Session: TPrismSession);
   procedure OnReconnectSession(Session: TPrismSession);
   procedure OnExpiredSession(Session: TPrismSession; var Renew: boolean);
   procedure OnIdleSession(Session: TPrismSession; var Renew: boolean);
   procedure OnException(Form: TObject; Sender: TObject; E: Exception; FormName: String; ComponentName: String; EventName: string; APrismSession: IPrismSession);
   procedure OnSecurity(const SecEventInfo: TSecuritEventInfo);
   procedure OnRoute(const RestSession: TD2BridgeRestSession; const Request: TPrismHTTPRequest; const Response: TPrismHTTPResponse);
   procedure OnBeforeServerStart;
   procedure OnAfterServerStart;
   procedure OnBeforeServerStop;
   procedure OnAfterServerStop;
   //Routes
   //procedure GetPing(const RestSession: TD2BridgeRestSession; Request: TPrismHTTPRequest; Response: TPrismHTTPResponse);
  protected
   procedure RegisterRoutes(RestServer: TD2BridgeRestServer); override;
  public
   constructor Create(AOwner: TComponent); override;

 end;


var
 D2BridgeServerController: TMasterDetailTableWebAppGlobal;


Function MasterDetailTable: TMasterDetailTableSession;


implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

Uses
 D2Bridge.Instance,
 D2Bridge.Rest.Commom;

{$IFNDEF FPC}
{$R *.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF} 

Function MasterDetailTable: TMasterDetailTableSession;
begin
 Result:= TMasterDetailTableSession(D2BridgeInstance.PrismSession.Data);
end;

constructor TMasterDetailTableWebAppGlobal.Create(AOwner: TComponent);
begin
 inherited;
 {$IFDEF D2BRIDGE} 
  Prism.OnNewSession:= OnNewSession;
  Prism.OnCloseSession:= OnCloseSession;
  Prism.OnDisconnectSession:= OnDisconnectSession;
  Prism.OnReconnectSession:= OnReconnectSession;
  Prism.OnExpiredSession:= OnExpiredSession;
  Prism.OnIdleSession:= OnIdleSession;
  Prism.OnException:= OnException;
  Prism.OnSecurity:= OnSecurity;
  Prism.OnRoute:= OnRoute;
  prism.OnBeforeServerStart:= OnBeforeServerStart;
  prism.OnAfterServerStart:= OnAfterServerStart;
  prism.OnBeforeServerStop:= OnBeforeServerStop;
  prism.OnAfterServerStop:= OnAfterServerStop;
 {$ENDIF}

 
 //Our Code
 
  
 {$IFNDEF D2BRIDGE}
  OnNewSession(nil, nil, D2BridgeInstance.PrismSession as TPrismSession);
 {$ENDIF}
end;

procedure TMasterDetailTableWebAppGlobal.RegisterRoutes(RestServer: TD2BridgeRestServer);
begin
{
 RestServer.AddGet('/api/ping', GetPing);
 //or...
 RestServer.AddGet('/api/test', nil);
}
end;

procedure TMasterDetailTableWebAppGlobal.OnException(Form, Sender: TObject; E: Exception; FormName, ComponentName, EventName: string; APrismSession: IPrismSession);
begin
 //Show Error Messages
 {
  if Assigned(APrismSession) then
   APrismSession.ShowMessageError(E.Message);
 }
end;

procedure TMasterDetailTableWebAppGlobal.OnNewSession(const Request: TPrismHTTPRequest; Response: TPrismHTTPResponse; Session: TPrismSession);
begin
 D2BridgeInstance.PrismSession.Data := TMasterDetailTableSession.Create(Session);

 //Set Language just this Session
 //Session.Language:= TD2BridgeLang.English;

 //Our Code

end;

procedure TMasterDetailTableWebAppGlobal.OnCloseSession(Session: TPrismSession);
begin
 //Close ALL DataBase connection
 //Ex: Dm.DBConnection.Close;

end;

procedure TMasterDetailTableWebAppGlobal.OnExpiredSession(Session: TPrismSession; var Renew: boolean);
begin
 //Example of use Renew
 {
  if Session.InfoConnection.Identity = 'UserXYZ' then
   Renew:= true;
 }
end;

procedure TMasterDetailTableWebAppGlobal.OnIdleSession(Session: TPrismSession; var Renew: boolean);
begin

end;

procedure TMasterDetailTableWebAppGlobal.OnDisconnectSession(Session: TPrismSession);
begin

end;

procedure TMasterDetailTableWebAppGlobal.OnReconnectSession(Session: TPrismSession);
begin

end;

procedure TMasterDetailTableWebAppGlobal.OnSecurity(const SecEventInfo: TSecuritEventInfo);
begin
{
 if SecEventInfo.Event = TSecurityEvent.secNotDelistIPBlackList then
 begin
  //Write IP Delist to Reload in WhiteList
  SecEventInfo.IP...
 end;
}
end;

procedure TMasterDetailTableWebAppGlobal.OnRoute(const RestSession: TD2BridgeRestSession; const Request: TPrismHTTPRequest; const Response: TPrismHTTPResponse);
begin

end;


procedure TMasterDetailTableWebAppGlobal.OnAfterServerStart;
begin

end;

procedure TMasterDetailTableWebAppGlobal.OnAfterServerStop;
begin

end;

procedure TMasterDetailTableWebAppGlobal.OnBeforeServerStart;
begin

end;

procedure TMasterDetailTableWebAppGlobal.OnBeforeServerStop;
begin

end;


{$IFNDEF D2BRIDGE}
initialization
 D2BridgeServerController:= TMasterDetailTableWebAppGlobal.Create(D2BridgeInstance.Owner);
{$ENDIF}

end.
