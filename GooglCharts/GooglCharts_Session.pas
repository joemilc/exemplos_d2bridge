unit GooglCharts_Session;

interface

uses
  SysUtils, Classes,
  Prism.SessionBase;

type
  TGooglChartsSession = class(TPrismSessionBase)
  private

  public
   constructor Create(APrismSession: TPrismSession); override;  //OnNewSession
   destructor Destroy; override; //OnCloseSession
  end;


implementation

Uses
  D2Bridge.Instance,
  GooglChartsWebApp;

{$IFNDEF FPC}
{$R *.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF} 

constructor TGooglChartsSession.Create(APrismSession: TPrismSession); //OnNewSession
begin
 inherited;

 //Your code

end;

destructor TGooglChartsSession.Destroy; //OnCloseSession
begin
 //Close ALL DataBase connection
 //Ex: Dm.DBConnection.Close;

 inherited;
end;

end.

