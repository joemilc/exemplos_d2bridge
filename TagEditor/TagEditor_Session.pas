unit TagEditor_Session;

interface

uses
  SysUtils, Classes,
  Prism.SessionBase;

type
  TTagEditorSession = class(TPrismSessionBase)
  private

  public
   constructor Create(APrismSession: TPrismSession); override;  //OnNewSession
   destructor Destroy; override; //OnCloseSession
  end;


implementation

Uses
  D2Bridge.Instance,
  TagEditorWebApp;

{$IFNDEF FPC}
{$R *.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF} 

constructor TTagEditorSession.Create(APrismSession: TPrismSession); //OnNewSession
begin
 inherited;

 //Your code

end;

destructor TTagEditorSession.Destroy; //OnCloseSession
begin
 //Close ALL DataBase connection
 //Ex: Dm.DBConnection.Close;

 inherited;
end;

end.

