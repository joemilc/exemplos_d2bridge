unit CriaRESTClient;

interface

uses
  REST.Types,
  REST.Client,
  System.Net.HttpClient;

type

  ICriaRESTClient = interface
    ['{101B4F6E-89BB-434D-A6A3-C8977CFB7D52}']
    function SetURL(AURL: String): ICriaRESTClient;
    function SetRota(ARota: String): ICriaRESTClient;
    function GetStatusCode: Integer;
    function GetStatusText: String;
    function GetJsonRetorno: String;
    function AddHeader(AName, AValue: String): ICriaRESTClient;
    function AddParams(AName, AValue: String): ICriaRESTClient;
    function AddBody(AJson: String): ICriaRESTClient;
    function Get: ICriaRESTClient;
    function Post: ICriaRESTClient;
    function Put: ICriaRESTClient;
    function Delete: ICriaRESTClient;
    function Patch: ICriaRESTClient;
  end;

  TCriaRESTClient = class(TInterfacedObject, ICriaRESTClient)
  private
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    procedure Execute;
  public
    function SetURL(AURL: String): ICriaRESTClient;
    function SetRota(ARota: String): ICriaRESTClient;

    function GetStatusCode: Integer;
    function GetStatusText: String;
    function GetJsonRetorno: String;

    function AddHeader(AName, AValue: String): ICriaRESTClient;
    function AddParams(AName, AValue: String): ICriaRESTClient;
    function AddBody(AJson: String): ICriaRESTClient;

    function Get: ICriaRESTClient;
    function Post: ICriaRESTClient;
    function Put: ICriaRESTClient;
    function Delete: ICriaRESTClient;
    function Patch: ICriaRESTClient;

    constructor Create;
    destructor Destroy; override;
    class function New: ICriaRESTClient;
  end;

implementation

{ TCriaRESTClient }

function TCriaRESTClient.AddBody(AJson: String): ICriaRESTClient;
begin
  Result := Self;
  RESTRequest1.AddBody(AJson, TRESTContentType.ctAPPLICATION_JSON);
end;

function TCriaRESTClient.AddHeader(AName, AValue: String): ICriaRESTClient;
begin
  Result := Self;
  RESTRequest1.AddParameter(AName, AValue, pkHTTPHEADER, [poDoNotEncode]);
end;

function TCriaRESTClient.AddParams(AName, AValue: String): ICriaRESTClient;
begin
  Result := Self;
  RestRequest1.Params.AddItem(AName, AValue, pkGETorPOST);
end;

constructor TCriaRESTClient.Create;
begin
  RESTClient1 := TRESTClient.Create(nil);
  RESTResponse1 := TRESTResponse.Create(nil);
  RESTRequest1 := TRESTRequest.Create(nil);

  RESTClient1.SecureProtocols := [THttpSecureProtocol.SSL2,
    THttpSecureProtocol.SSL3,
    THttpSecureProtocol.TLS1,
    THttpSecureProtocol.TLS11,
    THttpSecureProtocol.TLS12,
    THttpSecureProtocol.TLS13];

  RESTClient1.Params.Clear;

  RESTRequest1.Params.Clear;
  RESTRequest1.AddParameter('Content-Type', 'application/json', pkHTTPHEADER, [poDoNotEncode]);

  RESTRequest1.AssignedValues := [TCustomRESTRequest.TAssignedValue.rvConnectTimeout,
    TCustomRESTRequest.TAssignedValue.rvReadTimeout];
  RESTRequest1.Client   := RESTClient1;
  RESTRequest1.Response := RESTResponse1;
end;

function TCriaRESTClient.Delete: ICriaRESTClient;
begin
  Result := Self;
end;

destructor TCriaRESTClient.Destroy;
begin
  RESTClient1.Free;
  RESTResponse1.Free;
  RESTRequest1.Free;

  inherited;
end;

procedure TCriaRESTClient.Execute;
begin
  RESTRequest1.Execute;
end;

function TCriaRESTClient.Get: ICriaRESTClient;
begin
  Result := Self;
  RESTRequest1.Method := rmGET;
  Execute;
end;

function TCriaRESTClient.GetJsonRetorno: String;
begin
  Result := RESTResponse1.JSONText
end;

function TCriaRESTClient.GetStatusCode: Integer;
begin
  Result := RESTResponse1.StatusCode;
end;

function TCriaRESTClient.GetStatusText: String;
begin
  Result := RESTResponse1.StatusText
end;

class function TCriaRESTClient.New: ICriaRESTClient;
begin
  Result := Self.Create;
end;

function TCriaRESTClient.Patch: ICriaRESTClient;
begin
  Result := Self;
  RESTRequest1.Method := rmPATCH;
  Execute;
end;

function TCriaRESTClient.Post: ICriaRESTClient;
var i: Integer;
begin
  Result := Self;
  RESTRequest1.Method := rmPost;
  Execute;
end;

function TCriaRESTClient.Put: ICriaRESTClient;
begin
  Result := Self;
  RESTRequest1.Method := rmPUT;
  Execute;
end;

function TCriaRESTClient.SetRota(ARota: String): ICriaRESTClient;
begin
  Result := Self;
  RESTRequest1.Resource := ARota;
end;

function TCriaRESTClient.SetURL(AURL: String): ICriaRESTClient;
begin
  Result := Self;
  RESTClient1.BaseURL := AURL;
end;

end.

