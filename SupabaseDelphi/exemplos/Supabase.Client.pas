(*

// SELECT
Supa.Table('clientes')
    .Select('id,nome')
    .Eq('ativo','true')
    .Limit(5)
    .Execute;

// INSERT
Supa.Table('clientes')
    .Insert('{"nome":"João","email":"joao@teste.com"}')
    .Execute;

// UPDATE
Supa.Table('clientes')
    .Update('{"nome":"João da Silva"}')
    .Eq('id','123')
    .Execute;

// DELETE
Supa.Table('clientes')
    .Delete
    .Eq('id','123')
    .Execute;
*)

unit Supabase.Client;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Net.HttpClient,
  System.Net.HttpClientComponent,
  System.JSON;

type
  TSupabaseOperation = (opNone, opSelect, opInsert, opUpdate, opDelete);

  ISupabaseRequest = interface
    ['{4D6D8F4D-5B35-4BC6-91C0-1F22C1C82E8D}']
    function Table(const ATable: string): ISupabaseRequest;
    function Select(const AFields: string = '*'): ISupabaseRequest;
    function Insert(const AJson: string): ISupabaseRequest;
    function Update(const AJson: string): ISupabaseRequest;
    function Delete: ISupabaseRequest;
    function Eq(const AField, AValue: string): ISupabaseRequest;
    function Limit(ALimit: Integer): ISupabaseRequest;
    function OnConflict(const AField: string): ISupabaseRequest;
    function Prefer(const AValue: string): ISupabaseRequest;
    function Execute: TJSONArray;
  end;

  TSupabaseClient = class(TInterfacedObject, ISupabaseRequest)
  private
    FBaseUrl: string;
    FApiKey: string;
    FTable: string;
    FSelect: string;
    FFilter: string;
    FLimit: string;
    FOnConflict: string;
    FPrefer: string;
    FOperation: TSupabaseOperation;
    FBody: string;
    FHttp: TNetHTTPClient;
  public
    constructor Create(const ABaseUrl, AApiKey: string);
    destructor Destroy; override;

    class function New(const ABaseUrl, AApiKey: string): ISupabaseRequest;

    function Table(const ATable: string): ISupabaseRequest;
    function Select(const AFields: string = '*'): ISupabaseRequest;
    function Insert(const AJson: string): ISupabaseRequest;
    function Update(const AJson: string): ISupabaseRequest;
    function Delete: ISupabaseRequest;
    function Eq(const AField, AValue: string): ISupabaseRequest;
    function Limit(ALimit: Integer): ISupabaseRequest;
    function OnConflict(const AField: string): ISupabaseRequest;
    function Prefer(const AValue: string): ISupabaseRequest;
    function Execute: TJSONArray;
  end;

implementation

{ TSupabaseClient }

constructor TSupabaseClient.Create(const ABaseUrl, AApiKey: string);
begin
  FBaseUrl := ABaseUrl;
  FApiKey := AApiKey;
  FHttp := TNetHTTPClient.Create(nil);
  FOperation := opNone;
  FOnConflict := '';
  FPrefer := '';
end;

destructor TSupabaseClient.Destroy;
begin
  FHttp.Free;
  inherited;
end;

class function TSupabaseClient.New(const ABaseUrl, AApiKey: string): ISupabaseRequest;
begin
  Result := TSupabaseClient.Create(ABaseUrl, AApiKey);
end;

function TSupabaseClient.Table(const ATable: string): ISupabaseRequest;
begin
  FTable := ATable;
  Result := Self;
end;

function TSupabaseClient.Select(const AFields: string): ISupabaseRequest;
begin
  FSelect := 'select=' + AFields;
  FOperation := opSelect;
  Result := Self;
end;

function TSupabaseClient.Insert(const AJson: string): ISupabaseRequest;
begin
  FBody := AJson;
  FOperation := opInsert;
  Result := Self;
end;

function TSupabaseClient.Update(const AJson: string): ISupabaseRequest;
begin
  FBody := AJson;
  FOperation := opUpdate;
  Result := Self;
end;

function TSupabaseClient.Delete: ISupabaseRequest;
begin
  FOperation := opDelete;
  Result := Self;
end;

function TSupabaseClient.Eq(const AField, AValue: string): ISupabaseRequest;
begin
  if FFilter <> '' then
    FFilter := FFilter + '&';
  FFilter := FFilter + Format('%s=eq.%s', [AField, AValue]);
  Result := Self;
end;

function TSupabaseClient.Limit(ALimit: Integer): ISupabaseRequest;
begin
  FLimit := Format('limit=%d', [ALimit]);
  Result := Self;
end;

function TSupabaseClient.OnConflict(const AField: string): ISupabaseRequest;
begin
  FOnConflict := AField;
  Result := Self;
end;

function TSupabaseClient.Prefer(const AValue: string): ISupabaseRequest;
begin
  FPrefer := AValue;
  Result := Self;
end;

function TSupabaseClient.Execute: TJSONArray;
var
  LUrl: string;
  LResp: IHTTPResponse;
  LJson: TJSONValue;
begin
  Result := nil;
  LUrl := Format('%s/rest/v1/%s?', [FBaseUrl, FTable]);

  if (FOperation = opSelect) and (FSelect <> '') then
    LUrl := LUrl + FSelect;

  if FFilter <> '' then
    LUrl := LUrl + '&' + FFilter;

  if FLimit <> '' then
    LUrl := LUrl + '&' + FLimit;

  if (FOnConflict <> '') and (FOperation in [opInsert, opUpdate]) then
    LUrl := LUrl + '&on_conflict=' + FOnConflict;

  FHttp.CustomHeaders['apikey'] := FApiKey;
  FHttp.CustomHeaders['Authorization'] := 'Bearer ' + FApiKey;
  FHttp.CustomHeaders['Content-Type'] := 'application/json';
  if FPrefer <> '' then
    FHttp.CustomHeaders['Prefer'] := FPrefer;

  case FOperation of
    opSelect:  LResp := FHttp.Get(LUrl);
    opInsert:  LResp := FHttp.Post(LUrl, TStringStream.Create(FBody, TEncoding.UTF8));
    opUpdate:  LResp := FHttp.Patch(LUrl, TStringStream.Create(FBody, TEncoding.UTF8));
    opDelete:  LResp := FHttp.Delete(LUrl);
  else
    raise Exception.Create('Nenhuma operação definida');
  end;

  if Assigned(LResp) and (LResp.StatusCode in [200,201,204]) then
  begin
    if LResp.ContentAsString <> '' then
    begin
      LJson := TJSONObject.ParseJSONValue(LResp.ContentAsString);
      try
        if Assigned(LJson) and (LJson is TJSONArray) then
          Result := TJSONArray(LJson.Clone)
        else if Assigned(LJson) then
        begin
          Result := TJSONArray.Create;
          Result.AddElement(LJson.Clone as TJSONValue);
        end
        else
          Result := TJSONArray.Create;
      finally
        LJson.Free;
      end;
    end
    else
      Result := TJSONArray.Create;
  end
  else
    raise Exception.CreateFmt('Erro na requisição Supabase: %d - %s',
      [LResp.StatusCode, LResp.ContentAsString]);
end;

end.
