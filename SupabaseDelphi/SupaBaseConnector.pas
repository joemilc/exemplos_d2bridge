unit SupaBaseConnector;

interface

uses System.SysUtils,
  CriaRESTClient;

type

  ISupabase = interface
    ['{06F118C0-1919-4D6C-A090-F91B7428D672}']
    function Receber(Tabela, Campos, Filtros, Ordenacao: String; Limite, Offset: Integer): String;
    function Enviar(Tabela, JsonBody: String): Boolean;
    function Atualizar(Tabela, Id, JsonBody: String): Boolean;
    function Deletar(Tabela, Id: String): Boolean;
  end;

  TSupabase = class(TInterfacedObject, ISupabase)
  private
    FRestClient: ICriaRESTClient;
    procedure Autenticar;
  public
    function Receber(Tabela, Campos, Filtros, Ordenacao: String; Limite, Offset: Integer): String;
    function Enviar(Tabela, JsonBody: String): Boolean;
    function Atualizar(Tabela, Id, JsonBody: String): Boolean;
    function Deletar(Tabela, Id: String): Boolean;

    constructor Create;
    destructor Destroy; override;
    class function New: ISupabase;
  end;

const
  BASE_URL = 'https://oyfmzmochxxxhgnlaera.supabase.co';
  API_KEY = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6Im95Zm16bW9jaHh4eGhnbmxhZXJhIiwicm9sZSI6ImFub24iLCJpYXQiOjE3NjYwNzgyOTIsImV4cCI6MjA4MTY1NDI5Mn0.lD84g0ShkOgDwVITzaqMa4pZhjRBfk-hhJJXfYT8reU';

implementation

constructor TSupabase.Create;
begin
  FRestClient := TCriaRESTClient.New;
  Autenticar;
end;

procedure TSupabase.Autenticar;
begin
  FRestClient
    .SetURL(BASE_URL)
    .AddHeader('apikey', API_KEY)
    .AddHeader('Authorization', 'Bearer ' + API_KEY)
    .AddHeader('Content-Type', 'application/json');
end;

function TSupabase.Receber(Tabela, Campos, Filtros, Ordenacao: String; Limite, Offset: Integer): String;
begin
  FRestClient
    .SetRota(Tabela)
    .AddParams('select', Campos);  // Define os campos a serem retornados

  if Filtros <> '' then
    FRestClient.AddParams('filters', Filtros);

  if Ordenacao <> '' then
    FRestClient.AddParams('order', Ordenacao);

  if Limite > 0 then
    FRestClient.AddParams('limit', IntToStr(Limite));

  if Offset >= 0 then
    FRestClient.AddParams('offset', IntToStr(Offset));

  FRestClient.Get;
  Result := FRestClient.GetJsonRetorno;
end;

function TSupabase.Enviar(Tabela, JsonBody: String): Boolean;
begin
  FRestClient
    .SetRota(Tabela)
    .AddBody(JsonBody)
    .Post;
  Result := FRestClient.GetStatusCode = 201;
end;

class function TSupabase.New: ISupabase;
begin
  Result := Self.Create;
end;

function TSupabase.Atualizar(Tabela, Id, JsonBody: String): Boolean;
begin
  FRestClient
    .SetRota(Tabela + '?id=eq.' + Id)
    .AddBody(JsonBody)
    .Patch;
  Result := FRestClient.GetStatusCode = 200;
end;

function TSupabase.Deletar(Tabela, Id: String): Boolean;
begin
  FRestClient
    .SetRota(Tabela + '?id=eq.' + Id)
    .Delete;
  Result := FRestClient.GetStatusCode = 204;
end;

destructor TSupabase.Destroy;
begin

  inherited;
end;

end.
