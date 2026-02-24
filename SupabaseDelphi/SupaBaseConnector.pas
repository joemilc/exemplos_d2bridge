unit SupaBaseConnector;

interface

uses System.SysUtils, System.JSON, Supabase.Client;

type
  ESupabaseError = class(Exception);

  ISupabase = interface
    ['{06F118C0-1919-4D6C-A090-F91B7428D672}']
    function Receber(Tabela, Campos, Filtros, Ordenacao: String; Limite, Offset: Integer): String;
    function Enviar(Tabela, JsonBody: String): Boolean;
    function Atualizar(Tabela, Id, JsonBody: String): Boolean;
    function Deletar(Tabela, Id: String): Boolean;
    function TestarConexao: Boolean;
  end;

  TSupabase = class(TInterfacedObject, ISupabase)
  private
    FClient: ISupabaseRequest;
    procedure VerificarErro(const AResponse: String; StatusCode: Integer);
  public
    constructor Create;
    function Receber(Tabela, Campos, Filtros, Ordenacao: String; Limite, Offset: Integer): String;
    function Enviar(Tabela, JsonBody: String): Boolean;
    function Atualizar(Tabela, Id, JsonBody: String): Boolean;
    function Deletar(Tabela, Id: String): Boolean;
    function TestarConexao: Boolean;
  end;

const
  BASE_URL = 'https://imjbnmucxwjnyzbohsya.supabase.co';
  API_KEY = 'sb_publishable_qmYRSNrJrQRNdbS1W6cBMw_g9J2K5Dc';

implementation

constructor TSupabase.Create;
begin
  FClient := TSupabaseClient.New(BASE_URL, API_KEY);
end;

procedure TSupabase.VerificarErro(const AResponse: String; StatusCode: Integer);
var
  JsonObj: TJSONObject;
  ErrorMsg: String;
begin
  if (StatusCode >= 400) then
  begin
    ErrorMsg := 'Erro na requisição (Status: ' + IntToStr(StatusCode) + ')';
    
    if AResponse <> '' then
    begin
      try
        JsonObj := TJSONObject.ParseJSONValue(AResponse) as TJSONObject;
        if Assigned(JsonObj) then
        try
          if JsonObj.GetValue('error') <> nil then
             ErrorMsg := ErrorMsg + ': ' + JsonObj.GetValue<String>('error')
          else if JsonObj.GetValue('message') <> nil then
             ErrorMsg := ErrorMsg + ': ' + JsonObj.GetValue<String>('message');
        finally
          JsonObj.Free;
        end;
      except
        // Se falhar o parse, mantemos a mensagem genérica ou o raw response se curto
      end;
    end;
    
    raise ESupabaseError.Create(ErrorMsg);
  end;
end;

function TSupabase.Receber(Tabela, Campos, Filtros, Ordenacao: String; Limite, Offset: Integer): String;
var
  J: TJSONArray;
  Field, Dir: String;
begin
  FClient.Table(Tabela).Select(Campos);

  if Filtros <> '' then
    FClient.Filter(Filtros);

  if Ordenacao <> '' then
  begin
    // Simple parsing for "field.desc" or just "field"
    if Pos('.', Ordenacao) > 0 then
    begin
       Field := Copy(Ordenacao, 1, Pos('.', Ordenacao) - 1);
       Dir := Copy(Ordenacao, Pos('.', Ordenacao) + 1, Length(Ordenacao));
       FClient.Order(Field, LowerCase(Dir) <> 'desc');
    end
    else
       FClient.Order(Ordenacao, True);
  end;

  if Limite > 0 then
    FClient.Limit(Limite);

  if Offset >= 0 then
    FClient.Offset(Offset);

  try
    J := FClient.Execute;
    if Assigned(J) then
    begin
      Result := J.ToString;
      J.Free;
    end
    else
      Result := '[]';
  except
    on E: Exception do
    begin
       if E.Message.StartsWith('Erro na requisição Supabase') then
         raise ESupabaseError.Create(E.Message)
       else
         raise;
    end;
  end;
end;

function TSupabase.Enviar(Tabela, JsonBody: String): Boolean;
var
  J: TJSONArray;
begin
  try
    J := FClient
      .Table(Tabela)
      .Insert(JsonBody)
      .Execute;
    Result := True;
    if Assigned(J) then J.Free;
  except
    Result := False;
    raise; 
  end;
end;

function TSupabase.Atualizar(Tabela, Id, JsonBody: String): Boolean;
var
  J: TJSONArray;
begin
  try
    J := FClient
      .Table(Tabela)
      .Update(JsonBody)
      .Eq('id', Id) // Assuming Id is for 'id' column
      .Execute;
    Result := True;
    if Assigned(J) then J.Free;
  except
    Result := False;
    raise;
  end;
end;

function TSupabase.Deletar(Tabela, Id: String): Boolean;
var
  J: TJSONArray;
begin
  try
    J := FClient
      .Table(Tabela)
      .Delete
      .Eq('id', Id)
      .Execute;
    Result := True;
    if Assigned(J) then J.Free;
  except
    Result := False;
    raise;
  end;
end;

function TSupabase.TestarConexao: Boolean;
begin
  try
    // Tenta buscar 1 registro da tabela 'cadastros'
    Receber('cadastros', 'id', '', '', 1, 0);
    Result := True;
  except
    Result := False;
  end;
end;

end.
