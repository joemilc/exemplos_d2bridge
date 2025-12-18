unit CadastrosService;

interface

uses
  FireDAC.Comp.Client, FireDAC.Stan.Intf, Data.DB, System.SysUtils,
  System.JSON, SupaBaseConnector, System.DateUtils, Vcl.Dialogs;

type
  TCadastrosService = class
  public
    class procedure ConfigurarDataSet(ADataSet: TFDMemTable);
    class procedure CarregarDados(ADataSet: TFDMemTable);
  end;

implementation

{ TCadastrosService }

class procedure TCadastrosService.ConfigurarDataSet(ADataSet: TFDMemTable);
begin
  if ADataSet.FieldCount = 0 then
  begin
    ADataSet.FieldDefs.Add('id', ftInteger);
    ADataSet.FieldDefs.Add('nome', ftString, 255);
    ADataSet.FieldDefs.Add('email', ftString, 255);
    ADataSet.FieldDefs.Add('telefone', ftString, 20);
    ADataSet.FieldDefs.Add('ativo', ftBoolean);
    ADataSet.FieldDefs.Add('created_at', ftDateTime);
    ADataSet.FieldDefs.Add('updated_at', ftDateTime);
    ADataSet.CreateDataSet;
  end;
end;

class procedure TCadastrosService.CarregarDados(ADataSet: TFDMemTable);
var
  Supabase: ISupabase;
  JsonStr: String;
  JsonArray: TJSONArray;
  JsonValue: TJSONValue;
begin
  if not ADataSet.Active then
    ADataSet.Open;

  ADataSet.EmptyDataSet;

  Supabase := TSupabase.New;
  try
    JsonStr := Supabase.Receber('cadastros', '*', '', 'id', 100, 0);

    if (JsonStr <> '') and (JsonStr <> '[]') then
    begin
      JsonValue := TJSONObject.ParseJSONValue(JsonStr);
      if JsonValue is TJSONArray then
      begin
        JsonArray := TJSONArray(JsonValue);
        try
          for JsonValue in JsonArray do
          begin
            ADataSet.Append;
            ADataSet.FieldByName('id').AsInteger := JsonValue.GetValue<Integer>('id');
            ADataSet.FieldByName('nome').AsString := JsonValue.GetValue<String>('nome');
            ADataSet.FieldByName('email').AsString := JsonValue.GetValue<String>('email');
            ADataSet.FieldByName('telefone').AsString := JsonValue.GetValue<String>('telefone');
            ADataSet.FieldByName('ativo').AsBoolean := JsonValue.GetValue<Boolean>('ativo');

            if not JsonValue.GetValue<String>('created_at').IsEmpty then
              ADataSet.FieldByName('created_at').AsDateTime := ISO8601ToDate(JsonValue.GetValue<String>('created_at'));

            if not JsonValue.GetValue<String>('updated_at').IsEmpty then
              ADataSet.FieldByName('updated_at').AsDateTime := ISO8601ToDate(JsonValue.GetValue<String>('updated_at'));

            ADataSet.Post;
          end;
        finally
          JsonArray.Free;
        end;
      end
      else if Assigned(JsonValue) then
        JsonValue.Free;
    end;
  except
    on E: Exception do
      ShowMessage('Erro ao carregar dados: ' + E.Message);
  end;
end;

end.
