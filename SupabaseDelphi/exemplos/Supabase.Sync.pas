unit Supabase.Sync;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections,
  System.StrUtils,
  System.IOUtils,
  System.DateUtils,
  Data.DB,
  Supabase.Client,
  DBAccess,
  Uni,
  SupaBaseConnector;

type

  TSupabaseSync = class
  private
    FClient: ISupabaseRequest;
    function BuildJSONArray(const DS: TDataSet; const FieldNames: array of string): TJSONArray;
    function GetExistingColumns(const Conn: TUniConnection; const Table: string): TArray<string>;
    procedure UpsertJSONArray(const Conn: TUniConnection; const Table, KeyColumn: string; const Arr: TJSONArray);
    procedure UpsertFichaCampoDados(const Conn: TUniConnection; const Arr: TJSONArray);

    // Export helpers (private por tabela)
    function ExportFamilias(const DS: TDataSet): Boolean;
    function ExportNomeCientifico(const DS: TDataSet): Boolean;
    function ExportEspecies(const DS: TDataSet): Boolean;
    function ExportProjetos(const DS: TDataSet): Boolean;
    function ExportTalhoes(const DS: TDataSet): Boolean;
    function ExportFichaCampo(const DS: TDataSet): Boolean; // NÃO exporta fichacampo_dados

    // Import helpers (private por tabela)
    function ImportFamilias: TJSONArray;
    function ImportNomeCientifico: TJSONArray;
    function ImportEspecies: TJSONArray;
    function ImportProjetos: TJSONArray;
    function ImportTalhoes: TJSONArray;
    function ImportFichaCampo: TJSONArray;
    function ImportFichaCampoDados: TJSONArray;
  public
    constructor Create;

    // Orquestra exportação: chama as privadas conforme datasets informados
    procedure Exportar(
      const DSFamilias, DSNomeCientifico, DSEspecies,
      DSProjetos, DSTalhoes, DSFichaCampo: TDataSet);

    // Orquestra importação: retorna dados de cada tabela (sem fichacampo_dados)
    procedure Importar(
      out JFamilias, JNomeCientifico, JEspecies,
      JProjetos, JTalhoes, JFichaCampo: TJSONArray);

    // Importação dedicada de fichacampo_dados (não exportamos esta tabela)
    function ImportarFichaCampoDados: TJSONArray;
    procedure ImportarParaMySQL(const Conn: TUniConnection);
    procedure ImportarFichaCampoDadosParaMySQL(
      const Conn: TUniConnection;
      AIdProjeto: Integer = 0;
      AIdTalhao: Integer = 0);
    procedure DeletarProjetoCascade(const AIdProjeto: Integer);
  end;

implementation

{ TSupabaseSync }

uses uFuncoes, Mensagens, uDados;

constructor TSupabaseSync.Create;
begin
  FClient := TSupabaseClient.New(BASE_URL, API_KEY);
end;

function TSupabaseSync.BuildJSONArray(const DS: TDataSet; const FieldNames: array of string): TJSONArray;
var
  LArray: TJSONArray;
  LObj: TJSONObject;
  I, J: Integer;
  F: TField;
begin
  LArray := TJSONArray.Create;
  if Assigned(DS) then
  begin
    DS.First;
    while not DS.Eof do
    begin
      LObj := TJSONObject.Create;
      for I := Low(FieldNames) to High(FieldNames) do
      begin
        F := DS.FieldByName(FieldNames[I]);
        if not Assigned(F) then
          Continue;
        if F.IsNull then
          LObj.AddPair(FieldNames[I], TJSONNull.Create)
        else
        begin
          case F.DataType of
            ftSmallint, ftInteger, ftWord, ftAutoInc, ftLongWord:
              LObj.AddPair(FieldNames[I], TJSONNumber.Create(F.AsInteger));
            ftFloat, ftCurrency, ftBCD, ftFMTBcd:
              LObj.AddPair(FieldNames[I], TJSONNumber.Create(F.AsFloat));
            ftBoolean:
              LObj.AddPair(FieldNames[I], TJSONBool.Create(F.AsBoolean));
            ftDate, ftTime, ftDateTime, ftTimeStamp:
              LObj.AddPair(FieldNames[I], TJSONString.Create(FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz', F.AsDateTime)));
          else
            LObj.AddPair(FieldNames[I], TJSONString.Create(F.AsString));
          end;
        end;
      end;
      LArray.AddElement(LObj);
      DS.Next;
    end;
  end;
  Result := LArray;
end;

procedure TSupabaseSync.Exportar(
  const DSFamilias, DSNomeCientifico, DSEspecies,
  DSProjetos, DSTalhoes, DSFichaCampo: TDataSet);
begin
  // Upsert para cada tabela; on_conflict no ID principal
  if Assigned(DSFamilias) then
    ExportFamilias(DSFamilias);
  if Assigned(DSNomeCientifico) then
    ExportNomeCientifico(DSNomeCientifico);
  if Assigned(DSEspecies) then
    ExportEspecies(DSEspecies);
  if Assigned(DSProjetos) then
    ExportProjetos(DSProjetos);
  if Assigned(DSTalhoes) then
    ExportTalhoes(DSTalhoes);
  if Assigned(DSFichaCampo) then
    ExportFichaCampo(DSFichaCampo);
  // Atenção: NÃO exportamos cf_engf_fichacampo_dados
  MostraMensagem('Envio concluído.');
end;

procedure TSupabaseSync.Importar(
  out JFamilias, JNomeCientifico, JEspecies,
  JProjetos, JTalhoes, JFichaCampo: TJSONArray);
begin
  JFamilias := ImportFamilias;
  JNomeCientifico := ImportNomeCientifico;
  JEspecies := ImportEspecies;
  JProjetos := ImportProjetos;
  JTalhoes := ImportTalhoes;
  JFichaCampo := ImportFichaCampo;
  // Para cf_engf_fichacampo_dados, use o método ImportarFichaCampoDados
end;

function TSupabaseSync.ExportFamilias(const DS: TDataSet): Boolean;
var
  J: TJSONArray;
begin
  Result := False;
  J := BuildJSONArray(DS, ['id_familia', 'familia']);
  try
    FClient
      .Table('cf_engf_familias')
      .OnConflict('id_familia')
      .Prefer('resolution=merge-duplicates,return=minimal')
      .Insert(J.ToString)
      .Execute;
    Result := True;
  finally
    J.Free;
  end;
end;

function TSupabaseSync.ExportNomeCientifico(const DS: TDataSet): Boolean;
var
  J: TJSONArray;
begin
  Result := False;
  J := BuildJSONArray(DS, ['id_nomecientifico', 'codigo', 'nome_cientifico', 'nome_popular']);
  try
    FClient
      .Table('cf_engf_nomecientifico')
      .OnConflict('id_nomecientifico')
      .Prefer('resolution=merge-duplicates,return=minimal')
      .Insert(J.ToString)
      .Execute;
    Result := True;
  finally
    J.Free;
  end;
end;

function TSupabaseSync.ExportEspecies(const DS: TDataSet): Boolean;
var
  J: TJSONArray;
begin
  Result := False;
  J := BuildJSONArray(DS, ['id_especies', 'id_familia', 'id_nomecientifico', 'finalidade', 'categoria']);
  try
    FClient
      .Table('cf_engf_especies')
      .OnConflict('id_especies')
      .Prefer('resolution=merge-duplicates,return=minimal')
      .Insert(J.ToString)
      .Execute;
    Result := True;
  finally
    J.Free;
  end;
end;

function TSupabaseSync.ExportProjetos(const DS: TDataSet): Boolean;
var
  J: TJSONArray;
begin
  Result := False;
  J := BuildJSONArray(DS, ['id_projeto', 'id_cliente', 'descricao', 'proprietario', 'ano', 'area', 'perc_casca', 'fator_forma',
    'processo', 'porta_semente', 'tipo_projeto', 'data', 'situacao', 'data_finalizacao', 'valor', 'parcelas']);
  try
    try
      FClient
        .Table('cf_engf_projetos')
        .OnConflict('id_projeto')
        .Prefer('resolution=merge-duplicates,return=minimal')
        .Insert(J.ToString)
        .Execute;
      Result := True;
    except
      On E: Exception do
      begin
        MostraMensagem(E.Message);
        //TFile.WriteAllText('d:\erro.txt', E.Message);
      end;
    end;
  finally
    J.Free;
  end;
end;

function TSupabaseSync.ExportTalhoes(const DS: TDataSet): Boolean;
var
  J: TJSONArray;
begin
  Result := False;
  J := BuildJSONArray(DS, ['id_talhao', 'id_projeto', 'numero', 'area', 'faixas', 'guid_id']);
  //TFile.WriteAllText('d:\talhoes.json', J.ToString);
  try
    try
      FClient
        .Table('cf_engf_talhoes')
        .OnConflict('id_talhao')
        .Prefer('resolution=merge-duplicates,return=minimal')
        .Insert(J.ToString)
        .Execute;
      Result := True;
    except
      On E: Exception do
      begin
        MostraMensagem(E.Message);
        //TFile.WriteAllText('d:\erro.txt', E.Message);
      end;
    end;
  finally
    J.Free;
  end;
end;

function TSupabaseSync.ExportFichaCampo(const DS: TDataSet): Boolean;
var
  J: TJSONArray;
begin
  Result := False;
  J := BuildJSONArray(DS, ['id_fichacampo', 'numero', 'data', 'guid_id']);
  try
    FClient
      .Table('cf_engf_fichacampo')
      .OnConflict('id_fichacampo')
      .Prefer('resolution=merge-duplicates,return=minimal')
      .Insert(J.ToString)
      .Execute;
    Result := True;
  finally
    J.Free;
  end;
end;

function TSupabaseSync.ImportFamilias: TJSONArray;
begin
  Result := FClient.Table('cf_engf_familias').Select('*').Execute;
end;

function TSupabaseSync.ImportNomeCientifico: TJSONArray;
begin
  Result := FClient.Table('cf_engf_nomecientifico').Select('*').Execute;
end;

function TSupabaseSync.ImportEspecies: TJSONArray;
begin
  Result := FClient.Table('cf_engf_especies').Select('*').Execute;
end;

function TSupabaseSync.ImportProjetos: TJSONArray;
begin
  Result := FClient.Table('cf_engf_projetos').Select('*').Execute;
end;

function TSupabaseSync.ImportTalhoes: TJSONArray;
begin
  Result := FClient.Table('cf_engf_talhoes').Select('*').Execute;
end;

function TSupabaseSync.ImportFichaCampo: TJSONArray;
begin
  Result := FClient.Table('cf_engf_fichacampo').Select('*').Execute;
end;

function TSupabaseSync.ImportFichaCampoDados: TJSONArray;
var
  I: Integer;
  V: TJSONValue;
  O: TJSONObject;
begin
  Result := FClient.Table('cf_engf_fichacampo_dados').Select('*').Execute;
  // Ajuste solicitado: Q Fust sempre = 1
  if Assigned(Result) then
  begin
    for I := 0 to Result.Count - 1 do
    begin
      V := Result.Items[I];
      if (V is TJSONObject) then
      begin
        O := TJSONObject(V);
        // Remove valor anterior (se existir) e define 1
        O.RemovePair('fust');
        O.AddPair('fust', TJSONNumber.Create(1));
      end;
    end;
  end;
end;

function TSupabaseSync.ImportarFichaCampoDados: TJSONArray;
begin
  Result := ImportFichaCampoDados;
end;

function TSupabaseSync.GetExistingColumns(const Conn: TUniConnection; const Table: string): TArray<string>;
var
  Q: TUniQuery;
  L: TList<string>;
begin
  Q := TUniQuery.Create(nil);
  L := TList<string>.Create;
  try
    Q.Connection := Conn;
    //Q.SQL.Text := 'SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_SCHEMA = :db AND TABLE_NAME = :t';
    //Q.ParamByName('db').AsString := Conn.Database;
    //Q.ParamByName('t').AsString := Table;
    Q.SQL.Text := 'DESC '+Table;
    Q.Open;
    while not Q.Eof do
    begin
      L.Add(Q.Fields[0].AsString);
      Q.Next;
    end;
    Result := L.ToArray;
  finally
    L.Free;
    Q.Free;
  end;
end;

procedure TSupabaseSync.UpsertJSONArray(const Conn: TUniConnection; const Table, KeyColumn: string; const Arr: TJSONArray);
var
  Cols: TArray<string>;
  FirstObj: TJSONObject;
  Keys: TArray<string>;
  I: Integer;
  O: TJSONObject;
  Q: TUniQuery;
  ColList, PlaceList, UpdateList: string;
  K: string;
  V: TJSONValue;
begin
  if (Arr = nil) or (Arr.Count = 0) then
    Exit;
  FirstObj := Arr.Items[0] as TJSONObject;
  SetLength(Keys, FirstObj.Count);
  for I := 0 to FirstObj.Count - 1 do
    Keys[I] := FirstObj.Pairs[I].JsonString.Value;
  Cols := GetExistingColumns(Conn, Table);
  ColList := '';
  PlaceList := '';
  UpdateList := '';
  for K in Keys do
    if MatchText(K, Cols) then
    begin
      if ColList <> '' then
      begin
        ColList := ColList + ',';
        PlaceList := PlaceList + ',';
        UpdateList := UpdateList + ', ';
      end;
      ColList := ColList + K;
      PlaceList := PlaceList + ':' + 'p_' + K;
      if K <> KeyColumn then
        UpdateList := UpdateList + K + '=VALUES(' + K + ')';
    end;
  if (ColList = '') then
    Exit;
  Q := TUniQuery.Create(nil);
  try
    Q.Connection := Conn;
    Q.SQL.Text := 'INSERT INTO ' + Table + ' (' + ColList + ') VALUES (' + PlaceList + ') ON DUPLICATE KEY UPDATE ' + UpdateList;
    for I := 0 to Arr.Count - 1 do
    begin
      O := Arr.Items[I] as TJSONObject;
      for K in Keys do
        if MatchText(K, Cols) then
        begin
          V := O.GetValue(K);
          if (V = nil) or (V is TJSONNull) then
            Q.ParamByName('p_' + K).Clear
          else if V is TJSONNumber then
            Q.ParamByName('p_' + K).AsFloat := TJSONNumber(V).AsDouble
          else if V is TJSONBool then
            Q.ParamByName('p_' + K).AsBoolean := TJSONBool(V).AsBoolean
          else
          begin
            if (MatchStr(LowerCase(K), ['created_at', 'updated_at', 'data'])) and (V.Value <> '') then
            begin
              try
                Q.ParamByName('p_' + K).AsDateTime := ISO8601ToDate(V.Value);
              except
                Q.ParamByName('p_' + K).AsString := V.Value;
              end;
            end
            else
              Q.ParamByName('p_' + K).AsString := V.Value;
          end;
        end;
      Q.ExecSQL;
    end;
  finally
    Q.Free;
  end;
end;

procedure TSupabaseSync.ImportarParaMySQL(const Conn: TUniConnection);
var
  Proj, Talh, Ficha, Dados: TJSONArray;
begin
  if not Conn.Connected then
    Conn.Connect;
  Proj := ImportProjetos;
  Talh := ImportTalhoes;
  Ficha := ImportFichaCampo;
  Dados := ImportFichaCampoDados;
  try
    UpsertJSONArray(Conn, 'engf_projetos', 'id_projeto', Proj);
    UpsertJSONArray(Conn, 'engf_talhoes', 'id_talhao', Talh);
    UpsertJSONArray(Conn, 'engf_fichacampo', 'id_fichacampo', Ficha);
    UpsertFichaCampoDados(Conn, Dados);
  finally
    Proj.Free;
    Talh.Free;
    Ficha.Free;
    Dados.Free;
  end;
end;

procedure TSupabaseSync.ImportarFichaCampoDadosParaMySQL(
  const Conn: TUniConnection;
  AIdProjeto: Integer;
  AIdTalhao: Integer);
var
  ArrAll, Arr, ArrTalhoes: TJSONArray;
  I: Integer;
  J: Integer;
  V: TJSONValue;
  O: TJSONObject;
  TalhaoIds: TList<Integer>;
begin
  TalhaoIds := TList<Integer>.Create;
  ArrAll := TJSONArray.Create;
  try
    if not Conn.Connected then
      Conn.Connect;

    if AIdTalhao > 0 then
      TalhaoIds.Add(AIdTalhao)
    else if AIdProjeto > 0 then
    begin
      ArrTalhoes := FClient.Table('cf_engf_talhoes').Select('*').Eq('id_projeto', IntToStr(AIdProjeto)).Execute;
      try
        for I := 0 to ArrTalhoes.Count - 1 do
        begin
          V := ArrTalhoes.Items[I];
          if V is TJSONObject then
          begin
            O := TJSONObject(V);
            if Assigned(O.GetValue('id_talhao')) and (O.GetValue('id_talhao') is TJSONNumber) then
              TalhaoIds.Add(TJSONNumber(O.GetValue('id_talhao')).AsInt);
          end;
        end;
      finally
        ArrTalhoes.Free;
      end;
    end;

    if TalhaoIds.Count > 0 then
    begin
      for I := 0 to TalhaoIds.Count - 1 do
      begin
        Arr := FClient.Table('cf_engf_fichacampo_dados').Select('*').Eq('id_talhao', IntToStr(TalhaoIds[I])).Execute;
        try
          // Ajuste: fust = 1 e merge no array final
          if Assigned(Arr) then
          begin
            J := 0;
            while J < Arr.Count do
            begin
              V := Arr.Items[J];
              if V is TJSONObject then
              begin
                O := TJSONObject(V);
                O.RemovePair('fust');
                O.AddPair('fust', TJSONNumber.Create(1));
                ArrAll.AddElement(O.Clone as TJSONValue);
              end;
              Inc(J);
            end;
          end;
        finally
          Arr.Free;
        end;
      end;
    end
    else
    begin
      Arr := ImportFichaCampoDados; // já ajusta fust = 1
      try
        for I := 0 to Arr.Count - 1 do
          ArrAll.AddElement((Arr.Items[I] as TJSONValue).Clone as TJSONValue);
      finally
        Arr.Free;
      end;
    end;

    UpsertFichaCampoDados(Conn, ArrAll);
  finally
    ArrAll.Free;
    TalhaoIds.Free;
  end;
end;

procedure TSupabaseSync.UpsertFichaCampoDados(const Conn: TUniConnection; const Arr: TJSONArray);
var
  Cols: TArray<string>;
  FirstObj: TJSONObject;
  Keys: TArray<string>;
  I: Integer;
  O: TJSONObject;
  Q: TUniQuery;
  ColList, PlaceList, UpdateList: string;
  K: string;
  V: TJSONValue;
begin
  if (Arr = nil) or (Arr.Count = 0) then
    Exit;

  FirstObj := Arr.Items[0] as TJSONObject;
  SetLength(Keys, FirstObj.Count);
  for I := 0 to FirstObj.Count - 1 do
    Keys[I] := FirstObj.Pairs[I].JsonString.Value;
  Cols := GetExistingColumns(Conn, 'engf_fichacampo_dados');
  ColList := '';
  PlaceList := '';
  UpdateList := '';
  for K in Keys do
    if MatchText(K, Cols) and not SameText(K, 'id_fichacampo_dados') then
    begin
      if ColList <> '' then
      begin
        ColList := ColList + ',';
        PlaceList := PlaceList + ',';
        UpdateList := UpdateList + ', ';
      end;
      ColList := ColList + K;
      PlaceList := PlaceList + ':' + 'p_' + K;
      UpdateList := UpdateList + K + '=VALUES(' + K + ')';
    end;
  if (ColList = '') then
    Exit;
  Q := TUniQuery.Create(nil);
  try
    Q.Connection := Conn;
    Q.SQL.Text := 'INSERT INTO engf_fichacampo_dados (' + ColList + ') VALUES (' + PlaceList + ') ON DUPLICATE KEY UPDATE ' +
      UpdateList;
    for I := 0 to Arr.Count - 1 do
    begin
      O := Arr.Items[I] as TJSONObject;
      for K in Keys do
        if MatchText(K, Cols) and not SameText(K, 'id_fichacampo_dados') then
        begin
          V := O.GetValue(K);
          if (V = nil) or (V is TJSONNull) then
            Q.ParamByName('p_' + K).Clear
          else if V is TJSONNumber then
            Q.ParamByName('p_' + K).AsFloat := TJSONNumber(V).AsDouble
          else if V is TJSONBool then
            Q.ParamByName('p_' + K).AsBoolean := TJSONBool(V).AsBoolean
          else
          begin
            if (MatchStr(LowerCase(K), ['created_at', 'updated_at', 'data'])) and (V.Value <> '') then
            begin
              try
                Q.ParamByName('p_' + K).AsDateTime := ISO8601ToDate(V.Value);
              except
                Q.ParamByName('p_' + K).AsString := V.Value;
              end;
            end
            else
              Q.ParamByName('p_' + K).AsString := V.Value;
          end;
        end;
      Q.ExecSQL;
    end;
  finally
    Q.Free;
  end;
end;

procedure TSupabaseSync.DeletarProjetoCascade(const AIdProjeto: Integer);
var
  ArrTalhoes: TJSONArray;
  I: Integer;
  V: TJSONValue;
  O: TJSONObject;
  TalhaoId: Integer;
  Guid: string;
begin
  ArrTalhoes := FClient.Table('cf_engf_talhoes').Select('id_talhao,guid_id').Eq('id_projeto', IntToStr(AIdProjeto)).Execute;
  try
    for I := 0 to ArrTalhoes.Count - 1 do
    begin
      V := ArrTalhoes.Items[I];
      if V is TJSONObject then
      begin
        O := TJSONObject(V);
        TalhaoId := 0;
        if Assigned(O.GetValue('id_talhao')) and (O.GetValue('id_talhao') is TJSONNumber) then
          TalhaoId := TJSONNumber(O.GetValue('id_talhao')).AsInt;
        if TalhaoId <> 0 then
          FClient.Table('cf_engf_fichacampo_dados').Delete.Eq('id_talhao', IntToStr(TalhaoId)).Execute;
        Guid := '';
        if Assigned(O.GetValue('guid_id')) then
          Guid := O.GetValue('guid_id').Value;
        if Guid <> '' then
          FClient.Table('cf_engf_fichacampo').Delete.Eq('guid_id', Guid).Execute;
      end;
    end;
  finally
    ArrTalhoes.Free;
  end;
  FClient.Table('cf_engf_talhoes').Delete.Eq('id_projeto', IntToStr(AIdProjeto)).Execute;
  FClient.Table('cf_engf_projetos').Delete.Eq('id_projeto', IntToStr(AIdProjeto)).Execute;
end;

end.
