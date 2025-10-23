unit uBuildTreeHtml;

interface

uses
  System.SysUtils, System.Classes, System.StrUtils, Datasnap.DBClient, Data.DB;

type
  TTreeViewHTML = class
  private
    class function HasChildren(Q: TDataSet; const Id: Integer): Boolean; static;
    class function BuildTreeHTML(Q: TDataSet; IdPai, Nivel: Integer): string;
    class function GetLabelField(Q: TDataSet): TField;
    class function IsIdField(const FieldName: string): Boolean;
    class procedure FieldToHTML(F: TField; out TDClass, TDValue: string);
  public
    class function CreateDemoDataset: TClientDataSet;
    class function CreateDemoDataset2: TClientDataSet;
    class function CreateUserTreeDataset: TClientDataSet;
    class function CreateProjectDataset: TClientDataSet;
    class function BuildHTMLFromDataset(Q: TDataSet): string;
    class var FGlobalCounter: Integer;
    class var FCurrentPrefix: string;
  end;

implementation

const
  TREEVIEW_STYLE_SCRIPT =
    '<link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css" rel="stylesheet">' +
    '<style>' +
    'ul, #tree { list-style-type: none; margin: 0; padding-left: 0; }' +
    '.caret { cursor: pointer; user-select: none; font-weight: 500; display: inline-flex; align-items: center; }' +
    '.caret::before { content: "▶"; color: #444; display: inline-block; margin-right: 6px; transition: transform .2s; }' +
    '.nested { display: none; }' +
    '.active { display: block; }' +
    '.tree-toggle { position: absolute; left: -9999px; }' +
    '.tree-toggle:checked ~ .nested { display: block; }' +
    '.tree-toggle:checked ~ table .caret::before { transform: rotate(90deg); }' +
    '.table-sm td, .table-sm th { padding: .25rem .5rem; vertical-align: middle; }' +
    '.tree-table { width: 900px; table-layout: fixed; margin-left: 25px; }' +
    '.desc  { width: 280px; font-weight: 600; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }' +
    '.desc-master { font-size: 1.12rem; }' +
    '.leaf-dot { display:inline-block; color:#888; margin-right:6px; }' +
    '.col-str { width: 240px; text-align: left; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }' +
    '.col-date { width: 110px; text-align: center; }' +
    '.col-int { width: 80px; text-align: right; }' +
    '.col-float { width: 90px; text-align: right; }' +
    '.col-currency { width: 100px; text-align: right; }' +
    '.col-bool { width: 60px; text-align: center; }' +
    '.icon { width: 16px; height: 16px; margin-right: 4px; vertical-align: middle; }' +
    '.tree-indent-0 { margin-left: 0; }' +
    '.tree-indent-1 { margin-left: 20px; }' +
    '.tree-indent-2 { margin-left: 40px; }' +
    '.tree-indent-3 { margin-left: 60px; }' +
    '.tree-indent-4 { margin-left: 80px; }' +
    '</style>' +

    '<script>' +
    'document.addEventListener("DOMContentLoaded", function(){' +
    '  document.querySelectorAll(".caret").forEach(function(el){' +
    '    el.addEventListener("click", function(){' +
    '      var nested = el.parentElement.querySelector(".nested");' +
    '      if (nested) {' +
    '        nested.classList.toggle("active");' +
    '        el.classList.toggle("caret-down");' +
    '      }' +
    '    });' +
    '  });' +
    '});' +
    '</script>';

  { === Funções internas === }

class function TTreeViewHTML.HasChildren(Q: TDataSet; const Id: Integer): Boolean;
var
  BM: TBookmark;
  Found: Boolean;
begin
  Result := False;
  if not Q.Active then
    Exit;

  BM := Q.GetBookmark;
  try
    Q.First;
    Found := False;
    while not Q.Eof do
    begin
      if Q.FieldByName('id_pai').AsInteger = Id then
      begin
        Found := True;
        Break;
      end;
      Q.Next;
    end;
    Result := Found;
  finally
    if Q.BookmarkValid(BM) then
      Q.GotoBookmark(BM);
    Q.FreeBookmark(BM);
  end;
end;

class function TTreeViewHTML.GetLabelField(Q: TDataSet): TField;
var
  F: TField;
begin
  // Preferência por nomes comuns de descrição
  F := Q.FindField('descricao');
  if Assigned(F) then Exit(F);
  F := Q.FindField('nome');
  if Assigned(F) then Exit(F);
  F := Q.FindField('titulo');
  if Assigned(F) then Exit(F);
  // Primeiro campo string disponível
  for F in Q.Fields do
    if (F.DataType in [ftString, ftWideString]) and not IsIdField(F.FieldName) then
      Exit(F);
  // Caso não haja string, retorna primeiro campo não-ID
  for F in Q.Fields do
    if not IsIdField(F.FieldName) then
      Exit(F);
  Result := nil;
end;

class function TTreeViewHTML.IsIdField(const FieldName: string): Boolean;
begin
  Result := SameText(FieldName, 'id') or SameText(FieldName, 'id_pai');
end;

class procedure TTreeViewHTML.FieldToHTML(F: TField; out TDClass, TDValue: string);
begin
  case F.DataType of
    ftBoolean:
      begin
        TDClass := 'col-bool';
        if F.AsBoolean then
          TDValue := '<img class="icon" src="https://cdn-icons-png.flaticon.com/512/190/190411.png" alt="True">'
        else
          TDValue := '<img class="icon" src="https://cdn-icons-png.flaticon.com/512/463/463612.png" alt="False">';
      end;
    ftDate:
      begin
        TDClass := 'col-date';
        if not F.IsNull then
          TDValue := FormatDateTime('dd/mm/yyyy', F.AsDateTime)
        else
          TDValue := '';
      end;
    ftDateTime:
      begin
        TDClass := 'col-date';
        if not F.IsNull then
          TDValue := FormatDateTime('dd/mm/yyyy hh:nn', F.AsDateTime)
        else
          TDValue := '';
      end;
    ftCurrency:
      begin
        TDClass := 'col-currency';
        TDValue := FormatFloat('0.00', F.AsCurrency);
      end;
    ftFloat:
      begin
        TDClass := 'col-float';
        TDValue := FormatFloat('0.##', F.AsFloat);
      end;
    ftInteger, ftSmallint, ftWord, ftLargeint:
      begin
        TDClass := 'col-int';
        TDValue := IntToStr(F.AsInteger);
      end;
    ftString, ftWideString, ftMemo, ftWideMemo:
      begin
        TDClass := 'col-str';
        TDValue := F.AsString;
      end;
  else
      TDClass := 'col-str';
      TDValue := F.AsString;
  end;
end;

class function TTreeViewHTML.BuildTreeHTML(Q: TDataSet; IdPai, Nivel: Integer): string;
var
  ListaFilhos: TStringList;
  IdAtual: Integer;
  LabelField: TField;
  LabelText, IconHTML: string;
  i: Integer;
  BM: TBookmark;
  IndentClass: string;
  RowHTML, TDClass, TDValue: string;
  F: TField;
  IsMaster: Boolean;
begin
  Result := '';
  ListaFilhos := TStringList.Create;
  IndentClass := Format('tree-indent-%d', [Nivel]);
  BM := Q.GetBookmark;
  LabelField := GetLabelField(Q);
  try
    Q.First;
    while not Q.Eof do
    begin
      if Q.FieldByName('id_pai').AsInteger = IdPai then
        ListaFilhos.Add(Q.FieldByName('id').AsString);
      Q.Next;
    end;

    if ListaFilhos.Count > 0 then
    begin
      Result := Result + '<ul>';
      for i := 0 to ListaFilhos.Count - 1 do
      begin
        Q.First;
        while not Q.Eof do
        begin
          IdAtual := Q.FieldByName('id').AsInteger;
          if IdAtual = StrToInt(ListaFilhos[i]) then
          begin
            if Assigned(LabelField) then
              LabelText := LabelField.AsString
            else
              LabelText := Q.FieldByName('id').AsString;

            IsMaster := (Nivel = 0);

            if HasChildren(Q, IdAtual) then
              IconHTML := ''
            else
              IconHTML := '<span class="leaf-dot">•</span>';

            Result := Result + '<li>';
            if HasChildren(Q, IdAtual) then
            begin
              Result := Result + '<input type="checkbox" class="tree-toggle" id="node-' + FCurrentPrefix + '-' + IntToStr(IdAtual) + '">';

              RowHTML := '<table class="table table-sm table-bordered mb-1 tree-table"><tr>' +
                         '<td class="desc ' + IfThen(IsMaster, ' desc-master', '') + '">' +
                         '<label class="caret ' + IndentClass + '" for="node-' + FCurrentPrefix + '-' + IntToStr(IdAtual) + '">' + IconHTML + LabelText + '</label>' +
                         '</td>';
              for F in Q.Fields do
              begin
                if IsIdField(F.FieldName) or (Assigned(LabelField) and (F = LabelField)) then
                  Continue;
                FieldToHTML(F, TDClass, TDValue);
                RowHTML := RowHTML + '<td class="' + TDClass + '">' + TDValue + '</td>';
              end;
              RowHTML := RowHTML + '</tr></table>';
              Result := Result + RowHTML;
              Result := Result + '<ul class="nested">' + BuildTreeHTML(Q, IdAtual, Nivel + 1) + '</ul>';
            end
            else
            begin
              RowHTML := '<table class="table table-sm table-bordered mb-1 tree-table"><tr>' +
                         '<td class="desc ' + IfThen(IsMaster, ' desc-master', '') + '"><span class="' + IndentClass + '">' + IconHTML + LabelText + '</span></td>';
              for F in Q.Fields do
              begin
                if IsIdField(F.FieldName) or (Assigned(LabelField) and (F = LabelField)) then
                  Continue;
                FieldToHTML(F, TDClass, TDValue);
                RowHTML := RowHTML + '<td class="' + TDClass + '">' + TDValue + '</td>';
              end;
              RowHTML := RowHTML + '</tr></table>';
              Result := Result + RowHTML;
            end;
            Result := Result + '</li>';
          end;
          Q.Next;
        end;
      end;
      Result := Result + '</ul>';
    end;
  finally
    if Q.BookmarkValid(BM) then
      Q.GotoBookmark(BM);
    Q.FreeBookmark(BM);
    ListaFilhos.Free;
  end;
end;

class function TTreeViewHTML.BuildHTMLFromDataset(Q: TDataSet): string;
begin
  // Prefixo único para cada renderização, evitando conflitos de IDs quando múltiplas árvores são injetadas
  Inc(FGlobalCounter);
  FCurrentPrefix := 'tree' + IntToStr(FGlobalCounter);
  Result := TREEVIEW_STYLE_SCRIPT + BuildTreeHTML(Q, 0, 0);
end;

class function TTreeViewHTML.CreateDemoDataset: TClientDataSet;
begin
  Result := TClientDataSet.Create(nil);
  with Result.FieldDefs do
  begin
    Add('id', ftInteger);
    Add('id_pai', ftInteger);
    Add('descricao', ftString, 100);
    Add('data', ftDate);
    Add('qtde', ftCurrency);
    Add('valor', ftCurrency);
    Add('total', ftCurrency);
    Add('observ', ftString, 200);
  end;
  Result.CreateDataSet;

  Result.AppendRecord([1, 0, 'Pasta A', Now, 0, 0, 0, '']);
  Result.AppendRecord([2, 1, 'Item 1', Now - 1, 2, 50.00, 100.00, 'Primeiro item']);
  Result.AppendRecord([3, 1, 'Item 2', Now - 2, 1, 75.00, 75.00, 'Segundo item']);
  Result.AppendRecord([4, 0, 'Pasta B', Now, 0, 0, 0, '']);
  Result.AppendRecord([5, 4, 'Subpasta B1', Now, 0, 0, 0, '']);
  Result.AppendRecord([6, 5, 'Item 3', Now, 3, 40.00, 120.00, 'Item dentro de Subpasta B1']);
  Result.First;
end;

class function TTreeViewHTML.CreateDemoDataset2: TClientDataSet;
begin
  Result := TClientDataSet.Create(nil);
  with Result.FieldDefs do
  begin
    Add('id', ftInteger);
    Add('id_pai', ftInteger);
    Add('descricao', ftString, 150);
    Add('tipo', ftString, 20);
    Add('data', ftDate);
    Add('valor', ftCurrency);
    Add('ativo', ftBoolean);
    Add('observ', ftString, 200);
  end;

  Result.CreateDataSet;

  // --- Dados de exemplo hierárquicos ---
  // Pastas raiz
  Result.AppendRecord([1, 0, 'Financeiro', 'Pasta', Date, 0, True, 'Contas e relatórios']);
  Result.AppendRecord([2, 0, 'Comercial',  'Pasta', Date, 0, True, 'Clientes e vendas']);

  // Subpastas e itens de Financeiro
  Result.AppendRecord([3, 1, 'Contas a Pagar', 'Pasta', Date - 1, 0, True, '']);
  Result.AppendRecord([4, 1, 'Contas a Receber', 'Pasta', Date - 2, 0, True, '']);
  Result.AppendRecord([5, 3, 'Fatura 123', 'Item', Date - 5, 450.75, True, 'Pagamento até dia 30']);
  Result.AppendRecord([6, 3, 'Fatura 124', 'Item', Date - 3, 125.00, False, 'Em atraso']);

  // Subpastas e itens de Comercial
  Result.AppendRecord([7, 2, 'Clientes', 'Pasta', Date - 1, 0, True, '']);
  Result.AppendRecord([8, 7, 'Cliente XPTO Ltda', 'Item', Date - 1, 5000.00, True, 'Cliente VIP']);
  Result.AppendRecord([9, 7, 'Cliente ABC S/A', 'Item', Date - 2, 2800.00, True, 'Cliente ativo']);
  Result.AppendRecord([10, 2, 'Vendas', 'Pasta', Date, 0, True, 'Resumo geral']);
  Result.AppendRecord([11, 10, 'Venda #1001', 'Item', Date - 1, 1200.50, True, 'Pago']);
  Result.AppendRecord([12, 10, 'Venda #1002', 'Item', Date, 800.00, True, 'A receber']);

  Result.First;
end;

class function TTreeViewHTML.CreateUserTreeDataset: TClientDataSet;
begin
  Result := TClientDataSet.Create(nil);
  with Result.FieldDefs do
  begin
    Add('id', ftInteger);
    Add('id_pai', ftInteger);
    Add('nome', ftString, 100);
    Add('perfil', ftString, 50);
    Add('ativo', ftBoolean);
    Add('criado_em', ftDateTime);
    Add('email', ftString, 120);
    Add('notas', ftString, 200);
  end;

  Result.CreateDataSet;

  // --- Estrutura hierárquica (Departamentos / Usuários) ---
  Result.AppendRecord([1, 0, 'Administração', 'Grupo', True, Now - 100, '', 'Grupo principal']);
  Result.AppendRecord([2, 1, 'João Silva', 'Administrador', True, Now - 30, 'joao@empresa.com', 'Responsável geral']);
  Result.AppendRecord([3, 1, 'Maria Souza', 'Financeiro', True, Now - 25, 'maria@empresa.com', 'Controla pagamentos']);

  Result.AppendRecord([4, 0, 'TI', 'Grupo', True, Now - 90, '', 'Suporte e desenvolvimento']);
  Result.AppendRecord([5, 4, 'Carlos Lima', 'Desenvolvedor', True, Now - 10, 'carlos@empresa.com', 'Full stack']);
  Result.AppendRecord([6, 4, 'Patrícia Reis', 'Suporte', False, Now - 15, 'patricia@empresa.com', 'Em licença']);

  Result.AppendRecord([7, 0, 'Vendas', 'Grupo', True, Now - 80, '', 'Equipe comercial']);
  Result.AppendRecord([8, 7, 'Ricardo Gomes', 'Vendedor', True, Now - 5, 'ricardo@empresa.com', 'Região Sul']);
  Result.AppendRecord([9, 7, 'Fernanda Alves', 'Vendedor', True, Now - 3, 'fernanda@empresa.com', 'Região Norte']);

  Result.First;
end;

class function TTreeViewHTML.CreateProjectDataset: TClientDataSet;
begin
  Result := TClientDataSet.Create(nil);
  with Result.FieldDefs do
  begin
    Add('id', ftInteger);
    Add('id_pai', ftInteger);
    Add('nome', ftString, 120);
    Add('ativo', ftBoolean);
    Add('prioridade', ftInteger);
    Add('progresso', ftFloat);
    Add('orcamento', ftCurrency);
    Add('inicio', ftDate);
    Add('termino', ftDate);
    Add('responsavel', ftString, 80);
    Add('observacao', ftString, 200);
  end;

  Result.CreateDataSet;

  // =========================
  // Nível 1 - Projetos
  // =========================
  Result.AppendRecord([1, 0, 'Sistema ERP', True, 1, 0.40, 50000.00, EncodeDate(2025, 1, 10), EncodeDate(2025, 12, 20), 'João Silva', 'Projeto principal de gestão']);
  Result.AppendRecord([2, 0, 'App Mobile', True, 2, 0.25, 20000.00, EncodeDate(2025, 3, 5), EncodeDate(2025, 10, 30), 'Maria Souza', 'Aplicativo integrado ao ERP']);

  // =========================
  // Nível 2 - Fases do ERP
  // =========================
  Result.AppendRecord([3, 1, 'Levantamento de Requisitos', True, 2, 1.0, 3000.00, EncodeDate(2025, 1, 10), EncodeDate(2025, 2, 20), 'Patrícia Lopes', 'Fase concluída']);
  Result.AppendRecord([4, 1, 'Desenvolvimento', True, 1, 0.35, 25000.00, EncodeDate(2025, 2, 21), EncodeDate(2025, 7, 15), 'Carlos Lima', 'Módulo principal em andamento']);
  Result.AppendRecord([5, 1, 'Testes e Implantação', False, 3, 0.0, 8000.00, EncodeDate(2025, 7, 16), EncodeDate(2025, 12, 20), 'Fernanda Alves', 'A iniciar']);

  // =========================
  // Nível 3 - Subtarefas de Desenvolvimento
  // =========================
  Result.AppendRecord([6, 4, 'Módulo Financeiro', True, 1, 0.60, 8000.00, EncodeDate(2025, 2, 21), EncodeDate(2025, 4, 10), 'Bruno Costa', 'Controle financeiro']);
  Result.AppendRecord([7, 4, 'Módulo Estoque', True, 2, 0.30, 7000.00, EncodeDate(2025, 4, 11), EncodeDate(2025, 6, 10), 'Ana Paula', 'Controle de produtos']);
  Result.AppendRecord([8, 4, 'Módulo Fiscal', True, 3, 0.10, 10000.00, EncodeDate(2025, 6, 11), EncodeDate(2025, 7, 15), 'Carlos Lima', 'Integração NF-e']);

  // =========================
  // Nível 4 - Subtarefas do Módulo Fiscal
  // =========================
  Result.AppendRecord([9, 8, 'Emissão de Notas', True, 1, 0.25, 4000.00, EncodeDate(2025, 6, 11), EncodeDate(2025, 6, 25), 'Rafael Torres', 'Rotinas principais']);
  Result.AppendRecord([10, 8, 'Validação XML', True, 2, 0.10, 3000.00, EncodeDate(2025, 6, 26), EncodeDate(2025, 7, 5), 'Luciana Prado', 'Em desenvolvimento']);
  Result.AppendRecord([11, 8, 'Integração SEFAZ', False, 3, 0.0, 3000.00, EncodeDate(2025, 7, 6), EncodeDate(2025, 7, 15), 'Carlos Lima', 'A iniciar']);

  // =========================
  // Nível 2 - Fases do App Mobile
  // =========================
  Result.AppendRecord([12, 2, 'Design UI/UX', True, 1, 0.5, 5000.00, EncodeDate(2025, 3, 5), EncodeDate(2025, 4, 20), 'Mariana Duarte', 'Protótipo aprovado']);
  Result.AppendRecord([13, 2, 'Backend API', True, 2, 0.15, 7000.00, EncodeDate(2025, 4, 21), EncodeDate(2025, 6, 30), 'Ricardo Gomes', 'Em integração']);
  Result.AppendRecord([14, 2, 'Testes e Publicação', False, 3, 0.0, 8000.00, EncodeDate(2025, 7, 1), EncodeDate(2025, 10, 30), 'Maria Souza', 'Previsto para o 2º semestre']);

  Result.First;
end;

end.
