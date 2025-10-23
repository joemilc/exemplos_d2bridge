unit uBuildTreeHtml;

interface

uses
  System.SysUtils, System.Classes, Datasnap.DBClient, Data.DB;

type
  TTreeViewHTML = class
  private
    class function HasChildren(Q: TDataSet; const Id: Integer): Boolean; static;
    class function BuildTreeHTML(Q: TDataSet; IdPai: Integer): string; static;
  public
    class function CreateDemoDataset: TClientDataSet;
    class function BuildHTMLFromDataset(Q: TDataSet): string;
  end;

implementation

const
  TREEVIEW_STYLE_SCRIPT =
    '<style>' +
    'ul, #tree { list-style-type: none; margin: 0; padding: 0; }' +
    '.caret { cursor: pointer; user-select: none; font-weight: bold; }' +
    '.caret::before { content: "▶"; color: black; display: inline-block; margin-right: 6px; }' +
    '.caret-down::before { transform: rotate(90deg); }' +
    '.nested { display: none; margin-left: 15px; }' +
    '.active { display: block; }' +

  // ======= TABELA =======
    '.itemtbl { ' +
    '  border-collapse: collapse; ' +
    '  margin: 4px 0 4px 25px; ' +
    '  font-size: 13px; ' +
    '  width: 700px; ' + // largura total fixa
    '  table-layout: fixed; ' +
    '}' +
    '.itemtbl td { ' +
    '  border: 1px solid #ccc; ' +
    '  padding: 4px 6px; ' +
    '  overflow: hidden; ' +
    '  text-overflow: ellipsis; ' +
    '  white-space: nowrap; ' +
    '}' +
    '.itemtbl .desc  { width: 200px; font-weight: bold; }' +
    '.itemtbl .data  { width: 90px; text-align: center; }' +
    '.itemtbl .qtde  { width: 60px; text-align: right; }' +
    '.itemtbl .valor { width: 80px; text-align: right; }' +
    '.itemtbl .total { width: 80px; text-align: right; }' +
    '.itemtbl .obs   { width: 190px; }' +

  // * ======= ÍCONE =======
    '.icon { width: 16px; margin-right: 4px; vertical-align: middle; }' +
    '</style>' +

    '<script>' +
    'document.addEventListener("DOMContentLoaded", function(){' +
    '  document.querySelectorAll(".caret").forEach(function(el){' +
    '    el.addEventListener("click", function(){' +
    '      el.parentElement.querySelector(".nested").classList.toggle("active");' +
    '      el.classList.toggle("caret-down");' +
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

class function TTreeViewHTML.BuildTreeHTML(Q: TDataSet; IdPai: Integer): string;
var
  ListaFilhos: TStringList;
  IdAtual: Integer;
  Descricao, DataStr, Obs, IconHTML: string;
  Qtde, Valor, Total: Currency;
  i: Integer;
  BM: TBookmark;
begin
  Result := '';
  ListaFilhos := TStringList.Create;
  BM := Q.GetBookmark;
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
            Descricao := Q.FieldByName('descricao').AsString;
            if Q.FindField('data') <> nil then
              DataStr := FormatDateTime('dd/mm/yyyy', Q.FieldByName('data').AsDateTime)
            else
              DataStr := '';
            if Q.FindField('qtde') <> nil then
              Qtde := Q.FieldByName('qtde').AsCurrency
            else
              Qtde := 0;
            if Q.FindField('valor') <> nil then
              Valor := Q.FieldByName('valor').AsCurrency
            else
              Valor := 0;
            if Q.FindField('total') <> nil then
              Total := Q.FieldByName('total').AsCurrency
            else
              Total := 0;
            if Q.FindField('observ') <> nil then
              Obs := Q.FieldByName('observ').AsString
            else
              Obs := '';

            if HasChildren(Q, IdAtual) then
              IconHTML := '<img class="icon" src="https://cdn-icons-png.flaticon.com/512/716/716784.png">'
            else
              IconHTML := '<img class="icon" src="https://cdn-icons-png.flaticon.com/512/337/337946.png">';

            Result := Result + '<li>';
            if HasChildren(Q, IdAtual) then
            begin
              Result := Result + '<span class="caret">' + IconHTML + Descricao + '</span>';
              Result := Result + '<table class="itemtbl"><tr>' +
                '<td class="desc">' + Descricao + '</td>' +
                '<td class="data">' + DataStr + '</td>' +
                '<td class="qtde">' + FormatFloat('0.##', Qtde) + '</td>' +
                '<td class="valor">' + FormatFloat('0.00', Valor) + '</td>' +
                '<td class="total">' + FormatFloat('0.00', Total) + '</td>' +
                '<td class="obs">' + Obs + '</td>' +
                '</tr></table>';
              Result := Result + '<ul class="nested">' + BuildTreeHTML(Q, IdAtual) + '</ul>';
            end
            else
            begin
              Result := Result +
                '<table class="itemtbl"><tr>' +
                '<td class="desc">' + IconHTML + Descricao + '</td>' +
                '<td>' + DataStr + '</td>' +
                '<td>' + FormatFloat('0.##', Qtde) + '</td>' +
                '<td>' + FormatFloat('0.00', Valor) + '</td>' +
                '<td>' + FormatFloat('0.00', Total) + '</td>' +
                '<td>' + Obs + '</td>' +
                '</tr></table>';
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
  if not Assigned(Q) or not Q.Active then
    raise Exception.Create('Dataset não está ativo.');

  Result :=
    '<html><head>' + TREEVIEW_STYLE_SCRIPT + '</head><body>' +
    '<div id="tree">' + BuildTreeHTML(Q, 0) + '</div>' +
    '</body></html>';
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

end.
