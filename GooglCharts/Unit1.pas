unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  System.Math, System.StrUtils,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.Menus,
  Data.DB, Datasnap.DBClient,
  D2Bridge.Json, D2Bridge.Forms;

type
  TForm1 = class(TD2BridgeForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MainMenu1: TMainMenu;
    Module11: TMenuItem;
    AppModule21: TMenuItem;
    Modules1: TMenuItem;
    Module12: TMenuItem;
    Module21: TMenuItem;
    SubModules1: TMenuItem;
    SubModule11: TMenuItem;
    SubModule21: TMenuItem;
    SubModule31: TMenuItem;
    CoreModules1: TMenuItem;
    CoreModule11: TMenuItem;
    CoreModule21: TMenuItem;
    btnAtualizar: TButton;
    procedure Module11Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnAtualizarClick(Sender: TObject);
  private
    CDS: TClientDataSet;
    function BuildGoogleChart(ADivID, AChartType: string; ADataSet: TClientDataSet): string;
    procedure PreencherCDSAleatorio;
  protected
    procedure ExportD2Bridge; override;
  end;

function Form1: TForm1;

implementation

uses
  GooglChartsWebApp;

{$R *.dfm}

function Form1: TForm1;
begin
  Result := TForm1(TForm1.GetInstance);
end;

{ ==== FORM CREATE ======================================================== }

procedure TForm1.FormCreate(Sender: TObject);
begin
  CDS := TClientDataSet.Create(Self);
  with CDS.FieldDefs do
  begin
    Add('Mes', ftString, 20);
    Add('Vendas', ftFloat);
    Add('Lucro', ftFloat);
  end;
  CDS.CreateDataSet;
end;

{ ==== EXPORT D2BRIDGE ==================================================== }

procedure TForm1.ExportD2Bridge;
begin
  inherited;

  Title := 'Demo Google Charts com D2Bridge';
  SubTitle := 'Layout: Barra à esquerda, Pizza à direita';

  PreencherCDSAleatorio;

  with D2Bridge.Items.Add do
  begin
    SideMenu(MainMenu1);
    VCLObj(Label1);
    VCLObj(Label2);
    VCLObj(Label3);

    with Row.Items.Add do
      VCLObj(btnAtualizar, CSSClass.Button.apply + ' ' + CSSClass.Col.colauto);

    with Row.Items.Add do
    begin
      Col7.Add.HtmlElement(BuildGoogleChart('chart_barras', 'ColumnChart', CDS));
      Col4.Add.HtmlElement(BuildGoogleChart('chart_pizza', 'PieChart', CDS));
    end;

    with Row.Items.add do
      HTMLElement(BuildGoogleChart('chart_vendas', 'LineChart', CDS));
  end;
end;

{ ==== GERA DADOS ALEATÓRIOS ============================================= }

procedure TForm1.PreencherCDSAleatorio;
const
  Meses: array [1 .. 6] of string = ('Jan', 'Fev', 'Mar', 'Abr', 'Mai', 'Jun');
var
  I: Integer;
begin
  CDS.DisableControls;
  CDS.EmptyDataSet;
  Randomize;
  for I := Low(Meses) to High(Meses) do
  begin
    CDS.Append;
    CDS.FieldByName('Mes').AsString := Meses[I];
    CDS.FieldByName('Vendas').AsFloat := Random(1000) + 100;
    CDS.FieldByName('Lucro').AsFloat := Random(800) + 50;
    CDS.Post;
  end;
  CDS.EnableControls;
end;

{ ==== ATUALIZAÇÃO DINÂMICA VIA BUTTON =================================== }

procedure TForm1.btnAtualizarClick(Sender: TObject);
var
  JSArray, vRow: string;
begin
  PreencherCDSAleatorio;

  JSArray := '[';
  CDS.First;
  while not CDS.Eof do
  begin
    vRow := Format('["%s", %s, %s]',
      [CDS.FieldByName('Mes').AsString,
      StringReplace(FormatFloat('0.00', CDS.FieldByName('Vendas').AsFloat), ',', '.', []),
      StringReplace(FormatFloat('0.00', CDS.FieldByName('Lucro').AsFloat), ',', '.', [])]);
    JSArray := JSArray + vRow + IfThen(not CDS.Eof, ',', '');
    CDS.Next;
  end;
  JSArray := JSArray + ']';

  // Atualiza ambos os gráficos sem recarregar a página
  Session.ExecJS('update_chart_barras(' + JSArray + ');');
  Session.ExecJS('update_chart_pizza(' + JSArray + ');');
end;

{ ==== CONSTRUÇÃO DO GOOGLE CHART ========================================= }

function TForm1.BuildGoogleChart(ADivID, AChartType: string; ADataSet: TClientDataSet): string;
var
  vHTML: TStringList;
  vRow: string;
begin
  vHTML := TStringList.Create;
  try
    vHTML.Add('<div id="' + ADivID + '" style="width:100%; height:400px;"></div>');
    vHTML.Add('<script type="text/javascript" src="https://www.gstatic.com/charts/loader.js"></script>');
    vHTML.Add('<script>');
    vHTML.Add('google.charts.load("current", {packages:["corechart"]});');
    vHTML.Add('google.charts.setOnLoadCallback(draw_' + ADivID + ');');

    vHTML.Add('function draw_' + ADivID + '() {');
    vHTML.Add('  var rows = getData_' + ADivID + '();');
    vHTML.Add('  drawChart_' + ADivID + '(rows);');
    vHTML.Add('}');

    vHTML.Add('function drawChart_' + ADivID + '(rows) {');
    vHTML.Add('  var data = new google.visualization.DataTable();');
    vHTML.Add('  data.addColumn("string", "Mês");');
    vHTML.Add('  data.addColumn("number", "Vendas");');
    vHTML.Add('  data.addColumn("number", "Lucro");');
    vHTML.Add('  data.addRows(rows);');

    if AChartType = 'PieChart' then
      vHTML.Add('  var options = {title: "Lucro por Mês", legend: {position: "bottom"}};')
    else
      vHTML.Add('  var options = {title: "Vendas x Lucro", hAxis:{title:"Mês"}, vAxis:{minValue:0}, legend:{position:"bottom"}};');

    vHTML.Add('  var chart = new google.visualization.' + AChartType + '(document.getElementById("' + ADivID + '"));');
    vHTML.Add('  chart.draw(data, options);');
    vHTML.Add('}');

    vHTML.Add('function getData_' + ADivID + '() { return [');
    ADataSet.First;
    while not ADataSet.Eof do
    begin
      vRow := Format('["%s", %s, %s]',
        [ADataSet.FieldByName('Mes').AsString,
        StringReplace(FormatFloat('0.00', ADataSet.FieldByName('Vendas').AsFloat), ',', '.', []),
        StringReplace(FormatFloat('0.00', ADataSet.FieldByName('Lucro').AsFloat), ',', '.', [])]);
      vHTML.Add(vRow + IfThen(not ADataSet.Eof, ',', ''));
      ADataSet.Next;
    end;
    vHTML.Add(']; }');

    vHTML.Add('window.update_' + ADivID + ' = function(rows) { drawChart_' + ADivID + '(rows); };');

    vHTML.Add('</script>');
    Result := vHTML.Text;
  finally
    vHTML.Free;
  end;
end;

{ ==== OUTROS MÉTODOS ===================================================== }

procedure TForm1.Module11Click(Sender: TObject);
begin
  TD2BridgeForm(Session.PrimaryForm).Show;
end;

end.

