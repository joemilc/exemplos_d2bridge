unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.Menus, Data.DB, Datasnap.DBClient, D2Bridge.Json, D2Bridge.Forms;


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
    ClientDataSet_Country: TClientDataSet;
    ClientDataSet_CountryAutoCod: TAutoIncField;
    ClientDataSet_CountryCountry: TStringField;
    ClientDataSet_CountryDDI: TStringField;
    ClientDataSet_CountryPopulation: TIntegerField;
    procedure Module11Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure PopuleClientDataSet;
    function BuildTabulator: String;
  public

  protected
    procedure ExportD2Bridge; override;
    procedure InitControlsD2Bridge(const PrismControl: TPrismControl); override;
    procedure RenderD2Bridge(const PrismControl: TPrismControl; var HTMLControl: string); override;
    procedure CallBack(const CallBackName: string; EventParams: TStrings); override;
  end;

Function Form1: TForm1;

implementation

Uses
   TabulatorWebApp;

Function Form1: TForm1;
begin
 Result:= TForm1(TForm1.GetInstance);
end;

{$R *.dfm}

{ TForm1 }

function TForm1.BuildTabulator: String;
var
  vTabulator: TStrings;
  Json: String;
begin
  vTabulator := TStringList.Create;
  ClientDataSet_Country.DisableControls;
  Json := ClientDataSet_Country.ToJSON.ToJSON;
  ClientDataSet_Country.EnableControls;
  // Texto antes da tabela
  vTabulator.Add('<h3>Tabulator - Lista de Países</h3>');
  vTabulator.Add('<h2>Adaptado por Joemil</h3>');
  vTabulator.Add('<a href="https://tabulator.info/examples">Tabulator Examples Oficial</a>');
  vTabulator.Add('</br>');
  vTabulator.Add('<p>Esta tabela mostra os países e suas populações:</p>');
  vTabulator.Add('<div id="example-table"></div>');
  vTabulator.Add('<script>');
  // Adiciona CSS do Tabulator
  vTabulator.Add('  var link = document.createElement("link");');
  vTabulator.Add('  link.rel = "stylesheet";');
  vTabulator.Add('  link.href = "https://unpkg.com/tabulator-tables@6.3.1/dist/css/tabulator.min.css";');
  vTabulator.Add('  document.head.appendChild(link);');
  // Adiciona JS do Tabulator
  vTabulator.Add('  var script = document.createElement("script");');
  vTabulator.Add('  script.src = "https://unpkg.com/tabulator-tables@6.3.1/dist/js/tabulator.min.js";');
  vTabulator.Add('  script.type = "text/javascript";');
  vTabulator.Add('  script.onload = function() {');
  vTabulator.Add('    var tabledata = ' + Json + ';');
  vTabulator.Add('    var table = new Tabulator("#example-table", {');
  vTabulator.Add('        height:"311px",');
  vTabulator.Add('        layout:"fitColumns",');  // Fit To Width
  vTabulator.Add('        data: tabledata,');
  vTabulator.Add('        autoColumns: true');
  vTabulator.Add('    });');
  vTabulator.Add('  };');
  vTabulator.Add('  document.body.appendChild(script);');
  vTabulator.Add('</script>');
  Result := vTabulator.Text;
  vTabulator.Free;
end;

procedure TForm1.CallBack(const CallBackName: string;
  EventParams: TStrings);
begin
  inherited;

end;

procedure TForm1.ExportD2Bridge;
begin
 inherited;

 Title:= 'My D2Bridge Web Application';
 SubTitle:= 'My WebApp';

  Title := 'My D2Bridge Application';

  // TemplateClassForm:= TD2BridgeFormTemplate;
  D2Bridge.FrameworkExportType.TemplateMasterHTMLFile := '';
  D2Bridge.FrameworkExportType.TemplatePageHTMLFile := '';

  // Export yours Controls
  with D2Bridge.Items.add do
  begin
    VCLObj(Label1, CSSClass.Text.Size.fs2 + ' ' + CSSClass.Text.Style.bold);
    VCLObj(Label2, CSSClass.Text.Size.fs3);
    VCLObj(Label3, CSSClass.Text.Size.fs4);

    with Row.Items.add do
      HTMLElement(BuildTabulator);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PopuleClientDataSet;
end;

procedure TForm1.InitControlsD2Bridge(const PrismControl: TPrismControl);
begin
 inherited;

 //Menu example
 {
  if PrismControl.VCLComponent = MainMenu1 then
   PrismControl.AsMainMenu.Title:= 'AppTeste'; //or in SideMenu use asSideMenu

  if PrismControl.VCLComponent = MainMenu1 then
   PrismControl.AsMainMenu.Image.URL:= 'https://d2bridge.com.br/images/LogoD2BridgeTransp.png'; //or in SideMenu use asSideMenu

  //GroupIndex example
  if PrismControl.VCLComponent = MainMenu1 then
   with PrismControl.AsMainMenu do  //or in SideMenu use asSideMenu
   begin
    MenuGroups[0].Caption:= 'Principal';
    MenuGroups[1].Caption:= 'Services';
    MenuGroups[2].Caption:= 'Items';
   end;

  //Chance Icon and Propertys MODE 1 *Using MenuItem component
  PrismControl.AsMainMenu.MenuItemFromVCLComponent(Abrout1).Icon:= 'fa-solid fa-rocket';

  //Chance Icon and Propertys MODE 2 *Using MenuItem name
  PrismControl.AsMainMenu.MenuItemFromName('Abrout1').Icon:= 'fa-solid fa-rocket';
 }

 //Change Init Property of Prism Controls
 {
  if PrismControl.VCLComponent = Edit1 then
   PrismControl.AsEdit.DataType:= TPrismFieldType.PrismFieldTypeInteger;

  if PrismControl.IsDBGrid then
  begin
   PrismControl.AsDBGrid.RecordsPerPage:= 10;
   PrismControl.AsDBGrid.MaxRecords:= 2000;
  end;
 }
end;

procedure TForm1.Module11Click(Sender: TObject);
begin
 TD2BridgeForm(Session.PrimaryForm).Show;
end;

procedure TForm1.PopuleClientDataSet;
begin
  ClientDataSet_Country.AppendRecord([1, 'China', '+86', 1444216107]);
  ClientDataSet_Country.AppendRecord([2, 'India', '+91', 1393409038]);
  ClientDataSet_Country.AppendRecord([3, 'United States', '+1', 332915073]);
  ClientDataSet_Country.AppendRecord([4, 'Indonesia', '+62', 276361783]);
  ClientDataSet_Country.AppendRecord([5, 'Pakistan', '+92', 225199937]);
  ClientDataSet_Country.AppendRecord([6, 'Brazil', '+55', 213993437]);
  ClientDataSet_Country.AppendRecord([7, 'Nigeria', '+234', 211400708]);
  ClientDataSet_Country.AppendRecord([8, 'Bangladesh', '+880', 166303498]);
  ClientDataSet_Country.AppendRecord([9, 'Russia', '+7', 145912025]);
  ClientDataSet_Country.AppendRecord([10, 'Mexico', '+52', 130262216]);
  ClientDataSet_Country.AppendRecord([11, 'Japan', '+81', 125943834]);
  ClientDataSet_Country.AppendRecord([12, 'Ethiopia', '+251', 120858976]);
  ClientDataSet_Country.AppendRecord([13, 'Philippines', '+63', 113850055]);
  ClientDataSet_Country.AppendRecord([14, 'Egypt', '+20', 104258327]);
  ClientDataSet_Country.AppendRecord([15, 'Vietnam', '+84', 97429061]);
  ClientDataSet_Country.AppendRecord([16, 'DR Congo', '+243', 90003954]);
  ClientDataSet_Country.AppendRecord([17, 'Turkey', '+90', 84339067]);
  ClientDataSet_Country.AppendRecord([18, 'Iran', '+98', 85004578]);
  ClientDataSet_Country.AppendRecord([19, 'Germany', '+49', 83149300]);
  ClientDataSet_Country.AppendRecord([20, 'Thailand', '+66', 69950807]);
  ClientDataSet_Country.AppendRecord([21, 'United Kingdom', '+44', 67886011]);
  ClientDataSet_Country.AppendRecord([22, 'France', '+33', 65273511]);
  ClientDataSet_Country.AppendRecord([23, 'Italy', '+39', 60244639]);
  ClientDataSet_Country.AppendRecord([24, 'South Africa', '+27', 60041932]);
  ClientDataSet_Country.AppendRecord([25, 'Tanzania', '+255', 59895231]);
  ClientDataSet_Country.AppendRecord([26, 'Myanmar', '+95', 54409800]);
  ClientDataSet_Country.AppendRecord([27, 'Kenya', '+254', 53771296]);
  ClientDataSet_Country.AppendRecord([28, 'South Korea', '+82', 51606633]);
  ClientDataSet_Country.AppendRecord([29, 'Colombia', '+57', 50976248]);
  ClientDataSet_Country.AppendRecord([30, 'Spain', '+34', 46754783]);
end;

procedure TForm1.RenderD2Bridge(const PrismControl: TPrismControl; var HTMLControl: string);
begin
 inherited;

 //Intercept HTML
 {
  if PrismControl.VCLComponent = Edit1 then
  begin
   HTMLControl:= '</>';
  end;
 }
end;

end.
