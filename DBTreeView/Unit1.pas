unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  DB, Datasnap.DBClient, uBuildTreeHtml,
  Vcl.Menus, D2Bridge.Forms; // Declare D2Bridge.Forms always in the last unit

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
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    procedure Module11Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    CDS, CDS2, CdsUser, CdsPrj: TClientDataset;
  public

  protected
    procedure ExportD2Bridge; override;
    procedure InitControlsD2Bridge(const PrismControl: TPrismControl); override;
    procedure RenderD2Bridge(const PrismControl: TPrismControl; var HTMLControl: string); override;
  end;

Function Form1: TForm1;

implementation

Uses
  DBTreeViewWebApp, System.IOUtils;

Function Form1: TForm1;
begin
  Result := TForm1(TForm1.GetInstance);
end;

{$R *.dfm}

{ TForm1 }

procedure TForm1.ExportD2Bridge;
var s: String;
begin
  inherited;

  Title := 'My D2Bridge Web Application';
  SubTitle := 'My WebApp';

  // TemplateClassForm:= TD2BridgeFormTemplate;
  D2Bridge.FrameworkExportType.TemplateMasterHTMLFile := '';
  D2Bridge.FrameworkExportType.TemplatePageHTMLFile := '';

  s := TTreeViewHTML.BuildHTMLFromDataset(CDS)+#13+
       TTreeViewHTML.BuildHTMLFromDataset(CDS2)+#13+
       TTreeViewHTML.BuildHTMLFromDataset(CdsUser)+#13+
       TTreeViewHTML.BuildHTMLFromDataset(CdsPrj);
  TFile.WriteAllText('d:\tree.html', s);
  // Export yours Controls
  with D2Bridge.Items.add do
  begin
    SideMenu(MainMenu1);
    VCLObj(Label1);
    VCLObj(Label2);
    VCLObj(Label3);
    VCLObj(Label4);
    with Row.Items.add do
      HTMLElement(TTreeViewHTML.BuildHTMLFromDataset(CDS));
    VCLObj(Label5);
    with Row.Items.add do
      HTMLElement(TTreeViewHTML.BuildHTMLFromDataset(CDS2));
    VCLObj(Label6);
    with Row.Items.add do
      HTMLElement(TTreeViewHTML.BuildHTMLFromDataset(CdsUser));
    VCLObj(Label7);
    with Row.Items.add do
      HTMLElement(TTreeViewHTML.BuildHTMLFromDataset(CdsPrj));
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  CDS.Free;
  CDS2.Free;
  CdsUser.Free;
  CdsPrj.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CDS  := TTreeViewHTML.CreateDemoDataset;
  CDS2 := TTreeViewHTML.CreateDemoDataset2;
  CdsUser := TTreeViewHTML.CreateUserTreeDataset;
  CdsPrj  := TTreeViewHTML.CreateProjectDataset;
end;

procedure TForm1.InitControlsD2Bridge(const PrismControl: TPrismControl);
begin
  inherited;

  // Menu example
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

  // Change Init Property of Prism Controls
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

procedure TForm1.RenderD2Bridge(const PrismControl: TPrismControl; var HTMLControl: string);
begin
  inherited;

  // Intercept HTML
  {
    if PrismControl.VCLComponent = Edit1 then
    begin
    HTMLControl:= '</>';
    end;
  }
end;

end.
