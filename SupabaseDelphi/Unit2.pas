unit Unit2;

{ Copyright 2025 / 2026 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  D2Bridge.Forms, Data.DB, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Comp.DataSet, FireDAC.Comp.Client, Vcl.Grids,
  Vcl.DBGrids;

type
  TForm2 = class(TD2BridgeForm)
    DBGrid1: TDBGrid;
    FDMemTable1: TFDMemTable;
    DataSource1: TDataSource;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  protected
    procedure ExportD2Bridge; override;
    procedure InitControlsD2Bridge(const PrismControl: TPrismControl); override;
    procedure RenderD2Bridge(const PrismControl: TPrismControl; var HTMLControl: string); override;
  end;

function Form2: TForm2;

implementation

Uses
  SupabaseDelphiWebApp, CadastrosService;

{$R *.dfm}


function Form2: TForm2;
begin
  result:= TForm2(TForm2.GetInstance);
end;

procedure TForm2.FormShow(Sender: TObject);
begin
  TCadastrosService.ConfigurarDataSet(FDMemTable1);
  TCadastrosService.CarregarDados(FDMemTable1);
end;

procedure TForm2.ExportD2Bridge;
begin
  inherited;

  Title:= 'My D2Bridge Form';

  // TemplateClassForm:= TD2BridgeFormTemplate;
  D2Bridge.FrameworkExportType.TemplateMasterHTMLFile:= '';
  D2Bridge.FrameworkExportType.TemplatePageHTMLFile := '';

  with D2Bridge.Items.add do
  begin
    {VCLObj(Label1, CSSClass.Text.Size.fs2 + ' ' + CSSClass.Text.Style.bold);
    VCLObj(Label2, CSSClass.Text.Size.fs3);
    VCLObj(Label3, CSSClass.Text.Size.fs4);
    VCLObj(Label4, CSSClass.Text.Size.fs2 + ' ' + CSSClass.Text.Style.bold);
    VCLObj(Button1);}
    VCLObj(DBGrid1);
  end;

end;

procedure TForm2.InitControlsD2Bridge(const PrismControl: TPrismControl);
begin
  inherited;

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

procedure TForm2.RenderD2Bridge(const PrismControl: TPrismControl; var HTMLControl: string);
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
