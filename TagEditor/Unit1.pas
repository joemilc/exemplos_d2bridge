unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
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
    Button1: TButton;
    procedure Module11Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    function BuildTagEditorHTML: string;
  public

  protected
    procedure ExportD2Bridge; override;
    procedure InitControlsD2Bridge(const PrismControl: TPrismControl); override;
    procedure RenderD2Bridge(const PrismControl: TPrismControl; var HTMLControl: string); override;
  end;

Function Form1: TForm1;

implementation

Uses
  TagEditorWebApp;

Function Form1: TForm1;
begin
  Result := TForm1(TForm1.GetInstance);
end;

{$R *.dfm}

{ TForm1 }

function TForm1.BuildTagEditorHTML: string;
var
  HTML: TStringList;
begin
  HTML := TStringList.Create;
  try
    HTML.Add('<style>');
    HTML.Add('.tag-container { display: flex; flex-wrap: wrap; padding: 8px; border: 1px solid #ccc; border-radius: 6px; min-height: 50px; cursor: text; }');
    HTML.Add('.tag { background-color: #007bff; color: white; padding: 5px 10px; margin: 4px; border-radius: 4px; font-size: 14px; display: flex; align-items: center; }');
    HTML.Add('.tag span { margin-left: 6px; cursor: pointer; font-weight: bold; }');
    HTML.Add('.tag-input { border: none; outline: none; flex: 1; padding: 5px; font-size: 14px; min-width: 100px; }');
    HTML.Add('</style>');

    HTML.Add('<div class="tag-container" id="tagContainer" onclick="focusInput()">');
    HTML.Add('  <input class="tag-input" id="tagInput" type="text" placeholder="Digite e pressione Enter..." />');
    HTML.Add('</div>');

    HTML.Add('<script>');
    HTML.Add('const input = document.getElementById("tagInput");');
    HTML.Add('const container = document.getElementById("tagContainer");');

    HTML.Add('function createTag(text) {');
    HTML.Add('  const tag = document.createElement("div");');
    HTML.Add('  tag.className = "tag";');
    HTML.Add('  tag.textContent = text;');
    HTML.Add('  const close = document.createElement("span");');
    HTML.Add('  close.textContent = "×";');
    HTML.Add('  close.onclick = () => tag.remove();');
    HTML.Add('  tag.appendChild(close);');
    HTML.Add('  container.insertBefore(tag, input);');
    HTML.Add('}');

    HTML.Add('input.addEventListener("keydown", function(e) {');
    HTML.Add('  if (e.key === "Enter" && this.value.trim() !== "") {');
    HTML.Add('    createTag(this.value.trim());');
    HTML.Add('    this.value = "";');
    HTML.Add('  }');
    HTML.Add('});');

    HTML.Add('function focusInput() { input.focus(); }');
    HTML.Add('</script>');

    Result := HTML.Text;
  finally
    HTML.Free;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Script, Resultado: string;
begin
  Script :=
    '(() => {' +
    '  var tags = []; ' +
    '  document.querySelectorAll(".tag").forEach(t => tags.push(t.childNodes[0].textContent.trim())); ' +
    '  return tags.join(",");' +
    '})()';

  Resultado := Session.ExecJSResponse(Script, 5000, True);

  if Trim(Resultado) = '' then
    ShowMessage('Nenhuma tag adicionada.')
  else
    ShowMessage('Tags recebidas: ' + Resultado);
end;

procedure TForm1.ExportD2Bridge;
begin
  inherited;

  Title := 'My D2Bridge Web Application';
  SubTitle := 'My WebApp';

  // TemplateClassForm:= TD2BridgeFormTemplate;
  D2Bridge.FrameworkExportType.TemplateMasterHTMLFile := '';
  D2Bridge.FrameworkExportType.TemplatePageHTMLFile := '';

  // Export yours Controls
  with D2Bridge.Items.Add do
  begin
    SideMenu(MainMenu1);
    VCLObj(Label1);
    VCLObj(Label2);
    VCLObj(Label3);
    VCLObj(Button1, CSSClass.Button.save);
    // Adiciona o componente HTML do editor de tags
    with Row.Items.Add do
      HTMLElement(BuildTagEditorHTML);
  end;
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
