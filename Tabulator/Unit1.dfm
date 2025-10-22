object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 122
    Height = 25
    Caption = 'Hello World'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 39
    Width = 294
    Height = 18
    Caption = 'This app is created with D2Bridge Framework'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 8
    Top = 65
    Width = 130
    Height = 14
    Caption = 'by Talis Jonatas  Gomes'
  end
  object MainMenu1: TMainMenu
    Left = 384
    Top = 128
    object Module11: TMenuItem
      Caption = 'Main'
      OnClick = Module11Click
    end
    object AppModule21: TMenuItem
      Caption = 'App Module 2'
    end
    object CoreModules1: TMenuItem
      Caption = 'Core Modules'
      object CoreModule11: TMenuItem
        Caption = 'Core Module 1'
      end
      object CoreModule21: TMenuItem
        Caption = 'Core Module 2'
      end
    end
    object Modules1: TMenuItem
      Caption = 'Modules'
      object Module12: TMenuItem
        Caption = 'Module 1'
      end
      object Module21: TMenuItem
        Caption = 'Module 2'
      end
      object SubModules1: TMenuItem
        Caption = 'Sub Modules'
        object SubModule11: TMenuItem
          Caption = 'SubModule 1'
        end
        object SubModule21: TMenuItem
          Caption = 'SubModule 2'
        end
        object SubModule31: TMenuItem
          Caption = 'SubModule 3'
        end
      end
    end
  end
  object ClientDataSet_Country: TClientDataSet
    PersistDataPacket.Data = {
      9C0000009619E0BD0100000018000000040000000000030000009C0007417574
      6F436F64040001000200010007535542545950450200490008004175746F696E
      630007436F756E74727901004900000001000557494454480200020064000344
      444901004900000001000557494454480200020014000A506F70756C6174696F
      6E040001000000000001000C4155544F494E4356414C55450400010001000000}
    Active = True
    Aggregates = <>
    Params = <>
    Left = 223
    Top = 96
    object ClientDataSet_CountryAutoCod: TAutoIncField
      DisplayWidth = 20
      FieldName = 'AutoCod'
    end
    object ClientDataSet_CountryCountry: TStringField
      FieldName = 'Country'
      Size = 100
    end
    object ClientDataSet_CountryDDI: TStringField
      FieldName = 'DDI'
    end
    object ClientDataSet_CountryPopulation: TIntegerField
      DisplayWidth = 50
      FieldName = 'Population'
    end
  end
end
