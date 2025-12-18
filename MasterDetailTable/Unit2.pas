unit Unit2;

interface

type

  IMasterDetailTable = interface
    ['{8BEB7BB3-978D-4145-92B2-E3911DCB6EFA}']
    function Add(ATexto: String): IMasterDetailTable;
  end;

  TMasterDetailTable = class(TInterfacedObject, IMasterDetailTable)
  private
  public
    property Detail: TList<T>;
  end;

implementation

end.

