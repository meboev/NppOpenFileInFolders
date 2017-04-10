unit AboutForms;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, NppForms, StdCtrls;

type
  TAboutForm = class(TNppForm)
    OkButton: TButton;
    VersionLabel: TLabel;
    ProductLabel: TLabel;
    AdditionalLabel: TLabel;
    procedure OkButtonClick(Sender: TObject);
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.dfm}

procedure TAboutForm.OkButtonClick(Sender: TObject);
begin
  Close;
end;

end.
