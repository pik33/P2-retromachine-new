unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, stdctrls, Graphics, Dialogs, ExtCtrls, Buttons,
   retro;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Image1: TImage;
    Memo1: TMemo;
    procedure BitBtn1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.BitBtn1Click(Sender: TObject);

var n,x,y,z,idx:cardinal;
  luma:double;
  luma16,chroma:cardinal ;
  fh:cardinal;
  filename:string;

begin
for z:=0 to 15 do
begin
filename:='balls'+inttohex(z,2)+'def' ;
fh:=filecreate(filename) ;
for n:=0 to 15 do
  for y:=0 to 31 do
 //   for z:=0 to 1 do
    for x:=0 to 31 do
      begin
      idx:=1024*n+32*y+x;
 //     image1.canvas.pixels[x+32*n,y]:=balls[idx] ;
      luma:=((balls[idx] and $ff)*0.3+ ((balls[idx] shr 8) and $ff)*0.2+((balls[idx] shr 16) and $ff)*0.3)/256 ;
      luma16:=round(luma*16);
       if (balls[idx] and $FF)<>(balls[idx] shr 16) then chroma:=16*z else chroma:=0;
       image1.canvas.pixels[x+32*n,y]:=luma16 shl 4+luma16 shl 12+luma16 shl 20 ;
       if balls[idx]<>0  then luma16+=0 else luma16:=0  ;
       luma16+=chroma;
       filewrite(fh,luma16,1);    //    filewrite(fh,luma16,1);

      end;
  fileclose(fh);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);


var i,j,fh:integer;
begin
  fh:=filecreate('amigafont.def');
  for i:=0 to 255 do
    for j:=0 to 15 do
     filewrite(fh,amigafont[i,j],1);
  fileclose(fh);
end;

procedure TForm1.Button2Click(Sender: TObject);

var n,x,y,z,idx:cardinal;
  luma:double;
  luma16,chroma:cardinal ;
  fh:cardinal;
  filename:string;

begin

filename:='mouse.def' ;
fh:=filecreate(filename) ;

for y:=0 to 31 do
    for x:=0 to 31 do
      begin
      idx:=32*y+x;
      luma:=((mysz[idx] and $ff)*0.3+ ((mysz[idx] shr 8) and $ff)*0.2+((mysz[idx] shr 16) and $ff)*0.3)/256 ;
      luma16:=round(luma*16);
      if mysz[idx]<>0  then luma16+=0 else luma16:=0  ;
      filewrite(fh,luma16,1);    //    filewrite(fh,luma16,1);
      end;
fileclose(fh);
end;

procedure TForm1.Button3Click(Sender: TObject);

var n,x,y,z,idx:cardinal;
  luma:double;
  luma16,chroma:cardinal ;
  fh:cardinal;
  filename:string;

begin
  filename:='mouse32.def';
  fh:=filecreate(filename) ;

  for y:=0 to 31 do
      for x:=0 to 31 do
        begin
        idx:=32*y+x;
        luma16:=mysz[idx] shl 8;
        filewrite(fh,luma16,4);    //    filewrite(fh,luma16,1);
        end;
  fileclose(fh);
end;

procedure TForm1.Button4Click(Sender: TObject);

  var n,x,y,z,idx:cardinal;
    luma:double;
    luma16,chroma:cardinal ;
    fh:cardinal;
    filename:string;

  begin

  filename:='balls32.def' ;
  fh:=filecreate(filename) ;
  for n:=0 to 15 do
    for y:=0 to 31 do
   //   for z:=0 to 1 do
      for x:=0 to 31 do
        begin
        idx:=1024*n+32*y+x;
   //     image1.canvas.pixels[x+32*n,y]:=balls[idx] ;
        luma16:=balls[idx] shl 8;
         filewrite(fh,luma16,4);    //    filewrite(fh,luma16,1);

        end;
    fileclose(fh);

end;

function fixhr(a:double):int64;

begin
result:=round(a*(1 shl 32));
end

procedure TForm1.Button5Click(Sender: TObject);
begin

memo1.lines.add(inttostr(fixhr(0.50190991877167369479/2 )));
memo1.lines.add(inttostr(fixhr(0.51763809020504152469/2 )));
memo1.lines.add(inttostr(fixhr(0.55168895948124587824/2 )));
memo1.lines.add(inttostr(fixhr(0.61038729438072803416/2 )));
memo1.lines.add(inttostr(fixhr(0.70710678118654752439/2 )));
memo1.lines.add(inttostr(fixhr(0.87172339781054900991/2 )));
memo1.lines.add(inttostr(fixhr(1.18310079157624925896/4 )));
memo1.lines.add(inttostr(fixhr(1.93185165257813657349/4 )));



end;

end.

