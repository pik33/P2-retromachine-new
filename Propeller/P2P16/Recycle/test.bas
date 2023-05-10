
const HEAPSIZE = 16384
const version$="Prop2play v.0.30"
const statusline$=" Propeller2 multiformat player v. 0.29 --- 2022.05.10 --- pik33@o2.pl --- use a serial terminal or a RPi KBM interface to control --- arrows up,down move - pgup/pgdn or w/s move 10 positions - enter selects - tab switches panels - +,- controls volume - 1..4 switch channels on/off - 5,6 stereo separation - 7,8,9 sample rate - a,d SID speed - x,z SID subtune - R rescans current directory ------"
const hubset338=%1_111011__11_1111_0111__1111_1011 '338_666_667 =30*44100 
const hubset336=%1_101101__11_0000_0110__1111_1011 '336_956_522 =paula*95
'const hubset338=%1_110000__11_0110_1100__1111_1011 ' to test at 354
'const hubset336=%1_110000__11_0110_1100__1111_1011

const scope_ptr=$75A00

const  dirpanelx1=  5,  dirpanely1=60,  dirpanelx2=357,  dirpanely2=235
const filepanelx1=363, filepanely1=60, filepanelx2=719, filepanely2=403

declare a6502buf alias $64000 as ubyte($10FFF) '64000 doesnt work, why?
declare mainstack alias $75000 as ubyte(2559)
declare filebuf alias $76400 as ubyte(16383)

a6502buf(0)=0
