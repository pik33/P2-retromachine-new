' Flash burner for the P2 retromachine v.0.02 -20221208
' Flashes:

'800000 - fonts
'810000 - palettes and sprites
'830000 - video drivers
'840000 - audio drivers
'850000 - keyboard,  mouse, joysticks, etc HID drivers
'860000 - other drivers (PSRAM etc)
'870000 - audio samples, 63 slots@2k+names at 87F800
'890000 - audio envelopes, 62 slots@1k+names at 89F800

'To do in next versions: old Amiga type font at               $802000
'		         Parallax type font at                $803000 - can it be done as 8x16 font?
'                        3-3-2 palette at                     $810400
'                        VGA palette at                       $810800
'		         8c32h palette at                     $810C00
'                        text cursor sprite at                $811400
'                        32 bit text cursor at	              $819000
'
'                        8bpp hdmi video driver at            $831000 
'                        8bpp vga driver at                   $832000
'                        SIDCog at                            $840000
' 			 8 chn Paula type drv at              $841000

'                        keyboard and mouse drivers at        $850000
'                        PSRAM drivers at                     $860000

'                        PC Softsynth type audio samples at   $870000, 64 slots @ 2k-88FFFF
'                        PC Softsynth type audio envelopes at $890000 with name list at $89F800


const _clkfreq        = 336956522

dim flash as class using "bitbanged_flash_002.spin2"

' here you can decide what to burn. 0=not burn, non 0=address to burn

const burn_st_mono_font=      0'$800000
const burn_pc_dos_font=       0'$801000
const burn_atari_8x8_font=    0'$80C000
const burn_atari_8bit_palette=0'$810000
const burn_mouse_pointer_8=   0'$811000
const burn_ball_sprites_8=    0'$814000
const burn_mouse_pointer_32=  0'$818000
const burn_ball_sprites_32=   0'$81A000
const burn_video32_driver=    0'$830000
const burn_psram_driver=      $860000

if burn_st_mono_font<>0       then print "Burning ST mono font at ",       hex$(burn_st_mono_font)      :burn(burn_st_mono_font,      varptr(stmono),       4096)
if burn_pc_dos_font<>0        then print "Burning PC DOS font at ",        hex$(burn_pc_dos_font)       :burn(burn_pc_dos_font,       varptr(pcdos),        4096)
if burn_atari_8x8_font<>0     then print "Burning Atari 8x8 font at ",     hex$(burn_atari_8x8_font)    :burn(burn_atari_8x8_font,    varptr(atari8),       4096)
if burn_atari_8bit_palette<>0 then print "Burning Atari 8-bit palette at ",hex$(burn_atari_8bit_palette):burn(burn_atari_8bit_palette,varptr(ataripalette), 4096)
if burn_mouse_pointer_8<>0    then print "Burning 8bpp mouse pointer at ", hex$(burn_mouse_pointer_8)   :burn(burn_mouse_pointer_8,   varptr(mouse8),       4096)
if burn_ball_sprites_8<>0     then print "Burning 8bpp ball sprites at ",  hex$(burn_ball_sprites_8)    :burn(burn_ball_sprites_8,    varptr(balls8),      16384)
if burn_mouse_pointer_32<>0   then print "Burning 32bpp mouse pointer at ",hex$(burn_mouse_pointer_32)  :burn(burn_mouse_pointer_32,  varptr(mouse32),      4096)
if burn_ball_sprites_32<>0    then print "Burning 32bpp ball sprites at " ,hex$(burn_ball_sprites_32)   :burn(burn_ball_sprites_32,   varptr(balls32), 	   65536)
if burn_video32_driver<>0     then print "Burning 32bpp video driver at ", hex$(burn_video32_driver)    :burn(burn_video32_driver,    varptr(hdmi32),       4096)
if burn_psram_driver<>0       then print "Burning PSRAM driver at ",       hex$(burn_psram_driver)      :burn(burn_psram_driver,      varptr(psram),        4096)

sub burn(flashaddr,hubaddr,amount)

' erase the flash in 4k blocks

let blocks=(amount-1)/4096
for i=0 to blocks
  print "Erasing: ",hex$(flashaddr+4096*i)
  flash.erase_block(flashaddr)
  next i
let blocks=(amount-1)/256
for i=0 to blocks
  print "Burning: ",hex$(hubaddr+256*i), hex$(flashaddr+256*i) 
  flash.write_block(flashaddr+256*i,hubaddr+256*i)
  next i
print "Burning completed."  
end sub  

asm shared
hdmi32		file "hdmi32.drv"
stmono  	file "st4font.def"
pcdos   	file "vgafont.def"
atari8  	file "atari8.fnt"
ataripalette 	file "ataripalettep2.def"   
mouse8		file "mouse.def"
balls8		file "balls01.def"
mouse32		file "mouse32.def"
balls32		file "balls32.def" 
psram 		file "psram.drv"
end asm
