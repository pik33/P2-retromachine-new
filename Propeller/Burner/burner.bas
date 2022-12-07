' Flash burner for the P2 retromachine v.0.01 -20220429
' Flashes:

'800000 - fonts
'810000 - palettes and sprites
'830000 - video drivers
'840000 - audio drivers
'850000 - keyboard,  mouse, joysticks, etc HID drivers
'860000 - other drivers (PSRAM etc)
'870000 - audio samples, 63 slots@2k+names at 87F800
'890000 - audio envelopes, 62 slots@1k+names at 89F800
'

' --- 800000 - fonts, 16 slots @ 4 kB
'- ST mono type font   at $800000   2-4 kB
'- PC DOS type font    at $801000   2-4 kB

'- Atari 8x8 type font at $80C000

'To do in next versions: old Amiga type font at $802000
'		         Parallax type font at $803000 - can it be done at 8x16?

'- Atari 8bit type palette at $810000


'To do in the next version:3-3-2 palette at           $810400
'                           VGA palette at $810800
'			   8c32h palette at $810C00

'- mouse pointer sprite     at $811000
'next version - text cursor sprite       at $811400
'- moving ball sprites      at $814000 - 16 kB
'- mouse pointer, 32bit        $818000
'next version - text cursor, 32 bit		819000
'- moving ball, 32 bit          81A000 - 64 kB

'- 32 bpp hdmi video driver at 830000
'- 8bpp hdmi video driver at 831000 
'- 8bpp vga driver at 832000
'- 833000-83F000 reserved for video drivers
'- SIDCog at 840000
'- 8 chn Paula at 841000
'- 842000-84F000 reserved for audio drivers
'- keyboard and mouse drivers at 850000
'- PSRAM drivers at 860000

'- 64 2k slots for samples     $870000-88FFFF
'- 32 2k slots for envelopes   $890000-89FFFF




const  CLK_FREQ = 285_000_000        ' system freq as a constant
const   _clkfreq = CLK_FREQ

dim spi as class using "jm_p2_flash.spin2"
dim buffer,b2,flashaddr,f2 as ulong

' here you can decide what to burn. 0=not burn, non 0=address to burn

const burn_st_mono_font=$800000
const burn_pc_dos_font=$801000
const burn_atari_8x8_font=$80C000
const burn_atari_8bit_palette=$810000
const burn_mouse_pointer_8=$811000
const burn_ball_sprites_8=$814000
const burn_mouse_pointer_32=$818000
const burn_ball_sprites_32=$81A000
const burn_video32_driver=$830000


spi.start(20000)
waitms(2000)       

if burn_video32_driver<>0 then
  flashaddr=burn_video32_driver
  spi.erase(flashaddr,$20,1)
  print(hex$(flashaddr))
  waitms(10)
  buffer=varptr(hdmi32)
  f2=flashaddr
  b2=buffer
  for i=1 to 16
    print hex$(buffer), hex$(flashaddr) 
    spi.wr_block (f2,256,b2)
    waitms(10)
    buffer+=256
    flashaddr+=256
    f2=flashaddr
    b2=buffer
  next i
endif  

asm shared
 
hdmi32	file "hdmi32.drv"    
end asm
