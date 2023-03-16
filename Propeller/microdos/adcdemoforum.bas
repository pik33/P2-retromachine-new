'' =================================================================================================
''
''   File....... jm_adc_demo.spin2
''   Purpose....
''   Author..... Jon "JonnyMac" McPhalen
''               Copyright (c) 2023 Jon McPhalen
''               -- see below for terms of use
''   E-mail..... jon.mcphalen@gmail.com
''   Started....
''   Updated.... 17 JAN 2023
''   Moded by Jim Meek to read and display dipswitch
''   {$P2}
''
'' =================================================================================================


con { timing }
    _clkfreq        = cfg#_clkfreq_def
    _xtlfreq        = cfg#_xtlfreq

                                       ' system freq as a constant
'  MS_001   = CLK_FREQ / 1_000                                   ' ticks in 1ms
 ' US_001   = CLK_FREQ / 1_000_000                               ' ticks in 1us

  '_clkfreq = CLK_FREQ                                           ' set system clock


con { terminal }
  BR_TERM =2_000_000
'  BR_TERM  = 230_400                                            ' terminal baud rate
  #0, T_PST, T_ANSI                                             ' terminal types

  T_TYPE = T_PST
  SER_BAUD = 2_000_000

con { fixed io pins }

  PGM_RX   = 63  { I }                                          ' programming / debug
  PGM_TX   = 62  { O }

  SF_CS    = 61  { O }                                          ' serial flash
  SF_SCK   = 60  { O }
  SF_SDO   = 59  { O }
  SF_SDI   = 58  { I }

  LED2     = 57  { O }                                          ' Eval and Edge LEDs
  LED1     = 56  { O }


con { app io pins }
  num_pins = 8
  _pins    = 32 addpins (num_pins -1)           'do conversions on these pins

  ADC_BASE = 32  { I}

  DIPSW = 0 addpins 7    'transmitter diswitch to com info

con

  #true,  ON, OFF
  #false, NO, YES

  ADC_CHANS = 8


obj

' main
  cfg  : "boardcfg.kiss"                                                          ' * master Spin cog
  adc  : "ADC_millivolts_pasm"                                         '   smart pin multi-channel ADC
  ser  : "com.serial.terminal.ansi"

' * uses cog when loaded


dat { pre-initialized }


var { globals }
  Long  cog
  word  rawadc,_samples[adc_chans]

  byte  packet[ADC_CHANS>>1+1],ds


pub main() | x,ax

  setup()
  waitms(1000)
  'send:=printf(@"%d,"millivolts",_sample[x])
  ser.printf(@"ADC \r\n")
  'repeat
    BYTEFILL (@packet,0,16)
    repeat x from 0 to ADC_CHANS
      ax:=x<<1      

      if x < ADC_CHANS
        'rawadc :=  cal_read(x + adc_base)
        rawadc :=_samples[x]
        wordmove(@packet[x],rawadc,1)
      else 
        rawadc := pinread(DIPSW)
        bytemove(@packet[ax], @rawadc, 1)                      ' transfer into packet
        ser.printf(@"%d  %4d  %.4x\r",x,rawadc,rawadc)

    repeat x from 0 to (ADC_CHANS * 2)
        SER.printf(@"%.2x ",@packet[x])
    waitms(100)
    repeat

pub setup()
  ser.start(SER_BAUD)
  
'  send:=ser.printf(@"%d %2x \r\n",)
'' Configure IO and objects for application
  wrpin(DIPSW,P_HIGH_15K|P_LOW_FLOAT)
  pinhigh(DIPSW)
  ds:= pinread(DIPSW)
  cog:=COGINIT(COGEXEC_NEW,ADC_pasm(_pins,_samples))   ' read adc pins as millivolts

DAT 
  ADC_pasm
  org            0
  adc_start     fltl    pins                    'set pins to ADC mode
                wrpin   adc_modes,pins
                wxpin   #9,pins                 '#9 is 512-clock, 10-bit conversion (8 per sample)
                drvl    pins                    'start pins on same clock

                mov     pin_base,pins           'get pin base
                and     pin_base,#$3F

                mov     pin_count,pins          'get pin count
                shr     pin_count,#6
                add     pin_count,#1

                mov     level_base,#adc_levels  'prepare level_base altd pointer
                sub     level_base,pin_base
                bith    level_base,#9

                'mov    ijmp1,#adc_isr          'set interrupt jump

                mov     x,pin_base              'set base pin IN-rise event
                or      x,#%001<<6
                setse1  x
                

'
        _ret_   setint1 #event_se1              'enable interrupt on event, return to Spin2
'
'
' ADC interrupt service routine - runs in background of Spin2 interpreter
'
  adc_loop      akpin   pins                    'ADC samples done, acknowledge pins

                alts    cycle,#vio_levels       'lookup vio and gio levels for sample computation
                mov     x,0
                alts    cycle,#gio_levels
                mov     y,0

                sub     x,y                     '(3300 << 12) / (vio_level - gio_level)
                qdiv    ##3300<<12,x            'cordic runs during REP

                rep     #3,pin_count            'read ADC samples and sum into adc_levels
                rdpin   x,pin_base
                altd    pin_base,level_base
                add     0,x

                sub     pin_base,pin_count      'restore pin_base

                getqx   x                       'get QDIV quotient

                alts    cycle,#pin_levels       '(quotient * (pin_level - gio_level)) >> 12
                subr    y,0
                muls    y,x
                sar     y,#12

                altd    cycle,#samples          'write finished sample
                mov     0,y
                wrword  D,#samples
                incmod  cycle,#7        wc      'repeat for 8 cycles
'       if_nc   reti1                           'return to Spin2
        if_nc   jmp    adc_loop                 'go back to begining of loop

                altd    state,#adc_modes        'end of 8th cycle, switch to next gio/vio/pin
                wrpin   0,pins

                'resi1                          'return to Spin2, resume on next interrupt


                akpin   pins                    '9th cycle, acknowledge pins

                alts    state,#moves            'move adc_levels to gio/vio/pin_levels
                mov     x,0
                rep     #2,pin_count
                alti    x,#%111_111
                mov     0,0

                'resi1                          'return to Spin2, resume on next interrupt


                akpin   pins                    '10th cycle, acknowledge pins

                setd    x,#adc_levels           'clear adc_levels
                rep     #2,pin_count
                alti    x,#%111_000
                mov     0,#0

                incmod  state,#3                'increment state

                'mov    ijmp1,#adc_isr          'return to Spin2, start over on next interrupt
'                reti1
                 jmp    adc_loop
'
' Defined data
'
  cycle         long    0       'cycles {0..7, 0, 0} for each state
  state         long    0       'states {0..3}

  adc_modes     long    p_adc_gio | p_adc               'adc modes, by state
                long    p_adc_1x  | p_adc
                long    p_adc_vio | p_adc
                long    p_adc_1x  | p_adc

  moves         long    pin_levels<<9 | adc_levels      'moves, by state
                long    gio_levels<<9 | adc_levels
                long    pin_levels<<9 | adc_levels
                long    vio_levels<<9 | adc_levels

  adc_end                               'end of PASM code to load into registers
'
'
' Undefined data
'
  pins          res     1       'initially set by Spin2 code to select the pins

  x             res     1
  y             res     1
  pin_base      res     1
  pin_count     res     1
  level_base    res     1

  adc_levels    res     8       'conversions are accumulated into this buffer
  gio_levels    res     8       '..and then copied to one of these three buffers
  vio_levels    res     8
  pin_levels    res     8

  samples               res     8       'final samples, available via REG[samples][{0..7}]
'
 
con { license }

{{

  Terms of Use: MIT License

  Permission is hereby granted, free of charge, to any person obtaining a copy of this
  software and associated documentation files (the "Software"), to deal in the Software
  without restriction, including without limitation the rights to use, copy, modify,
  merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
  permit persons to whom the Software is furnished to do so, subject to the following
  conditions:

  The above copyright notice and this permission notice shall be included in all copies
  or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
  INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
  PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
  HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
  CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
  OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

}}
