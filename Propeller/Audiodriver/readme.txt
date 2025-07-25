20230825: 0.95

Envelopes  and noise generator added
That's forced extending the channel's mailbox to 16 longs (power of 2 has to be used)

long #8  envelope phase accumulator / read only as long #0
long #9  envelope pointer. Points to the start of the envelope definition 0=envelope generator off
long #10 envelope speed. Number of Paula's periods between 2 envelope values. 
long #11 envelope length

In future versions, speed and length may be packed in one long 

The envelope is a multipoint definition that can have up to 1024 points. Use the last index (1023 for 1024 point envelope) as a length in long #1. The envelope is linear interpolated in the driver between these points.
After every sample, the speed value in long #10 is multiplied by the number of Paula's clock between samples. This value is added to the phase accumulator at #8. 10 high bits of this accumulator are compared to the length at #11. If more than, the envelope stops advancing.
At speed=1 and envelope length=1024, it will play for about 1211 seconds. If the speed equals the length, the envelope will play for 1.18 seconds - unless the sample rate was left at the default.


A sample based audio generator is not good at noise, particularly if the samples used are short, so I added a noise generator to the driver. If bit #27 of the sample pointer is set, tie driver will not sample anything, instead it will get a random number and output it. The sample period decides how fast new noise samples are generated, so big values generates low rumbling (brown noise), the less value in word #11, the more white is the noise.

------------------------------------------------------- 

20220423: 0.93  
Word 12 - #skip - is now 8.8 fixed point instead of 16bit integer. This means you have to use 256 instead of 1 
This allows finer tuning for synthesis and mind machine purpose, where Amiga Paula's period is much too coarse.
Bit 29 of the start pointer causes synchronizing of channels 0 and 1 for correct stereo 
Bit 28 of the start pointer enables playing interleaved samples with the fractional skip

20220401: 0.92a
Oscilloscope can be switched off and its buffer can be set at the start
PSRAM uses 256-byte cache for every channel, the cache address has to be set at the start 
Getting samples from HUB or PSRAM is settable using command bit #30 individually for each channel

20220314: the new version 0.91 added
8 channel only this time, PSRAM and oscilloscope support

long #7 is now command. Used at the channel #0
 - bit 31 - set sample rate (new sample rate divider for DACs at word #0)
 - bit 30 - 0 - use PSRAM, 1- use HUB

There are several TODOs. The oscilloscope data pointer is hardcoded at scope_ptr=$72238 (640 longs) and this should be (1) switchable (2) configurable via a command
A HUB/PSRAM selector should be set at individual channel level and not global.

---------------
This is a first beta version of Amiga inspired audio driver.
There are 4 files now, maybe this will reduce to one file and a preprocessor definitions
These are 8, 16 and 32 channel versions which work at standard Paula speed (PAL version,3.54 MHz)
There is also fast, 8 channels at 7.09 MHz version

To use:
- start the driver using start(mbox,scope,cache), where
   mailbox: PSRAM mailbox pointer, 0 if no PSRAM
   scope: oscilloscope pointer, 0 if oscilloscope not needed
   cache: channel number*256 bytes cache pointer, 0 if no PSRAM

- for normal Paula operation the clkfreq has to be 336956522, Paula x 95, (not 100 anymore because of my PSRAM speed limit)
- you can manually change "95" constant in the the DACs initialization code to anything not less than 50 (60 and more recommended) for different CPU clocks
- every channel register banks has 8 longs/16 words in it:

long #0: the sample phase accumulator: use it as read only although you -can- change this while playing (not recommended, the driver cog writes there at every sample)
long #1: the current sample generated, 2 words, right:left
long #2: the sample pointer.
         Set bit #31 to 1 if the sample is 16 bit, 0 for 8 bit. 
         Set bit #30 to 1 to start playing the sample from the beginning
         Set bit #29 to 1 to synchronize channels 0 and 1 for playing stereo without the phase error
	 Set bit #28 to 1 to eneble fractional skip for interleaved stereo samples (= wav files) 
long #3: sample loop start point
long #4: sample loop end point. 
         If the sample has to no loop and stop at the end, set loop start=end of sample, loop end=loop start +1 or 2 (for 16 bit)
long #5: volume and pan
         word #10: volume, 0..16384(=1). Values more than 16384 could cause clipping if sample is normalized
         word #11: pan. 16384: full left, 8192: center, 0: full right
long #6  period and skip
         word #11: period. This is the number of Paula cycles between two samples. 
         word #12: skip 
         From version 0.93 it is 8.8 fixed point, so set it to 256 for 8 bit or 512 for 16-bit samples. (was: 1 and 2) 
         Setting higher skip value skips the samples while playing, allows for higher frequencies for the same period
long #7  was: (reserved, unused. The planned usage is ADSR stuff.)
         Command, bit 31=set sample rate, bit 30 - set sample source

 

