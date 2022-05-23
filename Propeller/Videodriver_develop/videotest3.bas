const _clkfreq = 336956522

dim dl1(579)
dim videocog as ulong
dim mbox as ulong

dim v as class using "hg007t.spin2"
dim psram as class using "psram4.spin2"

psram.startx(0, 0, 12, -1)
mbox=psram.getMailbox(0)

videocog=v.start(0,mbox)
for thecog=0 to 7:psram.setQos(thecog, 112 << 16) :next thecog
psram.setQoS(videocog, $7FFFf400) 

for i=0 to 575
  dl1(i)=((v.buf_ptr+1024*(i)) shl 4) + %0010
next i

let olddl=v.dl_ptr
v.dl_ptr=cast(ulong,@dl1(0))

v.cls(200,0)

for i=0 to 99 : v.fastline(0,100,i,i):next i
v.box(0,475,100,574,40)
v.fastline(0,100,572,200)
v.fastline(0,1023,0,40)
v.fastline(0,55,1,122)
