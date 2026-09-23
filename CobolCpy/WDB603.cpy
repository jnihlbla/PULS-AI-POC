000100 01  SEGT-WDB603.                                                         
000200*                                 DC STYRREGISTER                         
000300*                                 SEGMENTATION TABLE                      
000400*                                 FYSISK NYCKEL: KDANSKSEG                
000500*                                                                         
000600     03 SEGT-KDANSKSEG       PIC S9(5)           COMP-3.                  
000700*                                 KOD FÖR ANSKAFFNINGSSEGMENT             
000800*                                 CODE FOR PROCUREMENT SEGMENT            
000900     03 SEGT-IDREFTAB-LFL    PIC X.                                       
001000*                                 TAB FÖR LÅG FREKV & LÅG LEDTID          
001100*                                 TBL FOR LOW FREQ & LOW LEADTIME         
001200     03 SEGT-IDREFTAB-LFM    PIC X.                                       
001300*                                 TAB FÖR LÅG FREKV & MED LEDTID          
001400*                                 TBL FOR LOW FREQ & MED LEADTIME         
001500     03 SEGT-IDREFTAB-LFH    PIC X.                                       
001600*                                 LÅG FREKVENTA OCH HÖGA LEDTIDER         
001700*                                 LOW FREQ & HIGH LEADTIME                
001800     03 SEGT-IDREFTAB-LFXH   PIC X.                                       
001900*                                 HÖG FREKVENTA & X-HÖGA LEDTIDER         
002000*                                 LOW FREQ & XTRA HIGH LEADTIME           
002100     03 SEGT-IDREFTAB-HFL    PIC X.                                       
002200*                                 HÖG FREKVENTA OCH LÅGA LEDTIDER         
002300*                                 HIGH FREQ & LOW LEADTIME                
002400     03 SEGT-IDREFTAB-HFM    PIC X.                                       
002500*                                 HÖG FREKVENTA OCH MEDEL LEDTID          
002600*                                 HIGH FREQ & MED LEADTIME                
002700     03 SEGT-IDREFTAB-HFH    PIC X.                                       
002800*                                 HÖG FREKVENTA OCH HÖGA LEDTIDER         
002900*                                 HIGH FREQ & HIGH LEADTIME               
003000     03 SEGT-IDREFTAB-HFXH   PIC X.                                       
003100*                                 HÖG FREKVENT OCH X-HÖG LEDTIDER         
003200*                                 HIGH FREQ & EXTRA HIGH LEADTIME         
003300*** END OF VILMAII-COPY LENGTH= 11 BYTES                                  
