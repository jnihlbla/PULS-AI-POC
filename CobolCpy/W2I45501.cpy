000100 01  MID-W2I45501.                                                        
000200*                                 MID-COPYTEXT FÖR W2045500               
000300     03 MID-IDDC-2455-IN     PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 MID-IDDC-2455-UT     PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 MID-COPY-IDDC        PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MID-KVOT-RULL12HF    PIC X(7).                                    
001000*                                 ANTAL ORDERINGÅNG HÖGFREKVENTA          
001100     03 MID-KVVECKOR-FTL     PIC X(3).                                    
001200*                                 MAX VECKOR FÖR LÅGA LEDTIDER            
001300     03 MID-KVVECKOR-FTM     PIC X(3).                                    
001400*                                 MAX VECKOR FÖR MEDEL LEDTIDER           
001500     03 MID-KVVECKOR-FTH     PIC X(3).                                    
001600*                                 MAX VECKOR FÖR HÖGA LEDTIDER            
001700     03 MID-GRP              OCCURS 13 TIMES.                             
001800        05 MID-KDANSKSEG     PIC 9(4).                                    
001900*                                 KOD FÖR ANSKAFFNINGSSEGMENT             
002000        05 MID-BEPSEGM       PIC X(40).                                   
002100        05 MID-IDREFTAB-LFL  PIC X.                                       
002200*                                 TAB FÖR LÅG FREKV & LÅG LEDTID          
002300        05 MID-IDREFTAB-LFM  PIC X.                                       
002400*                                 TAB FÖR LÅG FREKV & MED LEDTID          
002500        05 MID-IDREFTAB-LFH  PIC X.                                       
002600*                                 LÅG FREKVENTA OCH HÖGA LEDTIDER         
002700        05 MID-IDREFTAB-LFXH PIC X.                                       
002800*                                 HÖG FREKVENTA & X-HÖGA LEDTIDER         
002900        05 MID-IDREFTAB-HFL  PIC X.                                       
003000*                                 HÖG FREKVENTA OCH LÅGA LEDTIDER         
003100        05 MID-IDREFTAB-HFM  PIC X.                                       
003200*                                 HÖG FREKVENTA OCH MEDEL LEDTID          
003300        05 MID-IDREFTAB-HFH  PIC X.                                       
003400*                                 HÖG FREKVENTA OCH HÖGA LEDTIDER         
003500        05 MID-IDREFTAB-HFXH PIC X.                                       
003600*                                 HÖG FREKVENT OCH X-HÖG LEDTIDER         
003700*** END OF VILMAII-COPY LENGTH= 698 BYTES                                 
