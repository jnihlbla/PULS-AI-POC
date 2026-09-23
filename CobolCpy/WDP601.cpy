000100 01  SLB-WDP601.                                                          
000200*                                 FAKTORER FÖR SL                         
000300*                                 FACTORS FOR SL                          
000400*                                 FYSISK NYCKEL: KDPRODSL,                
000500*                                        KDPRISKL, KDFREKKL               
000600     03 SLB-KDPRODSL         PIC S9(3)           COMP-3.                  
000700*                                 PRODUKTSLAG                             
000800*                                 PRODUCT GROUP                           
000900     03 SLB-KDPRISKL         PIC X.                                       
001000*                                 PRISKLASS                               
001100*                                 PRICE CLASS                             
001200     03 SLB-KDFREKKL         PIC X.                                       
001300*                                 FREKVENSKLASS                           
001400*                                 FREQ. CLASS                             
001500     03 SLB-KVVECKOR-MINSL   PIC S9(2)V9(1)      COMP-3.                  
001600*                                 MINGRÄNS SÄKERHETSLAGER                 
001700*                                                                         
001800     03 SLB-KVVECKOR-MAXSL   PIC S9(2)V9(1)      COMP-3.                  
001900*                                 MAXGRÄNS SÄKERHETSLAGER                 
002000*                                                                         
002100     03 SLB-REOLAGK          PIC S9V9(2)         COMP-3.                  
002200*                                 PROCENT ÖVERLAGERKOSTNAD                
002300*                                                                         
002400     03 SLB-RETARGET         PIC S9V9(3)         COMP-3.                  
002500*                                 SERVICEGRADSMÅL                         
002600*                                                                         
002700     03 SLB-KFAKT            PIC S9V9(2)         COMP-3.                  
002800*                                 KONST FÖR SÄK.LAG.                      
002900*                                                                         
003000     03 SLB-FILLER           PIC X(9).                                    
003100*** END OF VILMAII-COPY LENGTH= 24 BYTES                                  
