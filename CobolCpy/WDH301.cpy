000100 01  SAP-WDH301.                                                          
000200*                                 SAP-KONTROLLREGISTER                    
000300*                                 FYSISK-NYCKEL: WDH301KY                 
000400*                                 (KDSEGKEY + KDTRADP +                   
000500*                                  IDKONTO-GRP(3 VARIANTER):              
000600*                                     VAR.1 => IDANALYS                   
000700*                                     VAR.2 => IDKONTO + 6-SPACE          
000800*                                     VAR.3 => IDKST   + 2-SPACE)         
000900     03 SAP-KDSEGKEY         PIC X.                                       
001000*                                 TEKNISK SEGMENT-NYCKEL                  
001100     03 SAP-KDTRADP          PIC X(4).                                    
001200*                                 TRADING PARTNER                         
001300     03 SAP-IDKONTO-GRP.                                                  
001400*                                 KONTERINGSBEGREPP                       
001500        05 SAP-IDANALYS      PIC X(12).                                   
001600*                                 ANALYSNUMMER                            
001700        05 SAP-IDKONTO-FILLER REDEFINES SAP-IDANALYS.                     
001800           07 SAP-IDKONTO    PIC S9(11)          COMP-3.                  
001900*                                 KONTO                                   
002000           07 FILLER         PIC X(6).                                    
002100        05 SAP-IDKST-FILLER REDEFINES SAP-IDANALYS.                       
002200           07 SAP-IDKST      PIC X(10).                                   
002300*                                 KOSTNADSSTÄLLE                          
002400           07 FILLER         PIC X(2).                                    
002500     03 SAP-FLKST            PIC X.                                       
002600*                                 KOSTNADSSTÄLLE OBLIGATORISKT            
002700     03 SAP-FLANALYS         PIC X.                                       
002800*                                 ANALYSNR OBLIGATORISKT                  
002900     03 SAP-FILLER           PIC X(11).                                   
003000*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  
