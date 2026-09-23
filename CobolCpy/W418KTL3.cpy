000100 01  KTL3-W418KTL3.                                                       
000200*                                 LÄNKAREA MATRIX CONTROL RETURN          
000300*                                                                         
000400*                                 FYLL I INDATAS ALLA FÄLT                
000500*                                                                         
000600*                                 MÖJLIGA VÄRDEN PÅ KDSVAR:               
000700*                                 Y -> ARTIKEL FINNS I MATRISEN           
000800*                                 N -> ARTIKEL FINNS EJ I MATRIS          
000900*                                 S -> ARTIKEL/DC SAKNAS                  
001000*                                 VID KDSVAR N OCH S BLIR DET             
001100*                                 NORMAL RETURHANTERING                   
001200*                                                                         
001300     03 KTL3-INDATA.                                                      
001400        05 KTL3-IDPGM        PIC X(8).                                    
001500*                                 PROGRAM IDENTITET                       
001600        05 KTL3-IDARTNR      PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800        05 KTL3-IDDISTR      PIC S9(5)           COMP-3.                  
001900*                                 DISTRIKTNUMMER                          
002000        05 KTL3-IDKUNDNR     PIC S9(7)           COMP-3.                  
002100*                                 KUNDNUMMER                              
002200        05 KTL3-IDDC         PIC X(2).                                    
002300*                                 IDENTIFIERARE LAGER                     
002400        05 KTL3-KDANMORS     PIC X(2).                                    
002500*                                 ORSAK TILL LEVERANSANMÄRKNING           
002600     03 KTL3-UTDATA.                                                      
002700        05 KTL3-KDSVAR       PIC X.                                       
002800*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
002900        05 KTL3-KDRETBEH     PIC X.                                       
003000*                                 RETUR BEHANDLING PER ANMORSAK           
003100*** END OF VILMAII-COPY LENGTH= 26 BYTES                                  
