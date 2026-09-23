000100 01  W2I43901.                                                            
000200*                                 COPYTEXT FÖR MID W2I43901               
000300     03 IDARTNR-IN           PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 IDARTNR-UT           PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 IDDC-IN              PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 IDDC-UT              PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 KDBEHX-PLAN-IN       PIC X.                                       
001200*                                 BEHANDLINGSKOD-X                        
001300     03 KDBEHX-PLAN-UT       PIC X.                                       
001400*                                 BEHANDLINGSKOD-X                        
001500     03 IDLEVNR-IN           PIC X(5).                                    
001600*                                 LEVERANTÖRNUMMER                        
001700     03 IDLEVNR-UT           PIC X(5).                                    
001800*                                 LEVERANTÖRNUMMER                        
001900     03 PERIOD-IN            PIC X(4).                                    
002000*                                 ÅR - PLANERINGSPERIOD (ÅÅPP)            
002100*                                 12 PER ÅR                               
002200*                                 NUMERA ÄR DETTA "PV-PERIOD"             
002300     03 PERIOD-UT            PIC X(4).                                    
002400*                                 ÅR - PLANERINGSPERIOD (ÅÅPP)            
002500*                                 12 PER ÅR                               
002600*                                 NUMERA ÄR DETTA "PV-PERIOD"             
002700     03 INPUT.                                                            
002800*                                                                         
002900        05 PERIODS           OCCURS 2 TIMES.                              
003000*                                                                         
003100           07 WEEKS          OCCURS 5 TIMES.                              
003200*                                                                         
003300              09 KVAVROP-DAY OCCURS 5 TIMES                               
003400                             PIC 9(6).                                    
003500*** END OF VILMAII-COPY LENGTH= 342 BYTES                                 
