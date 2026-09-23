000100 01  MID-W4I70701.                                                        
000200*                                                                         
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000600*                                 KUNDNUMMER                              
000700     03 MID-INPUT.                                                        
000800        05 MID-KDBEHX        PIC X.                                       
000900*                                 BEHANDLINGSKOD-X                        
001000        05 MID-KDANMORS-UPD  PIC X(2).                                    
001100*                                 ORSAK TILL LEVERANSANMÄRKNING           
001200        05 MID-FLINVFEE-UPD  PIC X.                                       
001300*                                 INVOICE FLAG                            
001400        05 MID-PRARTNTO-UPD  PIC X(10).                                   
001500*                                 ARTIKELPRIS NETTO                       
001600        05 MID-FLINVLDC-UPD  PIC X.                                       
001700*                                 INVOICE FLAG LDC                        
001800        05 MID-PRARTNTO-LDC-UPD                                           
001900                             PIC X(10).                                   
002000*                                 ARTIKELPRIS NETTO                       
002100     03 MID-INPUT-2.                                                      
002200        05 MID-KDBEHX-GRET   PIC X.                                       
002300*                                 BEHANDLINGSKOD-X                        
002400        05 MID-KDANMORS-GRET PIC X(2).                                    
002500*                                 ORSAK TILL LEVERANSANMÄRKNING           
002600        05 MID-SUARTBTO-GRET PIC X(10).                                   
002700*                                 SUMMA FÖRSÄLJNINGSVÄRDE                 
002800        05 MID-FLINVLDC-GRET PIC X.                                       
002900*                                 INVOICE FLAG LDC                        
003000        05 MID-SUARTBTO-LDC-GRET                                          
003100                             PIC X(10).                                   
003200*                                 SUMMA FÖRSÄLJNINGSVÄRDE                 
003300*** END OF VILMAII-COPY LENGTH= 59 BYTES                                  
