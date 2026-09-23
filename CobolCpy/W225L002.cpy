000100 01  W225L002.                                                            
000200*                                 LÄNKAREA I W225 MOT                     
000300*                                 LEVERANSPLANER                          
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAS-ARTIKELINFO     VALUE +1.                                    
000700      88 LAS-LEVERANSINFO    VALUE +2.                                    
000800*                                            KDCALL-W222-W228-002         
000900     03 TILEVBSK             PIC S9(5)           COMP-3.                  
001000*                                 LEVERANSBESKEDSVECKA   (ÅÅVV)           
001100     03 IDARTNR              PIC S9(9)           COMP-3.                  
001200*                                 ARTIKELNUMMER                           
001300     03 IDLEVNR              PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER                        
001500     03 FLJANEJ-IDLEVNR      PIC X.                                       
001600      88 ANROP-OK            VALUE 'J'.                                   
001700      88 ANROP-FEL           VALUE 'N'.                                   
001800*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001900     03 KVBR                 PIC S9(7)           COMP-3.                  
002000*                                 BESTÄLLNINGSREST                        
002100     03 LEVBESKED            OCCURS 3 TIMES.                              
002200        05 KVAVIS-LEVBESK    PIC S9(7)           COMP-3.                  
002300*                                 AVISERAT ANTAL                          
002400        05 TIAVIDAT-LEVBESK  PIC S9(5)           COMP-3.                  
002500*                                 LEVERANSBESKEDSVECKA   (ÅÅVV)           
002600     03 TILEVBSK-INLC1       PIC S9(5)           COMP-3.                  
002700*                                 LEVERANSBESKEDSVECKA   (ÅÅVV)           
002800     03 TILEVBSK-INLC2       PIC S9(5)           COMP-3.                  
002900*                                 LEVERANSBESKEDSVECKA   (ÅÅVV)           
003000*** END OF VILMAII-COPY LENGTH= 47 BYTES                                  
