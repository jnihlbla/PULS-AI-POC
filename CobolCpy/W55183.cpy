000100 01  W55183.                                                              
000200*                                 FIELDS FROM WDF101,WDF102 AND W         
000300*                                 DF106                                   
000400     03 IDLAND-WDF1          PIC X(2).                                    
000500*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
000600*                                 2-LETTER CODE FOR COUNTRY               
000700     03 IDLEVNR              PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001000     03 RETULF-1             PIC S9(3)V9(4)      COMP-3.                  
001100*                                 TULLFAKTOR FRÅN OCH MED                 
001200*                                 TILLÄMPNINGSDATUM                       
001300     03 KDVALLEV             PIC S9(3)           COMP-3.                  
001400*                                 VALUTAKOD LEVERANTÖR                    
001500     03 BELEV                PIC X(35).                                   
001600*                                 LEVERANTÖRSNAMN                         
001700*                                 SUPPLIER NAME                           
001800     03 IDLANDX2             PIC X(2).                                    
001900*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
002000*                                 2-LETTER CODE FOR COUNTRY               
002100     03 IDLAND               PIC X(2).                                    
002200*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
002300*                                 2-LETTER CODE FOR COUNTRY               
002400     03 NEXT-YR-DATE         PIC 9(8).                                    
002500*** END OF VILMAII-COPY LENGTH= 60 BYTES                                  
