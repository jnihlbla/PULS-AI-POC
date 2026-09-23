000100 01  W55144.                                                              
000200*                                  FIL MED UTDRAG UR WDF1-BASEN           
000300     03 IDLEVNR              PIC X(5).                                    
000400*                                 LEVERANTÖRNUMMER                        
000500     03 KDVALLEV             PIC S9(3)           COMP-3.                  
000600*                                 VALUTAKOD LEVERANTÖR                    
000700     03 RETULF-1             PIC S9(3)V9(4)      COMP-3.                  
000800*                                 TULLFAKTOR FRÅN OCH MED                 
000900*                                 TILLÄMPNINGSDATUM                       
001000     03 RETULF-2             PIC S9(3)V9(4)      COMP-3.                  
001100*                                 TULLFAKTOR FRAM TILL                    
001200*                                 TILLÄMPNINGSDATUM                       
001300     03 TITULF               PIC S9(7)           COMP-3.                  
001400*                                 TILLÄMPNINGSDATUM FÖR                   
001500*                                 TULLFAKTOR      (ÅÅMMDD)                
001600     03 IDLANDX2             PIC X(2).                                    
001700*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001800*** END OF VILMAII-COPY LENGTH= 21 BYTES                                  
