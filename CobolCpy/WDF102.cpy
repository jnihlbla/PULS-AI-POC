000100 01  TULL-WDF102.                                                         
000200*                                 LEVERANTÖRSREGISTER                     
000300*                                 TULLKURSINFORMATION                     
000400*                                 FYSISK NYCKEL: IDLANDX2                 
000500     03 TULL-IDLANDX2        PIC X(2).                                    
000600*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
000700*                                 2-LETTER CODE FOR COUNTRY               
000800     03 TULL-KDVALLEV        PIC S9(3)           COMP-3.                  
000900*                                 VALUTAKOD LEVERANTÖR                    
001000     03 TULL-TITULF          PIC S9(7)           COMP-3.                  
001100*                                 TILLÄMPNINGSDATUM FÖR                   
001200*                                 TULLFAKTOR      (ÅÅMMDD)                
001300     03 TULL-RETULF-1        PIC S9(3)V9(4)      COMP-3.                  
001400*                                 TULLFAKTOR FRÅN OCH MED                 
001500*                                 TILLÄMPNINGSDATUM                       
001600     03 TULL-RETULF-2        PIC S9(3)V9(4)      COMP-3.                  
001700*                                 TULLFAKTOR FRAM TILL                    
001800*                                 TILLÄMPNINGSDATUM                       
001900*** END OF VILMAII-COPY LENGTH= 16 BYTES                                  
