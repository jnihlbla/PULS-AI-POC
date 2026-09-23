000100 01  9308-WDGX9308.                                                       
000200*                                 STYRANDE VALUTA                         
000300*                                 GÄLLANDE VALUTAKURSER                   
000400*                                 FYSISK NYCKEL: TISTADAT-9KOMPL          
000500*                                                                         
000600     03 9308-TISTADAT-9KOMPL PIC S9(7)           COMP-3.                  
000700*                                 GENERELL STARTDATUM (9KOMPL)            
000800*                                 GENERAL START DATE (9KOMPL)             
000900     03 9308-REVALUTA-TO     PIC S9(5)           COMP-3.                  
001000*                                 OMRÄKNINGSFAKTOR TILL HUVUDVALU         
001100*                                 TA FROM ANDRA VALUTOR                   
001200*                                 RECALCULATION TO MAIN CURRENCY          
001300*                                 FROM OTHER CURRENCIES                   
001400     03 9308-REVALUTA-FROM   PIC S9(5)           COMP-3.                  
001500*                                 OMRÄKNINGSFAKTOR FRÅN HUVUDVALU         
001600*                                 TA TILL ANDRA VALUTOR                   
001700*                                 RECALCULATION FROM MAIN CURRENC         
001800*                                 Y TO OTHER CURRENCIES                   
001900     03 9308-PRKURS          PIC S9(6)V9(6)      COMP-3.                  
002000*                                 VALUTAKURS                              
002100*                                 CURRENCY EXCHANGE RATE                  
002200     03 9308-TISTADAT        PIC S9(7)           COMP-3.                  
002300*                                 GENERELLT STARTDATUM                    
002400*                                 GENERAL START DATE                      
002500     03 9308-TIREGDAT        PIC S9(7)           COMP-3.                  
002600*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002700*                                 REGISTRATION DATE (YYMMDD)              
002800*** END OF VILMAII-COPY LENGTH= 25 BYTES                                  
