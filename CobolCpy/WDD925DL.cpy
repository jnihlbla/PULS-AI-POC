000100 01  WDD925DL-CTX.                                                        
000200*                                 DELIVERY PLAN REGISTER                  
000300*                                 DELIVERY INFO DESCRIPTION               
000400     03 IDSEGM               PIC X(6).                                    
000500*                                 SEGMENT                                 
000600     03 FILLERX2             PIC X(2).                                    
000700     03 IDARTNR-KEY          PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 IDDC-KEY             PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 IDLEVNR-KEY          PIC X(5).                                    
001200*                                 LEVERANT÷RNUMMER                        
001300     03 IDLEVBSK-KEY         PIC S9              COMP-3.                  
001400*                                 TYP AV LEVERANSBESKEDSTEXT              
001500     03 FILLERX25            PIC X(25).                                   
001600     03 WDD925-CTX.                                                       
001700*                                 LEVERANSPLANEREGISTER                   
001800*                                 BESKRIVN LEVERANSBESKED                 
001900*                                 FYSISK NYCKEL: IDLEVBSK                 
002000        05 IDLEVBSK          PIC S9              COMP-3.                  
002100*                                 TYP AV LEVERANSBESKEDSTEXT              
002200        05 TELEVBSK          PIC X(80).                                   
002300*                                 LEVERANSBESKEDSINFORMATION              
002400        05 TIBORT            PIC S9(7)           COMP-3.                  
002500*                                 BORTTAGSDATUM  (≈≈MMDD)                 
002600        05 TIREGDAT          PIC S9(7)           COMP-3.                  
002700*                                 REGISTRERINGSDATUM (≈≈MMDD)             
002800        05 TIREGTID          PIC S9(7)           COMP-3.                  
002900*                                 REGISTRERINGSTID                        
003000*** END OF VILMAII-COPY LENGTH= 139 BYTES                                 
