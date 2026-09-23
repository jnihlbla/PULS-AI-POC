000100 01  SEQA-WDA3A1.                                                         
000200*                                 SÄNDNINGSREGISTER                       
000300*                                 SECONDARY INDEX                         
000400*                                 FYSISK NYCKEL: WDA3A1KY                 
000500*                                  (IDRT, IDDC, IDDISTR,                  
000600*                                   IDKUNDNR, IDRAPPNR,                   
000700*                                   DAREGDAT, TIKLOCK)                    
000800*                                 SEKUNDÄR NYCKEL: WDA3ASEQ               
000900*                                  (IDRT, IDDC, IDDISTR,                  
001000*                                   IDKUNDNR, IDRAPPNR)                   
001100     03 SEQA-IDRT            PIC X(3).                                    
001200*                                 RETURTERMINAL                           
001300*                                 RETURN TERMINAL                         
001400     03 SEQA-IDDC            PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600*                                 WAREHOUSE IDENTIFIER                    
001700     03 SEQA-IDDISTR         PIC S9(5)           COMP-3.                  
001800*                                 DISTRIKTNUMMER                          
001900*                                 DISTRICT NUMBER                         
002000     03 SEQA-IDKUNDNR        PIC S9(7)           COMP-3.                  
002100*                                 KUNDNUMMER                              
002200*                                 CUSTOMER NO                             
002300     03 SEQA-IDRAPPNR        PIC 9(7).                                    
002400*                                 RAPPORT NUMMER                          
002500*                                 DISCREPANCY REPORT NUMBER               
002600     03 SEQA-DAREGDAT        PIC 9(8).                                    
002700*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002800*                                 REGISTRATION DATE (YYYYMMDD)            
002900     03 SEQA-TIKLOCK         PIC S9(9)           COMP-3.                  
003000*                                 KLOCKSLAG (TTMMSSTH)                    
003100*                                 TIME OF DAY (HHMMSSTH)                  
003200*** END OF VILMAII-COPY LENGTH= 32 BYTES                                  
