000100 01  SEQE-WDA3E1.                                                         
000200*                                 SÄNDNINGSREGISTER                       
000300*                                 SECONDARY INDEX                         
000400*                                 FYSISK NYCKEL: WDA3E1KY                 
000500*                                  (IDDC, IDRT, IDDISTR,                  
000600*                                   IDKUNDNR, IDRAPPNR,                   
000700*                                   DAREGDAT, TIKLOCK)                    
000800*                                 SEKUNDÄR NYCKEL: WDA3ESEQ               
000900*                                  (IDDC, IDRT, IDDISTR,                  
001000*                                   IDKUNDNR, IDRAPPNR)                   
001100     03 SEQE-IDDC            PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 SEQE-IDRT            PIC X(3).                                    
001500*                                 RETURTERMINAL                           
001600*                                 RETURN TERMINAL                         
001700     03 SEQE-IDDISTR         PIC S9(5)           COMP-3.                  
001800*                                 DISTRIKTNUMMER                          
001900*                                 DISTRICT NUMBER                         
002000     03 SEQE-IDKUNDNR        PIC S9(7)           COMP-3.                  
002100*                                 KUNDNUMMER                              
002200*                                 CUSTOMER NO                             
002300     03 SEQE-IDRAPPNR        PIC 9(7).                                    
002400*                                 RAPPORT NUMMER                          
002500*                                 DISCREPANCY REPORT NUMBER               
002600     03 SEQE-DAREGDAT        PIC 9(8).                                    
002700*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002800*                                 REGISTRATION DATE (YYYYMMDD)            
002900     03 SEQE-TIKLOCK         PIC S9(9)           COMP-3.                  
003000*                                 KLOCKSLAG (TTMMSSTH)                    
003100*                                 TIME OF DAY (HHMMSSTH)                  
003200     03 SEQE-IDKOLLI         PIC S9(5)           COMP-3.                  
003300*                                 KOLLINUMMER                             
003400*                                 CASE NUMBER                             
003500*** END OF VILMAII-COPY LENGTH= 35 BYTES                                  
