000100 01  SEQH-WDA3H1.                                                         
000200*                                 SÄNDNINGSREGISTER                       
000300*                                 SECONDARY INDEX                         
000400*                                 FYSISK NYCKEL: WDA3H1KY                 
000500*                                  (IDRT, KDRETSTA,                       
000600*                                   DAREGDAT, TIKLOCK,                    
000700*                                   IDDISTR, IDKUNDNR)                    
000800*                                 SEKUNDÄR NYCKEL: WDA3HSEQ               
000900*                                  (IDRT, KDRETSTA)                       
001000     03 SEQH-IDRT            PIC X(3).                                    
001100*                                 RETURTERMINAL                           
001200*                                 RETURN TERMINAL                         
001300     03 SEQH-KDRETSTA        PIC X.                                       
001400*                                 STATUS RETURER                          
001500*                                 RETURN STATUS                           
001600     03 SEQH-DAREGDAT        PIC 9(8).                                    
001700*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001800*                                 REGISTRATION DATE (YYYYMMDD)            
001900     03 SEQH-TIKLOCK         PIC S9(9)           COMP-3.                  
002000*                                 KLOCKSLAG (TTMMSSTH)                    
002100*                                 TIME OF DAY (HHMMSSTH)                  
002200     03 SEQH-IDDISTR         PIC S9(5)           COMP-3.                  
002300*                                 DISTRIKTNUMMER                          
002400*                                 DISTRICT NUMBER                         
002500     03 SEQH-IDKUNDNR        PIC S9(7)           COMP-3.                  
002600*                                 KUNDNUMMER                              
002700*                                 CUSTOMER NO                             
002800     03 SEQH-IDDC            PIC X(2).                                    
002900*                                 IDENTIFIERARE LAGER                     
003000*                                 WAREHOUSE IDENTIFIER                    
003100*** END OF VILMAII-COPY LENGTH= 26 BYTES                                  
