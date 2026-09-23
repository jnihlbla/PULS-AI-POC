000100 01  SEQI-WDA3I1.                                                         
000200*                                 SÄNDNINGSREGISTER                       
000300*                                 SECONDARY INDEX                         
000400*                                 FYSISK NYCKEL: WDA3I1KY                 
000500*                                  (IDRTTR, KDRETSTA,                     
000600*                                   DAREGDAT, TIKLOCK,                    
000700*                                   IDDISTR, IDKUNDNR)                    
000800*                                 SEKUNDÄR NYCKEL: WDA3ISEQ               
000900*                                  (IDRTTR, KDRETSTA)                     
001000     03 SEQI-IDRT-TRANSIT    PIC X(3).                                    
001100*                                 TRANSIT RETURTERMINAL                   
001200*                                 TRANSIT RETURN TERMINAL                 
001300     03 SEQI-KDRETSTA        PIC X.                                       
001400*                                 STATUS RETURER                          
001500*                                 RETURN STATUS                           
001600     03 SEQI-DAREGDAT        PIC 9(8).                                    
001700*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001800*                                 REGISTRATION DATE (YYYYMMDD)            
001900     03 SEQI-TIKLOCK         PIC S9(9)           COMP-3.                  
002000*                                 KLOCKSLAG (TTMMSSTH)                    
002100*                                 TIME OF DAY (HHMMSSTH)                  
002200     03 SEQI-IDDISTR         PIC S9(5)           COMP-3.                  
002300*                                 DISTRIKTNUMMER                          
002400*                                 DISTRICT NUMBER                         
002500     03 SEQI-IDKUNDNR        PIC S9(7)           COMP-3.                  
002600*                                 KUNDNUMMER                              
002700*                                 CUSTOMER NO                             
002800     03 SEQI-IDDC            PIC X(2).                                    
002900*                                 IDENTIFIERARE LAGER                     
003000*                                 WAREHOUSE IDENTIFIER                    
003100*** END OF VILMAII-COPY LENGTH= 26 BYTES                                  
