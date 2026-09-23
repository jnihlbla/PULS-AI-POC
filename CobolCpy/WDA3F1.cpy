000100 01  SEQF-WDA3F1.                                                         
000200*                                 SÄNDNINGSREGISTER                       
000300*                                 SECONDARY INDEX                         
000400*                                 FYSISK NYCKEL: WDA3F1KY                 
000500*                                  (IDDC, IDDISTR, IDKUNDNR,              
000600*                                   IDRAPPNR, IDRT, IDRTLOP,              
000700*                                   IDKOLLI, DAREGDAT, TIKLOCK)           
000800*                                 SEKUNDÄR NYCKEL: WDA3FSEQ               
000900*                                  (IDDC, IDDISTR,                        
001000*                                   IDKUNDNR, IDRAPPNR)                   
001100     03 SEQF-IDDC            PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 SEQF-IDDISTR         PIC S9(5)           COMP-3.                  
001500*                                 DISTRIKTNUMMER                          
001600*                                 DISTRICT NUMBER                         
001700     03 SEQF-IDKUNDNR        PIC S9(7)           COMP-3.                  
001800*                                 KUNDNUMMER                              
001900*                                 CUSTOMER NO                             
002000     03 SEQF-IDRAPPNR        PIC 9(7).                                    
002100*                                 RAPPORT NUMMER                          
002200*                                 DISCREPANCY REPORT NUMBER               
002300     03 SEQF-IDRT            PIC X(3).                                    
002400*                                 RETURTERMINAL                           
002500*                                 RETURN TERMINAL                         
002600     03 SEQF-IDRTLOP         PIC 9(3).                                    
002700*                                 RETUR TERMINAL LÖPNUMMER                
002800*                                 RETURN TERMINAL SEQUENCE NUMBER         
002900     03 SEQF-IDKOLLI         PIC S9(5)           COMP-3.                  
003000*                                 KOLLINUMMER                             
003100*                                 CASE NUMBER                             
003200     03 SEQF-DAREGDAT        PIC 9(8).                                    
003300*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
003400*                                 REGISTRATION DATE (YYYYMMDD)            
003500     03 SEQF-TIKLOCK         PIC S9(9)           COMP-3.                  
003600*                                 KLOCKSLAG (TTMMSSTH)                    
003700*                                 TIME OF DAY (HHMMSSTH)                  
003800*** END OF VILMAII-COPY LENGTH= 38 BYTES                                  
