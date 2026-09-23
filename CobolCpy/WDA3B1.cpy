000100 01  SEQB-WDA3B1.                                                         
000200*                                 SÄNDNINGSREGISTER                       
000300*                                 SECONDARY INDEX                         
000400*                                 FYSISK NYCKEL: WDA3B1KY                 
000500*                                  (IDRT, IDDC, IDRTLOP,                  
000600*                                   IDKOLLI, DAREGDAT,                    
000700*                                   TIKLOCK)                              
000800*                                 SEKUNDÄR NYCKEL: WDA3BSEQ               
000900*                                  (IDRT, IDDC, IDRTLOP,                  
001000*                                   IDKOLLI)                              
001100     03 SEQB-IDRT            PIC X(3).                                    
001200*                                 RETURTERMINAL                           
001300*                                 RETURN TERMINAL                         
001400     03 SEQB-IDDC            PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600*                                 WAREHOUSE IDENTIFIER                    
001700     03 SEQB-IDRTLOP         PIC 9(3).                                    
001800*                                 RETUR TERMINAL LÖPNUMMER                
001900*                                 RETURN TERMINAL SEQUENCE NUMBER         
002000     03 SEQB-IDKOLLI         PIC S9(5)           COMP-3.                  
002100*                                 KOLLINUMMER                             
002200*                                 CASE NUMBER                             
002300     03 SEQB-DAREGDAT        PIC 9(8).                                    
002400*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002500*                                 REGISTRATION DATE (YYYYMMDD)            
002600     03 SEQB-TIKLOCK         PIC S9(9)           COMP-3.                  
002700*                                 KLOCKSLAG (TTMMSSTH)                    
002800*                                 TIME OF DAY (HHMMSSTH)                  
002900*** END OF VILMAII-COPY LENGTH= 24 BYTES                                  
