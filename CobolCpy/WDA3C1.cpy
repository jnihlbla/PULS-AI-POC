000100 01  SEQC-WDA3C1.                                                         
000200*                                 SÄNDNINGSREGISTER                       
000300*                                 SECONDARY INDEX                         
000400*                                 FYSISK NYCKEL: WDA3C1KY                 
000500*                                  (IDDC, KDRETSTA, DARETANK              
000600*                                   IDRT, IDRTLOP,                        
000700*                                   IDDISTR, IDKUNDNR,                    
000800*                                   IDRAPPNR,                             
000900*                                   DAREGDAT, TIKLOCK)                    
001000*                                 SEKUNDÄR NYCKEL: WDA3CSEQ               
001100*                                  (IDDC, KDRETSTA,DARETANK,              
001200*                                   IDRETSND)                             
001300     03 SEQC-IDDC            PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500*                                 WAREHOUSE IDENTIFIER                    
001600     03 SEQC-KDRETSTA        PIC X.                                       
001700*                                 STATUS RETURER                          
001800*                                 RETURN STATUS                           
001900     03 SEQC-DARETANK        PIC 9(8).                                    
002000*                                 ANKOMSTDATUM (ÅÅÅÅMMDD)                 
002100*                                 DATE GOODS RECEIVING(YYYYMMDD)          
002200     03 SEQC-IDRETSND.                                                    
002300*                                 RETURSÄNDNING                           
002400*                                 RETURN TRANSFER                         
002500        05 SEQC-IDRT         PIC X(3).                                    
002600*                                 RETURTERMINAL                           
002700*                                 RETURN TERMINAL                         
002800        05 SEQC-IDRTLOP      PIC 9(3).                                    
002900*                                 RETUR TERMINAL LÖPNUMMER                
003000*                                 RETURN TERMINAL SEQUENCE NUMBER         
003100     03 SEQC-IDDISTR         PIC S9(5)           COMP-3.                  
003200*                                 DISTRIKTNUMMER                          
003300*                                 DISTRICT NUMBER                         
003400     03 SEQC-IDKUNDNR        PIC S9(7)           COMP-3.                  
003500*                                 KUNDNUMMER                              
003600*                                 CUSTOMER NO                             
003700     03 SEQC-IDRAPPNR        PIC 9(7).                                    
003800*                                 RAPPORT NUMMER                          
003900*                                 DISCREPANCY REPORT NUMBER               
004000     03 SEQC-DAREGDAT        PIC 9(8).                                    
004100*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
004200*                                 REGISTRATION DATE (YYYYMMDD)            
004300     03 SEQC-TIKLOCK         PIC S9(9)           COMP-3.                  
004400*                                 KLOCKSLAG (TTMMSSTH)                    
004500*                                 TIME OF DAY (HHMMSSTH)                  
004600*** END OF VILMAII-COPY LENGTH= 44 BYTES                                  
