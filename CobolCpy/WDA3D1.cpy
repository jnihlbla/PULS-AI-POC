000100 01  SEQD-WDA3D1.                                                         
000200*                                 SƒNDNINGSREGISTER                       
000300*                                 SECONDARY INDEX                         
000400*                                 FYSISK NYCKEL: WDA3D1KY                 
000500*                                  (IDDC, KDRETSTA, KDARBTYP,             
000600*                                   IDPERSON, DARETANK, IDRT,             
000700*                                   DASNDDAT, IDDISTR, IDKUNDNR,          
000800*                                   IDRAPPNR, DAREGDAT, TIKLOCK)          
000900*                                 SEKUNDƒR NYCKEL: WDA3DSEQ               
001000*                                  (IDDC, KDRETSTA, KDARBTYP,             
001100*                                   IDPERSON)                             
001200     03 SEQD-IDDC            PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400*                                 WAREHOUSE IDENTIFIER                    
001500     03 SEQD-KDRETSTA        PIC X.                                       
001600*                                 STATUS RETURER                          
001700*                                 RETURN STATUS                           
001800     03 SEQD-KDARBTYP        PIC X(8).                                    
001900*                                 TYP AV ARBETE                           
002000*                                 CATEGORY OF WORK                        
002100     03 SEQD-IDPERSON        PIC S9(3)           COMP-3.                  
002200*                                 PERSONKOD                               
002300*                                 STAFF CODE                              
002400     03 SEQD-DARETANK        PIC 9(8).                                    
002500*                                 ANKOMSTDATUM (≈≈≈≈MMDD)                 
002600*                                 DATE GOODS RECEIVING(YYYYMMDD)          
002700     03 SEQD-IDRT            PIC X(3).                                    
002800*                                 RETURTERMINAL                           
002900*                                 RETURN TERMINAL                         
003000     03 SEQD-DASNDDAT        PIC 9(8).                                    
003100*                                 SƒNDNINGSDATUM   (≈≈≈≈MMDD)             
003200*                                 SHIPPING DATE   (YYYYMMDD)              
003300     03 SEQD-IDDISTR         PIC S9(5)           COMP-3.                  
003400*                                 DISTRIKTNUMMER                          
003500*                                 DISTRICT NUMBER                         
003600     03 SEQD-IDKUNDNR        PIC S9(7)           COMP-3.                  
003700*                                 KUNDNUMMER                              
003800*                                 CUSTOMER NO                             
003900     03 SEQD-IDRAPPNR        PIC 9(7).                                    
004000*                                 RAPPORT NUMMER                          
004100*                                 DISCREPANCY REPORT NUMBER               
004200     03 SEQD-DAREGDAT        PIC 9(8).                                    
004300*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
004400*                                 REGISTRATION DATE (YYYYMMDD)            
004500     03 SEQD-TIKLOCK         PIC S9(9)           COMP-3.                  
004600*                                 KLOCKSLAG (TTMMSSTH)                    
004700*                                 TIME OF DAY (HHMMSSTH)                  
004800     03 SEQD-KVRADER         PIC S9(5)           COMP-3.                  
004900*                                 ANTAL RADER                             
005000*                                 NUMBER OF LINES                         
005100     03 SEQD-IDRTLOP         PIC 9(3).                                    
005200*                                 RETUR TERMINAL L÷PNUMMER                
005300*                                 RETURN TERMINAL SEQUENCE NUMBER         
005400     03 SEQD-KVKOLLI-AAF     PIC S9(5)           COMP-3.                  
005500*                                 ANTAL KOLLI FR≈N ≈TERF÷RSƒLJARE         
005600*                                 QTY CASES FROM DEALER                   
005700*** END OF VILMAII-COPY LENGTH= 68 BYTES                                  
