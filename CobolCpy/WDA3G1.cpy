000100 01  SEQG-WDA3G1.                                                         
000200*                                 SƒNDNINGSREGISTER                       
000300*                                 SECONDARY INDEX                         
000400*                                 FYSISK NYCKEL: WDA3G1KY                 
000500*                                  (IDDC, KDRETSTA,                       
000600*                                   KDARBTYP, IDPERSON                    
000700*                                   DARETANK, IDRT, IDRTLOP               
000800*                                   IDKOLLI , DAREGDAT                    
000900*                                   TIKLOCK)                              
001000*                                 SEKUNDƒR NYCKEL: WDA3GSEQ               
001100*                                  (IDDC, KDRETSTA, KDARBTYP              
001200*                                   IDPERSON)                             
001300     03 SEQG-IDDC            PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500*                                 WAREHOUSE IDENTIFIER                    
001600     03 SEQG-KDRETSTA        PIC X.                                       
001700*                                 STATUS RETURER                          
001800*                                 RETURN STATUS                           
001900     03 SEQG-KDARBTYP        PIC X(8).                                    
002000*                                 TYP AV ARBETE                           
002100*                                 CATEGORY OF WORK                        
002200     03 SEQG-IDPERSON        PIC S9(3)           COMP-3.                  
002300*                                 PERSONKOD                               
002400*                                 STAFF CODE                              
002500     03 SEQG-DARETANK        PIC 9(8).                                    
002600*                                 ANKOMSTDATUM (≈≈≈≈MMDD)                 
002700*                                 DATE GOODS RECEIVING(YYYYMMDD)          
002800     03 SEQG-IDRT            PIC X(3).                                    
002900*                                 RETURTERMINAL                           
003000*                                 RETURN TERMINAL                         
003100     03 SEQG-IDRTLOP         PIC 9(3).                                    
003200*                                 RETUR TERMINAL L÷PNUMMER                
003300*                                 RETURN TERMINAL SEQUENCE NUMBER         
003400     03 SEQG-IDKOLLI         PIC S9(5)           COMP-3.                  
003500*                                 KOLLINUMMER                             
003600*                                 CASE NUMBER                             
003700     03 SEQG-DAREGDAT        PIC 9(8).                                    
003800*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
003900*                                 REGISTRATION DATE (YYYYMMDD)            
004000     03 SEQG-TIKLOCK         PIC S9(9)           COMP-3.                  
004100*                                 KLOCKSLAG (TTMMSSTH)                    
004200*                                 TIME OF DAY (HHMMSSTH)                  
004300     03 SEQG-IDRT-TRANSIT    PIC X(3).                                    
004400*                                 TRANSIT RETURTERMINAL                   
004500*                                 TRANSIT RETURN TERMINAL                 
004600*** END OF VILMAII-COPY LENGTH= 46 BYTES                                  
