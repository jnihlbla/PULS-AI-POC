000100 01  SEQF-WDT1F1-CTX.                                                     
000200*                                 SEKUNDÄRT INDEX TILL WDT1               
000300*                                 PÅFYLLNING AV PLOCKSATS                 
000400*                                 IDUSER TOM PLATS INGÅNG                 
000500*                                 FYSISK NYCKEL = WDT1F1KY                
000600*                                 (IDDC + IDUSER + ADLAGOMR-T +           
000700*                                  ADGANG-T + ADPLATS-T +                 
000800*                                  KDSTAPF + TIORDTIME)                   
000900*                                 SEKUNDÄR NYCKEL: WDT1FSEQ               
001000*                                 (IDDC + IDUSER + ADLAGOMR-T)            
001100     03 SEQF-IDDC            PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 SEQF-IDUSER          PIC X(8).                                    
001500*                                 ANVÄNDARENS SÄKERHETS ID                
001600*                                 USER SECURITY-IDENTITY                  
001700     03 SEQF-ADLAGOMR-TOM    PIC S9(3)           COMP-3.                  
001800*                                 LAGEROMRÅDE TILL OCH MED                
001900*                                 AREA ADDRESS TO                         
002000     03 SEQF-ADGANG-TOM      PIC S9(3)           COMP-3.                  
002100*                                 GÅNG TILL OCH MED                       
002200*                                 AISLE ADDRESS TO                        
002300     03 SEQF-ADPLATS-TOM     PIC S9(5)           COMP-3.                  
002400*                                 LAGERPLATS TILL OCH MED                 
002500*                                 LOCATION ADDRESS TO                     
002600     03 SEQF-KDSTAPF         PIC X.                                       
002700*                                 STATUS PÅ PÅFYLLNING                    
002800*                                 STATUS FOR FILLING                      
002900     03 SEQF-TIORDTIME       PIC 9(12).                                   
003000*                                 ORDER DATE AND TIME                     
003100*                                 (YYMMDDHHMM[SS])                        
003200     03 SEQF-IDWDT101        PIC X(19).                                   
003300*                                 NYCKEL TILL WDT101                      
003400*                                 KEY TO WDT101                           
003500*** END OF VILMAII-COPY LENGTH= 49 BYTES                                  
