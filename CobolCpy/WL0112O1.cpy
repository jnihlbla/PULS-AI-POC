000100 01  RESP-WL0112O1.                                                       
000200*                                 RESPONS FROM PGM WL0112O1               
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500*                                 WAREHOUSE IDENTIFIER                    
000600     03 RESP-IDARTNR-KEY     PIC Z(7)9.                                   
000700*                                 ARTIKELNUMMER                           
000800*                                 PART NUMBER                             
000900     03 RESP-KDLOC-KEY       PIC X.                                       
001000*                                 TYP AV LAGERPLATS                       
001100*                                 TYPE OF LOCATION                        
001200     03 RESP-BEART           PIC X(25).                                   
001300*                                 ARTIKELBENÄMNING                        
001400*                                 PART DESCRIPTION                        
001500     03 RESP-KVRADER         PIC Z(4)9.                                   
001600*                                 ANTAL RADER                             
001700*                                 NUMBER OF LINES                         
001800     03 RESP-RAD             OCCURS 500 TIMES.                            
001900        05 RESP-TISTADAT     PIC X(6).                                    
002000*                                 GENERELLT STARTDATUM                    
002100*                                 GENERAL START DATE                      
002200        05 RESP-ADLAGOMR     PIC Z9.                                      
002300*                                 LAGEROMRÅDE                             
002400*                                 AREA                                    
002500        05 RESP-ADGANG       PIC Z9.                                      
002600*                                 GÅNG                                    
002700*                                 AISLE                                   
002800        05 RESP-ADPLATS      PIC Z(4)9.                                   
002900*                                 LAGERPLATSNUMMER                        
003000*                                 LOCATION                                
003100        05 RESP-TISTODAT     PIC X(6).                                    
003200*                                 GENERELLT STOPPDATUM                    
003300*                                 GENERAL STOP DATE YYMMDD                
003400        05 RESP-KDLOC        PIC X.                                       
003500*                                 TYP AV LAGERPLATS                       
003600*                                 TYPE OF LOCATION                        
003700        05 RESP-IDUSER-STA   PIC X(8).                                    
003800*                                 ANVÄNDARENS SÄKERHETS ID                
003900*                                 USER SECURITY-IDENTITY                  
004000        05 RESP-IDUSER-STO   PIC X(8).                                    
004100*                                 ANVÄNDARENS SÄKERHETS ID                
004200*                                 USER SECURITY-IDENTITY                  
004300*** END OF VILMAII-COPY LENGTH= 19041 BYTES                               
