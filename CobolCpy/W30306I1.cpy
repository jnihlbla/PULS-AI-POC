000100 01  MID-W30306I1.                                                        
000200*                                 MID-COPYTEXT FOR W30306                 
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500*                                 DISTRICT NUMBER                         
000600     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000700*                                 KUNDNUMMER                              
000800*                                 CUSTOMER NO                             
000900     03 MID-KDPRSTA-IN       PIC X.                                       
001000*                                 STATUS PRISFRÅGA                        
001100*                                 STATUS PRICE QUESTION                   
001200     03 MID-INPUT            OCCURS 13 TIMES.                             
001300*                                 RADINFORMATION                          
001400*                                 LINE INFORMATION                        
001500        05 MID-KDCMD         PIC X.                                       
001600*                                 RAD-UPPDATERINGSKOMMANDO                
001700*                                  BLANK  = INGENTING                     
001800*                                  D , B  = DELETE                        
001900*                                  R , Ä  = REPLACE                       
002000*                                  I , N  = INSERT                        
002100*                                  S , V  = SELECT                        
002200*                                  P , P  = PRINT                         
002300*                                 LINE UPDATE COMMAND                     
002400        05 MID-IDDISTR       PIC 9(4).                                    
002500*                                 DISTRIKTNUMMER                          
002600*                                 DISTRICT NUMBER                         
002700        05 MID-IDKUNDNR      PIC X(6).                                    
002800*                                 KUNDNUMMER                              
002900*                                 CUSTOMER NO                             
003000        05 MID-KDPRSTA       PIC X.                                       
003100*                                 STATUS PRISFRÅGA                        
003200*                                 STATUS PRICE QUESTION                   
003300        05 MID-IDARTNR       PIC 9(9).                                    
003400*                                 ARTIKELNUMMER                           
003500*                                 PART NUMBER                             
003600        05 MID-IDORDNR7      PIC 9(7).                                    
003700*                                 ORDERNUMMER                             
003800*                                 ORDER NUMBER                            
003900        05 MID-KDVALISO      PIC X(3).                                    
004000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004100*                                 CURRENCY CODE BY ISO-STANDARD.          
004200        05 MID-PRARTBTO-LOC  PIC X(10).                                   
004300*                                 PRIS I LOKAL VALUTA                     
004400*                                 LOCAL GROSS SALES PRICE                 
004500        05 MID-PRARTNTO-LOC  PIC X(10).                                   
004600*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
004700*                                 NET PRICE EACH LOCAL CURRENCY           
004800        05 MID-KDRAB         PIC X(5).                                    
004900*                                 RABATTKOD                               
005000*** END OF VILMAII-COPY LENGTH= 739 BYTES                                 
