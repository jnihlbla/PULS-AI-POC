000100 01  REQU-WL0102I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WL0102             
000300*                                 LDC UNLOADING                           
000400     03 REQU-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 REQU-IDFAKT-KEY      PIC X(7).                                    
000800*                                 FAKTURANUMMER                           
000900*                                 INVOICE NO.                             
001000     03 REQU-IDUSER-003      PIC X(5).                                    
001100*                                 ANSVARIGT USERID INLÄGGN.(R32)          
001200     03 REQU-KVRADER         PIC 9(5).                                    
001300*                                 ANTAL RADER                             
001400*                                 NUMBER OF LINES                         
001500     03 REQU-KDMATT          PIC X.                                       
001600*                                 MÅTTKOD                                 
001700*                                 MEASUREMENT CODE                        
001800     03 REQU-TABELLRAD       OCCURS 500 TIMES.                            
001900*                                 GRUPP MED TABELLRADER                   
002000        05 REQU-CMD-IN       PIC X(3).                                    
002100        05 REQU-IDKUNDRF     PIC X(10).                                   
002200*                                 KUNDENS REFERENS (ORDERID)              
002300*                                 CUSTOMER REFERENCE (ORDER ID)           
002400        05 REQU-IDKUNDNR     PIC 9(6).                                    
002500*                                 KUNDNUMMER                              
002600*                                 CUSTOMER NO                             
002700        05 REQU-IDKOLLI      PIC 9(5).                                    
002800*                                 KOLLINUMMER                             
002900*                                 CASE NUMBER                             
003000        05 REQU-IDDISTR      PIC 9(5).                                    
003100*                                 DISTRIKTNUMMER                          
003200*                                 DISTRICT NUMBER                         
003300        05 REQU-ADINLOMR     PIC X(4).                                    
003400*                                 INLEVERANSOMRÅDE                        
003500*                                 RECEIVING AREA                          
003600*** END OF VILMAII-COPY LENGTH= 16520 BYTES                               
