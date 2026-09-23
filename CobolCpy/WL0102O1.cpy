000100 01  RESP-WL0102O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WL0102         
000300*                                 LDC UNLOADING                           
000400     03 RESP-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 RESP-IDFAKT-KEY      PIC X(7).                                    
000800*                                 FAKTURANUMMER                           
000900*                                 INVOICE NO.                             
001000     03 RESP-IDUSER-003      PIC X(5).                                    
001100*                                 ANSVARIGT USERID INLÄGGN.(R32)          
001200     03 RESP-KVRADER         PIC 9(5).                                    
001300*                                 ANTAL RADER                             
001400*                                 NUMBER OF LINES                         
001500     03 RESP-TABELLRAD       OCCURS 500 TIMES.                            
001600*                                 GRUPP MED TABELLRADER                   
001700        05 RESP-CMD-IN       PIC X(3).                                    
001800        05 RESP-IDKUNDRF     PIC X(10).                                   
001900*                                 KUNDENS REFERENS (ORDERID)              
002000*                                 CUSTOMER REFERENCE (ORDER ID)           
002100        05 RESP-IDKUNDNR     PIC Z(5)9.                                   
002200*                                 KUNDNUMMER                              
002300*                                 CUSTOMER NO                             
002400        05 RESP-IDKOLLI      PIC Z(4)9.                                   
002500*                                 KOLLINUMMER                             
002600*                                 CASE NUMBER                             
002700        05 RESP-KDKOLLI      PIC X(8).                                    
002800*                                 KOLLIKOD                                
002900*                                 KOLLI CODE                              
003000        05 RESP-IDARTNR-KOLLI                                             
003100                             PIC Z(5)9.                                   
003200*                                 ANTAL                                   
003300*                                 NUMBER                                  
003400        05 RESP-IDARTNR-NEW  PIC Z(6).                                    
003500*                                 ANTAL                                   
003600*                                 NUMBER                                  
003700        05 RESP-IDARTNR-PRIO PIC Z(6).                                    
003800*                                 ANTAL                                   
003900*                                 NUMBER                                  
004000        05 RESP-TEINFO       PIC X(7).                                    
004100        05 RESP-ADINLOMR     PIC X(4).                                    
004200*                                 INLEVERANSOMRÅDE                        
004300*                                 RECEIVING AREA                          
004400        05 RESP-IDMSG-ERROR-LINE                                          
004500                             PIC X(3).                                    
004600*                                 FELMEDDELANDE ID                        
004700*                                 ERROR MESSAGE ID                        
004800*** END OF VILMAII-COPY LENGTH= 32019 BYTES                               
