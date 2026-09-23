000100 01  RESP-WL0195O1.                                                       
000200*                                 RESPONS FROM PGM WL0195                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500*                                 WAREHOUSE IDENTIFIER                    
000600     03 RESP-IDARTNR-KEY     PIC Z(7)9.                                   
000700*                                 ARTIKELNUMMER                           
000800*                                 PART NUMBER                             
000900     03 RESP-BELEVART-KEY    PIC X(30).                                   
001000*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
001100*                                 SUPPLIER PART DESCRIPTION               
001200     03 RESP-KVRADER         PIC Z(4)9.                                   
001300*                                 ANTAL RADER                             
001400*                                 NUMBER OF LINES                         
001500     03 RESP-RADER           OCCURS 500 TIMES.                            
001600*                                                                         
001700        05 RESP-IDARTNR      PIC Z(7)9.                                   
001800*                                 ARTIKELNUMMER                           
001900*                                 PART NUMBER                             
002000        05 RESP-BEART        PIC X(25).                                   
002100*                                 ARTIKELBENÄMNING                        
002200*                                 PART DESCRIPTION                        
002300        05 RESP-IDLEVNR      PIC X(5).                                    
002400*                                 LEVERANTÖRNUMMER                        
002500*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002600        05 RESP-IDBENR       PIC 9.                                       
002700*                                 BENÄMNINGSNUMMER                        
002800        05 RESP-BELEVART     PIC X(30).                                   
002900*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
003000*                                 SUPPLIER PART DESCRIPTION               
003100*** END OF VILMAII-COPY LENGTH= 34545 BYTES                               
