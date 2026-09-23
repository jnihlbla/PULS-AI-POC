000100 01  RESP-WL0141O1.                                                       
000200*                                 RESPONS FROM PGM WL0141                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDDISTR-KEY     PIC Z(3)9.                                   
000600*                                 DISTRIKTNUMMER                          
000700     03 RESP-IDKUNDNR-KEY    PIC Z(5)9.                                   
000800*                                 KUNDNUMMER                              
000900     03 RESP-IDKUNDRF-KEY    PIC Z(6)9.                                   
001000*                                 ORDERNUMMER                             
001100     03 RESP-IDARTNR-KEY     PIC Z(7)9.                                   
001200*                                 ARTIKELNUMMER                           
001300     03 RESP-IDKOLLI-KEY     PIC Z(4)9.                                   
001400*                                 KOLLINUMMER                             
001500     03 RESP-IDPRODNR-KEY    PIC Z(6)9.                                   
001600*                                 PRODUKTIONSNUMMER                       
001700     03 RESP-TEDDI           PIC X(11).                                   
001800*                                 TEXTFÄLT DDI                            
001900     03 RESP-KVRADER         PIC Z(4)9.                                   
002000*                                 ANTAL RADER                             
002100     03 RESP-RAD             OCCURS 500 TIMES.                            
002200*                                 RAD                                     
002300        05 RESP-IDARTNR      PIC Z(7)9.                                   
002400*                                 ARTIKELNUMMER                           
002500        05 RESP-KVBEART-Q    PIC Z(6)9.                                   
002600        05 RESP-KVAVBART     PIC Z(6)9.                                   
002700        05 RESP-KVLEVART     PIC Z(6)9.                                   
002800        05 RESP-KDAVVIK      PIC X.                                       
002900*                                 AVVIKELSE-KOD                           
003000        05 RESP-IDKUNDRF-WIP PIC X(10).                                   
003100*                                 KUNDENS REFERENS (ORDERID)              
003200        05 RESP-KDKOLSTA     PIC X(2).                                    
003300*                                 KOLLI-STATUS       KDKOLSTA-002         
003400        05 RESP-IDKOLLI      PIC Z(4)9.                                   
003500*                                 KOLLINUMMER                             
003600        05 RESP-KDFARLIG     PIC X.                                       
003700*                                 KOD FÖR FARLIGT GODS                    
003800        05 RESP-IDFAKT       PIC Z(6)9.                                   
003900*                                 FAKTURANUMMER                           
004000        05 RESP-IDLEVNR      PIC X(5).                                    
004100*                                 LEVERANTÖRNUMMER                        
004200        05 RESP-IDKUNDRF-URS PIC Z(6)9.                                   
004300*                                 ORDERNUMMER                             
004400*** END OF VILMAII-COPY LENGTH= 33555 BYTES                               
