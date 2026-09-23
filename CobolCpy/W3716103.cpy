000100 01  W3716103-CTX.                                                        
000200*                                 RAPPORT-POST FÖR BYTES                  
000300*                                 REGISTRERADE OBJEKT UTAN ORDER          
000400*                                                                         
000500     03 IDARTNR-OBJ          PIC S9(9)           COMP-3.                  
000600*                                 OBJEKTNUMMER                            
000700     03 REKSIFFR             PIC S9              COMP-3.                  
000800*                                 KONTROLLSIFFRA                          
000900     03 BEART-OBJ            PIC X(25).                                   
001000*                                 ARTIKELBENÄMNING                        
001100     03 IDDC                 PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 IDORDNR              PIC S9(5)           COMP-3.                  
001400*                                 ORDERNUMMER                             
001500     03 TIAAMMDD-REG         PIC S9(7)           COMP-3.                  
001600*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001700     03 KVRETUR-REST         PIC S9(5)           COMP-3.                  
001800*                                 RETURNERAD EJ AVBOKAD                   
001900     03 KVRETUR              PIC S9(5)           COMP-3.                  
002000*                                 ANTAL I RETUR                           
002100     03 IDKUNDRF             PIC X(10).                                   
002200*                                 KUNDENS REFERENS (ORDERID)              
002300     03 TIAAMMDD-RENS        PIC S9(7)           COMP-3.                  
002400*                                 RENSNINGSDATUM                          
002500     03 IDTABNR              PIC S9(3)           COMP-3.                  
002600*                                 TABELLNUMMER                            
002700*** END OF VILMAII-COPY LENGTH= 62 BYTES                                  
