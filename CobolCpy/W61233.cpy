000100 01  W61233.                                                              
000200*                                 COPYTEXT TILL FIL W61233                
000300     03 IDDC                 PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 IDFAKT               PIC S9(7)           COMP-3.                  
000600*                                 FAKTURANUMMER                           
000700     03 IDKUNDRF-GRP.                                                     
000800*                                 KUNDENS REFERENS (ORDERID)              
000900        05 IDKUNDRF          PIC X(10).                                   
001000*                                 KUNDENS REFERENS (ORDERID)              
001100        05 IDORDNR5-FILLER REDEFINES IDKUNDRF.                            
001200           07 IDORDNR5       PIC 9(5).                                    
001300*                                 ORDERNUMMER                             
001400           07 FILLER         PIC X(5).                                    
001500        05 IDORDNR7-FILLER REDEFINES IDKUNDRF.                            
001600           07 IDORDNR7       PIC 9(7).                                    
001700*                                 ORDERNUMMER                             
001800           07 FILLER         PIC X(3).                                    
001900     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
002000*                                 KUNDNUMMER                              
002100     03 IDKOLLI              PIC S9(5)           COMP-3.                  
002200*                                 KOLLINUMMER                             
002300     03 IDLBBET              PIC X(12).                                   
002400*                                 LASTBÄRARBETECKNING                     
002500     03 IDARTNR-KOLLI        PIC S9(7)           COMP-3.                  
002600*                                 DATAELEMENT                             
002700     03 IDARTNR-NEW          PIC S9(7)           COMP-3.                  
002800*                                 DATAELEMENT                             
002900     03 IDARTNR-PRIO         PIC S9(7)           COMP-3.                  
003000*                                 DATAELEMENT                             
003100     03 TEINFO               PIC X(7).                                    
003200     03 IDARTNR-BO           PIC S9(7)           COMP-3.                  
003300*                                 DATAELEMENT                             
003400     03 DABERANK             PIC 9(8).                                    
003500*                                 BERÄKNAD ANKOMSTDATUM                   
003600*** END OF VILMAII-COPY LENGTH= 66 BYTES                                  
