000100 01  W4405D.                                                              
000200*                                 UPPF. POST VORKÖ NY                     
000300     03 IDDISTR              PIC S9(5)           COMP-3.                  
000400*                                 DISTRIKTNUMMER                          
000500     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000600*                                 KUNDNUMMER                              
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
001900     03 TIREGDAT-URSP        PIC S9(7)           COMP-3.                  
002000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002100     03 IDARTNR              PIC S9(9)           COMP-3.                  
002200*                                 ARTIKELNUMMER                           
002300     03 KDVORATG             PIC X.                                       
002400*                                 TYP AV ÅTGÄRD FÖR POST VOR-KÖN          
002500     03 KVWORKD              PIC S9(3)           COMP-3.                  
002600*                                 ANTAL ARBETSDAGAR                       
002700     03 KVRADER              PIC S9(5)           COMP-3.                  
002800*                                 ANTAL RADER                             
002900*** END OF VILMAII-COPY LENGTH= 32 BYTES                                  
