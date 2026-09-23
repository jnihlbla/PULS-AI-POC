000100 01  WL10022.                                                             
000200*                                 COPYTEXT TILL CASE-REPORT LDC           
000300     03 IDAFPRCD             PIC X(10).                                   
000400*                                 AFP-BLANKETT POSTTYP                    
000500     03 IDKUNDRF-GRP.                                                     
000600*                                 KUNDENS REFERENS (ORDERID)              
000700        05 IDKUNDRF          PIC X(10).                                   
000800*                                 KUNDENS REFERENS (ORDERID)              
000900        05 IDORDNR5-FILLER REDEFINES IDKUNDRF.                            
001000           07 IDORDNR5       PIC Z(4)9.                                   
001100*                                 ORDERNUMMER                             
001200           07 FILLER         PIC X(5).                                    
001300        05 IDORDNR7-FILLER REDEFINES IDKUNDRF.                            
001400           07 IDORDNR7       PIC Z(6)9.                                   
001500*                                 ORDERNUMMER                             
001600           07 FILLER         PIC X(3).                                    
001700     03 IDKUNDNR             PIC Z(5)9.                                   
001800*                                 KUNDNUMMER                              
001900     03 IDKOLLI              PIC Z(4)9.                                   
002000*                                 KOLLINUMMER                             
002100     03 KVANTAL-KOLLI        PIC Z(5)9.                                   
002200*                                 ANTAL                                   
002300     03 KVANTAL-NEW          PIC Z(5)9.                                   
002400*                                 ANTAL                                   
002500     03 KVANTAL-PRIO         PIC Z(5)9.                                   
002600*                                 ANTAL                                   
002700     03 TEINFO               PIC X(7).                                    
002800     03 KVANTAL-BO           PIC Z(5)9.                                   
002900*                                 ANTAL                                   
003000*** END OF VILMAII-COPY LENGTH= 62 BYTES                                  
