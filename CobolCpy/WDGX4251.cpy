000100 01  4251-WDGX4251.                                                       
000200*                                 ORDER CONSOLIDATION - DIRLEV            
000300*                                 NYCKEL: WDGXKEY                         
000400*                                  (IDHTYP, IDGMTREF, LOWVALUE)           
000500     03 4251-IDHTYP          PIC X(4).                                    
000600*                                 HÄNDELSETYP                             
000700     03 4251-IDGMTREF.                                                    
000800*                                 GODSMOTTAGAREREFERENS                   
000900*                                 GOODS RECEIVER REFERENS                 
001000        05 4251-IDDISTR      PIC S9(5)           COMP-3.                  
001100*                                 DISTRIKTNUMMER                          
001200*                                 DISTRICT NUMBER                         
001300        05 4251-IDKUNDNR     PIC S9(7)           COMP-3.                  
001400*                                 KUNDNUMMER                              
001500*                                 CUSTOMER NO                             
001600        05 4251-IDKUNDRF-GRP.                                             
001700*                                 KUNDENS REFERENS (ORDERID)              
001800*                                 CUSTOMER REFERENCE (ORDER ID)           
001900           07 4251-IDKUNDRF  PIC X(10).                                   
002000*                                 KUNDENS REFERENS (ORDERID)              
002100*                                 CUSTOMER REFERENCE (ORDER ID)           
002200           07 4251-IDORDNR5-FILLER REDEFINES 4251-IDKUNDRF.               
002300              09 4251-IDORDNR5                                            
002400                             PIC 9(5).                                    
002500*                                 ORDERNUMMER                             
002600*                                 ORDER NUMBER                            
002700              09 FILLER      PIC X(5).                                    
002800           07 4251-IDORDNR7-FILLER REDEFINES 4251-IDKUNDRF.               
002900              09 4251-IDORDNR7                                            
003000                             PIC 9(7).                                    
003100*                                 ORDERNUMMER                             
003200*                                 ORDER NUMBER                            
003300              09 FILLER      PIC X(3).                                    
003400     03 4251-LOWVALUE        PIC X(9).                                    
003500*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  
