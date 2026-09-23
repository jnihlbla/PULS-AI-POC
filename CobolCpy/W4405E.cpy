000100 01  W4405E.                                                              
000200*                                 UPPF. POST VORK÷ NY                     
000300     03 IDDISTR              PIC 9(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 FILLER               PIC X.                                       
000600     03 IDKUNDNR             PIC 9(7).                                    
000700*                                 KUNDNUMMER                              
000800     03 FILLER               PIC X.                                       
000900     03 IDKUNDRF-GRP.                                                     
001000*                                 KUNDENS REFERENS (ORDERID)              
001100        05 IDKUNDRF          PIC X(10).                                   
001200*                                 KUNDENS REFERENS (ORDERID)              
001300        05 IDORDNR5-FILLER REDEFINES IDKUNDRF.                            
001400           07 IDORDNR5       PIC 9(5).                                    
001500*                                 ORDERNUMMER                             
001600           07 FILLER         PIC X(5).                                    
001700        05 IDORDNR7-FILLER REDEFINES IDKUNDRF.                            
001800           07 IDORDNR7       PIC 9(7).                                    
001900*                                 ORDERNUMMER                             
002000           07 FILLER         PIC X(3).                                    
002100     03 FILLER               PIC X.                                       
002200     03 TIREGDAT-URSP        PIC 9(6).                                    
002300*                                 REGISTRERINGSDATUM (≈≈MMDD)             
002400     03 FILLER               PIC X.                                       
002500     03 IDARTNR              PIC 9(9).                                    
002600*                                 ARTIKELNUMMER                           
002700     03 FILLER               PIC X.                                       
002800     03 KDVORSTA             PIC X.                                       
002900*                                 STATUS P≈ VORRAD, F÷R UPPF÷LJN.         
003000     03 FILLER               PIC X.                                       
003100     03 KVWORKD              PIC 9(3).                                    
003200*                                 ANTAL ARBETSDAGAR                       
003300     03 FILLER               PIC X.                                       
003400     03 KVRADER              PIC 9(5).                                    
003500*                                 ANTAL RADER                             
003600*** END OF VILMAII-COPY LENGTH= 52 BYTES                                  
