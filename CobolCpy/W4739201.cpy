000100 01  W4739201.                                                            
000200*                                       SORTAREA                          
000300     03 IDDISTR-4768         PIC 9(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 IDKUNDNR-4768        PIC 9(6).                                    
000600*                                 KUNDNUMMER                              
000700     03 IDPRODNR-4768        PIC 9(7).                                    
000800*                                 PRODUKTIONSNUMMER                       
000900     03 IDDC-SEND            PIC X(2).                                    
001000*                                 SÄNDANDE LAGER                          
001100     03 IDKUNDRF-4768        PIC X(10).                                   
001200*                                 KUNDENS REFERENS (ORDERID)              
001300     03 4768-KDORDKL         PIC 9.                                       
001400*                                 ORDERKLASS                              
001500     03 4768-EMBTAB.                                                      
001600*                                 EMBALLAGEÅTGÅNGS-TABELL                 
001700        05 4768-EMBKLASS     OCCURS 4 TIMES.                              
001800           07 4768-KVEMBTYP  OCCURS 24 TIMES                              
001900                             PIC 9(5).                                    
002000*                                 FÖRBRUKNINGMÄNGD AV EMBALLAGE           
002100     03 4768-KDFAKTYP        PIC X.                                       
002200*                                 FAKTURATYP                              
002300     03 4768-IDFAKT          PIC 9(7).                                    
002400*                                 FAKTURANUMMER                           
002500     03 IDDC-REC             PIC X(2).                                    
002600*                                 MOTTAGANDE LAGER                        
002700*** END OF VILMAII-COPY LENGTH= 520 BYTES                                 
