000100 01  MOD-W4O51301.                                                        
000200*                                 MOD-COPYTEXT F÷R W40513                 
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001200*                                 KUNDNUMMER                              
001300     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MOD-IDDC-IN          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MOD-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MOD-KDORDKL-IN       PIC X.                                       
002000*                                 ORDERKLASS                              
002100     03 MOD-KDORDKL-UT       PIC X.                                       
002200*                                 ORDERKLASS                              
002300     03 MOD-KDORDSTA-IN      PIC X.                                       
002400*                                 VOLVOORDERSTATUS                        
002500     03 MOD-KDORDSTA-UT      PIC X.                                       
002600*                                 VOLVOORDERSTATUS                        
002700     03 MOD-IDTRANS-RAD      OCCURS 14 TIMES                              
002800                             PIC X(4).                                    
002900*                                 BILDNUMMER                              
003000     03 MOD-IDKUNDNR         OCCURS 14 TIMES                              
003100                             PIC Z(5)9.                                   
003200*                                 KUNDNUMMER                              
003300     03 MOD-IDORDNR7         OCCURS 14 TIMES                              
003400                             PIC Z(6)9.                                   
003500*                                 ORDERNUMMER                             
003600     03 MOD-IDDC             OCCURS 14 TIMES                              
003700                             PIC X(2).                                    
003800*                                 IDENTIFIERARE LAGER                     
003900     03 MOD-TIREGDAT         OCCURS 14 TIMES                              
004000                             PIC 9(6).                                    
004100*                                 REGISTRERINGSDATUM (≈≈MMDD)             
004200     03 MOD-KDORDKL          OCCURS 14 TIMES                              
004300                             PIC 9.                                       
004400*                                 ORDERKLASS                              
004500     03 MOD-KDORDSTA         OCCURS 14 TIMES                              
004600                             PIC X(2).                                    
004700*                                 VOLVOORDERSTATUS                        
004800     03 MOD-TEMFSINF         PIC X(55).                                   
004900*                                 INFORMATIONSMEDDELANDE                  
005000*** END OF VILMAII-COPY LENGTH= 519 BYTES                                 
