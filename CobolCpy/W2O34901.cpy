000100 01  MOD-W2O34901.                                                        
000200*                                 MOD-COPYTEXT FÖR W2034900               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDC-SEND-IN     PIC X(2).                                    
000800*                                 SÄNDANDE LAGER                          
000900     03 MOD-IDDC-REC-IN      PIC X(2).                                    
001000*                                 MOTTAGANDE LAGER                        
001100     03 MOD-KDARBTYP-IN      PIC X(8).                                    
001200*                                 TYP AV ARBETE                           
001300     03 MOD-IDDC-SEND-UT     PIC X(2).                                    
001400*                                 SÄNDANDE LAGER                          
001500     03 MOD-IDDC-REC-UT      PIC X(2).                                    
001600*                                 MOTTAGANDE LAGER                        
001700     03 MOD-KDARBTYP-UT      PIC X(8).                                    
001800*                                 TYP AV ARBETE                           
001900     03 MOD-DCRUBR1          PIC X(6).                                    
002000     03 MOD-DCRUBR2          PIC X(6).                                    
002100     03 MOD-TABELLRAD        OCCURS 22 TIMES.                             
002200        05 MOD-CMD-ATTR      PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400        05 MOD-CMD           PIC X(2).                                    
002500*                                 MFS BEHANDLING AV INPUTFÄLT             
002600        05 MOD-IDDC          PIC X(2).                                    
002700*                                 IDENTIFIERARE LAGER                     
002800        05 MOD-IDDISTR       PIC Z(3)9.                                   
002900*                                 DISTRIKTNUMMER                          
003000        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
003100*                                 KUNDNUMMER                              
003200     03 MOD-NY-IDDC-ATTR     PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 MOD-NY-IDDC          PIC X(2).                                    
003500*                                 IDENTIFIERARE LAGER                     
003600     03 MOD-NY-IDDISTR-ATTR  PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800     03 MOD-NY-IDDISTR       PIC Z(3)9.                                   
003900*                                 DISTRIKTNUMMER                          
004000     03 MOD-NY-IDKUNDNR-ATTR PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200     03 MOD-NY-IDKUNDNR      PIC Z(5)9.                                   
004300*                                 KUNDNUMMER                              
004400     03 MOD-TEMFSINF         PIC X(55).                                   
004500*                                 INFORMATIONSMEDDELANDE                  
004600*** END OF VILMAII-COPY LENGTH= 505 BYTES                                 
