000100 01  MOD-W4O43101.                                                        
000200*                                 MOD-COPYTEXT FÖR W40431                 
000300*                                 PRIME COUNT SELECTION                   
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDIRLEV-IN      PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDDIRLEV-UT      PIC X(5).                                    
001100*                                 LEVERANTÖRNUMMER                        
001200     03 MOD-IDDISTR-IN       PIC X(2).                                    
001300*                                 MFS BEHANDLING AV INPUTFÄLT             
001400     03 MOD-IDDISTR-UT       PIC X(4).                                    
001500*                                 DISTRIKTNUMMER                          
001600     03 MOD-KDORDKL-IN       PIC X(2).                                    
001700*                                 MFS BEHANDLING AV INPUTFÄLT             
001800     03 MOD-KDORDKL-UT       PIC X.                                       
001900*                                 ORDERKLASS                              
002000     03 MOD-CMD-ATTR         PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200     03 MOD-CMD              PIC X.                                       
002300     03 MOD-KDORDKL-E-ATTR   PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500     03 MOD-KDORDKL-E        PIC 9.                                       
002600*                                 ORDERKLASS                              
002700     03 MOD-IDDISTR-E-ATTR   PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-IDDISTR-E        PIC Z(3)9.                                   
003000*                                 DISTRIKTNUMMER                          
003100     03 MOD-IDKUNDNR-E-ATTR  PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300     03 MOD-IDKUNDNR-E       PIC Z(5)9.                                   
003400*                                 KUNDNUMMER                              
003500     03 MOD-IDDC-E-ATTR      PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700     03 MOD-IDDC-E           PIC X(2).                                    
003800*                                 IDENTIFIERARE LAGER                     
003900     03 MOD-KDVIA-E-ATTR     PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 MOD-KDVIA-E          PIC X(2).                                    
004200*                                 IDENTIFIERARE LAGER                     
004300     03 MOD-TIMINUT-CUT-E-ATTR                                            
004400                             PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 MOD-TIMINUT-CUT-E    PIC X(5).                                    
004700     03 MOD-KVDAGAR-LEV-E-ATTR                                            
004800                             PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000     03 MOD-KVDAGAR-LEV-E    PIC Z(2)9.                                   
005100*                                 WORKDAYS TO DELIVERY                    
005200     03 MOD-KVDAGAR-TPO-E-ATTR                                            
005300                             PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500     03 MOD-KVDAGAR-TPO-E    PIC Z(2)9.                                   
005600*                                 ANTAL DAGAR TILL START AV TPO           
005700     03 MOD-TECKEN-ATTR      PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900     03 MOD-TECKEN           PIC X.                                       
006000     03 MOD-KVDAGAR-DIFF-E-ATTR                                           
006100                             PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300     03 MOD-KVDAGAR-DIFF-E   PIC Z(2)9.                                   
006400*                                 TRANSPORT DAY DIFF. (+/- CONTRA         
006500*                                  CDC)                                   
006600     03 MOD-TABELLRAD        OCCURS 10 TIMES.                             
006700*                                 GRUPP MED TABELL RADER                  
006800        05 MOD-KDORDKL       PIC 9.                                       
006900*                                 ORDERKLASS                              
007000        05 MOD-IDDISTR       PIC Z(3)9.                                   
007100*                                 DISTRIKTNUMMER                          
007200        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
007300*                                 KUNDNUMMER                              
007400        05 MOD-IDDC          PIC X(2).                                    
007500*                                 IDENTIFIERARE LAGER                     
007600        05 MOD-KDVIA         PIC X(2).                                    
007700*                                 KOD FöR LEVERANS VIA                    
007800        05 MOD-TIMINUT       PIC 9(2).9(2).                               
007900*                                 KLOCKSLAG (HH.MM)                       
008000        05 MOD-KVDAGAR-LEV   PIC -Z(2)9.                                  
008100*                                 WORKDAYS TO DELIVERY                    
008200        05 MOD-KVDAGAR-TPO   PIC -Z(2)9.                                  
008300*                                 ANTAL DAGAR TILL START AV TPO           
008400        05 MOD-KVDAGAR-DIFF  PIC -Z(2)9.                                  
008500*                                 TRANSPORT DAY DIFF. (+/- CONTRA         
008600*                                  CDC)                                   
008700     03 MOD-TEMFSINF         PIC X(55).                                   
008800*                                 INFORMATIONSMEDDELANDE                  
008900*** END OF VILMAII-COPY LENGTH= 488 BYTES                                 
