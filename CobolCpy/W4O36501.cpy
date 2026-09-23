000100 01  MOD-W4O36501.                                                        
000200*                                 MOD-COPYTEXT FÖR W4O36500               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-PRCNR-IN.                                                     
000800*                                 PRODUKTIONSKANAL                        
000900        05 MOD-IDPRCBAS      PIC X(3).                                    
001000*                                 PRC-BAS                                 
001100        05 MOD-IDPRCVAR      PIC X.                                       
001200*                                 PRC-VARIANT                             
001300     03 MOD-PRCNR-UT.                                                     
001400*                                 PRODUKTIONSKANAL                        
001500        05 MOD-IDPRCBAS      PIC X(3).                                    
001600*                                 PRC-BAS                                 
001700        05 MOD-IDPRCVAR      PIC X.                                       
001800*                                 PRC-VARIANT                             
001900     03 MOD-IDDC-IN          PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 MOD-IDDC-UT          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MOD-LAGOMR-ENTER     PIC Z9.                                      
002400*                                 LAGEROMRÅDE                             
002500     03 MOD-LAGOMR-NEXT      PIC Z9.                                      
002600*                                 LAGEROMRÅDE                             
002700     03 MOD-KDPRTGEN-PU-ATTR PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-KDPRTGEN-PU      PIC X(3).                                    
003000*                                 PRINTERKOD PACKUNDERLAG                 
003100     03 MOD-KDPRTGEN-PLE-ATTR                                             
003200                             PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 MOD-KDPRTGEN-PLE     PIC X(3).                                    
003500*                                 PRINTERKOD PLOCKETIKETTER               
003600     03 MOD-TABELLRAD        OCCURS 20 TIMES.                             
003700*                                 GRUPP MED TABELL RADER                  
003800        05 MOD-LAGOMR-ATTR   PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000        05 MOD-LAGOMR        PIC Z9.                                      
004100*                                 LAGEROMRÅDE                             
004200        05 MOD-IDPRC-ATTR    PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400        05 MOD-IDPRC.                                                     
004500*                                 PRODUKTIONSKANAL                        
004600           07 MOD-IDPRCBAS   PIC X(3).                                    
004700*                                 PRC-BAS                                 
004800           07 MOD-IDPRCVAR   PIC X.                                       
004900*                                 PRC-VARIANT                             
005000        05 MOD-KDPRT-PU-ATTR PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200        05 MOD-KDPRT-PU      PIC X(3).                                    
005300*                                 PRINTERKOD PACKUNDERLAG                 
005400        05 MOD-KDSS-PU-ATTR  PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600        05 MOD-KDSS-PU       PIC X.                                       
005700*                                 SIDOSKIPSKOD                            
005800        05 MOD-KDPRT-PLE-ATTR                                             
005900                             PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100        05 MOD-KDPRT-PLE     PIC X(3).                                    
006200*                                 PRINTERKOD PLOCKETIKETTER               
006300        05 MOD-KDSS-PLE-ATTR PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500        05 MOD-KDSS-PLE      PIC X.                                       
006600*                                 SIDOSKIPSKOD                            
006700     03 MOD-LAGOMR-UPP-ATTR  PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900     03 MOD-LAGOMR-UPP       PIC X(2).                                    
007000*                                 MFS BEHANDLING AV INPUTFÄLT             
007100     03 MOD-INDATA.                                                       
007200*                                 INDATAFÄLT                              
007300        05 MOD-IDPRC-UPP-ATTR                                             
007400                             PIC X(2).                                    
007500*                                 MFS ATTRIBUTFÄLT                        
007600        05 MOD-IDPRC-UPP     PIC X(2).                                    
007700*                                 MFS BEHANDLING AV INPUTFÄLT             
007800        05 MOD-KDPRT-PU-UPP-ATTR                                          
007900                             PIC X(2).                                    
008000*                                 MFS ATTRIBUTFÄLT                        
008100        05 MOD-KDPRT-PU-UPP  PIC X(2).                                    
008200*                                 MFS BEHANDLING AV INPUTFÄLT             
008300        05 MOD-KDSS-PU-UPP-ATTR                                           
008400                             PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600        05 MOD-KDSS-PU-UPP   PIC X(2).                                    
008700*                                 MFS BEHANDLING AV INPUTFÄLT             
008800        05 MOD-KDPRT-PLE-UPP-ATTR                                         
008900                             PIC X(2).                                    
009000*                                 MFS ATTRIBUTFÄLT                        
009100        05 MOD-KDPRT-PLE-UPP PIC X(2).                                    
009200*                                 MFS BEHANDLING AV INPUTFÄLT             
009300        05 MOD-KDSS-PLE-UPP-ATTR                                          
009400                             PIC X(2).                                    
009500*                                 MFS ATTRIBUTFÄLT                        
009600        05 MOD-KDSS-PLE-UPP  PIC X(2).                                    
009700*                                 MFS BEHANDLING AV INPUTFÄLT             
009800     03 MOD-TEMFSINF         PIC X(55).                                   
009900*                                 INFORMATIONSMEDDELANDE                  
010000*** END COPY W4O36501    LENGTH=669                                       
