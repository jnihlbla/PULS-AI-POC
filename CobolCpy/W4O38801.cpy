000100 01  MOD-W4O38801.                                                        
000200*                                 COPYTEXT FOR MOD W4O38801               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-KDPRCGRP-IN      PIC X(5).                                    
000800*                                 PRODUKTIONSKANALSGRUPP                  
000900     03 MOD-IDDC-IN          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-KDPRCGRP-UT      PIC X(5).                                    
001200*                                 PRODUKTIONSKANALSGRUPP                  
001300     03 MOD-IDDC-UT          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-KDPRODKL         PIC X.                                       
001600*                                 PRODUKTIONSKLASS                        
001700     03 MOD-IDPRC.                                                        
001800*                                 PRODUKTIONSKANAL                        
001900        05 MOD-IDPRCBAS      PIC X(3).                                    
002000*                                 PRC-BAS                                 
002100        05 MOD-IDPRCVAR      PIC X.                                       
002200*                                 PRC-VARIANT                             
002300     03 MOD-IDLISTTYP-ENTER  PIC X(6).                                    
002400*                                 LISTTYP                                 
002500     03 MOD-IDLISTA-ENTER    PIC X(3).                                    
002600*                                 LISTNUMMER                              
002700     03 MOD-IDLISTTYP-NEXT   PIC X(6).                                    
002800*                                 LISTTYP                                 
002900     03 MOD-IDLISTA-NEXT     PIC X(3).                                    
003000*                                 LISTNUMMER                              
003100     03 MOD-LISTBEST         OCCURS 13 TIMES.                             
003200*                                 ORDERED LISTS                           
003300        05 MOD-LISTA-ATTR    PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500        05 MOD-LISTA.                                                     
003600*                                 LIST-INPUT                              
003700           07 MOD-IDLISTTYP-UT                                            
003800                             PIC X(6).                                    
003900*                                 LISTTYP                                 
004000           07 MOD-FILLER-UT  PIC X.                                       
004100           07 MOD-IDLISTA-UT PIC X(3).                                    
004200*                                 LISTNUMMER                              
004300        05 MOD-FLBEST-UT-ATTR                                             
004400                             PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600        05 MOD-FLBEST-UT     PIC X.                                       
004700*                                 LISTA BESTÄLLD                          
004800        05 MOD-BELISTA-UT-ATTR                                            
004900                             PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100        05 MOD-BELISTA-UT    PIC X(25).                                   
005200*                                 TYP AV LISTNING                         
005300     03 MOD-INDATA.                                                       
005400*                                 LIST-INPUT                              
005500        05 MOD-IDLISTTYP-IN-ATTR                                          
005600                             PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800        05 MOD-IDLISTTYP-IN  PIC X(2).                                    
005900*                                 MFS BEHANDLING AV INPUTFÄLT             
006000        05 MOD-IDLISTA-IN-ATTR                                            
006100                             PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300        05 MOD-IDLISTA-IN    PIC X(2).                                    
006400*                                 MFS BEHANDLING AV INPUTFÄLT             
006500        05 MOD-FLBEST-IN-ATTR                                             
006600                             PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800        05 MOD-FLBEST-IN     PIC X(2).                                    
006900*                                 MFS BEHANDLING AV INPUTFÄLT             
007000        05 MOD-BELISTA-IN-ATTR                                            
007100                             PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300        05 MOD-BELISTA-IN    PIC X(2).                                    
007400*                                 MFS BEHANDLING AV INPUTFÄLT             
007500        05 MOD-FLBORT-IN-ATTR                                             
007600                             PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800        05 MOD-FLBORT-IN     PIC X(2).                                    
007900*                                 MFS BEHANDLING AV INPUTFÄLT             
008000     03 MOD-TEMFSINF         PIC X(55).                                   
008100*                                 INFORMATIONSMEDDELANDE                  
