000100 01  MOD-W4O11501.                                                        
000200*                                 MOD-COPYTEXT                            
000300*                                 FÖR W4O115                              
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDKVAFELI        PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDSKYLTI         PIC X(2).                                    
001100*                                 MFS BEHANDLING AV INPUTFÄLT             
001200     03 MOD-IDKVAFELU        PIC X(2).                                    
001300*                                 KVALITET FELKOD FÖR ARTIKEL             
001400     03 MOD-IDSKYLTU         PIC X(3).                                    
001500*                                 NATIONALITETSTECKEN                     
001600     03 MOD-IDKVAFEL-NX      PIC X(2).                                    
001700*                                 KVALITET FELKOD FÖR ARTIKEL             
001800     03 MOD-IDSKYLT-NX       PIC X(3).                                    
001900*                                 NATIONALITETSTECKEN                     
002000     03 MOD-IDKVAFEL-EN      PIC X(2).                                    
002100*                                 KVALITET FELKOD FÖR ARTIKEL             
002200     03 MOD-IDSKYLT-EN       PIC X(3).                                    
002300*                                 NATIONALITETSTECKEN                     
002400     03 MOD-RAD              OCCURS 10 TIMES.                             
002500        05 MOD-IDKVAFEL-UT-ATTR                                           
002600                             PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800        05 MOD-IDKVAFEL-UT   PIC Z9.                                      
002900*                                 KVALITET FELKOD FÖR ARTIKEL             
003000        05 MOD-KDKVAFG-UT-ATTR                                            
003100                             PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300        05 MOD-KDKVAFG-UT    PIC 9.                                       
003400*                                 FELGRUPP FÖR FELKOD                     
003500        05 MOD-BEKVAFEL-UT-ATTR                                           
003600                             PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800        05 MOD-BEKVAFEL-UT   PIC X(40).                                   
003900*                                 KVALITET FELKODSBETECKNING              
004000        05 MOD-BEKVAFGR-UT-ATTR                                           
004100                             PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300        05 MOD-BEKVAFGR-UT   PIC X(20).                                   
004400*                                 KVALITET ALLVARLIGHETSGRAD FELK         
004500*                                 OD                                      
004600        05 MOD-KVKVAFPO-UT-ATTR                                           
004700                             PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900        05 MOD-KVKVAFPO-UT   PIC Z(2)9.                                   
005000*                                 KVALITET POÄNG FÖR FELKOD               
005100     03 MOD-IDKVAFEL-IN-ATTR PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300     03 MOD-IDKVAFEL-IN      PIC X(2).                                    
005400*                                 MFS BEHANDLING AV INPUTFÄLT             
005500     03 MOD-KDKVAFG-IN-ATTR  PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700     03 MOD-KDKVAFG-IN       PIC X(2).                                    
005800*                                 MFS BEHANDLING AV INPUTFÄLT             
005900     03 MOD-BEKVAFEL-IN-ATTR PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100     03 MOD-BEKVAFEL-IN      PIC X(2).                                    
006200*                                 MFS BEHANDLING AV INPUTFÄLT             
006300     03 MOD-BEKVAFGR-IN-ATTR PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500     03 MOD-BEKVAFGR-IN      PIC X(2).                                    
006600*                                 MFS BEHANDLING AV INPUTFÄLT             
006700     03 MOD-KVKVAFPO-IN-ATTR PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900     03 MOD-KVKVAFPO-IN      PIC X(2).                                    
007000*                                 MFS BEHANDLING AV INPUTFÄLT             
007100     03 MOD-IDSKYLT-IN-ATTR  PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300     03 MOD-IDSKYLT-IN       PIC X(2).                                    
007400*                                 MFS BEHANDLING AV INPUTFÄLT             
007500     03 MOD-KDCMD-IN-ATTR    PIC X(2).                                    
007600*                                 MFS ATTRIBUTFÄLT                        
007700     03 MOD-KDCMD-IN         PIC X(2).                                    
007800*                                 MFS BEHANDLING AV INPUTFÄLT             
007900     03 MOD-TEMFSINF         PIC X(61).                                   
008000*                                 INFORMATIONSMEDDELANDE                  
008100*** END COPY W4O11501C0  LENGTH=912                                       
