000100 01  MOD-W2O10601.                                                        
000200*                                 MOD-COPYTEXT                            
000300*                                 FÖR W2O106                              
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDARTNR-IN       PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 MOD-IDARTNR-UT       PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 MOD-IDLEVNR-IN       PIC X(5).                                    
001300*                                 LEVERANTÖRNUMMER                        
001400     03 MOD-IDLEVNR-UT       PIC X(5).                                    
001500*                                 LEVERANTÖRNUMMER                        
001600     03 MOD-IDLEVNR-SHIP-UT  PIC X(5).                                    
001700*                                 SKEPPANDE LEVERANTÖR                    
001800     03 MOD-DALEVBSK-FOERSTA-DOLD                                         
001900                             PIC 9(8).                                    
002000*                                 LEVERANSBESKED  (ÅÅÅÅMMDD)              
002100     03 MOD-DALEVBSK-NAESTA-DOLD                                          
002200                             PIC 9(8).                                    
002300*                                 LEVERANSBESKED  (ÅÅÅÅMMDD)              
002400     03 MOD-IDLEVNR-FOERSTA-DOLD                                          
002500                             PIC X(5).                                    
002600*                                 LEVERANTÖRNUMMER                        
002700     03 MOD-IDLEVNR-NAESTA-DOLD                                           
002800                             PIC X(5).                                    
002900*                                 LEVERANTÖRNUMMER                        
003000     03 MOD-RAD              OCCURS 8 TIMES.                              
003100        05 MOD-KDCMD-IN-ATTR PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300        05 MOD-KDCMD-IN      PIC X(2).                                    
003400*                                 MFS BEHANDLING AV INPUTFÄLT             
003500        05 MOD-TILEVBSK-AVS-UT-ATTR                                       
003600                             PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800        05 MOD-TILEVBSK-AVS-UT                                            
003900                             PIC 9(5).                                    
004000*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
004100        05 MOD-TILEVBSK-RAD-IN-ATTR                                       
004200                             PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400        05 MOD-TILEVBSK-RAD-IN                                            
004500                             PIC X(2).                                    
004600*                                 MFS BEHANDLING AV INPUTFÄLT             
004700        05 MOD-KVAVIS-UT     PIC Z(6)9.                                   
004800*                                 AVISERAT ANTAL                          
004900        05 MOD-KVAVIS-IN-ATTR                                             
005000                             PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200        05 MOD-KVAVIS-IN     PIC X(2).                                    
005300*                                 MFS BEHANDLING AV INPUTFÄLT             
005400        05 MOD-TILEVBSK-INL-C1-UT                                         
005500                             PIC 9(5).                                    
005600*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
005700        05 MOD-TILEVBSK-INL-C1-IN-ATTR                                    
005800                             PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000        05 MOD-TILEVBSK-INL-C1-IN                                         
006100                             PIC X(2).                                    
006200*                                 MFS BEHANDLING AV INPUTFÄLT             
006300        05 MOD-TILEVBSK-DISP-C1-UT                                        
006400                             PIC 9(5).                                    
006500*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
006600        05 MOD-FLFORAVI-C1-UT                                             
006700                             PIC X.                                       
006800*                                 FÖRAVISERAD INLEVERANS                  
006900        05 MOD-IDLEVNR-RAD-ATTR                                           
007000                             PIC X(2).                                    
007100*                                 MFS BEHANDLING AV INPUTFÄLT             
007200        05 MOD-IDLEVNR-RAD   PIC X(5).                                    
007300*                                 LEVERANTÖRNUMMER                        
007400     03 MOD-RAD-NY.                                                       
007500        05 MOD-TILEVBSK-AVS-NY-ATTR                                       
007600                             PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800        05 MOD-TILEVBSK-AVS-NY                                            
007900                             PIC X(2).                                    
008000*                                 MFS BEHANDLING AV INPUTFÄLT             
008100        05 MOD-KVAVIS-NY-ATTR                                             
008200                             PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400        05 MOD-KVAVIS-NY     PIC X(2).                                    
008500*                                 MFS BEHANDLING AV INPUTFÄLT             
008600        05 MOD-TILEVBSK-INL-C1-NY-ATTR                                    
008700                             PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900        05 MOD-TILEVBSK-INL-C1-NY                                         
009000                             PIC X(2).                                    
009100*                                 MFS BEHANDLING AV INPUTFÄLT             
009200        05 MOD-IDLEVNR-NY-ATTR                                            
009300                             PIC X(2).                                    
009400*                                 MFS ATTRIBUTFÄLT                        
009500        05 MOD-IDLEVNR-NY    PIC X(2).                                    
009600*                                 MFS BEHANDLING AV INPUTFÄLT             
009700        05 MOD-IDLEVNR-SHIP-NY-ATTR                                       
009800                             PIC X(2).                                    
009900*                                 MFS ATTRIBUTFÄLT                        
010000        05 MOD-IDLEVNR-SHIP-NY                                            
010100                             PIC X(2).                                    
010200*                                 MFS BEHANDLING AV INPUTFÄLT             
010300     03 MOD-UNDERDEL.                                                     
010400        05 MOD-TELEVBSK-EXT-ATTR                                          
010500                             PIC X(2).                                    
010600*                                 MFS ATTRIBUTFÄLT                        
010700        05 MOD-TELEVBSK-EXT  PIC X(80).                                   
010800*                                 LEVERANSBESKED FÖR EXTERNT              
010900        05 MOD-TELEVBSK-EXT2-ATTR                                         
011000                             PIC X(2).                                    
011100*                                 MFS ATTRIBUTFÄLT                        
011200        05 MOD-TELEVBSK-EXT2 PIC X(80).                                   
011300*                                 LEVERANSBESKED FÖR EXTERNT              
011400        05 MOD-TIBORT-ATTR   PIC X(2).                                    
011500*                                 MFS ATTRIBUTFÄLT                        
011600        05 MOD-TIBORT        PIC 9(5).                                    
011700*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
011800        05 MOD-TELEVBSK-EXT3-ATTR                                         
011900                             PIC X(2).                                    
012000*                                 MFS ATTRIBUTFÄLT                        
012100        05 MOD-TELEVBSK-EXT3 PIC X(80).                                   
012200*                                 LEVERANSBESKEDSINFORMATION              
012300        05 MOD-TELEVBSK-EXT4-ATTR                                         
012400                             PIC X(2).                                    
012500*                                 MFS ATTRIBUTFÄLT                        
012600        05 MOD-TELEVBSK-EXT4 PIC X(80).                                   
012700*                                 LEVERANSBESKEDSINFORMATION              
012800     03 MOD-TEMFSINF         PIC X(55).                                   
012900*                                 INFORMATIONSMEDDELANDE                  
013000*** END OF VILMAII-COPY LENGTH= 897 BYTES                                 
