000100 01  MOD-W2O14401.                                                        
000200*                                 MOD-COPYTEXT FÖR W2014400               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-KDPRODSL-IN      PIC Z9.                                      
000800*                                 PRODUKTSLAG                             
000900     03 MOD-KDPRISKL-IN      PIC X.                                       
001000*                                 PRISKLASS                               
001100     03 MOD-KDFREKKL-IN      PIC X.                                       
001200*                                 FREKVENSKLASS                           
001300     03 MOD-KDPRODSL-UT      PIC Z9.                                      
001400*                                 PRODUKTSLAG                             
001500     03 MOD-KDPRISKL-UT      PIC X.                                       
001600*                                 PRISKLASS                               
001700     03 MOD-KDFREKKL-UT      PIC X.                                       
001800*                                 FREKVENSKLASS                           
001900     03 MOD-KVVECKOR-MINSL-UT                                             
002000                             PIC X(4).                                    
002100*                                 MINGRÄNS SÄKERHETSLAGER                 
002200     03 MOD-KVVECKOR-MINSL-IN-ATTR                                        
002300                             PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500     03 MOD-KVVECKOR-MINSL-IN                                             
002600                             PIC X(4).                                    
002700*                                 MINGRÄNS SÄKERHETSLAGER                 
002800     03 MOD-KVVECKOR-MAXSL-UT                                             
002900                             PIC X(4).                                    
003000*                                 MAXGRÄNS SÄKERHETSLAGER                 
003100     03 MOD-KVVECKOR-MAXSL-IN-ATTR                                        
003200                             PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 MOD-KVVECKOR-MAXSL-IN                                             
003500                             PIC X(4).                                    
003600*                                 MAXGRÄNS SÄKERHETSLAGER                 
003700     03 MOD-RETARGET-UT      PIC X(4).                                    
003800*                                 SERVICEGRADSMÅL                         
003900     03 MOD-RETARGET-IN-ATTR PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 MOD-RETARGET-IN      PIC X(4).                                    
004200*                                 SERVICEGRADSMÅL                         
004300     03 MOD-REOLAGK-UT       PIC 9(2).                                    
004400*                                 PROCENT ÖVERLAGERKOSTNAD                
004500     03 MOD-REOLAGK-IN-ATTR  PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-REOLAGK-IN       PIC 9(2).                                    
004800*                                 PROCENT ÖVERLAGERKOSTNAD                
004900     03 MOD-KFAKT-UT         PIC X(4).                                    
005000*                                 KONST FÖR SÄK.LAG.                      
005100     03 MOD-KFAKT-IN-ATTR    PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300     03 MOD-KFAKT-IN         PIC X(4).                                    
005400*                                 KONST FÖR SÄK.LAG.                      
005500     03 MOD-TEMFSINF         PIC X(55).                                   
005600*                                 INFORMATIONSMEDDELANDE                  
005700*** END OF VILMAII-COPY LENGTH= 153 BYTES                                 
