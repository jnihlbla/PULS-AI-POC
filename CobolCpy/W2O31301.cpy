000100 01  MOD-W2O31301-CTX.                                                    
000200*                                 MOD-COPYTEXT TILL W2031300              
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDKAMP-IN        PIC X(7).                                    
000800*                                 SERVICEKAMPANJ                          
000900     03 MOD-IDKAMP-UT        PIC X(7).                                    
001000*                                 SERVICEKAMPANJ                          
001100     03 MOD-IDKAMP-GRP-IN    PIC X(7).                                    
001200*                                 ID FÖR KAMPANJGRUPPER                   
001300     03 MOD-IDKAMP-GRP-UT    PIC X(7).                                    
001400*                                 ID FÖR KAMPANJGRUPPER                   
001500     03 MOD-W2O31301-001-GRP OCCURS 7 TIMES.                              
001600        05 MOD-CMD-ATTR      PIC X(2).                                    
001700*                                 MFS ATTRIBUTFÄLT                        
001800        05 MOD-CMD           PIC X.                                       
001900        05 MOD-IDKAMP-RAD    PIC X(7).                                    
002000*                                 SERVICEKAMPANJ                          
002100        05 MOD-KVKAMP-CARS-RAD                                            
002200                             PIC Z(6)9.                                   
002300*                                 ANTAL BILAR I KAMPANJ                   
002400        05 MOD-TISTADAT-KAMP-RAD-ATTR                                     
002500                             PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700        05 MOD-TISTADAT-KAMP-RAD                                          
002800                             PIC 9(6).                                    
002900*                                 STARTDATUM FÖR KAMPANJ                  
003000        05 MOD-TISTODAT-KAMP-RAD-ATTR                                     
003100                             PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300        05 MOD-TISTODAT-KAMP-RAD                                          
003400                             PIC 9(6).                                    
003500*                                 STOPPDATUM FÖR KAMPANJ                  
003600     03 MOD-KDKAMP-RAD       PIC X.                                       
003700*                                                                         
003800     03 MOD-RERESPRT-ATTR    PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-RERESPRT-IN      PIC X(3).                                    
004100*                                                                         
004200     03 MOD-RERESPRT-UT      PIC Z(2)9.                                   
004300*                                                                         
004400     03 MOD-KVKAMP-CARS-TOT  PIC Z(6)9.                                   
004500     03 MOD-NOTE1-ATTR       PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-NOTE1            PIC X(79).                                   
004800     03 MOD-NOTE2-ATTR       PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000     03 MOD-NOTE2            PIC X(79).                                   
005100     03 MOD-KDKAMP-ATTR      PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300     03 MOD-KDKAMP           PIC X.                                       
005400*                                                                         
005500     03 MOD-TISTADAT-KAMP-ATTR                                            
005600                             PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800     03 MOD-TISTADAT-KAMP    PIC 9(6).                                    
005900*                                 STARTDATUM FÖR KAMPANJ                  
006000     03 MOD-TISTODAT-KAMP-ATTR                                            
006100                             PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300     03 MOD-TISTODAT-KAMP    PIC 9(6).                                    
006400*                                 STOPPDATUM FÖR KAMPANJ                  
006500     03 MOD-TEMFSINF         PIC X(55).                                   
006600*                                 INFORMATIONSMEDDELANDE                  
006700*** END OF VILMAII-COPY LENGTH= 555 BYTES                                 
