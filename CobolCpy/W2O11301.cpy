000100 01  MOD-W2O11301.                                                        
000200*                                 MOD-COPYTEXT FÖR W2011300               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDLEVNR-START-IN PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900     03 MOD-IDLEVNR-START-UT PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100     03 MOD-INFO-RAD         OCCURS 10 TIMES                              
001200                             INDEXED MOD-INFO-IND.                        
001300*                                 RADINFORMATION                          
001400        05 MOD-INFO-IDLEVNR  PIC X(5).                                    
001500*                                 LEVERANTÖRNUMMER                        
001600        05 MOD-INFO-IDOVERFNR                                             
001700                             PIC Z(4)9.                                   
001800*                                 ÖVERFÖRINGSNUMMER                       
001900        05 MOD-INFO-KDVECKOSL-ATTR                                        
002000                             PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200        05 MOD-INFO-KDVECKOSL                                             
002300                             PIC X.                                       
002400*                                 KOD    VECKOSLUTSSÄNDNING               
002500        05 MOD-INFO-TISEND-SEN                                            
002600                             PIC 9(6).                                    
002700*                                 SENASTE ÖVERFÖRINGSDATUM                
002800        05 MOD-INFO-TISEND-PER-ATTR                                       
002900                             PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100        05 MOD-INFO-TISEND-PER                                            
003200                             PIC 9(6).                                    
003300*                                 BEGÄRD ÖVERFÖRINGSDATUM                 
003400     03 MOD-IDLEVNR-IN-ATTR  PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600     03 MOD-IDLEVNR-IN       PIC X(2).                                    
003700*                                 MFS BEHANDLING AV INPUTFÄLT             
003800     03 MOD-KDVECKOSL-IN-ATTR                                             
003900                             PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 MOD-KDVECKOSL-IN     PIC X(2).                                    
004200*                                 MFS BEHANDLING AV INPUTFÄLT             
004300     03 MOD-TISEND-PER-IN-ATTR                                            
004400                             PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 MOD-TISEND-PER-IN    PIC X(2).                                    
004700*                                 MFS BEHANDLING AV INPUTFÄLT             
004800     03 MOD-TEMFSINF         PIC X(55).                                   
004900*                                 INFORMATIONSMEDDELANDE                  
005000*** END OF VILMAII-COPY LENGTH= 391 BYTES                                 
