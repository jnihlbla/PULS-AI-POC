000100 01  MOD-W2O11701.                                                        
000200*                                 MOD-COPYTEXT FÖR W2011700               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDLEVNR-IN       PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900     03 MOD-IDLEVNR-UT       PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100     03 MOD-IDDC-IN          PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 MOD-IDDC-UT          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-INFO-RAD         OCCURS 10 TIMES                              
001600                             INDEXED MOD-INFO-IND.                        
001700*                                 RADINFORMATION                          
001800        05 MOD-INFO-IDLEVNR  PIC X(5).                                    
001900*                                 LEVERANTÖRNUMMER                        
002000        05 MOD-INFO-IDDC     PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200        05 MOD-INFO-IDOVERFNR                                             
002300                             PIC Z(4)9.                                   
002400*                                 ÖVERFÖRINGSNUMMER                       
002500        05 MOD-INFO-KDVECKOSL-ATTR                                        
002600                             PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800        05 MOD-INFO-KDVECKOSL                                             
002900                             PIC X.                                       
003000*                                 KOD    VECKOSLUTSSÄNDNING               
003100        05 MOD-INFO-TISEND-SEN                                            
003200                             PIC 9(6).                                    
003300*                                 SENASTE ÖVERFÖRINGSDATUM                
003400     03 MOD-IDLEVNR-UPD-ATTR PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600     03 MOD-IDLEVNR-UPD      PIC X(2).                                    
003700*                                 MFS BEHANDLING AV INPUTFÄLT             
003800     03 MOD-IDDC-UPD-ATTR    PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-IDDC-UPD         PIC X(2).                                    
004100*                                 MFS BEHANDLING AV INPUTFÄLT             
004200     03 MOD-KDVECKOSL-UPD-ATTR                                            
004300                             PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500     03 MOD-KDVECKOSL-UPD    PIC X(2).                                    
004600*                                 MFS BEHANDLING AV INPUTFÄLT             
004700     03 MOD-TEMFSINF         PIC X(55).                                   
004800*                                 INFORMATIONSMEDDELANDE                  
004900*** END OF VILMAII-COPY LENGTH= 335 BYTES                                 
