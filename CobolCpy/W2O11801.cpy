000100 01  MOD-W2O11801.                                                        
000200*                                 MOD-COPYTEXT FÖR W2011800               
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
001500     03 MOD-INFO-IDLEVNR-ATTR                                             
001600                             PIC X(2).                                    
001700*                                 MFS ATTRIBUTFÄLT                        
001800     03 MOD-INFO-RAD         OCCURS 10 TIMES                              
001900                             INDEXED MOD-INFO-IND.                        
002000*                                 RADINFORMATION                          
002100        05 MOD-INFO-IDLEVNR  PIC X(5).                                    
002200*                                 LEVERANTÖRNUMMER                        
002300        05 MOD-INFO-IDDC     PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500        05 MOD-INFO-IDLEVKND PIC X(17).                                   
002600*                                 LEVERANTÖRENS KUNDIDENTITET             
002700        05 MOD-INFO-KDEDI    PIC X.                                       
002800*                                 ÖVERFÖRINGSSTANDARD                     
002900        05 MOD-INFO-FLAVIS   PIC X.                                       
003000*                                 LEVERANTÖRSAVISERING                    
003100        05 MOD-INFO-FLODETTE PIC X.                                       
003200*                                 FAKTURERING SKER VIA ODETTE             
003300     03 MOD-IDLEVNR-UPD-ATTR PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500     03 MOD-IDLEVNR-UPD      PIC X(2).                                    
003600*                                 MFS BEHANDLING AV INPUTFÄLT             
003700     03 MOD-IDDC-UPD-ATTR    PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900     03 MOD-IDDC-UPD         PIC X(2).                                    
004000*                                 MFS BEHANDLING AV INPUTFÄLT             
004100     03 MOD-IDLEVKND-UPD-ATTR                                             
004200                             PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 MOD-IDLEVKND-UPD     PIC X(2).                                    
004500*                                 MFS BEHANDLING AV INPUTFÄLT             
004600     03 MOD-KDEDI-UPD-ATTR   PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800     03 MOD-KDEDI-UPD        PIC X(2).                                    
004900*                                 MFS BEHANDLING AV INPUTFÄLT             
005000     03 MOD-FLAVIS-UPD-ATTR  PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 MOD-FLAVIS-UPD       PIC X(2).                                    
005300*                                 MFS BEHANDLING AV INPUTFÄLT             
005400     03 MOD-FLODETTE-UPD-ATTR                                             
005500                             PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700     03 MOD-FLODETTE-UPD     PIC X(2).                                    
005800*                                 MFS BEHANDLING AV INPUTFÄLT             
005900     03 MOD-KDCMD-UPD-ATTR   PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100     03 MOD-KDCMD-UPD        PIC X(2).                                    
006200*                                 MFS BEHANDLING AV INPUTFÄLT             
006300     03 MOD-TEMFSINF         PIC X(55).                                   
006400*                                 INFORMATIONSMEDDELANDE                  
006500*** END OF VILMAII-COPY LENGTH= 413 BYTES                                 
