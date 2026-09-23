000100 01  MOD-W2O11401.                                                        
000200*                                 MOD-COPYTEXT FÖR W2011400               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDLEVNR-START-IN PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900     03 MOD-IDLEVNR-START-UT PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100     03 MOD-INFO-IDLEVNR-ATTR                                             
001200                             PIC X(2).                                    
001300*                                 MFS ATTRIBUTFÄLT                        
001400     03 MOD-INFO-RAD         OCCURS 10 TIMES                              
001500                             INDEXED MOD-INFO-IND.                        
001600*                                 RADINFORMATION                          
001700        05 MOD-INFO-IDLEVNR  PIC X(5).                                    
001800*                                 LEVERANTÖRNUMMER                        
001900        05 MOD-INFO-IDLEVKND PIC X(17).                                   
002000*                                 LEVERANTÖRENS KUNDIDENTITET             
002100        05 MOD-INFO-KDEDI    PIC X.                                       
002200*                                 ÖVERFÖRINGSSTANDARD                     
002300        05 MOD-INFO-FLAVIS   PIC X.                                       
002400*                                 LEVERANTÖRSAVISERING                    
002500        05 MOD-INFO-FLODETTE PIC X.                                       
002600*                                 FAKTURERING SKER VIA ODETTE             
002700     03 MOD-IDLEVNR-IN-ATTR  PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-IDLEVNR-IN       PIC X(2).                                    
003000*                                 MFS BEHANDLING AV INPUTFÄLT             
003100     03 MOD-IDLEVKND-IN-ATTR PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300     03 MOD-IDLEVKND-IN      PIC X(2).                                    
003400*                                 MFS BEHANDLING AV INPUTFÄLT             
003500     03 MOD-KDEDI-IN-ATTR    PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700     03 MOD-KDEDI-IN         PIC X(2).                                    
003800*                                 MFS BEHANDLING AV INPUTFÄLT             
003900     03 MOD-FLAVIS-IN-ATTR   PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 MOD-FLAVIS-IN        PIC X(2).                                    
004200*                                 MFS BEHANDLING AV INPUTFÄLT             
004300     03 MOD-FLODETTE-IN-ATTR PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500     03 MOD-FLODETTE-IN      PIC X(2).                                    
004600*                                 MFS BEHANDLING AV INPUTFÄLT             
004700     03 MOD-KDCMD-IN-ATTR    PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900     03 MOD-KDCMD-IN         PIC X(2).                                    
005000*                                 MFS BEHANDLING AV INPUTFÄLT             
005100     03 MOD-TEMFSINF         PIC X(55).                                   
005200*                                 INFORMATIONSMEDDELANDE                  
005300*** END OF VILMAII-COPY LENGTH= 385 BYTES                                 
