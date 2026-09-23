000100 01  MOD-W4O32601.                                                        
000200*                                 MODCOPYTEXT TILL W40326.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDDC-IN          PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 MOD-IDDC-UT          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-RADER            OCCURS 39 TIMES.                             
001600*                                 RADINFORMATION                          
001700        05 MOD-KDCMD-ATTR    PIC X(2).                                    
001800*                                 MFS ATTRIBUTFÄLT                        
001900        05 MOD-KDCMD         PIC X.                                       
002000*                                 RAD-UPPDATERINGSKOMMANDO                
002100*                                  BLANK  = INGENTING                     
002200*                                  D , B  = DELETE                        
002300*                                  R , Ä  = REPLACE                       
002400*                                  I , N  = INSERT                        
002500        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
002600*                                 KUNDNUMMER                              
002700        05 MOD-IDPRT         PIC X(3).                                    
002800*                                 LOGISK PRINTERIDENTITET                 
002900     03 MOD-IDKUNDNR-IN-ATTR PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100     03 MOD-IDKUNDNR-IN      PIC X(2).                                    
003200*                                 MFS BEHANDLING AV INPUTFÄLT             
003300     03 MOD-IDPRT-IN-ATTR    PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500     03 MOD-IDPRT-IN         PIC X(2).                                    
003600*                                 MFS BEHANDLING AV INPUTFÄLT             
003700     03 MOD-TEMFSINF         PIC X(55).                                   
003800*                                 INFORMATIONSMEDDELANDE                  
003900*** END OF VILMAII-COPY LENGTH= 587 BYTES                                 
