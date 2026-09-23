000100 01  MOD-W4O74801.                                                        
000200*                                 MOD-COPYTEXT FOR PGM W4074800           
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-KDBEHX-IN        PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-KDBEHX-UT        PIC X.                                       
001000*                                 BEHANDLINGSKOD-X                        
001100     03 MOD-IDILIST-IN       PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-IDILIST-UT       PIC X(5).                                    
001400*                                 INLÄGGNINGSLISTEIDENTITET               
001500     03 MOD-INPUT.                                                        
001600*                                                                         
001700        05 MOD-IDPRT-UPD-ATTR                                             
001800                             PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000        05 MOD-IDPRT-UPD     PIC X(2).                                    
002100*                                 MFS BEHANDLING AV INPUTFÄLT             
002200        05 MOD-IDANSTNR-UPD-ATTR                                          
002300                             PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500        05 MOD-IDANSTNR-UPD  PIC X(2).                                    
002600*                                 MFS BEHANDLING AV INPUTFÄLT             
002700     03 MOD-RADER            OCCURS 14 TIMES.                             
002800*                                 RADINFORMATION                          
002900        05 MOD-KDCMD-ATTR    PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100        05 MOD-KDCMD         PIC X(2).                                    
003200*                                 MFS BEHANDLING AV INPUTFÄLT             
003300        05 MOD-IDILIST       PIC 9(5).                                    
003400*                                 INLÄGGNINGSLISTEIDENTITET               
003500        05 MOD-TIREGDAT-ILI  PIC 9(6).                                    
003600*                                 REG.DATUM PÅ INLÄGGNINGSLISTA           
003700        05 MOD-IDANSTNR-ILIR PIC Z(4)9.                                   
003800*                                 ANSTÄLLNINGSNUMMER I-LIST REG           
003900        05 MOD-TIUPPDAT-ILI  PIC 9(6).                                    
004000*                                 UPPD.DATUM PÅ INLÄGGNINGSLISTA          
004100        05 MOD-IDANSTNR-ILIU PIC Z(4)9.                                   
004200*                                 ANSTÄLLNINGSNUMMER I-LIST UPPD          
004300        05 MOD-TIUTSKR-ILI   PIC 9(6).                                    
004400*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
004500        05 MOD-IDANSTNR-RET-PRT                                           
004600                             PIC Z(4)9.                                   
004700*                                 ANSTÄLLNINGSNUMMER                      
004800     03 MOD-TEMFSINF         PIC X(55).                                   
004900*                                 INFORMATIONSMEDDELANDE                  
005000*** END OF VILMAII-COPY LENGTH= 705 BYTES                                 
