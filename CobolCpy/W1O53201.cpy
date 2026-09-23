000100 01  MOD-W1O53201.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 1532              
000300*                                 VADIS / KATALOG - SÖKBEGREPP            
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDCATNR-IN       PIC X(5).                                    
000900*                                 KATALOG-ID                              
001000     03 MOD-IDCATNR-UT       PIC X(5).                                    
001100*                                 KATALOG-ID                              
001200     03 MOD-PARTNER-GRP      OCCURS 6 TIMES.                              
001300        05 MOD-IDPARTGRP-ATTR                                             
001400                             PIC X(2).                                    
001500*                                 MFS ATTRIBUTFÄLT                        
001600        05 MOD-IDPARTGRP     PIC X(6).                                    
001700*                                 PARTNER-GRUPP                           
001800     03 MOD-BECAT-RAD1       PIC X(40).                                   
001900*                                 KATALOGBETECKNING                       
002000*                                 SE ÄVEN BEEMBLEM RESP BEMASTER          
002100     03 MOD-TIREGDAT-KAT     PIC Z(6).                                    
002200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002300     03 MOD-BEMASTER         PIC X(12).                                   
002400*                                 MASTERNAMN FÖR FORDON                   
002500     03 MOD-BEEMBLEM         PIC X(5).                                    
002600*                                 EMBLEM                                  
002700     03 MOD-FLKATVAD-ATTR    PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-FLKATVAD         PIC X(2).                                    
003000*                                 MFS BEHANDLING AV INPUTFÄLT             
003100     03 MOD-KDCATPUB-R       PIC X(3).                                    
003200*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
003300     03 MOD-RAD              OCCURS 8 TIMES.                              
003400        05 MOD-IDMODELL-ATTR PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600        05 MOD-IDMODELL      PIC X(3).                                    
003700*                                 BILENS NUMERISKA MODELLBET.             
003800        05 MOD-TIMODAAR-STA-ATTR                                          
003900                             PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100        05 MOD-TIMODAAR-STA  PIC Z(4).                                    
004200*                                 MODELLÅR (ÅÅÅÅ) STARTÅR                 
004300        05 MOD-TIMODAAR-STO-ATTR                                          
004400                             PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600        05 MOD-TIMODAAR-STO  PIC Z(4).                                    
004700*                                 MODELLÅR (ÅÅÅÅ) STOPPÅR                 
004800        05 MOD-IDVARIANT-ATTR                                             
004900                             PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100        05 MOD-IDVARIANT     PIC X(15).                                   
005200*                                 BILVARIANT                              
005300        05 MOD-IDRADNR       PIC 9(3).                                    
005400*                                 RADNUMMER                               
005500     03 MOD-TEMFSINF         PIC X(55).                                   
005600*                                 INFORMATIONSMEDDELANDE                  
005700*** END OF VILMAII-COPY LENGTH= 523 BYTES                                 
