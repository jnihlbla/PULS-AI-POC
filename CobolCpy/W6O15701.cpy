000100 01  MOD-W6O15701.                                                        
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W6O15701                                
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-BEFT-IN          PIC Z9.                                      
000900*                                 FÖRPACKNINGSTYP                         
001000     03 MOD-KDFPOMR-IN       PIC X(3).                                    
001100*                                 FÖRPACKNINGSOMRÅDE                      
001200     03 MOD-KDFPGRP-IN       PIC X(2).                                    
001300*                                 FÖRPACKNINGSGRUPP                       
001400     03 MOD-BEFT-UT          PIC Z9.                                      
001500*                                 FÖRPACKNINGSTYP                         
001600     03 MOD-KDFPOMR-UT       PIC X(3).                                    
001700*                                 FÖRPACKNINGSOMRÅDE                      
001800     03 MOD-KDFPGRP-UT       PIC X(2).                                    
001900*                                 FÖRPACKNINGSGRUPP                       
002000     03 MOD-RAD              OCCURS 12 TIMES.                             
002100*                                 LINES                                   
002200        05 MOD-KDCMD-ATTR    PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400        05 MOD-KDCMD         PIC X.                                       
002500*                                 RAD-UPPDATERINGSKOMMANDO                
002600*                                  BLANK  = INGENTING                     
002700*                                  D , B  = DELETE                        
002800*                                  R , Ä  = REPLACE                       
002900*                                  I , N  = INSERT                        
003000*                                  S , V  = SELECT                        
003100*                                  P , P  = PRINT                         
003200*                                  C , K  = COPY                          
003300        05 MOD-BEFT-RAD      PIC Z9.                                      
003400*                                 FÖRPACKNINGSTYP                         
003500        05 MOD-FLFPOMR-CDC-RAD                                            
003600                             PIC X.                                       
003700*                                 FÖRPACKNINGSOMRÅDE CDC                  
003800        05 MOD-FLFPOMR-SVS-RAD                                            
003900                             PIC X.                                       
004000*                                 FÖRPACKNINGSOMRÅDE SVS                  
004100        05 MOD-FLFPGRP-ST-RAD                                             
004200                             PIC X.                                       
004300*                                 FÖRPACKNINGSGRUPP: STYCK                
004400        05 MOD-FLFPGRP-28-RAD                                             
004500                             PIC X.                                       
004600*                                 FÖRPACKNINGSGRUPP: 28                   
004700        05 MOD-FLFPGRP-GR-RAD                                             
004800                             PIC X.                                       
004900*                                 FÖRPACKNINGSGRUPP: GROV                 
005000        05 MOD-FLFPGRP-MA-RAD                                             
005100                             PIC X.                                       
005200*                                 FÖRPACKNINGSGRUPP: MASKIN               
005300        05 MOD-FLFPGRP-SA-RAD                                             
005400                             PIC X.                                       
005500*                                 FÖRPACKNINGSGRUPP: SATS                 
005600        05 MOD-REFPCDC-RAD   PIC Z(2).                                    
005700*                                 CDC FÖRPACKN.PROCENT                    
005800        05 MOD-FLSATSIN-RAD  PIC X.                                       
005900*                                 SATSANTAL INKL                          
006000     03 MOD-UPD.                                                          
006100*                                 LINES                                   
006200        05 MOD-BEFT-UPD-ATTR PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400        05 MOD-BEFT-UPD      PIC Z9.                                      
006500*                                 FÖRPACKNINGSTYP                         
006600        05 MOD-FLFPOMR-CDC-UPD-ATTR                                       
006700                             PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900        05 MOD-FLFPOMR-CDC-UPD                                            
007000                             PIC X.                                       
007100*                                 FÖRPACKNINGSOMRÅDE CDC                  
007200        05 MOD-FLFPOMR-SVS-UPD-ATTR                                       
007300                             PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500        05 MOD-FLFPOMR-SVS-UPD                                            
007600                             PIC X.                                       
007700*                                 FÖRPACKNINGSOMRÅDE SVS                  
007800        05 MOD-FLFPGRP-ST-UPD-ATTR                                        
007900                             PIC X(2).                                    
008000*                                 MFS ATTRIBUTFÄLT                        
008100        05 MOD-FLFPGRP-ST-UPD                                             
008200                             PIC X.                                       
008300*                                 FÖRPACKNINGSGRUPP: STYCK                
008400        05 MOD-FLFPGRP-28-UPD-ATTR                                        
008500                             PIC X(2).                                    
008600*                                 MFS ATTRIBUTFÄLT                        
008700        05 MOD-FLFPGRP-28-UPD                                             
008800                             PIC X.                                       
008900*                                 FÖRPACKNINGSGRUPP: 28                   
009000        05 MOD-FLFPGRP-GR-UPD-ATTR                                        
009100                             PIC X(2).                                    
009200*                                 MFS ATTRIBUTFÄLT                        
009300        05 MOD-FLFPGRP-GR-UPD                                             
009400                             PIC X.                                       
009500*                                 FÖRPACKNINGSGRUPP: GROV                 
009600        05 MOD-FLFPGRP-MA-UPD-ATTR                                        
009700                             PIC X(2).                                    
009800*                                 MFS ATTRIBUTFÄLT                        
009900        05 MOD-FLFPGRP-MA-UPD                                             
010000                             PIC X.                                       
010100*                                 FÖRPACKNINGSGRUPP: MASKIN               
010200        05 MOD-FLFPGRP-SA-UPD-ATTR                                        
010300                             PIC X(2).                                    
010400*                                 MFS ATTRIBUTFÄLT                        
010500        05 MOD-FLFPGRP-SA-UPD                                             
010600                             PIC X.                                       
010700*                                 FÖRPACKNINGSGRUPP: SATS                 
010800        05 MOD-REFPCDC-UPD-ATTR                                           
010900                             PIC X(2).                                    
011000*                                 MFS ATTRIBUTFÄLT                        
011100        05 MOD-REFPCDC-UPD   PIC 9(2).                                    
011200*                                 CDC FÖRPACKN.PROCENT                    
011300        05 MOD-FLSATSIN-UPD-ATTR                                          
011400                             PIC X(2).                                    
011500*                                 MFS ATTRIBUTFÄLT                        
011600        05 MOD-FLSATSIN-UPD  PIC X.                                       
011700*                                 SATSANTAL INKL                          
011800     03 MOD-TEMFSINF         PIC X(55).                                   
011900*                                 INFORMATIONSMEDDELANDE                  
012000*** END OF VILMAII-COPY LENGTH= 325 BYTES                                 
