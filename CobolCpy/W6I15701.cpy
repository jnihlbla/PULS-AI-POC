000100 01  MID-W6I15701.                                                        
000200*                                 COPYTEXT FÖR MID                        
000300*                                 W6I15701                                
000400     03 MID-BEFT-IN          PIC 9(2).                                    
000500*                                 FÖRPACKNINGSTYP                         
000600     03 MID-KDFPOMR-IN       PIC X(3).                                    
000700*                                 FÖRPACKNINGSOMRÅDE                      
000800     03 MID-KDFPGRP-IN       PIC X(2).                                    
000900*                                 FÖRPACKNINGSGRUPP                       
001000     03 MID-BEFT-UT          PIC 9(2).                                    
001100*                                 FÖRPACKNINGSTYP                         
001200     03 MID-KDFPOMR-UT       PIC X(3).                                    
001300*                                 FÖRPACKNINGSOMRÅDE                      
001400     03 MID-KDFPGRP-UT       PIC X(2).                                    
001500*                                 FÖRPACKNINGSGRUPP                       
001600     03 MID-RAD              OCCURS 12 TIMES.                             
001700*                                 LINES                                   
001800        05 MID-KDCMD         PIC X.                                       
001900*                                 RAD-UPPDATERINGSKOMMANDO                
002000*                                  BLANK  = INGENTING                     
002100*                                  D , B  = DELETE                        
002200*                                  R , Ä  = REPLACE                       
002300*                                  I , N  = INSERT                        
002400*                                  S , V  = SELECT                        
002500*                                  P , P  = PRINT                         
002600*                                  C , K  = COPY                          
002700        05 MID-BEFT-RAD      PIC 9(2).                                    
002800*                                 FÖRPACKNINGSTYP                         
002900        05 MID-FLFPOMR-CDC-RAD                                            
003000                             PIC X.                                       
003100*                                 FÖRPACKNINGSOMRÅDE CDC                  
003200        05 MID-FLFPOMR-SVS-RAD                                            
003300                             PIC X.                                       
003400*                                 FÖRPACKNINGSOMRÅDE SVS                  
003500        05 MID-FLFPGRP-ST-RAD                                             
003600                             PIC X.                                       
003700*                                 FÖRPACKNINGSGRUPP: STYCK                
003800        05 MID-FLFPGRP-28-RAD                                             
003900                             PIC X.                                       
004000*                                 FÖRPACKNINGSGRUPP: 28                   
004100        05 MID-FLFPGRP-GR-RAD                                             
004200                             PIC X.                                       
004300*                                 FÖRPACKNINGSGRUPP: GROV                 
004400        05 MID-FLFPGRP-MA-RAD                                             
004500                             PIC X.                                       
004600*                                 FÖRPACKNINGSGRUPP: MASKIN               
004700        05 MID-FLFPGRP-SA-RAD                                             
004800                             PIC X.                                       
004900*                                 FÖRPACKNINGSGRUPP: SATS                 
005000        05 MID-REFPCDC-RAD   PIC 9(2).                                    
005100*                                 CDC FÖRPACKN.PROCENT                    
005200        05 MID-FLSATSIN-RAD  PIC X.                                       
005300*                                 SATSANTAL INKL                          
005400     03 MID-UPD.                                                          
005500*                                 LINES                                   
005600        05 MID-BEFT-UPD      PIC 9(2).                                    
005700*                                 FÖRPACKNINGSTYP                         
005800        05 MID-FLFPOMR-CDC-UPD                                            
005900                             PIC X.                                       
006000*                                 FÖRPACKNINGSOMRÅDE CDC                  
006100        05 MID-FLFPOMR-SVS-UPD                                            
006200                             PIC X.                                       
006300*                                 FÖRPACKNINGSOMRÅDE SVS                  
006400        05 MID-FLFPGRP-ST-UPD                                             
006500                             PIC X.                                       
006600*                                 FÖRPACKNINGSGRUPP: STYCK                
006700        05 MID-FLFPGRP-28-UPD                                             
006800                             PIC X.                                       
006900*                                 FÖRPACKNINGSGRUPP: 28                   
007000        05 MID-FLFPGRP-GR-UPD                                             
007100                             PIC X.                                       
007200*                                 FÖRPACKNINGSGRUPP: GROV                 
007300        05 MID-FLFPGRP-MA-UPD                                             
007400                             PIC X.                                       
007500*                                 FÖRPACKNINGSGRUPP: MASKIN               
007600        05 MID-FLFPGRP-SA-UPD                                             
007700                             PIC X.                                       
007800*                                 FÖRPACKNINGSGRUPP: SATS                 
007900        05 MID-REFPCDC-UPD   PIC 9(2).                                    
008000*                                 CDC FÖRPACKN.PROCENT                    
008100        05 MID-FLSATSIN-UPD  PIC X.                                       
008200*                                 SATSANTAL INKL                          
008300*** END OF VILMAII-COPY LENGTH= 182 BYTES                                 
