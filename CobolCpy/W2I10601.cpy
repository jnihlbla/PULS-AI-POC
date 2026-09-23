000100 01  MID-W2I10601.                                                        
000200*                                 MID-COPYTEXT                            
000300*                                 F÷R W2I106                              
000400     03 MID-IDARTNR-IN       PIC X(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 MID-IDARTNR-UT       PIC X(9).                                    
000700*                                 ARTIKELNUMMER                           
000800     03 MID-IDLEVNR-IN       PIC X(5).                                    
000900*                                 LEVERANT÷RNUMMER                        
001000     03 MID-IDLEVNR-UT       PIC X(5).                                    
001100*                                 LEVERANT÷RNUMMER                        
001200     03 MID-DALEVBSK-FOERSTA-DOLD                                         
001300                             PIC 9(8).                                    
001400*                                 LEVERANSBESKED  (≈≈≈≈MMDD)              
001500     03 MID-DALEVBSK-NAESTA-DOLD                                          
001600                             PIC 9(8).                                    
001700*                                 LEVERANSBESKED  (≈≈≈≈MMDD)              
001800     03 MID-IDLEVNR-FOERSTA-DOLD                                          
001900                             PIC X(5).                                    
002000*                                 LEVERANT÷RNUMMER                        
002100     03 MID-IDLEVNR-NAESTA-DOLD                                           
002200                             PIC X(5).                                    
002300*                                 LEVERANT÷RNUMMER                        
002400     03 MID-RAD              OCCURS 9 TIMES.                              
002500        05 MID-KDCMD         PIC X.                                       
002600         88 MID-KDCMD-INGENTING                                           
002700                             VALUE ' '.                                   
002800         88 MID-KDCMD-DELETE VALUE 'D'                                    
002900                             'B'.                                         
003000         88 MID-KDCMD-REPLACE                                             
003100                             VALUE 'R'                                    
003200                             'ƒ'.                                         
003300         88 MID-KDCMD-INSERT VALUE 'I'                                    
003400                             'N'                                          
003500                             'A'.                                         
003600         88 MID-KDCMD-SELECT VALUE 'S'                                    
003700                             'V'.                                         
003800         88 MID-KDCMD-PRINT  VALUE 'P'                                    
003900                             'P'.                                         
004000         88 MID-KDCMD-COPY   VALUE 'C'                                    
004100                             'K'.                                         
004200*                                 RAD-UPPDATERINGSKOMMANDO                
004300*                                  BLANK  = INGENTING                     
004400*                                  D , B  = DELETE                        
004500*                                  R , ƒ  = REPLACE                       
004600*                                  I,N,A  = INSERT                        
004700*                                  S , V  = SELECT                        
004800*                                  P , P  = PRINT                         
004900*                                  C , K  = COPY                          
005000        05 MID-TILEVBSK-AVS  PIC X(5).                                    
005100*                                 ≈R - VECKA - DAG   (≈≈VVD)              
005200        05 MID-TILEVBSK-RAD  PIC X(5).                                    
005300*                                 ≈R - VECKA - DAG   (≈≈VVD)              
005400        05 MID-KVAVIS        PIC X(7).                                    
005500*                                 AVISERAT ANTAL                          
005600        05 MID-TILEVBSK-INL-C1                                            
005700                             PIC X(5).                                    
005800*                                 ≈R - VECKA - DAG   (≈≈VVD)              
005900        05 MID-IDLEVNR-RAD   PIC X(5).                                    
006000*                                 LEVERANT÷RNUMMER                        
006100     03 MID-IDLEVNR-SHIP     PIC X(5).                                    
006200*                                 SKEPPANDE LEVERANT÷R                    
006300     03 MID-TELEVBSK-EXT     PIC X(80).                                   
006400*                                 LEVERANSBESKEDSINFORMATION              
006500     03 MID-TIBORT           PIC X(5).                                    
006600*                                 ≈R - VECKA - DAG   (≈≈VVD)              
006700     03 MID-TELEVBSK-EXT2    PIC X(80).                                   
006800*                                 LEVERANSBESKEDSINFORMATION              
006900     03 MID-TELEVBSK-EXT3    PIC X(80).                                   
007000*                                 LEVERANSBESKEDSINFORMATION              
007100     03 MID-TELEVBSK-EXT4    PIC X(80).                                   
007200*                                 LEVERANSBESKEDSINFORMATION              
007300*** END OF VILMAII-COPY LENGTH= 636 BYTES                                 
