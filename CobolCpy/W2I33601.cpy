000100 01  MID-W2I33601.                                                        
000200*                                 MID-COPYTEXT FÖR W20336                 
000300     03 MID-IDSPRGRP-IN      PIC X(10).                                   
000400*                                 SPÄRRADE GRUPPER                        
000500     03 MID-IDSPRGRP-UT      PIC X(10).                                   
000600*                                 SPÄRRADE GRUPPER                        
000700     03 MID-TABELLRAD        OCCURS 6 TIMES.                              
000800*                                 GRUPP MED TABELL RADER                  
000900        05 MID-KDCMD         PIC X.                                       
001000*                                 RAD-UPPDATERINGSKOMMANDO                
001100*                                  BLANK  = INGENTING                     
001200*                                  D , B  = DELETE                        
001300*                                  R , Ä  = REPLACE                       
001400*                                  I,N,A  = INSERT                        
001500*                                  S , V  = SELECT                        
001600*                                  P , P  = PRINT                         
001700*                                  C , K  = COPY                          
001800        05 MID-IDSPRGRP      PIC X(10).                                   
001900*                                 SPÄRRADE GRUPPER                        
002000     03 MID-CMD-E            PIC X.                                       
002100     03 MID-IDSPRGRP-E       PIC X(10).                                   
002200*                                 SPÄRRADE GRUPPER                        
002300     03 MID-FLAUTUPD-E       PIC X.                                       
002400     03 MID-TISTADAT-E       PIC 9(6).                                    
002500*                                 GENERELLT STARTDATUM                    
002600     03 MID-KDMARKBLK-E      PIC 9(3).                                    
002700*                                 MARKNADSSPÄRR                           
002800     03 MID-TENOTE-E         PIC X(40).                                   
002900*                                 NOTERINGSFÄLT                           
003000     03 MID-TENOTE-60-E      PIC X(60).                                   
003100*                                 NOTERINGSFÄLT                           
003200*** END OF VILMAII-COPY LENGTH= 207 BYTES                                 
