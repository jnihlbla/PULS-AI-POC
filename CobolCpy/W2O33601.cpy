000100 01  MOD-W2O33601.                                                        
000200*                                 MOD-COPYTEXT FÖR W20336                 
000300*                                 PRIME COUNT SELECTION                   
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDSPRGRP-IN      PIC X(10).                                   
000900*                                 SPÄRRADE GRUPPER                        
001000     03 MOD-IDSPRGRP-UT      PIC X(10).                                   
001100*                                 SPÄRRADE GRUPPER                        
001200     03 MOD-TABELLRAD        OCCURS 6 TIMES.                              
001300*                                 GRUPP MED TABELL RADER                  
001400        05 MOD-KDCMD-ATTR    PIC X(2).                                    
001500*                                 MFS ATTRIBUTFÄLT                        
001600        05 MOD-KDCMD         PIC X.                                       
001700*                                 RAD-UPPDATERINGSKOMMANDO                
001800*                                  BLANK  = INGENTING                     
001900*                                  D , B  = DELETE                        
002000*                                  R , Ä  = REPLACE                       
002100*                                  I,N,A  = INSERT                        
002200*                                  S , V  = SELECT                        
002300*                                  P , P  = PRINT                         
002400*                                  C , K  = COPY                          
002500        05 MOD-IDSPRGRP-ATTR PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700        05 MOD-IDSPRGRP      PIC X(10).                                   
002800*                                 SPÄRRADE GRUPPER                        
002900        05 MOD-FLAUTUPD      PIC X.                                       
003000*                                 AUT. SPÄRR PER REGEL/ARTIKEL            
003100        05 MOD-TISTADAT-ATTR PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300        05 MOD-TISTADAT      PIC X(6).                                    
003400*                                 GENERELLT STARTDATUM                    
003500        05 MOD-KDMARKBLK     PIC X(3).                                    
003600*                                 MARKNADSSPÄRR                           
003700        05 MOD-TENOTE        PIC X(40).                                   
003800*                                 NOTERINGSFÄLT                           
003900        05 MOD-IDUSER-CR     PIC X(10).                                   
004000        05 MOD-TENOTE-60     PIC X(60).                                   
004100*                                 NOTERINGSFÄLT                           
004200     03 MOD-CMD-E-ATTR       PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 MOD-CMD-E            PIC X.                                       
004500     03 MOD-IDSPRGRP-E-ATTR  PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-IDSPRGRP-E       PIC X(10).                                   
004800*                                 SPÄRRADE GRUPPER                        
004900     03 MOD-FLAUTUPD-E-ATTR  PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100     03 MOD-FLAUTUPD-E       PIC X.                                       
005200*                                 AUT. SPÄRR PER REGEL/ARTIKEL            
005300     03 MOD-TISTADAT-E-ATTR  PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500     03 MOD-TISTADAT-E       PIC 9(6).                                    
005600*                                 GENERELLT STARTDATUM                    
005700     03 MOD-KDMARKBLK-E-ATTR PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900     03 MOD-KDMARKBLK-E      PIC X(3).                                    
006000*                                 MARKNADSSPÄRR                           
006100     03 MOD-TENOTE-E-ATTR    PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300     03 MOD-TENOTE-E         PIC X(40).                                   
006400*                                 NOTERINGSFÄLT                           
006500     03 MOD-TENOTE-60-E-ATTR PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700     03 MOD-TENOTE-60-E      PIC X(60).                                   
006800*                                 NOTERINGSFÄLT                           
006900     03 MOD-TEMFSINF         PIC X(55).                                   
007000*                                 INFORMATIONSMEDDELANDE                  
007100*** END OF VILMAII-COPY LENGTH= 1076 BYTES                                
