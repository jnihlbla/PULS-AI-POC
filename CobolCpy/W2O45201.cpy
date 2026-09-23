000100 01  MOD-W2O45201-CTX.                                                    
000200*                                 MOD-COPYTEXT FÖR W2045200               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-KDPRODSL-IN      PIC X(2).                                    
000800*                                 PRODUKTSLAG                             
000900     03 MOD-KDPRODSL-UT      PIC X(2).                                    
001000*                                 PRODUKTSLAG                             
001100     03 MOD-IDLANDX2-IN      PIC X(2).                                    
001200*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001300     03 MOD-IDLANDX2-UT      PIC X(2).                                    
001400*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001500     03 MOD-W2O45201-001-GRP OCCURS 10 TIMES.                             
001600*                                 RADINFORMATION                          
001700        05 MOD-W2O45201-001-001-GRP                                       
001800                             OCCURS 3 TIMES.                              
001900*                                 COLINFORMATION                          
002000           07 MOD-IDFKNGRP-FOM-ATTR                                       
002100                             PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300           07 MOD-IDFKNGRP-FOM                                            
002400                             PIC Z(3)9.                                   
002500*                                 FUNKTIONSGRUPP                          
002600           07 MOD-IDFKNGRP-TOM-ATTR                                       
002700                             PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900           07 MOD-IDFKNGRP-TOM                                            
003000                             PIC Z(3)9.                                   
003100*                                 FUNKTIONSGRUPP                          
003200           07 MOD-IDANSK-ATTR                                             
003300                             PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500           07 MOD-IDANSK     PIC Z(2)9.                                   
003600*                                 ANSKAFFARNUMMER                         
003700     03 MOD-IDFKNGRP-FOM-IN-ATTR                                          
003800                             PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-IDFKNGRP-FOM-IN  PIC Z(3)9.                                   
004100*                                 FUNKTIONSGRUPP                          
004200     03 MOD-IDFKNGRP-TOM-IN-ATTR                                          
004300                             PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500     03 MOD-IDFKNGRP-TOM-IN  PIC Z(3)9.                                   
004600*                                 FUNKTIONSGRUPP                          
004700     03 MOD-IDANSK-IN-ATTR   PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900     03 MOD-IDANSK-IN        PIC Z(2)9.                                   
005000*                                 ANSKAFFARNUMMER                         
005100     03 MOD-KDCMD-IN-ATTR    PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300     03 MOD-KDCMD-IN         PIC X.                                       
005400*                                 RAD-UPPDATERINGSKOMMANDO                
005500*                                  BLANK  = INGENTING                     
005600*                                  D , B  = DELETE                        
005700*                                  R , Ä  = REPLACE                       
005800*                                  I,N,A  = INSERT                        
005900*                                  S , V  = SELECT                        
006000*                                  P , P  = PRINT                         
006100*                                  C , K  = COPY                          
006200     03 MOD-TEMFSINF         PIC X(55).                                   
006300*                                 INFORMATIONSMEDDELANDE                  
006400*** END OF VILMAII-COPY LENGTH= 637 BYTES                                 
