000100 01  MOD-W2O33901.                                                        
000200*                                 MOD-COPYTEXT FÖR W20339                 
000300*                                 PRIME COUNT SELECTION                   
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDSPRGRP-IN      PIC X(10).                                   
000900*                                 SPÄRRADE GRUPPER                        
001000     03 MOD-IDSPRGRP-UT      PIC X(10).                                   
001100*                                 SPÄRRADE GRUPPER                        
001200     03 MOD-IDDISTR-IN       PIC X(4).                                    
001300*                                 DISTRIKTNUMMER                          
001400     03 MOD-IDDISTR-UT       PIC X(4).                                    
001500*                                 DISTRIKTNUMMER                          
001600     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001700*                                 KUNDNUMMER                              
001800     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001900*                                 KUNDNUMMER                              
002000     03 MOD-FLAUTUPD         PIC X(3).                                    
002100     03 MOD-TISTADAT-GRP-ATTR                                             
002200                             PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400     03 MOD-TISTADAT-GRP     PIC 9(6).                                    
002500*                                 GENERELLT STARTDATUM                    
002600     03 MOD-TABELLRAD        OCCURS 11 TIMES.                             
002700*                                 GRUPP MED TABELL RADER                  
002800        05 MOD-CMD-ATTR      PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000        05 MOD-CMD           PIC X.                                       
003100        05 MOD-IDDISTR-FOM-ATTR                                           
003200                             PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400        05 MOD-IDDISTR-FOM   PIC Z(3)9.                                   
003500*                                 DISTRIKTNUMMER                          
003600        05 MOD-IDDISTR-TOM-ATTR                                           
003700                             PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900        05 MOD-IDDISTR-TOM   PIC Z(3)9.                                   
004000*                                 DISTRIKTNUMMER                          
004100        05 MOD-IDKUNDNR-FOM-ATTR                                          
004200                             PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400        05 MOD-IDKUNDNR-FOM  PIC Z(5)9.                                   
004500*                                 KUNDNUMMER                              
004600        05 MOD-IDKUNDNR-TOM-ATTR                                          
004700                             PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900        05 MOD-IDKUNDNR-TOM  PIC Z(5)9.                                   
005000*                                 KUNDNUMMER                              
005100        05 MOD-DATUMGRP      OCCURS 5 TIMES.                              
005200*                                 GRUPP MED DATUM                         
005300           07 MOD-TISTADAT-ATTR                                           
005400                             PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600           07 MOD-TISTADAT   PIC X(6).                                    
005700*                                 GENERELLT STARTDATUM                    
005800     03 MOD-CMD-E-ATTR       PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000     03 MOD-CMD-E            PIC X.                                       
006100     03 MOD-IDDISTR-FOM-E-ATTR                                            
006200                             PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400     03 MOD-IDDISTR-FOM-E    PIC Z(3)9.                                   
006500*                                 DISTRIKTNUMMER                          
006600     03 MOD-IDDISTR-TOM-E-ATTR                                            
006700                             PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900     03 MOD-IDDISTR-TOM-E    PIC Z(3)9.                                   
007000*                                 DISTRIKTNUMMER                          
007100     03 MOD-IDKUNDNR-FOM-E-ATTR                                           
007200                             PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400     03 MOD-IDKUNDNR-FOM-E   PIC Z(5)9.                                   
007500*                                 KUNDNUMMER                              
007600     03 MOD-IDKUNDNR-TOM-E-ATTR                                           
007700                             PIC X(2).                                    
007800*                                 MFS ATTRIBUTFÄLT                        
007900     03 MOD-IDKUNDNR-TOM-E   PIC Z(5)9.                                   
008000*                                 KUNDNUMMER                              
008100     03 MOD-GRUPP            OCCURS 5 TIMES.                              
008200*                                 GRUPP MED INPUTFÄLT                     
008300        05 MOD-TISTADAT-E-ATTR                                            
008400                             PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600        05 MOD-TISTADAT-E    PIC 9(6).                                    
008700*                                 GENERELLT STARTDATUM                    
008800     03 MOD-TEMFSINF         PIC X(55).                                   
008900*                                 INFORMATIONSMEDDELANDE                  
009000*** END OF VILMAII-COPY LENGTH= 1002 BYTES                                
