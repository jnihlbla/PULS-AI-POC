000100 01  MID-W4I32401.                                                        
000200*                                 MID-COPYTEXT FÖR W40324                 
000300     03 MID-KDPRCGRP-IN      PIC X(5).                                    
000400*                                 PRODUKTIONSKANALSGRUPP                  
000500     03 MID-KDPRCGRP-UT      PIC X(5).                                    
000600*                                 PRODUKTIONSKANALSGRUPP                  
000700     03 MID-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MID-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MID-IDKUNDNR-IN      PIC X(6).                                    
001200*                                 KUNDNUMMER                              
001300     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MID-IDORDNR-IN       PIC X(5).                                    
001600*                                 ORDERNUMMER                             
001700     03 MID-IDORDNR-UT       PIC X(5).                                    
001800*                                 ORDERNUMMER                             
001900     03 MID-KDORDKL-IN       PIC X.                                       
002000*                                 ORDERKLASS                              
002100     03 MID-KDORDKL-UT       PIC X.                                       
002200*                                 ORDERKLASS                              
002300     03 MID-IDPRODNR-IN      PIC X(7).                                    
002400*                                 PRODUKTIONSNUMMER                       
002500     03 MID-IDPRODNR-UT      PIC X(7).                                    
002600*                                 PRODUKTIONSNUMMER                       
002700     03 MID-IDDC-IN          PIC X(2).                                    
002800*                                 IDENTIFIERARE LAGER                     
002900     03 MID-IDDC-UT          PIC X(2).                                    
003000*                                 IDENTIFIERARE LAGER                     
003100     03 MID-TIRFS-NYCKEL     PIC 9(11).                                   
003200*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
003300     03 MID-IDPRODNR-NYCKEL  PIC 9(7).                                    
003400*                                 PRODUKTIONSNUMMER                       
003500     03 MID-INPUT.                                                        
003600        05 MID-KDCMD         OCCURS 13 TIMES                              
003700                             PIC X.                                       
003800         88 MID-KDCMD-INGENTING                                           
003900                             VALUE ' '.                                   
004000         88 MID-KDCMD-DELETE VALUE 'D'                                    
004100                             'B'.                                         
004200         88 MID-KDCMD-REPLACE                                             
004300                             VALUE 'R'                                    
004400                             'Ä'.                                         
004500         88 MID-KDCMD-INSERT VALUE 'I'                                    
004600                             'N'                                          
004700                             'A'.                                         
004800         88 MID-KDCMD-SELECT VALUE 'S'                                    
004900                             'V'.                                         
005000         88 MID-KDCMD-PRINT  VALUE 'P'                                    
005100                             'P'.                                         
005200         88 MID-KDCMD-COPY   VALUE 'C'                                    
005300                             'K'.                                         
005400*                                 RAD-UPPDATERINGSKOMMANDO                
005500*                                  BLANK  = INGENTING                     
005600*                                  D , B  = DELETE                        
005700*                                  R , Ä  = REPLACE                       
005800*                                  I,N,A  = INSERT                        
005900*                                  S , V  = SELECT                        
006000*                                  P , P  = PRINT                         
006100*                                  C , K  = COPY                          
006200*** END OF VILMAII-COPY LENGTH= 91 BYTES                                  
