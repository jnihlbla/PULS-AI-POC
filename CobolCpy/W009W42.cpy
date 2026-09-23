000100 01  W009W42.                                                             
000200*                                 ANVÄNDS VID ANROP AV W00942             
000300*                                 ALIAS ANSKGRP                           
000400*                                 IDANSKNR IFYLLES OCH SVAR               
000500*                                 GES I IDGRUPP IDSEKT IDFUNK             
000600*                                 TOTALT-IX SOM ÄR ETT INDEX              
000700*                                 SAMT TEXTER OCH INTERVALLER             
000800*                                                                         
000900*                                 IDANSK > 999 GER 0 I ALLA               
001000*                                 INDEX SAMT BLANKT I TEXTER              
001100*                                                                         
001200     03 IDANSK               PIC S9(3)           COMP-3.                  
001300*                                 ANSKAFFARNUMMER                         
001400     03 IDGRUPP              PIC S9(3)           COMP-3.                  
001500*                                 GRUPPNUMMER                             
001600     03 IDSEKT               PIC S9(3)           COMP-3.                  
001700*                                 SEKTIONSNUMMER                          
001800     03 IDFUNK               PIC S9(3)           COMP-3.                  
001900*                                 FUNKTIONANUMMER                         
002000     03 IDAFFOMR             PIC S9(3)           COMP-3.                  
002100*                                 AFFÄRSOMRÅDE ANSKAFFNING                
002200     03 TOTAL-IX             PIC S9(3)           COMP-3.                  
002300     03 TESEKT               PIC X(8).                                    
002400*                                 SEKTIONSNAMN                            
002500     03 TEFUNK               PIC X(8).                                    
002600*                                 FUNKTIONSNAMN                           
002700     03 TEAFFOMR             PIC X(8).                                    
002800*                                 AFFÄRSOMRÅDESNAMN                       
002900     03 TOTAL-TXT            PIC X(5).                                    
003000     03 GRUPP-INTERVALL      PIC X(7).                                    
003100     03 SEKT-INTERVALL       PIC X(7).                                    
003200     03 FUNK-INTERVALL       PIC X(7).                                    
003300     03 OMR-INTERVALL        PIC X(7).                                    
003400*** END COPY W009W42CC0  LENGTH=69                                        
