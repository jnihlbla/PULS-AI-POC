000100 01  W3303102.                                                            
000200*                                 ARTIKELSTATISTIK                        
000300*                                 URVAL SOM GJORTS PÅ GRUPPER             
000400*                                 AV ARTIKLAR I BILD 3202                 
000500*                                 AVSER AF-OMFATTNINGAR                   
000600     03 001-GRUPP.                                                        
000700*                                 ARTIKELSTATISTIK                        
000800*                                 IDENTIFIERING AV URVAL                  
000900*                                 OBS DENNA GRUPP ANVÄNDS I               
001000*                                 FLERA COPYTEXTER                        
001100        05 IDUSER            PIC X(8).                                    
001200*                                 ANVÄNDARENS SÄKERHETS ID                
001300        05 DAREGDAT          PIC 9(8).                                    
001400*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001500        05 TIREGTID          PIC S9(7)           COMP-3.                  
001600*                                 REGISTRERINGSTID                        
001700        05 IDFSGURV          PIC X(8).                                    
001800*                                 URVALS IDENTITET                        
001900        05 IDPTYP            PIC X(3).                                    
002000*                                 POSTTYP                                 
002100        05 IDGTYP            PIC S9              COMP-3.                  
002200*                                 GRUPPTYP                                
002300     03 IDTRANS              PIC X(4).                                    
002400*                                 BILDNUMMER                              
002500     03 KDNIVA               PIC S9(3)           COMP-3.                  
002600*                                 NIVÅ NUMMER                             
002700     03 IDKONCNR             OCCURS 8 TIMES                               
002800                             PIC S9(3)           COMP-3.                  
002900*                                 KONCERNNUMMER                           
003000     03 MARKNADS-GRP         OCCURS 8 TIMES.                              
003100*                                 GRUPPNIVÅ MARKNADS URVALET              
003200        05 KDMARK-BUDG-FOM   PIC S9(3)           COMP-3.                  
003300*                                 MARKNADSKOD BUDGET 96 MARKNADER         
003400        05 KDMARK-BUDG-TOM   PIC S9(3)           COMP-3.                  
003500*                                 MARKNADSKOD BUDGET 96 MARKNADER         
003600     03 DISTRIKT-GRP         OCCURS 4 TIMES.                              
003700*                                 GRUPPNIVÅ DISTRIKTS URVALET             
003800        05 IDDISTR-FOM       PIC S9(5)           COMP-3.                  
003900*                                 DISTRIKTNUMMER                          
004000        05 IDDISTR-TOM       PIC S9(5)           COMP-3.                  
004100*                                 DISTRIKTNUMMER                          
004200     03 KDPRODSL             OCCURS 7 TIMES                               
004300                             PIC S9(3)           COMP-3.                  
004400*                                 PRODUKTSLAG                             
004500     03 KDSVAR               PIC X.                                       
004600*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
004700     03 KDVVKL               OCCURS 5 TIMES                               
004800                             PIC S9              COMP-3.                  
004900*                                 VOLYMVÄRDESKLASS                        
005000     03 IDLKTO               OCCURS 8 TIMES                               
005100                             PIC S9(7)           COMP-3.                  
005200*                                 LAGERKONTO (FFHHHUU)                    
005300     03 IDLEVNR              OCCURS 8 TIMES                               
005400                             PIC X(5).                                    
005500*                                 LEVERANTÖRNUMMER                        
005600     03 ANSKAFFAR-GRP        OCCURS 4 TIMES.                              
005700*                                 GRUPPNIVÅ ANSKAFFAR URVALET             
005800        05 IDANSK-FOM        PIC S9(3)           COMP-3.                  
005900*                                 ANSKAFFARNUMMER                         
006000        05 IDANSK-TOM        PIC S9(3)           COMP-3.                  
006100*                                 ANSKAFFARNUMMER                         
006200     03 FUNKTIONS-GRP        OCCURS 99 TIMES.                             
006300*                                 GRUPPNIVÅ FKNGRP URVALET                
006400        05 IDFKNGRP-FOM      PIC S9(5)           COMP-3.                  
006500*                                 FUNKTIONSGRUPP                          
006600        05 IDFKNGRP-TOM      PIC S9(5)           COMP-3.                  
006700*                                 FUNKTIONSGRUPP                          
006800*** END OF VILMAII-COPY LENGTH= 812 BYTES                                 
