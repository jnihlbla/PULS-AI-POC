000100 01  W3303502.                                                            
000200*                                 ARTIKELSTATISTIK                        
000300*                                 URVAL SOM GJORTS PÅ GRUPPER             
000400*                                 AV ARTIKLAR I BILD 3202                 
000500*                                 AVSER UNIKA VECKONR                     
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
002500     03 KDPRTYPG             PIC X.                                       
002600*                                 TYP AV PRISTILLÄMPNINGSGRUPP            
002700     03 DAFSGVV-FOM          PIC 9(6).                                    
002800*                                 FÖRSÄLJNINGSVECKA ARTIKEL               
002900     03 DAFSGVV-TOM          PIC 9(6).                                    
003000*                                 FÖRSÄLJNINGSVECKA ARTIKEL               
003100     03 IDKONCNR             OCCURS 8 TIMES                               
003200                             PIC S9(3)           COMP-3.                  
003300*                                 KONCERNNUMMER                           
003400     03 MARKNADS-GRP         OCCURS 8 TIMES.                              
003500*                                 GRUPPNIVÅ MARKNADS URVAL                
003600        05 KDMARK-BUDG-FOM   PIC S9(3)           COMP-3.                  
003700*                                 MARKNADSKOD BUDGET 96 MARKNADER         
003800        05 KDMARK-BUDG-TOM   PIC S9(3)           COMP-3.                  
003900*                                 MARKNADSKOD BUDGET 96 MARKNADER         
004000     03 DISTRIKT-GRP         OCCURS 4 TIMES.                              
004100*                                 GRUPPNIVÅ DISTRIKT URVAL                
004200        05 IDDISTR-FOM       PIC S9(5)           COMP-3.                  
004300*                                 DISTRIKTNUMMER                          
004400        05 IDDISTR-TOM       PIC S9(5)           COMP-3.                  
004500*                                 DISTRIKTNUMMER                          
004600     03 KDNIVA               PIC S9(3)           COMP-3.                  
004700*                                 NIVÅ NUMMER                             
004800     03 KDPRODSL             OCCURS 7 TIMES                               
004900                             PIC S9(3)           COMP-3.                  
005000*                                 PRODUKTSLAG                             
005100     03 KDSVAR               PIC X.                                       
005200*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
005300     03 KDVVKL               OCCURS 5 TIMES                               
005400                             PIC S9              COMP-3.                  
005500*                                 VOLYMVÄRDESKLASS                        
005600     03 IDLKTO               OCCURS 8 TIMES                               
005700                             PIC S9(7)           COMP-3.                  
005800*                                 LAGERKONTO (FFHHHUU)                    
005900     03 IDLEVNR              OCCURS 8 TIMES                               
006000                             PIC X(5).                                    
006100*                                 LEVERANTÖRNUMMER                        
006200     03 ANSKAFFAR-GRP        OCCURS 4 TIMES.                              
006300*                                 GRUPPNIVÅ ANSKAFFAR URVAL               
006400        05 IDANSK-FOM        PIC S9(3)           COMP-3.                  
006500*                                 ANSKAFFARNUMMER                         
006600        05 IDANSK-TOM        PIC S9(3)           COMP-3.                  
006700*                                 ANSKAFFARNUMMER                         
006800     03 FUNKTIONS-GRP        OCCURS 99 TIMES.                             
006900*                                 GRUPPNIVÅ FKNGRP URVAL                  
007000        05 IDFKNGRP-FOM      PIC S9(5)           COMP-3.                  
007100*                                 FUNKTIONSGRUPP                          
007200        05 IDFKNGRP-TOM      PIC S9(5)           COMP-3.                  
007300*                                 FUNKTIONSGRUPP                          
007400*** END OF VILMAII-COPY LENGTH= 825 BYTES                                 
