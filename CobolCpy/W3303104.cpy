000100 01  W3303104.                                                            
000200*                                 URVAL SOM GJORTS PÅ TOP                 
000300*                                 SALES ARTIKLAR I BILD                   
000400*                                 3204                                    
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
002500     03 IDKONCNR             OCCURS 8 TIMES                               
002600                             PIC S9(3)           COMP-3.                  
002700*                                 KONCERNNUMMER                           
002800     03 IDLEVNR              OCCURS 8 TIMES                               
002900                             PIC X(5).                                    
003000*                                 LEVERANTÖRNUMMER                        
003100     03 MARKNADS-GRP         OCCURS 4 TIMES.                              
003200*                                 GRUPPNIVÅ MARKNADURVALET                
003300        05 KDMARK-BUDG-FOM   PIC S9(3)           COMP-3.                  
003400*                                 MARKNADSKOD BUDGET 96 MARKNADER         
003500        05 KDMARK-BUDG-TOM   PIC S9(3)           COMP-3.                  
003600*                                 MARKNADSKOD BUDGET 96 MARKNADER         
003700     03 DISTRIKT-GRP         OCCURS 4 TIMES.                              
003800*                                 GRUPPNIVÅ DISTRURVALET                  
003900        05 IDDISTR-FOM       PIC S9(5)           COMP-3.                  
004000*                                 DISTRIKTNUMMER                          
004100        05 IDDISTR-TOM       PIC S9(5)           COMP-3.                  
004200*                                 DISTRIKTNUMMER                          
004300     03 ANSKAFFAR-GRP        OCCURS 4 TIMES.                              
004400*                                 GRUPPNIVÅ ANSKAFFAR URVALET             
004500        05 IDANSK-FOM        PIC S9(3)           COMP-3.                  
004600*                                 ANSKAFFARNUMMER                         
004700        05 IDANSK-TOM        PIC S9(3)           COMP-3.                  
004800*                                 ANSKAFFARNUMMER                         
004900     03 KDPRODSL             OCCURS 4 TIMES                               
005000                             PIC S9(3)           COMP-3.                  
005100*                                 PRODUKTSLAG                             
005200     03 KDSVAR               PIC X.                                       
005300*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
005400     03 KVART                PIC S9(5)           COMP-3.                  
005500*                                 ANTAL ARTIKLAR PER BRYTBEGREPP          
005600     03 IDFKNGRP-FOM         PIC S9(5)           COMP-3.                  
005700*                                 FUNKTIONSGRUPP                          
005800     03 IDFKNGRP-TOM         PIC S9(5)           COMP-3.                  
005900*                                 FUNKTIONSGRUPP                          
006000*** END OF VILMAII-COPY LENGTH= 166 BYTES                                 
