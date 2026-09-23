000100 01  W3303103.                                                            
000200*                                 ARTIKELSTATISTIK                        
000300*                                 URVAL SOM GJORTS PÅ ARTIKELNR           
000400*                                 I BILD 3203                             
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
002800     03 MARKNADS-GRP         OCCURS 8 TIMES.                              
002900*                                 GRUPPNIVÅ MARKNADURVALET                
003000        05 KDMARK-BUDG-FOM   PIC S9(3)           COMP-3.                  
003100*                                 MARKNADSKOD BUDGET 96 MARKNADER         
003200        05 KDMARK-BUDG-TOM   PIC S9(3)           COMP-3.                  
003300*                                 MARKNADSKOD BUDGET 96 MARKNADER         
003400     03 DISTRIKT-GRP         OCCURS 4 TIMES.                              
003500*                                 GRUPPNIVÅ DISTRURVALET                  
003600        05 IDDISTR-FOM       PIC S9(5)           COMP-3.                  
003700*                                 LÄGSTA DISTRIKTNR I INTERVALL           
003800        05 IDDISTR-TOM       PIC S9(5)           COMP-3.                  
003900*                                 HÖGSTA DISTRIKTNR I INTERVALL           
004000     03 IDARTNR              OCCURS 500 TIMES                             
004100                             PIC S9(9)           COMP-3.                  
004200*                                 ARTIKELNUMMER                           
004300*** END OF VILMAII-COPY LENGTH= 2608 BYTES                                
