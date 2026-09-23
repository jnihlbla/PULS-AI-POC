000100 01  W3303503.                                                            
000200*                                 ARTIKELSTATISTIK                        
000300*                                 URVAL SOM GJORTS PÅ ARTIKELNR           
000400*                                 I BILD 3203                             
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
004100*                                 GRUPPNIVÅ DISTRIKTS URVAL               
004200        05 IDDISTR-FOM       PIC S9(5)           COMP-3.                  
004300*                                 LÄGSTA DISTRIKTNR I INTERVALL           
004400        05 IDDISTR-TOM       PIC S9(5)           COMP-3.                  
004500*                                 HÖGSTA DISTRIKTNR I INTERVALL           
004600     03 IDARTNR              OCCURS 500 TIMES                             
004700                             PIC S9(9)           COMP-3.                  
004800*                                 ARTIKELNUMMER                           
004900*** END OF VILMAII-COPY LENGTH= 2621 BYTES                                
