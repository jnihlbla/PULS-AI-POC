000010*** EDIT ALLOWED                                                          
000010***  OBS  SE DATA MANAGER VILKA PGM SOM BEHÖVER KOMPILERAS OM             
000020***  OBS  ÄVEN W20103, W20163 DÄR LIKNANDE LIGGER HÅRDKODAT               
000030***       (SÖK PERIODINDELNING)                                           
000100                                                                          
000200 01  PER-TAB.                                                             
000300     03  GRUPP.                                                           
000400      05 FILLER                  PIC 9(2)   VALUE 1.                      
000500      05 FILLER                  PIC 9(2)   VALUE 1.                      
000600      05 FILLER                  PIC 9(2)   VALUE 6.                      
000700      05 FILLER                  PIC 9(2)   VALUE 6.                      
000800      05 FILLER                  PIC 9(2)   VALUE 2.                      
000900      05 FILLER                  PIC 9(2)   VALUE 7.                      
001000      05 FILLER                  PIC 9(2)   VALUE 12.                     
001100      05 FILLER                  PIC 9(2)   VALUE 6.                      
001200      05 FILLER                  PIC 9(2)   VALUE 3.                      
001300      05 FILLER                  PIC 9(2)   VALUE 13.                     
001400      05 FILLER                  PIC 9(2)   VALUE 18.                     
001500      05 FILLER                  PIC 9(2)   VALUE 6.                      
001600      05 FILLER                  PIC 9(2)   VALUE 4.                      
001700      05 FILLER                  PIC 9(2)   VALUE 19.                     
001800      05 FILLER                  PIC 9(2)   VALUE 24.                     
001900      05 FILLER                  PIC 9(2)   VALUE 6.                      
002000      05 FILLER                  PIC 9(2)   VALUE 5.                      
002100      05 FILLER                  PIC 9(2)   VALUE 25.                     
002200      05 FILLER                  PIC 9(2)   VALUE 34.                     
002300      05 FILLER                  PIC 9(2)   VALUE 10.                     
002400      05 FILLER                  PIC 9(2)   VALUE 6.                      
002500      05 FILLER                  PIC 9(2)   VALUE 35.                     
002600      05 FILLER                  PIC 9(2)   VALUE 40.                     
002700      05 FILLER                  PIC 9(2)   VALUE 6.                      
002800      05 FILLER                  PIC 9(2)   VALUE 7.                      
002900      05 FILLER                  PIC 9(2)   VALUE 41.                     
003000      05 FILLER                  PIC 9(2)   VALUE 46.                     
003100      05 FILLER                  PIC 9(2)   VALUE 6.                      
003200      05 FILLER                  PIC 9(2)   VALUE 8.                      
003300      05 FILLER                  PIC 9(2)   VALUE 47.                     
003400      05 FILLER                  PIC 9(2)   VALUE 52.                     
003500      05 FILLER                  PIC 9(2)   VALUE 6.                      
003600     03  FILLER                  REDEFINES GRUPP.                         
003700      05 FILLER                  OCCURS 8.                                
003800       10 PER-NR                 PIC 9(2).                                
003900       10 PER-VECKA-FOM          PIC 9(2).                                
004000       10 PER-VECKA-TOM          PIC 9(2).                                
004100       10 PER-ANT-VECKOR         PIC 9(2).                                
004200                                                                          
