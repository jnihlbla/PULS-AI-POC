000100 01  MID-W90413I1.                                                        
000200*                                 MID-COPYTEXT FÖR W9041300               
000300     03 MID-STRNR-IN         PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-STRNR-UT         PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-FILLER           PIC X(3).                                    
000800     03 MID-FILLER           PIC X(3).                                    
000900     03 MID-IDRADNR-IN       PIC X(4).                                    
001000*                                 RADNUMMER                               
001100     03 MID-IDRADNR-UT       PIC X(4).                                    
001200*                                 RADNUMMER                               
001300     03 MID-IDRADNR-B        PIC 9(4).                                    
001400*                                 RADNUMMER                               
001500     03 MID-INPUT.                                                        
001600*                                                                         
001700        05 MID-FILLER        PIC X(4).                                    
001800        05 MID-FILLER        PIC X(4).                                    
001900        05 MID-UPPDAT.                                                    
002000*                                                                         
002100           07 MID-IDARTNR    PIC X(9).                                    
002200*                                 ARTIKELNUMMER                           
002300           07 MID-FILLER     PIC X(5).                                    
002400           07 MID-FILLER     PIC X(30).                                   
002500           07 MID-FILLER     PIC X(25).                                   
002600           07 MID-FILLER     PIC X.                                       
002700           07 MID-REANTPSA   PIC X(6).                                    
002800*                                 ANTAL PER SATS                          
002900           07 MID-FILLER     PIC X.                                       
003000           07 MID-FILLER     PIC X(2).                                    
003100        05 MID-IDAO          PIC X(10).                                   
003200*                                 ÄNDRINGSORDERNUMMER                     
003300        05 MID-TIAAVV        PIC X(4).                                    
003400*                                 ÅR - VECKA  (ÅÅVV)                      
003500        05 MID-TESTRNOT      OCCURS 2 TIMES                               
003600                             PIC X(70).                                   
003700*                                 STRUKTURNOTERING                        
003800        05 MID-KLAR          PIC X.                                       
003900*                                 ALLMÄN SVARSFLAGGA                      
004000        05 MID-BORT          PIC X.                                       
004100*                                 ALLMÄN SVARSFLAGGA                      
004200*** END OF VILMAII-COPY LENGTH= 279 BYTES                                 
