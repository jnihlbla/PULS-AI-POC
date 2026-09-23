000100 01  W27155.                                                              
000200*                                 BRISTRETURER SDC -> CDC                 
000300*                                                                         
000400*                                 ARTIKLAR MED BRIST I CDC,               
000500*                                 BRISTEN KAN AVHJÄLPAS MED               
000600*                                 BRISTRETUR FRÅN SDC                     
000700     03 IDARTNR              PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 RERF-ART             PIC S9V9(4)         COMP-3.                  
001000*                                 RANSONERINGSFAKTOR ARTIKEL              
001100     03 KVLS                 PIC S9(7)           COMP-3.                  
001200*                                 LAGERSALDO                              
001300     03 KDERS                PIC S9(3)           COMP-3.                  
001400*                                 ERSÄTTNINGSKOD                          
001500     03 BEART-ENG            PIC X(25).                                   
001600*                                 ENGELSK ARTIKELBENÄMNING                
001700     03 RETURBEHOV-CDC       PIC S9(7)           COMP-3.                  
001800*                                 ANTAL ALLMÄNT                           
001900     03 RETURANTAL           OCCURS 5 TIMES.                              
002000*                                 ARTIKLAR PÅ RESP SDC, MÖJLIGA           
002100*                                 FÖR BRISTRETUR TILL CDC                 
002200        05 IDDC              PIC X(2).                                    
002300*                                 IDENTIFIERARE LAGER                     
002400        05 KVANTAL-SDC       PIC S9(7)           COMP-3.                  
002500*                                 ANTAL ALLMÄNT                           
002600*** END OF VILMAII-COPY LENGTH= 73 BYTES                                  
