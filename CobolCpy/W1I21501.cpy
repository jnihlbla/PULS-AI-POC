000100 01  MID-W1I21501.                                                        
000200*                                 MID-COPYTEXT F÷R W1021500               
000300     03 MID-IDARTNR-STR-IN   PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-STR-UT   PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-KDSTRRAD-ENTER   PIC X.                                       
000800*                                 TYP AV STRUKTURRAD                      
000900     03 MID-IDRADNR-ENTER    PIC 9(5).                                    
001000*                                 RADNUMMER                               
001100     03 MID-KDSTRRAD-NEXT    PIC X.                                       
001200*                                 TYP AV STRUKTURRAD                      
001300     03 MID-IDRADNR-NEXT     PIC 9(5).                                    
001400*                                 RADNUMMER                               
001500     03 MID-FLEXFORP-UT      PIC X.                                       
001600*                                 FLAGGA F÷R EXTERNF÷RPACKNING            
001700     03 MID-FLEXFORP-IN      PIC X.                                       
001800*                                 FLAGGA F÷R EXTERNF÷RPACKNING            
001900     03 MID-UTRAD            OCCURS 3 TIMES.                              
002000*                                                                         
002100        05 MID-IDRADNR       PIC X(5).                                    
002200*                                 RADNUMMER                               
002300        05 MID-IDARTNR-UT    PIC X(9).                                    
002400*                                 ARTIKELNUMMER                           
002500        05 MID-REANTPSA-UT   PIC X(6).                                    
002600*                                 ANTAL PER SATS                          
002700        05 MID-TISTADAT-UT   PIC 9(4).                                    
002800*                                 ≈R - VECKA  (≈≈VV)                      
002900        05 MID-TISTODAT-UT   PIC 9(4).                                    
003000*                                 ≈R - VECKA  (≈≈VV)                      
003100        05 MID-INDATA.                                                    
003200*                                                                         
003300           07 MID-BORT       PIC X.                                       
003400           07 MID-IDARTNR-IN PIC X(9).                                    
003500*                                 ARTIKELNUMMER                           
003600           07 MID-REANTPSA-IN                                             
003700                             PIC X(6).                                    
003800*                                 ANTAL PER SATS                          
003900           07 MID-TISTADAT-IN                                             
004000                             PIC X(4).                                    
004100*                                 ≈R - VECKA  (≈≈VV)                      
004200           07 MID-TISTODAT-IN                                             
004300                             PIC X(4).                                    
004400*                                 ≈R - VECKA  (≈≈VV)                      
004500           07 MID-TESTRNOT-IN                                             
004600                             PIC X(70).                                   
004700*                                 STRUKTURNOTERING                        
004800     03 MID-KLAR             PIC X.                                       
004900*** END COPY W1I21501C0  LENGTH=399                                       
