000100 01  SPAERR-WDGX4420.                                                     
000200*                                 SPÄRRTEXTER INOM STÄLLAGE/NIVÅ          
000300*                                 NYCKEL: WDGXKEY                         
000400*                                         (ADVMODUL, LOWVALUE)            
000500*                                 SÖKBEGREPP: TESPAERR                    
000600     03 SPAERR-ADVMODUL      PIC S9(3)           COMP-3.                  
000700*                                 VÄNSTER-MODUL                           
000800     03 SPAERR-LOWVALUE      PIC X(8).                                    
000900     03 SPAERR-ADHMODUL      PIC S9(3)           COMP-3.                  
001000*                                 HÖGER-MODUL                             
001100     03 SPAERR-TESPAERR      PIC X(20).                                   
001200*                                 SPÄRRTEXT                               
001300*** END COPY WDGX4420C0  LENGTH=32                                        
