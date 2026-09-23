000100 01  W61158.                                                              
000200*                                 LÄNKAREA FÖR W61158                     
000300*                                 W61158 SKA VARA IDENTISK MED            
000400*                                 W61154                                  
000500*                                                                         
000600     03 INDATAFLT.                                                        
000700        05 IDARTNR           PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900        05 TIAAVVD           PIC S9(5)           COMP-3.                  
001000*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
001100     03 RESULTATFLT.                                                      
001200        05 KVBEHOV-CD        OCCURS 4 TIMES                               
001300                             PIC S9(7)V9(2)      COMP-3.                  
001400*                                 SUMMA BEHOV FÖR ETT CROSS               
001500*                                 DOCKING OMRÅDE                          
001600*                                                                         
001700        05 IDDC              OCCURS 20 TIMES                              
001800                             PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000        05 KVBEHOV-DC        OCCURS 20 TIMES                              
002100                             PIC S9(7)V9(2)      COMP-3.                  
002200*                                 BEHOVSSTORLEK                           
002300*** END OF VILMAII-COPY LENGTH= 168 BYTES                                 
