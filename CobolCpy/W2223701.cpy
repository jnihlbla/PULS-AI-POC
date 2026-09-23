000100 01  W2223701.                                                            
000200*                                 CALCULATION FORECAST                    
000300*                                                                         
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 IDDC-REF             PIC X(2).                                    
000700*                                 SÄNDANDE LAGER FÖR REFILL               
000800     03 RESEASON-PLAN        OCCURS 12 TIMES                              
000900                             PIC S9V9(2)         COMP-3.                  
001000*                                 SÄSONGSINDEX INKLUSIVE REFILL           
001100     03 KVPB-PLAN            PIC S9(6)V9(1)      COMP-3.                  
001200*                                 PLANERAT PERIODBEHOV                    
001300*** END OF VILMAII-COPY LENGTH= 35 BYTES                                  
