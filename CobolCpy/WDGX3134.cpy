000100 01  3134-WDGX3134.                                                       
000200*                                 ARTIKELSTATISTIK                        
000300*                                 SÄSONG                                  
000400*                                 FYSISK NYCKEL: WDGXKEY                  
000500*                                 (IDSKURVA + LOW-VALUE)                  
000600     03 3134-IDSKURVA        PIC S9(3)           COMP-3.                  
000700*                                 SÄSONGSKURVA                            
000800*                                 SEASON INDEX NUMBER                     
000900     03 3134-LOW-VALUE       PIC X(13).                                   
001000     03 3134-REFSGSIX        OCCURS 12 TIMES                              
001100                             PIC S9(2)V9(1)      COMP-3.                  
001200*                                 SÄSONGSINDEX FÖRSÄLJNING (%)            
001300*                                 SEASONAL INDEX SALES     (%)            
001400     03 3134-TIUPPDAT        PIC S9(7)           COMP-3.                  
001500*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
001600*                                 UPDATING DATE     (YYMMDD)              
001700     03 FILLER               PIC X(22).                                   
001800*** END COPY WDGX3134C0  LENGTH=65                                        
