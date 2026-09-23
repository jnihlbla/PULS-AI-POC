000100 01  W55145.                                                              
000200*                                  FIL MED UTDRAG UR WDR2-BASEN           
000300*                                 VALUTAREG.                              
000400     03 TIAA                 PIC S9(3)           COMP-3.                  
000500*                                 ÅR    (ÅÅ)                              
000600     03 KDVALISO             PIC X(3).                                    
000700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
000800     03 REVALUTA             PIC S9(3)           COMP-3.                  
000900*                                 OMRÄKNINGSTAL FÖR VALUTA                
001000     03 PRKURS               PIC S9(6)V9(5)      COMP-3.                  
001100*                                 VALUTAKURS                              
001200*** END COPY W55145      LENGTH=13                                        
