000100 01  4007-WDGX4007.                                                       
000200*                                 UTSKRIFT AV PLOCKSATSER                 
000300*                                 FYSISK NYCKEL: WDGXKEY                  
000400*                                 (IDHTYP   + IDPRODNR +                  
000500*                                  IDPLKLST + LOWVALUE)                   
000600     03 4007-IDHTYP          PIC X(4).                                    
000700*                                 HÄNDELSETYP                             
000800     03 4007-IDPRODNR        PIC 9(7).                                    
000900*                                 PRODUKTIONSNUMMER                       
001000*                                 PRODUCTION NUMBER                       
001100     03 4007-IDPLKLST        PIC 9(3).                                    
001200*                                 PLOCKLISTNUMMER                         
001300*                                 PICKING LIST NUMBER                     
001400     03 4007-LOWVALUE        PIC X(16).                                   
001500*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  
