000100 01  4017-WDGX4017.                                                       
000200*                                 UTSKRIFT AV PLOCKSATSER LDC-GB          
000300*                                 SPARAD TABELL VID OMSTART               
000400*                                 FYSISK NYCKEL: WDGXKEY                  
000500*                                 (IDHTYP   + IDPRODNR +                  
000600*                                  IDPLKLST + LOWVALUE)                   
000700     03 4017-IDHTYP          PIC X(4).                                    
000800*                                 HÄNDELSETYP                             
000900     03 4017-IDPRODNR        PIC 9(7).                                    
001000*                                 PRODUKTIONSNUMMER                       
001100*                                 PRODUCTION NUMBER                       
001200     03 4017-IDPLKLST        PIC 9(3).                                    
001300*                                 PLOCKLISTNUMMER                         
001400*                                 PICKING LIST NUMBER                     
001500     03 4017-LOWVALUE        PIC X(16).                                   
001600*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  
