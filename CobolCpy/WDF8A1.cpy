000100 01  SEQA-WDF8A1.                                                         
000200*                                 STYRNING SPÄRREGLER                     
000300*                                 SEKUNDÄRT INDEX TILL WDF801,            
000400*                                 DIST/KUND INGÅNG VIA WDF811             
000500*                                 FYSISK NYCKEL: WDF8A1KY                 
000600*                                 (IDDISTR-FOM + IDDISTR-TOM +            
000700*                                  IDKUNDNR-FOM + IDKUNDNR-TOM +          
000800*                                  IDSPRGRP)                              
000900*                                 SECONDARY NYCKEL: WDF8ASEQ              
001000*                                 (IDDISTR-FOM + IDDISTR-TOM +            
001100*                                  IDKUNDNR-FOM + IDKUNDNR-TOM)           
001200*                                                                         
001300     03 SEQA-IDDISTR-FOM     PIC S9(5)           COMP-3.                  
001400*                                 LÄGSTA DISTRIKTNR I INTERVALL           
001500     03 SEQA-IDDISTR-TOM     PIC S9(5)           COMP-3.                  
001600*                                 HÖGSTA DISTRIKTNR I INTERVALL           
001700     03 SEQA-IDKUNDNR-FOM    PIC S9(7)           COMP-3.                  
001800*                                 LÄGSTA KUNDNUMMER I INTERVALL           
001900     03 SEQA-IDKUNDNR-TOM    PIC S9(7)           COMP-3.                  
002000*                                 HÖGSTA KUNDNUMMER I INTERVALL           
002100     03 SEQA-IDSPRGRP        PIC X(10).                                   
002200*                                 SPÄRRADE GRUPPER                        
002300     03 SEQA-TISTADAT-KL0    PIC S9(7)           COMP-3.                  
002400*                                 STARTDATUM FÖR SPÄRR ORDERKL 0          
002500     03 SEQA-TISTADAT-KL1    PIC S9(7)           COMP-3.                  
002600*                                 STARTDATUM FÖR SPÄRR ORDERKL 1          
002700     03 SEQA-TISTADAT-KL2    PIC S9(7)           COMP-3.                  
002800*                                 STARTDATUM FÖR SPÄRR ORDERKL 2          
002900     03 SEQA-TISTADAT-KL3    PIC S9(7)           COMP-3.                  
003000*                                 STARTDATUM FÖR SPÄRR ORDERKL 3          
003100     03 SEQA-TISTADAT-KL4    PIC S9(7)           COMP-3.                  
003200*                                 STARTDATUM FÖR SPÄRR ORDERKL 4          
003300*** END OF VILMAII-COPY LENGTH= 44 BYTES                                  
