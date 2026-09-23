000100 01  SEQB-WDA6B1.                                                         
000200*                                 VOR REGISTER                            
000300*                                 FYSISK NYCKEL: WDA6B1KY                 
000400*                                  (IDDISTR+ IDKUNDNR + IDKUNDRF-         
000500*                                 LEV + TIREGDAT-LEV + IDARTNR +          
000600*                                 TIREGTID-LEV + TIREGDAT-AVV +           
000700*                                 TIREGTID-AVV)                           
000800*                                 SEKUNDÄR NYCKEL: WDA6BSEQ               
000900*                                  (IDDISTR+ IDKUNDNR + IDKUNDRF-         
001000*                                 LEV + TIREGDAT-LEV + IDARTNR +          
001100*                                 TIREGTID-LEV + TIREGDAT-AVV +           
001200*                                 TIREGTID-AVV)                           
001300     03 SEQB-IDDISTR         PIC S9(5)           COMP-3.                  
001400*                                 DISTRIKTNUMMER                          
001500*                                 DISTRICT NUMBER                         
001600     03 SEQB-IDKUNDNR        PIC S9(7)           COMP-3.                  
001700*                                 KUNDNUMMER                              
001800*                                 CUSTOMER NO                             
001900     03 SEQB-IDKUNDRF-LEV    PIC X(10).                                   
002000*                                 KUND REF PÅ LEVERANSORDERN              
002100*                                 CUST REF ON DELIVERY ORDER              
002200     03 SEQB-TIREGDAT-LEV    PIC S9(7)           COMP-3.                  
002300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002400*                                 REGISTRATION DATE (YYMMDD)              
002500     03 SEQB-IDARTNR         PIC S9(9)           COMP-3.                  
002600*                                 ARTIKELNUMMER                           
002700*                                 PART NUMBER                             
002800     03 SEQB-TIREGTID-LEV    PIC S9(9)           COMP-3.                  
002900*                                 KLOCKSLAG (TTMMSSTH)                    
003000*                                 TIME OF DAY (HHMMSSTH)                  
003100     03 SEQB-TIREGDAT-AVV    PIC S9(7)           COMP-3.                  
003200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003300*                                 REGISTRATION DATE (YYMMDD)              
003400     03 SEQB-TIREGTID-AVV    PIC S9(9)           COMP-3.                  
003500*                                 KLOCKSLAG (TTMMSSTH)                    
003600*                                 TIME OF DAY (HHMMSSTH)                  
003700*** END OF VILMAII-COPY LENGTH= 40 BYTES                                  
