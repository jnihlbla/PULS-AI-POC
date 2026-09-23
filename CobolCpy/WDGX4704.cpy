000100 01  4704-WDGX4704.                                                       
000200*                                 INLÄGGNINGSLISTOR                       
000300*                                 UPPDATERINGS INFO                       
000400*                                 NYCKEL = IDILIST                        
000500     03 4704-IDILIST         PIC 9(5).                                    
000600*                                 INLÄGGNINGSLISTEIDENTITET               
000700*                                 REPORTINGLIST-IDENTITY                  
000800     03 4704-IDANSTNR-ILIR   PIC S9(5)           COMP-3.                  
000900*                                 ANSTÄLLNINGSNUMMER I-LIST REG           
001000*                                 EMPLOYEE NUMBER REP-LIST REG.           
001100     03 4704-IDANSTNR-ILIU   PIC S9(5)           COMP-3.                  
001200*                                 ANSTÄLLNINGSNUMMER I-LIST UPPD          
001300*                                 EMPLOYEE NUMBER REP-LIST UPDATE         
001400     03 4704-IDANSTNR-ILIP   PIC S9(5)           COMP-3.                  
001500*                                 ANSTÄLLNINGSNUMMER I-LIST PRINT         
001600*                                 EMPLOYEE NUMBER REP-LIST PRINT          
001700     03 4704-TIREGDAT-ILI    PIC S9(7)           COMP-3.                  
001800*                                 REG.DATUM PÅ INLÄGGNINGSLISTA           
001900*                                 REG.DATE OF REPORTING LIST              
002000     03 4704-TIUPPDAT-ILI    PIC S9(7)           COMP-3.                  
002100*                                 UPPD.DATUM PÅ INLÄGGNINGSLISTA          
002200     03 4704-TIUTSKR         PIC S9(7)           COMP-3.                  
002300*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
002400*                                 PRINTING DATE  (YYMMDD)                 
002500*** END OF VILMAII-COPY LENGTH= 26 BYTES                                  
