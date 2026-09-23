000100 01  6310-WDGX6310.                                                       
000200*                                 INLÄGGNINGSLISTOR                       
000300*                                 FÖR INBOUND REFILL                      
000400*                                 UPPDATERINGS INFO                       
000500*                                 NYCKEL = IDILIST                        
000600     03 6310-IDILIST         PIC 9(5).                                    
000700*                                 INLÄGGNINGSLISTEIDENTITET               
000800*                                 REPORTINGLIST-IDENTITY                  
000900     03 6310-ADINLOMR-ILI    PIC X(4).                                    
001000*                                 PLACERING INLÄGGNINGS LISTA             
001100*                                 UNLOADING AREA BINNING LIST             
001200     03 6310-IDANSTNR-ILIR   PIC X(5).                                    
001300*                                 ANSTÄLLNINGSNUMMER I-LIST REG           
001400*                                 EMPLOYEE NUMBER REP-LIST REG.           
001500     03 6310-IDANSTNR-ILIU   PIC X(5).                                    
001600*                                 ANSTÄLLNINGSNUMMER I-LIST UPPD          
001700*                                 EMPLOYEE NUMBER REP-LIST UPDATE         
001800     03 6310-IDANSTNR-ILIP   PIC X(5).                                    
001900*                                 ANSTÄLLNINGSNUMMER I-LIST PRINT         
002000*                                 EMPLOYEE NUMBER REP-LIST PRINT          
002100     03 6310-TIREGDAT-ILI    PIC S9(7)           COMP-3.                  
002200*                                 REG.DATUM PÅ INLÄGGNINGSLISTA           
002300*                                 REG.DATE OF REPORTING LIST              
002400     03 6310-TIUPPDAT-ILI    PIC S9(7)           COMP-3.                  
002500*                                 UPPD.DATUM PÅ INLÄGGNINGSLISTA          
002600     03 6310-TIUTSKR         PIC S9(7)           COMP-3.                  
002700*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
002800*                                 PRINTING DATE  (YYMMDD)                 
002900*** END OF VILMAII-COPY LENGTH= 36 BYTES                                  
