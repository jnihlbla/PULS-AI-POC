000100 01  SEQA-WDM7A1.                                                         
000200*                                 TULLSYSTEM - IDDISTR INGÅNG             
000300*                                 SEKUNDÄRT INDEX TILL WDM701             
000400*                                 FYSISK NYCKEL: WDM7A1KY                 
000500*                                 (IDDISTR,  IDFAKT, IDORDNR7,            
000600*                                  IDKOLLI,  IDPRODNR)                    
000700*                                 SECONDARY NYCKEL: WDM7ASEQ              
000800*                                 (IDDISTR)                               
000900     03 SEQA-IDDISTR         PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100*                                 DISTRICT NUMBER                         
001200     03 SEQA-IDFAKT          PIC S9(7)           COMP-3.                  
001300*                                 FAKTURANUMMER                           
001400*                                 INVOICE NO.                             
001500     03 SEQA-IDORDNR7        PIC S9(7)           COMP-3.                  
001600*                                 ORDERNUMMER                             
001700*                                 ORDER NUMBER                            
001800     03 SEQA-IDKOLLI         PIC S9(5)           COMP-3.                  
001900*                                 KOLLINUMMER                             
002000*                                 CASE NUMBER                             
002100     03 SEQA-IDPRODNR        PIC S9(7)           COMP-3.                  
002200*                                 PRODUKTIONSNUMMER                       
002300*                                 PRODUCTION NUMBER                       
002400     03 SEQA-IDDC            PIC X(2).                                    
002500*                                 IDENTIFIERARE LAGER                     
002600*                                 WAREHOUSE IDENTIFIER                    
002700     03 SEQA-TIFAKT          PIC S9(7)           COMP-3.                  
002800*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
002900*                                 INVOICING DATE   (YYMMDD)               
003000     03 SEQA-KDFAKTYP        PIC X.                                       
003100*                                 FAKTURATYP                              
003200*                                 INVOICE TYPE                            
003300     03 SEQA-IDWDM701        PIC X(15).                                   
003400*                                 NYCKEL TILL WDM701                      
003500*                                 KEY TO WDM701                           
003600*** END OF VILMAII-COPY LENGTH= 40 BYTES                                  
