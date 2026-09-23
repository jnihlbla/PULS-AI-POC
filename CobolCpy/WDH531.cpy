000100 01  SYST-WDH531.                                                         
000200*                                 MOTTAGARE AV EKONOMISK DATA             
000300*                                 FYSISK NYCKEL: WDH531KY                 
000400*                                 IDSYSMOT + IDPTYP + IDSEKVNR            
000500     03 SYST-IDSYSMOT        PIC X(6).                                    
000600*                                 PULS MOTTAGANDE SYSTEMNAMN              
000700*                                 PULS RECEIVING SYSTEM NAME              
000800     03 SYST-IDPTYP          PIC X(3).                                    
000900*                                 POSTTYP                                 
001000*                                 RECORD TYPE                             
001100     03 SYST-IDSEKVNR        PIC S9(3)           COMP-3.                  
001200*                                 GENERELLT SEKVENSNUMMER                 
001300*                                 GENERAL SEQUENCE NUMBER                 
001400     03 SYST-FLALLOC         PIC X.                                       
001500*                                 IND. ALLOCATION FIELD VALUE             
001600*                                 IND. ALLOCATION FIELD VALUE             
001700     03 SYST-FLPRSEGM        PIC X.                                       
001800*                                 IND. PROFITABILITY SEGMENT              
001900*                                 IND. PROFITABILITY SEGMENT              
002000     03 SYST-IDANALYS        PIC X(12).                                   
002100*                                 ANALYSNUMMER                            
002200*                                 ANALYSIS NUMBER                         
002300     03 SYST-IDKONTO         PIC S9(11)          COMP-3.                  
002400*                                 KONTO                                   
002500*                                 ACCOUNT                                 
002600     03 SYST-IDKST           PIC X(10).                                   
002700*                                 KOSTNADSSTÄLLE                          
002800*                                 COST CENTRE                             
002900     03 SYST-IDPRCTR         PIC X(10).                                   
003000*                                 PROFIT CENTER                           
003100*                                 PROFIT CENTER                           
003200     03 SYST-KDANALYS        PIC X.                                       
003300*                                 ANALYSIS CODE                           
003400*                                 ANALYSIS CODE                           
003500     03 SYST-KDDOKTYP        PIC X(2).                                    
003600*                                 DOCUMENT TYPE                           
003700*                                 DOCUMENT TYPE                           
003800     03 SYST-KDPOST          PIC X(2).                                    
003900*                                 POSTING KEY                             
004000*                                 POSTING KEY                             
004100     03 SYST-KDTECKEN        PIC X.                                       
004200*                                 PLUS ELLER MINUS (+ -)                  
004300*                                 PLUS OR MINUS                           
004400     03 SYST-FILLER          PIC X(7).                                    
004500*** END OF VILMAII-COPY LENGTH= 64 BYTES                                  
