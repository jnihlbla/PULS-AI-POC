000100 01  4128-WDGX4128.                                                       
000200*                                 LEDTIDER LEVERANSANMÄRNINGAR            
000300*                                 OCH RETURER                             
000400*                                 DISTRIKT/KUND INTERVALL                 
000500*                                 FYSISK NYCKEL: KEY4128                  
000600*                                 (IDDISTR-FOM + IDDISTR-TOM +            
000700*                                  IDKUNDNR-FOM + IDKUNDNR-TOM +          
000800*                                  KDANMORS)                              
000900*                                                                         
001000     03 4128-IDDISTR-FOM     PIC S9(5)           COMP-3.                  
001100*                                 LÄGSTA DISTRIKTNR I INTERVALL           
001200*                                 LOWEST DISTRICT NUMBER                  
001300     03 4128-IDDISTR-TOM     PIC S9(5)           COMP-3.                  
001400*                                 HÖGSTA DISTRIKTNR I INTERVALL           
001500*                                 HIGHEST DISTRICT NUMBER                 
001600     03 4128-IDKUNDNR-FOM    PIC S9(7)           COMP-3.                  
001700*                                 LÄGSTA KUNDNUMMER I INTERVALL           
001800*                                 LOWEST CUSTOMER NUMBER                  
001900     03 4128-IDKUNDNR-TOM    PIC S9(7)           COMP-3.                  
002000*                                 HÖGSTA KUNDNUMMER I INTERVALL           
002100*                                 HIGHEST CUSTOMER NUMBER                 
002200     03 4128-KDANMORS        PIC X(2).                                    
002300*                                 ORSAK TILL LEVERANSANMÄRKNING           
002400*                                 DISCREPANCY REPORT REASON CODE          
002500     03 4128-KVDAGAR-LTRP    PIC S9(3)           COMP-3.                  
002600*                                 DAGAR FÖR TRANSP. AV LEV.ANM.           
002700     03 4128-KVDAGAR-LEVANM  PIC S9(3)           COMP-3.                  
002800*                                 ANT. DAGAR MAN KAN LEV.ANM.             
002900     03 4128-KVDAGAR-LTRP-LDC                                             
003000                             PIC S9(3)           COMP-3.                  
003100*                                 DAGAR FÖR TRP. AV LEV.ANM. LDC          
003200     03 4128-KVDAGAR-LEVANM-LDC                                           
003300                             PIC S9(3)           COMP-3.                  
003400*                                 ANT. DAGAR MAN KAN LEV.ANM. LDC         
003500     03 4128-KVDAGAR-RET     PIC S9(3)           COMP-3.                  
003600*                                 NO. OF DAYS RETURN HANDLING FOR         
003700*                                 A DESCRAPENCY REPORT                    
003800     03 4128-KVDAGAR-RTRP    PIC S9(3)           COMP-3.                  
003900*                                 DAGAR FÖR TRANSP. AV RETURER            
004000     03 4128-KVDAGAR-RET-LDC PIC S9(3)           COMP-3.                  
004100*                                 ANTAL DAGAR FÖR RETURHANT. LDC          
004200     03 4128-KVDAGAR-RTRP-LDC                                             
004300                             PIC S9(3)           COMP-3.                  
004400*                                 DAGAR FÖR TRP. AV LDC RETURER           
004500     03 4128-FILLER          PIC X(7).                                    
004600*** END OF VILMAII-COPY LENGTH= 39 BYTES                                  
