000100 01  4114-WDGX4114.                                                       
000200*                                 LEVERANSANMÄRKNING/RETUR                
000300*                                 ANSVARIG ADMINISTRATION                 
000400*                                 FYSISK NYCKEL: KEY4114                  
000500*                                 (KDANMORS + IDLOPNR)                    
000600     03 4114-KDANMORS        PIC X(2).                                    
000700*                                 ORSAK TILL LEVERANSANMÄRKNING           
000800*                                 DISCREPANCY REPORT REASON CODE          
000900     03 4114-IDLOPNR         PIC S9(3)           COMP-3.                  
001000*                                 LÖPNUMMER                               
001100*                                 SEQUENCE NUMBER                         
001200     03 4114-IDDISTR-FOM     PIC S9(5)           COMP-3.                  
001300*                                 LÄGSTA DISTRIKTNR I INTERVALL           
001400*                                 LOWEST DISTRICT NUMBER                  
001500     03 4114-IDDISTR-TOM     PIC S9(5)           COMP-3.                  
001600*                                 HÖGSTA DISTRIKTNR I INTERVALL           
001700*                                 HIGHEST DISTRICT NUMBER                 
001800     03 4114-IDKUNDNR-FOM    PIC S9(7)           COMP-3.                  
001900*                                 LÄGSTA KUNDNUMMER I INTERVALL           
002000*                                 LOWEST CUSTOMER NUMBER                  
002100     03 4114-IDKUNDNR-TOM    PIC S9(7)           COMP-3.                  
002200*                                 HÖGSTA KUNDNUMMER I INTERVALL           
002300*                                 HIGHEST CUSTOMER NUMBER                 
002400     03 4114-PRARTNTO        PIC S9(7)V9(2)      COMP-3.                  
002500*                                 ARTIKELPRIS NETTO                       
002600*                                 NET PRICE EACH   (FOB NET)              
002700     03 4114-KDARBTYP-ADM    PIC X(8).                                    
002800*                                 ANSVARIG LEVERANSANMÄRKNINGSAVD         
002900*                                 RESPONSIBLE AT DISCREPANCYDEPT          
003000     03 4114-IDPERSON-ADM    PIC S9(3)           COMP-3.                  
003100*                                 PERSONKOD LEVANM                        
003200*                                 STAFF CODE DISCREPANCY                  
003300*** END OF VILMAII-COPY LENGTH= 33 BYTES                                  
