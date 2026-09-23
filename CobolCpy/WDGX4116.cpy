000100 01  4116-WDGX4116.                                                       
000200*                                 LEVERANSANMÄRKNING/RETUR                
000300*                                 ANSVARIG RETURER                        
000400*                                 FYSISK NYCKEL: KEY4116                  
000500*                                 (KDANMORS + IDLOPNR)                    
000600     03 4116-KDANMORS        PIC X(2).                                    
000700*                                 ORSAK TILL LEVERANSANMÄRKNING           
000800*                                 DISCREPANCY REPORT REASON CODE          
000900     03 4116-IDLOPNR         PIC S9(3)           COMP-3.                  
001000*                                 LÖPNUMMER                               
001100*                                 SEQUENCE NUMBER                         
001200     03 4116-IDDC            PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400*                                 WAREHOUSE IDENTIFIER                    
001500     03 4116-IDDISTR-FOM     PIC S9(5)           COMP-3.                  
001600*                                 LÄGSTA DISTRIKTNR I INTERVALL           
001700*                                 LOWEST DISTRICT NUMBER                  
001800     03 4116-IDDISTR-TOM     PIC S9(5)           COMP-3.                  
001900*                                 HÖGSTA DISTRIKTNR I INTERVALL           
002000*                                 HIGHEST DISTRICT NUMBER                 
002100     03 4116-IDKUNDNR-FOM    PIC S9(7)           COMP-3.                  
002200*                                 LÄGSTA KUNDNUMMER I INTERVALL           
002300*                                 LOWEST CUSTOMER NUMBER                  
002400     03 4116-IDKUNDNR-TOM    PIC S9(7)           COMP-3.                  
002500*                                 HÖGSTA KUNDNUMMER I INTERVALL           
002600*                                 HIGHEST CUSTOMER NUMBER                 
002700     03 4116-PRARTNTO        PIC S9(7)V9(2)      COMP-3.                  
002800*                                 ARTIKELPRIS NETTO                       
002900*                                 NET PRICE EACH   (FOB NET)              
003000     03 4116-KDARBTYP-RET    PIC X(8).                                    
003100*                                 ANSVARIG RETURAVDELNINGEN               
003200*                                 RESPONSIBLE AT RETURNDEPARTMENT         
003300     03 4116-IDPERSON-RET    PIC S9(3)           COMP-3.                  
003400*                                 PERSONKOD RETURAVD.                     
003500*                                 STAFF CODE RETURN DEPT.                 
003600*** END OF VILMAII-COPY LENGTH= 35 BYTES                                  
