000100 01  4118-WDGX4118.                                                       
000200*                                 LEVERANSANMÄRKNING/RETUR                
000300*                                 ANSVARIG REMISS                         
000400*                                 FYSISK NYCKEL: KEY4118                  
000500*                                 (KDANMORS + IDDC + IDLOPNR)             
000600     03 4118-KDANMORS        PIC X(2).                                    
000700*                                 ORSAK TILL LEVERANSANMÄRKNING           
000800*                                 DISCREPANCY REPORT REASON CODE          
000900     03 4118-IDDC            PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100*                                 WAREHOUSE IDENTIFIER                    
001200     03 4118-IDLOPNR         PIC S9(3)           COMP-3.                  
001300*                                 LÖPNUMMER                               
001400*                                 SEQUENCE NUMBER                         
001500     03 4118-IDDISTR-FOM     PIC S9(5)           COMP-3.                  
001600*                                 LÄGSTA DISTRIKTNR I INTERVALL           
001700*                                 LOWEST DISTRICT NUMBER                  
001800     03 4118-IDDISTR-TOM     PIC S9(5)           COMP-3.                  
001900*                                 HÖGSTA DISTRIKTNR I INTERVALL           
002000*                                 HIGHEST DISTRICT NUMBER                 
002100     03 4118-IDKUNDNR-FOM    PIC S9(7)           COMP-3.                  
002200*                                 LÄGSTA KUNDNUMMER I INTERVALL           
002300*                                 LOWEST CUSTOMER NUMBER                  
002400     03 4118-IDKUNDNR-TOM    PIC S9(7)           COMP-3.                  
002500*                                 HÖGSTA KUNDNUMMER I INTERVALL           
002600*                                 HIGHEST CUSTOMER NUMBER                 
002700     03 4118-PRARTNTO        PIC S9(7)V9(2)      COMP-3.                  
002800*                                 ARTIKELPRIS NETTO                       
002900*                                 NET PRICE EACH   (FOB NET)              
003000     03 4118-KDARBTYP-REM    PIC X(8).                                    
003100*                                 REMISSANSVARIG LEVANM                   
003200*                                 PERSON WHO CONSIDERED DISCR.            
003300     03 4118-IDPERSON-REM    PIC S9(3)           COMP-3.                  
003400*                                 PERSONKOD REMISS                        
003500*                                 STAFF CODE CONSIDERATION                
003600     03 4118-ADLAGOMR        PIC S9(3)           COMP-3.                  
003700*                                 LAGEROMRÅDE                             
003800*                                 AREA                                    
003900     03 4118-KDORDKL         PIC S9              COMP-3.                  
004000*                                 ORDERKLASS                              
004100*                                 ORDER CLASS                             
004200*** END OF VILMAII-COPY LENGTH= 38 BYTES                                  
