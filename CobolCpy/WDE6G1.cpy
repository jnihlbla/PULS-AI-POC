000100 01  SEQG-WDE6G1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDE601             
000300*                                 FYSISK NYCKEL: WDE6G1KY                 
000400*                                 (IDDC, IDDISTR, DARFS,                  
000500*                                  IDKUNDNR, IDPRODNR)                    
000600*                                 SECONDARY KEY: WDE6GSEQ                 
000700*                                 (IDDC, IDDISTR)                         
000800     03 SEQG-IDDC            PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000*                                 WAREHOUSE IDENTIFIER                    
001100     03 SEQG-IDDISTR         PIC S9(5)           COMP-3.                  
001200*                                 DISTRIKTNUMMER                          
001300*                                 DISTRICT NUMBER                         
001400     03 SEQG-DARFS           PIC 9(12).                                   
001500*                                 KLART FÖR TRANSPORT                     
001600*                                 READY FOR SHIPMENT YYYYMMDDHHMM         
001700     03 SEQG-IDKUNDNR        PIC S9(7)           COMP-3.                  
001800*                                 KUNDNUMMER                              
001900*                                 CUSTOMER NO                             
002000     03 SEQG-IDPRODNR        PIC S9(7)           COMP-3.                  
002100*                                 PRODUKTIONSNUMMER                       
002200*                                 PRODUCTION NUMBER                       
002300*** END OF VILMAII-COPY LENGTH= 25 BYTES                                  
