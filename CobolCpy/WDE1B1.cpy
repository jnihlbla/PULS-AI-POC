000100 01  SEQB-WDE1B1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDE111             
000300*                                 DISTRIKT                                
000400*                                 FYSISK NYCKEL: WDE1B1KY                 
000500*                                 (IDDC, IDDISTR,TISKEPP9,                
000600*                                  IDKUNDNR, IDSHIPM)                     
000700*                                 SEKUNDÄR NYCKEL: WDE1BSEQ               
000800*                                 (IDDC, IDDISTR,TISKEPP9,                
000900*                                  IDKUNDNR)                              
001000     03 SEQB-IDDC            PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 SEQB-IDDISTR         PIC S9(5)           COMP-3.                  
001400*                                 DISTRIKTNUMMER                          
001500*                                 DISTRICT NUMBER                         
001600     03 SEQB-TISKEPPN-9KOMPL PIC S9(7)           COMP-3.                  
001700*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
001800*                                 9 KOMPLIMENT  9999999 - AAMMDD          
001900*                                 SHIPPING DATE    (YYMMDD)               
002000*                                 9 COMPLIMENT                            
002100     03 SEQB-IDSHIPM         PIC 9(7).                                    
002200*                                 SKEPPNINGSNUMMER                        
002300*                                 SHIPMENT NO                             
002400     03 SEQB-IDKUNDNR        PIC S9(7)           COMP-3.                  
002500*                                 KUNDNUMMER                              
002600*                                 CUSTOMER NO                             
002700*** END OF VILMAII-COPY LENGTH= 20 BYTES                                  
