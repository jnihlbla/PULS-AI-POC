000100 01  4516-WDGX4516-CTX.                                                   
000200*                                 FARLIGT GODS                            
000300*                                 UTSKRIFTS REGISTER                      
000400*                                 FYSISK NYCKEL: WDGXKEY                  
000500*                                 (IDDC + IDSKEPPN+ IDDISTR               
000600*                                 + IDKUNDNR)                             
000700     03 4516-IDDC            PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 4516-IDSKEPPN        PIC S9(7)           COMP-3.                  
001100*                                 SKEPPNINGSNUMMER                        
001200*                                 SHIPMENT NO                             
001300     03 4516-IDDISTR         PIC S9(5)           COMP-3.                  
001400*                                 DISTRIKTNUMMER                          
001500*                                 DISTRICT NUMBER                         
001600     03 4516-IDKUNDNR        PIC S9(7)           COMP-3.                  
001700*                                 KUNDNUMMER                              
001800*                                 CUSTOMER NO                             
001900     03 4516-IDPRODNR        PIC S9(7)           COMP-3.                  
002000*                                 PRODUKTIONSNUMMER                       
002100*                                 PRODUCTION NUMBER                       
002200     03 4516-IDKOLLI         PIC S9(5)           COMP-3.                  
002300*                                 KOLLINUMMER                             
002400*                                 CASE NUMBER                             
002500     03 4516-IDKUNDRF        PIC X(10).                                   
002600*                                 KUNDENS REFERENS (ORDERID)              
002700*                                 CUSTOMER REFERENCE (ORDER ID)           
002800*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  
