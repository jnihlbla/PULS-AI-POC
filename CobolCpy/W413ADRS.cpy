000100 01  ADRS-W413ADRS.                                                       
000200*                                 LÄNKAREA TILL W413ADRS -                
000300*                                 OMVANDL AV LAGOMR + PLATS               
000400     03 ADRS-INDATA.                                                      
000500*                                 INDATA TILL W413ADRS                    
000600        05 ADRS-ADLAGOMR-IN  PIC S9(3)           COMP-3.                  
000700*                                 LAGEROMRÅDE                             
000800*                                 AREA                                    
000900        05 ADRS-ADPLATS-IN   PIC S9(5)           COMP-3.                  
001000*                                 LAGERPLATSNUMMER                        
001100*                                 LOCATION                                
001200        05 ADRS-BEVARREF-IN  PIC X(10).                                   
001300*                                 VÅR REFERENS                            
001400*                                 OUR REFERENCE                           
001500        05 ADRS-FLFORBI-IN   PIC X.                                       
001600*                                 FÖRBIORDERFLAGGA                        
001700*                                                                         
001800        05 ADRS-IDDISTR-IN   PIC S9(5)           COMP-3.                  
001900*                                 DISTRIKTNUMMER                          
002000*                                 DISTRICT NUMBER                         
002100        05 ADRS-KDCALL-IN    PIC S9(3)           COMP-3.                  
002200*                                 ANROPSTYP                               
002300*                                 CALL TYPE                               
002400        05 ADRS-IDDC-IN      PIC X(2).                                    
002500*                                 IDENTIFIERARE LAGER                     
002600*                                 WAREHOUSE IDENTIFIER                    
002700        05 ADRS-KDORDKL-IN   PIC S9              COMP-3.                  
002800*                                 ORDERKLASS                              
002900*                                 ORDER CLASS                             
003000        05 ADRS-KVBEART-Q-IN PIC S9(7)           COMP-3.                  
003100*                                 BESTÄLLT KVANTANPASSAT ANTAL            
003200*                                 ORDERED QUANTITY ADAPTED                
003300*                                  ITEMS                                  
003400        05 ADRS-VLARTNTO-IN  PIC S9(8)V9(1)      COMP-3.                  
003500*                                 ARTIKELVOLYM NETTO (CM3)                
003600*                                 PART NET VOLUME    (CM3)                
003700     03 ADRS-UTDATA.                                                      
003800*                                 UTDATA FRÅN W413ADRS                    
003900        05 ADRS-ADLAGOMR-UT  PIC S9(3)           COMP-3.                  
004000*                                 LAGEROMRÅDE                             
004100*                                 AREA                                    
004200        05 ADRS-ADPLATS-UT   PIC S9(5)           COMP-3.                  
004300*                                 LAGERPLATSNUMMER                        
004400*                                 LOCATION                                
004500*** END OF VILMAII-COPY LENGTH= 38 BYTES                                  
