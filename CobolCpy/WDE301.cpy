000100 01  REF-WDE301.                                                          
000200*                                 REFILL REGISTER                         
000300*                                 ORDERFÖRSLAG                            
000400*                                 FYSISK NYCKEL: WDE301KY                 
000500*                                  IDDC + IDPERSON (-BUY) +               
000600*                                  KDREFTYP + IDARTNR + IDDISTR           
000700*                                 SÖKBEGREPP: IDDC, IDPERSON,             
000800*                                 KDREFTYP, IDARTNR, IDDISTR              
000900     03 REF-IDDC             PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100*                                 WAREHOUSE IDENTIFIER                    
001200     03 REF-IDPERSON-BUY     PIC S9(3)           COMP-3.                  
001300*                                 PERSONKOD REFILLANSVARIG                
001400*                                 REFILL RESPONSIBLE ID                   
001500     03 REF-KDREFTYP         PIC X.                                       
001600*                                 TYP AV REFILLORDER                      
001700*                                 TYPE OF REFILLINGORDER                  
001800     03 REF-IDARTNR          PIC S9(9)           COMP-3.                  
001900*                                 ARTIKELNUMMER                           
002000*                                 PART NUMBER                             
002100     03 REF-IDDISTR          PIC S9(5)           COMP-3.                  
002200*                                 DISTRIKTNUMMER                          
002300*                                 DISTRICT NUMBER                         
002400     03 REF-ADART-SDC.                                                    
002500*                                 ARTIKELADRESS I SUPPURT LAGRET          
002600*                                 PARTS-ADRESS IN SUPPORT WH              
002700        05 REF-ADLAGOMR-SDC  PIC S9(3)           COMP-3.                  
002800*                                 LAGEROMRÅDE                             
002900*                                 AREA                                    
003000        05 REF-ADGANG-SDC    PIC S9(3)           COMP-3.                  
003100*                                 GÅNG                                    
003200*                                 AISLE                                   
003300        05 REF-ADPLATS-SDC   PIC S9(5)           COMP-3.                  
003400*                                 LAGERPLATSNUMMER                        
003500*                                 LOCATION                                
003600     03 REF-ADART-CDC.                                                    
003700*                                 ARTIKELADRESS I CDC                     
003800*                                 PARTS-ADRESS IN CDC                     
003900        05 REF-ADLAGOMR-CDC  PIC S9(3)           COMP-3.                  
004000*                                 LAGEROMRÅDE                             
004100*                                 AREA                                    
004200        05 REF-ADGANG-CDC    PIC S9(3)           COMP-3.                  
004300*                                 GÅNG                                    
004400*                                 AISLE                                   
004500        05 REF-ADPLATS-CDC   PIC S9(5)           COMP-3.                  
004600*                                 LAGERPLATSNUMMER                        
004700*                                 LOCATION                                
004800     03 REF-IDKUNDNR         PIC S9(7)           COMP-3.                  
004900*                                 KUNDNUMMER                              
005000*                                 CUSTOMER NO                             
005100     03 REF-IDLEVNR          PIC X(5).                                    
005200*                                 LEVERANTÖRNUMMER                        
005300*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
005400     03 REF-KDFRAKT          PIC S9(3)           COMP-3.                  
005500*                                 FRAKTSÄTT DC TILL KUND                  
005600*                                 FREIGHT CODE                            
005700     03 REF-KDREFORS         PIC X.                                       
005800*                                 REFILL ORDER STATUSKOD                  
005900*                                 REFILL ORDER STATUS CODE                
006000     03 REF-KDREFTXT         PIC 9(2).                                    
006100*                                 KOD FÖR REFILL VARNINGSTEXT             
006200*                                 CODE FOR REFILL WARNINGS                
006300     03 REF-KVBEART          PIC S9(7)           COMP-3.                  
006400*                                 BESTÄLLT ANTAL STYCKEN                  
006500*                                 ORDERED QUANTITY                        
006600     03 REF-KVBEART-CD       PIC S9(7)           COMP-3.                  
006700*                                 BESTÄLLT ANTAL CROSS DOCKING            
006800*                                 ORD. QUANT FOR CROSS DOCKING            
006900     03 REF-ADART-CD.                                                     
007000*                                 ARTIKELADRESS I CD-LAGRET               
007100*                                 PARTS-ADRESS IN CD WAREHOUSE            
007200        05 REF-ADLAGOMR-CD   PIC S9(3)           COMP-3.                  
007300*                                 LAGEROMRÅDE                             
007400*                                 AREA                                    
007500        05 REF-ADGANG-CD     PIC S9(3)           COMP-3.                  
007600*                                 GÅNG                                    
007700*                                 AISLE                                   
007800        05 REF-ADPLATS-CD    PIC S9(5)           COMP-3.                  
007900*                                 LAGERPLATSNUMMER                        
008000*                                 LOCATION                                
008100     03 REF-IDDC-REF         PIC X(2).                                    
008200*                                 SÄNDANDE LAGER FÖR REFILL               
008300*                                 SENDING WAREHOUSE FOR REFILL            
008400     03 REF-FILLER           PIC X(9).                                    
008500*** END OF VILMAII-COPY LENGTH= 67 BYTES                                  
