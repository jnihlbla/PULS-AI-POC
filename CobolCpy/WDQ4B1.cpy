000100 01  SEQB-WDQ4B1.                                                         
000200*                                 ORDERRADSREGISTER                       
000300*                                 SEKUNDÄRT INDEX TILL WDQ401             
000400*                                 FYSISK NYCKEL: WDQ4B1KY                 
000500*                                 (IDARTNR,  IDLOPNR,  IDGMTREF,          
000600*                                  IDORDER,  IDDC, ADLAGOMR,              
000700*                                  ADGANG,   ADPLATS)                     
000800*                                 SECONDARY NYCKEL: WDQ4BSEQ              
000900*                                 (IDARTNR,  IDLOPNR,  IDGMTREF)          
001000     03 SEQB-IDARTNR         PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER                           
001200*                                 PART NUMBER                             
001300     03 SEQB-IDLOPNR         PIC S9(3)           COMP-3.                  
001400*                                 LÖPNUMMER                               
001500*                                 SEQUENCE NUMBER                         
001600     03 SEQB-IDGMTREF.                                                    
001700*                                 GODSMOTTAGAREREFERENS                   
001800*                                 GOODS RECEIVER REFERENS                 
001900        05 SEQB-IDDISTR      PIC S9(5)           COMP-3.                  
002000*                                 DISTRIKTNUMMER                          
002100*                                 DISTRICT NUMBER                         
002200        05 SEQB-IDKUNDNR     PIC S9(7)           COMP-3.                  
002300*                                 KUNDNUMMER                              
002400*                                 CUSTOMER NO                             
002500        05 SEQB-IDKUNDRF-GRP.                                             
002600*                                 KUNDENS REFERENS (ORDERID)              
002700*                                 CUSTOMER REFERENCE (ORDER ID)           
002800           07 SEQB-IDKUNDRF  PIC X(10).                                   
002900*                                 KUNDENS REFERENS (ORDERID)              
003000*                                 CUSTOMER REFERENCE (ORDER ID)           
003100           07 SEQB-IDORDNR5-FILLER REDEFINES SEQB-IDKUNDRF.               
003200              09 SEQB-IDORDNR5                                            
003300                             PIC 9(5).                                    
003400*                                 ORDERNUMMER                             
003500*                                 ORDER NUMBER                            
003600              09 FILLER      PIC X(5).                                    
003700           07 SEQB-IDORDNR7-FILLER REDEFINES SEQB-IDKUNDRF.               
003800              09 SEQB-IDORDNR7                                            
003900                             PIC 9(7).                                    
004000*                                 ORDERNUMMER                             
004100*                                 ORDER NUMBER                            
004200              09 FILLER      PIC X(3).                                    
004300     03 SEQB-IDORDER         PIC S9(7)           COMP-3.                  
004400*                                 VOLVO PARTS ORDERNUMMER                 
004500*                                 VOLVO PARTS ORDER NUMBER                
004600     03 SEQB-IDDC            PIC X(2).                                    
004700*                                 IDENTIFIERARE LAGER                     
004800*                                 WAREHOUSE IDENTIFIER                    
004900     03 SEQB-ADLAGOMR        PIC S9(3)           COMP-3.                  
005000*                                 LAGEROMRÅDE                             
005100*                                 AREA                                    
005200     03 SEQB-ADGANG          PIC S9(3)           COMP-3.                  
005300*                                 GÅNG                                    
005400*                                 AISLE                                   
005500     03 SEQB-ADPLATS         PIC S9(5)           COMP-3.                  
005600*                                 LAGERPLATSNUMMER                        
005700*                                 LOCATION                                
005800     03 SEQB-KVBEART-Q       PIC S9(7)           COMP-3.                  
005900*                                 BESTÄLLT KVANTANPASSAT ANTAL            
006000*                                 ORDERED QUANTITY ADAPTED                
006100*                                  ITEMS                                  
006200     03 SEQB-IDWDQ401        PIC X(20).                                   
006300*                                 NYCKEL TILL WDQ401                      
006400*                                 KEY TO WDQ401                           
006500*** END OF VILMAII-COPY LENGTH= 61 BYTES                                  
