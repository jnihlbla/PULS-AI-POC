000100 01  SEQB-WDQ1B1.                                                         
000200*                                 ORDERBEKRÄFTELSE REGISTER               
000300*                                 SEKUNDÄRT INDEX TILL WDQ101             
000400*                                 DISTR-KUND INGÅNG                       
000500*                                 FYSISK NYCKEL: WDQ1B1KY                 
000600*                                 (IDDISTR,  IDKUNDNR,                    
000700*                                  TITIORDD-9KOMPL                        
000800*                                  KDFRAKT,  KDORDKL,  IDKUNDRF,          
000900*                                  IDARTNR,  IDLOPNR,  IDSEKVNR,          
001000*                                  IDDC,     IDORDER,  KDORDBEK)          
001100*                                 SECONDARY NYCKEL: WDQ1BSEQ              
001200*                                 (IDDISTR,  IDKUNDNR,                    
001300*                                  TITIORDD-9KOMPL                        
001400*                                  KDFRAKT,  KDORDKL,  IDKUNDRF,          
001500*                                  IDARTNR,  IDLOPNR,  IDSEKVNR,          
001600*                                  IDDC)                                  
001700     03 SEQB-IDDISTR         PIC S9(5)           COMP-3.                  
001800*                                 DISTRIKTNUMMER                          
001900*                                 DISTRICT NUMBER                         
002000     03 SEQB-IDKUNDNR        PIC S9(7)           COMP-3.                  
002100*                                 KUNDNUMMER                              
002200*                                 CUSTOMER NO                             
002300     03 SEQB-TITIORDD-9KOMPL PIC S9(9)           COMP-3.                  
002400*                                 DATUMETS 9-KOMPLEMENT                   
002500*                                 DATES 9-COMPLEMENT                      
002600     03 SEQB-KDFRAKT         PIC S9(3)           COMP-3.                  
002700*                                 FRAKTSÄTT DC TILL KUND                  
002800*                                 FREIGHT CODE                            
002900     03 SEQB-KDORDKL         PIC S9              COMP-3.                  
003000*                                 ORDERKLASS                              
003100*                                 ORDER CLASS                             
003200     03 SEQB-IDKUNDRF.                                                    
003300*                                 KUNDENS REFERENS (ORDERID)              
003400*                                 CUSTOMER REFERENCE (ORDER ID)           
003500        05 SEQB-FILLER       PIC X(10).                                   
003600        05 SEQB-IDORDNR5-FILLER REDEFINES SEQB-FILLER.                    
003700           07 SEQB-IDORDNR5  PIC 9(5).                                    
003800*                                 ORDERNUMMER                             
003900*                                 ORDER NUMBER                            
004000           07 FILLER         PIC X(5).                                    
004100        05 SEQB-IDORDNR7-FILLER REDEFINES SEQB-FILLER.                    
004200           07 SEQB-IDORDNR7  PIC 9(7).                                    
004300*                                 ORDERNUMMER                             
004400*                                 ORDER NUMBER                            
004500           07 FILLER         PIC X(3).                                    
004600     03 SEQB-IDARTNR         PIC S9(9)           COMP-3.                  
004700*                                 ARTIKELNUMMER                           
004800*                                 PART NUMBER                             
004900     03 SEQB-IDLOPNR         PIC S9(3)           COMP-3.                  
005000*                                 LÖPNUMMER                               
005100*                                 SEQUENCE NUMBER                         
005200     03 SEQB-IDSEKVNR        PIC S9(3)           COMP-3.                  
005300*                                 GENERELLT SEKVENSNUMMER                 
005400*                                 GENERAL SEQUENCE NUMBER                 
005500     03 SEQB-IDDC            PIC X(2).                                    
005600*                                 IDENTIFIERARE LAGER                     
005700*                                 WAREHOUSE IDENTIFIER                    
005800     03 SEQB-IDORDER         PIC S9(7)           COMP-3.                  
005900*                                 VOLVO PARTS ORDERNUMMER                 
006000*                                 VOLVO PARTS ORDER NUMBER                
006100     03 SEQB-KDORDBEK        PIC 9(2).                                    
006200*                                 ORDERBEKRÄFTELSEKOD                     
006300*                                 ORDERCONFIMATIONCODE                    
006400     03 SEQB-IDWDQ101        PIC X(17).                                   
006500*                                 NYCKEL TILL WDQ101                      
006600*                                 KEY TO WDQ101                           
006700*** END OF VILMAII-COPY LENGTH= 59 BYTES                                  
