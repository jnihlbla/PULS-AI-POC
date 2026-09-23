000100 01  SEQA-WDQ5A1.                                                         
000200*                                 ORDERBEKR. FRÅN VIPS                    
000300*                                 SEKUNDÄRT INDEX TILL WDQ501             
000400*                                 DISTR-KUND INGÅNG                       
000500*                                 FYSISK NYCKEL: WDQ5A1KY                 
000600*                                 (IDGMTREF + IDARTNR + IDLOPNR +         
000700*                                 IDSEKVNR+IDDC+KDORDBEK+IDORDER)         
000800*                                 SECONDARY NYCKEL: WDQ5ASEQ              
000900*                                 (IDGMTREF)                              
001000     03 SEQA-IDGMTREF.                                                    
001100*                                 GODSMOTTAGAREREFERENS                   
001200*                                 GOODS RECEIVER REFERENS                 
001300        05 SEQA-IDDISTR      PIC S9(5)           COMP-3.                  
001400*                                 DISTRIKTNUMMER                          
001500*                                 DISTRICT NUMBER                         
001600        05 SEQA-IDKUNDNR     PIC S9(7)           COMP-3.                  
001700*                                 KUNDNUMMER                              
001800*                                 CUSTOMER NO                             
001900        05 SEQA-IDKUNDRF-GRP.                                             
002000*                                 KUNDENS REFERENS (ORDERID)              
002100*                                 CUSTOMER REFERENCE (ORDER ID)           
002200           07 SEQA-IDKUNDRF  PIC X(10).                                   
002300*                                 KUNDENS REFERENS (ORDERID)              
002400*                                 CUSTOMER REFERENCE (ORDER ID)           
002500           07 SEQA-IDORDNR5-FILLER REDEFINES SEQA-IDKUNDRF.               
002600              09 SEQA-IDORDNR5                                            
002700                             PIC 9(5).                                    
002800*                                 ORDERNUMMER                             
002900*                                 ORDER NUMBER                            
003000              09 FILLER      PIC X(5).                                    
003100           07 SEQA-IDORDNR7-FILLER REDEFINES SEQA-IDKUNDRF.               
003200              09 SEQA-IDORDNR7                                            
003300                             PIC 9(7).                                    
003400*                                 ORDERNUMMER                             
003500*                                 ORDER NUMBER                            
003600              09 FILLER      PIC X(3).                                    
003700     03 SEQA-IDARTNR         PIC S9(9)           COMP-3.                  
003800*                                 ARTIKELNUMMER                           
003900*                                 PART NUMBER                             
004000     03 SEQA-IDLOPNR         PIC S9(3)           COMP-3.                  
004100*                                 LÖPNUMMER                               
004200*                                 SEQUENCE NUMBER                         
004300     03 SEQA-IDSEKVNR        PIC S9(3)           COMP-3.                  
004400*                                 GENERELLT SEKVENSNUMMER                 
004500*                                 GENERAL SEQUENCE NUMBER                 
004600     03 SEQA-IDDC            PIC X(2).                                    
004700*                                 IDENTIFIERARE LAGER                     
004800*                                 WAREHOUSE IDENTIFIER                    
004900     03 SEQA-KDORDBEK        PIC 9(2).                                    
005000*                                 ORDERBEKRÄFTELSEKOD                     
005100*                                 ORDERCONFIMATIONCODE                    
005200     03 SEQA-IDORDER         PIC S9(7)           COMP-3.                  
005300*                                 VOLVO PARTS ORDERNUMMER                 
005400*                                 VOLVO PARTS ORDER NUMBER                
005500     03 SEQA-IDWDQ501        PIC X(17).                                   
005600*                                 NYCKEL TILL WDQ501                      
005700*                                 KEY TO WDQ501                           
005800*** END OF VILMAII-COPY LENGTH= 51 BYTES                                  
