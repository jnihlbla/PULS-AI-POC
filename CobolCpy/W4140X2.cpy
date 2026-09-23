000100 01  0X2-W4140X2.                                                         
000200*                                 ORDERRADSTRANSAKTIONER                  
000300*                                 FÖR SPX, POSTTYP 0X2                    
000400     03 0X2-IDPTYP           PIC X(3).                                    
000500*                                 POSTTYP                                 
000600*                                 RECORD TYPE                             
000700     03 0X2-IDSYSTEM         PIC X(4).                                    
000800*                                 VOLVO VCCS SYSTEMNUMMER                 
000900*                                 VOLVO VCCS SYSTEM NUMBER                
001000     03 0X2-IDDISTR          PIC S9(5)           COMP-3.                  
001100*                                 DISTRIKTNUMMER                          
001200*                                 DISTRICT NUMBER                         
001300     03 0X2-IDKUNDNR         PIC S9(7)           COMP-3.                  
001400*                                 KUNDNUMMER                              
001500*                                 CUSTOMER NO                             
001600     03 0X2-IDKUNDRF         PIC X(10).                                   
001700*                                 KUNDENS REFERENS (ORDERID)              
001800*                                 CUSTOMER REFERENCE (ORDER ID)           
001900     03 0X2-IDDC             PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100*                                 WAREHOUSE IDENTIFIER                    
002200     03 0X2-IDARTNR          PIC S9(9)           COMP-3.                  
002300*                                 ARTIKELNUMMER                           
002400*                                 PART NUMBER                             
002500     03 0X2-KVBEART          PIC S9(7)           COMP-3.                  
002600*                                 BESTÄLLT ANTAL STYCKEN                  
002700*                                 ORDERED QUANTITY                        
002800     03 0X2-KVBEART-Q        PIC S9(7)           COMP-3.                  
002900*                                 BESTÄLLT KVANTANPASSAT ANTAL            
003000*                                 ORDERED QUANTITY ADAPTED                
003100*                                  ITEMS                                  
003200     03 0X2-BERADREF         PIC X(10).                                   
003300*                                 KUNDENS RADREFERENS                     
003400*                                 CUSTOMERS ITEM REF.                     
003500*** END OF VILMAII-COPY LENGTH= 49 BYTES                                  
