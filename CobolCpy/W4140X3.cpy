000100 01  0X3-W4140X3.                                                         
000200*                                 ORDERBEKRÄFTELSETRANSAKT.               
000300*                                 FÖR SPX, POSTTYP 0X3                    
000400     03 0X3-IDPTYP           PIC X(3).                                    
000500*                                 POSTTYP                                 
000600*                                 RECORD TYPE                             
000700     03 0X3-IDSYSTEM         PIC X(4).                                    
000800*                                 VOLVO VCCS SYSTEMNUMMER                 
000900*                                 VOLVO VCCS SYSTEM NUMBER                
001000     03 0X3-IDDISTR          PIC S9(5)           COMP-3.                  
001100*                                 DISTRIKTNUMMER                          
001200*                                 DISTRICT NUMBER                         
001300     03 0X3-IDKUNDNR         PIC S9(7)           COMP-3.                  
001400*                                 KUNDNUMMER                              
001500*                                 CUSTOMER NO                             
001600     03 0X3-IDKUNDRF         PIC X(10).                                   
001700*                                 KUNDENS REFERENS (ORDERID)              
001800*                                 CUSTOMER REFERENCE (ORDER ID)           
001900     03 0X3-IDDC             PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100*                                 WAREHOUSE IDENTIFIER                    
002200     03 0X3-IDARTNR          PIC S9(9)           COMP-3.                  
002300*                                 ARTIKELNUMMER                           
002400*                                 PART NUMBER                             
002500     03 0X3-KVBEART          PIC S9(7)           COMP-3.                  
002600*                                 BESTÄLLT ANTAL STYCKEN                  
002700*                                 ORDERED QUANTITY                        
002800     03 0X3-KVBEART-Q        PIC S9(7)           COMP-3.                  
002900*                                 BESTÄLLT KVANTANPASSAT ANTAL            
003000*                                 ORDERED QUANTITY ADAPTED                
003100*                                  ITEMS                                  
003200     03 0X3-BERADREF         PIC X(10).                                   
003300*                                 KUNDENS RADREFERENS                     
003400*                                 CUSTOMERS ITEM REF.                     
003500     03 0X3-KDORDBEK         PIC 9(2).                                    
003600*                                 ORDERBEKRÄFTELSEKOD                     
003700*                                 ORDERCONFIMATIONCODE                    
003800     03 0X3-IDKUNDRF-RO      PIC X(10).                                   
003900*                                 KUND REF PÅ RO                          
004000*                                 CUST REF RO                             
004100     03 0X3-KDERS            PIC S9(3)           COMP-3.                  
004200*                                 ERSÄTTNINGSKOD                          
004300*                                 SUPERSESSION CODE                       
004400     03 0X3-FLTILLK          PIC X.                                       
004500*                                 TILLKOMMANDE ARTIKEL ?                  
004600*                                 REPLACEMENT PART FLAG                   
004700     03 0X3-IDARTNR-TILLK    PIC S9(9)           COMP-3.                  
004800*                                 TILLKOMMANDE ARTIKELNUMMER              
004900*                                 REPLACEMENT PART NO.                    
005000*** END OF VILMAII-COPY LENGTH= 69 BYTES                                  
