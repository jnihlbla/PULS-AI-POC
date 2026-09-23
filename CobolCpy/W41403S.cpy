000100 01  W41403S.                                                             
000200*                                 ORDERRADER - SKROT ORDRAR               
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500*                                 RECORD TYPE                             
000600     03 KDSORT1              PIC 9.                                       
000700*                                 SORTERINGSKOD                           
000800*                                 CODE FOR SORTING                        
000900     03 KDMFUP               PIC X(2).                                    
001000*                                 RAPPORTGRUPP  MA/CN/PF/NA               
001100*                                 REPORT GROUP  MA/CN/PF/NA               
001200     03 IDDC                 PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400*                                 WAREHOUSE IDENTIFIER                    
001500     03 IDDISTR              PIC S9(5)           COMP-3.                  
001600*                                 DISTRIKTNUMMER                          
001700*                                 DISTRICT NUMBER                         
001800     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001900*                                 KUNDNUMMER                              
002000*                                 CUSTOMER NO                             
002100     03 IDKUNDRF             PIC X(10).                                   
002200*                                 KUNDENS REFERENS (ORDERID)              
002300*                                 CUSTOMER REFERENCE (ORDER ID)           
002400     03 IDARTNR              PIC S9(9)           COMP-3.                  
002500*                                 ARTIKELNUMMER                           
002600*                                 PART NUMBER                             
002700     03 KVBEART              PIC S9(7)           COMP-3.                  
002800*                                 BESTÄLLT ANTAL STYCKEN                  
002900*                                 ORDERED QUANTITY                        
003000     03 KVBEART-Q            PIC S9(7)           COMP-3.                  
003100*                                 BESTÄLLT KVANTANPASSAT ANTAL            
003200*                                 ORDERED QUANTITY ADAPTED                
003300*                                  ITEMS                                  
003400     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
003500*                                 ARTIKELPRIS NETTO                       
003600*                                 NET PRICE EACH   (FOB NET)              
003700     03 SUARTNTO             PIC S9(9)V9(2)      COMP-3.                  
003800*                                 SUMMA RADVÄRDE TILL NETTOPRIS           
003900*                                 TOTAL VALUE PER ITEM NETPRICE           
004000     03 BERADREF             PIC X(10).                                   
004100*                                 KUNDENS RADREFERENS                     
004200*                                 CUSTOMERS ITEM REF.                     
004300*** END OF VILMAII-COPY LENGTH= 59 BYTES                                  
