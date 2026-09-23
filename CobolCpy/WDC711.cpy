000100 01  LPRQ-WDC711.                                                         
000200*                                 DDI PRIS FRÅGA                          
000300*                                 RADPRIS                                 
000400*                                 FYSISK NYCKEL: IDPRQUES                 
000500     03 LPRQ-IDPRQUES        PIC 9(7).                                    
000600*                                 PRISFRÅGA NR                            
000700*                                 PRICE QUESTION NO                       
000800     03 LPRQ-ADDISPABS       PIC X(50).                                   
000900*                                 ABSTRAKT ADRESS                         
001000*                                 ABSTRACT ADDRESS                        
001100     03 LPRQ-KDPRSTA         PIC X.                                       
001200*                                 STATUS PRISFRÅGA                        
001300*                                 STATUS PRICE QUESTION                   
001400     03 LPRQ-KDORDKL         PIC X.                                       
001500*                                 ORDERKLASS                              
001600*                                 ORDER CLASS                             
001700     03 LPRQ-IDARTNR         PIC 9(9).                                    
001800*                                 ARTIKELNUMMER                           
001900*                                 PART NUMBER                             
002000     03 LPRQ-KVBEART         PIC S9(7)           COMP-3.                  
002100*                                 BESTÄLLT ANTAL STYCKEN                  
002200*                                 ORDERED QUANTITY                        
002300     03 LPRQ-FLARTSTD        PIC X.                                       
002400*                                 INDIKERAR PRARTSTD                      
002500*                                 INDICATES PRARTSTD                      
002600     03 LPRQ-KDORDTYP        PIC X.                                       
002700*                                 ORDERTYP                                
002800     03 LPRQ-PRARTBTO-LOC    PIC S9(7)V9(2)      COMP-3.                  
002900*                                 PRIS I LOKAL VALUTA                     
003000*                                 LOCAL GROSS SALES PRICE                 
003100     03 LPRQ-PRARTNTO-LOC    PIC S9(7)V9(2)      COMP-3.                  
003200*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
003300*                                 NET PRICE EACH LOCAL CURRENCY           
003400     03 LPRQ-KDRAB           PIC X(5).                                    
003500*                                 RABATTKOD                               
003600     03 LPRQ-KDVALISO        PIC X(3).                                    
003700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003800*                                 CURRENCY CODE BY ISO-STANDARD.          
003900     03 LPRQ-KDVAT           PIC X(2).                                    
004000*                                 MOMSKOD                                 
004100*                                 VAT CODE                                
004200     03 LPRQ-REARTRAB        PIC S9(2)V9(2)      COMP-3.                  
004300*                                 ARTIKELRABATT                           
004400*                                 PARTS DISCOUNT PERCENT                  
004500     03 LPRQ-BEART-VIPS      PIC X(25).                                   
004600*                                 VIPS ARTIKELBENÄMNING                   
004700*                                 PÅ DEALERNS SPRÅK                       
004800     03 LPRQ-FLALL           PIC X.                                       
004900*                                 HELA BELOPPET VALT                      
005000*                                 TOTAL AMOUNT CHOOSEN                    
005100     03 LPRQ-KVANTAL-AVBOK   PIC S9(7)           COMP-3.                  
005200*                                 KVITTAT ANTAL                           
005300     03 LPRQ-DADATTID-REG    PIC 9(14).                                   
005400*                                 DAT-TID FÖR REG                         
005500*                                 DATE-TIME FOR REG                       
005600     03 LPRQ-DADATTID-SEND   PIC 9(14).                                   
005700*                                 DAT-TID FÖR SEND                        
005800*                                 DATE-TIME OF SEND                       
005900     03 LPRQ-DADATTID-SVAR   PIC 9(14).                                   
006000*                                 DAT-TID FÖR SVAR                        
006100*                                 DATE-TIME OF REPLY                      
006200     03 LPRQ-DADATTID-OK     PIC 9(14).                                   
006300*                                 DATOTID FÖR OK SVAR                     
006400*                                 DATE-TIME FOR OK                        
006500     03 LPRQ-KDFEL           PIC 9(3).                                    
006600*                                 FELKOD                                  
006700     03 LPRQ-KDSKEPP         PIC 9.                                       
006800*                                 STATUS PÅ SKEPPNING                     
006900     03 LPRQ-FILLER1         PIC X.                                       
007000     03 LPRQ-KDSEGKEY        PIC X.                                       
007100*                                 TEKNISK SEGMENT-NYCKEL                  
007200*                                 TECHNICAL SEGMENT KEY                   
007300     03 LPRQ-FILLER          PIC X(23).                                   
007400*** END OF VILMAII-COPY LENGTH= 212 BYTES                                 
