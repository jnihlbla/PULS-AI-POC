000100 01  W33590.                                                              
000200*                                 RENSNINGPOSTER AV WDC7                  
000300     03 IDSEGM               PIC X(6).                                    
000400*                                 SEGMENT                                 
000500     03 WDC701.                                                           
000600*                                 DDI PRIS FRÅGA                          
000700*                                 BUNT ID                                 
000800*                                 FYSISK NYCKEL: WDC701KY                 
000900*                                 (IDDISTR IDKUNDNR IDBUNDLE-GRP)         
001000        05 IDDISTR           PIC 9(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200        05 IDKUNDNR          PIC 9(7).                                    
001300*                                 KUNDNUMMER                              
001400        05 IDBUNDLE-GRP.                                                  
001500*                                 QUERY  REFERENS (ORDERID/RAPPN)         
001600           07 IDBUNDLE       PIC X(15).                                   
001700*                                 BUNDLE ID                               
001800           07 IDORDNR7-FILLER REDEFINES IDBUNDLE.                         
001900              09 IDORDNR7    PIC 9(7).                                    
002000*                                 ORDERNUMMER                             
002100              09 FILLER      PIC X(8).                                    
002200           07 IDRAPPNR-FILLER REDEFINES IDBUNDLE.                         
002300              09 IDRAPPNR    PIC 9(7).                                    
002400*                                 RAPPORT NUMMER                          
002500              09 FILLER      PIC X(8).                                    
002600           07 IDORDER-FILLER REDEFINES IDBUNDLE.                          
002700              09 IDORDER     PIC 9(7).                                    
002800*                                 VOLVO PARTS ORDERNUMMER                 
002900              09 FILLER      PIC X(8).                                    
003000     03 WDC711.                                                           
003100*                                 DDI PRIS FRÅGA                          
003200*                                 RADPRIS                                 
003300*                                 FYSISK NYCKEL: IDPRQUES                 
003400        05 IDPRQUES          PIC 9(7).                                    
003500*                                 PRISFRÅGA NR                            
003600        05 ADDISPABS         PIC X(50).                                   
003700*                                 ABSTRAKT ADRESS                         
003800        05 KDPRSTA           PIC X.                                       
003900*                                 STATUS PRISFRÅGA                        
004000        05 KDORDKL           PIC X.                                       
004100*                                 ORDERKLASS                              
004200        05 IDARTNR           PIC 9(9).                                    
004300*                                 ARTIKELNUMMER                           
004400        05 KVBEART           PIC S9(7)           COMP-3.                  
004500*                                 BESTÄLLT ANTAL STYCKEN                  
004600        05 FLARTSTD          PIC X.                                       
004700*                                 INDIKERAR PRARTSTD                      
004800        05 KDORDTYP          PIC X.                                       
004900*                                 ORDERTYP                                
005000        05 PRARTBTO-LOC      PIC S9(7)V9(2)      COMP-3.                  
005100*                                 PRIS I LOKAL VALUTA                     
005200        05 PRARTNTO-LOC      PIC S9(7)V9(2)      COMP-3.                  
005300*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
005400        05 KDRAB             PIC X(5).                                    
005500*                                 RABATTKOD                               
005600        05 KDVALISO          PIC X(3).                                    
005700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005800        05 KDVAT             PIC X(2).                                    
005900*                                 MOMSKOD                                 
006000        05 REARTRAB          PIC S9(2)V9(2)      COMP-3.                  
006100*                                 ARTIKELRABATT                           
006200        05 BEART-VIPS        PIC X(25).                                   
006300*                                 VIPS ARTIKELBENÄMNING                   
006400*                                 PÅ DEALERNS SPRÅK                       
006500        05 FLALL             PIC X.                                       
006600*                                 HELA BELOPPET VALT                      
006700        05 KVANTAL-AVBOK     PIC S9(7)           COMP-3.                  
006800*                                 KVITTAT ANTAL                           
006900        05 DADATTID-REG      PIC 9(14).                                   
007000*                                 DAT-TID FÖR REG                         
007100        05 DADATTID-SEND     PIC 9(14).                                   
007200*                                 DAT-TID FÖR SEND                        
007300        05 DADATTID-SVAR     PIC 9(14).                                   
007400*                                 DAT-TID FÖR SVAR                        
007500        05 DADATTID-OK       PIC 9(14).                                   
007600*                                 DATOTID FÖR OK SVAR                     
007700        05 KDFEL             PIC 9(3).                                    
007800*                                 FELKOD                                  
007900        05 KDSKEPP           PIC 9.                                       
008000*                                 STATUS PÅ SKEPPNING                     
008100        05 FILLER            PIC X(25).                                   
008200*** END OF VILMAII-COPY LENGTH= 244 BYTES                                 
