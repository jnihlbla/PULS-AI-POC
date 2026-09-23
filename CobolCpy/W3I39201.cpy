000100 01  MID-W3I39201.                                                        
000200*                                 MIDCOPYTEXT TILL W3039200.              
000300     03 MID-IDDISTR          PIC 9(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDKUNDNR         PIC 9(7).                                    
000600*                                 KUNDNUMMER                              
000700     03 MID-IDBUNDLE-GRP.                                                 
000800*                                 QUERY  REFERENS (ORDERID/RAPPN)         
000900        05 MID-IDBUNDLE      PIC X(15).                                   
001000*                                 BUNDLE ID                               
001100        05 MID-IDORDNR7-FILLER REDEFINES MID-IDBUNDLE.                    
001200           07 MID-IDORDNR7   PIC 9(7).                                    
001300*                                 ORDERNUMMER                             
001400           07 FILLER         PIC X(8).                                    
001500        05 MID-IDRAPPNR-FILLER REDEFINES MID-IDBUNDLE.                    
001600           07 MID-IDRAPPNR   PIC 9(7).                                    
001700*                                 RAPPORT NUMMER                          
001800           07 FILLER         PIC X(8).                                    
001900     03 MID-IDPRQUES-GRP.                                                 
002000*                                 PRISFRÅGENUMMER                         
002100        05 MID-IDPRQORD      PIC S9(7)           COMP-3.                  
002200*                                 PRISFRÅGA NR PER ORDER                  
002300        05 MID-IDPRQRAD      PIC S9(5)           COMP-3.                  
002400*                                 PRISFRÅGA NR ORAD                       
002500     03 MID-IDARTNR          PIC 9(9).                                    
002600*                                 ARTIKELNUMMER                           
002700     03 MID-KDORDKL          PIC X.                                       
002800*                                 ORDERKLASS                              
002900     03 MID-KVBEART          PIC 9(7).                                    
003000*                                 BESTÄLLT ANTAL STYCKEN                  
003100     03 MID-PRARTBTO-LOC     PIC S9(7)V9(2)      COMP-3.                  
003200*                                 PRIS I LOKAL VALUTA                     
003300     03 MID-PRARTNTO-LOC     PIC S9(7)V9(2)      COMP-3.                  
003400*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
003500     03 MID-KDRAB            PIC X(5).                                    
003600*                                 RABATTKOD                               
003700     03 MID-KDVALISO         PIC X(3).                                    
003800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003900     03 MID-KDVAT            PIC X(2).                                    
004000*                                 MOMSKOD                                 
004100     03 MID-REARTRAB         PIC 9(2)V9(2).                               
004200*                                 ARTIKELRABATT                           
004300     03 MID-BEART-VIPS       PIC X(25).                                   
004400*                                 VIPS ARTIKELBENÄMNING                   
004500*                                 PÅ DEALERNS SPRÅK                       
004600     03 MID-FLALL            PIC X.                                       
004700*                                 HELA BELOPPET VALT                      
004800     03 MID-KDFEL            PIC 9(3).                                    
004900*                                 FELKOD                                  
005000*** END OF VILMAII-COPY LENGTH= 103 BYTES                                 
