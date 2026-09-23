000100 01  MID-W30392I1.                                                        
000200*                                 MIDCOPYTEXT TILL W3039200.              
000300     03 MID-IDDISTR          PIC 9(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDKUNDNR         PIC 9(6).                                    
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
001900     03 MID-IDPRQUES         PIC 9(7).                                    
002000*                                 PRISFRÅGA NR                            
002100     03 MID-IDARTNR          PIC 9(8).                                    
002200*                                 ARTIKELNUMMER                           
002300     03 MID-KDORDKL          PIC 9.                                       
002400*                                 ORDERKLASS                              
002500     03 MID-KVBEART          PIC 9(6).                                    
002600*                                 BESTÄLLT ANTAL STYCKEN                  
002700     03 MID-PRARTBTO-LOC     PIC 9(7)V9(2).                               
002800*                                 PRIS I LOKAL VALUTA                     
002900     03 MID-PRARTNTO-LOC     PIC 9(7)V9(2).                               
003000*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
003100     03 MID-KDRAB            PIC X(5).                                    
003200*                                 RABATTKOD                               
003300     03 MID-KDVALISO         PIC X(3).                                    
003400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003500     03 MID-KDVAT            PIC X(2).                                    
003600*                                 MOMSKOD                                 
003700     03 MID-REARTRAB         PIC 9(2)V9(2).                               
003800*                                 ARTIKELRABATT                           
003900     03 MID-BEART-VIPS       PIC X(25).                                   
004000*                                 VIPS ARTIKELBENÄMNING                   
004100*                                 PÅ DEALERNS SPRÅK                       
004200     03 MID-FLALL            PIC X.                                       
004300*                                 HELA BELOPPET VALT                      
004400     03 MID-KDFEL            PIC 9(3).                                    
004500*                                 FELKOD                                  
004600*** END OF VILMAII-COPY LENGTH= 108 BYTES                                 
