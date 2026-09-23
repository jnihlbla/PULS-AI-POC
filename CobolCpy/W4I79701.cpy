000100 01  MID-W4I79701.                                                        
000200*                                 MIDCOPYTEXT TILL W40797.                
000300     03 MID-KVPOST           PIC 9(7).                                    
000400*                                 RÄKNARE, ANTAL POSTER                   
000500*                                 RECORD COUNTER                          
000600     03 MID-R32-POST         OCCURS 16 TIMES.                             
000700        05 MID-IDDISTR       PIC 9(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900*                                 DISTRICT NUMBER                         
001000        05 MID-IDKUNDNR      PIC 9(6).                                    
001100*                                 KUNDNUMMER                              
001200*                                 CUSTOMER NO                             
001300        05 MID-IDRAPPNR      PIC 9(7).                                    
001400*                                 RAPPORT NUMMER                          
001500*                                 DISCREPANCY REPORT NUMBER               
001600        05 MID-IDARTNR       PIC 9(8).                                    
001700*                                 ARTIKELNUMMER                           
001800*                                 PART NUMBER                             
001900        05 MID-IDRADNR       PIC 9(4).                                    
002000*                                 RADNUMMER                               
002100*                                 LINE NO                                 
002200        05 MID-KVRETINL      PIC 9(6).                                    
002300*                                 INLAGT ANTAL VID RETUR                  
002400*                                 RECEIVED QUANTITY ON RETURN             
002500        05 MID-KVAVV-KVANT   PIC 9(6).                                    
002600*                                 ANTALSAVVIKELSE KVANTITET               
002700*                                 QUANTITYDEVIATION QUANTITY              
002800        05 MID-KVRETINL-TRP  PIC 9(6).                                    
002900*                                 INLAGT ANTAL VID RETUR                  
003000*                                 RECEIVED QUANTITY ON RETURN             
003100        05 MID-KVRETINL-SKR  PIC 9(6).                                    
003200*                                 INRPT ANTAL SOM SKROTATS                
003300*                                 REPORTED QTY SCRAPPED                   
003400*** END OF VILMAII-COPY LENGTH= 855 BYTES                                 
