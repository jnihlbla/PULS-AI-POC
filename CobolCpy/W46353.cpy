000100 01  W46353.                                                              
000200*                                 DIRECT DELIVERIES                       
000300*                                 FROM VENDOR TO RETAILER                 
000400*                                                                         
000500     03 IDPRODNR             PIC 9(7).                                    
000600*                                 PRODUKTIONSNUMMER                       
000700*                                 PRODUCTION NUMBER                       
000800     03 IDKUNDNR             PIC 9(6).                                    
000900*                                 KUNDNUMMER                              
001000*                                 CUSTOMER NO                             
001100     03 IDORDNR7             PIC 9(7).                                    
001200*                                 ORDERNUMMER                             
001300*                                 ORDER NUMBER                            
001400     03 IDFAKT-GNB           PIC X(8).                                    
001500*                                 FAKTURANUMMER GNB                       
001600*                                 INVOICE NO. GNB                         
001700     03 DAFAKT-GNB           PIC 9(8).                                    
001800*                                 FAKTURADATUM GNB (≈≈≈≈MMDD)             
001900*                                 INVOICE DATE GNB (YYYYMMDD)             
002000     03 IDRADNR              PIC 9(4).                                    
002100*                                 RADNUMMER                               
002200*                                 LINE NO                                 
002300     03 IDARTNR              PIC 9(8).                                    
002400*                                 ARTIKELNUMMER                           
002500*                                 PART NUMBER                             
002600     03 KVLEVART             PIC 9(7).                                    
002700*                                 LEVERERAT ANTAL STYCK                   
002800*                                 DELIVERED QUANTITY                      
002900     03 PRARTNTO-GNB         PIC 9(7)V9(2).                               
003000*                                 NETTOPRIS FR≈N DIRLEV GNB               
003100*                                 NET PRICE FROM SUPPLIER GNB             
003200     03 KDVALISO             PIC X(3).                                    
003300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003400*                                 CURRENCY CODE BY ISO-STANDARD.          
003500*** END OF VILMAII-COPY LENGTH= 67 BYTES                                  
