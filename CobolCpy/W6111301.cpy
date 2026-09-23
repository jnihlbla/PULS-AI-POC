000100 01  W61113.                                                              
000200*                                 URVAL FRÅN W6D1                         
000300*                                 EN POST FÖR VARJE PRIORITERAT           
000400*                                 KOLLI, LO 10,20,21 OCH 76               
000500*                                 SELECTED EXTRACT FROM W6D1              
000600*                                 ONE REC PER EACH PRIORITIZED            
000700*                                 CASE, LO 10,20,21 AND 76                
000800     03 ASTERISK-1           PIC X.                                       
000900     03 IDARTNR              PIC 9(8).                                    
001000*                                 ARTIKELNUMMER                           
001100*                                 PART NUMBER                             
001200     03 ASTERISK-2           PIC X.                                       
001300     03 IDLEVNR-KOLLI        PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER KOLLI                  
001500*                                 SUPPLIER NUMBER CASE                    
001600     03 ASTERISK-3           PIC X.                                       
001700     03 IDOKOLLI             PIC 9(9).                                    
001800*                                 ODETTE KOLLINUMMER                      
001900*                                 ODETTE CASE NUMBER                      
002000     03 ASTERISK-4           PIC X.                                       
002100     03 ADLAGOMR             PIC 9(2).                                    
002200*                                 LAGEROMRÅDE                             
002300*                                 AREA                                    
002400     03 ASTERISK-5           PIC X.                                       
002500     03 KVINLART             PIC 9(6).                                    
002600*                                 ANTAL I PARTIRAD                        
002700*                                 QTY/LINE IN A LOT                       
002800     03 ASTERISK-6           PIC X.                                       
002900*** END OF VILMAII-COPY LENGTH= 36 BYTES                                  
