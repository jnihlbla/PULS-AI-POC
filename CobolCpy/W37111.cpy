000100 01  W37111.                                                              
000200*                                 SALDOPOSTER TILL                        
000300*                                 BYTESFAKTURERING                        
000400*                                 BALANCE TRANSACTIONS TO                 
000500*                                 EXCHANGE INVOICING                      
000600     03 IDSYSTEM             PIC X(4).                                    
000700*                                 VOLVO VCAS SYSTEMNUMMER                 
000800*                                 VOLVO VCAS SYSTEM NUMBER                
000900     03 IDDISTR-BET          PIC S9(5)           COMP-3.                  
001000*                                 BATALANDE DISTRIKT                      
001100*                                 DISTRICT TO CHARGE                      
001200     03 IDDISTR              PIC S9(5)           COMP-3.                  
001300*                                 DISTRIKTNUMMER                          
001400*                                 DISTRICT NUMBER                         
001500     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001600*                                 KUNDNUMMER                              
001700*                                 CUSTOMER NO                             
001800     03 IDORDNR              PIC S9(5)           COMP-3.                  
001900*                                 ORDERNUMMER UTGÅR PD90                  
002000*                                 ORDER NUMBER                            
002100     03 IDARTNR              PIC S9(9)           COMP-3.                  
002200*                                 ARTIKELNUMMER                           
002300*                                 PART NUMBER                             
002400     03 KDEXCHA              PIC S9(3)           COMP-3.                  
002500*                                 EXCHANGE ACCOUNT CODE                   
002600     03 KVBEART              PIC S9(7)           COMP-3.                  
002700*                                 BESTÄLLT ANTAL STYCKEN                  
002800*                                 ORDERED QUANTITY                        
002900     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
003000*                                 ARTIKELPRIS NETTO                       
003100*                                 NET PRICE EACH   (FOB NET)              
003200*** END OF VILMAII-COPY LENGTH= 33 BYTES                                  
