000100 01  RAD-WDM801.                                                          
000200*                                 TULLSYSTEM                              
000300*                                 TULL-RAD UPPGIFTER                      
000400*                                 FYSISK NYCKEL: WDM801KY                 
000500*                                 IDFAKT  + IDPRODNR + IDKOLLI +          
000600*                                 IDARTNR + IDRADNR                       
000700     03 RAD-IDFAKT           PIC S9(7)           COMP-3.                  
000800*                                 FAKTURANUMMER                           
000900*                                 INVOICE NO.                             
001000     03 RAD-IDPRODNR         PIC S9(7)           COMP-3.                  
001100*                                 PRODUKTIONSNUMMER                       
001200*                                 PRODUCTION NUMBER                       
001300     03 RAD-IDKOLLI          PIC S9(5)           COMP-3.                  
001400*                                 KOLLINUMMER                             
001500*                                 CASE NUMBER                             
001600     03 RAD-IDARTNR          PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800*                                 PART NUMBER                             
001900     03 RAD-IDRADNR          PIC S9(5)           COMP-3.                  
002000*                                 RADNUMMER                               
002100*                                 LINE NO                                 
002200     03 RAD-IDSTATNR         PIC S9(9)           COMP-3.                  
002300*                                 STATISTISKT NUMMER                      
002400*                                 1 = NORSKT                              
002500*                                 2 = ENGELSKT                            
002600*                                 3 = BELGISKT                            
002700*                                 4 = PERUANSKT                           
002800*                                 5 = SVENSKT                             
002900*                                 6 =                                     
003000*                                 STATISTICAL NO.                         
003100     03 RAD-KDARTURS         PIC X(2).                                    
003200*                                 ARTIKELURSPRUNGSKOD                     
003300*                                 COUNTRY OF ORIGIN                       
003400     03 RAD-KVLEVART         PIC S9(7)           COMP-3.                  
003500*                                 LEVERERAT ANTAL STYCK                   
003600*                                 DELIVERED QUANTITY                      
003700     03 RAD-PRARTNTO         PIC S9(7)V9(2)      COMP-3.                  
003800*                                 ARTIKELPRIS NETTO                       
003900*                                 NET PRICE EACH   (FOB NET)              
004000     03 RAD-VKART-NTO-KG     PIC S9(4)V9(3)      COMP-3.                  
004100*                                 ART. NETTOVIKT I KG UTAN EMB            
004200*                                 PART NET WEIGHT KG NO PACKAGING         
004300     03 RAD-KDVALISO         PIC X(3).                                    
004400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004500*                                 CURRENCY CODE BY ISO-STANDARD.          
004600     03 RAD-FLPCOO           PIC X.                                       
004700*                                 FLAGGA OM FÖRMÅNSAVTAL URS.LAND         
004800*                                 FLAG PREF.AGREM. COUNTRY ORIGIN         
004900     03 RAD-KDVALISO-AVC     PIC X(3).                                    
005000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005100*                                 CURRENCY CODE BY ISO-STANDARD.          
005200*                                 AVERAGE COST CURRENCY                   
005300     03 RAD-PRAVCOST         PIC S9(7)V9(2)      COMP-3.                  
005400*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
005500*                                 AVERAGE COST FOREIGN CURRENCY           
005600     03 RAD-IDREFDDS         PIC X(25).                                   
005700*                                 REFERENS ID DDS                         
005800*                                 REFERENCE ID DDS                        
005900     03 RAD-KDORSAK          PIC X(5).                                    
006000*                                 ORSAKSKOD EUDR                          
006100*                                 REASON CODE EUDR                        
006200*** END OF VILMAII-COPY LENGTH= 81 BYTES                                  
