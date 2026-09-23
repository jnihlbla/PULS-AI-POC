000100 01  6352-WDGX6352.                                                       
000200*                                 INLEVERANS FAST I TULLEN                
000300*                                 ARTIKLAR/KOLLIN                         
000400*                                 FYSISK NYCKEL: DAINLEV                  
000500     03 6352-DAINLEV         PIC 9(16).                                   
000600*                                 INLEVERANS NUMMER                       
000700*                                 CONSIGNMENT IDENTITY                    
000800*                                 (YYYYMMDD+HHMMSSTH)                     
000900     03 6352-IDARTNR         PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100*                                 PART NUMBER                             
001200     03 6352-IDFAKT          PIC S9(7)           COMP-3.                  
001300*                                 FAKTURANUMMER                           
001400*                                 INVOICE NO.                             
001500     03 6352-IDKUNDNR        PIC S9(7)           COMP-3.                  
001600*                                 KUNDNUMMER                              
001700*                                 CUSTOMER NO                             
001800     03 6352-IDOKOLLI        PIC 9(9).                                    
001900*                                 ODETTE KOLLINUMMER                      
002000*                                 ODETTE CASE NUMBER                      
002100     03 6352-IDORDNR7        PIC 9(7).                                    
002200*                                 ORDERNUMMER                             
002300*                                 ORDER NUMBER                            
002400     03 6352-KVAVIS          PIC S9(7)           COMP-3.                  
002500*                                 AVISERAT ANTAL                          
002600*                                 QUANTITY NOTIFIED                       
002700     03 6352-PRAVCOST        PIC S9(7)V9(2)      COMP-3.                  
002800*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
002900*                                 AVERAGE COST FOREIGN CURRENCY           
003000     03 6352-DAINLEV-9KOMPL  PIC 9(16).                                   
003100*                                 INLEVNR-9KOMP(AAAAMMDDTTMMSSTH)         
003200*                                 CONSIGN-9COMP(YYYYMMDDHHMMSSTH)         
003300*** END OF VILMAII-COPY LENGTH= 70 BYTES                                  
