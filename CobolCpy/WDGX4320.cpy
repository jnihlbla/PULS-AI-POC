000100 01  4320-WDGX4320.                                                       
000200*                                 PACKNING FYSISKA NOLLOR                 
000300*                                 LOGISKT BARN TILL 4319                  
000400*                                 FYSISK NYCKEL: WDGXKEY                  
000500*                                 (IDDISTR + IDKUNDNR +                   
000600*                                  IDORDNR + IDPRODNR +                   
000700*                                  IDARTNR + REKSIFFR)                    
000800     03 4320-IDDISTR         PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000*                                 DISTRICT NUMBER                         
001100     03 4320-IDKUNDNR        PIC S9(7)           COMP-3.                  
001200*                                 KUNDNUMMER                              
001300*                                 CUSTOMER NO                             
001400     03 4320-IDORDNR         PIC S9(5)           COMP-3.                  
001500*                                 ORDERNUMMER                             
001600*                                 ORDER NUMBER                            
001700     03 4320-IDPRODNR        PIC S9(7)           COMP-3.                  
001800*                                 PRODUKTIONSNUMMER                       
001900*                                 PRODUCTION-NUMBER                       
002000     03 4320-IDARTNR         PIC S9(9)           COMP-3.                  
002100*                                 ARTIKELNUMMER                           
002200*                                 PART NUMBER                             
002300     03 4320-REKSIFFR        PIC S9              COMP-3.                  
002400*                                 KONTROLLSIFFRA                          
002500*                                 PART NO CHECK DIGIT                     
002600     03 4320-BEVARREF        PIC X(10).                                   
002700*                                 VÅR REFERENS                            
002800*                                 OUR REFERENCE                           
002900     03 4320-BEART           PIC X(25).                                   
003000*                                 ARTIKELBENÄMNING                        
003100*                                 PART DESCRIPTION                        
003200     03 4320-KVAVBART        PIC S9(7)           COMP-3.                  
003300*                                 AVBOKAT ANTAL ARTIKLAR                  
003400*                                 ALLOCATED QUANTITY                      
003500     03 4320-KVLEVART        PIC S9(7)           COMP-3.                  
003600*                                 LEVERERAT ANTAL STYCK                   
003700*                                 DELIVERED QUANTITY                      
003800     03 4320-KDORDKL         PIC S9              COMP-3.                  
003900*                                 ORDERKLASS                              
004000*                                 ORDER CLASS                             
004100     03 4320-IDDC            PIC X(2).                                    
004200*                                 IDENTIFIERARE LAGER                     
004300*                                 WAREHOUSE IDENTIFIER                    
