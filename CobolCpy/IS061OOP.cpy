000010*** EDIT ALLOWED                                                          
000020*    COPYTEXT FRÅN EPIC , NYTT GRÄNSSNITT FÖR UPPDATERING                 
000100*                         TILL PGM W09278                                 
000030*                         ORDER PRICE                                     
000200 01  IS061OOP.                                                            
000300*                             RECORD TYPE OOP                             
000400     03  RT                  PIC X(3).                                    
000500*                             PLANT                                       
000600     03  PLANT               PIC X(5).                                    
001400*                             ORDER DATE VALID FROM                       
001500     03  EFFECTIVE-DATE      PIC X(8).                                    
000700*                             PART NO                                     
000800     03  PARTNO              PIC 9(8).                                    
000900*                             SUPPLIER SITE MFG                           
001000     03  SUPPLIER-ID         PIC X(5).                                    
001100*                             ORDER PRICE SEK                             
001100*                             POS 1-7=INTEGER PART                        
001100*                             POS 8-9=DECIMAL PART                        
001300     03  ORDER-PRICE-SEK     PIC 9(9).                                    
001100*                             ORDER PRICE LOCAL CURRENCY                  
001300     03  ORDER-PRICE-LOC     PIC 9(9).                                    
001400*                             PRICE UNIT                                  
001500     03  PRICE-UNIT          PIC X(1).                                    
001600*                             UNIT OF MESSURE                             
001600*                             VOLVO CODES                                 
001700     03  UOM                 PIC 9(1).                                    
001800*                             LOCAL CURRENCY                              
002100     03  LOC-CURR            PIC X(3).                                    
002200*                             ORDER PRICE EURO                            
001100*                             POS 1-7=INTEGER PART                        
001100*                             POS 8-9=DECIMAL PART                        
002300     03  ORDER-PRICE-EURO    PIC 9(9).                                    
002400*                             SUPPLIER SHIP-FROM                          
002500*****03  SUPPLIER-SHIP       PIC X(5).                                    
002400*                             SUPPLIER SAL                                
002500     03  SUPPLIER-SAL        PIC X(5).                                    
002600*                             MODUL PART NO                               
002700     03  PART-VERSION        PIC X(8).                                    
002800*                             UOM ISO                                     
002900     03  UOM-ISO             PIC X(3).                                    
002800*                             PACKAGING TYPE CODE   Y/N                   
002900     03  PACK-TYPE-CODE      PIC X(1).                                    
002800*                                                                         
005600*** END OF VILMAII-COPY LENGTH= 078 OLD LENGTH= 077                       
