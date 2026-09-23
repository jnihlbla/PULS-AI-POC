000100 01  SEQA-WDD4A1-CTX.                                                     
000200*                                 BETALAR REGISTER                        
000300*                                 SEKUNDÄRT INDEX TILL WDD401             
000400*                                 FYSISK NYCKEL: WDD4A1KY                 
000500*                                 (IDANSK + IDLEVNR + IDARTNR +           
000600*                                  DAREGDAT-9KOMP +TIKLOCK-9KOMP)         
000700*                                 SEKUNDÄR NYCKEL: WDD4ASEQ               
000800*                                 (IDANSK + IDLEVNR + IDARTNR +           
000900*                                  DAREGDAT-9KOMP +TIKLOCK-9KOMP)         
001000*                                                                         
001100     03 SEQA-IDANSK          PIC S9(3)           COMP-3.                  
001200*                                 ANSKAFFARNUMMER                         
001300*                                 PROCURER NO.                            
001400     03 SEQA-IDLEVNR         PIC X(5).                                    
001500*                                 LEVERANTÖRNUMMER                        
001600*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001700     03 SEQA-IDARTNR         PIC S9(9)           COMP-3.                  
001800*                                 ARTIKELNUMMER                           
001900*                                 PART NUMBER                             
002000     03 SEQA-DAREGDAT-9KOMPL PIC 9(8).                                    
002100*                                 DATUMETS 9-KOMPLEMENT                   
002200*                                 DATES 9-COMPLEMENT                      
002300     03 SEQA-TIKLOCK-9KOMPL  PIC S9(9)           COMP-3.                  
002400*                                 TID LAGRAT SOM 9-KOMPLEMENT             
002500*                                 TIME SAVED AS 9-COMPLEMENT              
002600     03 SEQA-IDDC            PIC X(2).                                    
002700*                                 IDENTIFIERARE LAGER                     
002800*                                 WAREHOUSE IDENTIFIER                    
002900     03 SEQA-IDWDD401        PIC X(13).                                   
003000*                                 NYCKEL TILL WDD401                      
003100*                                 KEY TO WDA501                           
003200*** END OF VILMAII-COPY LENGTH= 40 BYTES                                  
