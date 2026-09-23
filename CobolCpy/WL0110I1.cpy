000100 01  REQU-WL0110I1.                                                       
000200*                                 REQUEST TO PGM WL0110                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500*                                 WAREHOUSE IDENTIFIER                    
000600     03 REQU-IDARTNR-KEY     PIC 9(8).                                    
000700*                                 ARTIKELNUMMER                           
000800*                                 PART NUMBER                             
000900     03 REQU-IDLEVNR-KEY     PIC X(5).                                    
001000*                                 LEVERANT÷RNUMMER                        
001100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001200     03 REQU-IDOKOLLI-KEY    PIC 9(9).                                    
001300*                                 ODETTE KOLLINUMMER                      
001400*                                 ODETTE CASE NUMBER                      
001500     03 REQU-ADINLOMR-PRT    PIC X(4).                                    
001600*                                 PRINTERPLACERING                        
001700*                                 PLACE OF A PRINTER                      
001800     03 REQU-KDMATT          PIC X.                                       
001900*                                 M≈TTKOD                                 
002000*                                 MEASUREMENT CODE                        
002100     03 REQU-TIINLMOT        PIC 9(6).                                    
002200*                                 MOTTAGNINGSDATUM   (≈≈MMDD)             
002300*                                 RECEIVING DATE    (YYMMDD)              
002400     03 REQU-KVINLART        PIC 9(6).                                    
002500*                                 ANTAL I PARTIRAD                        
002600*                                 QTY/LINE IN A LOT                       
002700     03 REQU-KVINLART-LAST   PIC 9(6).                                    
002800*                                 ANTAL I PARTIRAD                        
002900*                                 QTY/LINE IN A LOT                       
003000     03 REQU-KVFLETI         PIC 9(2).                                    
003100*                                 ANTAL FLAGGOR EL ETIKETTER              
003200*                                 NO OF FLAGS OR LABELS                   
003300*** END OF VILMAII-COPY LENGTH= 49 BYTES                                  
