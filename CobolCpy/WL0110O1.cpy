000100 01  RESP-WL0110O1.                                                       
000200*                                 RESPONS FROM PGM WL0110                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500*                                 WAREHOUSE IDENTIFIER                    
000600     03 RESP-IDARTNR-KEY     PIC Z(7)9.                                   
000700*                                 ARTIKELNUMMER                           
000800*                                 PART NUMBER                             
000900     03 RESP-IDLEVNR-KEY     PIC X(5).                                    
001000*                                 LEVERANT÷RNUMMER                        
001100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001200     03 RESP-IDOKOLLI-KEY    PIC Z(8)9.                                   
001300*                                 ODETTE KOLLINUMMER                      
001400*                                 ODETTE CASE NUMBER                      
001500     03 RESP-ADINLOMR-PRT    PIC X(4).                                    
001600*                                 PRINTERPLACERING                        
001700*                                 PLACE OF A PRINTER                      
001800     03 RESP-TIINLMOT        PIC Z(5)9.                                   
001900*                                 MOTTAGNINGSDATUM   (≈≈MMDD)             
002000*                                 RECEIVING DATE    (YYMMDD)              
002100     03 RESP-KVINLART        PIC X(6).                                    
002200*                                 ANTAL I PARTIRAD                        
002300*                                 QTY/LINE IN A LOT                       
002400     03 RESP-KVINLART-LAST   PIC X(6).                                    
002500*                                 ANTAL I PARTIRAD                        
002600*                                 QTY/LINE IN A LOT                       
002700     03 RESP-KVFLETI         PIC X(2).                                    
002800*                                 ANTAL FLAGGOR EL ETIKETTER              
002900*                                 NO OF FLAGS OR LABELS                   
003000     03 RESP-KVRADER         PIC Z(4)9.                                   
003100*                                 ANTAL RADER                             
003200*                                 NUMBER OF LINES                         
003300     03 RESP-FLAGG-POST      OCCURS 15 TIMES.                             
003400        05 RESP-IDLOPNRM     PIC 9(8).                                    
003500*                                 L÷PNUMMER MOTTAGNINGSRAPPORT            
003600*                                 (0VVDLLLLK)                             
003700*                                 SERIAL NO RECEIVING REPORT              
003800*                                 (0WWDLLLLC)                             
003900        05 RESP-IDARTNR      PIC 9(8).                                    
004000*                                 ARTIKELNUMMER                           
004100*                                 PART NUMBER                             
004200        05 RESP-IDLEVNR      PIC X(5).                                    
004300*                                 LEVERANT÷RNUMMER                        
004400*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
004500        05 RESP-IDOKOLLI     PIC 9(9).                                    
004600*                                 ODETTE KOLLINUMMER                      
004700*                                 ODETTE CASE NUMBER                      
004800        05 RESP-KVINLART-LINE                                             
004900                             PIC 9(6).                                    
005000*                                 ANTAL I PARTIRAD                        
005100*                                 QTY/LINE IN A LOT                       
005200        05 RESP-TIINLMOT-LINE                                             
005300                             PIC 9(6).                                    
005400*                                 MOTTAGNINGSDATUM   (≈≈MMDD)             
005500*                                 RECEIVING DATE    (YYMMDD)              
005600        05 RESP-VKKOLLIN     PIC 9(5)V9(1).                               
005700*                                 KOLLI-VIKT-NETTO                        
005800*                                 NET WEIGHT OF PACKAGE                   
005900        05 RESP-ADLAGOMR     PIC 9(2).                                    
006000*                                 LAGEROMR≈DE                             
006100*                                 AREA                                    
006200        05 RESP-ADGANG       PIC 9(2).                                    
006300*                                 G≈NG                                    
006400*                                 AISLE                                   
006500        05 RESP-ADPLATS      PIC 9(5).                                    
006600*                                 LAGERPLATSNUMMER                        
006700*                                 LOCATION                                
006800        05 RESP-KDSORT       PIC X(2).                                    
006900*                                 SORT-KOD                                
007000*                                 UNIT OF MEASURE                         
007100*** END OF VILMAII-COPY LENGTH= 938 BYTES                                 
