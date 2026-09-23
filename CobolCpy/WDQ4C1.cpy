000100 01  SEQC-WDQ4C1.                                                         
000200*                                 ORDERRADSREGISTER                       
000300*                                 SEKUNDÄRT INDEX TILL WDQ401             
000400*                                 EXIT EXISTS IF KVOKS-PREL > 0           
000500*                                 FYSISK NYCKEL: WDQ4C1KY                 
000600*                                 (IDARTNR,  IDDC, IDORDER,               
000700*                                  IDLOPNR,  ADLAGOMR,                    
000800*                                  ADGANG,   ADPLATS)                     
000900*                                 SECONDARY NYCKEL: WDQ4CSEQ              
001000*                                 (IDARTNR,  IDDC)                        
001100     03 SEQC-IDARTNR         PIC S9(9)           COMP-3.                  
001200*                                 ARTIKELNUMMER                           
001300*                                 PART NUMBER                             
001400     03 SEQC-IDDC            PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600*                                 WAREHOUSE IDENTIFIER                    
001700     03 SEQC-IDORDER         PIC S9(7)           COMP-3.                  
001800*                                 VOLVO PARTS ORDERNUMMER                 
001900*                                 VOLVO PARTS ORDER NUMBER                
002000     03 SEQC-IDLOPNR         PIC S9(3)           COMP-3.                  
002100*                                 LÖPNUMMER                               
002200*                                 SEQUENCE NUMBER                         
002300     03 SEQC-ADLAGOMR        PIC S9(3)           COMP-3.                  
002400*                                 LAGEROMRÅDE                             
002500*                                 AREA                                    
002600     03 SEQC-ADGANG          PIC S9(3)           COMP-3.                  
002700*                                 GÅNG                                    
002800*                                 AISLE                                   
002900     03 SEQC-ADPLATS         PIC S9(5)           COMP-3.                  
003000*                                 LAGERPLATSNUMMER                        
003100*                                 LOCATION                                
003200     03 SEQC-KVOKS-PREL      PIC S9(7)           COMP-3.                  
003300*                                 PREL ORDERKÖSALDO VERKST.ORDER          
003400*                                 PREL ORDER QUEUE BALANCE                
003500     03 SEQC-IDWDQ401        PIC X(20).                                   
003600*                                 NYCKEL TILL WDQ401                      
003700*                                 KEY TO WDQ401                           
003800*** END OF VILMAII-COPY LENGTH= 44 BYTES                                  
