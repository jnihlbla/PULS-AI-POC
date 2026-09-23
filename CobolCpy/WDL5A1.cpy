000100 01  SEQA-WDL5A1.                                                         
000200*                                 FAKTURA HISTORIK                        
000300*                                 IDSHIPM INGÅNG                          
000400*                                 SEKUNDÄRT INDEX TILL WDL511             
000500*                                 FYSISK NYCKEL: WDL5A1KY                 
000600*                                 (IDSHIPM + IDFAKT + IDPRODNR +          
000700*                                  IDKOLLI)                               
000800*                                 SEKUNDÄR NYCKEL: WDL5ASEQ               
000900*                                 (IDSHIPM)                               
001000     03 SEQA-IDSHIPM         PIC 9(7).                                    
001100*                                 SKEPPNINGSNUMMER                        
001200*                                 SHIPMENT NO                             
001300     03 SEQA-IDFAKT          PIC S9(7)           COMP-3.                  
001400*                                 FAKTURANUMMER                           
001500*                                 INVOICE NO.                             
001600     03 SEQA-IDPRODNR        PIC S9(7)           COMP-3.                  
001700*                                 PRODUKTIONSNUMMER                       
001800*                                 PRODUCTION NUMBER                       
001900     03 SEQA-IDKOLLI         PIC S9(5)           COMP-3.                  
002000*                                 KOLLINUMMER                             
002100*                                 CASE NUMBER                             
002200     03 SEQA-IDDISTR         PIC S9(5)           COMP-3.                  
002300*                                 DISTRIKTNUMMER                          
002400*                                 DISTRICT NUMBER                         
002500     03 SEQA-TISKEPPN        PIC S9(7)           COMP-3.                  
002600*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
002700*                                 SHIPPING DATE    (YYMMDD)               
002800     03 SEQA-TIFAKT          PIC S9(7)           COMP-3.                  
002900*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
003000*                                 INVOICING DATE   (YYMMDD)               
003100*** END OF VILMAII-COPY LENGTH= 29 BYTES                                  
