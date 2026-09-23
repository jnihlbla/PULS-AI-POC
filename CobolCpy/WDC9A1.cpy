000100 01  SEQA-WDC9A1-CTX.                                                     
000200*                                 NYA ARTIKLAR KINA                       
000300*                                 SEKUNDÄRT INDEX TILL WDC901             
000400*                                 FYSISK NYCKEL: WDC9A1KY                 
000500*                                 (KDANSKQ + IDANSK +                     
000600*                                  IDARTNR + IDDC)                        
000700*                                 SEKUNDÄR NYCKEL: WDC9ASEQ               
000800*                                 (KDANSKQ + IDANSK)                      
000900*                                                                         
001000     03 SEQA-KDANSKQ         PIC X.                                       
001100*                                 KOD FÖR ANSKAFFARE KÖ                   
001200*                                 PROCURER QUEUE CODE                     
001300     03 SEQA-IDANSK          PIC S9(3)           COMP-3.                  
001400*                                 ANSKAFFARNUMMER                         
001500*                                 PROCURER NO.                            
001600     03 SEQA-IDARTNR         PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800*                                 PART NUMBER                             
001900     03 SEQA-IDDC            PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100*                                 WAREHOUSE IDENTIFIER                    
002200*** END OF VILMAII-COPY LENGTH= 10 BYTES                                  
