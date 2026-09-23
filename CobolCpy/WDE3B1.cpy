000100 01  SEQB-WDE3B1.                                                         
000200*                                 REFILL REGISTER ORDEFÖRSLAG             
000300*                                 SEKUNDÄRT INDEX TILL WDE301             
000400*                                 INDEX FINNS NÄR KDREFTYP =              
000500*                                   = A/B/C/L/T                           
000600*                                 FYSISK NYCKEL: WDE3B1KY                 
000700*                                 (KDREFTYP + IDARTNR + IDDC + )          
000800*                                 (IDPERSON-BUY)                          
000900*                                 SEKUNDÄR NYCKEL: WDE3BSEQ               
001000*                                 (KDREFTYP + IDARTNR)                    
001100     03 SEQB-KDREFTYP        PIC X.                                       
001200*                                 TYP AV REFILLORDER                      
001300*                                 TYPE OF REFILLINGORDER                  
001400     03 SEQB-IDARTNR         PIC S9(9)           COMP-3.                  
001500*                                 ARTIKELNUMMER                           
001600*                                 PART NUMBER                             
001700     03 SEQB-IDDC            PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900*                                 WAREHOUSE IDENTIFIER                    
002000     03 SEQB-IDPERSON-BUY    PIC S9(3)           COMP-3.                  
002100*                                 PERSONKOD REFILLANSVARIG                
002200*                                 REFILL RESPONSIBLE ID                   
002300     03 SEQB-KDREFORS        PIC X.                                       
002400*                                 REFILL ORDER STATUSKOD                  
002500*                                 REFILL ORDER STATUS CODE                
002600     03 SEQB-IDWDE301        PIC X(13).                                   
002700*                                 NYCKEL TILL WDE301                      
002800*                                 KEY TO WDE301                           
002900*** END OF VILMAII-COPY LENGTH= 24 BYTES                                  
