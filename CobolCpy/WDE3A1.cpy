000100 01  SEQA-WDE3A1.                                                         
000200*                                 REFILL REGISTER ORDEFÖRSLAG             
000300*                                 SEKUNDÄRT INDEX TILL WDE301             
000400*                                 INDEX FINNS NÄR KDREFORS = P            
000500*                                   OCH KDREFTYP = A/B/C/L                
000600*                                 FYSISK NYCKEL: WDE3A1KY                 
000700*                                 (KDREFTYP + IDPERSON +                  
000800*                                  IDARTNR + IDDC)                        
000900*                                 SEKUNDÄR NYCKEL: WDE3ASEQ               
001000*                                 (KDREFTYP + IDPERSON + IDARTNR)         
001100     03 SEQA-KDREFTYP        PIC X.                                       
001200*                                 TYP AV REFILLORDER                      
001300*                                 TYPE OF REFILLINGORDER                  
001400     03 SEQA-IDPERSON-BUY    PIC S9(3)           COMP-3.                  
001500*                                 PERSONKOD REFILLANSVARIG                
001600*                                 REFILL RESPONSIBLE ID                   
001700     03 SEQA-IDARTNR         PIC S9(9)           COMP-3.                  
001800*                                 ARTIKELNUMMER                           
001900*                                 PART NUMBER                             
002000     03 SEQA-IDDC            PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200*                                 WAREHOUSE IDENTIFIER                    
002300     03 SEQA-KDREFORS        PIC X.                                       
002400*                                 REFILL ORDER STATUSKOD                  
002500*                                 REFILL ORDER STATUS CODE                
002600*                                  P = PROPOSAL                           
002700*                                  O = ORDER                              
002800     03 SEQA-IDWDE301        PIC X(13).                                   
002900*                                 NYCKEL TILL WDE301                      
003000*                                 KEY TO WDE301                           
003100*** END OF VILMAII-COPY LENGTH= 24 BYTES                                  
