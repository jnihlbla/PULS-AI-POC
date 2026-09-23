000100 01  SEQA-W6D1A1.                                                         
000200*                                 INLEVERANSREGISTER                      
000300*                                 SEKUNDÄRT INDEX TILL W6D101             
000400*                                 NYCKLAR I SEKVENS +                     
000500*                                 LASTBÄRAR INGÅNG                        
000600*                                 TAS BORT NÄR SÄNDNINGEN                 
000700*                                 MOTTAGITS                               
000800*                                 EXIT: INDEX FINNS NÄR                   
000900*                                 TIINLMOT  = ZERO                        
001000*                                 FYSISK NYCKEL: W6D1A1KY                 
001100*                                 (IDDC, IDLEVNR,  IDFS,                  
001200*                                  TIAVIDAT, IDLBBET)                     
001300*                                 SECONDARY NYCKEL: W6D1ASEQ              
001400*                                 (IDDC, IDLEVNR,  IDFS,                  
001500*                                  TIAVIDAT, IDLBBET)                     
001600     03 SEQA-IDDC            PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800*                                 WAREHOUSE IDENTIFIER                    
001900     03 SEQA-IDLEVNR         PIC X(5).                                    
002000*                                 LEVERANTÖRNUMMER                        
002100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002200     03 SEQA-IDFS            PIC X(8).                                    
002300*                                 FÖLJESEDELSNUMMER ENL ODETTE            
002400*                                 ADVICE NOTE NUMBER ODETTE               
002500     03 SEQA-TIAVIDAT        PIC S9(7)           COMP-3.                  
002600*                                 AVISERINGSDATUM (YYMMDD)                
002700*                                 ADVICE NOTE DATE                        
002800     03 SEQA-IDLBBET         PIC X(12).                                   
002900*                                 LASTBÄRARBETECKNING                     
003000*                                 TRAILER NUMBER                          
003100*** END OF VILMAII-COPY LENGTH= 31 BYTES                                  
