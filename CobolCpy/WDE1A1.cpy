000100 01  SEQA-WDE1A1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDE101             
000300*                                 KOMPLETTA SKEPPNINGAR                   
000400*                                 INDEX FINNS NÄR KDKLAR = J              
000500*                                 FYSISK NYCKEL: WDE1A1KY                 
000600*                                 (IDDC, IDTRPTNR, IDLBBET,               
000700*                                  TISKEPPN, TISKPTID)                    
000800*                                 SEKUNDÄR NYCKEL: WDE1ASEQ               
000900*                                 (IDDC, IDTRPTNR, IDLBBET,               
001000*                                  TISKEPPN)                              
001100     03 SEQA-IDDC            PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 SEQA-IDTRPTNR        PIC S9(3)           COMP-3.                  
001500*                                 TRANSPORTIDENTITET                      
001600*                                 TRANSPORT IDENTITY                      
001700     03 SEQA-IDLBBET         PIC X(12).                                   
001800*                                 LASTBÄRARBETECKNING                     
001900*                                 TRAILER NUMBER                          
002000     03 SEQA-TISKEPPN        PIC S9(7)           COMP-3.                  
002100*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
002200*                                 SHIPPING DATE    (YYMMDD)               
002300     03 SEQA-TISKPTID        PIC S9(7)           COMP-3.                  
002400*                                 SKEPPNINGSTID                           
002500     03 SEQA-IDSHIPM         PIC 9(7).                                    
002600*                                 SKEPPNINGSNUMMER                        
002700*                                 SHIPMENT NO                             
002800*** END OF VILMAII-COPY LENGTH= 31 BYTES                                  
