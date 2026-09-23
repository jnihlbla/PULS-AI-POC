000100 01  SEQA-WDB1A1.                                                         
000200*                                 KUNDREGISTER BETALARE                   
000300*                                 PRISOMRÅDE                              
000400*                                 SEKUNDÄRT INDEX TILL WDB101             
000500*                                 FYSISK NYCKEL: WDB1A1KY                 
000600*                                 (IDPROMR + IDPARTNR + IDFTG)            
000700*                                 SEKUNDÄR NYCKEL: WDB1ASEQ               
000800*                                 (IDPROMR)                               
000900     03 SEQA-IDPROMR.                                                     
001000*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
001100*                                 PRICE AREA                              
001200        05 SEQA-IDMARKBO     PIC X.                                       
001300*                                 MARKNADSBOLAGSKOD                       
001400*                                                                         
001500*                                 MARKET COMPANY CODE                     
001600*                                                                         
001700*                                 A = RENAULT AND LANDROVER               
001800*                                                                         
001900*                                 B = BASE AND EUROPE                     
002000*                                                                         
002100*                                 C = VCOC                                
002200*                                                                         
002300*                                 D = VCSA                                
002400*                                                                         
002500*                                 E = VCNA                                
002600*                                                                         
002700*                                 F = ASIA                                
002800*                                                                         
002900*                                 G = VCOC-EAST                           
003000*                                                                         
003100        05 SEQA-IDPROMRN     PIC X(2).                                    
003200*                                 PRISOMRÅDE LÖPNUMMER                    
003300*                                 PRICE AREA SERIALNUMBER                 
003400     03 SEQA-IDPARTNR        PIC X(9).                                    
003500*                                 PARTNERNUMMER                           
003600*                                 PARTNER NO                              
003700     03 SEQA-IDFTG           PIC 9(2).                                    
003800*                                 FÖRETAGSID EKONOM REDOVISNING           
003900*                                 COMPANY IDENTITY ACCOUNTING             
004000     03 SEQA-IDWDB101        PIC X(11).                                   
004100*                                 NYCKEL TILL WDB101                      
004200*                                 KEY TO WDB101                           
004300*** END OF VILMAII-COPY LENGTH= 25 BYTES                                  
