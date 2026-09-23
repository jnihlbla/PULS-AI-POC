000100 01  SEQB-WDB1B1.                                                         
000200*                                 KUNDREGISTER BETALARE                   
000300*                                 PRISOMRÅDE                              
000400*                                 SEKUNDÄRT INDEX TILL WDB101             
000500*                                 FYSISK NYCKEL: WDB1B1KY                 
000600*                                 (IDLANDX2 + IDMARKBO +                  
000700*                                  IDPARTNR + IDFTG)                      
000800*                                 SEKUNDÄR NYCKEL: WDB1BSEQ               
000900*                                 (IDLANDX2)                              
001000     03 SEQB-IDLANDX2        PIC X(2).                                    
001100*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001200*                                 2-LETTER CODE FOR COUNTRY               
001300     03 SEQB-IDMARKBO        PIC X.                                       
001400*                                 MARKNADSBOLAGSKOD                       
001500*                                                                         
001600*                                 MARKET COMPANY CODE                     
001700*                                                                         
001800*                                 A = RENAULT AND LANDROVER               
001900*                                                                         
002000*                                 B = BASE AND EUROPE                     
002100*                                                                         
002200*                                 C = VCOC                                
002300*                                                                         
002400*                                 D = VCSA                                
002500*                                                                         
002600*                                 E = VCNA                                
002700*                                                                         
002800*                                 F = ASIA                                
002900*                                                                         
003000*                                 G = VCOC-EAST                           
003100*                                                                         
003200     03 SEQB-IDPARTNR        PIC X(9).                                    
003300*                                 PARTNERNUMMER                           
003400*                                 PARTNER NO                              
003500     03 SEQB-IDFTG           PIC 9(2).                                    
003600*                                 FÖRETAGSID EKONOM REDOVISNING           
003700*                                 COMPANY IDENTITY ACCOUNTING             
003800     03 SEQB-BEBETRAD-1      PIC X(35).                                   
003900*                                 DEL AV BETALNINGSANSVARIGS NAMN         
004000*                                 PART OF FINANCIAL CUSTOMER NAME         
004100     03 SEQB-IDWDB101        PIC X(11).                                   
004200*                                 NYCKEL TILL WDB101                      
004300*                                 KEY TO WDB101                           
004400*** END OF VILMAII-COPY LENGTH= 60 BYTES                                  
