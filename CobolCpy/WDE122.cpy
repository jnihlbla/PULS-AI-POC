000100 01  TILL-WDE122.                                                         
000200*                                 TRANSPORTRELEASEREGISTER                
000300*                                 TILLÄGSKOSTNADER                        
000400*                                 FYSISK NYCKEL: KDSEGKEY                 
000500     03 TILL-KDSEGKEY        PIC X.                                       
000600*                                 TEKNISK SEGMENT-NYCKEL                  
000700*                                 TECHNICAL SEGMENT KEY                   
000800     03 TILL-IDSIGILL        PIC X(15).                                   
000900*                                 SIGILL IDENTITET                        
001000*                                 SEAL IDENTITY                           
001100     03 TILL-FILLER          PIC X(10).                                   
001200     03 TILL-BESLULEV        PIC X(5).                                    
001300*                                 SLUTLEVERANS                            
001400*                                 FINAL DELIVERY                          
001500     03 TILL-FILLER1         PIC X(15).                                   
001600     03 TILL-IDBOKN          PIC X(15).                                   
001700*                                 BOKNINGSNUMMER                          
001800*                                 BOOKING NUMBER                          
001900     03 TILL-IDLC            PIC X(15).                                   
002000*                                 LC-NUMMER                               
002100*                                 LETTER OF CREDIT NO                     
002200     03 TILL-IDLICENS        PIC X(15).                                   
002300*                                 LICENSNUMMER                            
002400*                                 LICENCENO                               
002500     03 TILL-IDVCERT         PIC X(16).                                   
002600*                                 VARUCERTIFIKATNUMMER                    
002700*                                 MOVEMENT CERTIFICATE NO                 
002800     03 TILL-KDVALISO-MAN    PIC X(3).                                    
002900*                                 VALUTAKOD ENLIGT ISO-ST, ANGIVE         
003000*                                 N AV ANVÄNDARE                          
003100*                                 CURRENCY CODE BY ISO-STANDARD,          
003200*                                 UPDATED BAY USER                        
003300     03 TILL-PRFRAKT         PIC S9(7)V9(2)      COMP-3.                  
003400*                                 FRAKTKOSTNAD                            
003500*                                 FREIGHT COST                            
003600     03 TILL-PRFOERS         PIC S9(7)V9(2)      COMP-3.                  
003700*                                 FÖRSÄKRINGSPREMIE                       
003800*                                 INSURANCE FEE                           
003900     03 TILL-PRKURS-MAN      PIC S9(6)V9(5)      COMP-3.                  
004000*                                 VALUTAKURS ANGIVEN AV ANVÄNDARE         
004100*                                 EXCHANGE RATE, UPDATED BY USER          
004200     03 TILL-PRLEGKST        PIC S9(7)V9(2)      COMP-3.                  
004300*                                 LEGALISERINSKOSTNAD                     
004400*                                 LEGALIZATION FEE                        
004500     03 TILL-RELEGKST        PIC S9(2)V9(1)      COMP-3.                  
004600*                                 LEGALISERINGSKOSTNAD PROCENT            
004700*                                 LEGALIS. COST PERC.                     
004800     03 TILL-PREMBHNT        PIC S9(7)V9(2)      COMP-3.                  
004900*                                 EMBALLAGE O HANTERINGSKOST              
005000*                                 PACKING O HANDL COSTS                   
005100     03 TILL-REEMBHNT        PIC S9(2)V9(1)      COMP-3.                  
005200*                                 EMB OCH HANTERINGSKOST (%)              
005300*                                 PACKING AND HANDLING (%)                
005400     03 TILL-PRAVDRAG        PIC S9(7)V9(2)      COMP-3.                  
005500*                                 AVDRAGSBELOPP                           
005600*                                 DEDUCTION                               
005700     03 TILL-REAVDRAG        PIC S9(2)V9(1)      COMP-3.                  
005800*                                 AVDRAGSPROCENT                          
005900*                                 DEDUCTION PERCENT                       
006000     03 TILL-REFOERS         PIC S9(2)V9(3)      COMP-3.                  
006100*                                 FÖRSÄKRINGKOSTNADSFAKTOR                
006200*                                 INSURANCE COSTS                         
006300     03 TILL-REOVKOFF        PIC S9(2)V9(1)      COMP-3.                  
006400*                                 ÖVERFÖRSÄKRINGSKOEFFICIENT              
006500*                                 OVER INSURANCE COEFFICIENT              
006600     03 TILL-TISKEPPN-MAN    PIC S9(7)           COMP-3.                  
006700*                                 SKEPPNINGSDATUM MAN  (ÅÅMMDD)           
006800*                                 SHIPPING DATE MAN    (YYMMDD)           
006900*** END OF VILMAII-COPY LENGTH= 156 BYTES                                 
