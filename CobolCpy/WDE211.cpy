000100 01  BGMT-WDE211.                                                         
000200*                                 TRANSPORTRELEASEREGISTER                
000300*                                 FAKTURA -> BILL-IT                      
000400*                                 GODSMOTTAGARE                           
000500*                                 FYSISK NYCKEL: WDE211KY                 
000600*                                 (IDDISTR, IDKUNDNR)                     
000700     03 BGMT-IDDISTR         PIC S9(5)           COMP-3.                  
000800*                                 DISTRIKTNUMMER                          
000900*                                 DISTRICT NUMBER                         
001000     03 BGMT-IDKUNDNR        PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200*                                 CUSTOMER NO                             
001300     03 BGMT-IDPARTNR        PIC X(9).                                    
001400*                                 PARTNERNUMMER                           
001500*                                 PARTNER NO                              
001600     03 BGMT-FLCOD           PIC X.                                       
001700*                                 KONTANTBETALANDE KUND                   
001800*                                 CASH ON DELIVERY CUSTOMER               
001900     03 BGMT-FLSEPINV        PIC X.                                       
002000*                                 SEPARAT FAKTURA                         
002100*                                 SEPARATE INVOICE FLAG                   
002200     03 BGMT-KDLEVVIL        PIC S9              COMP-3.                  
002300*                                 LEVERANSVILLKOR                         
002400*                                 TERMS OF DELIVERY                       
002500     03 BGMT-PRAVDRAG        PIC S9(7)V9(2)      COMP-3.                  
002600*                                 AVDRAGSBELOPP                           
002700*                                 DEDUCTION                               
002800     03 BGMT-PREMBHNT        PIC S9(7)V9(2)      COMP-3.                  
002900*                                 EMBALLAGE O HANTERINGSKOST              
003000*                                 PACKING O HANDL COSTS                   
003100     03 BGMT-PRFOERS         PIC S9(7)V9(2)      COMP-3.                  
003200*                                 FÖRSÄKRINGSPREMIE                       
003300*                                 INSURANCE FEE                           
003400     03 BGMT-PRFRAKT         PIC S9(7)V9(2)      COMP-3.                  
003500*                                 FRAKTKOSTNAD                            
003600*                                 FREIGHT COST                            
003700     03 BGMT-PRLEGKST        PIC S9(7)V9(2)      COMP-3.                  
003800*                                 LEGALISERINSKOSTNAD                     
003900*                                 LEGALIZATION FEE                        
004000     03 BGMT-REAVDRAG        PIC S9(2)V9(1)      COMP-3.                  
004100*                                 AVDRAGSPROCENT                          
004200*                                 DEDUCTION PERCENT                       
004300     03 BGMT-REEMBHNT        PIC S9(2)V9(1)      COMP-3.                  
004400*                                 EMB OCH HANTERINGSKOST (%)              
004500*                                 PACKING AND HANDLING (%)                
004600     03 BGMT-REFOERS         PIC S9(2)V9(3)      COMP-3.                  
004700*                                 FÖRSÄKRINGKOSTNADSFAKTOR                
004800*                                 INSURANCE COSTS                         
004900     03 BGMT-RELEGKST        PIC S9(2)V9(1)      COMP-3.                  
005000*                                 LEGALISERINGSKOSTNAD PROCENT            
005100*                                 LEGALIS. COST PERC.                     
005200     03 BGMT-REOVKOFF        PIC S9(2)V9(1)      COMP-3.                  
005300*                                 ÖVERFÖRSÄKRINGSKOEFFICIENT              
005400*                                 OVER INSURANCE COEFFICIENT              
005500*** END OF VILMAII-COPY LENGTH= 55 BYTES                                  
