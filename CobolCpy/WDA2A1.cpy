000100 01  SEQA-WDA2A1.                                                         
000200*                                 LEVERANSANMÄRKNING                      
000300*                                 STATUS                                  
000400*                                 FYSISK NYCKEL: WDA2A1KY                 
000500*                                 (KDLEVANM+IDFTG+IDLEVANM)               
000600*                                 SEKUNDÄR NYCKEL: WDA2ASEQ               
000700*                                 (KDLEVANM+IDFTG)                        
000800     03 SEQA-KDLEVANM        PIC X.                                       
000900*                                 STATUS LEVERANSANMÄRKNING               
001000*                                 STATUS DISCREPANCY                      
001100     03 SEQA-IDFTG           PIC 9(2).                                    
001200*                                 FÖRETAGSID EKONOM REDOVISNING           
001300*                                 COMPANY IDENTITY ACCOUNTING             
001400     03 SEQA-IDLEVANM.                                                    
001500*                                 LEVERANSANMÄRKNINGSIDENTITET            
001600*                                 DISCREPANCY REPORT IDENTITY             
001700        05 SEQA-IDDISTR      PIC S9(5)           COMP-3.                  
001800*                                 DISTRIKTNUMMER                          
001900*                                 DISTRICT NUMBER                         
002000        05 SEQA-IDKUNDNR     PIC S9(7)           COMP-3.                  
002100*                                 KUNDNUMMER                              
002200*                                 CUSTOMER NO                             
002300        05 SEQA-IDRAPPNR     PIC 9(7).                                    
002400*                                 RAPPORT NUMMER                          
002500*                                 DISCREPANCY REPORT NUMBER               
002600     03 SEQA-IDUSER-ADM      PIC X(8).                                    
002700*                                 ANVÄNDAR-ID ADMINISTRATIV KONTR         
002800*                                 USER ID ADMINISTRATIVE INSPEC.          
002900     03 SEQA-KDLEVATT        PIC 9.                                       
003000*                                 ATTESTERING KOD LEVERANSANM.            
003100*                                 ATTEST CODE DISCREPANCY REPORT          
003200     03 SEQA-BEANST          PIC X(25).                                   
003300*                                 ANSTÄLLDS NAMN                          
003400*                                 NAME OF EMPLOYED                        
003500*** END OF VILMAII-COPY LENGTH= 51 BYTES                                  
