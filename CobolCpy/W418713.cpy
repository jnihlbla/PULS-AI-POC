000100 01  W418713.                                                             
000200*                                 RADPOST KREDITNOTA                      
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDLEVANM.                                                         
000700*                                 LEVERANSANMÄRKNINGSIDENTITET            
000800        05 IDDISTR           PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200        05 IDRAPPNR          PIC 9(7).                                    
001300*                                 RAPPORT NUMMER                          
001400     03 IDDC                 PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 IDKNOTNR             PIC S9(7)           COMP-3.                  
001700*                                 KREDITNOTANUMMER                        
001800     03 IDARTNR              PIC S9(9)           COMP-3.                  
001900*                                 ARTIKELNUMMER                           
002000     03 KDFAKTYP             PIC X.                                       
002100*                                 FAKTURATYP                              
002200     03 IDFAKT               PIC S9(7)           COMP-3.                  
002300*                                 FAKTURANUMMER                           
002400     03 TIFAKT               PIC S9(7)           COMP-3.                  
002500*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
002600     03 KVLEVANM             PIC S9(7)           COMP-3.                  
002700*                                 LEVERANSANMÄRKNINGSANTAL                
002800     03 KDANMORS             PIC X(2).                                    
002900*                                 ORSAK TILL LEVERANSANMÄRKNING           
003000     03 KVKREANT             PIC S9(7)           COMP-3.                  
003100*                                 KREDITERAT ANTAL                        
003200     03 PRARTBTO             PIC S9(7)V9(2)      COMP-3.                  
003300*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
003400     03 FLLSBOK              PIC X.                                       
003500*                                 LAGERAVBOKNING                          
003600     03 IDANALYS             PIC X(12).                                   
003700*                                 ANALYSNUMMER                            
003800     03 IDKONTO              PIC S9(11)          COMP-3.                  
003900*                                 KONTO                                   
004000     03 IDKST                PIC X(10).                                   
004100*                                 KOSTNADSSTÄLLE                          
004200*** END OF VILMAII-COPY LENGTH= 81 BYTES                                  
