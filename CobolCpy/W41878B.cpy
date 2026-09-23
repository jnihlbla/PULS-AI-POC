000100 01  W41878B.                                                             
000200*                                 RADPOST C1 K-NOTA VIA 713               
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
001600     03 BEART                PIC X(15).                                   
001700*                                 ARTIKELBENÄMNING      BEART-002         
001800     03 IDARTNR              PIC S9(9)           COMP-3.                  
001900*                                 ARTIKELNUMMER                           
002000     03 IDFAKT               PIC S9(7)           COMP-3.                  
002100*                                 FAKTURANUMMER                           
002200     03 IDKNOTNR             PIC S9(7)           COMP-3.                  
002300*                                 KREDITNOTANUMMER                        
002400     03 KDANMORS             PIC X(2).                                    
002500*                                 ORSAK TILL LEVERANSANMÄRKNING           
002600     03 KDFAKTYP             PIC X.                                       
002700*                                 FAKTURATYP                              
002800     03 KVKREANT             PIC S9(7)           COMP-3.                  
002900*                                 KREDITERAT ANTAL                        
003000     03 KVLEVANM             PIC S9(7)           COMP-3.                  
003100*                                 LEVERANSANMÄRKNINGSANTAL                
003200     03 PRARTBTO             PIC S9(7)V9(2)      COMP-3.                  
003300*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
003400     03 TIFAKT               PIC S9(7)           COMP-3.                  
003500*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
003600     03 SUKRENTO-RAD         PIC S9(11)V9(2)     COMP-3.                  
003700*                                 KREDITERAT VARUVÄRDE NETTO              
003800     03 PRLANDCO-RAD         PIC S9(7)V9(2)      COMP-3.                  
003900*                                 LANDING COST                            
004000     03 FLLSBOK              PIC X.                                       
004100*                                 LAGERAVBOKNING                          
004200     03 IDANALYS             PIC X(12).                                   
004300*                                 ANALYSNUMMER                            
004400     03 IDKONTO              PIC S9(11)          COMP-3.                  
004500*                                 KONTO                                   
004600     03 IDKST                PIC X(10).                                   
004700*                                 KOSTNADSSTÄLLE                          
004800*** END OF VILMAII-COPY LENGTH= 108 BYTES                                 
