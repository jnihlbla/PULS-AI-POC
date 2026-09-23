000100 01  W4797001.                                                            
000200*                                 AVSLUTADE LEV.ANM                       
000300*                                 IDPTYP = 211                            
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDDISTR              PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000     03 IDDC                 PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 IDLEVANM             PIC X(7).                                    
001300*                                 LEVANMNUMMER       IDLEVANM-002         
001400     03 TIM-LEVANM           PIC S9(7)           COMP-3.                  
001500*                                 DATUM LEVERANSANMÄRKN. (ÅÅMMDD)         
001600     03 KDUPPTYP             PIC S9              COMP-3.                  
001700*                                 UPPDATERINGSTYP                         
001800     03 KDTRSTAT             PIC S9              COMP-3.                  
001900*                                 TRANSAKTIONSSTATUS                      
002000     03 IDORDNR              PIC S9(5)           COMP-3.                  
002100*                                 ORDERNUMMER UTGÅR PD90                  
002200     03 IDARTNR              PIC S9(9)           COMP-3.                  
002300*                                 ARTIKELNUMMER                           
002400     03 KVLEVANM             PIC S9(7)           COMP-3.                  
002500*                                 LEVERANSANMÄRKNINGSANTAL                
002600     03 KDANMORS             PIC S9(3)           COMP-3.                  
002700*                                 ORSAK TILL LEVERAN KDANMORS-002         
002800     03 PRARTBTO             PIC S9(7)V9(2)      COMP-3.                  
002900*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
003000     03 FLSKROT              PIC S9              COMP-3.                  
003100*                                 SKROTNING ? (1=JA)  FLSKROT-002         
003200     03 RELANDCO             PIC S9(3)V9(2)      COMP-3.                  
003300*                                 LANDING COST PROCENT                    
003400     03 PREMBHNT             PIC S9(7)V9(2)      COMP-3.                  
003500*                                 EMBALLAGE O HANTERINGSKOST              
003600     03 PRFRAKT              PIC S9(7)V9(2)      COMP-3.                  
003700*                                 FRAKTKOSTNAD                            
003800     03 PRLEGKST             PIC S9(7)V9(2)      COMP-3.                  
003900*                                 LEGALISERINSKOSTNAD                     
004000     03 PRFOERS              PIC S9(7)V9(2)      COMP-3.                  
004100*                                 FÖRSÄKRINGSPREMIE                       
004200     03 PREXPKST             PIC S9(7)V9(2)      COMP-3.                  
004300*                                 EXPEDITIONSKOSTNADER                    
004400     03 IDRETILL             PIC S9(7)           COMP-3.                  
004500*                                 RETURTILLSTÅNDSNUMMER                   
004600     03 TIM-RETILL           PIC S9(7)           COMP-3.                  
004700*                                 DATUM RETURTILLSTÅND (ÅÅMMDD)           
004800     03 KVRETINL             PIC S9(7)           COMP-3.                  
004900*                                 INLAGT ANTAL VID RETUR                  
005000     03 IDKNOTNR             PIC S9(7)           COMP-3.                  
005100*                                 KREDITNOTANUMMER                        
005200     03 TIM-KN               PIC S9(7)           COMP-3.                  
005300*                                 DATUM KREDITNOTA                        
005400     03 TIV-RETREG           PIC S9(5)           COMP-3.                  
005500*                                 REGISTRERINGSDATUM FÖR RETUR            
005600*                                 (ÅÅVVD)                                 
005700*** END OF VILMAII-COPY LENGTH= 96 BYTES                                  
