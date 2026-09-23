000100 01  W425685.                                                             
000200*                                 FAKTURAHUVUD TILL VR-SYSTEM             
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDDISTR              PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000     03 IDDC                 PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 IDFAKT               PIC S9(7)           COMP-3.                  
001300*                                 FAKTURANUMMER                           
001400     03 KDFAKTYP             PIC X.                                       
001500*                                 FAKTURATYP                              
001600     03 KDFRAKT              PIC S9(3)           COMP-3.                  
001700*                                 FRAKTSÄTT DC TILL KUND                  
001800     03 PREMBHNT             PIC S9(7)V9(2)      COMP-3.                  
001900*                                 EMBALLAGE O HANTERINGSKOST              
002000     03 PRFRAKT              PIC S9(7)V9(2)      COMP-3.                  
002100*                                 FRAKTKOSTNAD                            
002200     03 PRFOERS              PIC S9(7)V9(2)      COMP-3.                  
002300*                                 FÖRSÄKRINGSPREMIE                       
002400     03 SUFKTBEL             PIC S9(9)V9(2)      COMP-3.                  
002500*                                 SUMMA FAKTURERAT BELOPP                 
002600     03 TIFAKT               PIC S9(7)           COMP-3.                  
002700*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
002800     03 PRKURS               PIC S9(6)V9(5)      COMP-3.                  
002900*                                 VALUTAKURS                              
003000     03 TIAAMMDD             PIC S9(7)           COMP-3.                  
003100*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
003200     03 TIKLOCK              PIC S9(9)           COMP-3.                  
003300*                                 KLOCKSLAG (TTMMSSTH)                    
003400*** END OF VILMAII-COPY LENGTH= 59 BYTES                                  
