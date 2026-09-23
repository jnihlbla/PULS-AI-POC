000100 01  W425SU6-CTX.                                                         
000200*                                 FAKTURAHUVUD1: IDFAKT                   
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 IDDISTR              PIC 9(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 IDKUNDNR             PIC 9(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 IDSUPPL              PIC 9(5).                                    
001000*                                 LEVERANSNR TILL ÅTERFÖRSÄLJARE          
001100     03 IDFAKT               PIC 9(7).                                    
001200*                                 FAKTURANUMMER                           
001300     03 TIFAKT               PIC 9(6).                                    
001400*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
001500     03 SUFKTBEL             PIC 9(8)V9(2).                               
001600*                                 SUMMA FAKTURERAT BELOPP                 
001700     03 PREMBHNT             PIC 9(8)V9(2).                               
001800*                                 EMBALLAGE O HANTERINGSKOST              
001900     03 PRFRAKT              PIC 9(8)V9(2).                               
002000*                                 FRAKTKOSTNAD                            
002100     03 PRFOERS              PIC 9(8)V9(2).                               
002200*                                 FÖRSÄKRINGSPREMIE                       
002300     03 KDFRAKT              PIC 9(2).                                    
002400*                                 FRAKTSÄTT DC TILL KUND                  
002500     03 KDFAKTYP             PIC X.                                       
002600*                                 FAKTURATYP                              
002700     03 PRKURS               PIC 9(6)V9(5).                               
002800*                                 VALUTAKURS                              
002900     03 KDVALUTA             PIC 9(3).                                    
003000*                                 VALUTAKOD                               
003100     03 SUFAKTBEL-UTL        PIC 9(9)V9(2).                               
003200*                                 SUMMA FAKTURERAT BELOPP                 
003300     03 TIAAMMDD             PIC 9(6).                                    
003400*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
003500     03 TIKLOCK              PIC 9(8).                                    
003600*                                 KLOCKSLAG (TTMMSSTH)                    
003700     03 FILLERX1             PIC X.                                       
003800*** END OF VILMAII-COPY LENGTH= 114 BYTES                                 
