000100 01  W425SUB-CTX.                                                         
000200*                                 INFORMATION OM KOLLITRANS               
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDDISTR              PIC 9(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 IDKUNDNR             PIC 9(6).                                    
000900*                                 KUNDNUMMER                              
001000     03 IDSUPPL              PIC S9(5)           COMP-3.                  
001100*                                 LEVERANSNR TILL ÅTERFÖRSÄLJARE          
001200     03 IDORDNR-002          PIC S9(7)           COMP-3.                  
001300*                                 ORDERNR             IDORDNR-002         
001400     03 IDFAKT               PIC S9(7)           COMP-3.                  
001500*                                 FAKTURANUMMER                           
001600     03 TIFAKT               PIC S9(7)           COMP-3.                  
001700*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
001800     03 IDKOLLI              PIC S9(5)           COMP-3.                  
001900*                                 KOLLINUMMER                             
002000     03 IDLBBET              PIC X(12).                                   
002100*                                 LASTBÄRARBETECKNING                     
002200     03 VKORDBTO-KOLLI       PIC S9(6)V9(1)      COMP-3.                  
002300*                                 ORDERVIKT BRUTTO PER KOLLI              
002400     03 VLORDBTO-KOLLI       PIC S9(4)V9(3)      COMP-3.                  
002500*                                 ORDERVOLYM BRUTTO KOLLI                 
002600     03 TIAAMMDD             PIC S9(7)           COMP-3.                  
002700*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
002800     03 TIKLOCK              PIC S9(9)           COMP-3.                  
002900*                                 KLOCKSLAG (TTMMSSTH)                    
003000     03 FILLERX54            PIC X(54).                                   
003100*** END OF VILMAII-COPY LENGTH= 114 BYTES                                 
