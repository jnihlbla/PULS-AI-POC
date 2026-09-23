000100 01  W475898T.                                                            
000200*                                 DDC-FAKTUROR    - MOMSFIL               
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDDC                 PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 IDVAT-DDC            PIC X(17).                                   
000900*                                 MOMSREGISTRERINGSNUMMER DDC             
001000     03 IDLANDX2             PIC X(2).                                    
001100*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001200     03 IDVAT                PIC X(17).                                   
001300*                                 MOMSREGISTRERINGSNUMMER                 
001400     03 IDDISTR              PIC S9(5)           COMP-3.                  
001500*                                 DISTRIKTNUMMER                          
001600     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001700*                                 KUNDNUMMER                              
001800     03 IDFAKT               PIC S9(7)           COMP-3.                  
001900*                                 FAKTURANUMMER                           
002000     03 TIFAKT               PIC S9(7)           COMP-3.                  
002100*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
002200     03 KDVALISO             PIC X(3).                                    
002300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002400     03 SUFKTBEL             PIC S9(9)V9(2)      COMP-3.                  
002500*                                 SUMMA FAKTURERAT BELOPP                 
002600     03 PRMOMS               PIC S9(7)V9(2)      COMP-3.                  
002700*                                 MERVÄRDESSKATT                          
002800     03 SUFAKTBEL-VAT-LOC    PIC S9(9)V9(2)      COMP-3.                  
002900*                                 SUMMA FAKTURERAT BELOPP                 
003000     03 SUVAT-FAKT-LOC       PIC S9(11)V9(2)     COMP-3.                  
003100*                                 MOMSVÄRDE PER MOMSKOD                   
003200*** END OF VILMAII-COPY LENGTH= 83 BYTES                                  
