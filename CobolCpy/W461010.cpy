000100 01  FHUV-W461010.                                                        
000200*                                 FAKTURAHUVUD TILL NOAC PT-010           
000300     03 FHUV-IDPTYP          PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 FHUV-IDDISTR         PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700     03 FHUV-IDKUNDNR        PIC S9(7)           COMP-3.                  
000800*                                 KUNDNUMMER                              
000900     03 FHUV-KDFAKTYP        PIC X.                                       
001000*                                 FAKTURATYP                              
001100     03 FHUV-IDFAKT          PIC S9(7)           COMP-3.                  
001200*                                 FAKTURANUMMER                           
001300     03 FHUV-KDSORT2         PIC S9(3)           COMP-3.                  
001400*                                 SORTERINGSFÄLT                          
001500     03 FHUV-IDPRODNR        PIC S9(7)           COMP-3.                  
001600*                                 PRODUKTIONSNUMMER                       
001700     03 FHUV-IDKOLLI         PIC S9(5)           COMP-3.                  
001800*                                 KOLLINUMMER                             
001900     03 FHUV-IDARTNR         PIC S9(9)           COMP-3.                  
002000*                                 ARTIKELNUMMER                           
002100     03 FHUV-IDORDNR         PIC S9(7)           COMP-3.                  
002200*                                 ORDERNR             IDORDNR-002         
002300     03 FHUV-TIFAKT          PIC S9(7)           COMP-3.                  
002400*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
002500     03 FHUV-IDDC            PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700     03 FHUV-IDFRASED        PIC X(15).                                   
002800*                                 FRAKTSEDELSNUMMER                       
002900     03 FHUV-SUFKTBEL        PIC S9(9)V9(2)      COMP-3.                  
003000*                                 SUMMA FAKTURERAT BELOPP                 
003100     03 FHUV-KDVALISO        PIC X(3).                                    
003200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003300     03 FHUV-PRKURS          PIC S9(6)V9(5)      COMP-3.                  
003400*                                 VALUTAKURS                              
003500     03 FHUV-SUFKTUTL        PIC S9(11)V9(2)     COMP-3.                  
003600*                                 FAKTURABELOPP I UTLÄNDSK VALUTA         
003700     03 FHUV-KDFAKNOT        PIC X(2).                                    
003800*                                 FAKTURA NOTERINGAR                      
003900     03 FHUV-PRAVDRAG        PIC S9(7)V9(2)      COMP-3.                  
004000*                                 AVDRAGSBELOPP                           
004100     03 FHUV-PREMBHNT        PIC S9(7)V9(2)      COMP-3.                  
004200*                                 EMBALLAGE O HANTERINGSKOST              
004300     03 FHUV-PRFOERS         PIC S9(7)V9(2)      COMP-3.                  
004400*                                 FÖRSÄKRINGSPREMIE                       
004500     03 FHUV-PRFRAKT         PIC S9(7)V9(2)      COMP-3.                  
004600*                                 FRAKTKOSTNAD                            
004700     03 FHUV-PRFRAKT-LOC     PIC S9(7)V9(2)      COMP-3.                  
004800*                                 FRAKTKOSTNAD LOKAL VALUTA               
004900     03 FHUV-PRLEGKST        PIC S9(7)V9(2)      COMP-3.                  
005000*                                 LEGALISERINSKOSTNAD                     
005100     03 FHUV-PRMOMS          PIC S9(7)V9(2)      COMP-3.                  
005200*                                 MERVÄRDESSKATT                          
005300     03 FHUV-SUFKTTILL       PIC S9(7)V9(2)      COMP-3.                  
005400*                                 PRISTILLÄGG (KR)                        
005500     03 FHUV-KDFRAKT         PIC S9(3)           COMP-3.                  
005600*                                 FRAKTSÄTT DC TILL KUND                  
005700     03 FHUV-IDTRPTNR        PIC S9(3)           COMP-3.                  
005800*                                 TRANSPORTIDENTITET                      
005900     03 FHUV-IDLBBET         PIC X(12).                                   
006000*                                 LASTBÄRARBETECKNING                     
006100*** END OF VILMAII-COPY LENGTH= 134 BYTES                                 
