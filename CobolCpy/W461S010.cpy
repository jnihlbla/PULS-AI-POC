000100 01  FHUV-W461S010.                                                       
000200*                                 SAMMANSLAGNING AV SORTDEL OCH           
000300*                                 FAKTURAHUVUD INFO TILL NOAC             
000400     03 FHUV-SOR0-IDDISTR    PIC S9(5)           COMP-3.                  
000500*                                 DISTRIKTNUMMER                          
000600     03 FHUV-SOR0-IDKUNDNR   PIC S9(7)           COMP-3.                  
000700*                                 KUNDNUMMER                              
000800     03 FHUV-SOR0-IDRONR     PIC S9(7)           COMP-3.                  
000900*                                 RESTORDERNUMMER      IDRONR-002         
001000     03 FHUV-SOR0-TIRODAT    PIC S9(7)           COMP-3.                  
001100*                                 RESTORDERDATUM         (ÅÅMMDD)         
001200     03 FHUV-SOR0-IDPTYP     PIC X(3).                                    
001300*                                 POSTTYP                                 
001400     03 FHUV-SOR0-IDLOPNR    PIC S9(5)           COMP-3.                  
001500*                                 LÖPNUMMER          IDLOPNR-002          
001600     03 FHUV-W461010.                                                     
001700*                                 FAKTURAHUVUD TILL NOAC PT-010           
001800        05 FHUV-IDPTYP       PIC X(3).                                    
001900*                                 POSTTYP                                 
002000        05 FHUV-IDDISTR      PIC S9(5)           COMP-3.                  
002100*                                 DISTRIKTNUMMER                          
002200        05 FHUV-IDKUNDNR     PIC S9(7)           COMP-3.                  
002300*                                 KUNDNUMMER                              
002400        05 FHUV-KDFAKTYP     PIC X.                                       
002500*                                 FAKTURATYP                              
002600        05 FHUV-IDFAKT       PIC S9(7)           COMP-3.                  
002700*                                 FAKTURANUMMER                           
002800        05 FHUV-KDSORT2      PIC S9(3)           COMP-3.                  
002900*                                 SORTERINGSFÄLT                          
003000        05 FHUV-IDPRODNR     PIC S9(7)           COMP-3.                  
003100*                                 PRODUKTIONSNUMMER                       
003200        05 FHUV-IDKOLLI      PIC S9(5)           COMP-3.                  
003300*                                 KOLLINUMMER                             
003400        05 FHUV-IDARTNR      PIC S9(9)           COMP-3.                  
003500*                                 ARTIKELNUMMER                           
003600        05 FHUV-IDORDNR      PIC S9(7)           COMP-3.                  
003700*                                 ORDERNR             IDORDNR-002         
003800        05 FHUV-TIFAKT       PIC S9(7)           COMP-3.                  
003900*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
004000        05 FHUV-IDDC         PIC X(2).                                    
004100*                                 IDENTIFIERARE LAGER                     
004200        05 FHUV-IDFRASED     PIC X(15).                                   
004300*                                 FRAKTSEDELSNUMMER                       
004400        05 FHUV-SUFKTBEL     PIC S9(9)V9(2)      COMP-3.                  
004500*                                 SUMMA FAKTURERAT BELOPP                 
004600        05 FHUV-KDVALISO     PIC X(3).                                    
004700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004800        05 FHUV-PRKURS       PIC S9(6)V9(5)      COMP-3.                  
004900*                                 VALUTAKURS                              
005000        05 FHUV-SUFKTUTL     PIC S9(11)V9(2)     COMP-3.                  
005100*                                 FAKTURABELOPP I UTLÄNDSK VALUTA         
005200        05 FHUV-KDFAKNOT     PIC X(2).                                    
005300*                                 FAKTURA NOTERINGAR                      
005400        05 FHUV-PRAVDRAG     PIC S9(7)V9(2)      COMP-3.                  
005500*                                 AVDRAGSBELOPP                           
005600        05 FHUV-PREMBHNT     PIC S9(7)V9(2)      COMP-3.                  
005700*                                 EMBALLAGE O HANTERINGSKOST              
005800        05 FHUV-PRFOERS      PIC S9(7)V9(2)      COMP-3.                  
005900*                                 FÖRSÄKRINGSPREMIE                       
006000        05 FHUV-PRFRAKT      PIC S9(7)V9(2)      COMP-3.                  
006100*                                 FRAKTKOSTNAD                            
006200        05 FHUV-PRFRAKT-LOC  PIC S9(7)V9(2)      COMP-3.                  
006300*                                 FRAKTKOSTNAD LOKAL VALUTA               
006400        05 FHUV-PRLEGKST     PIC S9(7)V9(2)      COMP-3.                  
006500*                                 LEGALISERINSKOSTNAD                     
006600        05 FHUV-PRMOMS       PIC S9(7)V9(2)      COMP-3.                  
006700*                                 MERVÄRDESSKATT                          
006800        05 FHUV-SUFKTTILL    PIC S9(7)V9(2)      COMP-3.                  
006900*                                 PRISTILLÄGG (KR)                        
007000        05 FHUV-KDFRAKT      PIC S9(3)           COMP-3.                  
007100*                                 FRAKTSÄTT DC TILL KUND                  
007200        05 FHUV-IDTRPTNR     PIC S9(3)           COMP-3.                  
007300*                                 TRANSPORTIDENTITET                      
007400        05 FHUV-IDLBBET      PIC X(12).                                   
007500*                                 LASTBÄRARBETECKNING                     
007600*** END OF VILMAII-COPY LENGTH= 155 BYTES                                 
