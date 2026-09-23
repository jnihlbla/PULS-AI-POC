000100 01  KRED-W461021.                                                        
000200*                                 KREDITERINGSTRANSAR  FÖR ORDER          
000300     03 KRED-IDPTYP          PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 KRED-IDDISTR         PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700     03 KRED-IDKUNDNR        PIC S9(7)           COMP-3.                  
000800*                                 KUNDNUMMER                              
000900     03 KRED-IDDC            PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 KRED-IDRAPPNR        PIC 9(7).                                    
001200*                                 RAPPORT NUMMER                          
001300     03 KRED-IDKNOTNR        PIC S9(7)           COMP-3.                  
001400*                                 KREDITNOTANUMMER                        
001500     03 KRED-TIM-KN          PIC S9(7)           COMP-3.                  
001600*                                 DATUM KREDITNOTA                        
001700     03 KRED-IDFAKT          PIC S9(7)           COMP-3.                  
001800*                                 FAKTURANUMMER                           
001900     03 KRED-IDORDNR         PIC S9(7)           COMP-3.                  
002000*                                 ORDERNR             IDORDNR-002         
002100     03 KRED-IDARTNR         PIC S9(9)           COMP-3.                  
002200*                                 ARTIKELNUMMER                           
002300     03 KRED-REKSIFFR        PIC S9              COMP-3.                  
002400*                                 KONTROLLSIFFRA                          
002500     03 KRED-IDRADNR         PIC S9(5)           COMP-3.                  
002600*                                 RADNUMMER                               
002700     03 KRED-KDANMORS        PIC S9(3)           COMP-3.                  
002800*                                 ORSAK TILL LEVERAN KDANMORS-002         
002900     03 KRED-KDPSLLOC        PIC 9(2).                                    
003000*                                 PRODUKTSLAG LOKALT                      
003100     03 KRED-KVKREANT        PIC S9(7)           COMP-3.                  
003200*                                 KREDITERAT ANTAL                        
003300     03 KRED-PRARTBTO        PIC S9(7)V9(2)      COMP-3.                  
003400*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
003500     03 KRED-PRAVCOST        PIC S9(7)V9(2)      COMP-3.                  
003600*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
003700     03 KRED-PRAVCOST-CORE   PIC S9(7)V9(2)      COMP-3.                  
003800*                                 OBJEKTETS MEDELVÄRDESKOSTNAD I          
003900*                                 UTL.VALUTA                              
004000     03 KRED-PREMBHNT        PIC S9(7)V9(2)      COMP-3.                  
004100*                                 EMBALLAGE O HANTERINGSKOST              
004200     03 KRED-PRFRAKT         PIC S9(7)V9(2)      COMP-3.                  
004300*                                 FRAKTKOSTNAD                            
004400     03 KRED-PRLEGKST        PIC S9(7)V9(2)      COMP-3.                  
004500*                                 LEGALISERINSKOSTNAD                     
004600     03 KRED-PRFOERS         PIC S9(7)V9(2)      COMP-3.                  
004700*                                 FÖRSÄKRINGSPREMIE                       
004800     03 KRED-PRMOMS          PIC S9(7)V9(2)      COMP-3.                  
004900*                                 MERVÄRDESSKATT                          
005000     03 KRED-PRARTBTO-LOC    PIC S9(7)V9(2)      COMP-3.                  
005100*                                 PRIS I LOKAL VALUTA                     
005200     03 KRED-SUVAT-FAKT      PIC S9(11)V9(2)     COMP-3.                  
005300*                                 TOTALT MOMSVÄRDE PER FAKTURA/KR         
005400*                                 EDITNOTA                                
005500     03 KRED-KDVALISO        PIC X(3).                                    
005600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005700     03 KRED-KDVAT           PIC X(2).                                    
005800*                                 MOMSKOD                                 
005900     03 KRED-PRARTSTD        PIC S9(7)V9(2)      COMP-3.                  
006000*                                 ARTIKELSTANDARDPRIS                     
006100     03 KRED-PRARTSJK        PIC S9(7)V9(2)      COMP-3.                  
006200*                                 ARTIKELNS SJÄLVKOSTNAD                  
006300     03 KRED-SULNELOC        PIC S9(9)V9(2)      COMP-3.                  
006400*                                 FAKTURARADSUMMA EXKL MOMS               
006500     03 KRED-SUKRENTO        PIC S9(11)V9(2)     COMP-3.                  
006600*                                 KREDITERAT VARUVÄRDE NETTO              
006700     03 KRED-SUKRETOT        PIC S9(11)V9(2)     COMP-3.                  
006800*                                 TOTALT KREDITERAT VÄRDE                 
006900*** END OF VILMAII-COPY LENGTH= 139 BYTES                                 
