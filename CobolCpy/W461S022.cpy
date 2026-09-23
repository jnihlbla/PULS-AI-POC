000100 01  RAD-W461S022.                                                        
000200*                                 SAMMANSLAGNING AV SORTDEL OCH           
000300*                                 KREDIT   POST TILL NOAC                 
000400     03 RAD-SOR0-IDDISTR     PIC S9(5)           COMP-3.                  
000500*                                 DISTRIKTNUMMER                          
000600     03 RAD-SOR0-IDKUNDNR    PIC S9(7)           COMP-3.                  
000700*                                 KUNDNUMMER                              
000800     03 RAD-SOR0-IDRONR      PIC S9(7)           COMP-3.                  
000900*                                 RESTORDERNUMMER      IDRONR-002         
001000     03 RAD-SOR0-TIRODAT     PIC S9(7)           COMP-3.                  
001100*                                 RESTORDERDATUM         (ÅÅMMDD)         
001200     03 RAD-SOR0-IDPTYP      PIC X(3).                                    
001300*                                 POSTTYP                                 
001400     03 RAD-SOR0-IDLOPNR     PIC S9(5)           COMP-3.                  
001500*                                 LÖPNUMMER          IDLOPNR-002          
001600     03 RAD-W461022.                                                      
001700*                                 KREDITERINGSTRANSAR  FÖR ORDER          
001800*                                 FRÅN DISTRIKT 1283                      
001900*                                 FRÅN VR SYSTEMET PT 022                 
002000        05 RAD-IDPTYP        PIC X(3).                                    
002100*                                 POSTTYP                                 
002200        05 RAD-IDDISTR       PIC S9(5)           COMP-3.                  
002300*                                 DISTRIKTNUMMER                          
002400        05 RAD-IDKUNDNR      PIC S9(7)           COMP-3.                  
002500*                                 KUNDNUMMER                              
002600        05 RAD-IDRONR        PIC S9(7)           COMP-3.                  
002700*                                 RESTORDERNUMMER      IDRONR-002         
002800        05 RAD-TIRODAT       PIC S9(7)           COMP-3.                  
002900*                                 RESTORDERDATUM         (ÅÅMMDD)         
003000        05 RAD-IDLOPNR       PIC S9(3)           COMP-3.                  
003100*                                 LÖPNUMMER                               
003200        05 RAD-IDFAKT        PIC S9(7)           COMP-3.                  
003300*                                 FAKTURANUMMER                           
003400        05 RAD-IDORDNR       PIC S9(7)           COMP-3.                  
003500*                                 ORDERNR             IDORDNR-002         
003600        05 RAD-IDRADNR       PIC S9(5)           COMP-3.                  
003700*                                 RADNUMMER                               
003800        05 RAD-IDARTNR       PIC S9(9)           COMP-3.                  
003900*                                 ARTIKELNUMMER                           
004000        05 RAD-REKSIFFR      PIC S9              COMP-3.                  
004100*                                 KONTROLLSIFFRA                          
004200        05 RAD-KDANMORS      PIC S9(3)           COMP-3.                  
004300*                                 ORSAK TILL LEVERAN KDANMORS-002         
004400        05 RAD-KVKREANT      PIC S9(7)           COMP-3.                  
004500*                                 KREDITERAT ANTAL                        
004600        05 RAD-PRARTBTO      PIC S9(7)V9(2)      COMP-3.                  
004700*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
004800        05 RAD-KDPSLLOC      PIC 9(2).                                    
004900*                                 PRODUKTSLAG LOKALT                      
005000        05 RAD-PRAVCOST      PIC S9(7)V9(2)      COMP-3.                  
005100*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
005200        05 RAD-PRAVCOST-CORE PIC S9(7)V9(2)      COMP-3.                  
005300*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
005400        05 FILLER            PIC X(3).                                    
005500        05 RAD-PRARTBTO-LOC  PIC S9(7)V9(2)      COMP-3.                  
005600*                                 PRIS I LOKAL VALUTA                     
005700        05 RAD-KDVAT         PIC X(2).                                    
005800*                                 MOMSKOD                                 
005900        05 RAD-PRMOMS-RAD    PIC S9(7)V9(2)      COMP-3.                  
006000*                                 MERVÄRDESSKATT                          
006100        05 RAD-SULNELOC      PIC S9(9)V9(2)      COMP-3.                  
006200*                                 FAKTURARADSUMMA EXKL MOMS               
006300        05 RAD-PRARTSTD      PIC S9(7)V9(2)      COMP-3.                  
006400*                                 ARTIKELSTANDARDPRIS                     
006500        05 RAD-PRARTSJK      PIC S9(7)V9(2)      COMP-3.                  
006600*                                 ARTIKELNS SJÄLVKOSTNAD                  
006700*** END OF VILMAII-COPY LENGTH= 112 BYTES                                 
