000100 01  RKD-W461S023-CTX.                                                    
000200*                                 SAMMANSLAGNING AV SORTDEL OCH           
000300*                                 KREDIT   POST (RKD) TILL NOAC           
000400     03 RKD-SOR0-IDDISTR     PIC S9(5)           COMP-3.                  
000500*                                 DISTRIKTNUMMER                          
000600     03 RKD-SOR0-IDKUNDNR    PIC S9(7)           COMP-3.                  
000700*                                 KUNDNUMMER                              
000800     03 RKD-SOR0-IDRONR      PIC S9(7)           COMP-3.                  
000900*                                 RESTORDERNUMMER      IDRONR-002         
001000     03 RKD-SOR0-TIRODAT     PIC S9(7)           COMP-3.                  
001100*                                 RESTORDERDATUM         (ÅÅMMDD)         
001200     03 RKD-SOR0-IDPTYP      PIC X(3).                                    
001300*                                 POSTTYP                                 
001400     03 RKD-SOR0-IDLOPNR     PIC S9(5)           COMP-3.                  
001500*                                 LÖPNUMMER          IDLOPNR-002          
001600     03 RKD-W461RKDN-CTX.                                                 
001700*                                 KREDITTRANS-RAD                         
001800*                                 AVVISAD ELLER ÄNDRAD                    
001900*                                 RECORD TYP  RKD                         
002000        05 RKD-IDPTYP        PIC X(3).                                    
002100*                                 POSTTYP                                 
002200        05 RKD-IDDISTR       PIC 9(4).                                    
002300*                                 DISTRIKTNUMMER                          
002400        05 RKD-IDKUNDNR      PIC 9(6).                                    
002500*                                 KUNDNUMMER                              
002600        05 RKD-IDDC          PIC X(2).                                    
002700*                                 IDENTIFIERARE LAGER                     
002800        05 RKD-IDRAPPNR      PIC 9(7).                                    
002900*                                 RAPPORT NUMMER                          
003000        05 RKD-IDORDNR       PIC 9(7).                                    
003100*                                 ORDERNR             IDORDNR-002         
003200        05 RKD-IDKOLLI       PIC 9(5).                                    
003300*                                 KOLLINUMMER                             
003400        05 RKD-IDARTNR       PIC 9(9).                                    
003500*                                 ARTIKELNUMMER                           
003600        05 RKD-REKSIFFR      PIC 9.                                       
003700*                                 KONTROLLSIFFRA                          
003800        05 RKD-IDRADNR       PIC 9(4).                                    
003900*                                 RADNUMMER                               
004000        05 RKD-KDKREBEH      PIC X(3).                                    
004100*                                 BEHANDLINGSSTATUS                       
004200        05 RKD-KDANMORS      PIC 9(2).                                    
004300*                                 ORSAK TILL LEVERAN KDANMORS-002         
004400        05 RKD-KVLEVANM      PIC 9(6).                                    
004500*                                 LEVERANSANMÄRKNINGSANTAL                
004600        05 RKD-PRARTBTO      PIC 9(7)V9(2).                               
004700*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
004800        05 RKD-FLSKROT       PIC 9.                                       
004900*                                 SKROTNING ? (1=JA)  FLSKROT-002         
005000        05 RKD-FILLERX11     PIC X(11).                                   
005100        05 RKD-KDVALISO      PIC X(3).                                    
005200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005300        05 RKD-PRARTBTO-LOC  PIC 9(7)V9(2).                               
005400*                                 PRIS I LOKAL VALUTA                     
005500        05 RKD-SULNELOC      PIC 9(9)V9(2).                               
005600*                                 FAKTURARADSUMMA EXKL MOMS               
005700        05 RKD-PRARTSTD      PIC 9(7)V9(2).                               
005800*                                 ARTIKELSTANDARDPRIS                     
005900        05 RKD-PRARTSJK      PIC 9(7)V9(2).                               
006000*                                 ARTIKELNS SJÄLVKOSTNAD                  
006100*** END OF VILMAII-COPY LENGTH= 142 BYTES                                 
