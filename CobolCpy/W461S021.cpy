000100 01  HUV-W461S021.                                                        
000200*                                 SAMMANSLAGNING AV SORTDEL OCH           
000300*                                 KREDIT   POST TILL NOAC                 
000400     03 HUV-SOR0-IDDISTR     PIC S9(5)           COMP-3.                  
000500*                                 DISTRIKTNUMMER                          
000600     03 HUV-SOR0-IDKUNDNR    PIC S9(7)           COMP-3.                  
000700*                                 KUNDNUMMER                              
000800     03 HUV-SOR0-IDRONR      PIC S9(7)           COMP-3.                  
000900*                                 RESTORDERNUMMER      IDRONR-002         
001000     03 HUV-SOR0-TIRODAT     PIC S9(7)           COMP-3.                  
001100*                                 RESTORDERDATUM         (ÅÅMMDD)         
001200     03 HUV-SOR0-IDPTYP      PIC X(3).                                    
001300*                                 POSTTYP                                 
001400     03 HUV-SOR0-IDLOPNR     PIC S9(5)           COMP-3.                  
001500*                                 LÖPNUMMER          IDLOPNR-002          
001600     03 HUV-W461021.                                                      
001700*                                 KREDITERINGSTRANSAR  FÖR ORDER          
001800        05 HUV-IDPTYP        PIC X(3).                                    
001900*                                 POSTTYP                                 
002000        05 HUV-IDDISTR       PIC S9(5)           COMP-3.                  
002100*                                 DISTRIKTNUMMER                          
002200        05 HUV-IDKUNDNR      PIC S9(7)           COMP-3.                  
002300*                                 KUNDNUMMER                              
002400        05 HUV-IDDC          PIC X(2).                                    
002500*                                 IDENTIFIERARE LAGER                     
002600        05 HUV-IDRAPPNR      PIC 9(7).                                    
002700*                                 RAPPORT NUMMER                          
002800        05 HUV-IDKNOTNR      PIC S9(7)           COMP-3.                  
002900*                                 KREDITNOTANUMMER                        
003000        05 HUV-TIM-KN        PIC S9(7)           COMP-3.                  
003100*                                 DATUM KREDITNOTA                        
003200        05 HUV-IDFAKT        PIC S9(7)           COMP-3.                  
003300*                                 FAKTURANUMMER                           
003400        05 HUV-IDORDNR       PIC S9(7)           COMP-3.                  
003500*                                 ORDERNR             IDORDNR-002         
003600        05 HUV-IDARTNR       PIC S9(9)           COMP-3.                  
003700*                                 ARTIKELNUMMER                           
003800        05 HUV-REKSIFFR      PIC S9              COMP-3.                  
003900*                                 KONTROLLSIFFRA                          
004000        05 HUV-IDRADNR       PIC S9(5)           COMP-3.                  
004100*                                 RADNUMMER                               
004200        05 HUV-KDANMORS      PIC S9(3)           COMP-3.                  
004300*                                 ORSAK TILL LEVERAN KDANMORS-002         
004400        05 HUV-KDPSLLOC      PIC 9(2).                                    
004500*                                 PRODUKTSLAG LOKALT                      
004600        05 HUV-KVKREANT      PIC S9(7)           COMP-3.                  
004700*                                 KREDITERAT ANTAL                        
004800        05 HUV-PRARTBTO      PIC S9(7)V9(2)      COMP-3.                  
004900*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
005000        05 HUV-PRAVCOST      PIC S9(7)V9(2)      COMP-3.                  
005100*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
005200        05 HUV-PRAVCOST-CORE PIC S9(7)V9(2)      COMP-3.                  
005300*                                 OBJEKTETS MEDELVÄRDESKOSTNAD I          
005400*                                 UTL.VALUTA                              
005500        05 HUV-PREMBHNT      PIC S9(7)V9(2)      COMP-3.                  
005600*                                 EMBALLAGE O HANTERINGSKOST              
005700        05 HUV-PRFRAKT       PIC S9(7)V9(2)      COMP-3.                  
005800*                                 FRAKTKOSTNAD                            
005900        05 HUV-PRLEGKST      PIC S9(7)V9(2)      COMP-3.                  
006000*                                 LEGALISERINSKOSTNAD                     
006100        05 HUV-PRFOERS       PIC S9(7)V9(2)      COMP-3.                  
006200*                                 FÖRSÄKRINGSPREMIE                       
006300        05 HUV-PRMOMS        PIC S9(7)V9(2)      COMP-3.                  
006400*                                 MERVÄRDESSKATT                          
006500        05 HUV-PRARTBTO-LOC  PIC S9(7)V9(2)      COMP-3.                  
006600*                                 PRIS I LOKAL VALUTA                     
006700        05 HUV-SUVAT-FAKT    PIC S9(11)V9(2)     COMP-3.                  
006800*                                 TOTALT MOMSVÄRDE PER FAKTURA/KR         
006900*                                 EDITNOTA                                
007000        05 HUV-KDVALISO      PIC X(3).                                    
007100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
007200        05 HUV-KDVAT         PIC X(2).                                    
007300*                                 MOMSKOD                                 
007400        05 HUV-PRARTSTD      PIC S9(7)V9(2)      COMP-3.                  
007500*                                 ARTIKELSTANDARDPRIS                     
007600        05 HUV-PRARTSJK      PIC S9(7)V9(2)      COMP-3.                  
007700*                                 ARTIKELNS SJÄLVKOSTNAD                  
007800        05 HUV-SULNELOC      PIC S9(9)V9(2)      COMP-3.                  
007900*                                 FAKTURARADSUMMA EXKL MOMS               
008000        05 HUV-SUKRENTO      PIC S9(11)V9(2)     COMP-3.                  
008100*                                 KREDITERAT VARUVÄRDE NETTO              
008200        05 HUV-SUKRETOT      PIC S9(11)V9(2)     COMP-3.                  
008300*                                 TOTALT KREDITERAT VÄRDE                 
008400*** END OF VILMAII-COPY LENGTH= 160 BYTES                                 
