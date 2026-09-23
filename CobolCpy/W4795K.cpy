000100 01  W4795K.                                                              
000200*                                 FAKTURERADE OCH LASTADE                 
000300*                                 RADER                                   
000400     03 IDDC-LEV             PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600     03 IDDISTR              PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000     03 IDKUNDRF             PIC X(10).                                   
001100*                                 KUNDENS REFERENS (ORDERID)              
001200     03 GRUPP1 REDEFINES IDKUNDRF.                                        
001300*                                                                         
001400        05 IDORDNR5          PIC 9(5).                                    
001500*                                 ORDERNUMMER                             
001600        05 FILLER            PIC X(5).                                    
001700     03 IDPRODNR             PIC S9(7)           COMP-3.                  
001800*                                 PRODUKTIONSNUMMER                       
001900     03 IDPLKLST             PIC S9(3)           COMP-3.                  
002000*                                 PLOCKLISTNUMMER                         
002100     03 IDKOLLI              PIC S9(5)           COMP-3.                  
002200*                                 KOLLINUMMER                             
002300     03 IDARTNR              PIC S9(9)           COMP-3.                  
002400*                                 ARTIKELNUMMER                           
002500     03 KDFRAKT              PIC S9(3)           COMP-3.                  
002600*                                 FRAKTSÄTT DC TILL KUND                  
002700     03 KDORDKL              PIC S9              COMP-3.                  
002800*                                 ORDERKLASS                              
002900     03 TIORDREG             PIC S9(7)           COMP-3.                  
003000*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
003100     03 TIPACKN              PIC S9(7)           COMP-3.                  
003200*                                 PACKNINGSDATUM         (ÅÅMMDD)         
003300     03 TIFAKT               PIC S9(7)           COMP-3.                  
003400*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
003500     03 TILASTN              PIC S9(7)           COMP-3.                  
003600*                                 LASTNINGSDATUM         (ÅÅMMDD)         
003700     03 FLKRED               PIC X.                                       
003800*                                 KREDITERING BYTESORDER ?                
003900     03 LOW-VALUE1           PIC X.                                       
004000     03 FLDIRLEV             PIC X.                                       
004100*                                 DIREKTLEVERANS ?                        
004200     03 IDDIVORD             PIC S9(3)           COMP-3.                  
004300*                                 DIVERSEORDERNUMMER                      
004400     03 IDKUNDRF-RO          PIC X(10).                                   
004500*                                 KUND REF PÅ RO                          
004600     03 GRUPP2 REDEFINES IDKUNDRF-RO.                                     
004700*                                                                         
004800        05 IDRONR            PIC 9(5).                                    
004900*                                 RESTORDERNUMMER                         
005000        05 FILLER            PIC X(5).                                    
005100     03 KDORDTYP             PIC S9              COMP-3.                  
005200*                                 ORDERTYP                                
005300*                                 3 = SKROTORDER                          
005400     03 KDPRODSL             PIC S9(3)           COMP-3.                  
005500*                                 PRODUKTSLAG                             
005600     03 FILLER               PIC X.                                       
005700     03 FLPRTILL             PIC X.                                       
005800*                                 PRISTILLÄGGS FLAGGA                     
005900     03 IDKONTO              PIC S9(11)          COMP-3.                  
006000*                                 KONTO                                   
006100     03 IDKST                PIC S9(5)           COMP-3.                  
006200*                                 KOSTNADSSTÄLLE   IDKST-002              
006300     03 KVBEART              PIC S9(7)           COMP-3.                  
006400*                                 BESTÄLLT ANTAL STYCKEN                  
006500     03 KVLEVART             PIC S9(7)           COMP-3.                  
006600*                                 LEVERERAT ANTAL STYCK                   
006700     03 KVLEVART2            PIC S9(7)           COMP-3.                  
006800*                                 FAKTISKT LEVERERAT ANTAL I KOLL         
006900*                                 IT                                      
007000     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
007100*                                 ARTIKELPRIS NETTO                       
007200     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
007300*                                 ARTIKELSTANDARDPRIS                     
007400     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
007500*                                 ARTIKELNS SJÄLVKOSTNAD                  
007600     03 IDLKTO               PIC S9(7)           COMP-3.                  
007700*                                 LAGERKONTO (FFHHHUU)                    
007800     03 KDPERSON             PIC S9(3)           COMP-3.                  
007900*                                 PERSONKOD                               
008000     03 KDFAKTYP             PIC X.                                       
008100*                                 FAKTURATYP                              
008200     03 IDFAKT               PIC S9(7)           COMP-3.                  
008300*                                 FAKTURANUMMER                           
008400     03 VKARTNTO             PIC S9(4)V9(3)      COMP-3.                  
008500*                                 ARTIKELVIKT NETTO (KG)                  
008600     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
008700*                                 ARTIKELVOLYM NETTO (CM3)                
008800     03 IDARTNR-SATS         PIC S9(9)           COMP-3.                  
008900*                                 ARTIKELNUMMER FÖR SATS                  
009000     03 FLUTSKR              PIC X.                                       
009100*                                 ORDER SOM ÄR UTSKRIVNA I FAKT           
009200     03 KDARTURS             PIC X(2).                                    
009300*                                 ARTIKELURSPRUNGSKOD                     
009400     03 FILLER               PIC X(3).                                    
009500*** END OF VILMAII-COPY LENGTH= 139 BYTES                                 
