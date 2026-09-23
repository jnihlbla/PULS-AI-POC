000100 01  W4795N.                                                              
000200*                                 FAKTURERADE OCH LASTADE                 
000300*                                 RADER KOMPLETTERADE MED IDPRC           
000400*                                 OCH AVERAGE COST.                       
000500     03 IDDC-LEV             PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 IDDISTR              PIC S9(5)           COMP-3.                  
000800*                                 DISTRIKTNUMMER                          
000900     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001000*                                 KUNDNUMMER                              
001100     03 IDKUNDRF             PIC X(10).                                   
001200*                                 KUNDENS REFERENS (ORDERID)              
001300     03 GRUPP1 REDEFINES IDKUNDRF.                                        
001400*                                                                         
001500        05 IDORDNR5          PIC 9(5).                                    
001600*                                 ORDERNUMMER                             
001700        05 FILLER            PIC X(5).                                    
001800     03 IDPRODNR             PIC S9(7)           COMP-3.                  
001900*                                 PRODUKTIONSNUMMER                       
002000     03 IDPLKLST             PIC S9(3)           COMP-3.                  
002100*                                 PLOCKLISTNUMMER                         
002200     03 IDPRC.                                                            
002300*                                 PRODUKTIONSKANAL                        
002400        05 IDPRCBAS          PIC X(3).                                    
002500*                                 PRC-BAS                                 
002600        05 IDPRCVAR          PIC X.                                       
002700*                                 PRC-VARIANT                             
002800     03 IDKOLLI              PIC S9(5)           COMP-3.                  
002900*                                 KOLLINUMMER                             
003000     03 IDARTNR              PIC S9(9)           COMP-3.                  
003100*                                 ARTIKELNUMMER                           
003200     03 KDFRAKT              PIC S9(3)           COMP-3.                  
003300*                                 FRAKTSÄTT DC TILL KUND                  
003400     03 KDORDKL              PIC S9              COMP-3.                  
003500*                                 ORDERKLASS                              
003600     03 TIORDREG             PIC S9(7)           COMP-3.                  
003700*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
003800     03 TIPACKN              PIC S9(7)           COMP-3.                  
003900*                                 PACKNINGSDATUM         (ÅÅMMDD)         
004000     03 TIFAKT               PIC S9(7)           COMP-3.                  
004100*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
004200     03 TILASTN              PIC S9(7)           COMP-3.                  
004300*                                 LASTNINGSDATUM         (ÅÅMMDD)         
004400     03 FLKRED               PIC X.                                       
004500*                                 KREDITERING BYTESORDER ?                
004600     03 LOW-VALUE1           PIC X.                                       
004700     03 FLDIRLEV             PIC X.                                       
004800*                                 DIREKTLEVERANS ?                        
004900     03 IDDIVORD             PIC S9(3)           COMP-3.                  
005000*                                 DIVERSEORDERNUMMER                      
005100     03 IDKUNDRF-RO          PIC X(10).                                   
005200*                                 KUND REF PÅ RO                          
005300     03 GRUPP2 REDEFINES IDKUNDRF-RO.                                     
005400*                                                                         
005500        05 IDRONR            PIC 9(5).                                    
005600*                                 RESTORDERNUMMER                         
005700        05 FILLER            PIC X(5).                                    
005800     03 KDORDTYP             PIC S9              COMP-3.                  
005900*                                 ORDERTYP                                
006000*                                 3 = SKROTORDER                          
006100     03 KDPRODSL             PIC S9(3)           COMP-3.                  
006200*                                 PRODUKTSLAG                             
006300     03 FILLER               PIC X.                                       
006400     03 FLPRTILL             PIC X.                                       
006500*                                 PRISTILLÄGGS FLAGGA                     
006600     03 IDKONTO              PIC S9(11)          COMP-3.                  
006700*                                 KONTO                                   
006800     03 IDKST                PIC S9(5)           COMP-3.                  
006900*                                 KOSTNADSSTÄLLE   IDKST-002              
007000     03 KVBEART              PIC S9(7)           COMP-3.                  
007100*                                 BESTÄLLT ANTAL STYCKEN                  
007200     03 KVLEVART             PIC S9(7)           COMP-3.                  
007300*                                 LEVERERAT ANTAL STYCK                   
007400     03 KVLEVART2            PIC S9(7)           COMP-3.                  
007500*                                 FAKTISKT LEVERERAT ANTAL I KOLL         
007600*                                 IT                                      
007700     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
007800*                                 ARTIKELPRIS NETTO                       
007900     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
008000*                                 ARTIKELSTANDARDPRIS                     
008100     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
008200*                                 ARTIKELNS SJÄLVKOSTNAD                  
008300     03 PRAVCOST             PIC S9(7)V9(2)      COMP-3.                  
008400*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
008500     03 IDLKTO               PIC S9(7)           COMP-3.                  
008600*                                 LAGERKONTO (FFHHHUU)                    
008700     03 KDPERSON             PIC S9(3)           COMP-3.                  
008800*                                 PERSONKOD                               
008900     03 KDFAKTYP             PIC X.                                       
009000*                                 FAKTURATYP                              
009100     03 IDFAKT               PIC S9(7)           COMP-3.                  
009200*                                 FAKTURANUMMER                           
009300     03 VKARTNTO             PIC S9(4)V9(3)      COMP-3.                  
009400*                                 ARTIKELVIKT NETTO (KG)                  
009500     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
009600*                                 ARTIKELVOLYM NETTO (CM3)                
009700     03 IDARTNR-SATS         PIC S9(9)           COMP-3.                  
009800*                                 ARTIKELNUMMER FÖR SATS                  
009900     03 FLUTSKR              PIC X.                                       
010000*                                 ORDER SOM ÄR UTSKRIVNA I FAKT           
010100     03 KDARTURS             PIC S9(3)           COMP-3.                  
010200*                                 ARTIKELURSPRUNGSKOD                     
010300     03 KDURLAND             PIC X(3).                                    
010400*                                 URSPRUNGSLANDKOD                        
010500*** END OF VILMAII-COPY LENGTH= 148 BYTES                                 
