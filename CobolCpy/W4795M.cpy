000100 01  W4795M.                                                              
000200*                                 FAKTURERADE OCH LASTADE                 
000300*                                 RADER KOMPLETTERADE MED IDPRC           
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
002100     03 IDPRC.                                                            
002200*                                 PRODUKTIONSKANAL                        
002300        05 IDPRCBAS          PIC X(3).                                    
002400*                                 PRC-BAS                                 
002500        05 IDPRCVAR          PIC X.                                       
002600*                                 PRC-VARIANT                             
002700     03 IDKOLLI              PIC S9(5)           COMP-3.                  
002800*                                 KOLLINUMMER                             
002900     03 IDARTNR              PIC S9(9)           COMP-3.                  
003000*                                 ARTIKELNUMMER                           
003100     03 KDFRAKT              PIC S9(3)           COMP-3.                  
003200*                                 FRAKTSÄTT DC TILL KUND                  
003300     03 KDORDKL              PIC S9              COMP-3.                  
003400*                                 ORDERKLASS                              
003500     03 TIORDREG             PIC S9(7)           COMP-3.                  
003600*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
003700     03 TIPACKN              PIC S9(7)           COMP-3.                  
003800*                                 PACKNINGSDATUM         (ÅÅMMDD)         
003900     03 TIFAKT               PIC S9(7)           COMP-3.                  
004000*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
004100     03 TILASTN              PIC S9(7)           COMP-3.                  
004200*                                 LASTNINGSDATUM         (ÅÅMMDD)         
004300     03 FLKRED               PIC X.                                       
004400*                                 KREDITERING BYTESORDER ?                
004500     03 LOW-VALUE1           PIC X.                                       
004600     03 FLDIRLEV             PIC X.                                       
004700*                                 DIREKTLEVERANS ?                        
004800     03 IDDIVORD             PIC S9(3)           COMP-3.                  
004900*                                 DIVERSEORDERNUMMER                      
005000     03 IDKUNDRF-RO          PIC X(10).                                   
005100*                                 KUND REF PÅ RO                          
005200     03 GRUPP2 REDEFINES IDKUNDRF-RO.                                     
005300*                                                                         
005400        05 IDRONR            PIC 9(5).                                    
005500*                                 RESTORDERNUMMER                         
005600        05 FILLER            PIC X(5).                                    
005700     03 KDORDTYP             PIC S9              COMP-3.                  
005800*                                 ORDERTYP                                
005900*                                 3 = SKROTORDER                          
006000     03 KDPRODSL             PIC S9(3)           COMP-3.                  
006100*                                 PRODUKTSLAG                             
006200     03 FILLER               PIC X.                                       
006300     03 FLPRTILL             PIC X.                                       
006400*                                 PRISTILLÄGGS FLAGGA                     
006500     03 IDKONTO              PIC S9(11)          COMP-3.                  
006600*                                 KONTO                                   
006700     03 IDKST                PIC S9(5)           COMP-3.                  
006800*                                 KOSTNADSSTÄLLE   IDKST-002              
006900     03 KVBEART              PIC S9(7)           COMP-3.                  
007000*                                 BESTÄLLT ANTAL STYCKEN                  
007100     03 KVLEVART             PIC S9(7)           COMP-3.                  
007200*                                 LEVERERAT ANTAL STYCK                   
007300     03 KVLEVART2            PIC S9(7)           COMP-3.                  
007400*                                 FAKTISKT LEVERERAT ANTAL I KOLL         
007500*                                 IT                                      
007600     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
007700*                                 ARTIKELPRIS NETTO                       
007800     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
007900*                                 ARTIKELSTANDARDPRIS                     
008000     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
008100*                                 ARTIKELNS SJÄLVKOSTNAD                  
008200     03 IDLKTO               PIC S9(7)           COMP-3.                  
008300*                                 LAGERKONTO (FFHHHUU)                    
008400     03 KDPERSON             PIC S9(3)           COMP-3.                  
008500*                                 PERSONKOD                               
008600     03 KDFAKTYP             PIC X.                                       
008700*                                 FAKTURATYP                              
008800     03 IDFAKT               PIC S9(7)           COMP-3.                  
008900*                                 FAKTURANUMMER                           
009000     03 VKARTNTO             PIC S9(4)V9(3)      COMP-3.                  
009100*                                 ARTIKELVIKT NETTO (KG)                  
009200     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
009300*                                 ARTIKELVOLYM NETTO (CM3)                
009400     03 IDARTNR-SATS         PIC S9(9)           COMP-3.                  
009500*                                 ARTIKELNUMMER FÖR SATS                  
009600     03 FLUTSKR              PIC X.                                       
009700*                                 ORDER SOM ÄR UTSKRIVNA I FAKT           
009800     03 KDARTURS             PIC S9(3)           COMP-3.                  
009900*                                 ARTIKELURSPRUNGSKOD                     
010000     03 KDURLAND             PIC X(3).                                    
010100*                                 URSPRUNGSLANDKOD                        
010200*** END OF VILMAII-COPY LENGTH= 143 BYTES                                 
