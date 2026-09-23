000100 01  W479055.                                                             
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
001900     03 IDKOLLI              PIC S9(5)           COMP-3.                  
002000*                                 KOLLINUMMER                             
002100     03 IDARTNR              PIC S9(9)           COMP-3.                  
002200*                                 ARTIKELNUMMER                           
002300     03 KDFRAKT              PIC S9(3)           COMP-3.                  
002400*                                 FRAKTSÄTT DC TILL KUND                  
002500     03 KDORDKL              PIC S9              COMP-3.                  
002600*                                 ORDERKLASS                              
002700     03 TIORDREG             PIC S9(7)           COMP-3.                  
002800*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
002900     03 TIPACKN              PIC S9(7)           COMP-3.                  
003000*                                 PACKNINGSDATUM         (ÅÅMMDD)         
003100     03 TIFAKT               PIC S9(7)           COMP-3.                  
003200*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
003300     03 TILASTN              PIC S9(7)           COMP-3.                  
003400*                                 LASTNINGSDATUM         (ÅÅMMDD)         
003500     03 TIFAKT-BILLIT        PIC S9(7)           COMP-3.                  
003600*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
003700     03 TILASTN-BILLIT       PIC S9(7)           COMP-3.                  
003800*                                 LASTNINGSDATUM         (ÅÅMMDD)         
003900     03 FLKRED               PIC X.                                       
004000*                                 KREDITERING BYTESORDER ?                
004100     03 LOW-VALUE1           PIC X.                                       
004200     03 FLDIRLEV             PIC X.                                       
004300*                                 DIREKTLEVERANS ?                        
004400     03 IDDIVORD             PIC S9(3)           COMP-3.                  
004500*                                 DIVERSEORDERNUMMER                      
004600     03 IDKUNDRF-RO          PIC X(10).                                   
004700*                                 KUND REF PÅ RO                          
004800     03 GRUPP2 REDEFINES IDKUNDRF-RO.                                     
004900*                                                                         
005000        05 IDRONR            PIC 9(5).                                    
005100*                                 RESTORDERNUMMER                         
005200        05 FILLER            PIC X(5).                                    
005300     03 KDORDTYP             PIC S9              COMP-3.                  
005400*                                 ORDERTYP                                
005500     03 KDPRODSL             PIC S9(3)           COMP-3.                  
005600*                                 PRODUKTSLAG                             
005700     03 FILLER               PIC X.                                       
005800     03 FLPRTILL             PIC X.                                       
005900*                                 PRISTILLÄGGS FLAGGA                     
006000     03 IDKONTO              PIC S9(11)          COMP-3.                  
006100*                                 KONTO                                   
006200     03 IDKST                PIC X(10).                                   
006300*                                 KOSTNADSSTÄLLE                          
006400     03 KVBEART              PIC S9(7)           COMP-3.                  
006500*                                 BESTÄLLT ANTAL STYCKEN                  
006600     03 KVLEVART             PIC S9(7)           COMP-3.                  
006700*                                 LEVERERAT ANTAL STYCK                   
006800     03 KVLEVART2            PIC S9(7)           COMP-3.                  
006900*                                 FAKTISKT LEVERERAT ANTAL I KOLL         
007000*                                 IT                                      
007100     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
007200*                                 ARTIKELPRIS NETTO                       
007300     03 PRARTNTO-LOC         PIC S9(7)V9(2)      COMP-3.                  
007400*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
007500     03 PRARTNTO-LOCPREL     PIC S9(7)V9(2)      COMP-3.                  
007600*                                 PREL NETTO SLUTKUNDSPRIS I              
007700*                                 LOKAL VALUTA                            
007800     03 KDVALISO             PIC X(3).                                    
007900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
008000     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
008100*                                 ARTIKELSTANDARDPRIS                     
008200     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
008300*                                 ARTIKELNS SJÄLVKOSTNAD                  
008400     03 IDLKTO               PIC S9(7)           COMP-3.                  
008500*                                 LAGERKONTO (FFHHHUU)                    
008600     03 KDPERSON             PIC S9(3)           COMP-3.                  
008700*                                 PERSONKOD                               
008800     03 KDFAKTYP             PIC X.                                       
008900*                                 FAKTURATYP                              
009000     03 IDFAKT               PIC S9(7)           COMP-3.                  
009100*                                 FAKTURANUMMER                           
009200     03 VKARTNTO             PIC S9(4)V9(3)      COMP-3.                  
009300*                                 ARTIKELVIKT NETTO (KG)                  
009400     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
009500*                                 ARTIKELVOLYM NETTO (CM3)                
009600     03 IDARTNR-SATS         PIC S9(9)           COMP-3.                  
009700*                                 ARTIKELNUMMER FÖR SATS                  
009800     03 FLUTSKR              PIC X.                                       
009900*                                 ORDER SOM ÄR UTSKRIVNA I FAKT           
010000     03 KDARTURS             PIC X(2).                                    
010100*                                 ARTIKELURSPRUNGSKOD                     
010200     03 FILLER               PIC X(3).                                    
010300     03 VKORDBTO-KOLLI       PIC S9(6)V9(1)      COMP-3.                  
010400*                                 ORDERVIKT BRUTTO PER KOLLI              
010500     03 VKORDNTO-KOLLI       PIC S9(6)V9(1)      COMP-3.                  
010600*                                 ORDERVIKT NETTO PER KOLLI               
010700     03 IDPRC.                                                            
010800*                                 PRODUKTIONSKANAL                        
010900        05 IDPRCBAS          PIC X(3).                                    
011000*                                 PRC-BAS                                 
011100        05 IDPRCVAR          PIC X.                                       
011200*                                 PRC-VARIANT                             
011300*** END OF VILMAII-COPY LENGTH= 177 BYTES                                 
