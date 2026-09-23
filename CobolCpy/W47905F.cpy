000100 01  W47905F.                                                             
000200*                                 FAKTURERADE OCH LASTADE                 
000300*                                 RADER TILL FAKTURAHISTORIK              
000400     03 IDDC-LEV             PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600     03 IDDC-RET             PIC X(2).                                    
000700*                                 MOTTAGANDE LAGER FÖR RETURER            
000800     03 IDDISTR              PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000     03 IDFTG                PIC 9(2).                                    
001100*                                 FÖRETAGSID EKONOM REDOVISNING           
001200     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001300*                                 KUNDNUMMER                              
001400     03 IDKUNDRF             PIC X(10).                                   
001500*                                 KUNDENS REFERENS (ORDERID)              
001600     03 GRUPP1 REDEFINES IDKUNDRF.                                        
001700*                                                                         
001800        05 IDORDNR5          PIC 9(5).                                    
001900*                                 ORDERNUMMER                             
002000        05 FILLER            PIC X(5).                                    
002100     03 IDPRODNR             PIC S9(7)           COMP-3.                  
002200*                                 PRODUKTIONSNUMMER                       
002300     03 IDKOLLI              PIC S9(5)           COMP-3.                  
002400*                                 KOLLINUMMER                             
002500     03 IDARTNR              PIC S9(9)           COMP-3.                  
002600*                                 ARTIKELNUMMER                           
002700     03 IDPURAD              PIC S9(5)           COMP-3.                  
002800*                                 RADNUMMER PÅ PACKUNDERLAG               
002900     03 KDFRAKT              PIC S9(3)           COMP-3.                  
003000*                                 FRAKTSÄTT DC TILL KUND                  
003100     03 KDORDKL              PIC S9              COMP-3.                  
003200*                                 ORDERKLASS                              
003300     03 TIORDREG             PIC S9(7)           COMP-3.                  
003400*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
003500     03 TIPACKN              PIC S9(7)           COMP-3.                  
003600*                                 PACKNINGSDATUM         (ÅÅMMDD)         
003700     03 TIFAKT               PIC S9(7)           COMP-3.                  
003800*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
003900     03 TILASTN              PIC S9(7)           COMP-3.                  
004000*                                 LASTNINGSDATUM         (ÅÅMMDD)         
004100     03 FLKRED               PIC X.                                       
004200*                                 KREDITERING BYTESORDER ?                
004300     03 LOW-VALUE1           PIC X.                                       
004400     03 FLDIRLEV             PIC X.                                       
004500*                                 DIREKTLEVERANS ?                        
004600     03 IDDIVORD             PIC S9(3)           COMP-3.                  
004700*                                 DIVERSEORDERNUMMER                      
004800     03 IDKUNDRF-RO          PIC X(10).                                   
004900*                                 KUND REF PÅ RO                          
005000     03 GRUPP2 REDEFINES IDKUNDRF-RO.                                     
005100*                                                                         
005200        05 IDRONR            PIC 9(5).                                    
005300*                                 RESTORDERNUMMER                         
005400        05 FILLER            PIC X(5).                                    
005500     03 KDORDTYP             PIC S9              COMP-3.                  
005600*                                 ORDERTYP                                
005700     03 KDPRODSL             PIC S9(3)           COMP-3.                  
005800*                                 PRODUKTSLAG                             
005900     03 FILLER               PIC X.                                       
006000     03 FLPRTILL             PIC X.                                       
006100*                                 PRISTILLÄGGS FLAGGA                     
006200     03 IDKONTO              PIC S9(11)          COMP-3.                  
006300*                                 KONTO                                   
006400     03 IDKST                PIC X(10).                                   
006500*                                 KOSTNADSSTÄLLE                          
006600     03 IDANALYS             PIC X(12).                                   
006700*                                 ANALYSNUMMER                            
006800     03 KVBEART              PIC S9(7)           COMP-3.                  
006900*                                 BESTÄLLT ANTAL STYCKEN                  
007000     03 KVLEVART             PIC S9(7)           COMP-3.                  
007100*                                 LEVERERAT ANTAL STYCK                   
007200     03 KVLEVART2            PIC S9(7)           COMP-3.                  
007300*                                 FAKTISKT LEVERERAT ANTAL I KOLL         
007400*                                 IT                                      
007500     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
007600*                                 ARTIKELPRIS NETTO                       
007700     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
007800*                                 ARTIKELSTANDARDPRIS                     
007900     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
008000*                                 ARTIKELNS SJÄLVKOSTNAD                  
008100     03 IDLKTO               PIC S9(7)           COMP-3.                  
008200*                                 LAGERKONTO (FFHHHUU)                    
008300     03 KDPERSON             PIC S9(3)           COMP-3.                  
008400*                                 PERSONKOD                               
008500     03 KDFAKTYP             PIC X.                                       
008600*                                 FAKTURATYP                              
008700     03 IDFAKT               PIC S9(7)           COMP-3.                  
008800*                                 FAKTURANUMMER                           
008900     03 VKARTNTO             PIC S9(4)V9(3)      COMP-3.                  
009000*                                 ARTIKELVIKT NETTO (KG)                  
009100     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
009200*                                 ARTIKELVOLYM NETTO (CM3)                
009300     03 IDARTNR-SATS         PIC S9(9)           COMP-3.                  
009400*                                 ARTIKELNUMMER FÖR SATS                  
009500     03 FLUTSKR              PIC X.                                       
009600*                                 ORDER SOM ÄR UTSKRIVNA I FAKT           
009700     03 KDARTURS             PIC X(2).                                    
009800*                                 ARTIKELURSPRUNGSKOD                     
009900     03 FILLER               PIC X(3).                                    
010000     03 IDUSER-OREG          PIC X(8).                                    
010100*                                 ANSVARIGT USERID ORDERREG.              
010200     03 IDUSER-PACK          PIC X(8).                                    
010300*                                 ANSVARIGT USERID PACKARE                
010400     03 KDKOLLI              PIC X(8).                                    
010500*                                 KOLLIKOD                                
010600     03 KVAVBART             PIC S9(7)           COMP-3.                  
010700*                                 AVBOKAT ANTAL ARTIKLAR                  
010800     03 KVORDRAD             PIC S9(5)           COMP-3.                  
010900*                                 ANTAL ORDERRADER                        
011000     03 SUORDV-KOLLI         PIC S9(9)V9(2)      COMP-3.                  
011100*                                 VARUVÄRDE PER KOLLI                     
011200     03 VKORDBTO-KOLLI       PIC S9(6)V9(1)      COMP-3.                  
011300*                                 ORDERVIKT BRUTTO PER KOLLI              
011400     03 VKORDNTO-KOLLI       PIC S9(6)V9(1)      COMP-3.                  
011500*                                 ORDERVIKT NETTO PER KOLLI               
011600     03 IDLEVNR              PIC X(5).                                    
011700*                                 LEVERANTÖRNUMMER                        
011800     03 IDBORD               PIC X(3).                                    
011900*                                 PACK-BORD                               
012000     03 KDVALISO             PIC X(3).                                    
012100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
012200     03 PRARTNTO-LOC         PIC S9(7)V9(2)      COMP-3.                  
012300*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
012400     03 KDVAT                PIC X(2).                                    
012500*                                 MOMSKOD                                 
012600     03 BEART-VIPS           PIC X(25).                                   
012700*                                 VIPS ARTIKELBENÄMNING                   
012800*                                 PÅ DEALERNS SPRÅK                       
012900     03 IDSHIPM              PIC 9(7).                                    
013000*                                 SKEPPNINGSNUMMER                        
013100     03 TISKEPPN             PIC S9(7)           COMP-3.                  
013200*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
013300     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
013400*                                 LAGEROMRÅDE                             
013500*** END OF VILMAII-COPY LENGTH= 264 BYTES                                 
