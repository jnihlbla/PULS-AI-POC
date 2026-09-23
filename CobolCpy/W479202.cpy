000100 01  W479202.                                                             
000200*                                                                         
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 IDDISTR              PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000800*                                 KUNDNUMMER                              
000900     03 IDKUNDRF             PIC X(10).                                   
001000*                                 KUNDENS REFERENS                        
001100     03 KDFAKTYP             PIC X.                                       
001200*                                 FAKTURATYP                              
001300     03 KDFRAKT              PIC S9(3)           COMP-3.                  
001400*                                 FRAKTSÄTT C1-C2 TILL KUND               
001500     03 KDORDKL              PIC S9              COMP-3.                  
001600*                                 ORDERKLASS                              
001700     03 TIBEGPAC-C1          PIC S9(7)           COMP-3.                  
001800*                                 BEGÄRD PACKNINGSDAG C1 (ÅÅMMDD)         
001900     03 TIBEGPAC-C2          PIC S9(7)           COMP-3.                  
002000*                                 BEGÄRD PACKNINGSDAG C2 (ÅÅMMDD)         
002100     03 TIUTSKR-C1           PIC S9(7)           COMP-3.                  
002200*                                 UTSKRIFTSDATUM C1      (ÅÅMMDD)         
002300     03 TIUTSKR-C2           PIC S9(7)           COMP-3.                  
002400*                                 UTSKRIFTSDATUM C2      (ÅÅMMDD)         
002500     03 TIREF1               PIC S9(7)           COMP-3.                  
002600*                                 KUNDREFERENS-DATUM 1   (ÅÅMMDD)         
002700     03 TIORDREG             PIC S9(7)           COMP-3.                  
002800*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
002900     03 IDARTNR              PIC S9(9)           COMP-3.                  
003000*                                 ARTIKELNUMMER                           
003100     03 REKSIFFR             PIC S9              COMP-3.                  
003200*                                 KONTROLLSIFFRA                          
003300     03 IDARTNR-ERS          PIC S9(9)           COMP-3.                  
003400*                                 ERSATT ARTIKELNUMMER                    
003500     03 ADLEVPL              PIC S9(3)           COMP-3.                  
003600*                                 LEVERANSPLATS                           
003700     03 BERADREF             PIC X(10).                                   
003800*                                 KUNDENS RADREFERENS                     
003900     03 FLKRED               PIC X.                                       
004000*                                 KREDITERING BYTESORDER ?                
004100     03 FLEMBORD             PIC X.                                       
004200*                                 EMBALLAGEORDER ?                        
004300     03 FLDIRLEV             PIC X.                                       
004400*                                 DIREKTLEVERANS ?                        
004500     03 FLRESTN              PIC X.                                       
004600*                                 RESTNOTERING ?                          
004700     03 IDDIVORD             PIC S9(3)           COMP-3.                  
004800*                                 DIVERSEORDERNUMMER                      
004900     03 IDPRODNR             PIC S9(7)           COMP-3.                  
005000*                                 PRODUKTIONSNUMMER                       
005100     03 IDKUNDRF-RO          PIC X(10).                                   
005200*                                 KUND REF PÅ RO                          
005300     03 KDCLAGER-LEV         PIC S9              COMP-3.                  
005400*                                 LEVERERANDE C-LAGER                     
005500     03 KDFRAKT-C1           PIC S9(3)           COMP-3.                  
005600*                                 FRAKTKODEN FÖR C1                       
005700     03 KDORDTYP             PIC S9              COMP-3.                  
005800*                                 ORDERTYP                                
005900     03 KDPRODSL             PIC S9(3)           COMP-3.                  
006000*                                 PRODUKTSLAG                             
006100     03 IDKONTO              PIC S9(11)          COMP-3.                  
006200*                                 KONTO                                   
006300     03 IDKST                PIC S9(5)           COMP-3.                  
006400*                                 KOSTNADSSTÄLLE                          
006500     03 KVANNANT             PIC S9(7)           COMP-3.                  
006600*                                 ANNULLERAT ANTAL ARTIKLAR               
006700     03 KVAVBART             PIC S9(7)           COMP-3.                  
006800*                                 AVBOKAT ANTAL ARTIKLAR                  
006900     03 KVBEART              PIC S9(7)           COMP-3.                  
007000*                                 BESTÄLLT ANTAL ARTIKLAR                 
007100     03 KVBEART-ERS          PIC S9(7)           COMP-3.                  
007200*                                 BESTÄLLT ANTAL ERSATT ARTIKEL           
007300     03 KVLEVART             PIC S9(7)           COMP-3.                  
007400*                                 LEVERERAT ANTAL ARTIKLAR                
007500     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
007600*                                 ARTIKELPRIS NETTO                       
007700     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
007800*                                 ARTIKELSTANDARDPRIS                     
007900     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
008000*                                 ARTIKELNS SJÄLVKOSTNAD                  
008100     03 TIRODAT              PIC S9(7)           COMP-3.                  
008200*                                 RESTORDERDATUM         (ÅÅMMDD)         
008300     03 TIUTSKR              PIC S9(7)           COMP-3.                  
008400*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
008500     03 VKARTNTO             PIC S9(4)V9(3)      COMP-3.                  
008600*                                 ARTIKELVIKT NETTO (KG)                  
008700     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
008800*                                 ARTIKELVOLYM NETTO (CM3)                
008900     03 TIFAKT               PIC S9(7)           COMP-3.                  
009000*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
009100     03 KVLEVART2            PIC S9(7)           COMP-3.                  
009200*                                 FAKTISKT LEVERERAT ANTAL                
009300     03 IDFAKT               PIC S9(7)           COMP-3.                  
009400*                                 FAKTURANUMMER                           
009500     03 IDKONTO-AVDRAG       PIC S9(11)          COMP-3.                  
009600*                                 KONTO FÖR AVDRAG                        
009700     03 IDKST-AVDRAG         PIC S9(5)           COMP-3.                  
009800*                                 KOSTNADSSTÄLLE AVDRAG                   
009900     03 KDFAKTYP2            PIC X.                                       
010000*                                 FAKTURATYP                              
010100     03 IDRADNR              PIC S9(5)           COMP-3.                  
010200*                                 RADNUMMER                               
010300     03 IDKOLLI              PIC S9(5)           COMP-3.                  
010400*                                 KOLLINUMMER                             
010500     03 TIPACKN              PIC S9(7)           COMP-3.                  
010600*                                 PACKNINGSDATUM         (ÅÅMMDD)         
010700     03 TILASTN              PIC S9(7)           COMP-3.                  
010800*                                 LASTNINGSDATUM         (ÅÅMMDD)         
010900     03 IDPROD               PIC S9(3)           COMP-3.                  
011000*                                 PRODUKTKOD                              
011100     03 KDPRORED             PIC S9(3)           COMP-3.                  
011200*                                 PRODUKTKOD REDOVISNING                  
011300*** END COPY W479202CC0  LENGTH=198                                       
