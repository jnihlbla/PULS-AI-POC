000100 01  W479052.                                                             
000200*                                 EV. ORDER I ARBETE                      
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 IDDISTR              PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000800*                                 KUNDNUMMER                              
000900     03 IDKUNDRF             PIC X(10).                                   
001000*                                 KUNDENS REFERENS (ORDERID)              
001100     03 FLOVRLEV             PIC X.                                       
001200*                                 ÖVERLEVERANS                            
001300     03 IDPURAD              PIC S9(5)           COMP-3.                  
001400*                                 RADNUMMER PÅ PACKUNDERLAG               
001500     03 IDDC                 PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 KDFAKTYP             PIC X.                                       
001800*                                 FAKTURATYP                              
001900     03 KDFRAKT              PIC S9(3)           COMP-3.                  
002000*                                 FRAKTSÄTT DC TILL KUND                  
002100     03 KDORDKL              PIC S9              COMP-3.                  
002200*                                 ORDERKLASS                              
002300     03 KVORDRAD-PACK        PIC S9(5)           COMP-3.                  
002400*                                 ANTAL PACKADE ORDERRADER                
002500     03 TIBEGPAC             PIC S9(7)           COMP-3.                  
002600*                                 BEGÄRD PACKNINGSDAG    (ÅÅMMDD)         
002700     03 TIUTSKR              PIC S9(7)           COMP-3.                  
002800*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
002900     03 TIREF1               PIC S9(7)           COMP-3.                  
003000*                                 KUNDREFERENS-DATUM 1   (ÅÅMMDD)         
003100     03 TIORDREG             PIC S9(7)           COMP-3.                  
003200*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
003300     03 KDRADSTA             PIC S9              COMP-3.                  
003400*                                 STATUS PÅ ORDERRAD                      
003500     03 IDARTNR              PIC S9(9)           COMP-3.                  
003600*                                 ARTIKELNUMMER                           
003700     03 REKSIFFR             PIC S9              COMP-3.                  
003800*                                 KONTROLLSIFFRA                          
003900     03 IDARTNR-ERS          PIC S9(9)           COMP-3.                  
004000*                                 ERSATT ARTIKELNUMMER                    
004100     03 ADLEVPL              PIC S9(3)           COMP-3.                  
004200*                                 LEVERANSPLATS                           
004300     03 BERADREF             PIC X(10).                                   
004400*                                 KUNDENS RADREFERENS                     
004500     03 FLKRED               PIC X.                                       
004600*                                 KREDITERING BYTESORDER ?                
004700     03 FLDIRLEV             PIC X.                                       
004800*                                 DIREKTLEVERANS ?                        
004900     03 FLRESTN              PIC X.                                       
005000*                                 RESTNOTERING ?                          
005100     03 IDDIVORD             PIC S9(3)           COMP-3.                  
005200*                                 DIVERSEORDERNUMMER                      
005300     03 IDPRODNR             PIC S9(7)           COMP-3.                  
005400*                                 PRODUKTIONSNUMMER                       
005500     03 IDKUNDRF-RO          PIC X(10).                                   
005600*                                 KUND REF PÅ RO                          
005700     03 KDORDTYP             PIC S9              COMP-3.                  
005800*                                 ORDERTYP                                
005900     03 KDPRODSL             PIC S9(3)           COMP-3.                  
006000*                                 PRODUKTSLAG                             
006100     03 IDKONTO              PIC S9(11)          COMP-3.                  
006200*                                 KONTO                                   
006300     03 IDKST                PIC X(10).                                   
006400*                                 KOSTNADSSTÄLLE                          
006500     03 KVAVBART             PIC S9(7)           COMP-3.                  
006600*                                 AVBOKAT ANTAL ARTIKLAR                  
006700     03 KVLEVART             PIC S9(7)           COMP-3.                  
006800*                                 LEVERERAT ANTAL STYCK                   
006900     03 KVBEART              PIC S9(7)           COMP-3.                  
007000*                                 BESTÄLLT ANTAL STYCKEN                  
007100     03 KVEFRS-PACK          PIC S9(7)           COMP-3.                  
007200*                                 PACKAT EJ FAKTURERAT ANTAL STYC         
007300*                                 K                                       
007400     03 KVEFRS-OPACK         PIC S9(7)           COMP-3.                  
007500*                                 EJ FAKTURERAT OPACKAT ANTAL ST          
007600     03 KVEFRS-SKEPP         PIC S9(7)           COMP-3.                  
007700*                                 EJ FAKTURERAT ANTAL STYCK               
007800     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
007900*                                 ARTIKELPRIS NETTO                       
008000     03 PRARTNTO-LOC         PIC S9(7)V9(2)      COMP-3.                  
008100*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
008200     03 PRARTNTO-LOCPREL     PIC S9(7)V9(2)      COMP-3.                  
008300*                                 PREL NETTO SLUTKUNDSPRIS I              
008400*                                 LOKAL VALUTA                            
008500     03 KDVALISO             PIC X(3).                                    
008600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
008700     03 TIRODAT              PIC S9(7)           COMP-3.                  
008800*                                 RESTORDERDATUM         (ÅÅMMDD)         
008900     03 VKARTNTO             PIC S9(4)V9(3)      COMP-3.                  
009000*                                 ARTIKELVIKT NETTO (KG)                  
009100     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
009200*                                 ARTIKELVOLYM NETTO (CM3)                
009300     03 KDKVBRYT             PIC S9              COMP-3.                  
009400*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
009500     03 BEVOLREF             PIC X(10).                                   
009600*                                 VOLVO REFERENS                          
009700     03 IDLOPNR              PIC S9(3)           COMP-3.                  
009800*                                 LÖPNUMMER                               
009900*** END OF VILMAII-COPY LENGTH= 179 BYTES                                 
