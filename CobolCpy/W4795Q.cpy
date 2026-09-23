000100 01  W4795Q.                                                              
000200*                                 EFR-DATA                                
000300     03 IDDISTR              PIC S9(5)           COMP-3.                  
000400*                                 DISTRIKTNUMMER                          
000500     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000600*                                 KUNDNUMMER                              
000700     03 IDKUNDRF             PIC X(10).                                   
000800*                                 KUNDENS REFERENS (ORDERID)              
000900     03 IDPRODNR             PIC S9(7)           COMP-3.                  
001000*                                 PRODUKTIONSNUMMER                       
001100     03 IDPURAD              PIC S9(5)           COMP-3.                  
001200*                                 RADNUMMER PÅ PACKUNDERLAG               
001300     03 IDPLKLST             PIC S9(3)           COMP-3.                  
001400*                                 PLOCKLISTNUMMER                         
001500     03 IDARTNR              PIC S9(9)           COMP-3.                  
001600*                                 ARTIKELNUMMER                           
001700     03 REKSIFFR             PIC S9              COMP-3.                  
001800*                                 KONTROLLSIFFRA                          
001900     03 IDDC                 PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 IDPRC.                                                            
002200*                                 PRODUKTIONSKANAL                        
002300        05 IDPRCBAS          PIC X(3).                                    
002400*                                 PRC-BAS                                 
002500        05 IDPRCVAR          PIC X.                                       
002600*                                 PRC-VARIANT                             
002700     03 KDORDKL              PIC S9              COMP-3.                  
002800*                                 ORDERKLASS                              
002900     03 KDFRAKT              PIC S9(3)           COMP-3.                  
003000*                                 FRAKTSÄTT DC TILL KUND                  
003100     03 KDRADSTA             PIC S9              COMP-3.                  
003200*                                 STATUS PÅ ORDERRAD                      
003300     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
003400*                                 LAGEROMRÅDE                             
003500     03 IDLEVNR              PIC X(5).                                    
003600*                                 LEVERANTÖRNUMMER                        
003700     03 KDFAKTYP             PIC X.                                       
003800*                                 FAKTURATYP                              
003900     03 TIBEGPAC             PIC S9(7)           COMP-3.                  
004000*                                 BEGÄRD PACKNINGSDAG    (ÅÅMMDD)         
004100     03 KVORDRAD             PIC S9(5)           COMP-3.                  
004200*                                 ANTAL ORDERRADER                        
004300     03 KVORDRAD-PACK        PIC S9(5)           COMP-3.                  
004400*                                 ANTAL PACKADE ORDERRADER                
004500     03 TIREF1               PIC S9(7)           COMP-3.                  
004600*                                 KUNDREFERENS-DATUM 1   (ÅÅMMDD)         
004700     03 TIORDREG             PIC S9(7)           COMP-3.                  
004800*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
004900     03 IDARTNR-ERS          PIC S9(9)           COMP-3.                  
005000*                                 ERSATT ARTIKELNUMMER                    
005100     03 ADLEVPL              PIC S9(3)           COMP-3.                  
005200*                                 LEVERANSPLATS                           
005300     03 BERADREF             PIC X(10).                                   
005400*                                 KUNDENS RADREFERENS                     
005500     03 FLKRED               PIC X.                                       
005600*                                 KREDITERING BYTESORDER ?                
005700     03 LOW-VALUE1           PIC X.                                       
005800     03 FLDIRLEV             PIC X.                                       
005900*                                 DIREKTLEVERANS ?                        
006000     03 FLRESTN              PIC X.                                       
006100*                                 RESTNOTERING ?                          
006200     03 IDDIVORD             PIC S9(3)           COMP-3.                  
006300*                                 DIVERSEORDERNUMMER                      
006400     03 IDKUNDRF-RO          PIC X(10).                                   
006500*                                 KUND REF PÅ RO                          
006600     03 KDORDTYP             PIC S9              COMP-3.                  
006700*                                 ORDERTYP                                
006800     03 KDPRODSL             PIC S9(3)           COMP-3.                  
006900*                                 PRODUKTSLAG                             
007000     03 IDKONTO              PIC S9(11)          COMP-3.                  
007100*                                 KONTO                                   
007200     03 IDKST                PIC S9(5)           COMP-3.                  
007300*                                 KOSTNADSSTÄLLE   IDKST-002              
007400     03 LOW-VALUE2           PIC X(4).                                    
007500     03 KVBEART              PIC S9(7)           COMP-3.                  
007600*                                 BESTÄLLT ANTAL STYCKEN                  
007700     03 KVAVBART             PIC S9(7)           COMP-3.                  
007800*                                 AVBOKAT ANTAL ARTIKLAR                  
007900     03 KVLEVART             PIC S9(7)           COMP-3.                  
008000*                                 LEVERERAT ANTAL STYCK                   
008100     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
008200*                                 ARTIKELPRIS NETTO                       
008300     03 TIRODAT              PIC S9(7)           COMP-3.                  
008400*                                 RESTORDERDATUM         (ÅÅMMDD)         
008500     03 TIUTSKR              PIC S9(7)           COMP-3.                  
008600*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
008700     03 VKARTNTO             PIC S9(4)V9(3)      COMP-3.                  
008800*                                 ARTIKELVIKT NETTO (KG)                  
008900     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
009000*                                 ARTIKELVOLYM NETTO (CM3)                
009100     03 KDKVBRYT             PIC S9              COMP-3.                  
009200*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
009300     03 KVEFRS-PACK          PIC S9(7)           COMP-3.                  
009400*                                 PACKAT EJ FAKTURERAT ANTAL STYC         
009500*                                 K                                       
009600     03 KVEFRS-OPACK         PIC S9(7)           COMP-3.                  
009700*                                 EJ FAKTURERAT OPACKAT ANTAL ST          
009800     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
009900*                                 ARTIKELSTANDARDPRIS                     
010000     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
010100*                                 ARTIKELNS SJÄLVKOSTNAD                  
010200*** END OF VILMAII-COPY LENGTH= 170 BYTES                                 
