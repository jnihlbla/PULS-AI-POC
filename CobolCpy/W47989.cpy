000100 01  W47989.                                                              
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
002100     03 KDORDKL              PIC S9              COMP-3.                  
002200*                                 ORDERKLASS                              
002300     03 KDFRAKT              PIC S9(3)           COMP-3.                  
002400*                                 FRAKTSÄTT DC TILL KUND                  
002500     03 KDRADSTA             PIC S9              COMP-3.                  
002600*                                 STATUS PÅ ORDERRAD                      
002700     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
002800*                                 LAGEROMRÅDE                             
002900     03 IDLEVNR              PIC X(5).                                    
003000*                                 LEVERANTÖRNUMMER                        
003100     03 KDFAKTYP             PIC X.                                       
003200*                                 FAKTURATYP                              
003300     03 TIBEGPAC             PIC S9(7)           COMP-3.                  
003400*                                 BEGÄRD PACKNINGSDAG    (ÅÅMMDD)         
003500     03 TIUTSKR              PIC S9(7)           COMP-3.                  
003600*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
003700     03 KVORDRAD             PIC S9(5)           COMP-3.                  
003800*                                 ANTAL ORDERRADER                        
003900     03 KVORDRAD-PACK        PIC S9(5)           COMP-3.                  
004000*                                 ANTAL PACKADE ORDERRADER                
004100     03 TIREF1               PIC S9(7)           COMP-3.                  
004200*                                 KUNDREFERENS-DATUM 1   (ÅÅMMDD)         
004300     03 TIORDREG             PIC S9(7)           COMP-3.                  
004400*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
004500     03 IDARTNR-ERS          PIC S9(9)           COMP-3.                  
004600*                                 ERSATT ARTIKELNUMMER                    
004700     03 ADLEVPL              PIC S9(3)           COMP-3.                  
004800*                                 LEVERANSPLATS                           
004900     03 BERADREF             PIC X(10).                                   
005000*                                 KUNDENS RADREFERENS                     
005100     03 FLKRED               PIC X.                                       
005200*                                 KREDITERING BYTESORDER ?                
005300     03 LOW-VALUE1           PIC X.                                       
005400     03 FLDIRLEV             PIC X.                                       
005500*                                 DIREKTLEVERANS ?                        
005600     03 FLRESTN              PIC X.                                       
005700*                                 RESTNOTERING ?                          
005800     03 IDDIVORD             PIC S9(3)           COMP-3.                  
005900*                                 DIVERSEORDERNUMMER                      
006000     03 IDKUNDRF-RO          PIC X(10).                                   
006100*                                 KUND REF PÅ RO                          
006200     03 KDORDTYP             PIC S9              COMP-3.                  
006300*                                 ORDERTYP                                
006400     03 KDPRODSL             PIC S9(3)           COMP-3.                  
006500*                                 PRODUKTSLAG                             
006600     03 IDKONTO              PIC S9(11)          COMP-3.                  
006700*                                 KONTO                                   
006800     03 IDKST                PIC S9(5)           COMP-3.                  
006900*                                 KOSTNADSSTÄLLE   IDKST-002              
007000     03 LOW-VALUE2           PIC X(4).                                    
007100     03 KVBEART              PIC S9(7)           COMP-3.                  
007200*                                 BESTÄLLT ANTAL STYCKEN                  
007300     03 KVAVBART             PIC S9(7)           COMP-3.                  
007400*                                 AVBOKAT ANTAL ARTIKLAR                  
007500     03 KVLEVART             PIC S9(7)           COMP-3.                  
007600*                                 LEVERERAT ANTAL STYCK                   
007700     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
007800*                                 ARTIKELPRIS NETTO                       
007900     03 TIRODAT              PIC S9(7)           COMP-3.                  
008000*                                 RESTORDERDATUM         (ÅÅMMDD)         
008100     03 VKARTNTO             PIC S9(4)V9(3)      COMP-3.                  
008200*                                 ARTIKELVIKT NETTO (KG)                  
008300     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
008400*                                 ARTIKELVOLYM NETTO (CM3)                
008500     03 KDKVBRYT             PIC S9              COMP-3.                  
008600*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
008700     03 KVEFRS-PACK          PIC S9(7)           COMP-3.                  
008800*                                 PACKAT EJ FAKTURERAT ANTAL STYC         
008900*                                 K                                       
009000     03 KVEFRS-OPACK         PIC S9(7)           COMP-3.                  
009100*                                 EJ FAKTURERAT OPACKAT ANTAL ST          
009200*** END OF VILMAII-COPY LENGTH= 156 BYTES                                 
