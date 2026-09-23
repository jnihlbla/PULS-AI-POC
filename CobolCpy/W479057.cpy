000100 01  W479057.                                                             
000200*                                 ORDER I ARBETE                          
000300     03 IDDISTR              PIC S9(5)           COMP-3.                  
000400*                                 DISTRIKTNUMMER                          
000500     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000600*                                 KUNDNUMMER                              
000700     03 IDKUNDRF             PIC X(10).                                   
000800*                                 KUNDENS REFERENS (ORDERID)              
000900     03 FLOVRLEV             PIC X.                                       
001000*                                 ÖVERLEVERANS                            
001100     03 KDFAKTYP             PIC X.                                       
001200*                                 FAKTURATYP                              
001300     03 KDFRAKT              PIC S9(3)           COMP-3.                  
001400*                                 FRAKTSÄTT C1-C2 TILL KUND               
001500     03 KDORDKL              PIC S9              COMP-3.                  
001600*                                 ORDERKLASS                              
001700     03 KVORDRAD-PACK        PIC S9(5)           COMP-3.                  
001800*                                 ANTAL PACKADE ORDERRADER                
001900     03 TIBEGPAC             PIC S9(7)           COMP-3.                  
002000*                                 BEGÄRD PACKNINGSDAG    (ÅÅMMDD)         
002100     03 TIUTSKR              PIC S9(7)           COMP-3.                  
002200*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
002300     03 TIREF1               PIC S9(7)           COMP-3.                  
002400*                                 KUNDREFERENS-DATUM 1   (ÅÅMMDD)         
002500     03 TIORDREG             PIC S9(7)           COMP-3.                  
002600*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
002700     03 IDARTNR              PIC S9(9)           COMP-3.                  
002800*                                 ARTIKELNUMMER                           
002900     03 REKSIFFR             PIC S9              COMP-3.                  
003000*                                 KONTROLLSIFFRA                          
003100     03 IDARTNR-ERS          PIC S9(9)           COMP-3.                  
003200*                                 ERSATT ARTIKELNUMMER                    
003300     03 ADLEVPL              PIC S9(3)           COMP-3.                  
003400*                                 LEVERANSPLATS                           
003500     03 BERADREF             PIC X(10).                                   
003600*                                 KUNDENS RADREFERENS                     
003700     03 FLKRED               PIC X.                                       
003800*                                 KREDITERING BYTESORDER ?                
003900     03 LOW-VALUE1           PIC X.                                       
004000     03 FLDIRLEV             PIC X.                                       
004100*                                 DIREKTLEVERANS ?                        
004200     03 FLRESTN              PIC X.                                       
004300*                                 RESTNOTERING ?                          
004400     03 IDDIVORD             PIC S9(3)           COMP-3.                  
004500*                                 DIVERSEORDERNUMMER                      
004600     03 IDPRODNR             PIC S9(7)           COMP-3.                  
004700*                                 PRODUKTIONSNUMMER                       
004800     03 IDKUNDRF-RO          PIC X(10).                                   
004900*                                 KUND REF PÅ RO                          
005000     03 IDDC                 PIC X(2).                                    
005100*                                 IDENTIFIERARE LAGER                     
005200     03 KDORDTYP             PIC S9              COMP-3.                  
005300*                                 ORDERTYP                                
005400*                                 3 = SKROTORDER                          
005500     03 KDPRODSL             PIC S9(3)           COMP-3.                  
005600*                                 PRODUKTSLAG                             
005700     03 IDKONTO              PIC S9(11)          COMP-3.                  
005800*                                 KONTO                                   
005900     03 IDKST                PIC S9(5)           COMP-3.                  
006000*                                 KOSTNADSSTÄLLE   IDKST-002              
006100     03 KVAVBART             PIC S9(7)           COMP-3.                  
006200*                                 AVBOKAT ANTAL ARTIKLAR                  
006300     03 KVBEART              PIC S9(7)           COMP-3.                  
006400*                                 BESTÄLLT ANTAL STYCKEN                  
006500     03 LOW-VALUE2           PIC X(4).                                    
006600     03 KVEFRS-PACK          PIC S9(7)           COMP-3.                  
006700*                                 PACKAT EJ FAKTURERAT ANTAL STYC         
006800*                                 K                                       
006900     03 KVEFRS-OPACK         PIC S9(7)           COMP-3.                  
007000*                                 EJ FAKTURERAT OPACKAT ANTAL ST          
007100     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
007200*                                 ARTIKELPRIS NETTO                       
007300     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
007400*                                 ARTIKELSTANDARDPRIS                     
007500     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
007600*                                 ARTIKELNS SJÄLVKOSTNAD                  
007700     03 TIRODAT              PIC S9(7)           COMP-3.                  
007800*                                 RESTORDERDATUM         (ÅÅMMDD)         
007900     03 VKARTNTO             PIC S9(4)V9(3)      COMP-3.                  
008000*                                 ARTIKELVIKT NETTO (KG)                  
008100     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
008200*                                 ARTIKELVOLYM NETTO (CM3)                
008300     03 KDKVBRYT             PIC S9              COMP-3.                  
008400*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
008500     03 BEVOLREF             PIC X(10).                                   
008600*                                 VOLVO REFERENS                          
008700     03 KDRADSTA             PIC S9              COMP-3.                  
008800*                                 STATUS PÅ ORDERRAD                      
008900     03 KVLEVART             PIC S9(7)           COMP-3.                  
009000*                                 LEVERERAT ANTAL STYCK                   
009100     03 IDLOPNR              PIC S9(3)           COMP-3.                  
009200*                                 LÖPNUMMER                               
009300*** END OF VILMAII-COPY LENGTH= 164 BYTES                                 
