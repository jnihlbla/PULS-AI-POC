000100 01  W479053.                                                             
000200*                                 ORDER I ARBETE                          
000300     03 IDDISTR              PIC S9(5)           COMP-3.                  
000400*                                 DISTRIKTNUMMER                          
000500     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000600*                                 KUNDNUMMER                              
000700     03 IDKUNDRF             PIC X(10).                                   
000800*                                 KUNDENS REFERENS (ORDERID)              
000900     03 KDFAKTYP             PIC X.                                       
001000*                                 FAKTURATYP                              
001100     03 KDFRAKT              PIC S9(3)           COMP-3.                  
001200*                                 FRAKTSÄTT DC TILL KUND                  
001300     03 KDORDKL              PIC S9              COMP-3.                  
001400*                                 ORDERKLASS                              
001500     03 TIBEGPAC             PIC S9(7)           COMP-3.                  
001600*                                 BEGÄRD PACKNINGSDAG    (ÅÅMMDD)         
001700     03 TIUTSKR              PIC S9(7)           COMP-3.                  
001800*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
001900     03 TIREF1               PIC S9(7)           COMP-3.                  
002000*                                 KUNDREFERENS-DATUM 1   (ÅÅMMDD)         
002100     03 TIORDREG             PIC S9(7)           COMP-3.                  
002200*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
002300     03 IDARTNR              PIC S9(9)           COMP-3.                  
002400*                                 ARTIKELNUMMER                           
002500     03 REKSIFFR             PIC S9              COMP-3.                  
002600*                                 KONTROLLSIFFRA                          
002700     03 IDARTNR-ERS          PIC S9(9)           COMP-3.                  
002800*                                 ERSATT ARTIKELNUMMER                    
002900     03 ADLEVPL              PIC S9(3)           COMP-3.                  
003000*                                 LEVERANSPLATS                           
003100     03 BERADREF             PIC X(10).                                   
003200*                                 KUNDENS RADREFERENS                     
003300     03 FLKRED               PIC X.                                       
003400*                                 KREDITERING BYTESORDER ?                
003500     03 LOW-VALUE1           PIC X.                                       
003600     03 FLDIRLEV             PIC X.                                       
003700*                                 DIREKTLEVERANS ?                        
003800     03 FLRESTN              PIC X.                                       
003900*                                 RESTNOTERING ?                          
004000     03 IDDIVORD             PIC S9(3)           COMP-3.                  
004100*                                 DIVERSEORDERNUMMER                      
004200     03 IDPRODNR             PIC S9(7)           COMP-3.                  
004300*                                 PRODUKTIONSNUMMER                       
004400     03 IDKUNDRF-RO          PIC X(10).                                   
004500*                                 KUND REF PÅ RO                          
004600     03 IDDC                 PIC X(2).                                    
004700*                                 IDENTIFIERARE LAGER                     
004800     03 KDORDTYP             PIC S9              COMP-3.                  
004900*                                 ORDERTYP                                
005000     03 KDPRODSL             PIC S9(3)           COMP-3.                  
005100*                                 PRODUKTSLAG                             
005200     03 IDKONTO              PIC S9(11)          COMP-3.                  
005300*                                 KONTO                                   
005400     03 IDKST                PIC X(10).                                   
005500*                                 KOSTNADSSTÄLLE                          
005600     03 KVAVBART             PIC S9(7)           COMP-3.                  
005700*                                 AVBOKAT ANTAL ARTIKLAR                  
005800     03 KVBEART              PIC S9(7)           COMP-3.                  
005900*                                 BESTÄLLT ANTAL STYCKEN                  
006000     03 LOW-VALUE2           PIC X(4).                                    
006100     03 KVEFRS-PACK          PIC S9(7)           COMP-3.                  
006200*                                 PACKAT EJ FAKTURERAT ANTAL STYC         
006300*                                 K                                       
006400     03 KVEFRS-OPACK         PIC S9(7)           COMP-3.                  
006500*                                 EJ FAKTURERAT OPACKAT ANTAL ST          
006600     03 KVEFRS-SKEPP         PIC S9(7)           COMP-3.                  
006700*                                 EJ FAKTURERAT ANTAL STYCK               
006800     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
006900*                                 ARTIKELPRIS NETTO                       
007000     03 PRARTNTO-LOC         PIC S9(7)V9(2)      COMP-3.                  
007100*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
007200     03 PRARTNTO-LOCPREL     PIC S9(7)V9(2)      COMP-3.                  
007300*                                 PREL NETTO SLUTKUNDSPRIS I              
007400*                                 LOKAL VALUTA                            
007500     03 KDVALISO             PIC X(3).                                    
007600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
007700     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
007800*                                 ARTIKELSTANDARDPRIS                     
007900     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
008000*                                 ARTIKELNS SJÄLVKOSTNAD                  
008100     03 TIRODAT              PIC S9(7)           COMP-3.                  
008200*                                 RESTORDERDATUM         (ÅÅMMDD)         
008300     03 VKARTNTO             PIC S9(4)V9(3)      COMP-3.                  
008400*                                 ARTIKELVIKT NETTO (KG)                  
008500     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
008600*                                 ARTIKELVOLYM NETTO (CM3)                
008700     03 KDKVBRYT             PIC S9              COMP-3.                  
008800*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
008900*** END OF VILMAII-COPY LENGTH= 167 BYTES                                 
