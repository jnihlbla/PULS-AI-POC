000100 01  W26156.                                                              
000200*                                 SKROTORDER I ARBETE KOMPL M. AN         
000300*                                 SK.                                     
000400     03 IDDISTR              PIC S9(5)           COMP-3.                  
000500*                                 DISTRIKTNUMMER                          
000600     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000700*                                 KUNDNUMMER                              
000800     03 IDKUNDRF             PIC X(10).                                   
000900*                                 KUNDENS REFERENS (ORDERID)              
001000     03 KDFAKTYP             PIC X.                                       
001100*                                 FAKTURATYP                              
001200     03 KDFRAKT              PIC S9(3)           COMP-3.                  
001300*                                 FRAKTSÄTT DC TILL KUND                  
001400     03 KDORDKL              PIC S9              COMP-3.                  
001500*                                 ORDERKLASS                              
001600     03 TIBEGPAC             PIC S9(7)           COMP-3.                  
001700*                                 BEGÄRD PACKNINGSDAG    (ÅÅMMDD)         
001800     03 TIUTSKR              PIC S9(7)           COMP-3.                  
001900*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
002000     03 TIREF1               PIC S9(7)           COMP-3.                  
002100*                                 KUNDREFERENS-DATUM 1   (ÅÅMMDD)         
002200     03 TIORDREG             PIC S9(7)           COMP-3.                  
002300*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
002400     03 IDARTNR              PIC S9(9)           COMP-3.                  
002500*                                 ARTIKELNUMMER                           
002600     03 REKSIFFR             PIC S9              COMP-3.                  
002700*                                 KONTROLLSIFFRA                          
002800     03 IDARTNR-ERS          PIC S9(9)           COMP-3.                  
002900*                                 ERSATT ARTIKELNUMMER                    
003000     03 ADLEVPL              PIC S9(3)           COMP-3.                  
003100*                                 LEVERANSPLATS                           
003200     03 BERADREF             PIC X(10).                                   
003300*                                 KUNDENS RADREFERENS                     
003400     03 FLKRED               PIC X.                                       
003500*                                 KREDITERING BYTESORDER ?                
003600     03 LOW-VALUE1           PIC X.                                       
003700     03 FLDIRLEV             PIC X.                                       
003800*                                 DIREKTLEVERANS ?                        
003900     03 FLRESTN              PIC X.                                       
004000*                                 RESTNOTERING ?                          
004100     03 IDDIVORD             PIC S9(3)           COMP-3.                  
004200*                                 DIVERSEORDERNUMMER                      
004300     03 IDPRODNR             PIC S9(7)           COMP-3.                  
004400*                                 PRODUKTIONSNUMMER                       
004500     03 IDKUNDRF-RO          PIC X(10).                                   
004600*                                 KUND REF PÅ RO                          
004700     03 IDDC                 PIC X(2).                                    
004800*                                 IDENTIFIERARE LAGER                     
004900     03 KDORDTYP             PIC S9              COMP-3.                  
005000*                                 ORDERTYP                                
005100     03 KDPRODSL             PIC S9(3)           COMP-3.                  
005200*                                 PRODUKTSLAG                             
005300     03 IDKONTO              PIC S9(11)          COMP-3.                  
005400*                                 KONTO                                   
005500     03 IDKST                PIC X(10).                                   
005600*                                 KOSTNADSSTÄLLE                          
005700     03 KVAVBART             PIC S9(7)           COMP-3.                  
005800*                                 AVBOKAT ANTAL ARTIKLAR                  
005900     03 KVBEART              PIC S9(7)           COMP-3.                  
006000*                                 BESTÄLLT ANTAL STYCKEN                  
006100     03 LOW-VALUE2           PIC X(4).                                    
006200     03 KVEFRS-PACK          PIC S9(7)           COMP-3.                  
006300*                                 PACKAT EJ FAKTURERAT ANTAL STYC         
006400*                                 K                                       
006500     03 KVEFRS-OPACK         PIC S9(7)           COMP-3.                  
006600*                                 EJ FAKTURERAT OPACKAT ANTAL ST          
006700     03 KVEFRS-SKEPP         PIC S9(7)           COMP-3.                  
006800*                                 EJ FAKTURERAT ANTAL STYCK               
006900     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
007000*                                 ARTIKELPRIS NETTO                       
007100     03 PRARTNTO-LOC         PIC S9(7)V9(2)      COMP-3.                  
007200*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
007300     03 PRARTNTO-LOCPREL     PIC S9(7)V9(2)      COMP-3.                  
007400*                                 PREL NETTO SLUTKUNDSPRIS I              
007500*                                 LOKAL VALUTA                            
007600     03 KDVALISO             PIC X(3).                                    
007700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
007800     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
007900*                                 ARTIKELSTANDARDPRIS                     
008000     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
008100*                                 ARTIKELNS SJÄLVKOSTNAD                  
008200     03 TIRODAT              PIC S9(7)           COMP-3.                  
008300*                                 RESTORDERDATUM         (ÅÅMMDD)         
008400     03 VKARTNTO             PIC S9(4)V9(3)      COMP-3.                  
008500*                                 ARTIKELVIKT NETTO (KG)                  
008600     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
008700*                                 ARTIKELVOLYM NETTO (CM3)                
008800     03 KDKVBRYT             PIC S9              COMP-3.                  
008900*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
009000     03 IDANSK               PIC S9(3)           COMP-3.                  
009100*                                 ANSKAFFARNUMMER                         
009200     03 GRP-INT              PIC X(7).                                    
009300     03 FLSKROT-AUTO         PIC X.                                       
009400*                                 SKROTNING AUTOMATISKT BEORDRAD          
009500*** END OF VILMAII-COPY LENGTH= 177 BYTES                                 
