000100 01  W479054.                                                             
000200*                                 EV. FAKTURERADE RADER                   
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 IDDISTR              PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000800*                                 KUNDNUMMER                              
000900     03 IDKUNDRF             PIC X(10).                                   
001000*                                 KUNDENS REFERENS (ORDERID)              
001100     03 IDDC                 PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 IDRADNR-KO           PIC S9(5)           COMP-3.                  
001400*                                 RADNUMMER KUNDORDER                     
001500     03 IDPRODNR             PIC S9(7)           COMP-3.                  
001600*                                 PRODUKTIONSNUMMER                       
001700     03 KDFAKTYP             PIC X.                                       
001800*                                 FAKTURATYP                              
001900     03 KDFRAKT              PIC S9(3)           COMP-3.                  
002000*                                 FRAKTSÄTT DC TILL KUND                  
002100     03 KDORDKL              PIC S9              COMP-3.                  
002200*                                 ORDERKLASS                              
002300     03 KDPERSON             PIC S9(3)           COMP-3.                  
002400*                                 PERSONKOD                               
002500     03 TIORDREG             PIC S9(7)           COMP-3.                  
002600*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
002700     03 IDARTNR              PIC S9(9)           COMP-3.                  
002800*                                 ARTIKELNUMMER                           
002900     03 FLKRED               PIC X.                                       
003000*                                 KREDITERING BYTESORDER ?                
003100     03 FLDIRLEV             PIC X.                                       
003200*                                 DIREKTLEVERANS ?                        
003300     03 IDDIVORD             PIC S9(3)           COMP-3.                  
003400*                                 DIVERSEORDERNUMMER                      
003500     03 IDKUNDRF-RO          PIC X(10).                                   
003600*                                 KUND REF PÅ RO                          
003700     03 KDORDTYP             PIC S9              COMP-3.                  
003800*                                 ORDERTYP                                
003900     03 KDPRODSL             PIC S9(3)           COMP-3.                  
004000*                                 PRODUKTSLAG                             
004100     03 IDKONTO              PIC S9(11)          COMP-3.                  
004200*                                 KONTO                                   
004300     03 IDKST                PIC X(10).                                   
004400*                                 KOSTNADSSTÄLLE                          
004500     03 IDANALYS             PIC X(12).                                   
004600*                                 ANALYSNUMMER                            
004700     03 KVBEART              PIC S9(7)           COMP-3.                  
004800*                                 BESTÄLLT ANTAL STYCKEN                  
004900     03 KVLEVART             PIC S9(7)           COMP-3.                  
005000*                                 LEVERERAT ANTAL STYCK                   
005100     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
005200*                                 ARTIKELPRIS NETTO                       
005300     03 PRARTNTO-LOC         PIC S9(7)V9(2)      COMP-3.                  
005400*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
005500     03 PRARTNTO-LOCPREL     PIC S9(7)V9(2)      COMP-3.                  
005600*                                 PREL NETTO SLUTKUNDSPRIS I              
005700*                                 LOKAL VALUTA                            
005800     03 KDVALISO             PIC X(3).                                    
005900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
006000     03 VKARTNTO             PIC S9(4)V9(3)      COMP-3.                  
006100*                                 ARTIKELVIKT NETTO (KG)                  
006200     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
006300*                                 ARTIKELVOLYM NETTO (CM3)                
006400     03 IDARTNR-SATS         PIC S9(9)           COMP-3.                  
006500*                                 ARTIKELNUMMER FÖR SATS                  
006600     03 FLPRTILL             PIC X.                                       
006700*                                 PRISTILLÄGGS FLAGGA                     
006800     03 KDRADSTA             PIC S9              COMP-3.                  
006900*                                 STATUS PÅ ORDERRAD                      
007000     03 KDARTURS             PIC X(2).                                    
007100*                                 ARTIKELURSPRUNGSKOD                     
007200     03 KVAVBART             PIC S9(7)           COMP-3.                  
007300*                                 AVBOKAT ANTAL ARTIKLAR                  
007400     03 IDUSER-OREG          PIC X(8).                                    
007500*                                 ANVÄNDARENS SÄKERHETS ID                
007600     03 IDUSER-PACK          PIC X(8).                                    
007700*                                 ANVÄNDARENS SÄKERHETS ID                
007800     03 IDLEVNR              PIC X(5).                                    
007900*                                 LEVERANTÖRNUMMER                        
008000     03 IDBORD               PIC X(3).                                    
008100*                                 PACK-BORD                               
008200     03 KDVAT                PIC X(2).                                    
008300*                                 MOMSKOD                                 
008400     03 BEART-VIPS           PIC X(25).                                   
008500*                                 VIPS ARTIKELBENÄMNING                   
008600*                                 PÅ DEALERNS SPRÅK                       
008700     03 IDPRC.                                                            
008800*                                 PRODUKTIONSKANAL                        
008900        05 IDPRCBAS          PIC X(3).                                    
009000*                                 PRC-BAS                                 
009100        05 IDPRCVAR          PIC X.                                       
009200*                                 PRC-VARIANT                             
009300     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
009400*                                 LAGEROMRÅDE                             
009500*** END OF VILMAII-COPY LENGTH= 194 BYTES                                 
