000100 01  W4795I.                                                              
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
001700     03 IDPLKLST             PIC S9(3)           COMP-3.                  
001800*                                 PLOCKLISTNUMMER                         
001900     03 KDFAKTYP             PIC X.                                       
002000*                                 FAKTURATYP                              
002100     03 KDFRAKT              PIC S9(3)           COMP-3.                  
002200*                                 FRAKTSÄTT DC TILL KUND                  
002300     03 KDORDKL              PIC S9              COMP-3.                  
002400*                                 ORDERKLASS                              
002500     03 KDPERSON             PIC S9(3)           COMP-3.                  
002600*                                 PERSONKOD                               
002700     03 TIORDREG             PIC S9(7)           COMP-3.                  
002800*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
002900     03 IDARTNR              PIC S9(9)           COMP-3.                  
003000*                                 ARTIKELNUMMER                           
003100     03 FLDIRLEV             PIC X.                                       
003200*                                 DIREKTLEVERANS ?                        
003300     03 IDKUNDRF-RO          PIC X(10).                                   
003400*                                 KUND REF PÅ RO                          
003500     03 KDORDTYP             PIC S9              COMP-3.                  
003600*                                 ORDERTYP                                
003700     03 KDPRODSL             PIC S9(3)           COMP-3.                  
003800*                                 PRODUKTSLAG                             
003900     03 IDKONTO              PIC S9(11)          COMP-3.                  
004000*                                 KONTO                                   
004100     03 IDKST                PIC X(10).                                   
004200*                                 KOSTNADSSTÄLLE                          
004300     03 KVBEART              PIC S9(7)           COMP-3.                  
004400*                                 BESTÄLLT ANTAL STYCKEN                  
004500     03 KVLEVART             PIC S9(7)           COMP-3.                  
004600*                                 LEVERERAT ANTAL STYCK                   
004700     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
004800*                                 ARTIKELPRIS NETTO                       
004900     03 PRARTNTO-LOC         PIC S9(7)V9(2)      COMP-3.                  
005000*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
005100     03 PRARTNTO-LOCPREL     PIC S9(7)V9(2)      COMP-3.                  
005200*                                 PREL NETTO SLUTKUNDSPRIS I              
005300*                                 LOKAL VALUTA                            
005400     03 KDVALISO             PIC X(3).                                    
005500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005600     03 VKARTNTO             PIC S9(4)V9(3)      COMP-3.                  
005700*                                 ARTIKELVIKT NETTO (KG)                  
005800     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
005900*                                 ARTIKELVOLYM NETTO (CM3)                
006000     03 IDARTNR-SATS         PIC S9(9)           COMP-3.                  
006100*                                 ARTIKELNUMMER FÖR SATS                  
006200     03 FLPRTILL             PIC X.                                       
006300*                                 PRISTILLÄGGS FLAGGA                     
006400     03 KDRADSTA             PIC S9              COMP-3.                  
006500*                                 STATUS PÅ ORDERRAD                      
006600     03 KDARTURS             PIC X(2).                                    
006700*                                 ARTIKELURSPRUNGSKOD                     
006800     03 KVAVBART             PIC S9(7)           COMP-3.                  
006900*                                 AVBOKAT ANTAL ARTIKLAR                  
007000     03 IDUSER-OREG          PIC X(8).                                    
007100*                                 ANVÄNDARENS SÄKERHETS ID                
007200     03 IDUSER-PACK          PIC X(8).                                    
007300*                                 ANVÄNDARENS SÄKERHETS ID                
007400*** END OF VILMAII-COPY LENGTH= 140 BYTES                                 
