000100 01  W261224.                                                             
000200*                                 NEDLÄSNING AV ARTREG FÖR                
000300*                                 LTK-UPPFÖLJN OCH LAGERBALANSER.         
000400*                                 EJ UTGÅNGNA ARTIKLAR                    
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 IDARTNR              PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 IDANSK               PIC S9(3)           COMP-3.                  
001000*                                 ANSKAFFARNUMMER                         
001100     03 KDLTK                PIC S9              COMP-3.                  
001200*                                 LAGERTILLHÖRIGHETSKOD                   
001300     03 TILTK                PIC S9(5)           COMP-3.                  
001400*                                 LTK-ÄNDRINGSDATUM                       
001500     03 TIFINLV              PIC S9(5)           COMP-3.                  
001600*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
001700     03 KDVVKL               PIC S9              COMP-3.                  
001800*                                 VOLYMVÄRDESKLASS                        
001900     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
002000*                                 ARTIKELSTANDARDPRIS                     
002100     03 IDLEVNR              PIC X(5).                                    
002200*                                 LEVERANTÖRNUMMER                        
002300     03 KDGK                 PIC S9              COMP-3.                  
002400*                                 GODSMOTTAGAREKOD                        
002500     03 FLMANGK              PIC X.                                       
002600*                                 FLAGGA MANUELL GODSMOTTAGARKOD          
002700     03 IDPROD               PIC S9(3)           COMP-3.                  
002800*                                 PRODUKTKOD, DEL AV PRODUKTSLAG          
002900     03 KVSKKNST             PIC S9(3)           COMP-3.                  
003000*                                 SKROT KONSTANT                          
003100     03 TIURPROD             PIC S9(5)           COMP-3.                  
003200*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
003300     03 KVSLUTKP             PIC S9(7)           COMP-3.                  
003400*                                 SLUTKÖPSSALDO                           
003500     03 KVOVERF              PIC S9(7)           COMP-3.                  
003600*                                 ÖVERFÖRINGSSALDO                        
003700     03 IDLKTO               PIC S9(7)           COMP-3.                  
003800*                                 LAGERKONTO (FFHHHUU)                    
003900     03 KDPROD               PIC S9(3)           COMP-3.                  
004000*                                 PRODUKTIONSKOD                          
004100     03 KDKG                 PIC S9              COMP-3.                  
004200*                                 KURANSGRUPP                             
004300     03 FLJANEJ-C2           PIC X.                                       
004400*                                 JA/NEJ-FLAGGA                           
004500     03 C-LAGERDEL           OCCURS 2 TIMES.                              
004600        05 KDERS             PIC S9(3)           COMP-3.                  
004700*                                 ERSÄTTNINGSKOD                          
004800        05 KVLS              PIC S9(7)           COMP-3.                  
004900*                                 LAGERSALDO                              
005000        05 KVRESS            PIC S9(7)           COMP-3.                  
005100*                                 RESERVERAT ANTAL ARTIKLAR               
005200        05 KVAKS             PIC S9(7)           COMP-3.                  
005300*                                 ANKOMSTSALDO                            
005400        05 KVAKS-E           PIC S9(7)           COMP-3.                  
005500*                                 DEL AV EFR TILL ANDRA CLAGRET           
005600        05 KVAKS-F           PIC S9(7)           COMP-3.                  
005700*                                 DEL AV AKS TILL ANDRA CLAGRET           
005800        05 KVOKS-BULK        PIC S9(7)           COMP-3.                  
005900*                                 ORDERKÖSALDO, KLASS 2-4                 
006000        05 KVOKS-DAG         PIC S9(7)           COMP-3.                  
006100*                                 ORDERKÖSALDO, KLASS 1                   
006200        05 KVOKS-VOR         PIC S9(7)           COMP-3.                  
006300*                                 ORDERKÖSALDO, VOR                       
006400        05 SUTPO-TOT         PIC S9(7)           COMP-3.                  
006500*                                 TPO-KVANTITET, TOTAL                    
006600        05 KVROS             PIC S9(7)           COMP-3.                  
006700*                                 RESTORDERSALDO                          
006800        05 KVPB-SEP          PIC S9(6)V9(1)      COMP-3.                  
006900*                                 SEPARAT PERIODBEHOV                     
007000        05 KVPB-SATS         PIC S9(6)V9(1)      COMP-3.                  
007100*                                 SATS-PERIODBEHOV                        
007200        05 KVMP              PIC S9(7)           COMP-3.                  
007300*                                 MAXPUNKT                                
007400        05 KVSLAGER          PIC S9(7)           COMP-3.                  
007500*                                 SÄKERHETSLAGER                          
007600        05 FLSKROT-BEORD     PIC X.                                       
007700*                                 SKROTNING BEORDRAD AV ANSK              
007800     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
007900*                                 LAGEROMRÅDE                             
008000     03 ADPLATS              PIC S9(5)           COMP-3.                  
008100*                                 LAGERPLATSNUMMER                        
008200*** END OF VILMAII-COPY LENGTH= 176 BYTES                                 
