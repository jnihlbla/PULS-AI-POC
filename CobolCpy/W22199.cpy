000100 01  W22199.                                                              
000200*                                 W22199  = EXTRAKT FRÅN WDK6             
000300     03 IDARTNR              PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500     03 KDCLAGER             PIC S9              COMP-3.                  
000600*                                 CENTRALLAGERKOD                         
000700     03 TIFINLV              PIC S9(5)           COMP-3.                  
000800*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
000900     03 IDLEVNR              PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100     03 KDPRODSL             PIC S9(3)           COMP-3.                  
001200*                                 PRODUKTSLAG                             
001300     03 FLAVRART             PIC X.                                       
001400*                                 AVROPSARTIKEL                           
001500     03 FLMANQ               PIC X.                                       
001600*                                 MANUELL HEMTAGNINGSKVANTITET            
001700     03 FLMANKP              PIC X.                                       
001800*                                 MANUELL FRAMTAGEN KÖPPUNKT ?            
001900     03 IDPROD               PIC S9(3)           COMP-3.                  
002000*                                 PRODUKTKOD, DEL AV PRODUKTSLAG          
002100     03 KDHF                 PIC S9              COMP-3.                  
002200*                                 HUVUDFÖRRÅDSMÄRKNING                    
002300     03 KDVVKL               PIC S9              COMP-3.                  
002400*                                 VOLYMVÄRDESKLASS                        
002500     03 KVKP                 PIC S9(7)           COMP-3.                  
002600*                                 KÖPPUNKT                                
002700     03 KVVECKOR-BT          PIC S9(3)           COMP-3.                  
002800*                                 ANTAL VECKOR BESTÄLLNINGSTID            
002900     03 KVVECKOR-FT          PIC S9(3)           COMP-3.                  
003000*                                 ANTAL VECKOR FRYSNINGSTID               
003100     03 FLFSP                PIC X.                                       
003200*                                 FÖRDELNINGSSPÄRR                        
003300     03 FLMPB                PIC X.                                       
003400*                                 MASKINELLT UPPDAT PERIODBEHOV ?         
003500     03 KVMAD-SEP            PIC S9(6)V9(1)      COMP-3.                  
003600*                                 SEPARAT PROGNOSFEL                      
003700     03 KVMAD-TOT            PIC S9(6)V9(1)      COMP-3.                  
003800*                                 TOTALT PROGNOSFEL                       
003900     03 KVPB-SATS            PIC S9(6)V9(1)      COMP-3.                  
004000*                                 SATS-PERIODBEHOV                        
004100     03 KVPB-SEP             PIC S9(6)V9(1)      COMP-3.                  
004200*                                 SEPARAT PERIODBEHOV                     
004300     03 KVPB-VESL            PIC S9(6)V9(1)      COMP-3.                  
004400*                                 GÄLLANDE PB VID VECKOSLUT               
004500     03 REDIRLEV             PIC S9V9(2)         COMP-3.                  
004600*                                 DIREKTLEVERANSANDEL                     
004700     03 RESLJUST             PIC S9(2)V9(1)      COMP-3.                  
004800*                                 SÄKERHETSLAGER-JUSTERINGFAKTOR          
004900     03 TISLJUST             PIC S9(5)           COMP-3.                  
005000*                                 VECKA DÅ JUSTERING AV SÄKER-            
005100*                                 HETSLAGER UPPHÖR (ÅÅVV)                 
005200     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
005300*                                 ARTIKELSTANDARDPRIS                     
005400     03 KDUART               PIC X.                                       
005500*                                 UNDANTAGSARTIKEL                        
005600     03 KDGK                 PIC S9              COMP-3.                  
005700*                                 GODSMOTTAGAREKOD                        
005800     03 KDLTK                PIC S9              COMP-3.                  
005900*                                 LAGERTILLHÖRIGHETSKOD                   
006000     03 KDERS                PIC S9(3)           COMP-3.                  
006100*                                 ERSÄTTNINGSKOD                          
006200     03 KVSLAGER             PIC S9(7)           COMP-3.                  
006300*                                 SÄKERHETSLAGER                          
006400     03 KVSLAGER-NYTT        PIC S9(7)           COMP-3.                  
006500*                                 SÄKERHETSLAGER                          
006600     03 KVPB-TPO             PIC S9(6)V9(1)      COMP-3.                  
006700*                                 PERIODBEHOV FÖR TPO1 OCH TPO2           
006800     03 KDPRISKL             PIC X.                                       
006900*                                 PRISKLASS                               
007000     03 KDFREKKL             PIC X.                                       
007100*                                 FREKVENSKLASS                           
007200     03 KVBR                 PIC S9(7)           COMP-3.                  
007300*                                 BESTÄLLNINGSREST                        
007400     03 KVPALL               PIC S9(7)           COMP-3.                  
007500*                                 ANTAL I PALL                            
007600     03 KVSLAGER-NYTT2       PIC S9(7)           COMP-3.                  
007700*                                 SÄKERHETSLAGER                          
007800     03 KVQ                  PIC S9(7)           COMP-3.                  
007900*                                 EKONOMISK HEMTAGNINGSKVANTITET          
008000     03 KVQ-NYTT             PIC S9(7)           COMP-3.                  
008100*                                 EKONOMISK HEMTAGNINGSKVANTITET          
008200     03 PRARTBES             PIC S9(7)V9(2)      COMP-3.                  
008300*                                 BESTÄLLNINGSPRIS I KRONOR               
008400     03 PRORDSK              PIC S9(5)V9(2)      COMP-3.                  
008500*                                 ORDERSÄRKOSTNAD                         
008600     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
008700*                                 ARTIKELVOLYM NETTO (CM3)                
008800*** END OF VILMAII-COPY LENGTH= 118 BYTES                                 
