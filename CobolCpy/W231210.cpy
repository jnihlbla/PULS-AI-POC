000100 01  W231210.                                                             
000200*                                 NEDLÄSNING AV ARTREG FÖR                
000300*                                 ANALYSER   ANSKAFFNING                  
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 TIFINLV              PIC S9(5)           COMP-3.                  
000900*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
001000     03 FLAVRART             PIC X.                                       
001100*                                 AVROPSARTIKEL                           
001200     03 IDANSK               PIC S9(3)           COMP-3.                  
001300*                                 ANSKAFFARNUMMER                         
001400     03 IDLEVNR              PIC X(5).                                    
001500*                                 LEVERANTÖRNUMMER                        
001600     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
001700*                                 FUNKTIONSGRUPP                          
001800     03 KDPRODSL             PIC S9(3)           COMP-3.                  
001900*                                 PRODUKTSLAG                             
002000     03 IDFTG                PIC S9(3)           COMP-3.                  
002100*                                 FÖRETAGSID EKONOM REDOVISNING K         
002200*                                 DFTG-003                                
002300     03 IDPROD               PIC S9(3)           COMP-3.                  
002400*                                 PRODUKTKOD, DEL AV PRODUKTSLAG          
002500     03 KDHF                 PIC S9              COMP-3.                  
002600*                                 HUVUDFÖRRÅDSMÄRKNING                    
002700     03 KDVVKL               PIC S9              COMP-3.                  
002800*                                 VOLYMVÄRDESKLASS                        
002900     03 KVLAAN               PIC S9(7)           COMP-3.                  
003000*                                 LÅNESALDO                               
003100     03 KVQ                  PIC S9(7)           COMP-3.                  
003200*                                 EKONOMISK HEMTAGNINGSKVANTITET          
003300     03 KVOVERF              PIC S9(7)           COMP-3.                  
003400*                                 ÖVERFÖRINGSSALDO                        
003500     03 KVSLUTKP             PIC S9(7)           COMP-3.                  
003600*                                 SLUTKÖPSSALDO                           
003700     03 IDLKTO               PIC S9(7)           COMP-3.                  
003800*                                 LAGERKONTO (FFHHHUU)                    
003900     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
004000*                                 ARTIKELSTANDARDPRIS                     
004100     03 KDUART               PIC X.                                       
004200*                                 UNDANTAGSARTIKEL                        
004300     03 KDLTK                PIC S9              COMP-3.                  
004400*                                 LAGERTILLHÖRIGHETSKOD                   
004500     03 KDERS                PIC S9(3)           COMP-3.                  
004600*                                 ERSÄTTNINGSKOD                          
004700     03 KVBR                 PIC S9(7)           COMP-3.                  
004800*                                 BESTÄLLNINGSREST                        
004900     03 KVAVROP-EFTERSLAP    PIC S9(7)           COMP-3.                  
005000*                                 AVROPSKVANTITET                         
005100     03 KVAVROP-EFTERSLAP-X REDEFINES KVAVROP-EFTERSLAP                   
005200                             PIC X(4).                                    
005300     03 PLAN-INLEV-DEL       OCCURS 12 TIMES.                             
005400*                                 PLANERADE INLEVERANSER 12 PER           
005500*                                 FRAM                                    
005600        05 KVAVROP-PLANINL   PIC S9(7)           COMP-3.                  
005700*                                 AVROPSKVANTITET                         
005800     03 FLJANEJ-C2           PIC X.                                       
005900*                                 JA/NEJ-FLAGGA                           
006000     03 CLAGERDEL            OCCURS 2 TIMES.                              
006100        05 KVMP              PIC S9(7)           COMP-3.                  
006200*                                 MAXPUNKT                                
006300        05 KVPB-SATS         PIC S9(6)V9(1)      COMP-3.                  
006400*                                 SATS-PERIODBEHOV                        
006500        05 KVPB-SEP          PIC S9(6)V9(1)      COMP-3.                  
006600*                                 SEPARAT PERIODBEHOV                     
006700        05 REDIRLEV          PIC S9V9(2)         COMP-3.                  
006800*                                 DIREKTLEVERANSANDEL                     
006900        05 KVAKS             PIC S9(7)           COMP-3.                  
007000*                                 ANKOMSTSALDO                            
007100        05 KVAKS-E           PIC S9(7)           COMP-3.                  
007200*                                 DEL AV EFR TILL ANDRA CLAGRET           
007300        05 KVAKS-F           PIC S9(7)           COMP-3.                  
007400*                                 DEL AV AKS TILL ANDRA CLAGRET           
007500        05 KVLS              PIC S9(7)           COMP-3.                  
007600*                                 LAGERSALDO                              
007700        05 KVRESS            PIC S9(7)           COMP-3.                  
007800*                                 RESERVERAT ANTAL ARTIKLAR               
007900        05 KVROS             PIC S9(7)           COMP-3.                  
008000*                                 RESTORDERSALDO                          
008100        05 KVSLAGER          PIC S9(7)           COMP-3.                  
008200*                                 SÄKERHETSLAGER                          
008300        05 BEHOVSDEL         OCCURS 12 TIMES.                             
008400           07 KVBEHOV-PERIOD PIC S9(7)V9(2)      COMP-3.                  
008500*                                 BEHOV PER PERIOD                        
008600*** END OF VILMAII-COPY LENGTH= 320 BYTES                                 
