000100 01  W231L001.                                                            
000200*                                 LÄNKAREA FÖR LÄSNING AV ARTINFO         
000300*                                 OCH MATERIALFÖRSÖRJNINGSINFO            
000400*                                 SAMT EKONOMIINFO                        
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAES-ART-START      VALUE +1.                                    
000700     03 FLJANEJ-ARTIKEL      PIC X.                                       
000800*                                 JA/NEJ-FLAGGA                           
000900     03 FLJANEJ-C2           PIC X.                                       
001000*                                 JA/NEJ-FLAGGA                           
001100     03 IO-AREA.                                                          
001200        05 IDARTNR           PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400        05 TIFINLV           PIC S9(5)           COMP-3.                  
001500*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
001600        05 TIERSDAT          PIC S9(5)           COMP-3.                  
001700*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
001800        05 FLAVRART          PIC X.                                       
001900*                                 AVROPSARTIKEL                           
002000        05 IDANSK            PIC S9(3)           COMP-3.                  
002100*                                 ANSKAFFARNUMMER                         
002200        05 IDLEVNR           PIC X(5).                                    
002300*                                 LEVERANTÖRNUMMER                        
002400        05 IDFKNGRP          PIC S9(5)           COMP-3.                  
002500*                                 FUNKTIONSGRUPP                          
002600        05 KDPRODSL          PIC S9(3)           COMP-3.                  
002700*                                 PRODUKTSLAG                             
002800        05 IDFTG             PIC 9(2).                                    
002900*                                 FÖRETAGSID EKONOM REDOVISNING           
003000        05 IDPROD            PIC S9(3)           COMP-3.                  
003100*                                 PRODUKTKOD, DEL AV PRODUKTSLAG          
003200        05 KDHF              PIC S9              COMP-3.                  
003300*                                 HUVUDFÖRRÅDSMÄRKNING                    
003400        05 KDVVKL            PIC S9              COMP-3.                  
003500*                                 VOLYMVÄRDESKLASS                        
003600        05 KVLAAN            PIC S9(7)           COMP-3.                  
003700*                                 LÅNESALDO                               
003800        05 KVQ               PIC S9(7)           COMP-3.                  
003900*                                 EKONOMISK HEMTAGNINGSKVANTITET          
004000        05 KVOVERF           PIC S9(7)           COMP-3.                  
004100*                                 ÖVERFÖRINGSSALDO                        
004200        05 KVSLUTKP          PIC S9(7)           COMP-3.                  
004300*                                 SLUTKÖPSSALDO                           
004400        05 IDLKTO            PIC S9(7)           COMP-3.                  
004500*                                 LAGERKONTO (FFHHHUU)                    
004600        05 PRARTSTD          PIC S9(7)V9(2)      COMP-3.                  
004700*                                 ARTIKELSTANDARDPRIS                     
004800        05 CLAGERDEL         OCCURS 2 TIMES.                              
004900           07 KVMP           PIC S9(7)           COMP-3.                  
005000*                                 MAXPUNKT                                
005100           07 KVPB-SATS      PIC S9(6)V9(1)      COMP-3.                  
005200*                                 SATS-PERIODBEHOV                        
005300           07 KVPB-SEP       PIC S9(6)V9(1)      COMP-3.                  
005400*                                 SEPARAT PERIODBEHOV                     
005500           07 REDIRLEV       PIC S9V9(2)         COMP-3.                  
005600*                                 DIREKTLEVERANSANDEL                     
005700*** END OF VILMAII-COPY LENGTH= 87 BYTES                                  
