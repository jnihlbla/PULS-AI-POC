000100 01  W55159.                                                              
000200*                                 ARTIKLAR FÖR PRISSÄTTNING AV            
000300*                                 KALKYLPÅLÄGG                            
000400*                                                                         
000500     03 IDARTNR              PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700     03 IDLEVNR              PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900     03 KDERS                PIC S9(3)           COMP-3.                  
001000*                                 ERSÄTTNINGSKOD                          
001100     03 KDGK                 PIC S9              COMP-3.                  
001200*                                 GODSMOTTAGAREKOD                        
001300     03 FLIART               PIC X.                                       
001400*                                 ARTIKELN INGÅR I SATS                   
001500     03 KVLS                 PIC S9(7)           COMP-3.                  
001600*                                 LAGERSALDO                              
001700     03 KVBEHOVAR            PIC S9(7)           COMP-3.                  
001800*                                 BERÄKNAT ÅRSBEHOV AV EN ARTIKEL         
001900     03 KDFORP.                                                           
002000*                                 FÖRPACKNINGSKOD                         
002100        05 KDFORPPL          PIC 9.                                       
002200*                                 FÖRPACKNINGSPLATS                       
002300        05 KDFORPGP          PIC 9(2).                                    
002400*                                 FÖRPACKNINGSGRUPP                       
002500        05 KDFORPUF          PIC 9.                                       
002600*                                 UPPRÄKNINGSFAKTOR                       
002700     03 KVQPACK-0            PIC S9(5)           COMP-3.                  
002800*                                 ANTAL I Q0 FÖRPACKNING                  
002900     03 KVQPACK-1            PIC S9(5)           COMP-3.                  
003000*                                 ANTAL I Q1 FÖRPACKNING                  
003100     03 KVQPACK-2            PIC S9(5)           COMP-3.                  
003200*                                 ANTAL I Q2 FÖRPACKNING                  
003300     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
003400*                                 ARTIKELNS SJÄLVKOSTNAD                  
003500     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
003600*                                 ARTIKELSTANDARDPRIS                     
003700     03 TIFINLV              PIC S9(5)           COMP-3.                  
003800*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
003900     03 PRDIRLON             PIC S9(4)V9(3)      COMP-3.                  
004000*                                 DIREKT LÖN                              
004100     03 PRDMTRL              PIC S9(6)V9(3)      COMP-3.                  
004200*                                 DIREKT MATERIAL                         
004300     03 PROVRPAL             PIC S9(4)V9(3)      COMP-3.                  
004400*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
004500     03 BEFT                 PIC S9(3)           COMP-3.                  
004600*                                 FÖRPACKNINGSTYP                         
004700     03 IDARTNR-EMBQ0        PIC S9(9)           COMP-3.                  
004800*                                 EMBALLAGEARTIKELNR FÖR Q0               
004900     03 IDARTNR-EMBQ1        PIC S9(9)           COMP-3.                  
005000*                                 EMBALLAGEARTIKELNR FÖR Q1               
005100     03 IDARTNR-EMBQ2        PIC S9(9)           COMP-3.                  
005200*                                 EMBALLAGEARTIKELNR FÖR Q2               
005300     03 IDARTNR-EMBQ3        PIC S9(9)           COMP-3.                  
005400*                                 EMBALLAGEARTIKELNR FÖR Q3               
005500     03 IDARTNR-EMBQ4        PIC S9(9)           COMP-3.                  
005600*                                 EMBALLAGEARTIKELNR FÖR Q4               
005700     03 KDVTH                PIC S9              COMP-3.                  
005800*                                 KOD FÖR OMKOSTNADSBÄRANDE AVD.          
005900     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
006000*                                 LAGEROMRÅDE                             
006100     03 ADGANG               PIC S9(3)           COMP-3.                  
006200*                                 GÅNG                                    
006300     03 ADPLATS              PIC S9(5)           COMP-3.                  
006400*                                 LAGERPLATSNUMMER                        
006500     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
006600*                                 FUNKTIONSGRUPP                          
006700     03 KDPRODSL             PIC S9(3)           COMP-3.                  
006800*                                 PRODUKTSLAG                             
006900     03 KDFARLIG             PIC S9              COMP-3.                  
007000*                                 KOD FÖR FARLIGT GODS                    
007100     03 KVLASTB              PIC 9(4).                                    
007200*                                 ANTAL ARTIKLAR I EN LASTBÄRARE          
007300*** END OF VILMAII-COPY LENGTH= 106 BYTES                                 
