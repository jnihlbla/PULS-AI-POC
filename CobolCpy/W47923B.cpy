000100 01  W47923B.                                                             
000200*                                                                         
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 IDDISTR              PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000800*                                 KUNDNUMMER                              
000900     03 IDPRODNR             PIC S9(7)           COMP-3.                  
001000*                                 PRODUKTIONSNUMMER                       
001100     03 IDORDER              PIC S9(7)           COMP-3.                  
001200*                                 VOLVO PARTS ORDERNUMMER                 
001300     03 IDDC                 PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 TIBEGPAC             PIC S9(7)           COMP-3.                  
001600*                                 BEGÄRD PACKNINGSDAG    (ÅÅMMDD)         
001700     03 TIORDREG             PIC S9(7)           COMP-3.                  
001800*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
001900     03 IDPLKLST             PIC S9(3)           COMP-3.                  
002000*                                 PLOCKLISTNUMMER                         
002100     03 IDARTNR              PIC S9(9)           COMP-3.                  
002200*                                 ARTIKELNUMMER                           
002300     03 ADLAGOMR-DLB         PIC S9(3)           COMP-3.                  
002400*                                 LAGEROMRÅDE                             
002500     03 ADGANG-DLB           PIC S9(3)           COMP-3.                  
002600*                                 GÅNG                                    
002700     03 ADPLATS-DLB          PIC S9(5)           COMP-3.                  
002800*                                 LAGERPLATSNUMMER                        
002900     03 DARFS                PIC 9(12).                                   
003000*                                 KLART FÖR TRANSPORT                     
003100     03 KDFRAKT              PIC S9(3)           COMP-3.                  
003200*                                 FRAKTSÄTT DC TILL KUND                  
003300     03 KDORDKL              PIC S9              COMP-3.                  
003400*                                 ORDERKLASS                              
003500     03 KVBEART              PIC S9(7)           COMP-3.                  
003600*                                 BESTÄLLT ANTAL STYCKEN                  
003700     03 KVLEVART             PIC S9(7)           COMP-3.                  
003800*                                 LEVERERAT ANTAL STYCK                   
003900     03 VKARTNTO             PIC S9(4)V9(3)      COMP-3.                  
004000*                                 ARTIKELVIKT NETTO (KG) MED EMB          
004100     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
004200*                                 ARTIKELVOLYM (CM3)                      
004300     03 FLDIRLEV             PIC X.                                       
004400*                                 DIREKTLEVERANS ?                        
004500     03 IDKUNDRF-RO          PIC X(10).                                   
004600*                                 KUND REF PÅ RO                          
004700     03 IDKOLLI              PIC S9(5)           COMP-3.                  
004800*                                 KOLLINUMMER                             
004900     03 IDUSER               PIC X(8).                                    
005000*                                 ANVÄNDARENS SÄKERHETS ID                
005100     03 KVLEVART2            PIC S9(7)           COMP-3.                  
005200*                                 FAKTISKT LEVERERAT ANTAL I KOLL         
005300*                                 IT                                      
005400     03 IDPRC.                                                            
005500*                                 PRODUKTIONSKANAL                        
005600        05 IDPRCBAS          PIC X(3).                                    
005700*                                 PRC-BAS                                 
005800        05 IDPRCVAR          PIC X.                                       
005900*                                 PRC-VARIANT                             
006000     03 IDSHIFT              PIC X.                                       
006100*                                 SHIFT IDENTITET                         
006200     03 KDPRODSL             PIC S9(3)           COMP-3.                  
006300*                                 PRODUKTSLAG                             
006400     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
006500*                                 ARTIKELSTANDARDPRIS                     
006600     03 TIPACKN              PIC S9(7)           COMP-3.                  
006700*                                 PACKNINGSDATUM         (ÅÅMMDD)         
006800     03 IDPLOCK              PIC S9(7)           COMP-3.                  
006900*                                 PLOCKARE                                
007000     03 KDRADSTA             PIC S9              COMP-3.                  
007100*                                 STATUS PÅ ORDERRAD                      
007200     03 IDSYSTEM             PIC X(4).                                    
007300*                                 VOLVO VCCS SYSTEMNUMMER                 
007400     03 IDPSN                PIC 9(3).                                    
007500*                                 PROPER SHIPPING NAME                    
007600     03 KDFAKTYP             PIC X.                                       
007700*                                 FAKTURATYP                              
007800     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
007900*                                 ARTIKELPRIS NETTO                       
008000     03 PRARTNTO-LOC         PIC S9(7)V9(2)      COMP-3.                  
008100*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
008200     03 ADLAGOMR-VERKLIG     PIC S9(3)           COMP-3.                  
008300*                                 LAGEROMRÅDE                             
008400     03 DASUPREF             PIC 9(8).                                    
008500*                                 SÄNDNINGSDATUM DIREKTLEVERANTÖR         
008600     03 DATRPAVT.                                                         
008700*                                 TRANSPORTAVGÅNGSTIDPUNKT                
008800        05 DATRPAVD          PIC 9(8).                                    
008900*                                 TRANSPORTAVGÅNGSDATUM                   
009000        05 TIHHMM            PIC S9(5)           COMP-3.                  
009100*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
009200     03 FLLDCKND             PIC X.                                       
009300*                                 FL LDC-KUND                             
009400     03 IDFAKT               PIC S9(7)           COMP-3.                  
009500*                                 FAKTURANUMMER                           
009600     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
009700*                                 FUNKTIONSGRUPP                          
009800     03 IDLBBET              PIC X(12).                                   
009900*                                 LASTBÄRARBETECKNING                     
010000     03 IDLEVNR              PIC X(5).                                    
010100*                                 LEVERANTÖRNUMMER                        
010200     03 IDORDNR5             PIC 9(5).                                    
010300*                                 ORDERNUMMER                             
010400     03 IDPSN-DC             PIC 9(3).                                    
010500*                                 PROPER SHIPPING NAME PER XDC            
010600     03 IDSHIPM              PIC 9(7).                                    
010700*                                 SKEPPNINGSNUMMER                        
010800     03 IDTRP.                                                            
010900*                                 TRANSPORTIDENTITET                      
011000        05 IDTRPLOS          PIC X(3).                                    
011100*                                 TRANSPORTLÖSNING                        
011200        05 IDTRPVAR          PIC X(2).                                    
011300*                                 TRANSPORTLÖSNINGSGRUPP                  
011400     03 IDUSER-PACK          PIC X(8).                                    
011500*                                 ANVÄNDARENS SÄKERHETS ID                
011600     03 KDARTURS             PIC X(2).                                    
011700*                                 ARTIKELURSPRUNGSKOD                     
011800     03 KDFARLIG             PIC S9              COMP-3.                  
011900*                                 KOD FÖR FARLIGT GODS                    
012000     03 KDKOLLI              PIC X(8).                                    
012100*                                 KOLLIKOD                                
012200     03 KDPRCGRP             PIC X(5).                                    
012300*                                 PRODUKTIONSKANALSGRUPP                  
012400     03 KDSORT               PIC X(2).                                    
012500*                                 SORT-KOD                                
012600     03 KDVIA                PIC X(2).                                    
012700*                                 KOD FöR LEVERANS VIA                    
012800     03 KVQPACK-0            PIC S9(5)           COMP-3.                  
012900*                                 ANTAL I Q0 FÖRPACKNING                  
013000     03 KVQPACK-1            PIC S9(5)           COMP-3.                  
013100*                                 ANTAL I Q1 FÖRPACKNING                  
013200     03 KVQPACK-2            PIC S9(5)           COMP-3.                  
013300*                                 ANTAL I Q2 FÖRPACKNING                  
013400     03 KVQPACK-3            PIC S9(5)           COMP-3.                  
013500*                                 ANTAL I Q3 FÖRPACKNING                  
013600     03 KVQPACK-4            PIC S9(5)           COMP-3.                  
013700*                                 ANTAL I Q4 FÖRPACKNING                  
013800     03 KVULOAD              PIC S9(7)           COMP-3.                  
013900*                                 MIN ENHETSLAST FRÅN LEVERANTÖR          
014000     03 TIBEGTID             PIC S9(4).                                   
014100     03 TIFAKT               PIC S9(7)           COMP-3.                  
014200*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
014300     03 TIFAKTID             PIC S9(7)           COMP-3.                  
014400*                                 FAKTURERINGSTID                         
014500     03 TILASTN              PIC S9(7)           COMP-3.                  
014600*                                 LASTNINGSDATUM         (ÅÅMMDD)         
014700     03 TILASTID             PIC S9(7)           COMP-3.                  
014800*                                 LASTNINGSTID                            
014900     03 TIPACTID             PIC S9(7)           COMP-3.                  
015000*                                 PACKNINGSTID  TTMMSS                    
015100     03 TIREGTID             PIC S9(7)           COMP-3.                  
015200*                                 REGISTRERINGSTID                        
015300     03 TIREPDAT             PIC S9(7)           COMP-3.                  
015400*                                 REPAIR DATE                             
015500     03 TISKEPPN             PIC S9(7)           COMP-3.                  
015600*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
015700     03 TISKPTID             PIC S9(7)           COMP-3.                  
015800*                                 SKEPPNINGSTID                           
015900     03 TISUPTID             PIC S9(5)           COMP-3.                  
016000*                                 SÄNDNINGSTID DIREKTLEVERANTÖR           
016100     03 TIUTSKR              PIC S9(7)           COMP-3.                  
016200*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
016300     03 TIUTSTID             PIC S9(7)           COMP-3.                  
016400*                                 UTSKRIFTSTID (TTMMSS)                   
016500     03 IDVIN                PIC X(17).                                   
016600*                                 VIN ID FORDON                           
016700*** END OF VILMAII-COPY LENGTH= 320 BYTES                                 
