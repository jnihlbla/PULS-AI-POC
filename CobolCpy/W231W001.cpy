000100 01  W231W001.                                                            
000200*                                 POST FÖR SORTERING AV                   
000300*                                 ANALYSPOSTER   ANSKAFFNING              
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 FLJANEJ-NORMAL       PIC X.                                       
000700*                                 JA/NEJ-FLAGGA                           
000800     03 SORTAREA-NORMAL.                                                  
000900        05 KDVVKL-SORT       PIC S9              COMP-3.                  
001000*                                 VOLYMVÄRDESKLASS                        
001100        05 FILLER            PIC X.                                       
001200     03 SORTAREA-EJNORMAL REDEFINES SORTAREA-NORMAL.                      
001300*                                 KDSORT2-VÄRDEN:                         
001400*                                 10=HF/AVROP                             
001500*                                 20=ÖVRIGA AVROP                         
001600*                                 30=MILITÄRARTIKLAR                      
001700*                                 40=SPECIALARTIKLAR                      
001800*                                 50=100 % DIR.LEVER. ARTIKLAR            
001900*                                 60=ERSATTA ARTIKLAR                     
002000*                                 70=DEF ERSATTA ARTIKLAR                 
002100*                                 80=NYA ARTIKLAR                         
002200        05 KDSORT2           PIC S9(3)           COMP-3.                  
002300*                                 SORTERINGSFÄLT                          
002400     03 IDARTNR              PIC S9(9)           COMP-3.                  
002500*                                 ARTIKELNUMMER                           
002600     03 IDANSK               PIC S9(3)           COMP-3.                  
002700*                                 ANSKAFFARNUMMER                         
002800     03 IDLEVNR              PIC X(5).                                    
002900*                                 LEVERANTÖRNUMMER                        
003000     03 IDPROD               PIC S9(3)           COMP-3.                  
003100*                                 PRODUKTKOD, DEL AV PRODUKTSLAG          
003200     03 KDPRODSL             PIC S9(3)           COMP-3.                  
003300*                                 PRODUKTSLAG                             
003400     03 KDVVKL               PIC S9              COMP-3.                  
003500*                                 VOLYMVÄRDESKLASS                        
003600     03 KVQ                  PIC S9(7)           COMP-3.                  
003700*                                 EKONOMISK HEMTAGNINGSKVANTITET          
003800     03 KVOVERF              PIC S9(7)           COMP-3.                  
003900*                                 ÖVERFÖRINGSSALDO                        
004000     03 KVSLUTKP             PIC S9(7)           COMP-3.                  
004100*                                 SLUTKÖPSSALDO                           
004200     03 IDLKTO               PIC S9(7)           COMP-3.                  
004300*                                 LAGERKONTO (FFHHHUU)                    
004400     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
004500*                                 ARTIKELSTANDARDPRIS                     
004600     03 KDLTK                PIC S9              COMP-3.                  
004700*                                 LAGERTILLHÖRIGHETSKOD                   
004800     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
004900*                                 FUNKTIONSGRUPP                          
005000     03 KVBR                 PIC S9(7)           COMP-3.                  
005100*                                 BESTÄLLNINGSREST                        
005200     03 KVAVROP-EFTERSLAP    PIC S9(7)           COMP-3.                  
005300*                                 AVROPSKVANTITET                         
005400     03 PLAN-INLEV-DEL       OCCURS 12 TIMES.                             
005500*                                 PLANERADE INLEVERANSER 12 PER           
005600*                                 FRAM                                    
005700        05 KVAVROP-PLANINL   PIC S9(7)           COMP-3.                  
005800*                                 AVROPSKVANTITET                         
005900     03 FLJANEJ-C2           PIC X.                                       
006000*                                 JA/NEJ-FLAGGA                           
006100     03 CLAGERDEL            OCCURS 2 TIMES.                              
006200        05 KVMP              PIC S9(7)           COMP-3.                  
006300*                                 MAXPUNKT                                
006400        05 KVPB-SATS         PIC S9(6)V9(1)      COMP-3.                  
006500*                                 SATS-PERIODBEHOV                        
006600        05 KVPB-SEP          PIC S9(6)V9(1)      COMP-3.                  
006700*                                 SEPARAT PERIODBEHOV                     
006800        05 REDIRLEV          PIC S9V9(2)         COMP-3.                  
006900*                                 DIREKTLEVERANSANDEL                     
007000        05 KVAKS             PIC S9(7)           COMP-3.                  
007100*                                 ANKOMSTSALDO                            
007200        05 KVAKS-E           PIC S9(7)           COMP-3.                  
007300*                                 DEL AV EFR TILL ANDRA CLAGRET           
007400        05 KVAKS-F           PIC S9(7)           COMP-3.                  
007500*                                 DEL AV AKS TILL ANDRA CLAGRET           
007600        05 KVLS              PIC S9(7)           COMP-3.                  
007700*                                 LAGERSALDO                              
007800        05 KVRESS            PIC S9(7)           COMP-3.                  
007900*                                 RESERVERAT ANTAL ARTIKLAR               
008000        05 KVOKS             PIC S9(7)           COMP-3.                  
008100*                                 ORDERKÖSALDO                            
008200        05 KVROS             PIC S9(7)           COMP-3.                  
008300*                                 RESTORDERSALDO                          
008400        05 KVSLAGER          PIC S9(7)           COMP-3.                  
008500*                                 SÄKERHETSLAGER                          
008600        05 BEHOVSDEL         OCCURS 12 TIMES.                             
008700           07 KVBEHOV-PERIOD PIC S9(7)V9(2)      COMP-3.                  
008800*                                 BEHOV PER PERIOD                        
008900        05 RO-DEL.                                                        
009000           07 TIRODAT        PIC S9(5)           COMP-3.                  
009100*                                 RESTORDERDATUM      TIRODAT-002         
009200*                                 (AAVVD)                                 
009300           07 KVRORAD-0-4    PIC S9(7)           COMP-3.                  
009400*                                 ANTAL RO-RADER 0 - 4 VECKOR             
009500           07 KVRORAD-5-8    PIC S9(7)           COMP-3.                  
009600*                                 ANTAL RO-RADER 5 - 8 VECKOR             
009700           07 KVRORAD-9      PIC S9(7)           COMP-3.                  
009800*                                 ANTAL RO-RADER 9 VECKOR ELLER           
009900*                                 ÄLDRE                                   
010000        05 TILL-OCH-FRAN-DEL.                                             
010100           07 KVANTAL-UTLEV-12P                                           
010200                             PIC S9(7)           COMP-3.                  
010300*                                 ANTAL                                   
010400           07 KVANTAL-TILL-AR                                             
010500                             PIC S9(7)           COMP-3.                  
010600*                                 ANTAL                                   
010700           07 KVANTAL-FRAN-AR                                             
010800                             PIC S9(7)           COMP-3.                  
010900*                                 ANTAL                                   
011000           07 KVANTAL-FRAN-12P                                            
011100                             PIC S9(7)           COMP-3.                  
011200*                                 ANTAL                                   
011300           07 KVANTAL-UTCLEAR                                             
011400                             PIC S9(7)           COMP-3.                  
011500*                                 ANTAL                                   
011600           07 KVANTAL-INCLEAR                                             
011700                             PIC S9(7)           COMP-3.                  
011800*                                 ANTAL                                   
011900           07 KVANTAL-UPPINV PIC S9(7)           COMP-3.                  
012000*                                 ANTAL                                   
012100           07 KVANTAL-NEDINV PIC S9(7)           COMP-3.                  
012200*                                 ANTAL                                   
012300           07 KVANTAL-PLANINL                                             
012400                             PIC S9(7)           COMP-3.                  
012500*                                 ANTAL                                   
012600           07 KVANTAL-OPLANINL                                            
012700                             PIC S9(7)           COMP-3.                  
012800*                                 ANTAL                                   
012900           07 KVANTAL-LAN-RETUR                                           
013000                             PIC S9(7)           COMP-3.                  
013100*                                 ANTAL                                   
013200           07 KVANTAL-SKROT  PIC S9(7)           COMP-3.                  
013300*                                 ANTAL                                   
013400           07 KVANTAL-K-ORDER                                             
013500                             PIC S9(7)           COMP-3.                  
013600*                                 ANTAL                                   
013700        05 AVBOKADE-RADER.                                                
013800*                                 INDEX 1 = 1:A VECKAN I PER              
013900*                                 INDEX 2 = SISTA VECKAN I PER            
014000*                                 INDEX 3 = SUMMA ALLA VECKOR/PER         
014100           07 KVAVBRAD       OCCURS 3 TIMES                               
014200                             PIC S9(7)V9(2)      COMP-3.                  
014300*                                 AVBOKADE RADER                          
014400           07 KVINORD        OCCURS 3 TIMES                               
014500                             PIC S9(7)           COMP-3.                  
014600*                                 ANTAL INKOMNA ORDERRADER                
014700           07 KVFYSAVV       PIC S9(5)V9(2)      COMP-3.                  
014800*                                 ANTAL FYSISKA AVVIKIKELSER              
014900     03 OI-DEL.                                                           
015000        05 KVOI-PROGNOSPAV-C1                                             
015100                             PIC S9(7)           COMP-3.                  
015200*                                 PROGNOSPÅVERKANDE ORDERINGÅNG           
015300        05 KVOI-DIVERSE-C1   PIC S9(7)           COMP-3.                  
015400*                                 ORDERINGÅNG DIVERSE OCH TPO             
015500        05 KVOI-SATS-C1      PIC S9(7)           COMP-3.                  
015600*                                 ORDERINGÅNG SATSFÖRBRUKNING             
015700        05 KVOI-PROGNOSPAV-C2                                             
015800                             PIC S9(7)           COMP-3.                  
015900*                                 PROGNOSPÅVERKANDE ORDERINGÅNG           
016000        05 KVOI-DIVERSE-C2   PIC S9(7)           COMP-3.                  
016100*                                 ORDERINGÅNG DIVERSE OCH TPO             
016200        05 SUOI-AR-C1        PIC S9(11)          COMP-3.                  
016300*                                 TOTAL ORDERINGÅNGSKVANTITET             
016400*                                 UNDER KALENDERÅRET                      
016500        05 SUOI-AR-C2        PIC S9(11)          COMP-3.                  
016600*                                 TOTAL ORDERINGÅNGSKVANTITET             
016700*                                 UNDER KALENDERÅRET                      
016800        05 SUOI-PROGNOS-12P-C1                                            
016900                             PIC S9(11)          COMP-3.                  
017000*                                 SUMMA ORDERINGÅNGSKVANTITET             
017100*                                 PROGNOSPÅVERKANDE                       
017200        05 SUOI-DIVERSE-12P-C1                                            
017300                             PIC S9(11)          COMP-3.                  
017400*                                 SUMMA ORDERINGÅNGKVANTITET              
017500*                                 DIVERSE                                 
017600        05 SUOI-SATS-12P-C1  PIC S9(11)          COMP-3.                  
017700*                                 SUMMA ORDERINGÅNGSKVANTITET             
017800*                                 SATSER                                  
017900        05 SUOI-PROGNOS-12P-C2                                            
018000                             PIC S9(11)          COMP-3.                  
018100*                                 SUMMA ORDERINGÅNGSKVANTITET             
018200*                                 PROGNOSPÅVERKANDE                       
018300        05 SUOI-DIVERSE-12P-C2                                            
018400                             PIC S9(11)          COMP-3.                  
018500*                                 SUMMA ORDERINGÅNGKVANTITET              
018600*                                 DIVERSE                                 
018700*** END OF VILMAII-COPY LENGTH= 575 BYTES                                 
