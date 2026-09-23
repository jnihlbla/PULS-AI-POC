000100 01  MOD-W3O30401.                                                        
000200*                                 MOD-COPYTEXT FÖR W3030400               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDPROMR-IN.                                                   
001200*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
001300        05 MOD-IDMARKBO      PIC X.                                       
001400*                                 MARKNADSBOLAGSKOD                       
001500*                                                                         
001600        05 MOD-IDPROMRN      PIC X(2).                                    
001700*                                 PRISOMRÅDE LÖPNUMMER                    
001800     03 MOD-IDPROMR-UT.                                                   
001900*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
002000        05 MOD-IDMARKBO      PIC X.                                       
002100*                                 MARKNADSBOLAGSKOD                       
002200*                                                                         
002300        05 MOD-IDPROMRN      PIC X(2).                                    
002400*                                 PRISOMRÅDE LÖPNUMMER                    
002500     03 MOD-IDDISTR-IN       PIC X(4).                                    
002600*                                 DISTRIKTNUMMER                          
002700     03 MOD-IDDISTR-UT       PIC X(4).                                    
002800*                                 DISTRIKTNUMMER                          
002900     03 MOD-FLMRKVAL-IN      PIC X.                                       
003000*                                 FLAGGA FÖR MARKNADSVALUTA               
003100     03 MOD-FLMRKVAL-UT      PIC X.                                       
003200*                                 FLAGGA FÖR MARKNADSVALUTA               
003300     03 MOD-KDVALISO         PIC X(3).                                    
003400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003500     03 MOD-BEART-SVE        PIC X(25).                                   
003600*                                 SVENSK ARTIKELBENÄMNING                 
003700     03 MOD-KDERS            PIC Z9.                                      
003800*                                 ERSÄTTNINGSKOD                          
003900     03 MOD-KDARTKAM         PIC Z(4)9.                                   
004000*                                 TRANSFER KOD                            
004100     03 MOD-TIUPPDAT         PIC 9(6).                                    
004200*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
004300     03 MOD-BEKOPARE-RAD1    PIC X(27).                                   
004400*                                 DEL AV KÖPARNAMN                        
004500     03 MOD-KDPRODSL         PIC Z9.                                      
004600*                                 PRODUKTSLAG                             
004700     03 MOD-IDFKNGRP         PIC Z(3)9.                                   
004800*                                 FUNKTIONSGRUPP                          
004900     03 MOD-TIAVIDAT-SEN     PIC 9(6).                                    
005000*                                 SENASTE AVISERINGSDATUM  ÅÅMMDD         
005100     03 MOD-PRARTSTD         PIC Z(6)9.9(2).                              
005200*                                 ARTIKELSTANDARDPRIS                     
005300     03 MOD-PRARTSJK         PIC Z(6)9.9(2).                              
005400*                                 ARTIKELNS SJÄLVKOSTNAD                  
005500     03 MOD-PRARTSJK-MON     PIC Z(6)9.9(2).                              
005600*                                 ARTIKELNS SJÄLVKOSTNAD TILL MÅN         
005700*                                 ADSKURS                                 
005800     03 MOD-PRARTSJK-MONLOC  PIC Z(6)9.9(2).                              
005900*                                 ARTIKELNS SJÄLVKOSTNAD TILL MÅN         
006000*                                 ADSKURS                                 
006100     03 MOD-FLSOURCE         PIC X.                                       
006200     03 MOD-SUARTFSG-PER     PIC -(3)B-(3)B-(2)9.9(2).                    
006300*                                 SUMMA FSG/ART SENASTE PERIOD            
006400*                                 (AF5)                                   
006500     03 MOD-RETOTBV-PER      PIC Z9.9-.                                   
006600*                                 TG SENASTE PERIOD  (AF5)                
006700*                                                                         
006800     03 MOD-SUTOTBV-PER      PIC -(3)B-(3)B-(3)B-(2)9.9(2).               
006900*                                 TÄCKNINGSB SENASTE PERIOD (AF5)         
007000     03 MOD-SULEVANT-PER     PIC -(3)B-(3)B-(2)9.                         
007100*                                 ANTAL LEV ART SENASTE PERIOD            
007200*                                 (AF5)                                   
007300     03 MOD-SUARTFSG-AAR     PIC -(3)B-(3)B-(2)9.9(2).                    
007400*                                 SUMMA FSG/ART HITTILLS I ÅR             
007500*                                 (AF4)                                   
007600     03 MOD-REFSG-AAR        PIC Z9.9-.                                   
007700*                                 FÖRSÄLJNINGSAVVIKELSE AF4 -AF3          
007800     03 MOD-RETOTBV-AAR      PIC Z9.9-.                                   
007900*                                 TG HITTILLS I ÅR   (AF4)                
008000*                                                                         
008100     03 MOD-SUTOTBV-AAR      PIC -(3)B-(3)B-(3)B-(2)9.9(2).               
008200*                                 TÄCKNINGSB HITTILLS I ÅR  (AF4)         
008300     03 MOD-SULEVANT-AAR     PIC -(3)B-(3)B-(2)9.                         
008400*                                 ANTAL LEVERERADE ARTIKLAR               
008500*                                 HITTILLS DETTA ÅR  (AF4)                
008600     03 MOD-RELEVANT-AAR     PIC Z9.9-.                                   
008700*                                 ANTALSAVVIKELSE (AF4 - AF3)             
008800     03 MOD-SUARTFSG-FAAR    PIC -(3)B-(3)B-(2)9.9(2).                    
008900*                                 SUMMA FSG "HITTILLS I ÅR"               
009000*                                 MEN FÖREGÅENDE ÅR (AF3)                 
009100     03 MOD-RETOTBV-FAAR     PIC Z9.9-.                                   
009200*                                 TG "HITTILLS I ÅR" MEN MOTSV.           
009300*                                 FÖREGÅENDE ÅR (AF3)                     
009400     03 MOD-SUTOTBV-FAAR     PIC -(3)B-(3)B-(3)B-(2)9.9(2).               
009500*                                 TÄCKNINGSBIDRAG "HITTILLS I ÅR"         
009600*                                 MEN FÖREGÅENDE ÅR (AF3)                 
009700     03 MOD-SULEVANT-FAAR    PIC -(3)B-(3)B-(2)9.                         
009800*                                 ANTAL LEV ART "HITTILLS I ÅR"           
009900*                                 MEN FÖREGÅENDE ÅR (AF3)                 
010000     03 MOD-SUARTFSG-RAAR    PIC -(3)B-(3)B-(2)9.9(2).                    
010100*                                 SUMMA FSG/ART  RULLANDE ÅR              
010200*                                 (AF2)                                   
010300     03 MOD-REFSG-RAAR       PIC Z9.9-.                                   
010400*                                 FÖRSÄLJNINGSAVVIKELSE AF2 -AF1          
010500     03 MOD-RETOTBV-RAAR     PIC Z9.9-.                                   
010600*                                 TG RULLANDE ÅR     (AF2)                
010700     03 MOD-SUTOTBV-RAAR     PIC -(3)B-(3)B-(3)B-(2)9.9(2).               
010800*                                 TÄCKNINGSB RULLANDE ÅR    (AF2)         
010900     03 MOD-SULEVANT-RAAR    PIC -(3)B-(3)B-(2)9.                         
011000*                                 ANTAL LEV ART RULLANDE ÅR               
011100*                                 (AF2)                                   
011200     03 MOD-RELEVANT-RAAR    PIC Z9.9-.                                   
011300*                                 ANTALSAVVIKELSE (AF2 - AF1)             
011400     03 MOD-SUARTFSG-FRAAR   PIC -(3)B-(3)B-(2)9.9(2).                    
011500*                                 SUMMA FSG/ARTIKEL FÖREG.                
011600*                                 RULLANDE ÅR (AF1)                       
011700     03 MOD-RETOTBV-FRAAR    PIC Z9.9-.                                   
011800*                                 TG RULLANDE FÖREGÅENDE ÅR (AF1)         
011900*                                                                         
012000     03 MOD-SUTOTBV-FRAAR    PIC -(3)B-(3)B-(3)B-(2)9.9(2).               
012100*                                 TÄCKNINGSBIDRAG FÖR RULLANDE            
012200*                                 FÖREGÅENDE ÅR (AF1)                     
012300     03 MOD-SULEVANT-FRAAR   PIC -(3)B-(3)B-(2)9.                         
012400*                                 ANTAL LEVERERADE ARTIKLAR               
012500*                                 FÖREGÅENDE RULLANDE ÅR (AF1)            
012600     03 MOD-KDARTKAM-VALID   PIC Z(4)9.                                   
012700*                                 TRANSFER KOD                            
012800     03 MOD-ARTNR-RAB-FINNS  PIC X.                                       
012900     03 MOD-MO-RAB-PRIS      PIC Z(6)9.9(2).                              
013000*                                 ARTIKELPRIS NETTO                       
013100     03 MOD-DO-RAB-PRIS      PIC Z(6)9.9(2).                              
013200*                                 ARTIKELPRIS NETTO                       
013300     03 MOD-MO-RAB           PIC Z9.9(2).                                 
013400*                                 ARTIKELRABATT BULKORDER                 
013500     03 MOD-DO-RAB           PIC Z9.9(2).                                 
013600*                                 ARTIKELRABATT DAGORDER                  
013700     03 MOD-KVPOINT          PIC Z(7).                                    
013800*                                 POINT VALUE                             
013900     03 MOD-NORMAL-RABATT    PIC 9(2).                                    
014000*                                 RABATTKOD (ARTIKELPRIS)                 
014100     03 MOD-MO-NOR-PRIS      PIC Z(6)9.9(2).                              
014200*                                 ARTIKELPRIS NETTO                       
014300     03 MOD-DO-NOR-PRIS      PIC Z(6)9.9(2).                              
014400*                                 ARTIKELPRIS NETTO                       
014500     03 MOD-MO-NOR-RAB       PIC Z9.9(2).                                 
014600*                                 ARTIKELRABATT BULKORDER                 
014700     03 MOD-DO-NOR-RAB       PIC Z9.9(2).                                 
014800*                                 ARTIKELRABATT DAGORDER                  
014900     03 MOD-PRARTBTO-MARK    PIC Z(6)9.9(2).                              
015000*                                 BRUTTOPRIS PER MARKNAD (FOB)            
015100     03 MOD-TIUPPDAT-BTO     PIC X(6).                                    
015200*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
015300     03 MOD-TEMFSINF         PIC X(55).                                   
015400*                                 INFORMATIONSMEDDELANDE                  
015500*** END OF VILMAII-COPY LENGTH= 605 BYTES                                 
