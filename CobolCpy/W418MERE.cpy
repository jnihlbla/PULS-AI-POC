000100 01  MERE-W418MERE.                                                       
000200*                                 LÄNKAREA TILL W418MERE  -               
000300*                                 SKAPAR MEMO TILL REMISS-                
000400*                                 ANSVARIG                                
000500     03 MERE-IDMAIL          PIC X(60).                                   
000600*                                 MAIL ADRESS                             
000700     03 MERE-IDDISTR         PIC S9(5)           COMP-3.                  
000800*                                 DISTRIKTNUMMER                          
000900     03 MERE-IDKUNDNR        PIC S9(7)           COMP-3.                  
001000*                                 KUNDNUMMER                              
001100     03 MERE-IDRAPPNR        PIC 9(7).                                    
001200*                                 RAPPORT NUMMER                          
001300     03 MERE-IDARTNR         PIC S9(9)           COMP-3.                  
001400*                                 ARTIKELNUMMER                           
001500     03 MERE-IDRADNR         PIC S9(5)           COMP-3.                  
001600*                                 RADNUMMER                               
001700     03 MERE-KDARBTYP        PIC X(8).                                    
001800*                                 TYP AV ARBETE                           
001900     03 MERE-IDPERSON        PIC S9(3)           COMP-3.                  
002000*                                 PERSONKOD                               
002100     03 MERE-ANM-INFO.                                                    
002200*                                                                         
002300        05 MERE-KDANMORS     PIC X(2).                                    
002400*                                 ORSAK TILL LEVERANSANMÄRKNING           
002500        05 MERE-IDFAKT       PIC S9(7)           COMP-3.                  
002600*                                 FAKTURANUMMER                           
002700        05 MERE-IDKOLLI      PIC S9(5)           COMP-3.                  
002800*                                 KOLLINUMMER                             
002900        05 MERE-IDKUNDRF     PIC X(10).                                   
003000*                                 KUNDENS REFERENS (ORDERID)              
003100        05 MERE-IDORDNR5-FILLER REDEFINES MERE-IDKUNDRF.                  
003200           07 MERE-IDORDNR5  PIC 9(5).                                    
003300*                                 ORDERNUMMER                             
003400           07 FILLER         PIC X(5).                                    
003500        05 MERE-IDORDNR7-FILLER REDEFINES MERE-IDKUNDRF.                  
003600           07 MERE-IDORDNR7  PIC 9(7).                                    
003700*                                 ORDERNUMMER                             
003800           07 FILLER         PIC X(3).                                    
003900        05 MERE-KDFAKTYP     PIC X.                                       
004000         88 MERE-KDFAKTYP-HANDELS                                         
004100                             VALUE 'R'.                                   
004200         88 MERE-KDFAKTYP-KONSIGN                                         
004300                             VALUE 'K'.                                   
004400         88 MERE-KDFAKTYP-INTERN                                          
004500                             VALUE 'N'.                                   
004600         88 MERE-KDFAKTYP-GRATIS                                          
004700                             VALUE 'G'.                                   
004800         88 MERE-KDFAKTYP-TULL                                            
004900                             VALUE 'F'.                                   
005000         88 MERE-KDFAKTYP-PROFORMA                                        
005100                             VALUE 'P'.                                   
005200*                                 FAKTURATYP                              
005300        05 MERE-KVLEVANM-BEKR                                             
005400                             PIC S9(7)           COMP-3.                  
005500*                                 BEKRÄFTAT RETURANTAL                    
005600        05 MERE-PRARTBTO     PIC S9(7)V9(2)      COMP-3.                  
005700*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
005800        05 MERE-TIFAKT       PIC S9(7)           COMP-3.                  
005900*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
006000     03 MERE-FAKT-INFO.                                                   
006100*                                                                         
006200        05 MERE-KVBEART-Q    PIC S9(7)           COMP-3.                  
006300*                                 BESTÄLLT KVANTANPASSAT ANTAL            
006400        05 MERE-FLDIRLEV     PIC X.                                       
006500*                                 DIREKTLEVERANS ?                        
006600        05 MERE-IDPRODNR     PIC S9(7)           COMP-3.                  
006700*                                 PRODUKTIONSNUMMER                       
006800        05 MERE-IDUSER-OREG  PIC X(8).                                    
006900*                                 ANSVARIGT USERID ORDERREG.              
007000        05 MERE-IDUSER-PACK  PIC X(8).                                    
007100*                                 ANSVARIGT USERID PACKARE                
007200        05 MERE-KDORDKL      PIC S9              COMP-3.                  
007300         88 MERE-KDORDKL-VOR VALUE +0.                                    
007400         88 MERE-KDORDKL-DAG VALUE +1.                                    
007500         88 MERE-KDORDKL-2   VALUE +2.                                    
007600         88 MERE-KDORDKL-SNABB                                            
007700                             VALUE +2.                                    
007800         88 MERE-KDORDKL-SPECIAL                                          
007900                             VALUE +3.                                    
008000         88 MERE-KDORDKL-KVANT                                            
008100                             VALUE +4.                                    
008200         88 MERE-KDORDKL-SATS                                             
008300                             VALUE +5.                                    
008400*                                 ORDERKLASS                              
008500        05 MERE-KVLEVART     PIC S9(7)           COMP-3.                  
008600*                                 LEVERERAT ANTAL STYCK                   
008700        05 MERE-KVORDRAD     PIC S9(5)           COMP-3.                  
008800*                                 ANTAL ORDERRADER                        
008900        05 MERE-TIREGDAT     PIC S9(7)           COMP-3.                  
009000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
009100        05 MERE-VKORDBTO-KOLLI                                            
009200                             PIC S9(6)V9(1)      COMP-3.                  
009300*                                 ORDERVIKT BRUTTO PER KOLLI              
009400        05 MERE-VKORDNTO-TOT PIC S9(6)V9(1)      COMP-3.                  
009500*                                 ORDERVIKT NETTO (KG)                    
009600        05 MERE-VKORDNTO-KOLLI                                            
009700                             PIC S9(6)V9(1)      COMP-3.                  
009800*                                 ORDERVIKT NETTO PER KOLLI               
009900        05 MERE-VKTARA       PIC S9(6)V9(1)      COMP-3.                  
010000*                                 TARAVIKT (KG)                           
010100     03 MERE-ART-INFO.                                                    
010200*                                                                         
010300        05 MERE-ADLAGOMR     PIC S9(3)           COMP-3.                  
010400*                                 LAGEROMRÅDE                             
010500        05 MERE-ADGANG       PIC S9(3)           COMP-3.                  
010600*                                 GÅNG                                    
010700        05 MERE-ADPLATS      PIC S9(5)           COMP-3.                  
010800*                                 LAGERPLATSNUMMER                        
010900        05 MERE-BEART        PIC X(25).                                   
011000*                                 ARTIKELBENÄMNING                        
011100        05 MERE-IDANSK       PIC S9(3)           COMP-3.                  
011200*                                 ANSKAFFARNUMMER                         
011300        05 MERE-IDFKNGRP     PIC S9(5)           COMP-3.                  
011400*                                 FUNKTIONSGRUPP                          
011500        05 MERE-KDERS        PIC S9(3)           COMP-3.                  
011600*                                 ERSÄTTNINGSKOD                          
011700        05 MERE-KDPRODSL     PIC S9(3)           COMP-3.                  
011800*                                 PRODUKTSLAG                             
011900        05 MERE-KVINVS       PIC S9(7)           COMP-3.                  
012000*                                 INVENTERINGSSALDO                       
012100        05 MERE-KVLS         PIC S9(7)           COMP-3.                  
012200*                                 LAGERSALDO                              
012300        05 MERE-KVPB-TOT     PIC S9(6)V9(1)      COMP-3.                  
012400*                                 TOTALT PERIODBEHOV                      
012500        05 MERE-PRARTBTO-EXP PIC S9(7)V9(2)      COMP-3.                  
012600*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
012700        05 MERE-PRINK        PIC S9(7)V9(2)      COMP-3.                  
012800*                                 INKÖPSPRIS                              
012900        05 MERE-REKSIFFR     PIC 9.                                       
013000*                                 KONTROLLSIFFRA                          
013100        05 MERE-TIINVDAT     PIC S9(5)           COMP-3.                  
013200*                                 INVENTERINGSDATUM                       
013300        05 MERE-VKART        PIC S9(7)           COMP-3.                  
013400*                                 ARTIKELVIKT (G)                         
013500     03 MERE-TEXT-INFO.                                                   
013600*                                                                         
013700        05 MERE-TEANMNOT-REG OCCURS 3 TIMES                               
013800                             PIC X(70).                                   
013900*                                 FRI TEXT FRÅN REGISTRERINGEN            
014000        05 MERE-TEANMNOT-ADM OCCURS 3 TIMES                               
014100                             PIC X(70).                                   
014200*                                 FRI TEXT FRÅN ADMINISTRATION            
014300        05 MERE-TEANMNOT-REM OCCURS 3 TIMES                               
014400                             PIC X(70).                                   
014500*                                 FRI TEXT FRÅN REMISSINSTANS             
014600        05 MERE-TEANMNOT-RET OCCURS 3 TIMES                               
014700                             PIC X(70).                                   
014800*                                 FRI TEXT FRÅN RETURAVDELNINGEN          
014900*** END OF VILMAII-COPY LENGTH= 1089 BYTES                                
