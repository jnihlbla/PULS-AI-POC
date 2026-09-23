000010 PROCESS DYNAM                                                            
000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W3352200.                                                
000400*AUTHOR.         ELEONOR ÖSTRÖM.                                          
000500*DATE-WRITTEN.   07/04/19.                                                
000600                                                                          
000700*    REMARKS.                                                             
001300*    FUNKTION:                                                            
001400*        DETTA PGM SKRIVER UT ARTIKEL-INFORMATION                         
001500*        MED HJÄLP AV ETT URVAL                                           
001600*        SOM VIA SOP-PARAMETRAR                                           
001700*        KOMMIT FRÅN BILD 3331.                                           
001900*        UTSKRIFTSVAL                                                     
002000*        OCH SORTERINGS-ALTERNATIV KOMMER OCKSÅ DENNA VÄG.                
002100*                                                                         
002200*        PROGRAMMET LÄSER      WDC1                                       
002300*        PROGRAMMET LÄSER      WDC2                                       
002400*        PROGRAMMET LÄSER      WDB1A                                      
002500*        PROGRAMMET LÄSER      WDB2                                       
002610*        PROGRAMMET LÄSER DB2  TP7PART                                    
002700*                                                                         
002800*                                                                         
002900*        ETRACKER 4054855 070419/EÖ                                       
003000*                                                                         
003100*    ABENDKODER:                                                          
003200*        U0016 -  . . . .                                                 
003300*        U1000 -  . . . .                                                 
003400*                                                                         
003500                                                                          
003600     EJECT                                                                
003700 ENVIRONMENT DIVISION.                                                    
003800     SKIP2                                                                
003900 INPUT-OUTPUT SECTION.                                                    
004000                                                                          
004100 FILE-CONTROL.                                                            
004200     SKIP2                                                                
004300*          --- PARAMETRAR FRÅN BILD 3331                                  
004400     SELECT W33522                     ASSIGN TO W33522D1.                
004500     SKIP2                                                                
004600*          --- PULS DAGLAGERFIL FRÅN W9104200                             
004700     SELECT W91042                     ASSIGn TO W33522D2.                
004800     SKIP2                                                                
004900*          --- LISTA ARTIKEL-INFORMATION PAPPERSLISTA                     
005000     SELECT W33522-001                 ASSIGN TO W33522D3.                
005100     SKIP2                                                                
005200*          --- SORTERINGSFIL                                              
005300     SELECT SORTFIL                    ASSIGN TO W33522DS.                
005400     EJECT                                                                
005500 DATA DIVISION.                                                           
005600     SKIP2                                                                
005700 FILE SECTION.                                                            
005800     SKIP2                                                                
005900 FD  W33522                                                               
006000     RECORDING       F                                                    
006100     BLOCK CONTAINS  0.                                                   
006200     SKIP2                                                                
006300*01  -COPY W33522      -L.                                                
006400     SKIP3                                                                
006500 FD  W91042                                                               
006600     RECORDING       F                                                    
006700     BLOCK CONTAINS  0.                                                   
006800     SKIP2                                                                
006900*01  -COPY W91042      -L.                                                
007000     SKIP3                                                                
007100 FD  W33522-001                                                           
007200     RECORDING       V                                                    
007300     BLOCK CONTAINS  0.                                                   
007600 01  W33522-001-RAD              PIC X(134).                              
007700     EJECT                                                                
007800 SD  SORTFIL.                                                             
007900     SKIP2                                                                
008000*01  POST -COPY W33522S     -PRE SORT-                                    
008100     EJECT                                                                
008200 WORKING-STORAGE SECTION.                                                 
008300     SKIP2                                                                
008400                                                                          
008500*    -COPY WY2000W1                                                       
008600                                                                          
008700*    -COPY WY2000W2                                                       
008720                                                                          
008730*    -COPY WY2000W3                                                       
008800                                                                          
008900*    -- CHECKED BY WY2000                                                 
009000 77  IDPGM                       PIC X(8)    Value 'W3352200'.            
009100 01  FILLER                      PIC X(24) VALUE 'IMS-SEKTION ='.         
009200 77  IMS-SEKTION                 PIC X(30).                               
009300 77  CURRENT-SECTION             PIC X(50)   VALUE ' '.                   
009400 77  IX                          PIC S9(4) COMP SYNC.                     
009410 77  INDX                        PIC S9(4) COMP SYNC.                     
009600 77  TAB-IX                      PIC S9(4) COMP SYNC.                     
009700 77  RAB-INDX                    PIC 9(2)    VALUE ZERO.                  
009800 77  TAB-TECKEN                  PIC X     VALUE '	'.                     
010100 77  ANT-LB-POST                 PIC S9(9) COMP SYNC VALUE Zero.          
010200 77  JA                          PIC X       VALUE 'J'.                   
010300 77  NEJ                         PIC X       VALUE 'N'.                   
010400 77  LAENGD                      PIC 9(4)    VALUE 134.                   
010500 77  FLAGGA                      PIC X       VALUE 'N'.                   
010510 77  FL-DISTR                    PIC X       VALUE 'N'.                   
010600 77  FOERSTA-MAIL                PIC X       VALUE 'J'.                   
010700 77  WS-RETAILPRIS               PIC S9(9)V9(2) VALUE ZERO COMP-3.        
010710 77  WS-RETAILPRIS-NOR-DO        PIC S9(9)V9(2) VALUE ZERO COMP-3.        
010720 77  WS-RETAILPRIS-NOR-MO        PIC S9(9)V9(2) VALUE ZERO COMP-3.        
010800 77  SPAR-KDARTRAB               PIC 9(2)    VALUE ZERO.                  
010810 77  AAR-SEDAN-TIERSDAT          PIC S9(3) COMP-3 VALUE ZERO.             
010820 77  W-IDPARTNR                  PIC X(9)    VALUE SPACE.                 
010900 77  WS-IDDISTR                  PIC S9(5) COMP-3 VALUE ZERO.             
010901 77  WS-IDDISTR-DB2              PIC S9(5) COMP-3 VALUE ZERO.             
010902 77  TEST-SAMMA-IDDISTR          PIC S9(5) COMP-3 VALUE ZERO.             
010910 77  GMT-SAKNAS-SW               PIC X       VALUE 'N'.                   
010920     88  GMT-SAKNAS                          VALUE 'J'.                   
011000                                                                          
011010 01  WS-PLUS-VECKOR              PIC S9(3) COMP-3 VALUE ZERO.             
011020 01  WS-TIFINLV                  PIC 9(5).                                
011030 01  FILLER REDEFINES WS-TIFINLV.                                         
011040     03  WS-TIFINLV-AAVV         PIC 9(4).                                
011050     03  FILLER                  PIC 9(1).                                
011060 01  WS-AKT-AAVV                 PIC 9(4).                                
011070 01  FILLER REDEFINES WS-AKT-AAVV.                                        
011080     03  WS-AKT-AA               PIC 9(2).                                
011090     03  WS-AKT-VV               PIC 9(2).                                
011091 01  WS-AKT-AAVV-NUM             PIC S9(5) COMP-3.                        
011092                                                                          
011093 01  AAR-VECKA                   PIC 9(5).                                
011094 01  FILLER REDEFINES AAR-VECKA.                                          
011095     03  AAR                     PIC 99.                                  
011096     03  VECKA                   PIC 99.                                  
011097     03  FILLER                  PIC  9.                                  
011098     SKIP3                                                                
011099 01  AAVVD-WEEK                  PIC 9(5) VALUE ZERO.                     
011100 01  FILLER REDEFINES AAVVD-WEEK.                                         
011101     03  AA-WEEK                 PIC 99.                                  
011102     03  VV-WEEK                 PIC 99.                                  
011103     03  FILLER                  PIC  9.                                  
011104                                                                          
011110 77  W33522-EOF-SW               PIC X       VALUE 'N'.                   
011200     88  END-OF-W33522                       VALUE 'J'.                   
011300                                                                          
011400 77  W91042-EOF-SW               PIC X       VALUE 'N'.                   
011500     88  END-OF-W91042                       VALUE 'J'.                   
011600                                                                          
011700 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
011800     88  END-OF-SORTFIL                      VALUE 'J'.                   
011900                                                                          
012000 77  URVAL-SW                    PIC X       VALUE 'N'.                   
012100     88  URVAL-OK                            VALUE 'J'.                   
012200                                                                          
012300 77  URVAL-PROMR-SW              PIC X       VALUE 'N'.                   
012400     88  URVAL-PROMR-OK                      VALUE 'J'.                   
012500                                                                          
012600 77  AVBRYT-SW                   PIC X       VALUE 'N'.                   
012700     88  AVBRYTES-EJ                         VALUE 'N'.                   
012800     88  AVBRYT                              VALUE 'J'.                   
012900 77  TRAEFF-SW                   PIC X       Value 'N'.                   
013000     88  TRAEFF-OK                           Value 'J'.                   
013100     88  TRAEFF-NEJ                          Value 'N'.                   
013200                                                                          
013400*                                                                         
013500*                                                                         
013600 01  FILLER          PIC X(24) Value 'DATUMBERÄKNINGSAREOR'.              
013700                                                                          
013800 01  DAGENS-DATUM                PIC 9(8)    VALUE ZERO.                  
013900                                                                          
014000 01  DAGENS-DADATUM              PIC 9(8)    VALUE ZERO.                  
014100 01  FILLER REDEFINES DAGENS-DADATUM.                                     
014200     03  DAGENS-DADATUM-SEKEL    PIC 9(2).                                
014300     03  DAGENS-TIDATUM          PIC 9(6).                                
014400                                                                          
014500 01 DAGENS-AAAAVVD               PIC 9(7)    VALUE ZERO.                  
014600 01 FILLER REDEFINES DAGENS-AAAAVVD.                                      
014700     03 DAGENS-SEKEL             PIC 9(2).                                
014800     03 DAGENS-AA                PIC 9(2).                                
014900     03 DAGENS-VVD               PIC X(3).                                
015000 01 FILLER REDEFINES DAGENS-AAAAVVD.                                      
015100     03 DAGENS-DAAVROP           PIC 9(6).                                
015200     03 DAGENS-TILEVDAG          PIC 9(1).                                
015300 01 FILLER REDEFINES DAGENS-AAAAVVD.                                      
015400     03 FILLER                   PIC 9(2).                                
015500     03 DAGENS-TIAAVVD           PIC 9(5).                                
015600                                                                          
015700 01  WS-IDPROMR                  PIC X(3)    VALUE SPACE.                 
015800 01  FILLER REDEFINES WS-IDPROMR.                                         
015900     03  WS-IDMARKBO             PIC X(1).                                
016000     03  WS-IDPROMRN             PIC X(2).                                
016100                                                                          
016200 01  SPAR-TABELL.                                                         
016300     03  SPAR-IDPROMR     OCCURS 3.                                       
016400         05  SPAR-IDMARKBO           PIC X(1).                            
016500         05  SPAR-IDPROMRN           PIC X(2).                            
016900*                                                                         
017100     EJECT                                                                
017200*      --- VALID IDDC CODES                                               
017300*01    -COPY WWDC99                                                       
017400     EJECT                                                                
017500 01  DYNAMISKA-SUBPROGRAM.                                                
017600*                                                                         
017700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
017800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
017810     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
017900     SKIP2                                                                
018000*    --- PARAMETRAR TILL ABEND                                            
018100                                                                          
018200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
018300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
018400     SKIP2                                                                
018500 01  FELTEXT.                                                             
018600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
018700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
018710 01  ABEND-RAD2.                                                          
018720     03 FILLER              PIC X(120)  VALUE ' '.                        
018800     EJECT                                                                
018900*    --- PARAMETRAR TILL POSTSUM                                          
019000*                                                                         
019100*01  -COPY W0005   -PRE  POSTSUM-                                         
019200     EJECT                                                                
019300     SKIP2                                                                
019310 01  FILLER                   PIC X(16)   VALUE 'SQLCA-AREA'.             
019320       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
019330                                                                          
019340 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
019350 01  DB2-WS.                                                              
019360     03  SQLCODE-WS              PIC S9(3)    VALUE ZERO.                 
019370         88  CURSOR-OK                        VALUE 000.                  
019380         88  ROWS-FOUND                       VALUE 000.                  
019390         88  ROWS-MISSING                     VALUE +100.                 
019391     03  GOOD-SQLCODECODES.                                               
019392         05  GOOD-SQLCODE OCCURS 5                                        
019393             INDEXED BY SQLCODE-IX PIC S9(3).                             
019394*01  FILLER -COPY TP7PART -PRE TP7PART-                                   
019395     EJECT                                                                
019396 01  FILLER                      PIC X(16)   VALUE 'TP7PART-AREA'.        
019397       EXEC SQL INCLUDE TP7PART END-EXEC.                                 
019398     EJECT                                                                
019399     SKIP2                                                                
019400*          TABELL                                                         
019500*    FÖR EVENTUELLA VARIABLER UT PÅ LISTAN                                
019600*    OM START > 0 SKALL DENNA MED, OCH ANGER KOLUMN-POSITION.             
019700*    LÄNGD ANGER VARIABELNS LÄNGD PÅ LISTAN (BA-INIT).                    
019800*    RUB1 = VARIABELNS RUBRIKRAD 1                                        
019900*    RUB2 = VARIABELNS RUBRIKRAD 2                                        
020000*                                                                         
020100                                                                          
020200 01  TABELL.                                                              
020300     03  TABELL-RAD  OCCURS 107.                                          
020400*        --- DENNA TABELL INNEHÅLLER ALLA KOLUMNER SOM SKA SKAPAS         
020800*        --- TAB-ELEMENT INDEXERAS MED TAB-IX                             
020900         05  TAB-START           PIC S9(3) COMP-3  VALUE ZERO.            
021000         05  TAB-LAENGD          PIC S9(3) COMP-3  VALUE ZERO.            
021100         05  TAB-RUB1            PIC X(25) VALUE SPACE.                   
021200         05  TAB-RUB2            PIC X(25) VALUE SPACE.                   
021300     SKIP2                                                                
021400 01  WS-AREA-PARM1.                                                       
021500     03  WS-IDUSER              PIC X(8).                                 
021600     03  WS-FLAGGA              PIC X.                                    
021800     03  WS-KDSORT1             PIC 9.                                    
022000     03  FILLER                 PIC X(60).                                
022100*                          SUMMA = 80 TECKEN                              
022200                                                                          
022300     EJECT                                                                
022400 01 WS10-AREA.                                                            
022500     03 WS10-IDPROMRGRP     OCCURS 3 TIMES.                               
022600        05  WS10-IDMARKBO      PIC X.                                     
022700        05  WS10-IDPROMRN      PIC X(2).                                  
022800* 9                                                                       
022900     03 WS10-IDFKNGRP       OCCURS 4 TIMES.                               
023000        05  WS10-IDFKNGRP-FOM  PIC 9(4).                                  
023100        05  WS10-IDFKNGRP-TOM  PIC 9(4).                                  
023200*41                                                                       
023300     03 WS10-IDDISTRGRP     OCCURS 4 TIMES.                               
023400        05  WS10-IDDISTR-FOM   PIC 9(4).                                  
023500        05  WS10-IDDISTR-TOM   PIC 9(4).                                  
023600*73                                                                       
023700     03 FILLER                 PIC X(7).                                  
023800*80                                                                       
023900*                          SUMMA = 80 TECKEN                              
024000                                                                          
024100**********************************************************                
024200*    O B S   ÄNDRAS DET HÄR SKALL DET EV. ÄNDRAS I                        
024300*            BA-INITIERA-RUBRIK   OCKSÅ.                                  
024400**********************************************************                
024500                                                                          
024600 01  W-ARBETS-AREOR.                                                      
024700*      ---- Fälten visas i denna ordning på listan                        
024800*    03  WS-IDMARKBO             PIC X(3).                                
024900     03  WS-IDARTNR              PIC Z(9).                                
025000     03  W-IDFKNGRP              PIC Z(4).                                
025100     03  W-IDDISTR               PIC Z(4).                                
025200     03  W-KDPRODSL              PIC Z(2).                                
025300     03  W-KDERS                 PIC 9(2).                                
025400     03  W-MO-NOR-PRIS           PIC -(6)9v,99.                           
025500     03  W-DO-NOR-PRIS           PIC -(6)9v,99.                           
025600     03  W-PRARTBTO-MARK         PIC -(6)9V,99.                           
025700     03  W-DESCRIPTION-GB        PIC X(25).                               
025800     EJECT                                                                
025900                                                                          
026000 01  IN10-AREA-START    PIC X(24)   VALUE 'IN10-AREA-START  '.            
026100*    PARAMETRAR IN , FRÅN BILD 3331 VIA SOP                               
026200*    --- HÄR LIGGER SYSIN-DATA-RAD-3 NÄR PGM-SLINGAR KÖRS.                
026300*01  AREA -COPY W33522     -PRE IN10-                                     
026400                                                                          
026500     EJECT                                                                
026600                                                                          
026700 01  IN42-AREA-START    PIC X(24)   Value 'IN42-AREA-START  '.            
026800*    DAGLAGERFILEN, HÄRIFRÅN HÄMTAS ARTIKELINFO TILL LISTAN               
026900                                                                          
027000*01  AREA -COPY W91042     -PRE IN42-                                     
027100                                                                          
027200     EJECT                                                                
027300                                                                          
027400 01  W001-AREA-START    PIC X(24)   Value 'W001-AREA-START  '.            
027500 01  W001-HJALPAREOR.                                                     
027600*                                                                         
027700     03  W001-SKIP               PIC 9(3) COMP-3  VALUE 1.                
027800     03  ANTAL-RADER             PIC 9(3)    VALUE 99.                    
027900     03  W001-MAX-RADER-PER-SIDA PIC 9(3)    VALUE 42.                    
028000     03  W001-MAX-POSITIONER-PER-RAD                                      
028100                                 PIC 9(3)    VALUE 120.                   
028200     03  W001-LISTNR             PIC X(11)   VALUE 'W33522-001'.          
028300     03  W001-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
028400     SKIP2                                                                
028500                                                                          
028600                                                                          
028700                                                                          
028800 01  W001-URVAL-RAD1.                                                     
028900     03  FILLER           PIC X(3)  VALUE SPACE.                          
029000     03  FILLER           PIC X(15) VALUE 'PRICEAREA LIST:'.              
029100     03  FILLER           PIC X(2)  VALUE SPACE.                          
029200     03  FILLER OCCURS 3.                                                 
029300         05  W001-IDPROMR     PIC X(3)  VALUE SPACE.                      
029400         05  W001-IDPROMR-SEP PIC X(2)  VALUE ', '.                       
029500     03  FILLER          PIC X(20)  VALUE SPACE.                          
029600     03  FILLER          PIC X(10)  VALUE 'SORT.ALT  '.                   
029700     03  W001-SORTERING  PIC X(27)  VALUE SPACE.                          
029800     03  FILLER          PIC X(30)  VALUE SPACE.                          
029900*                        SUMMA 120 TKN                                    
030000                                                                          
030100                                                                          
030200 01  W001-URVAL-RAD2.                                                     
030300     03  FILLER           PIC X(3) VALUE SPACE.                           
030400     03  FILLER           PIC X(18) VALUE 'FUNC.GRP INTERVAL:'.           
030500     03  FILLER           PIC X(2)  VALUE SPACE.                          
030600     03  FILLER  OCCURS 4.                                                
030700         05  W001-IDFKNGRP-FOM     PIC X(5)  VALUE '    -'.               
030800         05  W001-IDFKNGRP-TOM     PIC X(4)  VALUE '    '.                
030900         05  W001-IDFKNGRP-SEP     PIC X(2)  VALUE ', '.                  
031000     03  FILLER           PIC X(53) VALUE SPACE.                          
031100*                        SUMMA 120 TKN                                    
031200                                                                          
031300 01  W001-URVAL-RAD3.                                                     
031400     03  FILLER             PIC X(3)  VALUE SPACE.                        
031500     03  FILLER             PIC X(18) VALUE 'DISTRICT INTERVAL:'.         
031600     03  FILLER             PIC X(2)  VALUE SPACE.                        
031700     03  FILLER  OCCURS 4.                                                
031800         05  W001-IDDISTR-FOM      PIC X(5)  VALUE '    -'.               
031900         05  W001-IDDISTR-TOM      PIC X(4)  VALUE '    '.                
032000         05  W001-IDDISTR-SEP      PIC X(2)  VALUE ', '.                  
032100     03  FILLER             PIC X(53) VALUE SPACE.                        
032200*                        SUMMA 120 TKN                                    
032300                                                                          
032400 01  W001-URVAL-RAD4.                                                     
032500     03  FILLER             PIC X(3)  VALUE SPACE.                        
032600     03  FILLER             PIC X(18) VALUE 'PGRP.GRP.INTERVAL:'.         
032700     03  FILLER             PIC X(2)  Value SPACE.                        
032800     03  FILLER  OCCURS 4.                                                
032900         05  W001-KDPRODSL-FOM      PIC X(3)  VALUE '  -'.                
033000         05  W001-KDPRODSL-TOM      PIC X(2)  VALUE '  '.                 
033100         05  W001-KDPRODSL-SEP      PIC X(2)  VALUE ', '.                 
033200     03  FILLER             PIC X(49) Value Space.                        
033300*                        SUMMA 120 TKN                                    
033400                                                                          
033500                                                                          
033600 01  W001-URVAL-RAD5.                                                     
033700     03  FILLER             PIC X(3)  VALUE SPACE.                        
033800     03  FILLER             PIC X(18) VALUE 'SUP.CODE INTERVAL:'.         
033900     03  FILLER             PIC X(2)  VALUE SPACE.                        
034000     03  FILLER  OCCURS 4.                                                
034100         05  W001-KDERS-FOM         PIC X(3)  VALUE '  -'.                
034200         05  W001-KDERS-TOM         PIC X(2)  VALUE '  '.                 
034300         05  W001-KDERS-SEP         PIC X(2)  VALUE ', '.                 
034400     03  FILLER             PIC X(49) Value Space.                        
034500*                        SUMMA 120 TKN                                    
034600                                                                          
034700     EJECT                                                                
034800 01  W001-RUBRIK1.                                                        
034900*                   HUVUDRUBRIKEN                                         
035000     03  FILLER      PIC X(3)  VALUE SPACE.                               
035100     03  FILLER      PIC X(18) VALUE 'VCCS              '.                
035200     03  FILLER      PIC X(12) VALUE 'W33522-001'.                        
035300     03  FILLER      PIC X(30) VALUE 'ART.STAT URVAL PRISOMRÅDE'.         
035400     03  FILLER      PIC X(06) VALUE 'USER: '.                            
035500     03  W001-IDUSER.                                                     
035600         05  W001-R0 PIC X(2)  VALUE SPACE.                               
035700         05  FILLER  PIC X(6)  VALUE SPACE.                               
035800     03  FILLER      PIC X(09) VALUE SPACE.                               
035900     03  W001-DATUM  PIC XXBXXBXX.                                        
036000     03  FILLER      PIC X(05) VALUE SPACE.                               
036100     03  FILLER      PIC X(4)  VALUE 'SID'.                               
036200     03  W001-SID    PIC Z(4)9.                                           
036300     SKIP2                                                                
036400                                                                          
036410 01  W001-RUBRIK2.                                                        
036420*                   UNDERRUBRIKEN                                         
036430     03  FILLER      PIC X(1)  VALUE SPACE.                               
036431     03  FILLER      PIC X(9)  VALUE 'PRICEAREA'.                         
036432     03  FILLER      PIC X(1)  VALUE  SPACE.                              
036440     03  FILLER      PIC X(9)  VALUE 'PART NO'.                           
036441     03  FILLER      PIC X(1)  VALUE  SPACE.                              
036442     03  FILLER      PIC X(14) VALUE 'DESCRIPTION GB'.                    
036443     03  FILLER      PIC X(1)  VALUE  SPACE.                              
036450     03  FILLER      PIC X(4)  VALUE 'FGRP'.                              
036451     03  FILLER      PIC X(1)  VALUE  SPACE.                              
036460     03  FILLER      PIC X(8)  VALUE 'DISTRICT'.                          
036461     03  FILLER      PIC X(1)  VALUE  SPACE.                              
036470     03  FILLER      PIC X(4)  VALUE 'PGRP'.                              
036471     03  FILLER      PIC X(1)  VALUE  SPACE.                              
036472     03  FILLER      PIC X(12) VALUE 'SUPERSESSION'.                      
036473     03  FILLER      PIC X(1)  VALUE  SPACE.                              
036474     03  FILLER      PIC X(10) VALUE 'SO-PRICE  '.                        
036475     03  FILLER      PIC X(1)  VALUE  SPACE.                              
036476     03  FILLER      PIC X(10) VALUE 'DO-PRICE  '.                        
036477     03  FILLER      PIC X(1)  VALUE  SPACE.                              
036478     03  FILLER      PIC X(11) VALUE 'SUGG-RETAIL'.                       
036497     SKIP2                                                                
036498                                                                          
036500 01  W001-DETALJ.                                                         
036600     03  FILLER       PIC X(1)   VALUE SPACE.                             
036700     03  W001-ART-RAD PIC X(120) VALUE SPACE.                             
036800     EJECT                                                                
036900                                                                          
037000 01  W002-DETALJ.                                                         
037100     03  FILLER       PIC X(1)   VALUE SPACE.                             
037200     03  W002-ART-RAD PIC X(134) VALUE SPACE.                             
037210*    03  W002-ART-RAD PIC X(1183) VALUE SPACE.                            
037300     EJECT                                                                
037400*                                                                         
037500     SKIP2                                                                
037600 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
037700                                  'SORTWS-AREA-START  '.                  
037800*01  AREA -COPY W33522S     -PRE SORTWS-                                  
037900 01  SORT-RETURN-X               PIC X(4)  VALUE SPACE.                   
038000     EJECT                                                                
038100                                                                          
038200 01  GENERELLA-SUBPROGRAM.                                                
038300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
038400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
038500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
038900*                                                                         
039000*                                                                         
039100*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV "KONVERTERA DATUM"           
039200 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
039300*01 -COPY WDATAREA                                                        
039400     EJECT                                                                
039500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
039600     SKIP3                                                                
039700 01  FILLER                      PIC X(16)   VALUE 'CHKP'.                
039800 01  CHKP-VAR.                                                            
039900     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
040000     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
040100     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
040200     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
040300     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
040400     03 CHKP-MAX                 PIC S9(3)   VALUE +100 COMP-3.           
040500                                                                          
040600                                                                          
040700 01  NYCKLAR-TILL-DLI.                                                    
040800                                                                          
040900     03  W-IDARTNR-X.                                                     
041000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
041100                                                                          
041200     03    W-IDPROMR-X.                                                   
041300         05    W-IDPROMR         PIC X(3)    VALUE SPACE.                 
041400                                                                          
041500     03  W-DASTADAT-X.                                                    
041600         05  W-DASTADAT          PIC 9(8)    VALUE ZERO.                  
041700                                                                          
041800     03  W-WDC101KY-X.                                                    
041900         05  W-IDARTNR-111       PIC S9(9)   VALUE ZERO COMP-3.           
042000         05  W-IDMARKBO-111      PIC X       VALUE SPACE.                 
042100                                                                          
043500     03  W-IDDISTR-X.                                                     
043600         05  W-IDDISTR           PIC S9(5)       COMP-3.                  
043700                                                                          
043800*   NYCKLAR TILL BETALARREGISTRET    ***********                          
043810     03  W-WDB1ASEQ-X.                                                    
043820         05  W-IDPROMR-SEQ       PIC X(3)    VALUE SPACE.                 
043840                                                                          
044200*    --- IMS FUNKTIONSKODER                                               
044300*01  -COPY W0003                                                          
044400     EJECT                                                                
044500*    ---  DLI INPUT-OUTPUT AREA                                           
045400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC101'.                      
045500 01  DLI-IO-WDC101.                                                       
045600*    03  -COPY WDC101      -PRE WDC1-                                     
045700     EJECT                                                                
045800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC201'.                      
045900 01  DLI-IO-WDC201.                                                       
046000*    03  -COPY WDC201      -PRE WDC2                                      
046100     EJECT                                                                
046200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC213'.                      
046300 01  DLI-IO-WDC213.                                                       
046400*    03  -COPY WDC213      -PRE WDC2-                                     
046500     EJECT                                                                
046510 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
046530 01  DLI-IO-WDB101.                                                       
046550*    03  -COPY WDB101      -PRE WDB1A-                                    
046560     EJECT                                                                
047400*    --- STATUS-KOD FRÅN IMS                                              
047500 01  STATUS-WS                   PIC XX.                                  
047600     88  SEGMENT-FINNS                       VALUE '  '.                  
047700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
047800     88  IMS-EJ-OK                           VALUE 'XD'.                  
047900     88  SEGMENT-SAKNAS            VALUES ARE 'GE' 'GB'.                  
048000     SKIP2                                                                
048100 01  GODK-STATUSKODER.                                                    
048200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
048300     SKIP3                                                                
048400 01  SSA1                        PIC X(128).                              
048500 01  SSA2                        PIC X(128).                              
048600 01  SSA3                        PIC X(128).                              
048700     EJECT                                                                
048800                                                                          
048900 LINKAGE SECTION.                                                         
049000                                                                          
049100*01    -COPY W0009     -PRE MSG-                                          
049200     EJECT                                                                
049600*01    -COPY W0008     -PRE WDC1-                                         
049700     05  FILLER                  PIC X.                                   
049800                                                                          
049900*01    -COPY W0008     -PRE WDC2-                                         
050000     05  FILLER                  PIC X.                                   
050100                                                                          
050200*01    -COPY W0008     -PRE WDB1A-                                        
050300     05  FILLER                  PIC X.                                   
050700     EJECT                                                                
050800                                                                          
050900 PROCEDURE DIVISION  USING MSG-PCB WDC1-PCB WDC2-PCB WDB1A-PCB.           
051100 MAIN SECTION.                                                            
051200     ENTRY 'DLITCBL' USING MSG-PCB WDC1-PCB WDC2-PCB WDB1A-PCB.           
051500     PERFORM A-INIT                                                       
051600                                                                          
051700     IF IN10-FLEXCEL = JA                                                 
051800       CONTINUE                                                           
051900     ELSE                                                                 
052000       PERFORM B-SKAPA-TABELL                                             
052100     END-IF                                                               
052200                                                                          
052300     SORT SORTFIL ASCENDING KEY SORT-SORT-1                               
052400                                SORT-SORT-2                               
052500                                SORT-SORT-3                               
052600                                SORT-SORT-4                               
052800                  INPUT PROCEDURE C-SORT-INPUT                            
052900                  OUTPUT PROCEDURE D-SORT-OUTPUT                          
053000                                                                          
053100     IF SORT-RETURN NOT = 0 AND AVBRYTES-EJ                               
053200       MOVE SORT-RETURN TO SORT-RETURN-X                                  
053300       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
053400       DELIMITED BY SIZE INTO FELTEXT-STR                                 
053500       DISPLAY FELTEXT                                                    
053600       PERFORM S99-ABEND                                                  
053700     ELSE                                                                 
053800       PERFORM Z-FINIT                                                    
053900                                                                          
054000       MOVE ZERO TO RETURN-CODE                                           
054100       GOBACK                                                             
054200     END-IF                                                               
054300     .                                                                    
054400     EJECT                                                                
054500 A-INIT SECTION.                                                          
054600                                                                          
054700     OPEN INPUT  W33522                                                   
054800                 W91042                                                   
054900     OPEN OUTPUT W33522-001                                               
055000     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
055100                                                                          
055200     MOVE DAGENS-DATUM TO W-DASTADAT                                      
055300                                                                          
055400     MOVE "IDAG  " TO DAT-KDDATFORM                                       
055500     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
055600                         DAT-O-TIDATUM DAT-KDSVAR                         
055700     IF DAT-KDSVAR-OK                                                     
055800        MOVE DAT-TIAAMMDD         TO DAGENS-TIDATUM                       
055900        MOVE DAT-TISEKEL          TO DAGENS-SEKEL                         
056000                                     DAGENS-DADATUM-SEKEL                 
056100        MOVE DAT-TIAA             TO DAGENS-AA                            
056200        MOVE DAT-TIAAVVD-GRP(3:3) TO DAGENS-VVD                           
056400     ELSE                                                                 
056500        MOVE 'FEL FRÅN DATKONV - DAGENS DATUM' TO FELTEXT                 
056600        CALL FELLOG                                                       
056700     END-IF                                                               
056800     MOVE DAGENS-TIDATUM To W001-DATUM                                    
056900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
057000                                                                          
057900**********************************************                            
058000*    HÄR LÄSES PARAMETRARNA FRÅN BILD 3331 IN                             
058100**********************************************                            
058200                                                                          
058300*    --- LÄS INDATA RAD 1 FRÅN 3331                                       
058400     PERFORM S01-LAES-W33522                                              
058500     IF NOT END-OF-W33522                                                 
058600                                                                          
058700       MOVE IN10-RAD1 TO WS-AREA-PARM1                                    
058800                                                                          
058900     ELSE                                                                 
059000       MOVE 'PARAMETRAR FRÅN SOP SAKNAS '                                 
059100             TO FELTEXT-STR                                               
059200       DISPLAY FELTEXT                                                    
059300       PERFORM S99-ABEND                                                  
059400     END-IF                                                               
059500                                                                          
059600*    --- LÄS INDATA RAD 2 FRÅN 3331  = URVAL1                             
059700     PERFORM S01-LAES-W33522                                              
059800     IF NOT END-OF-W33522                                                 
059900                                                                          
060000       MOVE IN10-RAD2 TO WS10-AREA                                        
060100                                                                          
060200     ELSE                                                                 
060300       MOVE 'PARAMETERRAD 2 OCH 3 FRÅN SOP SAKNAS '                       
060400             TO FELTEXT-STR                                               
060500       DISPLAY FELTEXT                                                    
060600       PERFORM S99-ABEND                                                  
060700     END-IF                                                               
060800                                                                          
060900     MOVE WS-IDUSER TO W001-IDUSER                                        
061000     DISPLAY '*********** PARAMETRAR:   '                                 
061100     DISPLAY WS-AREA-PARM1                                                
061200     DISPLAY IN10-RAD2                                                    
061300                                                                          
061400*    --- LÄS INDATA RAD 3 FRÅN 3331                                       
061500     PERFORM S01-LAES-W33522                                              
061600     IF NOT END-OF-W33522                                                 
061700*                                                                         
061800*      RAD 3 LIGGER NU KVAR I IN10-AREAN                                  
061900*                                                                         
062000       CONTINUE                                                           
062100     ELSE                                                                 
062200       MOVE 'PARAMETERRAD 3 FRÅN SOP SAKNAS '                             
062300             TO FELTEXT-STR                                               
062400       DISPLAY FELTEXT                                                    
062500       PERFORM S99-ABEND                                                  
062600                                                                          
062700     END-IF                                                               
062800                                                                          
062900     DISPLAY IN10-RAD3                                                    
063000     DISPLAY '*********** '                                               
063600                                                                          
063700     PERFORM IMS-RESTART                                                  
063800     .                                                                    
063900     EJECT                                                                
064000                                                                          
064100 B-SKAPA-TABELL   SECTION.                                                
064200*06:6 klar                                                                
064300**************************************************                        
064400*    INITIERA LÄNGDEN PÅ  VARIABLERNA + RUBRIKER                          
064500*      ( EJ VID EXCEL ) EXCEL-LISTA ANVÄNDER EJ DENNA SEKTION.            
064600**************************************************                        
064700     PERFORM BA-INITIERA-RUBRIK                                           
064900     CONTINUE                                                             
065000                                                                          
065100     EJECT                                                                
065200     .                                                                    
065300 BA-INITIERA-RUBRIK SECTION.                                              
065400*06:6 KLAR                                                                
065500     SKIP2                                                                
065600* IDPROMR                                                                 
065700* LIST 1                                           (TAB-IX)               
065800     MOVE 9                           TO TAB-LAENGD (1)                   
065900     MOVE SPACE                       TO TAB-RUB1   (1)                   
066000     MOVE 'PRICEAREA                ' TO TAB-RUB2   (1)                   
066100* ART                                                                     
066200* LIST 2                                           (TAB-IX)               
066300     MOVE 9                           TO TAB-LAENGD (2)                   
066400     MOVE SPACE                       TO TAB-RUB1   (2)                   
066500     MOVE '  PART NO                ' TO TAB-RUB2   (2)                   
066510* ENG                                                                     
066520* LIST 3                                           (TAB-IX)               
066530     MOVE 25                          TO TAB-LAENGD (5)                   
066540     MOVE SPACE                       TO TAB-RUB1   (5)                   
066550     MOVE 'DESCRIPTION.GB. _________' TO TAB-RUB2   (5)                   
066600* IDFKNGRP                                                                
066700* LIST 4                                           (TAB-IX)               
066800     MOVE 4                           TO TAB-LAENGD (3)                   
066900     MOVE SPACE                       TO TAB-RUB1   (3)                   
067000     MOVE 'FGRP                     ' TO TAB-RUB2   (3)                   
067100* LIST 5                                           (TAB-IX)               
067200     MOVE 4                           TO TAB-LAENGD (4)                   
067300     MOVE SPACE                       TO TAB-RUB1   (4)                   
067400     MOVE 'IDDISTR                  ' TO TAB-RUB2   (4)                   
067500* LIST 6                                           (TAB-IX)               
067600     MOVE 2                           TO TAB-LAENGD (4)                   
067700     MOVE SPACE                       TO TAB-RUB1   (4)                   
067800     MOVE 'PGRP                     ' TO TAB-RUB2   (4)                   
067900* LIST 7                                           (TAB-IX)               
068000     MOVE 2                           TO TAB-LAENGD (4)                   
068100     MOVE SPACE                       TO TAB-RUB1   (4)                   
068200     MOVE 'SUPERSESSION             ' TO TAB-RUB2   (4)                   
068300* LIST 8                                           (TAB-IX)               
068400     MOVE 10                          TO TAB-LAENGD (4)                   
068500     MOVE SPACE                       TO TAB-RUB1   (4)                   
068600     MOVE 'SO-PRICE                 ' TO TAB-RUB2   (4)                   
068700* LIST 9                                           (TAB-IX)               
068800     MOVE 10                          TO TAB-LAENGD (4)                   
068900     MOVE SPACE                       TO TAB-RUB1   (4)                   
069000     MOVE 'DO-PRICE                 ' TO TAB-RUB2   (4)                   
069100* LIST 10                                          (TAB-IX)               
069200     MOVE 10                          TO TAB-LAENGD (4)                   
069300     MOVE SPACE                       TO TAB-RUB1   (4)                   
069400     MOVE 'SUGG.RETAIL              ' TO TAB-RUB2   (4)                   
070000     .                                                                    
070100     EJECT                                                                
070200 C-SORT-INPUT  SECTION.                                                   
070300     SKIP2                                                                
070400*06:6 klar                                                                
070500****************************************************************          
070600*    HÄR LÄSES DAGLAGERBANDET IN                                          
070700*        OM ART UPPFYLLER URVALET SÅ SKAPAS SORTFIL                       
070800****************************************************************          
070900     PERFORM S02-LAES-W91042                                              
071000     PERFORM UNTIL END-OF-W91042                                          
071100                                                                          
071200       PERFORM CA-KOLLA-URVAL                                             
071300       IF URVAL-OK                                                        
071400         MOVE IN42-IDARTNR           TO W-IDARTNR-111                     
071500         MOVE +1 TO INDX                                                  
071600         PERFORM UNTIL INDX > 3                                           
071700           IF SPAR-IDMARKBO(INDX) > SPACE                                 
071800              AND SPAR-IDPROMRN(INDX) > SPACE                             
071900             MOVE SPAR-IDMARKBO(INDX)    TO W-IDMARKBO-111                
071910                                          WS-IDMARKBO                     
071920                                          SORTWS-IDMARKBO                 
072000             PERFORM IMS-GU-WDC101                                        
072100             IF SEGMENT-FINNS AND WDC1-ART-KDARTRAB NOT = ZERO            
072200               MOVE WDC1-ART-KDARTRAB TO SPAR-KDARTRAB                    
072300               MOVE WDC1-ART-PRARTBTO-MARK                                
072400                                        TO SORTWS-PRARTBTO-MARK           
072500                                           WS-RETAILPRIS                  
072600               MOVE SPAR-IDPROMRN(INDX)    TO WS-IDPROMRN                 
072601                                              SORTWS-IDPROMRN             
072610               MOVE WS-IDPROMR             TO W-IDPROMR                   
072700               PERFORM IMS-GU-WDC201                                      
072800               IF SEGMENT-FINNS                                           
072900                 PERFORM IMS-GNP-WDC213                                   
073000                 IF SEGMENT-FINNS                                         
073100                   IF WS-RETAILPRIS NOT = ZERO                            
073200                     MOVE SPAR-KDARTRAB      TO RAB-INDX                  
073300                     COMPUTE WS-RETAILPRIS-NOR-DO ROUNDED =               
073400                           ((100 - WDC2-RAB-REARTRAB-DO(RAB-INDX))        
073500                           * WS-RETAILPRIS ) / 100                        
073600                     COMPUTE WS-RETAILPRIS-NOR-MO ROUNDED =               
073700                         ((100 - WDC2-RAB-REARTRAB-BULK(RAB-INDX))        
073800                         * WS-RETAILPRIS ) / 100                          
073900                     MOVE WS-RETAILPRIS-NOR-DO                            
074000                                           TO SORTWS-DO-NOR-PRIS          
074100                     MOVE WS-RETAILPRIS-NOR-MO                            
074200                                           TO SORTWS-MO-NOR-PRIS          
074300                   END-IF                                                 
074400                 ELSE                                                     
074500                   MOVE ZERO            TO SORTWS-DO-NOR-PRIS             
074600                   MOVE ZERO            TO SORTWS-MO-NOR-PRIS             
074700                 END-IF                                                   
074800               ELSE                                                       
074900                 MOVE ZERO            TO SORTWS-DO-NOR-PRIS               
075000                 MOVE ZERO            TO SORTWS-MO-NOR-PRIS               
075100               END-IF                                                     
075110               PERFORM CD-KOLLA-DISTR                                     
075130             ELSE                                                         
075140               CONTINUE                                                   
075400             END-IF                                                       
075600           END-IF                                                         
075700           ADD +1 TO INDX                                                 
075800         END-PERFORM                                                      
075900       END-IF                                                             
076000       PERFORM S02-LAES-W91042                                            
076100     END-PERFORM                                                          
076200     .                                                                    
076300     EJECT                                                                
076400 CA-KOLLA-URVAL SECTION.                                                  
076500     SKIP2                                                                
076600     MOVE JA  TO URVAL-SW                                                 
076700                                                                          
076800* KOLLA IDPROMR-URVAL                                                     
076900* DETTA MÅSTE ALLTID VARA IFYLLT SKA DEN KOLLEN VARA HÄR?                 
077000     IF URVAL-OK                                                          
077100       IF WS10-IDPROMRN(1) > SPACE OR WS10-IDPROMRN(2) > SPACE            
077200          OR WS10-IDPROMRN(3) > SPACE                                     
077300         MOVE +1 TO IX                                                    
077400         PERFORM UNTIL IX > 3                                             
077500           IF  WS10-IDMARKBO(IX) > SPACE                                  
077600             MOVE WS10-IDMARKBO(IX)   TO SPAR-IDMARKBO(IX)                
077700             MOVE WS10-IDPROMRN(IX)   TO SPAR-IDPROMRN(IX)                
077800           END-IF                                                         
077900           ADD +1 TO IX                                                   
078000         END-PERFORM                                                      
078100       ELSE                                                               
078200         MOVE NEJ TO URVAL-SW                                             
078300****  MÅSTE VARA IFYLLT   FELMEDDELANDE??                                 
078400       END-IF                                                             
078500     END-IF                                                               
078600                                                                          
078700* KOLLA FUNKTIONSGRUPPSURVAL                                              
078800     IF URVAL-OK                                                          
078900       IF WS10-IDFKNGRP-FOM(1) > ZERO OR                                  
079000          WS10-IDFKNGRP-FOM(2) > ZERO OR                                  
079100          WS10-IDFKNGRP-FOM(3) > ZERO OR                                  
079200          WS10-IDFKNGRP-FOM(4) > ZERO                                     
079300          MOVE +1 TO IX                                                   
079310          MOVE NEJ TO TRAEFF-SW                                           
079400          PERFORM UNTIL IX > 4 OR TRAEFF-OK                               
079500            IF (IN42-IDFKNGRP NOT < WS10-IDFKNGRP-FOM (IX) AND            
079600               IN42-IDFKNGRP NOT > WS10-IDFKNGRP-TOM (IX))                
079610               MOVE JA TO TRAEFF-SW                                       
079620            END-IF                                                        
080010            ADD +1 TO IX                                                  
080100          END-PERFORM                                                     
080110          IF TRAEFF-NEJ                                                   
080120            MOVE NEJ TO URVAL-SW                                          
080130          END-IF                                                          
080200       END-IF                                                             
080300     END-IF                                                               
080400                                                                          
080500     IF URVAL-OK                                                          
080600       IF WS10-IDDISTR-FOM(1) > ZERO OR                                   
080700          WS10-IDDISTR-FOM(2) > ZERO OR                                   
080800          WS10-IDDISTR-FOM(3) > ZERO OR                                   
080900          WS10-IDDISTR-FOM(4) > ZERO                                      
081000         MOVE +1 TO IX                                                    
081010         MOVE NEJ TO TRAEFF-SW                                            
081100         PERFORM UNTIL  IX > 4 OR TRAEFF-OK                               
081200           IF WS10-IDDISTR-FOM(IX) > ZERO                                 
081300*            MOVE WS10-IDDISTR-FOM(IX)   TO WS-IDDISTR-FOM(IX)            
081400*            MOVE WS10-IDDISTR-TOM(IX)   TO WS-IDDISTR-TOM(IX)            
081410             MOVE JA TO TRAEFF-SW                                         
081500           END-IF                                                         
081510           ADD +1 TO IX                                                   
081600         END-PERFORM                                                      
081700       ELSE                                                               
081800         CONTINUE                                                         
081900       END-IF                                                             
082000     END-IF                                                               
082100                                                                          
082200* KOLLA PRODUKTSLAGSURVAL                                                 
082300     IF URVAL-OK                                                          
082400       IF IN10-KDPRODSL-FOM(1) > ZERO OR                                  
082500          IN10-KDPRODSL-FOM(2) > ZERO OR                                  
082600          IN10-KDPRODSL-FOM(3) > ZERO OR                                  
082700          IN10-KDPRODSL-FOM(4) > ZERO                                     
082800          MOVE +1 TO IX                                                   
082810          MOVE NEJ TO TRAEFF-SW                                           
082900          PERFORM UNTIL IX > 4 OR TRAEFF-OK                               
083000            IF (IN42-KDPRODSL NOT < IN10-KDPRODSL-FOM (IX) AND            
083100               IN42-KDPRODSL NOT > IN10-KDPRODSL-TOM (IX))                
083200              MOVE JA TO TRAEFF-SW                                        
083500            END-IF                                                        
083510            ADD +1 TO IX                                                  
083600          END-PERFORM                                                     
083610          IF TRAEFF-NEJ                                                   
083620            MOVE NEJ TO URVAL-SW                                          
083630          END-IF                                                          
083700       END-IF                                                             
083800     END-IF                                                               
083900                                                                          
084000* KOLLA ERSÄTTNINGSKODURVAL                                               
084100     IF URVAL-OK                                                          
084200       IF IN10-KDERS-FOM(1) < 99 OR                                       
084300          IN10-KDERS-FOM(2) < 99 OR                                       
084400          IN10-KDERS-FOM(3) < 99 OR                                       
084500          IN10-KDERS-FOM(4) < 99                                          
084600          MOVE +1 TO IX                                                   
084610          MOVE NEJ TO TRAEFF-SW                                           
084700          PERFORM UNTIL IX > 4 OR TRAEFF-OK                               
084800            IF (IN42-KDERS NOT < IN10-KDERS-FOM (IX) AND                  
084900               IN42-KDERS NOT > IN10-KDERS-TOM (IX))                      
085000              MOVE JA TO TRAEFF-SW                                        
085300            END-IF                                                        
085310            ADD +1 TO IX                                                  
085400          END-PERFORM                                                     
085410          IF TRAEFF-NEJ                                                   
085420            MOVE NEJ TO URVAL-SW                                          
085430          END-IF                                                          
085500       END-IF                                                             
085600     END-IF                                                               
085610                                                                          
085620     IF URVAL-OK                                                          
085640******************************************************************        
085650*                                                                         
085660*    SELEKTERAR BORT FÖLJANDE POSTER:                                     
085670*    -OM MER ÄN 4 VECKOR TILL FÖRSTA INLEVERANS                           
085690*    -OM KDPRODSL = 0                                                     
085691*    -OM IDFKNGRP = 0                                                     
085692*    -OM TIERSDAT ÄLDRE ÄN 4 ÅR                                           
085693*                                                                         
085694******************************************************************        
085695*      SKIP2                                                              
085697       MOVE DAT-TIAA          TO WS-AKT-AA                                
085698       MOVE DAT-TIVV          TO WS-AKT-VV                                
085699       MOVE WS-AKT-AAVV       TO WS-AKT-AAVV-NUM                          
085700       MOVE +4                TO WS-PLUS-VECKOR                           
085701       CALL W009VADD USING WS-AKT-AAVV-NUM WS-PLUS-VECKOR                 
085702       MOVE IN42-TIFINLV      TO WS-TIFINLV                               
085703       MOVE WS-TIFINLV-AAVV   TO TMP1-YYWW                                
085704       MOVE WS-AKT-AAVV-NUM   TO TMP2-YYWW                                
085705       PERFORM WY2000P3                                                   
085706       IF TMP1-YYWW > TMP2-YYWW                                           
085707         MOVE NEJ             TO URVAL-SW                                 
085708       ELSE                                                               
085709         MOVE JA              TO URVAL-SW                                 
085710       END-IF                                                             
085711*                                                                         
085712       IF IN42-TIERSDAT = 0                                               
085713         MOVE 0 TO AAR-SEDAN-TIERSDAT                                     
085714       ELSE                                                               
085715         MOVE IN42-TIERSDAT   TO TMP1-YYWWD                               
085717         MOVE DAT-TIAA        TO  AA-WEEK                                 
085718         MOVE DAT-TIVV        TO  VV-WEEK                                 
085719         MOVE AAVVD-WEEK      TO TMP2-YYWWD                               
085720         PERFORM WY2000P2                                                 
085721         COMPUTE AAR-VECKA = TMP2-YYWWD - TMP1-YYWWD                      
085722         MOVE AAR TO AAR-SEDAN-TIERSDAT                                   
085723         IF VECKA < VV-WEEK                                               
085724           ADD +1 TO AAR-SEDAN-TIERSDAT                                   
085725         END-IF                                                           
085726       END-IF                                                             
085727*                                                                         
085728       IF URVAL-OK                                                        
085729         IF IN42-IDFKNGRP = 0                                             
085730           OR IN42-KDPRODSL = 0                                           
085731           OR AAR-SEDAN-TIERSDAT > 8                                      
085732           MOVE NEJ TO URVAL-SW                                           
085733         END-IF                                                           
085734       END-IF                                                             
085735     END-IF                                                               
085736*                                                                         
085800     .                                                                    
085900     EJECT                                                                
086000                                                                          
086100 CB-SKAPA-SORTKEY SECTION.                                                
086200*                                                                         
086300     IF WS-KDSORT1 = 1                                                    
086400* IDPROMRN + IDMARKBO = SORT1                                             
086610        MOVE WS-IDPROMR          TO SORTWS-SORT-1                         
086700        MOVE WS-IDDISTR          TO SORTWS-SORT-2                         
086800        MOVE IN42-IDFKNGRP       TO SORTWS-SORT-3                         
086900        MOVE IN42-IDARTNR        TO SORTWS-SORT-4                         
087000     ELSE                                                                 
087100        IF WS-KDSORT1 = 2                                                 
087200           MOVE SPACE         TO SORTWS-SORT-1                            
087400           MOVE WS-IDDISTR    TO SORTWS-SORT-2                            
087500           MOVE IN42-IDARTNR  TO SORTWS-SORT-3                            
087600           MOVE ZERO          TO SORTWS-SORT-4                            
087700        ELSE                                                              
087800           IF WS-KDSORT1 = 3                                              
087900             MOVE SPACE         TO SORTWS-SORT-1                          
088100             MOVE IN42-IDARTNR  TO SORTWS-SORT-2                          
088200             MOVE ZERO          TO SORTWS-SORT-3                          
088300             MOVE ZERO          TO SORTWS-SORT-4                          
088400           END-IF                                                         
088500        END-IF                                                            
088600     END-IF                                                               
088700     .                                                                    
088800     EJECT                                                                
088900 CC-SKAPA-SORTFIL SECTION.                                                
089000                                                                          
089010     MOVE WS-IDMARKBO        TO SORTWS-IDMARKBO                           
089020     MOVE WS-IDPROMRN        TO SORTWS-IDPROMRN                           
089100     MOVE IN42-IDARTNR       TO SORTWS-IDARTNR                            
089200     MOVE IN42-BEART(4)      TO SORTWS-BEART-ENG                          
089300     MOVE IN42-BEART(8)      TO SORTWS-BEART-SVE                          
089400     MOVE IN42-KDPRODSL      TO SORTWS-KDPRODSL                           
089500     MOVE IN42-IDFKNGRP      TO SORTWS-IDFKNGRP                           
089600     MOVE IN42-KDERS         TO SORTWS-KDERS                              
089610     MOVE WS-IDDISTR         TO SORTWS-IDDISTR                            
089700                                                                          
089800     .                                                                    
089900     EJECT                                                                
090000 CD-KOLLA-DISTR SECTION.                                                  
090001                                                                          
090002     MOVE NEJ             TO FL-DISTR                                     
090003     MOVE WS-IDPROMR      TO W-IDPROMR-SEQ                                
090005                                                                          
090006     PERFORM IMS-GU-WDB101                                                
090007* KOLL OM SEGMENT FINNS IN HÄR                                            
090008     MOVE WDB1A-BET-IDPARTNR TO W-IDPARTNR                                
090009     IF SEGMENT-FINNS                                                     
090010       PERFORM DB2-DCL-OPN-TP7PART-CRS                                    
090020       PERFORM DB2-FETCH-TP7PART-CRS                                      
090030       MOVE ZERO TO TEST-SAMMA-IDDISTR                                    
090040                                                                          
090041       PERFORM UNTIL ROWS-MISSING                                         
090042         IF WS-IDDISTR-DB2 NOT = TEST-SAMMA-IDDISTR                       
090043           MOVE 1 TO IX                                                   
090044           PERFORM UNTIL IX > 4                                           
090045             IF WS-IDDISTR-DB2 NOT < WS10-IDDISTR-FOM(IX) AND             
090046               WS-IDDISTR-DB2 NOT > WS10-IDDISTR-TOM(IX)                  
090047               MOVE WS-IDDISTR-DB2  TO WS-IDDISTR                         
090048                                                                          
090049               PERFORM CB-SKAPA-SORTKEY                                   
090050               PERFORM CC-SKAPA-SORTFIL                                   
090051               PERFORM S31-SORT-RELEASE                                   
090053               MOVE WS-IDDISTR-DB2 TO TEST-SAMMA-IDDISTR                  
090054             END-IF                                                       
090055             ADD 1 TO IX                                                  
090056           END-PERFORM                                                    
090057          END-IF                                                          
090058         PERFORM DB2-FETCH-TP7PART-CRS                                    
090059       END-PERFORM                                                        
090060       PERFORM DB2-CLOSE-TP7PART-CRS                                      
090061     END-IF                                                               
090063                                                                          
090069     .                                                                    
090070     EJECT                                                                
090080 D-SORT-OUTPUT SECTION.                                                   
090100     SKIP2                                                                
090300                                                                          
090310     PERFORM S32-SORT-RETURN                                              
090320                                                                          
090400     IF END-OF-SORTFIL                                                    
090500        MOVE 134 TO LAENGD                                                
090600        MOVE '*******  INGA ARTIKLAR UPPFYLLDE URVALET ***'               
090700                              TO W001-DETALJ W002-DETALJ                  
090800        PERFORM S21-SKRIV-W33522-001                                      
090900     ELSE                                                                 
091000        CONTINUE                                                          
091300     END-If                                                               
091400                                                                          
091500     IF IN10-FLEXCEL = JA                                                 
091600       PERFORM DD-SKAPA-EXCEL-RUBRIK                                      
091700     END-IF                                                               
091800     PERFORM UNTIL END-OF-SORTFIL                                         
091900                                                                          
092000       IF IN10-FLEXCEL = JA                                               
092100         PERFORM DE-SKAPA-EXCELRAD                                        
092200       ELSE                                                               
092400         PERFORM DB-SKAPA-LISTRAD                                         
092500       END-IF                                                             
092600       PERFORM S21-SKRIV-W33522-001                                       
092700                                                                          
093400       PERFORM S32-SORT-RETURN                                            
093410                                                                          
093500     END-PERFORM                                                          
093600                                                                          
094200     .                                                                    
094300     EJECT                                                                
094310 DB-SKAPA-LISTRAD SECTION.                                                
094311                                                                          
094312     MOVE 1 TO LAENGD                                                     
094313                                                                          
094315     MOVE SORTWS-IDMARKBO  TO WS-IDMARKBO                                 
094316     MOVE SORTWS-IDPROMRN  TO WS-IDPROMRN                                 
094317     MOVE WS-IDPROMR       TO W001-ART-RAD (LAENGD:9)                     
094318     ADD 9                 TO LAENGD                                      
094319     MOVE SPACE            TO W001-ART-RAD(LAENGD:1)                      
094320     ADD 1                 TO LAENGD                                      
094321                                                                          
094322     MOVE SORTWS-IDARTNR   TO WS-IDARTNR                                  
094323     MOVE WS-IDARTNR       TO W001-ART-RAD (LAENGD:9)                     
094324     ADD 9                 TO LAENGD                                      
094325     MOVE SPACE            TO W001-ART-RAD(LAENGD:1)                      
094326     ADD 1                 TO LAENGD                                      
094327                                                                          
094328     MOVE SORTWS-BEART-ENG TO W-DESCRIPTION-GB                            
094329     MOVE W-DESCRIPTION-GB TO W001-ART-RAD (LAENGD:25)                    
094330     ADD 25                TO LAENGD                                      
094331     MOVE SPACE            TO W001-ART-RAD(LAENGD:1)                      
094332     ADD 1                 TO LAENGD                                      
094333                                                                          
094334     MOVE SORTWS-IDFKNGRP  TO W-IDFKNGRP                                  
094335     MOVE W-IDFKNGRP       TO W001-ART-RAD (LAENGD:4)                     
094336     ADD 4                 TO LAENGD                                      
094337     MOVE SPACE            TO W001-ART-RAD(LAENGD:1)                      
094338     ADD 1                 TO LAENGD                                      
094339                                                                          
094340     MOVE SORTWS-IDDISTR   TO WS-IDDISTR                                  
094341     MOVE WS-IDDISTR       TO W001-ART-RAD (LAENGD:8)                     
094342     ADD 8                 TO LAENGD                                      
094343     MOVE SPACE            TO W001-ART-RAD(LAENGD:1)                      
094344     ADD 1                 TO LAENGD                                      
094345                                                                          
094346     MOVE SORTWS-KDPRODSL  TO W-KDPRODSL                                  
094347     MOVE W-KDPRODSL       TO W001-ART-RAD (LAENGD:4)                     
094348     ADD 4                 TO LAENGD                                      
094349     MOVE SPACE            TO W001-ART-RAD(LAENGD:1)                      
094350     Add 1                 TO LAENGD                                      
094351                                                                          
094352     MOVE SORTWS-KDERS     TO W-KDERS                                     
094353     MOVE W-KDERS          TO W001-ART-RAD (LAENGD:12)                    
094354     ADD 12                TO LAENGD                                      
094355     MOVE SPACE            TO W001-ART-RAD(LAENGD:1)                      
094356     ADD 1                 TO LAENGD                                      
094357                                                                          
094358     MOVE SORTWS-MO-NOR-PRIS TO W-MO-NOR-PRIS                             
094359     MOVE W-MO-NOR-PRIS    TO W001-ART-RAD (LAENGD:10)                    
094360     ADD 10                TO LAENGD                                      
094361     MOVE SPACE            TO W001-ART-RAD(LAENGD:1)                      
094362     ADD 1                 TO LAENGD                                      
094363                                                                          
094364     MOVE SORTWS-DO-NOR-PRIS TO W-DO-NOR-PRIS                             
094365     MOVE W-DO-NOR-PRIS    TO W001-ART-RAD (LAENGD:10)                    
094366     ADD 10                TO LAENGD                                      
094367     MOVE SPACE            TO W001-ART-RAD(LAENGD:1)                      
094368     ADD 1                 TO LAENGD                                      
094369                                                                          
094370     MOVE SORTWS-PRARTBTO-MARK TO W-PRARTBTO-MARK                         
094371     MOVE W-PRARTBTO-MARK  TO W001-ART-RAD (LAENGD:11)                    
094372     ADD 11                TO LAENGD                                      
094373     MOVE SPACE            TO W001-ART-RAD(LAENGD:1)                      
094374     ADD 1                 TO LAENGD                                      
094375                                                                          
094377     .                                                                    
094378     EJECT                                                                
094379                                                                          
094380                                                                          
094400 DD-SKAPA-EXCEL-RUBRIK SECTION.                                           
094500                                                                          
094600     MOVE 1 TO LAENGD                                                     
094700      MOVE 'PRICEAREA'               TO W002-ART-RAD(LAENGD:9)            
094800      ADD  9                         TO LAENGD                            
094900      MOVE TAB-TECKEN                TO W002-ART-RAD(LAENGD:1)            
095000      ADD  1                         TO LAENGD                            
095100*                                                                         
095200      MOVE 'PART NO  '               TO W002-ART-RAD(LAENGD:9)            
095300      ADD  9                         TO LAENGD                            
095400      MOVE TAB-TECKEN                TO W002-ART-RAD(LAENGD:1)            
095500      ADD 1                          TO LAENGD                            
095600*                                                                         
095610      MOVE 'DESCRIPTION.GB           ' TO W002-ART-RAD(LAENGD:25)         
095620      ADD  25                        TO LAENGD                            
095630      MOVE TAB-TECKEN                TO W002-ART-RAD(LAENGD:1)            
095640      ADD 1                          TO LAENGD                            
095650                                                                          
095700      MOVE 'FGRP'                    TO W002-ART-RAD(LAENGD:4)            
095800      ADD  4                         TO LAENGD                            
095900      MOVE TAB-TECKEN                TO W002-ART-RAD(LAENGD:1)            
096000      ADD 1                          TO LAENGD                            
096100*                                                                         
096200      MOVE 'DISTRICT'                TO W002-ART-RAD(LAENGD:8)            
096300      ADD  8                         TO LAENGD                            
096400      MOVE TAB-TECKEN                TO W002-ART-RAD(LAENGD:1)            
096500      ADD 1                          TO LAENGD                            
096600*                                                                         
096700      MOVE 'PGRP'                    TO W002-ART-RAD(LAENGD:4)            
096800      ADD  4                         TO LAENGD                            
096900      MOVE TAB-TECKEN                TO W002-ART-RAD(LAENGD:1)            
097000      ADD 1                          TO LAENGD                            
097100*                                                                         
097200      MOVE 'SUPERSESSION'            TO W002-ART-RAD(LAENGD:12)           
097300      ADD  12                        TO LAENGD                            
097400      MOVE TAB-TECKEN                TO W002-ART-RAD(LAENGD:1)            
097500      ADD 1                          TO LAENGD                            
097600*                                                                         
097700      MOVE 'S0-PRICE    '            TO W002-ART-RAD(LAENGD:10)           
097800      ADD  10                        TO LAENGD                            
097900      MOVE TAB-TECKEN                TO W002-ART-RAD(LAENGD:1)            
098000      ADD 1                          TO LAENGD                            
098100*                                                                         
098200      MOVE 'DO-PRICE    '            TO W002-ART-RAD(LAENGD:10)           
098300      ADD  10                        TO LAENGD                            
098400      MOVE TAB-TECKEN                TO W002-ART-RAD(LAENGD:1)            
098500      ADD 1                          TO LAENGD                            
098600*                                                                         
098700      MOVE 'SUGG.RETAIL '            TO W002-ART-RAD(LAENGD:12)           
098800      ADD  12                        TO LAENGD                            
098900      MOVE TAB-TECKEN                TO W002-ART-RAD(LAENGD:1)            
099000      ADD 1                          TO LAENGD                            
099100*                                                                         
099700     PERFORM S21-SKRIV-W33522-001                                         
099800     .                                                                    
099900     EJECT                                                                
100000                                                                          
100100 DE-SKAPA-EXCELRAD SECTION.                                               
100200                                                                          
100300     MOVE 1 TO LAENGD                                                     
100400                                                                          
100500     MOVE SORTWS-IDMARKBO  TO WS-IDMARKBO                                 
100600     MOVE SORTWS-IDPROMRN  TO WS-IDPROMRN                                 
100700     MOVE WS-IDPROMR       TO W002-ART-RAD (LAENGD:3)                     
100800     ADD 3                 TO LAENGD                                      
100900     MOVE TAB-TECKEN       TO W002-ART-RAD(LAENGD:1)                      
101000     ADD 1                 TO LAENGD                                      
101100                                                                          
101200     MOVE SORTWS-IDARTNR   TO WS-IDARTNR                                  
101300     MOVE WS-IDARTNR       TO W002-ART-RAD (LAENGD:9)                     
101400     ADD 9                 TO LAENGD                                      
101500     MOVE TAB-TECKEN       TO W002-ART-RAD(LAENGD:1)                      
101600     ADD 1                 TO LAENGD                                      
101700                                                                          
101710     MOVE SORTWS-BEART-ENG TO W-DESCRIPTION-GB                            
101720     MOVE W-DESCRIPTION-GB TO W002-ART-RAD (LAENGD:25)                    
101730     ADD 25                TO LAENGD                                      
101740     MOVE TAB-TECKEN       TO W002-ART-RAD(LAENGD:1)                      
101750     ADD 1                 TO LAENGD                                      
101760                                                                          
101800     MOVE SORTWS-IDFKNGRP  TO W-IDFKNGRP                                  
101900     MOVE W-IDFKNGRP       TO W002-ART-RAD (LAENGD:4)                     
102000     ADD 4                 TO LAENGD                                      
102100     MOVE TAB-TECKEN       TO W002-ART-RAD(LAENGD:1)                      
102200     ADD 1                 TO LAENGD                                      
102300                                                                          
102400     MOVE SORTWS-IDDISTR   TO WS-IDDISTR                                  
102500     MOVE WS-IDDISTR        TO W002-ART-RAD (LAENGD:5)                    
102600     ADD 5                 TO LAENGD                                      
102700     MOVE TAB-TECKEN       TO W002-ART-RAD(LAENGD:1)                      
102800     ADD 1                 TO LAENGD                                      
102900                                                                          
103000     MOVE SORTWS-KDPRODSL  TO W-KDPRODSL                                  
103100     MOVE W-KDPRODSL       TO W002-ART-RAD (LAENGD:2)                     
103200     ADD 2                 TO LAENGD                                      
103300     MOVE TAB-TECKEN       TO W002-ART-RAD(LAENGD:1)                      
103400     Add 1                 TO LAENGD                                      
103500                                                                          
103600     MOVE SORTWS-KDERS     TO W-KDERS                                     
103700     Move W-KDERS          TO W002-ART-RAD (LAENGD:2)                     
103800     Add 2                 TO LAENGD                                      
103900     Move TAB-TECKEN       TO W002-ART-RAD(LAENGD:1)                      
104000     Add 1                 TO LAENGD                                      
104100                                                                          
104200     MOVE SORTWS-MO-NOR-PRIS TO W-MO-NOR-PRIS                             
104300     MOVE W-MO-NOR-PRIS    TO W002-ART-RAD (LAENGD:10)                    
104400     ADD 10                TO LAENGD                                      
104500     MOVE TAB-TECKEN       TO W002-ART-RAD(LAENGD:1)                      
104600     ADD 1                 TO LAENGD                                      
104700                                                                          
104800     MOVE SORTWS-DO-NOR-PRIS TO W-DO-NOR-PRIS                             
104900     MOVE W-DO-NOR-PRIS    TO W002-ART-RAD (LAENGD:10)                    
105000     ADD 10                TO LAENGD                                      
105100     MOVE TAB-TECKEN       TO W002-ART-RAD(LAENGD:1)                      
105200     ADD 1                 TO LAENGD                                      
105300                                                                          
105400     MOVE SORTWS-PRARTBTO-MARK TO W-PRARTBTO-MARK                         
105500     MOVE W-PRARTBTO-MARK  TO W002-ART-RAD (LAENGD:10)                    
105600     ADD 10                TO LAENGD                                      
105700     MOVE TAB-TECKEN       TO W002-ART-RAD(LAENGD:1)                      
105800     ADD 1                 TO LAENGD                                      
105900                                                                          
106600     .                                                                    
106700     EJECT                                                                
106800                                                                          
106900 Z-FINIT SECTION.                                                         
107000                                                                          
107100     CLOSE W33522                                                         
107200           W91042                                                         
107300           W33522-001                                                     
107400                                                                          
107500     MOVE 'T'         TO POSTSUM-OPKOD                                    
107600     MOVE 'W33522'    TO POSTSUM-FDNAMN                                   
107700     MOVE 'W33522D2'  TO POSTSUM-DDNAMN2                                  
107800     MOVE 'IN'        TO POSTSUM-TRANSTYP                                 
107900     MOVE ANT-LB-POST TO POSTSUM-TOTTRANS                                 
108000     CALL POSTSUM USING POSTSUM-PARM                                      
108100                                                                          
108200     MOVE 'S' TO POSTSUM-OPKOD                                            
108300     CALL POSTSUM USING POSTSUM-PARM                                      
108400     .                                                                    
108500     EJECT                                                                
108600 S01-LAES-W33522  SECTION.                                                
108700                                                                          
108800     READ W33522 INTO IN10-AREA                                           
108900     AT End                                                               
109000        SET END-OF-W33522 TO TRUE                                         
109100                                                                          
109200     NOT AT End                                                           
109300        MOVE 'W33522' TO POSTSUM-FDNAMN                                   
109400        MOVE 'W33522D1' TO POSTSUM-DDNAMN2                                
109500        MOVE 'PARM'     TO POSTSUM-TRANSTYP                               
109600        CALL POSTSUM USING POSTSUM-PARM                                   
109700     END-READ                                                             
109800     .                                                                    
109900     EJECT                                                                
110000 S02-LAES-W91042  SECTION.                                                
110100                                                                          
110200     READ W91042 INTO IN42-AREA                                           
110300     AT END                                                               
110400        SET END-OF-W91042 TO TRUE                                         
110500                                                                          
110600     NOT AT END                                                           
110700        ADD +1 TO ANT-LB-POST                                             
110800     END-READ                                                             
110900     .                                                                    
111000     EJECT                                                                
111100 S21-SKRIV-W33522-001  SECTION.                                           
111200                                                                          
111300     IF IN10-FLEXCEL = JA                                                 
111400       WRITE W33522-001-RAD From W002-DETALJ(1:LAENGD)                    
111600       MOVE SPACE TO W002-DETALJ                                          
111700     ELSE                                                                 
111800       IF ANTAL-RADER > W001-MAX-RADER-PER-SIDA                           
111900           PERFORM S21A-SKRIV-RUBRIKER                                    
112000       END-IF                                                             
112100                                                                          
112200*      IF IN10-FLMAIL = JA                                                
112300*        WRITE W33522-001-RAD FROM W001-DETALJ                            
112400*      ELSE                                                               
112500         WRITE W33522-001-RAD FROM W001-DETALJ AFTER W001-SKIP            
112600*      END-IF                                                             
112700       MOVE SPACE TO W001-DETALJ                                          
112800     END-IF                                                               
112900     MOVE 1     TO W001-SKIP                                              
113000     ADD +1     TO ANTAL-RADER                                            
113100                                                                          
113200     MOVE 'LISTA ' TO POSTSUM-FDNAMN                                      
113300     MOVE 'W33522D3' TO POSTSUM-DDNAMN2                                   
113400     MOVE 'RAD'      TO POSTSUM-TRANSTYP                                  
113500     CALL POSTSUM USING POSTSUM-PARM                                      
113600     .                                                                    
113700     EJECT                                                                
113800 S21A-SKRIV-RUBRIKER SECTION.                                             
113900                                                                          
114000*    IF IN10-FLMAIL = JA                                                  
114100*      IF FOERSTA-MAIL = JA                                               
114200*        WRITE W33522-001-RAD FROM W001-RUBRIK1                           
114500*        MOVE 2               TO W001-SKIP                                
114600*        MOVE 7               TO ANTAL-RADER                              
114700*        MOVE NEJ             TO FOERSTA-MAIL                             
114800*      END-IF                                                             
114900*    ELSE                                                                 
115000       ADD +1               TO W001-SIDRAKNARE                            
115100       MOVE W001-SIDRAKNARE TO W001-SID                                   
115200       WRITE W33522-001-RAD FROM W001-RUBRIK1 AFTER PAGE                  
115300       WRITE W33522-001-RAD FROM W001-RUBRIK2 AFTER 2                     
115500       MOVE 2               TO W001-SKIP                                  
115600       MOVE 7               TO ANTAL-RADER                                
115700*    END-IF                                                               
115800     .                                                                    
115900     EJECT                                                                
116000                                                                          
117000 S31-SORT-RELEASE  SECTION.                                               
117100                                                                          
117200     RELEASE SORT-POST FROM SORTWS-AREA                                   
117300     .                                                                    
117400     EJECT                                                                
117500                                                                          
117600 S32-SORT-RETURN  SECTION.                                                
117700                                                                          
117800     RETURN SORTFIL INTO SORTWS-AREA                                      
117900     AT END                                                               
118000         SET END-OF-SORTFIL TO TRUE                                       
118300     END-RETURN                                                           
118400     .                                                                    
118500     EJECT                                                                
118600                                                                          
118700 S99-ABEND SECTION.                                                       
118800                                                                          
118900     MOVE 'T'         TO POSTSUM-OPKOD                                    
119000     MOVE 'W33522'    TO POSTSUM-FDNAMN                                   
119100     MOVE 'W33522D2'  TO POSTSUM-DDNAMN2                                  
119200     MOVE 'IN'        TO POSTSUM-TRANSTYP                                 
119300     MOVE ANT-LB-POST TO POSTSUM-TOTTRANS                                 
119400     CALL POSTSUM USING POSTSUM-PARM                                      
119500                                                                          
119600     MOVE 'S' TO POSTSUM-OPKOD                                            
119700     CALL POSTSUM USING POSTSUM-PARM                                      
119800     CALL ABEND USING RKOD-ABEnd-UTAN-DUMP                                
119900     .                                                                    
120000     EJECT                                                                
120100** IMS-SECTIONS  ****                                                     
120200*                                                                         
120300 IMS-RESTART SECTION.                                                     
120400     SKIP2                                                                
120500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
120600     MOVE '  ' TO GODK-STATUSKODER                                        
120700     CALL CBLTDLI USING XRST MSG-PCB                                      
120800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
120900                        CHKP-AREA-LENGTH CHKP-AREA                        
121000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
121100     PERFORM IMS-STATUSKONTROLL                                           
121200     .                                                                    
121300     SKIP3                                                                
121400*                                                                         
124000 IMS-GU-WDC101 SECTION.                                                   
124100     STRING 'WDC101  (WDC101KY =' W-WDC101KY-X ')'                        
124200          DELIMITED BY SIZE INTO SSA1                                     
124300     MOVE '  GE' TO GODK-STATUSKODER                                      
124400     CALL CBLTDLI USING GU WDC1-PCB DLI-IO-WDC101 SSA1                    
124500     MOVE WDC1-STATUS-CODE TO STATUS-WS                                   
124600     PERFORM IMS-STATUSKONTROLL                                           
124700     .                                                                    
124800     SKIP3                                                                
124900 IMS-GU-WDC201 SECTION.                                                   
125000     STRING 'WDC201  (IDPROMR  =' W-IDPROMR-X ')'                         
125100          DELIMITED BY SIZE INTO SSA1                                     
125200     MOVE '  GE' TO GODK-STATUSKODER                                      
125300     CALL CBLTDLI USING GU WDC2-PCB DLI-IO-WDC201 SSA1                    
125400     MOVE WDC2-STATUS-CODE TO STATUS-WS                                   
125500     PERFORM IMS-STATUSKONTROLL                                           
125600     .                                                                    
125700     SKIP3                                                                
125800 IMS-GNP-WDC213 SECTION.                                                  
125900     STRING 'WDC201  (IDPROMR  =' W-IDPROMR-X ')'                         
126000          DELIMITED BY SIZE INTO SSA1                                     
126100     STRING 'WDC213  (DASTADAT<=' W-DASTADAT-X ')'                        
126200          DELIMITED BY SIZE INTO SSA2                                     
126300     MOVE '  GE' TO GODK-STATUSKODER                                      
126400     CALL CBLTDLI USING GNP WDC2-PCB DLI-IO-WDC213 SSA1 SSA2              
126500     MOVE WDC2-STATUS-CODE TO STATUS-WS                                   
126600     PERFORM IMS-STATUSKONTROLL                                           
126700     .                                                                    
126800     EJECT                                                                
126810 IMS-GU-WDB101 SECTION.                                                   
126820     STRING 'WDB101  (WDB1ASEQ =' W-WDB1ASEQ-X ')'                        
126840          DELIMITED BY SIZE INTO SSA1                                     
126850                                                                          
126860     MOVE '  GEGB' TO GODK-STATUSKODER                                    
126870     CALL CBLTDLI USING GU WDB1A-PCB DLI-IO-WDB101 SSA1                   
126880     MOVE WDB1A-STATUS-CODE TO STATUS-WS                                  
126890     PERFORM IMS-STATUSKONTROLL                                           
126891     .                                                                    
126892     EJECT                                                                
126924 DB2-DCL-OPN-TP7PART-CRS SECTION.                                         
126925                                                                          
126926     PERFORM DB2-INITIALIZE-GOOD-SQLCODE                                  
126927     MOVE ZERO      TO GOOD-SQLCODE(1)                                    
126928                                                                          
126929     EXEC SQL                                                             
126930         DECLARE TP7PART-CRS CURSOR WITH HOLD FOR                         
126931         SELECT IDDISTR                                                   
126932         FROM   TP7PART                                                   
126933         WHERE  IDPARTNR = :W-IDPARTNR                                    
126934         FOR FETCH ONLY                                                   
126935     END-EXEC                                                             
126936                                                                          
126937     EXEC SQL OPEN TP7PART-CRS END-EXEC                                   
126938     MOVE SQLCODE TO SQLCODE-WS                                           
126939     PERFORM DB2-STATUS-CHECK                                             
126940     .                                                                    
126941     SKIP3                                                                
126942 DB2-FETCH-TP7PART-CRS SECTION.                                           
126943                                                                          
126944     PERFORM DB2-INITIALIZE-GOOD-SQLCODE                                  
126945     MOVE ZERO      TO GOOD-SQLCODE(1)                                    
126946     MOVE +100      TO GOOD-SQLCODE(2)                                    
126947     EXEC SQL                                                             
126948         FETCH TP7PART-CRS INTO :WS-IDDISTR-DB2                           
126949     END-EXEC                                                             
126950                                                                          
126951     MOVE SQLCODE TO SQLCODE-WS                                           
126952     PERFORM DB2-STATUS-CHECK                                             
126953     .                                                                    
126954     SKIP3                                                                
126955 DB2-CLOSE-TP7PART-CRS SECTION.                                           
126956                                                                          
126957     PERFORM DB2-INITIALIZE-GOOD-SQLCODE                                  
126958     MOVE ZERO      TO GOOD-SQLCODE(1)                                    
126959     EXEC SQL CLOSE TP7PART-CRS END-EXEC                                  
126960                                                                          
126961     MOVE SQLCODE TO SQLCODE-WS                                           
126962     PERFORM DB2-STATUS-CHECK                                             
126963     .                                                                    
126964     EJECT                                                                
126965 DB2-INITIALIZE-GOOD-SQLCODE SECTION.                                     
126966                                                                          
126967     SET SQLCODE-IX                 TO 1                                  
126968     PERFORM UNTIL SQLCODE-IX > 5                                         
126969        MOVE ZERO                   TO GOOD-SQLCODE(SQLCODE-IX)           
126970        SET SQLCODE-IX           UP BY 1                                  
126971     END-PERFORM                                                          
126972     .                                                                    
126973     EJECT                                                                
126974 DB2-STATUS-CHECK  SECTION.                                               
126975                                                                          
126976     MOVE 'DB2-STATUS-CHECK  '  TO CURRENT-SECTION                        
126977                                                                          
126978     SET SQLCODE-IX TO 1                                                  
126979     SEARCH GOOD-SQLCODE                                                  
126980       AT END                                                             
126981         STRING ' INVALID SQLCODE FROM DB2 ' SQLCODE-WS                   
126982         DELIMITED BY SIZE INTO ABEND-RAD2                                
126983         PERFORM S99-ABEND                                                
126984       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
126985     END-SEARCH                                                           
126986     .                                                                    
126990 IMS-STATUSKONTROLL SECTION.                                              
127000                                                                          
127100     SET STATUS-IX TO 1                                                   
127200     SEARCH GODK-STATUS                                                   
127300       AT END                                                             
127400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
127500         DELIMITED BY SIZE INTO FELTEXT                                   
127600         CALL FELLOG                                                      
127700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
127800         CONTINUE                                                         
127900     END-SEARCH                                                           
128000     .                                                                    
128100     EJECT                                                                
128200*    -COPY WY2000P2                                                       
128300     EJECT                                                                
128400*    -COPY WY2000P3                                                       
