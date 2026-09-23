000101 ID DIVISION.                                                             
000201 PROGRAM-ID.     W4057800.                                                
000301 AUTHOR.         GÖRAN KJELLSON  GUIDE                                    
000401 DATE-WRITTEN.   JULI 2005                                                
000501 DATE-COMPILED.                                                           
000601                                                                          
000701*    FUNKTION:                                                            
000801*        LDC BACKORDER RELAEASE                                           
000901*                                                                         
001001*        PROGRAM    READ    WDP7                                          
001101*                           USERDATABAS                                   
001201*        PROGRAM    UPDATE  WDA5                                          
001301*                           BACKORDER                                     
001401*        PROGRAM    UPDATE  WDR5 4563/64                                  
001501*                           NOTERINGSFÄLT RESTORDER                       
001601*        PROGRAM    READ    WDA5A                                         
001701*                           BACKORDER BY PARTNO                           
001801*        PROGRAM    READ    WDB2                                          
001901*                           CUSTOMER                                      
002001*        PROGRAM    UPDATE  WDK6                                          
002101*                           ARTIKELINFO                                   
002201*        PROGRAM    UPDATE  WDK7                                          
002301*                           S-LAGER ARTIKLEINFO                           
002401*        PROGRAM    READ    WDD9                                          
002501*                           ARTIKELINFOREGITER                            
002601*                                                                         
002700*    INDATA.                                                              
002800*        TRANSAKTION: W4T578                                              
002900*        MID:         W4I57801                                            
003000*                                                                         
003100*    UTDATA.                                                              
003200*        MOD:         W4O57801                                            
003300*                                                                         
003400*    E-TRACKER:  7450328  2008-HÖST  VOHF                                 
003500*    E-TRACKER: 10254592  2015-HÖST  DECOMISSION VOHF                     
003510*    STORY 2375089 / ADD IDSYSTEM VOUI, ECOM                              
003600     SKIP3                                                                
003700 ENVIRONMENT DIVISION.                                                    
003800 CONFIGURATION SECTION.                                                   
003801 SPECIAL-NAMES.                                                           
003802     CLASS IDSYS4578 IS 'LDC' 'LYN' 'ECO' 'VOU' 'POL' 'TAD' 'ACC'         
003803                        'APA' 'APB' 'APC' 'APD' 'APE' 'APF' 'APG'         
003804                        'APH' 'API' 'APJ'                                 
003805     CLASS IDSYSAPIS IS 'LYNK' 'ECOM' 'VOUI' 'POLE' 'TAD ' 'ACC '         
003806                        'APA' 'APB' 'APC' 'APD' 'APE' 'APF' 'APG'         
003807                        'APH' 'API' 'APJ'.                                
003808                                                                          
003900 DATA DIVISION.                                                           
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004400 77  IDPGM                          PIC X(08) VALUE 'W4057800'.           
004500                                                                          
004600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004700 77  FELTEXT                        PIC X(80) VALUE SPACE.                
004800 77  RKOD-FELLOG                    PIC S9(4) COMP VALUE +33.             
004900 77  SPRAK-IX                      PIC S9(9) VALUE ZERO COMP SYNC.        
005000 77  DATUM-MED-ARHUNDR              PIC 9(8) VALUE ZERO.                  
005100                                                                          
005200 77  JA                             PIC X     VALUE 'J'.                  
005300 77  NEJ                            PIC X     VALUE 'N'.                  
005400 77  WS-IDDC                        PIC X(2)  VALUE SPACE.                
005410 77  WS-IDDC-GMT                    PIC X(2)  VALUE SPACE.                
005420 77  WS-RFS-DATE-SW                 PIC X(1)  VALUE 'N'.                  
005430 77  SKIP-REC-SW                    PIC X(1)  VALUE 'N'.                  
005500 77  RFS-IX                         PIC S9(4) VALUE +1  COMP SYNC.        
005600 77  MAX-RFS-IX                     PIC S9(4) VALUE +4  COMP SYNC.        
005700 77  SPEC-FORBI                     PIC X     VALUE 'S'.                  
005800 77  VERKSTADSORDER                 PIC X(2)  VALUE 'PW'.                 
005900 77  BUTIKSORDER                    PIC X(2)  VALUE 'PC'.                 
006000 77  W-UPDATE                       PIC X(1)  VALUE 'N'.                  
006100 77  CURR-SECTION                   PIC X(16) VALUE 'MAIN'.               
006200 77  CURR-IMS-SECTION               PIC X(16) VALUE SPACE.                
006300 77  SW-TRAEFF                      PIC X     VALUE SPACE.                
006400 77  RKOD-ABEND-UTAN-DUMP           PIC S9(4) VALUE +16 COMP SYNC.        
006500 77  RKOD-ABEND-MED-DUMP            PIC S9(4) COMP VALUE +1000.           
006600 77  ERROR-TEXT                     PIC X(80) VALUE SPACE.                
006700 77  KDRC-DISPLAY                   PIC Z(5).                             
006800 77  WZ04-SEND-IDCOM                PIC S9(9) COMP VALUE +0.              
006900                                                                          
007000                                                                          
007100 77  W-TITPO-TIAAVV                 PIC 9(4)  VALUE ZERO.                 
007200 77  W-DAGENS-DAT-TIAAVV            PIC 9(4)  VALUE ZERO.                 
007300 77  WS-TIRFS                       PIC 9(6).                             
007310 77  WS-TIRFS-CDC                   PIC 9(6).                             
007400 77  WS-DELETE-PLUS-MAIL            PIC X     VALUE 'D'.                  
007500 77  WS-NORMAL-TILL-LDC             PIC X     VALUE 'N'.                  
007600 77  WS-FORBIORDER                  PIC X     VALUE 'F'.                  
007700 77  WS-FORBIORDER-VOR              PIC X     VALUE 'V'.                  
007800 77  WS-MAIL                        PIC X     VALUE 'M'.                  
007900 77  SPAR-KVART                     PIC 9(7).                             
008000 77  SPARA-IDORDNR                  PIC X(7)  VALUE SPACE.                
008101 77  W-SPAR-TILEVBSK                PIC S9(7) COMP-3 VALUE ZERO.          
008200                                                                          
008300 77  SW-NASTA-FINNS            PIC X         VALUE 'N'.                   
008400     88  NASTA-FINNS                         VALUE 'J'.                   
008500     88  NASTA-SAKNAS                        VALUE 'N'.                   
008600                                                                          
008700 77  FRYSTID-SW                PIC X         VALUE 'N'.                   
008800     88  BORTOM-FRYSTID                      VALUE 'J'.                   
008900     88  INOM-FRYSTID                        VALUE 'N'.                   
009000 77  IDDC-BULK-SW              PIC X         VALUE 'N'.                   
009001     88  IDDC-BULK-FOUND                     VALUE 'J'.                   
009003 77  IDDC-DAY-SW               PIC X         VALUE 'N'.                   
009004     88  IDDC-DAY-FOUND                      VALUE 'J'.                   
009006                                                                          
009102 77  TPO-BLOCK-SW              PIC X         VALUE 'N'.                   
009202     88  SKIP-RAD-ROW                        VALUE 'J'.                   
009203 77  HOLIDAY-SW                PIC X         VALUE 'N'.                   
009204     88  ITS-HOLIDAY                         VALUE 'J'.                   
009300                                                                          
009400 77  WS-IDDISTR                     PIC 9(4)  VALUE ZERO.                 
009500 77  WS-IDKUNDNR                    PIC 9(6)  VALUE ZERO.                 
009600 77  WS-KDFRAKT                     PIC 9(2)  VALUE ZERO.                 
009700 77  WS-KVART                       PIC 9(6)  VALUE ZERO.                 
009801 77  W-IDTRP                        PIC X(5)  VALUE SPACE.                
009901 01  W-KVLEDTIM                     PIC 9(5)  VALUE ZERO.                 
009902 01  FILLER REDEFINES W-KVLEDTIM.                                         
009903     03  FILLER                     PIC 9(3).                             
009904     03  W-LEDTIM-MM                PIC 9(2).                             
009905 01  W-TITRPAVG                     PIC 9(7)  VALUE ZERO.                 
009906 01  FILLER REDEFINES W-TITRPAVG.                                         
009907     03  FILLER                     PIC 9(5).                             
009908     03  W-TITRP-MM                 PIC 9(2).                             
010001 77  W-FIRST-CUT-OFF                PIC 9(5)  VALUE ZERO.                 
010101 01  W-CUT-OFF                      PIC 9(4)  VALUE ZERO.                 
010201 01  FILLER REDEFINES W-CUT-OFF.                                          
010301     03  W-CUT-OFF-TT               PIC 9(2).                             
010401     03  W-CUT-OFF-MM               PIC 9(2).                             
010501 01  W-CUT-OFF-RED.                                                       
010701     03  W-RED-TT                   PIC 9(2)  VALUE ZERO.                 
010801     03  FILLER                     PIC X(1)  VALUE '.'.                  
010901     03  W-RED-MM                   PIC 9(2)  VALUE ZERO.                 
011001     03  FILLER                     PIC X(1)  VALUE SPACE.                
011101 01  W-CUT-OFF-NOT-FOUND           PIC X(12) VALUE '  TOO LATE  '.        
011201                                                                          
011300 77  WS-SELECTION                   PIC X(3).                             
011400     88 SHOW-DISTRICT               VALUE 'D  '.                          
011501     88 SHOW-DISTRICT-CUST          VALUE 'DC '.                          
011502     88 SHOW-DISTRICT-PARTNO        VALUE 'DP '.                          
011601     88 SHOW-DISTRICT-CUST-PARTNO   VALUE 'DCP'.                          
011701     88 SHOW-PARTNO                 VALUE 'P  '.                          
011801     88 SHOW-DISTRICT-RFSDATE       VALUE 'DR '.                          
011901     88 SHOW-DANGEROUS              VALUE 'DG '.                          
012001     88 SHOW-RFSDATE                VALUE 'R  '.                          
012002     88 SHOW-DANG-RFSDATE           VALUE 'DGR'.                          
012100                                                                          
012200*    --- INDEX FÖR BLÄDDRINGSRADER                                        
012300 77  INDX                           PIC S9(4) VALUE +0  COMP SYNC.        
012301 77  DC-INDX                        PIC S9(4) VALUE +0  COMP SYNC.        
012310 77  WS-INDX                        PIC S9(4) VALUE +1  COMP SYNC.        
012400 77  INDX-JUST-NEW                  PIC S9(4) VALUE +0  COMP SYNC.        
012500 77  INDX-JUST-OLD                  PIC S9(4) VALUE +0  COMP SYNC.        
012600 77  MAX-INDX                       PIC S9(4) VALUE +6  COMP SYNC.        
012700*77  MAX-INDX                       PIC S9(4) VALUE +4  COMP SYNC.        
012800                                                                          
012900 77  WS-CURR-AAVVD                  PIC 9(5)  VALUE ZERO.                 
013000 77  WS-CURR-AAMMDD                 PIC 9(6)  VALUE ZERO.                 
013100                                                                          
013201 01  WS-ADBETRAD-1                 PIC X(35) VALUE SPACE.                 
013301 01  WS-ADBETRAD-2                 PIC X(35) VALUE SPACE.                 
013401 01  WS-BEBETRAD-1                 PIC X(35) VALUE SPACE.                 
013501 01  WS-BEBETRAD-2                 PIC X(35) VALUE SPACE.                 
013601                                                                          
013602*PRATRTNTO-LOC  CONVERSION                                                
013603 01  W-PRARTNTO-LOC              PIC 9(7)V9(2).                           
013604 01  FILLER REDEFINES W-PRARTNTO-LOC.                                     
013605    03  W-PRARTNTO-LOC-HEL       PIC 9(7).                                
013606    03  W-PRARTNTO-LOC-DEC       PIC 9(2).                                
013607                                                                          
013608 01 W-PRARTNTO-LOC-X.                                                     
013609     03 W-PRARTNTO-LOC-X-HEL      PIC X(7).                               
013610     03 FILLER                    PIC X(1)    VALUE '.'.                  
013620     03 W-PRARTNTO-LOC-X-DEC      PIC X(2).                               
013700 01  W-DABEHOV.                                                           
013800     03  W-DABEHOV-SEKEL       PIC 9(2)      VALUE ZERO.                  
013900     03  W-DABEHOV-AAVV        PIC 9(4)      VALUE ZERO.                  
014000                                                                          
014100 01  DAGENS-DATUM                PIC 9(6)   VALUE ZERO.                   
014200 01  CURRENT-TIME.                                                        
014300     03  CURR-DATE               PIC 9(8).                                
014400     03  CURR-TIME.                                                       
014500         05  CURR-NUM-TIME       PIC 9(8).                                
014601         05  FILLER REDEFINES CURR-NUM-TIME.                              
014701             07  CURR-TTMM       PIC 9(4).                                
014801             07  FILLER          PIC X(4).                                
014900                                                                          
015000 01  WS-TIREPDAT-X.                                                       
015100     03 WS-TIREPDAT                 PIC S9(7) COMP-3.                     
015200                                                                          
015300 77  NYCKLAR-SW                     PIC X     VALUE 'J'.                  
015400     88  NYCKLAR-OK                           VALUE 'J'.                  
015500     88  NYCKLAR-FEL                          VALUE 'N'.                  
015600                                                                          
015700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
015800     88  INDATA-OK                           VALUE 'J'.                   
015900     88  INDATA-FEL                          VALUE 'N'.                   
016000                                                                          
016100 77  W-IDTRANS                      PIC X(4)  VALUE SPACE.                
016200     88  EGEN-MID                             VALUE '4578'.               
016300     88  HELP-MID                             VALUE '0551'.               
016400                                                                          
016500     EJECT                                                                
016600 01  TEST-IDDISTR              PIC S9(5) COMP-3.                          
016700*01  FILLER -COPY WWDIST19 -RED TEST-IDDISTR.                             
016800     EJECT                                                                
016900*01  FILLER -COPY WWDIST03 -RED TEST-IDDISTR.                             
017001     EJECT                                                                
017101*01  FILLER -COPY WWBYT03                                                 
017200     EJECT                                                                
017300*01    -COPY WWTEXT01                                                     
017401     EJECT                                                                
017501*01    -COPY WWDCKONS                                                     
017600     EJECT                                                                
017700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
017800 01  GENERELLA-SUBPROGRAM.                                                
017900     03  W411ORDN                   PIC X(8) VALUE 'W411ORDN'.            
018000     03  W411KREG                   PIC X(8) VALUE 'W411KREG'.            
018100     03  WMEDKONV                   PIC X(8) VALUE 'WMEDKONV'.            
018200     03  WDATKONV                   PIC X(8) VALUE 'WDATKONV'.            
018300     03  WORKDAY                    PIC X(8) VALUE 'WORKDAY '.            
018400     03  W005INIT                   PIC X(8) VALUE 'W005INIT'.            
018500     03  CBLTDLI                    PIC X(8) VALUE 'CBLTDLI '.            
018600     03  FELLOG                     PIC X(8) VALUE 'FELLOG  '.            
018700     03  ABEND                      PIC X(8) VALUE 'ABEND   '.            
018800     03  WZ01SEND                   PIC X(8) VALUE 'WZ01SEND'.            
018900     03  WSECURIT                   PIC X(8) VALUE 'WSECURIT'.            
019000     03  W009VADD                   PIC X(8) VALUE 'W009VADD'.            
019100     03  W009KSIF                   PIC X(8) VALUE 'W009KSIF'.            
019200     03  W006KOM                    PIC X(8) VALUE 'W006KOM '.            
019300     EJECT                                                                
019400 01  FILLER                      PIC X(16)   VALUE 'W411ORDN'.            
019500*    --- PARAMETRAR TILL SUBPROGRAM W411ORDN                              
019600*01 -COPY W411ORDN                                                        
019700     EJECT                                                                
019800 01  FILLER                      PIC X(16)   VALUE 'W411KREG'.            
019900*    --- PARAMETRAR TILL SUBPROGRAM W411KREG                              
020000*01 -COPY W411KREG                                                        
020100     EJECT                                                                
020200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
020300*01 -COPY WMEDAREA                                                        
020400                                                                          
020500*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
020600*01 -COPY WDATAREA                                                        
020700                                                                          
020800*    --- PARAMETRAR TILL SUBPROGRAM WORKDAY                               
020900*01 -COPY WORKAREA                                                        
021000                                                                          
021100*   --- PARAMETRAR TILL SUBPROGRAM W009VADD                               
021200                                                                          
021300 01  W009VADD-AREA.                                                       
021400     03  VECKO-DATUM-AAVV         PIC S9(5) COMP-3.                       
021500     03  VECKO-ANTAL              PIC S9(3) COMP-3.                       
021600     EJECT                                                                
021700                                                                          
021800 01  KONTROLL-SIFFRA.                                                     
021900     03  REK-IDARTNR             PIC 9(9)    VALUE 0.                     
022000     03  REK-LNGD                PIC 9(1)    VALUE 9.                     
022100     03  REK-REKSIFFR            PIC 9(1)    VALUE 0.                     
022200                                                                          
022300 01  MESSAGE-CODES.                                                       
022400     03  ERR-CORR-HILITE-FLDS       PIC X(3) VALUE '001'.                 
022500     03  ERR-KEYS-ARE-MISSING       PIC X(3) VALUE '005'.                 
022600     03  INF-FIRST-PAGE             PIC X(3) VALUE '006'.                 
022700     03  ERR-PF11-AND-NO-DATA       PIC X(3) VALUE '011'.                 
022800     03  ERR-INOM-FRYS              PIC X(3) VALUE '069'.                 
022900     03  INF-UPDATE-DONE            PIC X(3) VALUE '101'.                 
023000     03  INF-MORE-INFO-EXISTS       PIC X(3) VALUE '105'.                 
023100     03  INF-LAST-PAGE              PIC X(3) VALUE '106'.                 
023200     03  ERR-LAST-PAGE-SHOWN        PIC X(3) VALUE '115'.                 
023300     03  ERR-WRONG-KEY              PIC X(3) VALUE '401'.                 
023400     EJECT                                                                
023500*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
023600*                                                                         
023700 01  FILLER                         PIC X(16) VALUE 'WMSGINIT'.           
023800     SKIP3                                                                
023900*01 -COPY WMSGINIT                                                        
024000     EJECT                                                                
024100*  03    FILLER -COPY WSECAREA.                                           
024200     EJECT                                                                
024300                                                                          
024400 01  WS-HELPDAT                     PIC 9(6).                             
024500                                                                          
024600 01  WS-DAT                         PIC 9(6).                             
024700 01  FILLER REDEFINES WS-DAT.                                             
024800     03 WS-YEAR                     PIC 9(2).                             
024900     03 WS-MONTH                    PIC 9(2).                             
025000     03 WS-DAYS                     PIC 9(2).                             
025100 01  WS-TID                         PIC 9(6).                             
025200 01  FILLER REDEFINES WS-TID.                                             
025300     03 WS-HOURS                    PIC 9(2).                             
025400     03 WS-MINUTES                  PIC 9(2).                             
025500     03 WS-SECONDS                  PIC 9(2).                             
025600                                                                          
025700 01  WS-HELP-AREA.                                                        
025800     03  WS-HELP-RAD  OCCURS 6.                                           
025900         05  HELP-IDDISTR-RAD    PIC 9(5).                                
026000         05  HELP-IDKUNDNR-RAD   PIC 9(7).                                
026100         05  HELP-IDORDNR-RAD    PIC 9(5).                                
026200         05  HELP-IDARTNR-RAD    PIC 9(9).                                
026300         05  HELP-IDLOPNR-RAD    PIC 9(3).                                
026400         05  HELP-BETEXT-RAD     PIC X(10).                               
026500                                                                          
026600 01  FILLER PIC X(16)   VALUE 'SPARAREA'.                                 
026700 01  SPAR-AREA.                                                           
026800     03  SPAR-IDTRANS            PIC X(4)    VALUE '4578'.                
026900     03  SPAR-URVAL              PIC X(3).                                
027000     03  SPAR-IDDISTR-ENTER      PIC S9(5)   COMP-3.                      
027100     03  SPAR-IDKUNDNR-ENTER     PIC S9(7)   COMP-3.                      
027200     03  SPAR-IDKUNDRF-ENTER     PIC X(10).                               
027300     03  SPAR-IDARTNR-ENTER      PIC S9(9)   COMP-3.                      
027400     03  SPAR-IDLOPNR-ENTER      PIC S9(3)   COMP-3.                      
027500     03  SPAR-IDDC-ENTER         PIC X(2).                                
027600     03  SPAR-KDRAPRIO-ENTER     PIC S9(3)   COMP-3.                      
027700     03  SPAR-DARODAT-ENTER      PIC 9(8).                                
027801     03  SPAR-TIREGTID-ENTER     PIC S9(7)   COMP-3.                      
027802     03  SPAR-TIRFSDAT-CDC-ENTER PIC 9(6).                                
027803     03  SPAR-KDFARLIG-ENTER     PIC X.                                   
027900                                                                          
028000     03  SPAR-IDDISTR-NEXT       PIC S9(5)   COMP-3.                      
028100     03  SPAR-IDKUNDNR-NEXT      PIC S9(7)   COMP-3.                      
028200     03  SPAR-IDKUNDRF-NEXT      PIC X(10).                               
028300     03  SPAR-IDARTNR-NEXT       PIC S9(9)   COMP-3.                      
028400     03  SPAR-IDLOPNR-NEXT       PIC S9(3)   COMP-3.                      
028500     03  SPAR-IDDC-NEXT          PIC X(2).                                
028600     03  SPAR-KDRAPRIO-NEXT      PIC S9(3)   COMP-3.                      
028700     03  SPAR-DARODAT-NEXT       PIC 9(8).                                
028801     03  SPAR-TIREGTID-NEXT      PIC S9(7)   COMP-3.                      
028802     03  SPAR-KDFARLIG-NEXT      PIC X.                                   
028803     03  SPAR-TIRFSDAT-CDC-NEXT  PIC 9(6).                                
028900                                                                          
029000     03  SPAR-RAD  OCCURS 6.                                              
029100         05  SPAR-IDDISTR-RAD    PIC 9(5).                                
029200         05  SPAR-IDKUNDNR-RAD   PIC 9(7).                                
029300         05  SPAR-IDORDNR-RAD    PIC 9(5).                                
029400         05  SPAR-IDARTNR-RAD    PIC 9(9).                                
029500         05  SPAR-IDLOPNR-RAD    PIC 9(3).                                
029600         05  SPAR-BETEXT-RAD     PIC X(10).                               
029610         05  SPAR-KDFARLIG-RAD   PIC X.                                   
029700         05  SPAR-TIRFSDAT-CDC-RAD   PIC 9(6).                            
029702     03  SPAR-TIRFSDAT-CDC       PIC 9(6).                                
029800                                                                          
029900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
030000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
030100     SKIP3                                                                
030200*01  MID -COPY W4I57801                                                   
030300     EJECT                                                                
030400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
030500     SKIP3                                                                
030600*01  -COPY WMSGAREA                                                       
030700     EJECT                                                                
030800     03  MOD REDEFINES MSG-AREA.                                          
030900*      05  -COPY W4O57801                                                 
031000     EJECT                                                                
031100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
031200     SKIP3                                                                
031300*01  -COPY WMFSAREA                                                       
031400     EJECT                                                                
031500                                                                          
031600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
031700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
031800     SKIP3                                                                
031900 01  NYCKLAR-TILL-DLI.                                                    
032000                                                                          
032101     03  W-WDB301KY-X.                                                    
032102         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
032301         05  W-IDDISTR-DC        PIC S9(5)   VALUE ZERO COMP-3.           
032401         05  W-IDKUNDNR-DC       PIC S9(7)   VALUE ZERO COMP-3.           
032501     03  W-WDB301KY-DEF-X.                                                
032502         05  W-IDDC-DEF          PIC  X(2)   VALUE SPACE.                 
032701         05  W-IDDISTR-DC-DEF    PIC S9(5)   VALUE ZERO COMP-3.           
032801         05  W-IDKUNDNR-DC-DEF   PIC S9(7)  VALUE +9999999 COMP-3.        
032901                                                                          
033000     03  W-IDARTNR-ARTC01-X.                                              
033100         05  W-IDARTNR-ARTC01     PIC S9(9) COMP-3   VALUE ZERO.          
033200                                                                          
033300     03  W-WDA5KEY-X.                                                     
033400         05  W-A5-IDDISTR-KEY    PIC S9(5)   VALUE ZERO COMP-3.           
033500         05  W-A5-IDKUNDNR-KEY   PIC S9(7)   VALUE ZERO COMP-3.           
033600         05  W-A5-IDKUNDRF-KEY   PIC X(10)   VALUE SPACE.                 
033700         05  FILLER REDEFINES W-A5-IDKUNDRF-KEY.                          
033800             07  W-A5-IDORDNR-KEY    PIC 9(5).                            
033900             07  FILLER              PIC X(5).                            
034000         05  W-A5-IDARTNR-KEY    PIC S9(9)   VALUE ZERO COMP-3.           
034100         05  W-A5-IDLOPNR-KEY    PIC S9(3)   VALUE ZERO COMP-3.           
034200                                                                          
034300     03  W-WDA5KEY-MIN-X.                                                 
034400         05  W-A5-IDDISTR-MIN    PIC S9(5)   VALUE ZERO COMP-3.           
034500         05  W-A5-IDKUNDNR-MIN   PIC S9(7)   VALUE ZERO COMP-3.           
034600         05  W-A5-IDKUNDRF-MIN   PIC X(10)   VALUE SPACE.                 
034700         05  W-A5-IDARTNR-MIN    PIC S9(9)   VALUE ZERO COMP-3.           
034800         05  W-A5-IDLOPNR-MIN    PIC S9(3)   VALUE ZERO COMP-3.           
034900                                                                          
035000     03  W-WDA5KEY-MAX-X.                                                 
035100         05  W-A5-IDDISTR-MAX    PIC S9(5)   VALUE ZERO COMP-3.           
035200         05  W-A5-IDKUNDNR-MAX   PIC S9(7)   VALUE ZERO COMP-3.           
035300         05  W-A5-IDKUNDRF-MAX   PIC X(10)   VALUE SPACE.                 
035400         05  W-A5-IDARTNR-MAX    PIC S9(9)   VALUE ZERO COMP-3.           
035500         05  W-A5-IDLOPNR-MAX    PIC S9(3)   VALUE ZERO COMP-3.           
035600                                                                          
035700     03  W-A5-IDARTNR-X.                                                  
035800         05  W-A5-IDARTNR        PIC S9(9)   COMP-3 VALUE 3.              
035900                                                                          
036000     03  W-IDSKYLT-X.                                                     
036100         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
036200                                                                          
036300     03  W-KDORDKL-X.                                                     
036400         05  W-KDORDKL               PIC S9      COMP-3 VALUE 3.          
036500                                                                          
037200     03  W-KDSTARAD-X.                                                    
037300         05  W-KDSTARAD              PIC X(1)    VALUE '3'.               
037400                                                                          
037500     03  W-WDA5A1KY-X.                                                    
037600         05  W-WDA5A1-IDARTNR        PIC S9(9)   COMP-3.                  
037700         05  W-WDA5A1-IDDC           PIC X(2).                            
037800         05  W-WDA5A1-KDRAPRIO       PIC S9(3)   COMP-3.                  
037900         05  W-WDA5A1-DARODAT        PIC  9(8).                           
038001         05  W-WDA5A1-TIREGTID       PIC S9(7)   COMP-3.                  
038100         05  W-WDA5A1-IDDISTR        PIC S9(5)   COMP-3.                  
038200         05  W-WDA5A1-IDKUNDNR       PIC S9(7)   COMP-3.                  
038300         05  W-WDA5A1-IDKUNDRF       PIC X(10).                           
038400         05  W-WDA5A1-IDLOPNR        PIC S9(3)   COMP-3.                  
038500                                                                          
038600     03  W-WDA5A1KY-MIN-X.                                                
038700         05  W-WDA5A1-IDARTNR-MIN    PIC S9(9)   COMP-3.                  
038800         05  W-WDA5A1-IDDC-MIN       PIC X(2).                            
038900         05  W-WDA5A1-KDRAPRIO-MIN   PIC S9(3)   COMP-3.                  
039000         05  W-WDA5A1-DARODAT-MIN    PIC  9(8).                           
039101         05  W-WDA5A1-TIREGTID-MIN   PIC S9(7)   COMP-3.                  
039200         05  W-WDA5A1-IDDISTR-MIN    PIC S9(5)   COMP-3.                  
039300         05  W-WDA5A1-IDKUNDNR-MIN   PIC S9(7)   COMP-3.                  
039400         05  W-WDA5A1-IDKUNDRF-MIN   PIC X(10).                           
039500         05  W-WDA5A1-IDLOPNR-MIN    PIC S9(3)   COMP-3.                  
039600                                                                          
039700     03  W-WDA5A1KY-MAX-X.                                                
039800         05  W-WDA5A1-IDARTNR-MAX    PIC S9(9)   COMP-3.                  
039900         05  W-WDA5A1-IDDC-MAX       PIC X(2).                            
040000         05  W-WDA5A1-KDRAPRIO-MAX   PIC S9(3)   COMP-3.                  
040100         05  W-WDA5A1-DARODAT-MAX    PIC  9(8).                           
040201         05  W-WDA5A1-TIREGTID-MAX   PIC S9(7)   COMP-3.                  
040300         05  W-WDA5A1-IDDISTR-MAX    PIC S9(5)   COMP-3.                  
040400         05  W-WDA5A1-IDKUNDNR-MAX   PIC S9(7)   COMP-3.                  
040500         05  W-WDA5A1-IDKUNDRF-MAX   PIC X(10).                           
040600         05  W-WDA5A1-IDLOPNR-MAX    PIC S9(3)   COMP-3.                  
040700                                                                          
040800     03  W-WDQ101KY-MIN-X.                                                
040900         05  W-IDORDER-MIN-N10    PIC S9(7)   VALUE ZERO COMP-3.          
041000         05  W-IDARTNR-MIN-N10    PIC S9(9)   VALUE ZERO COMP-3.          
041100         05  W-IDLOPNR-MIN-N10    PIC S9(3)   VALUE ZERO COMP-3.          
041200         05  W-IDSEKVNR-MIN-N10   PIC S9(3)   VALUE ZERO COMP-3.          
041300         05  W-IDDC-MIN-N10       PIC  X(2)   VALUE '11'.                 
041400         05  W-KDORDBEK-MIN-N10   PIC  9(2)   VALUE ZERO.                 
041500                                                                          
041600     03  W-WDQ101KY-MAX-X.                                                
041700         05  W-IDORDER-MAX-N10   PIC S9(7) VALUE 9999999   COMP-3.        
041800         05  W-IDARTNR-MAX-N10   PIC S9(9) VALUE 999999999 COMP-3.        
041900         05  W-IDLOPNR-MAX-N10   PIC S9(3) VALUE 999       COMP-3.        
042000         05  W-IDSEKVNR-MAX-N10  PIC S9(3) VALUE 999       COMP-3.        
042100         05  W-IDDC-MAX-N10      PIC  X(2) VALUE '99'.                    
042200         05  W-KDORDBEK-MAX-N10  PIC  9(2) VALUE 99.                      
042300                                                                          
042400     03  W-WDB2C1KY-X.                                                    
042500         05  W-WDB2C1-IDDC           PIC X(2)  VALUE SPACE.               
042600         05  W-WDB2C1-IDDISTR        PIC S9(5) VALUE ZERO COMP-3.         
042700         05  W-WDB2C1-IDKUNDNR       PIC S9(7) VALUE ZERO COMP-3.         
042800                                                                          
042900     03  W-WDB2C1KY-MIN-X.                                                
043000         05  W-WDB2C1-IDDC-MIN       PIC X(2).                            
043100         05  W-WDB2C1-IDDISTR-MIN    PIC S9(5) VALUE ZERO COMP-3.         
043200         05  W-WDB2C1-IDKUNDNR-MIN   PIC S9(7) VALUE ZERO COMP-3.         
043300                                                                          
043400     03  W-WDB2C1KY-MAX-X.                                                
043500         05  W-WDB2C1-IDDC-MAX       PIC X(2).                            
043600         05  W-WDB2C1-IDDISTR-MAX    PIC S9(5) VALUE ZERO COMP-3.         
043700         05  W-WDB2C1-IDKUNDNR-MAX   PIC S9(7) VALUE ZERO COMP-3.         
043800                                                                          
043900     03  W-IDGMT-X.                                                       
044000         05 W-IDDISTR-WDB2       PIC S9(5) VALUE ZERO COMP-3.             
044100         05 W-IDKUNDNR-WDB2      PIC S9(7) VALUE ZERO COMP-3.             
044200                                                                          
044300     03  W-IDGMT-MIN-X.                                                   
044400         05 W-IDDISTR-WDB2-MIN   PIC S9(5) VALUE ZERO COMP-3.             
044500         05 W-IDKUNDNR-WDB2-MIN  PIC S9(7) VALUE ZERO COMP-3.             
044600                                                                          
044700     03  W-IDGMT-MAX-X.                                                   
044800         05 W-IDDISTR-WDB2-MAX   PIC S9(5) VALUE ZERO COMP-3.             
044900         05 W-IDKUNDNR-WDB2-MAX  PIC S9(7) VALUE ZERO COMP-3.             
045000                                                                          
045100     03  W-4563KEY-X.                                                     
045200         05  W-4563-IDHTYP      PIC X(4)     VALUE '4563'.                
045300         05  FILLER             PIC X(26)    VALUE LOW-VALUE.             
045400     03  W-4564KEY-X.                                                     
045500         05  W-IDGMTREF-4564    PIC X(17)    VALUE SPACE.                 
045600         05  W-IDARTNR-4564     PIC S9(9)    VALUE ZERO COMP-3.           
045700         05  W-IDLOPNR-4564     PIC S9(3)    VALUE ZERO COMP-3.           
045800                                                                          
045900     03  W-IDARTNR-X.                                                     
046000         05  W-IDARTNR          PIC S9(9) COMP-3   VALUE ZERO.            
046100                                                                          
046200     03  W-IDDC-B6-X.                                                     
046300         05 W-IDDC-B6           PIC X(2).                                 
046400                                                                          
046500     03  W-WDGX2231-X.                                                    
046600         05  W-IDHTYP-2231       PIC X(4)     VALUE '2231'.               
046700         05  W-VALFRI-2231       PIC X(26)    VALUE LOW-VALUE.            
046800                                                                          
046900     03  W-WDGX2232-X.                                                    
047000         05  W-IDANSK-2232       PIC S9(3)    VALUE ZERO COMP-3.          
047100         05  W-LOW-VALUE-2232    PIC X(3)     VALUE LOW-VALUE.            
047200                                                                          
047300     03  W-WDGX2223-X.                                                    
047400         05  W-IDHTYP             PIC X(4)           VALUE '2223'.        
047500         05  W-IDANSK-2223        PIC S9(3) COMP-3   VALUE ZERO.          
047600         05  W-LOW-VALUE          PIC X(24) VALUE LOW-VALUE.              
047700                                                                          
047800     03  W-WDGX2224-X.                                                    
047900         05  W-TISENBEK-DAG       PIC S9(7) COMP-3   VALUE ZERO.          
048000         05  W-TISENBEK-KL        PIC S9(7) COMP-3   VALUE ZERO.          
048100         05  W-KDLARM             PIC S9(3) COMP-3   VALUE ZERO.          
048200                                                                          
048300     03  W-DABEHOV-X.                                                     
048400         05  W-DABEHOV-N          PIC 9(6)  VALUE ZERO.                   
048500                                                                          
048600     03  W-IDGMTREF-X.                                                    
048700         05  W-IDDISTR-N9         PIC S9(5) COMP-3   VALUE ZERO.          
048800         05  W-IDKUNDNR-N9        PIC S9(7) COMP-3   VALUE ZERO.          
048900         05  W-IDKUNDRF-N9        PIC X(10) VALUE SPACE.                  
049000                                                                          
049100     03  FILLER                   PIC X(16)   VALUE 'WDA501KY'.           
049200     03  W-WDA501KY.                                                      
049300         05  W-IDDISTR-N2         PIC S9(5) COMP-3   VALUE ZERO.          
049400         05  W-IDKUNDNR-N2        PIC S9(7) COMP-3   VALUE ZERO.          
049500         05  W-IDKUNDRF-N2.                                               
049600             07  W-IDORDNR-N2     PIC  9(5)          VALUE ZERO.          
049700             07  FILLER           PIC  X(5)          VALUE SPACE.         
049800         05  W-IDARTNR-N2         PIC S9(9) COMP-3   VALUE ZERO.          
049900         05  W-IDLOPNR-N2         PIC S9(3) COMP-3   VALUE ZERO.          
050000                                                                          
050100     03  W-IDARTNR-K7-X.                                                  
050200         05  W-IDARTNR-K7         PIC S9(9) COMP-3   VALUE ZERO.          
050300                                                                          
050400     03  W-IDDC-K7-X.                                                     
050500         05 W-IDDC-K7             PIC X(2).                               
050600                                                                          
050701     03  W-WDD901KY-X.                                                    
050801         05  W-IDARTNR-D9         PIC S9(9) COMP-3   VALUE ZERO.          
050901         05  W-IDDC-D9            PIC X(2)           VALUE SPACE.         
051001                                                                          
051101     03  W-IDLEVNR-X.                                                     
051201         05  W-IDLEVNR            PIC X(5)  VALUE SPACE.                  
051301                                                                          
051401     03  W-WDB501KY-X.                                                    
051501         05  W-IDDC-WDB5         PIC X(2)    VALUE SPACE.                 
051601         05  W-KDFRAKT-WDB5      PIC S9(3)   VALUE ZERO COMP-3.           
051701         05  W-IDDISTR-WDB5      PIC S9(5)   VALUE ZERO COMP-3.           
051801         05  W-IDKUNDNR-WDB5     PIC S9(7) VALUE +9999999 COMP-3.         
051901*                                                                         
052001   03  W-WDGXKEY-4433-X.                                                  
052101       05  W-4433-IDHTYP       PIC  X(4)        VALUE '4433'.             
052201       05  W-4433-IDDC         PIC  X(2)        VALUE SPACE.              
052301       05  FILLER              PIC  X(24)       VALUE LOW-VALUE.          
052401*                                                                         
052501     03  W-WDGXKEY-4434-MIN-X.                                            
052601         05  W-4434-IDTRP-MIN    PIC  X(5)   VALUE SPACE.                 
052701         05  W-4434-TITRPAVG-MIN PIC S9(7)   COMP-3 VALUE ZERO.           
052801         05  W-4434-LOW-VALUE    PIC X       VALUE LOW-VALUE.             
052901*                                                                         
053001     03  W-WDGXKEY-4434-MAX-X.                                            
053101         05  W-4434-IDTRP-MAX    PIC  X(5)   VALUE SPACE.                 
053201         05  W-4434-TITRPAVG-MAX PIC S9(7)   COMP-3 VALUE 9999.           
053301         05  W-4434-LOW-VALUE    PIC X       VALUE LOW-VALUE.             
053401                                                                          
053501 01  DAP-MAIL-BODY-AREA1.                                                 
053601      03  DAP-MAIL-LINE1-AREA1.                                           
053701          05 DAP-MAIL-LINE-TEXT-1 PIC X(47) VALUE                         
053801               'FROM  VOLVO CAR CUSTOMER SERVICE CDC GOTHENBURG'.         
053901                                                                          
054001      03  DAP-MAIL-LINE2-AREA1.                                           
054101          05 DAP-MAIL-LINE-TEXT-2 PIC X(57) VALUE                         
054201     'FOR THE URGENT ATTENTION OF THE PARTS ORDERING DEPARTMENT'.         
054301                                                                          
054401      03  DAP-MAIL-LINE3-AREA1.                                           
054501          05 DAP-MAIL-LINE-TEXT-3      PIC X(23) VALUE                    
054601                                       'Please see the details:'.         
054701                                                                          
054801 01  DAP-MAIL-BODY-AREA2.                                                 
054901      03  DAP-MAIL-LINE1-AREA2.                                           
055001          05 DAP-MAIL-LINE-TEXT-4      PIC X(14).                         
055101      03  DAP-MAIL-LINE2-AREA2.                                           
055200          05 DAP-MAIL-LINE-TEXT-5      PIC X(10).                         
055301      03  DAP-MAIL-LINE3-AREA2.                                           
055401          05 DAP-MAIL-LINE-TEXT-6      PIC X(10).                         
055501      03  DAP-MAIL-LINE4-AREA2.                                           
055601          05 DAP-MAIL-LINE-TEXT-7      PIC X(27).                         
055701      03  DAP-MAIL-LINE5-AREA2.                                           
055801          05 DAP-MAIL-LINE-TEXT-8      PIC X(10).                         
055901      03  DAP-MAIL-LINE6-AREA2.                                           
056001          05 DAP-MAIL-LINE-TEXT-9      PIC X(12).                         
056101      03  DAP-MAIL-LINE7-AREA2.                                           
056201          05 DAP-MAIL-LINE-TEXT-10     PIC X(13).                         
056301      03  DAP-MAIL-LINE8-AREA2.                                           
056401          05 DAP-MAIL-LINE-TEXT-11     PIC X(17).                         
056501                                                                          
056601 01  DAP-MAIL-BODY-AREA3.                                                 
056701      03  DAP-MAIL-LINE1-AREA3.                                           
056801          05 DAP-MAIL-LINE-FILLER1     PIC X(8) VALUE SPACE.              
056901          05 DAP-MAIL-DISTRICT-NO      PIC Z(3)9.                         
057001          05 DAP-MAIL-LINE-FILLER2     PIC X(2) VALUE SPACE.              
057101      03  DAP-MAIL-LINE2-AREA3.                                           
057201          05 DAP-MAIL-LINE-FILLER3     PIC X(2) VALUE SPACE.              
057301          05 DAP-MAIL-CUSTOMER-NO      PIC Z(5)9.                         
057401          05 DAP-MAIL-LINE-FILLER4     PIC X(2) VALUE SPACE.              
057501      03  DAP-MAIL-LINE3-AREA3.                                           
057601          05 DAP-MAIL-PART-NO          PIC Z(7)9.                         
057701          05 DAP-MAIL-LINE-FILLER5     PIC X(2) VALUE SPACE.              
057801      03  DAP-MAIL-LINE4-AREA3.                                           
057901          05 DAP-MAIL-PART-DESC        PIC X(25).                         
058001          05 DAP-MAIL-LINE-FILLER6     PIC X(2) VALUE SPACE.              
058101      03  DAP-MAIL-LINE5-AREA3.                                           
058201          05 DAP-MAIL-LINE-FILLER7     PIC X(1) VALUE SPACE.              
058301          05 DAP-MAIL-QTY              PIC Z(6)9.                         
058401          05 DAP-MAIL-LINE-FILLER8     PIC X(2) VALUE SPACE.              
058501      03  DAP-MAIL-LINE6-AREA3.                                           
058601          05 DAP-MAIL-ORDER-NO         PIC X(10).                         
058701          05 DAP-MAIL-LINE-FILLER9     PIC X(2) VALUE SPACE.              
058801      03  DAP-MAIL-LINE7-AREA3.                                           
058901          05 DAP-MAIL-LINE-FILLERA     PIC X(5) VALUE SPACE.              
059001          05 DAP-MAIL-REPAIR-DATE      PIC X(6).                          
059101          05 DAP-MAIL-LINE-FILLERB     PIC X(2) VALUE SPACE.              
059201      03  DAP-MAIL-LINE8-AREA3.                                           
059301          05 DAP-MAIL-LINE-FILLERC     PIC X(5) VALUE SPACE.              
059401          05 DAP-MAIL-WRKSHOP-ORDER    PIC X(10).                         
059501          05 DAP-MAIL-LINE-FILLERD     PIC X(2) VALUE SPACE.              
059601                                                                          
059701                                                                          
059801 01  DAP-MAIL-BODY-AREA4.                                                 
059901      03  DAP-MAIL-LINE1-AREA4.                                           
060001          05 DAP-MAIL-LINE-TEXT-12-1   PIC X(44) VALUE                    
060101     'WE WISH TO ADVISE YOU THAT THIS PART IS NOT '.                      
060201          05 DAP-MAIL-LINE-TEXT-12-2   PIC X(17) VALUE                    
060301                                             'AVAILABLE AND WE '.         
060401                                                                          
060501      03  DAP-MAIL-LINE2-AREA4.                                           
060601          05 DAP-MAIL-LINE-TEXT-13-1   PIC X(55) VALUE                    
060701       'WILL BE UNABLE TO OBTAIN YOUR ORDERED QUANTITY IN TIME '.         
060801          05 DAP-MAIL-LINE-TEXT-13-2   PIC X(04) VALUE    'FOR '.         
060901                                                                          
061001      03  DAP-MAIL-LINE3-AREA4.                                           
061101          05 DAP-MAIL-LINE-TEXT-14     PIC X(17) VALUE                    
061201                         'YOUR REPAIR DATE.'.                             
061301                                                                          
061401      03  DAP-MAIL-LINE4-AREA4.                                           
061501          05 DAP-MAIL-LINE-TEXT-15-1   PIC X(57) VALUE                    
061601     'YOUR ORDER IS CANCELLED OR KEPT DEPENDING ON THE CURRENT '.         
061701          05 DAP-MAIL-LINE-TEXT-15-2   PIC X(42) VALUE                    
061801     'RULES FOR YOUR MARKET. PLEASE TAKE ACTION.'.                        
061901                                                                          
062001      03  DAP-MAIL-LINE5-AREA4.                                           
062101          05 DAP-MAIL-LINE-TEXT-16     PIC X(43) VALUE                    
062201                   'EXPECTED TIME OF ARRIVAL AT DC 11 EARLIEST '.         
062301          05 DAP-MAIL-LINE-VALUE-WW    PIC 9(2) VALUE 0.                  
062401          05 DAP-MAIL-LINE-CONST       PIC X(1) VALUE ':'.                
062501          05 DAP-MAIL-LINE-VALUE-D     PIC 9(1) VALUE 0.                  
062601                                                                          
062701      03  DAP-MAIL-LINE6-AREA4.                                           
062801          05 DAP-MAIL-LINE-TEXT-17-1   PIC X(46) VALUE                    
062901                'IF THERE ARE ANY QUESTIONS PLEASE CONTACT THE '.         
063001          05 DAP-MAIL-LINE-TEXT-17-2   PIC X(22) VALUE                    
063101                                        'DEALER SERVICE OFFICE.'.         
063200      03  DAP-MAIL-LINE7-AREA4.                                           
063301          05 DAP-MAIL-LINE-TEXT-18     PIC X(46) VALUE                    
063401                        '(DO NOT REPLY ON THIS E-MAIL MESSAGE.)'.         
063501                                                                          
063601      03  DAP-MAIL-LINE-AREA.                                             
063701          05 DAP-MAIL-LINE-TEXT-SPACE  PIC X(80) VALUE SPACES.            
063801                                                                          
063901     SKIP2                                                                
064001*    --- STATUS-KOD FRÅN IMS                                              
064100 01  STATUS-WS                   PIC XX.                                  
064200     88  SEGMENT-FINNS                       VALUE '  '.                  
064300     88  SEGMENT-HAR-LAGTS-TILL              VALUE '  '.                  
064400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
064500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
064600     88  BASEN-SLUT                          VALUE 'GB'.                  
064700     SKIP2                                                                
064800 01  GODK-STATUSKODER.                                                    
064900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
065000     SKIP3                                                                
065100 01  SSA1                        PIC X(400).                              
065200 01  SSA2                        PIC X(160).                              
065300     EJECT                                                                
065400*    --- IMS FUNKTIONSKODER                                               
065500*01  -COPY W0003                                                          
065600     EJECT                                                                
065700*    ---  DLI INPUT-OUTPUT AREA                                           
065801 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB301'.         
065901 01  DLI-IO-WDB301.                                                       
066001*    03  -COPY WDB301                                                     
066101                                                                          
066200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA5  '.                      
066300 01  DLI-IO-WDA5.                                                         
066400*    03  WDA5 -COPY WDA501                                                
066500                                                                          
066600 01  FILLER                  PIC X(16)  VALUE 'IO-AREA-BENA11  '.         
066700 01  DLI-IO-AREA-BENA.                                                    
066800     03  WLBENA11.                                                        
066900*        05  -COPY WDD311                                                 
067000                                                                          
067100     EJECT                                                                
067200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA5A '.                      
067300 01  DLI-IO-WDA5A.                                                        
067400*    03  WDA5A -COPY WDA5A1                                               
067500                                                                          
067600 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK901'.         
067700 01    DLI-IO-WDK901.                                                     
067800*  03    WDK901 -COPY WDK901                                              
067900                                                                          
068000 01  FILLER                      PIC X(16) VALUE 'DLI-IO-2224  '.         
068100 01  DLI-IO-2224.                                                         
068200*  03    WDGX   -COPY WDGX2224 -PRE XXBU-                                 
068300     EJECT                                                                
068400                                                                          
068500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB2  '.                      
068600 01  DLI-IO-WDB2.                                                         
068700*    03  WDB2  -COPY WDB201                                               
068800                                                                          
068900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB2C '.                      
069000 01  DLI-IO-WDB2C.                                                        
069100*    03  WDB2C -COPY WDB2C1                                               
069200                                                                          
069300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR5  '.                      
069400 01  DLI-IO-WDR5.                                                         
069500*    03   -COPY WDGX4564                                                  
069600                                                                          
069700 01  FILLER                    PIC X(16) VALUE 'DLI-IO-GU-Q101  '.        
069800 01    DLI-IO-GU-Q101.                                                    
069900*  03    WDQ101 -COPY WDQ101 -PRE ORQM2-                                  
070000     EJECT                                                                
070100                                                                          
070200 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK911'.         
070300 01  DLI-IO-WDK911.                                                       
070400*  03    WDK911 -COPY WDK911                                              
070500                                                                          
070600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
070700 01  DLI-IO-WDK611.                                                       
070800*  03    WDK611 -COPY WDK611                                              
070900                                                                          
071000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
071100 01  DLI-IO-WDK711.                                                       
071200*  03    WDK711 -COPY WDK711                                              
071300                                                                          
071400 01  FILLER                      PIC X(16) VALUE 'DLI-IO-ZZAC01'.         
071500 01  DLI-IO-ZZAC01.                                                       
071600*  03    WDGZ01 -COPY WDGZ01 -PRE ZZAC-                                   
071700                                                                          
071800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
071900 01   DLI-IO-AREA-B601.                                                   
072000*     03  -COPY WDB601                                                    
072101                                                                          
072201 01  FILLER               PIC X(16)   VALUE 'WDB501 AREA'.                
072301 01   DLI-IO-AREA-B501.                                                   
072401*     03  -COPY WDB501                                                    
072500                                                                          
072600 01  FILLER               PIC X(16)   VALUE 'WDR101 AREA'.                
072700 01   DLI-IO-AREA-R101.                                                   
072800*     03  -COPY WDGX4433                                                  
072901                                                                          
073001 01  FILLER               PIC X(16)   VALUE 'WDR130 AREA'.                
073101 01   DLI-IO-AREA-R130.                                                   
073201*     03  -COPY WDGX4434                                                  
073300                                                                          
073400     EJECT                                                                
073500*  ---COPYTEXT TILL RY9-TRANS                                             
073600     SKIP2                                                                
073700*01    -COPY WDGZRY9                                                      
073800                                                                          
073900     EJECT                                                                
074000*01    -COPY WDGZRY9S                                                     
074100                                                                          
074200     EJECT                                                                
074300                                                                          
074400 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDQ201'.         
074500 01    DLI-IO-WDQ201.                                                     
074600*  03    WDQ201 -COPY WDQ201 -PRE ORQI-                                   
074700     EJECT                                                                
074800                                                                          
074900 01  FILLER                    PIC X(16) VALUE 'DLI-IO-ISRT-Q101'.        
075000 01    DLI-IO-ISRT-Q101.                                                  
075100*  03    WDQ101 -COPY WDQ101 -PRE ORQM-                                   
075200                                                                          
075300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK601'.         
075400 01  DLI-IO-WDK601.                                                       
075500*  03    WDK601 -COPY WDK601                                              
075600                                                                          
075701 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDD901'.         
075801 01  DLI-IO-WDD901.                                                       
075901*  03  WDD901 -COPY WDD901.                                               
076000     EJECT                                                                
076101 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDD902'.         
076201 01  DLI-IO-WDD902.                                                       
076301*  03  WDD902 -COPY WDD902                                                
076401     EJECT                                                                
076501 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDD924'.         
076601 01  DLI-IO-WDD924.                                                       
076701*  03  WDD924 -COPY WDD924                                                
076801     EJECT                                                                
076900                                                                          
077000*    --- AREOR FÖR KOMMUNIKATION                                          
077100*                                                                         
077200 01  FILLER                      PIC X(16)  VALUE 'SEND-CONTROL'.         
077300*   -COPY WZ01SEND                                                        
077400                                                                          
077500 01  FILLER                      PIC X(16)  VALUE 'HDR-AREA'.             
077600                                                                          
077700 01  HDR-AREA.                                                            
077800*    03 -COPY WZ01REQU   -PRE HDR-                                        
077900*    03 -COPY WZ04HDR                                                     
078000     EJECT                                                                
078100                                                                          
078200 01  FILLER                    PIC X(16) VALUE 'DLI-IO-2232  '.           
078300 01    DLI-IO-2232.                                                       
078400*  03    WDGX2232 -COPY WDGX2232 -PRE XXBX-                               
078500                                                                          
078600 01  FILLER                     PIC X(16) VALUE 'MSG-KOM-AREA'.           
078700*01  -COPY WMSGKOM                                                        
078800     EJECT                                                                
078900                                                                          
079000 01  P-TO-P-SW.                                                           
079100   03  P-TO-P-KVLL           PIC S9(4)   COMP SYNC.                       
079200   03  P-TO-P-KDZ1           PIC X(1)    VALUE LOW-VALUE.                 
079300   03  P-TO-P-KDZ2           PIC X(1)    VALUE LOW-VALUE.                 
079400   03  P-TO-P-KDTRANS        PIC X(8).                                    
079500   03  P-TO-P-IDTRANS        PIC X(4).                                    
079600   03  P-TO-P-KDMFSFOR       PIC X(1).                                    
079700   03  P-TO-P-DATA           PIC X(1000).                                 
079800     EJECT                                                                
079900*                                                                         
080000*    --- AREOR FÖR W006KOM SUBMODUL                                       
080100*                                                                         
080200 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
080300 01  KOM-IO-AREA.                                                         
080400   03  KOM-AREA                     PIC X(2500) VALUE SPACE.              
080500*03  FILLER  -COPY W4I25101 -PRE OHUV-  -RED KOM-AREA.                    
080600     EJECT                                                                
080700*03  FILLER  -COPY W4I25201 -PRE ORAD-  -RED KOM-AREA.                    
080800     EJECT                                                                
080900                                                                          
081000                                                                          
081100*    MSG-AREA FÖR HOPP TILL W20109                                        
081200 01  FILLER            PIC X(16)   VALUE '2109-MSG-IO-AREA'.              
081300 01  W-PROG-TO-PROG-SW-1.                                                 
081400     03  2109-KVLL                 PIC S9(4) COMP SYNC.                   
081500     03  2109-Z1                   PIC X.                                 
081600     03  2109-Z2                   PIC X.                                 
081700     03  2109-TRANSKOD             PIC X(8)  VALUE 'W2T109X '.            
081800     03  2109-IDTRANS              PIC X(4)  VALUE '4572'.                
081900     03  2109-KDMFSFOR             PIC X.                                 
082000*    03  -COPY W2I10902    -PRE 2109-                                     
082100                                                                          
082200 LINKAGE SECTION.                                                         
082300*01  -COPY W0009  -PRE MSG-                                               
082400*01  -COPY W0009  -PRE DISTRDOC-                                          
082500*01  -COPY W0009  -PRE 2109-                                              
082600*01  -COPY W0009  -PRE ALT-                                               
082700*01  -COPY W0009  -PRE KOMA-                                              
082800*01  -COPY W0008  -PRE WDP7-                                              
082900     05  FILLER                  PIC X.                                   
083000*01  -COPY W0008  -PRE WDA5-                                              
083100     05  FILLER                  PIC X.                                   
083200*01  -COPY W0008  -PRE WDA5A-                                             
083300     05  FILLER                  PIC X.                                   
083400*01  -COPY W0008  -PRE WDB2-                                              
083500     05  FILLER                  PIC X.                                   
083600*01  -COPY W0008  -PRE WDB2C-                                             
083700     05  FILLER                  PIC X.                                   
083800*01  -COPY W0008  -PRE WDR5-                                              
083900     05  FILLER                  PIC X.                                   
084000*01  -COPY W0008  -PRE WDK6-                                              
084100     05  FILLER                  PIC X.                                   
084200*01  -COPY W0008  -PRE WDB6-                                              
084300     05  FILLER                  PIC X.                                   
084400*01  -COPY W0008  -PRE ART-                                               
084500     05  FILLER                  PIC X.                                   
084600*01  -COPY W0008  -PRE ORDP-                                              
084700     05  FILLER                  PIC X.                                   
084800*01  -COPY W0008  -PRE XXBX-                                              
084900     05  FILLER                  PIC X.                                   
085000*01  -COPY W0008  -PRE XXBU-                                              
085100     05  FILLER                  PIC X.                                   
085200*01  -COPY W0008  -PRE ARTM-                                              
085300     05  FILLER                  PIC X.                                   
085400*01  -COPY W0008  -PRE ORQI-                                              
085500     05  FILLER                  PIC X.                                   
085600*01  -COPY W0008  -PRE ORQM-                                              
085700     05  FILLER                  PIC X.                                   
085800*01  -COPY W0008  -PRE ZZAC-                                              
085900     05  FILLER                  PIC X.                                   
086000*01  -COPY W0008  -PRE BENA-                                              
086100     05  FILLER                  PIC X.                                   
086200*01  -COPY W0008  -PRE WDK7-                                              
086301     05  FILLER                  PIC X.                                   
086401*01  -COPY W0008  -PRE WDD9-                                              
086501     05  FILLER                  PIC X.                                   
086601*01  -COPY W0008  -PRE WDB5-                                              
086701     05  FILLER                  PIC X.                                   
086801*01  -COPY W0008  -PRE WDR1-                                              
086901     05  FILLER                  PIC X.                                   
087001     EJECT                                                                
087100 01  ORDN-XXKP-PCB               PIC X.                                   
087200 01  ORDN-ORQL-PCB               PIC X.                                   
087300 01  ORDN-PROC-PCB               PIC X.                                   
087400 01  ORDN-ORQI-PCB               PIC X.                                   
087500                                                                          
087600 01  KREG-GMTA-PCB               PIC X.                                   
087701                                                                          
087801*01  -COPY W0008 -PRE KREG-GMTB-                                          
087900     05  FILLER                  PIC X.                                   
088001                                                                          
088100 01  KREG-GMTC-PCB               PIC X.                                   
088201                                                                          
088300 01  KREG-BETC-PCB               PIC X.                                   
088400                                                                          
088501                                                                          
088600 PROCEDURE DIVISION  USING MSG-PCB   DISTRDOC-PCB                         
088700                           2109-PCB  ALT-PCB    KOMA-PCB                  
088800                           WDP7-PCB  WDA5-PCB   WDA5A-PCB                 
088900                           WDB2-PCB  WDB2C-PCB  WDR5-PCB                  
089000                           WDK6-PCB  WDB6-PCB   ART-PCB                   
089100                           ORDP-PCB  XXBX-PCB   XXBU-PCB                  
089200                           ARTM-PCB  ORQI-PCB   ORQM-PCB                  
089300                           ZZAC-PCB  BENA-PCB   WDK7-PCB                  
089401                           WDD9-PCB  WDB5-PCB   WDR1-PCB                  
089500                           ORDN-XXKP-PCB ORDN-ORQL-PCB                    
089600                           ORDN-PROC-PCB ORDN-ORQI-PCB                    
089700                           KREG-GMTA-PCB KREG-GMTB-PCB                    
089800                           KREG-GMTC-PCB KREG-BETC-PCB.                   
089901                                                                          
090000 MAIN SECTION.                                                            
090100     ENTRY 'DLITCBL' USING MSG-PCB   DISTRDOC-PCB                         
090200                           2109-PCB  ALT-PCB    KOMA-PCB                  
090300                           WDP7-PCB  WDA5-PCB   WDA5A-PCB                 
090400                           WDB2-PCB  WDB2C-PCB  WDR5-PCB                  
090500                           WDK6-PCB  WDB6-PCB   ART-PCB                   
090600                           ORDP-PCB  XXBX-PCB   XXBU-PCB                  
090700                           ARTM-PCB  ORQI-PCB   ORQM-PCB                  
090800                           ZZAC-PCB  BENA-PCB   WDK7-PCB                  
090901                           WDD9-PCB  WDB5-PCB   WDR1-PCB                  
091000                           ORDN-XXKP-PCB ORDN-ORQL-PCB                    
091100                           ORDN-PROC-PCB ORDN-ORQI-PCB                    
091200                           KREG-GMTA-PCB KREG-GMTB-PCB                    
091300                           KREG-GMTC-PCB KREG-BETC-PCB.                   
091400                                                                          
091500     PERFORM IMS-GET-MSG                                                  
091600     IF SEGMENT-FINNS                                                     
091750        PERFORM A-INIT                                                    
091800        PERFORM B-KONTROLLERA-NYCKLAR                                     
091900        IF NYCKLAR-OK                                                     
092000           IF MFS-UPDATE                                                  
092100              PERFORM C-KOLLA-INPUT                                       
092200              IF INDATA-OK                                                
092300                 PERFORM D-UPPDATERA                                      
092400                 PERFORM G-SAME-PAGE                                      
092500                 PERFORM H-READ-SHOW-INFO                                 
092600              END-IF                                                      
092700           ELSE                                                           
092800              IF MFS-FIRST                                                
092900                 PERFORM E-FIRST-PAGE                                     
093000              ELSE                                                        
093100                 IF MFS-NEXT                                              
093210                    PERFORM F-NEXT-PAGE                                   
093300                 ELSE                                                     
093400                    PERFORM G-SAME-PAGE                                   
093500                 END-IF                                                   
093600              END-IF                                                      
093700              PERFORM H-READ-SHOW-INFO                                    
093800           END-IF                                                         
093900                                                                          
093902         MOVE 1 TO INDX                                                   
093910         PERFORM UNTIL INDX > MAX-INDX                                    
093921         ADD 1 TO INDX                                                    
093930         END-PERFORM                                                      
094000        END-IF                                                            
094100        PERFORM IMS-INSERT-MSG                                            
094200     END-IF                                                               
094202         MOVE 1 TO INDX                                                   
094210         PERFORM UNTIL INDX > MAX-INDX                                    
094222         ADD 1 TO INDX                                                    
094230         END-PERFORM                                                      
094300                                                                          
094400     MOVE ZERO TO RETURN-CODE                                             
094500     GOBACK                                                               
094600     .                                                                    
094700     EJECT                                                                
094800 A-INIT SECTION.                                                          
094900     MOVE 'A-INIT          ' TO CURR-SECTION                              
095000                                                                          
095100     MOVE FUNCTION CURRENT-DATE(1:8) TO CURR-DATE                         
095200     ACCEPT CURR-TIME FROM TIME                                           
095300     ACCEPT DAGENS-DATUM             FROM DATE                            
095400                                                                          
095500     IF MSG-DUBBLA-TRANSKODER                                             
095600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I57801                 
095700       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
095800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
095900     ELSE                                                                 
096000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I57801                  
096100       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
096200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
096300     END-IF                                                               
096400                                                                          
096500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
096600     MOVE MSG-IDPFK TO MFS-IDPFK                                          
096700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
096800                                                                          
096900     MOVE LOW-VALUE TO MSG-AREA                                           
097000     MOVE 'W4O578N1' TO MFS-IDMOD                                         
097100     MOVE '4578' TO MOD-IDTRANS                                           
097200     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
097300                                                                          
097400     IF EGEN-MID OR HELP-MID                                              
097500       CONTINUE                                                           
097600     ELSE                                                                 
097700       MOVE SPACE TO MFS-KDTRTYP                                          
097800       MOVE '7' TO MFS-IDPFK                                              
097900     END-IF                                                               
098000                                                                          
098100     COMPUTE MSG-KVLL = LENGTH OF MOD-W4O57801 + 4                        
098200                                                                          
098300     ACCEPT WS-DAT     FROM DATE                                          
098400     ACCEPT WS-TID     FROM TIME                                          
098500                                                                          
098600     MOVE "AAMMDD"       TO DAT-KDDATFORM                                 
098700     MOVE WS-DAT         TO DAT-I-TIDATUM                                 
098800     CALL WDATKONV USING    DAT-KDDATFORM,                                
098900                            DAT-I-TIDATUM,                                
099000                            DAT-O-TIDATUM,                                
099100                            DAT-KDSVAR                                    
099200                                                                          
099300     IF DAT-KDSVAR-OK                                                     
099400        MOVE DAT-TIAAVVD    TO WS-CURR-AAVVD                              
099500        MOVE DAT-TIAAMMDD   TO WS-CURR-AAMMDD                             
099600        MOVE WS-CURR-AAMMDD TO WS-TIREPDAT                                
099700     END-IF                                                               
099800                                                                          
099900     MOVE 1 TO INDX                                                       
100000     PERFORM UNTIL INDX > MAX-INDX                                        
100100        MOVE SPACE TO WS-HELP-RAD(INDX)                                   
100200        ADD 1 TO INDX                                                     
100300     END-PERFORM                                                          
100401                                                                          
100501                                                                          
100601     IF MFS-SPLIT                                                         
100701        MOVE 'CO TIM'             TO MOD-RUBVAR                           
100801     ELSE                                                                 
100901        IF MFS-FIRST                                                      
101001           MOVE 'LDCRFS'          TO MOD-RUBVAR                           
101101        ELSE                                                              
101201           IF MID-RUBVAR = 'CUT OFF TIME'                                 
101301              MOVE 'CO TIM'        TO MOD-RUBVAR                          
101401           ELSE                                                           
101500              MOVE 'LDCRFS'       TO MOD-RUBVAR                           
101601           END-IF                                                         
101701        END-IF                                                            
101801     END-IF                                                               
101901     .                                                                    
102000     EJECT                                                                
102100 B-KONTROLLERA-NYCKLAR     SECTION.                                       
102200     MOVE 'B-KONTROLLERA-NY' TO CURR-SECTION                              
102300                                                                          
102400     MOVE ALL '+' TO MSGI-WMSGINIT                                        
102500     MOVE '001' TO MSGI-KDCALL                                            
102600     MOVE MSG-LTERM-NAME TO MSGI-IDLTERM-USER                             
102700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
102800     MOVE '4578' TO MSGI-IDTRANS                                          
102900     IF EGEN-MID                                                          
103010                                                                          
103100        MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                              
103200        MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                             
103300        MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                              
103310        MOVE MID-KDFARLIG-IN TO MSGI-KDFARLIG                             
103320        MOVE MID-TIRFSDAT-CDC-IN TO MSGI-TIRFSDAT-CDC                     
103400     END-IF                                                               
103500     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
103600     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
103700       MOVE 'S  '  TO MED-IDSKYLT                                         
103800       MOVE +1     TO SPRAK-IX                                            
103900     ELSE                                                                 
104000       MOVE 'GB '  TO MED-IDSKYLT                                         
104100       MOVE +2     TO SPRAK-IX                                            
104200     END-IF                                                               
104300     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
104400     IF SPAR-IDTRANS NOT = '4578'                                         
104500        MOVE '4578'    TO SPAR-IDTRANS                                    
104600        MOVE SPACE     TO SPAR-URVAL                                      
104700        MOVE ZERO      TO SPAR-IDDISTR-ENTER                              
104800        MOVE ZERO      TO SPAR-IDKUNDNR-ENTER                             
104900        MOVE SPACE     TO SPAR-IDKUNDRF-ENTER                             
105000        MOVE ZERO      TO SPAR-IDARTNR-ENTER                              
105100        MOVE ZERO      TO SPAR-IDLOPNR-ENTER                              
105200        MOVE SPACE     TO SPAR-IDDC-ENTER                                 
105300        MOVE ZERO      TO SPAR-KDRAPRIO-ENTER                             
105400        MOVE ZERO      TO SPAR-DARODAT-ENTER                              
105501        MOVE ZERO      TO SPAR-TIREGTID-ENTER                             
105502        MOVE ZERO      TO SPAR-TIRFSDAT-CDC-ENTER                         
105503        MOVE SPACE     TO SPAR-KDFARLIG-ENTER                             
105600        MOVE ZERO      TO SPAR-IDDISTR-NEXT                               
105700        MOVE ZERO      TO SPAR-IDKUNDNR-NEXT                              
105800        MOVE SPACE     TO SPAR-IDKUNDRF-NEXT                              
105900        MOVE ZERO      TO SPAR-IDARTNR-NEXT                               
106000        MOVE ZERO      TO SPAR-IDLOPNR-NEXT                               
106100        MOVE SPACE     TO SPAR-IDDC-NEXT                                  
106200        MOVE ZERO      TO SPAR-KDRAPRIO-NEXT                              
106300        MOVE ZERO      TO SPAR-DARODAT-NEXT                               
106401        MOVE ZERO      TO SPAR-TIREGTID-NEXT                              
106402        MOVE ZERO      TO SPAR-TIRFSDAT-CDC-NEXT                          
106403        MOVE SPACE     TO SPAR-KDFARLIG-NEXT                              
106404     ELSE                                                                 
106405        IF SPAR-IDDISTR-ENTER      NOT NUMERIC                            
106406           MOVE ZERO      TO SPAR-IDDISTR-ENTER                           
106407        END-IF                                                            
106408        IF SPAR-IDKUNDNR-ENTER     NOT NUMERIC                            
106410           MOVE ZERO      TO SPAR-IDKUNDNR-ENTER                          
106411        END-IF                                                            
106412        IF SPAR-IDARTNR-ENTER      NOT NUMERIC                            
106413           MOVE ZERO      TO SPAR-IDARTNR-ENTER                           
106414        END-IF                                                            
106415        IF SPAR-IDLOPNR-ENTER      NOT NUMERIC                            
106416           MOVE ZERO      TO SPAR-IDLOPNR-ENTER                           
106417        END-IF                                                            
106418        IF SPAR-KDRAPRIO-ENTER     NOT NUMERIC                            
106420           MOVE ZERO      TO SPAR-KDRAPRIO-ENTER                          
106421        END-IF                                                            
106422        IF SPAR-DARODAT-ENTER      NOT NUMERIC                            
106430           MOVE ZERO      TO SPAR-DARODAT-ENTER                           
106431        END-IF                                                            
106432        IF SPAR-TIREGTID-ENTER     NOT NUMERIC                            
106440           MOVE ZERO      TO SPAR-TIREGTID-ENTER                          
106441        END-IF                                                            
106442        IF SPAR-TIRFSDAT-CDC-ENTER NOT NUMERIC                            
106450           MOVE ZERO      TO SPAR-TIRFSDAT-CDC-ENTER                      
106460        END-IF                                                            
106461        IF SPAR-IDDISTR-NEXT       NOT NUMERIC                            
106470           MOVE ZERO      TO SPAR-IDDISTR-NEXT                            
106471        END-IF                                                            
106472        IF SPAR-IDKUNDNR-NEXT      NOT NUMERIC                            
106480           MOVE ZERO      TO SPAR-IDKUNDNR-NEXT                           
106490        END-IF                                                            
106491        IF SPAR-IDARTNR-NEXT       NOT NUMERIC                            
106492           MOVE ZERO      TO SPAR-IDARTNR-NEXT                            
106493        END-IF                                                            
106494        IF SPAR-IDLOPNR-NEXT       NOT NUMERIC                            
106495           MOVE ZERO      TO SPAR-IDLOPNR-NEXT                            
106496        END-IF                                                            
106497        IF SPAR-KDRAPRIO-NEXT      NOT NUMERIC                            
106498           MOVE ZERO      TO SPAR-KDRAPRIO-NEXT                           
106499        END-IF                                                            
106500        IF SPAR-DARODAT-NEXT       NOT NUMERIC                            
106501           MOVE ZERO      TO SPAR-DARODAT-NEXT                            
106502        END-IF                                                            
106503        IF SPAR-TIREGTID-NEXT      NOT NUMERIC                            
106504           MOVE ZERO      TO SPAR-TIREGTID-NEXT                           
106505        END-IF                                                            
106506        IF SPAR-TIRFSDAT-CDC-NEXT  NOT NUMERIC                            
106507           MOVE ZERO      TO SPAR-TIRFSDAT-CDC-NEXT                       
106508        END-IF                                                            
106510     END-IF                                                               
106600                                                                          
106700     MOVE JA TO NYCKLAR-SW                                                
106800                                                                          
106900     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
107000     IF MID-IDDISTR-IN NOT = ALL '+'                                      
107100       MOVE '7' TO MFS-IDPFK                                              
107200       MOVE SPACE TO MFS-KDTRTYP                                          
107301       MOVE 'LDCRFS'       TO MOD-RUBVAR                                  
107400     END-IF                                                               
107500                                                                          
107600     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
107700     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
107800       MOVE '7' TO MFS-IDPFK                                              
107900       MOVE SPACE TO MFS-KDTRTYP                                          
108001       MOVE 'LDCRFS'       TO MOD-RUBVAR                                  
108100     END-IF                                                               
108200                                                                          
108300     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
108400     IF MID-IDARTNR-IN NOT = ALL '+'                                      
108500       MOVE '7' TO MFS-IDPFK                                              
108600       MOVE SPACE TO MFS-KDTRTYP                                          
108701       MOVE 'LDCRFS'      TO MOD-RUBVAR                                   
108800     END-IF                                                               
108900                                                                          
109000     MOVE MFS-RENSA-FAELT TO MOD-KDFARLIG-IN                              
109100     IF MID-KDFARLIG-IN NOT = ALL '+'                                     
109200       MOVE '7' TO MFS-IDPFK                                              
109300       MOVE SPACE TO MFS-KDTRTYP                                          
109401       MOVE 'LDCRFS'       TO MOD-RUBVAR                                  
109500     END-IF                                                               
109600                                                                          
109610     MOVE MFS-RENSA-FAELT TO MOD-TIRFSDAT-CDC-IN                          
109620     IF MID-TIRFSDAT-CDC-IN NOT = ALL '+'                                 
109630       MOVE '7' TO MFS-IDPFK                                              
109640       MOVE SPACE TO MFS-KDTRTYP                                          
109650       MOVE 'LDCRFS'       TO MOD-RUBVAR                                  
109660     END-IF                                                               
109670                                                                          
109700     IF MID-IDDISTR-IN = ALL '+' AND                                      
109800        MID-IDKUNDNR-IN = ALL '+' AND                                     
109900        MID-IDARTNR-IN  = ALL '+' AND                                     
109910        MID-TIRFSDAT-CDC-IN   = ALL '+' AND                               
110000        MID-KDFARLIG-IN = ALL '+'                                         
110100                                                                          
110200        MOVE SPAR-URVAL TO WS-SELECTION                                   
110300        IF WS-SELECTION = SPACE                                           
110400           MOVE NEJ TO NYCKLAR-SW                                         
110500        END-IF                                                            
110600                                                                          
110700        IF SHOW-DISTRICT                                                  
110800           MOVE ZERO  TO MSGI-IDKUNDNR                                    
110900                         MSGI-IDARTNR                                     
111000           MOVE SPACE TO MSGI-IDDC-BULK                                   
111020                         MSGI-KDFARLIG                                    
111030                         MSGI-TIRFSDAT-CDC                                
111100        END-IF                                                            
111110        IF SHOW-DISTRICT-PARTNO                                           
111130           MOVE ZERO  TO MSGI-IDKUNDNR                                    
111150           MOVE SPACE TO MSGI-IDDC-BULK                                   
111154                         MSGI-KDFARLIG                                    
111155                         MSGI-TIRFSDAT-CDC                                
111160        END-IF                                                            
111200        IF SHOW-DISTRICT-CUST                                             
111300           MOVE ZERO  TO MSGI-IDARTNR                                     
111400           MOVE SPACE TO MSGI-IDDC-BULK                                   
111420                         MSGI-KDFARLIG                                    
111430                         MSGI-TIRFSDAT-CDC                                
111500        END-IF                                                            
111600        IF SHOW-DISTRICT-CUST-PARTNO                                      
111700           MOVE SPACE TO MSGI-IDDC-BULK                                   
111720                         MSGI-KDFARLIG                                    
111730                         MSGI-TIRFSDAT-CDC                                
111800        END-IF                                                            
111900        IF SHOW-PARTNO                                                    
112000           MOVE ZERO  TO MSGI-IDDISTR                                     
112100                         MSGI-IDKUNDNR                                    
112200           MOVE SPACE TO MSGI-IDDC-BULK                                   
112220                         MSGI-KDFARLIG                                    
112230                         MSGI-TIRFSDAT-CDC                                
112300        END-IF                                                            
112310        IF SHOW-DANGEROUS                                                 
112320           MOVE ZERO  TO MSGI-IDDISTR                                     
112330                         MSGI-IDKUNDNR                                    
112331                         MSGI-IDARTNR                                     
112340           MOVE SPACE TO MSGI-IDDC-BULK                                   
112341                         MSGI-TIRFSDAT-CDC                                
112350        END-IF                                                            
112351                                                                          
112360        IF SHOW-RFSDATE                                                   
112370           MOVE ZERO  TO MSGI-IDDISTR                                     
112380                         MSGI-IDKUNDNR                                    
112390                         MSGI-IDARTNR                                     
112391           MOVE SPACE TO MSGI-IDDC-BULK                                   
112392                         MSGI-KDFARLIG                                    
112394        END-IF                                                            
112395                                                                          
112396        IF SHOW-DISTRICT-RFSDATE                                          
112397           MOVE ZERO  TO MSGI-IDKUNDNR                                    
112398                         MSGI-IDARTNR                                     
112400           MOVE SPACE TO MSGI-IDDC-BULK                                   
112401                         MSGI-KDFARLIG                                    
112402        END-IF                                                            
112403        IF SHOW-DANG-RFSDATE                                              
112408           MOVE ZERO  TO MSGI-IDDISTR                                     
112409                         MSGI-IDKUNDNR                                    
112410                         MSGI-IDARTNR                                     
112411           MOVE SPACE TO MSGI-IDDC-BULK                                   
112412                                                                          
112413        END-IF                                                            
112420                                                                          
112500     ELSE                                                                 
112600        IF MID-IDDISTR-IN = ALL '+'                                       
112700           MOVE ZERO  TO MSGI-IDDISTR                                     
112800        ELSE                                                              
112900           IF MID-IDDISTR-IN NOT NUMERIC                                  
113000              MOVE NEJ TO NYCKLAR-SW                                      
113100           END-IF                                                         
113200        END-IF                                                            
113300        IF MID-IDKUNDNR-IN = ALL '+'                                      
113400           MOVE ZERO  TO MSGI-IDKUNDNR                                    
113500        ELSE                                                              
113600           IF MID-IDKUNDNR-IN NOT NUMERIC                                 
113700              MOVE NEJ TO NYCKLAR-SW                                      
113800           END-IF                                                         
113900        END-IF                                                            
114000        IF MID-IDARTNR-IN  = ALL '+'                                      
114100           MOVE ZERO  TO MSGI-IDARTNR                                     
114200        ELSE                                                              
114300           IF MID-IDARTNR-IN NOT NUMERIC                                  
114400              MOVE NEJ TO NYCKLAR-SW                                      
114500           END-IF                                                         
114600        END-IF                                                            
114700        IF MID-KDFARLIG-IN = ALL '+'                                      
114800           MOVE SPACE TO MSGI-KDFARLIG                                    
114810        ELSE                                                              
114820           IF MID-KDFARLIG-IN = SPACE                                     
114830              MOVE SPACE TO MSGI-KDFARLIG                                 
114840           END-IF                                                         
114900        END-IF                                                            
114914        IF MID-TIRFSDAT-CDC-IN = ALL '+'                                  
114920           MOVE SPACE TO MSGI-TIRFSDAT-CDC                                
114930        END-IF                                                            
115000        PERFORM BA-KOLLA-URVAL                                            
115100        MOVE WS-SELECTION TO SPAR-URVAL                                   
115200     END-IF                                                               
115300                                                                          
115400     IF NYCKLAR-FEL                                                       
115500        MOVE ERR-WRONG-KEY  TO MED-IDMFSFEL                               
115600        CALL WMEDKONV USING MED-WMEDAREA                                  
115700        MOVE MED-MFSFEL     TO MOD-TEMFSFEL                               
115800        PERFORM MFS-RENSA-ALLA-FAELT-UT                                   
115900     ELSE                                                                 
116000        MOVE MSGI-IDDISTR      TO MOD-IDDISTR-UT                          
116100        MOVE MSGI-IDKUNDNR     TO MOD-IDKUNDNR-UT                         
116200        MOVE MSGI-IDARTNR      TO MOD-IDARTNR-UT                          
116300        MOVE MSGI-KDFARLIG     TO MOD-KDFARLIG-UT                         
116320        MOVE MSGI-TIRFSDAT-CDC TO MOD-TIRFSDAT-CDC-UT                     
116400     END-IF                                                               
116500     .                                                                    
116600                                                                          
116700 BA-KOLLA-URVAL SECTION.                                                  
116800     MOVE 'BA-KOLLA-URVAL  ' TO CURR-SECTION                              
116900                                                                          
117000     IF MSGI-IDDISTR  NOT = ZERO   AND                                    
117100        MSGI-IDKUNDNR     = ZERO   AND                                    
117200        MSGI-IDARTNR      = ZERO   AND                                    
117300        MSGI-IDARTNR      = ZERO   AND                                    
117401        MSGI-KDFARLIG     = SPACE  AND                                    
117501        MSGI-TIRFSDAT-CDC = SPACE                                         
117600        MOVE 'D' TO WS-SELECTION                                          
117700     ELSE                                                                 
117800        IF MSGI-IDDISTR   NOT = ZERO     AND                              
117900           MSGI-IDKUNDNR  NOT = ZERO     AND                              
118001           MSGI-IDARTNR       = ZERO     AND                              
118101           MSGI-IDARTNR       = ZERO     AND                              
118201           MSGI-KDFARLIG      = SPACE    AND                              
118301           MSGI-TIRFSDAT-CDC  = SPACE                                     
118401           MOVE 'DC' TO WS-SELECTION                                      
118500        ELSE                                                              
118600           IF MSGI-IDDISTR   NOT = ZERO     AND                           
118700              MSGI-IDKUNDNR      = ZERO     AND                           
118800              MSGI-IDARTNR   NOT = ZERO     AND                           
119001              MSGI-KDFARLIG      = SPACE    AND                           
119100              MSGI-TIRFSDAT-CDC  = SPACE                                  
119201              MOVE 'DP'  TO WS-SELECTION                                  
119300           ELSE                                                           
119400              IF MSGI-IDDISTR       = ZERO     AND                        
119500                 MSGI-IDKUNDNR      = ZERO     AND                        
119600                 MSGI-IDARTNR   NOT = ZERO     AND                        
119710                 MSGI-KDFARLIG      = SPACE    AND                        
119800                 MSGI-TIRFSDAT-CDC  = SPACE                               
119901                 MOVE 'P' TO WS-SELECTION                                 
120000              ELSE                                                        
120200                 IF MSGI-IDDISTR   NOT   = ZERO     AND                   
120300                    MSGI-IDKUNDNR  NOT   = ZERO     AND                   
120310                    MSGI-IDARTNR   NOT   = ZERO     AND                   
120320                    MSGI-KDFARLIG        = SPACE    AND                   
120330                    MSGI-TIRFSDAT-CDC    = SPACE                          
120340                    MOVE 'DCP'  TO WS-SELECTION                           
120370                ELSE                                                      
120380                  IF MSGI-IDDISTR       = ZERO     AND                    
120390                     MSGI-IDKUNDNR      = ZERO     AND                    
120391                     MSGI-IDARTNR       = ZERO     AND                    
120392                     MSGI-KDFARLIG      = SPACE    AND                    
120393                     MSGI-TIRFSDAT-CDC NOT = SPACE                        
120396                     MOVE 'R '   TO WS-SELECTION                          
120397                  ELSE                                                    
120399                    IF MSGI-IDDISTR   NOT  = ZERO     AND                 
120400                     MSGI-IDKUNDNR         = ZERO     AND                 
120401                     MSGI-IDARTNR          = ZERO     AND                 
120402                     MSGI-KDFARLIG         = SPACE    AND                 
120403                     MSGI-TIRFSDAT-CDC NOT = SPACE                        
120405                     MOVE 'DR'   TO WS-SELECTION                          
120406                    ELSE                                                  
120413                       IF  MSGI-IDDISTR    = ZERO     AND                 
120414                        MSGI-IDKUNDNR      = ZERO     AND                 
120415                        MSGI-IDARTNR       = ZERO     AND                 
120416                        MSGI-KDFARLIG  NOT = SPACE    AND                 
120418                        MSGI-TIRFSDAT-CDC  = SPACE                        
120419                        MOVE 'DG'   TO WS-SELECTION                       
120420                       ELSE                                               
120421                         IF  MSGI-IDDISTR       = ZERO     AND            
120422                             MSGI-IDKUNDNR      = ZERO     AND            
120423                             MSGI-IDARTNR       = ZERO     AND            
120424                             MSGI-KDFARLIG  NOT = SPACE    AND            
120426                             MSGI-TIRFSDAT-CDC NOT = SPACE                
120427                             MOVE 'DGR'  TO WS-SELECTION                  
120428                         ELSE                                             
120430                           IF NOT MFS-IDPFK = '8'                         
120500                              MOVE NEJ TO NYCKLAR-SW                      
120600                           END-IF                                         
120601                         END-IF                                           
120602                       END-IF                                             
120603                    END-IF                                                
120604                  END-IF                                                  
120610                END-IF                                                    
120700              END-IF                                                      
120800           END-IF                                                         
120900        END-IF                                                            
121000     END-IF                                                               
121100                                                                          
121200     .                                                                    
121300     EJECT                                                                
121400                                                                          
121500 C-KOLLA-INPUT  SECTION.                                                  
121600     MOVE 'C-KOLLA-INPUT   ' TO CURR-SECTION                              
121700                                                                          
121800     MOVE JA  TO INDATA-SW                                                
121900     MOVE +1 TO INDX                                                      
122000     PERFORM UNTIL INDX > MAX-INDX                                        
122100        IF MID-KDCMD(INDX) = ALL '+' OR SPACE                             
122200          CONTINUE                                                        
122300        ELSE                                                              
122400          IF MID-KDCMD (INDX) = WS-DELETE-PLUS-MAIL                       
122500                             OR WS-NORMAL-TILL-LDC                        
122600                             OR WS-FORBIORDER                             
122700                             OR WS-FORBIORDER-VOR                         
122800                             OR WS-MAIL                                   
122900             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR (INDX)           
123000             IF MID-KDCMD (INDX) = WS-DELETE-PLUS-MAIL                    
123100                IF DIST19-SATS                                            
123200                   MOVE TEXT-0422 (SPRAK-IX) TO MOD-TEMFSFEL              
123300                   MOVE NEJ                  TO INDATA-SW                 
123400                END-IF                                                    
123500             END-IF                                                       
123600             IF MID-KDCMD (INDX) = WS-NORMAL-TILL-LDC                     
123700                                OR WS-FORBIORDER                          
123800                                OR WS-FORBIORDER-VOR                      
123900                                                                          
124000                PERFORM CA-KOLLA-KUNDREG                                  
124100             END-IF                                                       
124200          ELSE                                                            
124300            MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-ATTR (INDX)            
124400            MOVE NEJ TO INDATA-SW                                         
124500          END-IF                                                          
124600        END-IF                                                            
124700        ADD +1 TO INDX                                                    
124800     END-PERFORM                                                          
124900                                                                          
125000     IF INDATA-FEL                                                        
125100        MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                         
125200        CALL WMEDKONV USING MED-WMEDAREA                                  
125300        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
125400        PERFORM MFS-ROER-EJ-MOD-FAELT                                     
125500     END-IF                                                               
125600     .                                                                    
125700     EJECT                                                                
125800                                                                          
125900     EJECT                                                                
126000                                                                          
126100 CA-KOLLA-KUNDREG  SECTION.                                               
126200     MOVE 'CA-KOLLA-KUNDREG' TO CURR-SECTION                              
126300                                                                          
126401     PERFORM CAA-CHECK-NUMERIC                                            
126501                                                                          
126601     IF INDATA-OK                                                         
126700       MOVE SPACE                   TO W-WDA5KEY-X                        
126800       MOVE SPAR-IDDISTR-RAD (INDX) TO W-A5-IDDISTR-KEY                   
126900       MOVE SPAR-IDKUNDNR-RAD(INDX) TO W-A5-IDKUNDNR-KEY                  
127000       MOVE SPAR-IDORDNR-RAD (INDX) TO W-A5-IDORDNR-KEY                   
127100       MOVE SPAR-IDARTNR-RAD (INDX) TO W-A5-IDARTNR-KEY                   
127200       MOVE SPAR-IDLOPNR-RAD (INDX) TO W-A5-IDLOPNR-KEY                   
127300       PERFORM IMS-05-GHU-WDA501                                          
127400                                                                          
127500       MOVE RAD-IDDISTR            TO WS-IDDISTR                          
127600       MOVE WS-IDDISTR             TO KREG-IDDISTR                        
127610                                      W-IDDISTR-DC                        
127620                                      W-IDDISTR-DC-DEF                    
127700       MOVE RAD-IDKUNDNR           TO WS-IDKUNDNR                         
127710                                      W-IDKUNDNR-DC                       
127800       MOVE WS-IDKUNDNR            TO KREG-IDKUNDNR                       
127900       MOVE 'LDCB'                 TO KREG-IDSYSTEM                       
128600                                                                          
128700       IF MID-KDORDKL(INDX) NOT = ALL '+'                                 
128800          MOVE MID-KDORDKL(INDX)   TO KREG-KDORDKL                        
128900       ELSE                                                               
129000          MOVE '3'                 TO KREG-KDORDKL                        
129100       END-IF                                                             
129200                                                                          
129300       IF MID-IDDC-PRIM(INDX) NOT = ALL '+'                               
129400          MOVE MID-IDDC-PRIM(INDX) TO KREG-IDDC-TVS                       
129410                                      WS-IDDC-GMT                         
129500       ELSE                                                               
129600          MOVE GMT-IDDC-BULK(1)    TO KREG-IDDC-TVS                       
129610                                      WS-IDDC-GMT                         
129700       END-IF                                                             
129800                                                                          
129810       IF WS-IDDC-GMT NOT = SPACE                                         
129812          MOVE RAD-IDDISTR  TO W-IDDISTR-WDB2                             
129813          MOVE RAD-IDKUNDNR TO W-IDKUNDNR-WDB2                            
129816          PERFORM IMS-07-GU-WDB201                                        
129817          PERFORM S06-GET-KDFRAKT                                         
129818       END-IF                                                             
129819                                                                          
129820       MOVE WS-KDFRAKT             TO KREG-KDFRAKT-IN                     
129830                                                                          
129900       MOVE SPACE                   TO KREG-KDFAKTYP-IN                   
130000       MOVE NEJ                     TO KREG-FLVORKO                       
130100                                       KREG-FLVORFK                       
130200                                                                          
130300       CALL W411KREG USING KREG-W411KREG                                  
130401            KREG-GMTA-PCB KREG-GMTB-PCB                                   
130500            KREG-GMTC-PCB KREG-BETC-PCB                                   
130600                                                                          
130700       IF KREG-IDDC-OK     = NEJ OR                                       
130800          KREG-KDFRAKT-OK  = NEJ                                          
130900                                                                          
131000          MOVE MFS-NUM-FAELT-FEL      TO MOD-KDORDKL-ATTR  (INDX)         
131200          MOVE TEXT-0409 (SPRAK-IX) TO MOD-TEMFSFEL                       
131300          MOVE NEJ                  TO INDATA-SW                          
131400       END-IF                                                             
131501     END-IF                                                               
131600                                                                          
131700     .                                                                    
131800                                                                          
131901 CAA-CHECK-NUMERIC SECTION.                                               
132001     MOVE 'CAA-CHECK-NUMER ' TO CURR-SECTION                              
132101                                                                          
132901     IF MID-KDORDKL(INDX) NOT = ALL '+'                                   
133001       IF MID-KDORDKL(INDX) NOT NUMERIC                                   
133101          MOVE MFS-NUM-FAELT-FEL      TO MOD-KDORDKL-ATTR  (INDX)         
133201          MOVE NEJ TO INDATA-SW                                           
133301       END-IF                                                             
133401     END-IF                                                               
133501     .                                                                    
133601                                                                          
133700 D-UPPDATERA    SECTION.                                                  
133800     MOVE 'D-UPPDATERA     ' TO CURR-SECTION                              
133900                                                                          
134000     MOVE +1 TO INDX                                                      
134100     PERFORM UNTIL INDX > MAX-INDX                                        
134200        IF MID-KDCMD (INDX) = WS-DELETE-PLUS-MAIL                         
134301           PERFORM DA-SKICKA-MAIL                                         
134400           PERFORM DB-DELETE-ORDER                                        
134500        END-IF                                                            
134600                                                                          
134700        IF MID-KDCMD (INDX) = WS-NORMAL-TILL-LDC                          
134800        OR MID-KDCMD (INDX) = WS-FORBIORDER                               
134900        OR MID-KDCMD (INDX) = WS-FORBIORDER-VOR                           
135000           PERFORM DC-SKAPA-ORDER                                         
135100        END-IF                                                            
135200                                                                          
135300        IF INDATA-OK                                                      
135400        IF MID-KDCMD (INDX) = WS-MAIL                                     
135500           PERFORM DA-SKICKA-MAIL                                         
135600        END-IF                                                            
135700                                                                          
135800        PERFORM DD-UPPDATERA-NOTERING                                     
135900        END-IF                                                            
136000                                                                          
136100        ADD +1 TO INDX                                                    
136200     END-PERFORM                                                          
136300                                                                          
136400     PERFORM DE-JUSTERA-SPARAREA                                          
136500                                                                          
136600     MOVE INF-UPDATE-DONE      TO MED-IDMFSINF                            
136700     CALL WMEDKONV USING MED-WMEDAREA                                     
136800     MOVE MED-MFSINF           TO MOD-TEMFSINF                            
136900     .                                                                    
137000     EJECT                                                                
137100                                                                          
137200 DA-SKICKA-MAIL SECTION.                                                  
137300     MOVE 'DA-SKICKA-MAIL  ' TO CURR-SECTION                              
137400                                                                          
137500     MOVE SPACE                   TO W-WDA5KEY-X                          
137600     MOVE SPAR-IDDISTR-RAD (INDX) TO W-A5-IDDISTR-KEY                     
137700                                     TEST-IDDISTR                         
137800     MOVE SPAR-IDKUNDNR-RAD(INDX) TO W-A5-IDKUNDNR-KEY                    
137900     MOVE SPAR-IDORDNR-RAD (INDX) TO W-A5-IDORDNR-KEY                     
138000     MOVE SPAR-IDARTNR-RAD (INDX) TO W-A5-IDARTNR-KEY                     
138100     MOVE SPAR-IDLOPNR-RAD (INDX) TO W-A5-IDLOPNR-KEY                     
138200                                                                          
138300     PERFORM IMS-04-GU-WDA501                                             
138400     IF SEGMENT-FINNS                                                     
138500        IF WZ04-SEND-IDCOM = ZERO                                         
138600           PERFORM S05-SEND-OPEN                                          
138700           MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                             
138800        END-IF                                                            
138901                                                                          
139000        IF RAD-IDDISTR  NOT = W-IDDISTR-WDB2 OR                           
139100           RAD-IDKUNDNR NOT = W-IDKUNDNR-WDB2                             
139200                                                                          
139300           MOVE RAD-IDDISTR  TO W-IDDISTR-WDB2                            
139400           MOVE RAD-IDKUNDNR TO W-IDKUNDNR-WDB2                           
139500           PERFORM IMS-07-GU-WDB201                                       
139600        END-IF                                                            
139701                                                                          
139801        MOVE RAD-IDDISTR            TO WS-IDDISTR                         
139901        MOVE RAD-IDKUNDNR           TO WS-IDKUNDNR                        
140000                                                                          
140101        MOVE 001              TO HDR-REQU-IDMSGVER                        
140200        MOVE SPACE            TO HDR-REQU-KDPGMACT                        
140300        MOVE MSGI-IDUSER      TO HDR-REQU-IDUSER                          
140400        MOVE 'BO-RELEASE-DOC' TO HDR-IDOUTTYPE                            
140500        MOVE SPACE            TO HDR-IDOUTREC                             
140601        MOVE WS-IDDISTR       TO HDR-IDOUTREC(1:4)                        
140701        MOVE WS-IDKUNDNR      TO HDR-IDOUTREC(5:6)                        
140801        MOVE 'W40578'         TO HDR-IDLIST                               
140900        PERFORM S05-PUT-HEADER                                            
141001        PERFORM S05-PUT-MAIL-LINE-AREA1                                   
141100                                                                          
141200        MOVE 'District no.  ' TO DAP-MAIL-LINE-TEXT-4                     
141301        MOVE 'Customer  '     TO DAP-MAIL-LINE-TEXT-5                     
141401        MOVE 'Part no.  '     TO DAP-MAIL-LINE-TEXT-6                     
141501        MOVE 'Part description           '                                
141601                              TO DAP-MAIL-LINE-TEXT-7                     
141701        MOVE 'Quantity  '     TO DAP-MAIL-LINE-TEXT-8                     
141801        MOVE 'Order no.  '    TO DAP-MAIL-LINE-TEXT-9                     
141901        MOVE 'Repair Date  '  TO DAP-MAIL-LINE-TEXT-10                    
142001        MOVE 'Work Shop Order  '                                          
142101                              TO DAP-MAIL-LINE-TEXT-11                    
142200        MOVE RAD-IDDISTR                  TO DAP-MAIL-DISTRICT-NO         
142300                                             TEST-IDDISTR                 
142400        MOVE RAD-IDKUNDNR                 TO DAP-MAIL-CUSTOMER-NO         
142500        MOVE RAD-IDARTNR                  TO DAP-MAIL-PART-NO             
142600                                             W-IDARTNR                    
142700        IF DIST03-SVERIGE                                                 
142800           MOVE 'S '           TO W-IDSKYLT                               
142900        ELSE                                                              
143000           MOVE 'GB'           TO W-IDSKYLT                               
143100        END-IF                                                            
143200        PERFORM IMS-28-GU-BENA-WDD311                                     
143300        MOVE TEXT-BEART        TO DAP-MAIL-PART-DESC                      
143400        MOVE RAD-KVART         TO DAP-MAIL-QTY                            
143500        MOVE RAD-IDKUNDRF      TO DAP-MAIL-ORDER-NO                       
143600        MOVE RAD-TIREPDAT      TO WS-HELPDAT                              
143700        MOVE WS-HELPDAT        TO DAP-MAIL-REPAIR-DATE                    
143800        MOVE RAD-IDKUNDRF-WIP  TO DAP-MAIL-WRKSHOP-ORDER                  
143901        PERFORM S05-PUT-MAIL-LINE-AREA2                                   
144001        PERFORM S05-PUT-MAIL-LINE-AREA3                                   
144101        PERFORM DAA-REDIGERA-WDD9-INLA                                    
144200        PERFORM S05-PUT-MAIL-LINE-AREA4                                   
144301                                                                          
144400        IF WZ04-SEND-IDCOM > ZERO                                         
144500           PERFORM S05-SEND-CLOSE                                         
144600           MOVE ZERO TO WZ04-SEND-IDCOM                                   
144700        END-IF                                                            
144800     END-IF                                                               
144900     .                                                                    
145000     EJECT                                                                
145100                                                                          
145201 DAA-REDIGERA-WDD9-INLA SECTION.                                          
145301                                                                          
145401     MOVE 999999                             TO W-SPAR-TILEVBSK           
145501                                                                          
145601     MOVE W-IDARTNR                          TO W-IDARTNR-D9              
145701     MOVE RAD-IDDC                           TO W-IDDC-D9                 
145801     PERFORM IMS-42-GU-WDD901                                             
145901     IF SEGMENT-FINNS                                                     
146001       PERFORM IMS-43-GNP-WDD902                                          
146101       PERFORM UNTIL SEGMENT-SAKNAS                                       
146201         MOVE IDLEVNR                        TO W-IDLEVNR                 
146301         PERFORM IMS-44-GNP-WDD924                                        
146401         IF SEGMENT-FINNS                                                 
146801            IF LEV-TILEVBSK-INL < W-SPAR-TILEVBSK AND                     
146901               LEV-FLSENLEV = NEJ                                         
147000                MOVE LEV-TILEVBSK-INL        TO W-SPAR-TILEVBSK           
147101            END-IF                                                        
147201         END-IF                                                           
147301         PERFORM IMS-43-GNP-WDD902                                        
147401       END-PERFORM                                                        
147501     ELSE                                                                 
147601       MOVE MFS-RENSA-FAELT                  TO MOD-IDARTNR(INDX)         
147701     END-IF                                                               
147801                                                                          
147901     IF W-SPAR-TILEVBSK < 999999                                          
148001         MOVE W-SPAR-TILEVBSK                TO DAT-I-TIDATUM             
148101         MOVE 'AAMMDD'                       TO DAT-KDDATFORM             
148201         CALL WDATKONV USING DAT-KDDATFORM,                               
148301                             DAT-I-TIDATUM,                               
148401                             DAT-O-TIDATUM,                               
148501                             DAT-KDSVAR                                   
148601         IF DAT-KDSVAR-OK                                                 
148701            MOVE DAT-TIAAVVD(3:2)       TO DAP-MAIL-LINE-VALUE-WW         
148801            MOVE DAT-TIAAVVD(5:1)       TO DAP-MAIL-LINE-VALUE-D          
148901         END-IF                                                           
149001     END-IF                                                               
149101     .                                                                    
149201     EJECT                                                                
149301 DB-DELETE-ORDER SECTION.                                                 
149400     MOVE 'DB-DELETE-ORDER' TO CURR-SECTION                               
149500                                                                          
149600     PERFORM DBA-KOLLA-TPO-FRYSTID                                        
149700     IF BORTOM-FRYSTID OR                                                 
149800        SEC-KDSVAR = SPACE                                                
149900        PERFORM DBB-DELETE                                                
150000     END-IF                                                               
150100     .                                                                    
150200     EJECT                                                                
150300 DBA-KOLLA-TPO-FRYSTID SECTION.                                           
150400     MOVE 'DBA-KOLLA-TPO-FRYSTID' TO CURR-SECTION                         
150500                                                                          
150600     MOVE SPACE                   TO W-WDA5KEY-X                          
150700     MOVE SPAR-IDDISTR-RAD (INDX) TO W-A5-IDDISTR-KEY                     
150800                                     TEST-IDDISTR                         
150900     MOVE SPAR-IDKUNDNR-RAD(INDX) TO W-A5-IDKUNDNR-KEY                    
151000     MOVE SPAR-IDORDNR-RAD (INDX) TO W-A5-IDORDNR-KEY                     
151100     MOVE SPAR-IDARTNR-RAD (INDX) TO W-A5-IDARTNR-KEY                     
151200     MOVE SPAR-IDLOPNR-RAD (INDX) TO W-A5-IDLOPNR-KEY                     
151300                                                                          
151400     PERFORM IMS-04-GU-WDA501                                             
151500     IF SEGMENT-FINNS                                                     
151600         IF RAD-KDTPOTYP > ZERO AND RAD-FLTPOBEK = JA                     
151700                                AND RAD-KDSTARAD = 1                      
151800           MOVE RAD-TITPO    TO DAT-I-TIDATUM                             
151900           MOVE 'AAMMDD'     TO DAT-KDDATFORM                             
152000           CALL WDATKONV USING DAT-KDDATFORM                              
152100                               DAT-I-TIDATUM                              
152200                               DAT-O-TIDATUM                              
152300                               DAT-KDSVAR                                 
152400                                                                          
152500           IF DAT-KDSVAR-OK                                               
152600             MOVE DAT-TIAAVV-GRP TO W-TITPO-TIAAVV                        
152700           ELSE                                                           
152800             MOVE 'FEL FRÅN PROGRAM W4057800 I SECTION DAA' TO            
152900                                                     FELTEXT              
153000             CALL ABEND USING RKOD-FELLOG                                 
153100           END-IF                                                         
153200                                                                          
153300           MOVE ZERO         TO DAT-I-TIDATUM                             
153400           MOVE 'IDAG  '     TO DAT-KDDATFORM                             
153500           CALL WDATKONV USING DAT-KDDATFORM                              
153600                               DAT-I-TIDATUM                              
153700                               DAT-O-TIDATUM                              
153800                               DAT-KDSVAR                                 
153900                                                                          
154000           IF DAT-KDSVAR-OK                                               
154100             MOVE DAT-TIAAVV-GRP TO W-DAGENS-DAT-TIAAVV                   
154200           ELSE                                                           
154300             MOVE 'FEL FRÅN PROGRAM W4057200 I SECTION DAA' TO            
154400                                                     FELTEXT              
154500             CALL ABEND USING RKOD-FELLOG                                 
154600           END-IF                                                         
154700                                                                          
154800           MOVE RAD-IDARTNR  TO W-IDARTNR                                 
154900           PERFORM IMS-10-GU-WDK611                                       
155000           MOVE CLAG-KVFRYSTI TO VECKO-ANTAL                              
155100           MOVE W-DAGENS-DAT-TIAAVV TO VECKO-DATUM-AAVV                   
155200                                                                          
155300           CALL W009VADD USING VECKO-DATUM-AAVV VECKO-ANTAL               
155400                                                                          
155800           IF W-TITPO-TIAAVV < VECKO-DATUM-AAVV                           
155900             MOVE NEJ TO FRYSTID-SW                                       
156000             IF SEC-KDSVAR NOT = ' '                                      
156100                MOVE MAX-INDX  TO INDX                                    
156200                MOVE ERR-INOM-FRYS TO MED-IDMFSFEL                        
156300                CALL WMEDKONV USING MED-WMEDAREA                          
156400                MOVE MED-MFSFEL TO MOD-TEMFSFEL                           
156500             END-IF                                                       
156600           ELSE                                                           
156700             MOVE JA TO FRYSTID-SW                                        
156800           END-IF                                                         
156900         ELSE                                                             
157000           MOVE JA TO FRYSTID-SW                                          
157100         END-IF                                                           
157200     END-IF                                                               
157300                                                                          
157400     .                                                                    
157500     EJECT                                                                
157600 DBB-DELETE SECTION.                                                      
157700     MOVE 'DBB-DELETE      ' TO CURR-SECTION                              
157800* I DENNA SEKTION DELETAS EN RAD PÅ ROREG. OCH DET DELETADE               
157900* ANTALET DRAS FRÅN AKTUELL ARTIKEL PÅ ARTREG.                            
158000                                                                          
158100     MOVE SPACE                   TO W-WDA5KEY-X                          
158200     MOVE SPACE                   TO W-IDKUNDRF-N2                        
158300     MOVE SPAR-IDDISTR-RAD (INDX) TO W-A5-IDDISTR-KEY                     
158400                                     W-IDDISTR-N2                         
158500     MOVE SPAR-IDKUNDNR-RAD(INDX) TO W-A5-IDKUNDNR-KEY                    
158600                                     W-IDKUNDNR-N2                        
158700     MOVE SPAR-IDORDNR-RAD (INDX) TO W-A5-IDORDNR-KEY                     
158800                                     W-IDORDNR-N2                         
158900     MOVE SPAR-IDARTNR-RAD (INDX) TO W-A5-IDARTNR-KEY                     
159000                                     W-IDARTNR-N2                         
159100     MOVE SPAR-IDLOPNR-RAD (INDX) TO W-A5-IDLOPNR-KEY                     
159200                                     W-IDLOPNR-N2                         
159300                                                                          
159400     PERFORM IMS-14-GHU-RO                                                
159500                                                                          
159600     PERFORM IMS-05-GHU-WDA501                                            
159700                                                                          
159800     MOVE RAD-KVART           TO SPAR-KVART                               
159900     IF RAD-KDSTARAD = '1'                                                
160000       IF RAD-FLTPOBEK = JA                                               
160100         PERFORM DBBA-UPPDATERA-WDK9                                      
160200       END-IF                                                             
160300     ELSE                                                                 
160400        IF DCS-IDDC NOT = RAD-IDDC                                        
160500           MOVE RAD-IDDC TO W-IDDC-B6                                     
160600           PERFORM IMS-11-GU-WDB601                                       
160700        END-IF                                                            
160800        IF  DCS-CDC                                                       
160900            PERFORM S10-MINSKA-KVROS-WDK6                                 
161000        ELSE                                                              
161100            IF DCS-NDC                                                    
161200               PERFORM S11-MINSKA-KVROS-WDK7                              
161300            END-IF                                                        
161400        END-IF                                                            
161500     END-IF                                                               
161600     MOVE JA TO W-UPDATE                                                  
161700     IF RAD-KDTPOTYP = 2 OR 6                                             
161800        PERFORM DBBB-BEHANDLA-LARMKO                                      
161900     END-IF                                                               
162000     PERFORM IMS-15-DLET-RO                                               
162100     PERFORM DBBC-UPPDATERA-WDQ1                                          
162200                                                                          
162300     MOVE ZERO TO RY9-KVART                                               
162400     IF RAD-KDTPOTYP = 6 OR RAD-KDSTARAD = 2                              
162500        MOVE SPAR-KVART   TO RY9-KVART                                    
162600     END-IF                                                               
162700     PERFORM DBBD-SKAPA-RY9-TRANS                                         
162800     PERFORM DBBE-SKRIV-LOGG                                              
162900                                                                          
163000     IF RAD-KDSTARAD = 2 OR RAD-KDTPOTYP = 6                              
163100     OR RAD-KDTPOTYP = 7                                                  
163200        PERFORM DBBF-SKAPA-2109-TRANS                                     
163300     END-IF                                                               
163400     .                                                                    
163500     EJECT                                                                
163600 DBBA-UPPDATERA-WDK9 SECTION.                                             
163700     MOVE 'DBBA-UPPDATERA-WDK9  ' TO CURR-SECTION                         
163800                                                                          
163900     MOVE RAD-IDARTNR TO W-IDARTNR                                        
164000     PERFORM IMS-19-GHU-ARTM-WDK901                                       
164100     SUBTRACT SPAR-KVART FROM ART-SUTPO-TOT                               
164200     PERFORM IMS-20-REPL-ARTM-WDK901                                      
164300     MOVE RAD-TITPO TO DAT-I-TIDATUM                                      
164400     MOVE 'AAMMDD'  TO DAT-KDDATFORM                                      
164500     CALL WDATKONV USING DAT-KDDATFORM                                    
164600                         DAT-I-TIDATUM                                    
164700                         DAT-O-TIDATUM                                    
164800                         DAT-KDSVAR                                       
164900     IF DAT-KDSVAR-OK                                                     
165000       MOVE DAT-TIAAVV-GRP TO W-DABEHOV-AAVV                              
165100       MOVE DAT-TISEKEL    TO W-DABEHOV-SEKEL                             
165200       MOVE W-DABEHOV      TO W-DABEHOV-N                                 
165300     ELSE                                                                 
165400       MOVE 'FEL FRÅN PROGRAM W4057200 I SECTION S01' TO FELTEXT          
165500       CALL ABEND USING RKOD-FELLOG                                       
165600     END-IF                                                               
165700                                                                          
165800     PERFORM IMS-21-GHNP-ARTM-WDK911                                      
165900*  PROD ABEND 000321 - DABEHOV FANNS INTE PÅ K9                           
166000     IF SEGMENT-FINNS                                                     
166100       IF RAD-KDTPOTYP = 1 OR 2                                           
166200         SUBTRACT SPAR-KVART FROM ANT-SUTPO-PB                            
166300       ELSE                                                               
166400         SUBTRACT SPAR-KVART FROM ANT-SUTPO-EJPB                          
166500       END-IF                                                             
166600       IF ANT-SUTPO-PB = ZERO AND ANT-SUTPO-EJPB = ZERO                   
166700         PERFORM IMS-22-DLET-ARTM-WDK9                                    
166800       ELSE                                                               
166900         PERFORM IMS-23-REPL-ARTM-WDK911                                  
167000       END-IF                                                             
167100     END-IF                                                               
167200     .                                                                    
167300     EJECT                                                                
167400 DBBB-BEHANDLA-LARMKO SECTION.                                            
167500     MOVE 'DBBB-BEHANDLA-LARMKO ' TO CURR-SECTION                         
167600                                                                          
167700     MOVE RAD-IDANSK TO W-IDANSK-2232                                     
167800     PERFORM IMS-16-GU-XXBX-WDR220                                        
167900     IF SEGMENT-FINNS                                                     
168000       MOVE XXBX-2232-IDANSK-LARM TO W-IDANSK-2223                        
168100     ELSE                                                                 
168200       MOVE ZERO TO W-IDANSK-2223                                         
168300     END-IF                                                               
168400     MOVE RAD-DASENDAT (3:6) TO W-TISENBEK-DAG                            
168500     MOVE RAD-TISENBEK-KL  TO W-TISENBEK-KL                               
168600     IF RAD-KDTPOTYP = 2                                                  
168700       MOVE 110 TO W-KDLARM                                               
168800     ELSE                                                                 
168900       MOVE 100 TO W-KDLARM                                               
169000     END-IF                                                               
169100     PERFORM IMS-17-GHU-XXBU-WDR5                                         
169200     IF SEGMENT-FINNS                                                     
169300       PERFORM IMS-18-DLET-XXBU-WDR5                                      
169400     END-IF                                                               
169500                                                                          
169600     .                                                                    
169700     EJECT                                                                
169800 DBBC-UPPDATERA-WDQ1 SECTION.                                             
169900     MOVE 'DBBC-UPPDATERA-WDQ1  ' TO CURR-SECTION                         
170000                                                                          
170100     MOVE RAD-IDDISTR  TO W-IDDISTR-N9                                    
170200     MOVE RAD-IDKUNDNR TO W-IDKUNDNR-N9                                   
170300     MOVE '00'         TO W-IDKUNDRF-N9(1:2)                              
170400     MOVE RAD-IDKUNDRF TO W-IDKUNDRF-N9(3:5)                              
170500     PERFORM IMS-24-GU-ORQI01-CSEQ                                        
170600                                                                          
170700     MOVE SPACE                     TO DLI-IO-ISRT-Q101                   
170800     MOVE ORQI-OHUV-IDORDER         TO ORQM-OBKR-IDORDER                  
170900     MOVE RAD-IDARTNR               TO ORQM-OBKR-IDARTNR                  
171000     MOVE +1                        TO ORQM-OBKR-IDLOPNR                  
171100                                       ORQM-OBKR-IDSEKVNR                 
171200     MOVE RAD-IDDC                  TO ORQM-OBKR-IDDC                     
171300     MOVE RAD-IDDC-RO               TO ORQM-OBKR-IDDC-RO                  
171400     MOVE 85                        TO ORQM-OBKR-KDORDBEK                 
171500     MOVE SPACE                     TO ORQM-OBKR-BEERS                    
171600     MOVE SPACE                     TO ORQM-OBKR-IDBIL                    
171700     MOVE RAD-BEKUNDRF              TO ORQM-OBKR-BEKUNDRF                 
171800     MOVE RAD-BERADREF              TO ORQM-OBKR-BERADREF                 
171900     MOVE RAD-BEVOLREF              TO ORQM-OBKR-BEVOLREF                 
172000     MOVE RAD-IDKAMPRF              TO ORQM-OBKR-IDKAMPRF                 
172100     MOVE ZERO                      TO ORQM-OBKR-DIERS-KVOT               
172200     MOVE NEJ                       TO ORQM-OBKR-FLAKPLOC                 
172300     MOVE RAD-FLINVEST              TO ORQM-OBKR-FLINVEST                 
172400     MOVE JA                        TO ORQM-OBKR-FLOBOK                   
172500     MOVE NEJ                       TO ORQM-OBKR-FLOBTRAN                 
172600                                       ORQM-OBKR-FLOBPRT                  
172700     MOVE RAD-FLPRTILL              TO ORQM-OBKR-FLPRTILL                 
172800     MOVE JA                        TO ORQM-OBKR-FLRESTN                  
172900     MOVE NEJ                       TO ORQM-OBKR-FLSLATT                  
173000     MOVE RAD-FLERS                 TO ORQM-OBKR-FLTILLK                  
173100     MOVE ZERO                      TO ORQM-OBKR-IDARTNR-TILLK            
173200     MOVE RAD-IDDISTR               TO ORQM-OBKR-IDDISTR                  
173300     MOVE RAD-IDKUNDNR              TO ORQM-OBKR-IDKUNDNR                 
173400     MOVE '00'                      TO ORQM-OBKR-IDKUNDRF(1:2)            
173500     MOVE RAD-IDORDNR5              TO ORQM-OBKR-IDKUNDRF(3:5)            
173600     MOVE '0000000   '              TO ORQM-OBKR-IDKUNDRF-RO              
173700     MOVE RAD-IDLEVNR               TO ORQM-OBKR-IDLEVNR                  
173800     MOVE RAD-IDLOPNR               TO ORQM-OBKR-IDLOPNR-RO               
173900     MOVE IDPGM                     TO ORQM-OBKR-IDPGM                    
174000     MOVE RAD-IDSYSTEM              TO ORQM-OBKR-IDSYSTEM                 
174100     MOVE RAD-KDDSP                 TO ORQM-OBKR-KDDSP                    
174200     MOVE ZERO                      TO ORQM-OBKR-KDERS                    
174300     MOVE RAD-KDKVBRYT              TO ORQM-OBKR-KDKVBRYT                 
174400     MOVE RAD-KDPRTYP               TO ORQM-OBKR-KDPRTYP                  
174500     MOVE RAD-KDTPOTYP              TO ORQM-OBKR-KDTPOTYP                 
174600     MOVE RAD-KDVRINFO              TO ORQM-OBKR-KDVRINFO                 
174700     MOVE SPAR-KVART                TO ORQM-OBKR-KVANNANT                 
174800     MOVE ZERO                      TO ORQM-OBKR-KVAVBART                 
174900     MOVE RAD-KVART                 TO ORQM-OBKR-KVBEART                  
175000                                       ORQM-OBKR-KVBEART-Q                
175100     MOVE ZERO                      TO ORQM-OBKR-KVBEART-TILLK            
175200                                       ORQM-OBKR-KVPREAVB                 
175300                                       ORQM-OBKR-KVPRERO                  
175400                                       ORQM-OBKR-KVQPACK                  
175500                                       ORQM-OBKR-KVRO                     
175600                                       ORQM-OBKR-KVSLATT                  
175700     MOVE RAD-PRARTNTO              TO ORQM-OBKR-PRARTNTO                 
175800     MOVE RAD-DEAL-PR-LINE          TO ORQM-OBKR-DEAL-PR-LINE             
175900     MOVE ZERO                      TO ORQM-OBKR-PRBPRIS                  
176000     MOVE RAD-REKSIFFR              TO ORQM-OBKR-REKSIFFR                 
176100     MOVE ZERO                      TO ORQM-OBKR-REKSIFFR-TILLK           
176200                                       ORQM-OBKR-RERF-RAD                 
176300                                       ORQM-OBKR-TIDISPIN                 
176400     MOVE ORQI-OHUV-TIREGDAT        TO ORQM-OBKR-TIORDREG                 
176500     MOVE ZERO                      TO ORQM-OBKR-TIPRIS                   
176600                                       ORQM-OBKR-TIRODAT                  
176700     ACCEPT ORQM-OBKR-TIREGDAT FROM DATE                                  
176800     ACCEPT ORQM-OBKR-TIREGTID FROM TIME                                  
176900                                                                          
177000     MOVE MSGI-TILOKDAT TO ORQM-OBKR-TIREGDAT                             
177100     MOVE MSGI-TILOKTID TO ORQM-OBKR-TIREGTID                             
177200                                                                          
177300     MOVE FUNCTION CURRENT-DATE (1:8) TO DATUM-MED-ARHUNDR                
177400     SUBTRACT DATUM-MED-ARHUNDR FROM +999999999 GIVING                    
177500                                     ORQM-OBKR-TITIREGD-9KOMPL            
177600     MOVE RAD-TITPO                 TO ORQM-OBKR-TITPO                    
177700     IF  ORQI-OHUV-TIREGDAT > +500000                                     
177800         ADD +19000000 TO ORQI-OHUV-TIREGDAT GIVING                       
177900                                     DATUM-MED-ARHUNDR                    
178000     ELSE                                                                 
178100         ADD +20000000 TO ORQI-OHUV-TIREGDAT GIVING                       
178200                                     DATUM-MED-ARHUNDR                    
178300     END-IF                                                               
178400     SUBTRACT DATUM-MED-ARHUNDR FROM +999999999 GIVING                    
178500                                     ORQM-OBKR-TITIORDD-9KOMPL            
178600     MOVE RAD-KDFRAKT               TO ORQM-OBKR-KDFRAKT                  
178700     MOVE ORQI-OHUV-KDORDKL         TO ORQM-OBKR-KDORDKL                  
178800                                                                          
178900     MOVE RAD-KDORDTYP-LDC          TO ORQM-OBKR-KDORDTYP-LDC             
179000     MOVE RAD-TIREPDAT              TO ORQM-OBKR-TIREPDAT                 
179100     MOVE RAD-IDKUNDRF-WIP          TO ORQM-OBKR-IDKUNDRF-WIP             
179201     MOVE ZERO                      TO ORQM-OBKR-TIDLEVDAT                
179301     MOVE RAD-PRAVCOST              TO ORQM-OBKR-PRAVCOST                 
179401     MOVE RAD-KDVALISO              TO ORQM-OBKR-KDVALISO                 
179500                                                                          
179600     MOVE ORQI-OHUV-IDORDER      TO   W-IDORDER-MIN-N10                   
179700                                      W-IDORDER-MAX-N10                   
179800     MOVE RAD-IDARTNR            TO   W-IDARTNR-MIN-N10                   
179900                                      W-IDARTNR-MAX-N10                   
180000     MOVE +1                     TO   W-IDLOPNR-MIN-N10                   
180100                                      W-IDLOPNR-MAX-N10                   
180200     MOVE +1                     TO   W-IDSEKVNR-MIN-N10                  
180300                                      W-IDSEKVNR-MAX-N10                  
180400     MOVE RAD-IDDC               TO   W-IDDC-MIN-N10                      
180500                                      W-IDDC-MAX-N10                      
180600     PERFORM IMS-25-GU-ORQM-WDQ1                                          
180700     IF SEGMENT-FINNS                                                     
180800       PERFORM UNTIL SEGMENT-SAKNAS                                       
180900         ADD +1 TO ORQM-OBKR-IDLOPNR                                      
181000                   W-IDLOPNR-MIN-N10                                      
181100                   W-IDLOPNR-MAX-N10                                      
181200         PERFORM IMS-25-GU-ORQM-WDQ1                                      
181300       END-PERFORM                                                        
181400     END-IF                                                               
181500     PERFORM IMS-26-ISRT-ORQM-WDQ1                                        
181600     .                                                                    
181700     EJECT                                                                
181800 DBBD-SKAPA-RY9-TRANS  SECTION.                                           
181900     MOVE 'DBBD-SKAPA-RY9-TRANS ' TO CURR-SECTION                         
182000                                                                          
182100     MOVE 'RY9'            TO RY9-IDPTYP                                  
182200     MOVE RAD-BERADREF     TO RY9-BERADREF                                
182300     MOVE RAD-BEVOLREF     TO RY9-BEVOLREF                                
182400     MOVE RAD-FLERS        TO RY9-FLERS                                   
182500     MOVE RAD-IDDISTR      TO W-IDDISTR-WDB2                              
182600     MOVE RAD-IDKUNDNR     TO W-IDKUNDNR-WDB2                             
182700     PERFORM IMS-07-GU-WDB201                                             
182800     MOVE GMT-FLNC         TO RY9-FLNC                                    
182900     MOVE RAD-IDARTNR      TO RY9-IDARTNR                                 
183000     MOVE ZERO             TO RY9-IDDIVORD                                
183100     MOVE RAD-IDKUNDRF     TO RY9-IDKUNDRF                                
183200     MOVE RAD-IDLOPNR      TO RY9-IDLOPNR                                 
183300     MOVE MSG-LTERM-NAME   TO RY9-IDUSER                                  
183400     MOVE RAD-KDFAKTYP     TO RY9-KDFAKTYP                                
183500     MOVE RAD-KDKVBRYT     TO RY9-KDKVBRYT                                
183600     MOVE RAD-KDORDKL      TO RY9-KDORDKL                                 
183700     MOVE RAD-KDDSP        TO RY9-KDDSP                                   
183800     MOVE RAD-KDRAPRIO     TO RY9-KDRAPRIO                                
183900     MOVE RAD-KDSTARAD     TO RY9-KDSTARAD                                
184000     MOVE RAD-KDTPOTYP     TO RY9-KDTPOTYP                                
184100     MOVE SPACE            TO RY9-KDUART                                  
184200     MOVE RAD-KDVRINFO     TO RY9-KDVRINFO                                
184300     IF RAD-KDTPOTYP = 1 AND RAD-IDSYSTEM = 'VR'                          
184400       MOVE 1              TO RY9-KDVRTPO                                 
184500     END-IF                                                               
184600     IF RAD-KDTPOTYP = 1 AND RAD-IDSYSTEM NOT = 'VR'                      
184700       MOVE 2              TO RY9-KDVRTPO                                 
184800     END-IF                                                               
184900     IF RAD-KDTPOTYP NOT = 1                                              
185000       MOVE 0              TO RY9-KDVRTPO                                 
185100     END-IF                                                               
185200     MOVE RAD-PRARTNTO     TO RY9-PRARTNTO                                
185300     MOVE RAD-TIREGDAT     TO RY9-TIREGDAT                                
185400     MOVE RAD-TIRES        TO RY9-TIRES                                   
185500     MOVE RAD-TITPO        TO RY9-TITPO                                   
185600     MOVE RAD-DARODAT (3:6) TO RY9-TIRODAT                                
185700     MOVE GMT-FLVR         TO RY9-FLVR                                    
185800                                                                          
185900     MOVE RY9-WDGZRY9      TO ZZAC-LOGGPOST                               
186000                                                                          
186100     MOVE SPACE            TO RY9S-WDGZRY9S                               
186200     MOVE RAD-IDDISTR      TO RY9S-IDDISTR                                
186300     MOVE RAD-IDKUNDNR     TO RY9S-IDKUNDNR                               
186400     MOVE ORQI-OHUV-IDORDER TO RY9S-IDORDER                               
186500     MOVE RAD-IDDC         TO RY9S-IDDC                                   
186600     MOVE RAD-KDFRAKT      TO RY9S-KDFRAKT                                
186700     MOVE RAD-KDORDKL      TO RY9S-KDORDKL                                
186800     IF RAD-FLTPOBEK = NEJ                                                
186900        MOVE 83            TO RY9S-KDORDBEK                               
187000     ELSE                                                                 
187100        MOVE 85            TO RY9S-KDORDBEK                               
187200     END-IF                                                               
187300                                                                          
187400     MOVE RY9S-WDGZRY9S    TO ZZAC-SORTPOST                               
187500     .                                                                    
187600     EJECT                                                                
187700 DBBE-SKRIV-LOGG SECTION.                                                 
187800     MOVE 'DBBE-SKRIV-LOGG      ' TO CURR-SECTION                         
187900                                                                          
188000     ACCEPT ZZAC-TIAAMMDD  FROM DATE                                      
188100     ACCEPT ZZAC-TIKLOCK   FROM TIME                                      
188200     MOVE  +1            TO ZZAC-IDLOGLOP                                 
188300                                                                          
188400     PERFORM IMS-27-ISRT-LOGG                                             
188500     IF SEGMENT-FINNS-REDAN                                               
188600       PERFORM UNTIL SEGMENT-HAR-LAGTS-TILL                               
188700         ADD +1 TO ZZAC-IDLOGLOP                                          
188800         PERFORM IMS-27-ISRT-LOGG                                         
188900       END-PERFORM                                                        
189000     END-IF                                                               
189100     .                                                                    
189200     EJECT                                                                
189300 DBBF-SKAPA-2109-TRANS SECTION.                                           
189400     MOVE 'DBBF-SKAPA-2109-TRANS' TO CURR-SECTION                         
189500                                                                          
189601*    FÖR BYYTESARTIKLAR SKALL INGEN ORDERINGÅNG SKAPAS                    
189701     MOVE RAD-IDARTNR           TO BYT03-IDARTNR                          
189801     IF NOT BYT03-OBJEKT                                                  
189901                                                                          
190000        IF RAD-KDOI NOT = SPACE                                           
190100          MOVE SPACE            TO 2109-MID2-W2I10902                     
190200          MOVE 1                TO 2109-MID2-KVANTART                     
190300          MOVE RAD-IDARTNR      TO 2109-MID2-IDARTNR (1)                  
190400          MOVE ORQI-OHUV-IDDC-PRIM                                        
190500                                TO 2109-MID2-IDDC(1)                      
190600          MOVE '-'              TO 2109-MID2-KDTECKEN (1)                 
190700          MOVE RAD-KDOI         TO 2109-MID2-KDOI (1)                     
190800          MOVE RAD-CLEARGROUP   TO 2109-MID2-CLEARGROUP (1)               
190900          MOVE SPAR-KVART       TO 2109-MID2-KVOI (1)                     
191000                                                                          
191100          IF RAD-KDTPOTYP >= 1 AND <= 5 AND RAD-KDSTARAD = 1              
191200            IF RAD-FLTPOBEK = NEJ                                         
191300              MOVE RAD-TIREGDAT TO 2109-MID2-TIUPPDAT (1)                 
191400            ELSE                                                          
191500              MOVE RAD-TITPO    TO 2109-MID2-TIUPPDAT (1)                 
191600            END-IF                                                        
191700          ELSE                                                            
191800            IF RAD-KDTPOTYP >= 1 AND <= 5                                 
191900              MOVE 'DT'         TO 2109-MID2-KDOI (1)                     
192000              MOVE SPACE        TO 2109-MID2-CLEARGROUP (1)               
192100              MOVE RAD-TITPO    TO 2109-MID2-TIUPPDAT (1)                 
192200            ELSE                                                          
192300              MOVE RAD-TIREGDAT TO 2109-MID2-TIUPPDAT (1)                 
192400            END-IF                                                        
192500          END-IF                                                          
192600                                                                          
192700          COMPUTE 2109-KVLL = LENGTH OF 2109-MID2-W2I10902 + 17           
192800                                                                          
192900          PERFORM IMS-PURG-ALT-MSG-2109                                   
193000        END-IF                                                            
193101     END-IF                                                               
193200                                                                          
193300     .                                                                    
193400     EJECT                                                                
193500                                                                          
193600 DC-SKAPA-ORDER SECTION.                                                  
193700     MOVE 'DC-SKAPA-ORDER' TO CURR-SECTION                                
193800                                                                          
193900     PERFORM DCA-SKAPA-ORDERNR-W411ORDN                                   
194000     PERFORM DCB-SKAPA-MSG-KOM-AREA                                       
194100     PERFORM DCC-SKAPA-W40251-MID                                         
194200     PERFORM DCD-SKAPA-W40252-MID                                         
194300     PERFORM DCE-UPPDATERA-WDA5                                           
194400     IF RAD-KDTPOTYP > ZERO                                               
194500       IF RAD-FLTPOBEK = JA                                               
194600         PERFORM DBBA-UPPDATERA-WDK9                                      
194700       END-IF                                                             
194800     ELSE                                                                 
194900        IF DCS-IDDC NOT = RAD-IDDC                                        
195000           MOVE RAD-IDDC TO W-IDDC-B6                                     
195100           PERFORM IMS-11-GU-WDB601                                       
195200        END-IF                                                            
195300        IF  DCS-CDC                                                       
195400            PERFORM S10-MINSKA-KVROS-WDK6                                 
195500        ELSE                                                              
195600            IF DCS-NDC                                                    
195700               PERFORM S11-MINSKA-KVROS-WDK7                              
195800            END-IF                                                        
195900        END-IF                                                            
196000     END-IF                                                               
196100     IF RAD-KDTPOTYP = 2 OR 6                                             
196200        PERFORM DBBB-BEHANDLA-LARMKO                                      
196300     END-IF                                                               
196400     .                                                                    
196500     EJECT                                                                
196600                                                                          
196700 DCA-SKAPA-ORDERNR-W411ORDN  SECTION.                                     
196800     MOVE 'DCA-SKAPA-ORDERNR-W411ORDN' TO CURR-SECTION                    
196900                                                                          
197000     MOVE 'LDCB'                  TO ORDN-IDSYSTEM                        
197100     MOVE SPAR-IDDISTR-RAD(INDX)  TO ORDN-IDDISTR                         
197200     MOVE SPAR-IDKUNDNR-RAD(INDX) TO ORDN-IDKUNDNR                        
197300     MOVE ZERO                    TO ORDN-IDORDNR-IN                      
197400                                                                          
197500     CALL W411ORDN USING ORDN-W411ORDN ORDN-XXKP-PCB ORDN-ORQL-PCB        
197600                                       ORDN-PROC-PCB ORDN-ORQI-PCB        
197700                                                                          
197800     MOVE ORDN-IDORDNR-UT         TO SPARA-IDORDNR                        
197900     .                                                                    
198000     EJECT                                                                
198100 DCB-SKAPA-MSG-KOM-AREA SECTION.                                          
198200     MOVE 'DCB-SKAPA-MSG-KOM-AREA' TO CURR-SECTION                        
198300                                                                          
198400     MOVE SPACE            TO MSG-KOM-WMSGKOM                             
198500     MOVE +54              TO MSG-KOM-KVLL                                
198600     MOVE LOW-VALUE        TO MSG-KOM-KDZ1                                
198700     MOVE LOW-VALUE        TO MSG-KOM-KDZ2                                
198800     MOVE SPACE            TO MSG-KOM-KDTRANS                             
198900     MOVE 'LDCREL'         TO MSG-KOM-IDSNDNOD                            
199000     MOVE 'W4057800'       TO MSG-KOM-IDSNDJOB                            
199100     MOVE DAGENS-DATUM     TO MSG-KOM-TIREGDAT                            
199200     MOVE CURR-TIME        TO MSG-KOM-TIKLOCK                             
199300     MOVE SPACE            TO MSG-KOM-IDMFSMED                            
199400     .                                                                    
199500     EJECT                                                                
199600                                                                          
199700 DCC-SKAPA-W40251-MID   SECTION.                                          
199800     MOVE 'DCC-SKAPA-W40251-MID  ' TO CURR-SECTION                        
199900                                                                          
200001     MOVE SPACE                   TO WS-ADBETRAD-1                        
200101     MOVE SPACE                   TO WS-ADBETRAD-2                        
200201     MOVE SPACE                   TO WS-BEBETRAD-1                        
200301     MOVE SPACE                   TO WS-BEBETRAD-2                        
200401                                                                          
200500     MOVE SPACE                   TO W-WDA5KEY-X                          
200600     MOVE SPAR-IDDISTR-RAD (INDX) TO W-A5-IDDISTR-KEY                     
200700     MOVE SPAR-IDKUNDNR-RAD(INDX) TO W-A5-IDKUNDNR-KEY                    
200800     MOVE SPAR-IDORDNR-RAD (INDX) TO W-A5-IDORDNR-KEY                     
200900     MOVE SPAR-IDARTNR-RAD (INDX) TO W-A5-IDARTNR-KEY                     
201000     MOVE SPAR-IDLOPNR-RAD (INDX) TO W-A5-IDLOPNR-KEY                     
201100     PERFORM IMS-05-GHU-WDA501                                            
201200                                                                          
201300     MOVE RAD-IDDISTR    TO W-IDDISTR-WDB2                                
201400     MOVE RAD-IDKUNDNR   TO W-IDKUNDNR-WDB2                               
201500     MOVE RAD-KVART      TO SPAR-KVART                                    
201600     PERFORM IMS-07-GU-WDB201                                             
201700     IF SEGMENT-SAKNAS                                                    
201800        MOVE SPACE       TO GMT-IDDC-BULK(1)                              
201900     END-IF                                                               
202000                                                                          
202101*    *Read order to fetch adbet and bebet = contact info                  
202201*    for LYNK orders.                                                     
202301     IF (RAD-IDSYSTEM = 'LYNK' OR 'ECOM' OR 'VOUI' OR 'TAD '              
202303                               OR 'ACC ' OR 'APA'  OR 'APB'               
202304                               OR 'APC ' OR 'APD'  OR 'APE'               
202305                               OR 'ACF ' OR 'APG'  OR 'APH'               
202306                               OR 'ACI ' OR 'APJ' )                       
202401       MOVE RAD-IDDISTR  TO W-IDDISTR-N9                                  
202501       MOVE RAD-IDKUNDNR TO W-IDKUNDNR-N9                                 
202601       MOVE '00'         TO W-IDKUNDRF-N9(1:2)                            
202701       MOVE RAD-IDKUNDRF TO W-IDKUNDRF-N9(3:5)                            
202801       PERFORM IMS-24-GU-ORQI01-CSEQ                                      
202901       IF SEGMENT-FINNS                                                   
203001          MOVE ORQI-OHUV-ADBETRAD-1  TO WS-ADBETRAD-1                     
203101          MOVE ORQI-OHUV-ADBETRAD-2  TO WS-ADBETRAD-2                     
203201          MOVE ORQI-OHUV-BEBETRAD-1  TO WS-BEBETRAD-1                     
203301          MOVE ORQI-OHUV-BEBETRAD-2  TO WS-BEBETRAD-2                     
203401       END-IF                                                             
203501     END-IF                                                               
203601                                                                          
203700     MOVE 'W4I25101'             TO MSG-KOM-IDCPYTXT                      
203800     MOVE SPACE                  TO OHUV-MID-W4I25101                     
203900     MOVE RAD-IDSYSTEM           TO OHUV-MID-IDSYSTEM                     
203901     IF OHUV-MID-IDSYSTEM(1:4) IS IDSYSAPIS                               
204001       MOVE 'B'                  TO OHUV-MID-IDSYSTEM(4:1)                
204101     ELSE                                                                 
204200       MOVE 'LDCB'               TO OHUV-MID-IDSYSTEM                     
204301     END-IF                                                               
204400     MOVE RAD-IDDISTR            TO WS-IDDISTR                            
204500     MOVE WS-IDDISTR             TO OHUV-MID-IDDISTR                      
204510                                    W-IDDISTR-DC                          
204520                                    W-IDDISTR-DC-DEF                      
204600     MOVE RAD-IDKUNDNR           TO WS-IDKUNDNR                           
204700     MOVE WS-IDKUNDNR            TO OHUV-MID-IDKUNDNR                     
204710                                    W-IDKUNDNR-DC                         
204800     MOVE SPARA-IDORDNR          TO OHUV-MID-IDORDNR                      
204900                                                                          
206100     IF MID-KDORDKL(INDX) NOT = ALL '+'                                   
206200        MOVE MID-KDORDKL(INDX)     TO OHUV-MID-KDORDKL                    
206300     ELSE                                                                 
206400        MOVE '3'                   TO OHUV-MID-KDORDKL                    
206500     END-IF                                                               
206600                                                                          
206700     IF MID-KDCMD (INDX) = WS-NORMAL-TILL-LDC                             
206800        MOVE SPACE                 TO OHUV-MID-IDDC                       
206810                                      OHUV-MID-KDFRAKT                    
206900     ELSE                                                                 
207000       IF MID-IDDC-PRIM(INDX) NOT = ALL '+'                               
207100          MOVE MID-IDDC-PRIM(INDX) TO OHUV-MID-IDDC                       
207200                                      WS-IDDC                             
207210                                      WS-IDDC-GMT                         
207300       ELSE                                                               
207400          MOVE GMT-IDDC-BULK(1)    TO OHUV-MID-IDDC                       
207500                                      WS-IDDC                             
207510                                      WS-IDDC-GMT                         
207600       END-IF                                                             
207700     END-IF                                                               
207801                                                                          
207802     IF WS-IDDC-GMT NOT = SPACE                                           
207803       PERFORM S06-GET-KDFRAKT                                            
207860     END-IF                                                               
207870                                                                          
207880     IF WS-KDFRAKT = 0                                                    
207890       MOVE SPACE                  TO OHUV-MID-KDFRAKT                    
207900     ELSE                                                                 
207901       MOVE WS-KDFRAKT             TO OHUV-MID-KDFRAKT                    
207902     END-IF                                                               
207903     MOVE RAD-IDORDNR5             TO OHUV-MID-BEKUNDRF                   
208000                                                                          
208100     IF MID-BELAGINS(INDX) NOT = ALL '+'                                  
208200        MOVE MID-BELAGINS(INDX)  TO OHUV-MID-BELAGINS                     
208300     ELSE                                                                 
208400        MOVE SPACE               TO OHUV-MID-BELAGINS                     
208500     END-IF                                                               
208600     MOVE NEJ                    TO OHUV-MID-FLAUTPAC                     
208700     MOVE NEJ                    TO OHUV-MID-FLAUTFAK                     
208800     MOVE NEJ                    TO OHUV-MID-FLEMBORD                     
208900     MOVE NEJ                    TO OHUV-MID-FLOVRLEV                     
209000     MOVE ZERO                   TO OHUV-MID-IDDEPT                       
209100     IF MID-KDCMD (INDX) = WS-NORMAL-TILL-LDC                             
209200        MOVE NEJ                 TO OHUV-MID-FLFORBI                      
209300     END-IF                                                               
209400     IF MID-KDCMD (INDX) = WS-FORBIORDER                                  
209500        MOVE JA                  TO OHUV-MID-FLFORBI                      
209600     END-IF                                                               
209700     IF MID-KDCMD (INDX) = WS-FORBIORDER-VOR                              
209800        MOVE SPEC-FORBI          TO OHUV-MID-FLFORBI                      
209900     END-IF                                                               
210000                                                                          
210100     MOVE RAD-KDORDTYP-LDC       TO OHUV-MID-KDORDTYP-LDC                 
210200     PERFORM S01A-SKAPA-RFSDATUM                                          
210210     PERFORM S03A-SKAPA-RFSDATUM                                          
210300     MOVE WS-TIRFS               TO OHUV-MID-TIRFS                        
210400     MOVE RAD-TIREPDAT           TO OHUV-MID-TIREPDAT                     
210500     MOVE NEJ                    TO OHUV-MID-FLORDTIL                     
210600     MOVE ZERO                   TO OHUV-MID-IDGROSS                      
210700*    *IF LYNK ORDER ADBET AND BEBET CONTAINS CONTACT INFO                 
210801     MOVE WS-ADBETRAD-1          TO OHUV-MID-ADBETRAD-1                   
210901     MOVE WS-ADBETRAD-2          TO OHUV-MID-ADBETRAD-2                   
211001     MOVE WS-BEBETRAD-1          TO OHUV-MID-BEBETRAD-1                   
211101     MOVE WS-BEBETRAD-2          TO OHUV-MID-BEBETRAD-2                   
211201                                                                          
211300                                                                          
211400     COMPUTE P-TO-P-KVLL = LENGTH OF OHUV-MID-W4I25101 + 17               
211500     MOVE 'W4T251X '       TO P-TO-P-KDTRANS                              
211600     MOVE '4251'           TO P-TO-P-IDTRANS                              
211700     MOVE '1'              TO P-TO-P-KDMFSFOR                             
211800                                                                          
211900     MOVE KOM-AREA                TO P-TO-P-DATA                          
212000     CALL W006KOM USING MSG-PCB                                           
212100                        ALT-PCB                                           
212200                        KOMA-PCB                                          
212300                        MSG-KOM-WMSGKOM                                   
212400                        P-TO-P-SW                                         
212500     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
212600*       FELAKTIG UPPDATERING PÅ KOMMUNIKATIONS-DB                         
212700*       FELAKTIG DATUM, TID EJ NUM FÅR EJ INTRÄFFS                        
212800        MOVE 'FELAKTIG PÅ INPUT TILL DISPATCHEN' TO FELTEXT               
212900        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
213000     END-IF                                                               
213100                                                                          
213200     MOVE SPACE TO KOM-AREA                                               
213300     .                                                                    
213400     EJECT                                                                
213410                                                                          
213500 DCD-SKAPA-W40252-MID   SECTION.                                          
213600     MOVE 'DCD-SKAPA-W40252-MID  ' TO CURR-SECTION                        
213700                                                                          
213800     MOVE 'W4I25201'       TO MSG-KOM-IDCPYTXT                            
213900     MOVE SPACE            TO ORAD-MID-W4I25201                           
214001     MOVE RAD-IDSYSTEM           TO ORAD-MID-IDSYSTEM                     
214002     IF ORAD-MID-IDSYSTEM(1:4) IS IDSYSAPIS                               
214003       MOVE 'B'                  TO ORAD-MID-IDSYSTEM(4:1)                
214004     ELSE                                                                 
214005       MOVE 'LDCB'               TO ORAD-MID-IDSYSTEM                     
214006     END-IF                                                               
214500     MOVE RAD-IDDISTR      TO WS-IDDISTR                                  
214600     MOVE WS-IDDISTR       TO ORAD-MID-IDDISTR                            
214700     MOVE RAD-IDKUNDNR     TO WS-IDKUNDNR                                 
214800     MOVE WS-IDKUNDNR      TO ORAD-MID-IDKUNDNR                           
214900     MOVE SPARA-IDORDNR    TO ORAD-MID-IDORDNR                            
215000     MOVE JA               TO ORAD-MID-FLSLUT                             
215100     MOVE RAD-IDARTNR      TO ORAD-MID-IDARTNR(1)                         
215200                              REK-IDARTNR                                 
215300     MOVE 9                TO REK-LNGD                                    
215400     MOVE 0                TO REK-REKSIFFR                                
215500     CALL W009KSIF      USING REK-IDARTNR                                 
215600                              REK-LNGD                                    
215700                              REK-REKSIFFR                                
215800     MOVE REK-REKSIFFR     TO ORAD-MID-REKSIFFR(1)                        
215900     MOVE RAD-KVART        TO WS-KVART                                    
216000     MOVE WS-KVART         TO ORAD-MID-KVBEART(1)                         
216100     MOVE RAD-IDKUNDRF-WIP TO ORAD-MID-IDKUNDRF-WIP(1)                    
216200     MOVE RAD-BERADREF     TO ORAD-MID-BERADREF(1)                        
216201     MOVE '0000000   '     TO ORAD-MID-IDKUNDRF-RO                        
216202     MOVE RAD-IDORDNR5     TO ORAD-MID-IDKUNDRF-RO(3:5)                   
216210     IF RAD-IDSYSTEM(1:3) = 'ECO'                                         
216221          MOVE RAD-PRARTNTO-LOC    TO W-PRARTNTO-LOC                      
216222          MOVE W-PRARTNTO-LOC-HEL  TO W-PRARTNTO-LOC-X-HEL                
216223          MOVE W-PRARTNTO-LOC-DEC  TO W-PRARTNTO-LOC-X-DEC                
216224                                                                          
216225          MOVE W-PRARTNTO-LOC-X    TO                                     
216226                                      ORAD-MID-PRARTNTO-LOC (1)           
216227          MOVE RAD-KDVALISO        TO ORAD-MID-KDVALISO (1)               
216230     END-IF                                                               
216300                                                                          
216400     COMPUTE P-TO-P-KVLL = LENGTH OF ORAD-MID-W4I25201 + 17               
216500     MOVE 'W4T252X '       TO P-TO-P-KDTRANS                              
216600     MOVE '4252'           TO P-TO-P-IDTRANS                              
216700     MOVE '1'              TO P-TO-P-KDMFSFOR                             
216800                                                                          
216900     MOVE KOM-AREA                TO P-TO-P-DATA                          
217000     CALL W006KOM USING MSG-PCB                                           
217100                        ALT-PCB                                           
217200                        KOMA-PCB                                          
217300                        MSG-KOM-WMSGKOM                                   
217400                        P-TO-P-SW                                         
217500     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
217600*       FELAKTIG UPPDATERING PÅ KOMMUNIKATIONS-DB                         
217700*       FELAKTIG DATUM, TID EJ NUM FÅR EJ INTRÄFFS                        
217800        MOVE 'FELAKTIG PÅ INPUT TILL DISPATCHEN' TO FELTEXT               
217900        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
218000     END-IF                                                               
218100                                                                          
218200     MOVE SPACE TO KOM-AREA                                               
218300     .                                                                    
218400     EJECT                                                                
218500                                                                          
218600 DCE-UPPDATERA-WDA5     SECTION.                                          
218700     MOVE 'DCE-UPPDATERA-WDA5    ' TO CURR-SECTION                        
218800                                                                          
218900     MOVE '4'                 TO RAD-KDSTARAD                             
219000     MOVE SPACE               TO RAD-IDKUNDRF-LEV                         
219100     MOVE SPARA-IDORDNR (3:5) TO RAD-IDKUNDRF-LEV (1:5)                   
219200     ACCEPT RAD-TIAVBOKN FROM DATE                                        
219300     PERFORM IMS-06-REPL-WDA501                                           
219400     .                                                                    
219500     EJECT                                                                
219600                                                                          
219700 DD-UPPDATERA-NOTERING SECTION.                                           
219800     MOVE 'DD-UPPDATERA-NOTERING' TO CURR-SECTION                         
219900                                                                          
220000     IF MID-BETEXT(INDX) NOT = ALL '+'                                    
220600        MOVE SPAR-IDDISTR-RAD (INDX) TO W-A5-IDDISTR-KEY                  
220700        MOVE SPAR-IDKUNDNR-RAD(INDX) TO W-A5-IDKUNDNR-KEY                 
220800        MOVE SPAR-IDORDNR-RAD (INDX) TO W-A5-IDORDNR-KEY                  
220900        MOVE SPAR-IDARTNR-RAD (INDX) TO W-A5-IDARTNR-KEY                  
221000        MOVE SPAR-IDLOPNR-RAD (INDX) TO W-A5-IDLOPNR-KEY                  
221100                                                                          
221200        PERFORM IMS-04-GU-WDA501                                          
221300        IF SEGMENT-FINNS                                                  
221400                                                                          
221500           MOVE RAD-IDGMTREF    TO W-IDGMTREF-4564                        
221600           MOVE RAD-IDARTNR     TO W-IDARTNR-4564                         
221700           MOVE RAD-IDLOPNR     TO W-IDLOPNR-4564                         
221800                                                                          
221900           PERFORM IMS-29-GHU-WDR5                                        
222000           MOVE MID-BETEXT(INDX)  TO 4564-BETEXT-010                      
222100           IF SEGMENT-FINNS                                               
222200              PERFORM IMS-30-REPL-WDR5                                    
222300           ELSE                                                           
222400              MOVE RAD-IDGMTREF TO 4564-IDGMTREF                          
222500              MOVE RAD-IDARTNR  TO 4564-IDARTNR                           
222600              MOVE RAD-IDLOPNR  TO 4564-IDLOPNR                           
222700              MOVE RAD-TIREPDAT TO 4564-TIREPDAT                          
222800              PERFORM IMS-31-ISRT-WDR5                                    
222900           END-IF                                                         
223000        END-IF                                                            
223100     END-IF                                                               
223200     .                                                                    
223300     EJECT                                                                
223400                                                                          
223500 DE-JUSTERA-SPARAREA  SECTION.                                            
223600     MOVE 'DE-JUSTERA-SPARA' TO CURR-SECTION                              
223700                                                                          
223800*    SPAR-RAD ANVÄNDS FÖR ATT VISA SIDAN                                  
223900*    VID FRISLÄPPNING AV ORDER BLIR RADEN INTE AKTUELL LÄNGRE             
224000*    OCH STÄLLER BARA TILL TRASSEL OM DEN FINNS KVAR                      
224100*    I TABELLEN SPAR-RAD                                                  
224200*                                                                         
224300     MOVE 1 TO INDX                                                       
224400               INDX-JUST-NEW                                              
224500                                                                          
224600     PERFORM UNTIL INDX > MAX-INDX                                        
224700        IF MID-KDCMD (INDX) = WS-NORMAL-TILL-LDC                          
224800        OR MID-KDCMD (INDX) = WS-FORBIORDER                               
224900        OR MID-KDCMD (INDX) = WS-FORBIORDER-VOR                           
224910        OR MID-KDCMD (INDX) = WS-DELETE-PLUS-MAIL                         
225000           CONTINUE                                                       
225100        ELSE                                                              
225200           MOVE SPAR-RAD(INDX) TO WS-HELP-RAD(INDX-JUST-NEW)              
225300           ADD 1 TO INDX-JUST-NEW                                         
225400        END-IF                                                            
225500        ADD 1 TO INDX                                                     
225600     END-PERFORM                                                          
225700                                                                          
225800     MOVE 1    TO INDX                                                    
225900     PERFORM UNTIL INDX > MAX-INDX                                        
226000        IF WS-HELP-RAD(INDX) = SPACE                                      
226100           MOVE ZERO TO SPAR-IDDISTR-RAD  (INDX)                          
226200                        SPAR-IDKUNDNR-RAD (INDX)                          
226300                        SPAR-IDORDNR-RAD  (INDX)                          
226400                        SPAR-IDARTNR-RAD  (INDX)                          
226500                        SPAR-IDLOPNR-RAD  (INDX)                          
226501                        SPAR-KDFARLIG-RAD(INDX)                           
226510                        SPAR-TIRFSDAT-CDC-RAD (INDX)                      
226600        ELSE                                                              
226700           MOVE WS-HELP-RAD(INDX) TO SPAR-RAD(INDX)                       
226800        END-IF                                                            
226900                                                                          
227000        ADD +1 TO INDX                                                    
227100     END-PERFORM                                                          
227200     .                                                                    
227300                                                                          
227400                                                                          
227500 E-FIRST-PAGE SECTION.                                                    
227600     MOVE 'E-FIRST-PAGE    ' TO CURR-SECTION                              
227700                                                                          
227800     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
227900     CALL WMEDKONV USING MED-WMEDAREA                                     
228000     MOVE MED-MFSINF     TO MOD-TEMFSFEL                                  
228100                                                                          
228200     MOVE LOW-VALUE  TO W-WDA5KEY-MIN-X                                   
228300                        W-WDA5A1KY-MIN-X                                  
228400                        W-WDB2C1KY-MIN-X                                  
228500     MOVE HIGH-VALUE TO W-WDA5KEY-MAX-X                                   
228600                        W-WDA5A1KY-MAX-X                                  
228700                        W-WDB2C1KY-MAX-X                                  
228800     IF SHOW-DISTRICT                                                     
228900        MOVE MSGI-IDDISTR  TO W-A5-IDDISTR-MIN                            
229000        MOVE MSGI-IDDISTR  TO W-A5-IDDISTR-MAX                            
229100     END-IF                                                               
229200                                                                          
229300     IF SHOW-DISTRICT-CUST                                                
229400        MOVE MSGI-IDDISTR  TO W-A5-IDDISTR-MIN                            
229500        MOVE MSGI-IDDISTR  TO W-A5-IDDISTR-MAX                            
229600        MOVE MSGI-IDKUNDNR TO W-A5-IDKUNDNR-MIN                           
229700        MOVE MSGI-IDKUNDNR TO W-A5-IDKUNDNR-MAX                           
229800     END-IF                                                               
229900                                                                          
230000     IF SHOW-DISTRICT-CUST-PARTNO                                         
230520        MOVE MSGI-IDDISTR  TO W-A5-IDDISTR-MIN                            
230530        MOVE MSGI-IDDISTR  TO W-A5-IDDISTR-MAX                            
230540        MOVE MSGI-IDKUNDNR TO W-A5-IDKUNDNR-MIN                           
230550        MOVE MSGI-IDKUNDNR TO W-A5-IDKUNDNR-MAX                           
230560        MOVE MSGI-IDARTNR  TO W-A5-IDARTNR                                
230600     END-IF                                                               
230601                                                                          
230610     IF SHOW-DISTRICT-PARTNO                                              
230671        MOVE MSGI-IDDISTR  TO W-A5-IDDISTR-MIN                            
230672        MOVE MSGI-IDDISTR  TO W-A5-IDDISTR-MAX                            
230675        MOVE MSGI-IDARTNR  TO W-A5-IDARTNR                                
230680     END-IF                                                               
230700                                                                          
230800     IF SHOW-PARTNO                                                       
230900        MOVE MSGI-IDARTNR  TO W-WDA5A1-IDARTNR-MIN                        
231000        MOVE MSGI-IDARTNR  TO W-WDA5A1-IDARTNR-MAX                        
231100     END-IF                                                               
231110     IF SHOW-DANGEROUS                                                    
231125        MOVE LOW-VALUE     TO W-WDA5KEY-MIN-X                             
231126                              W-WDA5A1KY-MIN-X                            
231127                              W-WDB2C1KY-MIN-X                            
231128        MOVE HIGH-VALUE    TO W-WDA5KEY-MAX-X                             
231129                              W-WDA5A1KY-MAX-X                            
231130                              W-WDB2C1KY-MAX-X                            
231140     END-IF                                                               
231200                                                                          
231210     IF SHOW-RFSDATE                                                      
231220        MOVE LOW-VALUE  TO    W-WDA5KEY-MIN-X                             
231230                              W-WDA5A1KY-MIN-X                            
231240                              W-WDB2C1KY-MIN-X                            
231250        MOVE HIGH-VALUE TO    W-WDA5KEY-MAX-X                             
231260                              W-WDA5A1KY-MAX-X                            
231270                              W-WDB2C1KY-MAX-X                            
231280     END-IF                                                               
231281                                                                          
231282     IF SHOW-DISTRICT-RFSDATE                                             
231283        MOVE LOW-VALUE     TO   W-WDA5KEY-MIN-X                           
231284                                W-WDA5A1KY-MIN-X                          
231285                                W-WDB2C1KY-MIN-X                          
231286        MOVE HIGH-VALUE    TO   W-WDA5KEY-MAX-X                           
231287                                W-WDA5A1KY-MAX-X                          
231288                                W-WDB2C1KY-MAX-X                          
231289        MOVE MSGI-IDDISTR  TO   W-A5-IDDISTR-MIN                          
231290        MOVE MSGI-IDDISTR  TO   W-A5-IDDISTR-MAX                          
231292     END-IF                                                               
231293                                                                          
231294     IF SHOW-DANG-RFSDATE                                                 
231295        MOVE LOW-VALUE     TO  W-WDA5KEY-MIN-X                            
231296                               W-WDA5A1KY-MIN-X                           
231297                               W-WDB2C1KY-MIN-X                           
231298        MOVE HIGH-VALUE    TO  W-WDA5KEY-MAX-X                            
231299                               W-WDA5A1KY-MAX-X                           
231300                               W-WDB2C1KY-MAX-X                           
231301     END-IF                                                               
231310                                                                          
231400     .                                                                    
231500     EJECT                                                                
231600 F-NEXT-PAGE SECTION.                                                     
231700     MOVE 'F-NEXT-PAGE     ' TO CURR-SECTION                              
231800                                                                          
231900     MOVE LOW-VALUE  TO W-WDA5KEY-MIN-X                                   
232000                        W-WDA5A1KY-MIN-X                                  
232100                        W-WDB2C1KY-MIN-X                                  
232200     MOVE HIGH-VALUE TO W-WDA5KEY-MAX-X                                   
232300                        W-WDA5A1KY-MAX-X                                  
232400                        W-WDB2C1KY-MAX-X                                  
232500                                                                          
232600     IF SPAR-IDTRANS = '4578'                                             
232700        MOVE SPAR-IDDISTR-NEXT  TO W-A5-IDDISTR-MIN                       
232800        MOVE SPAR-IDKUNDNR-NEXT TO W-A5-IDKUNDNR-MIN                      
232900        IF SHOW-DISTRICT                                                  
233000           MOVE SPAR-IDDISTR-NEXT  TO W-A5-IDDISTR-MIN                    
233100           MOVE SPAR-IDKUNDNR-NEXT TO W-A5-IDKUNDNR-MIN                   
233200           MOVE SPAR-IDDISTR-NEXT  TO W-A5-IDDISTR-MAX                    
233300           MOVE SPAR-IDKUNDRF-NEXT TO W-A5-IDKUNDRF-MIN                   
233400           MOVE SPAR-IDARTNR-NEXT  TO W-A5-IDARTNR-MIN                    
233500           MOVE SPAR-IDLOPNR-NEXT  TO W-A5-IDLOPNR-MIN                    
233600           MOVE SPAR-IDARTNR-NEXT  TO W-A5-IDARTNR                        
233700        END-IF                                                            
233800        IF SHOW-DISTRICT-CUST                                             
233900           MOVE SPAR-IDDISTR-NEXT  TO W-A5-IDDISTR-MAX                    
234000           MOVE SPAR-IDKUNDNR-NEXT TO W-A5-IDKUNDNR-MAX                   
234100           MOVE SPAR-IDKUNDRF-NEXT TO W-A5-IDKUNDRF-MIN                   
234200           MOVE SPAR-IDARTNR-NEXT  TO W-A5-IDARTNR-MIN                    
234300           MOVE SPAR-IDLOPNR-NEXT  TO W-A5-IDLOPNR-MIN                    
234400           MOVE SPAR-IDARTNR-NEXT  TO W-A5-IDARTNR                        
234500        END-IF                                                            
234600        IF SHOW-DISTRICT-CUST-PARTNO                                      
234700           MOVE SPAR-IDDISTR-NEXT  TO W-A5-IDDISTR-MAX                    
234800           MOVE SPAR-IDKUNDNR-NEXT TO W-A5-IDKUNDNR-MAX                   
234810           MOVE SPAR-IDKUNDRF-NEXT TO W-A5-IDKUNDRF-MIN                   
234900           MOVE SPAR-IDARTNR-NEXT  TO W-A5-IDARTNR-MIN                    
235100           MOVE SPAR-IDLOPNR-NEXT  TO W-A5-IDLOPNR-MIN                    
235200           MOVE SPAR-IDARTNR-NEXT  TO W-A5-IDARTNR                        
235300        END-IF                                                            
235310        IF SHOW-DISTRICT-PARTNO                                           
235320           MOVE SPAR-IDDISTR-NEXT  TO W-A5-IDDISTR-MAX                    
235331           MOVE SPAR-IDARTNR-NEXT  TO W-A5-IDARTNR                        
235332           MOVE SPAR-IDKUNDNR-NEXT TO W-A5-IDKUNDNR-MIN                   
235340           MOVE SPAR-IDKUNDRF-NEXT TO W-A5-IDKUNDRF-MIN                   
235360           MOVE SPAR-IDLOPNR-NEXT  TO W-A5-IDLOPNR-MIN                    
235380        END-IF                                                            
235400        IF SHOW-PARTNO                                                    
235500           MOVE SPAR-IDARTNR-NEXT  TO W-WDA5A1-IDARTNR-MIN                
235600           MOVE SPAR-IDARTNR-NEXT  TO W-WDA5A1-IDARTNR-MAX                
235700           MOVE SPAR-IDDC-NEXT     TO W-WDA5A1-IDDC-MIN                   
235800           MOVE SPAR-KDRAPRIO-NEXT TO W-WDA5A1-KDRAPRIO-MIN               
235900           MOVE SPAR-DARODAT-NEXT  TO W-WDA5A1-DARODAT-MIN                
236001           MOVE SPAR-TIREGTID-NEXT TO W-WDA5A1-TIREGTID-MIN               
236100           MOVE SPAR-IDDISTR-NEXT  TO W-WDA5A1-IDDISTR-MIN                
236200           MOVE SPAR-IDKUNDNR-NEXT TO W-WDA5A1-IDKUNDNR-MIN               
236300           MOVE SPAR-IDKUNDRF-NEXT TO W-WDA5A1-IDKUNDRF-MIN               
236400           MOVE SPAR-IDLOPNR-NEXT  TO W-WDA5A1-IDLOPNR-MIN                
236500        END-IF                                                            
236510        IF SHOW-DANGEROUS                                                 
236599           MOVE SPAR-IDARTNR-NEXT  TO W-WDA5A1-IDARTNR-MIN                
236601           MOVE SPAR-IDDC-NEXT     TO W-WDA5A1-IDDC-MIN                   
236602           MOVE SPAR-KDRAPRIO-NEXT TO W-WDA5A1-KDRAPRIO-MIN               
236603           MOVE SPAR-DARODAT-NEXT  TO W-WDA5A1-DARODAT-MIN                
236604           MOVE SPAR-TIREGTID-NEXT TO W-WDA5A1-TIREGTID-MIN               
236605           MOVE SPAR-IDDISTR-NEXT  TO W-WDA5A1-IDDISTR-MIN                
236606           MOVE SPAR-IDKUNDNR-NEXT TO W-WDA5A1-IDKUNDNR-MIN               
236607           MOVE SPAR-IDKUNDRF-NEXT TO W-WDA5A1-IDKUNDRF-MIN               
236608           MOVE SPAR-IDLOPNR-NEXT  TO W-WDA5A1-IDLOPNR-MIN                
236619        END-IF                                                            
236620        IF SHOW-RFSDATE                                                   
236621           MOVE SPAR-IDARTNR-NEXT  TO W-WDA5A1-IDARTNR-MIN                
236623           MOVE SPAR-IDDC-NEXT     TO W-WDA5A1-IDDC-MIN                   
236624           MOVE SPAR-KDRAPRIO-NEXT TO W-WDA5A1-KDRAPRIO-MIN               
236625           MOVE SPAR-DARODAT-NEXT  TO W-WDA5A1-DARODAT-MIN                
236626           MOVE SPAR-TIREGTID-NEXT TO W-WDA5A1-TIREGTID-MIN               
236627           MOVE SPAR-IDDISTR-NEXT  TO W-WDA5A1-IDDISTR-MIN                
236628           MOVE SPAR-IDKUNDNR-NEXT TO W-WDA5A1-IDKUNDNR-MIN               
236629           MOVE SPAR-IDKUNDRF-NEXT TO W-WDA5A1-IDKUNDRF-MIN               
236630           MOVE SPAR-IDLOPNR-NEXT  TO W-WDA5A1-IDLOPNR-MIN                
236631        END-IF                                                            
236632        IF SHOW-DISTRICT-RFSDATE                                          
236633           MOVE SPAR-IDARTNR-NEXT  TO W-WDA5A1-IDARTNR-MIN                
236635           MOVE SPAR-IDDC-NEXT     TO W-WDA5A1-IDDC-MIN                   
236636           MOVE SPAR-KDRAPRIO-NEXT TO W-WDA5A1-KDRAPRIO-MIN               
236637           MOVE SPAR-DARODAT-NEXT  TO W-WDA5A1-DARODAT-MIN                
236638           MOVE SPAR-TIREGTID-NEXT TO W-WDA5A1-TIREGTID-MIN               
236639           MOVE SPAR-IDDISTR-NEXT  TO W-WDA5A1-IDDISTR-MIN                
236640           MOVE SPAR-IDKUNDNR-NEXT TO W-WDA5A1-IDKUNDNR-MIN               
236641           MOVE SPAR-IDKUNDRF-NEXT TO W-WDA5A1-IDKUNDRF-MIN               
236642           MOVE SPAR-IDLOPNR-NEXT  TO W-WDA5A1-IDLOPNR-MIN                
236643        END-IF                                                            
236644        IF SHOW-DANG-RFSDATE                                              
236645           MOVE SPAR-IDARTNR-NEXT  TO W-WDA5A1-IDARTNR-MIN                
236646           MOVE SPAR-IDDC-NEXT     TO W-WDA5A1-IDDC-MIN                   
236647           MOVE SPAR-KDRAPRIO-NEXT TO W-WDA5A1-KDRAPRIO-MIN               
236648           MOVE SPAR-DARODAT-NEXT  TO W-WDA5A1-DARODAT-MIN                
236649           MOVE SPAR-TIREGTID-NEXT TO W-WDA5A1-TIREGTID-MIN               
236650           MOVE SPAR-IDDISTR-NEXT  TO W-WDA5A1-IDDISTR-MIN                
236651           MOVE SPAR-IDKUNDNR-NEXT TO W-WDA5A1-IDKUNDNR-MIN               
236652           MOVE SPAR-IDKUNDRF-NEXT TO W-WDA5A1-IDKUNDRF-MIN               
236653           MOVE SPAR-IDLOPNR-NEXT  TO W-WDA5A1-IDLOPNR-MIN                
236654        END-IF                                                            
236660     ELSE                                                                 
236700        MOVE ERR-LAST-PAGE-SHOWN   TO MED-IDMFSFEL                        
236800        CALL WMEDKONV USING MED-WMEDAREA                                  
236900        MOVE MED-MFSFEL            TO MOD-TEMFSFEL                        
237000     END-IF                                                               
237100     .                                                                    
237200     EJECT                                                                
237300 G-SAME-PAGE SECTION.                                                     
237400     MOVE 'G-SAME-PAGE     ' TO CURR-SECTION                              
237500                                                                          
237600     MOVE LOW-VALUE  TO W-WDA5KEY-MIN-X                                   
237700                        W-WDA5A1KY-MIN-X                                  
237800                        W-WDB2C1KY-MIN-X                                  
237900     MOVE HIGH-VALUE TO W-WDA5KEY-MAX-X                                   
238000                        W-WDA5A1KY-MAX-X                                  
238100                        W-WDB2C1KY-MAX-X                                  
238200                                                                          
238600     IF SHOW-DISTRICT                                                     
238700       MOVE SPAR-IDDISTR-ENTER     TO W-A5-IDDISTR-MAX                    
238710       MOVE SPAR-IDDISTR-ENTER     TO W-A5-IDDISTR-MIN                    
238720       MOVE SPAR-IDKUNDNR-ENTER    TO W-A5-IDKUNDNR-MIN                   
238730       MOVE SPAR-IDKUNDRF-ENTER    TO W-A5-IDKUNDRF-MIN                   
238800     END-IF                                                               
238900     IF SHOW-DISTRICT-CUST                                                
239000       MOVE SPAR-IDDISTR-ENTER     TO W-A5-IDDISTR-MAX                    
239100       MOVE SPAR-IDKUNDNR-ENTER    TO W-A5-IDKUNDNR-MAX                   
239110       MOVE SPAR-IDDISTR-ENTER     TO W-A5-IDDISTR-MIN                    
239120       MOVE SPAR-IDKUNDNR-ENTER    TO W-A5-IDKUNDNR-MIN                   
239130       MOVE SPAR-IDKUNDRF-ENTER    TO W-A5-IDKUNDRF-MIN                   
239200     END-IF                                                               
239300     IF SHOW-DISTRICT-CUST-PARTNO                                         
239400       MOVE SPAR-IDDISTR-ENTER     TO W-A5-IDDISTR-MAX                    
239500       MOVE SPAR-IDKUNDNR-ENTER    TO W-A5-IDKUNDNR-MAX                   
239600       MOVE SPAR-IDARTNR-ENTER     TO W-A5-IDARTNR                        
239610       MOVE SPAR-IDDISTR-ENTER     TO W-A5-IDDISTR-MIN                    
239620       MOVE SPAR-IDKUNDNR-ENTER    TO W-A5-IDKUNDNR-MIN                   
239630       MOVE SPAR-IDKUNDRF-ENTER    TO W-A5-IDKUNDRF-MIN                   
239700     END-IF                                                               
239710     IF SHOW-DISTRICT-PARTNO                                              
239720       MOVE SPAR-IDDISTR-ENTER     TO W-A5-IDDISTR-MAX                    
239740       MOVE SPAR-IDARTNR-ENTER     TO W-A5-IDARTNR                        
239741       MOVE SPAR-IDDISTR-ENTER     TO W-A5-IDDISTR-MIN                    
239742       MOVE SPAR-IDKUNDNR-ENTER    TO W-A5-IDKUNDNR-MIN                   
239743       MOVE SPAR-IDKUNDRF-ENTER    TO W-A5-IDKUNDRF-MIN                   
239750     END-IF                                                               
239800     IF SHOW-PARTNO                                                       
239900       MOVE SPAR-IDARTNR-ENTER     TO W-WDA5A1-IDARTNR-MIN                
240000       MOVE SPAR-IDARTNR-ENTER     TO W-WDA5A1-IDARTNR-MAX                
240100       MOVE SPAR-IDDC-ENTER        TO W-WDA5A1-IDDC-MIN                   
240200       MOVE SPAR-KDRAPRIO-ENTER    TO W-WDA5A1-KDRAPRIO-MIN               
240300       MOVE SPAR-DARODAT-ENTER     TO W-WDA5A1-DARODAT-MIN                
240401       MOVE SPAR-TIREGTID-ENTER    TO W-WDA5A1-TIREGTID-MIN               
240500       MOVE SPAR-IDDISTR-ENTER     TO W-WDA5A1-IDDISTR-MIN                
240600       MOVE SPAR-IDKUNDNR-ENTER    TO W-WDA5A1-IDKUNDNR-MIN               
240700       MOVE SPAR-IDKUNDRF-ENTER    TO W-WDA5A1-IDKUNDRF-MIN               
240800       MOVE SPAR-IDLOPNR-ENTER     TO W-WDA5A1-IDLOPNR-MIN                
240900     END-IF                                                               
240910     IF SHOW-DANGEROUS                                                    
240920       MOVE SPAR-IDARTNR-ENTER     TO W-WDA5A1-IDARTNR-MIN                
240940       MOVE SPAR-IDDC-ENTER        TO W-WDA5A1-IDDC-MIN                   
240950       MOVE SPAR-KDRAPRIO-ENTER    TO W-WDA5A1-KDRAPRIO-MIN               
240960       MOVE SPAR-DARODAT-ENTER     TO W-WDA5A1-DARODAT-MIN                
240970       MOVE SPAR-TIREGTID-ENTER    TO W-WDA5A1-TIREGTID-MIN               
240980       MOVE SPAR-IDDISTR-ENTER     TO W-WDA5A1-IDDISTR-MIN                
240990       MOVE SPAR-IDKUNDNR-ENTER    TO W-WDA5A1-IDKUNDNR-MIN               
240991       MOVE SPAR-IDKUNDRF-ENTER    TO W-WDA5A1-IDKUNDRF-MIN               
240992       MOVE SPAR-IDLOPNR-ENTER     TO W-WDA5A1-IDLOPNR-MIN                
241003     END-IF                                                               
241004     IF SHOW-RFSDATE                                                      
241006       MOVE SPAR-IDARTNR-ENTER     TO W-WDA5A1-IDARTNR-MIN                
241008       MOVE SPAR-IDDC-ENTER        TO W-WDA5A1-IDDC-MIN                   
241009       MOVE SPAR-KDRAPRIO-ENTER    TO W-WDA5A1-KDRAPRIO-MIN               
241010       MOVE SPAR-DARODAT-ENTER     TO W-WDA5A1-DARODAT-MIN                
241011       MOVE SPAR-TIREGTID-ENTER    TO W-WDA5A1-TIREGTID-MIN               
241012       MOVE SPAR-IDDISTR-ENTER     TO W-WDA5A1-IDDISTR-MIN                
241013       MOVE SPAR-IDKUNDNR-ENTER    TO W-WDA5A1-IDKUNDNR-MIN               
241014       MOVE SPAR-IDKUNDRF-ENTER    TO W-WDA5A1-IDKUNDRF-MIN               
241015       MOVE SPAR-IDLOPNR-ENTER     TO W-WDA5A1-IDLOPNR-MIN                
241016     END-IF                                                               
241017     IF SHOW-DISTRICT-RFSDATE                                             
241019       MOVE SPAR-IDARTNR-ENTER     TO W-WDA5A1-IDARTNR-MIN                
241021       MOVE SPAR-IDDC-ENTER        TO W-WDA5A1-IDDC-MIN                   
241022       MOVE SPAR-KDRAPRIO-ENTER    TO W-WDA5A1-KDRAPRIO-MIN               
241023       MOVE SPAR-DARODAT-ENTER     TO W-WDA5A1-DARODAT-MIN                
241024       MOVE SPAR-TIREGTID-ENTER    TO W-WDA5A1-TIREGTID-MIN               
241025       MOVE SPAR-IDDISTR-ENTER     TO W-WDA5A1-IDDISTR-MIN                
241026       MOVE SPAR-IDKUNDNR-ENTER    TO W-WDA5A1-IDKUNDNR-MIN               
241027       MOVE SPAR-IDKUNDRF-ENTER    TO W-WDA5A1-IDKUNDRF-MIN               
241028       MOVE SPAR-IDLOPNR-ENTER     TO W-WDA5A1-IDLOPNR-MIN                
241029     END-IF                                                               
241030     IF SHOW-DANG-RFSDATE                                                 
241031        MOVE SPAR-IDARTNR-ENTER    TO W-WDA5A1-IDARTNR-MIN                
241032        MOVE SPAR-IDDC-ENTER       TO W-WDA5A1-IDDC-MIN                   
241033        MOVE SPAR-KDRAPRIO-ENTER   TO W-WDA5A1-KDRAPRIO-MIN               
241034        MOVE SPAR-DARODAT-ENTER    TO W-WDA5A1-DARODAT-MIN                
241035        MOVE SPAR-TIREGTID-ENTER   TO W-WDA5A1-TIREGTID-MIN               
241036        MOVE SPAR-IDDISTR-ENTER    TO W-WDA5A1-IDDISTR-MIN                
241037        MOVE SPAR-IDKUNDNR-ENTER   TO W-WDA5A1-IDKUNDNR-MIN               
241038        MOVE SPAR-IDKUNDRF-ENTER   TO W-WDA5A1-IDKUNDRF-MIN               
241039        MOVE SPAR-IDLOPNR-ENTER    TO W-WDA5A1-IDLOPNR-MIN                
241040     END-IF                                                               
241050     .                                                                    
241100     EJECT                                                                
241200 H-READ-SHOW-INFO SECTION.                                                
241300     MOVE 'H-READ-SHOW-INFO' TO CURR-SECTION                              
241400                                                                          
241500     IF SHOW-DISTRICT                                                     
241600        PERFORM HA-SHOW-DISTRICT                                          
241700     END-IF                                                               
241800     IF SHOW-DISTRICT-CUST                                                
241900        PERFORM HB-SHOW-DISTRICT-CUST                                     
242000     END-IF                                                               
242100     IF SHOW-DISTRICT-CUST-PARTNO                                         
242200        PERFORM HC-SHOW-DISTRICT-CUST-PARTNO                              
242300     END-IF                                                               
242400     IF SHOW-PARTNO                                                       
242500        PERFORM HD-SHOW-PARTNO                                            
242600     END-IF                                                               
242610     IF SHOW-DISTRICT-PARTNO                                              
242620        PERFORM HE-SHOW-DISTRICT-PARTNO                                   
242630     END-IF                                                               
242640     IF SHOW-DANGEROUS                                                    
242650        PERFORM HF-SHOW-DANGEROUS                                         
242660     END-IF                                                               
242670     IF SHOW-RFSDATE                                                      
242680        PERFORM HG-SHOW-RFSDATE                                           
242690     END-IF                                                               
242700                                                                          
242710     IF SHOW-DISTRICT-RFSDATE                                             
242720        PERFORM HH-SHOW-DISTRICT-RFSDATE                                  
242730     END-IF                                                               
242740                                                                          
242750     IF SHOW-DANG-RFSDATE                                                 
242770        PERFORM HI-SHOW-DANG-RFSDATE                                      
242780     END-IF                                                               
242802     IF SEGMENT-FINNS                                                     
242900        MOVE RAD-IDDISTR          TO SPAR-IDDISTR-NEXT                    
243000        MOVE RAD-IDKUNDNR         TO SPAR-IDKUNDNR-NEXT                   
243100        MOVE RAD-IDKUNDRF         TO SPAR-IDKUNDRF-NEXT                   
243200        MOVE RAD-IDARTNR          TO SPAR-IDARTNR-NEXT                    
243300        MOVE RAD-IDLOPNR          TO SPAR-IDLOPNR-NEXT                    
243400        MOVE RAD-IDDC             TO SPAR-IDDC-NEXT                       
243500        MOVE RAD-KDRAPRIO         TO SPAR-KDRAPRIO-NEXT                   
243600        MOVE RAD-DARODAT          TO SPAR-DARODAT-NEXT                    
243701        MOVE RAD-TIREGTID         TO SPAR-TIREGTID-NEXT                   
243702        MOVE CLAG-KDFARLIG        TO SPAR-KDFARLIG-NEXT                   
243703        MOVE WS-TIRFS-CDC                                                 
243704                                  TO SPAR-TIRFSDAT-CDC-NEXT               
243800        IF MED-IDMFSINF NOT = INF-UPDATE-DONE                             
243900           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
244000           CALL WMEDKONV USING MED-WMEDAREA                               
244100           MOVE MED-MFSINF           TO MOD-TEMFSINF                      
244200        END-IF                                                            
244300     ELSE                                                                 
244400        MOVE SPAR-IDDISTR-ENTER   TO SPAR-IDDISTR-NEXT                    
244500        MOVE SPAR-IDKUNDNR-ENTER  TO SPAR-IDKUNDNR-NEXT                   
244600        MOVE SPAR-IDKUNDRF-ENTER  TO SPAR-IDKUNDRF-NEXT                   
244700        MOVE SPAR-IDARTNR-ENTER   TO SPAR-IDARTNR-NEXT                    
244800        MOVE SPAR-IDLOPNR-ENTER   TO SPAR-IDLOPNR-NEXT                    
244900        MOVE SPAR-IDDC-ENTER      TO SPAR-IDDC-NEXT                       
245000        MOVE SPAR-KDRAPRIO-ENTER  TO SPAR-KDRAPRIO-NEXT                   
245100        MOVE SPAR-DARODAT-ENTER   TO SPAR-DARODAT-NEXT                    
245201        MOVE SPAR-TIREGTID-ENTER  TO SPAR-TIREGTID-NEXT                   
245202        MOVE SPAR-TIRFSDAT-CDC-ENTER                                      
245203                                  TO SPAR-TIRFSDAT-CDC-NEXT               
245204        MOVE SPAR-KDFARLIG-ENTER  TO SPAR-KDFARLIG-NEXT                   
245300        IF MED-IDMFSINF NOT = INF-UPDATE-DONE                             
245400           MOVE INF-LAST-PAGE     TO MED-IDMFSINF                         
245500           CALL WMEDKONV USING MED-WMEDAREA                               
245600           MOVE MED-MFSINF        TO MOD-TEMFSINF                         
245700        END-IF                                                            
245800     END-IF                                                               
246330     MOVE '002'     TO MSGI-KDCALL                                        
246340     MOVE '4578'    TO SPAR-IDTRANS                                       
246360     MOVE SPAR-AREA TO MSGI-SPAR-AREA                                     
246380     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
246400     .                                                                    
246500                                                                          
246600 HA-SHOW-DISTRICT            SECTION.                                     
246700     MOVE 'HA-SHOW-DISTRICT' TO CURR-SECTION                              
246800                                                                          
246900     IF MFS-UPDATE                                                        
247000        PERFORM S02-A5-POSITION-EFTER-UPDATE                              
247100     ELSE                                                                 
247200        PERFORM IMS-01-GN-WDA501                                          
247300     END-IF                                                               
247400     IF SEGMENT-FINNS                                                     
247500        IF RAD-IDSYSTEM (1:3) IS IDSYS4578                                
247510           MOVE '4578'       TO SPAR-IDTRANS                              
247600           MOVE RAD-IDDISTR  TO SPAR-IDDISTR-ENTER                        
247700           MOVE RAD-IDKUNDNR TO SPAR-IDKUNDNR-ENTER                       
247800           MOVE RAD-IDKUNDRF TO SPAR-IDKUNDRF-ENTER                       
247900           MOVE RAD-IDARTNR  TO SPAR-IDARTNR-ENTER                        
248000           MOVE RAD-IDLOPNR  TO SPAR-IDLOPNR-ENTER                        
248010           MOVE RAD-IDDC     TO SPAR-IDDC-ENTER                           
248100           MOVE +1 TO INDX                                                
248200           PERFORM UNTIL INDX > MAX-INDX                                  
248300              IF SEGMENT-FINNS                                            
248310                 IF RAD-IDSYSTEM (1:3) IS IDSYS4578                       
248400                    PERFORM S01-REDIGERA-ARTIKELRAD                       
248510                    IF NOT SKIP-RAD-ROW                                   
248520                       ADD 1 TO INDX                                      
248530                    END-IF                                                
248540                 END-IF                                                   
248550                 PERFORM IMS-01-GN-WDA501                                 
248600              ELSE                                                        
248700                 PERFORM MFS-RENSA-FAELT-UT                               
248710                 ADD 1 TO INDX                                            
248720              END-IF                                                      
249000           END-PERFORM                                                    
249100        END-IF                                                            
249101     ELSE                                                                 
249102        MOVE MSGI-IDDISTR TO SPAR-IDDISTR-ENTER                           
249103        MOVE MSGI-IDKUNDNR TO SPAR-IDKUNDNR-ENTER                         
249104        MOVE MSGI-IDARTNR TO SPAR-IDARTNR-ENTER                           
249105        MOVE MSGI-TIRFSDAT-CDC TO SPAR-TIRFSDAT-CDC-ENTER                 
249106        MOVE MSGI-KDFARLIG TO SPAR-KDFARLIG-ENTER                         
249107        MOVE +1 TO INDX                                                   
249108        PERFORM UNTIL INDX > MAX-INDX                                     
249109          PERFORM MFS-RENSA-FAELT-UT                                      
249110          ADD 1 TO INDX                                                   
249111        END-PERFORM                                                       
249120     END-IF                                                               
249200     .                                                                    
249300                                                                          
249400 HB-SHOW-DISTRICT-CUST       SECTION.                                     
249500     MOVE 'HB-SHOW-DISTRICT-CUST' TO CURR-SECTION                         
249600                                                                          
249700     IF MFS-UPDATE                                                        
249800        PERFORM S02-A5-POSITION-EFTER-UPDATE                              
249900     ELSE                                                                 
250000        PERFORM IMS-01-GN-WDA501                                          
250100     END-IF                                                               
250200     IF SEGMENT-FINNS                                                     
250210        IF RAD-IDSYSTEM (1:3) IS IDSYS4578                                
250300           MOVE '4578'       TO SPAR-IDTRANS                              
250400           MOVE RAD-IDDISTR  TO SPAR-IDDISTR-ENTER                        
250500           MOVE RAD-IDKUNDNR TO SPAR-IDKUNDNR-ENTER                       
250600           MOVE RAD-IDKUNDRF TO SPAR-IDKUNDRF-ENTER                       
250700           MOVE RAD-IDARTNR  TO SPAR-IDARTNR-ENTER                        
250800           MOVE RAD-IDLOPNR  TO SPAR-IDLOPNR-ENTER                        
250810           MOVE RAD-IDDC     TO SPAR-IDDC-ENTER                           
250900           MOVE +1 TO INDX                                                
251000           PERFORM UNTIL INDX > MAX-INDX                                  
251100              IF SEGMENT-FINNS                                            
251110                 IF RAD-IDSYSTEM (1:3) IS IDSYS4578                       
251200                    PERFORM S01-REDIGERA-ARTIKELRAD                       
251310                    IF NOT SKIP-RAD-ROW                                   
251320                       ADD 1 TO INDX                                      
251330                    END-IF                                                
251340                 END-IF                                                   
251350                 PERFORM IMS-01-GN-WDA501                                 
251400              ELSE                                                        
251500                 PERFORM MFS-RENSA-FAELT-UT                               
251510                 ADD 1 TO INDX                                            
251520              END-IF                                                      
251800           END-PERFORM                                                    
251900        END-IF                                                            
251910     ELSE                                                                 
251920        MOVE MSGI-IDDISTR TO SPAR-IDDISTR-ENTER                           
251921        MOVE MSGI-IDKUNDNR TO SPAR-IDKUNDNR-ENTER                         
251922        MOVE MSGI-IDARTNR TO SPAR-IDARTNR-ENTER                           
251923        MOVE MSGI-TIRFSDAT-CDC TO SPAR-TIRFSDAT-CDC-ENTER                 
251924        MOVE MSGI-KDFARLIG TO SPAR-KDFARLIG-ENTER                         
251925        MOVE +1 TO INDX                                                   
251926        PERFORM UNTIL INDX > MAX-INDX                                     
251927          PERFORM MFS-RENSA-FAELT-UT                                      
251928          ADD 1 TO INDX                                                   
251929        END-PERFORM                                                       
251930     END-IF                                                               
252000     .                                                                    
252100                                                                          
252200 HC-SHOW-DISTRICT-CUST-PARTNO SECTION.                                    
252300     MOVE 'HC-SHOW-DISTRICT-CUST-PARTNO' TO CURR-SECTION                  
252400                                                                          
252420     IF MFS-UPDATE                                                        
252430        PERFORM S02-A5-POSITION-EFTER-UPDATE                              
252440     ELSE                                                                 
252450        PERFORM IMS-02-GN-WDA501-ARTIKEL                                  
252460     END-IF                                                               
252470     IF SEGMENT-FINNS                                                     
252471        IF RAD-IDSYSTEM (1:3) IS IDSYS4578                                
252480           MOVE '4578'       TO SPAR-IDTRANS                              
252490           MOVE RAD-IDDISTR  TO SPAR-IDDISTR-ENTER                        
252500           MOVE RAD-IDKUNDNR TO SPAR-IDKUNDNR-ENTER                       
252600           MOVE RAD-IDKUNDRF TO SPAR-IDKUNDRF-ENTER                       
252700           MOVE RAD-IDARTNR  TO SPAR-IDARTNR-ENTER                        
252800           MOVE RAD-IDLOPNR  TO SPAR-IDLOPNR-ENTER                        
252900           MOVE RAD-KDRAPRIO TO SPAR-KDRAPRIO-ENTER                       
253000           MOVE RAD-DARODAT  TO SPAR-DARODAT-ENTER                        
253100           MOVE RAD-TIREGTID TO SPAR-TIREGTID-ENTER                       
253110           MOVE RAD-IDDC     TO SPAR-IDDC-ENTER                           
253200           MOVE +1 TO INDX                                                
253300           PERFORM UNTIL INDX > MAX-INDX                                  
253400              IF SEGMENT-FINNS                                            
253410                 IF RAD-IDSYSTEM (1:3) IS IDSYS4578                       
253500                    PERFORM S01-REDIGERA-ARTIKELRAD                       
253610                    IF NOT SKIP-RAD-ROW                                   
253620                       ADD 1 TO INDX                                      
253630                    END-IF                                                
253631                 END-IF                                                   
253640                 PERFORM IMS-02-GN-WDA501-ARTIKEL                         
253700              ELSE                                                        
253800                 PERFORM MFS-RENSA-FAELT-UT                               
253810                 ADD 1 TO INDX                                            
253820              END-IF                                                      
254100           END-PERFORM                                                    
254200        END-IF                                                            
254300     ELSE                                                                 
254310        MOVE MSGI-IDDISTR TO SPAR-IDDISTR-ENTER                           
254320        MOVE MSGI-IDKUNDNR TO SPAR-IDKUNDNR-ENTER                         
254330        MOVE MSGI-IDARTNR TO SPAR-IDARTNR-ENTER                           
254340        MOVE MSGI-TIRFSDAT-CDC TO SPAR-TIRFSDAT-CDC-ENTER                 
254350        MOVE MSGI-KDFARLIG TO SPAR-KDFARLIG-ENTER                         
254360        MOVE +1 TO INDX                                                   
254370        PERFORM UNTIL INDX > MAX-INDX                                     
254380          PERFORM MFS-RENSA-FAELT-UT                                      
254390          ADD 1 TO INDX                                                   
254391        END-PERFORM                                                       
254400     END-IF                                                               
255100     .                                                                    
255200                                                                          
255300 HD-SHOW-PARTNO              SECTION.                                     
255400     MOVE 'HD-SHOW-PARTNO'   TO CURR-SECTION                              
255500                                                                          
255620     IF MFS-UPDATE                                                        
255700        PERFORM S03-A5A1-POSITION-EFTER-UPDATE                            
255800     ELSE                                                                 
255900        PERFORM IMS-03-GN-WDA5A1                                          
256000     END-IF                                                               
256100     IF SEGMENT-FINNS                                                     
256200        MOVE SEQA-IDWDA501 TO W-WDA5KEY-X                                 
256300        PERFORM IMS-04-GU-WDA501                                          
256310     ELSE                                                                 
256320        MOVE MSGI-IDDISTR TO SPAR-IDDISTR-ENTER                           
256330        MOVE MSGI-IDKUNDNR TO SPAR-IDKUNDNR-ENTER                         
256340        MOVE MSGI-IDARTNR TO SPAR-IDARTNR-ENTER                           
256350        MOVE MSGI-TIRFSDAT-CDC TO SPAR-TIRFSDAT-CDC-ENTER                 
256360        MOVE MSGI-KDFARLIG TO SPAR-KDFARLIG-ENTER                         
256370        MOVE +1 TO INDX                                                   
256380        PERFORM UNTIL INDX > MAX-INDX                                     
256390          PERFORM MFS-RENSA-FAELT-UT                                      
256391          ADD 1 TO INDX                                                   
256392        END-PERFORM                                                       
256400     END-IF                                                               
256500     IF SEGMENT-FINNS                                                     
256600        IF RAD-IDSYSTEM (1:3) IS IDSYS4578                                
256610           MOVE '4578'       TO SPAR-IDTRANS                              
256700           MOVE RAD-IDDISTR  TO SPAR-IDDISTR-ENTER                        
256800           MOVE RAD-IDKUNDNR TO SPAR-IDKUNDNR-ENTER                       
256900           MOVE RAD-IDKUNDRF TO SPAR-IDKUNDRF-ENTER                       
257000           MOVE RAD-IDARTNR  TO SPAR-IDARTNR-ENTER                        
257100           MOVE RAD-IDLOPNR  TO SPAR-IDLOPNR-ENTER                        
257200           MOVE RAD-KDRAPRIO TO SPAR-KDRAPRIO-ENTER                       
257300           MOVE RAD-DARODAT  TO SPAR-DARODAT-ENTER                        
257401           MOVE RAD-TIREGTID TO SPAR-TIREGTID-ENTER                       
257402           MOVE RAD-IDDC     TO SPAR-IDDC-ENTER                           
257500           MOVE +1 TO INDX                                                
257600           PERFORM UNTIL INDX > MAX-INDX                                  
258000              IF SEGMENT-FINNS                                            
258001                 IF RAD-IDSYSTEM (1:3) IS IDSYS4578                       
258002                    PERFORM S01-REDIGERA-ARTIKELRAD                       
258003                    MOVE WS-TIRFS-CDC                                     
258004                             TO SPAR-TIRFSDAT-CDC-ENTER                   
258310                    IF NOT SKIP-RAD-ROW                                   
258320                       ADD 1 TO INDX                                      
258330                    END-IF                                                
258331                 END-IF                                                   
258332                 PERFORM IMS-03-GN-WDA5A1                                 
258333                 IF SEGMENT-FINNS                                         
258334                    MOVE SEQA-IDWDA501 TO W-WDA5KEY-X                     
258335                    PERFORM IMS-04-GU-WDA501                              
258336                 END-IF                                                   
258400              ELSE                                                        
258500                 PERFORM MFS-RENSA-FAELT-UT                               
258501                 ADD 1 TO INDX                                            
258502              END-IF                                                      
258800           END-PERFORM                                                    
258900        END-IF                                                            
258920     END-IF                                                               
259000     .                                                                    
259120                                                                          
259121 HE-SHOW-DISTRICT-PARTNO SECTION.                                         
259122     MOVE 'HE-SHOW-DISTRICT-PARTNO' TO CURR-SECTION                       
259130                                                                          
259140     IF MFS-UPDATE                                                        
259150        PERFORM S02-A5-POSITION-EFTER-UPDATE                              
259160     ELSE                                                                 
259170        PERFORM IMS-02-GN-WDA501-ARTIKEL                                  
259180     END-IF                                                               
259190     IF SEGMENT-FINNS                                                     
259200        IF RAD-IDSYSTEM (1:3) IS IDSYS4578                                
259201           MOVE '4578'       TO SPAR-IDTRANS                              
259210           MOVE RAD-IDDISTR  TO SPAR-IDDISTR-ENTER                        
259220           MOVE RAD-IDKUNDNR TO SPAR-IDKUNDNR-ENTER                       
259230           MOVE RAD-IDKUNDRF TO SPAR-IDKUNDRF-ENTER                       
259231           MOVE RAD-IDARTNR  TO SPAR-IDARTNR-ENTER                        
259232           MOVE RAD-IDLOPNR  TO SPAR-IDLOPNR-ENTER                        
259233           MOVE RAD-KDRAPRIO TO SPAR-KDRAPRIO-ENTER                       
259234           MOVE RAD-DARODAT  TO SPAR-DARODAT-ENTER                        
259235           MOVE RAD-TIREGTID TO SPAR-TIREGTID-ENTER                       
259236           MOVE RAD-IDDC     TO SPAR-IDDC-ENTER                           
259237           MOVE +1 TO INDX                                                
259238           PERFORM UNTIL INDX > MAX-INDX                                  
259239              IF SEGMENT-FINNS                                            
259240                 IF RAD-IDSYSTEM (1:3) IS IDSYS4578                       
259241                    PERFORM S01-REDIGERA-ARTIKELRAD                       
259242                    MOVE WS-TIRFS-CDC                                     
259243                             TO SPAR-TIRFSDAT-CDC-ENTER                   
259244                    IF NOT SKIP-RAD-ROW                                   
259245                       ADD 1 TO INDX                                      
259246                    END-IF                                                
259247                 END-IF                                                   
259248                 PERFORM IMS-02-GN-WDA501-ARTIKEL                         
259249              ELSE                                                        
259250                 PERFORM MFS-RENSA-FAELT-UT                               
259251                 ADD 1 TO INDX                                            
259252              END-IF                                                      
259253           END-PERFORM                                                    
259254        END-IF                                                            
259255     ELSE                                                                 
259256        MOVE MSGI-IDDISTR TO SPAR-IDDISTR-ENTER                           
259257        MOVE MSGI-IDKUNDNR TO SPAR-IDKUNDNR-ENTER                         
259258        MOVE MSGI-IDARTNR TO SPAR-IDARTNR-ENTER                           
259259        MOVE MSGI-TIRFSDAT-CDC TO SPAR-TIRFSDAT-CDC-ENTER                 
259260        MOVE MSGI-KDFARLIG TO SPAR-KDFARLIG-ENTER                         
259261        MOVE +1 TO INDX                                                   
259262        PERFORM UNTIL INDX > MAX-INDX                                     
259263          PERFORM MFS-RENSA-FAELT-UT                                      
259264          ADD 1 TO INDX                                                   
259265        END-PERFORM                                                       
259266     END-IF                                                               
259267     .                                                                    
259268                                                                          
259269 HF-SHOW-DANGEROUS       SECTION.                                         
259270     MOVE 'HF-SHOW-DANGEROUS      ' TO CURR-SECTION                       
259271                                                                          
259272     IF MFS-UPDATE                                                        
259273        PERFORM S03-A5A1-POSITION-EFTER-UPDATE                            
259274     ELSE                                                                 
259275        PERFORM IMS-03-GN-WDA5A1                                          
259276     END-IF                                                               
259277     IF SEGMENT-FINNS                                                     
259278        MOVE SEQA-IDWDA501 TO W-WDA5KEY-X                                 
259279        PERFORM IMS-04-GU-WDA501                                          
259280     ELSE                                                                 
259281        MOVE MSGI-IDDISTR TO SPAR-IDDISTR-ENTER                           
259282        MOVE MSGI-IDKUNDNR TO SPAR-IDKUNDNR-ENTER                         
259283        MOVE MSGI-IDARTNR TO SPAR-IDARTNR-ENTER                           
259284        MOVE MSGI-TIRFSDAT-CDC TO SPAR-TIRFSDAT-CDC-ENTER                 
259285        MOVE MSGI-KDFARLIG TO SPAR-KDFARLIG-ENTER                         
259286        MOVE +1 TO INDX                                                   
259287        PERFORM UNTIL INDX > MAX-INDX                                     
259288          PERFORM MFS-RENSA-FAELT-UT                                      
259289          ADD 1 TO INDX                                                   
259290        END-PERFORM                                                       
259291     END-IF                                                               
259292     IF SEGMENT-FINNS                                                     
259293        IF RAD-IDSYSTEM (1:3) IS IDSYS4578                                
259294           MOVE '4578'       TO SPAR-IDTRANS                              
259295           MOVE RAD-IDDISTR  TO SPAR-IDDISTR-ENTER                        
259296           MOVE RAD-IDKUNDNR TO SPAR-IDKUNDNR-ENTER                       
259297           MOVE RAD-IDKUNDRF TO SPAR-IDKUNDRF-ENTER                       
259298           MOVE RAD-IDARTNR  TO SPAR-IDARTNR-ENTER                        
259299           MOVE RAD-IDLOPNR  TO SPAR-IDLOPNR-ENTER                        
259300           MOVE RAD-KDRAPRIO TO SPAR-KDRAPRIO-ENTER                       
259301           MOVE RAD-DARODAT  TO SPAR-DARODAT-ENTER                        
259302           MOVE RAD-TIREGTID TO SPAR-TIREGTID-ENTER                       
259303           MOVE RAD-IDDC     TO SPAR-IDDC-ENTER                           
259304           MOVE +1 TO INDX                                                
259305           PERFORM UNTIL INDX > MAX-INDX                                  
259306              IF SEGMENT-FINNS                                            
259307                 IF RAD-IDSYSTEM (1:3) IS IDSYS4578                       
259308                    IF MSGI-KDFARLIG    = 'Y'                             
259309                       MOVE RAD-IDARTNR TO W-IDARTNR                      
259310                       PERFORM IMS-10-GU-WDK611                           
259311                       IF CLAG-KDFARLIG = 4                               
259312                          PERFORM S01-REDIGERA-ARTIKELRAD                 
259313                          MOVE WS-TIRFS-CDC                               
259314                            TO SPAR-TIRFSDAT-CDC-ENTER                    
259315                          IF NOT SKIP-RAD-ROW                             
259316                             ADD 1 TO INDX                                
259317                          END-IF                                          
259318                       END-IF                                             
259319                    ELSE                                                  
259320                       IF MSGI-KDFARLIG = 'N'                             
259321                          MOVE RAD-IDARTNR TO W-IDARTNR                   
259322                          PERFORM IMS-10-GU-WDK611                        
259323                          IF CLAG-KDFARLIG NOT = 4                        
259324                             PERFORM S01-REDIGERA-ARTIKELRAD              
259325                             MOVE WS-TIRFS-CDC                            
259326                               TO SPAR-TIRFSDAT-CDC-ENTER                 
259327                             IF NOT SKIP-RAD-ROW                          
259328                                ADD 1 TO INDX                             
259330                             END-IF                                       
259331                          END-IF                                          
259336                       END-IF                                             
259337                    END-IF                                                
259338                 END-IF                                                   
259339                                                                          
259340                 PERFORM IMS-03-GN-WDA5A1                                 
259341                 IF SEGMENT-FINNS                                         
259342                    MOVE SEQA-IDWDA501 TO W-WDA5KEY-X                     
259343                    PERFORM IMS-04-GU-WDA501                              
259344                 END-IF                                                   
259345              ELSE                                                        
259346                 PERFORM MFS-RENSA-FAELT-UT                               
259347                 ADD 1 TO INDX                                            
259348              END-IF                                                      
259349           END-PERFORM                                                    
259350        END-IF                                                            
259351     END-IF                                                               
259352     .                                                                    
259353                                                                          
259354 HG-SHOW-RFSDATE         SECTION.                                         
259355     MOVE 'HG-SHOW-RFSDATE        ' TO CURR-SECTION                       
259356                                                                          
259357     IF MFS-UPDATE                                                        
259358        PERFORM S03-A5A1-POSITION-EFTER-UPDATE                            
259359     ELSE                                                                 
259360        PERFORM IMS-03-GN-WDA5A1                                          
259361     END-IF                                                               
259362     IF SEGMENT-FINNS                                                     
259363        MOVE SEQA-IDWDA501 TO W-WDA5KEY-X                                 
259364        PERFORM IMS-04-GU-WDA501                                          
259365     ELSE                                                                 
259366        MOVE MSGI-IDDISTR TO SPAR-IDDISTR-ENTER                           
259367        MOVE MSGI-IDKUNDNR TO SPAR-IDKUNDNR-ENTER                         
259368        MOVE MSGI-IDARTNR TO SPAR-IDARTNR-ENTER                           
259369        MOVE MSGI-TIRFSDAT-CDC TO SPAR-TIRFSDAT-CDC-ENTER                 
259370        MOVE MSGI-KDFARLIG TO SPAR-KDFARLIG-ENTER                         
259371        MOVE +1 TO INDX                                                   
259372        PERFORM UNTIL INDX > MAX-INDX                                     
259373          PERFORM MFS-RENSA-FAELT-UT                                      
259374          ADD 1 TO INDX                                                   
259375        END-PERFORM                                                       
259376     END-IF                                                               
259377     IF SEGMENT-FINNS                                                     
259378        IF RAD-IDSYSTEM (1:3) IS IDSYS4578                                
259379           MOVE '4578'       TO SPAR-IDTRANS                              
259380           MOVE RAD-IDDISTR  TO SPAR-IDDISTR-ENTER                        
259381           MOVE RAD-IDKUNDNR TO SPAR-IDKUNDNR-ENTER                       
259382           MOVE RAD-IDKUNDRF TO SPAR-IDKUNDRF-ENTER                       
259383           MOVE RAD-IDARTNR  TO SPAR-IDARTNR-ENTER                        
259384           MOVE RAD-IDLOPNR  TO SPAR-IDLOPNR-ENTER                        
259385           MOVE RAD-KDRAPRIO TO SPAR-KDRAPRIO-ENTER                       
259386           MOVE RAD-DARODAT  TO SPAR-DARODAT-ENTER                        
259387           MOVE RAD-TIREGTID TO SPAR-TIREGTID-ENTER                       
259388           MOVE RAD-IDDC     TO SPAR-IDDC-ENTER                           
259389           MOVE +1 TO INDX                                                
259390           PERFORM UNTIL INDX > MAX-INDX                                  
259391              IF SEGMENT-FINNS                                            
259392                 IF RAD-IDSYSTEM (1:3) IS IDSYS4578                       
259393                    MOVE 'N'     TO SKIP-REC-SW                           
259394                    PERFORM S01-REDIGERA-ARTIKELRAD                       
259395*   IF RFS-DATE IS NOT SAME AS MID-RFSDATE THEN DELETE THE OUTPUT         
259396*   LINE DONT DISPLAY ON THE MOD-FIELD                                    
259397                    IF WS-TIRFS-CDC NOT = MSGI-TIRFSDAT-CDC               
259409                       PERFORM MFS-RENSA-FAELT-UT                         
259410                       MOVE 'Y'  TO SKIP-REC-SW                           
259412                    END-IF                                                
259413*   IF RFS-DATE IS SAME AS MID-RFSDATE THEN INCREASE THE INDX             
259416                    IF NOT SKIP-RAD-ROW AND SKIP-REC-SW = 'N'             
259417                       MOVE WS-TIRFS-CDC                                  
259418                                 TO SPAR-TIRFSDAT-CDC-ENTER               
259419                       ADD 1 TO INDX                                      
259420                    END-IF                                                
259421                 END-IF                                                   
259422                 PERFORM IMS-03-GN-WDA5A1                                 
259423                 IF SEGMENT-FINNS                                         
259424                    MOVE SEQA-IDWDA501 TO W-WDA5KEY-X                     
259425                    PERFORM IMS-04-GU-WDA501                              
259426                 END-IF                                                   
259427              ELSE                                                        
259428                 PERFORM MFS-RENSA-FAELT-UT                               
259429                 ADD 1 TO INDX                                            
259430              END-IF                                                      
259431           END-PERFORM                                                    
259432        END-IF                                                            
259433     END-IF                                                               
259434     .                                                                    
259435                                                                          
259436 HH-SHOW-DISTRICT-RFSDATE  SECTION.                                       
259437     MOVE 'HH-SHOW-DISTRICT-RFSDATE  ' TO CURR-SECTION                    
259438                                                                          
259439     IF MFS-UPDATE                                                        
259440        PERFORM S02-A5-POSITION-EFTER-UPDATE                              
259441     ELSE                                                                 
259442        PERFORM IMS-01-GN-WDA501                                          
259443     END-IF                                                               
259444     IF SEGMENT-FINNS                                                     
259445        IF RAD-IDSYSTEM (1:3) IS IDSYS4578                                
259446        MOVE '4578'       TO SPAR-IDTRANS                                 
259447        MOVE RAD-IDDISTR  TO SPAR-IDDISTR-ENTER                           
259448        MOVE RAD-IDKUNDNR TO SPAR-IDKUNDNR-ENTER                          
259449        MOVE RAD-IDKUNDRF TO SPAR-IDKUNDRF-ENTER                          
259450        MOVE RAD-IDARTNR  TO SPAR-IDARTNR-ENTER                           
259451        MOVE RAD-IDLOPNR  TO SPAR-IDLOPNR-ENTER                           
259452        MOVE RAD-KDRAPRIO TO SPAR-KDRAPRIO-ENTER                          
259453        MOVE RAD-DARODAT  TO SPAR-DARODAT-ENTER                           
259454        MOVE RAD-TIREGTID TO SPAR-TIREGTID-ENTER                          
259455        MOVE RAD-IDDC     TO SPAR-IDDC-ENTER                              
259456        MOVE +1 TO INDX                                                   
259457        PERFORM UNTIL INDX > MAX-INDX                                     
259458          IF SEGMENT-FINNS                                                
259459                MOVE 'N'  TO SKIP-REC-SW                                  
259460                PERFORM S01-REDIGERA-ARTIKELRAD                           
259461*   IF RFS-DATE IS NOT SAME AS MID-RFSDATE THEN DELETE THE OUTPUT         
259462*   LINE DONT DISPLAY ON THE MOD-FIELD                                    
259463                IF WS-TIRFS-CDC NOT= MSGI-TIRFSDAT-CDC                    
259464                   MOVE 'Y' TO SKIP-REC-SW                                
259467                   PERFORM MFS-RENSA-FAELT-UT                             
259469                END-IF                                                    
259470*   IF RFS-DATE IS SAME AS MID-RFSDATE THEN INCREASE THE INDX             
259471                IF NOT SKIP-RAD-ROW AND SKIP-REC-SW='N'                   
259472                MOVE WS-TIRFS-CDC                                         
259473                                  TO SPAR-TIRFSDAT-CDC-ENTER              
259474                   ADD 1  TO INDX                                         
259475                END-IF                                                    
259476              PERFORM IMS-01-GN-WDA501                                    
259478          ELSE                                                            
259480             PERFORM MFS-RENSA-FAELT-UT                                   
259481             ADD 1        TO INDX                                         
259482          END-IF                                                          
259484        END-PERFORM                                                       
259485     ELSE                                                                 
259486        MOVE MSGI-IDDISTR TO SPAR-IDDISTR-ENTER                           
259487        MOVE MSGI-IDKUNDNR TO SPAR-IDKUNDNR-ENTER                         
259488        MOVE MSGI-IDARTNR TO SPAR-IDARTNR-ENTER                           
259489        MOVE MSGI-TIRFSDAT-CDC TO SPAR-TIRFSDAT-CDC-ENTER                 
259490        MOVE MSGI-KDFARLIG TO SPAR-KDFARLIG-ENTER                         
259491        MOVE +1 TO INDX                                                   
259492        PERFORM UNTIL INDX > MAX-INDX                                     
259493          PERFORM MFS-RENSA-FAELT-UT                                      
259494          ADD 1 TO INDX                                                   
259495        END-PERFORM                                                       
259496     END-IF                                                               
259497     .                                                                    
259498                                                                          
259499 HI-SHOW-DANG-RFSDATE    SECTION.                                         
259500     MOVE 'HI-SHOW-DANG-RFSDATE   ' TO CURR-SECTION                       
259501                                                                          
259502     IF MFS-UPDATE                                                        
259503        PERFORM S03-A5A1-POSITION-EFTER-UPDATE                            
259504     ELSE                                                                 
259505        PERFORM IMS-03-GN-WDA5A1                                          
259506     END-IF                                                               
259507     IF SEGMENT-FINNS                                                     
259508        MOVE SEQA-IDWDA501 TO W-WDA5KEY-X                                 
259509        PERFORM IMS-04-GU-WDA501                                          
259510     ELSE                                                                 
259511        MOVE MSGI-IDDISTR TO SPAR-IDDISTR-ENTER                           
259512        MOVE MSGI-IDKUNDNR TO SPAR-IDKUNDNR-ENTER                         
259513        MOVE MSGI-IDARTNR TO SPAR-IDARTNR-ENTER                           
259514        MOVE MSGI-TIRFSDAT-CDC TO SPAR-TIRFSDAT-CDC-ENTER                 
259515        MOVE MSGI-KDFARLIG TO SPAR-KDFARLIG-ENTER                         
259516        MOVE +1 TO INDX                                                   
259517        PERFORM UNTIL INDX > MAX-INDX                                     
259518          PERFORM MFS-RENSA-FAELT-UT                                      
259519          ADD 1 TO INDX                                                   
259520        END-PERFORM                                                       
259521     END-IF                                                               
259522     IF SEGMENT-FINNS                                                     
259523        IF RAD-IDSYSTEM (1:3) IS IDSYS4578                                
259524           MOVE '4578'       TO SPAR-IDTRANS                              
259525           MOVE RAD-IDDISTR  TO SPAR-IDDISTR-ENTER                        
259526           MOVE RAD-IDKUNDNR TO SPAR-IDKUNDNR-ENTER                       
259527           MOVE RAD-IDKUNDRF TO SPAR-IDKUNDRF-ENTER                       
259528           MOVE RAD-IDARTNR  TO SPAR-IDARTNR-ENTER                        
259529           MOVE RAD-IDLOPNR  TO SPAR-IDLOPNR-ENTER                        
259530           MOVE RAD-KDRAPRIO TO SPAR-KDRAPRIO-ENTER                       
259531           MOVE RAD-DARODAT  TO SPAR-DARODAT-ENTER                        
259532           MOVE RAD-TIREGTID TO SPAR-TIREGTID-ENTER                       
259533           MOVE RAD-IDDC     TO SPAR-IDDC-ENTER                           
259534           MOVE +1 TO INDX                                                
259535           PERFORM UNTIL INDX > MAX-INDX                                  
259536              IF SEGMENT-FINNS                                            
259537                 IF RAD-IDSYSTEM (1:3) IS IDSYS4578                       
259538                    MOVE 'N'               TO SKIP-REC-SW                 
259539                    IF MSGI-KDFARLIG  = 'Y'                               
259540                       MOVE RAD-IDARTNR  TO W-IDARTNR                     
259541                       PERFORM IMS-10-GU-WDK611                           
259542                       IF CLAG-KDFARLIG = 4                               
259543                          PERFORM S01-REDIGERA-ARTIKELRAD                 
259544*   IF RFS-DATE IS NOT SAME MID-RFSDATE AND KDFARLIG IS ALSO 4            
259545                          IF WS-TIRFS-CDC NOT = MSGI-TIRFSDAT-CDC         
259546                             MOVE 'Y'     TO SKIP-REC-SW                  
259547                             PERFORM MFS-RENSA-FAELT-UT                   
259548                          END-IF                                          
259549*   IF RFS-DATE IS SAME AS MID-RFSDATE AND KDFARLIG IS ALSO 4             
259550                          IF NOT SKIP-RAD-ROW AND SKIP-REC-SW ='N'        
259551                             MOVE WS-TIRFS-CDC                            
259552                                    TO SPAR-TIRFSDAT-CDC-ENTER            
259553                             ADD 1 TO INDX                                
259554                          END-IF                                          
259555                       END-IF                                             
259556                    ELSE                                                  
259557                       IF MSGI-KDFARLIG  = 'N'                            
259558                          MOVE RAD-IDARTNR TO W-IDARTNR                   
259559                          PERFORM IMS-10-GU-WDK611                        
259560                          IF CLAG-KDFARLIG NOT = 4                        
259561                             PERFORM S01-REDIGERA-ARTIKELRAD              
259562                             IF WS-TIRFS-CDC NOT =                        
259563                                                MSGI-TIRFSDAT-CDC         
259564                                MOVE 'Y'   TO SKIP-REC-SW                 
259565                                PERFORM MFS-RENSA-FAELT-UT                
259566                             END-IF                                       
259567                             IF NOT SKIP-RAD-ROW AND                      
259568                                    SKIP-REC-SW ='N'                      
259569                                MOVE WS-TIRFS-CDC                         
259570                                        TO SPAR-TIRFSDAT-CDC-ENTER        
259571                                ADD 1 TO INDX                             
259572                             END-IF                                       
259573                          END-IF                                          
259574                       END-IF                                             
259575                    END-IF                                                
259576                 END-IF                                                   
259577                                                                          
259578                 PERFORM IMS-03-GN-WDA5A1                                 
259579                 IF SEGMENT-FINNS                                         
259580                    MOVE SEQA-IDWDA501 TO W-WDA5KEY-X                     
259581                    PERFORM IMS-04-GU-WDA501                              
259582                 END-IF                                                   
259583              ELSE                                                        
259584                 PERFORM MFS-RENSA-FAELT-UT                               
259585                 ADD 1 TO INDX                                            
259586              END-IF                                                      
259587           END-PERFORM                                                    
259588        END-IF                                                            
259589     END-IF                                                               
259590     .                                                                    
259591                                                                          
259592 S01-REDIGERA-ARTIKELRAD SECTION.                                         
259593     MOVE 'S01-REDIGERA-ARTI' TO CURR-SECTION                             
259594                                                                          
259595     IF RAD-KDTPOTYP > +0                                                 
259596       SET SKIP-RAD-ROW  TO TRUE                                          
259597     ELSE                                                                 
259598       MOVE RAD-IDARTNR  TO W-IDARTNR                                     
259599       PERFORM IMS-10-GU-WDK611                                           
259600                                                                          
259802       IF CLAG-KDUART = 'P' OR 'L' OR 'M' OR 'S'                          
259902             SET SKIP-RAD-ROW   TO TRUE                                   
260310       ELSE                                                               
260402         IF W-IDDISTR-N9 = ZERO                                           
260502            MOVE RAD-IDDISTR     TO W-IDDISTR-N9                          
260602            MOVE RAD-IDKUNDNR    TO W-IDKUNDNR-N9                         
260702            MOVE '00'            TO W-IDKUNDRF-N9(1:2)                    
260802            MOVE RAD-IDKUNDRF    TO W-IDKUNDRF-N9(3:5)                    
260902            PERFORM IMS-24-GU-ORQI01-CSEQ                                 
261002         ELSE                                                             
261102            IF ORQI-OHUV-IDDISTR  = W-IDDISTR-N9 AND                      
261202               ORQI-OHUV-IDKUNDNR = W-IDKUNDNR-N9 AND                     
261302               ORQI-OHUV-IDKUNDRF = W-IDKUNDRF-N9(3:5)                    
261402                                                                          
261502               CONTINUE                                                   
261602            ELSE                                                          
261702               MOVE RAD-IDDISTR  TO W-IDDISTR-N9                          
261802               MOVE RAD-IDKUNDNR TO W-IDKUNDNR-N9                         
261902               MOVE '00'         TO W-IDKUNDRF-N9(1:2)                    
262002               MOVE RAD-IDKUNDRF TO W-IDKUNDRF-N9(3:5)                    
262102               PERFORM IMS-24-GU-ORQI01-CSEQ                              
262202            END-IF                                                        
262302         END-IF                                                           
262402         IF RAD-IDDISTR  NOT = W-IDDISTR-WDB2 OR                          
262502            RAD-IDKUNDNR NOT = W-IDKUNDNR-WDB2                            
262602                                                                          
262702            MOVE RAD-IDDISTR     TO W-IDDISTR-WDB2                        
262802            MOVE RAD-IDKUNDNR    TO W-IDKUNDNR-WDB2                       
262902            PERFORM IMS-07-GU-WDB201                                      
263002         END-IF                                                           
263102                                                                          
263202         MOVE 001                 TO WORK-KDCALL                          
263302         MOVE GMT-IDDC-BULK(1)    TO WORK-IDDC                            
263402         MOVE ORQI-OHUV-TIREGDAT  TO WORK-TIAAMMDD-FOM                    
263502         MOVE RAD-TIREPDAT        TO WORK-TIAAMMDD-TOM                    
263602         CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR        
263702         IF WORK-KDSVAR-FEL                                               
263802            MOVE ZERO TO WORK-KVWORKD                                     
263902         END-IF                                                           
264002                                                                          
264003         IF CLAG-KDFARLIG = 4                                             
264004            MOVE 'Y'              TO MOD-KDFARLIG       (INDX)            
264005                                     SPAR-KDFARLIG-RAD   (INDX)           
264006                                                                          
264007            MOVE MFS-ADD-HILIGHT-FIELD                                    
264008                                   TO MOD-KDFARLIG-ATTR  (INDX)           
264009         ELSE                                                             
264010            MOVE 'N'               TO MOD-KDFARLIG       (INDX)           
264011                                      SPAR-KDFARLIG-RAD  (INDX)           
264020         END-IF                                                           
264102         MOVE RAD-IDDISTR          TO MOD-IDDISTR      (INDX)             
264202                                      SPAR-IDDISTR-RAD (INDX)             
264302                                      W-IDDISTR-DC                        
264402                                      W-IDDISTR-DC-DEF                    
264502                                                                          
264602         MOVE RAD-IDKUNDNR         TO   MOD-IDKUNDNR     (INDX)           
264702                                        SPAR-IDKUNDNR-RAD(INDX)           
264802                                        W-IDKUNDNR-DC                     
264902                                                                          
265002         MOVE RAD-IDARTNR          TO   MOD-IDARTNR      (INDX)           
265102                                        SPAR-IDARTNR-RAD (INDX)           
265202         MOVE RAD-KVART            TO   MOD-KVART        (INDX)           
265302         MOVE RAD-IDORDNR5         TO   MOD-IDORDNR5     (INDX)           
265402                                          SPAR-IDORDNR-RAD (INDX)         
265502         MOVE RAD-KDORDKL          TO   MOD-KDORDKL      (INDX)           
266002                                                                          
266202         MOVE GMT-IDDC-BULK(1)     TO   WS-IDDC                           
266302         PERFORM S01A-SKAPA-RFSDATUM                                      
266304         PERFORM S03A-SKAPA-RFSDATUM                                      
266402         MOVE WS-TIRFS             TO   MOD-TIRFSDAT-LDC (INDX)           
266403         MOVE WS-TIRFS-CDC        TO SPAR-TIRFSDAT-CDC-RAD (INDX)         
266405                                        MOD-TIRFSDAT-CDC (INDX)           
266502         MOVE RAD-TIREPDAT         TO   MOD-TIREPDAT (INDX)               
266602         IF MFS-SPLIT                                                     
266702            MOVE 'CO TIM'          TO   MOD-RUBVAR                        
266802         END-IF                                                           
266902         IF MOD-RUBVAR = 'LDCRFS'                                         
267002            MOVE RAD-IDKUNDRF-WIP  TO   MOD-IDKUNDRF-WIP (INDX)           
267102         ELSE                                                             
267202            PERFORM S01C-GET-CUT-OFF-TIME                                 
267302            IF W-CUT-OFF > ZERO                                           
267402               MOVE W-CUT-OFF-RED  TO   MOD-TIRFSDAT-LDC (INDX)           
267502            ELSE                                                          
267602               MOVE W-CUT-OFF-NOT-FOUND                                   
267603                                   TO MOD-TIRFSDAT-LDC (INDX)             
267702            END-IF                                                        
267802         END-IF                                                           
268902         MOVE GMT-IDDC-BULK(1)     TO   MOD-IDDC-PRIM    (INDX)           
269002         MOVE SPACE                TO   MOD-BELAGINS     (INDX)           
269102         PERFORM S01B-KOMMENTAR                                           
269202         IF 4564-BETEXT-010 = SPACE                                       
269203           MOVE MFS-ADD-HILIGHT-FIELD                                     
269204                                   TO   MOD-IDDISTR-ATTR (INDX)           
269206         END-IF                                                           
269208         MOVE 4564-BETEXT-010      TO   MOD-BETEXT       (INDX)           
269302                                        SPAR-BETEXT-RAD  (INDX)           
269402         MOVE RAD-IDLOPNR          TO  SPAR-IDLOPNR-RAD (INDX)            
269403         MOVE 'N'                  TO  TPO-BLOCK-SW                       
269502       END-IF                                                             
269503     END-IF                                                               
269702     .                                                                    
269802     EJECT                                                                
269902 S01A-SKAPA-RFSDATUM SECTION.                                             
270002     MOVE 'S01A-SKAPA-RFSDAT' TO CURR-SECTION                             
270102                                                                          
270202     MOVE WS-IDDC                  TO WORK-IDDC                           
270302*    MOVE RAD-IDDC                 TO WORK-IDDC                           
270303*LK  MOVE GMT-IDDC-BULK(1)         TO WORK-IDDC                           
270402     MOVE +002                     TO WORK-KDCALL                         
270502     MOVE +001                     TO WORK-KVWORKD                        
270602     IF  RAD-TIREPDAT     = ZERO                                          
270702        MOVE WS-DAT                TO WORK-TIAAMMDD-FOM                   
270802     ELSE                                                                 
270902        MOVE RAD-TIREPDAT          TO WORK-TIAAMMDD-FOM                   
271002     END-IF                                                               
271102     CALL WORKDAY                  USING WORK-KDCALL                      
271202                                         WORK-DATE-AREA                   
271302                                         WORK-KDSVAR                      
271402     IF WORK-KDSVAR-FEL                                                   
271502        MOVE 'SECT S01-1, DATUM SAKNAS I WORKDAY'                         
271602                                   TO FELTEXT                             
271702        CALL ABEND                 USING RKOD-ABEND-UTAN-DUMP             
271802     ELSE                                                                 
271902       MOVE +003                   TO WORK-KDCALL                         
272002       MOVE GMT-KVDAGAR-RFS-DEF    TO WORK-KVWORKD                        
272102       PERFORM                                                            
272202       VARYING RFS-IX FROM 1 BY 1                                         
272302         UNTIL RFS-IX > MAX-RFS-IX                                        
272402         IF GMT-IDDC-RFS (RFS-IX) = WORK-IDDC                             
272502           MOVE GMT-KVDAGAR-RFS (RFS-IX)                                  
272602                                   TO WORK-KVWORKD                        
272702         END-IF                                                           
272802       END-PERFORM                                                        
272902       ADD +1  TO WORK-KVWORKD                                            
273002*      +1 FÖR ATT VARIABELN SKALL KUNNA INNEHÅLLA                         
273102*      ANTAL DAGAR FÖRE RFS.                                              
273202*      0 GER DÅ SAMMA DAG, 1 GER FÖRSTA ARBETSDAG FÖRE OSV...             
273302*      OM VI INTE ADDERAR +1 SKULLE VARIABELN SÄTTAS SÅ                   
273402*      1 GER SAMMA DAG, 2 FÖRSTA ARBETSDAG FÖRE OSV...                    
273502*                                                                         
273602                                                                          
273702       CALL WORKDAY                USING WORK-KDCALL                      
273802                                         WORK-DATE-AREA                   
273902                                         WORK-KDSVAR                      
274002       IF WORK-KDSVAR-FEL                                                 
274102          MOVE 'SECT S01-2, DATUM SAKNAS I WORKDAY'                       
274202                                   TO FELTEXT                             
274302          CALL ABEND               USING RKOD-ABEND-UTAN-DUMP             
274402       ELSE                                                               
274502         IF WORK-TIAAMMDD-FOM < WS-DAT                                    
274602           MOVE WS-IDDC            TO WORK-IDDC                           
274702           MOVE +002               TO WORK-KDCALL                         
274802           MOVE +001               TO WORK-KVWORKD                        
274902           MOVE WS-DAT             TO WORK-TIAAMMDD-FOM                   
275002           CALL WORKDAY            USING WORK-KDCALL                      
275102                                         WORK-DATE-AREA                   
275202                                         WORK-KDSVAR                      
275302           IF WORK-KDSVAR-FEL                                             
275402              MOVE 'SECT S01-3, DATUM SAKNAS I WORKDAY'                   
275502                                   TO FELTEXT                             
275602              CALL ABEND           USING RKOD-ABEND-UTAN-DUMP             
275702           ELSE                                                           
275802              MOVE WORK-TIAAMMDD-TOM TO WS-TIRFS                          
275902           END-IF                                                         
276002         ELSE                                                             
276102           MOVE WORK-TIAAMMDD-FOM  TO WS-TIRFS                            
276202         END-IF                                                           
276302       END-IF                                                             
276402     END-IF                                                               
276502     .                                                                    
276602     EJECT                                                                
276702 S01B-KOMMENTAR      SECTION.                                             
276802     MOVE 'S01B-KOMMENTAR   ' TO CURR-SECTION                             
276902                                                                          
277002     MOVE RAD-IDGMTREF    TO W-IDGMTREF-4564                              
277102     MOVE RAD-IDARTNR     TO W-IDARTNR-4564                               
277202     MOVE RAD-IDLOPNR     TO W-IDLOPNR-4564                               
277302                                                                          
277402     PERFORM IMS-09-GU-WDR5                                               
277502     IF SEGMENT-SAKNAS                                                    
277602        MOVE SPACE TO 4564-BETEXT-010                                     
277702     END-IF                                                               
277802     .                                                                    
277902     EJECT                                                                
278002                                                                          
278102 S01C-GET-CUT-OFF-TIME SECTION.                                           
278202     MOVE 'S01C-GET-CUT-OFF' TO CURR-SECTION                              
278302                                                                          
278402     PERFORM S01CA-GET-TRP-AND-TIME                                       
278502     IF SEGMENT-FINNS                                                     
278602        PERFORM S01CB-GET-DEPARTURE-TIME                                  
278702*       IF SEGMENT-FINNS                                                  
278802*       ELSE                                                              
278902*       END-IF                                                            
279002     END-IF                                                               
279102     .                                                                    
279202     EJECT                                                                
279302                                                                          
279402 S01CA-GET-TRP-AND-TIME SECTION.                                          
279502     MOVE 'S01CA-GET-IDTRP' TO CURR-SECTION                               
279602                                                                          
279702     MOVE RAD-IDDC             TO W-IDDC-WDB5                             
279802     MOVE RAD-KDFRAKT          TO W-KDFRAKT-WDB5                          
279902     MOVE RAD-IDDISTR          TO W-IDDISTR-WDB5                          
280002     MOVE RAD-IDKUNDNR         TO W-IDKUNDNR-WDB5                         
280102     PERFORM IMS-GU-WDB501                                                
280202     IF SEGMENT-SAKNAS                                                    
280302        MOVE 9999999           TO W-IDKUNDNR-WDB5                         
280402        PERFORM IMS-GU-WDB501                                             
280502     END-IF                                                               
280602     IF SEGMENT-FINNS                                                     
280702        IF RAD-KDORDKL = 0                                                
280802           MOVE FK-IDTRP-0             TO W-IDTRP                         
280902           COMPUTE W-KVLEDTIM = DC-KVLEDTIM-0 * 100                       
281002        ELSE                                                              
281102           IF RAD-KDORDKL = 1                                             
281202              MOVE FK-IDTRP-1          TO W-IDTRP                         
281302              COMPUTE W-KVLEDTIM = DC-KVLEDTIM-1 * 100                    
281402           ELSE                                                           
281502              IF RAD-KDORDKL = 2                                          
281602                 MOVE FK-IDTRP-2       TO W-IDTRP                         
281702                 COMPUTE W-KVLEDTIM = DC-KVLEDTIM-2 * 100                 
281802              ELSE                                                        
281902                 IF RAD-KDORDKL = 3                                       
282002                    MOVE FK-IDTRP-3    TO W-IDTRP                         
282102                    COMPUTE W-KVLEDTIM = DC-KVLEDTIM-3 * 100              
282202                 ELSE                                                     
282302                    IF RAD-KDORDKL = 4                                    
282402                       MOVE FK-IDTRP-4 TO W-IDTRP                         
282502                       COMPUTE W-KVLEDTIM = DC-KVLEDTIM-4 * 100           
282602                    END-IF                                                
282702                 END-IF                                                   
282802              END-IF                                                      
282902           END-IF                                                         
283002        END-IF                                                            
283102     END-IF                                                               
283202     .                                                                    
283302     EJECT                                                                
283402                                                                          
283502 S01CB-GET-DEPARTURE-TIME SECTION.                                        
283602     MOVE 'S01CB-DEP-TIME ' TO CURR-SECTION                               
283702                                                                          
283802     MOVE    WC-CDC-SE        TO W-4433-IDDC                              
283902     PERFORM IMS-GU-WDR101                                                
284002     MOVE    W-IDTRP          TO W-4434-IDTRP-MIN                         
284102                                 W-4434-IDTRP-MAX                         
284202     MOVE ZERO                TO W-4434-TITRPAVG-MIN                      
284302                                 W-FIRST-CUT-OFF                          
284402*    In W-WDGXKEY-4434-MAX we have W-4434-TITRPAVG-MAX = 9999             
284502*    as we are only looking for daily transports.                         
284602                                                                          
284702     MOVE ZERO TO W-CUT-OFF                                               
284802     PERFORM IMS-GNP-WDR130                                               
284902                                                                          
285002     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
285102              W-CUT-OFF > 0                                               
285202        MOVE 4434-TITRPAVG  TO W-FIRST-CUT-OFF                            
285203                               W-TITRPAVG                                 
285204        IF W-TITRP-MM < W-LEDTIM-MM                                       
285402           ADD 40 TO W-KVLEDTIM                                           
285403        END-IF                                                            
285500                                                                          
285502        COMPUTE W-CUT-OFF = 4434-TITRPAVG - W-KVLEDTIM                    
285602        IF DAGENS-DATUM < WS-TIRFS                                        
285702           MOVE W-CUT-OFF-TT TO W-RED-TT                                  
285802           MOVE W-CUT-OFF-MM TO W-RED-MM                                  
285902        ELSE                                                              
286002           IF CURR-TTMM < W-CUT-OFF                                       
286102              MOVE W-CUT-OFF-TT TO W-RED-TT                               
286202              MOVE W-CUT-OFF-MM TO W-RED-MM                               
286302           ELSE                                                           
286402              MOVE ZERO TO W-CUT-OFF                                      
286502              PERFORM IMS-GNP-WDR130                                      
286602           END-IF                                                         
286702        END-IF                                                            
286802     END-PERFORM                                                          
286902     .                                                                    
287002     EJECT                                                                
287102                                                                          
287103 S03A-SKAPA-RFSDATUM SECTION.                                             
287104     MOVE 'S03A-SKAPA-RFSDAT' TO CURR-SECTION                             
287106                                                                          
287108     MOVE 'N'                      TO WS-RFS-DATE-SW                      
287111     MOVE +003                     TO WORK-KDCALL                         
287112     MOVE 11                       TO WORK-IDDC                           
287113                                                                          
287114       IF  RAD-TIREPDAT     = ZERO                                        
287115          MOVE WS-DAT              TO WORK-TIAAMMDD-TOM                   
287116       ELSE                                                               
287117          MOVE RAD-TIREPDAT        TO WORK-TIAAMMDD-TOM                   
287118          PERFORM S03AA-CALC-DC11RFS                                      
287119       END-IF                                                             
287120                                                                          
287206       PERFORM                                                            
287207       VARYING RFS-IX FROM 1 BY 1                                         
287208         UNTIL RFS-IX > MAX-RFS-IX                                        
287209         IF GMT-IDDC-RFS (RFS-IX) = WORK-IDDC                             
287210           MOVE GMT-KVDAGAR-RFS (RFS-IX)                                  
287211                                   TO WORK-KVWORKD                        
287215           MOVE 'Y'                TO WS-RFS-DATE-SW                      
287216         END-IF                                                           
287217       END-PERFORM                                                        
287219       IF WS-RFS-DATE-SW = 'N'                                            
287221          MOVE GMT-KVDAGAR-RFS-DEF  TO WORK-KVWORKD                       
287223       END-IF                                                             
287224                                                                          
287225       IF HOLIDAY-SW = NEJ                                                
287226          ADD +1  TO WORK-KVWORKD                                         
287227       END-IF                                                             
287228                                                                          
287237                                                                          
287238       CALL WORKDAY                USING WORK-KDCALL                      
287239                                         WORK-DATE-AREA                   
287240                                         WORK-KDSVAR                      
287250       MOVE WORK-TIAAMMDD-FOM  TO WS-TIRFS-CDC                            
287256     .                                                                    
287257     EJECT                                                                
287258******************************************************************        
287259*STEPS TO CALC CDCRFS IF REPDATE FALLS ON BANK HOLIDAY/WORKING DAY        
287260**                                                                        
287261*CALL WORKDAY WITH REPDATE IN TODATE & PASS NO OF DAYS AS 001             
287262*IF FOM AND TOM HAS SAME DATE,THEN IT IS WORKDAY,ADD +1 TO KVWORD         
287263*IF FOM NOT SAME AS TOM, IT IS A HOLIDAY,SKIP ADDING +1 TO KVWORD         
287264*& MOVE FOM TO TOM AS REPDATE TO CALC CDC RFS                             
287265******************************************************************        
287266 S03AA-CALC-DC11RFS SECTION.                                              
287267     MOVE 'S03AA-CALC-DC11' TO CURR-SECTION                               
287268                                                                          
287269     MOVE +1                TO WORK-KVWORKD                               
287270     MOVE NEJ               TO HOLIDAY-SW                                 
287271                                                                          
287272     CALL WORKDAY                USING WORK-KDCALL                        
287273                                       WORK-DATE-AREA                     
287274                                       WORK-KDSVAR                        
287275                                                                          
287276     IF WORK-TIAAMMDD-FOM   = WORK-TIAAMMDD-TOM                           
287277        CONTINUE                                                          
287278     ELSE                                                                 
287279        MOVE WORK-TIAAMMDD-FOM  TO WORK-TIAAMMDD-TOM                      
287280        SET ITS-HOLIDAY TO TRUE                                           
287281     END-IF                                                               
287282     .                                                                    
287283     EJECT                                                                
287290 S02-A5-POSITION-EFTER-UPDATE SECTION.                                    
287302     MOVE 'S02-A5-POSITION' TO CURR-SECTION                               
287402                                                                          
287502     MOVE SPACE                TO W-WDA5KEY-X                             
287602     IF SPAR-IDDISTR-RAD (1) > ZERO                                       
287702        MOVE SPAR-IDDISTR-RAD (1) TO W-A5-IDDISTR-KEY                     
287802        MOVE SPAR-IDKUNDNR-RAD(1) TO W-A5-IDKUNDNR-KEY                    
287902        MOVE SPAR-IDORDNR-RAD (1) TO W-A5-IDORDNR-KEY                     
288002        MOVE SPAR-IDARTNR-RAD (1) TO W-A5-IDARTNR-KEY                     
288102        MOVE SPAR-IDLOPNR-RAD (1) TO W-A5-IDLOPNR-KEY                     
288208*    ELSE                                                                 
288302*       MOVE SPAR-IDDISTR-ENTER   TO W-A5-IDDISTR-KEY                     
288402*       MOVE SPAR-IDKUNDNR-ENTER  TO W-A5-IDKUNDNR-KEY                    
288502*       MOVE SPAR-IDORDNR-ENTER   TO W-A5-IDORDNR-KEY                     
288602*       MOVE SPAR-IDARTNR-ENTER   TO W-A5-IDARTNR-KEY                     
288702*       MOVE SPAR-IDLOPNR-ENTER   TO W-A5-IDLOPNR-KEY                     
288802     END-IF                                                               
288902                                                                          
289002     PERFORM IMS-04-GU-WDA501                                             
289102     .                                                                    
289202     EJECT                                                                
289302                                                                          
289402 S03-A5A1-POSITION-EFTER-UPDATE SECTION.                                  
289502     MOVE 'S03-A5A1-POSITION' TO CURR-SECTION                             
289602                                                                          
289702     MOVE SPACE                TO W-WDA5KEY-X                             
289802     IF SPAR-IDDISTR-RAD (1) > ZERO                                       
289902        MOVE SPAR-IDDISTR-RAD (1) TO W-A5-IDDISTR-KEY                     
290002        MOVE SPAR-IDKUNDNR-RAD(1) TO W-A5-IDKUNDNR-KEY                    
290102        MOVE SPAR-IDORDNR-RAD (1) TO W-A5-IDORDNR-KEY                     
290202        MOVE SPAR-IDARTNR-RAD (1) TO W-A5-IDARTNR-KEY                     
290302        MOVE SPAR-IDLOPNR-RAD (1) TO W-A5-IDLOPNR-KEY                     
290402     END-IF                                                               
290502                                                                          
290602     PERFORM IMS-04-GU-WDA501                                             
290702     IF SEGMENT-FINNS                                                     
290802        MOVE RAD-IDARTNR    TO W-WDA5A1-IDARTNR                           
290902        MOVE RAD-IDDC       TO W-WDA5A1-IDDC                              
291002        MOVE RAD-KDRAPRIO   TO W-WDA5A1-KDRAPRIO                          
291102        MOVE RAD-DARODAT    TO W-WDA5A1-DARODAT                           
291202        MOVE RAD-TIREGTID   TO W-WDA5A1-TIREGTID                          
291302        MOVE RAD-IDDISTR    TO W-WDA5A1-IDDISTR                           
291402        MOVE RAD-IDKUNDNR   TO W-WDA5A1-IDKUNDNR                          
291502        MOVE RAD-IDKUNDRF   TO W-WDA5A1-IDKUNDRF                          
291602        MOVE RAD-IDLOPNR    TO W-WDA5A1-IDLOPNR                           
291702        PERFORM IMS-32-GU-WDA5A1                                          
291802     END-IF                                                               
291902     .                                                                    
292002     EJECT                                                                
292102                                                                          
292202 S04-B2C1-POSITION-EFTER-UPDATE SECTION.                                  
292302     MOVE 'S04-B2C1-POSITION' TO CURR-SECTION                             
292402                                                                          
292502     MOVE SPACE                TO W-WDA5KEY-X                             
292602     IF SPAR-IDDISTR-RAD (1) > ZERO                                       
292702        MOVE ZERO                 TO W-A5-IDDISTR-KEY                     
292802        MOVE ZERO                 TO W-A5-IDKUNDNR-KEY                    
292902        MOVE SPAR-IDORDNR-RAD (1) TO W-A5-IDORDNR-KEY                     
293002        MOVE SPAR-IDARTNR-RAD (1) TO W-A5-IDARTNR-KEY                     
293102        MOVE SPAR-IDLOPNR-RAD (1) TO W-A5-IDLOPNR-KEY                     
293202     END-IF                                                               
293302                                                                          
293402     PERFORM IMS-04-GU-WDA501                                             
293502*    VI LÄSER BARA FÖR ATT POSITIONERA OSS I BÖRJAN                       
293602*                                                                         
293702     MOVE MSGI-IDDC-BULK       TO W-WDB2C1-IDDC                           
293802     MOVE SPAR-IDDISTR-RAD (1) TO W-WDB2C1-IDDISTR                        
293902     MOVE SPAR-IDKUNDNR-RAD(1) TO W-WDB2C1-IDKUNDNR                       
294002     PERFORM IMS-33-GU-WDB2C1                                             
294102     .                                                                    
294202     EJECT                                                                
294302                                                                          
294402                                                                          
294403 S06-GET-KDFRAKT SECTION.                                                 
294404     MOVE 'S06-GET-KDFRAKT       ' TO CURR-SECTION                        
294405                                                                          
294406     MOVE +1                       TO DC-INDX                             
294407     MOVE 'N'                      TO IDDC-BULK-SW                        
294408                                      IDDC-DAY-SW                         
294409                                                                          
294410     PERFORM UNTIL DC-INDX > MAX-INDX OR IDDC-BULK-FOUND                  
294411        IF GMT-IDDC-BULK (DC-INDX) = WS-IDDC-GMT                          
294420          MOVE GMT-IDDC-BULK (DC-INDX)  TO W-IDDC                         
294430                                           W-IDDC-DEF                     
294440          SET IDDC-BULK-FOUND TO TRUE                                     
294450        END-IF                                                            
294451        ADD +1                TO DC-INDX                                  
294460     END-PERFORM                                                          
294470                                                                          
294480     IF IDDC-BULK-SW = 'N'                                                
294490       MOVE +1 TO DC-INDX                                                 
294500       PERFORM UNTIL DC-INDX > MAX-INDX  OR IDDC-DAY-FOUND                
294501          IF GMT-IDDC-DAY (DC-INDX) = WS-IDDC-GMT                         
294502            MOVE GMT-IDDC-DAY (DC-INDX)  TO W-IDDC                        
294503                                            W-IDDC-DEF                    
294504            SET IDDC-DAY-FOUND TO TRUE                                    
294505          END-IF                                                          
294506          ADD +1                TO DC-INDX                                
294507       END-PERFORM                                                        
294508     END-IF                                                               
294509                                                                          
294510     IF IDDC-BULK-FOUND OR IDDC-DAY-FOUND                                 
294511       PERFORM IMS-GU-WDB301                                              
294512       IF SEGMENT-FINNS                                                   
294513         IF IDDC-BULK-FOUND                                               
294514           MOVE DC-KDGENFRA-MO           TO WS-KDFRAKT                    
294515         ELSE                                                             
294516           MOVE DC-KDGENFRA-DO           TO WS-KDFRAKT                    
294517         END-IF                                                           
294518       ELSE                                                               
294519         MOVE ZEROS                      TO WS-KDFRAKT                    
294520       END-IF                                                             
294521     ELSE                                                                 
294522       MOVE ZEROS                        TO WS-KDFRAKT                    
294523     END-IF                                                               
294524     .                                                                    
294525     EJECT                                                                
294530 S10-MINSKA-KVROS-WDK6  SECTION.                                          
294602     MOVE 'S10-MINSKA-KVROS-WDK6 ' TO CURR-SECTION                        
294702                                                                          
294802     MOVE RAD-IDARTNR TO W-IDARTNR-ARTC01                                 
294902     PERFORM IMS-12-GHU-ARTC11                                            
295002     COMPUTE CLAG-KVROS = CLAG-KVROS - SPAR-KVART                         
295102     PERFORM IMS-13-REPL-ARTK611                                          
295202     .                                                                    
295302     EJECT                                                                
295402                                                                          
295502 S11-MINSKA-KVROS-WDK7  SECTION.                                          
295602     MOVE 'S11-MINSKA-KVROS-WDK7 ' TO CURR-SECTION                        
295702                                                                          
295802     MOVE RAD-IDARTNR TO W-IDARTNR-K7                                     
295902     MOVE RAD-IDDC    TO W-IDDC-K7                                        
296002                                                                          
296102     PERFORM IMS-40-GHU-WDK711                                            
296202     IF SEGMENT-FINNS                                                     
296302        IF RAD-KDORDKL = +0 OR +1                                         
296402           COMPUTE SLAG-KVROS-DAG = SLAG-KVROS-DAG - SPAR-KVART           
296502        ELSE                                                              
296602           IF RAD-KDORDKL = +2 OR +3 OR +4                                
296702              COMPUTE SLAG-KVROS-BULK =                                   
296802                      SLAG-KVROS-BULK - SPAR-KVART                        
296902           END-IF                                                         
297002        END-IF                                                            
297102        PERFORM IMS-41-REPL-WDK711                                        
297202     END-IF                                                               
297302     .                                                                    
297402     EJECT                                                                
297502                                                                          
297602 MFS-RENSA-FAELT-UT SECTION.                                              
297702                                                                          
297802     MOVE MFS-RENSA-FAELT TO MOD-KDCMD       (INDX)                       
297902                             MOD-IDDISTR     (INDX)                       
298002                             MOD-IDKUNDNR    (INDX)                       
298102                             MOD-IDARTNR     (INDX)                       
298202                             MOD-KVART       (INDX)                       
298302                             MOD-IDORDNR5    (INDX)                       
298402                             MOD-KDORDKL     (INDX)                       
298404                             MOD-KDFARLIG    (INDX)                       
298502                             MOD-TIRFSDAT-CDC(INDX)                       
298602                             MOD-TIRFSDAT-LDC(INDX)                       
298702                             MOD-TIREPDAT    (INDX)                       
298802                             MOD-IDKUNDRF-WIP(INDX)                       
298902                             MOD-IDDC-PRIM   (INDX)                       
299002                             MOD-BELAGINS    (INDX)                       
299102                             MOD-BETEXT      (INDX)                       
299202                                                                          
299302*    FÖR SÄKERHETS SKULL STÄDAR VI LITE I SPARAREAN OCKSÅ                 
299402     MOVE ZERO TO SPAR-IDDISTR-RAD  (INDX)                                
299502                  SPAR-IDKUNDNR-RAD (INDX)                                
299602                  SPAR-IDORDNR-RAD  (INDX)                                
299702                  SPAR-IDARTNR-RAD  (INDX)                                
299802                  SPAR-IDLOPNR-RAD  (INDX)                                
299902     MOVE SPACE TO SPAR-BETEXT-RAD  (INDX)                                
299903     MOVE SPACE TO SPAR-KDFARLIG-RAD (INDX)                               
300002     .                                                                    
300102 MFS-RENSA-ALLA-FAELT-UT SECTION.                                         
300202                                                                          
300302     MOVE +1 TO INDX                                                      
300402     PERFORM UNTIL INDX > MAX-INDX                                        
300502       MOVE MFS-RENSA-FAELT TO MOD-KDCMD       (INDX)                     
300602                               MOD-IDDISTR     (INDX)                     
300702                               MOD-IDKUNDNR    (INDX)                     
300802                               MOD-IDARTNR     (INDX)                     
300902                               MOD-KVART       (INDX)                     
300903                               MOD-KDFARLIG    (INDX)                     
301002                               MOD-IDORDNR5    (INDX)                     
301102                               MOD-KDORDKL     (INDX)                     
301202                               MOD-TIRFSDAT-CDC(INDX)                     
301302                               MOD-TIRFSDAT-LDC(INDX)                     
301402                               MOD-TIREPDAT    (INDX)                     
301502                               MOD-IDKUNDRF-WIP(INDX)                     
301602                               MOD-IDDC-PRIM   (INDX)                     
301702                               MOD-BELAGINS    (INDX)                     
301802                               MOD-BETEXT      (INDX)                     
301902       ADD 1 TO INDX                                                      
302002     END-PERFORM                                                          
302102     .                                                                    
302202     SKIP3                                                                
302302 MFS-ROER-EJ-MOD-FAELT  SECTION.                                          
302402                                                                          
302502*    --- ALLA UTDATA-FÄLT                                                 
302602                                                                          
302702     MOVE +1 TO INDX                                                      
302802     PERFORM UNTIL INDX > MAX-INDX                                        
302902        MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD       (INDX)                  
303002                                  MOD-IDDISTR     (INDX)                  
303102                                  MOD-IDKUNDNR    (INDX)                  
303202                                  MOD-IDARTNR     (INDX)                  
303302                                  MOD-KVART       (INDX)                  
303402                                  MOD-IDORDNR5    (INDX)                  
303502                                  MOD-KDORDKL     (INDX)                  
303603                                  MOD-KDFARLIG    (INDX)                  
303702                                  MOD-TIRFSDAT-CDC(INDX)                  
303802                                  MOD-TIRFSDAT-LDC(INDX)                  
303902                                  MOD-TIREPDAT    (INDX)                  
304002                                  MOD-IDKUNDRF-WIP(INDX)                  
304102                                  MOD-IDDC-PRIM   (INDX)                  
304202                                  MOD-BELAGINS    (INDX)                  
304302                                  MOD-BETEXT      (INDX)                  
304402                                                                          
304502        ADD +1 TO INDX                                                    
304602     END-PERFORM                                                          
304702     .                                                                    
304802     EJECT                                                                
304902 S05-SEND-OPEN SECTION.                                                   
305002                                                                          
305102     MOVE 'CARPARTS.DAP.DISTRDOC'         TO SEND-ADDISPABS               
305202     MOVE 'OPEN'                          TO SEND-KDFUNC                  
305302     CALL WZ01SEND USING SEND-CONTROL-AREA                                
305402                         SEND-OPEN-AREA                                   
305502     IF SEND-KDRC > ZERO                                                  
305602       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
305702       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
305802       DELIMITED BY SIZE INTO ERROR-TEXT                                  
305902       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
306002     END-IF                                                               
306102     .                                                                    
306202     SKIP3                                                                
306302 S05-PUT-HEADER SECTION.                                                  
306402                                                                          
306502     MOVE 'PUT'                           TO SEND-KDFUNC                  
306602     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
306702     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
306802     CALL WZ01SEND USING SEND-CONTROL-AREA                                
306902                         SEND-KVDLEN                                      
307002                         HDR-AREA                                         
307102     IF SEND-KDRC > ZERO                                                  
307202       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
307302       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
307402       DELIMITED BY SIZE INTO ERROR-TEXT                                  
307502       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
307602     END-IF                                                               
307702     .                                                                    
307802     EJECT                                                                
307902 S05-PUT-MAIL-LINE-AREA1 SECTION.                                         
308002                                                                          
308102     MOVE 'PUT'                           TO SEND-KDFUNC                  
308202     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
308302     MOVE LENGTH OF DAP-MAIL-LINE1-AREA1  TO SEND-KVDLEN                  
308402     CALL WZ01SEND USING SEND-CONTROL-AREA                                
308502                         SEND-KVDLEN                                      
308602                         DAP-MAIL-LINE1-AREA1                             
308702     IF SEND-KDRC > ZERO                                                  
308802       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
308902       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
309002       DELIMITED BY SIZE INTO ERROR-TEXT                                  
309102       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
309202     END-IF                                                               
309302     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
309402     MOVE LENGTH OF DAP-MAIL-LINE-TEXT-SPACE                              
309502                                          TO SEND-KVDLEN                  
309602     CALL WZ01SEND USING SEND-CONTROL-AREA                                
309702                         SEND-KVDLEN                                      
309802                         DAP-MAIL-LINE-TEXT-SPACE                         
309902     IF SEND-KDRC > ZERO                                                  
310002       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
310102       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
310202       DELIMITED BY SIZE INTO ERROR-TEXT                                  
310302       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
310402     END-IF                                                               
310502     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
310602     MOVE LENGTH OF DAP-MAIL-LINE2-AREA1  TO SEND-KVDLEN                  
310702     CALL WZ01SEND USING SEND-CONTROL-AREA                                
310802                         SEND-KVDLEN                                      
310902                         DAP-MAIL-LINE2-AREA1                             
311002     IF SEND-KDRC > ZERO                                                  
311102       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
311202       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
311302       DELIMITED BY SIZE INTO ERROR-TEXT                                  
311402       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
311502     END-IF                                                               
311602     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
311702     MOVE LENGTH OF DAP-MAIL-LINE-TEXT-SPACE                              
311802                                          TO SEND-KVDLEN                  
311902     CALL WZ01SEND USING SEND-CONTROL-AREA                                
312002                         SEND-KVDLEN                                      
312102                         DAP-MAIL-LINE-TEXT-SPACE                         
312202     IF SEND-KDRC > ZERO                                                  
312302       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
312402       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
312502       DELIMITED BY SIZE INTO ERROR-TEXT                                  
312602       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
312702     END-IF                                                               
312802     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
312902     MOVE LENGTH OF DAP-MAIL-LINE3-AREA1  TO SEND-KVDLEN                  
313002     CALL WZ01SEND USING SEND-CONTROL-AREA                                
313102                         SEND-KVDLEN                                      
313202                         DAP-MAIL-LINE3-AREA1                             
313302     IF SEND-KDRC > ZERO                                                  
313402       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
313502       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
313602       DELIMITED BY SIZE INTO ERROR-TEXT                                  
313702       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
313802     END-IF                                                               
313902     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
314002     MOVE LENGTH OF DAP-MAIL-LINE-TEXT-SPACE                              
314102                                          TO SEND-KVDLEN                  
314202     CALL WZ01SEND USING SEND-CONTROL-AREA                                
314302                         SEND-KVDLEN                                      
314402                         DAP-MAIL-LINE-TEXT-SPACE                         
314502     IF SEND-KDRC > ZERO                                                  
314602       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
314702       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
314802       DELIMITED BY SIZE INTO ERROR-TEXT                                  
314902       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
315002     END-IF                                                               
315102     .                                                                    
315202     SKIP3                                                                
315302 S05-PUT-MAIL-LINE-AREA2 SECTION.                                         
315402                                                                          
315502     MOVE 'PUT'                           TO SEND-KDFUNC                  
315602     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
315702     MOVE LENGTH OF DAP-MAIL-BODY-AREA2   TO SEND-KVDLEN                  
315802     CALL WZ01SEND USING SEND-CONTROL-AREA                                
315902                         SEND-KVDLEN                                      
316002                         DAP-MAIL-BODY-AREA2                              
316102     IF SEND-KDRC > ZERO                                                  
316202       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
316302       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
316402       DELIMITED BY SIZE INTO ERROR-TEXT                                  
316502       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
316602     END-IF                                                               
316702     .                                                                    
316802     SKIP3                                                                
316902 S05-PUT-MAIL-LINE-AREA3 SECTION.                                         
317002                                                                          
317102     MOVE 'PUT'                           TO SEND-KDFUNC                  
317202     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
317302     MOVE LENGTH OF DAP-MAIL-BODY-AREA3   TO SEND-KVDLEN                  
317402     CALL WZ01SEND USING SEND-CONTROL-AREA                                
317502                         SEND-KVDLEN                                      
317602                         DAP-MAIL-BODY-AREA3                              
317702     IF SEND-KDRC > ZERO                                                  
317802       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
317902       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
318002       DELIMITED BY SIZE INTO ERROR-TEXT                                  
318102       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
318202     END-IF                                                               
318302     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
318402     MOVE LENGTH OF DAP-MAIL-LINE-TEXT-SPACE                              
318502                                          TO SEND-KVDLEN                  
318602     CALL WZ01SEND USING SEND-CONTROL-AREA                                
318702                         SEND-KVDLEN                                      
318802                         DAP-MAIL-LINE-TEXT-SPACE                         
318902     IF SEND-KDRC > ZERO                                                  
319002       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
319102       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
319202       DELIMITED BY SIZE INTO ERROR-TEXT                                  
319302       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
319402     END-IF                                                               
319502     .                                                                    
319602     SKIP3                                                                
319702 S05-PUT-MAIL-LINE-AREA4 SECTION.                                         
319802                                                                          
319902     MOVE 'PUT'                           TO SEND-KDFUNC                  
320002     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
320102     MOVE LENGTH OF DAP-MAIL-LINE1-AREA4  TO SEND-KVDLEN                  
320202     CALL WZ01SEND USING SEND-CONTROL-AREA                                
320302                         SEND-KVDLEN                                      
320402                         DAP-MAIL-LINE1-AREA4                             
320502     IF SEND-KDRC > ZERO                                                  
320602       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
320702       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
320802       DELIMITED BY SIZE INTO ERROR-TEXT                                  
320902       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
321002     END-IF                                                               
321102     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
321202     MOVE LENGTH OF DAP-MAIL-LINE2-AREA4  TO SEND-KVDLEN                  
321302     CALL WZ01SEND USING SEND-CONTROL-AREA                                
321402                         SEND-KVDLEN                                      
321502                         DAP-MAIL-LINE2-AREA4                             
321602     IF SEND-KDRC > ZERO                                                  
321702       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
321802       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
321902       DELIMITED BY SIZE INTO ERROR-TEXT                                  
322002       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
322102     END-IF                                                               
322202     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
322302     MOVE LENGTH OF DAP-MAIL-LINE3-AREA4  TO SEND-KVDLEN                  
322402     CALL WZ01SEND USING SEND-CONTROL-AREA                                
322502                         SEND-KVDLEN                                      
322602                         DAP-MAIL-LINE3-AREA4                             
322702     IF SEND-KDRC > ZERO                                                  
322802       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
322902       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
323002       DELIMITED BY SIZE INTO ERROR-TEXT                                  
323102       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
323202     END-IF                                                               
323302     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
323402     MOVE LENGTH OF DAP-MAIL-LINE-TEXT-SPACE                              
323502                                          TO SEND-KVDLEN                  
323602     CALL WZ01SEND USING SEND-CONTROL-AREA                                
323702                         SEND-KVDLEN                                      
323802                         DAP-MAIL-LINE-TEXT-SPACE                         
323902     IF SEND-KDRC > ZERO                                                  
324002       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
324102       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
324202       DELIMITED BY SIZE INTO ERROR-TEXT                                  
324302       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
324402     END-IF                                                               
324502     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
324602     MOVE LENGTH OF DAP-MAIL-LINE4-AREA4  TO SEND-KVDLEN                  
324702     CALL WZ01SEND USING SEND-CONTROL-AREA                                
324802                         SEND-KVDLEN                                      
324902                         DAP-MAIL-LINE4-AREA4                             
325002     IF SEND-KDRC > ZERO                                                  
325102       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
325202       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
325302       DELIMITED BY SIZE INTO ERROR-TEXT                                  
325402       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
325502     END-IF                                                               
325602     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
325702     MOVE LENGTH OF DAP-MAIL-LINE-TEXT-SPACE                              
325802                                          TO SEND-KVDLEN                  
325902     CALL WZ01SEND USING SEND-CONTROL-AREA                                
326002                         SEND-KVDLEN                                      
326102                         DAP-MAIL-LINE-TEXT-SPACE                         
326202     IF SEND-KDRC > ZERO                                                  
326302       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
326402       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
326502       DELIMITED BY SIZE INTO ERROR-TEXT                                  
326602       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
326702     END-IF                                                               
326802     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
326902     MOVE LENGTH OF DAP-MAIL-LINE5-AREA4  TO SEND-KVDLEN                  
327002     CALL WZ01SEND USING SEND-CONTROL-AREA                                
327102                         SEND-KVDLEN                                      
327202                         DAP-MAIL-LINE5-AREA4                             
327302     IF SEND-KDRC > ZERO                                                  
327402       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
327502       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
327602       DELIMITED BY SIZE INTO ERROR-TEXT                                  
327702       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
327802     END-IF                                                               
327902     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
328002     MOVE LENGTH OF DAP-MAIL-LINE-TEXT-SPACE                              
328102                                          TO SEND-KVDLEN                  
328202     CALL WZ01SEND USING SEND-CONTROL-AREA                                
328302                         SEND-KVDLEN                                      
328402                         DAP-MAIL-LINE-TEXT-SPACE                         
328502     IF SEND-KDRC > ZERO                                                  
328602       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
328702       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
328802       DELIMITED BY SIZE INTO ERROR-TEXT                                  
328902       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
329002     END-IF                                                               
329102     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
329202     MOVE LENGTH OF DAP-MAIL-LINE6-AREA4  TO SEND-KVDLEN                  
329302     CALL WZ01SEND USING SEND-CONTROL-AREA                                
329402                         SEND-KVDLEN                                      
329502                         DAP-MAIL-LINE6-AREA4                             
329602     IF SEND-KDRC > ZERO                                                  
329702       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
329802       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
329902       DELIMITED BY SIZE INTO ERROR-TEXT                                  
330002       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
330102     END-IF                                                               
330202     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
330302     MOVE LENGTH OF DAP-MAIL-LINE7-AREA4  TO SEND-KVDLEN                  
330402     CALL WZ01SEND USING SEND-CONTROL-AREA                                
330502                         SEND-KVDLEN                                      
330602                         DAP-MAIL-LINE7-AREA4                             
330702     IF SEND-KDRC > ZERO                                                  
330802       MOVE SEND-KDRC                   TO KDRC-DISPLAY                   
330902       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
331002       DELIMITED BY SIZE INTO ERROR-TEXT                                  
331102       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
331202     END-IF                                                               
331302     .                                                                    
331402                                                                          
331502 S05-SEND-CLOSE SECTION.                                                  
331602                                                                          
331702     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
331802     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
331902     CALL WZ01SEND USING SEND-CONTROL-AREA                                
332002     .                                                                    
332102                                                                          
332202                                                                          
332302* --- IMS SEKTIONER                                                       
332402 IMS-GET-MSG SECTION.                                                     
332502                                                                          
332602     MOVE '  QC' TO GODK-STATUSKODER                                      
332702     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
332802     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
332902     PERFORM IMS-STATUSKONTROLL                                           
333002     .                                                                    
333102                                                                          
333202                                                                          
333302 IMS-INSERT-MSG SECTION.                                                  
333402                                                                          
333502     MOVE SPACE TO GODK-STATUSKODER                                       
333602     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
333702     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
333802     PERFORM IMS-STATUSKONTROLL                                           
333902     .                                                                    
334002                                                                          
334102 IMS-PURG-ALT-MSG-2109 SECTION.                                           
334202     MOVE LOW-VALUE TO 2109-Z1 2109-Z2                                    
334302     MOVE SPACE TO GODK-STATUSKODER                                       
334402     CALL CBLTDLI USING PURG 2109-PCB W-PROG-TO-PROG-SW-1                 
334502     MOVE 2109-STATUS-CODE TO STATUS-WS                                   
334602     PERFORM IMS-STATUSKONTROLL                                           
334702     .                                                                    
334802     EJECT                                                                
334902                                                                          
335002 IMS-GU-WDB301 SECTION.                                                   
335102                                                                          
335202     STRING 'WLGMTB01(WDB301KY =' W-WDB301KY-X                            
335302                    '!WDB301KY =' W-WDB301KY-DEF-X ')'                    
335402          DELIMITED BY SIZE INTO SSA1                                     
335502     MOVE '    '              TO GODK-STATUSKODER                         
335602     CALL CBLTDLI USING GU KREG-GMTB-PCB DLI-IO-WDB301 SSA1               
335702     MOVE KREG-GMTB-STATUS-CODE    TO STATUS-WS                           
335802     PERFORM IMS-STATUSKONTROLL                                           
335902     .                                                                    
336002     EJECT                                                                
336102 IMS-01-GN-WDA501 SECTION.                                                
336202                                                                          
336203     MOVE 'IMS-01' TO CURR-IMS-SECTION                                    
336204     STRING 'WDA501  (WDA501KY=>' W-WDA5KEY-MIN-X                         
336205                    '&WDA501KY=<' W-WDA5KEY-MAX-X                         
336206                    '&KDORDKL = ' W-KDORDKL-X                             
336207                    '&TIREPDAT >' WS-TIREPDAT-X                           
336209                    '&KDSTARAD <' W-KDSTARAD                              
336210                    '&KDORDTYP= ' VERKSTADSORDER                          
336220                    '!WDA501KY=>' W-WDA5KEY-MIN-X                         
336230                    '&WDA501KY=<' W-WDA5KEY-MAX-X                         
336240                    '&KDORDKL = ' W-KDORDKL-X                             
336250                    '&TIREPDAT >' WS-TIREPDAT-X                           
337622                    '&KDSTARAD <' W-KDSTARAD                              
337623                    '&KDORDTYP= ' BUTIKSORDER ')'                         
337630          DELIMITED BY SIZE  INTO SSA1                                    
337700     MOVE '  GEGB'             TO GODK-STATUSKODER                        
337800     CALL CBLTDLI USING GN  WDA5-PCB DLI-IO-WDA5 SSA1                     
337900     MOVE WDA5-STATUS-CODE     TO STATUS-WS                               
338000     PERFORM IMS-STATUSKONTROLL                                           
338100     .                                                                    
339802                                                                          
339902                                                                          
340002 IMS-02-GN-WDA501-ARTIKEL SECTION.                                        
340102                                                                          
340202     MOVE 'IMS-02' TO CURR-IMS-SECTION                                    
340302     STRING 'WDA501  (WDA501KY=>' W-WDA5KEY-MIN-X                         
340402                    '&WDA501KY=<' W-WDA5KEY-MAX-X                         
340502                    '&KDORDKL = ' W-KDORDKL-X                             
340602                    '&IDARTNR = ' W-A5-IDARTNR-X                          
340702                    '&TIREPDAT >' WS-TIREPDAT-X                           
340902                    '&KDSTARAD <' W-KDSTARAD                              
341002                    '&KDORDTYP= ' VERKSTADSORDER                          
341102                    '!WDA501KY=>' W-WDA5KEY-MIN-X                         
341202                    '&WDA501KY=<' W-WDA5KEY-MAX-X                         
341302                    '&KDORDKL = ' W-KDORDKL-X                             
341402                    '&IDARTNR = ' W-A5-IDARTNR-X                          
341502                    '&TIREPDAT >' WS-TIREPDAT-X                           
343530                    '&KDSTARAD <' W-KDSTARAD                              
343531                    '&KDORDTYP= ' BUTIKSORDER ')'                         
343540          DELIMITED BY SIZE  INTO SSA1                                    
343602     MOVE '  GEGB'             TO GODK-STATUSKODER                        
343702     CALL CBLTDLI USING GN  WDA5-PCB DLI-IO-WDA5 SSA1                     
343802     MOVE WDA5-STATUS-CODE     TO STATUS-WS                               
343902     PERFORM IMS-STATUSKONTROLL                                           
344002     .                                                                    
344102                                                                          
344202 IMS-03-GN-WDA5A1 SECTION.                                                
344302                                                                          
344402     MOVE 'IMS-03' TO CURR-IMS-SECTION                                    
344502     STRING 'WDA5A1  (WDA5A1KY>=' W-WDA5A1KY-MIN-X                        
344602                    '&WDA5A1KY<=' W-WDA5A1KY-MAX-X                        
344702                    '&KDORDKL = ' W-KDORDKL-X                             
344802                    '&TIREPDAT >' WS-TIREPDAT-X                           
345002                    '&KDSTARAD <' W-KDSTARAD                              
345102                    '&KDORDTYP= ' VERKSTADSORDER                          
345202                    '!WDA5A1KY>=' W-WDA5A1KY-MIN-X                        
345302                    '&WDA5A1KY<=' W-WDA5A1KY-MAX-X                        
345402                    '&KDORDKL = ' W-KDORDKL-X                             
345502                    '&TIREPDAT >' WS-TIREPDAT-X                           
347324                    '&KDSTARAD <' W-KDSTARAD                              
347325                    '&KDORDTYP= ' BUTIKSORDER ')'                         
347330          DELIMITED BY SIZE INTO SSA1                                     
347402     MOVE '  GBGE' TO GODK-STATUSKODER                                    
347530     CALL CBLTDLI USING GN WDA5A-PCB DLI-IO-WDA5A SSA1                    
347602     MOVE WDA5A-STATUS-CODE TO STATUS-WS                                  
347702     PERFORM IMS-STATUSKONTROLL                                           
347802     .                                                                    
347902                                                                          
348002                                                                          
348102                                                                          
348202 IMS-04-GU-WDA501 SECTION.                                                
348302                                                                          
348402     MOVE 'IMS-04' TO CURR-IMS-SECTION                                    
348502     STRING 'WDA501  (WDA501KY =' W-WDA5KEY-X                             
348602                    '&KDSTARAD <' W-KDSTARAD ')'                          
348702          DELIMITED BY SIZE  INTO SSA1                                    
348802     MOVE '  GE'               TO GODK-STATUSKODER                        
348902     CALL CBLTDLI USING GU  WDA5-PCB DLI-IO-WDA5 SSA1                     
349002     MOVE WDA5-STATUS-CODE     TO STATUS-WS                               
349102     PERFORM IMS-STATUSKONTROLL                                           
349202     .                                                                    
349302                                                                          
349402 IMS-05-GHU-WDA501 SECTION.                                               
349502                                                                          
349602     MOVE 'IMS-05' TO CURR-IMS-SECTION                                    
349702     STRING 'WDA501  (WDA501KY= ' W-WDA5KEY-X ')'                         
349802          DELIMITED BY SIZE  INTO SSA1                                    
349902     MOVE '  '                 TO GODK-STATUSKODER                        
350002     CALL CBLTDLI USING GHU WDA5-PCB DLI-IO-WDA5 SSA1                     
350102     MOVE WDA5-STATUS-CODE     TO STATUS-WS                               
350202     PERFORM IMS-STATUSKONTROLL                                           
350302     .                                                                    
350402                                                                          
350502 IMS-06-REPL-WDA501 SECTION.                                              
350602                                                                          
350702     MOVE 'IMS-06' TO CURR-IMS-SECTION                                    
350802     MOVE '  ' TO GODK-STATUSKODER                                        
350902     CALL CBLTDLI USING REPL WDA5-PCB DLI-IO-WDA5                         
351002     MOVE WDA5-STATUS-CODE TO STATUS-WS                                   
351102     PERFORM IMS-STATUSKONTROLL                                           
351202     .                                                                    
351302     SKIP2                                                                
351402 IMS-07-GU-WDB201 SECTION.                                                
351502                                                                          
351602     MOVE 'IMS-07' TO CURR-IMS-SECTION                                    
351702     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
351802            DELIMITED BY SIZE INTO SSA1                                   
351902                                                                          
352002     MOVE '  GE' TO GODK-STATUSKODER                                      
352102     CALL CBLTDLI USING                                                   
352202           GU WDB2-PCB DLI-IO-WDB2 SSA1                                   
352302     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
352402     PERFORM IMS-STATUSKONTROLL                                           
352502     .                                                                    
352602                                                                          
352702 IMS-08-GN-WDB2C1 SECTION.                                                
352802                                                                          
352902     MOVE 'IMS-08' TO CURR-IMS-SECTION                                    
353002     STRING 'WDB2C1  (WDB2C1KY>=' W-WDB2C1KY-MIN-X                        
353102                    '&WDB2C1KY<=' W-WDB2C1KY-MAX-X ')'                    
353202          DELIMITED BY SIZE INTO SSA1                                     
353302     MOVE '  GBGE' TO GODK-STATUSKODER                                    
353402     CALL CBLTDLI USING GN WDB2C-PCB DLI-IO-WDB2C SSA1                    
353502     MOVE WDB2C-STATUS-CODE TO STATUS-WS                                  
353602     PERFORM IMS-STATUSKONTROLL                                           
353702     .                                                                    
353802                                                                          
353902                                                                          
354002 IMS-09-GU-WDR5 SECTION.                                                  
354102                                                                          
354202     MOVE 'IMS-09' TO CURR-IMS-SECTION                                    
354302     STRING 'WDR501  (WDGXKEY  =' W-4563KEY-X ')'                         
354402            DELIMITED BY SIZE INTO SSA1                                   
354502     STRING 'WDGX4564(KY4564   =' W-4564KEY-X ')'                         
354602            DELIMITED BY SIZE INTO SSA2                                   
354702                                                                          
354802     MOVE '  GE' TO GODK-STATUSKODER                                      
354902     CALL CBLTDLI USING                                                   
355002           GU WDR5-PCB DLI-IO-WDR5 SSA1 SSA2                              
355102     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
355202     PERFORM IMS-STATUSKONTROLL                                           
355302     .                                                                    
355402                                                                          
355502 IMS-10-GU-WDK611 SECTION.                                                
355602                                                                          
355702     MOVE 'IMS-10' TO CURR-IMS-SECTION                                    
355802     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
355902            DELIMITED BY SIZE INTO SSA1                                   
356002     MOVE   'WDK611  '          TO SSA2                                   
356102     MOVE '  '                  TO GODK-STATUSKODER                       
356202     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
356302     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
356402     PERFORM IMS-STATUSKONTROLL                                           
356502     .                                                                    
356602     SKIP2                                                                
356702                                                                          
356802 IMS-11-GU-WDB601    SECTION.                                             
356902                                                                          
357002     MOVE 'IMS-11' TO CURR-IMS-SECTION                                    
357102     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
357202          DELIMITED BY SIZE INTO SSA1                                     
357302     MOVE '  GE' TO GODK-STATUSKODER                                      
357402     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
357502     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
357602     PERFORM IMS-STATUSKONTROLL                                           
357702     IF SEGMENT-SAKNAS                                                    
357802         MOVE SPACE TO DCS-KDDC                                           
357902     END-IF                                                               
358002     .                                                                    
358102                                                                          
358202 IMS-12-GHU-ARTC11 SECTION.                                               
358302                                                                          
358402     MOVE 'IMS-12' TO CURR-IMS-SECTION                                    
358502     STRING  'WLARTC01(IDARTNR  =' W-IDARTNR-ARTC01-X ')'                 
358602              DELIMITED BY SIZE INTO SSA1                                 
358702     MOVE    'WLARTC11'           TO SSA2                                 
358802     MOVE    '  '                 TO GODK-STATUSKODER                     
358902     CALL     CBLTDLI USING GHU ART-PCB DLI-IO-WDK611 SSA1 SSA2           
359002     MOVE     ART-STATUS-CODE     TO STATUS-WS                            
359102     PERFORM  IMS-STATUSKONTROLL                                          
359202     .                                                                    
359302     SKIP2                                                                
359402 IMS-13-REPL-ARTK611 SECTION.                                             
359502                                                                          
359602     MOVE 'IMS-13' TO CURR-IMS-SECTION                                    
359702     MOVE '  ' TO GODK-STATUSKODER                                        
359802     CALL CBLTDLI USING REPL ART-PCB DLI-IO-WDK611                        
359902     MOVE ART-STATUS-CODE TO STATUS-WS                                    
360002     PERFORM IMS-STATUSKONTROLL                                           
360102     .                                                                    
360202                                                                          
360302     EJECT                                                                
360402 IMS-14-GHU-RO SECTION.                                                   
360502                                                                          
360602     MOVE 'IMS-14' TO CURR-IMS-SECTION                                    
360702     STRING 'WLORDP01(WDA501KY =' W-WDA501KY ')'                          
360802            DELIMITED BY SIZE INTO SSA1                                   
360902     MOVE '  GE' TO GODK-STATUSKODER                                      
361002     CALL CBLTDLI USING GHU ORDP-PCB DLI-IO-WDA5 SSA1                     
361102     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
361202     PERFORM IMS-STATUSKONTROLL                                           
361302     .                                                                    
361402     SKIP2                                                                
361502                                                                          
361602 IMS-15-DLET-RO SECTION.                                                  
361702                                                                          
361802     MOVE 'IMS-15' TO CURR-IMS-SECTION                                    
361902     MOVE '  ' TO GODK-STATUSKODER                                        
362002     CALL CBLTDLI USING DLET ORDP-PCB DLI-IO-WDA5                         
362102     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
362202     PERFORM IMS-STATUSKONTROLL                                           
362302     .                                                                    
362402     SKIP2                                                                
362502                                                                          
362602 IMS-16-GU-XXBX-WDR220 SECTION.                                           
362702                                                                          
362802     MOVE 'IMS-16' TO CURR-IMS-SECTION                                    
362902     STRING 'WLXXBX01(WDGXKEY  =' W-WDGX2231-X ')'                        
363002          DELIMITED BY SIZE INTO SSA1                                     
363102     STRING 'WLXXBX11(WDGXKEY  =' W-WDGX2232-X ')'                        
363202          DELIMITED BY SIZE INTO SSA2                                     
363302     MOVE '  GE' TO GODK-STATUSKODER                                      
363402     CALL CBLTDLI USING GU XXBX-PCB DLI-IO-2232 SSA1 SSA2                 
363502     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
363602     PERFORM IMS-STATUSKONTROLL                                           
363702     .                                                                    
363802     EJECT                                                                
363902 IMS-17-GHU-XXBU-WDR5 SECTION.                                            
364002                                                                          
364102     MOVE 'IMS-17' TO CURR-IMS-SECTION                                    
364202     STRING 'WLXXBU01(WDGXKEY  =' W-WDGX2223-X ')'                        
364302          DELIMITED BY SIZE INTO SSA1                                     
364402     STRING 'WLXXBU11(WDGXKEY  =' W-WDGX2224-X ')'                        
364502          DELIMITED BY SIZE INTO SSA2                                     
364602     MOVE '  GE' TO GODK-STATUSKODER                                      
364702     CALL CBLTDLI USING GHU XXBU-PCB DLI-IO-2224 SSA1 SSA2                
364802     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
364902     PERFORM IMS-STATUSKONTROLL                                           
365002     .                                                                    
365102     SKIP2                                                                
365202 IMS-18-DLET-XXBU-WDR5 SECTION.                                           
365302                                                                          
365402     MOVE 'IMS-18' TO CURR-IMS-SECTION                                    
365502     MOVE '  ' TO GODK-STATUSKODER                                        
365602     CALL CBLTDLI USING DLET XXBU-PCB DLI-IO-2224                         
365702     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
365802     PERFORM IMS-STATUSKONTROLL                                           
365902     .                                                                    
366002     EJECT                                                                
366102 IMS-19-GHU-ARTM-WDK901 SECTION.                                          
366202                                                                          
366302     MOVE 'IMS-19' TO CURR-IMS-SECTION                                    
366402     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
366502          DELIMITED BY SIZE INTO SSA1                                     
366602     MOVE '  ' TO GODK-STATUSKODER                                        
366702     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-WDK901 SSA1                   
366802     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
366902     PERFORM IMS-STATUSKONTROLL                                           
367002     .                                                                    
367102     SKIP3                                                                
367202 IMS-20-REPL-ARTM-WDK901 SECTION.                                         
367302                                                                          
367402     MOVE 'IMS-20' TO CURR-IMS-SECTION                                    
367502     MOVE '  ' TO GODK-STATUSKODER                                        
367602     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-WDK901                       
367702     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
367802     PERFORM IMS-STATUSKONTROLL                                           
367902     .                                                                    
368002     SKIP2                                                                
368102 IMS-21-GHNP-ARTM-WDK911 SECTION.                                         
368202                                                                          
368302     MOVE 'IMS-21' TO CURR-IMS-SECTION                                    
368402     STRING 'WLARTM11(DABEHOV  =' W-DABEHOV-X ')'                         
368502          DELIMITED BY SIZE INTO SSA1                                     
368602     MOVE '  GE' TO GODK-STATUSKODER                                      
368702     CALL CBLTDLI USING GHNP ARTM-PCB DLI-IO-WDK911 SSA1                  
368802     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
368902     PERFORM IMS-STATUSKONTROLL                                           
369002     .                                                                    
369102     SKIP2                                                                
369202 IMS-22-DLET-ARTM-WDK9 SECTION.                                           
369302                                                                          
369402     MOVE 'IMS-22' TO CURR-IMS-SECTION                                    
369502     MOVE '  ' TO GODK-STATUSKODER                                        
369602     CALL CBLTDLI USING DLET ARTM-PCB DLI-IO-WDK911                       
369702     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
369802     PERFORM IMS-STATUSKONTROLL                                           
369902     .                                                                    
370002     EJECT                                                                
370102 IMS-23-REPL-ARTM-WDK911 SECTION.                                         
370202                                                                          
370302     MOVE 'IMS-23' TO CURR-IMS-SECTION                                    
370402     MOVE '  ' TO GODK-STATUSKODER                                        
370502     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-WDK911                       
370602     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
370702     PERFORM IMS-STATUSKONTROLL                                           
370802     .                                                                    
370902     SKIP2                                                                
371002 IMS-24-GU-ORQI01-CSEQ SECTION.                                           
371102                                                                          
371202     MOVE 'IMS-24' TO CURR-IMS-SECTION                                    
371302     STRING 'WLORQI01(WDQ2CSEQ =' W-IDGMTREF-X ')'                        
371402          DELIMITED BY SIZE INTO SSA1                                     
371502     MOVE '  ' TO GODK-STATUSKODER                                        
371602     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-WDQ201 SSA1                    
371702     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
371802     PERFORM IMS-STATUSKONTROLL                                           
371902     .                                                                    
372002     EJECT                                                                
372102 IMS-25-GU-ORQM-WDQ1 SECTION.                                             
372202                                                                          
372302     MOVE 'IMS-25' TO CURR-IMS-SECTION                                    
372402     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MIN-X                        
372502                    '&WDQ101KY<=' W-WDQ101KY-MAX-X ')'                    
372602          DELIMITED BY SIZE INTO SSA1                                     
372702     MOVE '  GBGE' TO GODK-STATUSKODER                                    
372802     CALL CBLTDLI USING GU ORQM-PCB DLI-IO-GU-Q101 SSA1                   
372902     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
373002     PERFORM IMS-STATUSKONTROLL                                           
373102     .                                                                    
373202     EJECT                                                                
373302 IMS-26-ISRT-ORQM-WDQ1 SECTION.                                           
373402                                                                          
373502     MOVE 'IMS-26' TO CURR-IMS-SECTION                                    
373602     MOVE 'WLORQM01 ' TO SSA1                                             
373702     MOVE '  II' TO GODK-STATUSKODER                                      
373802     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-ISRT-Q101 SSA1               
373902     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
374002     PERFORM IMS-STATUSKONTROLL                                           
374102     .                                                                    
374202     EJECT                                                                
374302 IMS-27-ISRT-LOGG SECTION.                                                
374402                                                                          
374502     MOVE 'IMS-27' TO CURR-IMS-SECTION                                    
374602     MOVE 'WLZZAC01' TO SSA1                                              
374702     MOVE '  II' TO GODK-STATUSKODER                                      
374802     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-ZZAC01 SSA1                  
374902     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
375002     PERFORM IMS-STATUSKONTROLL                                           
375102     .                                                                    
375202     EJECT                                                                
375302 IMS-28-GU-BENA-WDD311 SECTION.                                           
375402                                                                          
375502     MOVE 'IMS-28' TO CURR-IMS-SECTION                                    
375602     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
375702          DELIMITED BY SIZE INTO SSA1                                     
375802     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
375902          DELIMITED BY SIZE INTO SSA2                                     
376002     MOVE '  GE'               TO GODK-STATUSKODER                        
376102     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-BENA SSA1 SSA2            
376202     MOVE BENA-STATUS-CODE     TO STATUS-WS                               
376302     PERFORM IMS-STATUSKONTROLL                                           
376402     .                                                                    
376502     EJECT                                                                
376602 IMS-29-GHU-WDR5 SECTION.                                                 
376702                                                                          
376802     MOVE 'IMS-29' TO CURR-IMS-SECTION                                    
376902     STRING 'WDR501  (WDGXKEY  =' W-4563KEY-X ')'                         
377002            DELIMITED BY SIZE INTO SSA1                                   
377102     STRING 'WDGX4564(KY4564   =' W-4564KEY-X ')'                         
377202            DELIMITED BY SIZE INTO SSA2                                   
377302                                                                          
377402     MOVE '  GE' TO GODK-STATUSKODER                                      
377502     CALL CBLTDLI USING                                                   
377602          GHU WDR5-PCB DLI-IO-WDR5 SSA1 SSA2                              
377702     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
377802     PERFORM IMS-STATUSKONTROLL                                           
377902     .                                                                    
378002                                                                          
378102 IMS-30-REPL-WDR5 SECTION.                                                
378202                                                                          
378302     MOVE 'IMS-30' TO CURR-IMS-SECTION                                    
378402                                                                          
378502     MOVE '    ' TO GODK-STATUSKODER                                      
378602     CALL CBLTDLI USING REPL WDR5-PCB DLI-IO-WDR5                         
378702     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
378802     PERFORM IMS-STATUSKONTROLL                                           
378902     .                                                                    
379002                                                                          
379102 IMS-31-ISRT-WDR5 SECTION.                                                
379202                                                                          
379302     MOVE 'IMS-31' TO CURR-IMS-SECTION                                    
379402     STRING 'WDR501  (WDGXKEY  =' W-4563KEY-X ')'                         
379502            DELIMITED BY SIZE INTO SSA1                                   
379602     MOVE 'WDGX4564' TO SSA2                                              
379702     MOVE '    '     TO GODK-STATUSKODER                                  
379802     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDR5 SSA1 SSA2               
379902     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
380002     PERFORM IMS-STATUSKONTROLL                                           
380102     .                                                                    
380202                                                                          
380302 IMS-32-GU-WDA5A1 SECTION.                                                
380402                                                                          
380502     MOVE 'IMS-32' TO CURR-IMS-SECTION                                    
380602     STRING 'WDA5A1  (WDA5A1KY =' W-WDA5A1KY-X ')'                        
380702          DELIMITED BY SIZE INTO SSA1                                     
380802     MOVE '  GE' TO GODK-STATUSKODER                                      
380902     CALL CBLTDLI USING GU WDA5A-PCB DLI-IO-WDA5A SSA1                    
381002     MOVE WDA5A-STATUS-CODE TO STATUS-WS                                  
381102     PERFORM IMS-STATUSKONTROLL                                           
381202     .                                                                    
381302                                                                          
381402 IMS-33-GU-WDB2C1 SECTION.                                                
381502                                                                          
381602     MOVE 'IMS-33' TO CURR-IMS-SECTION                                    
381702     STRING 'WDB2C1  (WDB2C1KY =' W-WDB2C1KY-X ')'                        
381802          DELIMITED BY SIZE INTO SSA1                                     
381902     MOVE '  GE' TO GODK-STATUSKODER                                      
382002     CALL CBLTDLI USING GU WDB2C-PCB DLI-IO-WDB2C SSA1                    
382102     MOVE WDB2C-STATUS-CODE TO STATUS-WS                                  
382202     PERFORM IMS-STATUSKONTROLL                                           
382302     .                                                                    
382402                                                                          
382502 IMS-40-GHU-WDK711 SECTION.                                               
382602                                                                          
382702     MOVE 'IMS-40' TO CURR-IMS-SECTION                                    
382802     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-K7-X ')'                      
382902              DELIMITED BY SIZE INTO SSA1                                 
383002     STRING 'WDK711  (IDDC     =' W-IDDC-K7 ')'                           
383102          DELIMITED BY SIZE INTO SSA2                                     
383202     MOVE    '  '                 TO GODK-STATUSKODER                     
383302     CALL     CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2          
383402     MOVE     WDK7-STATUS-CODE    TO STATUS-WS                            
383502     PERFORM  IMS-STATUSKONTROLL                                          
383602     .                                                                    
383702     SKIP2                                                                
383802 IMS-41-REPL-WDK711 SECTION.                                              
383902                                                                          
384002     MOVE      'IMS-41'         TO CURR-IMS-SECTION                       
384102     MOVE      '  '             TO GODK-STATUSKODER                       
384202     CALL      CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                  
384302     MOVE      WDK7-STATUS-CODE TO STATUS-WS                              
384402     PERFORM  IMS-STATUSKONTROLL                                          
384502     .                                                                    
384602                                                                          
384702     EJECT                                                                
384802                                                                          
384902                                                                          
385002 IMS-42-GU-WDD901 SECTION.                                                
385102                                                                          
385202     STRING 'WDD901  (WDD901KY= ' W-WDD901KY-X ')'                        
385302              DELIMITED BY SIZE INTO SSA1                                 
385402     MOVE '  GE'                TO GODK-STATUSKODER                       
385502     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD901 SSA1                    
385602     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
385702     PERFORM IMS-STATUSKONTROLL                                           
385802     .                                                                    
385902     EJECT                                                                
386002                                                                          
386102                                                                          
386202 IMS-43-GNP-WDD902 SECTION.                                               
386302                                                                          
386402     STRING 'WDD901  (WDD901KY= ' W-WDD901KY-X ')'                        
386502              DELIMITED BY SIZE INTO SSA1                                 
386602     MOVE 'WDD902  '            TO SSA2                                   
386702     MOVE '  GE'                TO GODK-STATUSKODER                       
386802     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD902 SSA1 SSA2              
386902     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
387002     PERFORM IMS-STATUSKONTROLL                                           
387102     .                                                                    
387202     EJECT                                                                
387302                                                                          
387402                                                                          
387502 IMS-44-GNP-WDD924 SECTION.                                               
387602                                                                          
387702     STRING 'WDD902  (IDLEVNR = ' W-IDLEVNR-X ')'                         
387802              DELIMITED BY SIZE INTO SSA1                                 
387902     MOVE 'WDD924  '            TO SSA2                                   
388002     MOVE '  GE'                TO GODK-STATUSKODER                       
388102     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD924 SSA1 SSA2              
388202     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
388302     PERFORM IMS-STATUSKONTROLL                                           
388402     .                                                                    
388502     EJECT                                                                
388602                                                                          
388702                                                                          
388802 IMS-GU-WDB501 SECTION.                                                   
388902                                                                          
389002     STRING 'WDB501  (WDB501KY =' W-WDB501KY-X ')'                        
389102          DELIMITED BY SIZE INTO SSA1                                     
389202     MOVE '  GE' TO GODK-STATUSKODER                                      
389302     CALL CBLTDLI USING GU WDB5-PCB DLI-IO-AREA-B501 SSA1                 
389402     MOVE WDB5-STATUS-CODE TO STATUS-WS                                   
389502     PERFORM IMS-STATUSKONTROLL                                           
389602     .                                                                    
389702                                                                          
389802                                                                          
389902 IMS-GU-WDR101 SECTION.                                                   
390002                                                                          
390102     STRING  'WDR101  (WDGXKEY  =' W-WDGXKEY-4433-X ')'                   
390202             DELIMITED BY SIZE INTO SSA1                                  
390302     MOVE    '  '               TO GODK-STATUSKODER                       
390402     CALL    CBLTDLI USING GU WDR1-PCB DLI-IO-AREA-R101 SSA1              
390502     MOVE    WDR1-STATUS-CODE   TO STATUS-WS                              
390602     PERFORM IMS-STATUSKONTROLL                                           
390702     .                                                                    
390802                                                                          
390902                                                                          
391002 IMS-GNP-WDR130 SECTION.                                                  
391102                                                                          
391202     STRING 'WDR130  (WDGXKEY >=' W-WDGXKEY-4434-MIN-X                    
391302                    '&WDGXKEY <=' W-WDGXKEY-4434-MAX-X ')'                
391402             DELIMITED BY SIZE INTO SSA1                                  
391502     MOVE    '  GE'             TO GODK-STATUSKODER                       
391602     CALL    CBLTDLI USING GNP WDR1-PCB DLI-IO-AREA-R130 SSA1             
391702     MOVE    WDR1-STATUS-CODE   TO STATUS-WS                              
391802     PERFORM IMS-STATUSKONTROLL                                           
391902     .                                                                    
392002                                                                          
392102                                                                          
392202 IMS-STATUSKONTROLL SECTION.                                              
392302                                                                          
392402     SET STATUS-IX TO 1                                                   
392502     SEARCH GODK-STATUS                                                   
392602       AT END                                                             
392702         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
392802         DELIMITED BY SIZE INTO FELTEXT                                   
392902         CALL FELLOG                                                      
393002       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
393102         CONTINUE                                                         
393202     END-SEARCH                                                           
393302     .                                                                    
393402     EJECT                                                                
