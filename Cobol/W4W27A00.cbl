000100 PROCESS DYNAM                                                            
000200*COMPOPT DB2BIND=YES                                                      
000300 ID DIVISION.                                                             
000400 PROGRAM-ID.     W4W27A00.                                                
000500 AUTHOR.         ARUP DATTA.                                              
000600 DATE-WRITTEN.   NOVEMBER 2014.                                           
000700                                                                          
000800     REMARKS.                                                             
000900*    W4W27A00 IS A NEW PROGRAM. THIS PROGRAM SHOWS ALL THE                
001000*    VOR RELATED DATA IN ONE SCREEN.                                      
001100*                                                                         
001200*    NAMN:       CARPARTS.VOR.TOTALINFO                                   
001300*                                                                         
001400                                                                          
001500     REMARKS.                                                             
001600*                                                                         
001700*    FUNCTION.   TP-PROGRAM. FOR FACILITATING EXECUTION OF                
001800*                VOR ORDER, ALL INFORMATION RELATED TO THE                
001900*                PART IS DISPLAYED.                                       
002000*                                                                         
002100*                DATABASES (ONLY READ ACCESS)                             
002200*                    DLI  WDK6  - ARTIKELREGISTER                         
002300*                         WDD3  - BENÄMNINGSREGISTER                      
002400*                         WDB6  - DC STYRPARAMETRAR                       
002500*                         WDD9  - LEVERANSPLAN-REGISTER                   
002600*                         WDD7  - ERSÄTTNINGSREG                          
002700*                         WDP3  - PERSONKODSREGISTER                      
002800*                         WDB2  - KUNDREGISTER GODSMOTTAGARE              
002900*                         WDB3  - KUNDREGISTER-GODSMOTTAGARE              
003000*                         WDL2  - INLEV-HISTORIK                          
003100*                         WDK7  - ARTIKELREGISTER S-LAGER                 
003200*                         WDD8  - BUFFERT REGISTER                        
003300*                         W6D1  - INLEVERANSREGISTER                      
003400*                         WDA5  - RESTORDER-REGISTER + TPO                
003500*                         WDL6  - INLEVERANS HISTORIK                     
003600                          WDP5  - INFORMATION TEXT REGISTER               
003700*                    DB2  BYPRO - BYTESARTIKEL/PRODUKTIONSNUMMER-         
003800*                                 RELATION                                
003900*                                                                         
004000*                                                                         
004100*                                                                         
004200*    INDATA.                                                              
004300*        TRANSAKTION: W4W27AT                                             
004400*        REQUEST    : WZ01REQU                                            
004500*                     W4W27AI1                                            
004600*                                                                         
004700*    UTDATA.                                                              
004800*        RESPONSE   : WZ01RESP                                            
004900*                     W4W27AO1                                            
005000*                                                                         
005100*                                                                         
005200 ENVIRONMENT DIVISION.                                                    
005300                                                                          
005400 DATA DIVISION.                                                           
005500     EJECT                                                                
005600 WORKING-STORAGE SECTION.                                                 
005700     SKIP3                                                                
005800 77  PROGRAM-NAMN                    PIC X(8)  VALUE 'W4W27A00'.          
005900                                                                          
006000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
006100 77  FELTEXT                         PIC X(64) VALUE SPACE.               
006200 77  KDRC-DISPLAY                    PIC Z(5)  VALUE ZERO.                
006300*                                                                         
006400 77  JA                              PIC X     VALUE 'J'.                 
006500 77  NEJ                             PIC X     VALUE 'N'.                 
006600 77  SW-TRAEFF                       PIC X     VALUE SPACE.               
006700 77  DEL-RAPP-FINNS                  PIC X     VALUE 'N'.                 
006800 77  FOERSTA-GAANG                   PIC X     VALUE 'N'.                 
006900 77  IX                              PIC S9(3) VALUE +1   COMP-3.         
007000 77  IX-A                            PIC S9(3) VALUE +1   COMP-3.         
007100 77  INDX                            PIC S9(3) VALUE +1   COMP-3.         
007200 77  MAX-INDX                        PIC S9(4) VALUE +6                   
007300                                                        COMP SYNC.        
007400 77  SDC-IX                          PIC S9(3) VALUE +1   COMP-3.         
007500 77  MAX-ANT-PROENH                  PIC S9(9) VALUE +3                   
007600                                                       COMP SYNC.         
007700 77  MAX-IX-IDDC                     PIC S9(9) VALUE +6                   
007800                                                       COMP SYNC.         
007900 77  INDX-TVSVOR-MAX                 PIC S9(9) VALUE +16                  
008000                                                       COMP SYNC.         
008100 77  WS-FLKLAR                       PIC  X    VALUE SPACE.               
008200 77  WS-KVROS-SDC                    PIC S9(7) VALUE ZERO COMP-3.         
008300 77  WS-KVDISP-SLAG                  PIC S9(7) VALUE ZERO COMP-3.         
008400 77  WS-KVRADER-MAX1                 PIC  9(5) VALUE 100.                 
008500 77  WS-KVRADER-MAX2                 PIC  9(5) VALUE 500.                 
008600 77  WS-KVRADER-MAX7                 PIC  9(5) VALUE  50.                 
008700 77  WS-KVRADER-MAX8                 PIC  9(5) VALUE  50.                 
008800 77  W-KVART-TOT-C1                  PIC S9(9) VALUE ZERO COMP-3.         
008900 77  WS-KVAVIS                       PIC S9(7) VALUE ZERO COMP-3.         
009000 77  W-IDARTNR-BYT                   PIC S9(9) VALUE ZERO COMP-3.         
009100 77  W-IDPRODNR                      PIC S9(9) VALUE ZERO COMP-3.         
009200 77  W-IDLEVNR                       PIC  X(5) VALUE SPACE.               
009300 77  SPAR-IDANSK                     PIC S9(3) VALUE +0   COMP-3.         
009400 77  SPAR-IDBERED                    PIC S9(3) VALUE +0   COMP-3.         
009500 77  SPAR-IDLEVNR                    PIC  X(5) VALUE SPACE.               
009600 77  SPAR-IDFKNGRP                   PIC S9(5) VALUE ZERO COMP-3.         
009700 77  SPAR-IDARTNR                    PIC  9(9).                           
009800 77  SPAR-IDLOPNRM                   PIC  9(9) VALUE ZERO.                
009900 77  SPAR-IDDC                       PIC  X(2).                           
010000 77  WS-IDPERSON                     PIC S9(3) VALUE +0   COMP-3.         
010100 77  W-DEL-KVRAPP                    PIC S9(7) VALUE ZERO COMP-3.         
010200 77  W-C2-FORDEL                     PIC S9(7) VALUE ZERO COMP-3.         
010300 77  WS-TIUPPDAT                     PIC S9(7) VALUE ZERO COMP-3.         
010400 77  BUTIKSORDER                     PIC  X(2) VALUE 'PC'.                
010500 77  VERKSTADSORDER                  PIC  X(2) VALUE 'PW'.                
010600 77  WS-CURR-AAVVD                   PIC  9(5) VALUE ZERO.                
010700 77  WS-CURR-AAMMDD                  PIC  9(6) VALUE ZERO.                
010800 77  WS-TIBERANK                     PIC  9(6) VALUE ZERO.                
010900                                                                          
011000*                                                                         
011100 77  TRAEFF-SW                       PIC  X    VALUE 'J'.                 
011200     88  TRAEFF-JA                             VALUE 'J'.                 
011300     88  TRAEFF-NEJ                            VALUE 'N'.                 
011400                                                                          
011500 77  SLUT-SW                         PIC  X    VALUE 'J'.                 
011600     88  SLUT-JA                               VALUE 'J'.                 
011700     88  SLUT-NEJ                              VALUE 'N'.                 
011800                                                                          
011900 77  NYCKLAR-SW                      PIC  X    VALUE 'J'.                 
012000     88  NYCKLAR-OK                            VALUE 'J'.                 
012100     88  NYCKLAR-FEL                           VALUE 'N'.                 
012200                                                                          
012300 77  ARTIKEL-SW                      PIC  X    VALUE 'J'.                 
012400     88  ARTIKEL-OK                            VALUE 'J'.                 
012500     88  ARTIKEL-FEL                           VALUE 'N'.                 
012600 77  WS-WDL2-ART-SW                  PIC X     VALUE 'J'.                 
012700     88 WS-WDL2-ART-FOUND                      VALUE 'J'.                 
012800     88 WS-WDL2-ART-SAKNAS                     VALUE 'N'.                 
012900 77  WS-WDL2-INL-SW                  PIC X     VALUE 'J'.                 
013000     88 WS-WDL2-INL-FOUND                      VALUE 'J'.                 
013100     88 WS-WDL2-INL-SAKNAS                     VALUE 'N'.                 
013200 77  WS-WDL6-ART-SW                  PIC X     VALUE 'J'.                 
013300     88 WS-WDL6-ART-FOUND                      VALUE 'J'.                 
013400     88 WS-WDL6-ART-SAKNAS                     VALUE 'N'.                 
013500 77  WS-WDL6-INL-SW                  PIC X     VALUE 'J'.                 
013600     88 WS-WDL6-INL-FOUND                      VALUE 'J'.                 
013700     88 WS-WDL6-INL-SAKNAS                     VALUE 'N'.                 
013800                                                                          
013900 01  WS-TIREPDAT-X.                                                       
014000     03 WS-TIREPDAT                  PIC S9(7) COMP-3.                    
014100                                                                          
014200 01  WS-DAT                          PIC  9(6).                           
014300 01  FILLER REDEFINES WS-DAT.                                             
014400     03 WS-YEAR                      PIC  9(2).                           
014500     03 WS-MONTH                     PIC  9(2).                           
014600     03 WS-DAYS                      PIC  9(2).                           
014700 01  WS-TID                          PIC  9(6).                           
014800 01  FILLER REDEFINES WS-TID.                                             
014900     03 WS-HOURS                     PIC  9(2).                           
015000     03 WS-MINUTES                   PIC  9(2).                           
015100     03 WS-SECONDS                   PIC  9(2).                           
015200                                                                          
015300*                                                                         
015400 01  WS-IDARTNR                      PIC  X(9).                           
015500 01  IDARTNR-WS REDEFINES WS-IDARTNR PIC  9(9).                           
015600 01  WS-IDDISTR                      PIC  X(4).                           
015700 01  IDDISTR-WS REDEFINES WS-IDDISTR PIC  9(4).                           
015800 01  WS-IDKUNDNR                     PIC  X(6).                           
015900 01  IDKUNDNR-WS REDEFINES                                                
016000                        WS-IDKUNDNR  PIC  9(6).                           
016100*                                                                         
016200 01  WS-DALEVBSK-AVS                 PIC  9(8).                           
016300 01  FILLER  REDEFINES WS-DALEVBSK-AVS.                                   
016400     03  WS-DALEVBSK-SS              PIC  9(2).                           
016500     03  WS-DALEVBSK-AAMMDD          PIC  9(6).                           
016600*                                                                         
016700 01  WS-IDRADNR                      PIC  9(5).                           
016800 01  WS-IDRADNR-X REDEFINES WS-IDRADNR.                                   
016900     03  FILLER                      PIC  X(2).                           
017000     03  WS-RED-IDRADNR              PIC  X(3).                           
017100                                                                          
017200 01  WS-KVINLART                     PIC S9(7).                           
017300 01  WS-SPAR-IDLOPNRM                PIC  9(9) VALUE ZERO.                
017400 01  WS-SPAR-IDLEVNR                 PIC  X(5) VALUE SPACE.               
017500 01  WS-TIREGDAT                     PIC 9(6)  VALUE ZERO.                
017600 01  WS-IDINLEV-REGDAT               PIC 9(6)  VALUE ZERO.                
017700 01  WS-DAINLEV                      PIC 9(16) VALUE ZERO.                
017800                                                                          
017900     EJECT                                                                
018000*    --- PARAMETRAR TILL SUBPROGRAM WINTSOR                               
018100 01  TABENTRY-PARM.                                                       
018200     03  STEGLANGD                   PIC S9(9) COMP   VALUE 36.           
018300     03  ANTAL                       PIC S9(9) COMP.                      
018400     03  NYCKELLANGD                 PIC S9(9) COMP   VALUE 10.           
018500                                                                          
018600 01  IX-RAD                          PIC S9(3) COMP-3 VALUE 1.            
018700 01  IX-RAD-TAB                      PIC S9(3) COMP-3 VALUE 1.            
018800                                                                          
018900 01  TAB-MAX                         PIC S9(9) COMP   VALUE 200.          
019000 01  WS-IDAVINR                      PIC Z(6)9        VALUE ZERO.         
019100 01  X                               PIC 9            VALUE ZERO.         
019200 01  AX                              PIC 9            VALUE ZERO.         
019300 01  BX                              PIC 9            VALUE ZERO.         
019400*    --- TABELL SOM SORTERAS AV WINTSOR                                   
019500 01  TABELL.                                                              
019600     03  TAB-POST  OCCURS 200.                                            
019700       04  TAB-RAD.                                                       
019800         05  TAB-PLATSTYP            PIC  X(1).                           
019900         05  TAB-ADBUFFOMR           PIC  9(2).                           
020000         05  TAB-ADBUFFGANG          PIC  9(2).                           
020100         05  TAB-ADBUFFPL            PIC  9(5).                           
020200         05  TAB-KVBUFF-F            PIC S9(8).                           
020300         05  TAB-KVBUFF-OF           PIC  9(8).                           
020400       04  TAB-SORT.                                                      
020500         05  TAB-PLATSTYP-SORT       PIC  X(1).                           
020600         05  TAB-ADBUFFOMR-SORT      PIC  9(2).                           
020700         05  TAB-ADBUFFGANG-SORT     PIC  9(2).                           
020800         05  TAB-ADBUFFPL-SORT       PIC  9(5).                           
020900     SKIP3                                                                
021000                                                                          
021100 01  SPLIT-DAINLEV                   PIC 9(16).                           
021200 01  FILLER    REDEFINES SPLIT-DAINLEV.                                   
021300     03  SPLIT-TISEKEL               PIC  9(2).                           
021400     03  SPLIT-TIAAMMDD              PIC  9(6).                           
021500     03  FILLER                      PIC  9(8).                           
021600                                                                          
021700***                                                                       
021800*** KEYS TO ACESS IMS DATABASES                                           
021900***                                                                       
022000 01  NYCKLAR-TILL-DLI.                                                    
022100     03  W-IDARTNR-X.                                                     
022200         05  W-IDARTNR           PIC S9(9) VALUE ZERO COMP-3.             
022300     03  W-IDDC-X.                                                        
022400         05  W-IDDC              PIC X(2)  VALUE '11'.                    
022500     03  W-KDSEGKEY-X.                                                    
022600         05  W-KDSEGKEY          PIC X(1)  VALUE '1'.                     
022700     03  W-IDSKYLT-X.                                                     
022800         05  W-IDSKYLT           PIC X(3)  VALUE 'GB'.                    
022900     03  W-KDNOTTYP-X.                                                    
023000         05  W-KDNOTTYP          PIC S9(1) VALUE ZERO COMP-3.             
023100*                                                                         
023200     03  W-W6D1HSEQ-X.                                                    
023300         05  W-IDARTNR-HSEQ      PIC S9(9) VALUE ZERO COMP-3.             
023400*                                                                         
023500     03  W-IDDC-B6-X.                                                     
023600         05  W-IDDC-B6           PIC X(2)  VALUE SPACE.                   
023700*                                                                         
023800     03  W-WDD901KY-X.                                                    
023900         05  W-IDARTNR-LEV-X.                                             
024000             07  W-IDARTNR-LEV   PIC S9(9) VALUE ZERO COMP-3.             
024100         05  W-IDDC-LEV-X.                                                
024200             07  W-IDDC-LEV      PIC X(2)  VALUE '11'.                    
024300     03  W-WDD902KY-X.                                                    
024400         05  W-IDLEVNR-LEV-X.                                             
024500             07  W-IDLEVNR-LEV   PIC X(5)  VALUE SPACE.                   
024600     03  W-WDD925KY-X.                                                    
024700         05  W-IDLEVBSK-X.                                                
024800             07  W-IDLEVBSK      PIC S9(1) VALUE ZERO COMP-3.             
024900*                                                                         
025000     03  W-ERSATT-IDARTNR-X.                                              
025100         05  W-ERSATT-IDARTNR    PIC S9(9) VALUE ZERO COMP-3.             
025200*                                                                         
025300     03  W-IDPERSON-X.                                                    
025400         05  W-IDPERSON          PIC S9(3) VALUE +0   COMP-3.             
025500     03  W-KDARBTYP-X.                                                    
025600         05  W-KDARBTYP          PIC  X(8) VALUE SPACE.                   
025700     03  W-WDP3A1-MIN.                                                    
025800         05  W-IDLAND-A-MIN      PIC  X(2) VALUE 'SE'.                    
025900         05  W-IDARTNRF-MIN      PIC S9(9) VALUE ZERO COMP-3.             
026000         05  W-IDARTNRT-MIN      PIC S9(9) VALUE ZERO COMP-3.             
026100         05  W-KDARBTYP-A-MIN    PIC  X(8) VALUE SPACE.                   
026200     03  W-WDP3A1-MAX.                                                    
026300         05  W-IDLAND-A-MAX      PIC  X(2) VALUE 'SE'.                    
026400         05  W-IDARTNRF-MAX      PIC S9(9) VALUE ZERO COMP-3.             
026500         05  W-IDARTNRT-MAX      PIC S9(9) VALUE ZERO COMP-3.             
026600         05  W-KDARBTYP-A-MAX    PIC  X(8) VALUE SPACE.                   
026700     03  W-WDP3B1-X.                                                      
026800         05  W-IDLAND-B          PIC  X(2) VALUE 'SE'.                    
026900         05  W-IDLEVNR-B         PIC  X(5) VALUE SPACE.                   
027000         05  W-KDARBTYP-B        PIC  X(8) VALUE SPACE.                   
027100     03  W-WDP3C1-MIN.                                                    
027200         05  W-IDLAND-C-MIN      PIC  X(2) VALUE 'SE'.                    
027300         05  W-IDFKNGRPF-MIN     PIC S9(5) VALUE ZERO COMP-3.             
027400         05  W-IDFKNGRPT-MIN     PIC S9(5) VALUE ZERO COMP-3.             
027500         05  W-KDARBTYP-C-MIN    PIC  X(8) VALUE SPACE.                   
027600     03  W-WDP3C1-MAX.                                                    
027700         05  W-IDLAND-C-MAX      PIC  X(2) VALUE 'SE'.                    
027800         05  W-IDFKNGRPF-MAX     PIC S9(5) VALUE ZERO COMP-3.             
027900         05  W-IDFKNGRPT-MAX     PIC S9(5) VALUE ZERO COMP-3.             
028000         05  W-KDARBTYP-C-MAX    PIC  X(8) VALUE SPACE.                   
028100*                                                                         
028200     03  W-IDGMT-X.                                                       
028300         05  W-IDDISTR           PIC S9(5) VALUE ZERO COMP-3.             
028400         05  W-IDKUNDNR          PIC S9(7) VALUE ZERO COMP-3.             
028500*                                                                         
028600     03  W-WDB301KY-X.                                                    
028700         05  W-IDDC-WDB3         PIC X(2)  VALUE SPACE.                   
028800         05  W-IDDISTR-WDB3      PIC S9(5) VALUE ZERO COMP-3.             
028900         05  W-IDKUNDNR-WDB3     PIC S9(7) VALUE ZERO COMP-3.             
029000     03  W-WDB301KY-DEF-X.                                                
029100         05  W-IDDC-WDB3-DEF     PIC X(2)  VALUE SPACE.                   
029200         05  W-IDDISTR-WDB3-DEF  PIC S9(5) VALUE ZERO COMP-3.             
029300         05  W-IDKUNDNR-WDB3-DEF PIC S9(7) VALUE +9999999                 
029400                                                      COMP-3.             
029500     03  W-DAINLEV-X.                                                     
029600         05  W-DAINLEV           PIC 9(16) VALUE ZERO.                    
029700*                                                                         
029800     03  W-WDD811KY-MIN-X.                                                
029900         05  W-IDDC-WDD8-MIN     PIC  X(2).                               
030000         05  W-ADBUFFOMR-MIN     PIC S9(3) VALUE ZERO COMP-3.             
030100         05  W-DABUFPAF-MIN      PIC  9(8) VALUE ZERO.                    
030200         05  W-ADBUFFGANG-MIN    PIC S9(3) VALUE ZERO COMP-3.             
030300         05  W-ADBUFFPL-MIN      PIC S9(5) VALUE ZERO COMP-3.             
030400     03  W-WDD811KY-MAX-X.                                                
030500         05  W-IDDC-WDD8-MAX     PIC  X(2).                               
030600         05  W-ADBUFFOMR-MAX     PIC S9(3) VALUE +99  COMP-3.             
030700         05  W-DABUFPAF-MAX      PIC  9(8) VALUE  99999999.               
030800         05  W-ADBUFFGANG-MAX    PIC S9(3) VALUE +99  COMP-3.             
030900         05  W-ADBUFFPL-MAX      PIC S9(5) VALUE +99999                   
031000                                                      COMP-3.             
031100*                                                                         
031200     02 W-MINKEY-W6D101KY.                                                
031300         03  W-MINKEY-IDLEVNR    PIC  X(5) VALUE SPACE.                   
031400         03  W-MINKEY-IDFS       PIC  X(8) VALUE SPACE.                   
031500         03  W-MINKEY-TIAVIDAT   PIC S9(7) VALUE +0   COMP-3.             
031600         03  W-MINKEY-IDRADNR-INL                                         
031700                                 PIC S9(5) VALUE +0   COMP-3.             
031800         03  W-MINKEY-IDRADNR    PIC S9(5) VALUE +0   COMP-3.             
031900         03  W-MINKEY-IDARTNR    PIC S9(9) VALUE +0   COMP-3.             
032000         03  W-MINKEY-IDLOPNRM   PIC S9(9) VALUE +0   COMP-3.             
032100     03  W-W6D1H1KY-X.                                                    
032200         05  WH1-IDARTNR         PIC S9(9) VALUE ZERO COMP-3.             
032300         05  WH1-IDDC            PIC  X(2) VALUE SPACE.                   
032400         05  WH1-IDLEVNR         PIC  X(5) VALUE SPACE.                   
032500         05  WH1-IDFS            PIC  X(8) VALUE SPACE.                   
032600         05  WH1-TIAVIDAT        PIC S9(7)            COMP-3.             
032700         05  WH1-IDRADNR-INL     PIC S9(5)            COMP-3.             
032800                                                                          
032900     03  W-W6D1H1KY-MIN-X.                                                
033000         05  WH1-MIN-IDARTNR     PIC S9(9) VALUE ZERO COMP-3.             
033100         05  WH1-MIN-IDDC        PIC  X(2) VALUE SPACE.                   
033200         05  WH1-MIN-IDLEVNR     PIC  X(5) VALUE SPACE.                   
033300         05  WH1-MIN-IDFS        PIC  X(8) VALUE SPACE.                   
033400         05  WH1-MIN-TIAVIDAT    PIC S9(7)            COMP-3.             
033500         05  WH1-MIN-IDRADNR-INL PIC S9(5) VALUE ZERO COMP-3.             
033600                                                                          
033700     03  W-W6D1H1KY-MAX-X.                                                
033800         05  WH1-MAX-IDARTNR     PIC S9(9) VALUE ZERO COMP-3.             
033900         05  WH1-MAX-IDDC        PIC  X(2) VALUE SPACE.                   
034000         05  WH1-MAX-IDLEVNR     PIC  X(5) VALUE SPACE.                   
034100         05  WH1-MAX-IDFS        PIC  X(8) VALUE SPACE.                   
034200         05  WH1-MAX-TIAVIDAT    PIC S9(7)            COMP-3.             
034300         05  WH1-MAX-IDRADNR-INL PIC S9(5) VALUE +99999                   
034400                                                      COMP-3.             
034500     03  W-IDRADNR-INL-X.                                                 
034600         05  W-IDRADNR-INL       PIC S9(5)            COMP-3.             
034700     03  W-IDRADNR-X.                                                     
034800         05  W-IDRADNR-D1        PIC S9(5)            COMP-3.             
034900     03  W-W6D101KY-X.                                                    
035000         05  W-IDDC-D1           PIC X(2)  VALUE SPACE.                   
035100         05  W-IDLEVNR-D1        PIC X(5)  VALUE SPACE.                   
035200         05  W-IDFS-D1           PIC X(8)  VALUE SPACE.                   
035300         05  W-TIAVIDAT-D1       PIC S9(7)            COMP-3.             
035400     03  W-IDARTNR-D1-X.                                                  
035500         05  W-IDARTNR-D1        PIC S9(9)            COMP-3.             
035600*                                                                         
035700     03  W-KDORDKL-X.                                                     
035800         05  W-KDORDKL           PIC S9    VALUE 3    COMP-3.             
035900     03  W-IDSYSTX3-X.                                                    
036000         05  W-IDSYSTX3          PIC X(3)  VALUE 'LDC'.                   
036100     03  W-KDSTARAD-X.                                                    
036200         05  W-KDSTARAD          PIC X(1)  VALUE '3'.                     
036300                                                                          
036400     03  W-WDA5A1KY-MIN-X.                                                
036500         05  W-WDA5A1-IDARTNR-MIN    PIC S9(9)        COMP-3.             
036600         05  FILLER                  PIC X(35) VALUE LOW-VALUE.           
036700                                                                          
036800     03  W-WDA5A1KY-MAX-X.                                                
036900         05  W-WDA5A1-IDARTNR-MAX    PIC S9(9)        COMP-3.             
037000         05  FILLER                  PIC X(35) VALUE HIGH-VALUE.          
037100                                                                          
037200     03  W-WDA5KEY-X.                                                     
037300         05  W-A5-IDDISTR-KEY        PIC S9(5) VALUE ZERO COMP-3.         
037400         05  W-A5-IDKUNDNR-KEY       PIC S9(7) VALUE ZERO COMP-3.         
037500         05  W-A5-IDKUNDRF-KEY       PIC X(10) VALUE SPACE.               
037600         05  FILLER REDEFINES W-A5-IDKUNDRF-KEY.                          
037700             07  W-A5-IDORDNR-KEY    PIC 9(5).                            
037800             07  FILLER              PIC X(5).                            
037900         05  W-A5-IDARTNR-KEY        PIC S9(9) VALUE ZERO COMP-3.         
038000         05  W-A5-IDLOPNR-KEY        PIC S9(3) VALUE ZERO COMP-3.         
038100                                                                          
038200     03  W-WDP501KY-X.                                                    
038300         05  W-IDSKYLT-P5            PIC X(3)    VALUE SPACE.             
038400         05  W-IDDOKTYP              PIC X(8)    VALUE SPACE.             
038500         05  W-IDDOK                 PIC X(8)    VALUE SPACE.             
038600                                                                          
038700     03  W-IDSID-X.                                                       
038800         05  W-IDSID                 PIC S9(3) VALUE ZERO COMP-3.         
038900                                                                          
039000     EJECT                                                                
039100*                                                                         
039200 01    DYNAMISKA-SUBPROGRAM.                                              
039300   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI'.             
039400   03  FELLOG                    PIC X(8)    VALUE 'FELLOG'.              
039500   03  WMEDKONV                  PIC X(8)    VALUE 'WMEDKONV'.            
039600   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
039700   03  WINTSOR                   PIC X(8)    VALUE 'WINTSOR'.             
039800   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
039900   03  WZ01SEND                  PIC X(8)    VALUE 'WZ01SEND'.            
040000   03  WZ01SUB                   PIC X(8)    VALUE 'WZ01SUB '.            
040100*                                                                         
040200*    --- PARAMETERS TO ABEND                                              
040300 77  FILLER                      PIC X(08)   VALUE 'ABENDKOD'.            
040400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +33.              
040500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
040600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
040700     SKIP2                                                                
040800 77  FILLER                      PIC X(08)   VALUE 'MESSAGES'.            
040900 01  MESSAGE-CODES.                                                       
041000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
041100     03  ERR-PART-MISSING        PIC X(3)    VALUE '025'.                 
041200     03  ERR-INVALID-KEY-COMB    PIC X(3)    VALUE '032'.                 
041300     03  INF-DIST-CUST-NEEDED    PIC X(3)    VALUE '026'.                 
041400     03  INF-DIST-CUST-MISSING   PIC X(3)    VALUE '025'.                 
041500     03  INF-PART-REPLACED       PIC X(3)    VALUE '321'.                 
041600     03  INF-PART-EXPIRE         PIC X(3)    VALUE '322'.                 
041700     03  INF-REPLACING-PART      PIC X(3)    VALUE '323'.                 
041800*                                                                         
041900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
042000*                                                                         
042100 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
042200     SKIP3                                                                
042300*01  -COPY WZ01SUB                                                        
042400     EJECT                                                                
042500 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
042600     SKIP3                                                                
042700 01  REQU-AREA.                                                           
042800*    03  -COPY WZ01REQU                                                   
042900*    03  -COPY W4W27AI1                                                   
043000     EJECT                                                                
043100 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
043200     SKIP3                                                                
043300 01  RESP-AREA.                                                           
043400*    03  -COPY WZ01RESP                                                   
043500*    03  -COPY W4W27AO1                                                   
043600     SKIP3                                                                
043700*01  -COPY WWLNDKON                                                       
043800*01  -COPY WWDC99                                                         
043900     EJECT                                                                
044000 01  FILLER                      PIC X(16) VALUE   'WDATAREA '.           
044100*01  -COPY WDATAREA.                                                      
044200     EJECT                                                                
044300 01  FILLER                      PIC X(16) VALUE   'WWLEV04  '.           
044400*01  -COPY WWLEV04.                                                       
044500     EJECT                                                                
044600 01  FILLER                      PIC X(16) VALUE   'WY2000W1 '.           
044700*    -COPY WY2000W1                                                       
044800     EJECT                                                                
044900******************************************************************        
045000***                                                                       
045100***      ARBETS-AREOR TILL DB2- OCH IMS-SEKTIONERNA                       
045200***                                                                       
045300 01  FILLER                      PIC X(16) VALUE 'DB2-WS     '.           
045400*01  -COPY BYPRO -PRE BYPRO-                                              
045500 01  FILLER                      PIC X(16) VALUE 'BYPRO-AREA'.            
045600       EXEC SQL INCLUDE BYPRO END-EXEC.                                   
045700     SKIP3                                                                
045800 01  FILLER                      PIC X(16) VALUE 'SQLCA-AREA'.            
045900       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
046000*                        **** STATUS-KOD FRÅN DB2                         
046100 01  FILLER                      PIC X(16) VALUE 'SQLCODE-WS'.            
046200 01  DB2-WS.                                                              
046300     03  SQLCODE-WS              PIC 9(3)  VALUE ZERO.                    
046400         88  CURSOR-OK                     VALUE 000.                     
046500         88  RADER-FINNS                   VALUE 000.                     
046600         88  RADER-SAKNAS                  VALUE 100.                     
046700         88  904-KOD                       VALUE 904.                     
046800     SKIP1                                                                
046900     03  GODK-SQLCODESKODER.                                              
047000         05  GODK-SQLCODE OCCURS 5                                        
047100             INDEXED BY SQLCODE-IX PIC 999.                               
047200     EJECT                                                                
047300 01  IMS-WS.                                                              
047400     03    FILLER                PIC X(16) VALUE 'IMS-WS'.                
047500                                                                          
047600*                                STATUS-KOD FRÅN IMS                      
047700     03  STATUS-WS               PIC XX.                                  
047800         88  SEGMENT-FINNS                 VALUE '  '.                    
047900         88  SEGMENT-SAKNAS                VALUE 'GE'.                    
048000         88  SEGMENT-SLUT                  VALUE 'GB'.                    
048100                                                                          
048200     03      GODK-STATUSKODER.                                            
048300         05  GODK-STATUS OCCURS 3 INDEXED BY STATUS-IX PIC XX.            
048400                                                                          
048500 01  SSA1                        PIC X(512).                              
048600 01  SSA2                        PIC X(256).                              
048700 01  SSA3                        PIC X(128).                              
048800     EJECT                                                                
048900***                                                                       
049000***                IMS FUNKTIONSKODER                                     
049100***                                                                       
049200*01      -COPY W0003                                                      
049300     EJECT                                                                
049400*                                DLI INPUT-OUTPUT AREA                    
049500 01  FILLER                  PIC X(24)   VALUE 'DLI-IO-WDK601'.           
049600 01  DLI-IO-WDK601.                                                       
049700*    03  -COPY WDK601                                                     
049800     EJECT                                                                
049900 01  FILLER                  PIC X(24)   VALUE 'DLI-IO-WDK611'.           
050000 01  DLI-IO-WDK611.                                                       
050100*    03  -COPY WDK611                                                     
050200     EJECT                                                                
050300 01  FILLER                  PIC X(24)   VALUE 'DLI-IO-WDK625'.           
050400 01  DLI-IO-WDK625.                                                       
050500*    03  -COPY WDK625                                                     
050600     EJECT                                                                
050700 01  FILLER                  PIC X(24)   VALUE 'DLI-IO-WDD311'.           
050800 01  DLI-IO-WDD311.                                                       
050900*    03  -COPY WDD311                                                     
051000     EJECT                                                                
051100 01  FILLER                  PIC X(24)   VALUE 'DLI-IO-WDB601'.           
051200 01  DLI-IO-WDB601.                                                       
051300*    03  -COPY WDB601                                                     
051400     EJECT                                                                
051500 01  FILLER                  PIC X(24)   VALUE 'DLI-IO-WDD902'.           
051600 01  DLI-IO-WDD902.                                                       
051700*    03  -COPY WDD902  -PRE WDD9-                                         
051800     EJECT                                                                
051900 01  FILLER                  PIC X(24)   VALUE 'DLI-IO-WDD924'.           
052000 01  DLI-IO-WDD924.                                                       
052100*    03  -COPY WDD924                                                     
052200     EJECT                                                                
052300 01  FILLER                  PIC X(24)   VALUE 'DLI-IO-WDD925'.           
052400 01  DLI-IO-WDD925.                                                       
052500*    03  -COPY WDD925                                                     
052600     EJECT                                                                
052700 01  FILLER                  PIC X(24)   VALUE 'DLI-IO-WDD7  '.           
052800 01  DLI-IO-WDD7.                                                         
052900     03  DLI-IO-WDD702.                                                   
053000*      05  -COPY WDD702  -PRE TILLK-                                      
053100     EJECT                                                                
053200     03  DLI-IO-WDD701.                                                   
053300*      05  -COPY WDD701  -PRE ERSATT-                                     
053400     EJECT                                                                
053500 01  FILLER                  PIC X(24)   VALUE 'DLI-IO-WDP311'.           
053600 01  DLI-IO-WDP311.                                                       
053700*    03  -COPY WDP311                                                     
053800     EJECT                                                                
053900 01  FILLER                  PIC X(24)   VALUE 'DLI-IO-WDP3A'.            
054000 01  DLI-IO-WDP3A.                                                        
054100*    03  -COPY WDP3A1                                                     
054200     EJECT                                                                
054300 01  FILLER                  PIC X(24)   VALUE 'DLI-IO-WDP3B'.            
054400 01  DLI-IO-WDP3B.                                                        
054500*    03  -COPY WDP3B1                                                     
054600     EJECT                                                                
054700 01  FILLER                  PIC X(24)   VALUE 'DLI-IO-WDP3C'.            
054800 01  DLI-IO-WDP3C.                                                        
054900*    03  -COPY WDP3C1                                                     
055000     EJECT                                                                
055100 01  FILLER                  PIC X(24)   VALUE 'DLI-IO-WDB201'.           
055200 01  DLI-IO-WDB201.                                                       
055300*    03  -COPY WDB201  -PRE GMTA-                                         
055400     EJECT                                                                
055500 01  FILLER                  PIC X(24)   VALUE 'DLI-IO-WDB301'.           
055600 01  DLI-IO-WDB301.                                                       
055700*    03  -COPY WDB301  -PRE GMTB-                                         
055800     EJECT                                                                
055900 01  FILLER                  PIC X(24)   VALUE 'DLI-IO-WDL2  '.           
056000 01  DLI-IO-WDL2             PIC X(200)  VALUE SPACE.                     
056100     EJECT                                                                
056200*01  -COPY WDL201      -RED DLI-IO-WDL2.                                  
056300     EJECT                                                                
056400*01  -COPY WDL211      -RED DLI-IO-WDL2.                                  
056500     EJECT                                                                
056600*01  -COPY WDL221      -RED DLI-IO-WDL2.                                  
056700     EJECT                                                                
056800*01  -COPY WDL222      -RED DLI-IO-WDL2.                                  
056900     EJECT                                                                
057000*01  -COPY WDL223      -RED DLI-IO-WDL2.                                  
057100     EJECT                                                                
057200*01  -COPY WDL231      -RED DLI-IO-WDL2.                                  
057300     EJECT                                                                
057400 01  FILLER                  PIC X(24)   VALUE 'DLI-IO-WDK701'.           
057500 01  DLI-IO-WDK701.                                                       
057600*    03  -COPY WDK701                                                     
057700     EJECT                                                                
057800 01  FILLER                  PIC X(24)   VALUE 'DLI-IO-WDK711'.           
057900 01  DLI-IO-WDK711.                                                       
058000*    03  -COPY WDK711                                                     
058100     EJECT                                                                
058200 01  FILLER                  PIC X(24)   VALUE 'DLI-IO-WDD801'.           
058300 01  DLI-IO-WDD801.                                                       
058400*    03  -COPY WDD801  -PRE BUFF-                                         
058500     EJECT                                                                
058600 01  FILLER                  PIC X(24)   VALUE 'DLI-IO-WDD811'.           
058700 01  DLI-IO-WDD811.                                                       
058800*    03  -COPY WDD811  -PRE BUFF-                                         
058900     EJECT                                                                
059000 01  FILLER                  PIC X(24)   VALUE 'DLI-IO-W6D1  '.           
059100 01  DLI-IO-W6D1             PIC X(150)  VALUE SPACE.                     
059200     EJECT                                                                
059300*01  -COPY W6D101              -RED DLI-IO-W6D1.                          
059400     EJECT                                                                
059500*01  -COPY W6D111      -PRE H- -RED DLI-IO-W6D1.                          
059600     EJECT                                                                
059700*01  -COPY W6D121              -RED DLI-IO-W6D1.                          
059800     EJECT                                                                
059900*01  -COPY W6D1H1              -RED DLI-IO-W6D1.                          
060000     EJECT                                                                
060100 01  FILLER                  PIC X(24)   VALUE 'DLI-IO-WDA5  '.           
060200 01  DLI-IO-WDA5.                                                         
060300*    03  WDA5 -COPY WDA501                                                
060400     EJECT                                                                
060500 01  FILLER                  PIC X(24)   VALUE 'DLI-IO-WDA5A '.           
060600 01  DLI-IO-WDA5A.                                                        
060700*    03  WDA5A -COPY WDA5A1                                               
060800     EJECT                                                                
060900 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDL601'.            
061000 01  DLI-IO-WDL601.                                                       
061100*    03    -COPY WDL601 -PRE WDL6-                                        
061200     EJECT                                                                
061300 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDL611'.            
061400 01  DLI-IO-WDL611.                                                       
061500*    03    -COPY WDL611 -PRE WDL6-                                        
061600     EJECT                                                                
061700 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDL612'.            
061800 01  DLI-IO-WDL612.                                                       
061900*    03    -COPY WDL612 -PRE WDL6-                                        
062000     EJECT                                                                
062100 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDP501'.           
062200 01  DLI-IO-WDP501.                                                       
062300*    03  -COPY WDP501                                                     
062400     EJECT                                                                
062500 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDP512'.           
062600 01  DLI-IO-WDP512.                                                       
062700*    03  -COPY WDP512                                                     
062800     EJECT                                                                
062900                                                                          
063000 LINKAGE SECTION.                                                         
063100*01    -COPY W0009     -PRE MSG-                                          
063200                                                                          
063300*01    -COPY W0008     -PRE WDK6-                                         
063400         05  FILLER           PIC X.                                      
063500*01    -COPY W0008     -PRE WDD3-                                         
063600         05  FILLER           PIC X.                                      
063700*01    -COPY W0008     -PRE WDB6-                                         
063800         05  FILLER           PIC X.                                      
063900*01    -COPY W0008     -PRE WDD9-                                         
064000         05  FILLER           PIC X.                                      
064100*01    -COPY W0008     -PRE WDD7A-                                        
064200         05  FILLER           PIC X.                                      
064300*01    -COPY W0008     -PRE WDD7-                                         
064400         05  FILLER           PIC X.                                      
064500*01    -COPY W0008     -PRE WDP3-                                         
064600         05  FILLER           PIC X.                                      
064700*01    -COPY W0008     -PRE WDP3A-                                        
064800         05  FILLER           PIC X.                                      
064900*01    -COPY W0008     -PRE WDP3B-                                        
065000         05  FILLER           PIC X.                                      
065100*01    -COPY W0008     -PRE WDP3C-                                        
065200         05  FILLER           PIC X.                                      
065300*01    -COPY W0008     -PRE WDB2-                                         
065400         05  FILLER           PIC X.                                      
065500*01    -COPY W0008     -PRE WDB3-                                         
065600         05  FILLER           PIC X.                                      
065700*01    -COPY W0008     -PRE WDL2-                                         
065800         05  FILLER           PIC X.                                      
065900*01    -COPY W0008     -PRE WDK7-                                         
066000         05  FILLER           PIC X.                                      
066100*01    -COPY W0008     -PRE WDD8-                                         
066200         05  FILLER           PIC X.                                      
066300*01    -COPY W0008     -PRE W6D1-                                         
066400         05  FILLER           PIC X.                                      
066500*01    -COPY W0008     -PRE W6D1H-                                        
066600         05  FILLER           PIC X.                                      
066700*01    -COPY W0008     -PRE W6D1H1-                                       
066800         05  FILLER           PIC X.                                      
066900*01    -COPY W0008     -PRE WDA5-                                         
067000         05  FILLER           PIC X.                                      
067100*01    -COPY W0008     -PRE WDA5A-                                        
067200         05  FILLER           PIC X.                                      
067300*01    -COPY W0008     -PRE WDL6-                                         
067400         05  FILLER           PIC X.                                      
067500*01    -COPY W0008     -PRE WDP5-                                         
067600         05  FILLER           PIC X.                                      
067700     EJECT                                                                
067800*                                                                         
067900 PROCEDURE DIVISION  USING MSG-PCB   WDK6-PCB                             
068000                           WDD3-PCB  WDB6-PCB  WDD9-PCB                   
068100                           WDD7A-PCB WDD7-PCB  WDP3-PCB                   
068200                           WDP3A-PCB WDP3B-PCB WDP3C-PCB                  
068300                           WDB2-PCB  WDB3-PCB  WDL2-PCB                   
068400                           WDK7-PCB  WDD8-PCB  W6D1-PCB                   
068500                           W6D1H-PCB W6D1H1-PCB WDA5-PCB                  
068600                           WDA5A-PCB WDL6-PCB  WDP5-PCB.                  
068700     ENTRY 'DLITCBL' USING MSG-PCB   WDK6-PCB                             
068800                           WDD3-PCB  WDB6-PCB  WDD9-PCB                   
068900                           WDD7A-PCB WDD7-PCB  WDP3-PCB                   
069000                           WDP3A-PCB WDP3B-PCB WDP3C-PCB                  
069100                           WDB2-PCB  WDB3-PCB  WDL2-PCB                   
069200                           WDK7-PCB  WDD8-PCB  W6D1-PCB                   
069300                           W6D1H-PCB W6D1H1-PCB WDA5-PCB                  
069400                           WDA5A-PCB WDL6-PCB  WDP5-PCB.                  
069500 MAIN SECTION.                                                            
069600     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
069700     IF SUB-KDRC = 0                                                      
069800        PERFORM A-INIT                                                    
069900        PERFORM B-CHECK-KEYS                                              
070000        IF NYCKLAR-OK                                                     
070100           PERFORM F-LAES-VISA-INFO                                       
070200        END-IF                                                            
070300                                                                          
070400        PERFORM S02-RETURN-RESPONSE                                       
070500     END-IF                                                               
070600                                                                          
070700     MOVE ZERO TO RETURN-CODE                                             
070800     GOBACK                                                               
070900     .                                                                    
071000     EJECT                                                                
071100 A-INIT SECTION.                                                          
071200                                                                          
071300     MOVE SPACE                       TO RESP-AREA                        
071400     MOVE SPACE                       TO RESP-IDMSG-ERROR                 
071500                                         RESP-IDMSG-INFO                  
071600                                         RESP-IDELMT-ERROR                
071700     MOVE '001'                       TO RESP-IDMSGVER                    
071800     MOVE JA                          TO NYCKLAR-SW                       
071900                                                                          
072000     ACCEPT WS-DAT                  FROM DATE                             
072100     ACCEPT WS-TID                  FROM TIME                             
072200                                                                          
072300     MOVE WS-DAT                      TO DAT-I-TIDATUM                    
072400     PERFORM S03-DATUMKONV-TILL-AAVVD                                     
072500     IF DAT-KDSVAR-OK                                                     
072600        MOVE DAT-TIAAVVD              TO WS-CURR-AAVVD                    
072700        MOVE DAT-TIAAMMDD             TO WS-CURR-AAMMDD                   
072800        MOVE WS-CURR-AAMMDD           TO WS-TIREPDAT                      
072900     END-IF                                                               
073000     .                                                                    
073100     EJECT                                                                
073200                                                                          
073300 B-CHECK-KEYS SECTION.                                                    
073400***  CONTROL INPUT REQU-IDARTNR                                           
073500     IF REQU-IDARTNR-KEY  NOT  =  ALL '+'                                 
073600        INSPECT REQU-IDARTNR-KEY  REPLACING LEADING SPACE BY ZERO         
073700        MOVE REQU-IDARTNR-KEY         TO WS-IDARTNR                       
073800     ELSE                                                                 
073900        MOVE ALL ZEROES               TO WS-IDARTNR                       
074000     END-IF                                                               
074100                                                                          
074200     MOVE WS-IDARTNR                  TO RESP-IDARTNR-KEY                 
074300     INSPECT RESP-IDARTNR-KEY REPLACING LEADING ZERO BY SPACE             
074400                                                                          
074500     IF  IDARTNR-WS NUMERIC                                               
074600     AND IDARTNR-WS > ZERO                                                
074700         CONTINUE                                                         
074800     ELSE                                                                 
074900         MOVE 'IDARTNR'               TO RESP-IDELMT-ERROR                
075000         MOVE NEJ                     TO NYCKLAR-SW                       
075100     END-IF                                                               
075200***  CONTROL INPUT REQU-IDDISTR                                           
075300     IF REQU-IDDISTR-KEY  NOT  =  ALL '+'                                 
075400        INSPECT REQU-IDDISTR-KEY REPLACING LEADING SPACE BY ZERO          
075500        MOVE REQU-IDDISTR-KEY         TO WS-IDDISTR                       
075600     ELSE                                                                 
075700        MOVE ALL ZEROES               TO WS-IDDISTR                       
075800     END-IF                                                               
075900                                                                          
076000     MOVE WS-IDDISTR                  TO RESP-IDDISTR-KEY                 
076100     INSPECT RESP-IDDISTR-KEY REPLACING LEADING ZERO BY SPACE             
076200                                                                          
076300     IF  IDDISTR-WS NUMERIC                                               
076400         CONTINUE                                                         
076500     ELSE                                                                 
076600         MOVE 'IDDISTR'               TO RESP-IDELMT-ERROR                
076700         MOVE NEJ                     TO NYCKLAR-SW                       
076800     END-IF                                                               
076900                                                                          
077000***  CONTROL INPUT REQU-IDKUNNR                                           
077100     IF REQU-IDKUNDNR-KEY  NOT =  ALL '+'                                 
077200        INSPECT REQU-IDKUNDNR-KEY REPLACING LEADING SPACE BY ZERO         
077300        MOVE REQU-IDKUNDNR-KEY        TO WS-IDKUNDNR                      
077400     ELSE                                                                 
077500        MOVE ALL ZEROES               TO WS-IDKUNDNR                      
077600     END-IF                                                               
077700                                                                          
077800     MOVE WS-IDKUNDNR                 TO RESP-IDKUNDNR-KEY                
077900     INSPECT RESP-IDKUNDNR-KEY REPLACING LEADING ZERO BY SPACE            
078000                                                                          
078100     IF  IDKUNDNR-WS NUMERIC                                              
078200         CONTINUE                                                         
078300     ELSE                                                                 
078400       MOVE 'IDKUNDNR'                TO RESP-IDELMT-ERROR                
078500       MOVE NEJ                       TO NYCKLAR-SW                       
078600     END-IF                                                               
078700                                                                          
078800***  ERROR IN INPUT                                                       
078900     IF NYCKLAR-FEL                                                       
079000        MOVE ERR-WRONG-KEY            TO RESP-IDMSG-ERROR                 
079100     ELSE                                                                 
079200***    DISTRIICT AND CUSTOMER SHOULD BE ENTERED                           
079300       IF (IDDISTR-WS > ZERO AND IDKUNDNR-WS = ZERO) OR                   
079400          (IDDISTR-WS = ZERO AND IDKUNDNR-WS > ZERO)                      
079500          MOVE NEJ                    TO NYCKLAR-SW                       
079600          MOVE ERR-INVALID-KEY-COMB   TO RESP-IDMSG-ERROR                 
079700          MOVE INF-DIST-CUST-NEEDED   TO RESP-IDMSG-INFO                  
079800          MOVE 'IDDISTR-IDKUNDNR'     TO RESP-IDELMT-ERROR                
079900       END-IF                                                             
080000     END-IF                                                               
080100                                                                          
080200     .                                                                    
080300     EJECT                                                                
080400 F-LAES-VISA-INFO SECTION.                                                
080500*                                                                         
080600     PERFORM FA-GET-WDK6-INFO                                             
080700     IF ARTIKEL-OK                                                        
080800        PERFORM FB-GET-W6D1-CDC-INFO                                      
080900        PERFORM FD-GET-WDD7-INFO                                          
081000        PERFORM FE-DB2-FETCH-BYPRO                                        
081100        PERFORM FF-GET-WDB2-WDB3-INFO                                     
081200        PERFORM FG-GET-WDL2-WDL6-INFO                                     
081300        PERFORM FH-GET-WDK7-INFO                                          
081400        PERFORM FI-GET-BUFFER-INFO                                        
081500        PERFORM FJ-CASE-SURVEY-INFO                                       
081600        PERFORM FK-GET-WDD9-WEEK-INFO                                     
081700        PERFORM FL-GET-WDP5-INFO                                          
081800     END-IF                                                               
081900     .                                                                    
082000     EJECT                                                                
082100 FA-GET-WDK6-INFO SECTION.                                                
082200*                                                                         
082300     MOVE IDARTNR-WS                  TO W-IDARTNR                        
082400     PERFORM IMS-GU-WDK601                                                
082500     IF SEGMENT-FINNS                                                     
082600        MOVE ART-IDLEVNR              TO RESP-IDLEVNR                     
082700                                         W-IDLEVNR-LEV                    
082800                                         SPAR-IDLEVNR                     
082900        MOVE ART-IDFKNGRP             TO SPAR-IDFKNGRP                    
083000        MOVE ART-TIURPROD             TO RESP-TIURPROD                    
083100                                                                          
083200        PERFORM IMS-GNP-WDK611                                            
083300        IF SEGMENT-FINNS                                                  
083400           MOVE CLAG-ADLAGOMR         TO RESP-ADLAGOMR                    
083500           MOVE CLAG-ADGANG           TO RESP-ADGANG                      
083600           MOVE CLAG-ADPLATS          TO RESP-ADPLATS                     
083700           MOVE CLAG-KVLS             TO RESP-KVLS                        
083800           MOVE CLAG-VKART            TO RESP-VKART-OLD                   
083900           MOVE CLAG-VLARTNTO         TO RESP-VLARTNTO-OLD                
084000           MOVE CLAG-KDFARLIG         TO RESP-KDFARLIG                    
084100           MOVE CLAG-KVAKS-PAV        TO RESP-KVAKS-PAV-CDC               
084200           MOVE CLAG-IDANSK           TO RESP-IDANSK                      
084300           MOVE CLAG-KDERS            TO RESP-KDERS                       
084400           MOVE CLAG-KVAKS-CDC        TO RESP-KVAKS-LAGER                 
084500           MOVE CLAG-KVROS            TO RESP-KVROS                       
084600           MOVE CLAG-KVRESS           TO RESP-KVRESS                      
084700           MOVE CLAG-KVVORKO          TO RESP-KVVORKO                     
084800           MOVE CLAG-KVSPARR-KVAL     TO RESP-KVSPARR-KVAL-CDC            
084900           MOVE CLAG-KVUTRS           TO RESP-KVUTRS-CDC                  
085000           MOVE CLAG-KVSPANT          TO RESP-KVSPANT                     
085100           PERFORM FAD-GET-LDC-BO-QTY                                     
085200                                                                          
085300           MOVE 1                     TO IX                               
085400           PERFORM UNTIL (IX > MAX-ANT-PROENH)                            
085500             MOVE    CLAG-IDPROENH (IX)                                   
085600                                      TO RESP-IDPROENH (IX)               
085700             INSPECT RESP-IDPROENH (IX)                                   
085800                              REPLACING LEADING ZERO BY SPACES            
085900             ADD 1 TO IX                                                  
086000           END-PERFORM                                                    
086100                                                                          
086200                                                                          
086300           PERFORM FAA-READ-NAME                                          
086400           PERFORM FAB-GET-BLOCK-CODE                                     
086500           PERFORM FAC-GNP-WDK625                                         
086600                                                                          
086700           PERFORM IMS-GU-WDD311                                          
086800           IF SEGMENT-FINNS                                               
086900              MOVE TEXT-BEART         TO RESP-BEART                       
087000           END-IF                                                         
087100        END-IF                                                            
087200     ELSE                                                                 
087300        MOVE ERR-PART-MISSING         TO RESP-IDMSG-ERROR                 
087400        MOVE 'IDARTNR'                TO RESP-IDELMT-ERROR                
087500        SET ARTIKEL-FEL               TO TRUE                             
087600     END-IF                                                               
087700     .                                                                    
087800     EJECT                                                                
087900 FAA-READ-NAME         SECTION.                                           
088000*                                                                         
088100     MOVE LOW-VALUE                   TO W-WDP3A1-MIN                     
088200                                         W-WDP3C1-MIN                     
088300     MOVE HIGH-VALUE                  TO W-WDP3A1-MAX                     
088400                                         W-WDP3C1-MAX                     
088500     MOVE WC-LAND-SE                  TO W-IDLAND-A-MIN                   
088600                                         W-IDLAND-A-MAX                   
088700                                         W-IDLAND-B                       
088800                                         W-IDLAND-C-MIN                   
088900                                         W-IDLAND-C-MAX                   
089000                                                                          
089100     MOVE CLAG-IDANSK                 TO SPAR-IDANSK                      
089200     IF SPAR-IDANSK > +0                                                  
089300       MOVE 'ANSK    '                TO W-KDARBTYP                       
089400       MOVE SPAR-IDANSK               TO W-IDPERSON                       
089500       PERFORM IMS-GU-WDP311                                              
089600       IF SEGMENT-FINNS                                                   
089700          MOVE PERS-IDNAMN            TO RESP-IDNAMN-ANSK                 
089800          MOVE PERS-IDTFN             TO RESP-IDTFN-ANSK                  
089900          MOVE PERS-IDMAIL            TO RESP-IDMAIL-ANSK                 
090000       ELSE                                                               
090100          MOVE ALL SPACES             TO RESP-IDNAMN-ANSK                 
090200                                         RESP-IDTFN-ANSK                  
090300                                         RESP-IDMAIL-ANSK                 
090400       END-IF                                                             
090500     ELSE                                                                 
090600       MOVE ALL SPACES                TO RESP-IDNAMN-ANSK                 
090700                                         RESP-IDTFN-ANSK                  
090800                                         RESP-IDMAIL-ANSK                 
090900     END-IF                                                               
091000*                                                                         
091100     MOVE CLAG-IDBERED                TO SPAR-IDBERED                     
091200     IF SPAR-IDBERED > +0                                                 
091300        MOVE 'BER     '               TO W-KDARBTYP                       
091400        MOVE SPAR-IDBERED             TO W-IDPERSON                       
091500        PERFORM IMS-GU-WDP311                                             
091600        IF SEGMENT-FINNS                                                  
091700           MOVE PERS-IDNAMN           TO RESP-IDNAMN-BEREDARE             
091800           MOVE PERS-IDTFN            TO RESP-IDTFN-BEREDARE              
091900           MOVE PERS-IDMAIL           TO RESP-IDMAIL-BEREDARE             
092000        ELSE                                                              
092100           MOVE ALL SPACES            TO RESP-IDNAMN-BEREDARE             
092200                                         RESP-IDTFN-BEREDARE              
092300                                         RESP-IDMAIL-BEREDARE             
092400        END-IF                                                            
092500     ELSE                                                                 
092600        MOVE ALL SPACES               TO RESP-IDNAMN-BEREDARE             
092700                                         RESP-IDTFN-BEREDARE              
092800                                         RESP-IDMAIL-BEREDARE             
092900     END-IF                                                               
093000*                                                                         
093100     MOVE 'QUAL'                      TO W-KDARBTYP                       
093200                                         W-KDARBTYP-B                     
093300     IF IDARTNR-WS > ZERO                                                 
093400        PERFORM FAAA-SOEK-IDARTNR                                         
093500     END-IF                                                               
093600     IF SW-TRAEFF = 'J'                                                   
093700        MOVE WS-IDPERSON              TO W-IDPERSON                       
093800        PERFORM IMS-GU-WDP311                                             
093900        IF SEGMENT-FINNS                                                  
094000           MOVE PERS-IDNAMN           TO RESP-IDNAMN-KVAL                 
094100           MOVE PERS-IDTFN            TO RESP-IDTFN-KVAL                  
094200           MOVE PERS-IDMAIL           TO RESP-IDMAIL-KVAL                 
094300        ELSE                                                              
094400           MOVE ALL SPACES            TO RESP-IDNAMN-KVAL                 
094500                                         RESP-IDTFN-KVAL                  
094600                                         RESP-IDMAIL-KVAL                 
094700        END-IF                                                            
094800     ELSE                                                                 
094900       MOVE ALL SPACES                TO RESP-IDNAMN-KVAL                 
095000                                         RESP-IDTFN-KVAL                  
095100                                         RESP-IDMAIL-KVAL                 
095200     END-IF                                                               
095300     .                                                                    
095400     EJECT                                                                
095500 FAAA-SOEK-IDARTNR     SECTION.                                           
095600                                                                          
095700     MOVE NEJ                         TO SW-TRAEFF                        
095800     PERFORM IMS-GU-WDP3A                                                 
095900     PERFORM UNTIL SEGMENT-SAKNAS OR SW-TRAEFF = 'J'                      
096000       IF SEQA-IDARTNR-TOM < W-IDARTNR                                    
096100         PERFORM IMS-GN-WDP3A                                             
096200       ELSE                                                               
096300         IF  SEQA-IDARTNR-FOM <= W-IDARTNR                                
096400         AND SEQA-IDARTNR-TOM >= W-IDARTNR                                
096500             MOVE 'J'                 TO SW-TRAEFF                        
096600         ELSE                                                             
096700             MOVE 'GE'                TO STATUS-WS                        
096800         END-IF                                                           
096900       END-IF                                                             
097000     END-PERFORM                                                          
097100     IF SW-TRAEFF = 'J'                                                   
097200        MOVE SEQA-IDPERSON            TO WS-IDPERSON                      
097300     ELSE                                                                 
097400        MOVE SPAR-IDLEVNR             TO W-IDLEVNR-B                      
097500        PERFORM IMS-GU-WDP3B                                              
097600        IF SEGMENT-FINNS                                                  
097700           MOVE 'J'                   TO SW-TRAEFF                        
097800           MOVE SEQB-IDPERSON         TO WS-IDPERSON                      
097900        ELSE                                                              
098000           PERFORM IMS-GU-WDP3C                                           
098100           PERFORM UNTIL (SEGMENT-SAKNAS OR SW-TRAEFF = 'J')              
098200             IF SEQC-IDFKNGRP-TOM < SPAR-IDFKNGRP                         
098300                PERFORM IMS-GN-WDP3C                                      
098400             ELSE                                                         
098500               IF  SEQC-IDFKNGRP-FOM <= SPAR-IDFKNGRP                     
098600               AND SEQC-IDFKNGRP-TOM >= SPAR-IDFKNGRP                     
098700                   MOVE 'J'           TO SW-TRAEFF                        
098800               ELSE                                                       
098900                   MOVE 'GE'          TO STATUS-WS                        
099000               END-IF                                                     
099100             END-IF                                                       
099200           END-PERFORM                                                    
099300           IF SW-TRAEFF = 'J'                                             
099400              MOVE SEQC-IDPERSON      TO WS-IDPERSON                      
099500           END-IF                                                         
099600        END-IF                                                            
099700     END-IF                                                               
099800     .                                                                    
099900     EJECT                                                                
100000 FAB-GET-BLOCK-CODE    SECTION.                                           
100100*                                                                         
100200     MOVE ZERO                        TO RESP-SPARRKOD                    
100300                                                                          
100400     IF CLAG-KDUART = 'M'                                                 
100500        MOVE 6                        TO RESP-SPARRKOD                    
100600     ELSE                                                                 
100700       IF CLAG-KDUART = 'S'                                               
100800         MOVE 4                       TO RESP-SPARRKOD                    
100900       ELSE                                                               
101000         IF CLAG-FLLSRDEL = 'N'                                           
101100           MOVE 3                     TO RESP-SPARRKOD                    
101200         ELSE                                                             
101300           IF CLAG-KDLEVSP = 20 OR 21                                     
101400             MOVE 2                   TO RESP-SPARRKOD                    
101500           END-IF                                                         
101600         END-IF                                                           
101700       END-IF                                                             
101800     END-IF                                                               
101900     .                                                                    
102000     EJECT                                                                
102100 FAC-GNP-WDK625        SECTION.                                           
102200*                                                                         
102300     MOVE +1 TO IX                                                        
102400     PERFORM UNTIL IX > 3                                                 
102500       IF IX = 1                                                          
102600         MOVE +1                      TO W-KDNOTTYP                       
102700         PERFORM IMS-GNP-WDK625                                           
102800         IF SEGMENT-FINNS                                                 
102900           MOVE NOT-TEARTNOT          TO RESP-TEARTNOT-1                  
103000         ELSE                                                             
103100           MOVE SPACE                 TO RESP-TEARTNOT-1                  
103200         END-IF                                                           
103300       ELSE                                                               
103400         IF IX = 2                                                        
103500           MOVE +2                    TO W-KDNOTTYP                       
103600           PERFORM IMS-GNP-WDK625                                         
103700           IF SEGMENT-FINNS                                               
103800             MOVE NOT-TEARTNOT        TO RESP-TEARTNOT-2                  
103900           ELSE                                                           
104000             MOVE SPACE               TO RESP-TEARTNOT-2                  
104100           END-IF                                                         
104200         ELSE                                                             
104300           IF IX = 3                                                      
104400             MOVE +6                  TO W-KDNOTTYP                       
104500             PERFORM IMS-GNP-WDK625                                       
104600           END-IF                                                         
104700         END-IF                                                           
104800       END-IF                                                             
104900       ADD +1 TO IX                                                       
105000     END-PERFORM                                                          
105100     .                                                                    
105200     EJECT                                                                
105300 FAD-GET-LDC-BO-QTY    SECTION.                                           
105400                                                                          
105500     MOVE LOW-VALUE                   TO W-WDA5A1KY-MIN-X                 
105600     MOVE HIGH-VALUE                  TO W-WDA5A1KY-MAX-X                 
105700                                                                          
105800     MOVE IDARTNR-WS                  TO W-WDA5A1-IDARTNR-MIN             
105900                                         W-WDA5A1-IDARTNR-MAX             
106000                                                                          
106100     PERFORM IMS-03-GN-WDA5A1                                             
106200                                                                          
106300     IF SEGMENT-FINNS                                                     
106400        MOVE SEQA-IDWDA501            TO W-WDA5KEY-X                      
106500        PERFORM IMS-04-GU-WDA501                                          
106600     END-IF                                                               
106700     IF SEGMENT-FINNS                                                     
106800        MOVE +1                       TO INDX                             
106900        MOVE ZERO                     TO W-KVART-TOT-C1                   
107000        PERFORM UNTIL INDX > MAX-INDX                                     
107100         IF SEGMENT-FINNS                                                 
107200            COMPUTE W-KVART-TOT-C1 =                                      
107300                           W-KVART-TOT-C1 + RAD-KVART                     
107400            PERFORM IMS-03-GN-WDA5A1                                      
107500            IF SEGMENT-FINNS                                              
107600               MOVE SEQA-IDWDA501     TO W-WDA5KEY-X                      
107700               PERFORM IMS-04-GU-WDA501                                   
107800            END-IF                                                        
107900         END-IF                                                           
108000         ADD 1 TO INDX                                                    
108100        END-PERFORM                                                       
108200     END-IF                                                               
108300     MOVE W-KVART-TOT-C1              TO RESP-KVANTAL                     
108400     .                                                                    
108500     EJECT                                                                
108600 FB-GET-W6D1-CDC-INFO  SECTION.                                           
108700                                                                          
108800     MOVE IDARTNR-WS                  TO W-IDARTNR                        
108900                                         W-IDARTNR-HSEQ                   
109000     MOVE ZERO                        TO W-KVART-TOT-C1                   
109100     PERFORM IMS-GN-W6D111-W6D1HSEQ                                       
109200     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
109300       MOVE    H-ART-IDDC             TO W-IDDC-B6                        
109400       PERFORM IMS-GU-WDB601                                              
109500       IF (DCS-CDC                                                        
109600       OR  DCS-CDC-TR)                                                    
109700       AND H-ART-IDLOPNRM   = ZERO                                        
109800           MOVE H-ART-KVAVIS          TO WS-KVAVIS                        
109900           IF   H-ART-FLFEL = NEJ                                         
110000                ADD WS-KVAVIS         TO W-KVART-TOT-C1                   
110100           END-IF                                                         
110200       END-IF                                                             
110300       PERFORM IMS-GN-W6D111-W6D1HSEQ                                     
110400     END-PERFORM                                                          
110500     MOVE W-KVART-TOT-C1              TO RESP-KVART-FORAVIS               
110600     .                                                                    
110700     EJECT                                                                
110800 FK-GET-WDD9-WEEK-INFO SECTION.                                           
110900                                                                          
111000     MOVE +1                         TO IX-A                              
111100     MOVE IDARTNR-WS                 TO W-IDARTNR-LEV                     
111200     PERFORM IMS-GU-WDD902                                                
111300     IF SEGMENT-FINNS                                                     
111400        PERFORM IMS-GNP-WDD924                                            
111500        PERFORM UNTIL SEGMENT-SAKNAS OR                                   
111600                      IX-A            > WS-KVRADER-MAX7                   
111700         MOVE LEV-DALEVBSK-AVS       TO WS-DALEVBSK-AVS                   
111800         MOVE WS-DALEVBSK-AAMMDD     TO DAT-I-TIDATUM                     
111900         PERFORM S03-DATUMKONV-TILL-AAVVD                                 
112000         IF DAT-KDSVAR-OK                                                 
112100           MOVE DAT-TIAAVVD          TO RESP-TILEVBSK-AVS-UT(IX-A)        
112200         END-IF                                                           
112300         IF LEV-KVAVIS-BSKKVAR > 0                                        
112400           MOVE LEV-KVAVIS-BSKKVAR   TO RESP-KVAVIS-UT (IX-A)             
112500         END-IF                                                           
112600         IF LEV-TILEVBSK-INL = 0                                          
112700           MOVE ZERO                 TO RESP-TILEVBSK-INL-C1-UT           
112800                                        (IX-A)                            
112900         ELSE                                                             
113000           MOVE LEV-TILEVBSK-INL     TO DAT-I-TIDATUM                     
113100           PERFORM S03-DATUMKONV-TILL-AAVVD                               
113200           IF DAT-KDSVAR-OK                                               
113300             MOVE DAT-TIAAVVD        TO RESP-TILEVBSK-INL-C1-UT           
113400                                        (IX-A)                            
113500           END-IF                                                         
113600         END-IF                                                           
113700         MOVE LEV-FLFORAVI         TO RESP-FLFORAVI-C1-UT                 
113800                                        (IX-A)                            
113900         ADD +1                    TO IX-A                                
114000         PERFORM IMS-GNP-WDD924                                           
114100        END-PERFORM                                                       
114200                                                                          
114300        PERFORM FKA-GET-WDD9-EXTINFO                                      
114400     END-IF                                                               
114500     COMPUTE RESP-KVRADER-MAX7  = IX-A - 1                                
114600     .                                                                    
114700     EJECT                                                                
114800 FKA-GET-WDD9-EXTINFO     SECTION.                                        
114900                                                                          
115000     PERFORM FKAA-GET-WDD9-EXTINFO1                                       
115100     PERFORM FKAB-GET-WDD9-EXTINFO2                                       
115200     PERFORM FKAC-GET-WDD9-EXTINFO3                                       
115300     PERFORM FKAD-GET-WDD9-EXTINFO4                                       
115400     .                                                                    
115500     EJECT                                                                
115600 FKAA-GET-WDD9-EXTINFO1    SECTION.                                       
115700                                                                          
115800     MOVE +2                          TO W-IDLEVBSK                       
115900     PERFORM IMS-GU-WDD925                                                
116000     IF SEGMENT-FINNS                                                     
116100        MOVE INFO-TELEVBSK            TO RESP-TELEVBSK-EXT                
116200     END-IF                                                               
116300     .                                                                    
116400     EJECT                                                                
116500 FKAB-GET-WDD9-EXTINFO2    SECTION.                                       
116600                                                                          
116700     MOVE +4                          TO W-IDLEVBSK                       
116800     PERFORM IMS-GU-WDD925                                                
116900     IF SEGMENT-FINNS                                                     
117000        MOVE INFO-TELEVBSK            TO RESP-TELEVBSK-EXT2               
117100     END-IF                                                               
117200     .                                                                    
117300     EJECT                                                                
117400 FKAC-GET-WDD9-EXTINFO3    SECTION.                                       
117500                                                                          
117600     MOVE +5                          TO W-IDLEVBSK                       
117700     PERFORM IMS-GU-WDD925                                                
117800     IF SEGMENT-FINNS                                                     
117900        MOVE INFO-TELEVBSK            TO RESP-TELEVBSK-EXT3               
118000     END-IF                                                               
118100     .                                                                    
118200     EJECT                                                                
118300 FKAD-GET-WDD9-EXTINFO4    SECTION.                                       
118400                                                                          
118500     MOVE +6                          TO W-IDLEVBSK                       
118600     PERFORM IMS-GU-WDD925                                                
118700     IF SEGMENT-FINNS                                                     
118800        MOVE INFO-TELEVBSK            TO RESP-TELEVBSK-EXT4               
118900     END-IF                                                               
119000     .                                                                    
119100     EJECT                                                                
119200 FD-GET-WDD7-INFO         SECTION.                                        
119300                                                                          
119400     PERFORM FDA-GET-WDD7-ERSATT-INFO                                     
119500     PERFORM FDB-GET-WDD7-TILLK-INFO                                      
119600     .                                                                    
119700     EJECT                                                                
119800 FDA-GET-WDD7-ERSATT-INFO SECTION.                                        
119900                                                                          
120000     MOVE ZERO                        TO W-ERSATT-IDARTNR                 
120100     SUBTRACT 1                     FROM W-ERSATT-IDARTNR                 
120200     MOVE 1                           TO IX                               
120300                                                                          
120400     PERFORM IMS-GET-ERSATT-INFO                                          
120500     IF SEGMENT-FINNS                                                     
120600       PERFORM UNTIL                                                      
120700        NOT ( SEGMENT-FINNS AND                                           
120800              IX            NOT > WS-KVRADER-MAX1)                        
120900         IF ERSATT-IDARTNR > W-ERSATT-IDARTNR                             
121000           MOVE ERSATT-DIERS-ERS      TO RESP-DIERS-ERS (IX)              
121100           MOVE ERSATT-IDARTNR        TO RESP-IDARTNR   (IX)              
121200                                         W-ERSATT-IDARTNR                 
121300           ADD 1                      TO IX                               
121400         END-IF                                                           
121500         PERFORM IMS-GET-ERSATT-INFO                                      
121600       END-PERFORM                                                        
121700     END-IF                                                               
121800     COMPUTE RESP-KVRADER-MAX1  = IX - 1                                  
121900     .                                                                    
122000     EJECT                                                                
122100 FDB-GET-WDD7-TILLK-INFO  SECTION.                                        
122200                                                                          
122300     MOVE 1                           TO IX                               
122400     PERFORM IMS-GET-ERSATT-SEG                                           
122500     IF SEGMENT-FINNS                                                     
122600        PERFORM IMS-GET-TILLK-SEG                                         
122700        PERFORM UNTIL                                                     
122800        NOT ( SEGMENT-FINNS AND                                           
122900              IX            NOT > WS-KVRADER-MAX1)                        
123000                                                                          
123100         MOVE TILLK-FLTEXT            TO RESP-FLTEXT        (IX)          
123200         IF  TILLK-FLTEXT = NEJ                                           
123300             MOVE TILLK-IDARTNR-TILLK                                     
123400                                      TO RESP-IDARTNR-TILLK (IX)          
123500             MOVE TILLK-DIERS-TILLK   TO RESP-DIERS-TILLK   (IX)          
123600         ELSE                                                             
123700             MOVE TILLK-BEERS         TO RESP-TILLK-KOLUMNER(IX)          
123800         END-IF                                                           
123900         ADD 1                        TO IX                               
124000         PERFORM IMS-GET-TILLK-SEG                                        
124100        END-PERFORM                                                       
124200     END-IF                                                               
124300     COMPUTE RESP-KVRADER-MAX2  = IX - 1                                  
124400     .                                                                    
124500     EJECT                                                                
124600 FE-DB2-FETCH-BYPRO    SECTION.                                           
124700                                                                          
124800     MOVE IDARTNR-WS                  TO W-IDARTNR-BYT                    
124900     MOVE ZERO                        TO W-IDPRODNR                       
125000                                                                          
125100     PERFORM DB2-DCL-OPN-CRS-BYPRO                                        
125200     PERFORM DB2-FETCH-BYPRO                                              
125300     IF RADER-FINNS                                                       
125400        PERFORM FEA-GET-BYPRO-ART-INF                                     
125500        PERFORM DB2-CLOSE-BYPRO-CRS                                       
125600     END-IF                                                               
125700     .                                                                    
125800     EJECT                                                                
125900 FEA-GET-BYPRO-ART-INF SECTION.                                           
126000                                                                          
126100     MOVE +1                          TO IX                               
126200     PERFORM UNTIL NOT RADER-FINNS                                        
126300     OR IX > +18                                                          
126400        MOVE BYPRO-IDARTNR            TO RESP-BYPRO-IDARTNR (IX)          
126500        PERFORM DB2-FETCH-BYPRO                                           
126600        ADD +1                        TO IX                               
126700     END-PERFORM                                                          
126800     .                                                                    
126900     EJECT                                                                
127000 FF-GET-WDB2-WDB3-INFO SECTION.                                           
127100                                                                          
127200     IF  IDDISTR-WS    > ZERO                                             
127300     AND IDKUNDNR-WS   > ZERO                                             
127400         MOVE IDDISTR-WS              TO W-IDDISTR                        
127500                                         W-IDDISTR-WDB3                   
127600                                         W-IDDISTR-WDB3-DEF               
127700         MOVE IDKUNDNR-WS             TO W-IDKUNDNR                       
127800                                         W-IDKUNDNR-WDB3                  
127900*                                                                         
128000         PERFORM IMS-GU-WDB201                                            
128100*                                                                         
128200         IF SEGMENT-SAKNAS                                                
128300            MOVE INF-DIST-CUST-MISSING                                    
128400                                 TO RESP-IDMSG-INFO                       
128500            MOVE 'IDDISTR-IDKUNDNR'                                       
128600                                 TO RESP-IDELMT-ERROR                     
128700         ELSE                                                             
128800            MOVE +1              TO INDX                                  
128900            PERFORM UNTIL INDX > INDX-TVSVOR-MAX                          
129000              MOVE GMTA-GMT-IDDC-TVSVOR (INDX)                            
129100                                 TO RESP-IDDC-TVSVOR (INDX)               
129200              ADD +1             TO INDX                                  
129300            END-PERFORM                                                   
129400*                                                                         
129500            MOVE +1              TO IX                                    
129600            PERFORM UNTIL IX > MAX-IX-IDDC                                
129700              MOVE GMTA-GMT-IDDC-BULK (IX)                                
129800                                 TO RESP-IDDC-BULK (IX)                   
129900                                    W-IDDC-WDB3                           
130000                                    W-IDDC-WDB3-DEF                       
130100              PERFORM IMS-GU-WDB301                                       
130200              IF SEGMENT-FINNS                                            
130300                 MOVE GMTB-DC-KDGENFRA-MO                                 
130400                                 TO RESP-KDGENFRA-MO (IX)                 
130500              END-IF                                                      
130600                                                                          
130700              MOVE GMTA-GMT-IDDC-DAY (IX)                                 
130800                                 TO RESP-IDDC-DAY (IX)                    
130900                                    W-IDDC-WDB3                           
131000                                            W-IDDC-WDB3-DEF               
131100              PERFORM IMS-GU-WDB301                                       
131200              IF SEGMENT-FINNS                                            
131300                 MOVE GMTB-DC-KDGENFRA-DO                                 
131400                                 TO RESP-KDGENFRA-DO (IX)                 
131500              END-IF                                                      
131600                                                                          
131700              MOVE GMTA-GMT-IDDC-VOR (IX)                                 
131800                                 TO RESP-IDDC-VOR (IX)                    
131900                                    W-IDDC-WDB3                           
132000                                    W-IDDC-WDB3-DEF                       
132100              PERFORM IMS-GU-WDB301                                       
132200              IF SEGMENT-FINNS                                            
132300                 MOVE GMTB-DC-KDGENFRA-VOR                                
132400                                 TO RESP-KDGENFRA-VOR(IX)                 
132500              END-IF                                                      
132600                                                                          
132700              ADD +1             TO IX                                    
132800            END-PERFORM                                                   
132900         END-IF                                                           
133000     END-IF                                                               
133100     .                                                                    
133200     EJECT                                                                
133300 FG-GET-WDL2-WDL6-INFO SECTION.                                           
133400                                                                          
133500     MOVE +1                          TO INDX                             
133600     MOVE NEJ                         TO DEL-RAPP-FINNS                   
133700     MOVE ZERO                        TO W-DAINLEV                        
133800     MOVE JA                          TO WS-WDL6-ART-SW                   
133900                                         WS-WDL2-ART-SW                   
134000                                         WS-WDL6-INL-SW                   
134100                                         WS-WDL2-INL-SW                   
134200     PERFORM IMS-GET-WDL201                                               
134300     IF SEGMENT-SAKNAS                                                    
134400       MOVE NEJ                  TO WS-WDL2-ART-SW                        
134500                                    WS-WDL2-INL-SW                        
134600     END-IF                                                               
134700                                                                          
134800     PERFORM IMS-GET-WDL601                                               
134900     IF SEGMENT-SAKNAS                                                    
135000       MOVE NEJ                  TO WS-WDL6-ART-SW                        
135100                                    WS-WDL6-INL-SW                        
135200     END-IF                                                               
135300     IF WS-WDL2-ART-FOUND OR WS-WDL6-ART-FOUND                            
135400       IF WS-WDL2-ART-FOUND                                               
135500         PERFORM IMS-GET-WDL211                                           
135600         IF SEGMENT-SAKNAS                                                
135700           MOVE NEJ              TO WS-WDL2-INL-SW                        
135800           MOVE ZERO             TO INL-DAINLEV                           
135900         END-IF                                                           
136000       ELSE                                                               
136100         MOVE NEJ                TO WS-WDL2-INL-SW                        
136200         MOVE ZERO               TO INL-DAINLEV                           
136300       END-IF                                                             
136400                                                                          
136500       IF WS-WDL6-ART-FOUND                                               
136600         PERFORM IMS-GET-WDL611                                           
136700         IF SEGMENT-SAKNAS                                                
136800           MOVE NEJ              TO WS-WDL6-INL-SW                        
136900           MOVE ZERO             TO WDL6-INL-DAINLEV                      
137000         END-IF                                                           
137100       ELSE                                                               
137200         MOVE NEJ                TO WS-WDL6-INL-SW                        
137300         MOVE ZERO               TO WDL6-INL-DAINLEV                      
137400       END-IF                                                             
137500     END-IF                                                               
137600       PERFORM                                                            
137700         UNTIL (WS-WDL2-INL-SAKNAS AND WS-WDL6-INL-SAKNAS) OR             
137800               INDX > WS-KVRADER-MAX2                                     
137900         EVALUATE TRUE                                                    
138000           WHEN WS-WDL2-INL-FOUND AND                                     
138100                WS-WDL6-INL-SAKNAS                                        
138200             PERFORM FGA-PROCESS-INLE                                     
138300           WHEN WS-WDL2-INL-SAKNAS AND                                    
138400                WS-WDL6-INL-FOUND                                         
138500             PERFORM FGB-PROCESS-WDL6                                     
138600           WHEN WS-WDL2-INL-FOUND AND                                     
138700                WS-WDL6-INL-FOUND                                         
138800             IF INL-DAINLEV < WDL6-INL-DAINLEV                            
138900               PERFORM FGA-PROCESS-INLE                                   
139000             ELSE                                                         
139100               PERFORM FGB-PROCESS-WDL6                                   
139200             END-IF                                                       
139300         END-EVALUATE                                                     
139400       END-PERFORM                                                        
139500                                                                          
139600       COMPUTE RESP-KVRADER-MAX3 = INDX - 1                               
139700       .                                                                  
139800       EJECT                                                              
139900 FGA-PROCESS-INLE SECTION.                                                
140000      MOVE INL-DAINLEV            TO W-DAINLEV                            
140100                                     SPLIT-DAINLEV                        
140200      PERFORM IMS-GET-31-32-310                                           
140300      IF SEGMENT-FINNS                                                    
140400        IF MOT-IDDC = W-IDDC                                              
140500          IF MOT-IDPTYP = 'R32'                                           
140600            PERFORM FGAA-REDIGERA-R32                                     
140700            ADD +1                    TO INDX                             
140800          ELSE                                                            
140900            PERFORM FGAB-REDIGERA-R30-R31-310                             
141000          END-IF                                                          
141100        END-IF                                                            
141200      END-IF                                                              
141300      PERFORM IMS-GET-33-34                                               
141400      IF SEGMENT-FINNS                                                    
141500        IF DIR-IDDC = W-IDDC                                              
141600          PERFORM FGAC-REDIGERA-R33-R34                                   
141700          ADD +1                      TO INDX                             
141800        END-IF                                                            
141900      END-IF                                                              
142000      PERFORM IMS-GET-40                                                  
142100      IF SEGMENT-FINNS                                                    
142200        IF RET-IDDC = W-IDDC                                              
142300          PERFORM FGAD-REDIGERA-R40                                       
142400          ADD +1                      TO INDX                             
142500        END-IF                                                            
142600      END-IF                                                              
142700      IF DEL-RAPP-FINNS = JA                                              
142800        CONTINUE                                                          
142900      ELSE                                                                
143000        PERFORM IMS-GET-WDL211                                            
143100        IF SEGMENT-SAKNAS                                                 
143200          MOVE NEJ                    TO WS-WDL2-INL-SW                   
143300        END-IF                                                            
143400      END-IF                                                              
143500      .                                                                   
143600      EJECT                                                               
143700 FGAA-REDIGERA-R32 SECTION.                                               
143800     MOVE MOT-IDPTYP                  TO RESP-5107-IDPTYP  (INDX)         
143900     MOVE MOT-IDLOPNRM                TO RESP-5107-IDLOPNRM(INDX)         
144000     MOVE MOT-TIUPPDAT                TO WS-TIUPPDAT                      
144100     MOVE WS-TIUPPDAT                 TO DAT-I-TIDATUM                    
144200     PERFORM S03-DATUMKONV-TILL-AAVVD                                     
144300     IF DAT-KDSVAR-OK                                                     
144400       MOVE DAT-TIAAVVD               TO RESP-5107-TIAAVVD (INDX)         
144500     ELSE                                                                 
144600       MOVE ZERO                      TO RESP-5107-TIAAVVD (INDX)         
144700     END-IF                                                               
144800     MOVE MOT-IDLEVNR                 TO RESP-5107-IDLEVNR (INDX)         
144900     MOVE MOT-KDRT                    TO RESP-5107-KDRT    (INDX)         
145000     MOVE MOT-TIAVIDAT                TO RESP-5107-TIAVSDAT(INDX)         
145100     MOVE MOT-IDAVINR                 TO RESP-5107-IDAVINR (INDX)         
145200     MOVE MOT-KVANTMOT                TO RESP-5107-KVANTMOT(INDX)         
145300     MOVE MOT-KVAVIS                  TO RESP-5107-KVAVIS  (INDX)         
145400     .                                                                    
145500     EJECT                                                                
145600 FGAB-REDIGERA-R30-R31-310 SECTION.                                       
145700     MOVE JA                          TO FOERSTA-GAANG                    
145800     MOVE ZERO                        TO W-C2-FORDEL                      
145900                                                                          
146000     MOVE MOT-IDPTYP                  TO RESP-5107-IDPTYP  (INDX)         
146100     MOVE MOT-IDLOPNRM                TO RESP-5107-IDLOPNRM(INDX)         
146200                                                                          
146300     COMPUTE DAT-I-TIDATUM = 999999 - SPLIT-TIAAMMDD                      
146400     PERFORM S03-DATUMKONV-TILL-AAVVD                                     
146500     IF DAT-KDSVAR-OK                                                     
146600       MOVE DAT-TIAAVVD               TO RESP-5107-TIAAVVD (INDX)         
146700     ELSE                                                                 
146800       MOVE ZERO                      TO RESP-5107-TIAAVVD (INDX)         
146900     END-IF                                                               
147000     MOVE MOT-IDLEVNR                 TO RESP-5107-IDLEVNR (INDX)         
147100     MOVE MOT-KDRT                    TO RESP-5107-KDRT    (INDX)         
147200     MOVE MOT-TIAVIDAT                TO RESP-5107-TIAVSDAT(INDX)         
147300     MOVE MOT-IDAVINR                 TO RESP-5107-IDAVINR (INDX)         
147400     MOVE MOT-KVAVIS                  TO RESP-5107-KVAVIS  (INDX)         
147500     MOVE MOT-KVANTMOT                TO RESP-5107-KVANTMOT(INDX)         
147600     ADD +1                           TO INDX                             
147700                                                                          
147800     PERFORM IMS-GET-DEL                                                  
147900     MOVE ZERO                        TO W-C2-FORDEL                      
148000                                                                          
148100     PERFORM UNTIL                                                        
148200      NOT ( SEGMENT-FINNS AND INDX < WS-KVRADER-MAX2 )                    
148300       IF FOERSTA-GAANG = JA                                              
148400          MOVE ZERO                   TO W-DEL-KVRAPP                     
148500          PERFORM FGABA-FLYTTA-SPAR                                       
148600          MOVE NEJ                    TO FOERSTA-GAANG                    
148700       END-IF                                                             
148800       COMPUTE W-DEL-KVRAPP = W-DEL-KVRAPP + DEL-KVRAPP                   
148900       MOVE W-DEL-KVRAPP              TO RESP-5107-KVAVIS  (INDX)         
149000                                                                          
149100       PERFORM IMS-GET-DEL                                                
149200     END-PERFORM                                                          
149300     IF FOERSTA-GAANG = JA                                                
149400       CONTINUE                                                           
149500     ELSE                                                                 
149600       ADD +1                         TO INDX                             
149700     END-IF                                                               
149800     IF SEGMENT-FINNS                                                     
149900       MOVE JA                        TO DEL-RAPP-FINNS                   
150000     ELSE                                                                 
150100       MOVE NEJ                       TO DEL-RAPP-FINNS                   
150200     END-IF                                                               
150300     .                                                                    
150400     EJECT                                                                
150500 FGABA-FLYTTA-SPAR SECTION.                                               
150600                                                                          
150700     MOVE 'P32'                       TO RESP-5107-IDPTYP  (INDX)         
150800     MOVE ZERO                        TO RESP-5107-IDLOPNRM(INDX)         
150900                                         RESP-5107-KDRT    (INDX)         
151000                                         RESP-5107-TIAVSDAT(INDX)         
151100                                         RESP-5107-IDAVINR (INDX)         
151200                                         RESP-5107-KVANTMOT(INDX)         
151300     MOVE SPACES                      TO RESP-5107-IDLEVNR (INDX)         
151400                                                                          
151500     MOVE DEL-TIREGDAT                TO WS-TIUPPDAT                      
151600     MOVE WS-TIUPPDAT                 TO DAT-I-TIDATUM                    
151700     PERFORM S03-DATUMKONV-TILL-AAVVD                                     
151800     IF DAT-KDSVAR-OK                                                     
151900       MOVE DAT-TIAAVVD               TO RESP-5107-TIAAVVD (INDX)         
152000     ELSE                                                                 
152100       MOVE ZERO                      TO RESP-5107-TIAAVVD (INDX)         
152200     END-IF                                                               
152300     .                                                                    
152400     EJECT                                                                
152500 FGAC-REDIGERA-R33-R34  SECTION.                                          
152600                                                                          
152700     MOVE DIR-IDPTYP                  TO RESP-5107-IDPTYP  (INDX)         
152800     MOVE DIR-IDLOPNRM                TO RESP-5107-IDLOPNRM(INDX)         
152900     MOVE DIR-IDLEVNR                 TO RESP-5107-IDLEVNR (INDX)         
153000     MOVE DIR-KDRT                    TO RESP-5107-KDRT    (INDX)         
153100     MOVE DIR-TIAVSDAT                TO RESP-5107-TIAVSDAT(INDX)         
153200     MOVE DIR-IDAVINR                 TO RESP-5107-IDAVINR (INDX)         
153300     MOVE DIR-KVAVIS                  TO RESP-5107-KVANTMOT(INDX)         
153400                                                                          
153500     MOVE DIR-IDLEVNR                 TO LEV04-IDLEVNR                    
153600     IF LEV04-REFNR                                                       
153700        IF DIR-IDSUPREF NOT = SPACE                                       
153800           MOVE DIR-IDSUPREF (4:7)                                        
153900                                      TO RESP-5107-IDAVINR(INDX)          
154000        END-IF                                                            
154100     END-IF                                                               
154200*                                                                         
154300     COMPUTE DAT-I-TIDATUM = 999999 - SPLIT-TIAAMMDD                      
154400     PERFORM S03-DATUMKONV-TILL-AAVVD                                     
154500     IF DAT-KDSVAR-OK                                                     
154600        MOVE DAT-TIAAVVD              TO RESP-5107-TIAAVVD (INDX)         
154700     ELSE                                                                 
154800        MOVE ZERO                     TO RESP-5107-TIAAVVD (INDX)         
154900     END-IF                                                               
155000*                                                                         
155100     MOVE ZERO                        TO RESP-5107-KVAVIS  (INDX)         
155200     .                                                                    
155300     EJECT                                                                
155400 FGAD-REDIGERA-R40 SECTION.                                               
155500                                                                          
155600     MOVE RET-IDPTYP                  TO RESP-5107-IDPTYP  (INDX)         
155700     MOVE RET-IDLOPNRM                TO RESP-5107-IDLOPNRM(INDX)         
155800     MOVE RET-IDLEVNR                 TO RESP-5107-IDLEVNR (INDX)         
155900     MOVE RET-IDORDNR                 TO RESP-5107-IDAVINR (INDX)         
156000                                                                          
156100     COMPUTE DAT-I-TIDATUM = 999999 - SPLIT-TIAAMMDD                      
156200     PERFORM S03-DATUMKONV-TILL-AAVVD                                     
156300     IF DAT-KDSVAR-OK                                                     
156400        MOVE DAT-TIAAVVD              TO RESP-5107-TIAAVVD (INDX)         
156500     ELSE                                                                 
156600        MOVE ZERO                     TO RESP-5107-TIAAVVD (INDX)         
156700     END-IF                                                               
156800                                                                          
156900     MOVE ZERO                        TO RESP-5107-KDRT    (INDX)         
157000                                         RESP-5107-TIAVSDAT(INDX)         
157100                                         RESP-5107-KVAVIS  (INDX)         
157200     .                                                                    
157300     EJECT                                                                
157400 FGB-PROCESS-WDL6 SECTION.                                                
157500     MOVE WDL6-INL-IDLEVNR       TO W-IDLEVNR                             
157600     IF W-IDLEVNR = '1441 ' OR 'BP2TW' OR '7844 ' OR 'DAAJA'              
157700                 OR '15230'                                               
157800       CONTINUE                                                           
157900     ELSE                                                                 
158000       IF WDL6-INL-IDDC = W-IDDC                                          
158100         PERFORM FGBA-REDIGERA-R30-R310-R31-R32                           
158200       END-IF                                                             
158300     END-IF                                                               
158400     IF DEL-RAPP-FINNS = JA                                               
158500       CONTINUE                                                           
158600     ELSE                                                                 
158700       PERFORM IMS-GET-WDL611                                             
158800       IF SEGMENT-SAKNAS                                                  
158900         MOVE NEJ                     TO WS-WDL6-INL-SW                   
159000       END-IF                                                             
159100     END-IF                                                               
159200     .                                                                    
159300     EJECT                                                                
159400 FGBA-REDIGERA-R30-R310-R31-R32 SECTION.                                  
159500     MOVE WDL6-INL-IDPTYP             TO RESP-5107-IDPTYP  (INDX)         
159600     MOVE WDL6-INL-IDLEVNR            TO RESP-5107-IDLEVNR (INDX)         
159700     MOVE WDL6-INL-KDRT               TO RESP-5107-KDRT  (INDX)           
159800                                                                          
159900     IF WDL6-INL-IDPTYP = 'R30' OR '310'                                  
160000       MOVE WDL6-INL-IDFAKT           TO RESP-5107-IDLOPNRM(INDX)         
160100       MOVE WDL6-INL-KVAVIS           TO RESP-5107-KVAVIS(INDX)           
160200     ELSE                                                                 
160300       IF WDL6-INL-IDPTYP = 'R31'                                         
160400         MOVE WDL6-INL-IDLOPNRM                                           
160500                                      TO RESP-5107-IDLOPNRM(INDX)         
160600         MOVE WDL6-INL-KVAVIS         TO RESP-5107-KVAVIS(INDX)           
160700         MOVE WDL6-INL-KVANTMOT                                           
160800                                      TO RESP-5107-KVANTMOT(INDX)         
160900       ELSE                                                               
161000         IF WDL6-INL-IDPTYP = 'R32'                                       
161100           MOVE WDL6-INL-KVANTMOT                                         
161200                                      TO RESP-5107-KVANTMOT(INDX)         
161300           MOVE WDL6-INL-KVAVIS                                           
161400                                      TO RESP-5107-KVAVIS(INDX)           
161500           IF WDL6-INL-IDLOPNRM = 0                                       
161600             MOVE WDL6-INL-IDFAKT                                         
161700                                      TO RESP-5107-IDLOPNRM(INDX)         
161800           ELSE                                                           
161900             MOVE WDL6-INL-IDLOPNRM                                       
162000                                      TO RESP-5107-IDLOPNRM(INDX)         
162100           END-IF                                                         
162200         ELSE                                                             
162300           MOVE WDL6-INL-KVANTMOT                                         
162400                                      TO RESP-5107-KVANTMOT(INDX)         
162500           MOVE WDL6-INL-KVAVIS       TO RESP-5107-KVAVIS(INDX)           
162600           MOVE WDL6-INL-IDLOPNRM                                         
162700                                      TO RESP-5107-IDLOPNRM(INDX)         
162800         END-IF                                                           
162900       END-IF                                                             
163000     END-IF                                                               
163100     PERFORM FGBAA-FLYTTA-IDAVINR                                         
163200     MOVE WS-IDAVINR                  TO RESP-5107-IDAVINR(INDX)          
163300     MOVE WDL6-INL-DAINLEV            TO WS-DAINLEV                       
163400     MOVE WS-DAINLEV (3:6)            TO WS-IDINLEV-REGDAT                
163500     COMPUTE WS-TIREGDAT = 999999 - WS-IDINLEV-REGDAT                     
163600     MOVE WS-TIREGDAT                 TO RESP-5107-TIAVSDAT(INDX)         
163700                                                                          
163800     MOVE WDL6-INL-TIINLINL           TO WS-TIUPPDAT                      
163900     MOVE WS-TIUPPDAT                 TO DAT-I-TIDATUM                    
164000     PERFORM S03-DATUMKONV-TILL-AAVVD                                     
164100     IF DAT-KDSVAR-OK                                                     
164200       MOVE DAT-TIAAVVD               TO RESP-5107-TIAAVVD(INDX)          
164300     ELSE                                                                 
164400       MOVE ZERO                      TO RESP-5107-TIAAVVD(INDX)          
164500     END-IF                                                               
164600     ADD +1                           TO INDX                             
164700     .                                                                    
164800     EJECT                                                                
164900 FGBAA-FLYTTA-IDAVINR SECTION.                                            
165000     SKIP2                                                                
165100     MOVE +7                          TO AX                               
165200     MOVE +7                          TO BX                               
165300     MOVE +1                          TO X                                
165400     MOVE ZERO                        TO WS-IDAVINR                       
165500     PERFORM                                                              
165600       UNTIL X > 7                                                        
165700       IF WDL6-INL-IDKUNDRF(AX:1) >= 0                                    
165800         MOVE WDL6-INL-IDKUNDRF(AX:1)                                     
165900                                      TO WS-IDAVINR(BX:1)                 
166000         SUBTRACT 1                 FROM AX                               
166100         SUBTRACT 1                 FROM BX                               
166200         ADD 1                        TO X                                
166300       ELSE                                                               
166400         SUBTRACT 1                 FROM AX                               
166500         ADD 1                        TO X                                
166600       END-IF                                                             
166700     END-PERFORM                                                          
166800     .                                                                    
166900     EJECT                                                                
167000 FH-GET-WDK7-INFO      SECTION.                                           
167100                                                                          
167200     MOVE +1                          TO SDC-IX                           
167300     PERFORM IMS-GU-WDK701                                                
167400     IF SEGMENT-SAKNAS                                                    
167500        CONTINUE                                                          
167600     ELSE                                                                 
167700       PERFORM IMS-GNP-WDK711                                             
167800       PERFORM UNTIL                                                      
167900           (SEGMENT-SAKNAS  OR SDC-IX > WS-KVRADER-MAX1)                  
168000           IF  SLAG-IDDC NOT = '22'                                       
168100               PERFORM FHA-SDCINFO-TILL-RESP                              
168200               ADD +1                 TO SDC-IX                           
168300           END-IF                                                         
168400         PERFORM IMS-GNP-WDK711                                           
168500       END-PERFORM                                                        
168600     END-IF                                                               
168700     COMPUTE RESP-KVRADER-MAX4 =  SDC-IX - 1                              
168800     .                                                                    
168900     EJECT                                                                
169000 FHA-SDCINFO-TILL-RESP SECTION.                                           
169100                                                                          
169200     MOVE ZERO                     TO WS-KVDISP-SLAG                      
169300     COMPUTE WS-KVDISP-SLAG        =  SLAG-KVLS  -                        
169400                                      SLAG-KVOKS-DAG -                    
169500                                      SLAG-KVOKS-BULK                     
169600                                                                          
169700     MOVE SLAG-IDDC                TO RESP-IDDC-SDC      (SDC-IX)         
169800     MOVE SLAG-KVLS                TO RESP-KVLS-SDC      (SDC-IX)         
169900     MOVE WS-KVDISP-SLAG           TO RESP-KVDISP-SDC    (SDC-IX)         
170000     MOVE SLAG-KVOKS-DAG           TO RESP-KVOKS-DAG-SDC (SDC-IX)         
170100     MOVE SLAG-KVOKS-BULK          TO RESP-KVOKS-BULK-SDC(SDC-IX)         
170200     MOVE SLAG-KVUTRS              TO RESP-KVUTRS-SDC    (SDC-IX)         
170300     MOVE SLAG-KVAKS-SDC           TO RESP-KVAKS-SDC     (SDC-IX)         
170400     MOVE SLAG-KVAKS-PAV           TO RESP-KVAKS-PAV-SDC (SDC-IX)         
170500     MOVE SLAG-KDLEVSP             TO RESP-KDLEVSP-SDC   (SDC-IX)         
170600     MOVE SLAG-KVSPARR-KVAL        TO                                     
170700                                    RESP-KVSPARR-KVAL-SDC(SDC-IX)         
170800                                                                          
170900     PERFORM FHAA-WDL6-ETA-INFO                                           
171000     .                                                                    
171100     EJECT                                                                
171200 FHAA-WDL6-ETA-INFO    SECTION.                                           
171300     PERFORM IMS-GU-WDL601                                                
171400       IF SEGMENT-FINNS                                                   
171500          MOVE 999999              TO WS-TIBERANK                         
171600          MOVE ZERO                TO TMP1-YYMMDD                         
171700                                      TMP2-YYMMDD                         
171800          PERFORM IMS-GNP-WDL611                                          
171900          PERFORM UNTIL SEGMENT-SAKNAS                                    
172000            MOVE WDL6-INL-IDDC     TO WS-IDDC                             
172100            IF   NDC                                                      
172200            AND  WDL6-INL-IDDC      = SLAG-IDDC                           
172300                 IF WDL6-INL-IDPTYP = 'R30'                               
172400                    PERFORM FHAAA-WDL6-ETA-LATEST                         
172500                 END-IF                                                   
172600            END-IF                                                        
172700            PERFORM IMS-GNP-WDL611                                        
172800          END-PERFORM                                                     
172900                                                                          
173000       END-IF                                                             
173100     .                                                                    
173200     EJECT                                                                
173300 FHAAA-WDL6-ETA-LATEST  SECTION.                                          
173400                                                                          
173500     MOVE WDL6-INL-TIBERANK        TO TMP1-YYMMDD                         
173600     MOVE WS-TIBERANK              TO TMP2-YYMMDD                         
173700                                                                          
173800     PERFORM WY2000P1                                                     
173900     IF TMP1-YYMMDD                <  TMP2-YYMMDD                         
174000        MOVE WDL6-INL-TIBERANK     TO WS-TIBERANK                         
174100                                      RESP-TIBERANK-SDC(SDC-IX)           
174200     END-IF                                                               
174300     .                                                                    
174400     EJECT                                                                
174500 FI-GET-BUFFER-INFO    SECTION.                                           
174600                                                                          
174700     INITIALIZE TABELL                                                    
174800                                                                          
174900     PERFORM IMS-GU-WDK601                                                
175000     IF ART-KDERS-UTG              >  ZERO                                
175100        IF ART-KDERS-UTG           =  +29 OR +52                          
175200           MOVE INF-PART-EXPIRE    TO RESP-IDMSG-INFO                     
175300        ELSE                                                              
175400           MOVE INF-PART-REPLACED  TO RESP-IDMSG-INFO                     
175500        END-IF                                                            
175600     ELSE                                                                 
175700        IF ART-FLERS        = JA                                          
175800           MOVE INF-REPLACING-PART TO RESP-IDMSG-INFO                     
175900        END-IF                                                            
176000        PERFORM FIA-GET-WDK611-BUF                                        
176100     END-IF                                                               
176200     PERFORM FIB-GET-WDD811-BUF                                           
176300     PERFORM FIC-SORTERA-PLATSER                                          
176400     PERFORM FID-POPULATE-BUF-INFO                                        
176500     .                                                                    
176600     EJECT                                                                
176700 FIA-GET-WDK611-BUF    SECTION.                                           
176800                                                                          
176900     PERFORM IMS-GNP-WDK611                                               
177000                                                                          
177100     IF CLAG-ADLAGOMR-SVS        >  ZERO                                  
177200        MOVE 'P'                 TO TAB-PLATSTYP       (IX-RAD)           
177300        MOVE '1'                 TO TAB-PLATSTYP-SORT  (IX-RAD)           
177400        MOVE CLAG-ADLAGOMR-SVS                                            
177500                                 TO TAB-ADBUFFOMR      (IX-RAD)           
177600                                    TAB-ADBUFFOMR-SORT (IX-RAD)           
177700        MOVE CLAG-ADGANG-SVS                                              
177800                                 TO TAB-ADBUFFGANG     (IX-RAD)           
177900                                    TAB-ADBUFFGANG-SORT(IX-RAD)           
178000        MOVE CLAG-ADPLATS-SVS                                             
178100                                 TO TAB-ADBUFFPL       (IX-RAD)           
178200                                    TAB-ADBUFFPL-SORT  (IX-RAD)           
178300        MOVE CLAG-KVLS-SVS       TO TAB-KVBUFF-F       (IX-RAD)           
178400        MOVE ZERO                TO TAB-KVBUFF-OF      (IX-RAD)           
178500        ADD 1                    TO IX-RAD                                
178600     END-IF                                                               
178700                                                                          
178800     MOVE 1                      TO IX                                    
178900     PERFORM UNTIL IX > 4                                                 
179000     OR CLAG-ADLAGOMR-CD (IX)    =  ZERO                                  
179100        MOVE 'P'                 TO TAB-PLATSTYP       (IX-RAD)           
179200        MOVE '1'                 TO TAB-PLATSTYP-SORT  (IX-RAD)           
179300        MOVE CLAG-ADLAGOMR-CD (IX)                                        
179400                                 TO TAB-ADBUFFOMR      (IX-RAD)           
179500                                    TAB-ADBUFFOMR-SORT (IX-RAD)           
179600        MOVE CLAG-ADGANG-CD (IX)                                          
179700                                 TO TAB-ADBUFFGANG     (IX-RAD)           
179800                                    TAB-ADBUFFGANG-SORT(IX-RAD)           
179900        MOVE CLAG-ADPLATS-CD (IX)                                         
180000                                 TO TAB-ADBUFFPL       (IX-RAD)           
180100                                    TAB-ADBUFFPL-SORT  (IX-RAD)           
180200        MOVE CLAG-KVLS-CD (IX)                                            
180300                                 TO TAB-KVBUFF-F       (IX-RAD)           
180400        MOVE ZERO                TO TAB-KVBUFF-OF      (IX-RAD)           
180500        ADD 1                    TO IX                                    
180600                                    IX-RAD                                
180700     END-PERFORM                                                          
180800                                                                          
180900     IF CLAG-KDERS > ZERO                                                 
181000        IF CLAG-KDERS = +09 OR +19 OR +29 OR +52                          
181100          MOVE INF-PART-EXPIRE   TO RESP-IDMSG-INFO                       
181200        ELSE                                                              
181300          MOVE INF-PART-REPLACED TO RESP-IDMSG-INFO                       
181400        END-IF                                                            
181500     END-IF                                                               
181600     .                                                                    
181700     EJECT                                                                
181800 FIB-GET-WDD811-BUF    SECTION.                                           
181900                                                                          
182000     PERFORM IMS-GU-WDD801                                                
182100     IF SEGMENT-FINNS                                                     
182200        MOVE W-IDDC              TO W-IDDC-WDD8-MIN                       
182300                                    W-IDDC-WDD8-MAX                       
182400        MOVE ZERO                TO W-ADBUFFOMR-MIN                       
182500                                W-ADBUFFGANG-MIN                          
182600                                    W-ADBUFFPL-MIN                        
182700                                    W-DABUFPAF-MIN                        
182800                                                                          
182900        PERFORM IMS-GNP-WDD811                                            
183000        PERFORM UNTIL SEGMENT-SAKNAS OR IX-RAD > TAB-MAX                  
183100         IF SEGMENT-FINNS                                                 
183200           MOVE 'B'              TO TAB-PLATSTYP       (IX-RAD)           
183300           MOVE '2'              TO TAB-PLATSTYP-SORT  (IX-RAD)           
183400           MOVE BUFF-SALDO-ADBUFFOMR                                      
183500                                 TO TAB-ADBUFFOMR      (IX-RAD)           
183600                                    TAB-ADBUFFOMR-SORT (IX-RAD)           
183700           MOVE BUFF-SALDO-ADBUFFGANG                                     
183800                                 TO TAB-ADBUFFGANG     (IX-RAD)           
183900                                    TAB-ADBUFFGANG-SORT(IX-RAD)           
184000           MOVE BUFF-SALDO-ADBUFFPL                                       
184100                                 TO TAB-ADBUFFPL       (IX-RAD)           
184200                                    TAB-ADBUFFPL-SORT  (IX-RAD)           
184300           MOVE BUFF-SALDO-KVBUFF-OF                                      
184400                                 TO TAB-KVBUFF-OF      (IX-RAD)           
184500           MOVE BUFF-SALDO-KVBUFF-F                                       
184600                                 TO TAB-KVBUFF-F       (IX-RAD)           
184700           ADD 1                 TO IX-RAD                                
184800         END-IF                                                           
184900         PERFORM IMS-GNP-WDD811                                           
185000        END-PERFORM                                                       
185100     END-IF                                                               
185200     .                                                                    
185300     EJECT                                                                
185400                                                                          
185500 FIC-SORTERA-PLATSER   SECTION.                                           
185600                                                                          
185700     SUBTRACT 1                FROM IX-RAD                                
185800     MOVE IX-RAD                 TO ANTAL                                 
185900                                                                          
186000     CALL WINTSOR USING TABELL STEGLANGD ANTAL                            
186100                  TAB-SORT (1) NYCKELLANGD                                
186200     .                                                                    
186300     EJECT                                                                
186400 FID-POPULATE-BUF-INFO SECTION.                                           
186500                                                                          
186600     MOVE 1                      TO IX-RAD                                
186700                                    IX-RAD-TAB                            
186800     PERFORM UNTIL IX-RAD-TAB > TAB-MAX                                   
186900     OR TAB-PLATSTYP (IX-RAD-TAB) = SPACE                                 
187000     OR TAB-PLATSTYP (IX-RAD-TAB) = LOW-VALUE                             
187100        MOVE TAB-PLATSTYP  (IX-RAD-TAB)                                   
187200                                 TO RESP-4108-PLATSTYP  (IX-RAD)          
187300        MOVE TAB-ADBUFFOMR (IX-RAD-TAB)                                   
187400                                 TO RESP-4108-ADBUFFOMR (IX-RAD)          
187500        MOVE TAB-ADBUFFGANG(IX-RAD-TAB)                                   
187600                                 TO RESP-4108-ADBUFFGANG(IX-RAD)          
187700        MOVE TAB-ADBUFFPL  (IX-RAD-TAB)                                   
187800                                 TO RESP-4108-ADBUFFPL  (IX-RAD)          
187900        MOVE TAB-KVBUFF-OF (IX-RAD-TAB)                                   
188000                                 TO RESP-4108-KVBUFF-OF (IX-RAD)          
188100        MOVE TAB-KVBUFF-F  (IX-RAD-TAB)                                   
188200                                 TO RESP-4108-KVBUFF-F  (IX-RAD)          
188300        ADD 1                    TO IX-RAD                                
188400                                    IX-RAD-TAB                            
188500     END-PERFORM                                                          
188600     COMPUTE RESP-KVRADER-MAX5   = IX-RAD - 1                             
188700     .                                                                    
188800     EJECT                                                                
188900 FJ-CASE-SURVEY-INFO   SECTION.                                           
189000                                                                          
189100     PERFORM FJA-INIT-W6D1-KEYS                                           
189200     PERFORM FJB-GET-W6D111-INFO                                          
189300     .                                                                    
189400     EJECT                                                                
189500 FJA-INIT-W6D1-KEYS    SECTION.                                           
189600                                                                          
189700     MOVE IDARTNR-WS                  TO WH1-MIN-IDARTNR                  
189800     MOVE W-IDDC                      TO WH1-MIN-IDDC                     
189900     MOVE LOW-VALUE                   TO WH1-MIN-IDLEVNR                  
190000                                         WH1-MIN-IDFS                     
190100     MOVE +0000000                    TO WH1-MIN-TIAVIDAT                 
190200                                                                          
190300     MOVE IDARTNR-WS                  TO WH1-MAX-IDARTNR                  
190400     MOVE W-IDDC                      TO WH1-MAX-IDDC                     
190500     MOVE HIGH-VALUE                  TO WH1-MAX-IDLEVNR                  
190600                                         WH1-MAX-IDFS                     
190700     MOVE +9999999                    TO WH1-MAX-TIAVIDAT                 
190800                                                                          
190900     MOVE ZERO                        TO W-IDRADNR-D1                     
191000     MOVE W-IDDC                      TO W-IDDC-D1                        
191100                                                                          
191200     MOVE 'N'                         TO TRAEFF-SW                        
191300     .                                                                    
191400     EJECT                                                                
191500 FJB-GET-W6D111-INFO   SECTION.                                           
191600                                                                          
191700     MOVE ZERO                        TO RESP-KVRADER-MAX6                
191800     PERFORM IMS-GU-W6D1H1-D111-X                                         
191900                                                                          
192000     IF SEGMENT-SAKNAS                                                    
192100        CONTINUE                                                          
192200     ELSE                                                                 
192300        MOVE SEQH-IDARTNR             TO W-IDARTNR-D1                     
192400                                         W-MINKEY-IDARTNR                 
192500        MOVE SEQH-IDLEVNR             TO W-IDLEVNR-D1                     
192600                                         W-MINKEY-IDLEVNR                 
192700                                         WS-SPAR-IDLEVNR                  
192800        MOVE SEQH-IDFS                TO W-IDFS-D1                        
192900                                         W-MINKEY-IDFS                    
193000        MOVE SEQH-TIAVIDAT            TO W-TIAVIDAT-D1                    
193100                                         W-MINKEY-TIAVIDAT                
193200        MOVE SEQH-IDRADNR-INL         TO W-IDRADNR-INL                    
193300                                         W-MINKEY-IDRADNR-INL             
193400        PERFORM FJBA-BEARB-RADDATA                                        
193500     END-IF                                                               
193600     .                                                                    
193700     EJECT                                                                
193800 FJBA-BEARB-RADDATA    SECTION.                                           
193900                                                                          
194000     PERFORM IMS-GU-W6D1-D111                                             
194100     IF SEGMENT-FINNS                                                     
194200        MOVE H-ART-IDDC               TO SPAR-IDDC                        
194300        MOVE H-ART-IDLOPNRM           TO WS-SPAR-IDLOPNRM                 
194400        MOVE H-ART-FLKLAR             TO WS-FLKLAR                        
194500        PERFORM IMS-GNP-W6D1-D121                                         
194600        MOVE    RAD-IDRADNR           TO W-MINKEY-IDRADNR                 
194700                                                                          
194800        PERFORM FJBAA-LAES-RADDATA                                        
194900     END-IF                                                               
195000     .                                                                    
195100     EJECT                                                                
195200 FJBAA-LAES-RADDATA    SECTION.                                           
195300                                                                          
195400     MOVE NEJ                             TO SLUT-SW                      
195500     MOVE +1                              TO IX                           
195600     PERFORM UNTIL IX > WS-KVRADER-MAX2                                   
195700                                                                          
195800        IF W6D1-STATUS-CODE = SPACE AND SPAR-IDDC = W-IDDC-D1             
195900           IF IX < WS-KVRADER-MAX2                                        
196000              MOVE NEJ                    TO TRAEFF-SW                    
196100              IF WS-FLKLAR = NEJ                                          
196200                PERFORM FJBAAA-SEGM-21-RAD                                
196300                ADD +1                    TO RESP-KVRADER-MAX6            
196400              END-IF                                                      
196500              MOVE RAD-IDRADNR            TO W-IDRADNR-D1                 
196600              PERFORM IMS-GNP-W6D1-D121                                   
196700           END-IF                                                         
196800        ELSE                                                              
196900           PERFORM UNTIL TRAEFF-JA OR SLUT-JA                             
197000              PERFORM IMS-GN-W6D1H1-D111-X                                
197100              IF W6D1H1-STATUS-CODE = SPACE                               
197200                 IF IX < WS-KVRADER-MAX2                                  
197300                    MOVE SEQH-IDLEVNR     TO WH1-MIN-IDLEVNR              
197400                                             W-IDLEVNR-D1                 
197500                                             WS-SPAR-IDLEVNR              
197600                    MOVE SEQH-IDFS        TO WH1-MIN-IDFS                 
197700                                             W-IDFS-D1                    
197800                    MOVE SEQH-TIAVIDAT    TO WH1-MIN-TIAVIDAT             
197900                                             W-TIAVIDAT-D1                
198000                    MOVE SEQH-IDRADNR-INL TO WH1-MIN-IDRADNR-INL          
198100                                             W-IDRADNR-INL                
198200                    PERFORM IMS-GU-W6D1-D111                              
198300                    IF W6D1-STATUS-CODE = SPACE AND                       
198400                                        H-ART-IDDC = W-IDDC               
198500                       MOVE H-ART-FLKLAR  TO WS-FLKLAR                    
198600                       MOVE ZERO          TO W-IDRADNR-D1                 
198700                       MOVE H-ART-IDLOPNRM  TO WS-SPAR-IDLOPNRM           
198800                       PERFORM IMS-GNP-W6D1-D121                          
198900                       IF W6D1-STATUS-CODE = SPACE                        
199000                          MOVE JA         TO TRAEFF-SW                    
199100                       END-IF                                             
199200                    END-IF                                                
199300                 END-IF                                                   
199400              ELSE                                                        
199500                 MOVE JA                  TO SLUT-SW                      
199600              END-IF                                                      
199700           END-PERFORM                                                    
199800        END-IF                                                            
199900        IF TRAEFF-NEJ                                                     
200000          IF WS-FLKLAR = NEJ  OR SLUT-JA                                  
200100             ADD 1                        TO IX                           
200200          END-IF                                                          
200300        END-IF                                                            
200400                                                                          
200500     END-PERFORM                                                          
200600     .                                                                    
200700     EJECT                                                                
200800 FJBAAA-SEGM-21-RAD    SECTION.                                           
200900                                                                          
201000     MOVE WS-SPAR-IDLOPNRM       TO RESP-6123-IDLOPNRM    (IX)            
201100     MOVE RAD-IDRADNR            TO WS-IDRADNR                            
201200     MOVE WS-RED-IDRADNR         TO RESP-6123-IDRADNR     (IX)            
201300     MOVE RAD-IDOKOLLI           TO RESP-6123-IDOKOLLI    (IX)            
201400     MOVE ZERO                   TO WS-KVINLART                           
201500     MOVE RAD-KVINLART           TO WS-KVINLART                           
201600     IF RAD-KDINLSTA = 'AVV' OR 'ANT' OR 'KVA'                            
201700        IF WS-KVINLART < ZERO                                             
201800          COMPUTE WS-KVINLART = WS-KVINLART * - 1                         
201900        END-IF                                                            
202000     END-IF                                                               
202100     MOVE WS-KVINLART            TO RESP-6123-KVINLART    (IX)            
202200     MOVE RAD-ADINLOMR           TO RESP-6123-ADINLOMR    (IX)            
202300     MOVE RAD-IDINLVGN           TO RESP-6123-IDINLVGN    (IX)            
202400     MOVE RAD-ADINLOMR-NXT       TO RESP-6123-ADINLOMR-NXT(IX)            
202500     MOVE RAD-KDINLSTA           TO RESP-6123-KDINLSTA    (IX)            
202600     EVALUATE RESP-6123-KDINLSTA(IX)                                      
202700         WHEN 'FPK'                                                       
202800            MOVE 'PP '           TO RESP-6123-KDINLSTA    (IX)            
202900         WHEN 'INL'                                                       
203000            MOVE 'BIN'           TO RESP-6123-KDINLSTA    (IX)            
203100         WHEN 'SAK'                                                       
203200            MOVE 'MIS'           TO RESP-6123-KDINLSTA    (IX)            
203300         WHEN 'AVV'                                                       
203400            MOVE 'DEV'           TO RESP-6123-KDINLSTA    (IX)            
203500         WHEN 'ANT'                                                       
203600            MOVE 'DEV'           TO RESP-6123-KDINLSTA    (IX)            
203700         WHEN 'KVA'                                                       
203800            MOVE 'Q-D'           TO RESP-6123-KDINLSTA    (IX)            
203900         WHEN 'RET'                                                       
204000            MOVE 'RET'           TO RESP-6123-KDINLSTA    (IX)            
204100         WHEN 'FRD'                                                       
204200            MOVE 'TRP'           TO RESP-6123-KDINLSTA    (IX)            
204300         WHEN 'MAK'                                                       
204400            MOVE 'CAN'           TO RESP-6123-KDINLSTA    (IX)            
204500     END-EVALUATE                                                         
204600                                                                          
204700     IF RAD-IDLEVNR-KOLLI = SPACE                                         
204800        MOVE WS-SPAR-IDLEVNR     TO RESP-6123-IDLEVNR     (IX)            
204900     ELSE                                                                 
205000        MOVE RAD-IDLEVNR-KOLLI   TO RESP-6123-IDLEVNR     (IX)            
205100     END-IF                                                               
205200     .                                                                    
205300     EJECT                                                                
205400 FL-GET-WDP5-INFO SECTION.                                                
205500                                                                          
205600     MOVE +1                          TO IX-A                             
205700     MOVE 'S  '                       TO W-IDSKYLT-P5                     
205800     MOVE 'ARTNOT  '                  TO W-IDDOKTYP                       
205900     MOVE IDARTNR-WS(2:8)             TO W-IDDOK                          
206000     INSPECT W-IDDOK REPLACING ALL '+'      BY SPACE                      
206100     INSPECT W-IDDOK REPLACING LEADING ZERO BY SPACE                      
206200     PERFORM IMS-GU-WDP501                                                
206300     IF SEGMENT-FINNS                                                     
206400       PERFORM IMS-GNP-WDP512                                             
206500         PERFORM UNTIL                                                    
206600          NOT ( SEGMENT-FINNS AND                                         
206700                IX-A          NOT > WS-KVRADER-MAX8)                      
206800           PERFORM S03-DATUMKONV-TILL-AAVVD                               
206900           IF DAT-KDSVAR-OK                                               
207000             MOVE TEXT-TEINFO         TO RESP-TEINFO (IX-A)               
207100             ADD +1                   TO IX-A                             
207200           END-IF                                                         
207300           PERFORM IMS-GNP-WDP512                                         
207400         END-PERFORM                                                      
207500     END-IF                                                               
207600     COMPUTE RESP-KVRADER-MAX8  = IX-A - 1                                
207700     .                                                                    
207800     EJECT                                                                
207900 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
208000                                                                          
208100     MOVE 'GETARG'               TO SUB-KDFUNC                            
208200     MOVE 'CARPARTS.VOR.TOTALINFO'         TO SUB-ADDISPABS               
208300     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
208400                                                                          
208500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
208600                                                                          
208700     IF SUB-KDRC > 0                                                      
208800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
208900       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
209000       DELIMITED BY SIZE INTO FELTEXT                                     
209100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
209200     END-IF                                                               
209300     .                                                                    
209400     SKIP3                                                                
209500 S02-RETURN-RESPONSE SECTION.                                             
209600                                                                          
209700     MOVE 'RETURN'                   TO SUB-KDFUNC                        
209800     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
209900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
210000                                                                          
210100     IF SUB-KDRC > 0                                                      
210200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
210300       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
210400       DELIMITED BY SIZE INTO FELTEXT                                     
210500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
210600     END-IF                                                               
210700     .                                                                    
210800     EJECT                                                                
210900 S03-DATUMKONV-TILL-AAVVD  SECTION.                                       
211000                                                                          
211100     MOVE 'AAMMDD'                   TO DAT-KDDATFORM                     
211200     CALL WDATKONV USING                DAT-KDDATFORM,                    
211300                                        DAT-I-TIDATUM,                    
211400                                        DAT-O-TIDATUM,                    
211500                                        DAT-KDSVAR                        
211600     .                                                                    
211700     EJECT                                                                
211800* IMS SEKTIONER                                                           
211900*                                                                         
212000 IMS-GU-WDK601 SECTION.                                                   
212100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
212200             DELIMITED BY SIZE INTO SSA1                                  
212300     MOVE '  GE'               TO GODK-STATUSKODER                        
212400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
212500     MOVE WDK6-STATUS-CODE     TO STATUS-WS                               
212600     PERFORM IMS-STATUSKONTROLL                                           
212700     .                                                                    
212800     SKIP3                                                                
212900 IMS-GNP-WDK611 SECTION.                                                  
213000     MOVE 'WDK611   '          TO SSA1                                    
213100     MOVE '  GE'               TO GODK-STATUSKODER                        
213200     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1                    
213300     MOVE WDK6-STATUS-CODE     TO STATUS-WS                               
213400     PERFORM IMS-STATUSKONTROLL                                           
213500     .                                                                    
213600     SKIP3                                                                
213700 IMS-GNP-WDK625 SECTION.                                                  
213800     STRING 'WDK625  (KDNOTTYP =' W-KDNOTTYP-X ')'                        
213900          DELIMITED BY SIZE INTO SSA1                                     
214000     MOVE '  GE'               TO GODK-STATUSKODER                        
214100     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK625 SSA1                   
214200     MOVE WDK6-STATUS-CODE     TO STATUS-WS                               
214300     PERFORM IMS-STATUSKONTROLL                                           
214400     .                                                                    
214500     SKIP3                                                                
214600 IMS-GU-WDD311 SECTION.                                                   
214700     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
214800             DELIMITED BY SIZE INTO SSA1                                  
214900     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
215000             DELIMITED BY SIZE INTO SSA2                                  
215100     MOVE '  GE'               TO GODK-STATUSKODER                        
215200     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
215300     MOVE WDD3-STATUS-CODE     TO STATUS-WS                               
215400     PERFORM IMS-STATUSKONTROLL                                           
215500     .                                                                    
215600     SKIP3                                                                
215700 IMS-GU-WDB601    SECTION.                                                
215800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
215900             DELIMITED BY SIZE INTO SSA1                                  
216000     MOVE '  GE'               TO GODK-STATUSKODER                        
216100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
216200     MOVE WDB6-STATUS-CODE     TO STATUS-WS                               
216300     PERFORM IMS-STATUSKONTROLL                                           
216400     IF SEGMENT-SAKNAS                                                    
216500         MOVE SPACE TO DCS-KDDC                                           
216600     END-IF                                                               
216700     .                                                                    
216800     SKIP3                                                                
216900 IMS-GU-WDD902  SECTION.                                                  
217000     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
217100          DELIMITED BY SIZE INTO SSA1                                     
217200     STRING 'WDD902  (IDLEVNR  =' W-WDD902KY-X ')'                        
217300          DELIMITED BY SIZE INTO SSA2                                     
217400     MOVE '  GE'               TO GODK-STATUSKODER                        
217500     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD902 SSA1 SSA2               
217600     MOVE WDD9-STATUS-CODE     TO STATUS-WS                               
217700     PERFORM IMS-STATUSKONTROLL                                           
217800     .                                                                    
217900     SKIP3                                                                
218000 IMS-GNP-WDD924 SECTION.                                                  
218100     STRING 'WDD924   '                                                   
218200            DELIMITED BY SIZE INTO SSA1                                   
218300     MOVE '  GE'               TO GODK-STATUSKODER                        
218400     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD924 SSA1                   
218500     MOVE WDD9-STATUS-CODE     TO STATUS-WS                               
218600     PERFORM IMS-STATUSKONTROLL                                           
218700     .                                                                    
218800     SKIP3                                                                
218900 IMS-GU-WDD925       SECTION.                                             
219000     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
219100          DELIMITED BY SIZE INTO SSA1                                     
219200     STRING 'WDD902  (IDLEVNR  =' W-WDD902KY-X ')'                        
219300          DELIMITED BY SIZE INTO SSA2                                     
219400     STRING 'WDD925  (IDLEVBSK =' W-IDLEVBSK-X ')'                        
219500            DELIMITED BY SIZE INTO SSA3                                   
219600     MOVE '  GE'               TO GODK-STATUSKODER                        
219700     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD925 SSA1 SSA2 SSA3          
219800     MOVE WDD9-STATUS-CODE     TO STATUS-WS                               
219900     PERFORM IMS-STATUSKONTROLL                                           
220000     .                                                                    
220100     SKIP3                                                                
220200 IMS-GET-ERSATT-INFO SECTION.                                             
220300     STRING 'WDD702  *D(WDD7ASEQ =' W-IDARTNR-X ')'                       
220400            DELIMITED BY SIZE INTO SSA1                                   
220500     MOVE 'WDD701   ' TO SSA2                                             
220600     MOVE '  GBGE'             TO GODK-STATUSKODER                        
220700     CALL CBLTDLI USING GN WDD7A-PCB DLI-IO-WDD7 SSA1 SSA2                
220800     MOVE WDD7A-STATUS-CODE    TO STATUS-WS                               
220900     PERFORM IMS-STATUSKONTROLL                                           
221000     .                                                                    
221100     SKIP3                                                                
221200 IMS-GET-ERSATT-SEG SECTION.                                              
221300     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
221400            DELIMITED BY SIZE INTO SSA1                                   
221500     MOVE '  GE'               TO GODK-STATUSKODER                        
221600     CALL CBLTDLI USING GU WDD7-PCB DLI-IO-WDD701 SSA1                    
221700     MOVE WDD7-STATUS-CODE     TO STATUS-WS                               
221800     PERFORM IMS-STATUSKONTROLL                                           
221900     .                                                                    
222000     SKIP3                                                                
222100 IMS-GET-TILLK-SEG SECTION.                                               
222200     MOVE 'WDD702  ' TO SSA1                                              
222300     MOVE '  GE'               TO GODK-STATUSKODER                        
222400     CALL CBLTDLI USING GNP WDD7-PCB DLI-IO-WDD702 SSA1                   
222500     MOVE WDD7-STATUS-CODE     TO STATUS-WS                               
222600     PERFORM IMS-STATUSKONTROLL                                           
222700     .                                                                    
222800     SKIP3                                                                
222900 IMS-GU-WDP311     SECTION.                                               
223000     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
223100            DELIMITED BY SIZE INTO SSA1                                   
223200     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
223300            DELIMITED BY SIZE INTO SSA2                                   
223400     MOVE '  GE'               TO GODK-STATUSKODER                        
223500     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-WDP311 SSA1 SSA2               
223600     MOVE WDP3-STATUS-CODE     TO STATUS-WS                               
223700     PERFORM IMS-STATUSKONTROLL                                           
223800     .                                                                    
223900     SKIP3                                                                
224000 IMS-GU-WDP3A      SECTION.                                               
224100     STRING 'WDP3A1  (WDP3A1KY=>' W-WDP3A1-MIN                            
224200                    '&WDP3A1KY=<' W-WDP3A1-MAX                            
224300                    '&KDARBTYP =' W-KDARBTYP ')'                          
224400            DELIMITED BY SIZE INTO SSA1                                   
224500     MOVE '  GE'               TO GODK-STATUSKODER                        
224600     CALL CBLTDLI USING GU WDP3A-PCB DLI-IO-WDP3A SSA1                    
224700     MOVE WDP3A-STATUS-CODE    TO STATUS-WS                               
224800     PERFORM IMS-STATUSKONTROLL                                           
224900     .                                                                    
225000     SKIP3                                                                
225100 IMS-GN-WDP3A      SECTION.                                               
225200     STRING 'WDP3A1  (WDP3A1KY=>' W-WDP3A1-MIN                            
225300                    '&WDP3A1KY=<' W-WDP3A1-MAX                            
225400                    '&KDARBTYP =' W-KDARBTYP ')'                          
225500            DELIMITED BY SIZE INTO SSA1                                   
225600     MOVE '  GEGB'             TO GODK-STATUSKODER                        
225700     CALL CBLTDLI USING GN WDP3A-PCB DLI-IO-WDP3A SSA1                    
225800     MOVE WDP3A-STATUS-CODE    TO STATUS-WS                               
225900     PERFORM IMS-STATUSKONTROLL                                           
226000     .                                                                    
226100     SKIP3                                                                
226200 IMS-GU-WDP3B      SECTION.                                               
226300     STRING 'WDP3B1  (WDP3B1KY =' W-WDP3B1-X                              
226400                    '&KDARBTYP =' W-KDARBTYP ')'                          
226500            DELIMITED BY SIZE INTO SSA1                                   
226600     MOVE '  GE'               TO GODK-STATUSKODER                        
226700     CALL CBLTDLI USING GU WDP3B-PCB DLI-IO-WDP3B SSA1                    
226800     MOVE WDP3B-STATUS-CODE    TO STATUS-WS                               
226900     PERFORM IMS-STATUSKONTROLL                                           
227000     .                                                                    
227100     SKIP3                                                                
227200 IMS-GU-WDP3C      SECTION.                                               
227300     STRING 'WDP3C1  (WDP3C1KY=>' W-WDP3C1-MIN                            
227400                    '&WDP3C1KY=<' W-WDP3C1-MAX                            
227500                    '&KDARBTYP =' W-KDARBTYP ')'                          
227600            DELIMITED BY SIZE INTO SSA1                                   
227700     MOVE '  GE'               TO GODK-STATUSKODER                        
227800     CALL CBLTDLI USING GU WDP3C-PCB DLI-IO-WDP3C SSA1                    
227900     MOVE WDP3C-STATUS-CODE    TO STATUS-WS                               
228000     PERFORM IMS-STATUSKONTROLL                                           
228100     .                                                                    
228200     SKIP3                                                                
228300 IMS-GN-WDP3C      SECTION.                                               
228400     STRING 'WDP3C1  (WDP3C1KY=>' W-WDP3C1-MIN                            
228500                    '&WDP3C1KY=<' W-WDP3C1-MAX                            
228600                    '&KDARBTYP =' W-KDARBTYP ')'                          
228700            DELIMITED BY SIZE INTO SSA1                                   
228800     MOVE '  GEGB'             TO GODK-STATUSKODER                        
228900     CALL CBLTDLI USING GN WDP3C-PCB DLI-IO-WDP3C SSA1                    
229000     MOVE WDP3C-STATUS-CODE    TO STATUS-WS                               
229100     PERFORM IMS-STATUSKONTROLL                                           
229200     .                                                                    
229300     SKIP3                                                                
229400 IMS-GU-WDB301 SECTION.                                                   
229500     STRING 'WDB301  (WDB301KY =' W-WDB301KY-X                            
229600                    '+WDB301KY =' W-WDB301KY-DEF-X ')'                    
229700            DELIMITED BY SIZE INTO SSA1                                   
229800     MOVE '  GE'               TO GODK-STATUSKODER                        
229900     CALL CBLTDLI USING GU WDB3-PCB DLI-IO-WDB301 SSA1                    
230000     MOVE WDB3-STATUS-CODE     TO STATUS-WS                               
230100     PERFORM IMS-STATUSKONTROLL                                           
230200     .                                                                    
230300     SKIP3                                                                
230400 IMS-GU-WDB201  SECTION.                                                  
230500     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
230600            DELIMITED BY SIZE INTO SSA1                                   
230700     MOVE '  GE'               TO GODK-STATUSKODER                        
230800     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
230900     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
231000     PERFORM IMS-STATUSKONTROLL                                           
231100     .                                                                    
231200     SKIP3                                                                
231300 IMS-GET-WDL201 SECTION.                                                  
231400     STRING 'WDL201  (IDARTNR  =' W-IDARTNR-X ')'                         
231500             DELIMITED BY SIZE INTO SSA1                                  
231600     MOVE '  GE'               TO GODK-STATUSKODER                        
231700     CALL CBLTDLI USING GU WDL2-PCB DLI-IO-WDL2 SSA1                      
231800     MOVE WDL2-STATUS-CODE     TO STATUS-WS                               
231900     PERFORM IMS-STATUSKONTROLL                                           
232000     .                                                                    
232100     SKIP3                                                                
232200 IMS-GET-WDL211 SECTION.                                                  
232300     STRING 'WDL201  (IDARTNR  =' W-IDARTNR-X ')'                         
232400             DELIMITED BY SIZE INTO SSA1                                  
232500     STRING 'WDL211  (DAINLEV =>' W-DAINLEV-X ')'                         
232600             DELIMITED BY SIZE INTO SSA2                                  
232700     MOVE '  GE'               TO GODK-STATUSKODER                        
232800     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL2 SSA1 SSA2                
232900     MOVE WDL2-STATUS-CODE     TO STATUS-WS                               
233000     PERFORM IMS-STATUSKONTROLL                                           
233100     .                                                                    
233200     SKIP3                                                                
233300 IMS-GET-31-32-310 SECTION.                                               
233400     STRING 'WDL211  (DAINLEV  =' W-DAINLEV-X ')'                         
233500             DELIMITED BY SIZE INTO SSA1                                  
233600     MOVE 'WDL221  '           TO SSA2                                    
233700     MOVE '  GE' TO GODK-STATUSKODER                                      
233800     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL2 SSA1 SSA2                
233900     MOVE WDL2-STATUS-CODE     TO STATUS-WS                               
234000     PERFORM IMS-STATUSKONTROLL                                           
234100     .                                                                    
234200     SKIP3                                                                
234300 IMS-GET-33-34 SECTION.                                                   
234400     STRING 'WDL211  (DAINLEV  =' W-DAINLEV-X ')'                         
234500             DELIMITED BY SIZE INTO SSA1                                  
234600     MOVE 'WDL222  '           TO SSA2                                    
234700     MOVE '  GE' TO GODK-STATUSKODER                                      
234800     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL2 SSA1 SSA2                
234900     MOVE WDL2-STATUS-CODE     TO STATUS-WS                               
235000     PERFORM IMS-STATUSKONTROLL                                           
235100     .                                                                    
235200     SKIP3                                                                
235300 IMS-GET-40 SECTION.                                                      
235400     STRING 'WDL211  (DAINLEV  =' W-DAINLEV-X ')'                         
235500             DELIMITED BY SIZE INTO SSA1                                  
235600     MOVE 'WDL223  ' TO SSA2                                              
235700     MOVE '  GE'               TO GODK-STATUSKODER                        
235800     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL2 SSA1 SSA2                
235900     MOVE WDL2-STATUS-CODE     TO STATUS-WS                               
236000     PERFORM IMS-STATUSKONTROLL                                           
236100     .                                                                    
236200     SKIP3                                                                
236300 IMS-GET-DEL SECTION.                                                     
236400     STRING 'WDL211  (DAINLEV  =' W-DAINLEV-X ')'                         
236500             DELIMITED BY SIZE INTO SSA1                                  
236600     MOVE 'WDL221  ' TO SSA2                                              
236700     MOVE 'WDL231  ' TO SSA3                                              
236800     MOVE '  GE'               TO GODK-STATUSKODER                        
236900     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL2 SSA1 SSA2 SSA3           
237000     MOVE WDL2-STATUS-CODE     TO STATUS-WS                               
237100     PERFORM IMS-STATUSKONTROLL                                           
237200     .                                                                    
237300     SKIP3                                                                
237400 IMS-GET-WDL601 SECTION.                                                  
237500                                                                          
237600     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
237700          DELIMITED BY SIZE INTO SSA1                                     
237800     MOVE '  GE'   TO GODK-STATUSKODER                                    
237900     CALL CBLTDLI  USING GU WDL6-PCB DLI-IO-WDL601 SSA1                   
238000     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
238100     PERFORM IMS-STATUSKONTROLL                                           
238200     .                                                                    
238300     EJECT                                                                
238400 IMS-GET-WDL611 SECTION.                                                  
238500                                                                          
238600     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
238700          DELIMITED BY SIZE INTO SSA1                                     
238800     STRING 'WDL611  (DAINLEV =>' W-DAINLEV-X ')'                         
238900          DELIMITED BY SIZE INTO SSA2                                     
239000     MOVE '  GE'   TO GODK-STATUSKODER                                    
239100     CALL CBLTDLI  USING GNP WDL6-PCB DLI-IO-WDL611 SSA1 SSA2             
239200     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
239300     PERFORM IMS-STATUSKONTROLL                                           
239400     .                                                                    
239500     EJECT                                                                
239600 IMS-GU-WDK701         SECTION.                                           
239700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
239800          DELIMITED BY SIZE INTO SSA1                                     
239900     MOVE '  GE'               TO GODK-STATUSKODER                        
240000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
240100     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
240200     PERFORM IMS-STATUSKONTROLL                                           
240300     .                                                                    
240400     SKIP3                                                                
240500 IMS-GNP-WDK711        SECTION.                                           
240600     MOVE 'WDK711   ' TO SSA1                                             
240700     MOVE '  GE'               TO GODK-STATUSKODER                        
240800     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
240900     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
241000     PERFORM IMS-STATUSKONTROLL                                           
241100     .                                                                    
241200     SKIP3                                                                
241300 IMS-GU-WDD801    SECTION.                                                
241400     STRING 'WDD801  (IDARTNR  =' W-IDARTNR-X ')'                         
241500             DELIMITED BY SIZE INTO SSA1                                  
241600     MOVE '  GE'               TO GODK-STATUSKODER                        
241700     CALL CBLTDLI USING GU WDD8-PCB DLI-IO-WDD801 SSA1                    
241800     MOVE WDD8-STATUS-CODE     TO STATUS-WS                               
241900     PERFORM IMS-STATUSKONTROLL                                           
242000     .                                                                    
242100     SKIP3                                                                
242200 IMS-GNP-WDD811          SECTION.                                         
242300     STRING 'WDD811  (WDD811KY>=' W-WDD811KY-MIN-X                        
242400                    '&WDD811KY<=' W-WDD811KY-MAX-X ')'                    
242500             DELIMITED BY SIZE INTO SSA1                                  
242600     MOVE '  GE'               TO GODK-STATUSKODER                        
242700     CALL CBLTDLI USING GNP WDD8-PCB DLI-IO-WDD811 SSA1                   
242800     MOVE WDD8-STATUS-CODE     TO STATUS-WS                               
242900     PERFORM IMS-STATUSKONTROLL                                           
243000     .                                                                    
243100     SKIP3                                                                
243200 IMS-GN-W6D111-W6D1HSEQ SECTION.                                          
243300     STRING 'W6D111  (W6D1HSEQ =' W-W6D1HSEQ-X ')'                        
243400             DELIMITED BY SIZE INTO SSA1                                  
243500     MOVE '  GEGB'             TO GODK-STATUSKODER                        
243600     CALL CBLTDLI USING GN W6D1H-PCB DLI-IO-W6D1 SSA1                     
243700     MOVE W6D1H-STATUS-CODE    TO STATUS-WS                               
243800     PERFORM IMS-STATUSKONTROLL                                           
243900     .                                                                    
244000     SKIP3                                                                
244100 IMS-GU-W6D1H1-D111-X  SECTION.                                           
244200     STRING 'W6D1H1  (W6D1H1KY>=' W-W6D1H1KY-MIN-X                        
244300                    '&W6D1H1KY<=' W-W6D1H1KY-MAX-X ')'                    
244400             DELIMITED BY SIZE INTO SSA1                                  
244500     MOVE '  GE'                TO GODK-STATUSKODER                       
244600     CALL CBLTDLI USING GU W6D1H1-PCB DLI-IO-W6D1 SSA1                    
244700     MOVE W6D1H1-STATUS-CODE    TO STATUS-WS                              
244800     PERFORM IMS-STATUSKONTROLL                                           
244900     .                                                                    
245000     SKIP3                                                                
245100 IMS-GN-W6D1H1-D111-X  SECTION.                                           
245200     STRING 'W6D1H1  (W6D1H1KY>=' W-W6D1H1KY-MIN-X                        
245300                    '&W6D1H1KY<=' W-W6D1H1KY-MAX-X ')'                    
245400             DELIMITED BY SIZE INTO SSA1                                  
245500     MOVE '  GE'                TO GODK-STATUSKODER                       
245600     CALL CBLTDLI USING GN W6D1H1-PCB DLI-IO-W6D1 SSA1                    
245700     MOVE W6D1H1-STATUS-CODE    TO STATUS-WS                              
245800     PERFORM IMS-STATUSKONTROLL                                           
245900     .                                                                    
246000     SKIP3                                                                
246100 IMS-GU-W6D1-D111 SECTION.                                                
246200     STRING 'W6D101  (W6D101KY =' W-W6D101KY-X ')'                        
246300             DELIMITED BY SIZE INTO SSA1                                  
246400     STRING 'W6D111  (IDRADNRI =' W-IDRADNR-INL-X ')'                     
246500          DELIMITED BY SIZE INTO SSA2                                     
246600     MOVE '  GE'                TO GODK-STATUSKODER                       
246700     CALL CBLTDLI USING GU W6D1-PCB DLI-IO-W6D1 SSA1 SSA2                 
246800     MOVE W6D1-STATUS-CODE      TO STATUS-WS                              
246900     PERFORM IMS-STATUSKONTROLL                                           
247000     .                                                                    
247100     SKIP3                                                                
247200 IMS-GNP-W6D1-D121 SECTION.                                               
247300     STRING 'W6D101  (W6D101KY =' W-W6D101KY-X ')'                        
247400             DELIMITED BY SIZE INTO SSA1                                  
247500     STRING 'W6D111  (IDRADNRI =' W-IDRADNR-INL-X ')'                     
247600          DELIMITED BY SIZE INTO SSA2                                     
247700     STRING 'W6D121  (IDRADNR =>' W-IDRADNR-X ')'                         
247800          DELIMITED BY SIZE INTO SSA3                                     
247900     MOVE '  GE'                TO GODK-STATUSKODER                       
248000     CALL CBLTDLI USING GNP W6D1-PCB DLI-IO-W6D1 SSA1 SSA2 SSA3           
248100     MOVE W6D1-STATUS-CODE      TO STATUS-WS                              
248200     PERFORM IMS-STATUSKONTROLL                                           
248300     .                                                                    
248400     SKIP3                                                                
248500 IMS-03-GN-WDA5A1 SECTION.                                                
248600     STRING 'WDA5A1  (WDA5A1KY>=' W-WDA5A1KY-MIN-X                        
248700                    '&WDA5A1KY<=' W-WDA5A1KY-MAX-X                        
248800                    '&KDORDKL = ' W-KDORDKL-X                             
248900                    '&TIREPDAT >' WS-TIREPDAT-X                           
249000                    '&IDSYSTX3= ' W-IDSYSTX3                              
249100                    '&KDSTARAD <' W-KDSTARAD                              
249200                    '&KDORDTYP= ' VERKSTADSORDER                          
249300                    '!WDA5A1KY>=' W-WDA5A1KY-MIN-X                        
249400                    '&WDA5A1KY<=' W-WDA5A1KY-MAX-X                        
249500                    '&KDORDKL = ' W-KDORDKL-X                             
249600                    '&TIREPDAT >' WS-TIREPDAT-X                           
249700                    '&IDSYSTX3= ' W-IDSYSTX3                              
249800                    '&KDSTARAD <' W-KDSTARAD                              
249900                    '&KDORDTYP= ' BUTIKSORDER ')'                         
250000            DELIMITED BY SIZE INTO SSA1                                   
250100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
250200     CALL CBLTDLI USING GN WDA5A-PCB DLI-IO-WDA5A SSA1                    
250300     MOVE WDA5A-STATUS-CODE     TO STATUS-WS                              
250400     PERFORM IMS-STATUSKONTROLL                                           
250500     .                                                                    
250600     SKIP3                                                                
250700 IMS-04-GU-WDA501 SECTION.                                                
250800     STRING 'WDA501  (WDA501KY>=' W-WDA5KEY-X                             
250900                    '&KDSTARAD <' W-KDSTARAD ')'                          
251000            DELIMITED BY SIZE  INTO SSA1                                  
251100     MOVE '  GE' TO GODK-STATUSKODER                                      
251200     CALL CBLTDLI USING GU  WDA5-PCB DLI-IO-WDA5 SSA1                     
251300     MOVE WDA5-STATUS-CODE      TO STATUS-WS                              
251400     PERFORM IMS-STATUSKONTROLL                                           
251500     .                                                                    
251600     SKIP3                                                                
251700 IMS-GU-WDL601      SECTION.                                              
251800     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
251900            DELIMITED BY SIZE INTO SSA1                                   
252000     MOVE '  GE' TO GODK-STATUSKODER                                      
252100     CALL CBLTDLI USING GU  WDL6-PCB DLI-IO-WDL601 SSA1                   
252200     MOVE WDL6-STATUS-CODE      TO STATUS-WS                              
252300     PERFORM IMS-STATUSKONTROLL                                           
252400     .                                                                    
252500     SKIP3                                                                
252600 IMS-GNP-WDL611      SECTION.                                             
252700     STRING 'WDL611     '                                                 
252800            DELIMITED BY SIZE INTO SSA1                                   
252900     MOVE '  GE' TO GODK-STATUSKODER                                      
253000     CALL CBLTDLI USING GNP WDL6-PCB DLI-IO-WDL611 SSA1                   
253100     MOVE WDL6-STATUS-CODE      TO STATUS-WS                              
253200     PERFORM IMS-STATUSKONTROLL                                           
253300     .                                                                    
253400     SKIP3                                                                
253500 IMS-GNP-WDL612      SECTION.                                             
253600     STRING 'WDL612     '                                                 
253700            DELIMITED BY SIZE INTO SSA1                                   
253800     MOVE '  GE' TO GODK-STATUSKODER                                      
253900     CALL CBLTDLI USING GNP WDL6-PCB DLI-IO-WDL612 SSA1                   
254000     MOVE WDL6-STATUS-CODE      TO STATUS-WS                              
254100     PERFORM IMS-STATUSKONTROLL                                           
254200     .                                                                    
254300     EJECT                                                                
254400 IMS-GU-WDP501 SECTION.                                                   
254500                                                                          
254600     STRING 'WDP501  (WDP501KY =' W-WDP501KY-X ')'                        
254700            DELIMITED BY SIZE INTO SSA1                                   
254800     MOVE '  GE' TO GODK-STATUSKODER                                      
254900     CALL CBLTDLI USING GU  WDP5-PCB DLI-IO-WDP501 SSA1                   
255000     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
255100     PERFORM IMS-STATUSKONTROLL                                           
255200     .                                                                    
255300     EJECT                                                                
255400                                                                          
255500 IMS-GNP-WDP512 SECTION.                                                  
255600     STRING 'WDP512     '                                                 
255700            DELIMITED BY SIZE INTO SSA1                                   
255800     MOVE '  GE' TO GODK-STATUSKODER                                      
255900     CALL CBLTDLI USING GNP WDP5-PCB DLI-IO-WDP512 SSA1                   
256000     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
256100     PERFORM IMS-STATUSKONTROLL                                           
256200     .                                                                    
256300     EJECT                                                                
256400                                                                          
256500 IMS-STATUSKONTROLL SECTION.                                              
256600     SET STATUS-IX TO 1                                                   
256700     SEARCH GODK-STATUS AT END CALL FELLOG                                
256800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
256900     END-SEARCH                                                           
257000     .                                                                    
257100     EJECT                                                                
257200 DB2-DCL-OPN-CRS-BYPRO SECTION.                                           
257300* OBS!!! DECLARE GER INGEN SQLCODE I RETUR                                
257400     EXEC SQL DECLARE BYPRO-CRS CURSOR FOR                                
257500              SELECT IDARTNR                                              
257600              FROM BYPRO                                                  
257700              WHERE IDARTNR >= :W-IDPRODNR                                
257800              AND IDARTNR_BYT = :W-IDARTNR-BYT                            
257900              ORDER BY IDARTNR                                            
258000     END-EXEC                                                             
258100     MOVE 000               TO GODK-SQLCODESKODER                         
258200     EXEC SQL OPEN BYPRO-CRS END-EXEC                                     
258300     MOVE SQLCODE           TO SQLCODE-WS                                 
258400     PERFORM DB2-STATUSKONTROLL                                           
258500     .                                                                    
258600     SKIP3                                                                
258700 DB2-FETCH-BYPRO SECTION.                                                 
258800     MOVE 000100            TO GODK-SQLCODESKODER                         
258900     EXEC SQL FETCH BYPRO-CRS INTO                                        
259000            :BYPRO-IDARTNR                                                
259100     END-EXEC                                                             
259200     MOVE SQLCODE           TO SQLCODE-WS                                 
259300     PERFORM DB2-STATUSKONTROLL                                           
259400     .                                                                    
259500     SKIP3                                                                
259600 DB2-CLOSE-BYPRO-CRS SECTION.                                             
259700     SKIP2                                                                
259800     EXEC SQL CLOSE BYPRO-CRS END-EXEC                                    
259900     .                                                                    
260000     SKIP3                                                                
260100 DB2-STATUSKONTROLL SECTION.                                              
260200     SKIP2                                                                
260300     SET SQLCODE-IX          TO 1                                         
260400     SEARCH GODK-SQLCODE                                                  
260500       AT END                                                             
260600         CALL FELLOG                                                      
260700        WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
260800           CONTINUE                                                       
260900     END-SEARCH                                                           
261000     .                                                                    
261100*    -COPY WY2000P1                                                       
