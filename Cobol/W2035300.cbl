000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2035300.                                                
000300 AUTHOR.         PER-ANDERS HELGEGREN.                                    
000400 DATE-WRITTEN.   96/11/21.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        ON ORDER  USA/CANADA                                             
000900*                                                                         
001000*        PROGRAMMET LÄSER      WLINLC (WDL6)       + ETA                  
001100*        PROGRAMMET LÄSER      WDE4                                       
001200*        PRO35AMMET LÄSER      WDE4C                                      
001300*        PROGRAMMET LÄSER      WLORQF (WDQ4)                              
001400*        PROGRAMMET LÄSER      WLORDP (WDA5)                              
001500*        PROGRAMMET LÄSER      WLORDL (WDE3)                              
001600*        PROGRAMMET LÄSER      WLARTC (WDK6)    ETA                       
001700*        PROGRAMMET LÄSER              WDK7     ETA                       
001800*        PROGRAMMET LÄSER      WLLEVA (WDF1)    ETA                       
001900*        PROGRAMMET LÄSER      WLGMTB (WDB3)    IDFRAKT                   
002000*        PROGRAMMET LÄSER      WLBENA (WDD3B)   BENÄMNING                 
002100*        PROGRAMMET LÄSER      WDB6             DC-REGISTER               
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: W2T353                                              
002500*        MID:         W2I35301                                            
002600*                                                                         
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W2O35301                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700*    -- CHECKED BY WY2000                                                 
003800                                                                          
003900 77  IDPGM                       PIC X(08)   VALUE 'W2035300'.            
004000                                                                          
004100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004300                                                                          
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004600                                                                          
004700*01  -COPY WWDCKONS                                                       
004800                                                                          
004900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005000                                                                          
005100                                                                          
005200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005300     88  NYCKLAR-OK                          VALUE 'J'.                   
005400     88  NYCKLAR-FEL                         VALUE 'N'.                   
005500 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005600     88  INDATA-OK                           VALUE 'J'.                   
005700     88  INDATA-FEL                          VALUE 'N'.                   
005800 77  FAKTURERAD-SW               PIC X       VALUE 'J'.                   
005900     88  FAKTURERAD                          VALUE 'J'.                   
006000     88  EJ-FAKTURERAD                       VALUE 'N'.                   
006100 01  WS-FLYGORDER                PIC X       VALUE 'N'.                   
006200*01  WS-BAATORDER                PIC X       VALUE 'N'.                   
006300                                                                          
006400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006500     88  EGEN-MID                            VALUE '2353'.                
006600     88  GODK-MID                            VALUE '2351' '2352'          
006700                                                   '2353' '2354'          
006800                                                   '2355' '2356'          
006900                                                   '2357' '2358'          
007000                                                   '2359'.                
007100     88  HELP-MID                            VALUE '0551'.                
007110     88  2392-MID                            VALUE '2392'.                
007200                                                                          
007300 01  ARBETSAREOR.                                                         
007400     03  MIN-IDDC                PIC X(2).                                
007500     03  MAX-IDDC                PIC X(2).                                
007600     03  W-KDFRAKT               PIC S9(3) COMP-3.                        
007700     03  W-KDORDKL               PIC  9(1).                               
007800     03  W-IDDISTR               PIC S9(5) COMP-3.                        
007900     03  W-IDDISTR-X             PIC  9(5).                               
008000     03  W-IDKUNDNR              PIC S9(7) COMP-3.                        
008100     03  W-IDKUNDNR-NUM          PIC 9(6) VALUE ZERO.                     
008200     03  W-IDKUNDNR-ALFA         PIC X(6) VALUE SPACE.                    
008300     03  W-TEST-IDKUNDNR         PIC 9(2) VALUE ZERO.                     
008400     03  W-IDLEVNR               PIC X(5) VALUE SPACE.                    
008500     03  W-IDDC-SPAR             PIC X(2) VALUE SPACE.                    
008600     03  W-IDDC-REC              PIC X(2) VALUE SPACE.                    
008700     03  W-IDDC-SEND             PIC X(2) VALUE SPACE.                    
008800     03  W-IDDC-SEND-9 REDEFINES W-IDDC-SEND     PIC 9(2).                
008900     03  DAGENS-DATUM            PIC 9(6) VALUE ZERO.                     
009000     03  W-IDARTNR-NUM           PIC 9(9) VALUE ZERO.                     
009100     03  W-TIAAMMDD-ANROP        PIC 9(6) VALUE ZERO.                     
009200     03  W-TIAAMMDD-SVAR         PIC 9(6) VALUE ZERO.                     
009300     03  W-KDORDSTA              PIC 9    VALUE ZERO.                     
009400     03  W-IDPRODNR-9            PIC 9(7) VALUE ZERO.                     
009500     03  W-IDPURAD-9             PIC 9(1) VALUE ZERO.                     
009600                                                                          
009700 01  WS-MSGI-SPAR-AREA.                                                   
009800     03  WS-MSGI-PGM             PIC X(6) VALUE SPACE.                    
009900     03  WS-MSGI-RADNR           PIC 9(3) VALUE ZERO.                     
010000     03  FILLER                  PIC X(191) VALUE SPACE.                  
010100                                                                          
010200 01  KONV-TID                    PIC 9(16).                               
010300 01  FILLER REDEFINES KONV-TID.                                           
010400     03  KONV-TISEKEL            PIC 9(2).                                
010500     03  KONV-TIAAMMDD           PIC 9(6).                                
010600     03  KONV-TIHHMI             PIC 9(4).                                
010700     03  FILLER                  PIC X(4).                                
010800                                                                          
010900 01  W-IDUSER-MSGI.                                                       
011000     03  FILLER                  PIC X(5) VALUE 'WIDDC'.                  
011100     03  W-IDUSER-IDDC           PIC X(2) VALUE SPACE.                    
011200     03  FILLER                  PIC X(1) VALUE SPACE.                    
011300     EJECT                                                                
011400*    --- COPYTEXT FÖR ATT KUNNA UR DISTR FÅ MOTTAGANDE IDDC               
011500*01 -COPY WWDIST35                                                        
011600     EJECT                                                                
011700*      --- VALID IDDC CODES                                               
011800*                                                                         
011900*01    -COPY WWDC99 -PRE SEND-                                            
012000*01    -COPY WWDC99 -PRE REC-                                             
012100*01    -COPY WWDC99                                                       
012200       EJECT                                                              
012300*      --- COPYTEXT FOR SNEDING AND RECIEVING DC WITH DISTR               
012400*01    -COPY WWDC03                                                       
012500       EJECT                                                              
012600                                                                          
012700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
012800 01  GENERELLA-SUBPROGRAM.                                                
012900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
013000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
013100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013300     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR'.             
013400     03  W218ETA                 PIC X(8)    VALUE 'W218ETA'.             
013500     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
013600     EJECT                                                                
013700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
013800*01 -COPY WMEDAREA                                                        
013900     SKIP3                                                                
014000 01  MESSAGE-CODES.                                                       
014100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
014200     03  ERR-NOT-REFILL-PART     PIC X(3)    VALUE '957'.                 
014300     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
014400 01  MESSAGE-TEXT.                                                        
014500     03  MORE-LINES              PIC X(30)                                
014600                         VALUE 'MORE LINES EXIST, PRESS PF8'.             
014700     EJECT                                                                
014800*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
014900*                                                                         
015000 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
015100     SKIP2                                                                
015200*01 -COPY WMSGINIT                                                        
015300     EJECT                                                                
015400*    --- PARAMETRAR TILL SUBPROGRAM WORKDAY                               
015500*                                                                         
015600 01  FILLER                      PIC X(16)   VALUE 'WORKDAY'.             
015700     SKIP2                                                                
015800*01  -COPY WORKAREA                                                       
015900     EJECT                                                                
016000*    --- PARAMETRAR TILL SUBPROGRAM W218ETA                               
016100*                                                                         
016200 01  FILLER                      PIC X(16)   VALUE 'W218LETA'.            
016300     SKIP2                                                                
016400*01 -COPY W218LETA   -PRE LINK-                                           
016500     EJECT                                                                
016600*    --- PARAMETRAR TILL SUBPROGRAM WINTSOR                               
016700 01  TABENTRY-PARM.                                                       
016800     03  STEGLANGD               PIC S9(9) COMP  VALUE 88.                
016900     03  ANTAL                   PIC S9(9) COMP.                          
017000     03  NYCKELLANGD             PIC S9(9) COMP  VALUE 10.                
017100                                                                          
017200 01  IX-RAD                      PIC S9(3) COMP-3 VALUE 1.                
017300 01  IX-RAD-TAB                  PIC S9(3) COMP-3 VALUE 1.                
017400                                                                          
017500 01  TAB-MAX                     PIC S9(9) COMP VALUE 400.                
017600     EJECT                                                                
017700*    --- TABELL SOM SORTERAS AV WINTSOR                                   
017800 01  TABELL.                                                              
017900     03  TAB-POST  OCCURS 400.                                            
018000       04  TAB-RAD.                                                       
018100         05  TAB-IDDC            PIC X(2).                                
018200         05  FILLER              PIC X(2).                                
018300         05  TAB-IDLEVNR         PIC X(5).                                
018400         05  FILLER              PIC X(4).                                
018500         05  TAB-TIREGDAT        PIC 9(6) BLANK WHEN ZERO.                
018600         05  FILLER              PIC X(1).                                
018700         05  TAB-KDORDKL         PIC Z(1).                                
018800         05  FILLER              PIC X(1).                                
018900         05  TAB-IDKUNDRF        PIC X(10).                               
019000         05  FILLER              PIC X(1).                                
019100         05  TAB-KVBEART         PIC Z(7).                                
019200         05  FILLER              PIC X(1).                                
019300         05  TAB-KVRO            PIC Z(7).                                
019400         05  FILLER              PIC X(1).                                
019500         05  TAB-KVAVIS          PIC Z(7).                                
019600         05  FILLER              PIC X(1).                                
019700         05  TAB-IDFAKT          PIC Z(7).                                
019800         05  FILLER              PIC X(1).                                
019900         05  TAB-TIIDFAKT        PIC 9(6) BLANK WHEN ZERO.                
020000         05  FILLER              PIC X(1).                                
020100         05  TAB-TIBERANK        PIC 9(6) BLANK WHEN ZERO.                
020200       04  TAB-SORT.                                                      
020300         05  TAB-IDDC-SORT       PIC X(2).                                
020400         05  TAB-TISEKEL-SORT    PIC Z(2).                                
020500         05  TAB-TIBERANK-SORT   PIC Z(6).                                
020600     EJECT                                                                
020700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
020800*                                                                         
020900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
021000     SKIP3                                                                
021100*01  MID -COPY W2I35301                                                   
021200     EJECT                                                                
021300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
021400     SKIP3                                                                
021500*01  -COPY WMSGAREA                                                       
021600     EJECT                                                                
021700     03  MOD REDEFINES MSG-AREA.                                          
021800*      05  -COPY W2O35301                                                 
021900     EJECT                                                                
022000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
022100     SKIP3                                                                
022200*01  -COPY WMFSAREA                                                       
022300     EJECT                                                                
022400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
022500*                                                                         
022600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
022700     SKIP3                                                                
022800 01  NYCKLAR-TILL-DLI.                                                    
022900     03  W-IDARTNR-X.                                                     
023000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
023100     03  W-IDDC-X.                                                        
023200         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
023300     03 W-IDLEVNR-DC-X.                                                   
023400         05 W-IDLEVNR-DC         PIC X(5)    VALUE SPACE.                 
023500     03  W-DAINLEV-X.                                                     
023600         05  W-DAINLEV           PIC 9(16)   VALUE ZERO.                  
023700     03  W-WDL612KY-X.                                                    
023800         05  W-DAREGDAT          PIC  9(8)   VALUE ZERO.                  
023900         05  W-TIREGTID          PIC S9(7)   VALUE ZERO COMP-3.           
024000     03  W-WDE4KEY-X.                                                     
024100         05  W-IDDISTR-E4        PIC S9(5)   VALUE ZERO COMP-3.           
024200         05  W-IDKUNDNR-E4       PIC S9(7)   VALUE ZERO COMP-3.           
024300         05  W-IDKUNDRF-E4       PIC X(10)   VALUE SPACE.                 
024400         05  W-IDPRODNR-E4       PIC S9(7)   VALUE ZERO COMP-3.           
024500         05  W-IDPLKLST-E4       PIC S9(3)   VALUE ZERO COMP-3.           
024600     03  W-IDPURAD-X.                                                     
024700         05  W-IDPURAD           PIC S9(5)   VALUE ZERO COMP-3.           
024800     03  W-WDE4KEY21-X.                                                   
024900         05  W-IDPRODNR-21       PIC S9(7)   VALUE ZERO COMP-3.           
025000         05  W-IDKOLLI-21        PIC S9(5)   VALUE ZERO COMP-3.           
025100     03  W-WDE4C1KY-MIN.                                                  
025200         05  W-IDARTNR-E4C-MIN   PIC S9(9)   VALUE ZERO COMP-3.           
025300         05  W-IDPRODNR-E4C-MIN  PIC S9(7)   VALUE ZERO COMP-3.           
025400         05  W-IDPURAD-E4C-MIN   PIC S9(5)   VALUE ZERO COMP-3.           
025500     03  W-WDE4C1KY-MAX.                                                  
025600         05  W-IDARTNR-E4C-MAX   PIC S9(9)   VALUE ZERO COMP-3.           
025700         05  W-IDPRODNR-E4C-MAX  PIC S9(7)   VALUE 9999999 COMP-3.        
025800         05  W-IDPURAD-E4C-MAX   PIC S9(5)   VALUE 99999 COMP-3.          
025900     03  W-WDQ401KY-X.                                                    
026000         05  W-IDORDER-Q4        PIC S9(7)   VALUE ZERO COMP-3.           
026100         05  W-IDDC-Q4           PIC  X(2)   VALUE SPACE.                 
026200         05  W-ADLAGOMR-Q4       PIC S9(3)   VALUE ZERO COMP-3.           
026300         05  W-ADGANG-Q4         PIC S9(3)   VALUE ZERO COMP-3.           
026400         05  W-ADPLATS-Q4        PIC S9(5)   VALUE ZERO COMP-3.           
026500         05  W-IDARTNR-Q4        PIC S9(9)   VALUE ZERO COMP-3.           
026600         05  W-IDLOPNR-Q4        PIC S9(3)   VALUE ZERO COMP-3.           
026700     03  W-WDQ4BSEQ-MIN.                                                  
026800         05  W-IDARTNR-Q4B-MIN   PIC S9(9)   VALUE ZERO COMP-3.           
026900         05  W-IDLOPNR-Q4B-MIN   PIC S9(3)   VALUE ZERO COMP-3.           
027000         05  W-IDDISTR-Q4B-MIN   PIC S9(5)   VALUE ZERO COMP-3.           
027100         05  W-IDKUNDNR-Q4B-MIN  PIC S9(7)   VALUE ZERO COMP-3.           
027200         05  W-IDKUNDRF-Q4B-MIN  PIC X(10)   VALUE SPACE.                 
027300     03  W-WDQ4BSEQ-MAX.                                                  
027400         05  W-IDARTNR-Q4B-MAX   PIC S9(9)   VALUE ZERO COMP-3.           
027500         05  W-IDLOPNR-Q4B-MAX   PIC S9(3)   VALUE 999  COMP-3.           
027600         05  W-IDDISTR-Q4B-MAX   PIC S9(5)   VALUE 99999   COMP-3.        
027700         05  W-IDKUNDNR-Q4B-MAX  PIC S9(7)   VALUE 9999999 COMP-3.        
027800         05  W-IDKUNDRF-Q4B-MAX  PIC X(10)   VALUE HIGH-VALUE.            
027900     03  W-WDA501KY-X.                                                    
028000         05  W-IDDISTR-A5        PIC S9(5)   VALUE ZERO COMP-3.           
028100         05  W-IDKUNDNR-A5       PIC S9(7)   VALUE ZERO COMP-3.           
028200         05  W-IDKUNDRF-A5       PIC X(10)   VALUE SPACE.                 
028300         05  W-IDARTNR-A5        PIC S9(9)   VALUE ZERO COMP-3.           
028400         05  W-IDLOPNR-A5        PIC S9(3)   VALUE ZERO COMP-3.           
028500     03  W-WDA5ASEQ-MIN.                                                  
028600         05  W-IDARTNR-A5A-MIN   PIC S9(9)   VALUE ZERO COMP-3.           
028700         05  W-IDDC-A5A-MIN      PIC  X(2)   VALUE '00'.                  
028800         05  W-KDRAPRIO-A5A-MIN  PIC S9(3)   VALUE ZERO COMP-3.           
028900     03  W-WDA5ASEQ-MAX.                                                  
029000         05  W-IDARTNR-A5A-MAX   PIC S9(9)   VALUE ZERO COMP-3.           
029100         05  W-IDDC-A5A-MAX      PIC  X(2)   VALUE '99'.                  
029200         05  W-KDRAPRIO-A5A-MAX  PIC S9(3)   VALUE 999  COMP-3.           
029300     03  W-WDE301KY-X.                                                    
029400         05  W-IDDC-E3           PIC  X(2)   VALUE SPACE.                 
029500         05  W-IDPERSON-BUY-E3   PIC S9(3)   VALUE ZERO COMP-3.           
029600         05  W-KDREFTYP-E3       PIC  X(1)   VALUE SPACE.                 
029700         05  W-IDARTNR-E3        PIC S9(9)   VALUE ZERO COMP-3.           
029800         05  W-IDDISTR-E3        PIC S9(5)   VALUE ZERO COMP-3.           
029900     03  W-WDB301KY-X.                                                    
030000         05  W-IDDC-B3           PIC  X(2)   VALUE SPACE.                 
030100         05  W-IDDISTR-B3        PIC S9(5)   VALUE ZERO COMP-3.           
030200         05  W-IDKUNDNR-B3       PIC S9(7)   VALUE ZERO COMP-3.           
030300     03  W-WDB301KY-DEF-X.                                                
030400         05  W-IDDC-B3-DEF       PIC  X(2)   VALUE SPACE.                 
030500         05  W-IDDISTR-B3-DEF    PIC S9(5)   VALUE ZERO COMP-3.           
030600         05  W-IDKUNDNR-B3-DEF   PIC S9(7)  VALUE +9999999 COMP-3.        
030700     03  W-IDSKYLT-X.                                                     
030800         05  W-IDSKYLT           PIC  X(3)   VALUE 'USA'.                 
030900     03  W-IDPRODNR-X.                                                    
031000         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
031100     03  W-IDKOLLI-X.                                                     
031200         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
031300     03  W-IDDC-B6-X.                                                     
031400         05 W-IDDC-B6                  PIC X(2).                          
031500     EJECT                                                                
031600*    --- STATUS-KOD FRÅN IMS                                              
031700 01  STATUS-WS                   PIC XX.                                  
031800     88  SEGMENT-FINNS                       VALUE '  '.                  
031900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
032000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
032100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
032200     SKIP2                                                                
032300 01  GODK-STATUSKODER.                                                    
032400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
032500     SKIP3                                                                
032600 01  SSA1                        PIC X(96).                               
032700 01  SSA2                        PIC X(96).                               
032800     EJECT                                                                
032900*    --- IMS FUNKTIONSKODER                                               
033000*01  -COPY W0003                                                          
033100     EJECT                                                                
033200*    ---  DLI INPUT-OUTPUT AREA                                           
033300                                                                          
033400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLINLC01'.                    
033500 01  DLI-IO-WLINLC01.                                                     
033600*    03  -COPY WDL601                                                     
033700     EJECT                                                                
033800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLINLC11'.                    
033900 01  DLI-IO-WLINLC11.                                                     
034000*    03  -COPY WDL611                                                     
034100     EJECT                                                                
034200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLINLC12'.                    
034300 01  DLI-IO-WLINLC12.                                                     
034400*    03  -COPY WDL612                                                     
034500     EJECT                                                                
034600 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDE401  '.                    
034700 01  DLI-IO-WDE401.                                                       
034800*    03  -COPY WDE401                                                     
034900     EJECT                                                                
035000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDE411  '.                    
035100 01  DLI-IO-WDE411.                                                       
035200*    03  -COPY WDE411  -PRE C                                             
035300     EJECT                                                                
035400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDE421  '.                    
035500 01  DLI-IO-WDE421.                                                       
035600*    03  -COPY WDE421                                                     
035700     EJECT                                                                
035800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDE4C1'.                      
035900 01  DLI-IO-WDE4C1.                                                       
036000*    03  -COPY WDE4C1                                                     
036100     EJECT                                                                
036200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLORQF01'.                    
036300 01  DLI-IO-WLORQF01.                                                     
036400*    03  -COPY WDQ401                                                     
036500     EJECT                                                                
036600 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLORQH01'.                    
036700 01  DLI-IO-WLORQH01.                                                     
036800*    03  -COPY WDQ4B1                                                     
036900     EJECT                                                                
037000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLORDP01'.                    
037100 01  DLI-IO-WLORDP01.                                                     
037200*    03  -COPY WDA501                                                     
037300     EJECT                                                                
037400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLORDL01'.                    
037500 01  DLI-IO-WLORDL01.                                                     
037600*    03  -COPY WDE301                                                     
037700     EJECT                                                                
037800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLGMTB01'.                    
037900 01  DLI-IO-WLGMTB01.                                                     
038000*    03  -COPY WDB301                                                     
038100     EJECT                                                                
038200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLBENA11'.                    
038300 01  DLI-IO-WLBENA11.                                                     
038400*    03  -COPY WDD311                                                     
038500     EJECT                                                                
038600 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDE601'.                      
038700 01  DLI-IO-WDE601.                                                       
038800*    03  -COPY WDE601                                                     
038900     EJECT                                                                
039000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDE601'.                      
039100 01  DLI-IO-WDE611.                                                       
039200*    03  -COPY WDE611                                                     
039300     EJECT                                                                
039400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
039500 01  DLI-IO-WDK601.                                                       
039600*    03  -COPY WDK601                                                     
039700     EJECT                                                                
039800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
039900 01  DLI-IO-WDK611.                                                       
040000*    03  -COPY WDK611                                                     
040100                                                                          
040200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK701'.                      
040300 01  DLI-IO-WDK701.                                                       
040400*    03  -COPY WDK701                                                     
040500     EJECT                                                                
040600 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711'.                      
040700 01  DLI-IO-WDK711.                                                       
040800*    03  -COPY WDK711                                                     
040900                                                                          
041000 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
041100 01   DLI-IO-AREA-B601.                                                   
041200*     03  -COPY WDB601                                                    
041300     EJECT                                                                
041400 LINKAGE SECTION.                                                         
041500                                                                          
041600*01  -COPY W0009   -PRE MSG-                                              
041700     EJECT                                                                
041800*01  -COPY W0008   -PRE USEA-                                             
041900     05  FILLER                  PIC X.                                   
042000     EJECT                                                                
042100*01  -COPY W0008  -PRE INLC-                                              
042200     05  FILLER                  PIC X.                                   
042300     EJECT                                                                
042400*01  -COPY W0008  -PRE WDE4-                                              
042500     05  FILLER                  PIC X.                                   
042600     EJECT                                                                
042700*01  -COPY W0008  -PRE WDE4C-                                             
042800     05  FILLER                  PIC X.                                   
042900     EJECT                                                                
043000*01  -COPY W0008  -PRE ORQF-                                              
043100     05  FILLER                  PIC X.                                   
043200     EJECT                                                                
043300*01  -COPY W0008  -PRE ORDP-                                              
043400     05  FILLER                  PIC X.                                   
043500     EJECT                                                                
043600*01  -COPY W0008  -PRE ORDL-                                              
043700     05  FILLER                  PIC X.                                   
043800     EJECT                                                                
043900*01  -COPY W0008  -PRE ARTC-                                              
044000     05  FILLER                  PIC X.                                   
044100     EJECT                                                                
044200*01  -COPY W0008  -PRE WDK7-                                              
044300     05  FILLER                  PIC X.                                   
044400     EJECT                                                                
044500*01  -COPY W0008  -PRE LEVA-                                              
044600     05  FILLER                  PIC X.                                   
044700     EJECT                                                                
044800*01  -COPY W0008  -PRE GMTB-                                              
044900     05  FILLER                  PIC X.                                   
045000     EJECT                                                                
045100*01  -COPY W0008  -PRE BENA-                                              
045200     05  FILLER                  PIC X.                                   
045300     EJECT                                                                
045400*01  -COPY W0008  -PRE WDE6-                                              
045500     05  FILLER                  PIC X.                                   
045600     EJECT                                                                
045700*01  -COPY W0008  -PRE WDB6-                                              
045800     05  FILLER                  PIC X.                                   
045900     EJECT                                                                
046000*01  -COPY W0008  -PRE WDD9-                                              
046100     05  FILLER                  PIC X.                                   
046200     EJECT                                                                
046300*01  -COPY W0008  -PRE WDK6-                                              
046400     05  FILLER                  PIC X.                                   
046500     EJECT                                                                
046600 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB INLC-PCB WDE4-PCB             
046700        WDE4C-PCB ORQF-PCB ORDP-PCB ORDL-PCB                              
046800        ARTC-PCB WDK7-PCB LEVA-PCB GMTB-PCB BENA-PCB WDE6-PCB             
046900        WDB6-PCB WDD9-PCB WDK6-PCB.                                       
047000                                                                          
047100 MAIN SECTION.                                                            
047200     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB INLC-PCB WDE4-PCB             
047300        WDE4C-PCB ORQF-PCB ORDP-PCB ORDL-PCB                              
047400        ARTC-PCB WDK7-PCB LEVA-PCB GMTB-PCB BENA-PCB WDE6-PCB             
047500        WDB6-PCB WDD9-PCB WDK6-PCB.                                       
047600                                                                          
047700                                                                          
047800     PERFORM IMS-GET-MSG                                                  
047900     IF SEGMENT-FINNS                                                     
048000       PERFORM A-INIT                                                     
048100       PERFORM B-KOLLA-NYCKLAR                                            
048200                                                                          
048300       IF NYCKLAR-OK                                                      
048400         PERFORM C-LAES-WDL6                                              
048500         PERFORM D-LAES-WDE4                                              
048600         PERFORM E-LAES-WDQ4                                              
048700         PERFORM F-LAES-WDA5                                              
048800         PERFORM G-LAES-WDE3                                              
048900         PERFORM H-SORTERA-INFO                                           
049000         PERFORM I-VISA-INFO                                              
049100       END-IF                                                             
049200                                                                          
049300       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O35301 + 4                      
049400       PERFORM IMS-INSERT-MSG                                             
049500     END-IF                                                               
049600                                                                          
049700     MOVE ZERO TO RETURN-CODE                                             
049800     GOBACK                                                               
049900     .                                                                    
050000     EJECT                                                                
050100                                                                          
050200 A-INIT SECTION.                                                          
050300                                                                          
050310     IF MSG-IDTRANS-1 = '2391' OR '2392' OR '2393'                        
050320       MOVE WC-CDC-SE   TO MIN-IDDC                                       
050330                           MAX-IDDC                                       
050331                           MID-IDDC-IN                                    
050340     ELSE                                                                 
050400       MOVE WC-NDC-US-RU TO MIN-IDDC                                      
050500       MOVE WC-NDC-AE  TO MAX-IDDC                                        
050510     END-IF                                                               
050600                                                                          
050700     IF MSG-DUBBLA-TRANSKODER                                             
050800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I35301                 
050900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
051000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
051100     ELSE                                                                 
051200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I35301                  
051300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
051400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
051500     END-IF                                                               
051600                                                                          
051700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
051800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
051900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
052000                                                                          
052100     MOVE LOW-VALUE TO MSG-AREA                                           
052200     MOVE 'W2O35301' TO MFS-IDMOD                                         
052300     MOVE '2353' TO MOD-IDTRANS                                           
052400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
052500                                                                          
052600     IF EGEN-MID OR HELP-MID                                              
052700       CONTINUE                                                           
052800     ELSE                                                                 
052900       MOVE SPACE TO MFS-KDTRTYP                                          
053000       MOVE '7' TO MFS-IDPFK                                              
053100     END-IF                                                               
053200                                                                          
053300     ACCEPT DAGENS-DATUM FROM DATE                                        
053400                                                                          
053500     MOVE SPACE TO TABELL                                                 
053600     .                                                                    
053700     EJECT                                                                
053800                                                                          
053900 B-KOLLA-NYCKLAR SECTION.                                                 
054000                                                                          
054100     MOVE ALL '+'           TO MSGI-WMSGINIT                              
054200     MOVE '001'             TO MSGI-KDCALL                                
054300     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
054400     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
054500     MOVE '2353'            TO MSGI-IDTRANS                               
054600     IF GODK-MID                                                          
054700        MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                               
054800     END-IF                                                               
054900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
055000                                                                          
055100     MOVE MSGI-SPAR-AREA    TO WS-MSGI-SPAR-AREA                          
055200     MOVE JA TO NYCKLAR-SW                                                
055300                                                                          
055400*    -- KONTROLL AV IDARTNR                                               
055500     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
055600                                                                          
055700***  OM EJ PF8       ?                                                    
055800     IF MID-IDARTNR-IN NOT = ALL '+'                                      
055900       MOVE '7'         TO MFS-IDPFK                                      
056000       MOVE SPACE       TO MFS-KDTRTYP                                    
056100     END-IF                                                               
056200     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
056300     IF MSGI-IDARTNR NUMERIC                                              
056400       MOVE MSGI-IDARTNR  TO W-IDARTNR-NUM                                
056500       MOVE W-IDARTNR-NUM TO W-IDARTNR                                    
056600     ELSE                                                                 
056700       MOVE NEJ TO NYCKLAR-SW                                             
056800     END-IF                                                               
056900                                                                          
057000*    -- KONTROLL AV IDDC                                                  
057100     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
057200                                                                          
057300***  OM EJ PF8  ?                                                         
057310     IF W-IDTRANS = '2391' OR '2392' OR '2393'                            
057320       MOVE WC-CDC-SE       TO MID-IDDC-IN                                
057330     END-IF                                                               
057400     IF MID-IDDC-IN  NOT = ALL '+'                                        
057500       MOVE '7'         TO MFS-IDPFK                                      
057600       MOVE SPACE       TO MFS-KDTRTYP                                    
057700     END-IF                                                               
057800     IF MID-IDDC-IN = '++'                                                
057900        MOVE SPACE      TO MID-IDDC-IN                                    
058000        IF MID-IDDC-UT NOT = DCS-IDDC                                     
058100           MOVE MID-IDDC-UT TO W-IDDC-B6                                  
058200           PERFORM IMS-GU-WDB601                                          
058300        END-IF                                                            
058400        IF DCS-KDDC NOT = SPACE AND NOT DCS-DDC                           
058500           MOVE MID-IDDC-UT TO MID-IDDC-IN                                
058600        END-IF                                                            
058700     END-IF                                                               
058800     IF MID-IDDC-IN NOT = DCS-IDDC                                        
058900        MOVE MID-IDDC-IN TO W-IDDC-B6                                     
059000        PERFORM IMS-GU-WDB601                                             
059100     END-IF                                                               
059200     IF EGEN-MID                                                          
059300        IF MID-IDDC-IN = SPACE OR DCS-NDC OR DCS-CDC                      
059400          CONTINUE                                                        
059500        ELSE                                                              
059600          MOVE NEJ TO NYCKLAR-SW                                          
059700        END-IF                                                            
059800     ELSE                                                                 
059900        IF MID-IDDC-IN = SPACE OR DCS-NDC OR DCS-CDC                      
060000          CONTINUE                                                        
060100        ELSE                                                              
060200          MOVE SPACE     TO MID-IDDC-IN                                   
060300        END-IF                                                            
060400     END-IF                                                               
060500                                                                          
060600     IF MID-IDDC-IN = SPACE                                               
060700        MOVE WC-NDC-US-RU  TO MIN-IDDC                                    
060800        MOVE WC-NDC-AE     TO MAX-IDDC                                    
060900     ELSE                                                                 
061000        MOVE MID-IDDC-IN TO MIN-IDDC MAX-IDDC                             
061100     END-IF                                                               
061200                                                                          
061300     IF GODK-MID OR NYCKLAR-OK                                            
061400       MOVE MSGI-IDARTNR        TO MOD-IDARTNR-UT                         
061500       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
061600       MOVE MID-IDDC-IN         TO MOD-IDDC-UT                            
061700     ELSE                                                                 
061800       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT MOD-IDDC-UT                 
061900     END-IF                                                               
062000                                                                          
062100     IF NYCKLAR-FEL                                                       
062200       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
062300       CALL WMEDKONV USING MED-WMEDAREA                                   
062400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
062500       PERFORM MFS-RENSA-FAELT-UT                                         
062600       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT MOD-IDDC-UT                 
062700     END-IF                                                               
062800     .                                                                    
062900     EJECT                                                                
063000                                                                          
063100 C-LAES-WDL6      SECTION.                                                
063200                                                                          
063300     MOVE SPACES       TO W-IDDC-SPAR                                     
063400                                                                          
063500     PERFORM IMS-GET-WDL6-INLC01                                          
063600     IF SEGMENT-FINNS                                                     
063700        PERFORM IMS-GET-WDL6-INLC11                                       
063800        PERFORM UNTIL SEGMENT-SAKNAS                                      
063900           IF INL-IDDC >= MIN-IDDC AND                                    
064000              INL-IDDC <= MAX-IDDC                                        
064100              IF INL-IDPTYP = '310' OR 'R31' OR 'R30'                     
064200                 PERFORM CA-FYLL-I-RAD-FRAN-WDL611                        
064300              END-IF                                                      
064400           END-IF                                                         
064500           PERFORM IMS-GET-WDL6-INLC11                                    
064600        END-PERFORM                                                       
064700                                                                          
064800        PERFORM IMS-GET-WDL6-INLC12                                       
064900        PERFORM UNTIL SEGMENT-SAKNAS                                      
065000           IF ORD-IDLOPNRM = ZERO                                         
065100              IF ORD-IDDC >= MIN-IDDC AND                                 
065200                 ORD-IDDC <= MAX-IDDC                                     
065300                 PERFORM CB-FYLL-I-RAD-FRAN-WDL612                        
065400              END-IF                                                      
065500           END-IF                                                         
065600           PERFORM IMS-GET-WDL6-INLC12                                    
065700        END-PERFORM                                                       
065800     END-IF                                                               
065900     .                                                                    
066000     EJECT                                                                
066100                                                                          
066200 CA-FYLL-I-RAD-FRAN-WDL611 SECTION.                                       
066300                                                                          
066400     IF INL-IDDC-LEV = SPACE                                              
066500       MOVE INL-IDLEVNR    TO TAB-IDLEVNR   (IX-RAD)                      
066600     ELSE                                                                 
066700       MOVE INL-IDDC-LEV   TO W-IDDC-B6                                   
066800       PERFORM IMS-GU-WDB601                                              
066900       IF SEGMENT-FINNS                                                   
067000         MOVE DCS-IDLEVNR-DC  TO TAB-IDLEVNR   (IX-RAD)                   
067100       ELSE                                                               
067200         CALL FELLOG                                                      
067300       END-IF                                                             
067400     END-IF                                                               
067500     MOVE INL-IDDC         TO TAB-IDDC      (IX-RAD)                      
067600                              TAB-IDDC-SORT (IX-RAD)                      
067700     MOVE ZERO             TO TAB-TIREGDAT  (IX-RAD)                      
067800     MOVE INL-IDKUNDRF     TO TAB-IDKUNDRF  (IX-RAD)                      
067900     MOVE ZERO             TO TAB-KVBEART   (IX-RAD)                      
068000     MOVE ZERO             TO TAB-KVRO      (IX-RAD)                      
068100     MOVE INL-KVAVIS       TO TAB-KVAVIS    (IX-RAD)                      
068200     MOVE INL-IDFAKT       TO TAB-IDFAKT    (IX-RAD)                      
068300     COMPUTE KONV-TID = 9999999999999999 - INL-DAINLEV                    
068400                                                                          
068500     MOVE '011'            TO MSGI-KDCALL                                 
068600     MOVE INL-IDDC         TO W-IDUSER-IDDC                               
068700     MOVE W-IDUSER-MSGI    TO MSGI-IDUSER                                 
068800     MOVE KONV-TIAAMMDD    TO MSGI-TILOKDAT                               
068900     MOVE KONV-TIHHMI      TO MSGI-TILOKTID                               
069000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
069100     IF MSGI-KDSVAR = 'F'                                                 
069200        MOVE KONV-TIAAMMDD TO TAB-TIIDFAKT  (IX-RAD)                      
069300     ELSE                                                                 
069400        MOVE MSGI-TILOKDAT TO TAB-TIIDFAKT  (IX-RAD)                      
069500     END-IF                                                               
069600                                                                          
069700     MOVE INL-TIBERANK     TO TAB-TIBERANK  (IX-RAD)                      
069800                              TAB-TIBERANK-SORT (IX-RAD)                  
069900     IF INL-TIBERANK < 900000                                             
070000        MOVE  20           TO TAB-TISEKEL-SORT (IX-RAD)                   
070100     ELSE                                                                 
070200        MOVE  19           TO TAB-TISEKEL-SORT (IX-RAD)                   
070300     END-IF                                                               
070400     MOVE INL-KDFRAKT      TO W-KDFRAKT                                   
070500     MOVE INL-IDDISTR      TO W-IDDISTR                                   
070600     MOVE INL-IDKUNDNR     TO W-IDKUNDNR                                  
070700                              W-IDKUNDNR-NUM                              
070800     MOVE INL-IDLEVNR      TO W-IDLEVNR-DC                                
070900     PERFORM S2-BERAKNA-IDDC-SEND-REC                                     
071000     MOVE INL-IDDISTR      TO W-IDDISTR                                   
071100     MOVE W-IDDC-SEND     TO SEND-WS-IDDC                                 
071200     MOVE W-IDDC-REC      TO REC-WS-IDDC                                  
071300*    IF (W-IDKUNDNR = 1 OR 2 OR 17) AND                                   
071400     IF (DIST35-NONVCC-REFILL                                             
071500     OR  DIST35-REFILL-INOM-NDC)                                          
071600        IF (W-IDKUNDNR-NUM (5:2) = 01 OR 17) AND                          
071700          (SEND-NDC OR SEND-LDC OR REC-CDC OR REC-NDC OR                  
071800           REC-LDC)                                                       
071900                                                                          
072000           MOVE 1        TO W-KDORDKL                                     
072100        ELSE                                                              
072200           MOVE 4        TO W-KDORDKL                                     
072300        END-IF                                                            
072400     ELSE                                                                 
072500        IF (W-IDKUNDNR = 1 OR 2 OR 17) AND                                
072600          (SEND-CDC OR SEND-SDC OR REC-CDC OR REC-SDC)                    
072700                                                                          
072800           MOVE 1        TO W-KDORDKL                                     
072900        ELSE                                                              
073000           MOVE 4        TO W-KDORDKL                                     
073100        END-IF                                                            
073200     END-IF                                                               
073300***                                                                       
073400     MOVE W-KDORDKL        TO TAB-KDORDKL   (IX-RAD)                      
073500                                                                          
073600     IF INL-KVAVIS > ZERO AND IX-RAD < TAB-MAX                            
073700        ADD +1             TO IX-RAD                                      
073800     ELSE                                                                 
073900        MOVE SPACE         TO TAB-RAD (IX-RAD)                            
074000     END-IF                                                               
074100     .                                                                    
074200     EJECT                                                                
074300                                                                          
074400 CB-FYLL-I-RAD-FRAN-WDL612 SECTION.                                       
074500                                                                          
074600     MOVE ORD-IDDC         TO TAB-IDDC      (IX-RAD)                      
074700                              TAB-IDDC-SORT (IX-RAD)                      
074800     MOVE ORD-IDLEVNR      TO TAB-IDLEVNR   (IX-RAD)                      
074900     MOVE ORD-DAREGDAT (3:6) TO TAB-TIREGDAT  (IX-RAD)                    
075000     MOVE ORD-IDKUNDRF     TO TAB-IDKUNDRF  (IX-RAD)                      
075100     MOVE ORD-KVBEART      TO TAB-KVBEART   (IX-RAD)                      
075200     MOVE ZERO             TO TAB-KVRO      (IX-RAD)                      
075300     MOVE ZERO             TO TAB-KVAVIS    (IX-RAD)                      
075400     MOVE ZERO             TO TAB-IDFAKT    (IX-RAD)                      
075500     MOVE ZERO             TO TAB-TIIDFAKT  (IX-RAD)                      
075600     MOVE ORD-TIBERANK     TO TAB-TIBERANK  (IX-RAD)                      
075700                              TAB-TIBERANK-SORT (IX-RAD)                  
075800     IF ORD-TIBERANK < 900000                                             
075900        MOVE  20           TO TAB-TISEKEL-SORT (IX-RAD)                   
076000     ELSE                                                                 
076100        MOVE  19           TO TAB-TISEKEL-SORT (IX-RAD)                   
076200     END-IF                                                               
076300     MOVE ZERO             TO TAB-KDORDKL   (IX-RAD)                      
076400                                                                          
076500     IF ORD-KVBEART > ZERO AND IX-RAD < TAB-MAX                           
076600        ADD +1             TO IX-RAD                                      
076700     ELSE                                                                 
076800        MOVE SPACE         TO TAB-RAD (IX-RAD)                            
076900     END-IF                                                               
077000     .                                                                    
077100     EJECT                                                                
077200                                                                          
077300 D-LAES-WDE4      SECTION.                                                
077400                                                                          
077500     MOVE SPACES       TO W-IDDC-SPAR                                     
077600                                                                          
077700     MOVE W-IDARTNR TO W-IDARTNR-E4C-MIN W-IDARTNR-E4C-MAX                
077800     PERFORM IMS-GU-WDE4C1                                                
077900     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
078000       MOVE SEQC-IDDISTR     TO DIST35-IDDISTR                            
078100       IF DIST35-REFILL              OR                                   
078200          DIST35-REFILL-INOM-NDC     OR                                   
078300          DIST35-NONVCC-REFILL       OR                                   
078400          DIST35-REFILL-NA-JAP       OR                                   
078500          DIST35-VCC-NONVCC-REFILL   OR                                   
078600          DIST35-REFILL-INOM-JP      OR                                   
078700          DIST35-CN-TRANSFER         OR                                   
078800          DIST35-NA-TRANSFER         OR                                   
078900          DIST35-PACIFIC-TRANSFER    OR                                   
079000          DIST35-VCC-NONVCC-TRANSFER OR                                   
079100          DIST35-NONVCC-VCC-TRANSFER OR                                   
079101          DIST35-NONVCC-NONVCC-TRANSFER                                   
079102         MOVE SEQC-IDDISTR        TO W-IDDISTR-E4                         
079103         MOVE SEQC-IDKUNDNR       TO W-IDKUNDNR-E4                        
079200         MOVE SEQC-IDKUNDRF       TO W-IDKUNDRF-E4                        
079300         MOVE SEQC-IDPRODNR       TO W-IDPRODNR-E4                        
079400         MOVE SEQC-IDPLKLST       TO W-IDPLKLST-E4                        
079500         MOVE SEQC-IDPURAD        TO W-IDPURAD                            
079600         PERFORM IMS-GU-WDE401                                            
079700         PERFORM IMS-GNP-WDE411                                           
079800         PERFORM UNTIL SEGMENT-SAKNAS                                     
079900            IF CORAD-IDARTNR = W-IDARTNR                                  
080000               MOVE CORAD-IDPURAD TO W-IDPURAD W-IDPURAD-9                
080100               MOVE NEJ TO FAKTURERAD-SW                                  
080200               PERFORM IMS-GNP-WDE411-21                                  
080300               IF SEGMENT-FINNS                                           
080400                  MOVE KORD-IDPRODNR     TO W-IDPRODNR                    
080500                  PERFORM UNTIL SEGMENT-SAKNAS                            
080600                     MOVE KKOLLI-IDKOLLI    TO W-IDKOLLI                  
080700                     PERFORM IMS-GU-WDE601-11                             
080800                     IF SEGMENT-FINNS                                     
080900                        IF KOLLI-KDKOLSTA > 6                             
081000                           MOVE JA TO FAKTURERAD-SW                       
081100                        END-IF                                            
081200                     END-IF                                               
081300                     PERFORM IMS-GNP-WDE411-21                            
081400                  END-PERFORM                                             
081500               END-IF                                                     
081600               IF EJ-FAKTURERAD                                           
081700                  PERFORM DA-FYLL-I-RAD-FRAN-WDE420                       
081800               END-IF                                                     
081900            END-IF                                                        
082000            PERFORM IMS-GNP-WDE411                                        
082100         END-PERFORM                                                      
082200        END-IF                                                            
082300                                                                          
082400        PERFORM IMS-GN-WDE4C1                                             
082500     END-PERFORM                                                          
082600     .                                                                    
082700     EJECT                                                                
082800                                                                          
082900 DA-FYLL-I-RAD-FRAN-WDE420 SECTION.                                       
083000                                                                          
083100     MOVE KORD-IDDISTR     TO W-IDDISTR                                   
083200     MOVE KORD-IDKUNDNR    TO W-IDKUNDNR                                  
083300     MOVE KORD-IDDC        TO W-IDDC-SPAR                                 
083400     PERFORM S2-BERAKNA-IDDC-SEND-REC                                     
083500     IF W-IDDC-REC >= MIN-IDDC AND                                        
083600        W-IDDC-REC <= MAX-IDDC                                            
083700        IF CORAD-KDORDKL = 1                                              
083800           MOVE 601              TO LINK-KDCALL                           
083900        ELSE                                                              
084000           MOVE 602              TO LINK-KDCALL                           
084100        END-IF                                                            
084200        MOVE KORD-KDFRAKT        TO LINK-KDFRAKT                          
084300        MOVE W-IDDC-SEND         TO LINK-IDDC-SEND                        
084400        MOVE W-IDDC-REC          TO LINK-IDDC-REC                         
084500        MOVE KORD-TIORDREG       TO LINK-TIAAMMDD-ANROP                   
084600        IF LINK-TIAAMMDD-ANROP   > 900000                                 
084700           MOVE 19               TO LINK-TISEKEL-ANROP                    
084800        ELSE                                                              
084900           MOVE 20               TO LINK-TISEKEL-ANROP                    
085000        END-IF                                                            
085100                                                                          
085200        CALL W218ETA USING LINK-W218LETA ARTC-PCB WDK7-PCB                
085300                                         INLC-PCB LEVA-PCB                
085400                                         WDB6-PCB WDD9-PCB                
085500        IF LINK-SVAR-OK = JA                                              
085600           MOVE W-IDDC-REC       TO TAB-IDDC      (IX-RAD)                
085700                                    TAB-IDDC-SORT (IX-RAD)                
085800           PERFORM S3-BER-IDLEVNR-FRAN-IDDC-SEND                          
085900           MOVE W-IDLEVNR        TO TAB-IDLEVNR   (IX-RAD)                
086000           MOVE KORD-TIORDREG    TO TAB-TIREGDAT  (IX-RAD)                
086100           MOVE KORD-IDKUNDRF    TO TAB-IDKUNDRF  (IX-RAD)                
086200           IF CORAD-KDRADSTA > 3                                          
086300             MOVE CORAD-KVLEVART TO TAB-KVBEART   (IX-RAD)                
086400             IF CORAD-KVLEVART NOT = ZERO                                 
086500                MOVE CORAD-KVLEVART TO TAB-KVBEART(IX-RAD)                
086600             ELSE                                                         
086700                MOVE CORAD-KVAVBART TO TAB-KVBEART(IX-RAD)                
086800             END-IF                                                       
086900           ELSE                                                           
087000             MOVE CORAD-KVAVBART TO TAB-KVBEART   (IX-RAD)                
087100           END-IF                                                         
087200           MOVE ZERO             TO TAB-KVRO      (IX-RAD)                
087300           MOVE ZERO             TO TAB-KVAVIS    (IX-RAD)                
087400           MOVE ZERO             TO TAB-IDFAKT    (IX-RAD)                
087500           MOVE ZERO             TO TAB-TIIDFAKT  (IX-RAD)                
087600           MOVE CORAD-KDORDKL    TO TAB-KDORDKL   (IX-RAD)                
087700           MOVE LINK-TIAAMMDD-SVAR TO TAB-TIBERANK(IX-RAD)                
087800                                      TAB-TIBERANK-SORT (IX-RAD)          
087900           MOVE LINK-TISEKEL-SVAR  TO TAB-TISEKEL-SORT (IX-RAD)           
088000                                                                          
088100           IF (CORAD-KDRADSTA > 3 AND CORAD-KVLEVART > ZERO) OR           
088200              (CORAD-KDRADSTA > 3 AND CORAD-KVLEVART NOT = ZERO           
088300                                  AND CORAD-KVLEVART > ZERO) OR           
088400              (CORAD-KVAVBART > ZERO)                                     
088500              IF IX-RAD < TAB-MAX                                         
088600                 ADD +1          TO IX-RAD                                
088700              ELSE                                                        
088800                 MOVE SPACE      TO TAB-RAD (IX-RAD)                      
088900              END-IF                                                      
089000           ELSE                                                           
089100              MOVE SPACE         TO TAB-RAD (IX-RAD)                      
089200           END-IF                                                         
089300        END-IF                                                            
089400     END-IF                                                               
089500     .                                                                    
089600     EJECT                                                                
089700                                                                          
089800 E-LAES-WDQ4      SECTION.                                                
089900                                                                          
090000     MOVE SPACES       TO W-IDDC-SPAR                                     
090100                                                                          
090200     MOVE W-IDARTNR TO W-IDARTNR-Q4B-MIN W-IDARTNR-Q4B-MAX                
090300     PERFORM IMS-GU-WDQ4-ORQF01                                           
090400     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
090500        PERFORM EA-FYLL-I-RAD-FRAN-WDQ401                                 
090600                                                                          
090700        PERFORM IMS-GN-WDQ4-ORQF01                                        
090800     END-PERFORM                                                          
090900     .                                                                    
091000     EJECT                                                                
091100                                                                          
091200 EA-FYLL-I-RAD-FRAN-WDQ401 SECTION.                                       
091300                                                                          
091400     MOVE ORAD-IDDISTR     TO W-IDDISTR                                   
091500     MOVE ORAD-IDKUNDNR    TO W-IDKUNDNR                                  
091600     MOVE ORAD-IDDC        TO W-IDDC-SPAR                                 
091700     PERFORM S2-BERAKNA-IDDC-SEND-REC                                     
091800     IF W-IDDC-REC >= MIN-IDDC AND                                        
091900        W-IDDC-REC <= MAX-IDDC AND                                        
092000        ORAD-IDARTNR = W-IDARTNR                                          
092100        IF ORAD-KDORDKL = 1                                               
092200           MOVE 601              TO LINK-KDCALL                           
092300        ELSE                                                              
092400           MOVE 602              TO LINK-KDCALL                           
092500        END-IF                                                            
092600                                                                          
092700        MOVE ZERO                TO LINK-KDFRAKT                          
092800                                                                          
092900        MOVE W-IDDC-SEND         TO LINK-IDDC-SEND                        
093000        MOVE W-IDDC-REC          TO LINK-IDDC-REC                         
093100        MOVE ORAD-TIREGDAT       TO LINK-TIAAMMDD-ANROP                   
093200        IF LINK-TIAAMMDD-ANROP   > 900000                                 
093300           MOVE 19               TO LINK-TISEKEL-ANROP                    
093400        ELSE                                                              
093500           MOVE 20               TO LINK-TISEKEL-ANROP                    
093600        END-IF                                                            
093700                                                                          
093800        CALL W218ETA USING LINK-W218LETA ARTC-PCB WDK7-PCB                
093900                                         INLC-PCB LEVA-PCB                
094000                                         WDB6-PCB WDD9-PCB                
094100        IF LINK-SVAR-OK = JA                                              
094200           MOVE W-IDDC-REC       TO TAB-IDDC      (IX-RAD)                
094300                                    TAB-IDDC-SORT (IX-RAD)                
094400           PERFORM S3-BER-IDLEVNR-FRAN-IDDC-SEND                          
094500           MOVE W-IDLEVNR        TO TAB-IDLEVNR   (IX-RAD)                
094600           MOVE ORAD-TIREGDAT    TO TAB-TIREGDAT  (IX-RAD)                
094700           MOVE ORAD-IDKUNDRF    TO TAB-IDKUNDRF  (IX-RAD)                
094800           MOVE ORAD-KVBEART-Q   TO TAB-KVBEART   (IX-RAD)                
094900           MOVE ZERO             TO TAB-KVRO      (IX-RAD)                
095000           MOVE ZERO             TO TAB-KVAVIS    (IX-RAD)                
095100           MOVE ZERO             TO TAB-IDFAKT    (IX-RAD)                
095200           MOVE ZERO             TO TAB-TIIDFAKT  (IX-RAD)                
095300           MOVE ORAD-KDORDKL     TO TAB-KDORDKL   (IX-RAD)                
095400           MOVE LINK-TIAAMMDD-SVAR TO TAB-TIBERANK  (IX-RAD)              
095500                                      TAB-TIBERANK-SORT (IX-RAD)          
095600           MOVE LINK-TISEKEL-SVAR  TO TAB-TISEKEL-SORT (IX-RAD)           
095700                                                                          
095800           IF ORAD-KVBEART-Q > ZERO AND IX-RAD < TAB-MAX                  
095900              ADD +1             TO IX-RAD                                
096000           ELSE                                                           
096100              MOVE SPACE         TO TAB-RAD (IX-RAD)                      
096200           END-IF                                                         
096300        END-IF                                                            
096400     END-IF                                                               
096500     .                                                                    
096600     EJECT                                                                
096700                                                                          
096800 F-LAES-WDA5      SECTION.                                                
096900                                                                          
097000     MOVE SPACES       TO W-IDDC-SPAR                                     
097100                                                                          
097200     MOVE W-IDARTNR TO W-IDARTNR-A5A-MIN W-IDARTNR-A5A-MAX                
097300     PERFORM IMS-GU-WDA5-ORDP01                                           
097400     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
097500        IF RAD-KDSTARAD = '2' OR '3'                                      
097600           PERFORM FA-FYLL-I-RAD-FRAN-WDA501                              
097700        END-IF                                                            
097800                                                                          
097900        PERFORM IMS-GN-WDA5-ORDP01                                        
098000     END-PERFORM                                                          
098100     .                                                                    
098200     EJECT                                                                
098300                                                                          
098400 FA-FYLL-I-RAD-FRAN-WDA501 SECTION.                                       
098500                                                                          
098600     MOVE RAD-IDDISTR      TO W-IDDISTR                                   
098700     MOVE RAD-IDKUNDNR     TO W-IDKUNDNR                                  
098800     MOVE RAD-IDDC         TO W-IDDC-SPAR                                 
098900     PERFORM S2-BERAKNA-IDDC-SEND-REC                                     
099000     IF W-IDDC-REC >= MIN-IDDC AND                                        
099100        W-IDDC-REC <= MAX-IDDC AND                                        
099200        RAD-IDARTNR = W-IDARTNR                                           
099300        IF RAD-KDORDKL = 1                                                
099400           MOVE 609              TO LINK-KDCALL                           
099500        ELSE                                                              
099600           MOVE 610              TO LINK-KDCALL                           
099700        END-IF                                                            
099800        MOVE RAD-KDFRAKT         TO LINK-KDFRAKT                          
099900        MOVE W-IDDC-SEND         TO LINK-IDDC-SEND                        
100000        MOVE W-IDDC-REC          TO LINK-IDDC-REC                         
100100        MOVE W-IDARTNR           TO LINK-IDARTNR                          
100200        MOVE RAD-TIREGDAT        TO LINK-TIAAMMDD-ANROP                   
100300        IF LINK-TIAAMMDD-ANROP   > 900000                                 
100400           MOVE 19               TO LINK-TISEKEL-ANROP                    
100500        ELSE                                                              
100600           MOVE 20               TO LINK-TISEKEL-ANROP                    
100700        END-IF                                                            
100800                                                                          
100900        CALL W218ETA USING LINK-W218LETA ARTC-PCB WDK7-PCB                
101000                                         INLC-PCB LEVA-PCB                
101100                                         WDB6-PCB WDD9-PCB                
101200        IF LINK-SVAR-OK = JA                                              
101300           MOVE W-IDDC-REC       TO TAB-IDDC      (IX-RAD)                
101400                                    TAB-IDDC-SORT (IX-RAD)                
101500           PERFORM S3-BER-IDLEVNR-FRAN-IDDC-SEND                          
101600           MOVE W-IDLEVNR        TO TAB-IDLEVNR   (IX-RAD)                
101700           MOVE RAD-TIREGDAT     TO TAB-TIREGDAT  (IX-RAD)                
101800           MOVE RAD-IDKUNDRF     TO TAB-IDKUNDRF  (IX-RAD)                
101900           IF RAD-KVART > ZERO                                            
102000              MOVE ZERO          TO TAB-KVBEART   (IX-RAD)                
102100              MOVE RAD-KVART     TO TAB-KVRO      (IX-RAD)                
102200           ELSE                                                           
102300              MOVE RAD-KVBEART-Q TO TAB-KVBEART   (IX-RAD)                
102400              MOVE ZERO          TO TAB-KVRO      (IX-RAD)                
102500           END-IF                                                         
102600           MOVE ZERO             TO TAB-KVAVIS    (IX-RAD)                
102700           MOVE ZERO             TO TAB-IDFAKT    (IX-RAD)                
102800           MOVE ZERO             TO TAB-TIIDFAKT  (IX-RAD)                
102900           MOVE RAD-KDORDKL      TO TAB-KDORDKL   (IX-RAD)                
103000           MOVE LINK-TIAAMMDD-SVAR TO TAB-TIBERANK  (IX-RAD)              
103100                                      TAB-TIBERANK-SORT (IX-RAD)          
103200           MOVE LINK-TISEKEL-SVAR  TO TAB-TISEKEL-SORT (IX-RAD)           
103300                                                                          
103400           IF RAD-KVART > ZERO OR RAD-KVBEART-Q > ZERO                    
103500              IF IX-RAD < TAB-MAX                                         
103600                 ADD +1          TO IX-RAD                                
103700              ELSE                                                        
103800                 MOVE SPACE      TO TAB-RAD (IX-RAD)                      
103900              END-IF                                                      
104000           ELSE                                                           
104100              MOVE SPACE         TO TAB-RAD (IX-RAD)                      
104200           END-IF                                                         
104300        END-IF                                                            
104400     END-IF                                                               
104500     .                                                                    
104600     EJECT                                                                
104700                                                                          
104800 G-LAES-WDE3      SECTION.                                                
104900                                                                          
105000     MOVE SPACES       TO W-IDDC-SPAR                                     
105100                                                                          
105200     PERFORM IMS-GU-WDE3-ORDL01                                           
105300     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
105400        IF W-IDARTNR = REF-IDARTNR                                        
105500           IF REF-KDREFTYP = 'A' OR 'C' OR 'B' OR 'L' OR 'T'              
105600              IF REF-KDREFORS = 'O'                                       
105700                 PERFORM GA-FYLL-I-RAD-FRAN-WDE301                        
105800              END-IF                                                      
105900           END-IF                                                         
106000        END-IF                                                            
106100                                                                          
106200        PERFORM IMS-GN-WDE3-ORDL01                                        
106300     END-PERFORM                                                          
106400     .                                                                    
106500     EJECT                                                                
106600                                                                          
106700 GA-FYLL-I-RAD-FRAN-WDE301 SECTION.                                       
106800                                                                          
106900     MOVE REF-IDDISTR      TO W-IDDISTR                                   
107000     MOVE REF-IDKUNDNR     TO W-IDKUNDNR                                  
107100     MOVE REF-IDLEVNR      TO W-IDLEVNR-DC                                
107200     PERFORM S2-BERAKNA-IDDC-SEND-REC                                     
107300     IF W-IDDC-REC >= MIN-IDDC AND                                        
107400        W-IDDC-REC <= MAX-IDDC                                            
107500        IF REF-KDREFTYP = 'A' OR 'C'                                      
107600           MOVE 601              TO LINK-KDCALL                           
107700        ELSE                                                              
107800           IF REF-KDREFTYP = 'B' OR 'T'                                   
107900              MOVE 602           TO LINK-KDCALL                           
108000           ELSE                                                           
108100              IF REF-KDREFTYP = 'L'                                       
108200                MOVE 611         TO LINK-KDCALL                           
108300                MOVE REF-IDARTNR TO LINK-IDARTNR                          
108400              END-IF                                                      
108500           END-IF                                                         
108600        END-IF                                                            
108700        MOVE REF-KDFRAKT         TO LINK-KDFRAKT                          
108800        IF W-IDDC-SEND = ZERO                                             
108900          MOVE REF-IDLEVNR TO W-IDLEVNR-DC                                
109000          PERFORM IMS-GU-WDB601-LEV                                       
109100          IF SEGMENT-FINNS                                                
109200            MOVE DCS-IDDC        TO LINK-IDDC-SEND                        
109300          ELSE                                                            
109400            MOVE W-IDDC-SEND     TO LINK-IDDC-SEND                        
109500          END-IF                                                          
109600        ELSE                                                              
109700          MOVE W-IDDC-SEND       TO LINK-IDDC-SEND                        
109800        END-IF                                                            
109900        MOVE W-IDDC-REC          TO LINK-IDDC-REC                         
110000                                                                          
110100****    LÄGG TILL EN ARBETSDAG TILL DAGENS DATUM OM EJ FLYGORDER          
110200        IF LINK-KDCALL = 601                                              
110300           MOVE +1               TO WORK-KVWORKD                          
110400        ELSE                                                              
110500           MOVE +2               TO WORK-KVWORKD                          
110600        END-IF                                                            
110700        MOVE   2                 TO WORK-KDCALL                           
110800        MOVE W-IDDC-SEND         TO WORK-IDDC                             
110900        MOVE DAGENS-DATUM        TO WORK-TIAAMMDD-FOM                     
111000        CALL WORKDAY USING WORK-KDCALL                                    
111100                  WORK-DATE-AREA WORK-KDSVAR                              
111200        IF WORK-KDSVAR-OK                                                 
111300          MOVE WORK-TIAAMMDD-TOM  TO LINK-TIAAMMDD-ANROP                  
111400        ELSE                                                              
111500          MOVE DAGENS-DATUM       TO LINK-TIAAMMDD-ANROP                  
111600        END-IF                                                            
111700                                                                          
111800        IF LINK-TIAAMMDD-ANROP   > 900000                                 
111900           MOVE 19               TO LINK-TISEKEL-ANROP                    
112000        ELSE                                                              
112100           MOVE 20               TO LINK-TISEKEL-ANROP                    
112200        END-IF                                                            
112300        CALL W218ETA USING LINK-W218LETA ARTC-PCB WDK7-PCB                
112400                                         INLC-PCB LEVA-PCB                
112500                                         WDB6-PCB WDD9-PCB                
112600        IF LINK-SVAR-OK = JA                                              
112700* ?                                                                       
112800          MOVE LINK-TIAAMMDD-ANROP TO W-TIAAMMDD-ANROP                    
112900          MOVE LINK-TIAAMMDD-SVAR TO W-TIAAMMDD-SVAR                      
113000           MOVE W-IDDC-REC       TO TAB-IDDC      (IX-RAD)                
113100                                    TAB-IDDC-SORT (IX-RAD)                
113200                                    W-IDDC                                
113300           PERFORM S3-BER-IDLEVNR-FRAN-IDDC-SEND                          
113400           MOVE REF-IDLEVNR      TO TAB-IDLEVNR   (IX-RAD)                
113500           IF REF-KDREFTYP = 'L'                                          
113600              PERFORM IMS-GU-WDK711                                       
113700              IF SEGMENT-FINNS                                            
113800                MOVE SLAG-IDLEVNR  TO TAB-IDLEVNR   (IX-RAD)              
113900              ELSE                                                        
114000                MOVE SPACES        TO TAB-IDLEVNR   (IX-RAD)              
114100              END-IF                                                      
114200           END-IF                                                         
114300           MOVE DAGENS-DATUM     TO TAB-TIREGDAT  (IX-RAD)                
114400           MOVE SPACE            TO TAB-IDKUNDRF  (IX-RAD)                
114500           MOVE REF-KVBEART      TO TAB-KVBEART   (IX-RAD)                
114600           MOVE ZERO             TO TAB-KVRO      (IX-RAD)                
114700           MOVE ZERO             TO TAB-KVAVIS    (IX-RAD)                
114800           MOVE ZERO             TO TAB-IDFAKT    (IX-RAD)                
114900           MOVE ZERO             TO TAB-TIIDFAKT  (IX-RAD)                
115000           IF REF-KDREFTYP = 'A' OR 'C'                                   
115100              MOVE 1             TO TAB-KDORDKL   (IX-RAD)                
115200           ELSE                                                           
115300              MOVE 4             TO TAB-KDORDKL   (IX-RAD)                
115400           END-IF                                                         
115500           MOVE LINK-TIAAMMDD-SVAR TO TAB-TIBERANK  (IX-RAD)              
115600                                      TAB-TIBERANK-SORT (IX-RAD)          
115700           MOVE LINK-TISEKEL-SVAR  TO TAB-TISEKEL-SORT (IX-RAD)           
115800                                                                          
115900           IF REF-KVBEART > ZERO AND IX-RAD < TAB-MAX                     
116000              ADD +1             TO IX-RAD                                
116100           ELSE                                                           
116200              MOVE SPACE         TO TAB-RAD (IX-RAD)                      
116300           END-IF                                                         
116400        ELSE                                                              
116500          MOVE 'FEL I W218ETA (E3) ' TO MOD-TEMFSINF                      
116600          MOVE LINK-TIAAMMDD-ANROP TO W-TIAAMMDD-ANROP                    
116700          STRING 'LINK ' LINK-KDCALL ' ' LINK-IDDC-SEND                   
116800                                     ' ' LINK-IDDC-REC                    
116900                                     ' ' W-TIAAMMDD-ANROP                 
117000          DELIMITED BY SIZE INTO MOD-TEMFSINF                             
117100        END-IF                                                            
117200     END-IF                                                               
117300     .                                                                    
117400     EJECT                                                                
117500                                                                          
117600 H-SORTERA-INFO SECTION.                                                  
117700                                                                          
117800     SUBTRACT 1 FROM IX-RAD                                               
117900     MOVE IX-RAD TO ANTAL                                                 
118000                                                                          
118100     CALL WINTSOR USING TABELL STEGLANGD ANTAL                            
118200                  TAB-SORT (1) NYCKELLANGD                                
118300     .                                                                    
118400     EJECT                                                                
118500                                                                          
118600 I-VISA-INFO SECTION.                                                     
118700                                                                          
118800     PERFORM IMS-GU-BENA11-BSEQ                                           
118900                                                                          
119000     IF SEGMENT-FINNS                                                     
119100        MOVE TEXT-BEART     TO MOD-BEART                                  
119200     ELSE                                                                 
119300        MOVE 'PART MISSING' TO MOD-BEART                                  
119400     END-IF                                                               
119500                                                                          
119600***  HÄR LÄSES ETT STARTVÄRDE FÖR IX-RAD FRÅN USER-BASEN                  
119700***  SÅ ATT VID BLÄDDRING, START SKER MED RÄTT RAD                        
119800     IF MFS-IDPFK = '8' AND WS-MSGI-PGM = 'W20353'                        
119900        MOVE WS-MSGI-RADNR  TO IX-RAD-TAB                                 
120000     ELSE                                                                 
120100        MOVE +1             TO IX-RAD-TAB                                 
120200     END-IF                                                               
120300                                                                          
120400     IF MID-IDDC-IN NOT = SPACE                                           
120500        MOVE MID-IDDC-IN    TO WS-IDDC                                    
120600        IF NDC-CN OR NDC-US                                               
120700           MOVE MID-IDDC-IN TO W-IDDC                                     
120800           PERFORM IA-CHECK-EXT-SUPPLIER                                  
120900           IF INDATA-OK                                                   
121000              PERFORM IC-MOVE-TO-MOD                                      
121100           END-IF                                                         
121200        ELSE                                                              
121300           PERFORM IC-MOVE-TO-MOD                                         
121400        END-IF                                                            
121500     ELSE                                                                 
121600        PERFORM IB-CHECK-ER-SUPPLIER                                      
121700     END-IF                                                               
121800                                                                          
121900***  OM FLER RADER FINNS, SÅ SPARAS I USERBASEN NÄSTA RADNR               
122000***  FRÅN TABELLEN , ANNARS BLANKAS                                       
122100     IF IX-RAD-TAB      <= ANTAL                                          
122200        MOVE IX-RAD-TAB TO WS-MSGI-RADNR                                  
122300        MOVE 'W20353'   TO WS-MSGI-PGM                                    
122400        MOVE MORE-LINES TO MOD-TEMFSINF                                   
122500     ELSE                                                                 
122600        MOVE SPACE      TO WS-MSGI-SPAR-AREA                              
122700     END-IF                                                               
122800                                                                          
122900     MOVE '002'             TO MSGI-KDCALL                                
123000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
123100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
123200     MOVE '2353'            TO MSGI-IDTRANS                               
123300     MOVE WS-MSGI-SPAR-AREA TO MSGI-SPAR-AREA                             
123400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
123500                                                                          
123600     .                                                                    
123700     EJECT                                                                
123800                                                                          
123900 IA-CHECK-EXT-SUPPLIER SECTION.                                           
124000                                                                          
124100     PERFORM IMS-GU-WDK711                                                
124200     IF SEGMENT-FINNS                                                     
124300       IF SLAG-IDDC-REF = SPACES                                          
124400         MOVE NEJ                   TO INDATA-SW                          
124500         MOVE ERR-NOT-REFILL-PART   TO MED-IDMFSFEL                       
124600         CALL WMEDKONV USING MED-WMEDAREA                                 
124700         MOVE MED-MFSFEL            TO MOD-TEMFSFEL                       
124800         PERFORM MFS-RENSA-FAELT-UT                                       
124900       END-IF                                                             
125000     ELSE                                                                 
125100       MOVE NEJ                   TO INDATA-SW                            
125200       MOVE ERR-PART-MISSING      TO MED-IDMFSFEL                         
125300       CALL WMEDKONV USING MED-WMEDAREA                                   
125400       MOVE MED-MFSFEL            TO MOD-TEMFSFEL                         
125500       PERFORM MFS-RENSA-FAELT-UT                                         
125600     END-IF                                                               
125700     .                                                                    
125800     EJECT                                                                
125900 IB-CHECK-ER-SUPPLIER SECTION.                                            
126000     MOVE +1 TO IX-RAD                                                    
126100     PERFORM UNTIL IX-RAD > 13 OR IX-RAD-TAB > TAB-MAX                    
126200       MOVE TAB-IDDC(IX-RAD-TAB)         TO W-IDDC WS-IDDC                
126300       IF NDC-CN OR NDC-US                                                
126400         PERFORM IMS-GU-WDK711                                            
126500         IF SEGMENT-FINNS                                                 
126600           IF SLAG-IDDC-REF = SPACES                                      
126700              ADD -1 TO IX-RAD                                            
126800           ELSE                                                           
126900              MOVE TAB-RAD (IX-RAD-TAB)  TO MOD-GRP-RAD (IX-RAD)          
127000           END-IF                                                         
127100         END-IF                                                           
127200       ELSE                                                               
127300         MOVE TAB-RAD (IX-RAD-TAB)       TO MOD-GRP-RAD (IX-RAD)          
127400       END-IF                                                             
127500                                                                          
127600       ADD +1 TO IX-RAD IX-RAD-TAB                                        
127700     END-PERFORM                                                          
127800     .                                                                    
127900     EJECT                                                                
128000 IC-MOVE-TO-MOD SECTION.                                                  
128100     MOVE +1 TO IX-RAD                                                    
128200     PERFORM UNTIL IX-RAD > 13 OR IX-RAD-TAB > TAB-MAX                    
128300       MOVE TAB-RAD (IX-RAD-TAB)  TO MOD-GRP-RAD (IX-RAD)                 
128400       ADD +1 TO IX-RAD IX-RAD-TAB                                        
128500     END-PERFORM                                                          
128600     .                                                                    
128700     EJECT                                                                
128800                                                                          
128900 S2-BERAKNA-IDDC-SEND-REC SECTION.                                        
129000                                                                          
129100     MOVE ZERO                TO W-IDDC-SEND W-IDDC-REC                   
129200     MOVE W-IDDISTR           TO DIST35-IDDISTR                           
129300***                                                                       
129400***  ACCESS WWDC03 USING DISTRICT TO GET THE RECEIVING DC                 
129500***  WHEN RECEVING DC IS CDC THEN ACCESS WDK6 OTHERWISE WDK7              
129600***  ELSE ACCESS WDK7 TO GET THE SENDING DC                7              
129700***                                                                       
129800***  WHEN REFILL DISTRICTS                                                
129900***                                                                       
130000     IF DIST35-REFILL                                                     
130100     OR DIST35-REFILL-INOM-NDC                                            
130200     OR DIST35-NONVCC-REFILL                                              
130300     OR DIST35-REFILL-NA-JAP                                              
130400     OR DIST35-VCC-NONVCC-REFILL                                          
130500     OR DIST35-REFILL-INOM-JP                                             
130600        SEARCH ALL WWDC03-IDDC                                            
130700          AT END                                                          
130800            CALL FELLOG                                                   
130900         MOVE W-IDDC                     TO W-IDDC-REC                    
131000         MOVE ZERO                       TO W-IDDC-SEND                   
131100          WHEN WWDC03-SOK-IDDISTR-TAB2(WWDC03-IX2) = W-IDDISTR            
131200            MOVE WWDC03-SOK-IDDC-REC (WWDC03-IX2)                         
131300                                    TO W-IDDC-REC                         
131400        END-SEARCH                                                        
131500        MOVE W-IDDC-REC             TO W-IDDC                             
131600                                       WS-IDDC                            
131700*                                                                         
131800        IF W-IDDC-SPAR > SPACES                                           
131900           MOVE W-IDDC-SPAR        TO W-IDDC-SEND                         
132000        ELSE                                                              
132100          PERFORM IMS-GU-WDB601-LEV                                       
132300          IF SEGMENT-FINNS                                                
132400             MOVE DCS-IDDC            TO W-IDDC-SEND                      
132500          ELSE                                                            
132600             MOVE ZERO                TO W-IDDC-SEND                      
132700          END-IF                                                          
132800        END-IF                                                            
132900     END-IF                                                               
133000                                                                          
133100**FIND SENDING DC FROM CUSTOMER NUMBER WHEN REFILLORDER                   
133200**WHEN REFILLED FROM NDC'S                                                
133300     IF W-IDDC-SEND = ZERO                                                
133400       IF W-IDKUNDNR = 0                                                  
133500         MOVE REF-IDDC    TO W-IDDC                                       
133600         PERFORM IMS-GU-WDK711                                            
133700         IF SEGMENT-FINNS                                                 
133800           MOVE SLAG-IDDC-REF    TO W-IDDC-SEND                           
133900         ELSE                                                             
134000           MOVE ZERO        TO W-IDDC-SEND                                
134100         END-IF                                                           
134200       ELSE                                                               
134300         IF W-IDKUNDNR > 4099                                             
134400         AND W-IDKUNDNR < 4200                                            
134500           MOVE 41         TO W-IDDC-SEND                                 
134600         END-IF                                                           
134700         IF W-IDKUNDNR > 4299                                             
134800         AND W-IDKUNDNR < 4400                                            
134900           MOVE 43         TO W-IDDC-SEND                                 
135000         END-IF                                                           
135100         IF W-IDKUNDNR > 4399                                             
135200         AND W-IDKUNDNR < 4500                                            
135300           MOVE 44         TO W-IDDC-SEND                                 
135400         END-IF                                                           
135500         IF W-IDKUNDNR > 4499                                             
135600         AND W-IDKUNDNR < 4600                                            
135700           MOVE 45         TO W-IDDC-SEND                                 
135800         END-IF                                                           
135900         IF W-IDKUNDNR > 4599                                             
136000         AND W-IDKUNDNR < 4700                                            
136100           MOVE 46         TO W-IDDC-SEND                                 
136200         END-IF                                                           
136210         IF W-IDKUNDNR > 4699                                             
136220         AND W-IDKUNDNR < 4800                                            
136230           MOVE 47         TO W-IDDC-SEND                                 
136240         END-IF                                                           
136300         IF W-IDKUNDNR > 5099                                             
136400         AND W-IDKUNDNR < 5200                                            
136500           MOVE 51         TO W-IDDC-SEND                                 
136600         END-IF                                                           
136700         IF W-IDKUNDNR > 6099                                             
136800         AND W-IDKUNDNR < 6200                                            
136900           MOVE 61         TO W-IDDC-SEND                                 
137000         END-IF                                                           
137100         IF W-IDKUNDNR > 7099                                             
137200         AND W-IDKUNDNR < 7200                                            
137300           MOVE 71         TO W-IDDC-SEND                                 
137400         END-IF                                                           
137500         IF W-IDKUNDNR > 7199                                             
137600         AND W-IDKUNDNR < 7300                                            
137700           MOVE 72         TO W-IDDC-SEND                                 
137800         END-IF                                                           
137900         IF W-IDKUNDNR > 7299                                             
138000         AND W-IDKUNDNR < 7400                                            
138100           MOVE 73         TO W-IDDC-SEND                                 
138200         END-IF                                                           
138300         IF W-IDKUNDNR > 7399                                             
138400         AND W-IDKUNDNR < 7500                                            
138500           MOVE 74         TO W-IDDC-SEND                                 
138600         END-IF                                                           
138700       END-IF                                                             
138800     END-IF                                                               
138900                                                                          
139000**TRANSFERS BELOW                                                         
139100     IF DIST35-NA-TRANSFER                                                
139200        IF DIST35-FROM-US-TO-NDC41                                        
139300        OR DIST35-FROM-NDC51-TO-NDC41                                     
139400           MOVE WC-NDC-US-RU  TO W-IDDC-REC                               
139500        END-IF                                                            
139600        IF DIST35-FROM-US-TO-NDC43                                        
139700        OR DIST35-FROM-NDC51-TO-NDC43                                     
139800           MOVE WC-NDC-US-LA  TO W-IDDC-REC                               
139900        END-IF                                                            
140000        IF DIST35-FROM-US-TO-NDC44                                        
140100        OR DIST35-FROM-NDC51-TO-NDC44                                     
140200           MOVE WC-NDC-US-SE  TO W-IDDC-REC                               
140300        END-IF                                                            
140400        IF DIST35-FROM-US-TO-NDC45                                        
140500        OR DIST35-FROM-NDC51-TO-NDC45                                     
140600           MOVE WC-NDC-US-CH  TO W-IDDC-REC                               
140700        END-IF                                                            
140800        IF DIST35-FROM-US-TO-NDC46                                        
140900        OR DIST35-FROM-NDC51-TO-NDC46                                     
141000           MOVE WC-NDC-US-JA  TO W-IDDC-REC                               
141100        END-IF                                                            
141110        IF DIST35-FROM-US-TO-NDC47                                        
141120        OR DIST35-FROM-NDC51-TO-NDC47                                     
141130           MOVE WC-NDC-US-DA  TO W-IDDC-REC                               
141140        END-IF                                                            
141200        IF DIST35-FROM-US-TO-NDC92                                        
141300        OR DIST35-FROM-NDC51-TO-NDC92                                     
141400           MOVE WC-NDC-US-BAT TO W-IDDC-REC                               
141500        END-IF                                                            
141600        IF DIST35-US-CAN-TRANSFER                                         
141700           MOVE WC-NDC-CA     TO W-IDDC-REC                               
141800        END-IF                                                            
141900                                                                          
142000        IF W-IDKUNDNR = 41 OR 42 OR 43 OR 44 OR                           
142100                        45 OR 46 OR 47 OR 48 OR 49 OR 92                  
142200           MOVE W-IDKUNDNR TO W-IDDC-SEND-9                               
142300        END-IF                                                            
142400                                                                          
142500        IF W-IDKUNDNR > 509 AND W-IDKUNDNR < 520                          
142600           MOVE WC-NDC-CA       TO W-IDDC-SEND                            
142700        END-IF                                                            
142800     END-IF                                                               
142900                                                                          
143000     IF DIST35-PACIFIC-TRANSFER                                           
143100       IF DIST35-FROM-JP-TO-AU                                            
143200         MOVE WC-NDC-JP-61  TO W-IDDC-SEND                                
143300         MOVE WC-NDC-AU     TO W-IDDC-REC                                 
143400       END-IF                                                             
143500       IF DIST35-FROM-AU-TO-JP                                            
143600         MOVE WC-NDC-AU     TO W-IDDC-SEND                                
143700         MOVE WC-NDC-JP-61  TO W-IDDC-REC                                 
143800       END-IF                                                             
143900     END-IF                                                               
144000                                                                          
144100     IF DIST35-CN-TRANSFER                                                
144200                                                                          
144300        IF W-IDKUNDNR = 71 OR 72 OR 73 OR 74                              
144400           MOVE W-IDKUNDNR TO W-IDDC-SEND-9                               
144500        END-IF                                                            
144600     END-IF                                                               
144700                                                                          
144800**TO HAVE RECEIVING DC INFO                                               
144900     MOVE W-IDDISTR        TO W-IDDISTR-X                                 
144901**                                                                        
144903     IF DIST35-VCC-NONVCC-TRANSFER    OR                                  
144904        DIST35-NONVCC-VCC-TRANSFER    OR                                  
144905        DIST35-NONVCC-NONVCC-TRANSFER                                     
144906        MOVE W-IDKUNDNR        TO W-IDDC-SEND-9                           
144907        MOVE W-IDDISTR-X(4:2)  TO W-IDDC-REC                              
144913     END-IF                                                               
144914     .                                                                    
144915     EJECT                                                                
145000 S3-BER-IDLEVNR-FRAN-IDDC-SEND SECTION.                                   
145100                                                                          
145200     IF W-IDDC-SEND NOT = DCS-IDDC                                        
145300        MOVE W-IDDC-SEND TO W-IDDC-B6                                     
145400        PERFORM IMS-GU-WDB601                                             
145500     END-IF                                                               
145600     MOVE DCS-IDLEVNR-DC TO W-IDLEVNR                                     
145700                                                                          
145800     .                                                                    
145900     EJECT                                                                
146000 MFS-RENSA-FAELT-UT SECTION.                                              
146100                                                                          
146200     MOVE +1 TO IX-RAD                                                    
146300     PERFORM UNTIL IX-RAD > 13                                            
146400        MOVE MFS-RENSA-FAELT TO MOD-GRP-RAD (IX-RAD)                      
146500        ADD +1 TO IX-RAD                                                  
146600     END-PERFORM                                                          
146700     .                                                                    
146800     EJECT                                                                
146900* --- IMS SEKTIONER ---                                                   
147000                                                                          
147100 IMS-GET-MSG SECTION.                                                     
147200                                                                          
147300     MOVE '  QC' TO GODK-STATUSKODER                                      
147400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
147500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
147600     PERFORM IMS-STATUSKONTROLL                                           
147700     .                                                                    
147800     SKIP3                                                                
147900 IMS-INSERT-MSG SECTION.                                                  
148000                                                                          
148100     IF MSGI-IDLAND-SPR = 'GB'                                            
148200       MOVE 'N' TO MFS-KDHUVOMR                                           
148300     END-IF                                                               
148400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
148500     MOVE SPACE TO GODK-STATUSKODER                                       
148600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
148700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
148800     PERFORM IMS-STATUSKONTROLL                                           
148900     .                                                                    
149000     EJECT                                                                
149100 IMS-GU-WDK611       SECTION.                                             
149200                                                                          
149300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
149400          DELIMITED BY SIZE INTO SSA1                                     
149500     MOVE 'WDK611  '       TO SSA2                                        
149600     MOVE '  GE' TO GODK-STATUSKODER                                      
149700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
149800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
149900     PERFORM IMS-STATUSKONTROLL                                           
150000     .                                                                    
150100     EJECT                                                                
150200 IMS-GET-WDL6-INLC01 SECTION.                                             
150300                                                                          
150400     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X ')'                         
150500          DELIMITED BY SIZE INTO SSA1                                     
150600     MOVE '  GE' TO GODK-STATUSKODER                                      
150700     CALL CBLTDLI USING GU INLC-PCB DLI-IO-WLINLC01 SSA1                  
150800     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
150900     PERFORM IMS-STATUSKONTROLL                                           
151000     .                                                                    
151100     SKIP2                                                                
151200 IMS-GET-WDL6-INLC11 SECTION.                                             
151300                                                                          
151400     STRING 'WLINLC11   '                                                 
151500          DELIMITED BY SIZE INTO SSA1                                     
151600     MOVE '  GE' TO GODK-STATUSKODER                                      
151700     CALL CBLTDLI USING GNP INLC-PCB DLI-IO-WLINLC11 SSA1                 
151800     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
151900     PERFORM IMS-STATUSKONTROLL                                           
152000     .                                                                    
152100     SKIP2                                                                
152200 IMS-GET-WDL6-INLC12 SECTION.                                             
152300                                                                          
152400     STRING 'WLINLC12   '                                                 
152500          DELIMITED BY SIZE INTO SSA1                                     
152600     MOVE '  GE' TO GODK-STATUSKODER                                      
152700     CALL CBLTDLI USING GNP INLC-PCB DLI-IO-WLINLC12 SSA1                 
152800     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
152900     PERFORM IMS-STATUSKONTROLL                                           
153000     .                                                                    
153100     EJECT                                                                
153200 IMS-GU-WDE401 SECTION.                                                   
153300                                                                          
153400     STRING 'WDE401  (WDE401KY =' W-WDE4KEY-X ')'                         
153500          DELIMITED BY SIZE INTO SSA1                                     
153600     MOVE '  GE' TO GODK-STATUSKODER                                      
153700     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-WDE401 SSA1                    
153800     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
153900     PERFORM IMS-STATUSKONTROLL                                           
154000     .                                                                    
154100     SKIP3                                                                
154200 IMS-GNP-WDE411 SECTION.                                                  
154300                                                                          
154400     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
154500          DELIMITED BY SIZE INTO SSA1                                     
154600     MOVE '  GE' TO GODK-STATUSKODER                                      
154700     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-WDE411 SSA1                   
154800     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
154900     PERFORM IMS-STATUSKONTROLL                                           
155000     .                                                                    
155100     SKIP3                                                                
155200 IMS-GNP-WDE411-21 SECTION.                                               
155300                                                                          
155400     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
155500          DELIMITED BY SIZE INTO SSA1                                     
155600     STRING 'WDE421       '                                               
155700          DELIMITED BY SIZE INTO SSA2                                     
155800     MOVE '  GE' TO GODK-STATUSKODER                                      
155900     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-WDE421 SSA1 SSA2              
156000     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
156100     PERFORM IMS-STATUSKONTROLL                                           
156200     .                                                                    
156300     EJECT                                                                
156400 IMS-GU-WDE4C1 SECTION.                                                   
156500                                                                          
156600     STRING 'WDE4C1  (WDE4C1KY=>' W-WDE4C1KY-MIN                          
156700                    '&WDE4C1KY=<' W-WDE4C1KY-MAX ') '                     
156800          DELIMITED BY SIZE INTO SSA1                                     
156900     MOVE '  GE' TO GODK-STATUSKODER                                      
157000     CALL CBLTDLI USING GU WDE4C-PCB DLI-IO-WDE4C1 SSA1                   
157100     MOVE WDE4C-STATUS-CODE TO STATUS-WS                                  
157200     PERFORM IMS-STATUSKONTROLL                                           
157300     .                                                                    
157400     SKIP3                                                                
157500 IMS-GN-WDE4C1 SECTION.                                                   
157600                                                                          
157700     STRING 'WDE4C1  (WDE4C1KY=>' W-WDE4C1KY-MIN                          
157800                    '&WDE4C1KY=<' W-WDE4C1KY-MAX ') '                     
157900          DELIMITED BY SIZE INTO SSA1                                     
158000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
158100     CALL CBLTDLI USING GN WDE4C-PCB DLI-IO-WDE4C1 SSA1                   
158200     MOVE WDE4C-STATUS-CODE TO STATUS-WS                                  
158300     PERFORM IMS-STATUSKONTROLL                                           
158400     .                                                                    
158500     EJECT                                                                
158600 IMS-GU-WDQ4-ORQF01 SECTION.                                              
158700                                                                          
158800     STRING 'WLORQF01(WDQ4BSEQ=>' W-WDQ4BSEQ-MIN                          
158900                    '&WDQ4BSEQ=<' W-WDQ4BSEQ-MAX ')'                      
159000          DELIMITED BY SIZE INTO SSA1                                     
159100     MOVE '  GE' TO GODK-STATUSKODER                                      
159200     CALL CBLTDLI USING GU ORQF-PCB DLI-IO-WLORQF01 SSA1                  
159300     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
159400     PERFORM IMS-STATUSKONTROLL                                           
159500     .                                                                    
159600     SKIP3                                                                
159700 IMS-GN-WDQ4-ORQF01 SECTION.                                              
159800                                                                          
159900     STRING 'WLORQF01(WDQ4BSEQ=>' W-WDQ4BSEQ-MIN                          
160000                    '&WDQ4BSEQ=<' W-WDQ4BSEQ-MAX ')'                      
160100          DELIMITED BY SIZE INTO SSA1                                     
160200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
160300     CALL CBLTDLI USING GN ORQF-PCB DLI-IO-WLORQF01 SSA1                  
160400     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
160500     PERFORM IMS-STATUSKONTROLL                                           
160600     .                                                                    
160700     EJECT                                                                
160800 IMS-GU-WDA5-ORDP01 SECTION.                                              
160900                                                                          
161000     STRING 'WLORDP01(WDA5ASEQ=>' W-WDA5ASEQ-MIN                          
161100                    '&WDA5ASEQ=<' W-WDA5ASEQ-MAX ')'                      
161200          DELIMITED BY SIZE INTO SSA1                                     
161300     MOVE '  GE' TO GODK-STATUSKODER                                      
161400     CALL CBLTDLI USING GU ORDP-PCB DLI-IO-WLORDP01 SSA1                  
161500     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
161600     PERFORM IMS-STATUSKONTROLL                                           
161700     .                                                                    
161800     SKIP3                                                                
161900 IMS-GN-WDA5-ORDP01 SECTION.                                              
162000                                                                          
162100     STRING 'WLORDP01(WDA5ASEQ=>' W-WDA5ASEQ-MIN                          
162200                    '&WDA5ASEQ=<' W-WDA5ASEQ-MAX ')'                      
162300          DELIMITED BY SIZE INTO SSA1                                     
162400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
162500     CALL CBLTDLI USING GN ORDP-PCB DLI-IO-WLORDP01 SSA1                  
162600     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
162700     PERFORM IMS-STATUSKONTROLL                                           
162800     .                                                                    
162900     EJECT                                                                
163000 IMS-GU-WDE3-ORDL01 SECTION.                                              
163100                                                                          
163200     STRING 'WLORDL01(IDARTNR  =' W-IDARTNR-X ')'                         
163300          DELIMITED BY SIZE INTO SSA1                                     
163400     MOVE '  GE' TO GODK-STATUSKODER                                      
163500     CALL CBLTDLI USING GU ORDL-PCB DLI-IO-WLORDL01 SSA1                  
163600     MOVE ORDL-STATUS-CODE TO STATUS-WS                                   
163700     PERFORM IMS-STATUSKONTROLL                                           
163800     .                                                                    
163900     SKIP3                                                                
164000 IMS-GN-WDE3-ORDL01 SECTION.                                              
164100                                                                          
164200     STRING 'WLORDL01(IDARTNR  =' W-IDARTNR-X ')'                         
164300          DELIMITED BY SIZE INTO SSA1                                     
164400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
164500     CALL CBLTDLI USING GN ORDL-PCB DLI-IO-WLORDL01 SSA1                  
164600     MOVE ORDL-STATUS-CODE TO STATUS-WS                                   
164700     PERFORM IMS-STATUSKONTROLL                                           
164800     .                                                                    
164900     EJECT                                                                
165000 IMS-GU-BENA11-BSEQ SECTION.                                              
165100                                                                          
165200     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
165300             DELIMITED BY SIZE INTO SSA1                                  
165400     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
165500             DELIMITED BY SIZE INTO SSA2                                  
165600     MOVE '  GE' TO GODK-STATUSKODER                                      
165700     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WLBENA11 SSA1 SSA2             
165800     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
165900     PERFORM IMS-STATUSKONTROLL                                           
166000     .                                                                    
166100     EJECT                                                                
166200 IMS-GU-WDE601-11 SECTION.                                                
166300                                                                          
166400     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
166500          DELIMITED BY SIZE INTO SSA1                                     
166600     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
166700          DELIMITED BY SIZE INTO SSA2                                     
166800     MOVE '  GE' TO GODK-STATUSKODER                                      
166900     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE611 SSA1 SSA2               
167000     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
167100     PERFORM IMS-STATUSKONTROLL                                           
167200     .                                                                    
167300     EJECT                                                                
167400 IMS-GU-WDK711 SECTION.                                                   
167500                                                                          
167600     MOVE SPACES TO SSA1                                                  
167700                    SSA2                                                  
167800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
167900             DELIMITED BY SIZE INTO SSA1                                  
168000     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
168100             DELIMITED BY SIZE INTO SSA2                                  
168200     MOVE '  GE' TO GODK-STATUSKODER                                      
168300     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
168400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
168500     PERFORM IMS-STATUSKONTROLL                                           
168600     .                                                                    
168700 IMS-GU-WDB601    SECTION.                                                
168800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
168900             DELIMITED BY SIZE INTO SSA1                                  
169000     MOVE '  GE' TO GODK-STATUSKODER                                      
169100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
169200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
169300     PERFORM IMS-STATUSKONTROLL                                           
169400     IF SEGMENT-SAKNAS                                                    
169500        MOVE SPACE TO DCS-KDDC                                            
169600        MOVE SPACE TO DCS-IDLEVNR-DC                                      
169700     END-IF                                                               
169800            .                                                             
169900     EJECT                                                                
170000 IMS-GU-WDB601-LEV SECTION.                                               
170100                                                                          
170200     STRING 'WDB601  (IDLEVNDC =' W-IDLEVNR-DC-X ')'                      
170300          DELIMITED BY SIZE INTO SSA1                                     
170400     MOVE '  GE'              TO GODK-STATUSKODER                         
170500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
170600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
170700     PERFORM IMS-STATUSKONTROLL                                           
170800     .                                                                    
170900 IMS-STATUSKONTROLL SECTION.                                              
171000                                                                          
171100     SET STATUS-IX TO 1                                                   
171200     SEARCH GODK-STATUS                                                   
171300       AT END                                                             
171400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
171500         DELIMITED BY SIZE INTO FELTEXT                                   
171600         CALL FELLOG                                                      
171700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
171800         CONTINUE                                                         
171900     END-SEARCH                                                           
172000     .                                                                    
