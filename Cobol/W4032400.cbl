000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4032400.                                                
000400 AUTHOR.         M LUNDBERG.                                              
000500 DATE-WRITTEN.   DEC  1985.                                               
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PÅBÖRJADE EJ AVSLUTADE.                                          
001100*                                                                         
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W4T324                                              
001500*        MID:         W4I32401                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         W4O32401                                            
001900*                                                                         
002000*    CHANGE LOG                                                           
002100*                                                                         
002200*    DIGAMBAR/021011                                                      
002300*    STRUCTURE OF ACTION TRANSACTION 4487 IS CHANGED TO IMPROVE           
002400*    THE RESPONSE TIME OF THE SCREEN 4312.                                
002500*                                                                         
002600*    SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP3                                                                
002900 DATA DIVISION.                                                           
003000     EJECT                                                                
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003300*    -- CHECKED BY WY2000                                                 
003400 77   PROGRAM-NAMN           VALUE 'W4032400'                             
003500                                 PIC X(8).                                
003600 77    JA                        PIC X       VALUE 'J'.                   
003700 77    NEJ                       PIC X       VALUE 'N'.                   
003800 77    BORTTAG                   PIC X       VALUE 'B'.                   
003900 77    RAD-FINNS                 PIC X       VALUE 'N'.                   
004000 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
004100 77    SPRAK-INDX                PIC S9(9)   VALUE +0   COMP SYNC.        
004200 77    MID-RAD-IND               PIC S9(9)   VALUE +0   COMP SYNC.        
004300 77    MOD-RAD-IND               PIC S9(9)   VALUE +0   COMP SYNC.        
004400 77    MAX-LINE                  PIC S9(9)   VALUE +13  COMP SYNC.        
004500 77    WS-KVORDRAD               PIC S9(5)              COMP-3.           
004600 77    WS-IDPRODNR               PIC  X(7).                               
004610 77    WS-IDPRODNR-9             PIC S9(7).                               
004700 77    WS-SAVE-DARFS             PIC 9(12).                               
004800     SKIP2                                                                
004900 77    TIBEGPAC-WS               PIC S9(7)              COMP-3.           
005000 77    KDORDKL-WS                PIC S9(1)              COMP-3.           
005100 77    IDDISTR-WS                PIC S9(5)              COMP-3.           
005200 77    IDKUNDNR-WS               PIC S9(7)              COMP-3.           
005300 77    KDFRAKT-WS                PIC S9(3)              COMP-3.           
005400 77    IDPRODNR-WS               PIC S9(7)              COMP-3.           
005500 77    KVORDRAD-WS               PIC S9(7)              COMP-3.           
005600 77    KVORDRAD-PACK-WS          PIC S9(7)              COMP-3.           
005700 77    KVKOLPAC-WS               PIC S9(5)              COMP-3.           
005800 77    TIUTSKR-WS                PIC S9(7)              COMP-3.           
005900 77    WS-ARBDAT-NUM7            PIC S9(7).                               
006000 77    WS-IDTIDZON               PIC  X(2).                               
006100*      - - - - - - - - - - - - - *****                                    
006200 77    WS-KDFEL                  PIC 9(2)    VALUE ZERO COMP-3.           
006210 77    WS-KDFEL-9                PIC 9(2)    VALUE ZERO.                  
006300     SKIP2                                                                
006400 01    WS-KDORDKL                          PIC X.                         
006500 01    WS-KDORDKL-NUM REDEFINES WS-KDORDKL PIC 9.                         
006600     SKIP2                                                                
006610*                                                                         
006620 01  4324-SPAR-AREA.                                                      
006630     03  SPAR-IDTRANS            PIC X(4)    VALUE '4324'.                
006660     03  FILLER OCCURS 14.                                                
006670         05 SPAR-MOD-IDPRODNR    PIC 9(7)    VALUE ZERO.                  
006680     EJECT                                                                
006700*      --- VALID IDDD CODES                                               
006800*                                                                         
006900*01    -COPY WWDC99                                                       
007000       EJECT                                                              
007100 01    WS-KDPRCGRP                       PIC X(5).                        
007200 01    KDPRCGRP-WS REDEFINES WS-KDPRCGRP PIC 9(5).                        
007300     SKIP2                                                                
007400 01    WS-IDKUNDRF.                                                       
007500   03  WS-IDORDNR                PIC X(5).                                
007600   03  FILLER                    PIC X(5)    VALUE SPACE.                 
007700     SKIP2                                                                
007800 77    WS-DATUM                  PIC 9(6)    VALUE ZERO.                  
007900 77    WS-DATUM-TIRFS            PIC 9(6)    VALUE ZERO.                  
008000     EJECT                                                                
008100 01    INGAANG-SW                PIC X.                                   
008200   88  PRODNR-INGANG             VALUE 'J'.                               
008300   88  PRCGRP-INGANG             VALUE 'N'.                               
008400                                                                          
008500 01    NYCKEL-TYP                PIC S9(2)   COMP-3.                      
008600   88  GAMMAL-NYCKEL             VALUE +1.                                
008700   88  NY-NYCKEL                 VALUE +2.                                
008800     SKIP3                                                                
008900 01    FRAN-BILD                 PIC 9(4).                                
009000   88  FRAN-BILD-OK              VALUE 4312                               
009100                                       4321 THRU 4324.                    
009200   88  FRAN-BILD-EGEN            VALUE 4324.                              
009300   88  4312-BILD                 VALUE 4312.                              
009400     SKIP3                                                                
009500 01    INDATA-SW                 PIC X.                                   
009600   88  INDATA-OK                 VALUE 'J'.                               
009700   88  INDATA-FEL                VALUE 'N'.                               
009800     SKIP3                                                                
009900 01    KDCMD-SW                  PIC X.                                   
010000   88  KDCMD-OK                  VALUE 'J'.                               
010100     SKIP3                                                                
010200 01    WS-NYCKEL.                                                         
010300   03  WS-DARFS-NYCKEL           PIC 9(12).                               
010400   03  WS-IDPRODNR-NYCKEL        PIC 9(7).                                
010500     EJECT                                                                
010600 01    NYCKLAR-TILL-DLI.                                                  
010700   03    W-WDE601-IDPRODNR-X.                                             
010800     05    W-601-IDPRODNR        PIC S9(7)    COMP-3.                     
010900     SKIP1                                                                
011000   03    W-WDE401-KORD-X.                                                 
011100     05    W-401-IDDISTR         PIC S9(5)    COMP-3.                     
011200     05    W-401-IDKUNDNR        PIC S9(7)    COMP-3.                     
011300     05    W-401-IDKUNDRF.                                                
011400       07    W-401-IDORDNR       PIC 9(5).                                
011500       07    FILLER              PIC X(5)     VALUE SPACE.                
011600     05    W-401-IDPRODNR        PIC S9(7)    COMP-3.                     
011700     05    W-401-IDPLKLST        PIC S9(3)    COMP-3.                     
011800     SKIP1                                                                
011900   03    W-WDE411-KEYSEQ-MIN-X.                                           
012000     05    W-411-IDPRODNR-MIN    PIC S9(7)    COMP-3.                     
012100     05    W-411-IDPURAD-MIN     PIC S9(5)    COMP-3.                     
012200     SKIP1                                                                
012300   03    W-WDE411-KEYSEQ-MAX-X.                                           
012400     05    W-411-IDPRODNR-MAX    PIC S9(7)    COMP-3.                     
012500     05    W-411-IDPURAD-MAX     PIC S9(5)    COMP-3.                     
012600     SKIP1                                                                
012700   03    W-4487-KEY-X.                                                    
012800     05    W-4487-IDHTYP         PIC X(4)  VALUE '4487'.                  
012900     05    W-4487-IDDC           PIC X(2).                                
013000     05    W-4487-LOW-VALUE      PIC X(24) VALUE LOW-VALUE.               
013100     SKIP1                                                                
013200   03    W-4488-KEY-X.                                                    
013300     05    W-4488-KDPRCGRP       PIC X(5).                                
013400     SKIP1                                                                
013500   03    W-4490-KEY-X.                                                    
013600     05    W-4490-DARFS          PIC 9(12).                               
013700     05    W-4490-IDPRODNR       PIC S9(7)    COMP-3.                     
013800     05    W-4490-IDPLKLST       PIC S9(3)    COMP-3.                     
013900     EJECT                                                                
014000 01    MEDDELANDE.                                                        
014100   03    FEL1.                                                            
014200     05    FILLER                PIC X(40)   VALUE                        
014300             '784  BLÄDDRING EJ TILLÅTEN             '.                   
014400     05    FILLER                PIC X(40)   VALUE                        
014500             '784 SCROLLING NOT ALLOWED              '.                   
014600   03    FILLER REDEFINES FEL1.                                           
014700     05    FEL-1 OCCURS 2        PIC X(40).                               
014800     SKIP2                                                                
014900   03    FEL2.                                                            
015000     05    FILLER                PIC X(40)   VALUE                        
015100             '802  ORDER SAKNAS PÅ ANG FÖRMANSOMRÅDE '.                   
015200     05    FILLER                PIC X(40)   VALUE                        
015300             '802  WRONG AREA FOR ORDER              '.                   
015400   03    FILLER REDEFINES FEL2.                                           
015500     05    FEL-2 OCCURS 2        PIC X(40).                               
015600     SKIP2                                                                
015700   03    FEL3.                                                            
015800     05    FILLER                PIC X(40)   VALUE                        
015900             '749  FEL NYCKEL                        '.                   
016000     05    FILLER                PIC X(40)   VALUE                        
016100             '749  WRONG KEY                         '.                   
016200   03    FILLER REDEFINES FEL3.                                           
016300     05    FEL-3 OCCURS 2        PIC X(40).                               
016400     SKIP2                                                                
016500   03    FEL4.                                                            
016600     05    FILLER                PIC X(40)   VALUE                        
016700             '054  ORDER SAKNAS                      '.                   
016800     05    FILLER                PIC X(40)   VALUE                        
016900             '054  ORDER MISSING                     '.                   
017000   03    FILLER REDEFINES FEL4.                                           
017100     05    FEL-4 OCCURS 2        PIC X(40).                               
017200     SKIP2                                                                
017300   03    FEL5.                                                            
017400     05    FILLER                PIC X(40)   VALUE                        
017500             '127  PRESS PF9 TO SPLIT                '.                   
017600     05    FILLER                PIC X(40)   VALUE                        
017700             '127  PRESS PF9 TO SPLIT                '.                   
017800   03    FILLER REDEFINES FEL5.                                           
017900     05    FEL-5 OCCURS 2        PIC X(40).                               
018000     SKIP2                                                                
018100   03    FEL6.                                                            
018200     05    FILLER                PIC X(40)   VALUE                        
018300             '001  UPPLYSTA FÄLT FEL                 '.                   
018400     05    FILLER                PIC X(40)   VALUE                        
018500             '001  CORRECT HIGHLIT FIELDS            '.                   
018600   03    FILLER REDEFINES FEL6.                                           
018700     05    FEL-6 OCCURS 2        PIC X(40).                               
018800     SKIP2                                                                
018900   03    MED1.                                                            
019000     05    FILLER                PIC X(40)   VALUE                        
019100             '778  FLER RADER FINNS                  '.                   
019200     05    FILLER                PIC X(40)   VALUE                        
019300             '778  MORE LINES                        '.                   
019400   03    FILLER REDEFINES MED1.                                           
019500     05    MED-1 OCCURS 2        PIC X(40).                               
019600     EJECT                                                                
019700******************************************************************        
019800*                                                                         
019900*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
020000*                                                                         
020100 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
020200     SKIP3                                                                
020300*01    MID -COPY W4I32401.                                                
020400     EJECT                                                                
020500*01    -COPY WMSGAREA                                                     
020600     EJECT                                                                
020700*  03    MOD -COPY W4O32401  -RED MSG-AREA.                               
020800     EJECT                                                                
020900     03  MOD REDEFINES MSG-AREA.                                          
021000*      05  -COPY W4O31201 -PRE 4312-                                      
021100     EJECT                                                                
021200 01  FILLER                      PIC X(16) VALUE 'P-TO-P-SW-4312'.        
021300 01  P-TO-P-SW-4312.                                                      
021400     03  PTOP-LL                 PIC S9(4)   VALUE 0 COMP SYNC.           
021500     03  PTOP-Z1                 PIC  X(1)   VALUE LOW-VALUE.             
021600     03  PTOP-Z2                 PIC  X(1)   VALUE LOW-VALUE.             
021700     03  PTOP-KDTRANS            PIC  X(8)   VALUE 'W4T312  '.            
021800     03  PTOP-IDTRANS            PIC  X(4)   VALUE '4324'.                
021900     03  PTOP-KDMFSFOR           PIC  X(1).                               
022000*    03  -COPY W4I31201   -PRE PTOP-                                      
022100     EJECT                                                                
022200*01    -COPY WMFSAREA                                                     
022300     EJECT                                                                
022400******************************************************************        
022500*                                                                         
022600*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
022700*                                                                         
022800 01    IMS-WS.                                                            
022900   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
023000     SKIP3                                                                
023100*                        **** STATUS-KOD FRÅN IMS                         
023200   03    STATUS-WS               PIC XX.                                  
023300     88    SEGMENT-FINNS                    VALUE '  '.                   
023400     88    SEGMENT-SAKNAS                   VALUE 'GE'.                   
023500     88    SEGMENT-FINNS-REDAN              VALUE 'II'.                   
023600     88    SLUT-PA-BASEN                    VALUE 'GB'.                   
023700     SKIP3                                                                
023800   03    GODK-STATUSKODER.                                                
023900     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
024000     SKIP3                                                                
024100 01    SSA1                      PIC X(64).                               
024200 01    SSA2                      PIC X(64).                               
024300 01    SSA3                      PIC X(64).                               
024400     EJECT                                                                
024500 01    GENERELLA-SUBPROGRAM.                                              
024600   03  WMEDKONV                  PIC X(8)   VALUE 'WMEDKONV'.             
024700   03  WDATKONV                  PIC X(8)   VALUE 'WDATKONV'.             
024800   03  CBLTDLI                   PIC X(8)   VALUE 'CBLTDLI '.             
024900   03  FELLOG                    PIC X(8)   VALUE 'FELLOG  '.             
025000   03  W005INIT                  PIC X(8)   VALUE 'W005INIT'.             
025100     EJECT                                                                
025200                                                                          
025300*    --- PARAMETERS FOR WMEDKONV                                          
025400*01 -COPY WMEDAREA                                                        
025500 01  MESSAGE-CODES.                                                       
025600     03  KEYS-ARE-MISSING        PIC X(3)    VALUE '005'.                 
025700     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
025800     03  INF-ENTER-CMD           PIC X(3)    VALUE '048'.                 
025900     03  INF-PRESS-PF4-TO-PRINT  PIC X(3)    VALUE '081'.                 
026000     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
026100     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
026200     03  INF-PRINT-REQUESTED     PIC X(3)    VALUE '118'.                 
026300     03  INF-PRESS-PF9-TO-SPLIT  PIC X(3)    VALUE '127'.                 
026400     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
026500     03  ERR-MORE-THAN-ONE-CMD   PIC X(3)    VALUE '097'.                 
026600     03  ERR-LAST-PAGE-SHOWN     PIC X(3)    VALUE '115'.                 
026700     03  ERR-PF4-AND-NO-CMD      PIC X(3)    VALUE '231'.                 
026800     03  ERR-SELECT-LINE         PIC X(3)    VALUE '309'.                 
026900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
027000     03  ERR-ZERO-NOT-ALLOWED    PIC X(3)    VALUE '724'.                 
027100     03  ERR-WRONG-PRINTER       PIC X(3)    VALUE '772'.                 
027200     EJECT                                                                
027300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
027400*01 -COPY WMSGINIT                                                        
027500     EJECT                                                                
027600*01    -COPY WDATAREA                                                     
027700     EJECT                                                                
027800*                            IMS FUNKTIONSKODER                           
027900*01    -COPY W0003                                                        
028000     EJECT                                                                
028100*                            DLI INPUT-OUTPUT AREA                        
028200 01    DLI-IO-AREA.                                                       
028300   03    IO-AREA                 PIC X(510)  VALUE SPACE.                 
028400     SKIP3                                                                
028500*  03    WDE601 -COPY WDE601              -RED IO-AREA.                   
028600     EJECT                                                                
028700 01    DLI-IO-AREA2.                                                      
028800   03    IO-AREA2                PIC X(145)  VALUE SPACE.                 
028900     SKIP3                                                                
029000*  03    WDE401 -COPY WDE401              -RED IO-AREA2.                  
029100     EJECT                                                                
029200 01    DLI-IO-AREA3.                                                      
029300   03    IO-AREA3                PIC X(64)   VALUE SPACE.                 
029400     SKIP3                                                                
029500*  03    4487-AREA  -COPY WDGX4487            -RED IO-AREA3.              
029600     EJECT                                                                
029700*  03    WDGX4488 -COPY WDGX4488            -RED IO-AREA3.                
029800     EJECT                                                                
029900*  03    WDGX4490 -COPY WDGX4490            -RED IO-AREA3.                
030000     EJECT                                                                
030100 LINKAGE SECTION.                                                         
030200*01    -COPY W0009     -PRE MSG-                                          
030300     EJECT                                                                
030400*01    -COPY W0009     -PRE ALT-                                          
030500     05  FILLER                  PIC X.                                   
030600     EJECT                                                                
030610*01    -COPY W0008     -PRE WDP7-                                         
030620     05  FILLER                  PIC X.                                   
030630     EJECT                                                                
030700*01    -COPY W0008     -PRE WDE4-                                         
030800     05  FILLER                  PIC X.                                   
030900     EJECT                                                                
031000*01    -COPY W0008     -PRE WDE6-                                         
031100     05  FILLER                  PIC X.                                   
031200     EJECT                                                                
031300*01    -COPY W0008     -PRE 4487-                                         
031400     05  FILLER                  PIC X.                                   
031500     EJECT                                                                
031600*01    -COPY W0008     -PRE WDE42-                                        
031700     05  FILLER                  PIC X.                                   
031800     EJECT                                                                
031900 PROCEDURE DIVISION USING MSG-PCB  ALT-PCB  WDP7-PCB                      
032000                          WDE4-PCB WDE6-PCB 4487-PCB WDE42-PCB.           
032100     ENTRY 'DLITCBL' USING MSG-PCB  ALT-PCB  WDP7-PCB                     
032200                           WDE4-PCB WDE6-PCB 4487-PCB WDE42-PCB.          
032300     SKIP2                                                                
032400 STYR SECTION.                                                            
032500     PERFORM IMS-GET-MSG                                                  
032600     IF SEGMENT-FINNS                                                     
032700       PERFORM A-INIT-SPARA-INPUT                                         
032800       PERFORM B-KOLLA-INPUT                                              
032900       IF WS-KDFEL NOT > 0                                                
033000*        IF 4312-BILD                                                     
033100*          PERFORM O-RETURN-FROM-4312                                     
033200*        ELSE                                                             
033300           IF MFS-SPLIT                                                   
033400             PERFORM P-SPLIT-TO-W40312                                    
033500           ELSE                                                           
033600             IF PRCGRP-INGANG                                             
033700              IF WS-KDFEL = ZERO                                          
033800               IF MFS-IDPFK = '8'                                         
033900                 IF GAMMAL-NYCKEL                                         
034000                   PERFORM C-HAEMTA-NYCKEL                                
034100                 ELSE                                                     
034200                     MOVE 1 TO WS-KDFEL                                   
034300                 END-IF                                                   
034400               END-IF                                                     
034500               MOVE KDPRCGRP-WS   TO W-4488-KDPRCGRP                      
034600               MOVE WS-IDDC       TO W-4487-IDDC                          
034700               PERFORM IMS-GU-WDGX4488                                    
034800               IF SEGMENT-FINNS AND                                       
034900                  WS-KDFEL NOT > 0                                        
035000                  IF MFS-IDPFK = '8'                                      
035100                     PERFORM IMS-GU-WDGX4490                              
035200                     IF SEGMENT-FINNS                                     
035300                        MOVE 4490-DARFS TO WS-SAVE-DARFS                  
035400                     END-IF                                               
035500                  ELSE                                                    
035600                     PERFORM IMS-GNP-WDGX4490                             
035700                  END-IF                                                  
035800                  IF SEGMENT-FINNS                                        
035900                     PERFORM S02-BLANKA-RADER                             
036000                     MOVE ZERO TO MOD-RAD-IND                             
036100                                                                          
036200                     PERFORM UNTIL SEGMENT-SAKNAS OR                      
036300                                   SLUT-PA-BASEN OR                       
036400                                   MOD-RAD-IND > MAX-LINE                 
036500                       IF WS-KDORDKL-NUM = 4490-KDORDKL OR                
036600                              WS-KDORDKL = SPACE                          
036700                         MOVE 4490-IDPRODNR TO W-601-IDPRODNR             
036800                         MOVE 4490-DARFS    TO WS-SAVE-DARFS              
036900                                                                          
037000                         IF 4490-IDPRODNR    = IDPRODNR-WS                
037100                           CONTINUE                                       
037200*                          MOVE 4490-IDPLKLST TO W-401-IDPLKLST           
037300*                          PERFORM IMS-GU-KUNDORDER                       
037400*                          PERFORM S03-ADDERA-KVRADER                     
037500                         ELSE                                             
037600                           IF MOD-RAD-IND    > ZERO                       
037700                              IF WS-KDORDKL-NUM = KDORDKL-WS OR           
037800                                     WS-KDORDKL = SPACE                   
037900                                 IF RAD-FINNS = JA                        
038000                                    PERFORM M-FLYTTA-TILL-MOD             
038100                                    ADD 1 TO MOD-RAD-IND                  
038200                                    MOVE NEJ  TO RAD-FINNS                
038300                                 END-IF                                   
038400                              END-IF                                      
038500                           ELSE                                           
038600                              ADD 1 TO MOD-RAD-IND                        
038700                           END-IF                                         
038800                           PERFORM IMS-GET-WDE601                         
038900                           IF SEGMENT-FINNS                               
039000                            MOVE JA           TO RAD-FINNS                
039100                            PERFORM F-FLYTTA-VORD-TILL-RAD                
039200                          MOVE 4490-IDPRODNR TO W-411-IDPRODNR-MIN        
039300                                                W-411-IDPRODNR-MAX        
039400                            MOVE 1            TO W-411-IDPURAD-MIN        
039500                            MOVE 99999        TO W-411-IDPURAD-MAX        
039600                            PERFORM IMS-GU-KUNDORDER-SEK-INV              
039700                            MOVE KORD-IDDISTR TO W-401-IDDISTR            
039800                            MOVE KORD-IDKUNDNR TO W-401-IDKUNDNR          
039900                            MOVE KORD-IDORDNR5 TO W-401-IDORDNR           
040000                            MOVE 4490-IDPRODNR TO W-401-IDPRODNR          
040100                            MOVE 4490-IDPLKLST TO W-401-IDPLKLST          
040200                            PERFORM IMS-GU-KUNDORDER                      
040300                            PERFORM S01-FLYTTA-PLOC-TILL-RAD              
040400                            PERFORM G-SPARA-NYCKEL                        
040500                           END-IF                                         
040600                         END-IF                                           
040700                       END-IF                                             
040800                       PERFORM IMS-GNP-WDGX4490                           
040900                     END-PERFORM                                          
041000                                                                          
041100                     IF MOD-RAD-IND NOT > MAX-LINE                        
041200                        IF WS-KDORDKL-NUM = KDORDKL-WS OR                 
041300                               WS-KDORDKL = SPACE                         
041400                           IF RAD-FINNS = JA                              
041500                              PERFORM M-FLYTTA-TILL-MOD                   
041600                              ADD 1 TO MOD-RAD-IND                        
041700                           END-IF                                         
041800                        END-IF                                            
041900                     END-IF                                               
042000                     IF SEGMENT-FINNS AND MOD-RAD-IND = 14                
042100                        MOVE 15 TO WS-KDFEL                               
042200                     END-IF                                               
042210                                                                          
042220                   MOVE '002' TO MSGI-KDCALL                              
042230                   MOVE '4324' TO SPAR-IDTRANS                            
042240                   MOVE 4324-SPAR-AREA TO MSGI-SPAR-AREA                  
042250                   CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB             
042300                  ELSE                                                    
042400                     MOVE 2 TO WS-KDFEL                                   
042500                  END-IF                                                  
042600               ELSE                                                       
042700                  MOVE 2 TO WS-KDFEL                                      
042800               END-IF                                                     
042900              END-IF                                                      
043000             ELSE                                                         
043100               PERFORM N-VISA-VOLVOORDER                                  
043200             END-IF                                                       
043300           END-IF                                                         
043400*        END-IF                                                           
043500       END-IF                                                             
043600       IF WS-KDFEL > ZERO                                                 
043700         PERFORM J-HAMTA-MEDDELANDE                                       
043800         PERFORM K-VISA-BILD-IGEN                                         
043900         IF WS-KDFEL = 2 OR WS-KDFEL = 3                                  
044000           PERFORM S02-BLANKA-RADER                                       
044100         END-IF                                                           
044200       END-IF                                                             
044210       IF MFS-SPLIT AND WS-KDFEL = ZERO                                   
044220         COMPUTE PTOP-LL = LENGTH OF PTOP-MID-W4I31201 + 17               
044230         PERFORM IMS-ISRT-ALT-MSG-4312                                    
044240       ELSE                                                               
044300         COMPUTE MSG-KVLL = LENGTH OF MOD-W4O32401 + 4                    
044400         PERFORM IMS-INSERT-MSG                                           
044410       END-IF                                                             
044500     END-IF                                                               
044600     MOVE ZERO TO RETURN-CODE                                             
044700     GOBACK                                                               
044800     .                                                                    
044900     EJECT                                                                
045000 A-INIT-SPARA-INPUT SECTION.                                              
045100     SKIP2                                                                
045200     IF MSG-DUBBLA-TRANSKODER                                             
045300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I32401                 
045400       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
045500       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
045600       MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                  
045700       MOVE MSG-IDPFK                     TO MFS-IDPFK                    
045800     ELSE                                                                 
045900       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I32401                 
046000       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
046100       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
046200       MOVE ' '                           TO MFS-KDTRTYP                  
046300     END-IF                                                               
046400*                                                                         
046500     MOVE NEJ                  TO INGAANG-SW                              
046600     MOVE ZERO                 TO WS-KDFEL                                
046700                                  TIBEGPAC-WS                             
046800                                  KDORDKL-WS                              
046900                                  IDDISTR-WS                              
047000                                  IDKUNDNR-WS                             
047100                                  KDFRAKT-WS                              
047200                                  IDPRODNR-WS                             
047300                                  KVORDRAD-WS                             
047400                                  KVORDRAD-PACK-WS                        
047500                                  KVKOLPAC-WS                             
047600                                  TIUTSKR-WS                              
047610     MOVE JA                   TO INDATA-SW                               
047700     MOVE LOW-VALUE            TO MSG-AREA                                
047800     MOVE MFS-IDTRANS          TO FRAN-BILD                               
047900                                                                          
048000     MOVE ALL '+'           TO MSGI-WMSGINIT                              
048100     MOVE '001'             TO MSGI-KDCALL                                
048200     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
048300     MOVE '4324'            TO MSGI-IDTRANS                               
048400     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
048500     IF FRAN-BILD-EGEN                                                    
048510     OR 4312-BILD                                                         
048520*    OR (4312-BILD AND MFS-RETURN)                                        
048600        MOVE MID-IDPRODNR-IN    TO MSGI-IDPRODNR                          
048700     END-IF                                                               
048800     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
048900     MOVE MSGI-IDTIDZON     TO WS-IDTIDZON                                
048910     MOVE MSGI-SPAR-AREA    TO 4324-SPAR-AREA                             
048920                                                                          
048930     IF SPAR-IDTRANS NOT NUMERIC                                          
048940       MOVE ZEROES TO SPAR-IDTRANS                                        
048950     END-IF                                                               
049000                                                                          
049100     IF MSGI-IDLAND-SPR = 'GB'                                            
049200       MOVE +2 TO SPRAK-INDX                                              
049300     ELSE                                                                 
049400       MOVE +1 TO SPRAK-INDX                                              
049500     END-IF                                                               
049600                                                                          
049700     IF FRAN-BILD-EGEN                                                    
049701     OR 4312-BILD                                                         
049710*    OR (4312-BILD AND MFS-RETURN)                                        
049800        IF MID-KDPRCGRP-IN NOT = ALL '+'                                  
049900           INSPECT MID-KDPRCGRP-IN REPLACING LEADING                      
050000           SPACE BY ZEROES                                                
050100           MOVE MID-KDPRCGRP-IN       TO WS-KDPRCGRP                      
050200           MOVE +2 TO NYCKEL-TYP                                          
050300        ELSE                                                              
050400           IF MID-IDPRODNR-IN NOT = ALL '+'                               
050500              INSPECT MID-IDPRODNR-IN REPLACING LEADING                   
050600              SPACE BY ZEROES                                             
050700              MOVE MID-IDPRODNR-IN    TO WS-IDPRODNR                      
050800              MOVE '00000'            TO WS-KDPRCGRP                      
050900           ELSE                                                           
051000              IF MID-KDPRCGRP-UT NOT = ALL ' '                            
051100                 INSPECT MID-KDPRCGRP-UT REPLACING LEADING                
051200                 SPACE BY ZEROES                                          
051300                 MOVE MID-KDPRCGRP-UT TO WS-KDPRCGRP                      
051400                 MOVE +1 TO NYCKEL-TYP                                    
051500              ELSE                                                        
051600                 MOVE MSGI-IDPRODNR   TO WS-IDPRODNR                      
051700                 MOVE '00000'         TO WS-KDPRCGRP                      
051800              END-IF                                                      
051900           END-IF                                                         
052000        END-IF                                                            
052100     ELSE                                                                 
052200        MOVE 0000000                  TO WS-IDPRODNR                      
052300        MOVE '00000'                  TO WS-KDPRCGRP                      
052400        MOVE +1                       TO NYCKEL-TYP                       
052500     END-IF                                                               
052600                                                                          
052700     IF FRAN-BILD-EGEN                                                    
052800        IF MID-KDORDKL-IN = ALL '+'                                       
052900           MOVE MID-KDORDKL-UT        TO WS-KDORDKL                       
053000        ELSE                                                              
053100           MOVE MID-KDORDKL-IN        TO WS-KDORDKL                       
053200        END-IF                                                            
053300     ELSE                                                                 
053400        MOVE SPACE                    TO WS-KDORDKL                       
053500     END-IF                                                               
053600                                                                          
053700     IF FRAN-BILD-EGEN                                                    
053701     OR 4312-BILD                                                         
053710*    OR (4312-BILD AND MFS-RETURN)                                        
053800        IF MID-IDDC-IN = ALL '+'                                          
053900           MOVE MID-IDDC-UT           TO WS-IDDC                          
054000        ELSE                                                              
054100           MOVE MID-IDDC-IN           TO WS-IDDC                          
054200        END-IF                                                            
054300     ELSE                                                                 
054400        MOVE MSGI-IDDC                TO WS-IDDC                          
054500     END-IF                                                               
054600     MOVE WS-IDDC                     TO MOD-IDDC-UT                      
054700                                                                          
054800     MOVE 'W4O324N1' TO MFS-IDMOD                                         
054900     MOVE '4324' TO MOD-IDTRANS                                           
055000                                                                          
055100     IF  FRAN-BILD-EGEN                                                   
055101     OR 4312-BILD                                                         
055110*    OR (4312-BILD AND MFS-RETURN)                                        
055120       IF MID-TIRFS-NYCKEL NUMERIC                                        
055200         MOVE MID-TIRFS-NYCKEL    TO WS-DARFS-NYCKEL                      
055300                                     MOD-TIRFS-NYCKEL                     
055400         IF MID-TIRFS-NYCKEL NOT = ZERO                                   
055500           IF MID-TIRFS-NYCKEL < 5000000000                               
055600             MOVE 20              TO WS-DARFS-NYCKEL (1:2)                
055700           ELSE                                                           
055800             IF MID-TIRFS-NYCKEL < 9999999999                             
055900               MOVE 19            TO WS-DARFS-NYCKEL (1:2)                
056000             ELSE                                                         
056100               MOVE 999999999999  TO WS-DARFS-NYCKEL                      
056200             END-IF                                                       
056300           END-IF                                                         
056400         END-IF                                                           
056500         MOVE MID-IDPRODNR-NYCKEL TO WS-IDPRODNR-NYCKEL                   
056600                                     MOD-IDPRODNR-NYCKEL                  
056601       ELSE                                                               
056610         MOVE ZERO                TO WS-DARFS-NYCKEL                      
056620                                     MOD-TIRFS-NYCKEL                     
056630                                     WS-IDPRODNR-NYCKEL                   
056640                                     MOD-IDPRODNR-NYCKEL                  
056650       END-IF                                                             
056700     ELSE                                                                 
056800         MOVE ZERO                TO WS-DARFS-NYCKEL                      
056900                                     MOD-TIRFS-NYCKEL                     
057000                                     WS-IDPRODNR-NYCKEL                   
057100                                     MOD-IDPRODNR-NYCKEL                  
057200     END-IF                                                               
057300     SKIP2                                                                
057400     MOVE MFS-RENSA-FAELT TO MOD-KDPRCGRP-IN                              
057500                             MOD-IDDISTR-IN                               
057600                             MOD-IDKUNDNR-IN                              
057700                             MOD-IDORDNR-IN                               
057800                             MOD-KDORDKL-IN                               
057900                             MOD-IDPRODNR-IN                              
058000                             MOD-IDDC-IN                                  
058100                             MOD-TEMFSFEL                                 
058200                             MOD-TEMFSINF                                 
058300     MOVE 1 TO MOD-RAD-IND                                                
058400     PERFORM UNTIL MOD-RAD-IND > MAX-LINE                                 
058500       MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD       (MOD-RAD-IND)            
058600       MOVE MFS-ROER-EJ-FAELT TO MOD-TIBEGPAC    (MOD-RAD-IND)            
058700                                 MOD-KDORDKL     (MOD-RAD-IND)            
058800                                 MOD-IDDISTR     (MOD-RAD-IND)            
058900                                 MOD-IDKUNDNR    (MOD-RAD-IND)            
059000                                 MOD-IDORDNR     (MOD-RAD-IND)            
059100                                 MOD-KDFRAKT     (MOD-RAD-IND)            
059200                                 MOD-IDPRODNR    (MOD-RAD-IND)            
059300                                 MOD-KVORDRAD    (MOD-RAD-IND)            
059400                              MOD-KVORDRAD-PACK  (MOD-RAD-IND)            
059500                                 MOD-KVKOLPAC    (MOD-RAD-IND)            
059600                                 MOD-TIUTSKR     (MOD-RAD-IND)            
059700                                 MOD-TISTADAT    (MOD-RAD-IND)            
059800       ADD 1 TO MOD-RAD-IND                                               
059900     END-PERFORM                                                          
060000     .                                                                    
060100     EJECT                                                                
060200 B-KOLLA-INPUT SECTION.                                                   
060300                                                                          
060400     IF WS-KDORDKL   NUMERIC OR                                           
060500        WS-KDORDKL = ALL SPACE                                            
060600       IF WS-KDPRCGRP NUMERIC AND                                         
060700         WS-KDPRCGRP > ZERO                                               
060800          MOVE WS-KDPRCGRP  TO MOD-KDPRCGRP-UT                            
060900          MOVE WS-KDORDKL   TO MOD-KDORDKL-UT                             
061000          INSPECT MOD-KDPRCGRP-UT REPLACING                               
061100                                 LEADING ZEROES BY SPACE                  
061200          MOVE NEJ TO INGAANG-SW                                          
061300       ELSE                                                               
061400          IF WS-IDPRODNR NUMERIC AND                                      
061500            WS-IDPRODNR > ZERO                                            
061600             MOVE JA TO INGAANG-SW                                        
061700             MOVE WS-IDPRODNR  TO MOD-IDPRODNR-UT                         
061800             MOVE ZERO         TO MOD-KDORDKL-UT                          
061811             MOVE WS-IDPRODNR  TO WS-IDPRODNR-9                           
061900          ELSE                                                            
062000             MOVE 3 TO WS-KDFEL                                           
062100          END-IF                                                          
062200       END-IF                                                             
062300     ELSE                                                                 
062400        MOVE 3 TO WS-KDFEL                                                
062500     END-IF                                                               
064400     .                                                                    
064500     EJECT                                                                
064600 C-HAEMTA-NYCKEL SECTION.                                                 
064700     MOVE WS-DARFS-NYCKEL          TO W-4490-DARFS                        
064800     MOVE WS-IDPRODNR-NYCKEL       TO W-4490-IDPRODNR                     
064900     MOVE ZERO                     TO W-4490-IDPLKLST                     
065000     .                                                                    
065100     EJECT                                                                
065200 F-FLYTTA-VORD-TILL-RAD SECTION.                                          
065300                                                                          
065400     MOVE VORD-KVKOLPAC      TO KVKOLPAC-WS                               
065500     MOVE VORD-KVORDRAD      TO KVORDRAD-WS                               
065600     MOVE VORD-KVORDRAD-PACK TO KVORDRAD-PACK-WS                          
065700     MOVE VORD-TIUTSKR       TO WS-DATUM                                  
065800     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
065900     MOVE WS-DATUM           TO DAT-I-TIDATUM                             
066000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
066100                         DAT-O-TIDATUM DAT-KDSVAR                         
066200     IF DAT-KDSVAR-OK                                                     
066300       MOVE DAT-TIAAVVD      TO TIUTSKR-WS                                
066400     END-IF                                                               
066500     .                                                                    
066600     EJECT                                                                
066700 G-SPARA-NYCKEL SECTION.                                                  
066800     MOVE 4490-DARFS (3:10)           TO MOD-TIRFS-NYCKEL                 
066900     MOVE 4490-IDPRODNR               TO MOD-IDPRODNR-NYCKEL              
067000     .                                                                    
067100     EJECT                                                                
067200 J-HAMTA-MEDDELANDE SECTION.                                              
067300     EVALUATE WS-KDFEL                                                    
067400       WHEN  1 MOVE FEL-1 (SPRAK-INDX) TO MOD-TEMFSFEL                    
067500       WHEN  2 MOVE FEL-2 (SPRAK-INDX) TO MOD-TEMFSFEL                    
067600       WHEN  3 MOVE FEL-3 (SPRAK-INDX) TO MOD-TEMFSFEL                    
067700       WHEN  4 MOVE FEL-4 (SPRAK-INDX) TO MOD-TEMFSFEL                    
067800       WHEN  5 CONTINUE                                                   
067900       WHEN  6 CONTINUE                                                   
068000       WHEN 15 MOVE MED-1 (SPRAK-INDX) TO MOD-TEMFSINF                    
068100     END-EVALUATE                                                         
068200     .                                                                    
068300     EJECT                                                                
068400 K-VISA-BILD-IGEN SECTION.                                                
068500     IF WS-KDFEL NOT = 15                                                 
068600       MOVE 1 TO MOD-RAD-IND                                              
068700       PERFORM UNTIL MOD-RAD-IND > MAX-LINE                               
068800         MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD        (MOD-RAD-IND)         
068900         MOVE MFS-ROER-EJ-FAELT TO MOD-TIBEGPAC     (MOD-RAD-IND)         
069000                                   MOD-KDORDKL      (MOD-RAD-IND)         
069100                                   MOD-IDDISTR      (MOD-RAD-IND)         
069200                                   MOD-IDKUNDNR     (MOD-RAD-IND)         
069300                                   MOD-IDORDNR      (MOD-RAD-IND)         
069400                                   MOD-KDFRAKT      (MOD-RAD-IND)         
069500                                   MOD-IDPRODNR     (MOD-RAD-IND)         
069600                                   MOD-KVORDRAD     (MOD-RAD-IND)         
069700                                  MOD-KVORDRAD-PACK (MOD-RAD-IND)         
069800                                   MOD-KVKOLPAC     (MOD-RAD-IND)         
069900                                   MOD-TIUTSKR      (MOD-RAD-IND)         
070000                                   MOD-TISTADAT     (MOD-RAD-IND)         
070100         ADD 1 TO MOD-RAD-IND                                             
070200       END-PERFORM                                                        
070300       MOVE MFS-ROER-EJ-FAELT   TO MOD-IDPRODNR-NYCKEL                    
070400     END-IF                                                               
070500     .                                                                    
070600     EJECT                                                                
070700 L-TOM-SKAERM SECTION.                                                    
070800     MOVE MFS-RENSA-FAELT     TO MOD-TEMFSFEL                             
070900                                 MOD-KDPRCGRP-IN                          
071000                                 MOD-KDPRCGRP-UT                          
071100                                 MOD-IDDISTR-IN                           
071200                                 MOD-IDDISTR-UT                           
071300                                 MOD-IDKUNDNR-IN                          
071400                                 MOD-IDKUNDNR-UT                          
071500                                 MOD-IDORDNR-IN                           
071600                                 MOD-IDORDNR-UT                           
071700                                 MOD-KDORDKL-IN                           
071800                                 MOD-KDORDKL-UT                           
071900                                 MOD-IDPRODNR-IN                          
072000                                 MOD-IDPRODNR-UT                          
072100                                 MOD-IDDC-IN                              
072200                                 MOD-IDDC-UT                              
072300                                 MOD-TEMFSINF                             
072400     MOVE 1 TO MOD-RAD-IND                                                
072500     PERFORM UNTIL MOD-RAD-IND > MAX-LINE                                 
072600       MOVE MFS-RENSA-FAELT   TO MOD-TIBEGPAC     (MOD-RAD-IND)           
072700                                 MOD-KDORDKL      (MOD-RAD-IND)           
072800                                 MOD-IDDISTR      (MOD-RAD-IND)           
072900                                 MOD-IDKUNDNR     (MOD-RAD-IND)           
073000                                 MOD-IDORDNR      (MOD-RAD-IND)           
073100                                 MOD-KDFRAKT      (MOD-RAD-IND)           
073200                                 MOD-IDPRODNR     (MOD-RAD-IND)           
073300                                 MOD-KVORDRAD     (MOD-RAD-IND)           
073400                                MOD-KVORDRAD-PACK (MOD-RAD-IND)           
073500                                 MOD-KVKOLPAC     (MOD-RAD-IND)           
073600                                 MOD-TIUTSKR      (MOD-RAD-IND)           
073700                                 MOD-TISTADAT     (MOD-RAD-IND)           
073800       ADD 1 TO MOD-RAD-IND                                               
073900     END-PERFORM                                                          
074000     MOVE MFS-RENSA-FAELT     TO MOD-IDPRODNR-NYCKEL                      
074100     EJECT                                                                
074200     .                                                                    
074300 M-FLYTTA-TILL-MOD SECTION.                                               
074400                                                                          
074500     MOVE TIBEGPAC-WS          TO MOD-TIBEGPAC      (MOD-RAD-IND)         
074600     MOVE KDORDKL-WS           TO MOD-KDORDKL       (MOD-RAD-IND)         
074700     MOVE IDDISTR-WS           TO MOD-IDDISTR       (MOD-RAD-IND)         
074800     MOVE IDKUNDNR-WS          TO MOD-IDKUNDNR      (MOD-RAD-IND)         
074900     MOVE KDFRAKT-WS           TO MOD-KDFRAKT       (MOD-RAD-IND)         
075000     MOVE IDPRODNR-WS          TO MOD-IDPRODNR      (MOD-RAD-IND)         
075010     MOVE IDPRODNR-WS          TO SPAR-MOD-IDPRODNR (MOD-RAD-IND)         
075100     MOVE KVORDRAD-WS          TO MOD-KVORDRAD      (MOD-RAD-IND)         
075200     MOVE KVORDRAD-PACK-WS     TO MOD-KVORDRAD-PACK (MOD-RAD-IND)         
075300     MOVE KVKOLPAC-WS          TO MOD-KVKOLPAC      (MOD-RAD-IND)         
075400                                                                          
075500     MOVE TIUTSKR-WS           TO MOD-TIUTSKR       (MOD-RAD-IND)         
075600     MOVE WS-IDORDNR           TO MOD-IDORDNR       (MOD-RAD-IND)         
075700                                                                          
075800     MOVE ZERO                 TO  TIBEGPAC-WS                            
075900                                   KDORDKL-WS                             
076000                                   IDDISTR-WS                             
076100                                   IDKUNDNR-WS                            
076200                                   KDFRAKT-WS                             
076300                                   IDPRODNR-WS                            
076400                                   KVORDRAD-WS                            
076500                                   KVORDRAD-PACK-WS                       
076600                                   KVKOLPAC-WS                            
076700                                   TIUTSKR-WS                             
076800     .                                                                    
076900     SKIP2                                                                
077000 N-VISA-VOLVOORDER SECTION.                                               
077100                                                                          
077200     MOVE WS-IDPRODNR-9       TO W-601-IDPRODNR                           
077300                                 W-411-IDPRODNR-MIN                       
077400                                 W-411-IDPRODNR-MAX                       
077500     PERFORM IMS-GET-WDE601                                               
077600     IF SEGMENT-FINNS                                                     
077700        IF VORD-IDDC = WS-IDDC                                            
077800           PERFORM S02-BLANKA-RADER                                       
077900           MOVE VORD-KVKOLPAC   TO KVKOLPAC-WS                            
078000           MOVE VORD-KVORDRAD-PACK TO KVORDRAD-PACK-WS                    
078100           MOVE VORD-KDORDKL    TO KDORDKL-WS                             
078200           MOVE VORD-IDDISTR    TO IDDISTR-WS                             
078300           MOVE VORD-IDKUNDNR   TO IDKUNDNR-WS                            
078400           MOVE VORD-KDFRAKT    TO KDFRAKT-WS                             
078500           MOVE VORD-IDPRODNR   TO IDPRODNR-WS                            
078600                                   WS-IDPRODNR-NYCKEL                     
078700           MOVE VORD-KVORDRAD   TO KVORDRAD-WS                            
078800                                                                          
078900           MOVE VORD-TIUTSKR    TO WS-DATUM                               
079000           MOVE 'AAMMDD'        TO DAT-KDDATFORM                          
079100           MOVE WS-DATUM        TO DAT-I-TIDATUM                          
079200           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
079300                               DAT-O-TIDATUM DAT-KDSVAR                   
079400           IF DAT-KDSVAR-OK                                               
079500             MOVE DAT-TIAAVVD   TO TIUTSKR-WS                             
079600           END-IF                                                         
079700           MOVE 1               TO W-411-IDPURAD-MIN                      
079800           MOVE 99999           TO W-411-IDPURAD-MAX                      
079900           PERFORM IMS-GU-KUNDORDER-SEK-INV                               
080000                                                                          
080100           MOVE KORD-TIBEGPAC   TO WS-DATUM                               
080200           MOVE 'AAMMDD'        TO DAT-KDDATFORM                          
080300           MOVE WS-DATUM        TO DAT-I-TIDATUM                          
080400           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
080500                               DAT-O-TIDATUM DAT-KDSVAR                   
080600           IF DAT-KDSVAR-OK                                               
080700             MOVE DAT-TIAAVVD      TO TIBEGPAC-WS                         
080800           END-IF                                                         
080900           MOVE KORD-IDKUNDRF   TO WS-IDKUNDRF                            
081000           MOVE +1              TO MOD-RAD-IND                            
081100                                                                          
081200           MOVE TIBEGPAC-WS     TO MOD-TIBEGPAC      (MOD-RAD-IND)        
081300           MOVE KDORDKL-WS      TO MOD-KDORDKL       (MOD-RAD-IND)        
081400           MOVE IDDISTR-WS      TO MOD-IDDISTR       (MOD-RAD-IND)        
081500           MOVE IDKUNDNR-WS     TO MOD-IDKUNDNR      (MOD-RAD-IND)        
081600           MOVE KDFRAKT-WS      TO MOD-KDFRAKT       (MOD-RAD-IND)        
081700           MOVE IDPRODNR-WS     TO MOD-IDPRODNR      (MOD-RAD-IND)        
081710           MOVE IDPRODNR-WS     TO SPAR-MOD-IDPRODNR (MOD-RAD-IND)        
081800           MOVE KVORDRAD-WS     TO MOD-KVORDRAD      (MOD-RAD-IND)        
081900          MOVE KVORDRAD-PACK-WS TO MOD-KVORDRAD-PACK (MOD-RAD-IND)        
082000           MOVE KVKOLPAC-WS     TO MOD-KVKOLPAC      (MOD-RAD-IND)        
082100                                                                          
082200           MOVE TIUTSKR-WS      TO MOD-TIUTSKR       (MOD-RAD-IND)        
082300           MOVE WS-IDORDNR      TO MOD-IDORDNR       (MOD-RAD-IND)        
082400                                                                          
082500           MOVE ZERO            TO  TIBEGPAC-WS                           
082600                                    KDORDKL-WS                            
082700                                    IDDISTR-WS                            
082800                                    IDKUNDNR-WS                           
082900                                    KDFRAKT-WS                            
083000                                    IDPRODNR-WS                           
083100                                    KVORDRAD-WS                           
083200                                    KVORDRAD-PACK-WS                      
083300                                    KVKOLPAC-WS                           
083400                                    TIUTSKR-WS                            
083500        ELSE                                                              
083600           MOVE 4                TO WS-KDFEL                              
083700        END-IF                                                            
083800     ELSE                                                                 
083900        MOVE 4                   TO WS-KDFEL                              
084000     END-IF                                                               
084010                                                                          
084020     MOVE '002' TO MSGI-KDCALL                                            
084030     MOVE '4324' TO SPAR-IDTRANS                                          
084040     MOVE 4324-SPAR-AREA TO MSGI-SPAR-AREA                                
084050     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
084100     EJECT                                                                
084200     .                                                                    
084300 O-RETURN-FROM-4312 SECTION.                                              
084400                                                                          
084500     CONTINUE                                                             
084700     .                                                                    
084800     EJECT                                                                
084900                                                                          
085000 P-SPLIT-TO-W40312 SECTION.                                               
085100                                                                          
085200     MOVE +1 TO INDX                                                      
085300     PERFORM UNTIL INDX > MAX-LINE                                        
085310                OR KDCMD-SW = 'J'                                         
085400       IF MID-KDCMD (INDX) NOT = ALL '+'                                  
086200           MOVE JA TO KDCMD-SW                                            
086310           IF MID-KDCMD (INDX) = 'S'                                      
086400             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR (INDX)           
086500             PERFORM PA-BUILD-4312                                        
086511             MOVE  WS-KDFEL  TO WS-KDFEL-9                                
086600           ELSE                                                           
086610             IF MID-KDCMD (INDX) = '+'                                    
086611               MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR (INDX)         
086620             ELSE                                                         
086700               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
086800               CALL WMEDKONV USING MED-WMEDAREA                           
086900               MOVE MED-MFSFEL TO MOD-TEMFSFEL                            
087000               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR (INDX)           
087200               MOVE 5 TO WS-KDFEL                                         
087201                                                                          
087210*               STRING 'MID-KDCMD (INDX):'                                
087211*                       MID-KDCMD (INDX)                                  
087220*               DELIMITED BY SIZE INTO MOD-TEMFSINF                       
087292             END-IF                                                       
087300           END-IF                                                         
087500       END-IF                                                             
087600       ADD 1 TO INDX                                                      
087700     END-PERFORM                                                          
087800                                                                          
088600     IF INDATA-FEL                                                        
088800       PERFORM K-VISA-BILD-IGEN                                           
088900     END-IF                                                               
089000     .                                                                    
089100     EJECT                                                                
089200                                                                          
089300 PA-BUILD-4312  SECTION.                                                  
089400                                                                          
089500     MOVE ALL '+'                TO PTOP-MID-W4I31201                     
089600                                                                          
089700     IF SPAR-MOD-IDPRODNR(INDX) NUMERIC                                   
089800       IF SPAR-MOD-IDPRODNR(INDX) > ZERO                                  
090000                                                                          
090009         MOVE SPAR-MOD-IDPRODNR(INDX)   TO PTOP-MID-IDPRODNR-IN           
090010         MOVE MOD-KDPRCGRP-UT      TO PTOP-MID-KDPRCGRP-4324              
090011                                                                          
090020         MOVE LOW-VALUE            TO PTOP-Z1                             
090100         MOVE LOW-VALUE            TO PTOP-Z2                             
090200         MOVE 'W4T312  '           TO PTOP-KDTRANS                        
090300         MOVE '4324'               TO PTOP-IDTRANS                        
090400         MOVE MFS-KDMFSFOR         TO PTOP-KDMFSFOR                       
090500       ELSE                                                               
090620         MOVE 5 TO WS-KDFEL                                               
090700       END-IF                                                             
090800     ELSE                                                                 
090920       MOVE 5 TO WS-KDFEL                                                 
091000     END-IF                                                               
091100     .                                                                    
091200     EJECT                                                                
091300                                                                          
091400 S01-FLYTTA-PLOC-TILL-RAD SECTION.                                        
091500     MOVE KORD-KDORDKL      TO KDORDKL-WS                                 
091600     MOVE KORD-IDDISTR      TO IDDISTR-WS                                 
091700     MOVE KORD-IDKUNDNR     TO IDKUNDNR-WS                                
091800     MOVE KORD-IDKUNDRF     TO WS-IDKUNDRF                                
091900     MOVE KORD-KDFRAKT      TO KDFRAKT-WS                                 
092000     MOVE KORD-IDPRODNR     TO IDPRODNR-WS                                
092100                               WS-IDPRODNR-NYCKEL                         
092200*    IF CDC                                                               
092300*       COMPUTE WS-KVORDRAD = KORD-KVORDRAD-C1 +                          
092400*                             KORD-KVORDRAD-LEVPLC1                       
092500*    ELSE                                                                 
092600*       COMPUTE WS-KVORDRAD = KORD-KVORDRAD-C2 +                          
092700*                             KORD-KVORDRAD-LEVPLC2                       
092800*    END-IF                                                               
092900*    ADD WS-KVORDRAD        TO KVORDRAD-WS                                
093000                                                                          
093100     MOVE WS-SAVE-DARFS (3:6) TO WS-DATUM-TIRFS                           
093200                                                                          
093300     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
093400     MOVE WS-DATUM-TIRFS    TO DAT-I-TIDATUM                              
093500     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
093600                         DAT-O-TIDATUM DAT-KDSVAR                         
093700     IF DAT-KDSVAR-OK                                                     
093800       MOVE DAT-TIAAVVD     TO TIBEGPAC-WS                                
093900     END-IF                                                               
094000*    MOVE KORD-TISTADAT     TO WS-DATUM                                   
094100*    MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
094200*    MOVE WS-DATUM          TO DAT-I-TIDATUM                              
094300*    CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
094400*                        DAT-O-TIDATUM DAT-KDSVAR                         
094500*    IF DAT-KDSVAR-OK                                                     
094600*      MOVE DAT-TIAAVVD      TO MOD-TISTADAT     (MOD-RAD-IND)            
094700*    END-IF                                                               
094800     .                                                                    
094900     EJECT                                                                
095000 S02-BLANKA-RADER SECTION.                                                
095100     MOVE 1 TO MOD-RAD-IND                                                
095200     PERFORM UNTIL MOD-RAD-IND > MAX-LINE                                 
095300       MOVE MFS-RENSA-FAELT TO MOD-TIBEGPAC      (MOD-RAD-IND)            
095400                               MOD-KDORDKL       (MOD-RAD-IND)            
095500                               MOD-IDDISTR       (MOD-RAD-IND)            
095600                               MOD-IDKUNDNR      (MOD-RAD-IND)            
095700                               MOD-IDORDNR       (MOD-RAD-IND)            
095800                               MOD-KDFRAKT       (MOD-RAD-IND)            
095900                               MOD-IDPRODNR      (MOD-RAD-IND)            
096000                               MOD-KVORDRAD      (MOD-RAD-IND)            
096100                               MOD-KVORDRAD-PACK (MOD-RAD-IND)            
096200                               MOD-KVKOLPAC      (MOD-RAD-IND)            
096300                               MOD-TIUTSKR       (MOD-RAD-IND)            
096400                               MOD-TISTADAT      (MOD-RAD-IND)            
096500       ADD 1 TO MOD-RAD-IND                                               
096600     END-PERFORM                                                          
096700     .                                                                    
096800     EJECT                                                                
096900 S03-ADDERA-KVRADER SECTION.                                              
097000                                                                          
097100        COMPUTE WS-KVORDRAD = KORD-KVORDRAD +                             
097200                              KORD-KVORDRAD-LEVPL                         
097300     ADD WS-KVORDRAD        TO KVORDRAD-WS                                
097400     .                                                                    
097500     EJECT                                                                
097600* IMS SEKTIONER                                                           
097700     SKIP3                                                                
097800                                                                          
097900 IMS-GET-MSG SECTION.                                                     
098000                                                                          
098100     MOVE '  QC' TO GODK-STATUSKODER                                      
098200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
098300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
098400     PERFORM IMS-STATUSKONTROLL                                           
098500     SKIP3                                                                
098600     .                                                                    
098700 IMS-INSERT-MSG SECTION.                                                  
098800                                                                          
098900     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
099000       MOVE '0' TO MFS-KDHUVOMR                                           
099100     END-IF                                                               
099200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
099300     MOVE SPACE TO GODK-STATUSKODER                                       
099400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
099500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
099600     PERFORM IMS-STATUSKONTROLL                                           
099700     .                                                                    
099800     EJECT                                                                
099810 IMS-ISRT-ALT-MSG-4312 SECTION.                                           
099820                                                                          
099830     MOVE SPACE TO GODK-STATUSKODER                                       
099840     CALL CBLTDLI USING ISRT ALT-PCB P-TO-P-SW-4312                       
099850     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
099860     PERFORM IMS-STATUSKONTROLL                                           
099870     .                                                                    
099880     SKIP3                                                                
099900 IMS-GU-KUNDORDER-SEK-INV      SECTION.                                   
100000                                                                          
100100     STRING 'WDE411  (WDE4BSEQ>=' W-WDE411-KEYSEQ-MIN-X                   
100200                    '&WDE4BSEQ<=' W-WDE411-KEYSEQ-MAX-X ')'               
100300            DELIMITED BY SIZE INTO SSA1                                   
100400     MOVE 'WDE401   ' TO SSA2                                             
100500     MOVE '  '     TO GODK-STATUSKODER                                    
100600     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-AREA2 SSA1 SSA2                
100700     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
100800     PERFORM IMS-STATUSKONTROLL                                           
100900     .                                                                    
101000     EJECT                                                                
101100 IMS-GET-WDE601   SECTION.                                                
101200                                                                          
101300     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
101400            DELIMITED BY SIZE INTO SSA1                                   
101500     MOVE '  GE' TO GODK-STATUSKODER                                      
101600     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-AREA SSA1                      
101700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
101800     PERFORM IMS-STATUSKONTROLL                                           
101900     .                                                                    
102000     EJECT                                                                
102100 IMS-GU-WDGX4488  SECTION.                                                
102200                                                                          
102300     STRING 'WDR401  (WDGXKEY  =' W-4487-KEY-X ')'                        
102400            DELIMITED BY SIZE INTO SSA1                                   
102500     STRING 'WDGX4488(KDPRCGRP =' W-4488-KEY-X ')'                        
102600            DELIMITED BY SIZE INTO SSA2                                   
102700     MOVE '  GE' TO GODK-STATUSKODER                                      
102800     CALL CBLTDLI USING GU 4487-PCB DLI-IO-AREA3 SSA1 SSA2                
102900     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
103000     PERFORM IMS-STATUSKONTROLL                                           
103100     SKIP3                                                                
103200     .                                                                    
103300 IMS-GU-WDGX4490      SECTION.                                            
103400                                                                          
103500     STRING 'WDR401  (WDGXKEY  =' W-4487-KEY-X ')'                        
103600            DELIMITED BY SIZE INTO SSA1                                   
103700     STRING 'WDGX4488(KDPRCGRP =' W-4488-KEY-X ')'                        
103800            DELIMITED BY SIZE INTO SSA2                                   
103900     STRING 'WDGX4490(KY4490  >=' W-4490-KEY-X ')'                        
104000            DELIMITED BY SIZE INTO SSA3                                   
104100     MOVE '  GE'   TO GODK-STATUSKODER                                    
104200     CALL CBLTDLI USING GNP 4487-PCB DLI-IO-AREA3 SSA1 SSA2 SSA3          
104300     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
104400     PERFORM IMS-STATUSKONTROLL                                           
104500     SKIP3                                                                
104600     .                                                                    
104700 IMS-GNP-WDGX4490       SECTION.                                          
104800                                                                          
104900     STRING 'WDR401  (WDGXKEY  =' W-4487-KEY-X ')'                        
105000            DELIMITED BY SIZE INTO SSA1                                   
105100     STRING 'WDGX4488(KDPRCGRP =' W-4488-KEY-X ')'                        
105200            DELIMITED BY SIZE INTO SSA2                                   
105300     MOVE 'WDGX4490 ' TO SSA3                                             
105400     MOVE '  GE' TO GODK-STATUSKODER                                      
105500     CALL CBLTDLI USING GNP 4487-PCB DLI-IO-AREA3 SSA1 SSA2 SSA3          
105600     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
105700     PERFORM IMS-STATUSKONTROLL                                           
105800     .                                                                    
105900     EJECT                                                                
106000 IMS-GU-KUNDORDER SECTION.                                                
106100                                                                          
106200     STRING 'WDE401  (WDE401KY =' W-WDE401-KORD-X ')'                     
106300            DELIMITED BY SIZE INTO SSA1                                   
106400     MOVE '  GE' TO GODK-STATUSKODER                                      
106500     CALL CBLTDLI USING GU WDE42-PCB DLI-IO-AREA2 SSA1                    
106600     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
106700     PERFORM IMS-STATUSKONTROLL                                           
106800     .                                                                    
106900     EJECT                                                                
107000 IMS-STATUSKONTROLL SECTION.                                              
107100                                                                          
107200     SET STATUS-IX TO 1                                                   
107300     SEARCH GODK-STATUS AT END CALL FELLOG                                
107400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
107500     END-SEARCH                                                           
107600     CONTINUE                                                             
107700     .                                                                    
