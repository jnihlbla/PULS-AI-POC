000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6110400.                                                
000400*AUTHOR.         MÅNS SAMUELSSON.                                         
000500*DATE-WRITTEN.   94/09/29.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER     R33, R40 POSTER FRÅN FIL,                              
001100*        KONTROLLERAR OCH SKICKAR TRANSAR TILL RESPEKTIVE                 
001200*        MPP                                                              
001300*                                                                         
001400*        ÄNDRAT NOV. -95; PROGRAMMET FÅR INTE LÄNGRE IN                   
001500*        R34-TRANSAR FRÅN PROGRAM W61199.                                 
001600*                                                                         
001700*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001800*        PROGRAMMET UPPDATERAR WLZZAC (WDG6)                              
001900*                                                                         
002000*    ABENDKODER:                                                          
002100*        U0016 -  . . . .                                                 
002200*        U1000 -  . . . .                                                 
002300*                                                                         
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003100     SKIP2                                                                
003200*          ---     R33,R34 OCH R40 POSTER                                 
003300     SELECT W61104                     ASSIGN TO W61104D1.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900 FD  W61104                                                               
004000     RECORDING       V                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300*01  -COPY W211R33      -L.                                               
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600     SKIP2                                                                
004700*    -COPY WY2000W1                                                       
004800*    -COPY WY2000W9                                                       
004900     SKIP3                                                                
005000 77  IDPGM                       PIC X(8)    VALUE 'W6110400'.            
005100 01  CHKP-VAR.                                                            
005200 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
005300 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005400 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005500 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005600 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005700 03  CHKP-MAX                    PIC S9(3)   VALUE +23.                   
005800 01  LNG-P-TO-P-PREFIX           PIC S9(4)   VALUE +17 COMP SYNC.         
005900 77  611B-IX                     PIC S9(9)   VALUE +0 COMP SYNC.          
006000 77  611C-IX                     PIC S9(9)   VALUE +0 COMP SYNC.          
006100 77  611D-IX                     PIC S9(9)   VALUE +0 COMP SYNC.          
006200 77  611B-MAX                    PIC S9(9)   VALUE +8  COMP SYNC.         
006300 77  611C-MAX                    PIC S9(9)   VALUE +9  COMP SYNC.         
006400 77  611D-MAX                    PIC S9(9)   VALUE +22 COMP SYNC.         
006500 77  JA                          PIC X       VALUE 'J'.                   
006600 77  NEJ                         PIC X       VALUE 'N'.                   
006700 77  W-IDLOGLOP                  PIC S9(1)   VALUE +0.                    
006800 77  W-W61104-KVPOST-IN          PIC S9(9)   VALUE +0 COMP-3.             
006900     SKIP2                                                                
007000 01  FELTEXT.                                                             
007100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007300                                                                          
007400 77  W61104-EOF-SW               PIC X       VALUE 'N'.                   
007500     88  END-OF-W61104                       VALUE 'J'.                   
007600 77  OK-SW                       PIC X       VALUE 'J'.                   
007700     88  OK                                  VALUE 'J'.                   
007800 77  FOERSTA-611B-SW             PIC X       VALUE 'J'.                   
007900     88  FOERSTA-611B                        VALUE 'J'.                   
008000                                                                          
008100 77  FOERSTA-611C-SW             PIC X       VALUE 'J'.                   
008200     88  FOERSTA-611C                        VALUE 'J'.                   
008300                                                                          
008400     EJECT                                                                
008500 01  W-SORT-AREA.                                                         
008600     03  W-SORT-IDPTYP           PIC X(3).                                
008700     03  FILLER                  PIC X(10).                               
008800     03  W-SORT-KDCLAGER         PIC S9(1).                               
008900     03  FILLER                  PIC X(8).                                
009000     03  W-SORT-SORTBGP          PIC S9(8).                               
009100     03  W-SORT-KDFELMRK         PIC S9(1).                               
009200     03  FILLER                  PIC X(3).                                
009300     03  W-SORT-BLANK            PIC X(2).                                
009400     SKIP3                                                                
009500 01  W-W092-LOGGPOST.                                                     
009600     03 W-IDPTYP                 PIC X(3).                                
009700     03 W-LOGGPOST               PIC X(87).                               
009800     EJECT                                                                
009900*      --- VALID IDDC CODES                                               
010000*                                                                         
010100*01    -COPY WWDC99                                                       
010200*01    -COPY WWDCKONS                                                     
010300       EJECT                                                              
010400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
010500 01  FILLER REDEFINES DAGENS-DATUM.                                       
010600     03  DAGENS-AA               PIC 9(2).                                
010700     03  DAGENS-MM               PIC 9(2).                                
010800     03  DAGENS-DD               PIC 9(2).                                
010900                                                                          
011000 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
011100                                                                          
011200 01  TEST-DATUM                PIC 9(6)    VALUE ZERO.                    
011300 01  FILLER REDEFINES TEST-DATUM.                                         
011400     03  TEST-AA                 PIC 9(2).                                
011500     03  TEST-MM                 PIC 9(2).                                
011600     03  TEST-DD                 PIC 9(2).                                
011700                                                                          
011800 01  DYNAMISKA-SUBPROGRAM.                                                
011900*                                                                         
012000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
012300     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
012400     EJECT                                                                
012500*    --- PARAMETRAR TILL POSTSUM                                          
012600*                                                                         
012700*01  -COPY W0005   -PRE  POSTSUM-                                         
012800     EJECT                                                                
012900 01  IN-AREA-START               PIC X(24)   VALUE                        
013000                                             'IN-AREA-START'.             
013100 01  IN-AREA    -COPY W211R33 -L.                                         
013200 01  FILLER     REDEFINES IN-AREA.                                        
013300     03    -COPY W211R33 -PRE R33-                                        
013400*                                                                         
013500 01  FILLER     REDEFINES IN-AREA.                                        
013600     03    -COPY W211R40 -PRE R40-                                        
013700*                                                                         
013800     EJECT                                                                
013900 01  FEL-MED-AREA-START          PIC X(24)   VALUE                        
014000                                             'FEL MED AREA '.             
014100 01  FELMED-AREA                 PIC X(80).                               
014200 01  F33-AREA REDEFINES FELMED-AREA.                                      
014300*    03 -COPY W211F33 -PRE F33-                                           
014400*                                                                         
014500 01  F40-AREA REDEFINES FELMED-AREA.                                      
014600*    03 -COPY W211F40 -PRE F40-                                           
014700*                                                                         
014800     EJECT                                                                
014900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015000     SKIP3                                                                
015100 01  NYCKLAR-TILL-DLI.                                                    
015200     03  W-IDARTNR-X.                                                     
015300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
015400     03  W-WDG6KEY-X.                                                     
015500         05  W-WDG6KEY           PIC X(18)    VALUE SPACE.                
015600     03  W-KDSEGKEY-X.                                                    
015700         05  W-KDSGKEY           PIC X(1)     VALUE '1'.                  
015800     03  W-W6GX-6027-X.                                                   
015900         05  W-6027-IDHTYP       PIC X(4)     VALUE '6027'.               
016000         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
016100     SKIP2                                                                
016200*    --- STATUS-KOD FRÅN IMS                                              
016300 01  STATUS-WS                   PIC XX.                                  
016400     88  SEGMENT-FINNS                       VALUE '  '.                  
016500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
016800     88  IMS-EJ-OK                           VALUE 'XD'.                  
016900     SKIP2                                                                
017000 01  GODK-STATUSKODER.                                                    
017100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017200     SKIP3                                                                
017300 01  SSA1                        PIC X(64).                               
017400 01  SSA2                        PIC X(64).                               
017500     EJECT                                                                
017600*    --- IMS FUNKTIONSKODER                                               
017700*01  -COPY W0003                                                          
017800     EJECT                                                                
017900*    ---  DLI INPUT-OUTPUT AREA                                           
018000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
018100     SKIP3                                                                
018200 01  DLI-IO-AREA.                                                         
018300     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
018400     SKIP3                                                                
018500     03  WLARTC01 REDEFINES IO-AREA.                                      
018600*        05  -COPY WDK601  -PRE ARTC01-                                   
018700     SKIP3                                                                
018800     03  WLARTC11 REDEFINES IO-AREA.                                      
018900*        05  -COPY WDK611  -PRE ARTC11-                                   
019000     SKIP3                                                                
019100     03  WLZZAC01 REDEFINES IO-AREA.                                      
019200*        05  -COPY WDG601  -PRE ZZAC01-                                   
019300     SKIP3                                                                
019400     03  W6GX6028 REDEFINES IO-AREA.                                      
019500*        05  -COPY W6GX6028                                               
019600 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
019700     SKIP3                                                                
019800 01  KOM-MSG-IO-AREA.                                                     
019900*03  -COPY WMSGKOM                                                        
020000     EJECT                                                                
020100 01  FILLER -COPY WMSGSNUF -PRE SNUF-                                     
020200     EJECT                                                                
020300 01  MID611B-START               PIC X(16) VALUE                          
020400                                      'MID611B-START'.                    
020500     -COPY W6I11B01 -PRE MOD611B-                                         
020600     EJECT                                                                
020700 01  MID611C-START               PIC X(16) VALUE                          
020800                                      'MID611C-START'.                    
020900     -COPY W6I11C01 -PRE MOD611C-                                         
021000     EJECT                                                                
021100 01  MID611D-START               PIC X(16) VALUE                          
021200                                      'MID611D-START'.                    
021300     -COPY W6I11D01 -PRE MOD611D-                                         
021400     EJECT                                                                
021500 LINKAGE SECTION.                                                         
021600                                                                          
021700*01  -COPY W0009   -PRE MSG-                                              
021800     EJECT                                                                
021900*01  -COPY W0009   -PRE DISP-                                             
022000     EJECT                                                                
022100*01  -COPY W0008   -PRE KOMA-                                             
022200     05  FILLER                  PIC X.                                   
022300     EJECT                                                                
022400*01  -COPY W0008  -PRE ARTC-                                              
022500     05  FILLER                  PIC X.                                   
022600     EJECT                                                                
022700*01  -COPY W0008  -PRE ZZAC-                                              
022800     05  FILLER                  PIC X.                                   
022900     EJECT                                                                
023000*01  -COPY W0008  -PRE CKPG-                                              
023100     05  FILLER                  PIC X.                                   
023200     EJECT                                                                
023300 PROCEDURE DIVISION  USING MSG-PCB DISP-PCB KOMA-PCB                      
023400                           ARTC-PCB ZZAC-PCB CKPG-PCB.                    
023500     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB KOMA-PCB                      
023600                           ARTC-PCB ZZAC-PCB CKPG-PCB.                    
023700                                                                          
023800     SKIP2                                                                
023900     PERFORM A-INIT                                                       
024000     PERFORM S01-LAES-W61104                                              
024100     PERFORM UNTIL END-OF-W61104                                          
024200       IF CHKP-ANT > CHKP-MAX                                             
024300         PERFORM X-TAG-CHECKPOINT                                         
024400       END-IF                                                             
024500       PERFORM B-KONTROLLERA-POST                                         
024600       IF OK                                                              
024700         PERFORM C-SKICKA-TRANS                                           
024800       END-IF                                                             
024900       PERFORM S01-LAES-W61104                                            
025000     END-PERFORM                                                          
025100                                                                          
025200     IF 611B-IX > +0                                                      
025300       PERFORM S03-STARTA-R33-TRANS                                       
025400     END-IF                                                               
025500     IF 611C-IX > +0                                                      
025600       PERFORM S04-STARTA-R34-TRANS                                       
025700     END-IF                                                               
025800     IF 611D-IX > +0                                                      
025900       PERFORM S05-STARTA-R40-TRANS                                       
026000     END-IF                                                               
026100                                                                          
026200     PERFORM Z-FINIT                                                      
026300                                                                          
026400     MOVE ZERO TO RETURN-CODE                                             
026500     GOBACK                                                               
026600     .                                                                    
026700     EJECT                                                                
026800 A-INIT SECTION.                                                          
026900     SKIP2                                                                
027000                                                                          
027100     PERFORM IMS-RESTART                                                  
027200                                                                          
027300     OPEN INPUT W61104                                                    
027400                                                                          
027500     PERFORM IMS-LAS-ATERSTART                                            
027600                                                                          
027700     IF SEGMENT-FINNS                                                     
027800       IF 6028-KVPOST > +0                                                
027900         PERFORM S01-LAES-W61104                                          
028000         PERFORM UNTIL W-W61104-KVPOST-IN = 6028-KVPOST                   
028100           PERFORM S01-LAES-W61104                                        
028200         END-PERFORM                                                      
028300       END-IF                                                             
028400     END-IF                                                               
028500                                                                          
028600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
028700     ACCEPT DAGENS-DATUM FROM DATE                                        
028800     .                                                                    
028900     EJECT                                                                
029000 B-KONTROLLERA-POST SECTION.                                              
029100                                                                          
029200     MOVE JA        TO OK-SW                                              
029300     MOVE R33-IDARTNR      TO W-IDARTNR                                   
029400     PERFORM IMS-GU-ARTC01                                                
029500     IF SEGMENT-SAKNAS                                                    
029600       PERFORM S11-TRANS-FP110                                            
029700       MOVE NEJ    TO OK-SW                                               
029800     ELSE                                                                 
029900       IF ARTC01-ART-KDERS-UTG > 20                                       
030000         PERFORM S11-TRANS-FP110                                          
030100         MOVE NEJ    TO OK-SW                                             
030200       ELSE                                                               
030300         PERFORM IMS-GNP-ARTC11                                           
030400         IF SEGMENT-SAKNAS                                                
030500           PERFORM S11-TRANS-FP110                                        
030600           MOVE NEJ    TO OK-SW                                           
030700         ELSE                                                             
030800           IF ARTC11-CLAG-PRARTSTD = +0                                   
030900             PERFORM S12-TRANS-FP118                                      
031000             MOVE NEJ    TO OK-SW                                         
031100           ELSE                                                           
031200             IF R33-IDPTYP = 'R40'                                        
031300               CONTINUE                                                   
031400             ELSE                                                         
031500               PERFORM BA-KONTROLL-AVSDAT                                 
031600             END-IF                                                       
031700           END-IF                                                         
031800         END-IF                                                           
031900       END-IF                                                             
032000     END-IF                                                               
032100     IF OK                                                                
032200       EVALUATE R33-IDPTYP                                                
032300         WHEN 'R34'                                                       
032400           PERFORM BB-R34-KONTROLLER                                      
032500         WHEN 'R40'                                                       
032600           PERFORM BC-R40-KONTROLLER                                      
032700       END-EVALUATE                                                       
032800     END-IF                                                               
032900     .                                                                    
033000     EJECT                                                                
033100 BA-KONTROLL-AVSDAT SECTION.                                              
033200                                                                          
033300     MOVE R33-TIAVIDAT   TO TEST-DATUM                                    
033400     MOVE TEST-DATUM     TO TMP1-YYMMDD                                   
033500     MOVE DAGENS-DATUM   TO TMP2-YYMMDD                                   
033600     MOVE TEST-AA        TO TMP1-YY                                       
033700     MOVE DAGENS-AA      TO TMP2-YY                                       
033800     PERFORM WY2000P1                                                     
033900     PERFORM WY2000P9                                                     
034000     IF TMP1-YYMMDD > TMP2-YYMMDD   OR                                    
034100        TEST-MM      > 12           OR                                    
034200        TEST-DD      > 31           OR                                    
034300*       TEST-AA      < DAGENS-AA - 1                                      
034400        TMP1-YY      < TMP2-YY   - 1                                      
034500       PERFORM S13-TRANS-FP116                                            
034600        MOVE NEJ    TO OK-SW                                              
034700     END-IF                                                               
034800     .                                                                    
034900     EJECT                                                                
035000 BB-R34-KONTROLLER SECTION.                                               
035100                                                                          
035200     EVALUATE R33-KDRT                                                    
035300       WHEN 0                                                             
035400          IF R33-IDLEVNR-INL = SPACE OR '1000 ' OR '1004 '   OR           
035500                               '1012 ' OR '1013 '            OR           
035600                               '1021 ' OR '2120 '            OR           
035700                               'BP2TD' OR 'BP2TC'            OR           
035800             R33-IDKONTO > 0                                              
035900               PERFORM S15-TRANS-FP134                                    
036000               MOVE NEJ    TO OK-SW                                       
036100          END-IF                                                          
036200       WHEN 6                                                             
036300          IF R33-IDLEVNR-INL = SPACE OR '8265 ' OR '9999 '                
036400            CONTINUE                                                      
036500          ELSE                                                            
036600            PERFORM S15-TRANS-FP134                                       
036700            MOVE NEJ    TO OK-SW                                          
036800          END-IF                                                          
036900       WHEN 7                                                             
037000          IF R33-IDLEVNR-INL = SPACE OR                                   
037100             R33-IDKONTO > +0                                             
037200            PERFORM S15-TRANS-FP134                                       
037300            MOVE NEJ    TO OK-SW                                          
037400          END-IF                                                          
037500       WHEN OTHER                                                         
037600          EVALUATE R33-KDRT                                               
037700            WHEN 1                                                        
037800               IF (R33-IDLEVNR-INL = '1001 ' OR 'BL3YA') AND              
037900                  R33-IDKONTO = +0                                        
038000                 CONTINUE                                                 
038100               ELSE                                                       
038200                 PERFORM S15-TRANS-FP134                                  
038300                 MOVE NEJ    TO OK-SW                                     
038400               END-IF                                                     
038500            WHEN 2                                                        
038600             IF (R33-IDLEVNR-INL = '1003 ' OR '1012 ' OR 'BP2TH'          
038700                         OR 'BP2TC' OR '1013 ' OR '2120 ')  AND           
038800                R33-IDKONTO = +0                                          
038900               CONTINUE                                                   
039000             ELSE                                                         
039100               PERFORM S15-TRANS-FP134                                    
039200               MOVE NEJ    TO OK-SW                                       
039300             END-IF                                                       
039400            WHEN 4                                                        
039500             IF R33-IDLEVNR-INL = '1000 '                                 
039600               CONTINUE                                                   
039700             ELSE                                                         
039800               PERFORM S15-TRANS-FP134                                    
039900               MOVE NEJ    TO OK-SW                                       
040000             END-IF                                                       
040100            WHEN 5                                                        
040200             IF R33-IDLEVNR-INL = '1004 ' OR 'BP2TD'                      
040300               CONTINUE                                                   
040400             ELSE                                                         
040500               PERFORM S15-TRANS-FP134                                    
040600               MOVE NEJ    TO OK-SW                                       
040700             END-IF                                                       
040800            WHEN 9                                                        
040900             IF (R33-IDLEVNR-INL = SPACE OR '1000 ' OR                    
041000                                '1002 ' OR '1004 ' OR 'BP2TD') OR         
041100                R33-IDKONTO > +0                                          
041200               CONTINUE                                                   
041300             ELSE                                                         
041400               PERFORM S15-TRANS-FP134                                    
041500               MOVE NEJ    TO OK-SW                                       
041600             END-IF                                                       
041700          END-EVALUATE                                                    
041800     END-EVALUATE                                                         
041900     .                                                                    
042000     EJECT                                                                
042100 BC-R40-KONTROLLER SECTION.                                               
042200                                                                          
042300     IF R40-IDLEVNR     = '1000 ' OR '1002 ' OR '1004 ' OR 'BP2TD'        
042400        PERFORM S14-TRANS-FP131                                           
042500        MOVE NEJ    TO OK-SW                                              
042600     ELSE                                                                 
042700        IF R40-IDLEVNR     = '1001 ' OR '1003 ' OR '1012 ' OR             
042800                             '1013 ' OR '2120 ' OR 'BL3YA' OR             
042900                             'BP2TH' OR 'BP2TC'                           
043000           IF R40-IDORDNR = ZERO                                          
043100              PERFORM S14-TRANS-FP131                                     
043200              MOVE NEJ    TO OK-SW                                        
043300           END-IF                                                         
043400        END-IF                                                            
043500     END-IF                                                               
043600     .                                                                    
043700     EJECT                                                                
043800 C-SKICKA-TRANS SECTION.                                                  
043900                                                                          
044000     IF R33-IDPTYP = 'R33'                                                
044100        ADD +1       TO 611B-IX                                           
044200        MOVE R33-IDLEVNR-INL  TO MOD611B-MID-IDLEVNR  (611B-IX)           
044300        MOVE R33-IDAVINR      TO MOD611B-MID-IDAVINR  (611B-IX)           
044400        MOVE R33-TIAVIDAT     TO MOD611B-MID-TIAVIDAT (611B-IX)           
044500        MOVE R33-IDARTNR      TO MOD611B-MID-IDARTNR  (611B-IX)           
044600        MOVE R33-KVAVIS       TO MOD611B-MID-KVAVIS   (611B-IX)           
044700        MOVE R33-KDRT         TO MOD611B-MID-KDRT     (611B-IX)           
044800        MOVE R33-IDKONTO      TO MOD611B-MID-IDKONTO  (611B-IX)           
044900        MOVE R33-IDKST        TO MOD611B-MID-IDKST    (611B-IX)           
045000        MOVE R33-IDANALYS     TO MOD611B-MID-IDANALYS (611B-IX)           
045100        MOVE R33-IDDC         TO MOD611B-MID-IDDC     (611B-IX)           
045200        MOVE R33-IDDISTR      TO MOD611B-MID-IDDISTR  (611B-IX)           
045300        MOVE R33-IDKUNDNR     TO MOD611B-MID-IDKUNDNR (611B-IX)           
045400        MOVE R33-IDKUNDRF     TO MOD611B-MID-IDKUNDRF (611B-IX)           
045500        MOVE R33-IDPRODNR     TO MOD611B-MID-IDPRODNR (611B-IX)           
045600        MOVE R33-IDFAKT       TO MOD611B-MID-IDFAKT   (611B-IX)           
045700        MOVE R33-IDSUPREF     TO MOD611B-MID-IDSUPREF (611B-IX)           
045800        IF 611B-IX = 611B-MAX                                             
045900          PERFORM S03-STARTA-R33-TRANS                                    
046000        END-IF                                                            
046100     ELSE                                                                 
046200       IF R33-IDPTYP = 'R34'                                              
046300         ADD +1       TO 611C-IX                                          
046400         MOVE R33-IDLEVNR-INL TO MOD611C-MID-IDLEVNR  (611C-IX)           
046500         MOVE R33-IDAVINR     TO MOD611C-MID-IDAVINR  (611C-IX)           
046600         MOVE R33-TIAVIDAT    TO MOD611C-MID-TIAVIDAT (611C-IX)           
046700         MOVE R33-IDARTNR     TO MOD611C-MID-IDARTNR  (611C-IX)           
046800         MOVE R33-KVAVIS      TO MOD611C-MID-KVAVIS   (611C-IX)           
046900         MOVE R33-KDRT        TO MOD611C-MID-KDRT     (611C-IX)           
047000         MOVE R33-IDKONTO     TO MOD611C-MID-IDKONTO  (611C-IX)           
047100         MOVE R33-IDKST       TO MOD611C-MID-IDKST    (611C-IX)           
047200         MOVE R33-IDANALYS    TO MOD611C-MID-IDANALYS (611C-IX)           
047300         MOVE ZERO            TO                                          
047400                             MOD611C-MID-IDARTNR-FROM (611C-IX)           
047500         IF 611C-IX = 611C-MAX                                            
047600           PERFORM S04-STARTA-R34-TRANS                                   
047700         END-IF                                                           
047800       ELSE                                                               
047900         IF R33-IDPTYP = 'R40'                                            
048000           ADD +1       TO 611D-IX                                        
048100           MOVE R40-IDLEVNR     TO MOD611D-MID-IDLEVNR (611D-IX)          
048200           MOVE R40-IDORDNR     TO MOD611D-MID-IDORDNR (611D-IX)          
048300           MOVE R40-IDARTNR     TO MOD611D-MID-IDARTNR (611D-IX)          
048400           MOVE R40-KVANTAL     TO MOD611D-MID-KVANTAL (611D-IX)          
048500           MOVE WC-CDC-SE       TO MOD611D-MID-IDDC    (611D-IX)          
048600           MOVE ZERO            TO                                        
048700                           MOD611D-MID-KVART-SKROT-LDC                    
048800           IF 611D-IX = 611D-MAX                                          
048900             PERFORM S05-STARTA-R40-TRANS                                 
049000           END-IF                                                         
049100         END-IF                                                           
049200       END-IF                                                             
049300     END-IF                                                               
049400     .                                                                    
049500     EJECT                                                                
049600 Z-FINIT SECTION.                                                         
049700                                                                          
049800     PERFORM IMS-LAS-ATERSTART                                            
049900     MOVE ZERO                 TO 6028-KVPOST                             
050000     ACCEPT 6028-TIUPPDAT FROM DATE                                       
050100     ACCEPT 6028-TIUPPTID FROM TIME                                       
050200     IF SEGMENT-SAKNAS                                                    
050300       MOVE '1'                TO 6028-KDSEGKEY                           
050400       PERFORM IMS-ISRT-ATERSTART                                         
050500     ELSE                                                                 
050600       PERFORM IMS-REPL-ATERSTART                                         
050700     END-IF                                                               
050800                                                                          
050900     CLOSE W61104                                                         
051000     SKIP2                                                                
051100     MOVE 'S' TO POSTSUM-OPKOD                                            
051200     CALL POSTSUM USING POSTSUM-PARM                                      
051300     .                                                                    
051400     EJECT                                                                
051500 S01-LAES-W61104  SECTION.                                                
051600     SKIP2                                                                
051700     READ W61104 INTO IN-AREA                                             
051800     AT END                                                               
051900        SET END-OF-W61104 TO TRUE                                         
052000                                                                          
052100     NOT AT END                                                           
052200        MOVE 'W61104' TO POSTSUM-FDNAMN                                   
052300        MOVE 'W61104D1' TO POSTSUM-DDNAMN2                                
052400        MOVE R33-IDPTYP TO POSTSUM-TRANSTYP                               
052500        CALL POSTSUM USING POSTSUM-PARM                                   
052600                                                                          
052700        ADD 1 TO W-W61104-KVPOST-IN                                       
052800     END-READ                                                             
052900     .                                                                    
053000     EJECT                                                                
053100 S03-STARTA-R33-TRANS  SECTION.                                           
053200                                                                          
053300     ACCEPT DAGENS-DATUM       FROM DATE                                  
053400     ACCEPT DAGENS-TID         FROM TIME                                  
053500                                                                          
053600     MOVE SPACE                TO MSG-KOM-WMSGKOM                         
053700     MOVE +54                  TO MSG-KOM-KVLL                            
053800     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
053900     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
054000     MOVE SPACE                TO MSG-KOM-KDTRANS                         
054100     MOVE 'W6I11B01'           TO MSG-KOM-IDCPYTXT                        
054200     MOVE 'INLEV   '           TO MSG-KOM-IDSNDNOD                        
054300     MOVE 'W6110400'           TO MSG-KOM-IDSNDJOB                        
054400     MOVE DAGENS-DATUM         TO MSG-KOM-TIREGDAT                        
054500     MOVE DAGENS-TID           TO MSG-KOM-TIKLOCK                         
054600     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
054700                                                                          
054800     MOVE IDPGM              TO MOD611B-MID-IDPGM                         
054900     MOVE 611B-IX            TO MOD611B-MID-KVPOST                        
055000     COMPUTE SNUF-MSG-KVLL      =  LNG-P-TO-P-PREFIX +                    
055100                                   LENGTH OF MOD611B-MID-W6I11B01         
055200     MOVE LOW-VALUE             TO SNUF-MSG-KDZ1                          
055300     MOVE LOW-VALUE             TO SNUF-MSG-KDZ2                          
055400     MOVE 'W6T11BX '            TO SNUF-MSG-KDTRANS                       
055500     MOVE '6100'                TO SNUF-MSG-IDTRANS                       
055600     MOVE '1'                   TO SNUF-MSG-KDMFSFOR                      
055700                                                                          
055800     MOVE MOD611B-MID-W6I11B01  TO SNUF-MSG-INDATA                        
055900                                                                          
056000     CALL W006KOM USING MSG-PCB                                           
056100                        DISP-PCB                                          
056200                        KOMA-PCB                                          
056300                        MSG-KOM-WMSGKOM                                   
056400                        SNUF-MSG-IO-AREA-SNUF                             
056500                                                                          
056600     MOVE +0                   TO 611B-IX                                 
056700     ADD  +1                   TO CHKP-ANT                                
056800     .                                                                    
056900     EJECT                                                                
057000 S04-STARTA-R34-TRANS  SECTION.                                           
057100                                                                          
057200     ACCEPT DAGENS-DATUM       FROM DATE                                  
057300     ACCEPT DAGENS-TID         FROM TIME                                  
057400                                                                          
057500     MOVE SPACE                TO MSG-KOM-WMSGKOM                         
057600     MOVE +54                  TO MSG-KOM-KVLL                            
057700     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
057800     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
057900     MOVE SPACE                TO MSG-KOM-KDTRANS                         
058000     MOVE 'W6I11C01'           TO MSG-KOM-IDCPYTXT                        
058100     MOVE 'INLEV   '           TO MSG-KOM-IDSNDNOD                        
058200     MOVE 'W6110400'           TO MSG-KOM-IDSNDJOB                        
058300     MOVE DAGENS-DATUM         TO MSG-KOM-TIREGDAT                        
058400     MOVE DAGENS-TID           TO MSG-KOM-TIKLOCK                         
058500     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
058600                                                                          
058700     MOVE IDPGM              TO MOD611C-MID-IDPGM                         
058800     MOVE WC-CDC-SE          TO MOD611C-MID-IDDC                          
058900     MOVE 611C-IX            TO MOD611C-MID-KVPOST                        
059000     MOVE 'N'                TO MOD611C-MID-FLSVS                         
059100     COMPUTE SNUF-MSG-KVLL      =  LNG-P-TO-P-PREFIX +                    
059200                                   LENGTH OF MOD611C-MID-W6I11C01         
059300     MOVE 'W6T11CX '            TO SNUF-MSG-KDTRANS                       
059400     MOVE '6100'                TO SNUF-MSG-IDTRANS                       
059500     MOVE '1'                   TO SNUF-MSG-KDMFSFOR                      
059600                                                                          
059700     MOVE MOD611C-MID-W6I11C01  TO SNUF-MSG-INDATA                        
059800                                                                          
059900     CALL W006KOM USING MSG-PCB                                           
060000                        DISP-PCB                                          
060100                        KOMA-PCB                                          
060200                        MSG-KOM-WMSGKOM                                   
060300                        SNUF-MSG-IO-AREA-SNUF                             
060400                                                                          
060500     MOVE +0                   TO 611C-IX                                 
060600     ADD  +1                   TO CHKP-ANT                                
060700     .                                                                    
060800     EJECT                                                                
060900 S05-STARTA-R40-TRANS  SECTION.                                           
061000                                                                          
061100     ACCEPT DAGENS-DATUM       FROM DATE                                  
061200     ACCEPT DAGENS-TID         FROM TIME                                  
061300                                                                          
061400     MOVE SPACE                TO MSG-KOM-WMSGKOM                         
061500     MOVE +54                  TO MSG-KOM-KVLL                            
061600     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
061700     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
061800     MOVE SPACE                TO MSG-KOM-KDTRANS                         
061900     MOVE 'W6I11D01'           TO MSG-KOM-IDCPYTXT                        
062000     MOVE 'INLEV   '           TO MSG-KOM-IDSNDNOD                        
062100     MOVE 'W6110400'           TO MSG-KOM-IDSNDJOB                        
062200     MOVE DAGENS-DATUM         TO MSG-KOM-TIREGDAT                        
062300     MOVE DAGENS-TID           TO MSG-KOM-TIKLOCK                         
062400     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
062500                                                                          
062600     MOVE IDPGM              TO MOD611D-MID-IDPGM                         
062700     MOVE 611D-IX            TO MOD611D-MID-KVPOST                        
062800     COMPUTE SNUF-MSG-KVLL      =  LNG-P-TO-P-PREFIX +                    
062900                                   22 + (MOD611D-MID-KVPOST * 27)         
063000     MOVE 'W6T11DX '            TO SNUF-MSG-KDTRANS                       
063100     MOVE '6100'                TO SNUF-MSG-IDTRANS                       
063200     MOVE '1'                   TO SNUF-MSG-KDMFSFOR                      
063300                                                                          
063400     MOVE MOD611D-MID-W6I11D01  TO SNUF-MSG-INDATA                        
063500                                                                          
063600     CALL W006KOM USING MSG-PCB                                           
063700                        DISP-PCB                                          
063800                        KOMA-PCB                                          
063900                        MSG-KOM-WMSGKOM                                   
064000                        SNUF-MSG-IO-AREA-SNUF                             
064100                                                                          
064200     MOVE +0                   TO 611D-IX                                 
064300     ADD  +1                   TO CHKP-ANT                                
064400     .                                                                    
064500     EJECT                                                                
064600 S11-TRANS-FP110 SECTION.                                                 
064700*                                                                         
064800     MOVE R33-IDPTYP      TO F33-IDPTYP                                   
064900     MOVE R33-IDDC        TO WS-IDDC                                      
065000     IF CDC-SE                                                            
065100        MOVE +1           TO F33-KDCLAGER                                 
065200     ELSE                                                                 
065300        MOVE ZERO         TO F33-KDCLAGER                                 
065400     END-IF                                                               
065500     MOVE R33-IDARTNR     TO F33-IDARTNR                                  
065600     MOVE R33-IDLEVNR-INL TO F33-IDLEVNR-INL                              
065700     MOVE R33-KDRT        TO F33-KDRT                                     
065800     MOVE R33-TIAVIDAT    TO F33-TIAVSDAT                                 
065900     MOVE R33-IDKONTO     TO F33-IDKONTO                                  
066000     MOVE R33-IDAVINR     TO F33-IDAVINR                                  
066100     MOVE R33-KVAVIS      TO F33-KVAVIS                                   
066200*                                                                         
066300     MOVE F33-W211F33    TO W-LOGGPOST                                    
066400                                                                          
066500     MOVE '092'              TO  W-IDPTYP                                 
066600     MOVE W-W092-LOGGPOST TO ZZAC01-LOGGPOST                              
066700*                                                                         
066800     PERFORM  S21-SKAPA-FELTRANS                                          
066900     .                                                                    
067000     EJECT                                                                
067100 S12-TRANS-FP118 SECTION.                                                 
067200*                                                                         
067300     MOVE '   STD-PRIS SAKNAS.'  TO W-LOGGPOST                            
067400                                                                          
067500     MOVE '092'              TO  W-IDPTYP                                 
067600     MOVE W-W092-LOGGPOST TO ZZAC01-LOGGPOST                              
067700*                                                                         
067800     PERFORM S21-SKAPA-FELTRANS                                           
067900*                                                                         
068000     .                                                                    
068100     EJECT                                                                
068200 S13-TRANS-FP116 SECTION.                                                 
068300*                                                                         
068400     MOVE ' FELAKTIG TIAVSDAT'   TO W-LOGGPOST                            
068500     MOVE '092'              TO  W-IDPTYP                                 
068600     MOVE W-W092-LOGGPOST TO ZZAC01-LOGGPOST                              
068700*                                                                         
068800     PERFORM S21-SKAPA-FELTRANS                                           
068900     .                                                                    
069000     EJECT                                                                
069100 S14-TRANS-FP131 SECTION.                                                 
069200*                                                                         
069300     MOVE R40-IDPTYP      TO F40-IDPTYP                                   
069400     MOVE +1              TO F40-KDCLAGER                                 
069500     MOVE R40-IDARTNR     TO F40-IDARTNR                                  
069600     MOVE R40-IDLEVNR     TO F40-IDLEVNR                                  
069700     MOVE R40-KVANTAL     TO F40-KVANTAL                                  
069800     MOVE R40-FLUPPBR     TO F40-FLUPPBR                                  
069900     MOVE R40-IDORDNR     TO F40-IDORDNR                                  
070000*                                                                         
070100     MOVE F40-W211F40    TO W-LOGGPOST                                    
070200                                                                          
070300     MOVE '092'              TO  W-IDPTYP                                 
070400     MOVE W-W092-LOGGPOST TO ZZAC01-LOGGPOST                              
070500*                                                                         
070600     PERFORM S21-SKAPA-FELTRANS                                           
070700*                                                                         
070800     .                                                                    
070900     EJECT                                                                
071000 S15-TRANS-FP134 SECTION.                                                 
071100*                                                                         
071200     MOVE R33-IDPTYP      TO F33-IDPTYP                                   
071300     MOVE R33-IDDC        TO WS-IDDC                                      
071400     IF CDC-SE                                                            
071500        MOVE +1           TO F33-KDCLAGER                                 
071600     ELSE                                                                 
071700        MOVE ZERO         TO F33-KDCLAGER                                 
071800     END-IF                                                               
071900     MOVE R33-IDARTNR     TO F33-IDARTNR                                  
072000     MOVE R33-IDLEVNR-INL TO F33-IDLEVNR-INL                              
072100     MOVE R33-KDRT        TO F33-KDRT                                     
072200     MOVE R33-TIAVIDAT    TO F33-TIAVSDAT                                 
072300     MOVE R33-IDKONTO     TO F33-IDKONTO                                  
072400     MOVE R33-IDAVINR     TO F33-IDAVINR                                  
072500     MOVE R33-KVAVIS      TO F33-KVAVIS                                   
072600*                                                                         
072700     MOVE F33-W211F33    TO W-LOGGPOST                                    
072800                                                                          
072900     MOVE '092'              TO  W-IDPTYP                                 
073000     MOVE W-W092-LOGGPOST TO ZZAC01-LOGGPOST                              
073100*                                                                         
073200     PERFORM S21-SKAPA-FELTRANS                                           
073300     .                                                                    
073400     EJECT                                                                
073500 S21-SKAPA-FELTRANS               SECTION.                                
073600                                                                          
073700     ACCEPT ZZAC01-TIAAMMDD FROM DATE                                     
073800     ACCEPT ZZAC01-TIKLOCK FROM TIME                                      
073900     ADD +1 TO W-IDLOGLOP                                                 
074000     MOVE W-IDLOGLOP   TO ZZAC01-IDLOGLOP                                 
074100                                                                          
074200     MOVE ZERO               TO  W-SORT-AREA                              
074300     MOVE SPACE              TO  W-SORT-BLANK                             
074400     MOVE R33-IDPTYP         TO  W-SORT-IDPTYP                            
074500     MOVE R33-IDDC           TO WS-IDDC                                   
074600     IF CDC-SE                                                            
074700        MOVE +1              TO  W-SORT-KDCLAGER                          
074800     ELSE                                                                 
074900        MOVE ZERO            TO  W-SORT-KDCLAGER                          
075000     END-IF                                                               
075100     MOVE R33-IDARTNR        TO  W-SORT-SORTBGP                           
075200     MOVE 1                  TO  W-SORT-KDFELMRK                          
075300                                                                          
075400     MOVE W-SORT-AREA        TO  ZZAC01-SORTPOST                          
075500*                                                                         
075600     PERFORM  IMS-ISRT-LOGGPOST                                           
075700                                                                          
075800     PERFORM UNTIL SEGMENT-FINNS                                          
075900       IF W-IDLOGLOP < +8                                                 
076000         ADD +1 TO W-IDLOGLOP                                             
076100         MOVE W-IDLOGLOP   TO ZZAC01-IDLOGLOP                             
076200       ELSE                                                               
076300         ACCEPT ZZAC01-TIKLOCK FROM TIME                                  
076400         MOVE +0           TO W-IDLOGLOP                                  
076500         MOVE W-IDLOGLOP   TO ZZAC01-IDLOGLOP                             
076600       END-IF                                                             
076700       PERFORM  IMS-ISRT-LOGGPOST                                         
076800     END-PERFORM                                                          
076900     .                                                                    
077000     EJECT                                                                
077100 X-TAG-CHECKPOINT   SECTION.                                              
077200                                                                          
077300     IF 611B-IX > +0                                                      
077400       PERFORM S03-STARTA-R33-TRANS                                       
077500     END-IF                                                               
077600     IF 611C-IX > +0                                                      
077700       PERFORM S04-STARTA-R34-TRANS                                       
077800     END-IF                                                               
077900     IF 611D-IX > +0                                                      
078000       PERFORM S05-STARTA-R40-TRANS                                       
078100     END-IF                                                               
078200     PERFORM IMS-LAS-ATERSTART                                            
078300     IF SEGMENT-SAKNAS                                                    
078400       MOVE '1'                   TO 6028-KDSEGKEY                        
078500     END-IF                                                               
078600     MOVE W-W61104-KVPOST-IN    TO 6028-KVPOST                            
078700     ACCEPT 6028-TIUPPDAT FROM DATE                                       
078800     ACCEPT 6028-TIUPPTID FROM TIME                                       
078900     IF SEGMENT-SAKNAS                                                    
079000       PERFORM IMS-ISRT-ATERSTART                                         
079100     ELSE                                                                 
079200       PERFORM IMS-REPL-ATERSTART                                         
079300     END-IF                                                               
079400     PERFORM IMS-CHECKPOINT                                               
079500     MOVE ZERO TO CHKP-ANT                                                
079600     .                                                                    
079700     EJECT                                                                
079800* --- IMS SEKTIONER ---                                                   
079900     SKIP3                                                                
080000 IMS-LAS-ATERSTART SECTION.                                               
080100     SKIP2                                                                
080200     STRING 'W6CKPG01(W6GXKEY  =' W-W6GX-6027-X ')'                       
080300            DELIMITED BY SIZE INTO SSA1                                   
080400     STRING 'W6CKPG11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
080500            DELIMITED BY SIZE INTO SSA2                                   
080600     MOVE '  GE' TO GODK-STATUSKODER                                      
080700     CALL CBLTDLI USING GHU CKPG-PCB DLI-IO-AREA SSA1 SSA2                
080800     MOVE CKPG-STATUS-CODE TO STATUS-WS                                   
080900     PERFORM IMS-STATUSKONTROLL                                           
081000     .                                                                    
081100     EJECT                                                                
081200 IMS-ISRT-ATERSTART SECTION.                                              
081300     SKIP2                                                                
081400     STRING 'W6CKPG01(W6GXKEY  =' W-W6GX-6027-X ')'                       
081500          DELIMITED BY SIZE INTO SSA1                                     
081600     MOVE 'W6CKPG11' TO SSA2                                              
081700     MOVE '  ' TO GODK-STATUSKODER                                        
081800     CALL CBLTDLI USING ISRT CKPG-PCB DLI-IO-AREA SSA1 SSA2               
081900     MOVE CKPG-STATUS-CODE TO STATUS-WS                                   
082000     PERFORM IMS-STATUSKONTROLL                                           
082100     .                                                                    
082200     SKIP2                                                                
082300 IMS-REPL-ATERSTART SECTION.                                              
082400     SKIP2                                                                
082500     MOVE SPACE TO GODK-STATUSKODER                                       
082600     CALL CBLTDLI USING REPL CKPG-PCB DLI-IO-AREA                         
082700     MOVE CKPG-STATUS-CODE TO STATUS-WS                                   
082800     PERFORM IMS-STATUSKONTROLL                                           
082900     .                                                                    
083000     EJECT                                                                
083100 IMS-GU-ARTC01   SECTION.                                                 
083200                                                                          
083300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
083400          DELIMITED BY SIZE INTO SSA1                                     
083500     MOVE '  GE' TO GODK-STATUSKODER                                      
083600     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
083700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
083800     PERFORM IMS-STATUSKONTROLL                                           
083900     .                                                                    
084000     EJECT                                                                
084100 IMS-GNP-ARTC11   SECTION.                                                
084200                                                                          
084300     MOVE 'WLARTC11 ' TO SSA1                                             
084400     MOVE '  GE' TO GODK-STATUSKODER                                      
084500     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
084600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
084700     PERFORM IMS-STATUSKONTROLL                                           
084800     .                                                                    
084900     EJECT                                                                
085000 IMS-ISRT-LOGGPOST  SECTION.                                              
085100                                                                          
085200     MOVE 'WLZZAC01 ' TO SSA1                                             
085300     MOVE '  II' TO GODK-STATUSKODER                                      
085400     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA SSA1                    
085500     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
085600     PERFORM IMS-STATUSKONTROLL                                           
085700     .                                                                    
085800     EJECT                                                                
085900 IMS-RESTART SECTION.                                                     
086000     SKIP2                                                                
086100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
086200     MOVE '  ' TO GODK-STATUSKODER                                        
086300     CALL CBLTDLI USING XRST MSG-PCB                                      
086400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
086500                        CHKP-AREA-LENGTH CHKP-AREA                        
086600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
086700     PERFORM IMS-STATUSKONTROLL                                           
086800     .                                                                    
086900     EJECT                                                                
087000 IMS-CHECKPOINT SECTION.                                                  
087100     SKIP2                                                                
087200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
087300     MOVE '  XD' TO GODK-STATUSKODER                                      
087400     CALL CBLTDLI USING CHKP MSG-PCB                                      
087500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
087600                        CHKP-AREA-LENGTH CHKP-AREA                        
087700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
087800     PERFORM IMS-STATUSKONTROLL                                           
087900                                                                          
088000     IF IMS-EJ-OK                                                         
088100       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
088200       DISPLAY FELTEXT                                                    
088300       CALL FELLOG                                                        
088400     END-IF                                                               
088500     .                                                                    
088600     EJECT                                                                
088700 IMS-STATUSKONTROLL SECTION.                                              
088800     SKIP2                                                                
088900     SET STATUS-IX TO 1                                                   
089000     SEARCH GODK-STATUS                                                   
089100       AT END                                                             
089200         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
089300         DISPLAY FELTEXT                                                  
089400         CALL FELLOG                                                      
089500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
089600         CONTINUE                                                         
089700     END-SEARCH                                                           
089800     .                                                                    
089900     EJECT                                                                
090000*    -COPY WY2000P1                                                       
090100*    -COPY WY2000P9                                                       
