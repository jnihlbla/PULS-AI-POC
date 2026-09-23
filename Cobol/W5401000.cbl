000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5401000.                                                
000300 AUTHOR.         GAVIN SMITH.                                             
000400 DATE-WRITTEN.   98/09/02.                                                
000500 DATE-COMPILED.                                                           
000600******CHECKED BY WY2000                                                   
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        TAR IN HÄNDELSEBASEN WL1117 MED VECKANS P-SLAG ÄNDRINGAR         
001000*        OCH UPPDATERAR WDK601 MED DET NYA P-SLAG                         
001100*        WDK611 OCH WDK711 KOLLAS , FÖR SALDO > 0.                        
001200*        OM SALDO > 0 GENERERAS TVÅ BOKFÖRINGSTRANSAR SOM                 
001300*        UPPDATERAR WDR9. TVÅ TRANSAR FÖR VARJE ARTIKELNR/S-CLAGER        
001400*        MED LAGERSALDO > 0.                                              
001500*        DÅ WDR9 ÄR FRAMGÅNGSRIKT UPPDATERAT SKA ARTIKELPOSTEN            
001600*        PÅ HÄNDELSEBASEN WL1117 DELETAS.                                 
001700*        DETTA KÖRS INNAN  BÖRJAN AV VARJE VECKA.                         
001800*                                                                         
001900*        NYTT FRÅN DEC-2000:                                              
002000*        FÖR VARJE ARTIKEL SOM ÄNDRAS SKAPAS EN POST PÅ WDGX5142          
002100*        INNEHÅLLANDE INFO OM LOKAL PRODUKTSLAGSÄNDRING OCH I             
002200*        SAMBAND DÄRMED UPPDATERAS LOKALT PRODUKTSLAG PÅ WDK6.            
002300*                                                                         
002400*        PROGRAMMET UPPDATERAR  WDG2              (WL1118)                
002500*        PROGRAMMET UPPDATERAR  WDK6                                      
002600*        PROGRAMMET UPPDATERAR  WDK7                                      
002700*        PROGRAMMET UPPDATERAR  WDR9                                      
002800*        PROGRAMMET UPPDATERAR  WDR1              (WDGX5142)              
002900*        PROGRAMMET LÄSER       WDD3 VIA WDD3BSEQ                         
003000*        PROGRAMMET UPPDATERAR  WDR8                                      
003100*                                                                         
003200*    E-TRACKER 10143271 - CHINA WAREHOUSE PROJECT-1                       
003300*                                                                         
003400                                                                          
003500     SKIP3                                                                
003600 ENVIRONMENT DIVISION.                                                    
003700     SKIP2                                                                
003800 INPUT-OUTPUT SECTION.                                                    
003900                                                                          
004000 FILE-CONTROL.                                                            
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP3                                                                
004400 FILE SECTION.                                                            
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700                                                                          
004800 77  IDPGM                       PIC X(8)    VALUE 'W5401000'.            
004900 01  CHKP-VAR.                                                            
005000     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
005100     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
005200     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
005300     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
005400     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
005500     03 CHKP-MAX                 PIC S9(3)   VALUE +100 COMP-3.           
005600 77  JA                          PIC X       VALUE 'J'.                   
005700 77  NEJ                         PIC X       VALUE 'N'.                   
005800 77  WS-OLD-KDPSLLOC             PIC 9(2).                                
005900 77  WS-OLD-KDPRODSL             PIC S9(3)   COMP-3.                      
006000 77  WS-WORK-IDARTNR             PIC X(20)   VALUE SPACE.                 
006100 77  WS-KDSORT                   PIC X(2)    VALUE SPACE.                 
006200 77  WS-COUNT-INPUT              PIC 9(5)    VALUE ZERO.                  
006300 77  WS-COUNT-OUTPUT             PIC 9(5)    VALUE ZERO.                  
006400     SKIP2                                                                
006500 01  FELTEXT.                                                             
006600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006800     EJECT                                                                
006900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007000 01  FILLER REDEFINES DAGENS-DATUM.                                       
007100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007400     EJECT                                                                
007500*    --- INPUT TRANSACTION VALIDITY SWITCH                                
007600 01  TRANS-STATUS                PIC X       VALUE 'N'.                   
007700     88  DUMMY                               VALUE 'Y'.                   
007800 01  DYNAMISKA-SUBPROGRAM.                                                
007900*                                                                         
008000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008200     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
008300     03  W100LPC                 PIC X(8)    VALUE 'W100LPC'.             
008400     EJECT                                                                
008500*    --- VALID IDDC CODES                                                 
008600*                                                                         
008700*01  -COPY  WWDC99                                                        
008800*01  -COPY  WWDCKONS                                                      
008900*                                                                         
009000*01  -COPY  WWPRODSL                                                      
009100*    --- PARAMETRAR TILL DATKORT                                          
009200*                                                                         
009300 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W54010'.              
009400     SKIP2                                                                
009500 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
009600     SKIP2                                                                
009700*01  -COPY WDATKORT                                                       
009800*                                                                         
009900     EJECT                                                                
010000                                                                          
010100 01  FILLER                      PIC X(16)   VALUE 'W100LPC-AREA'.        
010200*01  LPC-AREA  -COPY W100LPC                                              
010300     EJECT                                                                
010400                                                                          
010500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010600     SKIP3                                                                
010700 01  NYCKLAR-TILL-DLI.                                                    
010800     03  W-WDGXKEY-1.                                                     
010900         05  FILLER              PIC X(4)     VALUE '1117'.               
011000         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
011100     03  W-IDARTNR-1117.                                                  
011200         05  W-IDARTNR1117       PIC S9(9)   VALUE ZERO COMP-3.           
011300     03  W-IDARTNR-C.                                                     
011400         05  W-IDARTNRC          PIC S9(9)   VALUE ZERO COMP-3.           
011500     03  W-IDARTNR-S.                                                     
011600         05  W-IDARTNRS          PIC S9(9)   VALUE ZERO COMP-3.           
011700     03  W-IDDC-S.                                                        
011800         05  W-IDDCS             PIC X(2)    VALUE SPACE.                 
011900     03  W-WDR901KY-X.                                                    
012000         05  W-WDR901KY          PIC X(31)    VALUE SPACE.                
012100     03  W-WDGXKEY-5141-X.                                                
012200         05  FILLER              PIC X(4)    VALUE '5141'.                
012300         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
012400     03  W-IDARTNR-WDD3-X.                                                
012500         05  W-IDARTNR-WDD3      PIC S9(9)   VALUE ZERO COMP-3.           
012600     03  W-IDSKYLT-WDD3-X.                                                
012700         05  W-IDSKYLT-WDD3      PIC X(3)    VALUE 'GB'.                  
012800     03  W-IDDC-B6-X.                                                     
012900         05 W-IDDC-B6            PIC X(2)    VALUE SPACE.                 
013000     SKIP2                                                                
013100*    --- STATUS-KOD FRÅN IMS                                              
013200 01  STATUS-WS                   PIC XX.                                  
013300     88  SEGMENT-FINNS                       VALUE '  '.                  
013400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013500     88  SEGMENT-SAKNAS                      VALUE 'GE' 'GP'.             
013600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
013700     88  IMS-EJ-OK                           VALUE 'XD'.                  
013800     SKIP2                                                                
013900 01  GODK-STATUSKODER.                                                    
014000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014100     SKIP3                                                                
014200 01  SSA1                        PIC X(64).                               
014300 01  SSA2                        PIC X(64).                               
014400     EJECT                                                                
014500*    --- IMS FUNKTIONSKODER                                               
014600*01  -COPY W0003                                                          
014700     EJECT                                                                
014800*    ---  DLI INPUT-OUTPUT AREA                                           
014900                                                                          
015000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WL111701'.                    
015100 01  DLI-IO-WL111701.                                                     
015200*    03  -COPY WDG201   -PRE 1117-                                        
015300     EJECT                                                                
015400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WL111711'.                    
015500 01  DLI-IO-WL111711.                                                     
015600*    03  -COPY WDGX1118 -PRE 1117-                                        
015700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC01'.                    
015800 01  DLI-IO-WLARTC01.                                                     
015900*    03  -COPY WDK601                                                     
016000     EJECT                                                                
016100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC11'.                    
016200 01  DLI-IO-WLARTC11.                                                     
016300*    03  -COPY WDK611                                                     
016400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTS01'.                    
016500 01  DLI-IO-WLARTS01.                                                     
016600*    03  -COPY WDK701                                                     
016700     EJECT                                                                
016800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTS11'.                    
016900 01  DLI-IO-WLARTS11.                                                     
017000*    03  -COPY WDK711                                                     
017100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLSAPA01'.                    
017200 01  DLI-IO-WLSAPA01.                                                     
017300*    03  -COPY WDR901                                                     
017400        05 -COPY W510EKHA -RED FIL-WDR901-DATA                            
017500     EJECT                                                                
017600 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDGX5141'.                    
017700 01  DLI-IO-WDGX5141.                                                     
017800*    03  -COPY WDGX01                                                     
017900     EJECT                                                                
018000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDGX5142'.                    
018100 01  DLI-IO-WDGX5142.                                                     
018200*    03  -COPY WDGX5142                                                   
018300     EJECT                                                                
018400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
018500 01  DLI-IO-WDD311.                                                       
018600*    03  -COPY WDD311                                                     
018700     EJECT                                                                
018800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
018900 01   DLI-IO-AREA-B601.                                                   
019000*     03  -COPY WDB601                                                    
019100     EJECT                                                                
019200 01  DLI-IO-WDR801.                                                       
019300*    03  -COPY WDR801                              -PRE EKO-              
019400        05 -COPY W510EKHA -RED EKO-FIL-WDR801-DATA -PRE EKO-              
019500     EJECT                                                                
019600 LINKAGE SECTION.                                                         
019700                                                                          
019800*01  -COPY W0009   -PRE MSG-                                              
019900                                                                          
020000*01  -COPY W0008  -PRE 1117-                                              
020100     05  FILLER                  PIC X.                                   
020200                                                                          
020300*01  -COPY W0008  -PRE ARTC-                                              
020400     05  FILLER                  PIC X.                                   
020500                                                                          
020600*01  -COPY W0008  -PRE ARTS-                                              
020700     05  FILLER                  PIC X.                                   
020800                                                                          
020900*01  -COPY W0008  -PRE SAPA-                                              
021000     05  FILLER                  PIC X.                                   
021100                                                                          
021200*01  -COPY W0008  -PRE 5141-                                              
021300     05  FILLER                  PIC X.                                   
021400                                                                          
021500*01  -COPY W0008  -PRE WDD3-                                              
021600     05  FILLER                  PIC X.                                   
021700                                                                          
021800*01  -COPY W0008  -PRE WDB6-                                              
021900     05  FILLER                  PIC X.                                   
022000                                                                          
022100*01  -COPY W0008  -PRE WDR8-                                              
022200     05  FILLER                  PIC X.                                   
022300                                                                          
022400     EJECT                                                                
022500 PROCEDURE DIVISION  USING MSG-PCB                                        
022600                           1117-PCB ARTC-PCB ARTS-PCB                     
022700                           SAPA-PCB 5141-PCB WDD3-PCB                     
022800                           WDB6-PCB WDR8-PCB.                             
022900 MAIN SECTION.                                                            
023000     ENTRY 'DLITCBL' USING MSG-PCB                                        
023100                           1117-PCB ARTC-PCB ARTS-PCB                     
023200                           SAPA-PCB 5141-PCB WDD3-PCB                     
023300                           WDB6-PCB WDR8-PCB.                             
023400                                                                          
023500     PERFORM A-INIT                                                       
023600     PERFORM IMS-GU-111701                                                
023700     PERFORM IMS-GHNP-111711                                              
023800     PERFORM UNTIL SEGMENT-SAKNAS                                         
023900        PERFORM B-UPDATE-WDK6X-5142                                       
024000        IF NOT DUMMY                                                      
024100          ADD 1 TO WS-COUNT-INPUT                                         
024200          PERFORM C-RD611-UPD901                                          
024300          PERFORM D-RD711-UPD801-UPD901                                   
024400        END-IF                                                            
024500        PERFORM IMS-DLET-111711                                           
024600        IF CHKP-ANT > CHKP-MAX                                            
024700          PERFORM X-TAG-CHECKPOINT                                        
024800          PERFORM IMS-GU-111701                                           
024900        END-IF                                                            
025000        PERFORM IMS-GHNP-111711                                           
025100        ADD 1 TO CHKP-ANT                                                 
025200     END-PERFORM                                                          
025300                                                                          
025400     PERFORM Z-FINIT                                                      
025500                                                                          
025600     MOVE ZERO TO RETURN-CODE                                             
025700     GOBACK                                                               
025800     .                                                                    
025900     EJECT                                                                
026000 A-INIT SECTION.                                                          
026100     SKIP2                                                                
026200                                                                          
026300     PERFORM IMS-RESTART                                                  
026400     .                                                                    
026500     EJECT                                                                
026600 B-UPDATE-WDK6X-5142 SECTION.                                             
026700     SKIP2                                                                
026800     MOVE 'N'                    TO TRANS-STATUS                          
026900     MOVE 1117-1118-IDARTNR      TO W-IDARTNR-C                           
027000     PERFORM IMS-GHU-WDK601                                               
027100     IF NOT ART-KDPRODSL = 1117-1118-KDPRODSL                             
027200       MOVE ART-KDPRODSL         TO WS-OLD-KDPRODSL                       
027300       MOVE 1117-1118-KDPRODSL   TO ART-KDPRODSL                          
027400       MOVE ART-KDSORT           TO WS-KDSORT                             
027500       PERFORM IMS-REPL-WDK601                                            
027600                                                                          
027700       PERFORM IMS-GHU-WDK601                                             
027800       PERFORM IMS-GHNP-WDK611                                            
027900       PERFORM BA-GET-LOCAL-PCODE                                         
028000       PERFORM IMS-REPL-WDK611                                            
028100                                                                          
028200       PERFORM IMS-GHU-WDR101                                             
028300       PERFORM BB-BUILD-5142                                              
028400       PERFORM IMS-ISRT-WDGX5142                                          
028500                                                                          
028600       MOVE 1117-1118-KDPRODSL   TO TEST-KDPRODSL                         
028700       IF KDPRODSL-LOCAL                                                  
028800         MOVE 'Y'                TO TRANS-STATUS                          
028900       END-IF                                                             
029000       MOVE WS-OLD-KDPRODSL      TO TEST-KDPRODSL                         
029100       IF KDPRODSL-LOCAL                                                  
029200         MOVE 'Y'                TO TRANS-STATUS                          
029300       END-IF                                                             
029400     ELSE                                                                 
029500       MOVE 'Y'                  TO TRANS-STATUS                          
029600     END-IF                                                               
029700     .                                                                    
029800     EJECT                                                                
029900 BA-GET-LOCAL-PCODE SECTION.                                              
030000     SKIP2                                                                
030100     MOVE CLAG-KDPSLLOC              TO WS-OLD-KDPSLLOC                   
030200                                                                          
030300     MOVE 1117-1118-IDARTNR          TO LPC-IDARTNR-IN                    
030400     MOVE ART-IDFKNGRP               TO LPC-IDFKNGRP-IN                   
030500     MOVE ART-KDPRODSL               TO LPC-KDPRODSL-IN                   
030600     MOVE ZERO                       TO LPC-KDPSLLOC-UT                   
030700     CALL W100LPC  USING LPC-AREA                                         
030800                                                                          
030900     MOVE LPC-KDPSLLOC-UT            TO CLAG-KDPSLLOC                     
031000     .                                                                    
031100     EJECT                                                                
031200 BB-BUILD-5142 SECTION.                                                   
031300     SKIP2                                                                
031400     MOVE 1117-1118-IDARTNR    TO 5142-IDARTNR                            
031500                                  W-IDARTNR-WDD3                          
031600                                                                          
031700     PERFORM IMS-GU-WDD311-BSEQ                                           
031800     IF SEGMENT-FINNS                                                     
031900       MOVE TEXT-BEART         TO 5142-BEART                              
032000     ELSE                                                                 
032100       MOVE 'UNKNOWN'          TO 5142-BEART                              
032200     END-IF                                                               
032300     MOVE 1117-1118-KDPRODSL   TO 5142-KDPRODSL                           
032400     MOVE WS-OLD-KDPSLLOC      TO 5142-KDPSLLOC                           
032500     MOVE CLAG-KDPSLLOC        TO 5142-KDPSLLOC-NEW                       
032600     .                                                                    
032700     EJECT                                                                
032800 C-RD611-UPD901  SECTION.                                                 
032900     SKIP2                                                                
033000*FLYTTA VÄRDEN TILL EKONOMI I-O AREAN.                                    
033100     MOVE 'W5401000'                 TO  FIL-IDPGM                        
033200     MOVE FUNCTION CURRENT-DATE(1:8) TO  FIL-DAREGDAT                     
033300                                         EKH-DAVERDAT                     
033400     MOVE 1                          TO  FIL-IDSEKVNR                     
033500     MOVE 'W510'                     TO  FIL-CT-IDSYSTEM                  
033600     MOVE 'EKH'                      TO  FIL-CT-IDPTYP                    
033700     MOVE 'A'                        TO  FIL-CT-IDVTYP                    
033800     MOVE 'UNKNOWN '                 TO  FIL-IDUSER                       
033900     MOVE SPACE                      TO  EKH-BEVAT                        
034000     MOVE SPACE                      TO  EKH-FLLSBOK                      
034100     MOVE SPACE                      TO  EKH-IDANALYS                     
034200     MOVE ART-IDARTNR                TO  EKH-IDARTNR                      
034300     MOVE SPACE                      TO  EKH-IDDC-REC                     
034400     MOVE ZERO                       TO  EKH-IDDISTR                      
034500     MOVE ZERO                       TO  EKH-IDKONTO                      
034600     MOVE SPACE                      TO  EKH-IDKST                        
034700     MOVE ZERO                       TO  EKH-IDKUNDNR                     
034800     MOVE SPACE                      TO  EKH-IDTRANS                      
034900     MOVE ART-IDARTNR                TO  WS-WORK-IDARTNR                  
035000     INSPECT  WS-WORK-IDARTNR REPLACING  LEADING '0' BY SPACE             
035100     UNSTRING WS-WORK-IDARTNR DELIMITED BY ALL SPACE                      
035200                                   INTO  EKH-IDVERGL                      
035300                                         EKH-IDVERGL                      
035400     END-UNSTRING                                                         
035500     MOVE SPACE                      TO  EKH-KDANMORS                     
035600     MOVE ZERO                       TO  EKH-KDFRAKT                      
035700     MOVE '402'                      TO  EKH-KDEKHHT                      
035800     MOVE '401'                      TO  EKH-KDEKSHT                      
035900     MOVE 'DET'                      TO  EKH-KDEKNIVA                     
036000     MOVE ZERO                       TO  EKH-KDPSLLOC                     
036100     MOVE 'SEK'                      TO  EKH-KDVALISO                     
036200     MOVE ZERO                       TO  EKH-PRARTNTO                     
036300     MOVE ZERO                       TO  EKH-PRARTSJK                     
036400     MOVE ZERO                       TO  EKH-PRHEMTAG                     
036500     MOVE CLAG-PRARTSTD              TO  EKH-PRARTSTD                     
036600     MOVE ZERO                       TO  EKH-PRLANDCO                     
036700     MOVE ZERO                       TO  EKH-PRDIRLON                     
036800     MOVE ZERO                       TO  EKH-PRDMTRL                      
036900     MOVE 1                          TO  EKH-PRKURS                       
037000     MOVE ZERO                       TO  EKH-PRINK                        
037100     MOVE ZERO                       TO  EKH-PROVRPAL                     
037200     MOVE ZERO                       TO  EKH-SUBEL                        
037300     MOVE ZERO                       TO  EKH-SUVAT                        
037400     MOVE ZERO                       TO  EKH-DAAVIDAT                     
037500     MOVE ZERO                       TO  EKH-IDAVINR                      
037600     MOVE SPACE                      TO  EKH-IDLEVNR                      
037700     MOVE ZERO                       TO  EKH-KDAVVTYP                     
037800     MOVE ZERO                       TO  EKH-KDRT                         
037900     MOVE ZERO                       TO  EKH-KVANTMOT                     
038000     MOVE ZERO                       TO  EKH-KVAVIS                       
038100     MOVE WS-KDSORT                  TO  EKH-KDSORT                       
038200     MOVE 'SEPV'                     TO  EKH-KDTRADP                      
038300     MOVE SPACE                      TO  EKH-FLDCET                       
038400     MOVE SPACE                      TO  EKH-IDKUNDRF                     
038500     MOVE SPACE                      TO  EKH-IDFAKT-EXP                   
038600*GENERATE IDDC12 TRANSACTIONS*************                                
038700     COMPUTE EKH-KVANTAL =                                                
038800     CLAG-KVAKS-T                                                         
038900     IF NOT EKH-KVANTAL  = 0                                              
039000        MOVE WC-CDC-TR                   TO  EKH-IDDC-SEND                
039100        MOVE ART-KDPRODSL                TO  EKH-KDPRODSL                 
039200        MOVE FUNCTION CURRENT-DATE (9:8) TO  FIL-TIKLOCK                  
039300        ADD  1                           TO  FIL-IDSEKVNR                 
039400                                                                          
039500*       -- EFTER CHECKPOINT KAN IDSEKVNR = 002 VARA UPPTAGET              
039600        PERFORM  IMS-ISRT-SAPA                                            
039700        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
039800          ADD 1                          TO  FIL-IDSEKVNR                 
039900          PERFORM IMS-ISRT-SAPA                                           
040000        END-PERFORM                                                       
040100                                                                          
040200        ADD 1 TO WS-COUNT-OUTPUT                                          
040300        MOVE WS-OLD-KDPRODSL             TO  EKH-KDPRODSL                 
040400        ADD  1                           TO  FIL-IDSEKVNR                 
040500        MULTIPLY EKH-KVANTAL BY -1 GIVING EKH-KVANTAL                     
040600        PERFORM  IMS-ISRT-SAPA                                            
040700        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
040800          ADD 1                          TO  FIL-IDSEKVNR                 
040900          PERFORM IMS-ISRT-SAPA                                           
041000        END-PERFORM                                                       
041100        ADD 1 TO WS-COUNT-OUTPUT                                          
041200     END-IF                                                               
041300********************************************                              
041400*GENERATE IDDC11 TRANSACTIONS*************                                
041500     COMPUTE EKH-KVANTAL =                                                
041600     CLAG-KVLS + CLAG-KVEFRS + CLAG-KVAKS-CDC + CLAG-KVAKS-PAV            
041700     IF NOT EKH-KVANTAL  = 0                                              
041800        MOVE WC-CDC-SE                   TO  EKH-IDDC-SEND                
041900        MOVE ART-KDPRODSL                TO  EKH-KDPRODSL                 
042000        MOVE FUNCTION CURRENT-DATE (9:8) TO  FIL-TIKLOCK                  
042100        ADD  1                           TO  FIL-IDSEKVNR                 
042200        PERFORM  IMS-ISRT-SAPA                                            
042300        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
042400          ADD 1                          TO  FIL-IDSEKVNR                 
042500          PERFORM IMS-ISRT-SAPA                                           
042600        END-PERFORM                                                       
042700        ADD 1 TO WS-COUNT-OUTPUT                                          
042800        MOVE WS-OLD-KDPRODSL             TO  EKH-KDPRODSL                 
042900        ADD  1                           TO  FIL-IDSEKVNR                 
043000        MULTIPLY EKH-KVANTAL BY -1 GIVING EKH-KVANTAL                     
043100        PERFORM  IMS-ISRT-SAPA                                            
043200        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
043300          ADD 1                          TO  FIL-IDSEKVNR                 
043400          PERFORM IMS-ISRT-SAPA                                           
043500        END-PERFORM                                                       
043600        ADD 1 TO WS-COUNT-OUTPUT                                          
043700     END-IF                                                               
043800********************************************                              
043900     .                                                                    
044000     EJECT                                                                
044100 D-RD711-UPD801-UPD901  SECTION.                                          
044200     MOVE 1117-1118-IDARTNR      TO W-IDARTNR-S                           
044300     PERFORM IMS-GU-WDK701                                                
044400     PERFORM IMS-GNP-WDK711                                               
044500     PERFORM UNTIL SEGMENT-SAKNAS                                         
044600*GENERATE IDDCXX TRANSACTIONS*************                                
044700       MOVE SLAG-IDDC         TO WS-IDDC                                  
044800                                 W-IDDC-B6                                
044900       PERFORM IMS-GU-WDB601                                              
045110       IF XDC-NON-VCC-OWNED                                               
045200         MOVE EKH-W510EKHA             TO  EKO-EKH-W510EKHA               
045300         MOVE 'W5401000'               TO  EKO-FIL-IDPGM                  
045400         ACCEPT EKO-FIL-TIREGDAT FROM DATE                                
045500         ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                
045600         MOVE FUNCTION CURRENT-DATE(1:8) TO EKO-EKH-DAVERDAT              
045700         MOVE 1                        TO  EKO-FIL-IDSEKVNR               
046510         IF NDC-CN                                                        
046520           MOVE 'W570'                 TO EKO-FIL-CT-IDSYSTEM             
046530         ELSE                                                             
046540           IF NDC-IN                                                      
046541             MOVE 'W515'               TO EKO-FIL-CT-IDSYSTEM             
046560           ELSE                                                           
046570             MOVE DCS-KDTRADP          TO EKO-FIL-CT-IDSYSTEM             
046580           END-IF                                                         
046590         END-IF                                                           
046591         MOVE 'EKH'                    TO  EKO-FIL-CT-IDPTYP              
046592         MOVE 'A'                      TO  EKO-FIL-CT-IDVTYP              
046600         PERFORM DA-RD711-UPD801                                          
046700       ELSE                                                               
046800         PERFORM DB-RD711-UPD901                                          
046900       END-IF                                                             
047000       PERFORM IMS-GNP-WDK711                                             
047100     END-PERFORM                                                          
047200     .                                                                    
047300     EJECT                                                                
047400                                                                          
047500 DA-RD711-UPD801  SECTION.                                                
047600     COMPUTE EKO-EKH-KVANTAL = SLAG-KVLS + SLAG-KVEFRS                    
047700     IF NOT EKO-EKH-KVANTAL  = 0                                          
047800       MOVE SLAG-IDDC                   TO  EKO-EKH-IDDC-SEND             
047900       MOVE ART-KDPRODSL                TO  EKO-EKH-KDPRODSL              
048000                                                                          
048100       MOVE SLAG-PRAVCOST               TO  EKO-EKH-PRARTSTD              
048710       MOVE DCS-KDVALISO                TO  EKO-EKH-KDVALISO              
048720       MOVE DCS-KDTRADP                 TO  EKO-EKH-KDTRADP               
048800                                                                          
048900       PERFORM  IMS-ISRT-WDR8                                             
049000       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
049100         ADD 1                          TO  EKO-FIL-IDSEKVNR              
049200         PERFORM IMS-ISRT-WDR8                                            
049300       END-PERFORM                                                        
049400                                                                          
049500       ADD 1 TO WS-COUNT-OUTPUT                                           
049600       MOVE WS-OLD-KDPRODSL             TO  EKO-EKH-KDPRODSL              
049700       ADD  1                           TO  EKO-FIL-IDSEKVNR              
049800       MULTIPLY EKO-EKH-KVANTAL BY -1 GIVING EKO-EKH-KVANTAL              
049900                                                                          
050000       MOVE SLAG-PRAVCOST               TO  EKO-EKH-PRARTSTD              
050610       MOVE DCS-KDVALISO                TO  EKO-EKH-KDVALISO              
050620       MOVE DCS-KDTRADP                 TO  EKO-EKH-KDTRADP               
050700                                                                          
050800       PERFORM  IMS-ISRT-WDR8                                             
050900       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
051000         ADD 1                          TO  EKO-FIL-IDSEKVNR              
051100         PERFORM IMS-ISRT-WDR8                                            
051200       END-PERFORM                                                        
051300       ADD 1 TO WS-COUNT-OUTPUT                                           
051400     END-IF                                                               
051500     .                                                                    
051600     EJECT                                                                
051700                                                                          
051800 DB-RD711-UPD901  SECTION.                                                
051900     IF DCS-SDC OR DCS-NDC-PF                                             
052000       COMPUTE EKH-KVANTAL =                                              
052100       SLAG-KVLS + SLAG-KVEFRS + SLAG-KVAKS-SDC + SLAG-KVAKS-PAV          
052200       IF NOT EKH-KVANTAL  = 0                                            
052300         MOVE SLAG-IDDC                   TO  EKH-IDDC-SEND               
052400         MOVE ART-KDPRODSL                TO  EKH-KDPRODSL                
052500         MOVE FUNCTION CURRENT-DATE (9:8) TO  FIL-TIKLOCK                 
052600         ADD  1                           TO  FIL-IDSEKVNR                
052700         PERFORM  IMS-ISRT-SAPA                                           
052800         PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                            
052900           ADD 1                          TO  FIL-IDSEKVNR                
053000           PERFORM IMS-ISRT-SAPA                                          
053100         END-PERFORM                                                      
053200         ADD 1 TO WS-COUNT-OUTPUT                                         
053300         MOVE WS-OLD-KDPRODSL             TO  EKH-KDPRODSL                
053400         ADD  1                           TO  FIL-IDSEKVNR                
053500         MULTIPLY EKH-KVANTAL BY -1 GIVING EKH-KVANTAL                    
053600         PERFORM  IMS-ISRT-SAPA                                           
053700         PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                            
053800           ADD 1                          TO  FIL-IDSEKVNR                
053900           PERFORM IMS-ISRT-SAPA                                          
054000         END-PERFORM                                                      
054100         ADD 1 TO WS-COUNT-OUTPUT                                         
054200       END-IF                                                             
054300     END-IF                                                               
054400     .                                                                    
054500     EJECT                                                                
054600 Z-FINIT SECTION.                                                         
054700                                                                          
054800     DISPLAY 'ANTAL UPPD.TRANSAKTIONER=' WS-COUNT-INPUT                   
054900     DISPLAY 'ANTAL EK.TRANSAKTIONER  =' WS-COUNT-OUTPUT                  
055000     .                                                                    
055100     EJECT                                                                
055200 X-TAG-CHECKPOINT   SECTION.                                              
055300                                                                          
055400* --- VID CHECKPOINTTAGNING SÅ TAPPAR MAN GN-POSITION I BASEN             
055500* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
055600     PERFORM IMS-CHECKPOINT                                               
055700     MOVE ZERO TO CHKP-ANT                                                
055800* --- LÄS OM DATABAS OM DET BEHÖVS                                        
055900     .                                                                    
056000     EJECT                                                                
056100* --- IMS SEKTIONER ---                                                   
056200                                                                          
056300     EJECT                                                                
056400 IMS-GU-111701 SECTION.                                                   
056500                                                                          
056600     STRING 'WL111701(WDGXKEY  =' W-WDGXKEY-1 ')'                         
056700          DELIMITED BY SIZE INTO SSA1                                     
056800     MOVE '  GE' TO GODK-STATUSKODER                                      
056900     CALL CBLTDLI USING GU 1117-PCB DLI-IO-WL111701 SSA1                  
057000     MOVE 1117-STATUS-CODE TO STATUS-WS                                   
057100     PERFORM IMS-STATUSKONTROLL                                           
057200     .                                                                    
057300                                                                          
057400 IMS-GHNP-111711  SECTION.                                                
057500                                                                          
057600     STRING 'WL111701(WDGXKEY  =' W-WDGXKEY-1 ')'                         
057700          DELIMITED BY SIZE INTO SSA1                                     
057800     MOVE   'WL111711 ' TO       SSA2                                     
057900     MOVE '  GE' TO GODK-STATUSKODER                                      
058000     CALL CBLTDLI USING GHNP 1117-PCB DLI-IO-WL111711 SSA1 SSA2           
058100     MOVE 1117-STATUS-CODE TO STATUS-WS                                   
058200     PERFORM IMS-STATUSKONTROLL                                           
058300     .                                                                    
058400                                                                          
058500 IMS-DLET-111711  SECTION.                                                
058600     MOVE '    ' TO GODK-STATUSKODER                                      
058700     CALL CBLTDLI USING DLET 1117-PCB DLI-IO-WL111711                     
058800     MOVE 1117-STATUS-CODE TO STATUS-WS                                   
058900     PERFORM IMS-STATUSKONTROLL                                           
059000     .                                                                    
059100     EJECT                                                                
059200 IMS-GHU-WDK601   SECTION.                                                
059300                                                                          
059400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-C ')'                         
059500          DELIMITED BY SIZE INTO SSA1                                     
059600     MOVE '    ' TO GODK-STATUSKODER                                      
059700     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-WLARTC01 SSA1                 
059800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
059900     PERFORM IMS-STATUSKONTROLL                                           
060000     .                                                                    
060100                                                                          
060200 IMS-REPL-WDK601  SECTION.                                                
060300                                                                          
060400     MOVE '    ' TO GODK-STATUSKODER                                      
060500     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-WLARTC01                     
060600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
060700     PERFORM IMS-STATUSKONTROLL                                           
060800     .                                                                    
060900                                                                          
061000 IMS-GHNP-WDK611    SECTION.                                              
061100                                                                          
061200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-C ')'                         
061300          DELIMITED BY SIZE INTO SSA1                                     
061400     MOVE   'WLARTC11 ' TO SSA2                                           
061500     MOVE '    ' TO GODK-STATUSKODER                                      
061600     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-WLARTC11 SSA1 SSA2           
061700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
061800     PERFORM IMS-STATUSKONTROLL                                           
061900     .                                                                    
062000                                                                          
062100 IMS-REPL-WDK611  SECTION.                                                
062200                                                                          
062300     MOVE '    ' TO GODK-STATUSKODER                                      
062400     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-WLARTC11                     
062500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
062600     PERFORM IMS-STATUSKONTROLL                                           
062700     .                                                                    
062800     EJECT                                                                
062900 IMS-GU-WDK701    SECTION.                                                
063000                                                                          
063100     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-S ')'                         
063200          DELIMITED BY SIZE INTO SSA1                                     
063300     MOVE '  GE' TO GODK-STATUSKODER                                      
063400     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-WLARTS01 SSA1                  
063500     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
063600     PERFORM IMS-STATUSKONTROLL                                           
063700     .                                                                    
063800                                                                          
063900 IMS-GNP-WDK711    SECTION.                                               
064000                                                                          
064100     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-S ')'                         
064200          DELIMITED BY SIZE INTO SSA1                                     
064300     MOVE   'WLARTS11 '       TO SSA2                                     
064400     MOVE 'GPGE' TO GODK-STATUSKODER                                      
064500     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-WLARTS11 SSA1  SSA2           
064600     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
064700     PERFORM IMS-STATUSKONTROLL                                           
064800     .                                                                    
064900     EJECT                                                                
065000 IMS-ISRT-SAPA    SECTION.                                                
065100                                                                          
065200     MOVE   'WLSAPA01 '    TO SSA1                                        
065300     MOVE '  II' TO GODK-STATUSKODER                                      
065400     CALL CBLTDLI USING ISRT SAPA-PCB DLI-IO-WLSAPA01 SSA1                
065500     MOVE SAPA-STATUS-CODE TO STATUS-WS                                   
065600     PERFORM IMS-STATUSKONTROLL                                           
065700     .                                                                    
065800     EJECT                                                                
065900 IMS-ISRT-WDR8    SECTION.                                                
066000                                                                          
066100     MOVE   'WDR801   '    TO SSA1                                        
066200     MOVE '  II' TO GODK-STATUSKODER                                      
066300     CALL CBLTDLI USING ISRT WDR8-PCB DLI-IO-WDR801   SSA1                
066400     MOVE WDR8-STATUS-CODE TO STATUS-WS                                   
066500     PERFORM IMS-STATUSKONTROLL                                           
066600     .                                                                    
066700     EJECT                                                                
066800 IMS-GU-WDD311-BSEQ SECTION.                                              
066900     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-WDD3-X ')'                    
067000          DELIMITED BY SIZE INTO SSA1                                     
067100     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-WDD3-X ')'                    
067200          DELIMITED BY SIZE INTO SSA2                                     
067300     MOVE '  GE'           TO GODK-STATUSKODER                            
067400     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
067500     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
067600     PERFORM IMS-STATUSKONTROLL                                           
067700     .                                                                    
067800     EJECT                                                                
067900 IMS-GHU-WDR101 SECTION.                                                  
068000                                                                          
068100     STRING 'WDR101  (WDGXKEY = ' W-WDGXKEY-5141-X ')'                    
068200          DELIMITED BY SIZE INTO SSA1                                     
068300     MOVE '  '             TO GODK-STATUSKODER                            
068400     CALL CBLTDLI USING GHU 5141-PCB DLI-IO-WDGX5141 SSA1                 
068500     MOVE 5141-STATUS-CODE TO STATUS-WS                                   
068600     PERFORM IMS-STATUSKONTROLL                                           
068700     .                                                                    
068800                                                                          
068900 IMS-ISRT-WDGX5142 SECTION.                                               
069000     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-5141-X ')'                    
069100          DELIMITED BY SIZE INTO SSA1                                     
069200     MOVE 'WDGX5142 '      TO SSA2                                        
069300     MOVE '  II'           TO GODK-STATUSKODER                            
069400     CALL CBLTDLI USING ISRT 5141-PCB DLI-IO-WDGX5142 SSA1 SSA2           
069500     MOVE 5141-STATUS-CODE TO STATUS-WS                                   
069600     PERFORM IMS-STATUSKONTROLL                                           
069700     .                                                                    
069800     EJECT                                                                
069900 IMS-RESTART SECTION.                                                     
070000     SKIP2                                                                
070100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
070200     MOVE '  ' TO GODK-STATUSKODER                                        
070300     CALL CBLTDLI USING XRST MSG-PCB                                      
070400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
070500                        CHKP-AREA-LENGTH CHKP-AREA                        
070600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
070700     PERFORM IMS-STATUSKONTROLL                                           
070800     .                                                                    
070900     SKIP3                                                                
071000 IMS-CHECKPOINT SECTION.                                                  
071100     SKIP2                                                                
071200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
071300     MOVE '  XD' TO GODK-STATUSKODER                                      
071400     CALL CBLTDLI USING CHKP MSG-PCB                                      
071500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
071600                        CHKP-AREA-LENGTH CHKP-AREA                        
071700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
071800     PERFORM IMS-STATUSKONTROLL                                           
071900                                                                          
072000     IF IMS-EJ-OK                                                         
072100       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
072200       DISPLAY FELTEXT                                                    
072300       CALL FELLOG                                                        
072400     END-IF                                                               
072500     .                                                                    
072600     EJECT                                                                
072700 IMS-GU-WDB601    SECTION.                                                
072800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
072900          DELIMITED BY SIZE INTO SSA1                                     
073000     MOVE '  ' TO GODK-STATUSKODER                                        
073100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
073200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
073300     PERFORM IMS-STATUSKONTROLL                                           
073400     .                                                                    
073500     EJECT                                                                
073600 IMS-STATUSKONTROLL SECTION.                                              
073700     SKIP2                                                                
073800     SET STATUS-IX TO 1                                                   
073900     SEARCH GODK-STATUS                                                   
074000       AT END                                                             
074100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
074200           DELIMITED BY SIZE INTO FELTEXT                                 
074300         DISPLAY FELTEXT                                                  
074400         CALL FELLOG                                                      
074500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
074600         CONTINUE                                                         
074700     END-SEARCH                                                           
074800     .                                                                    
