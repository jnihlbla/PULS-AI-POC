000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W9042600.                                                
000400*AUTHOR.         STEFAN ANDREASSON, FRONTEC.                              
000500*DATE-WRITTEN.   97/02/25.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAM SOM VISAR SALDOINFORMATION TOTALT, DVS FÖR               
001100*        BÅDE CDCR OCH NDC. CDC SAMT MAX FYRA                             
001200*        NDC VISAS.                                                       
001300*        PROGRAMMET ÄR EN KOPIA AV W2034400                               
001400*        PROGRAMMET ÄR EN KOPIA AV W2036100 (040304)                      
001500*                                                                         
001600*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001700*        PROGRAMMET LÄSER      WLARTS (WDK7)                              
001800*        PROGRAMMET LÄSER      WLARTM (WDK9)                              
001900*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
002000*        PROGRAMMET LÄSER      WLERSA (WDD7)                              
002100*        PROGRAMMET LÄSER              WDB6                               
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: W90426T                                             
002500*        MID:         W90426I1                                            
002600*                                                                         
002700*    UTDATA.                                                              
002800*        MOD:         W90426O1                                            
002900*                                                                         
003000*    ÄNDRINGAR:                                                           
003100*    2012-01  E-TRACKER 10143271 CHINA WAREHOUSE PROJECT-1                
003200*                                                                         
003300*                                                                         
003400                                                                          
003500     SKIP3                                                                
003600 ENVIRONMENT DIVISION.                                                    
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(08)   VALUE 'W9042600'.            
004300                                                                          
004400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004600                                                                          
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900                                                                          
005000 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005100 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +576  COMP SYNC.        
005200                                                                          
005300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005400 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
005500                                                                          
005600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005700     88  NYCKLAR-OK                          VALUE 'J'.                   
005800     88  NYCKLAR-FEL                         VALUE 'N'.                   
005900                                                                          
006000 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006100     88  ALLT-OK                             VALUE 'J'.                   
006200                                                                          
006300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006400     88  EGEN-MID                            VALUE '9426'.                
006500     88  GODK-MID                            VALUE '9426' '2362'          
006600                                                   '2363' '2364'          
006700                                                   '2365' '2366'          
006800                                                   '2367' '2368'          
006900                                                   '2369'.                
007000     88  HELP-MID                            VALUE '0551'.                
007100 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007200 77  DAGENS-DATUM-SEKEL          PIC 9(8)    VALUE ZERO.                  
007300     EJECT                                                                
007400 01  FILLER                      PIC X(8)    VALUE 'ARB-FÄLT'.            
007500 01  ARBETSFAELT.                                                         
007600     03  DC-IX                   PIC  9(2)   VALUE ZERO.                  
007700     03  WS-KVOKS-TOT-CDC        PIC S9(7)   VALUE ZERO COMP-3.           
007800     03  WS-KVDISP-CLAG          PIC S9(7)   VALUE ZERO COMP-3.           
007900     03  WS-KVDISP-SLAG          PIC S9(7)   VALUE ZERO COMP-3.           
008000     03  WS-KVOKS-TOT-NDC        PIC S9(7)   VALUE ZERO COMP-3.           
008100     03  WS-KVROS                PIC S9(7)   VALUE ZERO.                  
008200                                                                          
008300       EJECT                                                              
008400                                                                          
008500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008600 01  GENERELLA-SUBPROGRAM.                                                
008700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009200     EJECT                                                                
009300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009400*01 -COPY WMEDAREA                                                        
009500     EJECT                                                                
009600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
009700*01 -COPY WMSGINIT                                                        
009800     EJECT                                                                
009900*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
010000*01  -COPY WDATAREA                                                       
010100     EJECT                                                                
010200     SKIP3                                                                
010300 01  MESSAGE-CODES.                                                       
010400     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
010500     03  ARTIKEL-SAKNAS-NDC      PIC X(3)    VALUE '305'.                 
010600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010700                                                                          
010800     EJECT                                                                
010900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
011000*                                                                         
011100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011200     SKIP3                                                                
011300*01  MID -COPY W90426I1                                                   
011400     EJECT                                                                
011500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011600     SKIP3                                                                
011700*01  -COPY WMSGAREA                                                       
011800     EJECT                                                                
011900     03  MOD REDEFINES MSG-AREA.                                          
012000*      05  -COPY W90426O1                                                 
012100     EJECT                                                                
012200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012300     SKIP3                                                                
012400*01  -COPY WMFSAREA                                                       
012500     EJECT                                                                
012600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012700*                                                                         
012800     EJECT                                                                
012900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013000     SKIP3                                                                
013100 01  NYCKLAR-TILL-DLI.                                                    
013200     03  W-IDARTNR-X.                                                     
013300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
013400     03  W-IDDC-X.                                                        
013500         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
013600     03  W-IDSKYLT-X.                                                     
013700         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
013800     SKIP2                                                                
013900     03 W-WDD7A1KY-MIN.                                                   
014000       05 W-IDARTNR-MIN7          PIC S9(9)  COMP-3 VALUE ZERO.           
014100       05 FILLER                  PIC S9(9)  COMP-3 VALUE ZERO.           
014200       05 FILLER                  PIC S9(3)  COMP-3 VALUE ZERO.           
014300                                                                          
014400     03 W-WDD7A1KY-MAX.                                                   
014500       05 W-IDARTNR-MAX7   PIC S9(9)  COMP-3 VALUE ZERO.                  
014600       05 FILLER           PIC S9(9)  COMP-3 VALUE +999999999.            
014700       05 FILLER           PIC S9(3)  COMP-3 VALUE +999.                  
014800                                                                          
014900     03  W-IDDC-B6-X.                                                     
015000         05 W-IDDC-B6                  PIC X(2).                          
015100                                                                          
015200*    --- STATUS-KOD FRÅN IMS                                              
015300 01  STATUS-WS                   PIC XX.                                  
015400     88  SEGMENT-FINNS                       VALUE '  '.                  
015500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015700     SKIP2                                                                
015800 01  GODK-STATUSKODER.                                                    
015900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016000     SKIP3                                                                
016100 01  SSA1                        PIC X(64).                               
016200 01  SSA2                        PIC X(64).                               
016300     EJECT                                                                
016400*    --- IMS FUNKTIONSKODER                                               
016500*01  -COPY W0003                                                          
016600     EJECT                                                                
016700*    ---  DLI INPUT-OUTPUT AREA                                           
016800                                                                          
016900 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTC01'.                      
017000 01  DLI-IO-ARTC01.                                                       
017100*    03  -COPY WDK601                                                     
017200     EJECT                                                                
017300                                                                          
017400 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTC11'.                      
017500 01  DLI-IO-ARTC11.                                                       
017600*    03  -COPY WDK611                                                     
017700     EJECT                                                                
017800                                                                          
017900 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTM01'.                      
018000 01  DLI-IO-ARTM01.                                                       
018100*    03  -COPY WDK901 -PRE WDK9-                                          
018200     EJECT                                                                
018300                                                                          
018400 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTS01'.                      
018500 01  DLI-IO-ARTS01.                                                       
018600*    03  -COPY WDK701                                                     
018700     EJECT                                                                
018800                                                                          
018900 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTS11'.                      
019000 01  DLI-IO-ARTS11.                                                       
019100*    03  -COPY WDK711                                                     
019200     EJECT                                                                
019300                                                                          
019400 01  FILLER         PIC X(24) VALUE 'DLI-IO-BENA11'.                      
019500 01  DLI-IO-BENA11.                                                       
019600*    03  -COPY WDD311                                                     
019700     EJECT                                                                
019800 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-ERSA01'.            
019900     SKIP3                                                                
020000 01  DLI-IO-AREA-ERSA01.                                                  
020100*  03  WLERSA01 -COPY WDD701  -PRE ERSA01-                                
020200     EJECT                                                                
020300 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-ERSA11'.            
020400     SKIP3                                                                
020500 01  DLI-IO-AREA-ERSA11.                                                  
020600*  03  WLERSA11 -COPY WDD702  -PRE ERSA11-                                
020700     EJECT                                                                
020800 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-ERSB01'.            
020900     SKIP3                                                                
021000 01  DLI-IO-AREA-ERSB01.                                                  
021100*  03  WLERSB01 -COPY WDD7A1  -PRE ERSB01-                                
021200                                                                          
021300 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
021400 01   DLI-IO-AREA-B601.                                                   
021500*     03  -COPY WDB601                                                    
021600                                                                          
021700 01  FILLER               PIC X(16)   VALUE 'WDB601 BAS '.                
021800 01   DLI-IO-AREA-B601-BAS.                                               
021900*     03  -COPY WDB601   -PRE BAS-                                        
022000                                                                          
022100                                                                          
022200 LINKAGE SECTION.                                                         
022300                                                                          
022400*01  -COPY W0009   -PRE MSG-                                              
022500     EJECT                                                                
022600*01  -COPY W0008  -PRE USEA-                                              
022700     05  FILLER                  PIC X.                                   
022800     EJECT                                                                
022900*01  -COPY W0008  -PRE ARTC-                                              
023000     05  FILLER                  PIC X.                                   
023100     EJECT                                                                
023200*01  -COPY W0008  -PRE ARTS-                                              
023300     05  FILLER                  PIC X.                                   
023400     EJECT                                                                
023500*01  -COPY W0008  -PRE ARTM-                                              
023600     05  FILLER                  PIC X.                                   
023700     EJECT                                                                
023800*01  -COPY W0008  -PRE BENA-                                              
023900     05  FILLER                  PIC X.                                   
024000     EJECT                                                                
024100*01  -COPY W0008  -PRE ERSA-                                              
024200     05  FILLER                  PIC X.                                   
024300     EJECT                                                                
024400*01  -COPY W0008  -PRE ERSB-                                              
024500     05  FILLER                  PIC X.                                   
024600     EJECT                                                                
024700*01  -COPY W0008  -PRE WDB6-                                              
024800     05  FILLER                  PIC X.                                   
024900     EJECT                                                                
025000 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
025100                                   ARTC-PCB ARTS-PCB                      
025200                           ARTM-PCB BENA-PCB ERSA-PCB ERSB-PCB            
025300                           WDB6-PCB.                                      
025400     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
025500                                   ARTC-PCB ARTS-PCB                      
025600                           ARTM-PCB BENA-PCB ERSA-PCB ERSB-PCB            
025700                           WDB6-PCB.                                      
025800                                                                          
025900     PERFORM IMS-GET-MSG                                                  
026000     IF SEGMENT-FINNS                                                     
026100       PERFORM A-INIT                                                     
026200       PERFORM B-KOLLA-NYCKLAR                                            
026300       IF NYCKLAR-OK                                                      
026400         PERFORM F-LAES-VISA-INFO                                         
026500       END-IF                                                             
026600       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
026700       PERFORM IMS-INSERT-MSG                                             
026800     END-IF                                                               
026900                                                                          
027000     MOVE ZERO TO RETURN-CODE                                             
027100     GOBACK                                                               
027200     .                                                                    
027300     EJECT                                                                
027400 A-INIT SECTION.                                                          
027500                                                                          
027600     IF MSG-DUBBLA-TRANSKODER                                             
027700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W90426I1                 
027800       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
027900       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
028000     ELSE                                                                 
028100       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W90426I1                  
028200       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
028300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
028400     END-IF                                                               
028500                                                                          
028600     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
028700     MOVE MSG-IDPFK TO MFS-IDPFK                                          
028800     MOVE MFS-IDTRANS TO W-IDTRANS                                        
028900                                                                          
029000     MOVE LOW-VALUE TO MSG-AREA                                           
029100     MOVE 'W90426O1' TO MFS-IDMOD                                         
029200     MOVE '9426' TO MOD-IDTRANS                                           
029300     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
029400                                                                          
029500     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W90426O1 + 4                  
029600                                                                          
029700     IF EGEN-MID OR HELP-MID                                              
029800       CONTINUE                                                           
029900     ELSE                                                                 
030000       MOVE SPACE TO MFS-KDTRTYP                                          
030100       MOVE '7' TO MFS-IDPFK                                              
030200     END-IF                                                               
030300                                                                          
030400     MOVE +2                 TO SPRAK-IX                                  
030500     MOVE 'GB '              TO MED-IDSKYLT                               
030600                                W-IDSKYLT                                 
030700                                                                          
030800     ACCEPT DAGENS-DATUM FROM DATE                                        
030900                                                                          
031000     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
031100     MOVE DAGENS-DATUM       TO DAT-I-TIDATUM                             
031200                                                                          
031300     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
031400                     DAT-O-TIDATUM DAT-KDSVAR                             
031500                                                                          
031600     IF DAT-KDSVAR-OK                                                     
031700       MOVE DAGENS-DATUM      TO DAGENS-DATUM-SEKEL(3:6)                  
031800       MOVE DAT-TISEKEL       TO DAGENS-DATUM-SEKEL(1:2)                  
031900     ELSE                                                                 
032000       STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                  
032100       DELIMITED BY SIZE INTO FELTEXT                                     
032200       CALL FELLOG                                                        
032300     END-IF                                                               
032400     .                                                                    
032500     EJECT                                                                
032600 B-KOLLA-NYCKLAR SECTION.                                                 
032700                                                                          
032800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
032900     MOVE '001'             TO MSGI-KDCALL                                
033000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
033100                               MSGI-IDLTERM-USER                          
033200     MOVE '9426'            TO MSGI-IDTRANS                               
033300     IF EGEN-MID                                                          
033400       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
033500       MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                              
033600     ELSE                                                                 
033700       IF  MID-IDARTNR-IN NUMERIC                                         
033800       AND MID-IDARTNR-IN > ZERO                                          
033900         MOVE MID-IDARTNR-IN                                              
034000                            TO MSGI-IDARTNR                               
034100       END-IF                                                             
034200     END-IF                                                               
034300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
034400                                                                          
034500     MOVE JA TO NYCKLAR-SW                                                
034600                                                                          
034700*    -- KONTROLL AV IDARTNR                                               
034800                                                                          
034900     IF MID-IDARTNR-IN NOT = ALL '+'                                      
035000       MOVE '7'         TO MFS-IDPFK                                      
035100       MOVE SPACE       TO MFS-KDTRTYP                                    
035200     END-IF                                                               
035300     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
035400     IF MSGI-IDARTNR NUMERIC                                              
035500       MOVE MSGI-IDARTNR TO WS-IDARTNR                                    
035600     ELSE                                                                 
035700       MOVE NEJ TO NYCKLAR-SW                                             
035800     END-IF                                                               
035900                                                                          
036000*    -- KONTROLL AV IDDC                                                  
036100                                                                          
036200     IF MID-IDDC-IN NOT = ALL '+'                                         
036300       MOVE '7'              TO MFS-IDPFK                                 
036400       MOVE SPACE            TO MFS-KDTRTYP                               
036500     END-IF                                                               
036600                                                                          
036700       MOVE MSGI-IDDC-KEY    TO W-IDDC-B6                                 
036800       PERFORM IMS-GU-WDB601                                              
036900       IF DCS-CDC OR DCS-DDC OR DCS-NDC-CN OR                             
036910          DCS-KDDC = SPACE                                                
037000         MOVE NEJ TO NYCKLAR-SW                                           
037100       END-IF                                                             
037200                                                                          
037300     IF NYCKLAR-FEL                                                       
037400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
037500       CALL WMEDKONV USING MED-WMEDAREA                                   
037600       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
037700       PERFORM MFS-RENSA-FAELT-UT                                         
037800     ELSE                                                                 
037900       MOVE WS-IDARTNR     TO W-IDARTNR                                   
038000       MOVE MSGI-IDDC-KEY  TO W-IDDC                                      
038100     END-IF                                                               
038200     .                                                                    
038300     EJECT                                                                
038400 F-LAES-VISA-INFO SECTION.                                                
038500                                                                          
038600     PERFORM IMS-GU-WDD7-ERSA01                                           
038700     IF SEGMENT-FINNS                                                     
038800        PERFORM IMS-GNP-WDD7-ERSA11                                       
038900        IF SEGMENT-FINNS                                                  
039000           PERFORM IMS-GNP-WDD7-ERSA11                                    
039100        END-IF                                                            
039200     END-IF                                                               
039300                                                                          
039400     PERFORM FA-LAES-ARTC-ARTM                                            
039500     IF ALLT-OK                                                           
039600       IF ART-FLERS = JA                                                  
039700          MOVE ART-IDARTNR                                                
039800                         TO W-IDARTNR-MIN7                                
039900                            W-IDARTNR-MAX7                                
040000          PERFORM IMS-GU-WDD7-ERSB01-MINMAX                               
040100          IF SEGMENT-FINNS                                                
040200             IF ERSB01-ERS-IDARTNR NOT = ZERO                             
040300                PERFORM IMS-GN-WDD7-ERSB01-MINMAX                         
040400             END-IF                                                       
040500          END-IF                                                          
040600       END-IF                                                             
040700       PERFORM FB-LAES-ARTS                                               
040800       IF NOT ALLT-OK                                                     
040900         PERFORM MFS-RENSA-NDC                                            
041000         MOVE ARTIKEL-SAKNAS-NDC TO MED-IDMFSFEL                          
041100         CALL WMEDKONV USING MED-WMEDAREA                                 
041200         MOVE MED-MFSFEL     TO MOD-TEMFSFEL                              
041300       END-IF                                                             
041400       PERFORM FC-LAES-BENA                                               
041500     ELSE                                                                 
041600       PERFORM MFS-RENSA-NDC                                              
041700       MOVE ARTIKEL-SAKNAS    TO MED-IDMFSFEL                             
041800       CALL WMEDKONV USING MED-WMEDAREA                                   
041900       MOVE MED-MFSFEL        TO MOD-TEMFSFEL                             
042000     END-IF                                                               
042100     .                                                                    
042200     EJECT                                                                
042300 FA-LAES-ARTC-ARTM SECTION.                                               
042400                                                                          
042500     PERFORM IMS-GU-ARTC01                                                
042600     IF SEGMENT-FINNS                                                     
042700       PERFORM IMS-GU-ARTM-ART                                            
042800       IF SEGMENT-FINNS                                                   
042900         PERFORM FAA-SUMMERA-OKS                                          
043000       END-IF                                                             
043100       PERFORM IMS-GNP-ARTC11                                             
043200       IF SEGMENT-FINNS                                                   
043300         PERFORM FAB-FLYTTA-TILL-MOD                                      
043400       ELSE                                                               
043500         MOVE NEJ           TO ALLT-SW                                    
043600       END-IF                                                             
043700     ELSE                                                                 
043800       MOVE NEJ             TO ALLT-SW                                    
043900     END-IF                                                               
044000     .                                                                    
044100     EJECT                                                                
044200 FAA-SUMMERA-OKS SECTION.                                                 
044300                                                                          
044400     COMPUTE WS-KVOKS-TOT-CDC    = WDK9-ART-KVOKS-BULK +                  
044500                                   WDK9-ART-KVOKS-DAG +                   
044600                                   WDK9-ART-KVOKS-VOR                     
044700     .                                                                    
044800     EJECT                                                                
044900 FAB-FLYTTA-TILL-MOD SECTION.                                             
045000                                                                          
045100     COMPUTE WS-KVDISP-CLAG  = CLAG-KVLS    -                             
045200                               WS-KVOKS-TOT-CDC -                         
045300                               CLAG-KVRESS                                
045400     .                                                                    
045500     EJECT                                                                
045600 FB-LAES-ARTS  SECTION.                                                   
045700                                                                          
045800     PERFORM IMS-GU-ARTS01                                                
045900     IF SEGMENT-SAKNAS                                                    
046000       MOVE ARTIKEL-SAKNAS-NDC TO MED-IDMFSFEL                            
046100       CALL WMEDKONV USING MED-WMEDAREA                                   
046200       MOVE MED-MFSFEL       TO MOD-TEMFSFEL                              
046300       MOVE NEJ             TO ALLT-SW                                    
046400     ELSE                                                                 
046500       MOVE +1              TO DC-IX                                      
046600       PERFORM IMS-GNP-ARTS11                                             
046700       PERFORM UNTIL SEGMENT-SAKNAS                                       
046800         MOVE SLAG-IDDC TO W-IDDC-B6                                      
046900         PERFORM IMS-GU-WDB601-BAS                                        
047100           IF ((DCS-SDC AND NOT DCS-SWEDEN) AND                           
047200               (BAS-DCS-SDC AND NOT BAS-DCS-SWEDEN))                      
047300           OR (DCS-NDC-NA      AND BAS-DCS-NDC-NA)                        
047400           OR (DCS-NDC-PF      AND BAS-DCS-NDC-PF)                        
047401           OR (DCS-NDC-SA      AND BAS-DCS-NDC-SA)                        
047410           OR (DCS-NDC-OTHERS  AND BAS-DCS-NDC-OTHERS)                    
047500           OR ((DCS-SDC AND DCS-SWEDEN) AND                               
047600               (BAS-DCS-SDC AND BAS-DCS-SWEDEN))                          
047700              IF DC-IX           < 6                                      
047800                PERFORM FBA-NDCINFO-TILL-MOD                              
047900                ADD +1           TO DC-IX                                 
048000              END-IF                                                      
048100           END-IF                                                         
048200         PERFORM IMS-GNP-ARTS11                                           
048300       END-PERFORM                                                        
048400       PERFORM UNTIL DC-IX > 5                                            
048500         PERFORM MFS-RENSA-FAELT-NDC-KOL                                  
048600         ADD +1             TO DC-IX                                      
048700       END-PERFORM                                                        
048800     END-IF                                                               
048900     .                                                                    
049000     EJECT                                                                
049100 FBA-NDCINFO-TILL-MOD SECTION.                                            
049200                                                                          
049300     MOVE ZERO              TO WS-KVDISP-SLAG                             
049400                               WS-KVOKS-TOT-NDC                           
049500                               WS-KVROS                                   
049600     COMPUTE WS-KVDISP-SLAG =                                             
049700             SLAG-KVLS - (SLAG-KVOKS-DAG + SLAG-KVOKS-BULK)               
049800                       - SLAG-KVRESS                                      
049900                                                                          
050000     COMPUTE WS-KVOKS-TOT-NDC =                                           
050100                        SLAG-KVOKS-DAG + SLAG-KVOKS-BULK                  
050200     COMPUTE WS-KVROS =                                                   
050300             SLAG-KVROS-BULK + SLAG-KVROS-DAG                             
050400                                                                          
050500     MOVE SLAG-IDDC         TO MOD-IDDC-NDC(DC-IX)                        
050600     MOVE SLAG-KVLS         TO MOD-KVLS-NDC(DC-IX)                        
050700     .                                                                    
050800     SKIP2                                                                
050900 FC-LAES-BENA SECTION.                                                    
051000                                                                          
051100     PERFORM IMS-GU-BENA01-BSEQ                                           
051200     IF SEGMENT-FINNS                                                     
051300       IF DCS-NDC-NA                                                      
051400         MOVE 'USA'            TO W-IDSKYLT                               
051500       END-IF                                                             
051600       PERFORM IMS-GNP-BENA11                                             
051700     END-IF                                                               
051800     .                                                                    
051900     EJECT                                                                
052000 MFS-RENSA-FAELT-UT SECTION.                                              
052100                                                                          
052200     PERFORM MFS-RENSA-NDC                                                
052300     .                                                                    
052400     SKIP2                                                                
052500 MFS-RENSA-NDC    SECTION.                                                
052600                                                                          
052700     MOVE +1                  TO DC-IX                                    
052800     PERFORM UNTIL DC-IX      >  5                                        
052900       PERFORM MFS-RENSA-FAELT-NDC-KOL                                    
053000       ADD +1                 TO DC-IX                                    
053100     END-PERFORM                                                          
053200     .                                                                    
053300     SKIP2                                                                
053400 MFS-RENSA-FAELT-NDC-KOL SECTION.                                         
053500                                                                          
053600     MOVE MFS-RENSA-FAELT   TO                                            
053700                               MOD-IDDC-NDC(DC-IX)                        
053800                               MOD-KVLS-NDC(DC-IX)                        
053900     .                                                                    
054000     EJECT                                                                
054100* --- IMS SEKTIONER ---                                                   
054200     SKIP3                                                                
054300 IMS-GET-MSG SECTION.                                                     
054400                                                                          
054500     MOVE '  QC' TO GODK-STATUSKODER                                      
054600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
054700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
054800     PERFORM IMS-STATUSKONTROLL                                           
054900     .                                                                    
055000     SKIP3                                                                
055100 IMS-INSERT-MSG SECTION.                                                  
055200                                                                          
055300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
055400     MOVE SPACE TO GODK-STATUSKODER                                       
055500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
055600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
055700     PERFORM IMS-STATUSKONTROLL                                           
055800     .                                                                    
055900     EJECT                                                                
056000 IMS-GU-ARTC01   SECTION.                                                 
056100                                                                          
056200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
056300          DELIMITED BY SIZE INTO SSA1                                     
056400     MOVE '  GE' TO GODK-STATUSKODER                                      
056500     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-ARTC01 SSA1                    
056600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
056700     PERFORM IMS-STATUSKONTROLL                                           
056800     .                                                                    
056900     EJECT                                                                
057000 IMS-GNP-ARTC11     SECTION.                                              
057100     MOVE 'WLARTC11' TO SSA1                                              
057200     MOVE '  GE' TO GODK-STATUSKODER                                      
057300     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-ARTC11 SSA1                   
057400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
057500     PERFORM IMS-STATUSKONTROLL                                           
057600     .                                                                    
057700     EJECT                                                                
057800 IMS-GU-ARTM-ART SECTION.                                                 
057900     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
058000          DELIMITED BY SIZE INTO SSA1                                     
058100     MOVE '  GE' TO GODK-STATUSKODER                                      
058200     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-ARTM01 SSA1                    
058300     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
058400     PERFORM IMS-STATUSKONTROLL                                           
058500     .                                                                    
058600     EJECT                                                                
058700 IMS-GU-ARTS01    SECTION.                                                
058800     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
058900          DELIMITED BY SIZE INTO SSA1                                     
059000     MOVE '  GE' TO GODK-STATUSKODER                                      
059100     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-ARTS01 SSA1                    
059200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
059300     PERFORM IMS-STATUSKONTROLL                                           
059400     .                                                                    
059500     EJECT                                                                
059600 IMS-GNP-ARTS11 SECTION.                                                  
059700     MOVE 'WLARTS11 ' TO SSA1                                             
059800     MOVE '  GE' TO GODK-STATUSKODER                                      
059900     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-ARTS11 SSA1                   
060000     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
060100     PERFORM IMS-STATUSKONTROLL                                           
060200     .                                                                    
060300     EJECT                                                                
060400 IMS-GU-BENA01-BSEQ SECTION.                                              
060500     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
060600          DELIMITED BY SIZE INTO SSA1                                     
060700     MOVE '  GE' TO GODK-STATUSKODER                                      
060800     CALL CBLTDLI USING GU BENA-PCB DLI-IO-BENA11 SSA1                    
060900     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
061000     PERFORM IMS-STATUSKONTROLL                                           
061100     .                                                                    
061200 IMS-GNP-BENA11 SECTION.                                                  
061300     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
061400          DELIMITED BY SIZE INTO SSA1                                     
061500     MOVE '  GE' TO GODK-STATUSKODER                                      
061600     CALL CBLTDLI USING GU BENA-PCB DLI-IO-BENA11 SSA1                    
061700     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
061800     PERFORM IMS-STATUSKONTROLL                                           
061900     .                                                                    
062000     EJECT                                                                
062100 IMS-GU-WDD7-ERSA01 SECTION.                                              
062200     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
062300            DELIMITED BY SIZE INTO SSA1                                   
062400     MOVE '  GE' TO GODK-STATUSKODER                                      
062500     CALL CBLTDLI USING GU ERSA-PCB DLI-IO-AREA-ERSA01 SSA1               
062600     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
062700     PERFORM IMS-STATUSKONTROLL                                           
062800     .                                                                    
062900     SKIP2                                                                
063000 IMS-GNP-WDD7-ERSA11 SECTION.                                             
063100     STRING 'WLERSA11(FLTEXT   =N)'                                       
063200            DELIMITED BY SIZE INTO SSA1                                   
063300     MOVE '  GE' TO GODK-STATUSKODER                                      
063400     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-AREA-ERSA11 SSA1              
063500     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
063600     PERFORM IMS-STATUSKONTROLL                                           
063700     .                                                                    
063800 IMS-GN-WDD7-ERSB01-MINMAX SECTION.                                       
063900     STRING 'WLERSB01(WDD7A1KY=>' W-WDD7A1KY-MIN                          
064000                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
064100            DELIMITED BY SIZE INTO SSA1                                   
064200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
064300     CALL CBLTDLI USING GN ERSB-PCB DLI-IO-AREA-ERSB01 SSA1               
064400     MOVE ERSB-STATUS-CODE TO STATUS-WS                                   
064500     PERFORM IMS-STATUSKONTROLL                                           
064600     .                                                                    
064700     EJECT                                                                
064800 IMS-GU-WDD7-ERSB01-MINMAX SECTION.                                       
064900     STRING 'WLERSB01(WDD7A1KY=>' W-WDD7A1KY-MIN                          
065000                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
065100            DELIMITED BY SIZE INTO SSA1                                   
065200     MOVE '  GE' TO GODK-STATUSKODER                                      
065300     CALL CBLTDLI USING GU ERSB-PCB DLI-IO-AREA-ERSB01 SSA1               
065400     MOVE ERSB-STATUS-CODE TO STATUS-WS                                   
065500     PERFORM IMS-STATUSKONTROLL                                           
065600     .                                                                    
065700     EJECT                                                                
065800 IMS-GU-WDB601    SECTION.                                                
065900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
066000          DELIMITED BY SIZE INTO SSA1                                     
066100     MOVE '  GE' TO GODK-STATUSKODER                                      
066200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
066300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
066400     PERFORM IMS-STATUSKONTROLL                                           
066500     IF SEGMENT-SAKNAS                                                    
066600         MOVE SPACE TO DCS-KDDC                                           
066700     END-IF                                                               
066800     .                                                                    
066900                                                                          
067000 IMS-GU-WDB601-BAS SECTION.                                               
067100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
067200          DELIMITED BY SIZE INTO SSA1                                     
067300     MOVE '  GE' TO GODK-STATUSKODER                                      
067400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-BAS SSA1             
067500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
067600     PERFORM IMS-STATUSKONTROLL                                           
067700     IF SEGMENT-SAKNAS                                                    
067800         MOVE SPACE TO BAS-DCS-KDDC                                       
067900     END-IF                                                               
068000     .                                                                    
068100                                                                          
068200 IMS-STATUSKONTROLL SECTION.                                              
068300                                                                          
068400     SET STATUS-IX TO 1                                                   
068500     SEARCH GODK-STATUS                                                   
068600       AT END                                                             
068700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
068800         DELIMITED BY SIZE INTO FELTEXT                                   
068900         CALL FELLOG                                                      
069000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
069100         CONTINUE                                                         
069200     END-SEARCH                                                           
069300     .                                                                    
