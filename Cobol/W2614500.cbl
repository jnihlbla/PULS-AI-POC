000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2614500.                                                
000400 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000500 DATE-WRITTEN.   09/02/16.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        FRAMSTÄLLER UNDERLAG FÖR WARNING LAST CALL                       
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WDK6                                       
001300*        PROGRAMMET LÄSER      WDK6                                       
001400*        PROGRAMMET LÄSER      WDL8                                       
002200*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300     SKIP2                                                                
003400*          --- ARTIKLAR EV. WARNING LAST CALL                             
003500     SELECT W26144                     ASSIGN TO W26145D1.                
003600*          --- ARTIKLAR WARNING LAST CALL UPPDAT                          
003700     SELECT W26146                     ASSIGN TO W26145D2.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  W26144                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  -COPY W26144   -L.                                                   
004800     SKIP3                                                                
004900 FD  W26146                                                               
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200                                                                          
005300*01  POST -COPY W26144 -PRE  UT-  -L.                                     
005400     EJECT                                                                
005500 WORKING-STORAGE SECTION.                                                 
005600                                                                          
005700 77  IDPGM                       PIC X(8)    VALUE 'W2614500'.            
005800 01  CHKP-VAR.                                                            
005900     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
006000     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
006100     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
006200     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
006300     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
006400     03 CHKP-MAX                 PIC S9(3)   VALUE +200 COMP-3.           
006500 77  JA                          PIC X       VALUE 'J'.                   
006600 77  NEJ                         PIC X       VALUE 'N'.                   
006700 77  IX                          PIC 9(2)    VALUE ZERO.                  
006710 77  WS-IX                       PIC 9(2)    VALUE ZERO.                  
006800     SKIP2                                                                
006900 01  FELTEXT.                                                             
007000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007200 77  WS-SPAR-BEANST-GODK         PIC X(25)   VALUE SPACE.                 
007300 77  W26144-EOF-SW               PIC X       VALUE 'N'.                   
007400     88  END-OF-W26144                       VALUE 'J'.                   
007500 77  SW-K611-OK                  PIC X       VALUE 'N'.                   
007600 77  SW-KAMPANJ                  PIC X       VALUE 'N'.                   
007610 77  SW-DEKAL                    PIC X       VALUE 'N'.                   
007700 77  SW-SATS-OK                  PIC X       VALUE 'N'.                   
007800 77  SW-AUT-GODK                 PIC X       VALUE 'N'.                   
007900     EJECT                                                                
008000 01  SWITCHAR.                                                            
008100     03  INGAR-SATS-SW           PIC X.                                   
008200         88  INGAR-I-SATS    VALUE 'J'.                                   
008300*      --- VALID IDDC CODES                                               
008400*                                                                         
008500*01    -COPY WWDCKONS                                                     
008600     EJECT                                                                
008700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008800 01  FILLER REDEFINES DAGENS-DATUM.                                       
008900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009200                                                                          
009300 77  DAGENS-DATUM-Y2K            PIC 9(8)  VALUE ZERO.                    
009400     EJECT                                                                
009500 01  DYNAMISKA-SUBPROGRAM.                                                
009600*                                                                         
009700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009910     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010000     SKIP3                                                                
010100 01  ARB.                                                                 
010900     03  WS-KVOKS-C1             PIC S9(6)   VALUE ZERO.                  
011000     03  IDARTNR-WS              PIC S9(9)   VALUE ZERO COMP-3.           
011100     03  WS-FLERS                PIC X(1)    VALUE SPACE.                 
011200                                                                          
011700     03  WS-AUTO-USERID          PIC X(8)    VALUE 'W2616800'.            
011710     03  WS-IDKUNDNR             PIC S9(7)   VALUE ZERO COMP-3.           
011800     03  WKVSKRANT               PIC S9(7).                               
011900     03  IX-RAD                  PIC S9(3)   VALUE ZERO COMP-3.           
012000     03  WS-TEMEMO               PIC X(25)   VALUE SPACE.                 
012100     03  WS-KVBR                 PIC S9(7)   VALUE ZERO COMP-3.           
012200     03  ANT-SKROTU              PIC S9(3)   VALUE ZERO COMP-3.           
012300     03  MAX-ANT-SKROTU          PIC S9(3)   VALUE 500  COMP-3.           
012400     03  WS-KVBEART              PIC S9(7)   VALUE ZERO COMP-3.           
012500     03  WS-MAX-SUBEL            PIC S9(7)   VALUE ZERO COMP-3.           
012700     03  WS-SUARTSTD             PIC 9(7)V9(2) VALUE ZERO.                
012710     03  WS-TIAAAA               PIC 9(4)    VALUE ZERO.                  
012720     03  FILLER  REDEFINES  WS-TIAAAA.                                    
012730         05  WS-TISEKEL          PIC 9(2).                                
012740         05  WS-TIAA-VECKA       PIC 9(2).                                
012741     03  WS-TIAAAA-1             PIC 9(4)    VALUE ZERO.                  
012750     03  WS-TIVV                 PIC S9(3)   VALUE ZERO  COMP-3.          
012760     03  WS-ANTAL-OI             PIC S9(7)   VALUE ZERO COMP-3.           
012800                                                                          
012900 01  W-DATUM.                                                             
013000     05  W-DATUM-DATE    PIC X(6).                                        
013010     EJECT                                                                
013020*- - - - - - - - - - - - - -  PARAMETRAR TILL WDATKONV                    
013030                                                                          
013040*01  -COPY WDATAREA                                                       
013100     EJECT                                                                
013200*    --- PARAMETRAR TILL POSTSUM                                          
013300*                                                                         
013400*01  -COPY W0005   -PRE  POSTSUM-                                         
013500     EJECT                                                                
013600 01  IN-AREA-START               PIC X(24)   VALUE                        
013700                                             'IN-AREA-START'.             
013800     SKIP2                                                                
013900                                                                          
014000*01  AREA -COPY W26144     -PRE IN-                                       
014100*                                                                         
014200     SKIP2                                                                
014300                                                                          
014400*01  AREA -COPY W26144     -PRE UT-                                       
014500     EJECT                                                                
014600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014700     SKIP3                                                                
014800 01  NYCKLAR-TILL-DLI.                                                    
014900     03  W-IDARTNR-X.                                                     
015000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
015100     03  W-KDSEGKEY-X.                                                    
015200         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
019500                                                                          
019510     03  W-TIAAAA-X.                                                      
019520         05  W-TIAAAA            PIC 9(4).                                
020500                                                                          
022500     EJECT                                                                
022600     SKIP2                                                                
022700*    --- STATUS-KOD FRÅN IMS                                              
022800 01  STATUS-WS                   PIC XX.                                  
022900     88  SEGMENT-FINNS                       VALUE '  '.                  
023000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
023100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
023200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
023300     88  IMS-EJ-OK                           VALUE 'XD'.                  
023400     SKIP2                                                                
023500 01  GODK-STATUSKODER.                                                    
023600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023700     SKIP3                                                                
023800 01  SSA1                        PIC X(128).                              
023900 01  SSA2                        PIC X(64).                               
024000 01  SSA3                        PIC X(64).                               
024100 01  SSA4                        PIC X(64).                               
024200     EJECT                                                                
024300*    --- IMS FUNKTIONSKODER                                               
024400*01  -COPY W0003                                                          
024500     EJECT                                                                
024600*    ---  DLI INPUT-OUTPUT AREA                                           
024700                                                                          
024800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
024900 01  DLI-IO-WDK601.                                                       
025000*    03  -COPY WDK601                                                     
025100     EJECT                                                                
025200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
025300 01  DLI-IO-WDK611.                                                       
025400*    03  -COPY WDK611                                                     
025500     EJECT                                                                
033010 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL801'.                      
033020 01  DLI-IO-WDL801.                                                       
033030*    03  -COPY WDL801                                                     
033040     EJECT                                                                
033050 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL811'.                      
033060 01  DLI-IO-WDL811.                                                       
033070*    03  -COPY WDL811                                                     
033100                                                                          
035200     EJECT                                                                
036500 LINKAGE SECTION.                                                         
036600                                                                          
036700*01  -COPY W0009   -PRE MSG-                                              
036800                                                                          
036900*01  -COPY W0008  -PRE WDK6-                                              
037000     05  FILLER                  PIC X.                                   
037100                                                                          
040320*01  -COPY W0008  -PRE WDL8-                                              
040330     05  FILLER                  PIC X.                                   
040400     EJECT                                                                
040500 PROCEDURE DIVISION  USING MSG-PCB                                        
040600           WDK6-PCB WDL8-PCB.                                             
040900 MAIN SECTION.                                                            
041000     ENTRY 'DLITCBL' USING MSG-PCB                                        
041100           WDK6-PCB WDL8-PCB.                                             
041400                                                                          
041500     SKIP2                                                                
041600     PERFORM A-INIT                                                       
041800     PERFORM S01-LAES-W26144                                              
041900     PERFORM UNTIL END-OF-W26144                                          
042000       IF CHKP-ANT > CHKP-MAX                                             
042100         PERFORM X-TAG-CHECKPOINT                                         
042200       END-IF                                                             
042300       MOVE IN-IDARTNR TO W-IDARTNR                                       
042400                          IDARTNR-WS                                      
042500       PERFORM IMS-GET-WDK601                                             
042600       IF SEGMENT-FINNS                                                   
042900          IF ART-FLIART = JA                                              
043000             MOVE ART-FLIART TO INGAR-SATS-SW                             
043100          ELSE                                                            
043200             MOVE NEJ        TO INGAR-SATS-SW                             
043300          END-IF                                                          
043400          MOVE ART-FLERS     TO WS-FLERS                                  
043500          PERFORM B-LAES-SKROTINFO                                        
043600                                                                          
043700          IF  SW-K611-OK = JA                                             
044000              PERFORM D-UPPDATERA                                         
044100          END-IF                                                          
044200       END-IF                                                             
044300       PERFORM S01-LAES-W26144                                            
044400     END-PERFORM                                                          
044500                                                                          
044600                                                                          
044700     PERFORM Z-FINIT                                                      
044800                                                                          
044900     MOVE ZERO TO RETURN-CODE                                             
045000     GOBACK                                                               
045100     .                                                                    
045200     EJECT                                                                
045300 A-INIT SECTION.                                                          
045400     SKIP2                                                                
045500                                                                          
045600     PERFORM IMS-RESTART                                                  
045700                                                                          
045800     OPEN INPUT W26144                                                    
045900         OUTPUT W26146                                                    
046000                                                                          
046100                                                                          
046200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
046300                                                                          
046400     ACCEPT W-DATUM-DATE FROM DATE                                        
046500     ACCEPT DAGENS-DATUM FROM DATE                                        
046600                                                                          
046700     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM-Y2K                 
046800                                                                          
047010     MOVE 'IDAG' TO DAT-KDDATFORM                                         
047020     CALL WDATKONV USING DAT-KDDATFORM,                                   
047030                         DAT-I-TIDATUM,                                   
047040                         DAT-O-TIDATUM,                                   
047050                         DAT-KDSVAR                                       
047060                                                                          
047070     IF DAT-KDSVAR-FEL                                                    
047080        DISPLAY '****  FEL I WDATKONV  *******'                           
047091        CALL FELLOG                                                       
047092     END-IF                                                               
047093                                                                          
047094     MOVE DAT-TIVV       TO WS-TIVV                                       
047095     MOVE DAT-TISEKEL    TO WS-TISEKEL                                    
047096     MOVE DAT-TIAA-VECKA TO WS-TIAA-VECKA                                 
047097     MOVE WS-TIAAAA      TO WS-TIAAAA-1                                   
047100     SUBTRACT +1         FROM WS-TIAAAA-1                                 
047200     .                                                                    
047300     EJECT                                                                
047400 B-LAES-SKROTINFO SECTION.                                                
047500                                                                          
047600     PERFORM IMS-GET-WDK611                                               
047700     IF  SEGMENT-FINNS                                                    
048400        MOVE JA               TO SW-K611-OK                               
049000                                                                          
049910        IF INGAR-SATS-SW = JA                                             
049920           PERFORM BA-LAES-SATS-OI                                        
049930        END-IF                                                            
050000     ELSE                                                                 
050500        MOVE NEJ              TO SW-K611-OK                               
050600     END-IF                                                               
050700     .                                                                    
050800     EJECT                                                                
050900 BA-LAES-SATS-OI SECTION.                                                 
051000                                                                          
051001*    EFTERSOM INGÅENDE SATSARTIKLAR INTE FINNS MED FRÅN S&T               
051002*    SÅ ANVÄNDER VI ORDERINGÅNG I STÄLLET FÖR FÖRSÄLJNING                 
051003*    FÖR DESSA (ANTAL SÅLDA SISTA RULLANDE ÅR < 150)                      
051004                                                                          
051005     MOVE ZERO            TO WS-ANTAL-OI                                  
051006     MOVE IN-IDARTNR      TO W-IDARTNR                                    
051007     MOVE WS-TIAAAA       TO W-TIAAAA                                     
051008     PERFORM IMS-GU-WDL811                                                
051009     IF SEGMENT-FINNS                                                     
051013        MOVE +1           TO WS-IX                                        
051014        PERFORM UNTIL WS-IX >= WS-TIVV                                    
051015           ADD AAR-KVOI-SATS (WS-IX) TO WS-ANTAL-OI                       
051017           ADD +1         TO WS-IX                                        
051018        END-PERFORM                                                       
051019     END-IF                                                               
051020     MOVE WS-TIAAAA-1     TO W-TIAAAA                                     
051021     PERFORM IMS-GU-WDL811                                                
051022     IF SEGMENT-FINNS                                                     
051026        MOVE WS-TIVV      TO WS-IX                                        
051027        PERFORM UNTIL WS-IX >  53                                         
051028           ADD AAR-KVOI-SATS (WS-IX) TO WS-ANTAL-OI                       
051030           ADD +1         TO WS-IX                                        
051031        END-PERFORM                                                       
051032     END-IF                                                               
051033     IF (IN-SULEVANT-RAAR + WS-ANTAL-OI) > 150                            
051034*       EJ AKTUELL FÖR SKROT                                              
051035        MOVE NEJ TO SW-K611-OK                                            
051036     END-IF                                                               
051037     .                                                                    
051038     EJECT                                                                
052800 D-UPPDATERA SECTION.                                                     
052900                                                                          
053000     MOVE IDARTNR-WS TO W-IDARTNR                                         
053100                                                                          
053400     PERFORM DB-UPPD-WDK611                                               
053700     PERFORM DF-SKRIV-BEV-FIL                                             
053900                                                                          
057600     .                                                                    
057700     EJECT                                                                
057800 DB-UPPD-WDK611     SECTION.                                              
057900                                                                          
058000     PERFORM IMS-GET-WDK601                                               
058100     PERFORM IMS-GET-WDK611                                               
058110     IF CLAG-KVSLAGER    = 0  AND                                         
058130        CLAG-FLSKROT-BEV = JA AND                                         
058140        CLAG-TISKPREL    = IN-TISKPREL                                    
058150        CONTINUE                                                          
058160     ELSE                                                                 
058200        MOVE ZERO            TO CLAG-KVSLAGER                             
058210        MOVE 9999            TO CLAG-TISLJUST                             
058220        MOVE 9.9             TO CLAG-RESLJUST                             
058300        MOVE JA              TO CLAG-FLSKROT-BEV                          
058310        MOVE IN-TISKPREL     TO CLAG-TISKPREL                             
058400                                                                          
058500        PERFORM IMS-REPL-WDK611                                           
058510     END-IF                                                               
058600                                                                          
060100     .                                                                    
060200     EJECT                                                                
085900 DF-SKRIV-BEV-FIL  SECTION.                                               
086000                                                                          
086300     MOVE IN-AREA          TO UT-AREA                                     
086301     ADD WS-ANTAL-OI       TO IN-SULEVANT-RAAR                            
086700                                                                          
086800     PERFORM S11-SKRIV-W26146                                             
087000     .                                                                    
087100     EJECT                                                                
087200 Z-FINIT SECTION.                                                         
087300                                                                          
087400                                                                          
087500     CLOSE W26144                                                         
087600           W26146                                                         
087700     SKIP2                                                                
087800     MOVE 'S' TO POSTSUM-OPKOD                                            
087900     CALL POSTSUM USING POSTSUM-PARM                                      
088000     .                                                                    
088100     EJECT                                                                
088200 S01-LAES-W26144  SECTION.                                                
088300     SKIP2                                                                
088400     READ W26144 INTO IN-AREA                                             
088500     AT END                                                               
088600        SET END-OF-W26144 TO TRUE                                         
088700                                                                          
088800     NOT AT END                                                           
088900        MOVE 'W26144'   TO POSTSUM-FDNAMN                                 
089000        MOVE 'W26145D1' TO POSTSUM-DDNAMN2                                
089100        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
089200        CALL POSTSUM USING POSTSUM-PARM                                   
089300                                                                          
089400     END-READ                                                             
089500     .                                                                    
089600     EJECT                                                                
089700 S11-SKRIV-W26146 SECTION.                                                
089800                                                                          
089900     WRITE UT-POST FROM UT-AREA                                           
090000                                                                          
090100     MOVE 'UT'       TO POSTSUM-TRANSTYP                                  
090200     MOVE 'W26146'   TO POSTSUM-FDNAMN                                    
090300     MOVE 'W26145D2' TO POSTSUM-DDNAMN2                                   
090400     CALL POSTSUM USING POSTSUM-PARM                                      
090500     .                                                                    
090600     EJECT                                                                
090700 X-TAG-CHECKPOINT   SECTION.                                              
090800                                                                          
090900* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
091000* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
091100     PERFORM IMS-CHECKPOINT                                               
091200     MOVE ZERO TO CHKP-ANT                                                
091300* --- LÄS OM DATABAS OM DET BEHÖVS                                        
091400     .                                                                    
091500     EJECT                                                                
091600* --- IMS SEKTIONER ---                                                   
091700                                                                          
091800 IMS-GET-WDK601 SECTION.                                                  
091900                                                                          
092000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
092100          DELIMITED BY SIZE INTO SSA1                                     
092200     MOVE '  GE' TO GODK-STATUSKODER                                      
092300     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
092400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
092500     PERFORM IMS-STATUSKONTROLL                                           
092600     .                                                                    
092700     EJECT                                                                
092800 IMS-GET-WDK611 SECTION.                                                  
092900                                                                          
093000     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
093100          DELIMITED BY SIZE INTO SSA1                                     
093200     MOVE '  GE' TO GODK-STATUSKODER                                      
093300     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK611 SSA1                  
093400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
093500     PERFORM IMS-STATUSKONTROLL                                           
093600     .                                                                    
093700     SKIP3                                                                
093800 IMS-REPL-WDK611 SECTION.                                                 
093900                                                                          
094000     MOVE '  ' TO GODK-STATUSKODER                                        
094400     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
094500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
094600     PERFORM IMS-STATUSKONTROLL                                           
094700     ADD +1  TO CHKP-ANT                                                  
094800     .                                                                    
094900     EJECT                                                                
123410 IMS-GU-WDL811 SECTION.                                                   
123420                                                                          
123430     STRING 'WDL801  (IDARTNR  =' W-IDARTNR-X ')'                         
123440          DELIMITED BY SIZE INTO SSA1                                     
123441     STRING 'WDL811  (TIAAAA   =' W-TIAAAA-X ')'                          
123442          DELIMITED BY SIZE INTO SSA2                                     
123450     MOVE '  GE' TO GODK-STATUSKODER                                      
123460     CALL CBLTDLI USING GU WDL8-PCB DLI-IO-WDL811 SSA1 SSA2               
123470     MOVE WDL8-STATUS-CODE TO STATUS-WS                                   
123480     PERFORM IMS-STATUSKONTROLL                                           
123490     .                                                                    
123491     EJECT                                                                
123500 IMS-RESTART SECTION.                                                     
123600     SKIP2                                                                
123700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
123800     MOVE '  ' TO GODK-STATUSKODER                                        
123900     CALL CBLTDLI USING XRST MSG-PCB                                      
124000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
124100                        CHKP-AREA-LENGTH CHKP-AREA                        
124200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
124300     PERFORM IMS-STATUSKONTROLL                                           
124400     .                                                                    
124500     SKIP3                                                                
124600 IMS-CHECKPOINT SECTION.                                                  
124700     SKIP2                                                                
124800     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
124900     MOVE '  XD' TO GODK-STATUSKODER                                      
125000     CALL CBLTDLI USING CHKP MSG-PCB                                      
125100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
125200                        CHKP-AREA-LENGTH CHKP-AREA                        
125300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
125400     PERFORM IMS-STATUSKONTROLL                                           
125500                                                                          
125600     IF IMS-EJ-OK                                                         
125700       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
125800       DISPLAY FELTEXT                                                    
125900       CALL FELLOG                                                        
126000     END-IF                                                               
126100     .                                                                    
126200     EJECT                                                                
126300 IMS-STATUSKONTROLL SECTION.                                              
126400     SKIP2                                                                
126500     SET STATUS-IX TO 1                                                   
126600     SEARCH GODK-STATUS                                                   
126700       AT END                                                             
126800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
126900           DELIMITED BY SIZE INTO FELTEXT                                 
127000         DISPLAY FELTEXT                                                  
127100         CALL FELLOG                                                      
127200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
127300         CONTINUE                                                         
127400     END-SEARCH                                                           
127500     .                                                                    
