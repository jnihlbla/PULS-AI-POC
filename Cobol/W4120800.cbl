000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.             W4120800.                                        
000400 AUTHOR.                 FRANK THORBURN.                                  
000500 DATE-WRITTEN.           OKT. 1988.                                       
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        *************  SB    PROGRAM  ********************               
001100*                                                                         
001200*        PROGRAMMET LÄSER KUNDORDERREGISTRET WLKNDD (WDB4).               
001300*        AKTUELLT DATUM HÄMTAS FRÅN DATKORT OCH JÄMFÖRS                   
001400*        MED STARTDATUM, VECKA OCH DAGNUMMER PÅ BASEN.                    
001500*        STÄMMER VECKA OCH DAG ÖVERENS SKAPAS ETT ORDER-                  
001600*        HUVUD (R50).                                                     
001700*                                                                         
001800     EJECT                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000                                                                          
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*    ---- INFIL:                                                          
002600     SELECT  W41207-IN     ASSIGN TO UT-S-W41208D1.                       
002700*                          *** ORDERNUMMER-REGISTER                       
002800     SKIP2                                                                
002900*    ---- UTFILER:                                                        
003000     SELECT  W41207-UT     ASSIGN TO UT-S-W41208D2.                       
003100*                          *** ORDERNUMMER-REGISTER                       
003200                                                                          
003300     SELECT  W41208        ASSIGN TO UT-S-W41208D3.                       
003400*                          *** ORDERHUVUD R50                             
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700                                                                          
003800 FILE SECTION.                                                            
003900                                                                          
004000 FD  W41207-IN                                                            
004100     LABEL RECORD STANDARD                                                
004200     RECORDING  F                                                         
004300     BLOCK CONTAINS 0.                                                    
004400                                                                          
004500*01  POST -COPY W41207     -PRE I07-        -L.                           
004600     SKIP3                                                                
004700                                                                          
004800 FD  W41207-UT                                                            
004900     LABEL RECORD STANDARD                                                
005000     RECORDING  F                                                         
005100     BLOCK CONTAINS 0.                                                    
005200                                                                          
005300*01  POST -COPY W41207     -PRE U07-        -L.                           
005400     EJECT                                                                
005500 FD  W41208                                                               
005600     LABEL RECORD STANDARD                                                
005700     RECORDING  V                                                         
005800     BLOCK CONTAINS 0.                                                    
005900                                                                          
006000*01  POST -COPY W411500    -PRE U08-             -L.                      
006100     EJECT                                                                
006200 WORKING-STORAGE SECTION.                                                 
006300*    -COPY WY2000W1                                                       
006400     SKIP3                                                                
006500 77  PGM-NAMN                PIC X(8)    VALUE 'W4120800'.                
006600 77  FELTEXT                 PIC X(80)   VALUE SPACE.                     
006700 77  DAT-ID                  PIC X(8)    VALUE 'WDATUM'.                  
006800 77  JA                      PIC X(1)    VALUE 'J'.                       
006900 77  NEJ                     PIC X(1)    VALUE 'N'.                       
007000 77  IX                      PIC S9(9)   VALUE +0   COMP SYNC.            
007100 77  MIN-ORDNR               PIC S9(5)   VALUE ZERO.                      
007200 77  MAX-ORDNR               PIC S9(5)   VALUE ZERO.                      
007300 77  AKT-ORDNR               PIC S9(5)   VALUE ZERO.                      
007400 77  JAMN-W-DAY              PIC S9(2)   VALUE ZERO.                      
007500 77  UDDA-W-DAY              PIC S9(2)   VALUE ZERO.                      
007600 77  W41207-EOF              PIC X(1)    VALUE 'N'.                       
007700                                                                          
008202 77  SW-SKAPA-R50            PIC X(1).                                    
008203     88  SKAPA-R50                       VALUE 'J'.                       
008204                                                                          
008205 77  BIPKLAR-SW              PIC X       VALUE 'N'.                       
008206     88  BIPKLAR-RAD                     VALUE 'J'.                       
008300                                                                          
008400     EJECT                                                                
008500 01  DAGENS-DATUM            PIC 9(6).                                    
008600 01  FILLER REDEFINES DAGENS-DATUM.                                       
008700     03  DATAA               PIC 9(2).                                    
008800     03  DATMM               PIC 9(2).                                    
008900     03  DATDD               PIC 9(2).                                    
009000                                                                          
009100 01  VECKA-DAG               PIC 9(3).                                    
009200 01  FILLER REDEFINES VECKA-DAG.                                          
009300     03  VECKA-VV            PIC 9(2).                                    
009400     03  DAG-D               PIC 9(1).                                    
009500                                                                          
009600 01  WEEK                    PIC 9(2).                                    
009700 01  DYNAMISKA-SUBPROGRAM.                                                
009800     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM '.                
009900     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
010000     03  FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
010100     03  DATKORT             PIC X(8)    VALUE 'DATKORT '.                
010200     EJECT                                                                
010300*01  -COPY WDATKORT                                                       
010400     EJECT                                                                
010500*    ----  PARAMETRAR TILL POSTSUM                                        
010600                                                                          
010700*01  -COPY W0005       -PRE POSTSUM-.                                     
010800     EJECT                                                                
010900*    ----  IN-AREA (ORDERNUMMER-REGISTER)                                 
011000 01  FILLER                  PIC X(24) VALUE 'I07-AREA  '.                
011100                                                                          
011200 01  I07-AREA.                                                            
011300*03  FILLER   -PRE OREG-IN-    -COPY W41207                               
011400     EJECT                                                                
011500*    ----  UT-AREA                                                        
011600 01  FILLER                  PIC X(24) VALUE 'U07-AREA  '.                
011700                                                                          
011800 01  U07-AREA.                                                            
011900*03  FILLER   -PRE OREG-UT-    -COPY W41207                               
012000     EJECT                                                                
012100 01  FILLER                  PIC X(24) VALUE 'U08-AREA  '.                
012200                                                                          
012300 01  U08-AREA.                                                            
012400*03  FILLER   -PRE R50-        -COPY W411500                              
012500     EJECT                                                                
012600 01  FILLER                  PIC X(16) VALUE 'IMS-WS'.                    
012700 01  NYCKLAR-TILL-DLI.                                                    
012800                                                                          
012900     03  W-WDA5B1KY-MAX-X.                                                
013000     05  W-IDDISTR-MAX       PIC S9(5)   COMP-3   VALUE 99999.            
013100     05  W-IDKUNDNR-MAX      PIC S9(7)   COMP-3   VALUE 9999999.          
013200     05  FILLER              PIC X(19)   VALUE HIGH-VALUE.                
013300                                                                          
013400     03  W-WDA5B1KY-MIN-X.                                                
013500     05  W-IDDISTR-MIN       PIC S9(5)   COMP-3   VALUE ZERO.             
013600     05  W-IDKUNDNR-MIN      PIC S9(7)   COMP-3   VALUE ZERO.             
013700     05  FILLER              PIC X(19)   VALUE  LOW-VALUE.                
013800     SKIP3                                                                
013900*    ---- STATUSKOD FRÅN IMS                                              
014000                                                                          
014100 01  STATUS-WS               PIC X(2).                                    
014200     88  SEGMENT-SLUT                     VALUE 'GB'.                     
014300     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
014400     SKIP3                                                                
014500 01  GODK-STATUSKODER.                                                    
014600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014700     SKIP3                                                                
014800 01  SSA1                    PIC X(200).                                  
014900     EJECT                                                                
015000*01  -COPY W0003                                                          
015100     EJECT                                                                
015200 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA'.               
015300     SKIP3                                                                
015400 01  DLI-IO-AREA.                                                         
015500     03  IO-AREA             PIC X(180)       VALUE SPACE.                
015600     SKIP3                                                                
015700*03  WLKNDD01 -COPY WDB401                     -RED IO-AREA               
015800     EJECT                                                                
015900*03  WLKNDD14 -COPY WDB414                     -RED IO-AREA               
016000     EJECT                                                                
016100 01  DLI-IO-AREA1.                                                        
016200     03  IO-AREA1            PIC X(256)       VALUE SPACE.                
016300     SKIP3                                                                
016400*03  WLORDR01 -COPY WDA5B1                     -RED IO-AREA1              
016500     EJECT                                                                
016600 LINKAGE SECTION.                                                         
016700     SKIP2                                                                
016800*01  -COPY W0008      -PRE  KNDD-                                         
016900       05  FILLER                PIC X.                                   
017000     EJECT                                                                
017100*01  -COPY W0008      -PRE  ORDR-                                         
017200       05  FILLER                PIC X.                                   
017300     EJECT                                                                
017400 PROCEDURE DIVISION  USING  KNDD-PCB ORDR-PCB.                            
017500     ENTRY 'DLITCBL' USING  KNDD-PCB ORDR-PCB.                            
017600                                                                          
017700     PERFORM A-INIT                                                       
017800     PERFORM IMS-GET-KNDD                                                 
017900                                                                          
018000     PERFORM S01-LAS-ORDREG                                               
018100     MOVE OREG-IN-IDORDNR-MIN            TO MIN-ORDNR                     
018200     MOVE OREG-IN-IDORDNR-MAX            TO MAX-ORDNR                     
018300     MOVE OREG-IN-IDORDNR-AKT            TO AKT-ORDNR                     
018400     PERFORM UNTIL SEGMENT-SLUT                                           
018500       EVALUATE KNDD-SEG-NAME-FB                                          
018600         WHEN 'WDB401  ' PERFORM B-FLYTTA                                 
018700         WHEN 'WDB414  ' PERFORM C-BEHANDLA-O-FLYTTA                      
018800       END-EVALUATE                                                       
018900       PERFORM IMS-GET-KNDD                                               
019000     END-PERFORM                                                          
019100     MOVE AKT-ORDNR                 TO OREG-UT-IDORDNR-AKT                
019200     MOVE MIN-ORDNR                 TO OREG-UT-IDORDNR-MIN                
019300     MOVE MAX-ORDNR                 TO OREG-UT-IDORDNR-MAX                
019400     PERFORM S03-SKRIV-ORDREG                                             
019500     PERFORM Z-FINIT                                                      
019600     MOVE ZERO TO RETURN-CODE                                             
019700     GOBACK                                                               
019800                                                                          
019900     .                                                                    
020000     EJECT                                                                
020100 A-INIT SECTION.                                                          
020200                                                                          
020300     OPEN INPUT  W41207-IN                                                
020400                                                                          
020500     OPEN OUTPUT W41207-UT                                                
020600                 W41208                                                   
020700                                                                          
020800     INITIALIZE DAGENS-DATUM                                              
020900                VECKA-DAG                                                 
021000                                                                          
021100     CALL DATKORT USING PGM-NAMN DAT-ID DATUMKORT                         
021200     MOVE D-AAR    TO DATAA                                               
021300     MOVE D-MAANAD TO DATMM                                               
021400     MOVE D-DAG    TO DATDD                                               
021500     MOVE D-VECKA  TO VECKA-VV                                            
021600     MOVE D-DAGNR  TO DAG-D                                               
021700                                                                          
021800     MOVE PGM-NAMN TO POSTSUM-PROGNAMN                                    
021900                                                                          
022000     MOVE 'R50'                    TO R50-IDTYP                           
022100                                                                          
022200     .                                                                    
022300     EJECT                                                                
022400 B-FLYTTA SECTION.                                                        
022500                                                                          
022600     MOVE KUND-IDDISTR             TO R50-IDDISTR                         
022700                                      W-IDDISTR-MIN                       
022800                                      W-IDDISTR-MAX                       
022900                                                                          
023000     MOVE KUND-IDKUNDNR            TO R50-IDKUNDNR                        
023100                                      W-IDKUNDNR-MIN                      
023200                                      W-IDKUNDNR-MAX                      
023300     .                                                                    
023400     EJECT                                                                
023500 C-BEHANDLA-O-FLYTTA SECTION.                                             
023600                                                                          
023700     MOVE ORD-TISTADAT   TO TMP1-YYMMDD                                   
023800     MOVE DAGENS-DATUM   TO TMP2-YYMMDD                                   
023902     PERFORM WY2000P1                                                     
024000     IF (TMP1-YYMMDD < TMP2-YYMMDD ) OR                                   
024100        (ORD-TISTADAT = DAGENS-DATUM)                                     
024200       PERFORM CA-KOLLA-VECKA-DAG                                         
024300       IF SKAPA-R50                                                       
024400         PERFORM CB-KOLLA-OM-BIPKLAR-RAD-FINNS                            
024500       END-IF                                                             
024600       IF SKAPA-R50 AND BIPKLAR-RAD                                       
024700         PERFORM CC-TRANSFORMERA-SPRAKKOD                                 
024800         MOVE ORD-KDFRAKT              TO R50-KDFRAKT                     
024900         MOVE AKT-ORDNR                TO R50-IDORDNR                     
025000         MOVE ORD-BEKUNDRF             TO R50-BEKUNDRF-001                
025100         MOVE DAGENS-DATUM             TO R50-TIREF1                      
025200         MOVE ORD-KDORDKL              TO R50-KDORDKL                     
025300         MOVE ORD-KDNOTES              TO R50-KDNOTES                     
025400         MOVE ORD-KDROPACK (2)         TO R50-KDROPACK                    
025500         MOVE ORD-KDFAKTYP             TO R50-KDFAKTYP                    
025600         MOVE ORD-IDFTG                TO R50-IDFTG                       
025700         IF ORD-BEVARREF = SPACE OR (ORD-KDFAKTYP = 'G' OR 'N')           
025800           MOVE ORD-IDKONTO            TO R50-IDKONTO                     
025900         ELSE                                                             
026000           MOVE ORD-BEVARREF           TO R50-BEVARREF                    
026100         END-IF                                                           
026200         MOVE ORD-IDKST                TO R50-IDKST                       
026300         MOVE ORD-IDANALYS             TO R50-IDANALYS                    
026400                                                                          
026500         MOVE ZERO                     TO R50-KDCLAGER                    
026600                                          R50-TIPLLEVD                    
026700                                          R50-TIBEGPD                     
026800                                          R50-KDPRGRP                     
026900                                          R50-REOMRTAL                    
027000                                          R50-KDTULLVE                    
027100                                          R50-FLRESTN                     
027200                                          R50-IDPRODNR-002                
027300                                          R50-KDPERSON                    
027400         MOVE 'A'                      TO R50-KDMASK                      
027500                                                                          
027600         PERFORM S02-SKRIV-R50                                            
027700         PERFORM CD-BERAKNA-NYTT-ORDERNUMMER                              
027800       END-IF                                                             
027900     END-IF                                                               
028000     MOVE NEJ TO BIPKLAR-SW                                               
028100     .                                                                    
028200     EJECT                                                                
028300 CA-KOLLA-VECKA-DAG SECTION.                                              
028400                                                                          
028500     MOVE NEJ                     TO SW-SKAPA-R50                         
028600                                                                          
028700     MOVE ZERO                    TO JAMN-W-DAY                           
028800                                     UDDA-W-DAY                           
028900     COMPUTE WEEK = VECKA-VV / 2                                          
029000     IF (WEEK * 2) = VECKA-VV                                             
029100*                              *** JÄMN VECKA ***                         
029200       COMPUTE JAMN-W-DAY = DAG-D                                         
029300       IF  JAMN-W-DAY < +6                                                
029400       AND ORD-TID (JAMN-W-DAY) NOT = ZERO                                
029500         MOVE JA                  TO SW-SKAPA-R50                         
029600       END-IF                                                             
029700     ELSE                                                                 
029800*                              *** UDDA VECKA ***                         
029900       COMPUTE UDDA-W-DAY = DAG-D + 5                                     
030000       IF  UDDA-W-DAY < +11                                               
030100       AND ORD-TID (UDDA-W-DAY) NOT = ZERO                                
030200         MOVE JA                  TO SW-SKAPA-R50                         
030300       END-IF                                                             
030400     END-IF                                                               
030500     .                                                                    
030600     EJECT                                                                
030700 CB-KOLLA-OM-BIPKLAR-RAD-FINNS SECTION.                                   
030800                                                                          
030900     PERFORM IMS-GU-ORDR-WDA5B                                            
031000     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR BIPKLAR-RAD          
031100       IF SEQB-KDFAKTYP = ORD-KDFAKTYP                                    
031200         MOVE JA TO BIPKLAR-SW                                            
031602       END-IF                                                             
031603       PERFORM IMS-GN-ORDR-WDA5B                                          
031604     END-PERFORM                                                          
031642     .                                                                    
031700     EJECT                                                                
031800 CC-TRANSFORMERA-SPRAKKOD SECTION.                                        
035602                                                                          
035603     EVALUATE ORD-IDSKYLT                                                 
035604       WHEN 'S  ' MOVE '0'          TO R50-KDSPRAK                        
035605       WHEN 'GB ' MOVE '1'          TO R50-KDSPRAK                        
035606       WHEN 'F  ' MOVE '2'          TO R50-KDSPRAK                        
035607       WHEN 'E  ' MOVE '3'          TO R50-KDSPRAK                        
035608       WHEN 'D  ' MOVE '4'          TO R50-KDSPRAK                        
035609       WHEN 'I  ' MOVE '5'          TO R50-KDSPRAK                        
035610       WHEN OTHER MOVE SPACE        TO R50-KDSPRAK                        
035611     END-EVALUATE                                                         
035612                                                                          
035613     .                                                                    
035614     EJECT                                                                
035615 CD-BERAKNA-NYTT-ORDERNUMMER SECTION.                                     
035616                                                                          
035617     IF AKT-ORDNR < MAX-ORDNR                                             
035618       COMPUTE AKT-ORDNR = AKT-ORDNR + 1                                  
035619     ELSE                                                                 
035620       COMPUTE AKT-ORDNR = MIN-ORDNR                                      
035621     END-IF                                                               
035622                                                                          
035623     .                                                                    
035624     EJECT                                                                
035625 S01-LAS-ORDREG SECTION.                                                  
035626                                                                          
035627     READ W41207-IN INTO I07-AREA                                         
035628       AT END MOVE JA              TO W41207-EOF                          
035629     END-READ                                                             
035630     IF W41207-EOF = NEJ                                                  
035631       MOVE 'W41207-IN'            TO POSTSUM-FDNAMN                      
035632       MOVE 'W41208D1'             TO POSTSUM-DDNAMN2                     
035633       CALL POSTSUM USING POSTSUM-PARM                                    
035634     END-IF                                                               
035635                                                                          
035636     .                                                                    
035637     EJECT                                                                
035638 S02-SKRIV-R50 SECTION.                                                   
035639                                                                          
035700     WRITE U08-POST FROM U08-AREA                                         
035800     MOVE 'R50'                   TO POSTSUM-TRANSTYP                     
035900     MOVE 'W41208'                TO POSTSUM-FDNAMN                       
036000     MOVE 'W41208D3'              TO POSTSUM-DDNAMN2                      
036100     CALL POSTSUM USING POSTSUM-PARM                                      
036200                                                                          
036300     .                                                                    
036400     EJECT                                                                
036500 S03-SKRIV-ORDREG SECTION.                                                
036600                                                                          
036700     WRITE U07-POST FROM U07-AREA                                         
036800     MOVE 'W41207-UT'             TO POSTSUM-FDNAMN                       
036900     MOVE 'W41208D2'              TO POSTSUM-DDNAMN2                      
037000     CALL POSTSUM USING POSTSUM-PARM                                      
037100                                                                          
037200     .                                                                    
037300     EJECT                                                                
037400 Z-FINIT SECTION.                                                         
037500     SKIP2                                                                
037600     CLOSE W41207-IN                                                      
037700           W41207-UT                                                      
037800           W41208                                                         
037900                                                                          
038000     MOVE 'S' TO POSTSUM-OPKOD                                            
038100     CALL POSTSUM USING POSTSUM-PARM                                      
038200                                                                          
038300     .                                                                    
038400     EJECT                                                                
038500*    ---- IMS SEKTIONER                                                   
038600                                                                          
038700 IMS-GET-KNDD SECTION.                                                    
038800                                                                          
038900     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
039000     CALL CBLTDLI USING GN  KNDD-PCB DLI-IO-AREA                          
039100     MOVE KNDD-STATUS-CODE TO STATUS-WS                                   
039200     PERFORM IMS-STATUSKONTROLL                                           
039300     .                                                                    
039400     SKIP2                                                                
039500 IMS-GU-ORDR-WDA5B SECTION.                                               
039600                                                                          
039700     STRING 'WLORDR01(WDA5B1KY>=' W-WDA5B1KY-MIN-X                        
039800                    '&WDA5B1KY<=' W-WDA5B1KY-MAX-X ')'                    
039900            DELIMITED BY SIZE INTO SSA1                                   
040000     MOVE '  GEGB'            TO GODK-STATUSKODER                         
040100     CALL CBLTDLI USING GU ORDR-PCB DLI-IO-AREA1 SSA1                     
040200     MOVE ORDR-STATUS-CODE TO STATUS-WS                                   
040300     PERFORM IMS-STATUSKONTROLL                                           
040400     .                                                                    
040500     SKIP2                                                                
040600 IMS-GN-ORDR-WDA5B SECTION.                                               
040700                                                                          
040800     STRING 'WLORDR01(WDA5B1KY>=' W-WDA5B1KY-MIN-X                        
040900                    '&WDA5B1KY<=' W-WDA5B1KY-MAX-X ')'                    
041000            DELIMITED BY SIZE INTO SSA1                                   
041100     MOVE '  GEGB'            TO GODK-STATUSKODER                         
041200     CALL CBLTDLI USING GN ORDR-PCB DLI-IO-AREA1 SSA1                     
041300     MOVE ORDR-STATUS-CODE TO STATUS-WS                                   
041400     PERFORM IMS-STATUSKONTROLL                                           
041500     .                                                                    
041600     SKIP2                                                                
041700 IMS-STATUSKONTROLL SECTION.                                              
041800                                                                          
041900     SET STATUS-IX TO 1                                                   
042000     SEARCH GODK-STATUS                                                   
042100     AT END                                                               
042200     STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                               
042300     DELIMITED BY SIZE INTO FELTEXT                                       
042400     CALL FELLOG                                                          
042500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
042600     END-SEARCH                                                           
042700                                                                          
042800     .                                                                    
042900     EJECT                                                                
043000*    -COPY WY2000P1                                                       
