000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2717900.                                                
000300 AUTHOR.         STEFAN ANDREASSON.                                       
000400 DATE-WRITTEN.   99/09/16.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000900*        PROGRAM FÖR ATT LÄGGA UPP BUYER OCH KÖPTABELL                    
001000*                                                                         
001100*                                                                         
001200*                                                                         
001300                                                                          
001400     EJECT                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*          ---                                                            
002200     SELECT W27177                     ASSIGN TO W27179D1.                
002300*          ---                                                            
002400     SELECT W27178                     ASSIGN TO W27179D2.                
002500*          ---                                                            
002600     SELECT W27179                     ASSIGN TO W27179D3.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP2                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W27177                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600*01  POST -COPY W27177 -PRE  W27177-  -L.                                 
003700     SKIP3                                                                
003800 FD  W27178                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200*01  POST -COPY W27178 -PRE  W27178-  -L.                                 
004300     SKIP3                                                                
004400 FD  W27179                                                               
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700                                                                          
004800*01  POST -COPY W27179 -PRE  UT-  -L.                                     
004900                                                                          
005000     EJECT                                                                
005100 WORKING-STORAGE SECTION.                                                 
005200                                                                          
005300*    -- CHECKED BY WY2000                                                 
005400 77  IDPGM                       PIC X(8)    VALUE 'W2717900'.            
005500 77  JA                          PIC X       VALUE 'J'.                   
005600 77  NEJ                         PIC X       VALUE 'N'.                   
005700                                                                          
005800 77  W27177-EOF-SW               PIC X       VALUE 'N'.                   
005900     88  END-OF-W27177                       VALUE 'J'.                   
006000 77  W27178-EOF-SW               PIC X       VALUE 'N'.                   
006100     88  END-OF-W27178                       VALUE 'J'.                   
006200 77  STATNR-SW                      PIC X   VALUE 'N'.                    
006300     88  CCC-ARTIKEL-JA                     VALUE 'J'.                    
006400     88  CCC-ARTIKEL-NEJ                    VALUE 'N'.                    
006500                                                                          
006600                                                                          
006700 01  ARBETSAREOR.                                                         
006800     03 WS-DAGENS-TID            PIC 9(10)  VALUE ZERO.                   
006900     03 WS-ANTAL-W27177          PIC 9(8)   VALUE ZERO.                   
007000     03 WS-ANTAL-W27178          PIC 9(8)   VALUE ZERO.                   
007100     03 WS-ANTAL-UT              PIC 9(8)   VALUE ZERO.                   
007200     03 WS-IDPERSON-BUY          PIC 9(3)   VALUE ZERO.                   
007300                                                                          
007400     03 WS-IDPROJ                PIC X(4).                                
007500     03 WS-IDPROJ-REDEFINE       REDEFINES WS-IDPROJ.                     
007600        05 WS-IDPROJ-2POS        PIC X(2).                                
007700        05 WS-IDPROJ-FILLER      PIC X(2).                                
007800                                                                          
007900*01  -COPY WWSTATNR                                                       
008000                                                                          
008100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008200                                                                          
008300 01  FELTEXT.                                                             
008400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008500     03  FELTEXT-STATUS          PIC X(2)    VALUE SPACE.                 
008600     03  FILLER                  PIC X       VALUE SPACE.                 
008700     03  FELTEXT-TEXT            PIC X(69)   VALUE SPACE.                 
008800                                                                          
008900 01  DYNAMISKA-SUBPROGRAM.                                                
009000*                                                                         
009100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009500     03  W271BUYR                PIC X(8)    VALUE 'W271BUYR'.            
009600                                                                          
009700     EJECT                                                                
009800 01  IN-AREA-START-W27177    PIC X(24)   VALUE                            
009900                                 'IN-AREA-START-W27177 '.                 
010000                                                                          
010100                                                                          
010200*01  AREA -COPY W27177     -PRE W27177-                                   
010300                                                                          
010400     EJECT                                                                
010500 01  IN-AREA-START-W27178    PIC X(24)   VALUE                            
010600                                 'IN-AREA-START-W27178 '.                 
010700                                                                          
010800                                                                          
010900*01  AREA -COPY W27178     -PRE W27178-                                   
011000                                                                          
011100     EJECT                                                                
011200 01  UT-AREA-START           PIC X(24)   VALUE                            
011300                                 'UT-AREA-START  '.                       
011400                                                                          
011500                                                                          
011600*01  AREA -COPY W27179     -PRE UT-                                       
011700                                                                          
011800*    --- PARAMETRAR TILL W271BUYR                                         
011900*                                                                         
012000*01  -COPY W271BUYR                                                       
012200     EJECT                                                                
012201                                                                          
012210*    --- PARAMETRAR TILL POSTSUM                                          
012220*                                                                         
012230*01  -COPY W0005   -PRE  POSTSUM-                                         
012240     EJECT                                                                
012291                                                                          
012300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012400                                                                          
012500     SKIP3                                                                
012600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012700     SKIP3                                                                
012800 01  NYCKLAR-TILL-DLI.                                                    
012900     03  W-IDDC-B6-X.                                                     
013000         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
013100                                                                          
013200*                                                                         
013300     SKIP2                                                                
013400*    --- STATUS-KOD FRÅN IMS                                              
013500 01  STATUS-WS                   PIC XX.                                  
013600     88  SEGMENT-FINNS                       VALUE '  '.                  
013700     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
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
015000 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
015100 01   DLI-IO-AREA-B601.                                                   
015200*     03  -COPY WDB601                                                    
015300     EJECT                                                                
015400                                                                          
015500 LINKAGE SECTION.                                                         
015600                                                                          
015700     EJECT                                                                
015800*01  -COPY W0008      -PRE WDB6-                                          
015900     05  FILLER                  PIC X.                                   
016000                                                                          
016100 01  BUYR-WDB6-PCB                 PIC X.                                 
016200 01  BUYR-WDL7-PCB                 PIC X.                                 
016300 01  BUYR-WDL8-PCB                 PIC X.                                 
016400     EJECT                                                                
016500 PROCEDURE DIVISION  USING WDB6-PCB                                       
016600                           BUYR-WDB6-PCB BUYR-WDL7-PCB                    
016700                           BUYR-WDL8-PCB.                                 
016800     ENTRY 'DLITCBL' USING WDB6-PCB                                       
016900                           BUYR-WDB6-PCB BUYR-WDL7-PCB                    
017000                           BUYR-WDL8-PCB.                                 
017100                                                                          
017200 MAIN SECTION.                                                            
017300                                                                          
017400     PERFORM A-INIT                                                       
017500                                                                          
017600     PERFORM S01-LAES-W27177                                              
017700     PERFORM S02-LAES-W27178                                              
017800                                                                          
017900     PERFORM UNTIL END-OF-W27178                                          
018000       IF END-OF-W27177                                                   
018100       OR W27178-IDARTNR < W27177-IDARTNR                                 
018200         DISPLAY 'FEL PÅ REGISTREN : '  W27178-IDARTNR W27178-IDDC        
018300         PERFORM S02-LAES-W27178                                          
018400       ELSE                                                               
018500         IF END-OF-W27178                                                 
018600         OR W27177-IDARTNR < W27178-IDARTNR                               
018700           PERFORM S01-LAES-W27177                                        
018800         ELSE                                                             
018900           MOVE W27178-IDARTNR                                            
019000                             TO UT-IDARTNR                                
019100           MOVE W27178-IDDC  TO UT-IDDC                                   
019200                                W-IDDC-B6                                 
019300           PERFORM IMS-GU-WDB601                                          
019400                                                                          
019500           MOVE ZERO         TO UT-IDPERSON-BUY                           
019600                                                                          
019700           IF W27178-IDDC-REF = SPACE                                     
019800           AND (DCS-NDC-CN                                                
019900           OR DCS-USA)                                                    
020000             IF W27178-IDREFTAB = SPACE                                   
020100             OR W27178-IDREFTAB NUMERIC                                   
020200               MOVE 'A'      TO UT-IDREFTAB                               
020300               PERFORM S03-SKRIV-W27179                                   
020400             END-IF                                                       
020500           ELSE                                                           
020600             MOVE ZERO       TO UT-IDREFTAB                               
020610             MOVE NEJ        TO BUYR-FLBUYER-CHANGED                      
020620                                BUYR-FLTABLE-CHANGED                      
020700                                                                          
020800             IF W27178-FLBUYUPD = NEJ                                     
020900               PERFORM B-BUYERTILLDELNING                                 
021000             END-IF                                                       
021100             IF W27178-FLTABUPD = NEJ                                     
021200               PERFORM C-KOEPTABELL                                       
021300             END-IF                                                       
021400                                                                          
021500             IF UT-IDREFTAB > ZERO                                        
021600             OR UT-IDPERSON-BUY > ZERO                                    
021620             OR BUYR-FLBUYER-CHANGED = JA                                 
021630             OR BUYR-FLTABLE-CHANGED = JA                                 
021700                MOVE W27178-FLBUYUPD  TO UT-FLBUYUPD                      
021800                MOVE W27178-FLTABUPD  TO UT-FLTABUPD                      
021810                MOVE DCS-KDDCSTYR-BUY TO UT-KDDCSTYR-BUY                  
021900                PERFORM S03-SKRIV-W27179                                  
022000             END-IF                                                       
022100           END-IF                                                         
022200                                                                          
022300           PERFORM S02-LAES-W27178                                        
022400         END-IF                                                           
022500       END-IF                                                             
022600     END-PERFORM                                                          
022700     PERFORM Z-FINIT                                                      
022800                                                                          
022900     MOVE ZERO TO RETURN-CODE                                             
023000     GOBACK                                                               
023100     .                                                                    
023200                                                                          
023300     EJECT                                                                
023400 A-INIT SECTION.                                                          
023500                                                                          
023600     OPEN INPUT  W27177                                                   
023700                 W27178                                                   
023800     OPEN OUTPUT W27179                                                   
023900                                                                          
024000     ACCEPT DAGENS-DATUM FROM DATE                                        
024100     ACCEPT WS-DAGENS-TID   FROM TIME                                     
024200     DISPLAY 'START TID : ' WS-DAGENS-TID                                 
024201                                                                          
024210     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
024300     .                                                                    
024400                                                                          
024500     EJECT                                                                
024600 B-BUYERTILLDELNING SECTION.                                              
024700                                                                          
024800     MOVE NEJ TO STATNR-SW                                                
024900     MOVE W27177-IDSTATNR       TO STAT-IDSTATNR                          
025000     IF STAT-IDSTATNR-YES                                                 
025100       MOVE JA TO STATNR-SW                                               
025200     ELSE                                                                 
025300       MOVE NEJ TO STATNR-SW                                              
025400     END-IF                                                               
025500***                                                                       
025600***                                                                       
025700     IF DCS-KDDCSTYR-BUY = 0                                              
025800        PERFORM S20-PREPARE-BUYR-LINK-AREA                                
025900        CALL W271BUYR USING BUYR-W271BUYR                                 
026000                            BUYR-WDB6-PCB BUYR-WDL7-PCB                   
026100                            BUYR-WDL8-PCB                                 
026200        IF  BUYR-KDSVAR-OK                                                
026210           IF BUYR-FLBUYER-CHANGED = JA                                   
026300              MOVE BUYR-IDPERSON-BUY     TO UT-IDPERSON-BUY               
026310           ELSE                                                           
026320              MOVE BUYR-IDPERSON-BUY-IN  TO UT-IDPERSON-BUY               
026330           END-IF                                                         
026400        ELSE                                                              
026500           DISPLAY 'W271BUYR-ERROR1:' BUYR-TEXT                           
026600           CALL FELLOG                                                    
026700        END-IF                                                            
026800     END-IF                                                               
107400     .                                                                    
107500                                                                          
107600     EJECT                                                                
109700 C-KOEPTABELL SECTION.                                                    
109800                                                                          
109901     IF DCS-KDDCSTYR-REFTAB = 0                                           
109910        IF W27178-FLBUYUPD   = JA                                         
109911*-------- SINCE BUYER IS LOCKED W271BUYR WASNT CALLED IN B- .             
109914        OR BUYR-IDARTNR  NOT = W27178-IDARTNR                             
109915*-------- OR THE PART HAS KDDCSTYR-BUY <> 0                               
109916*-------- THEN MUST CALL W271BUYR HERE TO GET IDREFTAB.                   
109920           PERFORM S20-PREPARE-BUYR-LINK-AREA                             
109930           CALL W271BUYR USING BUYR-W271BUYR                              
109940                               BUYR-WDB6-PCB BUYR-WDL7-PCB                
109950                               BUYR-WDL8-PCB                              
109960           IF BUYR-KDSVAR-OK                                              
109970              CONTINUE                                                    
109980           ELSE                                                           
109990              DISPLAY 'W271BUYR-ERROR2:' BUYR-TEXT                        
109991              CALL FELLOG                                                 
109992           END-IF                                                         
109993        END-IF                                                            
109994        IF BUYR-FLTABLE-CHANGED = JA                                      
110000           MOVE BUYR-IDREFTAB     TO UT-IDREFTAB                          
110010        ELSE                                                              
110020           MOVE BUYR-IDREFTAB-IN  TO UT-IDREFTAB                          
110030        END-IF                                                            
110100     END-IF                                                               
139800     .                                                                    
139900                                                                          
140000     EJECT                                                                
140100 Z-FINIT SECTION.                                                         
140200     ACCEPT WS-DAGENS-TID   FROM TIME                                     
140300     DISPLAY 'SLUT  TID : ' WS-DAGENS-TID                                 
140400     CLOSE W27177                                                         
140500           W27178                                                         
140600           W27179                                                         
140700     DISPLAY 'ANTAL W27177    : ' WS-ANTAL-W27177                         
140800     DISPLAY 'ANTAL W27178    : ' WS-ANTAL-W27178                         
140900     DISPLAY 'ANTAL UT        : ' WS-ANTAL-UT                             
140910     SKIP2                                                                
140920     MOVE 'S' TO POSTSUM-OPKOD                                            
140930     CALL POSTSUM USING POSTSUM-PARM                                      
141000     .                                                                    
141100                                                                          
141200     EJECT                                                                
141300 S01-LAES-W27177  SECTION.                                                
141400     READ W27177    INTO W27177-AREA                                      
141500     AT END                                                               
141600        SET END-OF-W27177 TO TRUE                                         
141700     NOT AT END                                                           
141800        ADD 1                TO WS-ANTAL-W27177                           
141820        MOVE 'W27177 '       TO POSTSUM-FDNAMN                            
141830        MOVE 'W27179D1'      TO POSTSUM-DDNAMN2                           
141831        MOVE SPACE           TO POSTSUM-TRANSTYP                          
141840        CALL POSTSUM USING POSTSUM-PARM                                   
142000     END-READ                                                             
142100     .                                                                    
142200                                                                          
142300     EJECT                                                                
142400 S02-LAES-W27178      SECTION.                                            
142500     READ W27178    INTO W27178-AREA                                      
142600     AT END                                                               
142700        SET END-OF-W27178 TO TRUE                                         
142800     NOT AT END                                                           
142900        ADD 1                TO WS-ANTAL-W27178                           
143020        MOVE 'W27178 '       TO POSTSUM-FDNAMN                            
143030        MOVE 'W27179D2'      TO POSTSUM-DDNAMN2                           
143040        MOVE W27178-IDDC     TO POSTSUM-TRANSTYP                          
143050        CALL POSTSUM USING POSTSUM-PARM                                   
143100     END-READ                                                             
143200     .                                                                    
143300                                                                          
143400     EJECT                                                                
143500 S03-SKRIV-W27179     SECTION.                                            
143600                                                                          
143700     WRITE UT-POST FROM UT-AREA                                           
143800     ADD 1                   TO WS-ANTAL-UT                               
143810                                                                          
143820     MOVE 'W27179 '          TO POSTSUM-FDNAMN                            
143830     MOVE 'W27179D3'         TO POSTSUM-DDNAMN2                           
143840     MOVE UT-IDDC            TO POSTSUM-TRANSTYP                          
143850     CALL POSTSUM USING POSTSUM-PARM                                      
143900     .                                                                    
144000     EJECT                                                                
144100* --- IMS SEKTIONER ---                                                   
144200     SKIP3                                                                
144300     EJECT                                                                
144301                                                                          
144310 S20-PREPARE-BUYR-LINK-AREA SECTION.                                      
144320                                                                          
144330     MOVE 001                        TO BUYR-KDCALL                       
144340     MOVE W27178-IDARTNR             TO BUYR-IDARTNR                      
144350     MOVE W27178-IDPERSON-BUY        TO BUYR-IDPERSON-BUY-IN              
144351     MOVE W27178-FLBUYUPD            TO BUYR-FLBUYUPD                     
144352     MOVE W27178-IDREFTAB            TO BUYR-IDREFTAB-IN                  
144353     MOVE W27178-FLTABUPD            TO BUYR-FLTABUPD                     
144360     MOVE W27177-KDPRODSL            TO BUYR-KDPRODSL                     
144370     MOVE W27177-IDFKNGRP            TO BUYR-IDFKNGRP                     
144380     MOVE W27177-KDUART              TO BUYR-KDUART                       
144390     MOVE W27178-IDDC                TO BUYR-IDDC                         
144391     MOVE W27178-IDDC-REF            TO BUYR-IDDC-REF                     
144392     MOVE W27178-FLFLYG              TO BUYR-FLFLYG                       
144393     MOVE W27177-KDFARLIG            TO BUYR-KDFARLIG                     
144394     MOVE W27177-VLARTNTO            TO BUYR-VLARTNTO                     
144395     MOVE W27178-KVPB-REF-REOI-SUM   TO BUYR-KVPB-TOT                     
144398     MOVE W27177-TISOP               TO BUYR-TISOP                        
144399     MOVE W27177-TIURPROD            TO BUYR-TIURPROD                     
144400     MOVE W27177-FLBSNES             TO BUYR-FLBSNES                      
144401     MOVE W27177-KVEOP               TO BUYR-KVEOP                        
144402     .                                                                    
144403     EJECT                                                                
144410                                                                          
144500 IMS-GU-WDB601    SECTION.                                                
144600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
144700          DELIMITED BY SIZE INTO SSA1                                     
144800     MOVE '  ' TO GODK-STATUSKODER                                        
144900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
145000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
145100     PERFORM IMS-STATUSKONTROLL                                           
145200     .                                                                    
145300     EJECT                                                                
145400 IMS-STATUSKONTROLL SECTION.                                              
145500                                                                          
145600     SET STATUS-IX TO 1                                                   
145700     SEARCH GODK-STATUS                                                   
145800       AT END                                                             
145900         STRING 'OTILLÅTEN RETURKOD FRÅN IMS: ' STATUS-WS                 
146000           DELIMITED BY SIZE INTO FELTEXT-TEXT                            
146100         DISPLAY FELTEXT                                                  
146200         CALL FELLOG                                                      
146300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
146400         CONTINUE                                                         
146500     END-SEARCH                                                           
146600     .                                                                    
                                                                                
