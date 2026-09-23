000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2247000.                                                
000300 AUTHOR.         CHESTER COUCH.                                           
000400 DATE-WRITTEN.   2023-11-14.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000900*        THIS PROGRAM SETS THE PROCUREMENT IDREFTAB IN WDK711             
001000*        BASED ON THE PROCUREMENT SEGMENT CODE KDANSKSEG.                 
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
002200     SELECT W22168                     ASSIGN TO W22470D1.                
002300*          ---                                                            
002400     SELECT W27178                     ASSIGN TO W22470D2.                
002500*          ---                                                            
002600     SELECT W27179                     ASSIGN TO W22470D3.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP2                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W22168                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600*01  POST -COPY W22168 -PRE  W22168-  -L.                                 
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
005400 77  IDPGM                       PIC X(8)    VALUE 'W2247000'.            
005500 77  JA                          PIC X       VALUE 'J'.                   
005600 77  NEJ                         PIC X       VALUE 'N'.                   
005700                                                                          
005800 77  W22168-EOF-SW               PIC X       VALUE 'N'.                   
005900     88  END-OF-W22168                       VALUE 'J'.                   
006000 77  W27178-EOF-SW               PIC X       VALUE 'N'.                   
006100     88  END-OF-W27178                       VALUE 'J'.                   
006200                                                                          
006300 01  ARBETSAREOR.                                                         
006400     03 WS-DAGENS-TID            PIC 9(10)  VALUE ZERO.                   
006500     03 WS-COUNT-W22168          PIC 9(8)   VALUE ZERO.                   
006600     03 WS-COUNT-W27178          PIC 9(8)   VALUE ZERO.                   
006700     03 WS-COUNT-UT              PIC 9(8)   VALUE ZERO.                   
006800                                                                          
006900     03 WS-IDPROJ                PIC X(4).                                
007000     03 WS-IDPROJ-REDEFINE       REDEFINES WS-IDPROJ.                     
007100        05 WS-IDPROJ-2POS        PIC X(2).                                
007200        05 WS-IDPROJ-FILLER      PIC X(2).                                
007300                                                                          
007400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007500                                                                          
007600 01  FELTEXT.                                                             
007700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007800     03  FELTEXT-STATUS          PIC X(2)    VALUE SPACE.                 
007900     03  FILLER                  PIC X       VALUE SPACE.                 
008000     03  FELTEXT-TEXT            PIC X(69)   VALUE SPACE.                 
008100                                                                          
008200 01  DYNAMISKA-SUBPROGRAM.                                                
008300*                                                                         
008400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008800     03  W221SEGM                PIC X(8)    VALUE 'W221SEGM'.            
008900                                                                          
009000     EJECT                                                                
009100 01  IN-AREA-START-W22168    PIC X(24)   VALUE                            
009200                                 'IN-AREA-START-W22168 '.                 
009300                                                                          
009400                                                                          
009500*01  AREA -COPY W22168     -PRE W22168-                                   
009600                                                                          
009700     EJECT                                                                
009800 01  IN-AREA-START-W27178    PIC X(24)   VALUE                            
009900                                 'IN-AREA-START-W27178 '.                 
010000                                                                          
010100                                                                          
010200*01  AREA -COPY W27178     -PRE W27178-                                   
010300                                                                          
010400     EJECT                                                                
010500 01  UT-AREA-START           PIC X(24)   VALUE                            
010600                                 'UT-AREA-START  '.                       
010700                                                                          
010800                                                                          
010900*01  AREA -COPY W27179     -PRE UT-                                       
011000                                                                          
011100*    --- PARAMETRAR TILL W221SEGM                                         
011200*                                                                         
011300*01  -COPY W221SEGM                                                       
011400     EJECT                                                                
011500                                                                          
011600*    --- PARAMETRAR TILL POSTSUM                                          
011700*                                                                         
011800*01  -COPY W0005   -PRE  POSTSUM-                                         
011900     EJECT                                                                
012000                                                                          
012100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012200                                                                          
012300     SKIP3                                                                
012400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012500     SKIP3                                                                
012600 01  NYCKLAR-TILL-DLI.                                                    
012700     03  W-IDDC-B6-X.                                                     
012800         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
012900                                                                          
013000*                                                                         
013100     SKIP2                                                                
013200*    --- STATUS-KOD FRÅN IMS                                              
013300 01  STATUS-WS                   PIC XX.                                  
013400     88  SEGMENT-FINNS                       VALUE '  '.                  
013500     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
013600     SKIP2                                                                
013700 01  GODK-STATUSKODER.                                                    
013800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013900     SKIP3                                                                
014000 01  SSA1                        PIC X(64).                               
014100 01  SSA2                        PIC X(64).                               
014200     EJECT                                                                
014300*    --- IMS FUNKTIONSKODER                                               
014400*01  -COPY W0003                                                          
014500     EJECT                                                                
014600*    ---  DLI INPUT-OUTPUT AREA                                           
014700                                                                          
014800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
014900 01   DLI-IO-AREA-B601.                                                   
015000*     03  -COPY WDB601                                                    
015100     EJECT                                                                
015200                                                                          
015300 LINKAGE SECTION.                                                         
015400                                                                          
015500     EJECT                                                                
015600*01  -COPY W0008      -PRE WDB6-                                          
015700     05  FILLER                  PIC X.                                   
015800                                                                          
015900 01  SEGM-WDB6-PCB                 PIC X.                                 
016000 01  SEGM-WDL7-PCB                 PIC X.                                 
016100 01  SEGM-WDL8-PCB                 PIC X.                                 
016200 01  SEGM-WDD5-PCB                 PIC X.                                 
016210 01  SEGM-WDK7-PCB                 PIC X.                                 
016300     EJECT                                                                
016400 PROCEDURE DIVISION  USING WDB6-PCB                                       
016500                           SEGM-WDB6-PCB SEGM-WDL7-PCB                    
016600                           SEGM-WDL8-PCB SEGM-WDD5-PCB                    
016610                           SEGM-WDK7-PCB.                                 
016700     ENTRY 'DLITCBL' USING WDB6-PCB                                       
016800                           SEGM-WDB6-PCB SEGM-WDL7-PCB                    
016810                           SEGM-WDL8-PCB SEGM-WDD5-PCB                    
016820                           SEGM-WDK7-PCB.                                 
017000                                                                          
017100 MAIN SECTION.                                                            
017200                                                                          
017300     PERFORM A-INIT                                                       
017400                                                                          
017500     PERFORM S01-READ-W22168                                              
017600     PERFORM S02-READ-W27178                                              
017700                                                                          
017800     PERFORM UNTIL END-OF-W27178                                          
017900       IF END-OF-W22168                                                   
018000       OR W27178-IDARTNR < W22168-IDARTNR                                 
018100         DISPLAY 'FEL PÅ REGISTREN : '  W27178-IDARTNR W27178-IDDC        
018200         PERFORM S02-READ-W27178                                          
018300       ELSE                                                               
018400         IF END-OF-W27178                                                 
018500         OR W22168-IDARTNR < W27178-IDARTNR                               
018600           PERFORM S01-READ-W22168                                        
018700         ELSE                                                             
018710           IF W27178-IDDC-REF = SPACE                                     
018800             MOVE W27178-IDARTNR   TO UT-IDARTNR                          
019000             MOVE W27178-IDDC      TO UT-IDDC                             
019100                                      W-IDDC-B6                           
019200             PERFORM IMS-GU-WDB601                                        
019500                                                                          
019700             IF DCS-NDC-CN                                                
019800             OR DCS-USA                                                   
019900               MOVE SPACE      TO UT-IDREFTAB                             
020000               MOVE NEJ        TO SEGM-FLANSKSEG-CHANGED                  
020100                                                                          
020300               PERFORM C-CHECK-IDREFTAB                                   
020400                                                                          
020500               IF SEGM-FLTABLE-CHANGED = JA                               
020510                  MOVE ZERO             TO UT-IDPERSON-BUY                
020600                  MOVE 'Y'              TO UT-FLBUYUPD                    
020700                  MOVE 'N'              TO UT-FLTABUPD                    
020800                  MOVE ZERO             TO UT-KDDCSTYR-BUY                
020900                  PERFORM S03-WRITE-W27179                                
021000               END-IF                                                     
021100             END-IF                                                       
021110           END-IF                                                         
021200                                                                          
021300           PERFORM S02-READ-W27178                                        
021400         END-IF                                                           
021500       END-IF                                                             
021600     END-PERFORM                                                          
021700     PERFORM Z-FINIT                                                      
021800                                                                          
021900     MOVE ZERO TO RETURN-CODE                                             
022000     GOBACK                                                               
022100     .                                                                    
022200                                                                          
022300     EJECT                                                                
022400 A-INIT SECTION.                                                          
022500                                                                          
022600     OPEN INPUT  W22168                                                   
022700                 W27178                                                   
022800     OPEN OUTPUT W27179                                                   
022900                                                                          
023000     ACCEPT DAGENS-DATUM FROM DATE                                        
023100     ACCEPT WS-DAGENS-TID   FROM TIME                                     
023200     DISPLAY 'START TID : ' WS-DAGENS-TID                                 
023300                                                                          
023400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
023500     .                                                                    
023600                                                                          
023700     EJECT                                                                
023800 C-CHECK-IDREFTAB SECTION.                                                
023900                                                                          
024000     MOVE 002                 TO SEGM-KDCALL                              
024100     MOVE W27178-IDARTNR      TO SEGM-IDARTNR                             
024200     MOVE W22168-KDANSKSEG    TO SEGM-KDANSKSEG-IN                        
024300     MOVE W27178-IDDC         TO SEGM-IDDC                                
024400     MOVE W27178-IDREFTAB     TO SEGM-IDREFTAB-IN                         
024500                                                                          
024600     CALL W221SEGM USING SEGM-W221SEGM                                    
024700                         SEGM-WDB6-PCB                                    
024800                         SEGM-WDL7-PCB                                    
024900                         SEGM-WDL8-PCB                                    
025000                         SEGM-WDD5-PCB                                    
025010                         SEGM-WDK7-PCB                                    
025100     IF  SEGM-KDSVAR-OK                                                   
025200         IF SEGM-FLTABLE-CHANGED = JA                                     
025300            MOVE SEGM-IDREFTAB     TO UT-IDREFTAB                         
025400         ELSE                                                             
025500            MOVE SEGM-IDREFTAB-IN  TO UT-IDREFTAB                         
025600         END-IF                                                           
025700     ELSE                                                                 
025800         DISPLAY 'W221SEGM-ERROR1:' SEGM-TEXT                             
025900         CALL FELLOG                                                      
026000     END-IF                                                               
026100     .                                                                    
026200                                                                          
026300     EJECT                                                                
026400 Z-FINIT SECTION.                                                         
026500     ACCEPT WS-DAGENS-TID   FROM TIME                                     
026600     DISPLAY 'SLUT  TID : ' WS-DAGENS-TID                                 
026700     CLOSE W22168                                                         
026800           W27178                                                         
026900           W27179                                                         
027000     DISPLAY 'COUNT W22168    : ' WS-COUNT-W22168                         
027100     DISPLAY 'COUNT W27178    : ' WS-COUNT-W27178                         
027200     DISPLAY 'COUNT UT        : ' WS-COUNT-UT                             
027300     SKIP2                                                                
027400     MOVE 'S' TO POSTSUM-OPKOD                                            
027500     CALL POSTSUM USING POSTSUM-PARM                                      
027600     .                                                                    
027700                                                                          
027800     EJECT                                                                
027900 S01-READ-W22168  SECTION.                                                
028000     READ W22168    INTO W22168-AREA                                      
028100     AT END                                                               
028200        SET END-OF-W22168 TO TRUE                                         
028300     NOT AT END                                                           
028400        ADD 1                TO WS-COUNT-W22168                           
028500        MOVE 'W22168 '       TO POSTSUM-FDNAMN                            
028600        MOVE 'W22470D1'      TO POSTSUM-DDNAMN2                           
028700        MOVE SPACE           TO POSTSUM-TRANSTYP                          
028800        CALL POSTSUM USING POSTSUM-PARM                                   
028900     END-READ                                                             
029000     .                                                                    
029100                                                                          
029200     EJECT                                                                
029300 S02-READ-W27178      SECTION.                                            
029400     READ W27178    INTO W27178-AREA                                      
029500     AT END                                                               
029600        SET END-OF-W27178 TO TRUE                                         
029700     NOT AT END                                                           
029800        ADD 1                TO WS-COUNT-W27178                           
029900        MOVE 'W27178 '       TO POSTSUM-FDNAMN                            
030000        MOVE 'W22470D2'      TO POSTSUM-DDNAMN2                           
030100        MOVE W27178-IDDC     TO POSTSUM-TRANSTYP                          
030200        CALL POSTSUM USING POSTSUM-PARM                                   
030300     END-READ                                                             
030400     .                                                                    
030500                                                                          
030600     EJECT                                                                
030700 S03-WRITE-W27179     SECTION.                                            
030800                                                                          
030900     WRITE UT-POST FROM UT-AREA                                           
031000     ADD 1                   TO WS-COUNT-UT                               
031100                                                                          
031200     MOVE 'W27179 '          TO POSTSUM-FDNAMN                            
031300     MOVE 'W22470D3'         TO POSTSUM-DDNAMN2                           
031400     MOVE UT-IDDC            TO POSTSUM-TRANSTYP                          
031500     CALL POSTSUM USING POSTSUM-PARM                                      
031600     .                                                                    
031700     EJECT                                                                
031800* --- IMS SEKTIONER ---                                                   
031900     SKIP3                                                                
032000     EJECT                                                                
032100                                                                          
032200 IMS-GU-WDB601    SECTION.                                                
032300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
032400          DELIMITED BY SIZE INTO SSA1                                     
032500     MOVE '  ' TO GODK-STATUSKODER                                        
032600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
032700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
032800     PERFORM IMS-STATUSKONTROLL                                           
032900     .                                                                    
033000     EJECT                                                                
033100 IMS-STATUSKONTROLL SECTION.                                              
033200                                                                          
033300     SET STATUS-IX TO 1                                                   
033400     SEARCH GODK-STATUS                                                   
033500       AT END                                                             
033600         STRING 'OTILLÅTEN RETURKOD FRÅN IMS: ' STATUS-WS                 
033700           DELIMITED BY SIZE INTO FELTEXT-TEXT                            
033800         DISPLAY FELTEXT                                                  
033900         CALL FELLOG                                                      
034000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
034100         CONTINUE                                                         
034200     END-SEARCH                                                           
034300     .                                                                    
