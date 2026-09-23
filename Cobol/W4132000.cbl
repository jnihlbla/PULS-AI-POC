000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.             W4132000.                                        
000400 AUTHOR.                 LASSE CALAIS.                                    
000500     DATE-WRITTEN.       FEB. 1995.                                       
000600*    REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION:                                                            
000900*                                                                         
001000*       PGM. LOADS THE PRC-CALENDAR (WL4437) WITH WORKINGTIME.            
001100*       4:TH MONTH FORWARD IN TIME.                                       
001200*       IT ALSO DELETES ALL MONTHS BACK IN TIME, UNTIL LAST               
001300*       MONTH.                                                            
001400*                                                                         
001500*        DELETED         CURRENT                           NEW            
001600*         MONTH           MONTH                           MONTH           
001700*       !-------!-------!-------!-------!-------!-------!-------!         
001800*                                                                         
001900*        WORKING TIME IS COLLECTED FROM WLXXKC.                           
002000*                                                                         
002100*                                                                         
002200*                                                                         
002300*       FILES (DB) USED BY THE PROGRAM: WLXXKC                            
002400*                                       WL4437                            
002500*                                       WLXXKY                            
002600*                                       WLXXKH                            
002610*                                       WDB6                              
002700*                                                                         
002800 ENVIRONMENT DIVISION.                                                    
002900                                                                          
003000                                                                          
003100 DATA DIVISION.                                                           
003200                                                                          
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003401                                                                          
003402*    -COPY WY2000W1                                                       
003403     SKIP3                                                                
003500 77  PROGRAM-NAMN            PIC X(8)    VALUE 'W4132000'.                
003600 77  MSG-IO-AREA-LENGTH      PIC S9(9)   VALUE +32  COMP SYNC.            
003700 77  MSG-IO-AREA             PIC X(32)   VALUE SPACE.                     
003800 77  CHKP-AREA-1-LENGTH      PIC S9(9)   VALUE +32  COMP SYNC.            
003900 77  CHKP-AREA-1             PIC X(32)   VALUE SPACE.                     
004000 77  WS-IDPRC                PIC X(4).                                    
004100 77  JA                      PIC X       VALUE 'J'.                       
004200 77  NEJ                     PIC X       VALUE 'N'.                       
004300 77  KVOT                    PIC S9(2)   VALUE +00.                       
004400 77  REST                    PIC S9(2)   VALUE +00.                       
004500 01  WS-IDDC                 PIC X(2).                                    
004501                                                                          
004510*01  -COPY WWDCKONS                                                       
004520                                                                          
004600*    ---- INDEX                                                           
004700 01  FILLER                  PIC X(16)   VALUE 'INDEX'.                   
004900 77  INDX                    PIC S9(2)   VALUE +00  COMP SYNC.            
005000 77  MONTH-INDX              PIC S9(2)   VALUE +00  COMP SYNC.            
005100 77  MONTH-MAXINDX           PIC S9(2)   VALUE +00  COMP SYNC.            
005200                                                                          
005300*    ----  DATES                                                          
005400                                                                          
005500 01  FILLER                  PIC X(16)   VALUE 'DATES'.                   
005600 01  DELETE-DATE-Y2K         PIC 9(8).                                    
005700 01  FILLER REDEFINES DELETE-DATE-Y2K.                                    
005701   03  DELETE-YEAR-Y2K       PIC 9(4).                                    
005702   03  FILLER                PIC 9(4).                                    
005703 01  FILLER REDEFINES DELETE-DATE-Y2K.                                    
005704   03  DELETE-SEKEL          PIC 9(2).                                    
005710   03  DELETE-DATE           PIC 9(6).                                    
005720   03  FILLER REDEFINES DELETE-DATE.                                      
005810     05  DELETE-YEAR           PIC 9(2).                                  
005900     05  DELETE-MONTH          PIC 9(2).                                  
006000     05  DELETE-DAY            PIC 9(2).                                  
006100                                                                          
006200 01  WS-DELETE-MONTH         PIC S9(2).                                   
006300                                                                          
006400 01  WS-DATE                 PIC 9(6).                                    
006500 01  FILLER REDEFINES WS-DATE.                                            
006600   03  WS-YEAR               PIC 9(2).                                    
006700   03  WS-MONTH              PIC 9(2).                                    
006800   03  WS-DAY                PIC 9(2).                                    
006900                                                                          
007000 01  UPDATE-DATE             PIC 9(6).                                    
007100 01  FILLER REDEFINES UPDATE-DATE.                                        
007200   03  UPDATE-YEAR           PIC 9(2).                                    
007300   03  UPDATE-MONTH          PIC 9(2).                                    
007400   03  UPDATE-DAY            PIC 9(2).                                    
007500                                                                          
007600 01  WS-MONTH-TABLE.                                                      
007700   03  MONTH-TABLE OCCURS 31 TIMES.                                       
007800     05  MONTH-DATE            PIC 9(6).                                  
007900     05  MONTH-KVKALTIM        PIC 9(2).                                  
008000                                                                          
008100 01  FILLER                  PIC X(16)   VALUE 'MONTH'.                   
008200 01  WS-MONTH-VALUE.                                                      
008300   03    FILLER                PIC 9(2)  VALUE 31.                        
008400   03    FILLER                PIC 9(2)  VALUE 28.                        
008500   03    FILLER                PIC 9(2)  VALUE 31.                        
008600   03    FILLER                PIC 9(2)  VALUE 30.                        
008700   03    FILLER                PIC 9(2)  VALUE 31.                        
008800   03    FILLER                PIC 9(2)  VALUE 30.                        
008900   03    FILLER                PIC 9(2)  VALUE 31.                        
009000   03    FILLER                PIC 9(2)  VALUE 31.                        
009100   03    FILLER                PIC 9(2)  VALUE 30.                        
009200   03    FILLER                PIC 9(2)  VALUE 31.                        
009300   03    FILLER                PIC 9(2)  VALUE 30.                        
009400   03    FILLER                PIC 9(2)  VALUE 31.                        
009500                                                                          
009600 01  WS-MONTH-RECORD  REDEFINES WS-MONTH-VALUE.                           
009700   03    WS-MONTH-TAB          PIC 9(2)  OCCURS 12.                       
011200                                                                          
011300 01  FILLER                  PIC X(16)   VALUE 'TIME-TABLE'.              
011400 01  WORK-TIME-TABLE.                                                     
011500     03 FILLER                OCCURS 3.                                   
011600        05 -COPY WDGX4436  -PRE TAB-                                      
011700   EJECT                                                                  
011800*    ---- SUBPGM AND PARAMETER-AREAS                                      
011900 01  FILLER                  PIC X(16)   VALUE 'DYN-SUBPGM'.              
012000 01  DYNAMIC-SUBPGM.                                                      
012100   03  ABEND                 PIC X(8)    VALUE 'ABEND   '.                
012200   03  DATKORT               PIC X(8)    VALUE 'DATKORT '.                
012300   03  WORKDAY               PIC X(8)    VALUE 'WORKDAY '.                
012400   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
012500   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
012600                                                                          
012700*    ---- PARAMETERS TO ABEND                                             
012800*                                                                         
012900 01  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   VALUE +16  COMP SYNC.            
013000                                                                          
013100*    ---- PARAMETERS TO DATUMKORT                                         
013200 01  PARAMETRAR-TO-DATKORT.                                               
013300     03  DATKORT-ID          PIC X(6)    VALUE 'WDATUM'.                  
013400                                                                          
013500*    -COPY WDATKORT                                                       
013600     EJECT                                                                
013700*    -COPY WORKAREA                                                       
013800     EJECT                                                                
013900*    ---- WORK-AREAS FOR IMS-SECTIONS                                     
014000                                                                          
014100 01  FILLER                  PIC X(16) VALUE 'IMS-WS'.                    
014200                                                                          
014300*    ---- PROCEDCODE FROM IMS                                             
014400                                                                          
014500 01  STATUS-WS               PIC XX.                                      
014600     88  SEGMENT-FINNS                    VALUE '  '.                     
014700     88  SEGMENT-MISSING                  VALUE 'GE'.                     
014800     88  SEGMENT-ALREADY-EXISTS           VALUE 'II'.                     
014900     88  END-OF-DATA                      VALUE 'GB'.                     
015000     88  IMS-NOT-OK                       VALUE 'XD'.                     
015100                                                                          
015200 01  OK-STATUSCODES.                                                      
015300   03  OK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                    
015400                                                                          
015500 01  SSA1                    PIC X(96).                                   
015600 01  SSA2                    PIC X(96).                                   
015700                                                                          
015800*    ----  KEYS AND SEARCHFIELDS TO DLI                                   
015900                                                                          
016000 01  FILLER                  PIC X(16)   VALUE 'DLI-KEYS'.                
016100 01  NYCKLAR-TILL-DLI.                                                    
016200*                                                                         
016300   03  W-WDGXKEY-4435-X.                                                  
016400     05  FILLER              PIC X(4)    VALUE '4435'.                    
016500     05  W-4435-IDDC         PIC X(2).                                    
016600     05  FILLER              PIC X(24)   VALUE LOW-VALUE.                 
016700*                                                                         
016800   03  W-WDGXKEY-4436-X.                                                  
016900     05  W-4436-IDPRC        PIC X(4).                                    
017000     05  W-4436-KVKALTIM     PIC 9(2).                                    
017100     05  FILLER              PIC X(4)    VALUE LOW-VALUE.                 
017200*                                                                         
017300   03  W-WDGXKEY-4437-X.                                                  
017400     05  FILLER              PIC X(4)    VALUE '4437'.                    
017500     05  W-4437-IDDC         PIC X(2).                                    
017600     05  W-4437-IDPRC        PIC X(4).                                    
017700     05  FILLER              PIC X(20)   VALUE LOW-VALUE.                 
017800*                                                                         
017900   03  W-WDGXKEY-4475-X.                                                  
018000     05  FILLER              PIC X(4)    VALUE '4475'.                    
018100     05  FILLER              PIC X(26)   VALUE LOW-VALUE.                 
018200*                                                                         
018300   03  W-WDGXKEY-4447-X.                                                  
018400     05  FILLER              PIC X(4)    VALUE '4447'.                    
018500     05  W-4447-IDDC         PIC X(2).                                    
018600     05  FILLER              PIC X(24)   VALUE LOW-VALUE.                 
018700   03  W-WDGXKEY-4448-X.                                                  
018800     05  W-4448-IDPRC        PIC X(4).                                    
018900     05  FILLER              PIC X       VALUE LOW-VALUE.                 
018910                                                                          
018920   03  W-IDDC-B6-X.                                                       
018930       05 W-IDDC-B6                  PIC X(2).                            
018931                                                                          
018932   03  W-IDDC-B6-NEXT-X.                                                  
018933       05 W-IDDC-B6-NEXT             PIC X(2).                            
018940                                                                          
019000     EJECT                                                                
019100*01  -COPY W0003                                                          
019200     EJECT                                                                
019300 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA'.               
019400                                                                          
019500 01  DLI-IO-AREA.                                                         
019600   03  IO-AREA-KH            PIC X(200)   VALUE SPACE.                    
019700                                                                          
019800*03  WLXXKH01 -COPY WDGX4447        -RED IO-AREA-KH.                      
019900     EJECT                                                                
020000*03  WLXXKH11 -COPY WDGX4448        -RED IO-AREA-KH.                      
020100     EJECT                                                                
020200   03  IO-AREA-KC            PIC X(30)    VALUE SPACE.                    
020300                                                                          
020400*03  WLXXKC01 -COPY WDGX4435        -RED IO-AREA-KC.                      
020500     EJECT                                                                
020600*03  WLXXKC11 -COPY WDGX4436        -RED IO-AREA-KC.                      
020700     EJECT                                                                
020800   03  IO-AREA-KD            PIC X(30)    VALUE SPACE.                    
020900                                                                          
021000*03  WL443701 -COPY WDGX4437        -RED IO-AREA-KD.                      
021100     EJECT                                                                
021200*03  WL443711 -COPY WDGX4438        -RED IO-AREA-KD.                      
021300     EJECT                                                                
021400   03  IO-AREA-KY            PIC X(80)    VALUE SPACE.                    
021500                                                                          
021600*03  WLXXKY01 -COPY WDGX01          -RED IO-AREA-KY.                      
021700     EJECT                                                                
021800*03  WLXXKY11 -COPY WDGX4476        -RED IO-AREA-KY.                      
021900     EJECT                                                                
021910                                                                          
021920 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
021930 01   DLI-IO-AREA-B601.                                                   
021940*     03  -COPY WDB601                                                    
021941                                                                          
021942 01  FILLER               PIC X(16)   VALUE 'WDB601 NEXT'.                
021943 01   DLI-IO-AREA-B601-NEXT.                                              
021944*     03  -COPY WDB601   -PRE NEXT-                                       
021950                                                                          
022000 LINKAGE   SECTION.                                                       
022100                                                                          
022200*01  -COPY W0008      -PRE  MSG-.                                         
022300       05  FILLER                PIC X.                                   
022400     EJECT                                                                
022500*01  -COPY W0008      -PRE  XXKC-.                                        
022600       05  FILLER                PIC X.                                   
022700     EJECT                                                                
022800*01  -COPY W0008      -PRE  4437-.                                        
022900       05  FILLER                PIC X.                                   
023000     EJECT                                                                
023100*01  -COPY W0008      -PRE  XXKY-.                                        
023200       05  FILLER                PIC X.                                   
023300     EJECT                                                                
023400*01  -COPY W0008      -PRE  XXKH-.                                        
023500       05  FILLER                PIC X.                                   
023600     EJECT                                                                
023610*01  -COPY W0008      -PRE  WDB6-.                                        
023620       05  FILLER                PIC X.                                   
023630     EJECT                                                                
023640*01  -COPY W0008      -PRE  WDB6-NEXT-.                                   
023650       05  FILLER                PIC X.                                   
023660     EJECT                                                                
023700 PROCEDURE DIVISION  USING  MSG-PCB XXKC-PCB                              
023800                                    4437-PCB                              
023900                                    XXKY-PCB                              
024000                                    XXKH-PCB                              
024010                                    WDB6-PCB                              
024020                                    WDB6-NEXT-PCB.                        
024100     ENTRY 'DLITCBL' USING  MSG-PCB XXKC-PCB                              
024200                                    4437-PCB                              
024300                                    XXKY-PCB                              
024310                                    XXKH-PCB                              
024320                                    WDB6-PCB                              
024330                                    WDB6-NEXT-PCB.                        
024500 STYR SECTION.                                                            
024600                                                                          
024700     PERFORM A-INIT                                                       
024701*DISPLAY VISAR FÖRSTA DATUM I UPPDATERINGEN (AV 30).                      
024710     DISPLAY 'START-UPDATE-DATE=' UPDATE-DATE                             
024800     PERFORM D-COMPUTE-DELETE-MONTH                                       
024801*DISPLAY VISAR DELETE TILL DETTA DATUM (DELETE > DELETE-DATE).            
024810     DISPLAY 'DELETE-DATE =' DELETE-DATE                                  
024900                                                                          
025000     PERFORM IMS-GN-WDB601                                                
025100                                                                          
025200     PERFORM UNTIL END-OF-DATA                                            
025300                                                                          
025310        IF NOT NEXT-DCS-KDDC = SPACE AND                                  
025311           NOT NEXT-DCS-DDC AND NOT NEXT-DCS-CDC-TR                       
025312                                                                          
025320                                                                          
025400           PERFORM B-CREATE-NEW-MONTH-TAB                                 
025600           PERFORM C-CHANGE-4437-CALENDAR                                 
025610                                                                          
025700        END-IF                                                            
025800        PERFORM IMS-GN-WDB601                                             
025810                                                                          
025900     END-PERFORM                                                          
026000                                                                          
026100     PERFORM Z-FINIT                                                      
026200     MOVE ZERO TO RETURN-CODE                                             
026300     GOBACK                                                               
026400     .                                                                    
026500     EJECT                                                                
026600 A-INIT                        SECTION.                                   
026700                                                                          
026800***CALCULATION OF YEAR, MONTH AND DAY FOR UPDATE.                         
026900***FIRST DAY FOR UPDATE IS 01 AND LAST DAY IS MONTH-MAXINDX.              
027000                                                                          
027100     CALL DATKORT USING PROGRAM-NAMN DATKORT-ID DATUMKORT                 
027200     MOVE D-AAR                     TO UPDATE-YEAR                        
027201     MOVE D-MAANAD                  TO UPDATE-MONTH                       
027400     MOVE +01                       TO UPDATE-DAY                         
027500     MOVE UPDATE-DATE               TO DELETE-DATE                        
027510     IF UPDATE-DATE < 500000                                              
027520       MOVE 20                      TO DELETE-SEKEL                       
027530     ELSE                                                                 
027540       MOVE 19                      TO DELETE-SEKEL                       
027550     END-IF                                                               
027560                                                                          
027600     IF UPDATE-MONTH > +8                                                 
027700       COMPUTE UPDATE-YEAR = UPDATE-YEAR + 1                              
027800     END-IF                                                               
027900                                                                          
028000     COMPUTE UPDATE-MONTH = UPDATE-MONTH + 4                              
028100     EVALUATE UPDATE-MONTH                                                
028200       WHEN +13                                                           
028300         MOVE +01                   TO UPDATE-MONTH                       
028400       WHEN +14                                                           
028500         MOVE +02                   TO UPDATE-MONTH                       
028600       WHEN +15                                                           
028700         MOVE +03                   TO UPDATE-MONTH                       
028800       WHEN +16                                                           
028900         MOVE +04                   TO UPDATE-MONTH                       
029000     END-EVALUATE                                                         
029100                                                                          
029200     MOVE UPDATE-MONTH              TO INDX                               
029300     MOVE WS-MONTH-TAB(INDX)        TO MONTH-MAXINDX                      
029400                                                                          
029500***CONTROL IF LEAP-YEAR:                                                  
029600     IF UPDATE-YEAR = 00 AND UPDATE-MONTH = 02                            
029700       MOVE +29                     TO MONTH-MAXINDX                      
029800     ELSE                                                                 
029900       IF UPDATE-MONTH = 02                                               
030000         DIVIDE UPDATE-YEAR BY 4 GIVING KVOT REMAINDER REST               
030100         IF REST = ZERO                                                   
030200           MOVE +29                 TO MONTH-MAXINDX                      
030300         ELSE                                                             
030400           CONTINUE                                                       
030500         END-IF                                                           
030600       END-IF                                                             
030700     END-IF                                                               
030800                                                                          
030900     PERFORM IMS-RESTART                                                  
031000     PERFORM IMS-READ-RESTART                                             
031100                                                                          
031200     IF SEGMENT-FINNS                                                     
031300       IF 4476-IDPRC    > ZERO AND                                        
031400          4476-IDDC     > ZERO                                            
031500         MOVE 4476-IDDC             TO W-4435-IDDC                        
031600                                       W-4437-IDDC                        
031700                                       W-4447-IDDC                        
031800                                       WS-IDDC                            
031900         MOVE 4476-IDPRC            TO W-4436-IDPRC                       
032000                                       W-4437-IDPRC                       
032100                                       W-4448-IDPRC                       
032200                                       WS-IDPRC                           
032300       END-IF                                                             
032400     END-IF                                                               
032500     .                                                                    
032600     EJECT                                                                
032700 B-CREATE-NEW-MONTH-TAB        SECTION.                                   
032800                                                                          
032900***TABLE IS CREATED FOR MONTH THAT IS ADDED TO WL4437,                    
033000***CREATED BY KVKALTIM AND DATE                                           
033100                                                                          
033200     MOVE UPDATE-DATE               TO WORK-TIAAMMDD-FOM                  
033210                                       WORK-TIAAMMDD-TOM                  
033300                                       WS-DATE                            
033400     MOVE +1                        TO MONTH-INDX                         
033500     MOVE NEXT-DCS-IDDC             TO WS-IDDC                            
033600                                                                          
033700     PERFORM UNTIL MONTH-INDX > MONTH-MAXINDX                             
033900         MOVE 001                   TO WORK-KDCALL                        
033901         IF DCS-IDDC NOT = WS-IDDC                                        
033902            MOVE WS-IDDC TO W-IDDC-B6                                     
033903            PERFORM IMS-GU-WDB601                                         
033904         END-IF                                                           
033905         IF DCS-SDC AND DCS-IDLANDX2 = 'SE'                               
033906           MOVE WC-CDC-SE           TO WORK-IDDC                          
033907         ELSE                                                             
033910           MOVE WS-IDDC             TO WORK-IDDC                          
033920         END-IF                                                           
034000         CALL WORKDAY USING WORK-KDCALL                                   
034100                            WORK-DATE-AREA                                
034200                            WORK-KDSVAR                                   
034500                                                                          
034600       IF WORK-KDSVAR-OK                                                  
034610         IF WORK-KVWORKD = 1                                              
034700           MOVE 8                   TO MONTH-KVKALTIM(MONTH-INDX)         
034710         ELSE                                                             
034720           MOVE 0                   TO MONTH-KVKALTIM(MONTH-INDX)         
034730         END-IF                                                           
034800         MOVE WS-DATE               TO MONTH-DATE(MONTH-INDX)             
034900         COMPUTE WS-DAY = WS-DAY + 1                                      
035000         MOVE WS-DATE               TO WORK-TIAAMMDD-FOM                  
035010                                       WORK-TIAAMMDD-TOM                  
035100       ELSE                                                               
035200         DISPLAY 'NOT ACCEPTABLE CODE FROM WORKDAY: ' WORK-KDSVAR         
035300         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
035400       END-IF                                                             
035500       ADD  +1                      TO MONTH-INDX                         
035600     END-PERFORM                                                          
035700     .                                                                    
035800     EJECT                                                                
035900                                                                          
036000 C-CHANGE-4437-CALENDAR        SECTION.                                   
036100                                                                          
036200     MOVE NEXT-DCS-IDDC             TO W-4437-IDDC                        
036300                                       W-4435-IDDC                        
036400                                       W-4447-IDDC                        
036500                                       WS-IDDC                            
036600     PERFORM IMS-GU-XXKH01                                                
036700     PERFORM IMS-GU-XXKC01                                                
036800     IF SEGMENT-FINNS                                                     
036900        PERFORM IMS-GNP-XXKH11-FIRST                                      
037000        PERFORM UNTIL SEGMENT-MISSING                                     
037100           MOVE 4448-IDPRC          TO W-4437-IDPRC                       
037200                                       W-4436-IDPRC                       
037210                                       WS-IDPRC                           
037211*BRA-HA DISPLAY I TEST. *-MÄRK I PROD.                                    
037220*          DISPLAY 'WS-IDPRC & WS-IDDC=' WS-IDPRC '*' WS-IDDC '*'         
037230                                                                          
037300           PERFORM IMS-GU-443701                                          
037400           IF SEGMENT-FINNS                                               
037410*TAG BORT FÖREGÅENDE MÅNADS DAGAR (DAGENS MÅNAD -1).                      
037420                                                                          
037500             PERFORM IMS-GHNP-443711-FIRST                                
037600             IF SEGMENT-FINNS                                             
038013             PERFORM UNTIL SEGMENT-FINNS                                  
038014               AND 4438-DADATUM > DELETE-DATE-Y2K                         
038015*HÅLL PÅ SÅ LÄNGE DAGAR (SEGMENT) FINNS OCH TILLS INLÄST DAG              
038016*(=BÖRJA MED FÖRSTA DAG I FÖREGÅNDE MÅNAD) ÄR STÖRRE ÄN FRAMRÄKNAD        
038017*DAG (=SISTA DAG I FÖREGÅENDE MÅNAD).                                     
038100                  PERFORM IMS-DLET-443711                                 
038200                  PERFORM IMS-GHNP-443711                                 
038300             END-PERFORM                                                  
038310             END-IF                                                       
038400                                                                          
038500             PERFORM CA-CREATE-NEW-MONTH                                  
038600           END-IF                                                         
038700                                                                          
038800           PERFORM IMS-GNP-XXKH11                                         
038900           IF SEGMENT-FINNS                                               
039000              PERFORM CB-CHECKPOINT                                       
039100              PERFORM CC-READ-AFTER-CHECKPOINT                            
039200           END-IF                                                         
039300        END-PERFORM                                                       
039400     END-IF                                                               
039500     .                                                                    
039600     EJECT                                                                
039700 CA-CREATE-NEW-MONTH           SECTION.                                   
039800                                                                          
039900     PERFORM CAA-COMPUTE-WORKTIME                                         
040000     MOVE +1                        TO MONTH-INDX                         
040200                                                                          
040300     PERFORM UNTIL MONTH-INDX > MONTH-MAXINDX                             
040400                                                                          
040410        MOVE MONTH-DATE(MONTH-INDX) TO 4438-DADATUM                       
040420        IF MONTH-DATE(MONTH-INDX) NOT = ZERO                              
040430          IF MONTH-DATE(MONTH-INDX) < 500000                              
040440            MOVE 20                 TO 4438-DADATUM (1:2)                 
040450          ELSE                                                            
040460            IF MONTH-DATE(MONTH-INDX) < 999999                            
040470              MOVE 19               TO 4438-DADATUM (1:2)                 
040480            ELSE                                                          
040490              MOVE 99999999         TO 4438-DADATUM                       
040491            END-IF                                                        
040492          END-IF                                                          
040493        END-IF                                                            
040496                                                                          
040500        EVALUATE MONTH-KVKALTIM(MONTH-INDX)                               
040600        WHEN 0                                                            
040700           PERFORM CAB-COMPUTE-FREE-DAYS                                  
041000        WHEN 8                                                            
041100           PERFORM CAD-COMPUTE-WHOLE-DAYS                                 
041200        END-EVALUATE                                                      
041300        PERFORM IMS-ISRT-443711                                           
041400        ADD +1                      TO MONTH-INDX                         
041700     END-PERFORM                                                          
041800     .                                                                    
041900     EJECT                                                                
042000 CAA-COMPUTE-WORKTIME          SECTION.                                   
042100                                                                          
042200     MOVE 0                         TO W-4436-KVKALTIM                    
042300     PERFORM IMS-GNP-XXKC11-FIRST                                         
042400     IF SEGMENT-FINNS                                                     
042500        MOVE 4436-WDGX4436          TO TAB-4436-WDGX4436(1)               
042600     ELSE                                                                 
042700        MOVE '9999'                 TO W-4436-IDPRC                       
042800        PERFORM IMS-GNP-XXKC11-FIRST                                      
042900        MOVE 4436-WDGX4436          TO TAB-4436-WDGX4436(1)               
043000     END-IF                                                               
043100                                                                          
043200     MOVE 5                         TO W-4436-KVKALTIM                    
043300     PERFORM IMS-GNP-XXKC11                                               
043400     IF SEGMENT-FINNS                                                     
043500        MOVE 4436-WDGX4436          TO TAB-4436-WDGX4436(2)               
043600     ELSE                                                                 
043700        MOVE '9999'                 TO W-4436-IDPRC                       
043800        PERFORM IMS-GNP-XXKC11                                            
043900        MOVE 4436-WDGX4436          TO TAB-4436-WDGX4436(2)               
044000     END-IF                                                               
044100                                                                          
044200     MOVE 8                         TO W-4436-KVKALTIM                    
044300     PERFORM IMS-GNP-XXKC11                                               
044400     IF SEGMENT-FINNS                                                     
044500        MOVE 4436-WDGX4436          TO TAB-4436-WDGX4436(3)               
044600     ELSE                                                                 
044700        MOVE '9999'                 TO W-4436-IDPRC                       
044800        PERFORM IMS-GNP-XXKC11                                            
044900        MOVE 4436-WDGX4436          TO TAB-4436-WDGX4436(3)               
045000     END-IF                                                               
045100                                                                          
045200     .                                                                    
045300     EJECT                                                                
045400 CAB-COMPUTE-FREE-DAYS         SECTION.                                   
045500                                                                          
045600     MOVE TAB-4436-TISTAMIN-PAC(1)  TO 4438-TISTAMIN-PAC                  
045700     MOVE TAB-4436-TISTOMIN-PAC(1)  TO 4438-TISTOMIN-PAC                  
045800     MOVE TAB-4436-TISTAMIN-ADM(1)  TO 4438-TISTAMIN-ADM                  
045900     MOVE TAB-4436-TISTOMIN-ADM(1)  TO 4438-TISTOMIN-ADM                  
046000     MOVE TAB-4436-TISTAMIN-LAST(1) TO 4438-TISTAMIN-LAST                 
046100     MOVE TAB-4436-TISTOMIN-LAST(1) TO 4438-TISTOMIN-LAST                 
046200     .                                                                    
046300     EJECT                                                                
047400 CAD-COMPUTE-WHOLE-DAYS        SECTION.                                   
047500                                                                          
047600     MOVE TAB-4436-TISTAMIN-PAC(3)  TO 4438-TISTAMIN-PAC                  
047700     MOVE TAB-4436-TISTOMIN-PAC(3)  TO 4438-TISTOMIN-PAC                  
047800     MOVE TAB-4436-TISTAMIN-ADM(3)  TO 4438-TISTAMIN-ADM                  
047900     MOVE TAB-4436-TISTOMIN-ADM(3)  TO 4438-TISTOMIN-ADM                  
048000     MOVE TAB-4436-TISTAMIN-LAST(3) TO 4438-TISTAMIN-LAST                 
048100     MOVE TAB-4436-TISTOMIN-LAST(3) TO 4438-TISTOMIN-LAST                 
048200     .                                                                    
048300     EJECT                                                                
048400 CB-CHECKPOINT                 SECTION.                                   
048500                                                                          
048600     PERFORM IMS-READ-RESTART                                             
048700     MOVE NEXT-DCS-IDDC             TO 4476-IDDC                          
048800     MOVE 4448-IDPRC                TO 4476-IDPRC                         
048900     ACCEPT 4476-TIUPPDAT  FROM DATE                                      
049000     ACCEPT 4476-TIUPPTID  FROM TIME                                      
049100     PERFORM IMS-REPL-WLXXKY11                                            
049200     PERFORM IMS-CHECKPOINT                                               
049300     .                                                                    
049400     EJECT                                                                
049500 CC-READ-AFTER-CHECKPOINT      SECTION.                                   
049600                                                                          
049700     PERFORM IMS-READ-RESTART                                             
049800     MOVE 4476-IDDC                 TO W-4435-IDDC                        
049900                                       W-4437-IDDC                        
050000                                       W-4447-IDDC                        
050100                                       WS-IDDC                            
050200     MOVE 4476-IDPRC                TO W-4436-IDPRC                       
050300                                       W-4437-IDPRC                       
050400                                       W-4448-IDPRC                       
050500                                       WS-IDPRC                           
050600     PERFORM IMS-GU-XXKC01                                                
050700     PERFORM IMS-GU-XXKH01                                                
050800     PERFORM IMS-GNP-XXKH11-KVAL                                          
050810                                                                          
050820     MOVE NEXT-DCS-IDDC             TO W-IDDC-B6-NEXT                     
050830     PERFORM IMS-GU-WDB601-NEXT                                           
050900     .                                                                    
051000     EJECT                                                                
051100 D-COMPUTE-DELETE-MONTH        SECTION.                                   
051200                                                                          
051300     COMPUTE WS-DELETE-MONTH = DELETE-MONTH - 2                           
051400                                                                          
051500     EVALUATE WS-DELETE-MONTH                                             
051600       WHEN ZERO                                                          
051700         MOVE    12                 TO DELETE-MONTH                       
051800         COMPUTE DELETE-YEAR-Y2K = DELETE-YEAR-Y2K - 1                    
051810       WHEN -1                                                            
051820         MOVE    11                 TO DELETE-MONTH                       
051830         COMPUTE DELETE-YEAR-Y2K = DELETE-YEAR-Y2K - 1                    
051900       WHEN OTHER                                                         
052000         MOVE WS-DELETE-MONTH       TO DELETE-MONTH                       
052100     END-EVALUATE                                                         
052110     MOVE WS-MONTH-TAB (DELETE-MONTH)  TO DELETE-DAY                      
052200     .                                                                    
052300     EJECT                                                                
052400 Z-FINIT                       SECTION.                                   
052500                                                                          
052600     PERFORM IMS-READ-RESTART                                             
052700     MOVE ZERO                      TO 4476-IDDC                          
052800                                       4476-IDPRC                         
052900     ACCEPT 4476-TIUPPDAT  FROM DATE                                      
053000     ACCEPT 4476-TIUPPTID  FROM TIME                                      
053100     PERFORM IMS-REPL-WLXXKY11                                            
053200     PERFORM IMS-CHECKPOINT                                               
053300     .                                                                    
053400     EJECT                                                                
053500*    ---- IMS SEKTIONER ----                                              
053600                                                                          
053700 IMS-GU-XXKH01                 SECTION.                                   
053800                                                                          
053900     STRING 'WLXXKH01(WDGXKEY  =' W-WDGXKEY-4447-X ')'                    
054000            DELIMITED BY SIZE INTO SSA1                                   
054100     MOVE '  GE'                TO OK-STATUSCODES                         
054200     CALL CBLTDLI USING GU  XXKH-PCB IO-AREA-KH SSA1                      
054300     MOVE XXKH-STATUS-CODE      TO STATUS-WS                              
054400     PERFORM IMS-STATUSKONTROLL                                           
054500     .                                                                    
054600                                                                          
054700 IMS-GNP-XXKH11-KVAL           SECTION.                                   
054800                                                                          
054900     STRING 'WLXXKH11(WDGXKEY  =' W-WDGXKEY-4448-X ')'                    
055000            DELIMITED BY SIZE INTO SSA1                                   
055100     MOVE '  GE'                TO OK-STATUSCODES                         
055200     CALL CBLTDLI USING GNP XXKH-PCB IO-AREA-KH SSA1                      
055300     MOVE XXKH-STATUS-CODE      TO STATUS-WS                              
055400     PERFORM IMS-STATUSKONTROLL                                           
055500     .                                                                    
055600                                                                          
055700 IMS-GNP-XXKH11-FIRST          SECTION.                                   
055800                                                                          
055900     MOVE 'WLXXKH11*F'           TO SSA1                                  
056000     MOVE '  GE'                TO OK-STATUSCODES                         
056100     CALL CBLTDLI USING GNP XXKH-PCB IO-AREA-KH SSA1                      
056200     MOVE XXKH-STATUS-CODE      TO STATUS-WS                              
056300     PERFORM IMS-STATUSKONTROLL                                           
056400     .                                                                    
056500                                                                          
056600 IMS-GNP-XXKH11                SECTION.                                   
056700                                                                          
056800     MOVE 'WLXXKH11 '            TO SSA1                                  
056900     MOVE '  GE'                TO OK-STATUSCODES                         
057000     CALL CBLTDLI USING GNP XXKH-PCB IO-AREA-KH SSA1                      
057100     MOVE XXKH-STATUS-CODE      TO STATUS-WS                              
057200     PERFORM IMS-STATUSKONTROLL                                           
057300     .                                                                    
057400     EJECT                                                                
057500 IMS-GU-XXKC01                 SECTION.                                   
057600                                                                          
057700     STRING 'WLXXKC01(WDGXKEY  =' W-WDGXKEY-4435-X ')'                    
057800            DELIMITED BY SIZE INTO SSA1                                   
057900     MOVE '  GBGE'              TO OK-STATUSCODES                         
058000     CALL CBLTDLI USING GU  XXKC-PCB IO-AREA-KC SSA1                      
058100     MOVE XXKC-STATUS-CODE      TO STATUS-WS                              
058200     PERFORM IMS-STATUSKONTROLL                                           
058300     .                                                                    
058400                                                                          
058500 IMS-GNP-XXKC11-FIRST          SECTION.                                   
058600                                                                          
058700     STRING 'WLXXKC11*F(WDGXKEY  =' W-WDGXKEY-4436-X ')'                  
058800            DELIMITED BY SIZE INTO SSA1                                   
058900     MOVE '  GBGE'              TO OK-STATUSCODES                         
059000     CALL CBLTDLI USING GNP XXKC-PCB IO-AREA-KC SSA1                      
059100     MOVE XXKC-STATUS-CODE      TO STATUS-WS                              
059200     PERFORM IMS-STATUSKONTROLL                                           
059300     .                                                                    
059400                                                                          
059500 IMS-GNP-XXKC11                SECTION.                                   
059600                                                                          
059700     STRING 'WLXXKC11(WDGXKEY  =' W-WDGXKEY-4436-X ')'                    
059800            DELIMITED BY SIZE INTO SSA1                                   
059900     MOVE '  GBGE'              TO OK-STATUSCODES                         
060000     CALL CBLTDLI USING GNP XXKC-PCB IO-AREA-KC SSA1                      
060100     MOVE XXKC-STATUS-CODE      TO STATUS-WS                              
060200     PERFORM IMS-STATUSKONTROLL                                           
060300     .                                                                    
060400     EJECT                                                                
060500 IMS-GU-443701                 SECTION.                                   
060600                                                                          
060700     STRING 'WL443701*F(WDGXKEY  =' W-WDGXKEY-4437-X ')'                  
060800            DELIMITED BY SIZE INTO SSA1                                   
060900     MOVE '  GE'                TO OK-STATUSCODES                         
061000     CALL CBLTDLI USING GU  4437-PCB IO-AREA-KD SSA1                      
061100     MOVE 4437-STATUS-CODE      TO STATUS-WS                              
061200     PERFORM IMS-STATUSKONTROLL                                           
061300     .                                                                    
061400                                                                          
061500 IMS-GHNP-443711               SECTION.                                   
061600                                                                          
061700     MOVE 'WL443711 '           TO SSA1                                   
061800     MOVE '  GE'                TO OK-STATUSCODES                         
061900     CALL CBLTDLI USING GHNP 4437-PCB IO-AREA-KD SSA1                     
062000     MOVE 4437-STATUS-CODE      TO STATUS-WS                              
062100     PERFORM IMS-STATUSKONTROLL                                           
062200     .                                                                    
062300                                                                          
062400 IMS-GHNP-443711-FIRST         SECTION.                                   
062500                                                                          
062600     MOVE 'WL443711*F'          TO SSA1                                   
062700     MOVE '  GE'                TO OK-STATUSCODES                         
062800     CALL CBLTDLI USING GHNP 4437-PCB IO-AREA-KD SSA1                     
062900     MOVE 4437-STATUS-CODE      TO STATUS-WS                              
063000     PERFORM IMS-STATUSKONTROLL                                           
063100     .                                                                    
063200                                                                          
063300 IMS-ISRT-443711               SECTION.                                   
063400                                                                          
063500     STRING 'WL443701(WDGXKEY  =' W-WDGXKEY-4437-X ')'                    
063600            DELIMITED BY SIZE INTO SSA1                                   
063700     MOVE 'WL443711'            TO SSA2                                   
063800     MOVE '  II'                TO OK-STATUSCODES                         
063900     CALL CBLTDLI USING ISRT  4437-PCB IO-AREA-KD SSA1 SSA2               
064000     MOVE 4437-STATUS-CODE      TO STATUS-WS                              
064100     PERFORM IMS-STATUSKONTROLL                                           
064200     .                                                                    
064300                                                                          
064400 IMS-DLET-443711             SECTION.                                     
064500                                                                          
064600     MOVE '  '               TO OK-STATUSCODES                            
064700     CALL CBLTDLI USING DLET  4437-PCB IO-AREA-KD                         
064800     MOVE 4437-STATUS-CODE   TO STATUS-WS                                 
064900     PERFORM IMS-STATUSKONTROLL                                           
065000     .                                                                    
065100     EJECT                                                                
065200 IMS-REPL-WLXXKY11             SECTION.                                   
065300                                                                          
065400     MOVE '  '                 TO OK-STATUSCODES                          
065500     CALL CBLTDLI USING REPL  XXKY-PCB IO-AREA-KY                         
065600     MOVE XXKY-STATUS-CODE     TO STATUS-WS                               
065700     PERFORM IMS-STATUSKONTROLL                                           
065800     .                                                                    
065900     EJECT                                                                
066000 IMS-RESTART                   SECTION.                                   
066100                                                                          
066200     MOVE SPACE                 TO MSG-IO-AREA                            
066300     MOVE '  '                  TO OK-STATUSCODES                         
066400     CALL CBLTDLI USING XRST MSG-PCB                                      
066500                             MSG-IO-AREA-LENGTH MSG-IO-AREA               
066600                             CHKP-AREA-1-LENGTH CHKP-AREA-1               
066700     MOVE MSG-STATUS-CODE      TO STATUS-WS                               
066800     PERFORM IMS-STATUSKONTROLL                                           
066900     IF IMS-NOT-OK                                                        
067000       DISPLAY 'IMS-KONTROLLREGION-EJ-TILLGÄNGLIG-VID-RESTART'            
067100       CALL FELLOG                                                        
067200     END-IF                                                               
067300     .                                                                    
067400                                                                          
067500 IMS-CHECKPOINT                SECTION.                                   
067600                                                                          
067700     MOVE PROGRAM-NAMN          TO MSG-IO-AREA                            
067800     MOVE '  XD'                TO OK-STATUSCODES                         
067900     CALL CBLTDLI USING CHKP MSG-PCB                                      
068000                             MSG-IO-AREA-LENGTH MSG-IO-AREA               
068100                             CHKP-AREA-1-LENGTH CHKP-AREA-1               
068200     MOVE MSG-STATUS-CODE      TO STATUS-WS                               
068300     PERFORM IMS-STATUSKONTROLL                                           
068400     IF IMS-NOT-OK                                                        
068500       DISPLAY 'IMS-KONTROLLREGION-EJ-TILLGÄNGLIG-VID-CHECKPOINT'         
068600       CALL FELLOG                                                        
068700     END-IF                                                               
068800     .                                                                    
068900                                                                          
069000 IMS-READ-RESTART              SECTION.                                   
069100                                                                          
069200     STRING 'WLXXKY01(WDGXKEY  =' W-WDGXKEY-4475-X ')'                    
069300            DELIMITED BY SIZE INTO SSA1                                   
069400     MOVE 'WLXXKY11(KDSEGKEY =1)' TO SSA2                                 
069500     MOVE '  GE'                TO OK-STATUSCODES                         
069600     CALL CBLTDLI USING GHU  XXKY-PCB IO-AREA-KY SSA1 SSA2                
069700     MOVE XXKY-STATUS-CODE      TO STATUS-WS                              
069800     PERFORM IMS-STATUSKONTROLL                                           
069900     .                                                                    
070000     EJECT                                                                
070010 IMS-GU-WDB601    SECTION.                                                
070020     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
070030          DELIMITED BY SIZE INTO SSA1                                     
070040     MOVE '  GE' TO OK-STATUSCODES                                        
070050     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
070060     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
070070     PERFORM IMS-STATUSKONTROLL                                           
070080     IF SEGMENT-MISSING                                                   
070090         MOVE SPACE TO DCS-KDDC                                           
070091     END-IF                                                               
070092     .                                                                    
070093     EJECT                                                                
070094 IMS-GU-WDB601-NEXT SECTION.                                              
070095     STRING 'WDB601  (IDDC     =' W-IDDC-B6-NEXT-X ')'                    
070096          DELIMITED BY SIZE INTO SSA1                                     
070097     MOVE '  ' TO OK-STATUSCODES                                          
070098     CALL CBLTDLI USING GU WDB6-NEXT-PCB DLI-IO-AREA-B601-NEXT            
070099                           SSA1                                           
070100     MOVE WDB6-NEXT-STATUS-CODE    TO STATUS-WS                           
070101     PERFORM IMS-STATUSKONTROLL                                           
070104     .                                                                    
070105     EJECT                                                                
070106 IMS-GN-WDB601    SECTION.                                                
070107     MOVE 'WDB601  '  TO SSA1                                             
070108     MOVE '  GB' TO OK-STATUSCODES                                        
070109     CALL CBLTDLI USING GN WDB6-NEXT-PCB DLI-IO-AREA-B601-NEXT            
070110                           SSA1                                           
070111     MOVE WDB6-NEXT-STATUS-CODE    TO STATUS-WS                           
070112     PERFORM IMS-STATUSKONTROLL                                           
070113     .                                                                    
070114     EJECT                                                                
070120 IMS-STATUSKONTROLL            SECTION.                                   
070200                                                                          
070300     SET STATUS-IX TO 1                                                   
070400     SEARCH OK-STATUS                                                     
070500       AT END CALL FELLOG                                                 
070600       WHEN OK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
070700     END-SEARCH                                                           
070800     .                                                                    
070810     EJECT                                                                
