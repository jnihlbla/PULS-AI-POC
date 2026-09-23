000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2044700.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   12/10/04.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        SCREEN SHOWING PARTS THAT HAVE A NEW PROPOSAL FOR A              
000900*        DELIVERY SCHEDULE FOR THE CHINA WAREHOUSES. IF DC ID IS          
001000*        BLANK IN THE KEY FIELD, PROPOSALS FOR ALL THE 3                  
001100*        WAREHOUSES (71, 72 AND/OR 73) ARE SHOWN.                         
001200*                                                                         
001300*        THE PROGRAM READS     WDD6                                       
001400*        THE PROGRAM READS     WDB6                                       
001500*        THE PROGRAM READS     WDK6                                       
001600*           (eg. bara för att undvika problem i test)                     
001700*        THE PROGRAM READS     WDK7                                       
001800*           (eg. bara för att undvika problem i test)                     
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSACTION: W2T447                                              
002200*        MID:         W2I44701                                            
002300*                                                                         
002400*    OUTDATA.                                                             
002500*        MOD:         W2O447N1                                            
002600                                                                          
002700                                                                          
002800 ENVIRONMENT DIVISION.                                                    
002900                                                                          
003000 DATA DIVISION.                                                           
003100 WORKING-STORAGE SECTION.                                                 
003200 77  IDPGM                       PIC X(08)   VALUE 'W2044700'.            
003300                                                                          
003400*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003500 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003600                                                                          
003700 77  YES                         PIC X       VALUE 'J'.                   
003800 77  NOO                         PIC X       VALUE 'N'.                   
003900                                                                          
004000 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
004100 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
004200                                                                          
004300*    --- INDEX FOR SCROLL LINES                                           
004400 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004500 77  MAX-INDX                    PIC S9(4)  VALUE +15   COMP SYNC.        
004600*    --- INDEX FÖR SPARADE SID-NYCKLAR (För bläddring PREVIOUS)           
004700 01  SIDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004800 01  MAX-SIDX                    PIC S9(4)  VALUE +10   COMP SYNC.        
004900*    --- INDEX FÖR KDLPORS på WDD6                                        
005000 01  OIX                         PIC s9  VALUE Zero COMP-3.               
005100*    --- INDEX FÖR Orsaks-TEXT till skärm                                 
005200 01  TIX                         PIC s9  VALUE Zero COMP-3.               
005300*    --- Giltigt KDLPORS-nummer i W221W006                                
005400 01  KDLPORS-NUM                 PIC s999 VALUE Zero COMP-3.              
005500*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
005600                                                                          
005700 01 CURR-KEY-WDD601KY.                                                    
005800    03 CURR-IDDC                 PIC X(2).                                
005900    03 CURR-IDLEVNR              PIC X(5).                                
006000    03 CURR-IDARTNR              PIC S9(9)      COMP-3.                   
006100    03 CURR-IDANSK               PIC S9(3)      COMP-3.                   
006200                                                                          
006300 77  KEYS-SW                     PIC X       VALUE 'J'.                   
006400     88  KEYS-OK                             VALUE 'J'.                   
006500     88  KEYS-WRONG                          VALUE 'N'.                   
006600                                                                          
006700 77  DC-SW                       PIC X       VALUE 'J'.                   
006800     88  DC-OK                               VALUE 'J'.                   
006900     88  DC-WRONG                            VALUE 'N'.                   
007000                                                                          
007100 77  KDCMDVAL-SW                 PIC X       VALUE 'J'.                   
007200     88  KDCMDVAL-OK                         VALUE 'J'.                   
007300     88  NO-KDCMDVAL                         VALUE 'N'.                   
007400                                                                          
007500 77  GODK-POST-SW                PIC X       VALUE 'J'.                   
007600     88  GODK-POST                           VALUE 'J'.                   
007700     88  EJ-GODK-POST                        VALUE 'N'.                   
007800                                                                          
007900 77  SET-KVRADER-SW              PIC X       VALUE 'J'.                   
008000     88  SET-KVRADER                         VALUE 'J'.                   
008100     88  SET-EJ-KVRADER                      VALUE 'N'.                   
008200                                                                          
008300                                                                          
008400 01  W-IDANSK-HELP               PIC 9(3)    VALUE ZERO.                  
008500 01  FILLER REDEFINES W-IDANSK-HELP.                                      
008600     03  FILLER                  PIC 9(2).                                
008700     03  W-IDANSK-HELP-3         PIC 9(1).                                
008800                                                                          
008900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
009000     88  OWN-MID                             VALUE '2447'.                
009100     88  GOOD-MID                            VALUE '2441' '2442'          
009200                                                   '2443' '2444'          
009300                                                   '2445' '2446'          
009400                                                   '2447' '2448'          
009500                                                   '2449'.                
009600     88  HELP-MID                            VALUE '0551'.                
009700                                                                          
009800 01  FILLER                      PIC X(7)    VALUE 'WWIDFTG'.             
009900*01 -COPY WWIDFTG                                                         
010000     EJECT                                                                
010100*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
010200 01  GENERAL-SUBPROGRAMS.                                                 
010300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010800                                                                          
010900*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
011000*01 -COPY WMEDAREA                                                        
011100                                                                          
011200 01  MESSAGE-CODES.                                                       
011300     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
011400     03  INF-NO-LINES-EXISTS     PIC X(3)    VALUE '056'.                 
011500     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
011600     03  INF-LAST-PAGE-SHOWN     PIC X(3)    VALUE '106'.                 
011700     03  INF-PRESS-F9-TO-JUMP    PIC X(3)    VALUE '127'.                 
011800                                                                          
011900     03  ERR-WRONG-CODE          PIC X(3)    VALUE '013'.                 
012000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
012100     03  ERR-WRONG-DC            PIC X(3)    VALUE '440'.                 
012200                                                                          
012300 01  FILLER                      PIC X(8) VALUE 'WS-AREA'.                
012400 01  WS-AREA.                                                             
012500     03 W-DC-OK                  PIC X(1).                                
012600     03  WS-IDANSK-TO-UT         PIC 9(3).                                
012700     03  FILLER REDEFINES WS-IDANSK-TO-UT.                                
012800         05 WS-IDANSK-TO-UT-2    PIC 9(2).                                
012900         05 WS-IDANSK-TO-UT-3    PIC 9.                                   
013000*01  -COPY WDATAREA                                                       
013100                                                                          
013200*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
013300*                                                                         
013400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
013500                                                                          
013600*01 -COPY WMSGINIT                                                        
013700                                                                          
013800*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
013900*                                                                         
014000 01  SAVE-AREA.                                                           
014100     03 SAVE-IDTRANS              PIC XXXX    VALUE SPACE.                
014200     03 SAVE-PAGENO               PIC S9(3)   COMP-3.                     
014300     03 SAVE-KVRADER              PIC 9(4).                               
014400     03 W-IDANSK-TAB.                                                     
014500         05 W-IDANSK-RAD         PIC 9(3)    OCCURS 15.                   
014600                                                                          
014700     03 SEEK-ARGUMENT.                                                    
014800         05 SEEK-IDANSK-FOM       PIC S9(9)      COMP-3.                  
014900         05 SEEK-IDANSK-TOM       PIC S9(3)      COMP-3.                  
015000         05 SEEK-IDLEVNR          PIC X(5).                               
015100         05 SEEK-KDLPORS          PIC 9(2).                               
015200         05 SEEK-KDLEVPLF         PIC X(1).                               
015300         05 SEEK-IDDC             PIC X(2).                               
015400                                                                          
015500*    -- WDD6-NYCKLAR TILL SAMMA SIDA                                      
015600     03 SAMMA-KEY-WDD601KY.                                               
015700        05 SAMMA-IDDC             PIC X(2).                               
015800        05 SAMMA-IDLEVNR          PIC X(5).                               
015900        05 SAMMA-IDARTNR          PIC S9(9)      COMP-3.                  
016000        05 SAMMA-IDANSK           PIC S9(3)      COMP-3.                  
016100                                                                          
016200*    -- WDD6-NYCKLAR TILL NÄSTA SIDA                                      
016300     03 NEXT-KEY-WDD601KY.                                                
016400        05 NEXT-IDDC              PIC X(2).                               
016500        05 NEXT-IDLEVNR           PIC X(5).                               
016600        05 NEXT-IDARTNR           PIC S9(9)      COMP-3.                  
016700        05 NEXT-IDANSK            PIC S9(3)      COMP-3.                  
016800                                                                          
016900 01  SAVE-AREA-2403.                                                      
017000     03 SAVE-IDTRANS-2403         PIC X(4)    VALUE SPACE.                
017100     03 SAVE-IDDC-2403            PIC X(2)    VALUE SPACE.                
017200     03 SAVE-IDLEVNR-2403         PIC X(5)    VALUE SPACE.                
017300     03 SAVE-IDDC-D6-2403         PIC X(2)    VALUE SPACE.                
017400     03 SAVE-IDLEVNR-D6-2403      PIC X(5)    VALUE SPACE.                
017500     03 SAVE-IDARTNR-D6-2403      PIC S9(9)   COMP-3 VALUE ZERO.          
017600     03 SAVE-IDANSK-D6-2403       PIC S9(3)   COMP-3 VALUE ZERO.          
017700                                                                          
017800 01  SAVE-FROM-2403.                                                      
017900     03  SAVE-IDTRANS-FROM-2403   PIC X(4)    VALUE SPACE.                
018000     03  SAVE-KDLEVPLF-FROM-2403  PIC X(1)    VALUE SPACE.                
018100     03  SAVE-FLJIT-FROM-2403     PIC X(1)    VALUE SPACE.                
018200     03  SAVE-KDLPSP-FROM-2403    PIC 9(1)    VALUE ZERO.                 
018300     03  SAVE-IDDC-FROM-2403      PIC X(2)    VALUE SPACE.                
018400     03  SAVE-IDLEVNR-FROM-2403   PIC X(5)    VALUE SPACE.                
018500     03  SAVE-2403-DIALOG         PIC X(1)    VALUE SPACE.                
018600                                                                          
018700*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
018800*                                                                         
018900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
019000                                                                          
019100*01  MID -COPY W2I44701                                                   
019200                                                                          
019300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
019400                                                                          
019500*01  -COPY WMSGAREA                                                       
019600                                                                          
019700     03  MOD REDEFINES MSG-AREA.                                          
019800*      05  -COPY W2O44701                                                 
019900                                                                          
020000 01  FILLER                      PIC X(16)  VALUE 'KDLPORS-AREA'.         
020100*01   -COPY W221W006.                                                     
020200                                                                          
020300 01  FILLER          PIC X(16) VALUE 'PROG-TO-PROG-SW'.                   
020400*    -- P-WS-LL  sätts i A-init.                                          
020500 01  W-PROG-TO-PROG-SW.                                                   
020600     03  P-WS-LL     PIC S9(4)  COMP SYNC.                                
020700     03  P-WS-Z1-Z2  PIC X(2)   VALUE LOW-VALUE.                          
020800     03  KDTRANS-WS  PIC X(8)   VALUE 'W2T403  '.                         
020900     03  P-IDTRANS   PIC X(4)   VALUE '244G'.                             
021000     03  P-KDMFSFOR  PIC X(1)   VALUE '1'.                                
021100                                                                          
021200*    03  MID -COPY W2I40301 -PRE ALT-.                                    
021300                                                                          
021400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
021500                                                                          
021600*01  -COPY WMFSAREA                                                       
021700                                                                          
021800*    --- WORK-AREAS FOR IMS-SECTIONS                                      
021900*                                                                         
022000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
022100                                                                          
022200 01  KEYS-FOR-DLI.                                                        
022300*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
022400     03  W-WDD601KY-MIN-X.                                                
022500       05 W-IDDC-MIN.                                                     
022600         07  FILLER              PIC X(1)  VALUE SPACE.                   
022700         07  W-IDDC2-MIN         PIC X(1)  VALUE SPACE.                   
022800       05 FILLER                 PIC X(12) VALUE LOW-VALUE.               
022900     03  W-WDD601KY-MAX-X.                                                
023000       05 W-IDDC-MAX.                                                     
023100         07  FILLER              PIC X(1)  VALUE SPACE.                   
023200         07  W-IDDC2-MAX         PIC X(1)  VALUE SPACE.                   
023300       05 FILLER                 PIC X(12) VALUE HIGH-VALUE.              
023400                                                                          
023500     03  W-IDDC                  PIC X(2)  VALUE SPACE.                   
023600                                                                          
023700     03  W-IDDC-B6               PIC X(2)  VALUE SPACE.                   
023800                                                                          
023900     03  W-IDDC-B6-MIN-X.                                                 
024000         05  FILLER              PIC X(1)  VALUE SPACE.                   
024100         05  W-IDDC2-B6-MIN      PIC X(1)  VALUE SPACE.                   
024200     03  W-IDDC-B6-MAX-X.                                                 
024300         05  FILLER              PIC X(1)  VALUE SPACE.                   
024400         05  W-IDDC2-B6-MAX      PIC X(1)  VALUE SPACE.                   
024500                                                                          
024600     03  W-IDARTNR-X.                                                     
024700         05  W-IDARTNR           PIC S9(9) COMP-3 VALUE ZERO.             
024800                                                                          
024900*    --- STATUS CODES FROM IMS                                            
025000 01  STATUS-WS                   PIC XX.                                  
025100     88  SEGMENT-FOUND                       VALUE '  '.                  
025200     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
025300     88  SEGMENT-MISSING                     VALUE 'GE'.                  
025400     88  SEGMENT-END                         VALUE 'GB'.                  
025500                                                                          
025600 01  GOOD-STATUSCODES.                                                    
025700     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025800                                                                          
025900 01  ALL-SSA.                                                             
026000     03 SSA1                     PIC X(255).                              
026100     03 SSA2                     PIC X(255).                              
026200                                                                          
026300*    --- IMS FUNCTION CODES                                               
026400*01  -COPY W0003                                                          
026500                                                                          
026600*    ---  DLI INPUT-OUTPUT AREA                                           
026700                                                                          
026800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD601'.                      
026900 01  DLI-IO-WDD601.                                                       
027000*    03  -COPY WDD601                                                     
027100                                                                          
027200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
027300 01  DLI-IO-WDB601.                                                       
027400*    03  -COPY WDB601                                                     
027500                                                                          
027600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
027700 01  DLI-IO-WDK601.                                                       
027800*    03  -COPY WDK601                                                     
027900                                                                          
028000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
028100 01  DLI-IO-WDK711.                                                       
028200*    03  -COPY WDK711                                                     
028300                                                                          
028400 LINKAGE SECTION.                                                         
028500*01  -COPY W0009   -PRE MSG-                                              
028600*                                                                         
028700*01  -COPY W0008   -PRE ALT-                                              
028800     05  FILLER                  PIC X.                                   
028900*01  -COPY W0008   -PRE WDP7-                                             
029000     05  FILLER                  PIC X.                                   
029100                                                                          
029200*01  -COPY W0008  -PRE WDD6-                                              
029300     05  FILLER                  PIC X.                                   
029400*01  -COPY W0008  -PRE WDB6-                                              
029500     05  FILLER                  PIC X.                                   
029600*01  -COPY W0008  -PRE WDB6A-                                             
029700     05  FILLER                  PIC X.                                   
029800*01  -COPY W0008  -PRE WDK6-                                              
029900     05  FILLER                  PIC X.                                   
030000*01  -COPY W0008  -PRE WDK7-                                              
030100     05  FILLER                  PIC X.                                   
030200                                                                          
030300 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB WDP7-PCB WDD6-PCB              
030400                           WDB6-PCB WDB6A-PCB WDK6-PCB WDK7-PCB.          
030500 MAIN SECTION.                                                            
030600     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDP7-PCB WDD6-PCB              
030700                           WDB6-PCB WDB6A-PCB WDK6-PCB WDK7-PCB.          
030800                                                                          
030900     PERFORM IMS-GET-MSG                                                  
031000     IF SEGMENT-FOUND                                                     
031100       PERFORM A-INIT                                                     
031200       PERFORM B-CHECK-KEYS                                               
031300       IF KEYS-OK AND DC-OK                                               
031400                                                                          
031500         EVALUATE TRUE                                                    
031600             WHEN MFS-FIRST                                               
031700               PERFORM C-FIRST-PAGE                                       
031800             WHEN MFS-NEXT                                                
031900               PERFORM D-NEXT-PAGE                                        
032000             WHEN MFS-ENTER                                               
032100               PERFORM E-SAME-PAGE                                        
032200*            WHEN MFS-PREVIOUS                                            
032300*              PERFORM I-PREVIOUS-PAGE                                    
032400             WHEN MFS-SPLIT                                               
032500               PERFORM J-SWITCH-TO-2403                                   
032600             WHEN OTHER                                                   
032700               CONTINUE                                                   
032800         END-EVALUATE                                                     
032900                                                                          
033000         IF MFS-SPLIT                                                     
033100            IF KDCMDVAL-OK                                                
033200               MOVE '002'          TO MSGI-KDCALL                         
033300               MOVE SAVE-AREA-2403 TO MSGI-SPAR-AREA                      
033400                                                                          
033500               CALL W005INIT  USING MSGI-WMSGINIT WDP7-PCB                
033600               COMPUTE P-WS-LL = LENGTH OF ALT-MID + 17                   
033700               PERFORM IMS-INSERT-ALT-MSG                                 
033800            END-IF                                                        
033900          ELSE                                                            
034000             PERFORM F-READ-SHOW-INFO                                     
034100          END-IF                                                          
034200       END-IF                                                             
034300                                                                          
034400       IF NOT MFS-SPLIT                                                   
034500       OR ( MFS-SPLIT AND NO-KDCMDVAL )                                   
034600          COMPUTE MSG-KVLL = LENGTH OF MOD-W2O44701 + 4                   
034700          PERFORM IMS-INSERT-MSG                                          
034800       END-IF                                                             
034900     END-IF                                                               
035000                                                                          
035100     MOVE ZERO TO RETURN-CODE                                             
035200     GOBACK                                                               
035300     .                                                                    
035400                                                                          
035500                                                                          
035600 A-INIT SECTION.                                                          
035700     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
035800                                                                          
035900     IF MSG-DOUBLE-TRANSACTIONS                                           
036000       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W2I44701                 
036100       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
036200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
036300     ELSE                                                                 
036400       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W2I44701                  
036500       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
036600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
036700     END-IF                                                               
036800                                                                          
036900     MOVE MSG-KDTRTYP      TO MFS-KDTRTYP                                 
037000     MOVE MSG-IDPFK        TO MFS-IDPFK                                   
037100     MOVE MFS-IDTRANS      TO W-IDTRANS                                   
037200                                                                          
037300     MOVE LOW-VALUE        TO MSG-AREA                                    
037400     MOVE 'W2O447N1'       TO MFS-IDMOD                                   
037500     MOVE '2447' TO MOD-IDTRANS                                           
037600     MOVE MFS-ERASE-FIELD  TO MOD-TEMFSFEL MOD-TEMFSINF                   
037700                                                                          
037800     IF OWN-MID OR HELP-MID                                               
037900       CONTINUE                                                           
038000     ELSE                                                                 
038100       MOVE SPACE TO MFS-KDTRTYP                                          
038200       MOVE '7'   TO MFS-IDPFK                                            
038300     END-IF                                                               
038400     .                                                                    
038500                                                                          
038600                                                                          
038700 B-CHECK-KEYS SECTION.                                                    
038800     MOVE 'B-CHECK-KEYS    ' TO CURRENT-SECTION                           
038900                                                                          
039000     MOVE ALL '+'           TO MSGI-WMSGINIT                              
039100     MOVE '001'             TO MSGI-KDCALL                                
039200     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
039300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
039400     MOVE '2447'            TO MSGI-IDTRANS                               
039500                                                                          
039600*    - LANGUAGE TO BE USED BY MEDKONV                                     
039700     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
039800                                                                          
039900     MOVE YES TO KEYS-SW                                                  
040000                                                                          
040100     IF OWN-MID                                                           
040200       MOVE MID-IDANSK-FOM-IN TO MSGI-IDANSK-FOM                          
040300       MOVE MID-IDANSK-TOM-IN TO MSGI-IDANSK-TOM                          
040400       MOVE MID-IDLEVNR-IN    TO MSGI-IDLEVNR                             
040500       MOVE MID-KDLPORS-IN    TO MSGI-KDLPORS                             
040600       MOVE MID-KDLEVPLF-IN   TO MSGI-KDLEVPLF                            
040700       MOVE MID-IDDC-IN       TO MSGI-IDDC-KEY                            
040800     ELSE                                                                 
040900       MOVE ALL '+'           TO MSGI-IDANSK-FOM                          
041000                                 MSGI-IDANSK-TOM                          
041100                                 MSGI-IDLEVNR                             
041200                                 MSGI-KDLPORS                             
041300                                 MSGI-KDLEVPLF                            
041400                                 MSGI-IDDC-KEY                            
041500                                 MID-IDANSK-FOM-IN                        
041600                                 MID-IDANSK-TOM-IN                        
041700                                 MID-IDLEVNR-IN                           
041800                                 MID-KDLPORS-IN                           
041900                                 MID-KDLEVPLF-IN                          
042000                                 MID-IDDC-IN                              
042100     END-IF                                                               
042200                                                                          
042300*    --- Hämtar USER-SAVEAREA  KDCALL = 001                               
042400     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
042500     MOVE MSGI-SPAR-AREA  TO SAVE-AREA                                    
042600     MOVE SPACE           TO SAVE-FROM-2403                               
042700                                                                          
042800     IF SAVE-IDTRANS = '2447'                                             
042900       IF SEEK-IDANSK-FOM NOT NUMERIC                                     
043000          MOVE ZERO         TO SEEK-IDANSK-FOM                            
043100       END-IF                                                             
043200       IF SEEK-IDANSK-TOM  NOT NUMERIC                                    
043300          MOVE ZERO         TO SEEK-IDANSK-TOM                            
043400       END-IF                                                             
043500       IF SEEK-KDLPORS     NOT NUMERIC                                    
043600          MOVE ZERO         TO SEEK-KDLPORS                               
043700       END-IF                                                             
043800     ELSE                                                                 
043900       IF MSGI-IDANSK-FOM NUMERIC                                         
044000         MOVE MSGI-IDANSK-FOM TO MID-IDANSK-FOM-IN                        
044100                                 SEEK-IDANSK-FOM                          
044200       ELSE                                                               
044300         MOVE ALL '+'           TO MSGI-IDANSK-FOM                        
044400                                   MID-IDANSK-FOM-IN                      
044500         MOVE ZERO              TO SEEK-IDANSK-FOM                        
044600       END-IF                                                             
044700       IF MSGI-IDANSK-TOM NUMERIC                                         
044800         MOVE MSGI-IDANSK-TOM TO MID-IDANSK-TOM-IN                        
044900                                 SEEK-IDANSK-TOM                          
045000       ELSE                                                               
045100         MOVE ALL '+'           TO MSGI-IDANSK-TOM                        
045200                                   MID-IDANSK-TOM-IN                      
045300         MOVE ZERO              TO SEEK-IDANSK-TOM                        
045400       END-IF                                                             
045500       MOVE MSGI-IDLEVNR    TO MID-IDLEVNR-IN                             
045600                               SEEK-IDLEVNR                               
045700       IF MSGI-KDLPORS    NUMERIC                                         
045800         MOVE MSGI-KDLPORS    TO MID-KDLPORS-IN                           
045900                                 SEEK-KDLPORS                             
046000       ELSE                                                               
046100         MOVE ALL '+'           TO MSGI-KDLPORS                           
046200                                   MID-KDLPORS-IN                         
046300         MOVE ZERO              TO SEEK-KDLPORS                           
046400       END-IF                                                             
046500       MOVE MSGI-KDLEVPLF   TO MID-KDLEVPLF-IN                            
046600                               SEEK-KDLEVPLF                              
046700       MOVE MSGI-IDDC-KEY   TO MID-IDDC-IN                                
046800                               SEEK-IDDC                                  
046900       IF  SAVE-IDTRANS = '2403'                                          
047000           MOVE SAVE-AREA              TO SAVE-FROM-2403                  
047100           IF SAVE-2403-DIALOG = YES                                      
047200              MOVE SAVE-IDDC-FROM-2403 TO MID-IDDC-IN                     
047300                                             SEEK-IDDC                    
047400              MOVE SAVE-IDLEVNR-FROM-2403 TO MID-IDLEVNR-IN               
047500                                             SEEK-IDLEVNR                 
047600           ELSE                                                           
047700              MOVE SPACE         TO SAVE-FROM-2403                        
047800           END-IF                                                         
047900       END-IF                                                             
048000       MOVE SPACE                TO SAVE-AREA                             
048100       MOVE ZERO                 TO SAVE-PAGENO                           
048200                                    SAVE-KVRADER                          
048300     END-IF                                                               
048400                                                                          
048500*    |                                                                    
048600*    -- CONTROL OF MANDATORY KEY                                          
048700*    |                                                                    
048800     MOVE MFS-ERASE-FIELD TO MOD-IDANSK-FOM-IN                            
048900                                                                          
049000     IF  MID-IDANSK-FOM-IN = ALL '+'                                      
049100     AND MID-IDANSK-TOM-IN = ALL '+'                                      
049200     AND MID-IDLEVNR-IN    = ALL '+'                                      
049300     AND MID-KDLPORS-IN    = ALL '+'                                      
049400     AND MID-KDLEVPLF-IN   = ALL '+'                                      
049500     AND MID-IDDC-IN       = ALL '+'                                      
049600                                                                          
049700        SET SET-EJ-KVRADER TO TRUE                                        
049800     ELSE                                                                 
049900        MOVE '7'         TO MFS-IDPFK                                     
050000        MOVE SPACE       TO MFS-KDTRTYP                                   
050100*   --- New master key gives the first page                               
050200        MOVE ZERO        TO SAVE-KVRADER                                  
050300                            SAVE-PAGENO                                   
050400        MOVE SPACE       TO SAMMA-KEY-WDD601KY                            
050500                             NEXT-KEY-WDD601KY                            
050600         SET SET-KVRADER TO TRUE                                          
050700     END-IF                                                               
050800                                                                          
050900*    --- New or saved mandatory key                                       
051000     IF KEYS-OK                                                           
051100       IF MSGI-IDANSK-FOM NOT NUMERIC                                     
051200         MOVE MFS-NUM-FIELD-WRONG TO MOD-IDANSK-FOM-IN-ATTR               
051300         MOVE ERR-WRONG-KEY       TO MED-IDMFSFEL                         
051400         MOVE NOO                 TO KEYS-SW                              
051500       ELSE                                                               
051600         MOVE MSGI-IDANSK-FOM     TO MOD-IDANSK-FOM-UT                    
051700                                     SEEK-IDANSK-FOM                      
051800       END-IF                                                             
051900     ELSE                                                                 
052000       MOVE MFS-ERASE-FIELD       TO MOD-IDANSK-FOM-UT                    
052100     END-IF                                                               
052200                                                                          
052300*    -- IDDC                                                              
052400     MOVE MFS-ERASE-FIELD     TO MOD-IDDC-IN                              
052500                                                                          
052600     IF MSGI-IDDC-KEY = ALL '+' OR SPACE                                  
052700        MOVE MSGI-IDFTG        TO WS-IDFTG                                
052800        IF IDFTG-US                                                       
052900           MOVE '41'           TO W-IDDC-MIN                              
053000           MOVE '49'           TO W-IDDC-MAX                              
053100           MOVE '40'           to MSGI-IDDC-KEY                           
053200        ELSE                                                              
053300           IF IDFTG-CN                                                    
053400              MOVE '71'        TO W-IDDC-MIN                              
053500              MOVE '79'        TO W-IDDC-MAX                              
053600              move '70'        TO MSGI-IDDC-KEY                           
053700           ELSE                                                           
053800              MOVE NOO         TO KEYS-SW                                 
053900           END-IF                                                         
054000        END-IF                                                            
054100     ELSE                                                                 
054200        MOVE MSGI-IDDC-KEY     TO W-IDDC-MIN                              
054300                                  W-IDDC-MAX                              
054400        IF MSGI-IDDC-KEY(2:1) = '0'                                       
054500           MOVE '1'            TO W-IDDC2-MIN                             
054600           MOVE '9'            TO W-IDDC2-MAX                             
054700        END-IF                                                            
054800     END-IF                                                               
054900                                                                          
055000     PERFORM BA-CHECK-IDANSK-TO                                           
055100     PERFORM BB-CHECK-IDLEVNR                                             
055200     PERFORM BC-CHECK-KDLPORS                                             
055300     PERFORM BD-CHECK-KDLEVPLF                                            
055400     PERFORM BE-CHECK-IDDC                                                
055500                                                                          
055600     IF KEYS-WRONG                                                        
055700       CALL WMEDKONV     USING MED-WMEDAREA                               
055800       MOVE MED-MFSFEL      TO MOD-TEMFSFEL                               
055900       PERFORM MFS-ERASE-FIELD-IN                                         
056000       PERFORM MFS-ERASE-FIELD-OUT                                        
056100     END-IF                                                               
056200                                                                          
056300     IF DC-WRONG                                                          
056400       MOVE ERR-WRONG-DC    TO MED-IDMFSFEL                               
056500       CALL WMEDKONV     USING MED-WMEDAREA                               
056600       MOVE MED-MFSFEL      TO MOD-TEMFSFEL                               
056700       PERFORM MFS-ERASE-FIELD-IN                                         
056800       PERFORM MFS-ERASE-FIELD-OUT                                        
056900     END-IF                                                               
057000     .                                                                    
057100                                                                          
057200                                                                          
057300 BA-CHECK-IDANSK-TO       SECTION.                                        
057400     MOVE 'BA-CHECK-IDANSKT' TO CURRENT-SECTION                           
057500                                                                          
057600     MOVE MFS-ERASE-FIELD          TO MOD-IDANSK-TOM-UT                   
057700     IF KEYS-OK                                                           
057800        IF MSGI-IDANSK-TOM = ALL '+' OR SPACE                             
057900* ---      Normalt maxvärde  (IDANSK med slutsiffra = 9)                  
058000           MOVE SEEK-IDANSK-FOM TO W-IDANSK-HELP                          
058100           MOVE 9                TO W-IDANSK-HELP-3                       
058200           MOVE W-IDANSK-HELP    TO SEEK-IDANSK-TOM                       
058300        ELSE                                                              
058400           IF MSGI-IDANSK-TOM NUMERIC                                     
058500              MOVE MSGI-IDANSK-TOM       TO SEEK-IDANSK-TOM               
058600           ELSE                                                           
058700              MOVE ZERO                  TO SEEK-IDANSK-TOM               
058800              IF OWN-MID                                                  
058900* ---           bara felmeddelande om inmatat på egen mid                 
059000                MOVE MFS-NUM-FIELD-WRONG TO MOD-IDANSK-TOM-IN-ATTR        
059100                MOVE ERR-WRONG-KEY       TO MED-IDMFSFEL                  
059200                MOVE NOO                 TO KEYS-SW                       
059300              END-IF                                                      
059400           END-IF                                                         
059500        END-IF                                                            
059600                                                                          
059700                                                                          
059800        MOVE SEEK-IDANSK-TOM    TO MOD-IDANSK-TOM-UT                      
059900     END-IF                                                               
060000     .                                                                    
060100                                                                          
060200                                                                          
060300 BB-CHECK-IDLEVNR       SECTION.                                          
060400     MOVE 'BB-CHECK-IDLEVNR' TO CURRENT-SECTION                           
060500                                                                          
060600     MOVE MFS-ERASE-FIELD            TO MOD-IDLEVNR-UT                    
060700     IF KEYS-OK                                                           
060800                                                                          
060900        IF OWN-MID OR HELP-MID                                            
061000           IF MID-IDLEVNR-IN = ALL '+'                                    
061100              IF  SAVE-IDTRANS = '2447'                                   
061200              AND SEEK-IDLEVNR NOT = SPACE                                
061300                 MOVE SEEK-IDLEVNR   TO MOD-IDLEVNR-UT                    
061400              END-IF                                                      
061500           ELSE                                                           
061600              IF MID-IDLEVNR-IN = SPACE OR '0    '                        
061700* ---            MAN VILL ÅTERSTÄLLA TILL NORMAL FUNKTION                 
061800                 MOVE SPACE          TO SEEK-IDLEVNR                      
061900              ELSE                                                        
062000                 MOVE MSGI-IDLEVNR   TO SEEK-IDLEVNR                      
062100                                        MOD-IDLEVNR-UT                    
062200              END-IF                                                      
062300           END-IF                                                         
062400        ELSE                                                              
062500           IF  SAVE-IDTRANS-FROM-2403 = '2403'                            
062600              MOVE SAVE-IDLEVNR-FROM-2403 TO MOD-IDLEVNR-UT               
062700                                             SEEK-IDLEVNR                 
062800           ELSE                                                           
062900             IF SEEK-IDLEVNR NOT = SPACE                                  
063000                MOVE SEEK-IDLEVNR TO MOD-IDLEVNR-UT                       
063100             END-IF                                                       
063200           END-IF                                                         
063300        END-IF                                                            
063400     END-IF                                                               
063500     .                                                                    
063600                                                                          
063700 BC-CHECK-KDLPORS       SECTION.                                          
063800     MOVE 'BC-CHECK-KDLPORS' TO CURRENT-SECTION                           
063900                                                                          
064000     MOVE MFS-ERASE-FIELD                TO MOD-KDLPORS-UT                
064100     IF KEYS-OK                                                           
064200                                                                          
064300        IF OWN-MID                                                        
064400           IF MSGI-KDLPORS = ALL '+'                                      
064500              IF  SAVE-IDTRANS = '2447'                                   
064600              AND SEEK-KDLPORS NOT = ZERO                                 
064700                 MOVE SEEK-KDLPORS        TO MOD-KDLPORS-UT               
064800              END-IF                                                      
064900           ELSE                                                           
065000              IF MSGI-KDLPORS = SPACE OR '0 ' OR ZERO                     
065100              OR MSGI-KDLPORS NUMERIC                                     
065200                 IF MSGI-KDLPORS = SPACE OR '0 ' OR ZERO                  
065300* ---               Man vill återställa till normal funktion              
065400                    MOVE ZERO             TO SEEK-KDLPORS                 
065500                 ELSE                                                     
065600                    MOVE MSGI-KDLPORS     TO SEEK-KDLPORS                 
065700                                             MOD-KDLPORS-UT               
065800                 END-IF                                                   
065900              ELSE                                                        
066000                 MOVE MFS-NUM-FIELD-WRONG TO MOD-KDLPORS-IN-ATTR          
066100                 MOVE ERR-WRONG-CODE      TO MED-IDMFSFEL                 
066200                 MOVE NOO                 TO KEYS-SW                      
066300              END-IF                                                      
066400           END-IF                                                         
066500        ELSE                                                              
066600           MOVE SEEK-KDLPORS              TO MOD-KDLPORS-UT               
066700        END-IF                                                            
066800     END-IF                                                               
066900     .                                                                    
067000                                                                          
067100                                                                          
067200 BD-CHECK-KDLEVPLF       SECTION.                                         
067300     MOVE 'BD-CHECK-KDLEVPL' TO CURRENT-SECTION                           
067400                                                                          
067500     MOVE MFS-ERASE-FIELD                TO MOD-KDLEVPLF-UT               
067600     IF KEYS-OK                                                           
067700                                                                          
067800        IF OWN-MID                                                        
067900           IF MSGI-KDLEVPLF = ALL '+'                                     
068000              IF  SAVE-IDTRANS = '2447'                                   
068100              AND SEEK-KDLEVPLF NOT = SPACE                               
068200                 MOVE SEEK-KDLEVPLF      TO MOD-KDLEVPLF-UT               
068300              END-IF                                                      
068400           ELSE                                                           
068500              IF MSGI-KDLEVPLF =                                          
068600                  'J' OR 'N' OR 'S' OR 'G' OR 'P' OR ' '                  
068700                 IF MSGI-KDLEVPLF = SPACE                                 
068800* ---               MAN VILL ÅTERSTÄLLA TILL NORMAL FUNKTION              
068900                    MOVE SPACE           TO SEEK-KDLEVPLF                 
069000                 ELSE                                                     
069100                    MOVE MSGI-KDLEVPLF   TO SEEK-KDLEVPLF                 
069200                                            MOD-KDLEVPLF-UT               
069300                 END-IF                                                   
069400              ELSE                                                        
069500                 MOVE MFS-ALPHA-FIELD-WRONG                               
069600                                         TO MOD-KDLEVPLF-IN-ATTR          
069700                 MOVE ERR-WRONG-CODE     TO MED-IDMFSFEL                  
069800                 MOVE NOO                TO KEYS-SW                       
069900              END-IF                                                      
070000           END-IF                                                         
070100        ELSE                                                              
070200           IF  SAVE-IDTRANS = '2447'                                      
070300           AND SEEK-KDLEVPLF NOT = SPACE                                  
070400              MOVE SEEK-KDLEVPLF         TO MOD-KDLEVPLF-UT               
070500           ELSE                                                           
070600              MOVE SPACE                 TO MOD-KDLEVPLF-UT               
070700           END-IF                                                         
070800        END-IF                                                            
070900     END-IF                                                               
071000     .                                                                    
071100                                                                          
071200                                                                          
071300 BE-CHECK-IDDC SECTION.                                                   
071400     MOVE 'BE-CHECK-IDDC   ' TO CURRENT-SECTION                           
071500                                                                          
071600     MOVE MFS-ERASE-FIELD     TO MOD-IDDC-UT                              
071700     IF KEYS-OK                                                           
071800        IF  SAVE-IDTRANS-FROM-2403 = '2403'                               
071900            MOVE SAVE-IDDC-FROM-2403   TO SEEK-IDDC                       
072000                                          MOD-IDDC-UT                     
072100                                          W-IDDC-B6                       
072200        ELSE                                                              
072300           IF MID-IDDC-IN = ALL '+'                                       
072400              MOVE SEEK-IDDC           TO MOD-IDDC-UT                     
072500                                          W-IDDC-B6                       
072600           ELSE                                                           
072700              MOVE MSGI-IDDC-KEY       TO SEEK-IDDC                       
072800                                          MOD-IDDC-UT                     
072900                                          W-IDDC-B6                       
073000           END-IF                                                         
073100        END-IF                                                            
073200     END-IF                                                               
073300                                                                          
073400     IF W-IDDC-B6 = SPACE                                                 
073500        MOVE MSGI-IDFTG        TO WS-IDFTG                                
073600        IF IDFTG-US                                                       
073700           MOVE '41'           TO W-IDDC-B6-MIN-X                         
073800           MOVE '49'           TO W-IDDC-B6-MAX-X                         
073900        ELSE                                                              
074000           IF IDFTG-CN                                                    
074100              MOVE '71'        TO W-IDDC-B6-MIN-X                         
074200              MOVE '79'        TO W-IDDC-B6-MAX-X                         
074300           ELSE                                                           
074400              MOVE NOO         TO KEYS-SW                                 
074500           END-IF                                                         
074600        END-IF                                                            
074700     ELSE                                                                 
074800        MOVE W-IDDC-B6                 TO W-IDDC-B6-MIN-X                 
074900                                          W-IDDC-B6-MAX-X                 
075000        IF W-IDDC-B6(2:1) = '0'                                           
075100           MOVE '1'                    TO W-IDDC2-B6-MIN                  
075200           MOVE '9'                    TO W-IDDC2-B6-MAX                  
075300        END-IF                                                            
075400     END-IF                                                               
075500                                                                          
075600     MOVE NOO                          TO W-DC-OK                         
075700     PERFORM IMS-GU-WDB601-MIN-MAX                                        
075800     PERFORM UNTIL SEGMENT-MISSING                                        
075900                OR SEGMENT-END                                            
076000                OR W-DC-OK = YES                                          
076100        IF DCS-NDC-CN                                                     
076200        OR (DCS-NDC-NA AND DCS-USA)                                       
076300           MOVE YES                    TO W-DC-OK                         
076400        END-IF                                                            
076500        PERFORM IMS-GN-WDB601-MIN-MAX                                     
076600     END-PERFORM                                                          
076700                                                                          
076800     IF W-DC-OK = NOO                                                     
076900        MOVE NOO                       TO DC-SW                           
077000     END-IF                                                               
077100     .                                                                    
077200                                                                          
077300                                                                          
077400 C-FIRST-PAGE SECTION.                                                    
077500     MOVE 'C-FIRST-PAGE    ' TO CURRENT-SECTION                           
077600                                                                          
077700*    -- WHENEVER KEY CHANGES OR WHEN PF7 IS HIT                           
077800     MOVE INF-FIRST-PAGE     TO MED-IDMFSINF                              
077900     CALL WMEDKONV        USING MED-WMEDAREA                              
078000     MOVE MED-MFSINF         TO MOD-TEMFSFEL                              
078100     PERFORM MFS-ERASE-FIELD-IN                                           
078200                                                                          
078300     MOVE SPACE              TO NEXT-KEY-WDD601KY                         
078400     MOVE +1                 TO SAVE-PAGENO                               
078500     .                                                                    
078600                                                                          
078700                                                                          
078800 D-NEXT-PAGE SECTION.                                                     
078900     MOVE 'D-FIRST-PAGE    ' TO CURRENT-SECTION                           
079000                                                                          
079100     IF SAVE-IDTRANS = '2447'                                             
079200        MOVE NEXT-KEY-WDD601KY  TO CURR-KEY-WDD601KY                      
079300        MOVE SAVE-PAGENO        TO SIDX                                   
079400        IF SAMMA-KEY-WDD601KY NOT = NEXT-KEY-WDD601KY                     
079500* ---      NY SIDA, OM INTE VISAR VI SAMMA SIDA IGEN                      
079600           ADD +1               TO SAVE-PAGENO                            
079700        END-IF                                                            
079800                                                                          
079900     ELSE                                                                 
080000        MOVE SPACE              TO NEXT-KEY-WDD601KY                      
080100     END-IF                                                               
080200     .                                                                    
080300                                                                          
080400                                                                          
080500 E-SAME-PAGE SECTION.                                                     
080600     MOVE 'E-SAME-PAGE     ' TO CURRENT-SECTION                           
080700                                                                          
080800     IF SAVE-IDTRANS = '2447' OR '0551'                                   
080900                                                                          
081000       MOVE SAMMA-KEY-WDD601KY TO CURR-KEY-WDD601KY                       
081100       MOVE SPACE              TO NEXT-KEY-WDD601KY                       
081200                                                                          
081300       IF MID-IDANSK-FOM-IN  = ALL '+'                                    
081400       AND MID-IDANSK-TOM-IN = ALL '+'                                    
081500       AND MID-IDLEVNR-IN    = ALL '+'                                    
081600       AND MID-KDLPORS-IN    = ALL '+'                                    
081700       AND MID-KDLEVPLF-IN   = ALL '+'                                    
081800       AND MID-IDDC-IN       = ALL '+'                                    
081900       AND MID-KDCMDVAL(1)   = '+' AND MID-KDCMDVAL(2) = '+'              
082000       AND MID-KDCMDVAL(3)   = '+' AND MID-KDCMDVAL(4) = '+'              
082100       AND MID-KDCMDVAL(5)   = '+' AND MID-KDCMDVAL(6) = '+'              
082200       AND MID-KDCMDVAL(7)   = '+' AND MID-KDCMDVAL(8) = '+'              
082300       AND MID-KDCMDVAL(9)   = '+' AND MID-KDCMDVAL(10) = '+'             
082400       AND MID-KDCMDVAL(11)  = '+' AND MID-KDCMDVAL(12) = '+'             
082500       AND MID-KDCMDVAL(13)  = '+' AND MID-KDCMDVAL(14) = '+'             
082600       AND MID-KDCMDVAL(15)  = '+'                                        
082700         PERFORM MFS-ERASE-FIELD-IN                                       
082800       ELSE                                                               
082900         IF ( MID-IDANSK-FOM-IN = ALL '+'                                 
083000         AND MID-IDANSK-TOM-IN  = ALL '+'                                 
083100         AND MID-IDLEVNR-IN     = ALL '+'                                 
083200         AND MID-KDLPORS-IN     = ALL '+'                                 
083300         AND MID-KDLEVPLF-IN    = ALL '+'                                 
083400         AND MID-IDDC-IN        = ALL '+' )                               
083500                                                                          
083600         AND ((MID-KDCMDVAL(1)  NOT = '+')  OR                            
083700              (MID-KDCMDVAL(2)  NOT = '+')  OR                            
083800              (MID-KDCMDVAL(3)  NOT = '+')  OR                            
083900              (MID-KDCMDVAL(4)  NOT = '+')  OR                            
084000              (MID-KDCMDVAL(5)  NOT = '+')  OR                            
084100              (MID-KDCMDVAL(6)  NOT = '+')  OR                            
084200              (MID-KDCMDVAL(7)  NOT = '+')  OR                            
084300              (MID-KDCMDVAL(8)  NOT = '+')  OR                            
084400              (MID-KDCMDVAL(9)  NOT = '+')  OR                            
084500              (MID-KDCMDVAL(10) NOT = '+')  OR                            
084600              (MID-KDCMDVAL(11) NOT = '+')  OR                            
084700              (MID-KDCMDVAL(12) NOT = '+')  OR                            
084800              (MID-KDCMDVAL(13) NOT = '+')  OR                            
084900              (MID-KDCMDVAL(14) NOT = '+')  OR                            
085000              (MID-KDCMDVAL(15) NOT = '+') )                              
085100                                                                          
085200           MOVE INF-PRESS-F9-TO-JUMP TO MED-IDMFSINF                      
085300           CALL WMEDKONV          USING MED-WMEDAREA                      
085400           MOVE MED-MFSINF           TO MOD-TEMFSFEL                      
085500                                                                          
085600           PERFORM EA-MID-INDATA-TO-MOD                                   
085700           MOVE 'E-SAME-PAGE 2   '   TO CURRENT-SECTION                   
085800                                                                          
085900         END-IF                                                           
086000       END-IF                                                             
086100     ELSE                                                                 
086200       PERFORM MFS-ERASE-FIELD-IN                                         
086300     END-IF                                                               
086400     .                                                                    
086500                                                                          
086600                                                                          
086700 EA-MID-INDATA-TO-MOD SECTION.                                            
086800     MOVE 'EA-MID-TO-MOD   '   TO CURRENT-SECTION                         
086900                                                                          
087000     IF MID-IDANSK-FOM-IN = ALL '+'                                       
087100       MOVE MFS-ERASE-FIELD       TO MOD-IDANSK-FOM-IN                    
087200     ELSE                                                                 
087300       MOVE MID-IDANSK-FOM-IN     TO MOD-IDANSK-FOM-IN                    
087400       MOVE MFS-ADD-READ-FIELD    TO MOD-IDANSK-FOM-IN-ATTR               
087500     END-IF                                                               
087600                                                                          
087700     IF MID-IDANSK-TOM-IN = ALL '+'                                       
087800       MOVE MFS-ERASE-FIELD       TO MOD-IDANSK-TOM-IN                    
087900     ELSE                                                                 
088000       MOVE MID-IDANSK-TOM-IN     TO MOD-IDANSK-TOM-IN                    
088100       MOVE MFS-ADD-READ-FIELD    TO MOD-IDANSK-TOM-IN-ATTR               
088200     END-IF                                                               
088300                                                                          
088400     IF MID-IDLEVNR-IN  = ALL '+'                                         
088500       MOVE MFS-ERASE-FIELD       TO MOD-IDLEVNR-IN                       
088600     ELSE                                                                 
088700       MOVE MID-IDLEVNR-IN        TO MOD-IDLEVNR-IN                       
088800       MOVE MFS-ADD-READ-FIELD TO MOD-IDLEVNR-IN-ATTR                     
088900     END-IF                                                               
089000                                                                          
089100     IF MID-KDLPORS-IN = ALL '+'                                          
089200       MOVE MFS-ERASE-FIELD       TO MOD-KDLPORS-IN                       
089300     ELSE                                                                 
089400       MOVE MID-KDLPORS-IN        TO MOD-KDLPORS-IN                       
089500       MOVE MFS-ADD-READ-FIELD TO MOD-KDLPORS-IN-ATTR                     
089600     END-IF                                                               
089700                                                                          
089800     IF MID-KDLEVPLF-IN = ALL '+'                                         
089900       MOVE MFS-ERASE-FIELD       TO MOD-KDLEVPLF-IN                      
090000     ELSE                                                                 
090100       MOVE MID-KDLEVPLF-IN       TO MOD-KDLEVPLF-IN                      
090200       MOVE MFS-ADD-READ-FIELD TO MOD-KDLEVPLF-IN-ATTR                    
090300     END-IF                                                               
090400                                                                          
090500     IF MID-IDDC-IN = ALL '+'                                             
090600       MOVE MFS-ERASE-FIELD       TO MOD-IDDC-IN                          
090700     ELSE                                                                 
090800       MOVE MID-IDDC-IN           TO MOD-IDDC-IN                          
090900       MOVE MFS-ADD-READ-FIELD    TO MOD-IDDC-IN-ATTR                     
091000     END-IF                                                               
091100                                                                          
091200     MOVE +1                        TO INDX                               
091300     PERFORM UNTIL INDX > MAX-INDX                                        
091400       IF MID-KDCMDVAL(INDX) = ALL '+'                                    
091500         MOVE MFS-ERASE-FIELD       TO MOD-KDCMDVAL(INDX)                 
091600       ELSE                                                               
091700         MOVE MID-KDCMDVAL(INDX)    TO MOD-KDCMDVAL(INDX)                 
091800         MOVE MFS-ADD-READ-FIELD    TO MOD-KDCMDVAL-ATTR(INDX)            
091900       END-IF                                                             
092000       ADD +1                       TO INDX                               
092100     END-PERFORM                                                          
092200     .                                                                    
092300                                                                          
092400                                                                          
092500 F-READ-SHOW-INFO SECTION.                                                
092600     MOVE 'F-READ-SHOW-INFO'   TO CURRENT-SECTION                         
092700                                                                          
092800     IF NEXT-KEY-WDD601KY = SPACE                                         
092900        PERFORM FA-GET-FIRST-RECORD                                       
093000     ELSE                                                                 
093100        PERFORM FB-GET-NEXT-RECORD                                        
093200     END-IF                                                               
093300                                                                          
093400     IF SEGMENT-FOUND                                                     
093500        IF SET-KVRADER                                                    
093600           MOVE +1 TO SAVE-KVRADER                                        
093700        END-IF                                                            
093800                                                                          
093900        MOVE +1 TO INDX                                                   
094000        PERFORM FC-MOVE-DATA-TO-MOD                                       
094100                                                                          
094200*      -- Fyll på med resten av GODK-poster                               
094300        PERFORM IMS-GN-WDD601                                             
094400        ADD 1 TO INDX                                                     
094500                                                                          
094600        PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                      
094700                   OR INDX > MAX-INDX                                     
094800                                                                          
094900           PERFORM S100-KOLLA-OM-GODK-POST                                
095000           IF GODK-POST                                                   
095100              IF SET-KVRADER                                              
095200                 ADD +1           TO SAVE-KVRADER                         
095300              END-IF                                                      
095400              PERFORM FC-MOVE-DATA-TO-MOD                                 
095500              ADD +1                TO INDX                               
095600           END-IF                                                         
095700           PERFORM IMS-GN-WDD601                                          
095800        END-PERFORM                                                       
095900                                                                          
096000        IF INDX <= MAX-INDX                                               
096100* --- Töm sista raden / resten av raderna på skärmen                      
096200           PERFORM UNTIL INDX > MAX-INDX                                  
096300              MOVE MFS-ERASE-FIELD TO MOD-IDLEVNR (INDX)                  
096400                                      MOD-IDARTNR (INDX)                  
096500                                      MOD-KDLPORS-TEXT (INDX, 1)          
096600                                      MOD-KDLPORS-TEXT (INDX, 2)          
096700                                      MOD-KDLPORS-TEXT (INDX, 3)          
096800                                      MOD-KDLEVPLF (INDX)                 
096900                                      MOD-BEART    (INDX)                 
097000                                      MOD-TIOMSPEC (INDX)                 
097100                                      MOD-IDDC     (INDX)                 
097200              ADD 1 TO INDX                                               
097300           END-PERFORM                                                    
097400        END-IF                                                            
097500                                                                          
097600       IF SEGMENT-FOUND                                                   
097700*        -- Det fanns poster kvar att läsa på basen                       
097800*        -- Läs ev. fram till GODK-POST och fixa i.s.f. NEXT-keys         
097900          PERFORM S100-KOLLA-OM-GODK-POST                                 
098000          PERFORM UNTIL GODK-POST OR SEGMENT-END                          
098100                     OR SEGMENT-MISSING                                   
098200             PERFORM IMS-GN-WDD601                                        
098300             IF SEGMENT-FOUND                                             
098400               PERFORM S100-KOLLA-OM-GODK-POST                            
098500             END-IF                                                       
098600          END-PERFORM                                                     
098700                                                                          
098800          IF GODK-POST                                                    
098900             IF SET-KVRADER                                               
099000* ---           Fortsätter räkna upp KVRADER                              
099100                ADD +1 TO SAVE-KVRADER                                    
099200             END-IF                                                       
099300                                                                          
099400* ---        Flytta denna nyckel till NEXT-keys !                         
099500             MOVE LPF-IDLEVNR  TO NEXT-IDLEVNR                            
099600             MOVE LPF-IDARTNR  TO NEXT-IDARTNR                            
099700             MOVE LPF-IDANSK   TO NEXT-IDANSK                             
099800             MOVE LPF-IDDC     TO NEXT-IDDC                               
099900                                                                          
100000             MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                    
100100             CALL WMEDKONV USING MED-WMEDAREA                             
100200             MOVE MED-TEMFSINF TO MOD-TEMFSINF                            
100300          END-IF                                                          
100400       ELSE                                                               
100500          MOVE INF-LAST-PAGE-SHOWN  TO MED-IDMFSINF                       
100600          CALL WMEDKONV USING MED-WMEDAREA                                
100700          MOVE MED-TEMFSINF TO MOD-TEMFSINF                               
100800       END-IF                                                             
100900                                                                          
101000*      --- Slutkontroll                                                   
101100       IF SEGMENT-FOUND AND SET-KVRADER                                   
101200* ---     Fortsätter räkna upp KVRADER                                    
101300* ---     läser vidare till BASENS slut och räknar GODK-POST              
101400                                                                          
101500          PERFORM IMS-GN-WDD601                                           
101600          PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                    
101700             PERFORM S100-KOLLA-OM-GODK-POST                              
101800             IF GODK-POST AND SET-KVRADER                                 
101900                ADD +1 TO SAVE-KVRADER                                    
102000             END-IF                                                       
102100             PERFORM IMS-GN-WDD601                                        
102200          END-PERFORM                                                     
102300       END-IF                                                             
102400       MOVE SAVE-KVRADER TO MOD-KVRADER                                   
102500       MOVE SAVE-PAGENO  TO MOD-IDPAGE                                    
102600                                                                          
102700       MOVE '002'      TO MSGI-KDCALL                                     
102800       MOVE '2447'     TO SAVE-IDTRANS                                    
102900       MOVE SAVE-AREA  TO MSGI-SPAR-AREA                                  
103000       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
103100     ELSE                                                                 
103200       MOVE INF-NO-LINES-EXISTS TO MED-IDMFSINF                           
103300       CALL WMEDKONV         USING MED-WMEDAREA                           
103400       MOVE MED-MFSINF          TO MOD-TEMFSFEL                           
103500       PERFORM MFS-ERASE-FIELD-IN                                         
103600     END-IF                                                               
103700     .                                                                    
103800                                                                          
103900                                                                          
104000 FA-GET-FIRST-RECORD       SECTION.                                       
104100     MOVE 'FA-GET-FIRST-REC'   TO CURRENT-SECTION                         
104200                                                                          
104300     SET EJ-GODK-POST TO TRUE                                             
104400     PERFORM IMS-GU-WDD601                                                
104500     IF SEGMENT-FOUND                                                     
104600        PERFORM S100-KOLLA-OM-GODK-POST                                   
104700        PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                      
104800                   OR GODK-POST                                           
104900                                                                          
105000           PERFORM IMS-GN-WDD601                                          
105100           IF SEGMENT-FOUND                                               
105200              PERFORM S100-KOLLA-OM-GODK-POST                             
105300           END-IF                                                         
105400        END-PERFORM                                                       
105500     END-IF                                                               
105600                                                                          
105700     IF GODK-POST                                                         
105800        MOVE LPF-IDLEVNR    TO SAMMA-IDLEVNR                              
105900        MOVE LPF-IDARTNR    TO SAMMA-IDARTNR                              
106000        MOVE LPF-IDANSK     TO SAMMA-IDANSK                               
106100        MOVE LPF-IDDC       TO SAMMA-IDDC                                 
106200     END-IF                                                               
106300     .                                                                    
106400                                                                          
106500                                                                          
106600 FB-GET-NEXT-RECORD        SECTION.                                       
106700     MOVE 'FB-GET-NEXT-REC '   TO CURRENT-SECTION                         
106800                                                                          
106900     SET EJ-GODK-POST TO TRUE                                             
107000     PERFORM IMS-GU-WDD601                                                
107100     IF SEGMENT-FOUND                                                     
107200        PERFORM IMS-GN-WDD601                                             
107300        PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                      
107400                   OR GODK-POST                                           
107500                                                                          
107600           IF  LPF-IDDC    = NEXT-IDDC                                    
107700           AND LPF-IDLEVNR = NEXT-IDLEVNR                                 
107800           AND LPF-IDARTNR = NEXT-IDARTNR                                 
107900           AND LPF-IDANSK  = NEXT-IDANSK                                  
108000               SET GODK-POST TO TRUE                                      
108100           ELSE                                                           
108200              PERFORM IMS-GN-WDD601                                       
108300           END-IF                                                         
108400        END-PERFORM                                                       
108500     END-IF                                                               
108600                                                                          
108700     IF GODK-POST                                                         
108800        MOVE LPF-IDDC       TO SAMMA-IDDC                                 
108900        MOVE LPF-IDLEVNR    TO SAMMA-IDLEVNR                              
109000        MOVE LPF-IDARTNR    TO SAMMA-IDARTNR                              
109100        MOVE LPF-IDANSK     TO SAMMA-IDANSK                               
109200     END-IF                                                               
109300     .                                                                    
109400                                                                          
109500                                                                          
109600 FC-MOVE-DATA-TO-MOD SECTION.                                             
109700     MOVE 'FC-MOVE-TO-MOD  '   TO CURRENT-SECTION                         
109800                                                                          
109900     MOVE LPF-IDDC       TO MOD-IDDC     (INDX)                           
110000     MOVE LPF-IDLEVNR    TO MOD-IDLEVNR  (INDX)                           
110100     MOVE LPF-IDARTNR    TO MOD-IDARTNR  (INDX)                           
110200     MOVE LPF-IDANSK     TO W-IDANSK-RAD (INDX)                           
110300     MOVE SPACE          TO MOD-KDLPORS-TEXT (INDX, 1)                    
110400                            MOD-KDLPORS-TEXT (INDX, 2)                    
110500                            MOD-KDLPORS-TEXT (INDX, 3)                    
110600     MOVE +1 TO OIX, TIX                                                  
110700     PERFORM UNTIL OIX > +3                                               
110800       IF LPF-KDLPORS(OIX) > ZERO AND <= W006IX-MAX                       
110900         MOVE LPF-KDLPORS(OIX) TO KDLPORS-NUM                             
111000         MOVE TELPORS(KDLPORS-NUM)                                        
111100                         TO MOD-KDLPORS-TEXT (INDX, TIX)                  
111200         ADD +1 TO TIX                                                    
111300       END-IF                                                             
111400       ADD +1 TO OIX                                                      
111500     END-PERFORM                                                          
111600     MOVE LPF-KDLEVPLF   TO MOD-KDLEVPLF (INDX)                           
111700     MOVE LPF-BEART      TO MOD-BEART    (INDX)                           
111800     MOVE LPF-TIOMSPEC   TO MOD-TIOMSPEC (INDX)                           
111900     .                                                                    
112000                                                                          
112100                                                                          
112200*I-PREVIOUS-PAGE SECTION.                                                 
112300*    MOVE 'I-PREVIOUS-PAGE '   TO CURRENT-SECTION                         
112400*                                                                         
112500*    -- PF6 tryckt. Föregående sida skall visas. Max 10 sidor             
112600*    --             Rensa PREV-KEYS för hoppsidan                         
112700*    IF SAVE-PAGENO > +1                                                  
112800*      -- Rensa först innevarande sida i tabellen.                        
112900*      MOVE SAVE-PAGENO             TO SIDX                               
113000*      INITIALIZE SAVE-KEY-WDD601KY-PREV(SIDX)                            
113100*                                                                         
113200*      SUBTRACT +1                FROM SAVE-PAGENO                        
113300*      MOVE SAVE-PAGENO             TO SIDX                               
113400*      MOVE SAVE-KEY-WDD601KY-PREV(SIDX)                                  
113500*                                   TO W-WDD601KY-MIN-X                   
113600*    ELSE                                                                 
113700*      MOVE SAVE-KEY-WDD601KY-ENTER TO W-WDD601KY-MIN-X                   
113800*      MOVE MSGI-IDANSK             TO W-IDANSK-MIN                       
113900*                                                                         
114000*      MOVE INF-FIRST-PAGE          TO MED-IDMFSINF                       
114100*      CALL WMEDKONV             USING MED-WMEDAREA                       
114200*      MOVE MED-MFSINF              TO MOD-TEMFSFEL                       
114300*    END-IF                                                               
114400*    .                                                                    
114500                                                                          
114600                                                                          
114700 J-SWITCH-TO-2403 SECTION.                                                
114800     MOVE 'J-SWITCH-TO-2403'   TO CURRENT-SECTION                         
114900                                                                          
115000     MOVE YES                      TO KDCMDVAL-SW                         
115100                                                                          
115200     IF ((MID-KDCMDVAL(1)  = '+') AND                                     
115300         (MID-KDCMDVAL(2)  = '+') AND                                     
115400         (MID-KDCMDVAL(3)  = '+') AND                                     
115500         (MID-KDCMDVAL(4)  = '+') AND                                     
115600         (MID-KDCMDVAL(5)  = '+') AND                                     
115700         (MID-KDCMDVAL(6)  = '+') AND                                     
115800         (MID-KDCMDVAL(7)  = '+') AND                                     
115900         (MID-KDCMDVAL(8)  = '+') AND                                     
116000         (MID-KDCMDVAL(9)  = '+') AND                                     
116100         (MID-KDCMDVAL(10) = '+') AND                                     
116200         (MID-KDCMDVAL(11) = '+') AND                                     
116300         (MID-KDCMDVAL(12) = '+') AND                                     
116400         (MID-KDCMDVAL(13) = '+') AND                                     
116500         (MID-KDCMDVAL(14) = '+') AND                                     
116600         (MID-KDCMDVAL(15) = '+') )                                       
116700         MOVE NOO                  TO KDCMDVAL-SW                         
116800                                                                          
116900         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
117000         PERFORM MFS-FORM-ATTR                                            
117100         MOVE 'NO LINE SELECTED FOR JUMPING TO 2403'                      
117200                                   TO MOD-TEMFSFEL                        
117300     ELSE                                                                 
117400         MOVE '2447'               TO SAVE-IDTRANS-2403                   
117500         MOVE SEEK-IDDC            TO SAVE-IDDC-2403                      
117600         MOVE SEEK-IDLEVNR         TO SAVE-IDLEVNR-2403                   
117700                                                                          
117800         MOVE +1                   TO INDX                                
117900         PERFORM UNTIL INDX > MAX-INDX                                    
118000            IF MID-KDCMDVAL(INDX) NOT = ALL '+'                           
118100               MOVE SPACE               TO ALT-MID                        
                                                                                
                     IF ( FUNCTION TRIM (MID-IDARTNR (INDX)))                   
118300                                     NOT NUMERIC                          
118400                  MOVE MFS-ALPHA-FIELD-WRONG TO                           
118410                                 MOD-KDCMDVAL-ATTR (INDX)                 
118500                  MOVE ERR-WRONG-KEY      TO MED-IDMFSFEL                 
118600                  CALL WMEDKONV     USING MED-WMEDAREA                    
118700                  MOVE MED-MFSFEL         TO MOD-TEMFSFEL                 
118800                  MOVE NOO                TO KDCMDVAL-SW                  
118900               ELSE                                                       
119000                  MOVE MID-IDARTNR (INDX) TO ALT-IDARTNR-IN               
119100                                             SAVE-IDARTNR-D6-2403         
119200               END-IF                                                     
119300                                                                          
119400               MOVE MID-IDDC     (INDX) TO ALT-IDDC-IN                    
119500                                           SAVE-IDDC-D6-2403              
119600               MOVE MID-IDLEVNR  (INDX) TO ALT-IDLEVNR-IN                 
119700                                           SAVE-IDLEVNR-D6-2403           
119800               MOVE W-IDANSK-RAD (INDX) TO SAVE-IDANSK-D6-2403            
119900                               SAVE-IDANSK-D6-2403                        
120000               MOVE '+'                 TO ALT-KDAVROP-IN                 
120100               MOVE MAX-INDX            TO INDX                           
120200            END-IF                                                        
120300            ADD +1                      TO INDX                           
120400         END-PERFORM                                                      
120500     END-IF                                                               
120600     .                                                                    
120700                                                                          
120800                                                                          
120900 S100-KOLLA-OM-GODK-POST   SECTION.                                       
121000     MOVE 'S100-KOLLA-GODK '   TO CURRENT-SECTION                         
121100                                                                          
121200     SET GODK-POST TO TRUE                                                
121300                                                                          
121400* -- VI KOLLAR MOT K6/K7 FÖR ATT INTE FÅ PROBLEM I TEST                   
121500                                                                          
121600     MOVE LPF-IDARTNR TO W-IDARTNR                                        
121700     PERFORM IMS-GU-WDK601                                                
121800     IF SEGMENT-MISSING                                                   
121900        SET EJ-GODK-POST TO TRUE                                          
122000        MOVE SPACE       TO STATUS-WS                                     
122100     ELSE                                                                 
122200        MOVE LPF-IDDC    TO W-IDDC                                        
122300        PERFORM IMS-GU-WDK711                                             
122400        IF SEGMENT-MISSING                                                
122500           SET EJ-GODK-POST TO TRUE                                       
122600           MOVE SPACE       TO STATUS-WS                                  
122700        END-IF                                                            
122800     END-IF                                                               
122900                                                                          
123000     IF GODK-POST                                                         
123100        IF LPF-IDANSK < SEEK-IDANSK-FOM                                   
123200        OR LPF-IDANSK > SEEK-IDANSK-TOM                                   
123300                                                                          
123400           SET EJ-GODK-POST TO TRUE                                       
123500        END-IF                                                            
123600                                                                          
123700        IF SEEK-IDLEVNR NOT = SPACE                                       
123800           IF LPF-IDLEVNR NOT = SEEK-IDLEVNR                              
123900                                                                          
124000              SET EJ-GODK-POST TO TRUE                                    
124100           END-IF                                                         
124200        END-IF                                                            
124300                                                                          
124400        IF SEEK-KDLPORS = ZERO OR SPACE                                   
124500           CONTINUE                                                       
124600        ELSE                                                              
124700           IF LPF-KDLPORS(1) = SEEK-KDLPORS                               
124800           OR LPF-KDLPORS(2) = SEEK-KDLPORS                               
124900           OR LPF-KDLPORS(3) = SEEK-KDLPORS                               
125000                                                                          
125100              CONTINUE                                                    
125200           ELSE                                                           
125300              SET EJ-GODK-POST TO TRUE                                    
125400           END-IF                                                         
125500        END-IF                                                            
125600                                                                          
125700        IF SEEK-KDLEVPLF NOT = SPACE                                      
125800           IF LPF-KDLEVPLF NOT = SEEK-KDLEVPLF                            
125900                                                                          
126000              SET EJ-GODK-POST TO TRUE                                    
126100           END-IF                                                         
126200        END-IF                                                            
126300                                                                          
126400        IF SEEK-IDDC = SPACE                                              
126500        OR SEEK-IDDC (2:1) = '0'                                          
126600           CONTINUE                                                       
126700        ELSE                                                              
126800           IF LPF-IDDC NOT = SEEK-IDDC                                    
126900                                                                          
127000              SET EJ-GODK-POST TO TRUE                                    
127100           END-IF                                                         
127200        END-IF                                                            
127300     END-IF                                                               
127400     .                                                                    
127500                                                                          
127600                                                                          
127700 MFS-ERASE-FIELD-OUT SECTION.                                             
127800                                                                          
127900*    --- ALLA UTDATA-FÄLT                                                 
128000*    --- INCL. SCROLL KEYS                                                
128100     MOVE MFS-ERASE-FIELD   TO MOD-KVRADER                                
128200                               MOD-IDPAGE                                 
128300     MOVE +1                TO INDX                                       
128400     PERFORM MFS-ERASE-LINE-FIELD-OUT                                     
128500     .                                                                    
128600                                                                          
128700                                                                          
128800 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
128900                                                                          
129000*    --- OUTDATA-FIELD ON SCROLL KEYS                                     
129100     PERFORM UNTIL INDX > MAX-INDX                                        
129200       MOVE MFS-ERASE-FIELD TO MOD-KDCMDVAL(INDX)                         
129300                               MOD-IDDC    (INDX)                         
129400                               MOD-IDARTNR (INDX)                         
129500                               MOD-BEART   (INDX)                         
129600                               MOD-IDLEVNR (INDX)                         
129700                               MOD-KDLPORS-TEXT (INDX, 1)                 
129800                               MOD-KDLPORS-TEXT (INDX, 2)                 
129900                               MOD-KDLPORS-TEXT (INDX, 3)                 
130000                               MOD-KDLEVPLF (INDX)                        
130100                               MOD-TIOMSPEC (INDX)                        
130200       ADD +1               TO INDX                                       
130300     END-PERFORM                                                          
130400     .                                                                    
130500                                                                          
130600                                                                          
130700 MFS-ERASE-FIELD-IN SECTION.                                              
130800                                                                          
130900     MOVE MFS-ERASE-FIELD   TO MOD-IDANSK-FOM-IN                          
131000                               MOD-IDANSK-TOM-IN                          
131100                               MOD-IDLEVNR-IN                             
131200                               MOD-KDLPORS-IN                             
131300                               MOD-KDLEVPLF-IN                            
131400                               MOD-IDDC-IN                                
131500                                                                          
131600     MOVE +1                TO INDX                                       
131700     PERFORM UNTIL INDX > MAX-INDX                                        
131800       MOVE MFS-ERASE-FIELD TO MOD-KDCMDVAL(INDX)                         
131900       ADD +1               TO INDX                                       
132000     END-PERFORM                                                          
132100     .                                                                    
132200                                                                          
132300                                                                          
132400 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
132500                                                                          
132600*    --- ALLA UTDATA-FÄLT                                                 
132700*    --- INCL SCROLL KEYS AND LINEDATA                                    
132800     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDANSK-FOM-UT                     
132900                                    MOD-IDANSK-TOM-UT                     
133000                                    MOD-IDLEVNR-UT                        
133100                                    MOD-KDLPORS-UT                        
133200                                    MOD-KDLEVPLF-UT                       
133300                                    MOD-KVRADER                           
133400                                    MOD-IDPAGE                            
133500                                    MOD-IDDC-UT                           
133600                                                                          
133700     MOVE +1                     TO INDX                                  
133800     PERFORM UNTIL INDX > MAX-INDX                                        
133900       PERFORM MFS-DONT-TOUCH-LINE-FIELD-OUT                              
134000       ADD +1                    TO INDX                                  
134100     END-PERFORM                                                          
134200     .                                                                    
134300                                                                          
134400                                                                          
134500 MFS-DONT-TOUCH-LINE-FIELD-OUT  SECTION.                                  
134600                                                                          
134700*    --- OUTDATA FIELD ON SCROLL KEYS                                     
134800     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDARTNR(INDX)                     
134900                                    MOD-BEART(INDX)                       
135000                                    MOD-IDLEVNR(INDX)                     
135100                                    MOD-KDLPORS-TEXT(INDX, 1)             
135200                                    MOD-KDLPORS-TEXT(INDX, 2)             
135300                                    MOD-KDLPORS-TEXT(INDX, 3)             
135400                                    MOD-KDLEVPLF(INDX)                    
135500                                    MOD-TIOMSPEC (INDX)                   
135600                                    MOD-IDDC (INDX)                       
135700     .                                                                    
135800                                                                          
135900                                                                          
136000 MFS-FORM-ATTR SECTION.                                                   
136100                                                                          
136200*    --- ALL INDATA-FIELDS                                                
136300     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-IDANSK-FOM-IN-ATTR               
136400                                     MOD-IDANSK-TOM-IN-ATTR               
136500                                     MOD-IDLEVNR-IN-ATTR                  
136600                                     MOD-KDLPORS-IN-ATTR                  
136700                                     MOD-KDLEVPLF-IN-ATTR                 
136800                                     MOD-IDDC-IN-ATTR                     
136900     MOVE +1                      TO INDX                                 
137000     PERFORM UNTIL INDX > MAX-INDX                                        
137100       MOVE MFS-FORMAT-DEFAULT-ATTR                                       
137200                                  TO MOD-KDCMDVAL-ATTR(INDX)              
137300       ADD +1                     TO INDX                                 
137400     END-PERFORM                                                          
137500     .                                                                    
137600                                                                          
137700                                                                          
137800                                                                          
137900* --- IMS SECTIONS ---                                                    
138000                                                                          
138100 IMS-GET-MSG SECTION.                                                     
138200                                                                          
138300     MOVE '  QC' TO GOOD-STATUSCODES                                      
138400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
138500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
138600     PERFORM IMS-STATUSCHECK                                              
138700     .                                                                    
138800                                                                          
138900                                                                          
139000 IMS-INSERT-MSG SECTION.                                                  
139100                                                                          
139200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
139300     MOVE SPACE TO GOOD-STATUSCODES                                       
139400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
139500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
139600     PERFORM IMS-STATUSCHECK                                              
139700     .                                                                    
139800                                                                          
139900                                                                          
140000 IMS-INSERT-ALT-MSG Section.                                              
140100                                                                          
140200     IF ENGLISH-TEXT                                                      
140300       MOVE '2' TO P-KDMFSFOR                                             
140400     END-IF                                                               
140500     MOVE SPACE TO GOOD-STATUSCODES                                       
140600     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
140700     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
140800     PERFORM IMS-STATUSCHECK                                              
140900     .                                                                    
141000                                                                          
141100                                                                          
141200 IMS-GU-WDD601 SECTION.                                                   
141300     MOVE 'IMS-GU-WDD601   '   TO CURRENT-IMS-SECTION                     
141400                                                                          
141500     MOVE SPACE            TO ALL-SSA                                     
141600     STRING 'WDD601  (WDD601KY>=' W-WDD601KY-MIN-X                        
141700                    '&WDD601KY<=' W-WDD601KY-MAX-X ')'                    
141800                 DELIMITED BY SIZE INTO SSA1                              
141900     MOVE '  GE'           TO GOOD-STATUSCODES                            
142000     CALL CBLTDLI       USING GU WDD6-PCB DLI-IO-WDD601 SSA1              
142100     MOVE WDD6-STATUS-CODE TO STATUS-WS                                   
142200     PERFORM IMS-STATUSCHECK                                              
142300     .                                                                    
142400                                                                          
142500                                                                          
142600 IMS-GN-WDD601        SECTION.                                            
142700     MOVE 'IMS-GN-WDD601   '   TO CURRENT-IMS-SECTION                     
142800                                                                          
142900     MOVE SPACE            TO ALL-SSA                                     
143000     STRING 'WDD601  (WDD601KY>=' W-WDD601KY-MIN-X                        
143100                    '&WDD601KY<=' W-WDD601KY-MAX-X ')'                    
143200     DELIMITED BY SIZE   INTO SSA1                                        
143300     MOVE '  GEGB'         TO GOOD-STATUSCODES                            
143400     CALL CBLTDLI USING GN WDD6-PCB DLI-IO-WDD601 SSA1                    
143500     MOVE WDD6-STATUS-CODE TO STATUS-WS                                   
143600     PERFORM IMS-STATUSCHECK                                              
143700     .                                                                    
143800                                                                          
143900                                                                          
144000 IMS-GU-WDB601-MIN-MAX SECTION.                                           
144100     MOVE 'IMS-GU-WDB601-MIN-MAX'  TO CURRENT-IMS-SECTION                 
144200                                                                          
144300     MOVE SPACE             TO ALL-SSA                                    
144400     STRING 'WDB601  (WDB6ASEQ>=' W-IDDC-B6-MIN-X                         
144500                    '&WDB6ASEQ<=' W-IDDC-B6-MAX-X ')'                     
144600       DELIMITED BY SIZE  INTO SSA1                                       
144700     MOVE '  GEGB'          TO GOOD-STATUSCODES                           
144800     CALL CBLTDLI USING GU WDB6A-PCB DLI-IO-WDB601 SSA1                   
144900     MOVE WDB6A-STATUS-CODE TO STATUS-WS                                  
145000     PERFORM IMS-STATUSCHECK                                              
145100     .                                                                    
145200                                                                          
145300 IMS-GN-WDB601-MIN-MAX SECTION.                                           
145400     MOVE 'IMS-GN-WDB601-MIN-MAX'  TO CURRENT-IMS-SECTION                 
145500                                                                          
145600     MOVE SPACE             TO ALL-SSA                                    
145700     STRING 'WDB601  (WDB6ASEQ>=' W-IDDC-B6-MIN-X                         
145800                    '&WDB6ASEQ<=' W-IDDC-B6-MAX-X ')'                     
145900       DELIMITED BY SIZE  INTO SSA1                                       
146000     MOVE '  GEGB'          TO GOOD-STATUSCODES                           
146100     CALL CBLTDLI USING GN WDB6A-PCB DLI-IO-WDB601 SSA1                   
146200     MOVE WDB6A-STATUS-CODE TO STATUS-WS                                  
146300     PERFORM IMS-STATUSCHECK                                              
146400     .                                                                    
146500                                                                          
146600 IMS-GU-WDK601 SECTION.                                                   
146700     MOVE 'IMS-GU-WDK601   '  TO CURRENT-IMS-SECTION                      
146800                                                                          
146900     MOVE SPACE               TO ALL-SSA                                  
147000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
147100          DELIMITED BY SIZE INTO SSA1                                     
147200     MOVE '  GE'              TO GOOD-STATUSCODES                         
147300     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
147400     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
147500     PERFORM IMS-STATUSCHECK                                              
147600     .                                                                    
147700                                                                          
147800                                                                          
147900 IMS-GU-WDK711 SECTION.                                                   
148000     MOVE 'IMS-GU-WDK711   '  TO CURRENT-IMS-SECTION                      
148100                                                                          
148200     MOVE SPACE               TO ALL-SSA                                  
148300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
148400          DELIMITED BY SIZE INTO SSA1                                     
148500     STRING 'WDK711  (IDDC     =' W-IDDC ')'                              
148600          DELIMITED BY SIZE INTO SSA2                                     
148700     MOVE '  GE'              TO GOOD-STATUSCODES                         
148800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
148900     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
149000     PERFORM IMS-STATUSCHECK                                              
149100     .                                                                    
149200                                                                          
149300 IMS-STATUSCHECK SECTION.                                                 
149400                                                                          
149500     SET STATUS-IX TO 1                                                   
149600     SEARCH GOOD-STATUS                                                   
149700       AT END                                                             
149800         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
149900         DELIMITED BY SIZE INTO ERROR-TEXT                                
150000         CALL FELLOG                                                      
150100       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
150200         CONTINUE                                                         
150300     END-SEARCH                                                           
150400     .                                                                    
