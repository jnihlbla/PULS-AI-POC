000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2042500.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   12/08/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        SCREEN SHOWING THE BACK ORDERS ON A PART NUMBER PER DC.          
000900*        THE SCREEN ONLY SHOWS THE BACKORDERS FOR THE CHINA DC'S          
001000*        (71, 72, 73).                                                    
001100*                                                                         
001200*        THE PROGRAM READS     WDA5                                       
001300*        THE PROGRAM READS     WDA5A                                      
001400*        THE PROGRAM READS     WDK7                                       
001500*        THE PROGRAM READS     WDK6                                       
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSACTION: W2T425                                              
001900*        MID:         W2I42501                                            
002000*                                                                         
002100*    OUTDATA.                                                             
002200*        MOD:         W2O425N1                                            
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600                                                                          
002700 DATA DIVISION.                                                           
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
003000 77  IDPGM                       PIC X(08)   VALUE 'W2042500'.            
003100                                                                          
003200*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003300 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003400                                                                          
003500 77  YES                         PIC X       VALUE 'J'.                   
003600 77  NOO                         PIC X       VALUE 'N'.                   
003700                                                                          
003800*    --- INDEX FOR SCROLL LINES                                           
003900 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004000 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
004100*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004200                                                                          
004300                                                                          
004400 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004500     88  KEYS-OK                             VALUE 'J'.                   
004600     88  KEYS-WRONG                          VALUE 'N'.                   
004700                                                                          
004720 77  DC-SW                       PIC X       VALUE 'J'.                   
004730     88  DC-OK                               VALUE 'J'.                   
004740     88  DC-WRONG                            VALUE 'N'.                   
004750                                                                          
004800 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004900     88  INDATA-OK                           VALUE 'J'.                   
005000     88  INDATA-FEL                          VALUE 'N'.                   
005100                                                                          
005200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005300     88  OWN-MID                             VALUE '2425'.                
005400     88  GOOD-MID                            VALUE '2421' '2422'          
005500                                                   '2423' '2424'          
005600                                                   '2425' '2426'          
005700                                                   '2427' '2428'          
005800                                                   '2429'.                
005900     88  HELP-MID                            VALUE '0551'.                
006000     EJECT                                                                
006100 01  WS-VARIABLES.                                                        
006200     03 WS-IDLEVNR-8             PIC X(8)    VALUE SPACE.                 
006600     03 WS-TIRODAT               PIC 9(8)    VALUE ZERO.                  
006800     03 WS-IDARTNR-KEY           PIC X(9)    VALUE SPACE.                 
006900     03 WS-DASH                  PIC X(1)    VALUE '-'.                   
007000     03 WS-TIRES                 PIC 9(6)    VALUE ZERO.                  
007010     03 WS-NDC-40                PIC 9(2)    VALUE 40.                    
007011     03 WS-NDC-41                PIC 9(2)    VALUE 41.                    
007020     03 WS-NDC-49                PIC 9(2)    VALUE 49.                    
007030     03 WS-NDC-70                PIC 9(2)    VALUE 70.                    
007031     03 WS-NDC-71                PIC 9(2)    VALUE 71.                    
007040     03 WS-NDC-79                PIC 9(2)    VALUE 79.                    
007100                                                                          
007200     EJECT                                                                
007210 01  FILLER                      PIC X(7)    VALUE 'WWIDFTG'.             
007220*01 -COPY WWIDFTG                                                         
007230     EJECT                                                                
007300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
007400 01  GENERAL-SUBPROGRAMS.                                                 
007500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007900     EJECT                                                                
008000*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
008100*01 -COPY WMEDAREA                                                        
008200     SKIP3                                                                
008300 01  MESSAGE-CODES.                                                       
008400     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008500     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008600     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
008700                                                                          
008800     03  ERR-KEY-MISSING         PIC X(3)    VALUE '005'.                 
008810     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
008900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009000     03  ERR-BACKORDER-MISSING   PIC X(3)    VALUE '403'.                 
009100     03  ERR-USER-NOT-AUTH       PIC X(3)    VALUE '405'.                 
009110     03  ERR-WRONG-DC            PIC X(3)    VALUE '440'.                 
009200     EJECT                                                                
009300*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
009400*                                                                         
009500 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
009600     SKIP3                                                                
009700*01 -COPY WMSGINIT                                                        
009800     EJECT                                                                
010300*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
010400*                                                                         
010500 01  SAVE-AREA.                                                           
010600     03  SAVE-IDTRANS               PIC X(4)    VALUE '2425'.             
010700* PF8 KEYS                                                                
010800     03  SAVE-PF8-KEYS-AREA.                                              
010900         05  SAVE-IDARTNR-NEXT      PIC 9(9).                             
010901         05  SAVE-IDDC-NEXT         PIC X(2).                             
010910         05  SAVE-KDRAPRIO-NEXT     PIC 9(3).                             
011000         05  SAVE-TIRODAT-NEXT      PIC 9(6).                             
011010         05  SAVE-TIREGTID-NEXT     PIC 9(6).                             
011100         05  SAVE-IDDISTR-NEXT      PIC 9(4).                             
011200         05  SAVE-IDKUNDNR-NEXT     PIC 9(7).                             
011300         05  SAVE-IDKUNDRF-NEXT     PIC X(10).                            
011400         05  SAVE-IDLOPNR-NEXT      PIC 9(3).                             
011500* ENTER KEYS                                                              
011600     03  SAVE-ENTER-KEYS-AREA.                                            
011601         05  SAVE-IDARTNR-ENTER     PIC 9(9).                             
011610         05  SAVE-IDDC-ENTER        PIC X(2).                             
011700         05  SAVE-KDRAPRIO-ENTER    PIC 9(3).                             
011800         05  SAVE-TIRODAT-ENTER     PIC 9(6).                             
011810         05  SAVE-TIREGTID-ENTER    PIC 9(6).                             
011900         05  SAVE-IDDISTR-ENTER     PIC 9(4).                             
012000         05  SAVE-IDKUNDNR-ENTER    PIC 9(7).                             
012100         05  SAVE-IDKUNDRF-ENTER    PIC X(10).                            
012200         05  SAVE-IDLOPNR-ENTER     PIC 9(3).                             
012300     EJECT                                                                
012400*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
012500*                                                                         
012600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012700     SKIP3                                                                
012800*01  MID -COPY W2I42501                                                   
012900     EJECT                                                                
013000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
013100     SKIP3                                                                
013200*01  -COPY WMSGAREA                                                       
013300     EJECT                                                                
013400     03  MOD REDEFINES MSG-AREA.                                          
013500*      05  -COPY W2O42501                                                 
013600     EJECT                                                                
013700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013800     SKIP3                                                                
013900*01  -COPY WMFSAREA                                                       
014000     EJECT                                                                
014100*    --- WORK-AREAS FOR IMS-SECTIONS                                      
014200*                                                                         
014300 01  FILLER                       PIC X(16)   VALUE 'IMS-WS'.             
014400     SKIP3                                                                
014500 01  KEYS-FOR-DLI.                                                        
014600*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
014700     03  W-KDSTARAD-MIN           PIC  X(1)         VALUE '2'.            
014800     03  W-KDSTARAD-MAX           PIC  X(1)         VALUE '3'.            
014900     EJECT                                                                
015000     03  W-WDA5A1KY-MIN.                                                  
015100         05  W-IDARTNR-N3-MIN     PIC S9(9) COMP-3   VALUE ZERO.          
015200         05  W-IDDC-N3-MIN.                                               
015210             07 FILLER            PIC X(01)          VALUE SPACE.         
015220             07 W-IDDC2-N3-MIN    PIC X(01)         VALUE SPACE.          
015300         05  W-KDRAPRIO-N3-MIN    PIC S9(3) COMP-3   VALUE ZERO.          
015400         05  W-DARODAT-N3-MIN     PIC  9(8)          VALUE ZERO.          
015410         05  W-TIREGTID-N3-MIN    PIC S9(7) COMP-3   VALUE ZERO.          
015500         05  W-IDDISTR-N3-MIN     PIC S9(5) COMP-3   VALUE ZERO.          
015600         05  W-IDKUNDNR-N3-MIN    PIC S9(7) COMP-3   VALUE ZERO.          
015700         05  W-IDKUNDRF-N3-MIN.                                           
015800             07  W-IDORDNR-N3-MIN PIC 9(5)           VALUE ZERO.          
015900             07  FILLER           PIC X(5)           VALUE SPACE.         
016000         05  W-IDLOPNR-N3-MIN     PIC S9(3) COMP-3   VALUE ZERO.          
016100                                                                          
016200     03  W-WDA5A1KY-MAX.                                                  
016300         05  W-IDARTNR-N3-MAX     PIC S9(9) COMP-3   VALUE ZERO.          
016400         05  W-IDDC-N3-MAX.                                               
016410             07 FILLER            PIC X(01)          VALUE SPACE.         
016420             07 W-IDDC2-N3-MAX    PIC X(01)          VALUE SPACE.         
016500         05  W-KDRAPRIO-N3-MAX    PIC S9(3) COMP-3   VALUE ZERO.          
016600         05  W-DARODAT-N3-MAX     PIC  9(8)          VALUE ZERO.          
016610         05  W-TIREGTID-N3-MAX    PIC S9(7) COMP-3   VALUE ZERO.          
016700         05  W-IDDISTR-N3-MAX     PIC S9(5) COMP-3   VALUE ZERO.          
016800         05  W-IDKUNDNR-N3-MAX    PIC S9(7) COMP-3   VALUE ZERO.          
016900         05  W-IDKUNDRF-N3-MAX.                                           
017000             07  W-IDORDNR-N3-MAX PIC 9(5)           VALUE ZERO.          
017100             07  FILLER           PIC X(5)           VALUE SPACE.         
017200         05  W-IDLOPNR-N3-MAX     PIC S9(3) COMP-3   VALUE ZERO.          
017300                                                                          
017400     03  W-WDA501KY.                                                      
017500         05  W-IDDISTR-N2         PIC S9(5) COMP-3   VALUE ZERO.          
017600         05  W-IDKUNDNR-N2        PIC S9(7) COMP-3   VALUE ZERO.          
017700         05  W-IDKUNDRF-N2.                                               
017800             07  W-IDORDNR-N2     PIC 9(5)           VALUE ZERO.          
017900             07  FILLER           PIC X(5)           VALUE SPACE.         
018000         05  W-IDARTNR-N2         PIC S9(9) COMP-3   VALUE ZERO.          
018100         05  W-IDLOPNR-N2         PIC S9(3) COMP-3   VALUE ZERO.          
018200                                                                          
018300     03  W-IDDC-RO-X.                                                     
018400         05  W-IDDC-RO            PIC X(2)           VALUE SPACE.         
018401                                                                          
018402     03  W-IDDC-X.                                                        
018403         05  W-IDDC               PIC X(2)           VALUE SPACE.         
018410     03  W-IDDC-MIN-X.                                                    
018420         05  FILLER               PIC X(1)           VALUE SPACE.         
018430         05  W-IDDC2-MIN          PIC X(1)           VALUE SPACE.         
018440     03  W-IDDC-MAX-X.                                                    
018450         05  FILLER               PIC X(1)           VALUE SPACE.         
018460         05  W-IDDC2-MAX          PIC X(1)           VALUE SPACE.         
018470                                                                          
018500     03  W-IDARTNR-X.                                                     
018600         05  W-IDARTNR            PIC S9(9)  COMP-3  VALUE ZERO.          
018700     SKIP2                                                                
018800*    --- STATUS CODES FROM IMS                                            
018900 01  STATUS-WS                   PIC XX.                                  
019000     88  SEGMENT-FOUND                       VALUE '  '.                  
019100     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
019200     88  SEGMENT-MISSING                     VALUE 'GE'.                  
019300     88  SEGMENT-END                         VALUE 'GB'.                  
019400     SKIP2                                                                
019500 01  GOOD-STATUSCODES.                                                    
019600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019700     SKIP3                                                                
019800 01  ALL-SSA.                                                             
019810     03 SSA1                     PIC X(320).                              
019900     03 SSA2                     PIC X(320).                              
020000     EJECT                                                                
020100*    --- IMS FUNCTION CODES                                               
020200*01  -COPY W0003                                                          
020300     EJECT                                                                
020400*    ---  DLI INPUT-OUTPUT AREA                                           
020500                                                                          
020600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA501'.                      
020700 01  DLI-IO-WDA501.                                                       
020800*    03  -COPY WDA501                                                     
020900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA5A1'.                      
021000 01  DLI-IO-WDA5A1.                                                       
021100*    03  -COPY WDA5A1                                                     
021200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
021300 01  DLI-IO-WDK711.                                                       
021400*    03  -COPY WDK711                                                     
021500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
021600 01  DLI-IO-WDK601.                                                       
021700*    03  -COPY WDK601                                                     
021710 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
021720 01  DLI-IO-WDB601.                                                       
021730*    03  -COPY WDB601                                                     
021800     EJECT                                                                
021900 LINKAGE SECTION.                                                         
022000*01  -COPY W0009   -PRE MSG-                                              
022100*01  -COPY W0008   -PRE WDP7-                                             
022200     05  FILLER                  PIC X.                                   
022300                                                                          
022400*01  -COPY W0008  -PRE WDA5-                                              
022500     05  FILLER                  PIC X.                                   
022600                                                                          
022700*01  -COPY W0008  -PRE WDA5A-                                             
022800     05  FILLER                  PIC X.                                   
022900                                                                          
023000*01  -COPY W0008  -PRE WDK7-                                              
023100     05  FILLER                  PIC X.                                   
023200                                                                          
023300*01  -COPY W0008  -PRE WDK6-                                              
023400     05  FILLER                  PIC X.                                   
023401                                                                          
023410*01  -COPY W0008  -PRE WDB6-                                              
023420     05  FILLER                  PIC X.                                   
023500     EJECT                                                                
023600 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDA5-PCB WDA5A-PCB            
023700                           WDK7-PCB WDK6-PCB WDB6-PCB.                    
023800 MAIN SECTION.                                                            
023900     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDA5-PCB WDA5A-PCB            
024000                           WDK7-PCB WDK6-PCB WDB6-PCB.                    
024100                                                                          
024200     PERFORM IMS-GET-MSG                                                  
024300     IF SEGMENT-FOUND                                                     
024400       PERFORM A-INIT                                                     
024500       PERFORM B-CHECK-KEYS                                               
024600       IF KEYS-OK AND INDATA-OK                                           
024700          IF MFS-FIRST                                                    
024800             PERFORM C-FIRST-PAGE                                         
024900          ELSE                                                            
025000             IF MFS-NEXT                                                  
025100               PERFORM D-NEXT-PAGE                                        
025200             ELSE                                                         
025300               PERFORM E-SAME-PAGE                                        
025400             END-IF                                                       
025500          END-IF                                                          
025600          PERFORM F-READ-SHOW-INFO                                        
025700       END-IF                                                             
025800*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
026000       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O42501 + 4                      
026100       PERFORM IMS-INSERT-MSG                                             
026200     END-IF                                                               
026300                                                                          
026400     MOVE ZERO TO RETURN-CODE                                             
026500     GOBACK                                                               
026600     .                                                                    
026700     EJECT                                                                
026800 A-INIT SECTION.                                                          
026900                                                                          
027000     IF MSG-DOUBLE-TRANSACTIONS                                           
027100       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W2I42501-CTX             
027200       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
027300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
027400     ELSE                                                                 
027500       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W2I42501-CTX              
027600       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
027700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
027800     END-IF                                                               
027900                                                                          
028000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
028100     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
028200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
028300                                                                          
028400     MOVE LOW-VALUE TO MSG-AREA                                           
028500     MOVE 'W2O425N1' TO MFS-IDMOD                                         
028600     MOVE '2425' TO MOD-IDTRANS                                           
028700     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
028800                                                                          
028900     IF OWN-MID OR HELP-MID                                               
029000       CONTINUE                                                           
029100     ELSE                                                                 
029200       MOVE SPACE TO MFS-KDTRTYP                                          
029300       MOVE '7' TO MFS-IDPFK                                              
029400     END-IF                                                               
029500     .                                                                    
029600     EJECT                                                                
029700 B-CHECK-KEYS SECTION.                                                    
029800                                                                          
029900     MOVE ALL '+'             TO MSGI-WMSGINIT                            
030000     MOVE '001'               TO MSGI-KDCALL                              
030100     MOVE MSG-LTERM-NAME      TO MSGI-IDLTERM-USER                        
030200     MOVE MSG-SIGNON-USERID   TO MSGI-IDUSER                              
030300     MOVE '2425'              TO MSGI-IDTRANS                             
030400                                                                          
030500     IF GOOD-MID                                                          
030600        MOVE MID-IDARTNR-IN   TO MSGI-IDARTNR                             
030700        MOVE MID-IDDC-IN      TO MSGI-IDDC-KEY                            
030800     END-IF                                                               
030900     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
031100     MOVE MSGI-SPAR-AREA      TO SAVE-AREA                                
031200                                                                          
031300*    - LANGUAGE TO BE USED BY MEDKONV                                     
031400     MOVE MSGI-IDLAND-SPR     TO MED-IDSKYLT                              
031500                                                                          
031600     MOVE YES                 TO KEYS-SW                                  
031700                                                                          
031800     MOVE LOW-VALUE            TO W-WDA5A1KY-MIN                          
031900     MOVE HIGH-VALUE           TO W-WDA5A1KY-MAX                          
032000                                                                          
033800*    -- CHECK OF IDARTNR                                                  
033900     MOVE MFS-ERASE-FIELD     TO MOD-IDARTNR-IN                           
034000                                                                          
035220     IF MID-IDARTNR-IN NOT = ALL '+'                                      
035290       MOVE '7'               TO MFS-IDPFK                                
035291       MOVE SPACE             TO MFS-KDTRTYP                              
035293     END-IF                                                               
035294                                                                          
035295     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
035296     IF MSGI-IDARTNR NUMERIC                                              
035297        MOVE MSGI-IDARTNR     TO W-IDARTNR                                
035298                                 W-IDARTNR-N3-MIN                         
035299                                 W-IDARTNR-N3-MAX                         
035300     ELSE                                                                 
035310        MOVE NOO              TO KEYS-SW                                  
035400     END-IF                                                               
035900                                                                          
036000* GET THE CONTROL DIGIT TO THE PART NUMBER                                
036001     IF KEYS-OK                                                           
036010        PERFORM BB-GET-CNTRL-DIGIT                                        
036011     END-IF                                                               
036020                                                                          
036030*    -- CHECK OF IDDC                                                     
036040     MOVE MFS-ERASE-FIELD     TO MOD-IDDC-IN                              
036050                                                                          
036060     IF MID-IDDC-IN NOT = ALL '+'                                         
036070       MOVE '7'               TO MFS-IDPFK                                
036080       MOVE SPACE             TO MFS-KDTRTYP                              
036090     END-IF                                                               
036091                                                                          
036092     MOVE MSGI-IDDC-KEY       TO W-IDDC                                   
036095     IF W-IDDC = SPACE                                                    
036096        MOVE MSGI-IDFTG        TO WS-IDFTG                                
036097        IF IDFTG-US                                                       
036098           MOVE WS-NDC-41      TO W-IDDC-N3-MIN                           
036099           MOVE WS-NDC-49      TO W-IDDC-N3-MAX                           
036100           MOVE WS-NDC-40      TO MSGI-IDDC-KEY                           
036101                                  W-IDDC                                  
036104        ELSE                                                              
036105           IF IDFTG-CN                                                    
036106              MOVE WS-NDC-71   TO W-IDDC-N3-MIN                           
036107              MOVE WS-NDC-79   TO W-IDDC-N3-MAX                           
036108              MOVE WS-NDC-70   TO MSGI-IDDC-KEY                           
036109                                  W-IDDC                                  
036113           ELSE                                                           
036114              MOVE NOO         TO KEYS-SW                                 
036115           END-IF                                                         
036116        END-IF                                                            
036118     ELSE                                                                 
036119        MOVE W-IDDC            TO W-IDDC-N3-MIN                           
036120                                  W-IDDC-N3-MAX                           
036121        IF W-IDDC(2:1) = '0'                                              
036122           MOVE '1'            TO W-IDDC2-N3-MIN                          
036123           MOVE '9'            TO W-IDDC2-N3-MAX                          
036124        END-IF                                                            
036126     END-IF                                                               
036130                                                                          
036200     PERFORM BA-CHECK-IDDC                                                
036300                                                                          
036600     IF GOOD-MID OR KEYS-OK                                               
036800       MOVE MSGI-IDARTNR       TO MOD-IDARTNR-UT                          
036801       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
036802       MOVE MSGI-IDDC-KEY      TO MOD-IDDC-UT                             
037100     ELSE                                                                 
037200       MOVE MFS-ERASE-FIELD    TO MOD-IDDC-UT                             
037300                                  MOD-IDARTNR-UT                          
037400     END-IF                                                               
037500                                                                          
037520     IF DC-WRONG                                                          
037530       MOVE ERR-WRONG-DC       TO MED-IDMFSFEL                            
037540       CALL WMEDKONV USING MED-WMEDAREA                                   
037550       MOVE MED-MFSFEL        TO MOD-TEMFSFEL                             
037560       PERFORM MFS-ERASE-FIELD-IN                                         
037570       PERFORM MFS-ERASE-FIELD-OUT                                        
037580     END-IF                                                               
037590                                                                          
037600     IF KEYS-WRONG                                                        
037700       MOVE ERR-WRONG-KEY      TO MED-IDMFSFEL                            
037800       CALL WMEDKONV USING MED-WMEDAREA                                   
037900       MOVE MED-MFSFEL        TO MOD-TEMFSFEL                             
038000       PERFORM MFS-ERASE-FIELD-IN                                         
038100       PERFORM MFS-ERASE-FIELD-OUT                                        
038200     END-IF                                                               
038500                                                                          
038600     IF KEYS-WRONG AND WS-IDARTNR-KEY = ZEROS                             
038700       MOVE ERR-KEY-MISSING   TO MED-IDMFSFEL                             
038800       CALL WMEDKONV USING MED-WMEDAREA                                   
038900       MOVE MED-MFSFEL        TO MOD-TEMFSFEL                             
039000       MOVE MFS-ERASE-FIELD   TO MOD-IDARTNR                              
039100       PERFORM MFS-ERASE-FIELD-IN                                         
039200       PERFORM MFS-ERASE-FIELD-OUT                                        
039300     END-IF                                                               
039500     .                                                                    
039600     EJECT                                                                
039698 BA-CHECK-IDDC SECTION.                                                   
039705                                                                          
039710     IF W-IDDC = SPACE                                                    
039711        CONTINUE                                                          
039712     ELSE                                                                 
039714        MOVE W-IDDC            TO W-IDDC-MIN-X                            
039715                                  W-IDDC-MAX-X                            
039716        IF W-IDDC (2:1) = '0'                                             
039717           MOVE '1'            TO W-IDDC2-MIN                             
039718           MOVE '9'            TO W-IDDC2-MAX                             
039719        END-IF                                                            
039721                                                                          
039722        PERFORM IMS-GU-WDB601-MIN-MAX                                     
039723        IF SEGMENT-FOUND                                                  
039724           IF DCS-NDC-CN                                                  
039725           OR (DCS-NDC-NA AND DCS-USA)                                    
039726              MOVE YES         TO DC-SW                                   
039727           ELSE                                                           
039728              MOVE NOO         TO DC-SW                                   
039729                                  INDATA-SW                               
039730           END-IF                                                         
039731        ELSE                                                              
039732           MOVE NOO            TO DC-SW                                   
039733                                  INDATA-SW                               
039734        END-IF                                                            
039735     END-IF                                                               
039736     .                                                                    
039737     EJECT                                                                
039738 BB-GET-CNTRL-DIGIT SECTION.                                              
039740     PERFORM IMS-GU-WDK601                                                
039741     IF SEGMENT-FOUND                                                     
039742        MOVE ART-REKSIFFR      TO MOD-REKSIFFR                            
039743        MOVE WS-DASH           TO MOD-DASH-1                              
039744     ELSE                                                                 
039745        MOVE NOO               TO INDATA-SW                               
039746        MOVE ERR-PART-MISSING  TO MED-IDMFSFEL                            
039747        CALL WMEDKONV USING MED-WMEDAREA                                  
039748        MOVE MED-MFSFEL        TO MOD-TEMFSFEL                            
039749        PERFORM MFS-ERASE-FIELD-IN                                        
039750        PERFORM MFS-ERASE-FIELD-OUT                                       
039751     END-IF                                                               
039752     .                                                                    
039753     EJECT                                                                
039760 C-FIRST-PAGE SECTION.                                                    
039800                                                                          
039900     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
040000     CALL WMEDKONV    USING MED-WMEDAREA                                  
040100     MOVE MED-MFSINF     TO MOD-TEMFSINF                                  
040200                                                                          
040300     PERFORM MFS-ERASE-FIELD-IN                                           
040520     .                                                                    
040600     EJECT                                                                
040700 D-NEXT-PAGE SECTION.                                                     
040800                                                                          
040900     IF SAVE-IDTRANS = '2425'                                             
040960                                                                          
041100       MOVE SAVE-IDARTNR-NEXT   TO W-IDARTNR-N3-MIN                       
041102       MOVE SAVE-IDDC-NEXT      TO W-IDDC-N3-MIN                          
041103       MOVE SAVE-KDRAPRIO-NEXT  TO W-KDRAPRIO-N3-MIN                      
041400       MOVE SAVE-TIRODAT-NEXT   TO WS-TIRODAT                             
041500       PERFORM S02-FORMAT-DATE                                            
041510       MOVE SAVE-IDDISTR-NEXT   TO W-IDDISTR-N3-MIN                       
041520       MOVE SAVE-IDKUNDNR-NEXT  TO W-IDKUNDNR-N3-MIN                      
041530       MOVE SAVE-IDKUNDRF-NEXT  TO W-IDKUNDRF-N3-MIN                      
041800       MOVE SAVE-IDLOPNR-NEXT   TO W-IDLOPNR-N3-MIN                       
041910     ELSE                                                                 
042000       PERFORM MFS-ERASE-FIELD-IN                                         
042100     END-IF                                                               
042200     .                                                                    
042300     EJECT                                                                
042400 E-SAME-PAGE SECTION.                                                     
042500                                                                          
042600     IF SAVE-IDTRANS = '2425' OR '0551'                                   
042640                                                                          
042800        MOVE SAVE-IDARTNR-ENTER     TO W-IDARTNR-N3-MIN                   
042801        MOVE SAVE-IDDC-ENTER        TO W-IDDC-N3-MIN                      
042802        MOVE SAVE-KDRAPRIO-ENTER    TO W-KDRAPRIO-N3-MIN                  
043200        MOVE SAVE-TIRODAT-ENTER     TO WS-TIRODAT                         
043300        PERFORM S02-FORMAT-DATE                                           
043400        MOVE SAVE-IDDISTR-ENTER     TO W-IDDISTR-N3-MIN                   
043410        MOVE SAVE-IDKUNDNR-ENTER    TO W-IDKUNDNR-N3-MIN                  
043420        MOVE SAVE-IDKUNDRF-ENTER    TO W-IDKUNDRF-N3-MIN                  
043430        MOVE SAVE-IDLOPNR-ENTER     TO W-IDLOPNR-N3-MIN                   
043500                                                                          
043600        IF MID-IDARTNR-IN = ALL '+' AND MID-IDDC-IN = ALL '+'             
043700           PERFORM MFS-ERASE-FIELD-IN                                     
043800        END-IF                                                            
043900     ELSE                                                                 
044000        PERFORM MFS-ERASE-FIELD-IN                                        
044100     END-IF                                                               
044200     .                                                                    
044300     EJECT                                                                
044400 F-READ-SHOW-INFO SECTION.                                                
044500                                                                          
044600     PERFORM FA-READ-BASICDATA                                            
044700                                                                          
044701     IF MOD-TEMFSFEL = SPACES                                             
044710      IF MOD-IDDC-RO-LINE(1) = ALL '+' OR SPACES OR LOW-VALUES            
044711        MOVE NOO                TO INDATA-SW                              
044712        MOVE ERR-USER-NOT-AUTH  TO MED-IDMFSFEL                           
044713        CALL WMEDKONV USING MED-WMEDAREA                                  
044714        MOVE MED-MFSFEL         TO MOD-TEMFSFEL                           
044715        MOVE SPACES             TO MOD-TEMFSINF                           
044716        PERFORM MFS-ERASE-FIELD-IN                                        
044717        PERFORM MFS-ERASE-FIELD-OUT                                       
044720      END-IF                                                              
044730     END-IF                                                               
044800*                                                                         
044900     MOVE '002'                TO MSGI-KDCALL                             
045000     MOVE '2425'               TO SAVE-IDTRANS                            
045100     MOVE SAVE-AREA            TO MSGI-SPAR-AREA                          
045200     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
045300                                                                          
045400     .                                                                    
045500     EJECT                                                                
045600 FA-READ-BASICDATA SECTION.                                               
045612                                                                          
045617     PERFORM IMS-GU-WDA5A1                                                
047400                                                                          
047500     IF SEGMENT-MISSING                                                   
047502       MOVE SPACE              TO SAVE-PF8-KEYS-AREA                      
047503                                  SAVE-ENTER-KEYS-AREA                    
047510       MOVE W-IDARTNR          TO SAVE-IDARTNR-ENTER                      
047520                                  SAVE-IDARTNR-NEXT                       
047530       MOVE W-IDDC             TO SAVE-IDDC-ENTER                         
047540                                  SAVE-IDDC-NEXT                          
047550                                                                          
047600       MOVE ERR-BACKORDER-MISSING                                         
047700                               TO MED-IDMFSFEL                            
047800       CALL WMEDKONV        USING MED-WMEDAREA                            
047900       MOVE MED-MFSFEL         TO MOD-TEMFSFEL                            
048000       MOVE SPACES             TO MOD-TEMFSINF                            
048100     ELSE                                                                 
048200                                                                          
048300       MOVE +1                 TO INDX                                    
048400                                                                          
048500       PERFORM UNTIL SEGMENT-MISSING OR                                   
048600                     SEGMENT-END    OR                                    
048700                     INDX > MAX-INDX                                      
048800                                                                          
048900          MOVE SEQA-IDDISTR    TO  W-IDDISTR-N2                           
049000          MOVE SEQA-IDKUNDNR   TO  W-IDKUNDNR-N2                          
049100          MOVE SEQA-IDKUNDRF   TO  W-IDKUNDRF-N2                          
049200          MOVE SEQA-IDARTNR    TO  W-IDARTNR-N2                           
049300          MOVE SEQA-IDLOPNR    TO  W-IDLOPNR-N2                           
049400                                                                          
049500          PERFORM IMS-GU-WDA501                                           
049600          IF SEGMENT-FOUND                                                
049601            PERFORM FAA-MOVE-TO-MOD                                       
049916          END-IF                                                          
049917                                                                          
050000          PERFORM IMS-GN-WDA5A1                                           
050100                                                                          
050200       END-PERFORM                                                        
050300                                                                          
050400       IF SEGMENT-FOUND                                                   
050500          MOVE SEQA-IDDISTR    TO  W-IDDISTR-N2                           
050600          MOVE SEQA-IDKUNDNR   TO  W-IDKUNDNR-N2                          
050700          MOVE SEQA-IDKUNDRF   TO  W-IDKUNDRF-N2                          
050800          MOVE SEQA-IDARTNR    TO  W-IDARTNR-N2                           
050900          MOVE SEQA-IDLOPNR    TO  W-IDLOPNR-N2                           
051060                                                                          
051100          PERFORM IMS-GU-WDA501                                           
051200          IF SEGMENT-FOUND                                                
051300             PERFORM FAB-SAVE-PF8-KEYS                                    
051400          END-IF                                                          
051500       ELSE                                                               
051502          MOVE SAVE-IDARTNR-ENTER  TO SAVE-IDARTNR-NEXT                   
051503          MOVE SAVE-IDDC-ENTER     TO SAVE-IDDC-NEXT                      
051504          MOVE SAVE-KDRAPRIO-ENTER TO SAVE-KDRAPRIO-NEXT                  
051505          MOVE SAVE-TIRODAT-ENTER  TO SAVE-TIRODAT-NEXT                   
051506          MOVE SAVE-IDDISTR-ENTER  TO SAVE-IDDISTR-NEXT                   
051507          MOVE SAVE-IDKUNDNR-ENTER TO SAVE-IDKUNDNR-NEXT                  
051508          MOVE SAVE-IDKUNDRF-ENTER TO SAVE-IDKUNDRF-NEXT                  
051509          MOVE SAVE-IDLOPNR-ENTER  TO SAVE-IDLOPNR-NEXT                   
051591                                                                          
051600          MOVE INF-LAST-PAGE   TO MED-IDMFSINF                            
051700          CALL WMEDKONV     USING MED-WMEDAREA                            
051800          MOVE MED-MFSINF      TO MOD-TEMFSINF                            
051900       END-IF                                                             
052000     END-IF                                                               
052200     .                                                                    
052300     EJECT                                                                
052400                                                                          
054500 FAA-MOVE-TO-MOD SECTION.                                                 
054600                                                                          
054700     MOVE RAD-IDDC-RO      TO W-IDDC-RO                                   
054730     PERFORM IMS-GU-WDK711                                                
054740     IF SEGMENT-FOUND                                                     
054750        MOVE SLAG-IDLEVNR  TO WS-IDLEVNR-8                                
054760        IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                         
054761        OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                   
054770*          --- AUTHORIZED USER                                            
054780           MOVE YES        TO INDATA-SW                                   
054790        ELSE                                                              
054791           MOVE NOO        TO INDATA-SW                                   
054792        END-IF                                                            
054793     END-IF                                                               
054794                                                                          
054795     IF INDATA-OK                                                         
054900       MOVE RAD-IDDC-RO    TO MOD-IDDC-RO-LINE(INDX)                      
055000       MOVE RAD-IDARTNR    TO MOD-IDARTNR-LINE(INDX)                      
055100       MOVE RAD-IDDISTR    TO MOD-IDDISTR-LINE(INDX)                      
055200       MOVE RAD-IDKUNDNR   TO MOD-IDKUNDNR-LINE(INDX)                     
055300       MOVE RAD-IDORDNR5   TO MOD-IDORDNR-LINE(INDX)                      
055400       MOVE RAD-KVART      TO MOD-KVART-LINE(INDX)                        
055500                                                                          
055600       IF RAD-DARODAT(3:6) > 0                                            
055700          MOVE RAD-DARODAT(3:6) TO MOD-TIRODAT-LINE(INDX)                 
055800       ELSE                                                               
055900          MOVE SPACE       TO MOD-TIRODAT-LINE(INDX)                      
056000       END-IF                                                             
056100                                                                          
056110       MOVE RAD-TIRES            TO WS-TIRES                              
056200       IF WS-TIRES > ZERO                                                 
056300          MOVE WS-TIRES          TO MOD-TIRES-LINE(INDX)                  
056400       ELSE                                                               
056500          MOVE SPACE             TO MOD-TIRES-LINE(INDX)                  
056600       END-IF                                                             
056700                                                                          
056800       MOVE RAD-KDORDKL          TO MOD-KDORDKL-LINE(INDX)                
056900       MOVE RAD-KDROO            TO MOD-KDROO-LINE(INDX)                  
057000       MOVE RAD-IDANSK           TO MOD-IDANSK-LINE(INDX)                 
057100       MOVE RAD-KDSTARAD         TO MOD-KDSTARAD-LINE(INDX)               
057200       MOVE RAD-KDRAPRIO         TO MOD-KDRAPRIO-LINE(INDX)               
057300                                                                          
057400       IF INDX = 1                                                        
057500        MOVE RAD-IDARTNR         TO SAVE-IDARTNR-ENTER                    
057501        MOVE RAD-IDDC            TO SAVE-IDDC-ENTER                       
057510        MOVE RAD-IDDISTR         TO SAVE-IDDISTR-ENTER                    
057600        MOVE RAD-IDKUNDNR        TO SAVE-IDKUNDNR-ENTER                   
057700        MOVE RAD-IDKUNDRF        TO SAVE-IDKUNDRF-ENTER                   
057800        MOVE RAD-IDLOPNR         TO SAVE-IDLOPNR-ENTER                    
057900        IF RAD-DARODAT(3:6) > 0                                           
058000           MOVE RAD-DARODAT(3:6) TO SAVE-TIRODAT-ENTER                    
058100        ELSE                                                              
058200           MOVE 0                TO SAVE-TIRODAT-ENTER                    
058300        END-IF                                                            
058301        MOVE RAD-TIREGTID        TO SAVE-TIREGTID-ENTER                   
058400        MOVE RAD-KDRAPRIO        TO SAVE-KDRAPRIO-ENTER                   
058500       END-IF                                                             
058600                                                                          
058700       ADD +1                    TO INDX                                  
058801     ELSE                                                                 
058802      MOVE SPACES                TO MOD-TEMFSFEL                          
058810     END-IF                                                               
058900     .                                                                    
059000     EJECT                                                                
059100 FAB-SAVE-PF8-KEYS SECTION.                                               
059200                                                                          
059300     MOVE RAD-IDARTNR         TO SAVE-IDARTNR-NEXT                        
059301     MOVE RAD-IDDC            TO SAVE-IDDC-NEXT                           
059310     MOVE RAD-IDDISTR         TO SAVE-IDDISTR-NEXT                        
059400     MOVE RAD-IDKUNDNR        TO SAVE-IDKUNDNR-NEXT                       
059500     MOVE RAD-IDORDNR5        TO SAVE-IDKUNDRF-NEXT                       
059600     IF RAD-DARODAT(3:6) > 0                                              
059700        MOVE RAD-DARODAT(3:6) TO SAVE-TIRODAT-NEXT                        
059800     ELSE                                                                 
059900        MOVE 0                TO SAVE-TIRODAT-NEXT                        
060000     END-IF                                                               
060010     MOVE RAD-TIREGTID        TO SAVE-TIREGTID-NEXT                       
060100                                                                          
060200     MOVE RAD-KDRAPRIO        TO SAVE-KDRAPRIO-NEXT                       
060300     MOVE RAD-IDLOPNR         TO SAVE-IDLOPNR-NEXT                        
060400                                                                          
060500     MOVE INF-MORE-INFO-EXISTS                                            
060600                              TO MED-IDMFSINF                             
060700     CALL WMEDKONV         USING MED-WMEDAREA                             
060800     MOVE MED-MFSINF          TO MOD-TEMFSINF                             
060900     .                                                                    
061000     EJECT                                                                
062700                                                                          
062800 S02-FORMAT-DATE SECTION.                                                 
062900     MOVE WS-TIRODAT         TO W-DARODAT-N3-MIN                          
063000     IF WS-TIRODAT NOT = ZERO                                             
063100       IF WS-TIRODAT < 500000                                             
063200         MOVE 20              TO W-DARODAT-N3-MIN (1:2)                   
063300       ELSE                                                               
063400         IF WS-TIRODAT < 999999                                           
063500           MOVE 19            TO W-DARODAT-N3-MIN (1:2)                   
063600         ELSE                                                             
063700           MOVE 99999999      TO W-DARODAT-N3-MIN                         
063800         END-IF                                                           
063900       END-IF                                                             
064000     END-IF                                                               
064100     .                                                                    
064200     EJECT                                                                
064300 MFS-ERASE-FIELD-OUT SECTION.                                             
064400                                                                          
064500*    --- ALLA UTDATA-FÄLT                                                 
064600*    --- INCL. SCROLL KEYS                                                
064700     MOVE MFS-ERASE-FIELD  TO  SAVE-IDDISTR-NEXT                          
064710                               SAVE-IDARTNR-NEXT                          
064720                               SAVE-IDDC-NEXT                             
064800                               SAVE-IDKUNDNR-NEXT                         
064900                               SAVE-IDKUNDRF-NEXT                         
065000                               SAVE-TIRODAT-NEXT                          
065100                               SAVE-IDLOPNR-NEXT                          
065200                               SAVE-KDRAPRIO-NEXT                         
065300     PERFORM VARYING INDX FROM 1 BY 1                                     
065400     UNTIL INDX > MAX-INDX                                                
065500        PERFORM MFS-ERASE-LINE-FIELD-OUT                                  
065600     END-PERFORM                                                          
065700     .                                                                    
065800     SKIP3                                                                
065900 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
066000                                                                          
066100*    --- OUTDATA-FIELD ON SCROLL KEYS                                     
066200     MOVE MFS-ERASE-FIELD TO MOD-IDDC-RO-LINE(INDX)                       
066300                             MOD-IDARTNR-LINE(INDX)                       
066400                             MOD-IDDISTR-LINE(INDX)                       
066500                             MOD-IDKUNDNR-LINE(INDX)                      
066600                             MOD-IDORDNR-LINE(INDX)                       
066700                             MOD-KVART-LINE(INDX)                         
066800                             MOD-TIRODAT-LINE(INDX)                       
066900                             MOD-TIRES-LINE(INDX)                         
067000                             MOD-KDORDKL-LINE(INDX)                       
067100                             MOD-KDROO-LINE(INDX)                         
067200                             MOD-IDANSK-LINE(INDX)                        
067300                             MOD-KDSTARAD-LINE(INDX)                      
067400                             MOD-KDRAPRIO-LINE(INDX)                      
067500     .                                                                    
067600     SKIP3                                                                
067700 MFS-ERASE-FIELD-IN SECTION.                                              
067800                                                                          
067900*    --- ALLA INDATA-FÄLT                                                 
068000     MOVE MFS-ERASE-FIELD TO MOD-IDARTNR-IN                               
068100                             MOD-IDDC-IN                                  
068200     .                                                                    
068300     EJECT                                                                
068400* --- IMS SECTIONS ---                                                    
068500     SKIP3                                                                
068600 IMS-GET-MSG SECTION.                                                     
068700                                                                          
068800     MOVE '  QC' TO GOOD-STATUSCODES                                      
068900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
069000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
069100     PERFORM IMS-STATUSCHECK                                              
069200     .                                                                    
069300     SKIP3                                                                
069400 IMS-INSERT-MSG SECTION.                                                  
069500                                                                          
069900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
070000     MOVE SPACE TO GOOD-STATUSCODES                                       
070100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
070200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
070300     PERFORM IMS-STATUSCHECK                                              
070400     .                                                                    
070500     EJECT                                                                
070600 IMS-GU-WDA501 SECTION.                                                   
070700                                                                          
070800     MOVE SPACE            TO ALL-SSA                                     
070900     STRING 'WDA501  (WDA501KY =' W-WDA501KY ')'                          
071000       DELIMITED BY SIZE INTO SSA1                                        
071100     MOVE '  GE'           TO GOOD-STATUSCODES                            
071200     CALL CBLTDLI USING GU WDA5-PCB DLI-IO-WDA501 SSA1                    
071300     MOVE WDA5-STATUS-CODE TO STATUS-WS                                   
071400     PERFORM IMS-STATUSCHECK                                              
071500     .                                                                    
071600     EJECT                                                                
071700 IMS-GU-WDA5A1 SECTION.                                                   
071800                                                                          
071810     MOVE SPACE             TO ALL-SSA                                    
072000     STRING 'WDA5A1  (WDA5A1KY>=' W-WDA5A1KY-MIN                          
072100                    '&WDA5A1KY<=' W-WDA5A1KY-MAX                          
072200                    '&KDSTARAD>=' W-KDSTARAD-MIN                          
072300                    '&KDSTARAD<=' W-KDSTARAD-MAX ')'                      
072400        DELIMITED BY SIZE INTO SSA1                                       
072500     MOVE '  GBGE'          TO GOOD-STATUSCODES                           
072600     CALL CBLTDLI USING GU WDA5A-PCB DLI-IO-WDA5A1 SSA1                   
072700     MOVE WDA5A-STATUS-CODE TO STATUS-WS                                  
072800     PERFORM IMS-STATUSCHECK                                              
072900     .                                                                    
073000     EJECT                                                                
073010 IMS-GN-WDA5A1 SECTION.                                                   
073020                                                                          
073030     MOVE SPACE             TO ALL-SSA                                    
073040     STRING 'WDA5A1  (WDA5A1KY>=' W-WDA5A1KY-MIN                          
073050                    '&WDA5A1KY<=' W-WDA5A1KY-MAX                          
073060                    '&KDSTARAD>=' W-KDSTARAD-MIN                          
073070                    '&KDSTARAD<=' W-KDSTARAD-MAX ')'                      
073080        DELIMITED BY SIZE INTO SSA1                                       
073090     MOVE '  GBGE'          TO GOOD-STATUSCODES                           
073091     CALL CBLTDLI USING GN WDA5A-PCB DLI-IO-WDA5A1 SSA1                   
073092     MOVE WDA5A-STATUS-CODE TO STATUS-WS                                  
073093     PERFORM IMS-STATUSCHECK                                              
073094     .                                                                    
073095     EJECT                                                                
073100 IMS-GU-WDK711 SECTION.                                                   
073200                                                                          
073210     MOVE SPACE            TO ALL-SSA                                     
073300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
073400       DELIMITED BY SIZE INTO SSA1                                        
073500     STRING 'WDK711  (IDDC     =' W-IDDC-RO-X ')'                         
073600       DELIMITED BY SIZE INTO SSA2                                        
073700     MOVE '  GE' TO GOOD-STATUSCODES                                      
073800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
073900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
074000     PERFORM IMS-STATUSCHECK                                              
074100     .                                                                    
074200     EJECT                                                                
074300 IMS-GU-WDK601 SECTION.                                                   
074400                                                                          
074410     MOVE SPACE            TO ALL-SSA                                     
074600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
074700       DELIMITED BY SIZE INTO SSA1                                        
074800     MOVE '  GE'           TO GOOD-STATUSCODES                            
074900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
075000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
075100     PERFORM IMS-STATUSCHECK                                              
075200     .                                                                    
075300     EJECT                                                                
075310 IMS-GU-WDB601 SECTION.                                                   
075320                                                                          
075321     MOVE SPACE            TO ALL-SSA                                     
075330     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
075340       DELIMITED BY SIZE INTO SSA1                                        
075350     MOVE '  GE'           TO GOOD-STATUSCODES                            
075360     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
075370     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
075380     PERFORM IMS-STATUSCHECK                                              
075390     .                                                                    
075402     EJECT                                                                
075403 IMS-GU-WDB601-MIN-MAX SECTION.                                           
075409                                                                          
075410     MOVE SPACE            TO ALL-SSA                                     
075411     STRING 'WDB601  (IDDC    >=' W-IDDC-MIN-X                            
075412                    '&IDDC    <=' W-IDDC-MAX-X ')'                        
075413       DELIMITED BY SIZE INTO SSA1                                        
075414     MOVE '  GE'           TO GOOD-STATUSCODES                            
075415     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
075416     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
075417     PERFORM IMS-STATUSCHECK                                              
075418     .                                                                    
075419     EJECT                                                                
075420 IMS-STATUSCHECK SECTION.                                                 
075500                                                                          
075600     SET STATUS-IX TO 1                                                   
075700     SEARCH GOOD-STATUS                                                   
075800       AT END                                                             
075900         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
076000         DELIMITED BY SIZE INTO ERROR-TEXT                                
076100         CALL FELLOG                                                      
076200       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
076300         CONTINUE                                                         
076400     END-SEARCH                                                           
076500     .                                                                    
