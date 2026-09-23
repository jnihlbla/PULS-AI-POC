000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6012610.                                                
000300 AUTHOR.         EVA LUNDELL  / KJELL ANDRÉ                               
000400 DATE-WRITTEN.   96/05/14     / 2011-03-04                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET VISAR PLACERINGSHISTORIK FÖR GODS UNDER               
000900*        INLEVERANS                                                       
001000*                                                                         
001100*        PROGRAMMET LÄSER      W6UPPF (W6L2)                              
001200*                                                                         
001300*        NOTE: THIS PROGRAM IS CALLED FROM "DRIVER" PROGRAMS              
001400*              TAKING CARE OF DIFFERENT TECHNICAL DETAILS                 
001500*              DEPENDING ON HOW THE TRANSACTION WAS STARTED.              
001600*              ONE DRIVER EXIST FOR "CLASSICAL" INVOCATION                
001700*              VIA 3270 SCREEN - W6012600, AND ONE FOR INVOCATION         
001800*              FROM THE WEB - W6W12600.                                   
001900*                                                                         
002000*    INDATA.                                                              
002100*        PCB:         MSG-PCB                                             
002200*        PCB:         UPFB-PCB (W6L2)                                     
002300*        REQUEST:     W60126I1                                            
002400*                                                                         
002500*    UTDATA.                                                              
002600*        RESPONSE:    W60126O1                                            
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400 77  HHMMSSTT                    PIC 9(8).                                
003500*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W6012600'.            
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300                                                                          
004400 01 WS-TIKLOCK                   PIC 9(8).                                
004500 01 WS-TIHHMMSSTH  REDEFINES WS-TIKLOCK.                                  
004600    03  WS-TIHH                  PIC 9(2).                                
004700    03  WS-TIMM                  PIC 9(2).                                
004800    03 FILLER                    PIC 9(4).                                
004900 01 WS-TIHHMM-UT.                                                         
005000    03 WS-TIHH-UT                PIC 9(2).                                
005100    03 FILLER                    PIC X(1)  VALUE '.'.                     
005200    03 WS-TIMM-UT                PIC 9(2).                                
005300                                                                          
005400 01 WS-DAREGDAT-UT               PIC 9(6).                                
005500                                                                          
005600 01 WS-KVINLART-UT               PIC 9(6).                                
005700                                                                          
005800 01 WS-IDRADNR-UT                PIC 9(4).                                
005900                                                                          
006000                                                                          
006100*    --- INDEX FÖR BLÄDDRINGSRADER                                        
006200 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
006300                                                                          
006400                                                                          
006500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006600     88  NYCKLAR-OK                          VALUE 'J'.                   
006700     88  NYCKLAR-FEL                         VALUE 'N'.                   
006800                                                                          
006900     EJECT                                                                
007000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007100 01  GENERELLA-SUBPROGRAM.                                                
007200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007400     EJECT                                                                
007500 01  MESSAGE-CODES.                                                       
007600     03  INF-FIRST-PAGE          PIC X(3)    VALUE '010'.                 
007700     03  INF-SISTA-SIDAN         PIC X(3)    VALUE '012'.                 
007800     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '011'.                 
007900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
008000     03  ERR-INFO-MISSING        PIC X(3)    VALUE '027'.                 
008100     EJECT                                                                
008200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008300*                                                                         
008400     EJECT                                                                
008500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008600     SKIP3                                                                
008700 01  NYCKLAR-TILL-DLI.                                                    
008800     03  W-W6L201KY-MIN-X.                                                
008900         05  W-IDLOPNRM-MIN      PIC S9(9)   VALUE ZERO COMP-3.           
009000         05  W-IDRADNR-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
009100         05  W-DAREGDAT-MIN      PIC  9(8)   VALUE ZERO.                  
009200         05  W-TIKLOCK-MIN       PIC S9(9)   VALUE ZERO COMP-3.           
009300     SKIP2                                                                
009400     03  W-W6L201KY-MAX-X.                                                
009500         05  W-IDLOPNRM-MAX      PIC S9(9)   VALUE ZERO COMP-3.           
009600         05  W-IDRADNR-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
009700         05  W-DAREGDAT-MAX      PIC  9(8)   VALUE ZERO.                  
009800         05  W-TIKLOCK-MAX       PIC S9(9)   VALUE ZERO COMP-3.           
009900     SKIP2                                                                
010000*    --- STATUS-KOD FRÅN IMS                                              
010100 01  STATUS-WS                   PIC XX.                                  
010200     88  SEGMENT-FINNS                       VALUE '  '.                  
010300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010500     SKIP2                                                                
010600 01  GODK-STATUSKODER.                                                    
010700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010800     SKIP3                                                                
010900 01  SSA1                        PIC X(96).                               
011000 01  SSA2                        PIC X(64).                               
011100     EJECT                                                                
011200*    --- IMS FUNKTIONSKODER                                               
011300*01  -COPY W0003                                                          
011400     EJECT                                                                
011500*    ---  DLI INPUT-OUTPUT AREA                                           
011600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
011700     SKIP3                                                                
011800 01  DLI-IO-AREA.                                                         
011900     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
012000     SKIP3                                                                
012100     03  W6UPPF01 REDEFINES IO-AREA.                                      
012200*        05  -COPY W6L201                                                 
012300     EJECT                                                                
012400 LINKAGE SECTION.                                                         
012500*01  -COPY W0009   -PRE MSG-                                              
012600     EJECT                                                                
012700*01  -COPY W0008   -PRE UPFB-                                             
012800     05  FILLER                  PIC X.                                   
012900                                                                          
013000 01  MAX-KVRADER                 PIC S9(4)   COMP.                        
013100     EJECT                                                                
013200 01  REQU-AREA.                                                           
013300*    03 -COPY WZ01REQU                                                    
013400*    03 -COPY W60126I1                                                    
013500     EJECT                                                                
013600 01  RESP-AREA.                                                           
013700*    03 -COPY WZ01RESP                                                    
013800*    03 -COPY W60126O1                                                    
013900     EJECT                                                                
014000 PROCEDURE DIVISION  USING MSG-PCB UPFB-PCB                               
014100                     MAX-KVRADER                                          
014200                     REQU-AREA   RESP-AREA.                               
014300                                                                          
014400     PERFORM A-INIT                                                       
014500     PERFORM B-KOLLA-NYCKLAR                                              
014600     IF NYCKLAR-OK                                                        
014700       PERFORM F-LAES-VISA-INFO                                           
014800     END-IF                                                               
014900                                                                          
015000     MOVE ZERO TO RETURN-CODE                                             
015100     GOBACK                                                               
015200     .                                                                    
015300     EJECT                                                                
015400 A-INIT SECTION.                                                          
015500                                                                          
015600     MOVE ALL '+'     TO RESP-AREA                                        
015700     MOVE 001         TO RESP-IDMSGVER                                    
015800     MOVE SPACE       TO RESP-IDMSG-ERROR                                 
015900                         RESP-IDMSG-INFO                                  
016000                         RESP-IDELMT-ERROR                                
016100                                                                          
016200     MOVE LOW-VALUE   TO W-W6L201KY-MIN-X                                 
016300     MOVE HIGH-VALUE  TO W-W6L201KY-MAX-X                                 
016400                                                                          
016500     MOVE ZERO        TO RESP-KVRADER                                     
016600     .                                                                    
016700     EJECT                                                                
016800 B-KOLLA-NYCKLAR SECTION.                                                 
016900                                                                          
017000     MOVE JA TO NYCKLAR-SW                                                
017100                                                                          
017200     IF REQU-IDLOPNRM-KEY NUMERIC                                         
017300       MOVE REQU-IDLOPNRM-KEY TO W-IDLOPNRM-MIN                           
017400                                 W-IDLOPNRM-MAX                           
017500     ELSE                                                                 
017600       MOVE NEJ TO NYCKLAR-SW                                             
017700     END-IF                                                               
017800                                                                          
017900     IF REQU-IDRADNR-KEY  NUMERIC                                         
018000       IF REQU-IDRADNR-KEY > ZERO                                         
018100         MOVE REQU-IDRADNR-KEY  TO W-IDRADNR-MIN                          
018200                                   W-IDRADNR-MAX                          
018300       END-IF                                                             
018400     ELSE                                                                 
018500       MOVE NEJ TO NYCKLAR-SW                                             
018600     END-IF                                                               
018700                                                                          
018800     IF REQU-IDRADNR-START  NUMERIC                                       
018900       IF REQU-IDRADNR-START > ZERO                                       
019000         MOVE REQU-IDRADNR-START  TO W-IDRADNR-MIN                        
019100       END-IF                                                             
019200     ELSE                                                                 
019300       MOVE NEJ TO NYCKLAR-SW                                             
019400     END-IF                                                               
019500                                                                          
019600     IF REQU-DAREGDAT-START  NUMERIC                                      
019700       IF REQU-DAREGDAT-START > ZERO                                      
019800         MOVE REQU-DAREGDAT-START  TO W-DAREGDAT-MIN                      
019900       END-IF                                                             
020000     ELSE                                                                 
020100       MOVE NEJ TO NYCKLAR-SW                                             
020200     END-IF                                                               
020300                                                                          
020400     IF REQU-TIKLOCK-START  NUMERIC                                       
020500       IF REQU-TIKLOCK-START > ZERO                                       
020600         MOVE REQU-TIKLOCK-START  TO W-TIKLOCK-MIN                        
020700       END-IF                                                             
020800     ELSE                                                                 
020900       MOVE NEJ TO NYCKLAR-SW                                             
021000     END-IF                                                               
021100                                                                          
021200     IF NYCKLAR-FEL                                                       
021300       MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                             
021400     END-IF                                                               
021500     .                                                                    
021600     EJECT                                                                
021700 F-LAES-VISA-INFO SECTION.                                                
021800     SKIP2                                                                
021900     PERFORM IMS-GU-UPFB-W6UPFB01                                         
022000                                                                          
022100     IF SEGMENT-SAKNAS                                                    
022200        ACCEPT  HHMMSSTT FROM TIME                                        
022300        MOVE ERR-INFO-MISSING TO RESP-IDMSG-ERROR                         
022400     ELSE                                                                 
022500       MOVE +1 TO INDX                                                    
022600       IF SEGMENT-FINNS                                                   
022700         MOVE UPPF-IDRADNR     TO RESP-IDRADNR-START                      
022800         MOVE UPPF-DAREGDAT    TO RESP-DAREGDAT-START                     
022900         MOVE UPPF-TIKLOCK     TO RESP-TIKLOCK-START                      
023000       ELSE                                                               
023100         MOVE ZERO             TO RESP-IDRADNR-START                      
023200         MOVE ZERO             TO RESP-DAREGDAT-START                     
023300         MOVE ZERO             TO RESP-TIKLOCK-START                      
023400       END-IF                                                             
023500                                                                          
023600       PERFORM UNTIL INDX > MAX-KVRADER                                   
023700       OR NOT SEGMENT-FINNS                                               
023800         PERFORM FA-LAES-VISA-RADDATA                                     
023900         ADD 1 TO INDX                                                    
024000       END-PERFORM                                                        
024100                                                                          
024200       MOVE INDX TO RESP-KVRADER                                          
024300                                                                          
024400       IF SEGMENT-FINNS                                                   
024500         MOVE UPPF-IDRADNR   TO RESP-IDRADNR-NEXT                         
024600         MOVE UPPF-DAREGDAT  TO RESP-DAREGDAT-NEXT                        
024700         MOVE UPPF-TIKLOCK   TO RESP-TIKLOCK-NEXT                         
024800         MOVE INF-MORE-INFO-EXISTS TO RESP-IDMSG-INFO                     
024900       ELSE                                                               
025000         MOVE REQU-IDRADNR-START  TO RESP-IDRADNR-NEXT                    
025100         MOVE REQU-DAREGDAT-START TO RESP-DAREGDAT-NEXT                   
025200         MOVE REQU-TIKLOCK-START  TO RESP-TIKLOCK-NEXT                    
025300         MOVE INF-SISTA-SIDAN  TO RESP-IDMSG-INFO                         
025400       END-IF                                                             
025500                                                                          
025600     END-IF                                                               
025700     .                                                                    
025800     EJECT                                                                
025900 FA-LAES-VISA-RADDATA SECTION.                                            
026000     SKIP2                                                                
026100     MOVE UPPF-IDRADNR       TO WS-IDRADNR-UT                             
026200     MOVE WS-IDRADNR-UT      TO RESP-IDRADNR (INDX)                       
026300                                                                          
026400     MOVE UPPF-DAREGDAT      TO WS-DAREGDAT-UT                            
026500     MOVE WS-DAREGDAT-UT     TO RESP-TIREGDAT (INDX)                      
026600                                                                          
026700     MOVE UPPF-TIKLOCK       TO WS-TIKLOCK                                
026800     MOVE WS-TIHH            TO WS-TIHH-UT                                
026900     MOVE WS-TIMM            TO WS-TIMM-UT                                
027000     MOVE WS-TIHHMM-UT       TO RESP-TIHHMM (INDX)                        
027100                                                                          
027200     MOVE UPPF-KDINLSTA      TO RESP-KDINLSTA (INDX)                      
027210     IF REQU-IDSPRAK NOT = 'SV'                                           
027220       EVALUATE RESP-KDINLSTA (INDX)                                      
027230         WHEN 'SAK'                                                       
027250             MOVE 'MIS' TO RESP-KDINLSTA (INDX)                           
027290         WHEN 'ANT'                                                       
027292             MOVE 'DEV' TO RESP-KDINLSTA (INDX)                           
027293         WHEN 'AVV'                                                       
027295             MOVE 'DEV' TO RESP-KDINLSTA (INDX)                           
027296         WHEN 'FPK'                                                       
027298             MOVE 'PP ' TO RESP-KDINLSTA (INDX)                           
027299         WHEN 'FPP'                                                       
027300             MOVE 'PS ' TO RESP-KDINLSTA (INDX)                           
027302         WHEN 'INL'                                                       
027304             MOVE 'BIN' TO RESP-KDINLSTA (INDX)                           
027317         WHEN OTHER                                                       
027318             MOVE SPACE TO RESP-KDINLSTA (INDX)                           
027319       END-EVALUATE                                                       
027322     END-IF                                                               
027330     MOVE UPPF-ADINLOMR      TO RESP-ADINLOMR (INDX)                      
027400     MOVE UPPF-ADINLOMR-NXT  TO RESP-ADINLOMR-NXT (INDX)                  
027500                                                                          
027600     MOVE UPPF-KVINLART      TO WS-KVINLART-UT                            
027700     MOVE WS-KVINLART-UT     TO RESP-KVINLART (INDX)                      
027800                                                                          
027900     MOVE UPPF-IDUSER        TO RESP-IDUSER (INDX)                        
028000                                                                          
028100     PERFORM IMS-GN-UPFB-W6UPFB01                                         
028200                                                                          
028300     .                                                                    
028400     EJECT                                                                
028500 IMS-GU-UPFB-W6UPFB01 SECTION.                                            
028600                                                                          
028700     STRING 'W6UPFB01(W6L201KY>=' W-W6L201KY-MIN-X                        
028800                    '&W6L201KY<=' W-W6L201KY-MAX-X ')'                    
028900          DELIMITED BY SIZE INTO SSA1                                     
029000     MOVE '  GE' TO GODK-STATUSKODER                                      
029100     CALL CBLTDLI USING GU UPFB-PCB DLI-IO-AREA SSA1                      
029200     MOVE UPFB-STATUS-CODE TO STATUS-WS                                   
029300     PERFORM IMS-STATUSKONTROLL                                           
029400     .                                                                    
029500     EJECT                                                                
029600 IMS-GN-UPFB-W6UPFB01 SECTION.                                            
029700                                                                          
029800     STRING 'W6UPFB01(W6L201KY>=' W-W6L201KY-MIN-X                        
029900                    '&W6L201KY<=' W-W6L201KY-MAX-X ')'                    
030000          DELIMITED BY SIZE INTO SSA1                                     
030100     MOVE '  GE' TO GODK-STATUSKODER                                      
030200     CALL CBLTDLI USING GN UPFB-PCB DLI-IO-AREA SSA1                      
030300     MOVE UPFB-STATUS-CODE TO STATUS-WS                                   
030400     PERFORM IMS-STATUSKONTROLL                                           
030500     .                                                                    
030600     EJECT                                                                
030700 IMS-STATUSKONTROLL SECTION.                                              
030800                                                                          
030900     SET STATUS-IX TO 1                                                   
031000     SEARCH GODK-STATUS                                                   
031100       AT END                                                             
031200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
031300         DELIMITED BY SIZE INTO FELTEXT                                   
031400         CALL FELLOG                                                      
031500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
031600         CONTINUE                                                         
031700     END-SEARCH                                                           
031800     .                                                                    
