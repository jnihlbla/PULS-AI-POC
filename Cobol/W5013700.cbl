000100 ID DIVISION.                                                             
000201 PROGRAM-ID.     W5013700.                                                
000300 AUTHOR.         MAMATHA SHETTY.                                          
000400 DATE-WRITTEN.   APR 2023.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THIS PROGRAM HANDLES UPDATES OF LCF FOR TAIWAN                   
000900*                                                                         
001000*        THE PROGRAM UPDATES   WDB6                                       
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSACTION: W5T137                                              
001401*        MID:         W5I13701                                            
001500*                                                                         
001600*    OUTDATA.                                                             
001701*        MOD:         W5O137N1                                            
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100                                                                          
002200 DATA DIVISION.                                                           
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002501 77  IDPGM                       PIC X(08)   VALUE 'W5013700'.            
002600                                                                          
002700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
002800 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
002900                                                                          
003000 77  YES                         PIC X       VALUE 'J'.                   
003100 77  NOO                         PIC X       VALUE 'N'.                   
003200 77  WS-CURRENT-DATE             PIC X(8)  VALUE SPACE.                   
003300 77  WS-START-DATE               PIC 9(6)    VALUE ZERO.                  
003400 77  WS-PERCENT-FROM             PIC 9(3)V9(3) VALUE ZERO.                
003500 77  WS-PERCENT                  PIC 9(4)V9(2) VALUE ZERO.                
003600 77  WS-RELANDCO-PERC            PIC 9(3)V9(1) VALUE ZERO.                
003702 77  WS-RELANDCO-FG-FROM-PERC    PIC 9(3)V9(4) VALUE ZERO.                
003802 77  WS-RELANDCO-FG-TO-PERC      PIC 9(3)V9(4) VALUE ZERO.                
003902 77  WS-RELANDCO-FG-FROM         PIC 9(3)V9(4) VALUE ZERO.                
004002 77  WS-RELANDCO-FG-TO           PIC 9(3)V9(4) VALUE ZERO.                
004104 77  WS-RELANDCO-TOTAL           PIC 9(3)V9(4) VALUE ZERO.                
004105 77  WS-RELANDCO-FG-TOTAL        PIC 9(3)V9(3) VALUE ZERO.                
004204 77  WS-RELANDCO-IFROM-PERC      PIC 9(3)V9(4) VALUE ZERO.                
004304 77  WS-RELANDCO-GFROM-PERC      PIC 9(3)V9(4) VALUE ZERO.                
004404 77  WS-RELANDCO-OFROM-PERC      PIC 9(3)V9(4) VALUE ZERO.                
004405 77  WS-RELANDCO-FFROM-PERC      PIC 9(3)V9(4) VALUE ZERO.                
004406 77  WS-RELANDCO-OPERC           PIC 9(3)V9(4) VALUE ZERO.                
004407 77  WS-RELANDCO-IPERC           PIC 9(3)V9(4) VALUE ZERO.                
004408 77  WS-RELANDCO-GPERC           PIC 9(4)V9(4) VALUE ZERO.                
004409 77  WS-RELANDCO-FPERC           PIC 9(3)V9(4) VALUE ZERO.                
004504 77  WS-RELANDCO-IFROM-PERC1     PIC 9(3)V9(4) VALUE ZERO.                
004604 77  WS-RELANDCO-OFROM-PERC1     PIC 9(3)V9(4) VALUE ZERO.                
004704 77  WS-RELANDCO-GFROM-PERC1     PIC 9(3)V9(4) VALUE ZERO.                
004705 77  WS-RELANDCO-FFROM-PERC1     PIC 9(3)V9(4) VALUE ZERO.                
004804 77  WS-RELANDCO-FFROM           PIC 9(3)V9(1) VALUE ZERO.                
004904 77  WS-RELANDCO-IFROM           PIC 9(3)V9(4) VALUE ZERO.                
005004 77  WS-RELANDCO-GFROM           PIC 9(3)V9(4) VALUE ZERO.                
005104 77  WS-RELANDCO-OFROM           PIC 9(3)V9(4) VALUE ZERO.                
005204 77  WS-RELANDCO-TO              PIC 9(3)V9(1) VALUE ZERO.                
005304 77  WS-RELANDCO-ITO             PIC 9(3)V9(4) VALUE ZERO.                
005404 77  WS-RELANDCO-GTO             PIC 9(3)V9(4) VALUE ZERO.                
005504 77  WS-RELANDCO-OTO             PIC 9(3)V9(4) VALUE ZERO.                
005505 77  WS-RELANDCO-FTO             PIC 9(3)V9(4) VALUE ZERO.                
005602 77  WS-IDFKNGRP                 PIC X(4) VALUE SPACES.                   
005702 77  W-SAVE-IDFKNGRP             PIC S9(5)           COMP-3.              
005800 77  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005900 77  WS-IDFRIDATA                PIC X(25).                               
006000 77  WS-CNT                      PIC 9(2)    VALUE ZERO.                  
006102 77  WS-FGAD-IDFKNGRP            PIC X(5)    VALUE SPACE.                 
006200                                                                          
006300*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
006400                                                                          
006500 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006600     88  INDATA-OK                           VALUE 'J'.                   
006700     88  INDATA-WRONG                        VALUE 'N'.                   
006800                                                                          
006900 77  KEYS-SW                     PIC X       VALUE 'J'.                   
007000     88  KEYS-OK                             VALUE 'J'.                   
007100     88  KEYS-WRONG                          VALUE 'N'.                   
007200                                                                          
007300 77  INVALID-SW                  PIC X       VALUE 'J'.                   
007400     88  IDFTG-YES                           VALUE 'J'.                   
007500     88  INVALID-NO                          VALUE 'N'.                   
007600                                                                          
007700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007801     88  OWN-MID                             VALUE '5137'.                
007901     88  GOOD-MID                            VALUE '5137'.                
008000     88  HELP-MID                            VALUE '0551'.                
008100     EJECT                                                                
008200 01  W-DAGENS-DATUM-ONE-YEAR            PIC 9(6) VALUE ZERO.              
008300 01  FILLER REDEFINES W-DAGENS-DATUM-ONE-YEAR.                            
008400     03  W-DAGENS-DATUM-ONE-YEAR-YY     PIC 9(2).                         
008500     03  W-DAGENS-DATUM-ONE-YEAR-MM     PIC 9(2).                         
008600     03  W-DAGENS-DATUM-ONE-YEAR-DD     PIC 9(2).                         
008700                                                                          
008800 01  WORK-DATE                   PIC 9(6)   VALUE ZERO.                   
008900 01  FILLER REDEFINES WORK-DATE.                                          
009000     03  WORK-DATUM-YY                  PIC 9(2).                         
009100     03  WORK-DATUM-MM                  PIC 9(2).                         
009200     03  WORK-DATUM-DD                  PIC 9(2).                         
009300                                                                          
009400*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
009500 01  GENERAL-SUBPROGRAMS.                                                 
009600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010000     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
010100     EJECT                                                                
010200*    --- PARAMETERS FOR SUB PROGRAM WDECEDIT                              
010300*01  -COPY WDECAREA                                                       
010400*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
010500*01 -COPY WMEDAREA                                                        
010600*01 -COPY WWDCKONS                                                        
010700*01  -COPY WDATAREA                                                       
010800 01  WZ20DATE PIC X(8) VALUE 'WZ20DATE'.                                  
010900     SKIP3                                                                
011000*    -COPY WZ20DATE                                                       
011100     EJECT                                                                
011200*                                                                         
011300 01  WZ20DAYS                    PIC X(8)    VALUE 'WZ20DAYS'.            
011400     SKIP3                                                                
011500*    -COPY WZ20DAYS                                                       
011600     EJECT                                                                
011700*                                                                         
011800     EJECT                                                                
011900     SKIP3                                                                
012000 01  MESSAGE-CODES.                                                       
012100     03  ERR-CORR-HILITE-FLDS     PIC X(3)    VALUE '001'.                
012200     03  ERR-PF11-AND-NO-DATA     PIC X(3)    VALUE '011'.                
012300     03  INF-PRESS-PF11           PIC X(3)    VALUE '003'.                
012400     03  INF-UPDATE-DONE          PIC X(3)    VALUE '101'.                
012500     03  ERR-WRONG-KEY            PIC X(3)    VALUE '401'.                
012600     03  ERR-INVALID-VALUE        PIC X(3)    VALUE '492'.                
012700     03  INF-FIRST-PAGE           PIC X(3)    VALUE '006'.                
012800     03  ERR-FUTURE-DATE          PIC X(3)    VALUE '363'.                
012900     03  LAST-PAGE                PIC X(3)    VALUE '106'.                
013000     03  INF-MORE-LINE            PIC X(3)    VALUE '402'.                
013100     03  ERR-USER-NOT-AUTH        PIC X(3)    VALUE '405'.                
013200     EJECT                                                                
013300*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
013400*                                                                         
013500 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
013600     SKIP3                                                                
013700*01 -COPY WMSGINIT                                                        
013800     EJECT                                                                
013900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
014000     SKIP3                                                                
014101*01  MID -COPY W5I13701                                                   
014200     EJECT                                                                
014300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
014400     SKIP3                                                                
014500*01  -COPY WMSGAREA                                                       
014600     EJECT                                                                
014700     03  MOD REDEFINES MSG-AREA.                                          
014801*      05  -COPY W5O13701                                                 
014900     EJECT                                                                
015000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
015100     SKIP3                                                                
015200*01  -COPY WMFSAREA                                                       
015300     EJECT                                                                
015400*    --- WORK-AREAS FOR IMS-SECTIONS                                      
015500*                                                                         
015600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015700     SKIP3                                                                
015800 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
015900 77  MAX-INDX                    PIC S9(4)  VALUE +11   COMP SYNC.        
016000 01  KEYS-FOR-DLI.                                                        
016100     03  W-KDSEGKEY-X.                                                    
016200         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
016300     03  W-IDDC-X.                                                        
016400         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
016502     03  W-IDFKNGRP-X.                                                    
016602         05  W-IDFKNGRP          PIC S9(5)           COMP-3.              
016702     03  W-IDFKNGRP-MIN-X.                                                
016802         05  W-IDFKNGRP-MIN      PIC S9(5)           COMP-3.              
016902     03  W-IDFKNGRP-MAX-X.                                                
017002         05  W-IDFKNGRP-MAX      PIC S9(5)           COMP-3.              
017100     03  W-WDP701KY-X.                                                    
017200         05  W-IDUSER            PIC X(8)     VALUE SPACE.                
017300     03  W-IDFTG-X.                                                       
017400         05  W-IDFTG             PIC X(2)    VALUE SPACE.                 
017500     SKIP2                                                                
017600*    --- STATUS CODES FROM IMS                                            
017700 01  SAVE-AREA.                                                           
017801     03  SAVE-IDTRANS             PIC X(4)    VALUE '5137'.               
017902     03  SAVE-IDFKNGRP-ENTER      PIC S9(5)           COMP-3.             
018002     03  SAVE-IDFKNGRP-NEXT       PIC S9(5)           COMP-3.             
018100 01  STATUS-WS                    PIC XX.                                 
018200     88  SEGMENT-FOUND                       VALUE '  '.                  
018300     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
018400     88  SEGMENT-MISSING                     VALUE 'GE'.                  
018500     88  END-OF-DATABASE                     VALUE 'GB'.                  
018600     SKIP2                                                                
018700 01  GOOD-STATUSCODES.                                                    
018800     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018900     SKIP3                                                                
019000 01  SSA1                        PIC X(64).                               
019100 01  SSA2                        PIC X(64).                               
019200 01  SSA3                        PIC X(121).                              
019300     EJECT                                                                
019400*    --- IMS FUNCTION CODES                                               
019500 01  DLI-IO-WDP701.                                                       
019600*    03  -COPY WDP701                                                     
019700     EJECT                                                                
019800*01  -COPY W0003                                                          
019900     EJECT                                                                
020000*    ---  DLI INPUT-OUTPUT AREA                                           
020100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB617'.                      
020200 01  DLI-IO-WDB617.                                                       
020300*    03  -COPY WDB617                                                     
020400     EJECT                                                                
020501 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB622'.                      
020601 01  DLI-IO-WDB622.                                                       
020701*    03  -COPY WDB622                                                     
020800     EJECT                                                                
020900 LINKAGE SECTION.                                                         
021000*01  -COPY W0009   -PRE MSG-                                              
021100*01  -COPY W0008   -PRE WDP7-                                             
021200     05  FILLER                  PIC X.                                   
021300                                                                          
021400*01  -COPY W0008  -PRE WDB6-                                              
021500     05  FILLER                  PIC X.                                   
021600     EJECT                                                                
021700 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDB6-PCB.                     
021800 MAIN SECTION.                                                            
021900     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDB6-PCB.                     
022000                                                                          
022100     PERFORM IMS-GET-MSG                                                  
022200     IF SEGMENT-FOUND                                                     
022300       PERFORM A-INIT                                                     
022400       PERFORM B-CHECK-KEYS                                               
022500       IF KEYS-OK                                                         
022600         IF MFS-UPDATE                                                    
022700           PERFORM G-CHECK-INPUT                                          
022800           IF INDATA-OK                                                   
022900             PERFORM H-UPDATE                                             
023000           END-IF                                                         
023100         ELSE                                                             
023200           IF MFS-FIRST                                                   
023300             PERFORM C-FIRST-PAGE                                         
023400           ELSE                                                           
023500             IF MFS-NEXT                                                  
023600               PERFORM D-NEXT-PAGE                                        
023700             ELSE                                                         
023800               PERFORM E-SAME-PAGE                                        
023900             END-IF                                                       
024000           END-IF                                                         
024100         END-IF                                                           
024200         PERFORM F-READ-SHOW-INFO                                         
024300       END-IF                                                             
024401       COMPUTE MSG-KVLL = LENGTH OF MOD-W5O13701 + 4                      
024500       PERFORM IMS-INSERT-MSG                                             
024600     END-IF                                                               
024700                                                                          
024800     MOVE ZERO TO RETURN-CODE                                             
024900     GOBACK                                                               
025000     .                                                                    
025100     EJECT                                                                
025200 A-INIT SECTION.                                                          
025300     IF MSG-DOUBLE-TRANSACTIONS                                           
025401       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W5I13701                 
025500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
025600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
025700     ELSE                                                                 
025801       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W5I13701                  
025900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
026000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
026100     END-IF                                                               
026200                                                                          
026300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
026400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
026500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
026600                                                                          
026700     MOVE LOW-VALUE TO MSG-AREA                                           
026801     MOVE 'W5O137N1' TO MFS-IDMOD                                         
026901     MOVE '5137' TO MOD-IDTRANS                                           
027000     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
027100                                                                          
027200     IF OWN-MID  OR HELP-MID                                              
027300       CONTINUE                                                           
027400     ELSE                                                                 
027500       MOVE SPACE TO MFS-KDTRTYP                                          
027600       MOVE '7'   TO MFS-IDPFK                                            
027700     END-IF                                                               
027800     ACCEPT TODAYS-DATE FROM DATE                                         
027900     ACCEPT WORK-DATE    FROM DATE                                        
028000                                                                          
028100     MOVE WORK-DATE             TO DATE-TIDATE                            
028200     MOVE ZERO TO INDX                                                    
028300     .                                                                    
028400     EJECT                                                                
028500 B-CHECK-KEYS SECTION.                                                    
028600                                                                          
028700     MOVE ALL '+'           TO MSGI-WMSGINIT                              
028800     MOVE '001'             TO MSGI-KDCALL                                
028900     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
029000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
029101     MOVE '5137'            TO MSGI-IDTRANS                               
029200     IF GOOD-MID                                                          
029302        MOVE MID-IDFKNGRP-IN     TO MSGI-IDFKNGRP                         
029400     END-IF                                                               
029500     CALL W005INIT       USING MSGI-WMSGINIT WDP7-PCB                     
029600     MOVE MSGI-SPAR-AREA TO  SAVE-AREA                                    
029700     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
029800                                                                          
029900     MOVE YES               TO KEYS-SW                                    
030000                                                                          
030100** DC WITH FTG CODE ONLY 63ALLOWED **                                     
030200     MOVE MSGI-IDUSER TO W-IDUSER                                         
030300     MOVE MSGI-IDFTG        TO W-IDFTG                                    
030400                               MOD-IDFTG                                  
030500     MOVE MSGI-IDDC         TO W-IDDC                                     
030600                                                                          
030702     IF MSGI-IDFTG = 64                                                   
030800       MOVE YES TO KEYS-SW                                                
030900     ELSE                                                                 
031000       MOVE NOO TO KEYS-SW                                                
031100     END-IF                                                               
031200     IF KEYS-WRONG                                                        
031300       MOVE ERR-USER-NOT-AUTH TO MED-IDMFSFEL                             
031400       CALL WMEDKONV     USING   MED-WMEDAREA                             
031500       MOVE MED-TEMFSFEL   TO    MOD-TEMFSFEL                             
031600     ELSE                                                                 
031702       PERFORM BA-CHECK-IDFKNGRP                                          
031800     END-IF                                                               
031900     .                                                                    
032000     EJECT                                                                
032102 BA-CHECK-IDFKNGRP SECTION.                                               
032200                                                                          
032302     MOVE MFS-ERASE-FIELD   TO MOD-IDFKNGRP-IN                            
032402     IF MID-IDFKNGRP-IN  NOT  = ALL '+'                                   
032502        MOVE MID-IDFKNGRP-IN  TO WS-IDFKNGRP                              
032602                                 MOD-IDFKNGRP-UT                          
032700        MOVE SPACE TO MFS-KDTRTYP                                         
032800        MOVE '7'   TO MFS-IDPFK                                           
032900     ELSE                                                                 
033002        MOVE MSGI-IDFKNGRP    TO WS-IDFKNGRP                              
033102                                 MOD-IDFKNGRP-UT                          
033200     END-IF                                                               
033302     IF WS-IDFKNGRP IS NUMERIC                                            
033400       MOVE YES                  TO KEYS-SW                               
033502       MOVE WS-IDFKNGRP          TO W-IDFKNGRP-MIN                        
033602                                    W-IDFKNGRP-MAX                        
033700     ELSE                                                                 
033800       MOVE YES                  TO KEYS-SW                               
033902       MOVE SPACES               TO MOD-IDFKNGRP-UT                       
034002       MOVE LOW-VALUE            TO W-IDFKNGRP-MIN-X                      
034102       MOVE HIGH-VALUE           TO W-IDFKNGRP-MAX-X                      
034200     END-IF                                                               
034300     IF INVALID-NO                                                        
034400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
034500       CALL WMEDKONV USING   MED-WMEDAREA                                 
034600       MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                 
034700     END-IF                                                               
034800     .                                                                    
034900     EJECT                                                                
035000 C-FIRST-PAGE SECTION.                                                    
035100                                                                          
035200     PERFORM MFS-ERASE-FIELD-IN                                           
035300     .                                                                    
035400     EJECT                                                                
035500 E-SAME-PAGE SECTION.                                                     
035600                                                                          
035700       IF MID-INPUT = ALL '+'                                             
035800         PERFORM MFS-ERASE-FIELD-IN                                       
035900       ELSE                                                               
036000         MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                              
036100         CALL WMEDKONV  USING MED-WMEDAREA                                
036200         MOVE MED-MFSFEL   TO MOD-TEMFSFEL                                
036300         PERFORM MFS-READ-IN-AGAIN                                        
036400         PERFORM EA-MID-INDATA-TILL-MOD                                   
036500       END-IF                                                             
036600                                                                          
036701     IF SAVE-IDTRANS = '5137' OR '0551'                                   
036802       IF SAVE-IDFKNGRP-ENTER  NUMERIC                                    
036902         MOVE SAVE-IDFKNGRP-ENTER  TO W-IDFKNGRP-MIN                      
037002                                      W-IDFKNGRP                          
037100       ELSE                                                               
037202         MOVE LOW-VALUE          TO W-IDFKNGRP-MIN-X                      
037302         MOVE HIGH-VALUE         TO W-IDFKNGRP-MAX-X                      
037400       END-IF                                                             
037500     ELSE                                                                 
037600         PERFORM MFS-ERASE-FIELD-IN                                       
037700     END-IF                                                               
037800     .                                                                    
037900     EJECT                                                                
038000 D-NEXT-PAGE SECTION.                                                     
038100                                                                          
038201     IF SAVE-IDTRANS = '5137'                                             
038302       IF SAVE-IDFKNGRP-NEXT  NUMERIC                                     
038402         MOVE SAVE-IDFKNGRP-NEXT  TO W-IDFKNGRP-MIN                       
038502                                     W-IDFKNGRP                           
038600       ELSE                                                               
038702         MOVE ZERO               TO W-IDFKNGRP-MIN                        
038802                                     W-IDFKNGRP                           
038900                                                                          
039000       END-IF                                                             
039100     ELSE                                                                 
039200       PERFORM MFS-ERASE-FIELD-IN                                         
039300     END-IF                                                               
039400                                                                          
039500     .                                                                    
039600     EJECT                                                                
039700 EA-MID-INDATA-TILL-MOD SECTION.                                          
039800                                                                          
039902     IF MID-IDFKNGRP = ALL '+'                                            
040002       MOVE MFS-ERASE-FIELD         TO MOD-IDFKNGRP-UPD                   
040100     ELSE                                                                 
040202       MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-IDFKNGRP-UPD                   
040300     END-IF                                                               
040400                                                                          
040502     IF MID-RELANDCO-EOCF-FROM = ALL '+'                                  
040602       MOVE MFS-ERASE-FIELD        TO MOD-RELANDCO-EOCF-FROM-UPD          
040700     ELSE                                                                 
040802       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-RELANDCO-EOCF-FROM-UPD          
040900     END-IF                                                               
041002                                                                          
041102     IF MID-RELANDCO-EITX-FROM = ALL '+'                                  
041202       MOVE MFS-ERASE-FIELD        TO MOD-RELANDCO-EITX-FROM-UPD          
041302     ELSE                                                                 
041402       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-RELANDCO-EITX-FROM-UPD          
041502     END-IF                                                               
041602                                                                          
041702     IF MID-RELANDCO-EGTX-FROM = ALL '+'                                  
041802       MOVE MFS-ERASE-FIELD        TO MOD-RELANDCO-EGTX-FROM-UPD          
041902     ELSE                                                                 
042002       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-RELANDCO-EGTX-FROM-UPD          
042102     END-IF                                                               
042202                                                                          
042302     IF MID-TILANDCO = ALL '+'                                            
042402       MOVE MFS-ERASE-FIELD        TO MOD-TILANDCO-UPD                    
042502     ELSE                                                                 
042602       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-TILANDCO-UPD                    
042702     END-IF                                                               
042802                                                                          
042902     IF MID-KDCMD  = ALL '+'                                              
043002       MOVE MFS-ERASE-FIELD         TO MOD-KDCMD-UPD                      
043102     ELSE                                                                 
043202       MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-KDCMD-UPD                      
043302     END-IF                                                               
043402     .                                                                    
043502     EJECT                                                                
043602                                                                          
043702 F-READ-SHOW-INFO SECTION.                                                
043802                                                                          
043902     PERFORM IMS-GU-WDB617                                                
044002     PERFORM IMS-GHNP-WDB622                                              
044102     MOVE +1                     TO INDX                                  
044202     PERFORM UNTIL INDX > MAX-INDX                                        
044302       IF SEGMENT-FOUND                                                   
044402         IF INDX = 1                                                      
044502            MOVE FGAD-IDFKNGRP       TO SAVE-IDFKNGRP-ENTER               
044602         END-IF                                                           
044702         MOVE FGAD-IDFKNGRP          TO WS-FGAD-IDFKNGRP                  
044802         MOVE WS-FGAD-IDFKNGRP(2:4)  TO MOD-IDFKNGRP(INDX)                
044902         COMPUTE WS-RELANDCO-OFROM  =                                     
045002                  (FGAD-RELANDCO-EOCF-FROM) * 100                         
045102         MOVE WS-RELANDCO-OFROM       TO                                  
045103                                   MOD-RELANDCO-EOCF-FROM(INDX)           
045202         COMPUTE WS-RELANDCO-OTO   =                                      
045302                  (FGAD-RELANDCO-EOCF-TO  ) * 100                         
045403         COMPUTE WS-RELANDCO-IFROM =                                      
045404                  (FGAD-RELANDCO-EITX-FROM) * 100                         
045405         MOVE WS-RELANDCO-IFROM TO MOD-RELANDCO-EITX-FROM(INDX)           
045406         COMPUTE WS-RELANDCO-ITO   =                                      
045407                  (FGAD-RELANDCO-EITX-TO  ) * 100                         
045408         MOVE WS-RELANDCO-ITO TO MOD-RELANDCO-EITX-TO(INDX)               
045409         COMPUTE WS-RELANDCO-GFROM =                                      
045410                  (FGAD-RELANDCO-EGTX-FROM) * 100                         
045411         MOVE WS-RELANDCO-GFROM TO MOD-RELANDCO-EGTX-FROM(INDX)           
045412         COMPUTE WS-RELANDCO-GTO   =                                      
045413                  (FGAD-RELANDCO-EGTX-TO  ) * 100                         
045420         MOVE WS-RELANDCO-GTO TO MOD-RELANDCO-EGTX-TO(INDX)               
045440         COMPUTE WS-RELANDCO-FFROM = FGAD-RELANDCO-FG-FROM * 100          
045441                                                                          
045450         MOVE WS-RELANDCO-FFROM     TO MOD-RELANDCO-FG-FROM(INDX)         
045502         MOVE FGAD-TILANDCO          TO MOD-TILANDCO(INDX)                
045503         MOVE FGAD-IDUSER            TO MOD-IDUSER(INDX)                  
045504         MOVE FGAD-TIUPPDAT          TO MOD-TIUPPDAT(INDX)                
045505                                                                          
045802         PERFORM IMS-GHNP-WDB622                                          
045902       ELSE                                                               
046002         PERFORM MFS-ERASE-FIELD-OUT                                      
046102       END-IF                                                             
046202       ADD  +1 TO INDX                                                    
046302     END-PERFORM                                                          
046402       IF SEGMENT-FOUND                                                   
046502         MOVE FGAD-IDFKNGRP     TO SAVE-IDFKNGRP-NEXT                     
046602         MOVE INF-MORE-LINE TO MED-IDMFSINF                               
046702         CALL WMEDKONV USING MED-WMEDAREA                                 
046802         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
046902       ELSE                                                               
047002         MOVE LAST-PAGE TO MED-IDMFSINF                                   
047102         CALL WMEDKONV USING MED-WMEDAREA                                 
047202         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
047302       END-IF                                                             
047402       MOVE '002' TO MSGI-KDCALL                                          
047502       MOVE '5137' TO SAVE-IDTRANS                                        
047602       MOVE SAVE-AREA TO MSGI-SPAR-AREA                                   
047702       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
047802     .                                                                    
047902     EJECT                                                                
048002 G-CHECK-INPUT SECTION.                                                   
048102                                                                          
048202     MOVE YES  TO INDATA-SW                                               
048302       IF  (MID-INPUT     = ALL '+' OR SPACES)                            
048402       AND (MID-KDCMD     = ALL '+' OR SPACES)                            
048502          MOVE ERR-PF11-AND-NO-DATA  TO MED-IDMFSFEL                      
048602          CALL WMEDKONV           USING MED-WMEDAREA                      
048702          MOVE MED-TEMFSFEL          TO MOD-TEMFSFEL                      
048802          PERFORM MFS-ERASE-FIELD-IN                                      
048902          PERFORM MFS-ERASE-FIELD-UPD                                     
049002          MOVE NOO                   TO INDATA-SW                         
049102       ELSE                                                               
049220        IF MID-KDCMD  = ALL '+' OR SPACES                                 
049302          PERFORM GD-CHECK-IDFKNGRP                                       
049402          PERFORM GA-CHECK-RELANDCO                                       
049502          PERFORM GD-CHECK-TILANDCO                                       
049602        ELSE                                                              
049702         IF MID-KDCMD = 'D'                                               
049802           MOVE MFS-NUM-FIELD-OK    TO MOD-KDCMD-UPD-ATTR                 
049902           PERFORM GE-CHECK-IDFKNGRP                                      
050002         ELSE                                                             
050102           MOVE MFS-NUM-FIELD-WRONG TO MOD-KDCMD-UPD-ATTR                 
050202           MOVE MFS-NUM-FIELD-WRONG TO MOD-IDFKNGRP-UPD-ATTR              
050302           MOVE MID-IDFKNGRP        TO W-IDFKNGRP                         
050402           MOVE NOO               TO INDATA-SW                            
050502         END-IF                                                           
050602        END-IF                                                            
050702          IF INDATA-OK                                                    
050802             CONTINUE                                                     
050902          ELSE                                                            
051002           IF MED-TEMFSFEL = ERR-FUTURE-DATE                              
051102              MOVE ERR-FUTURE-DATE        TO MED-IDMFSFEL                 
051202           END-IF                                                         
051302                                                                          
051402           IF MED-TEMFSFEL NOT = ERR-FUTURE-DATE                          
051502             MOVE ERR-CORR-HILITE-FLDS   TO MED-IDMFSFEL                  
051602           END-IF                                                         
051702            CALL WMEDKONV      USING MED-WMEDAREA                         
051802            MOVE MED-TEMFSFEL  TO MOD-TEMFSFEL                            
051902            PERFORM MFS-DONT-TOUCH-FIELD-OUT                              
052002            PERFORM MFS-DONT-TOUCH-FIELD-IN                               
052102          END-IF                                                          
052202       END-IF                                                             
052302     .                                                                    
052402     EJECT                                                                
052502 GD-CHECK-IDFKNGRP SECTION.                                               
052602                                                                          
052702     IF MID-IDFKNGRP NOT = ALL '+'                                        
052802     AND (MID-KDCMD  = ALL '+' OR SPACES)                                 
052902       INSPECT MID-IDFKNGRP REPLACING LEADING SPACE                       
053002                               BY ZERO                                    
053102       IF MID-IDFKNGRP NUMERIC                                            
053202         MOVE MFS-NUM-FIELD-OK    TO MOD-IDFKNGRP-UPD-ATTR                
053302       ELSE                                                               
053402         MOVE MFS-NUM-FIELD-WRONG TO MOD-IDFKNGRP-UPD-ATTR                
053502         MOVE NOO                 TO INDATA-SW                            
053602       END-IF                                                             
053702     END-IF                                                               
053802     .                                                                    
053902     EJECT                                                                
054002 GE-CHECK-IDFKNGRP SECTION.                                               
054102                                                                          
054202     IF MID-IDFKNGRP NOT = ALL '+'                                        
054302       INSPECT MID-IDFKNGRP REPLACING LEADING SPACE                       
054402                               BY ZERO                                    
054502       IF MID-IDFKNGRP NUMERIC                                            
054602         MOVE MFS-NUM-FIELD-OK    TO MOD-IDFKNGRP-UPD-ATTR                
054702       ELSE                                                               
054802         MOVE MFS-NUM-FIELD-WRONG TO MOD-IDFKNGRP-UPD-ATTR                
054902         MOVE NOO                 TO INDATA-SW                            
055002       END-IF                                                             
055102     END-IF                                                               
055202     .                                                                    
055302     EJECT                                                                
055402 GA-CHECK-RELANDCO SECTION.                                               
055502                                                                          
055602     IF MID-RELANDCO-EOCF-FROM NOT = ALL '+'  AND                         
055603        MID-KDCMD = SPACES                                                
055702       MOVE MFS-NUM-FIELD-OK    TO MOD-RELANDCO-EOF-FROM-UPD-ATTR         
055802     ELSE                                                                 
055902       INSPECT MID-RELANDCO-EOCF-FROM TALLYING WS-CNT                     
056002        FOR ALL '-'                                                       
056102        IF WS-CNT > 0                                                     
056202          MOVE MFS-NUM-FIELD-WRONG TO                                     
056203                                  MOD-RELANDCO-EOF-FROM-UPD-ATTR          
056302          MOVE NOO                    TO INDATA-SW                        
056402        ELSE                                                              
056502          MOVE MID-RELANDCO-EOCF-FROM TO WS-IDFRIDATA                     
056602          MOVE WS-IDFRIDATA           TO DEC-IDFRIDATA                    
056702          MOVE 3                      TO DEC-KVHELTAL                     
056802          MOVE 1                      TO DEC-KVDECIMAL                    
056902           CALL WDECEDIT            USING DEC-WDECAREA                    
057002                                                                          
057102           IF DEC-KDSVAR-OK                                               
057202            MOVE DEC-IDEDITDATA      TO WS-RELANDCO-FFROM                 
057302          MOVE MFS-NUM-FIELD-OK TO MOD-RELANDCO-EOF-FROM-UPD-ATTR         
057402           ELSE                                                           
057502            MOVE MFS-NUM-FIELD-WRONG TO                                   
057602                                  MOD-RELANDCO-EOF-FROM-UPD-ATTR          
057702             MOVE NOO                TO INDATA-SW                         
057802           END-IF                                                         
057902        END-IF                                                            
058002     END-IF                                                               
058102                                                                          
058202     IF MID-RELANDCO-EITX-FROM NOT = ALL '+' AND                          
058302        MID-KDCMD = SPACES                                                
058402       MOVE MFS-NUM-FIELD-OK   TO MOD-RELANDCO-EIX-FROM-UPD-ATTR          
058502     ELSE                                                                 
058602       INSPECT MID-RELANDCO-EITX-FROM TALLYING WS-CNT                     
058702        FOR ALL '-'                                                       
058802        IF WS-CNT > 0                                                     
058902         MOVE MFS-NUM-FIELD-WRONG TO                                      
059002                                  MOD-RELANDCO-EIX-FROM-UPD-ATTR          
059102          MOVE NOO                    TO INDATA-SW                        
059202        ELSE                                                              
059302          MOVE MID-RELANDCO-EITX-FROM TO WS-IDFRIDATA                     
059402          MOVE WS-IDFRIDATA           TO DEC-IDFRIDATA                    
059502          MOVE 3                      TO DEC-KVHELTAL                     
059602          MOVE 1                      TO DEC-KVDECIMAL                    
059702           CALL WDECEDIT            USING DEC-WDECAREA                    
059802                                                                          
059902           IF DEC-KDSVAR-OK                                               
060002            MOVE DEC-IDEDITDATA      TO WS-RELANDCO-IFROM                 
060102            MOVE MFS-NUM-FIELD-OK TO                                      
060202                                  MOD-RELANDCO-EIX-FROM-UPD-ATTR          
060302           ELSE                                                           
060402            MOVE MFS-NUM-FIELD-WRONG TO                                   
060502                                  MOD-RELANDCO-EIX-FROM-UPD-ATTR          
060602             MOVE NOO                TO INDATA-SW                         
060702           END-IF                                                         
060802        END-IF                                                            
060902     END-IF                                                               
061002                                                                          
061102     IF MID-RELANDCO-EGTX-FROM NOT = ALL '+' AND                          
061202        MID-KDCMD = SPACES                                                
061302       MOVE MFS-NUM-FIELD-OK    TO MOD-RELANDCO-EGX-FROM-UPD-ATTR         
061402     ELSE                                                                 
061502       INSPECT MID-RELANDCO-EGTX-FROM TALLYING WS-CNT                     
061602        FOR ALL '-'                                                       
061702        IF WS-CNT > 0                                                     
061802         MOVE MFS-NUM-FIELD-WRONG TO                                      
061902                                   MOD-RELANDCO-EGX-FROM-UPD-ATTR         
062002          MOVE NOO                    TO INDATA-SW                        
062102        ELSE                                                              
062202          MOVE MID-RELANDCO-EGTX-FROM TO WS-IDFRIDATA                     
062302          MOVE WS-IDFRIDATA           TO DEC-IDFRIDATA                    
062402          MOVE 3                      TO DEC-KVHELTAL                     
062502          MOVE 1                      TO DEC-KVDECIMAL                    
062602           CALL WDECEDIT            USING DEC-WDECAREA                    
062702                                                                          
062802           IF DEC-KDSVAR-OK                                               
062902            MOVE DEC-IDEDITDATA      TO WS-RELANDCO-GFROM                 
063002            MOVE MFS-NUM-FIELD-OK TO                                      
063102                                   MOD-RELANDCO-EGX-FROM-UPD-ATTR         
063202           ELSE                                                           
063302            MOVE MFS-NUM-FIELD-WRONG TO                                   
063402                                   MOD-RELANDCO-EGX-FROM-UPD-ATTR         
063502             MOVE NOO                TO INDATA-SW                         
063602           END-IF                                                         
063702        END-IF                                                            
063802     END-IF                                                               
063902     .                                                                    
064002     EJECT                                                                
064102 GD-CHECK-TILANDCO SECTION.                                               
064202                                                                          
064302     IF MID-TILANDCO NOT = ALL '+'                                        
064402     AND (MID-KDCMD  = ALL '+' OR SPACES)                                 
064502       INSPECT MID-TILANDCO REPLACING LEADING SPACE                       
064602                                   BY ZERO                                
064702       IF MID-TILANDCO NUMERIC                                            
064802         MOVE MID-TILANDCO          TO WS-START-DATE                      
064902         MOVE WORK-DATE               TO DATE-TIDATE                      
065002         MOVE 'YYMMDD'                TO DATE-KDDATFMT                    
065102         CALL WZ20DATE USING DATE-WZ20DATE                                
065202          IF DATE-KDRC = 0                                                
065302            IF WS-START-DATE    <  DATE-TIDATE                            
065402              MOVE ERR-FUTURE-DATE     TO MED-TEMFSFEL                    
065502              MOVE MFS-NUM-FIELD-WRONG TO MOD-TILANDCO-UPD-ATTR           
065602              MOVE NOO                 TO INDATA-SW                       
065702            ELSE                                                          
065802              MOVE MFS-NUM-FIELD-OK    TO MOD-TILANDCO-UPD-ATTR           
065902            END-IF                                                        
066002          END-IF                                                          
066102       ELSE                                                               
066202         MOVE MFS-NUM-FIELD-WRONG TO MOD-TILANDCO-UPD-ATTR                
066302         MOVE NOO                 TO INDATA-SW                            
066402       END-IF                                                             
066502     END-IF                                                               
066602     .                                                                    
066702     EJECT                                                                
066802                                                                          
066902 H-UPDATE SECTION.                                                        
067002                                                                          
067102     MOVE WC-NDC-TW   TO W-IDDC                                           
067202     MOVE MID-IDFKNGRP     TO W-IDFKNGRP                                  
067302     PERFORM IMS-GHU-WDB622                                               
067402     IF SEGMENT-FOUND                                                     
067502       IF MID-KDCMD  = 'D' AND MID-IDFKNGRP NOT = ALL '+'                 
067602        PERFORM IMS-DLET-WDB622                                           
067702       ELSE                                                               
067802        PERFORM HB-MOVE-FIELDS                                            
067902        PERFORM IMS-REPL-WDB622                                           
068002       END-IF                                                             
068102     ELSE                                                                 
068202       IF  (MID-KDCMD  = ALL '+' OR SPACES)                               
068302        PERFORM HA-INIT-FIELDS                                            
068402        PERFORM HB-MOVE-FIELDS-ISRT                                       
068502        PERFORM IMS-ISRT-WDB622                                           
068602       END-IF                                                             
068702     END-IF                                                               
068802     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
068902     CALL WMEDKONV USING MED-WMEDAREA                                     
069002     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
069102     PERFORM MFS-FORM-ATTR                                                
069202     PERFORM MFS-ERASE-FIELD-IN                                           
069302     .                                                                    
069402     EJECT                                                                
069502 HA-INIT-FIELDS SECTION.                                                  
069602     INITIALIZE    FGAD-RELANDCO-FG-TO                                    
069702                   FGAD-TILANDCO                                          
069802                   FGAD-TIUPPDAT                                          
069902                   FGAD-IDFKNGRP                                          
070002                   FGAD-IDUSER                                            
070102                   FGAD-RELANDCO-FG-FROM                                  
070103                   FGAD-RELANDCO-EGTX-FROM                                
070104                   FGAD-RELANDCO-EGTX-TO                                  
070105                   FGAD-RELANDCO-EOCF-FROM                                
070106                   FGAD-RELANDCO-EOCF-TO                                  
070202     MOVE TODAYS-DATE            TO FGAD-TIUPPDAT                         
070302     MOVE MSGI-IDUSER            TO FGAD-IDUSER                           
070402     .                                                                    
070502     EJECT                                                                
070602 HB-MOVE-FIELDS-ISRT SECTION.                                             
070702                                                                          
070802     IF MID-IDFKNGRP NOT = ALL '+'                                        
070902       MOVE MID-IDFKNGRP           TO FGAD-IDFKNGRP                       
071002                                      W-IDFKNGRP                          
071102     END-IF                                                               
071202                                                                          
071302     IF MID-TILANDCO NOT = ALL '+'                                        
071402       MOVE MID-TILANDCO           TO FGAD-TILANDCO                       
071502     ELSE                                                                 
071602       MOVE TODAYS-DATE            TO FGAD-TILANDCO                       
071702     END-IF                                                               
071802                                                                          
071902     PERFORM IMS-GU-WDB617                                                
072002     IF SEGMENT-FOUND                                                     
072102       IF MID-RELANDCO-EOCF-FROM NOT = ALL '+'                            
072302         COMPUTE WS-RELANDCO-OFROM-PERC =                                 
072402                                    WS-RELANDCO-FFROM / 100               
072502         MOVE WS-RELANDCO-OFROM-PERC TO FGAD-RELANDCO-EOCF-FROM           
072602                                          FGAD-RELANDCO-EOCF-TO           
072702       END-IF                                                             
072803                                                                          
072903     IF MID-RELANDCO-EITX-FROM NOT = ALL '+'                              
073003     COMPUTE WS-RELANDCO-IFROM-PERC = WS-RELANDCO-IFROM / 100             
073103       MOVE WS-RELANDCO-IFROM-PERC TO FGAD-RELANDCO-EITX-FROM             
073203                                     FGAD-RELANDCO-EITX-TO                
073303     END-IF                                                               
073403                                                                          
073503     IF MID-RELANDCO-EGTX-FROM NOT = ALL '+'                              
073603     COMPUTE WS-RELANDCO-GFROM-PERC = WS-RELANDCO-GFROM / 100             
073703       MOVE WS-RELANDCO-GFROM-PERC TO FGAD-RELANDCO-EGTX-FROM             
073803                                     FGAD-RELANDCO-EGTX-TO                
073903     END-IF                                                               
074003                                                                          
074803     COMPUTE WS-RELANDCO-OPERC ROUNDED= WS-RELANDCO-OFROM-PERC + 1        
075003     COMPUTE WS-RELANDCO-IFROM-PERC1 ROUNDED =                            
075103             (WS-RELANDCO-OPERC * WS-RELANDCO-IFROM-PERC) +               
075104              WS-RELANDCO-OPERC                                           
075403     COMPUTE WS-RELANDCO-GPERC ROUNDED =                                  
075503            (WS-RELANDCO-IFROM-PERC1 * WS-RELANDCO-GFROM-PERC) +          
075504             WS-RELANDCO-IFROM-PERC1                                      
075603     COMPUTE WS-RELANDCO-TOTAL ROUNDED =                                  
075604                            (WS-RELANDCO-GPERC * 100) - 100               
075605     COMPUTE WS-RELANDCO-FG-TOTAL ROUNDED= WS-RELANDCO-TOTAL / 100        
075803     MOVE    WS-RELANDCO-FG-TOTAL   TO FGAD-RELANDCO-FG-FROM              
075804                                       FGAD-RELANDCO-FG-TO                
076002     END-IF                                                               
076102     MOVE TODAYS-DATE            TO FGAD-TIUPPDAT                         
076202     MOVE MSGI-IDUSER            TO FGAD-IDUSER                           
076302     .                                                                    
076402     EJECT                                                                
076502 HB-MOVE-FIELDS SECTION.                                                  
076602                                                                          
076702     IF MID-IDFKNGRP NOT = ALL '+'                                        
076802       MOVE MID-IDFKNGRP          TO FGAD-IDFKNGRP                        
076902                                     W-IDFKNGRP                           
077002     END-IF                                                               
077102                                                                          
077103     IF MID-RELANDCO-EOCF-FROM NOT = ALL '+'                              
077104      COMPUTE WS-RELANDCO-OFROM-PERC = WS-RELANDCO-FFROM / 100            
077106       MOVE FGAD-RELANDCO-EOCF-FROM TO FGAD-RELANDCO-EOCF-TO              
077107       MOVE WS-RELANDCO-OFROM-PERC TO FGAD-RELANDCO-EOCF-FROM             
077110     END-IF                                                               
077120                                                                          
077203     IF MID-RELANDCO-EITX-FROM NOT = ALL '+'                              
077303     COMPUTE WS-RELANDCO-IFROM-PERC = WS-RELANDCO-IFROM /      100        
077603       MOVE FGAD-RELANDCO-EITX-FROM TO FGAD-RELANDCO-EITX-TO              
077703       MOVE WS-RELANDCO-IFROM-PERC TO FGAD-RELANDCO-EITX-FROM             
077803     END-IF                                                               
077903                                                                          
078003     IF MID-RELANDCO-EGTX-FROM NOT = ALL '+'                              
078103     COMPUTE WS-RELANDCO-GFROM-PERC = WS-RELANDCO-GFROM /      100        
078403       MOVE FGAD-RELANDCO-EGTX-FROM TO FGAD-RELANDCO-EGTX-TO              
078503       MOVE WS-RELANDCO-GFROM-PERC TO FGAD-RELANDCO-EGTX-FROM             
078603     END-IF                                                               
078703                                                                          
078704     COMPUTE WS-RELANDCO-OPERC ROUNDED= WS-RELANDCO-OFROM-PERC + 1        
078705     COMPUTE WS-RELANDCO-IFROM-PERC1 ROUNDED =                            
078706             (WS-RELANDCO-OPERC * WS-RELANDCO-IFROM-PERC) +               
078707              WS-RELANDCO-OPERC                                           
078708     COMPUTE WS-RELANDCO-GPERC ROUNDED =                                  
078709            (WS-RELANDCO-IFROM-PERC1 * WS-RELANDCO-GFROM-PERC) +          
078710             WS-RELANDCO-IFROM-PERC1                                      
078711     COMPUTE WS-RELANDCO-TOTAL ROUNDED =                                  
078712                            (WS-RELANDCO-GPERC * 100) - 100               
078713     COMPUTE WS-RELANDCO-FG-TOTAL ROUNDED= WS-RELANDCO-TOTAL / 100        
078719     MOVE    WS-RELANDCO-FG-TOTAL   TO FGAD-RELANDCO-FG-FROM              
078720                                       FGAD-RELANDCO-FG-TO                
078780                                                                          
079302     IF MID-TILANDCO NOT = ALL '+'                                        
079402       MOVE MID-TILANDCO           TO FGAD-TILANDCO                       
079502     ELSE                                                                 
079602       MOVE TODAYS-DATE            TO FGAD-TILANDCO                       
079702     END-IF                                                               
079802                                                                          
079902     MOVE TODAYS-DATE            TO FGAD-TIUPPDAT                         
080002     MOVE MSGI-IDUSER            TO FGAD-IDUSER                           
080102     .                                                                    
080202     EJECT                                                                
080302 MFS-ERASE-FIELD-UPD SECTION.                                             
080402                                                                          
080502*    --- ALLA UTDATA-FÄLT                                                 
080602     MOVE MFS-ERASE-FIELD TO MOD-IDFKNGRP-UPD                             
080702                             MOD-RELANDCO-EOCF-FROM-UPD                   
080703                             MOD-RELANDCO-EITX-FROM-UPD                   
080704                             MOD-RELANDCO-EGTX-FROM-UPD                   
080802                             MOD-TILANDCO-UPD                             
080902                             MOD-IDFKNGRP-UT                              
081002                                                                          
081102     .                                                                    
081202     SKIP3                                                                
081302 MFS-ERASE-FIELD-OUT SECTION.                                             
081402     MOVE MFS-ERASE-FIELD   TO MOD-RELANDCO-FG-FROM(INDX)                 
081502                               MOD-TILANDCO(INDX)                         
081602                               MOD-IDFKNGRP(INDX)                         
081903                               MOD-RELANDCO-EITX-FROM(INDX)               
081904                               MOD-RELANDCO-EGTX-FROM(INDX)               
081905                               MOD-RELANDCO-EITX-TO(INDX)                 
081906                               MOD-RELANDCO-EGTX-TO(INDX)                 
081907                               MOD-RELANDCO-EOCF-FROM(INDX)               
081908                               MOD-IDUSER(INDX)                           
082002     .                                                                    
082102 MFS-ERASE-FIELD-IN SECTION.                                              
082202                                                                          
082302     MOVE ZERO TO INDX                                                    
082402     IF INDX > MAX-INDX                                                   
082502       PERFORM UNTIL INDX > MAX-INDX                                      
082602         MOVE MFS-ERASE-FIELD   TO MOD-RELANDCO-FG-FROM(INDX)             
082702                                   MOD-TILANDCO(INDX)                     
082802                                   MOD-IDFKNGRP(INDX)                     
083102                                   MOD-RELANDCO-EOCF-FROM(INDX)           
083103                                   MOD-RELANDCO-EITX-FROM(INDX)           
083104                                   MOD-RELANDCO-EGTX-FROM(INDX)           
083106                                   MOD-RELANDCO-EGTX-TO(INDX)             
083107                                   MOD-IDUSER(INDX)                       
083202         ADD +1 TO INDX                                                   
083302       END-PERFORM                                                        
083402     END-IF                                                               
083502     .                                                                    
083602     EJECT                                                                
083702 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
083802                                                                          
083902*    --- ALLA UTDATA-FÄLT                                                 
084002     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDFKNGRP-UPD                      
084102                                    MOD-IDFKNGRP-UT                       
084202                                    MOD-RELANDCO-EOCF-FROM-UPD            
084203                                    MOD-RELANDCO-EITX-FROM-UPD            
084204                                    MOD-RELANDCO-EGTX-FROM-UPD            
084302                                    MOD-TILANDCO-UPD                      
084402                                    MOD-KDCMD-UPD                         
084502     .                                                                    
084602     SKIP3                                                                
084702 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
084802                                                                          
084902*    --- ALLA INDATA-FÄLT                                                 
085002     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDFKNGRP-UPD                      
085102                                    MOD-KDCMD-UPD                         
085202                                    MOD-RELANDCO-EOCF-FROM-UPD            
085203                                    MOD-RELANDCO-EITX-FROM-UPD            
085204                                    MOD-RELANDCO-EGTX-FROM-UPD            
085302                                    MOD-TILANDCO-UPD                      
085402     .                                                                    
085502     EJECT                                                                
085602 MFS-FORM-ATTR SECTION.                                                   
085702                                                                          
085802*    --- ALL INDATA-FIELDS                                                
085902     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-IDFKNGRP-UPD-ATTR                
086002                                   MOD-RELANDCO-EOF-FROM-UPD-ATTR         
086003                                  MOD-RELANDCO-EIX-FROM-UPD-ATTR          
086004                                  MOD-RELANDCO-EGX-FROM-UPD-ATTR          
086102                                     MOD-TILANDCO-UPD-ATTR                
086202                                     MOD-KDCMD-UPD-ATTR                   
086302     .                                                                    
086402     SKIP2                                                                
086502* --- IMS SECTIONS ---                                                    
086602     SKIP3                                                                
086702 MFS-READ-IN-AGAIN SECTION.                                               
086802                                                                          
086902*    --- ALL INDATA-FIELDS                                                
087002     MOVE MFS-ADD-READ-FIELD TO MOD-IDFKNGRP-UPD-ATTR                     
087102                                MOD-RELANDCO-EOF-FROM-UPD-ATTR            
087103                                MOD-RELANDCO-EIX-FROM-UPD-ATTR            
087104                                MOD-RELANDCO-EGX-FROM-UPD-ATTR            
087202                                MOD-TILANDCO-UPD-ATTR                     
087302                                MOD-KDCMD-UPD-ATTR                        
087402     .                                                                    
087502     EJECT                                                                
087602* --- IMS SECTIONS ---                                                    
087702     SKIP3                                                                
087802 IMS-GET-MSG SECTION.                                                     
087902                                                                          
088002     MOVE '  QC' TO GOOD-STATUSCODES                                      
088102     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
088202     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
088302     PERFORM IMS-STATUSCHECK                                              
088402     .                                                                    
088502     SKIP3                                                                
088602 IMS-INSERT-MSG SECTION.                                                  
088702                                                                          
088802     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
088902     MOVE SPACE TO GOOD-STATUSCODES                                       
089002     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
089102     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
089202     PERFORM IMS-STATUSCHECK                                              
089302     .                                                                    
089402     EJECT                                                                
089502 IMS-GU-WDB617 SECTION.                                                   
089602                                                                          
089702     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
089802          DELIMITED BY SIZE INTO SSA1                                     
089902     STRING 'WDB617  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
090002          DELIMITED BY SIZE INTO SSA2                                     
090102     MOVE '    ' TO GOOD-STATUSCODES                                      
090202     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB617 SSA1 SSA2               
090302     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
090402     PERFORM IMS-STATUSCHECK                                              
090502     .                                                                    
090602     SKIP3                                                                
090702 IMS-GHNP-WDB622 SECTION.                                                 
090802                                                                          
090902     STRING 'WDB622  (IDFKNGRP>=' W-IDFKNGRP-MIN-X                        
091002                    '&IDFKNGRP<=' W-IDFKNGRP-MAX-X ')'                    
091102          DELIMITED BY SIZE INTO SSA1                                     
091202     MOVE '  GE' TO GOOD-STATUSCODES                                      
091302     CALL CBLTDLI USING GHNP WDB6-PCB DLI-IO-WDB622 SSA1                  
091402     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
091502     PERFORM IMS-STATUSCHECK                                              
091602     .                                                                    
091702     SKIP3                                                                
091802 IMS-ISRT-WDB622 SECTION.                                                 
091902                                                                          
092002     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
092102          DELIMITED BY SIZE INTO SSA1                                     
092202     STRING 'WDB617  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
092302          DELIMITED BY SIZE INTO SSA2                                     
092402     MOVE 'WDB622  '   TO SSA3                                            
092502     MOVE '  ' TO GOOD-STATUSCODES                                        
092602     CALL CBLTDLI USING ISRT WDB6-PCB DLI-IO-WDB622 SSA1 SSA2 SSA3        
092702     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
092802     PERFORM IMS-STATUSCHECK                                              
092902     .                                                                    
093002     SKIP3                                                                
093102 IMS-GHU-WDB622 SECTION.                                                  
093202                                                                          
093302     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
093402          DELIMITED BY SIZE INTO SSA1                                     
093502     STRING 'WDB617  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
093602          DELIMITED BY SIZE INTO SSA2                                     
093702     STRING 'WDB622  (IDFKNGRP =' W-IDFKNGRP-X ')'                        
093802          DELIMITED BY SIZE INTO SSA3                                     
093902     MOVE '  GE' TO GOOD-STATUSCODES                                      
094002     CALL CBLTDLI USING GHU WDB6-PCB DLI-IO-WDB622 SSA1 SSA2 SSA3         
094102     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
094202     PERFORM IMS-STATUSCHECK                                              
094302     .                                                                    
094402     SKIP3                                                                
094502 IMS-REPL-WDB622 SECTION.                                                 
094602                                                                          
094702     MOVE '  ' TO GOOD-STATUSCODES                                        
094802     CALL CBLTDLI USING REPL WDB6-PCB DLI-IO-WDB622                       
094902     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
095002     PERFORM IMS-STATUSCHECK                                              
095102     .                                                                    
095202     SKIP3                                                                
095302 IMS-DLET-WDB622 SECTION.                                                 
095402                                                                          
095502     MOVE '   ' TO GOOD-STATUSCODES                                       
095602     CALL CBLTDLI USING DLET WDB6-PCB DLI-IO-WDB622                       
095702     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
095802     PERFORM IMS-STATUSCHECK                                              
095902     .                                                                    
096002     SKIP3                                                                
096102 IMS-STATUSCHECK SECTION.                                                 
096202                                                                          
096302     SET STATUS-IX TO 1                                                   
096402     SEARCH GOOD-STATUS                                                   
096502       AT END                                                             
096602         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
096702         DELIMITED BY SIZE INTO ERROR-TEXT                                
096802         CALL FELLOG                                                      
096902       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
097002         CONTINUE                                                         
098002     END-SEARCH                                                           
100002     .                                                                    
