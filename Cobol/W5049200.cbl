000100 PROCESS DYNAM                                                            
001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     W5049200.                                                
001400 AUTHOR.         HENRIKSSON ANDERS.                                       
001500 DATE-WRITTEN.   14/05/11.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001710*    NAME                                                                 
001711*        CARPARTS.PULS.RECEIVEV                                           
001800*    FUNCTION:                                                            
001900*        STARTS BY COMMUNICATION REGISTER VIA WZ01SEND.                   
001910*        READS ALL ROWS IN SENT DATA VIA WZ01RECV.                        
001920*        BUILD UP ROWS IN T01IVW.                                         
002400*                                                                         
002410*        THE PROGRAM UPDATES TABLE T01IVW                                 
002500*                                                                         
002600*    INDATA.                                                              
002700*        TRANSAKTION: W50492X                                             
002810*        REQUEST:     WF2103I1 VAT                                        
002900*                                                                         
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
004200 DATA DIVISION.                                                           
004700 WORKING-STORAGE SECTION.                                                 
004800 77  IDPGM                       PIC X(08)   VALUE 'W5049200'.            
004900                                                                          
005000*    --- WORKFIELD FOR ERROR MESSAGES WHEN CALLING ABEND.                 
005100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
005200 77  KDRC-DISPLAY                PIC Z(5).                                
005300                                                                          
005400 77  YES                         PIC X       VALUE 'J'.                   
005500 77  NOO                         PIC X       VALUE 'N'.                   
005510                                                                          
005520 77  WS-IX                       PIC S9(9)  VALUE +0    COMP SYNC.        
005521 77  RADER-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005522 77  LOPNR-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005530 77  MAX-RADER                   PIC S9(9)  VALUE +50   COMP SYNC.        
005531 77  MAX-LINES                 PIC S9(9)  VALUE +80000 COMP SYNC.         
005540 77  W-ANT                       PIC S9(3)  VALUE ZERO COMP-3.            
005600                                                                          
006300 77  KEYS-SW                     PIC X      VALUE SPACE.                  
006400     88  KEYS-OK                            VALUE 'J'.                    
006500     88  KEYS-ERROR                         VALUE 'N'.                    
006501                                                                          
006502 77  DUPLICATE-SW                PIC X.                                   
006503     88  DUPLICATE-ERROR                    VALUE 'J'.                    
006504     88  DUPLICATE-OK                       VALUE 'N'.                    
006505                                                                          
006600     EJECT                                                                
006700*    --- SUBPROGRAMS OCH PARAMETER AREAS                                  
006800 01  GENERAL-SUBPROGRAMS.                                                 
007000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007100     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
007200     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
007300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007400     SKIP3                                                                
007410                                                                          
007500*    --- PARAMETERS TO ABEND                                              
007700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007810 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
007900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008000     SKIP3                                                                
008100 01  MESSAGE-CODES.                                                       
008300     03  ERR-DUPLICATE-LINES     PIC X(3)    VALUE '029'.                 
008800     EJECT                                                                
008801                                                                          
008802*    --- AREAS FOR WORK FIELDS                                            
008803 01  FILLER                      PIC X(16)   VALUE 'WDAT-CONTROL'.        
008804     SKIP3                                                                
008805 01  -COPY WDATAREA                                                       
008806     EJECT                                                                
008807 01  FILLER                      PIC X(16)   VALUE 'WDAT-AREA'.           
008808     SKIP3                                                                
008810                                                                          
008890 01  FILLER                      PIC X(16)   VALUE 'VAT-CONTROL'.         
008891     SKIP3                                                                
008892 01  -COPY W522VAT      -PRE VAT-                                         
008893     EJECT                                                                
008894 01  FILLER                      PIC X(16)   VALUE 'VAT-AREA'.            
008895     SKIP3                                                                
008896                                                                          
008900*    --- AREAS FOR COMMUNICATION                                          
009000 01  FILLER                      PIC X(16)   VALUE 'RECV-CONTROL'.        
009100     SKIP3                                                                
009200 01  -COPY WZ01RECV                                                       
009300     EJECT                                                                
009400 01  FILLER                      PIC X(16)   VALUE 'RECV-AREA'.           
009500     SKIP3                                                                
009510                                                                          
009910 01  RECV-AREA.                                                           
009920*    03  -COPY WZ01REQU -PRE IN2-                                         
009930*    03  -COPY WF2103I1 -PRE MID-WF2103I1-                                
009940     EJECT                                                                
009941                                                                          
009942 01  -COPY WZ01SEND                                                       
009943     EJECT                                                                
009944 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
009945     SKIP3                                                                
009946                                                                          
009952 01  SEND-AREA.                                                           
009953*    03  -COPY WZ01RESP                                                   
009955     EJECT                                                                
009956                                                                          
009960                                                                          
011002 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
011003       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
011004                                                                          
011005 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
011006 01  DB2-WS.                                                              
011007     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
011008         88  CURSOR-OK                       VALUE 000.                   
011009         88  LINES-FOUND                     VALUE 000.                   
011010         88  LINES-MISSING                   VALUE 100.                   
011011         88  DUPLICATE-LINES                 VALUE 811.                   
011012         88  RESOURCE-WRONG                  VALUE 904.                   
011020                                                                          
011021     03  GOOD-SQLCODECODES.                                               
011022         05  GOOD-SQLCODE OCCURS 5                                        
011030             INDEXED BY SQLCODE-IX PIC 9(3).                              
011401     EJECT                                                                
011402                                                                          
011403 01  FILLER                      PIC X(16)   VALUE 'WS-AREA'.             
011404 01  WS-AREA.                                                             
011405     03 WS-DATUM                 PIC X(8)    VALUE SPACE.                 
011407     03 WS-KLOCKAN               PIC 9(7)    VALUE ZERO.                  
011411     03 WS-IDDISTR-1             PIC 9(5)    VALUE ZERO.                  
011412     03 WS-IDKUNDNR-1            PIC 9(7)    VALUE ZERO.                  
011413     03 WS-IDARTNR-1             PIC 9(9)    VALUE ZERO.                  
011414                                                                          
011415     03 WS-DAREGDAT              PIC X(8)    VALUE SPACE.                 
011416     03 WS-DAEXDAT               PIC 9(8)    VALUE ZERO.                  
011417     03 WS-TIREGTID              PIC S9(6)   VALUE ZERO COMP-3.           
011418     03 WS-TIEXTID               PIC 9(6)    VALUE ZERO.                  
011419     03 WS-IDLOPNR               PIC S9(5)   VALUE ZERO COMP-3.           
011420     03 WS-IDPTYP                PIC X(3)    VALUE SPACE.                 
011421     03 WS-TIRP-1                PIC S9(2)   VALUE ZERO COMP-3.           
011422     03 WS-TIAA-1                PIC S9(2)   VALUE ZERO COMP-3.           
011423     03 WS-TIMM-1                PIC S9(2)   VALUE ZERO COMP-3.           
011424     03 WS-FLKLAR                PIC X(1)    VALUE 'N'.                   
011425                                                                          
011451     03 WS-VAT-DATA              PIC X(200)  VALUE SPACE.                 
011452     03 WS-VAT-DATA-2            PIC X(200)  VALUE SPACE.                 
011471                                                                          
011472 01  FILLER                      PIC X(16)   VALUE 'T01IVW-AREA'.         
011480*01  -COPY T01IVW -PRE T01IVW-                                            
011501     EJECT                                                                
011502                                                                          
011510     EXEC SQL INCLUDE T01IVW END-EXEC.                                    
011600     EJECT                                                                
011610                                                                          
011700 LINKAGE SECTION.                                                         
012101 PROCEDURE DIVISION.                                                      
012102 MAIN SECTION.                                                            
012200                                                                          
013710     PERFORM S01-READ-OPEN                                                
013720     PERFORM S02-READ-MESSAGE                                             
013730     IF RECV-KDRC = ZERO                                                  
013740       PERFORM A-INIT                                                     
013750       PERFORM UNTIL RECV-KDRC > ZERO OR WS-IX > MAX-LINES                
013760         ADD 1 TO LOPNR-IX                                                
013770         PERFORM B-PERFORM-LINES                                          
013780         PERFORM C-UPDATE-T01IVW                                          
013790         PERFORM S02-READ-MESSAGE                                         
013791       END-PERFORM                                                        
013800     END-IF                                                               
013801                                                                          
013802     PERFORM S03-READ-CLOSE                                               
013803* TO MANY LINES INCREASE MAX-LINES OR MAKE A RESTART FUNCTION             
013804     IF WS-IX > MAX-LINES                                                 
013805       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
013806     END-IF                                                               
014200                                                                          
014400     MOVE ZERO TO RETURN-CODE                                             
014500     GOBACK                                                               
014600     .                                                                    
014700     EJECT                                                                
014710                                                                          
014800 A-INIT SECTION.                                                          
015000     MOVE YES                          TO KEYS-SW                         
015001     MOVE NOO                          TO DUPLICATE-SW                    
015002     MOVE ZERO                         TO RADER-IX                        
015003     MOVE ZERO                         TO LOPNR-IX                        
015004     MOVE ZERO                         TO WS-IX                           
015005                                                                          
015170     INITIALIZE GOOD-SQLCODECODES                                         
015180     .                                                                    
015190     EJECT                                                                
015191                                                                          
015192 B-PERFORM-LINES SECTION.                                                 
015196     IF MID-WF2103I1-IDPTYP = 'VAT'                                       
015197       PERFORM BA-PERFORM-LINES-VAT                                       
015198     END-IF                                                               
015199     .                                                                    
015200     EJECT                                                                
015201                                                                          
015702 BA-PERFORM-LINES-VAT SECTION.                                            
015714     MOVE 'AAMMDD'      TO DAT-KDDATFORM                                  
015715     MOVE MID-WF2103I1-DAFINDOC(3:6) TO DAT-I-TIDATUM                     
015716     CALL WDATKONV USING                                                  
015717          DAT-KDDATFORM                                                   
015718          DAT-I-TIDATUM                                                   
015719          DAT-O-TIDATUM                                                   
015720          DAT-KDSVAR                                                      
015721     MOVE DAT-TIMM TO WS-TIMM-1                                           
015722                      WS-TIRP-1                                           
015723                                                                          
015724     MOVE MID-WF2103I1-DAEXDAT(3:2) TO WS-TIAA-1                          
015725                                                                          
015728     MOVE +0  TO W-ANT                                                    
015729     INSPECT MID-WF2103I1-IDEXCUST-1 TALLYING W-ANT FOR CHARACTERS        
015730             BEFORE INITIAL ' '                                           
015731     IF W-ANT = 0                                                         
015732       MOVE +1  TO W-ANT                                                  
015733     END-IF                                                               
015734     MOVE MID-WF2103I1-IDEXCUST-1(1:W-ANT) TO WS-IDDISTR-1                
015735                                                                          
015736     MOVE +0  TO W-ANT                                                    
015737     INSPECT MID-WF2103I1-IDEXCUST-2 TALLYING W-ANT FOR CHARACTERS        
015738             BEFORE INITIAL ' '                                           
015739     IF W-ANT = 0                                                         
015740       MOVE +1  TO W-ANT                                                  
015741     END-IF                                                               
015742     MOVE MID-WF2103I1-IDEXCUST-2(1:W-ANT) TO WS-IDKUNDNR-1               
015743                                                                          
015744     MOVE WS-TIAA-1                    TO VAT-TIAA                        
015745     MOVE WS-TIMM-1                    TO VAT-TIRP                        
015750     MOVE MID-WF2103I1-DAEXDAT         TO WS-DAREGDAT                     
015751     MOVE MID-WF2103I1-TIEXTID         TO WS-TIREGTID                     
015752     MOVE MID-WF2103I1-IDPTYP          TO WS-IDPTYP                       
015753     MOVE LOPNR-IX                     TO WS-IDLOPNR                      
015754     MOVE MID-WF2103I1-IDLANDX3-BET    TO VAT-IDLANDX3-BET                
015755     MOVE MID-WF2103I1-IDLANDX3-SEND   TO VAT-IDLANDX3-SEND               
015756     MOVE MID-WF2103I1-KDVALISO        TO VAT-KDVALISO                    
015757     MOVE MID-WF2103I1-PRKURS          TO VAT-PRKURS                      
015758     MOVE MID-WF2103I1-IDVAT-LEG       TO VAT-IDVAT-LEG                   
015759     MOVE MID-WF2103I1-IDVAT-RESP      TO VAT-IDVAT-RESP                  
015760     MOVE MID-WF2103I1-IDVAT-BET       TO VAT-IDVAT-BET                   
015761     MOVE MID-WF2103I1-IDPARTNR        TO VAT-IDPARTNR                    
015762     MOVE MID-WF2103I1-KDFINDOC        TO VAT-KDFINDOC                    
015763     MOVE MID-WF2103I1-DAFINDOC        TO VAT-DAFINDOC                    
015764     MOVE MID-WF2103I1-IDFINDOC        TO VAT-IDFINDOC                    
015765     MOVE MID-WF2103I1-SUNTO-TOT       TO VAT-SUNTO-TOT                   
015766     MOVE MID-WF2103I1-SUVAT-BILLIT-TOT TO VAT-SUVAT-BILLIT-TOT           
015767     MOVE WS-IDDISTR-1                 TO VAT-IDDISTR                     
015768     MOVE WS-IDKUNDNR-1                TO VAT-IDKUNDNR                    
015769                                                                          
015770     MOVE SPACE                        TO VAT-KDVALISO-SEND               
015771     MOVE ZERO                         TO VAT-PRKURS-SEND                 
015772                                                                          
015773     MOVE VAT-W522VAT                  TO WS-VAT-DATA                     
015774                                                                          
015775     ADD 1                             TO WS-IX                           
015776     .                                                                    
015777     EJECT                                                                
015780                                                                          
017100 C-UPDATE-T01IVW SECTION.                                                 
017104     IF MID-WF2103I1-IDPTYP = 'VAT'                                       
017105       PERFORM DB2-INSERT-T01IVW-VAT                                      
017106     END-IF                                                               
017107     .                                                                    
017108     EJECT                                                                
018290                                                                          
020100*    --- DISPATCHER-SECTIONS                                              
024220 S01-READ-OPEN SECTION.                                                   
024230     MOVE 'OPEN'                      TO RECV-KDFUNC                      
024240     MOVE 'CARPARTS.PULS.RECEIVEV'    TO RECV-ADDISPABS                   
024250     CALL WZ01RECV USING RECV-CONTROL-AREA                                
024260                         RECV-OPEN-AREA                                   
024270     IF RECV-KDRC > 0                                                     
024280       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
024290       STRING 'WZ01RECV OPEN ERROR RC=' KDRC-DISPLAY                      
024300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
024400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
024500     END-IF                                                               
024600     .                                                                    
024700     SKIP3                                                                
024800                                                                          
024900 S02-READ-MESSAGE SECTION.                                                
025000     MOVE 'GET'                           TO RECV-KDFUNC                  
025100     MOVE LENGTH OF RECV-AREA             TO RECV-KVDLEN                  
025200     CALL WZ01RECV USING RECV-CONTROL-AREA                                
025300                         RECV-KVDLEN                                      
025400                         RECV-AREA                                        
025500     IF RECV-KDRC > 1                                                     
025600       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
025700       STRING 'WZ01RECV GET ERROR RC=' KDRC-DISPLAY                       
025800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
025900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
026000     END-IF                                                               
026100     .                                                                    
026200     SKIP3                                                                
026300                                                                          
026400 S03-READ-CLOSE SECTION.                                                  
026500     MOVE 'CLOSE'                    TO RECV-KDFUNC                       
026600     CALL WZ01RECV USING RECV-CONTROL-AREA                                
026700                                                                          
026800     IF RECV-KDRC > 0                                                     
026900       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
027000       STRING 'WZ01RECV CLOSE ERROR RC=' KDRC-DISPLAY                     
027100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
027200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
027300     END-IF                                                               
027400     .                                                                    
027500     EJECT                                                                
027600                                                                          
029069 DB2-INSERT-T01IVW-VAT SECTION.                                           
029070     SKIP2                                                                
029071     MOVE 000811 TO GOOD-SQLCODECODES                                     
029072     EXEC SQL                                                             
029073         INSERT INTO T01IVW                                               
029074         (DAREGDAT,                                                       
029075          TIREGTID,                                                       
029076          IDLOPNR,                                                        
029077          IDPTYP,                                                         
029078          TIRP,                                                           
029080          FLKLAR,                                                         
029081          IV_DATA,                                                        
029082          IV_DATA2)                                                       
029083         VALUES(:WS-DAREGDAT,                                             
029084                :WS-TIREGTID,                                             
029085                :WS-IDLOPNR,                                              
029086                :WS-IDPTYP,                                               
029087                :WS-TIRP-1,                                               
029088                'N',                                                      
029089                :WS-VAT-DATA,                                             
029090                :WS-VAT-DATA-2)                                           
029091     END-EXEC                                                             
029092                                                                          
029093     MOVE SQLCODE TO SQLCODE-WS                                           
029094     PERFORM DB2-STATUS-CHECK                                             
029095     .                                                                    
029096     EJECT                                                                
029097                                                                          
029098 DB2-STATUS-CHECK     SECTION.                                            
029099     SET SQLCODE-IX TO 1                                                  
029100     SEARCH GOOD-SQLCODE                                                  
029101       AT END                                                             
029102          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
029103          DELIMITED BY SIZE INTO ERROR-TEXT                               
029104          CALL ABEND USING RKOD-ABEND-DB2                                 
029105       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
029110     END-SEARCH                                                           
029200     .                                                                    
