000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2226500.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   19/01/29.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        update PB-plan and valid date for PB plan.                       
001000*                                                                         
001100*        THE PROGRAM UPDATES   WDK6                                       
001200*                                                                         
001300                                                                          
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000*          --- OUTPUT FIL TO UPDATE PB-PLAN AND DATE FOR PB-PLAN          
002100     SELECT W22264                    ASSIGN TO W22265D1.                 
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400     SKIP3                                                                
002500 FILE SECTION.                                                            
002600     SKIP3                                                                
002700 FD  W22264                                                               
002800     RECORDING       F                                                    
002900     BLOCK CONTAINS  0.                                                   
003000                                                                          
003100*01  -COPY W22264      -L.                                                
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500 77  IDPGM                       PIC X(8)    VALUE 'W2226500'.            
003600 01  CHKP-VAR.                                                            
003700     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
003800     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
003900     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004000     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004100     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004200     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
004300 01  W-W22264-KVPOST-IN          PIC S9(5)   VALUE ZERO COMP-3.           
004400 77  YES                         PIC X       VALUE 'J'.                   
004500 77  NOO                         PIC X       VALUE 'N'.                   
004600     SKIP2                                                                
004700 01  ERROR-TEXT.                                                          
004800     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
004900     03  ERR-TEXT-STR            PIC X(72)   VALUE SPACE.                 
005000                                                                          
005100 77  W22264-EOF-SW               PIC X       VALUE 'N'.                   
005200     88  END-OF-W22264                       VALUE 'Y'.                   
005300     EJECT                                                                
005400 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005500 01  FILLER REDEFINES TODAYS-DATE.                                        
005600     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005700     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005800     03  TODAYS-DATE-DAY         PIC 9(2).                                
005900     EJECT                                                                
006000 01  GENERAL-SUBPROGRAMS.                                                 
006100*                                                                         
006200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006600     EJECT                                                                
006700*    --- PARAMETRAR TILL POSTSUM                                          
006800*                                                                         
006900*01  -COPY W0005   -PRE  POSTSUM-                                         
007000     EJECT                                                                
007100*    --- PARAMETRAR TILL WDATKONV                                         
007200*01  -COPY WDATAREA                                                       
007300     EJECT                                                                
007400                                                                          
007500 01  W22264-AREA-START           PIC X(24)   VALUE                        
007600                                         'W22264-AREA-START'.             
007700*01  AREA -COPY W22264     -PRE IN-                                       
007800                                                                          
007900     EJECT                                                                
008000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008100     SKIP3                                                                
008200 01  KEYS-TILL-DLI.                                                       
008300     03  W-IDARTNR-X.                                                     
008400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008500                                                                          
008600     03  W-KDSEGKEY-X.                                                    
008700         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
008800     SKIP2                                                                
008900*    --- STATUS-KOD FRÅN IMS                                              
009000 01  STATUS-WS                   PIC XX.                                  
009100     88  SEGMENT-FOUND                       VALUE '  '.                  
009200     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
009300     88  SEGMENT-MISSING                     VALUE 'GE'.                  
009400     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
009500     88  IMS-NOT-OK                          VALUE 'XD'.                  
009600     SKIP2                                                                
009700 01  GOOD-STATUSCODES.                                                    
009800     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009900     SKIP3                                                                
010000 01  SSA1                        PIC X(64).                               
010100 01  SSA2                        PIC X(64).                               
010200     EJECT                                                                
010300*    --- IMS FUNCTION CODES                                               
010400*01  -COPY W0003                                                          
010500     EJECT                                                                
010600*    ---  DLI INPUT-OUTPUT AREA                                           
010700                                                                          
010800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDk611'.                      
010900 01  DLI-IO-WDK611.                                                       
011000*    03  -COPY WDK611                                                     
011100                                                                          
011200     EJECT                                                                
011300 LINKAGE SECTION.                                                         
011400                                                                          
011500*01  -COPY W0009   -PRE MSG-                                              
011600                                                                          
011700*01  -COPY W0008  -PRE WDK6-                                              
011800     05  FILLER                  PIC X.                                   
011900     EJECT                                                                
012000 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB.                              
012100 MAIN SECTION.                                                            
012200     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB.                              
012300                                                                          
012400     PERFORM A-INIT                                                       
012500                                                                          
012600     PERFORM S01-READ-W22264                                              
012700                                                                          
012800     PERFORM UNTIL END-OF-W22264                                          
012900       IF CHKP-ANT > CHKP-MAX                                             
013000         PERFORM X-TAKE-CHECKPOINT                                        
013100       END-IF                                                             
013200                                                                          
013300       MOVE IN-IDARTNR               TO W-IDARTNR                         
013400       PERFORM IMS-GHU-WDK611                                             
013500       IF SEGMENT-FOUND                                                   
013600          MOVE IN-KVPB-PLAN          TO CLAG-KVPB-PLAN                    
013700          MOVE IN-DAPBPLAN           TO CLAG-DAPBPLAN                     
013800                                                                          
013900          MOVE IN-KVPB-PLAN-JUST1    TO CLAG-KVPB-PLAN-JUST1              
014000          MOVE IN-TIPBPLAN-JUST1-FOM TO CLAG-TIPBPLAN-JUST1-FOM           
014010          MOVE IN-TIPBPLAN-JUST1-TOM TO CLAG-TIPBPLAN-JUST1-TOM           
014100                                                                          
014200          MOVE IN-KVPB-PLAN-JUST2    TO CLAG-KVPB-PLAN-JUST2              
014300          MOVE IN-TIPBPLAN-JUST2-FOM TO CLAG-TIPBPLAN-JUST2-FOM           
014310          MOVE IN-TIPBPLAN-JUST2-TOM TO CLAG-TIPBPLAN-JUST2-TOM           
014400                                                                          
014610          PERFORM IMS-REPL-WDK611                                         
014700       END-IF                                                             
014800                                                                          
014900       PERFORM S01-READ-W22264                                            
015000     END-PERFORM                                                          
015100                                                                          
015200                                                                          
015300     PERFORM Z-FINIT                                                      
015400                                                                          
015500     MOVE ZERO TO RETURN-CODE                                             
015600     GOBACK                                                               
015700     .                                                                    
015800     EJECT                                                                
015900 A-INIT SECTION.                                                          
016000     SKIP2                                                                
016100                                                                          
016200     PERFORM IMS-RESTART                                                  
016300                                                                          
016400     OPEN INPUT W22264                                                    
016500                                                                          
016600     .                                                                    
016700     EJECT                                                                
016800 Z-FINIT SECTION.                                                         
016900                                                                          
017000     CLOSE W22264                                                         
017100     SKIP2                                                                
017200     MOVE 'S' TO POSTSUM-OPKOD                                            
017300     CALL POSTSUM USING POSTSUM-PARM                                      
017400     .                                                                    
017500     EJECT                                                                
017600 S01-READ-W22264  SECTION.                                                
017700     SKIP2                                                                
017800     READ W22264        INTO IN-AREA                                      
017900     AT END                                                               
018000        SET END-OF-W22264 TO TRUE                                         
018100                                                                          
018200     NOT AT END                                                           
018300        MOVE 'W22264'     TO POSTSUM-FDNAMN                               
018400        MOVE 'W22265D1'   TO POSTSUM-DDNAMN2                              
018500        MOVE SPACE        TO POSTSUM-TRANSTYP                             
018600        CALL POSTSUM   USING POSTSUM-PARM                                 
018700     END-READ                                                             
018800     .                                                                    
018900     EJECT                                                                
019000 X-TAKE-CHECKPOINT   SECTION.                                             
019100                                                                          
019200* --- AT CHECKPOINT YOU LOSE GN-POSITION IN THE BASE                      
019300* --- SAVE DATABASE KEYS IF NECESSARY                                     
019400     PERFORM IMS-CHECKPOINT                                               
019500     MOVE ZERO TO CHKP-ANT                                                
019600* --- REREAD DATABASE IF NECESSARY                                        
019700     .                                                                    
019800     EJECT                                                                
019900* --- IMS SECTIONS  ---                                                   
020000                                                                          
020100     EJECT                                                                
020200 IMS-GHU-WDK611 SECTION.                                                  
020300                                                                          
020400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
020500          DELIMITED BY SIZE INTO SSA1                                     
020600     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
020700          DELIMITED BY SIZE INTO SSA2                                     
020800     MOVE '  GE'              TO GOOD-STATUSCODES                         
020900     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
021000     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
021100     PERFORM IMS-STATUSCHECK                                              
021200     .                                                                    
021300                                                                          
021400 IMS-REPL-WDK611 SECTION.                                                 
021500                                                                          
021600     MOVE '  '             TO GOOD-STATUSCODES                            
021700     CALL CBLTDLI  USING REPL WDK6-PCB DLI-IO-WDK611                      
021800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
021900     PERFORM IMS-STATUSCHECK                                              
022000     ADD +2 TO CHKP-ANT                                                   
022100     .                                                                    
022200     EJECT                                                                
022300 IMS-RESTART SECTION.                                                     
022400     SKIP2                                                                
022500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
022600     MOVE '  ' TO GOOD-STATUSCODES                                        
022700     CALL CBLTDLI USING XRST MSG-PCB                                      
022800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
022900                        CHKP-AREA-LENGTH CHKP-AREA                        
023000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
023100     PERFORM IMS-STATUSCHECK                                              
023200     .                                                                    
023300     SKIP3                                                                
023400 IMS-CHECKPOINT SECTION.                                                  
023500     SKIP2                                                                
023600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
023700     MOVE '  XD' TO GOOD-STATUSCODES                                      
023800     CALL CBLTDLI USING CHKP MSG-PCB                                      
023900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
024000                        CHKP-AREA-LENGTH CHKP-AREA                        
024100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
024200     PERFORM IMS-STATUSCHECK                                              
024300                                                                          
024400     IF IMS-NOT-OK                                                        
024500       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE' TO ERR-TEXT-STR        
024600       DISPLAY ERROR-TEXT                                                 
024700       CALL FELLOG                                                        
024800     END-IF                                                               
024900     .                                                                    
025000     EJECT                                                                
025100 IMS-STATUSCHECK SECTION.                                                 
025200     SKIP2                                                                
025300     SET STATUS-IX TO 1                                                   
025400     SEARCH GOOD-STATUS                                                   
025500       AT END                                                             
025600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
025700           DELIMITED BY SIZE INTO ERROR-TEXT                              
025800         DISPLAY ERROR-TEXT                                               
025900         CALL FELLOG                                                      
026000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
026100         CONTINUE                                                         
026200     END-SEARCH                                                           
026300     .                                                                    
