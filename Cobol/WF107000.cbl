000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WF107000.                                                
000300 AUTHOR.         ANDERS HENRIKSSON.                                       
000400 DATE-WRITTEN.   JUNE    2003.                                            
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THE PGM                                                          
000900*        - READS PRM-DATA FROM SYSIN                                      
001000*        - READS FILE WITH NEW/CHANGED         CUSTOMER RECORDS           
001100*        - SENDS THE FOLLOWING CUSTOMER DATA TO VSS:                      
001200*            BY USING WZ01SEND (CARPARTS.VSS.RECCUST)                     
001400*                                                                         
001500                                                                          
001600 ENVIRONMENT DIVISION.                                                    
001700                                                                          
001800 INPUT-OUTPUT SECTION.                                                    
001900 FILE-CONTROL.                                                            
002300*          --- CUSTOMER-RECORDS                                           
002400     SELECT WF1018                     ASSIGN TO WF1070D1.                
002500     EJECT                                                                
002600                                                                          
002700*                                                                         
002800     SELECT WF1071                     ASSIGN TO WF1070D2.                
002900     EJECT                                                                
003000*------------------------------                                           
003100 DATA DIVISION.                                                           
003200                                                                          
003300 FILE SECTION.                                                            
004000 FD  WF1018                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400 01  FILLER.                                                              
004500*    03 -COPY WF10CUS2 -L.                                                
004600     EJECT                                                                
004700                                                                          
004800*                                                                         
004900 FD  WF1071                                                               
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200 01  WF1071-POST.                                                         
005300*    03  -COPY WF10VSS     -L.                                            
005400*    03  -COPY WF1070      -L.                                            
005500     EJECT                                                                
005600                                                                          
005700*------------------------------                                           
005800                                                                          
005900                                                                          
006000 WORKING-STORAGE SECTION.                                                 
006100 77  IDPGM                       PIC X(8)    VALUE 'WF107000'.            
006200 77  SYSIN-EOF                   PIC X       VALUE 'N'.                   
006300 77  WF1018-EOF-SW               PIC X       VALUE 'N'.                   
006400     88  END-OF-WF1018                       VALUE 'J'.                   
006410 77  FIRST-TIME-SW               PIC X       VALUE 'J'.                   
006420     88  FIRST-TIME                          VALUE 'J'.                   
006430 77  JA                          PIC X       VALUE 'J'.                   
006431 77  NEJ                         PIC X       VALUE 'N'.                   
006500     EJECT                                                                
006600                                                                          
006700 01  WS-ATAB-RECEIVERS.                                                   
006800     03  WS-VSS                  PIC X(50)   VALUE                        
006900                                 'CARPARTS.VSS.RECCUST'.                  
007000                                                                          
007010 01  WS-IDLOPNR.                                                          
007030     03  WS-TIME                 PIC 9(11)   VALUE ZERO.                  
007200 01  WS-TIMESTAMP.                                                        
007300     03  WS-YEAR                 PIC X(4)    VALUE SPACE.                 
007400     03  FILLER                  PIC X       VALUE '-'.                   
007500     03  WS-MONTH                PIC X(2)    VALUE SPACE.                 
007600     03  FILLER                  PIC X       VALUE '-'.                   
007700     03  WS-DAY                  PIC X(2)    VALUE SPACE.                 
007800     03  FILLER                  PIC X       VALUE SPACE.                 
007900     03  WS-HOUR                 PIC X(2)    VALUE SPACE.                 
008000     03  FILLER                  PIC X       VALUE ':'.                   
008100     03  WS-MINUTE               PIC X(2)    VALUE SPACE.                 
008200     03  FILLER                  PIC X       VALUE ':'.                   
008300     03  WS-SECOND               PIC X(2)    VALUE SPACE.                 
008400     03  FILLER                  PIC X       VALUE '.'.                   
008500     03  WS-DECIMAL              PIC X(2)    VALUE SPACE.                 
008510     03  FILLER                  PIC X(4)    VALUE '0000'.                
008600 01  WS-BILLIT                   PIC X(6)    VALUE 'BILCUS'.              
008700                                                                          
008800 01  WS-CURRENT-DATE             PIC X(8)    VALUE SPACE.                 
008900 01  WS-FIRST-DAY-OF-YEAR.                                                
009000     03  WS-NEW-YEAR             PIC X(4)    VALUE SPACE.                 
009100     03  FILLER                  PIC X(4)    VALUE '0101'.                
009200                                                                          
009300     EJECT                                                                
009400 01  ERRTEXT.                                                             
009500     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
009600     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
009700 01  KDRC-DISPLAY                PIC Z(5).                                
009800     EJECT                                                                
009900                                                                          
010000 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
010100 01  FILLER REDEFINES TODAYS-DATE.                                        
010200     03  TODAYS-DATE-YEAR        PIC 9(2).                                
010300     03  TODAYS-DATE-MONTH       PIC 9(2).                                
010400     03  TODAYS-DATE-DAY         PIC 9(2).                                
010500     EJECT                                                                
010600                                                                          
010700 01  GENERAL-SUBPROGRAMS.                                                 
010800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
010900     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
011000     EJECT                                                                
011100                                                                          
011200*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
011300                                                                          
011400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
011600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
011700     EJECT                                                                
011800                                                                          
011900*    --- AREOR FÖR KOMMUNIKATION                                          
012000 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
012100*01  -COPY WZ01SEND                                                       
012200     EJECT                                                                
012300                                                                          
012400 01  WZ20DAYS PIC X(8) VALUE 'WZ20DAYS'.                                  
012500     SKIP3                                                                
012600*    -COPY WZ20DAYS                                                       
012700     EJECT                                                                
012800                                                                          
013500 01  IN00-AREA-START             PIC X(24)   VALUE                        
013600                                             'IN00-AREA-START'.           
013700 01  IN00-AREA.                                                           
013900*    03  -COPY WF10CUS2                                                   
013910     EJECT                                                                
014000                                                                          
014010 01  UT00-AREA-START             PIC X(24)   VALUE                        
014020                                             'UT00-AREA-START'.           
014030 01  UT00-AREA.                                                           
014040*    03  -COPY WF10VSS                                                    
014050*    03  -COPY WF1070                                                     
014060*                                                                         
014100     EJECT                                                                
014200                                                                          
014300 LINKAGE SECTION.                                                         
014400*01  -COPY W0009   -PRE MSG-                                              
014500     EJECT                                                                
014600                                                                          
014700 PROCEDURE DIVISION  USING MSG-PCB.                                       
014800                                                                          
014900 MAIN SECTION.                                                            
015000     ENTRY 'DLITCBL' USING MSG-PCB.                                       
015100                                                                          
015200     PERFORM A-INIT                                                       
015300                                                                          
015400     PERFORM B-EXECUTE                                                    
015500                                                                          
015600     PERFORM Z-FINIT                                                      
015700     MOVE ZERO TO RETURN-CODE                                             
015800     GOBACK                                                               
015900     .                                                                    
016000     EJECT                                                                
016100                                                                          
016200 A-INIT SECTION.                                                          
016900     OPEN INPUT  WF1018                                                   
017100     OPEN OUTPUT WF1071                                                   
017300                                                                          
017400     MOVE FUNCTION CURRENT-DATE (1:4)  TO WS-YEAR                         
017500     MOVE FUNCTION CURRENT-DATE (1:4)  TO WS-NEW-YEAR                     
017600     MOVE FUNCTION CURRENT-DATE (5:2)  TO WS-MONTH                        
017700     MOVE FUNCTION CURRENT-DATE (7:2)  TO WS-DAY                          
017800     MOVE FUNCTION CURRENT-DATE (9:2)  TO WS-HOUR                         
017900     MOVE FUNCTION CURRENT-DATE (11:2) TO WS-MINUTE                       
018000     MOVE FUNCTION CURRENT-DATE (13:2) TO WS-SECOND                       
018100     MOVE FUNCTION CURRENT-DATE (15:6) TO WS-DECIMAL                      
018200     MOVE FUNCTION CURRENT-DATE (1:8)  TO WS-CURRENT-DATE                 
018210     MOVE FUNCTION CURRENT-DATE (3:11) TO WS-TIME                         
018300                                                                          
019500                                                                          
019510     MOVE WS-IDLOPNR   TO IDLOPNR                                         
019700     MOVE WS-TIMESTAMP TO TIMESTAMP                                       
019800     MOVE WS-BILLIT    TO BILLIT-TEXT                                     
020100     .                                                                    
020200     EJECT                                                                
020300                                                                          
020400 B-EXECUTE SECTION.                                                       
020500     PERFORM S01-READ-WF1018                                              
020600                                                                          
020700     PERFORM UNTIL END-OF-WF1018                                          
021130       IF FIRST-TIME                                                      
021132         MOVE NEJ TO FIRST-TIME-SW                                        
021133         PERFORM S11-CUST-OPEN                                            
021134       END-IF                                                             
021140       MOVE CORR WF10CUST IN IN00-AREA TO WF1070 IN UT00-AREA             
021150       PERFORM S12-CUST-PUT                                               
021200       WRITE WF1071-POST FROM UT00-AREA                                   
021500       PERFORM S01-READ-WF1018                                            
021600     END-PERFORM                                                          
021601                                                                          
021610     IF NOT FIRST-TIME                                                    
021620       PERFORM S13-CUST-CLOSE                                             
021630     END-IF                                                               
021700     .                                                                    
021800     EJECT                                                                
021900                                                                          
022000 Z-FINIT SECTION.                                                         
022400     CLOSE WF1071                                                         
022600     CLOSE WF1018                                                         
022700     .                                                                    
022800     EJECT                                                                
022900                                                                          
023000 S01-READ-WF1018  SECTION.                                                
023100     READ WF1018          INTO IN00-AREA                                  
023200     AT END                                                               
023300        MOVE HIGH-VALUE   TO   IN00-AREA                                  
023400        SET END-OF-WF1018 TO   TRUE                                       
023500     END-READ                                                             
023600     .                                                                    
023700     EJECT                                                                
023800                                                                          
023900 S11-CUST-OPEN SECTION.                                                   
024000     MOVE WS-VSS                          TO SEND-ADDISPABS               
024100     MOVE 'OPEN'                          TO SEND-KDFUNC                  
024200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
024300                         SEND-OPEN-AREA                                   
024400     IF SEND-KDRC > ZERO                                                  
024500       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
024600       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
024700       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
024800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
024900     END-IF                                                               
025000     .                                                                    
025100                                                                          
025200 S12-CUST-PUT SECTION.                                                    
025300     MOVE 'PUT'                           TO SEND-KDFUNC                  
025400     MOVE LENGTH OF UT00-AREA             TO SEND-KVDLEN                  
025500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
025600                         SEND-KVDLEN                                      
025700                         UT00-AREA                                        
025800     IF SEND-KDRC > 1                                                     
025900       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
026000       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
026100       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
026200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
026300     END-IF                                                               
026400     .                                                                    
026500                                                                          
026600 S13-CUST-CLOSE SECTION.                                                  
026700     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
026800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
026900     .                                                                    
