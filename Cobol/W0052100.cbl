000100 PROCESS DYNAM                                                            
000110 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0052100.                                                
000300 AUTHOR.         SARASWATHY S.                                            
000400 DATE-WRITTEN.   16/11/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        WEB RESPONSE TIME LOG                                            
000900*                                                                         
001000*        THE PROGRAM READS   TABLE TP0WLOG                                
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSACTION: W0T521                                              
001400*        MID:         W0I52101                                            
001500*                                                                         
001600*    OUTDATA.                                                             
001700*        MOD:         W0O52101                                            
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100                                                                          
002200 DATA DIVISION.                                                           
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500 77  IDPGM                       PIC X(08)   VALUE 'W0052100'.            
002600                                                                          
002700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
002800 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
002900                                                                          
003000 77  YES                         PIC X       VALUE 'J'.                   
003100 77  NOO                         PIC X       VALUE 'N'.                   
003200                                                                          
003300*    --- INDEX FOR SCROLL LINES                                           
003400 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003500 77  MAX-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
003600 77  MAX-KVRADER                 PIC S9(4)  VALUE +14   COMP SYNC.        
003700 77  CURRENT-SECTION             PIC X(40)  VALUE SPACES.                 
003710 01  MESSAGE-CODES.                                                       
003720     03  ERROR-CODES.                                                     
003780         05  INF-FIRST-PAGE          PIC X(3)   VALUE '010'.              
003790         05  INF-LAST-PAGE           PIC X(3)   VALUE '012'.              
003791         05  INF-MORE-LINES          PIC X(3)   VALUE '011'.              
003800*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
003900                                                                          
004000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004100     88  OWN-MID                             VALUE '0521'.                
004700     88  HELP-MID                            VALUE '0551'.                
004800     EJECT                                                                
004900*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005000 01  GENERAL-SUBPROGRAMS.                                                 
005100     03  W0052110                PIC X(8)    VALUE 'W0052110'.            
005200     03  WL01MCNV                PIC X(8)    VALUE 'WL01MCNV'.            
005300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
005400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005600     EJECT                                                                
005700*    --- PARAMETERS FOR SUBPROGRAM WL01MCNV                               
005800*01 -COPY WL01MCNV                                                        
005900     SKIP3                                                                
006000*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
006100*                                                                         
006200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
006300     SKIP3                                                                
006400*01 -COPY WMSGINIT                                                        
006500     EJECT                                                                
006600*    --- REQU AREA                                                        
006700 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
006800 01  REQU-AREA.                                                           
006900*    03 -COPY WZ01REQU                                                    
007000*    03 -COPY W00521I1                                                    
007100*    --- RESP AREA                                                        
007200 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007300 01  RESP-AREA.                                                           
007400*    03 -COPY WZ01RESP                                                    
007500*    03 -COPY W00521O1                                                    
007600     EJECT                                                                
007700*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
007800*                                                                         
007900 01  SAVE-AREA.                                                           
008000     03  SAVE-IDTRANS           PIC X(4)    VALUE '0521'.                 
008100     03  SAVE-PGNO              PIC 9(2)    VALUE 01.                     
008200     03  SAVE-FLKEY             PIC X(1)    VALUE SPACES.                 
008800     03  SAVE-AREA-PREV OCCURS 30 TIMES.                                  
008900         05  SAVE-IDDC-PREV         PIC X(2).                             
009000         05  SAVE-TIREGDAT-PREV     PIC 9(6).                             
009100         05  SAVE-TIREGTID-PREV     PIC 9(6).                             
009200         05  SAVE-IDUSER-PREV       PIC X(8).                             
009210         05  SAVE-KVMILSEC-PREV     PIC 9(6).                             
009300         05  SAVE-IDLOPNR-PREV      PIC X(2).                             
009400     03  SAVE-AREA-NEXT.                                                  
009500         05  SAVE-IDDC-NEXT         PIC X(2).                             
009600         05  SAVE-TIREGDAT-NEXT     PIC 9(6).                             
009700         05  SAVE-TIREGTID-NEXT     PIC 9(6).                             
009800         05  SAVE-IDUSER-NEXT       PIC X(8).                             
009810         05  SAVE-KVMILSEC-NEXT     PIC 9(6).                             
009900         05  SAVE-IDLOPNR-NEXT      PIC X(2).                             
010000     EJECT                                                                
010100*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
010200*                                                                         
010300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010400     SKIP3                                                                
010500*01  MID -COPY W0I52101                                                   
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010800     SKIP3                                                                
010900*01  -COPY WMSGAREA                                                       
011000     EJECT                                                                
011100     03  MOD REDEFINES MSG-AREA.                                          
011200*      05  -COPY W0O52101                                                 
011300     EJECT                                                                
011400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011500     SKIP3                                                                
011600*01  -COPY WMFSAREA                                                       
011700     EJECT                                                                
011800*    --- WORK-AREAS FOR IMS-SECTIONS                                      
011900*                                                                         
012000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012100     SKIP3                                                                
012200*    --- STATUS CODES FROM IMS                                            
012300 01  STATUS-WS                   PIC XX.                                  
012400     88  SEGMENT-FOUND                       VALUE '  '.                  
012500     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
012600     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012700     SKIP2                                                                
012800 01  GOOD-STATUSCODES.                                                    
012900     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013000     SKIP3                                                                
013100     EJECT                                                                
013200*    --- IMS FUNCTION CODES                                               
013300*01  -COPY W0003                                                          
013400     EJECT                                                                
013500 LINKAGE SECTION.                                                         
013600*01  -COPY W0009   -PRE MSG-                                              
013700*01  -COPY W0008   -PRE WDP7-                                             
013800     05  FILLER                  PIC X.                                   
013900     EJECT                                                                
014000 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB.                              
014100 MAIN SECTION.                                                            
014200     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB.                              
014300     MOVE 'MAIN' TO CURRENT-SECTION                                       
014400     PERFORM IMS-GET-MSG                                                  
014500     IF SEGMENT-FOUND                                                     
014600       PERFORM A-INIT                                                     
014700       PERFORM B-INIT-KEYS                                                
014800       IF MFS-FIRST                                                       
014900          SET REQU-FIRST    TO TRUE                                       
014910          PERFORM C-FIRST-PAGE                                            
015000       ELSE                                                               
015100          IF MFS-NEXT                                                     
015200             SET REQU-NEXT  TO TRUE                                       
015300             PERFORM D-NEXT-PAGE                                          
015400          ELSE                                                            
015500             IF MFS-PREVIOUS                                              
015600                SET REQU-PREVIOUS  TO TRUE                                
015700                PERFORM I-PREV-PAGE                                       
015800             ELSE                                                         
015810                IF MFS-PRINT                                              
015820                   SET REQU-PRINT  TO TRUE                                
015830                   PERFORM J-LATEST-PARA                                  
015840                ELSE                                                      
015900                   SET REQU-QUERY TO TRUE                                 
016000                   PERFORM E-SAME-PAGE                                    
016010                END-IF                                                    
016100             END-IF                                                       
016200          END-IF                                                          
016300       END-IF                                                             
016400       PERFORM F-CALL-BIZ-LOGIC-W0052110                                  
016500*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
016600*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
016700       COMPUTE MSG-KVLL = LENGTH OF MOD-W0O52101 + 4                      
016800       PERFORM IMS-INSERT-MSG                                             
016900     END-IF                                                               
017000                                                                          
017100     MOVE ZERO TO RETURN-CODE                                             
017200     GOBACK                                                               
017300     .                                                                    
017400     EJECT                                                                
017500 A-INIT SECTION.                                                          
017600     MOVE 'A-INIT' TO CURRENT-SECTION                                     
017700     IF MSG-DOUBLE-TRANSACTIONS                                           
017800       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W0I52101                 
017900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
018000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
018100     ELSE                                                                 
018200       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W0I52101                  
018300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
018400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018500     END-IF                                                               
018600                                                                          
018700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
019000                                                                          
019100     MOVE LOW-VALUE TO MSG-AREA                                           
019200     MOVE 'W0O521N1' TO MFS-IDMOD                                         
019300     MOVE '0521' TO MOD-IDTRANS                                           
019400     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
019500                                                                          
019600     IF OWN-MID OR HELP-MID                                               
019700       CONTINUE                                                           
019800     ELSE                                                                 
019900       MOVE SPACE TO MFS-KDTRTYP                                          
020000       MOVE '7' TO MFS-IDPFK                                              
020100     END-IF                                                               
020200     .                                                                    
020300     EJECT                                                                
020400 B-INIT-KEYS SECTION.                                                     
020500                                                                          
020510     MOVE 'B-INIT-KEYS'      TO CURRENT-SECTION                           
020600     MOVE ALL '+'            TO MSGI-WMSGINIT                             
020700     MOVE '001'              TO MSGI-KDCALL                               
020800     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
020900     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
021000     MOVE '0521'             TO MSGI-IDTRANS                              
021100     IF OWN-MID                                                           
021200        MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                             
021300        MOVE MID-TIREGDAT-IN TO MSGI-TIREGDAT                             
021400        MOVE MID-TIREGTID-IN TO MSGI-TIREGTID                             
021500        MOVE MID-IDUSER-IN   TO MSGI-IDUSER-KEY                           
021600        MOVE MID-FLSORT-IN   TO MSGI-FLSORT                               
021700        MOVE MID-BEWEBSCR-IN TO MSGI-BEWEBSCR                             
021800     END-IF                                                               
021900     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
022061*                                                                         
022062     MOVE MSGI-SPAR-AREA  TO SAVE-AREA                                    
022063*                                                                         
022200     MOVE MFS-ERASE-FIELD TO MOD-IDDC-IN                                  
022300     IF MID-IDDC-IN NOT = ALL '+'                                         
022400       MOVE '7'        TO MFS-IDPFK                                       
022500       MOVE SPACE      TO MFS-KDTRTYP                                     
             MOVE 'N'        TO SAVE-FLKEY                                      
022630     END-IF                                                               
022700     MOVE MSGI-IDDC-KEY TO REQU-IDDC-KEY                                  
022800*                                                                         
022900     MOVE MFS-ERASE-FIELD TO MOD-TIREGDAT-IN                              
023000     IF MID-TIREGDAT-IN NOT = ALL '+'                                     
023100       MOVE '7'        TO MFS-IDPFK                                       
023200       MOVE SPACE      TO MFS-KDTRTYP                                     
             MOVE 'N'        TO SAVE-FLKEY                                      
023300     END-IF                                                               
023310     IF MSGI-TIREGDAT = SPACES OR ZEROS                                   
023320        MOVE ZEROS TO MSGI-TIREGDAT                                       
023330     END-IF                                                               
023500     MOVE MSGI-TIREGDAT TO REQU-TIREGDAT-KEY                              
023600*                                                                         
023700     MOVE MFS-ERASE-FIELD TO MOD-TIREGTID-IN                              
023800     IF MID-TIREGTID-IN NOT = ALL '+'                                     
023900       MOVE '7'        TO MFS-IDPFK                                       
024000       MOVE SPACE      TO MFS-KDTRTYP                                     
             MOVE 'N'        TO SAVE-FLKEY                                      
024100     END-IF                                                               
024110     IF MSGI-TIREGTID = SPACES OR ZEROS                                   
024120        MOVE ZEROS TO MSGI-TIREGTID                                       
024140     END-IF                                                               
024300     MOVE MSGI-TIREGTID TO REQU-TIREGTID-KEY                              
024400*                                                                         
024500     MOVE MFS-ERASE-FIELD TO MOD-IDUSER-IN                                
024600     IF MID-IDUSER-IN NOT = ALL '+'                                       
024700       MOVE '7'        TO MFS-IDPFK                                       
024800       MOVE SPACE      TO MFS-KDTRTYP                                     
             MOVE 'N'        TO SAVE-FLKEY                                      
024900     END-IF                                                               
025000     MOVE MSGI-IDUSER-KEY TO REQU-IDUSER-KEY                              
025100*                                                                         
025200     MOVE MFS-ERASE-FIELD TO MOD-FLSORT-IN                                
025300     IF MID-FLSORT-IN NOT = ALL '+'                                       
             IF SAVE-IDTRANS = '0521' AND SAVE-FLKEY = 'N'                      
025400         MOVE '7'        TO MFS-IDPFK                                     
             END-IF                                                             
025500       MOVE SPACE      TO MFS-KDTRTYP                                     
             MOVE 'N'        TO SAVE-FLKEY                                      
025600     END-IF                                                               
025700     MOVE MSGI-FLSORT  TO REQU-FLSORT-KEY                                 
025800*                                                                         
025900     MOVE MFS-ERASE-FIELD TO MOD-BEWEBSCR-IN                              
026000     IF MID-BEWEBSCR-IN NOT = ALL '+'                                     
026100       MOVE '7'        TO MFS-IDPFK                                       
026200       MOVE SPACE      TO MFS-KDTRTYP                                     
             MOVE 'N'        TO SAVE-FLKEY                                      
026300     END-IF                                                               
026400     MOVE MSGI-BEWEBSCR TO REQU-BEWEBSCR-KEY                              
026500*                                                                         
026560                                                                          
026700       MOVE MSGI-IDDC-KEY       TO MOD-IDDC-UT                            
026800       MOVE MSGI-TIREGDAT       TO MOD-TIREGDAT-UT                        
026900       INSPECT MOD-TIREGDAT-UT REPLACING LEADING ZERO BY SPACE            
027000       MOVE MSGI-IDUSER-KEY     TO MOD-IDUSER-UT                          
027100       MOVE MSGI-TIREGTID       TO MOD-TIREGTID-UT                        
027200       INSPECT MOD-TIREGTID-UT REPLACING LEADING ZERO BY SPACE            
027300       MOVE MSGI-FLSORT         TO MOD-FLSORT-UT                          
027400       MOVE MSGI-BEWEBSCR       TO MOD-BEWEBSCR-UT                        
028300     .                                                                    
028400     EJECT                                                                
028410*                                                                         
028420 C-FIRST-PAGE SECTION.                                                    
028421                                                                          
028422     MOVE 'D-FIRST-PAGE' TO CURRENT-SECTION                               
028423                                                                          
028710     IF SAVE-FLKEY = 'Y'                                                  
028720        MOVE 'Y' TO REQU-PF4FLAG                                          
028730     ELSE                                                                 
028740        MOVE 'N' TO REQU-PF4FLAG                                          
028750     END-IF                                                               
028480     .                                                                    
028490     EJECT                                                                
028491*                                                                         
028500 D-NEXT-PAGE SECTION.                                                     
028600                                                                          
028700     MOVE 'D-NEXT-PAGE' TO CURRENT-SECTION                                
028710     IF SAVE-FLKEY = 'Y'                                                  
028720        MOVE 'Y' TO REQU-PF4FLAG                                          
028730     ELSE                                                                 
028740        MOVE 'N' TO REQU-PF4FLAG                                          
028750     END-IF                                                               
028800     IF SAVE-IDTRANS = '0521'                                             
028810      IF (SAVE-PGNO < 30) AND (SAVE-PGNO >= 1)                            
028900       MOVE SAVE-IDDC-NEXT        TO REQU-IDDC-START                      
029000       MOVE SAVE-TIREGDAT-NEXT    TO REQU-TIREGDAT-START                  
029100       MOVE SAVE-TIREGTID-NEXT    TO REQU-TIREGTID-START                  
029200       MOVE SAVE-IDUSER-NEXT      TO REQU-IDUSER-START                    
029210       MOVE SAVE-KVMILSEC-NEXT    TO REQU-KVMILSEC-START                  
029220      ELSE                                                                
029221        IF SAVE-PGNO >= 30                                                
029222         MOVE SAVE-IDDC-PREV (30)     TO REQU-IDDC-START                  
029223         MOVE SAVE-TIREGDAT-PREV (30) TO REQU-TIREGDAT-START              
029224         MOVE SAVE-TIREGTID-PREV (30) TO REQU-TIREGTID-START              
029225         MOVE SAVE-IDUSER-PREV (30)   TO REQU-IDUSER-START                
029226         MOVE SAVE-KVMILSEC-PREV (30) TO REQU-KVMILSEC-START              
029227        END-IF                                                            
            END-IF                                                              
029300     END-IF                                                               
029400     .                                                                    
029500     EJECT                                                                
029600 E-SAME-PAGE SECTION.                                                     
029700                                                                          
029710     MOVE 'E-SAME-PAGE' TO CURRENT-SECTION                                
029720     IF SAVE-FLKEY = 'Y'                                                  
029730        MOVE 'Y'  TO REQU-PF4FLAG                                         
029740     ELSE                                                                 
029750        MOVE 'N'  TO REQU-PF4FLAG                                         
029760     END-IF                                                               
029800     IF SAVE-IDTRANS = '0521'                                             
029900      IF SAVE-PGNO < 1                                                    
030000         MOVE 1   TO SAVE-PGNO                                            
030100      ELSE                                                                
030200        IF SAVE-PGNO >= 30                                                
030300          MOVE 30 TO SAVE-PGNO                                            
030400        END-IF                                                            
030401      END-IF                                                              
030402      MOVE SAVE-IDDC-PREV (SAVE-PGNO)     TO REQU-IDDC-START              
030403      MOVE SAVE-TIREGDAT-PREV (SAVE-PGNO) TO REQU-TIREGDAT-START          
030404      MOVE SAVE-TIREGTID-PREV (SAVE-PGNO) TO REQU-TIREGTID-START          
030405      MOVE SAVE-IDUSER-PREV (SAVE-PGNO)   TO REQU-IDUSER-START            
030406      MOVE SAVE-KVMILSEC-PREV (SAVE-PGNO) TO REQU-KVMILSEC-START          
           ELSE                                                                 
            MOVE 1      TO SAVE-PGNO                                            
030407     END-IF                                                               
030408     .                                                                    
030500     EJECT                                                                
030600 I-PREV-PAGE SECTION.                                                     
030700                                                                          
030710     MOVE 'I-PREV-PAGE' TO CURRENT-SECTION                                
030711     IF SAVE-FLKEY = 'Y'                                                  
030712        MOVE 'Y' TO REQU-PF4FLAG                                          
030713     ELSE                                                                 
030714        MOVE 'N' TO REQU-PF4FLAG                                          
030715     END-IF                                                               
030720     IF SAVE-IDTRANS = '0521'                                             
030726      IF SAVE-PGNO > 1 AND SAVE-PGNO <= 30                                
030729       MOVE SAVE-IDDC-PREV (SAVE-PGNO - 1)     TO REQU-IDDC-START         
030731       MOVE SAVE-TIREGDAT-PREV (SAVE-PGNO - 1) TO                         
030732                                              REQU-TIREGDAT-START         
030733       MOVE SAVE-TIREGTID-PREV (SAVE-PGNO - 1) TO                         
030734                                              REQU-TIREGTID-START         
030735       MOVE SAVE-IDUSER-PREV (SAVE-PGNO - 1)   TO                         
030736                                              REQU-IDUSER-START           
030737       MOVE SAVE-KVMILSEC-PREV (SAVE-PGNO - 1) TO                         
030738                                              REQU-KVMILSEC-START         
030800      ELSE                                                                
030900       IF SAVE-PGNO <= 1                                                  
031000        MOVE 1 TO SAVE-PGNO                                               
031100        MOVE SAVE-IDDC-PREV (SAVE-PGNO)     TO REQU-IDDC-START            
031200        MOVE SAVE-TIREGDAT-PREV (SAVE-PGNO) TO REQU-TIREGDAT-START        
031210        MOVE SAVE-TIREGTID-PREV (SAVE-PGNO) TO REQU-TIREGTID-START        
031211        MOVE SAVE-IDUSER-PREV (SAVE-PGNO)   TO REQU-IDUSER-START          
031212        MOVE SAVE-KVMILSEC-PREV (SAVE-PGNO) TO REQU-KVMILSEC-START        
031213       END-IF                                                             
031220      END-IF                                                              
031300     END-IF                                                               
031400     .                                                                    
031500     EJECT                                                                
031510*                                                                         
031520 J-LATEST-PARA SECTION.                                                   
031530     MOVE 'J-LATEST-PARA' TO CURRENT-SECTION                              
031540     MOVE 'Y' TO SAVE-FLKEY                                               
                       REQU-PF4FLAG                                             
031541     IF SAVE-IDTRANS = '0521'                                             
031549      MOVE SPACES     TO REQU-IDDC-START                                  
031552                         REQU-IDUSER-START                                
031554                         REQU-IDDC-KEY                                    
031555                         REQU-TIREGDAT-KEY                                
031556                         REQU-TIREGTID-KEY                                
031557                         REQU-IDUSER-KEY                                  
031559      MOVE ZEROS      TO REQU-TIREGDAT-START                              
031560                         REQU-TIREGTID-START                              
031561                         REQU-KVMILSEC-START                              
031562     END-IF                                                               
031563     .                                                                    
031564     EJECT                                                                
031570*                                                                         
031600 F-CALL-BIZ-LOGIC-W0052110 SECTION.                                       
031710     MOVE 'F-CALL-BIZ-LOGIC-W0052110' TO CURRENT-SECTION                  
031720     MOVE '101'          TO REQU-IDMSGVER                                 
031730     MOVE MSGI-IDUSER    TO REQU-IDUSER                                   
031800     CALL W0052110 USING REQU-AREA RESP-AREA MAX-KVRADER                  
032510     IF MFS-FIRST                                                         
032511* SAVE START AND NEXT KEYS IN PROFILE DB                                  
032520       MOVE 1 TO SAVE-PGNO                                                
032530       PERFORM FA-MOVE-KEYS-TO-PROFILE-DB                                 
032531       IF RESP-IDMSG-ERROR = SPACES                                       
032532          MOVE INF-FIRST-PAGE  TO RESP-IDMSG-ERROR                        
032533       END-IF                                                             
032534     END-IF                                                               
032510     IF MFS-PRINT AND REQU-PF4FLAG = 'Y'                                  
032511* SAVE START AND NEXT KEYS IN PROFILE DB                                  
032520       MOVE 1 TO SAVE-PGNO                                                
032530       PERFORM FA-MOVE-KEYS-TO-PROFILE-DB                                 
032531       IF RESP-IDMSG-ERROR = SPACES                                       
032532          MOVE INF-FIRST-PAGE  TO RESP-IDMSG-ERROR                        
032533       END-IF                                                             
032534     END-IF                                                               
032535     IF MFS-NEXT                                                          
032536* SAVE START AND NEXT KEYS IN PROFILE DB                                  
032537       IF (SAVE-IDDC-PREV(SAVE-PGNO) = RESP-IDDC-START AND                
032538          SAVE-TIREGDAT-PREV(SAVE-PGNO) = RESP-TIREGDAT-START AND         
032539          SAVE-TIREGTID-PREV(SAVE-PGNO) = RESP-TIREGTID-START AND         
032540          SAVE-IDUSER-PREV(SAVE-PGNO) = RESP-IDUSER-START AND             
032541          SAVE-KVMILSEC-PREV(SAVE-PGNO) = RESP-KVMILSEC-START)            
032550            CONTINUE                                                      
033547       ELSE                                                               
033548        IF SAVE-PGNO < 30                                                 
033549          COMPUTE SAVE-PGNO = SAVE-PGNO + 1                               
033550          PERFORM FA-MOVE-KEYS-TO-PROFILE-DB                              
033560        ELSE                                                              
033570         IF (SAVE-PGNO = 30 AND                                           
033571             RESP-IDMSG-INFO NOT = INF-LAST-PAGE)                         
033580             MOVE 30 TO SAVE-PGNO                                         
033600             MOVE INF-MORE-LINES    TO RESP-IDMSG-INFO                    
033700         END-IF                                                           
033701        END-IF                                                            
033702       END-IF                                                             
033703     END-IF                                                               
033704     IF MFS-PREVIOUS                                                      
033705* SAVE START AND NEXT KEYS IN PROFILE DB                                  
033706       IF SAVE-PGNO = 1 AND RESP-IDMSG-ERROR = SPACES                     
033707          MOVE INF-FIRST-PAGE  TO RESP-IDMSG-ERROR                        
033708       ELSE                                                               
033709        IF SAVE-PGNO > 1 AND SAVE-PGNO <= 30                              
033710          COMPUTE SAVE-PGNO = SAVE-PGNO - 1                               
033720          MOVE RESP-IDDC-NEXT      TO SAVE-IDDC-NEXT                      
033730          MOVE RESP-TIREGDAT-NEXT  TO SAVE-TIREGDAT-NEXT                  
033740          MOVE RESP-TIREGTID-NEXT  TO SAVE-TIREGTID-NEXT                  
033750          MOVE RESP-IDUSER-NEXT    TO SAVE-IDUSER-NEXT                    
033751          MOVE RESP-KVMILSEC-NEXT  TO SAVE-KVMILSEC-NEXT                  
033754        END-IF                                                            
033767       END-IF                                                             
033770     END-IF                                                               
033771     IF MFS-ENTER                                                         
033772       PERFORM FA-MOVE-KEYS-TO-PROFILE-DB                                 
033773     END-IF                                                               
033777     IF RESP-IDMSG-ERROR NOT = SPACE OR                                   
033778        RESP-IDMSG-INFO  NOT = SPACE                                      
033779       PERFORM FA-SET-MSG-AND-HILIGHT                                     
033780     END-IF                                                               
033781     PERFORM FB-MOVE-RESP-TO-MOD                                          
033782       MOVE '002'               TO MSGI-KDCALL                            
033790       MOVE '0521'              TO MSGI-IDTRANS                           
033800       MOVE '0521'              TO SAVE-IDTRANS                           
033900       MOVE MSG-LTERM-NAME      TO MSGI-IDLTERM-USER                      
034000       MOVE MSG-SIGNON-USERID   TO MSGI-IDUSER                            
034100       MOVE SAVE-AREA           TO MSGI-SPAR-AREA                         
034200       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
034400     .                                                                    
034500     EJECT                                                                
034510*                                                                         
034600 FA-MOVE-KEYS-TO-PROFILE-DB SECTION.                                      
           MOVE 'FA-MOVE-KEYS-TO-PROFILE-DB' TO CURRENT-SECTION                 
034610     MOVE RESP-IDDC-START     TO SAVE-IDDC-PREV(SAVE-PGNO)                
034620     MOVE RESP-TIREGDAT-START TO SAVE-TIREGDAT-PREV(SAVE-PGNO)            
034630     MOVE RESP-TIREGTID-START TO SAVE-TIREGTID-PREV(SAVE-PGNO)            
034640     MOVE RESP-IDUSER-START   TO SAVE-IDUSER-PREV(SAVE-PGNO)              
034650     MOVE RESP-KVMILSEC-START TO SAVE-KVMILSEC-PREV(SAVE-PGNO)            
034660                                                                          
034670     MOVE RESP-IDDC-NEXT      TO SAVE-IDDC-NEXT                           
034680     MOVE RESP-TIREGDAT-NEXT  TO SAVE-TIREGDAT-NEXT                       
034690     MOVE RESP-TIREGTID-NEXT  TO SAVE-TIREGTID-NEXT                       
034691     MOVE RESP-IDUSER-NEXT    TO SAVE-IDUSER-NEXT                         
034692     MOVE RESP-KVMILSEC-NEXT  TO SAVE-KVMILSEC-NEXT                       
034693     .                                                                    
034694     EJECT                                                                
034700 FA-SET-MSG-AND-HILIGHT SECTION.                                          
034800                                                                          
034910     MOVE 'FA-SET-MSG-AND-HILIGHT' TO CURRENT-SECTION                     
035000     MOVE RESP-IDMSG-ERROR       TO MCNV-IDMSG-ERROR                      
035100     MOVE RESP-IDMSG-INFO        TO MCNV-IDMSG-INFO                       
035200     MOVE RESP-IDELMT-ERROR      TO MCNV-IDELMT-ERROR                     
035300                                                                          
035400     CALL WL01MCNV USING MCNV-AREA                                        
035500     MOVE MCNV-MFSFEL            TO MOD-TEMFSFEL                          
035600     MOVE MCNV-MFSINF            TO MOD-TEMFSINF                          
035700     .                                                                    
035800     EJECT                                                                
035900*                                                                         
036000 FB-MOVE-RESP-TO-MOD SECTION.                                             
036100                                                                          
036110     MOVE 'FB-MOVE-RESP-TO-MOD'  TO CURRENT-SECTION                       
036116     IF RESP-KVMILSEC-AVG NOT = ALL '+'                                   
036117       MOVE RESP-KVMILSEC-AVG    TO MOD-KVMILSEC-AVG                      
036119     END-IF                                                               
036300     PERFORM                                                              
036400     VARYING INDX FROM +1 BY +1                                           
036500       UNTIL INDX > RESP-KVRADER                                          
036700       IF RESP-IDDC-LINE (INDX) = SPACE                                   
036800         MOVE MFS-ERASE-FIELD    TO MOD-IDDC (INDX)                       
036900       ELSE                                                               
037000         IF RESP-IDDC-LINE (INDX) = ALL '+'                               
037100           MOVE MFS-DO-NOT-TOUCH-FIELD                                    
037200                                 TO MOD-IDDC (INDX)                       
037300         ELSE                                                             
037400           MOVE RESP-IDDC-LINE (INDX)                                     
037500                                 TO MOD-IDDC (INDX)                       
037600         END-IF                                                           
037700       END-IF                                                             
037800                                                                          
037900       IF RESP-TIREGDAT-LINE (INDX) = SPACE                               
038000         MOVE MFS-ERASE-FIELD    TO MOD-TIREGDAT (INDX)                   
038100       ELSE                                                               
038200         IF RESP-TIREGDAT-LINE (INDX) = ALL '+'                           
038300           MOVE MFS-DO-NOT-TOUCH-FIELD                                    
038400                                 TO MOD-TIREGDAT (INDX)                   
038500         ELSE                                                             
038600           MOVE RESP-TIREGDAT-LINE (INDX)                                 
038700                                 TO MOD-TIREGDAT (INDX)                   
038800         END-IF                                                           
038900       END-IF                                                             
039000                                                                          
039100       IF RESP-TIREGTID-LINE (INDX) = SPACE                               
039200         MOVE MFS-ERASE-FIELD    TO MOD-TIREGTID (INDX)                   
039300       ELSE                                                               
039400         IF RESP-TIREGTID-LINE (INDX) = ALL '+'                           
039500           MOVE MFS-DO-NOT-TOUCH-FIELD                                    
039600                                 TO MOD-TIREGTID (INDX)                   
039700         ELSE                                                             
039800           MOVE RESP-TIREGTID-LINE (INDX)                                 
039900                                 TO MOD-TIREGTID (INDX)                   
040000         END-IF                                                           
040100       END-IF                                                             
040200                                                                          
040300       IF RESP-IDUSER-LINE (INDX) = SPACE                                 
040400         MOVE MFS-ERASE-FIELD    TO MOD-IDUSER (INDX)                     
040500       ELSE                                                               
040600         IF RESP-IDUSER-LINE (INDX) = ALL '+'                             
040700           MOVE MFS-DO-NOT-TOUCH-FIELD                                    
040800                                 TO MOD-IDUSER (INDX)                     
040900         ELSE                                                             
041000           MOVE RESP-IDUSER-LINE (INDX)                                   
041100                                 TO MOD-IDUSER (INDX)                     
041200         END-IF                                                           
041300       END-IF                                                             
041400                                                                          
041500       IF RESP-KVMILSEC-LINE (INDX) = SPACE                               
041600         MOVE MFS-ERASE-FIELD    TO MOD-KVMILSEC (INDX)                   
041700       ELSE                                                               
041800         IF RESP-KVMILSEC-LINE (INDX) = ALL '+'                           
041900           MOVE MFS-DO-NOT-TOUCH-FIELD                                    
042000                                 TO MOD-KVMILSEC (INDX)                   
042100         ELSE                                                             
042200           MOVE RESP-KVMILSEC-LINE (INDX)                                 
042300                                 TO MOD-KVMILSEC (INDX)                   
042400         END-IF                                                           
042500       END-IF                                                             
042600                                                                          
042700       IF RESP-BEWEBSCR-LINE (INDX) = SPACE                               
042800         MOVE MFS-ERASE-FIELD    TO MOD-BEWEBSCR (INDX)                   
042900       ELSE                                                               
043000         IF RESP-BEWEBSCR-LINE (INDX) = ALL '+'                           
043100           MOVE MFS-DO-NOT-TOUCH-FIELD                                    
043200                                 TO MOD-BEWEBSCR (INDX)                   
043300         ELSE                                                             
043400           MOVE RESP-BEWEBSCR-LINE (INDX)                                 
043500                                 TO MOD-BEWEBSCR (INDX)                   
043600         END-IF                                                           
043700       END-IF                                                             
043800                                                                          
043900       IF RESP-BEWEBURL-LINE (INDX) = SPACE                               
044000         MOVE MFS-ERASE-FIELD    TO MOD-BEWEBURL (INDX)                   
044100       ELSE                                                               
044200         IF RESP-BEWEBURL-LINE (INDX) = ALL '+'                           
044300           MOVE MFS-DO-NOT-TOUCH-FIELD                                    
044400                                 TO MOD-BEWEBURL (INDX)                   
044500         ELSE                                                             
044600           MOVE RESP-BEWEBURL-LINE (INDX)                                 
044700                                 TO MOD-BEWEBURL (INDX)                   
044800         END-IF                                                           
044900       END-IF                                                             
045000                                                                          
045100     END-PERFORM                                                          
045200     PERFORM                                                              
045300      VARYING INDX FROM INDX BY +1                                        
045400      UNTIL INDX > MAX-INDX                                               
045500         PERFORM MFS-ERASE-LINE-FIELD-OUT                                 
045600     END-PERFORM                                                          
045700                                                                          
045800     .                                                                    
045900     EJECT                                                                
046000                                                                          
046010 MFS-ERASE-UT-FIELD-OUT SECTION.                                          
046020                                                                          
046030     MOVE 'MFS-ERASE-UT-FIELD-OUT' TO CURRENT-SECTION                     
046040*    --- OUTDATA-FIELD ON UT                                              
046050     MOVE MFS-ERASE-FIELD TO MOD-IDDC-UT                                  
046060                             MOD-TIREGDAT-UT                              
046070                             MOD-TIREGTID-UT                              
046080                             MOD-IDUSER-UT                                
046091                             MOD-BEWEBSCR-UT                              
046093     .                                                                    
046094     SKIP3                                                                
046100                                                                          
046200 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
046300                                                                          
046310     MOVE 'MFS-ERASE-LINE-FIELD-OUT' TO CURRENT-SECTION                   
046400*    --- OUTDATA-FIELD ON SCROLL KEYS                                     
046500      MOVE MFS-ERASE-FIELD TO MOD-IDDC (INDX)                             
046600                              MOD-TIREGDAT (INDX)                         
046700                              MOD-TIREGTID (INDX)                         
046800                              MOD-IDUSER (INDX)                           
046900                              MOD-KVMILSEC (INDX)                         
047000                              MOD-BEWEBSCR (INDX)                         
047100                              MOD-BEWEBURL (INDX)                         
047200     .                                                                    
047300     SKIP3                                                                
047400                                                                          
047500* --- IMS SECTIONS ---                                                    
047600     SKIP3                                                                
047700 IMS-GET-MSG SECTION.                                                     
047800                                                                          
047810     MOVE 'IMS-GET-MSG' TO CURRENT-SECTION                                
047900     MOVE '  QC' TO GOOD-STATUSCODES                                      
048000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
048100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
048200     PERFORM IMS-STATUSCHECK                                              
048300     .                                                                    
048400     SKIP3                                                                
048500 IMS-INSERT-MSG SECTION.                                                  
048600                                                                          
048610     MOVE 'IMS-INSERT-MSG' TO CURRENT-SECTION                             
049000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
049100     MOVE SPACE TO GOOD-STATUSCODES                                       
049200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
049300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
049400     PERFORM IMS-STATUSCHECK                                              
049500     .                                                                    
049600     EJECT                                                                
049700 IMS-STATUSCHECK SECTION.                                                 
049800                                                                          
049810     MOVE 'IMS-STATUS-CHK' TO CURRENT-SECTION                             
049900     SET STATUS-IX TO 1                                                   
050000     SEARCH GOOD-STATUS                                                   
050100       AT END                                                             
050200         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
050300         DELIMITED BY SIZE INTO ERROR-TEXT                                
050400         CALL FELLOG                                                      
050500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
050600         CONTINUE                                                         
050700     END-SEARCH                                                           
050800     .                                                                    
050900     EJECT                                                                
