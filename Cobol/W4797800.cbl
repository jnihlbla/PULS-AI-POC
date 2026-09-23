000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4797800.                                                
000400 AUTHOR.         KUMAR LOVISH.                                            
000500 DATE-WRITTEN.   19/06/10.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000710                                                                          
000800*    FUNCTION:                                                            
000900*        CLEAN UP WDL1                                                    
001200*                                                                         
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002301*          --- WDL1 CLEAN UP RECORDS                                      
002302     SELECT W47977                     ASSIGN TO W47978D1.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W47977                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100 01  W47977-POST                PIC X(45).                                
005200     EJECT                                                                
005300 WORKING-STORAGE SECTION.                                                 
005400                                                                          
005401 77  IDPGM                       PIC X(8)    VALUE 'W4797800'.            
005402 01  CHKP-VAR.                                                            
005403     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
005404     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
005405     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
005406     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
005407     03 CHKP-ANT                 PIC S9(4)   VALUE +0   COMP-3.           
005408     03 CHKP-MAX                 PIC S9(4)   VALUE +3000 COMP-3.          
005409 77  YES                         PIC X       VALUE 'J'.                   
005410 77  NOO                         PIC X       VALUE 'N'.                   
005411     SKIP2                                                                
005412 01  ERROR-TEXT.                                                          
005413     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
005414     03  ERROR-STR               PIC X(72)   VALUE SPACE.                 
006100                                                                          
006101 77  W47977-EOF-SW               PIC X       VALUE 'N'.                   
006102     88  END-OF-W47977                       VALUE 'Y'.                   
015000     EJECT                                                                
015100 01  GENERAL-SUBPROGRAMS.                                                 
015200*                                                                         
015400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
015500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
016800     EJECT                                                                
016900*    --- PARAMETRAR TILL POSTSUM                                          
017000*                                                                         
017100*01  -COPY W0005   -PRE  POSTSUM-                                         
017200     EJECT                                                                
017500 01  IN-AREA-START               PIC X(24)   VALUE                        
017600                                             'IN-AREA-START'.             
017700     SKIP2                                                                
017800                                                                          
017801 01  IN-AREA.                                                             
017802   03 IN-SEGM              PIC X(8)    VALUE SPACES.                      
017803   03 IN-ORD-IDDISTR       PIC S9(5)   VALUE ZERO COMP-3.                 
017804   03 IN-ORD-IDKUNDNR      PIC S9(7)   VALUE ZERO COMP-3.                 
017805   03 IN-ORD-IDKUNDRF      PIC X(10)   VALUE SPACES.                      
017806   03 IN-ORD-IDDC          PIC X(2)    VALUE SPACES.                      
017807   03 IN-DAT-DAFAKT        PIC 9(8)    VALUE ZERO.                        
017808   03 IN-DAT-DAHISTOB      PIC 9(8)    VALUE ZERO.                        
017809                                                                          
018000     EJECT                                                                
019700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019800     SKIP3                                                                
019801 01  KEYS-TILL-DLI.                                                       
019802   03  W-WDL101KY-X.                                                      
019803     05  W-IDDISTR             PIC S9(5)  VALUE ZERO COMP-3.              
019804     05  W-IDKUNDNR            PIC S9(7)  VALUE ZERO COMP-3.              
019805     05  W-IDKUNDRF            PIC X(10)  VALUE SPACE.                    
019806     05  W-IDDC                PIC X(2)   VALUE '00'.                     
019807     SKIP2                                                                
019808   03  W-WDL111KY-X.                                                      
019809     05  W-DAT-DAFAKT          PIC 9(8)   VALUE ZEROES.                   
019810     SKIP2                                                                
019811   03  W-WDL112KY-X.                                                      
019812     05  W-DAT-DAHISTOB        PIC 9(8)   VALUE ZEROES.                   
020400     SKIP2                                                                
020500*    --- STATUS-KOD FRÅN IMS                                              
020600 01  STATUS-WS                   PIC XX.                                  
020601     88  SEGMENT-FOUND                       VALUE '  '.                  
020602     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
020603     88  SEGMENT-MISSING                     VALUE 'GE'.                  
020604     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
020605     88  IMS-NOT-OK                          VALUE 'XD'.                  
021000     SKIP2                                                                
021001 01  GOOD-STATUSCODES.                                                    
021002     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021300     SKIP3                                                                
021400 01  SSA1                        PIC X(64).                               
021500 01  SSA2                        PIC X(64).                               
021600     EJECT                                                                
021700*    --- IMS FUNCTION CODES                                               
021800*01  -COPY W0003                                                          
021900     EJECT                                                                
022000*    ---  DLI INPUT-OUTPUT AREA                                           
022001 01  DLI-IO-AREA.                                                         
022002   03 IO-AREA                    PIC X(50).                               
022003                                                                          
022400     EJECT                                                                
022500 LINKAGE SECTION.                                                         
022600                                                                          
022700*01  -COPY W0009   -PRE MSG-                                              
022800                                                                          
022900*01  -COPY W0008  -PRE WDL1-                                              
023000     05  FILLER                  PIC X.                                   
023010                                                                          
023100     EJECT                                                                
023101 PROCEDURE DIVISION  USING MSG-PCB WDL1-PCB.                              
023102     ENTRY 'DLITCBL' USING MSG-PCB WDL1-PCB.                              
023500                                                                          
023600     PERFORM A-INIT                                                       
023601     PERFORM S01-READ-W47977                                              
023602     PERFORM UNTIL END-OF-W47977                                          
023700                                                                          
023800       PERFORM DELETE-WDL1-SEGMENTS                                       
024300                                                                          
024301       IF CHKP-ANT > CHKP-MAX                                             
024303         PERFORM X-TAKE-CHECKPOINT                                        
024304       END-IF                                                             
024500                                                                          
024501       PERFORM S01-READ-W47977                                            
024502     END-PERFORM                                                          
024700                                                                          
025100                                                                          
025200     PERFORM Z-FINIT                                                      
025400                                                                          
025401     MOVE ZERO TO RETURN-CODE                                             
025402     GOBACK                                                               
026000     .                                                                    
026100     EJECT                                                                
026200 A-INIT SECTION.                                                          
026300                                                                          
026400     PERFORM IMS-RESTART                                                  
026500                                                                          
026600     OPEN INPUT W47977                                                    
026800                                                                          
026801     MOVE ZERO  TO CHKP-ANT                                               
026802     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
027000     .                                                                    
027100     EJECT                                                                
027200 DELETE-WDL1-SEGMENTS SECTION.                                            
027300                                                                          
027301     IF IN-SEGM  = 'WDL101  '                                             
027302        PERFORM IMS-GHU-WDL101                                            
027303        IF SEGMENT-FOUND                                                  
027304           PERFORM IMS-DLET-WDL1                                          
027305           ADD 1    TO CHKP-ANT                                           
028300        END-IF                                                            
028301     ELSE                                                                 
028302        IF IN-SEGM  = 'WDL111  '                                          
028303           PERFORM IMS-GHU-WDL111                                         
028304             IF SEGMENT-FOUND                                             
028305               PERFORM IMS-DLET-WDL1                                      
028306               ADD 1    TO CHKP-ANT                                       
028307             END-IF                                                       
028308        ELSE                                                              
028309           IF IN-SEGM  = 'WDL112  '                                       
028310             PERFORM IMS-GHU-WDL112                                       
028311               IF SEGMENT-FOUND                                           
028312                 PERFORM IMS-DLET-WDL1                                    
028313                 ADD 1    TO CHKP-ANT                                     
028314               END-IF                                                     
028315           END-IF                                                         
028316        END-IF                                                            
028317     END-IF                                                               
028402     .                                                                    
028403     EJECT                                                                
028404 Z-FINIT SECTION.                                                         
028600                                                                          
028601     CLOSE W47977                                                         
028602     SKIP2                                                                
028603     MOVE 'S' TO POSTSUM-OPKOD                                            
028604     CALL POSTSUM USING POSTSUM-PARM                                      
029200     .                                                                    
029300     EJECT                                                                
029301 S01-READ-W47977  SECTION.                                                
029302     SKIP2                                                                
029303                                                                          
029304     READ W47977           INTO IN-AREA                                   
029305     AT END                                                               
029306        MOVE HIGH-VALUE      TO IN-AREA                                   
029307        SET END-OF-W47977    TO TRUE                                      
029500                                                                          
029501     NOT AT END                                                           
029502        MOVE IN-ORD-IDDISTR      TO W-IDDISTR                             
029503        MOVE IN-ORD-IDKUNDNR     TO W-IDKUNDNR                            
029504        MOVE IN-ORD-IDKUNDRF     TO W-IDKUNDRF                            
029505        MOVE IN-ORD-IDDC         TO W-IDDC                                
029506        MOVE IN-DAT-DAFAKT       TO W-DAT-DAFAKT                          
029507        MOVE IN-DAT-DAHISTOB     TO W-DAT-DAHISTOB                        
029900                                                                          
029901        MOVE 'W47977'            TO POSTSUM-FDNAMN                        
029902        MOVE 'W47978D1'          TO POSTSUM-DDNAMN2                       
029903        CALL POSTSUM          USING POSTSUM-PARM                          
029904     END-READ                                                             
030100     .                                                                    
030200     EJECT                                                                
030300 X-TAKE-CHECKPOINT   SECTION.                                             
030400                                                                          
030401* --- AT CHECKPOINT YOU LOSE GN-POSITION IN THE BASE                      
030402* --- SAVE DATABASE KEYS IF NECESSARY                                     
030403     PERFORM IMS-CHECKPOINT                                               
030404     MOVE ZERO TO CHKP-ANT                                                
030405* --- REREAD DATABASE IF NECESSARY                                        
038900     .                                                                    
039000     EJECT                                                                
039100* --- IMS SECTIONS  ---                                                   
039200                                                                          
039201     EJECT                                                                
039202 IMS-RESTART SECTION.                                                     
039203     SKIP2                                                                
039204     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
039205     MOVE '  ' TO GOOD-STATUSCODES                                        
039206     CALL CBLTDLI USING XRST MSG-PCB                                      
039207                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
039208                        CHKP-AREA-LENGTH CHKP-AREA                        
039209     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
039210     PERFORM IMS-STATUSCHECK                                              
039211     .                                                                    
039212     SKIP3                                                                
039213 IMS-CHECKPOINT SECTION.                                                  
039214     SKIP2                                                                
039215     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
039216     MOVE '  XD' TO GOOD-STATUSCODES                                      
039217     CALL CBLTDLI USING CHKP MSG-PCB                                      
039218                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
039219                        CHKP-AREA-LENGTH CHKP-AREA                        
039220     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
039221     PERFORM IMS-STATUSCHECK                                              
039222                                                                          
039223     IF IMS-NOT-OK                                                        
039224       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE' TO ERROR-STR           
039225       DISPLAY ERROR-TEXT                                                 
039226       CALL FELLOG                                                        
039227     END-IF                                                               
039228     .                                                                    
039229     EJECT                                                                
039230 IMS-GHU-WDL101 SECTION.                                                  
039231                                                                          
039232     STRING 'WDL101  (WDL101KY =' W-WDL101KY-X ')'                        
039233            DELIMITED BY SIZE INTO SSA1                                   
039234     MOVE '  GE'             TO GOOD-STATUSCODES                          
039235     CALL CBLTDLI USING GHU WDL1-PCB DLI-IO-AREA SSA1                     
039236     MOVE WDL1-STATUS-CODE   TO STATUS-WS                                 
039237     PERFORM IMS-STATUSCHECK                                              
039238     .                                                                    
039239                                                                          
039240 IMS-GHU-WDL111 SECTION.                                                  
039241                                                                          
039242     STRING 'WDL101  (WDL101KY =' W-WDL101KY-X ')'                        
039243            DELIMITED BY SIZE INTO SSA1                                   
039244     STRING 'WDL111  (DAFAKT   =' W-DAT-DAFAKT ')'                        
039245            DELIMITED BY SIZE INTO SSA2                                   
039246     MOVE '  GE'             TO GOOD-STATUSCODES                          
039247     CALL CBLTDLI USING GHU WDL1-PCB DLI-IO-AREA SSA1 SSA2                
039248     MOVE WDL1-STATUS-CODE   TO STATUS-WS                                 
039249     PERFORM IMS-STATUSCHECK                                              
039250     .                                                                    
039251                                                                          
039252 IMS-GHU-WDL112 SECTION.                                                  
039253                                                                          
039254     STRING 'WDL101  (WDL101KY =' W-WDL101KY-X ')'                        
039255            DELIMITED BY SIZE INTO SSA1                                   
039256     STRING 'WDL112  (DAHISTOB =' W-DAT-DAHISTOB ')'                      
039257            DELIMITED BY SIZE INTO SSA2                                   
039258     MOVE '  GE'             TO GOOD-STATUSCODES                          
039259     CALL CBLTDLI USING GHU WDL1-PCB DLI-IO-AREA SSA1 SSA2                
039260     MOVE WDL1-STATUS-CODE   TO STATUS-WS                                 
039261     PERFORM IMS-STATUSCHECK                                              
039262     .                                                                    
039263                                                                          
039264 IMS-DLET-WDL1 SECTION.                                                   
039265                                                                          
039266     MOVE '  '                 TO GOOD-STATUSCODES                        
039267     CALL CBLTDLI USING DLET WDL1-PCB DLI-IO-AREA                         
039268     MOVE WDL1-STATUS-CODE     TO STATUS-WS                               
039269     PERFORM IMS-STATUSCHECK                                              
039270     .                                                                    
039271     SKIP3                                                                
039272 IMS-STATUSCHECK SECTION.                                                 
039273     SKIP2                                                                
039274                                                                          
039300     SET STATUS-IX TO 1                                                   
039400     SEARCH GOOD-STATUS                                                   
039500       AT END                                                             
039600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
039601           DELIMITED BY SIZE INTO ERROR-TEXT                              
039602         DISPLAY ERROR-TEXT                                               
039900         CALL FELLOG                                                      
040000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
040100         CONTINUE                                                         
040200     END-SEARCH                                                           
040300     .                                                                    
