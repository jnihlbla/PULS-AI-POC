001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     W6129900.                                                
001400 AUTHOR.         ARCHANA BHAT.                                            
001500 DATE-WRITTEN.   14/03/10.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800                                                                          
001900*    FUNCTION:                                                            
002000*        READS FILE FROM W61291 CONTAINING WDJ911 SEGMENTS.               
002100*        DELETES ALL THOSE SEGMENTS FROM THE DATABASE                     
002200*                                                                         
002310*        THE PROGRAM UPDATES   WDJ9                                       
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003201     SKIP2                                                                
003202*          --- WDJ911 SEGMENT KEYS                                        
003210     SELECT W61291                     ASSIGN TO W61299D1.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003801     SKIP3                                                                
003802 FD  W61291                                                               
003803     RECORDING       F                                                    
003804     BLOCK CONTAINS  0.                                                   
003805                                                                          
003810*01  -COPY W6129101      -L.                                              
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200 77  IDPGM                       PIC X(8)    VALUE 'W6129900'.            
004300 01  CHKP-VAR.                                                            
004400     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004500     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004600     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004700     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004800     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004900     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
005000 77  YES                         PIC X       VALUE 'J'.                   
005100 77  NOO                         PIC X       VALUE 'N'.                   
005200     SKIP2                                                                
005300 01  ERROR-TEXT.                                                          
005400     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
005500     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
005701                                                                          
005702 77  W61291-EOF-SW               PIC X       VALUE 'N'.                   
005710     88  END-OF-W61291                       VALUE 'Y'.                   
006000     EJECT                                                                
006100 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006200 01  FILLER REDEFINES TODAYS-DATE.                                        
006300     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006400     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006500     03  TODAYS-DATE-DAY         PIC 9(2).                                
006600     EJECT                                                                
006700 01  GENERAL-SUBPROGRAMS.                                                 
006800*                                                                         
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007110     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007201     EJECT                                                                
007202*    --- PARAMETRAR TILL POSTSUM                                          
007203*                                                                         
007210*01  -COPY W0005   -PRE  POSTSUM-                                         
007501     EJECT                                                                
007502 01  IN-AREA-START               PIC X(24)   VALUE                        
007503                                             'IN-AREA-START'.             
007504     SKIP2                                                                
007505                                                                          
007510*01  AREA -COPY W6129101     -PRE IN-                                     
007600*                                                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008000 01  KEYS-TILL-DLI.                                                       
008101     03  W-IDARTNR-X.                                                     
008102         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008103     03  W-WDJ911KY-X.                                                    
008120         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
008130         05  W-DASTADAT          PIC S9(9)   VALUE ZERO COMP-3.           
008140         05  W-TISTATID          PIC S9(7)   VALUE ZERO COMP-3.           
008150         05  W-ADLAGOMR          PIC 9(2)    VALUE ZERO.                  
008160         05  W-ADGANG            PIC 9(2)    VALUE ZERO.                  
008170         05  W-ADPLATS           PIC 9(5)    VALUE ZERO.                  
008200     SKIP2                                                                
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FOUND                       VALUE '  '.                  
008600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
008700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
008800     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
008900     88  IMS-NOT-OK                          VALUE 'XD'.                  
009000     SKIP2                                                                
009100 01  GOOD-STATUSCODES.                                                    
009200     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009300     SKIP3                                                                
009400 01  SSA1                        PIC X(64).                               
009500 01  SSA2                        PIC X(64).                               
009600     EJECT                                                                
009700*    --- IMS FUNCTION CODES                                               
009800*01  -COPY W0003                                                          
009900     EJECT                                                                
010100*    ---  DLI INPUT-OUTPUT AREA                                           
010200                                                                          
010301 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDJ901'.                      
010302 01  DLI-IO-WDJ901.                                                       
010303*    03  -COPY WDJ901                                                     
010304     EJECT                                                                
010305 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDJ911'.                      
010306 01  DLI-IO-WDJ911.                                                       
010310*    03  -COPY WDJ911                                                     
010400                                                                          
010800     EJECT                                                                
010900 LINKAGE SECTION.                                                         
011000                                                                          
011100*01  -COPY W0009  -PRE MSG-                                               
011201                                                                          
011202*01  -COPY W0008  -PRE WDJ9-                                              
011210     05  FILLER                  PIC X.                                   
011500     EJECT                                                                
011601 PROCEDURE DIVISION  USING MSG-PCB WDJ9-PCB.                              
011602 MAIN SECTION.                                                            
011610     ENTRY 'DLITCBL' USING MSG-PCB WDJ9-PCB.                              
011700                                                                          
011900     SKIP2                                                                
012000     PERFORM A-INIT                                                       
012110     PERFORM S01-READ-W61291                                              
012200     PERFORM UNTIL END-OF-W61291                                          
012210       PERFORM B-PROCESS-W61291                                           
012220                                                                          
012300       IF CHKP-ANT > CHKP-MAX                                             
012400         PERFORM X-TAKE-CHECKPOINT                                        
012500       END-IF                                                             
012520                                                                          
013210       PERFORM S01-READ-W61291                                            
013300     END-PERFORM                                                          
013500                                                                          
013600     PERFORM Z-FINIT                                                      
013700                                                                          
013800     MOVE ZERO TO RETURN-CODE                                             
013900     GOBACK                                                               
014000     .                                                                    
014100     EJECT                                                                
014200 A-INIT SECTION.                                                          
014300     SKIP2                                                                
014500     PERFORM IMS-RESTART                                                  
014701                                                                          
014710     OPEN INPUT W61291                                                    
015410     MOVE IDPGM              TO POSTSUM-PROGNAMN                          
015700     .                                                                    
015900     EJECT                                                                
016000 B-PROCESS-W61291 SECTION.                                                
016001                                                                          
016002* MOVE WDJ911 KEYS FROM INPUT FILE TO THE SEGMENT KEY AREAS               
016003     MOVE IN-IDARTNR         TO W-IDARTNR                                 
016004     MOVE IN-IDDC            TO W-IDDC                                    
016005     MOVE IN-DASTADAT-9KOMPL TO W-DASTADAT                                
016006     MOVE IN-TISTATID-9KOMPL TO W-TISTATID                                
016007     MOVE IN-ADLAGOMR        TO W-ADLAGOMR                                
016008     MOVE IN-ADGANG          TO W-ADGANG                                  
016009     MOVE IN-ADPLATS         TO W-ADPLATS                                 
016010                                                                          
016011     PERFORM IMS-GU-WDJ901                                                
016012     IF SEGMENT-FOUND                                                     
016013        PERFORM IMS-GHNP-WDJ911                                           
016014        IF SEGMENT-FOUND                                                  
016015           PERFORM IMS-DLET-WDJ911                                        
016016           ADD +1            TO CHKP-ANT                                  
016017        END-IF                                                            
016018     END-IF                                                               
016019     .                                                                    
016020     EJECT                                                                
016030 Z-FINIT SECTION.                                                         
016100                                                                          
016510     CLOSE W61291                                                         
016701     SKIP2                                                                
016702     MOVE 'S'             TO POSTSUM-OPKOD                                
016710     CALL POSTSUM      USING POSTSUM-PARM                                 
016900     .                                                                    
017001     EJECT                                                                
017002 S01-READ-W61291  SECTION.                                                
017003     SKIP2                                                                
017004     READ W61291        INTO IN-AREA                                      
017005     AT END                                                               
017006        MOVE HIGH-VALUE   TO IN-AREA                                      
017007        SET END-OF-W61291 TO TRUE                                         
017008                                                                          
017009     NOT AT END                                                           
017010        MOVE 'W61291'     TO POSTSUM-FDNAMN                               
017011        MOVE 'W61299D1'   TO POSTSUM-DDNAMN2                              
017012        MOVE SPACES       TO POSTSUM-TRANSTYP                             
017013        CALL POSTSUM   USING POSTSUM-PARM                                 
017016     END-READ                                                             
017020     .                                                                    
017300     EJECT                                                                
017400 X-TAKE-CHECKPOINT   SECTION.                                             
017500                                                                          
018100     PERFORM IMS-CHECKPOINT                                               
018200     MOVE ZERO            TO CHKP-ANT                                     
018400     .                                                                    
018500     EJECT                                                                
018600* --- IMS SECTIONS  ---                                                   
018700                                                                          
018801     EJECT                                                                
018802 IMS-GU-WDJ901 SECTION.                                                   
018803                                                                          
018804     STRING 'WDJ901  (IDARTNR  =' W-IDARTNR-X ')'                         
018805          DELIMITED BY SIZE INTO SSA1                                     
018806     MOVE '  GE'              TO GOOD-STATUSCODES                         
018807     CALL CBLTDLI          USING GU WDJ9-PCB DLI-IO-WDJ901 SSA1           
018808     MOVE WDJ9-STATUS-CODE    TO STATUS-WS                                
018809     PERFORM IMS-STATUSCHECK                                              
018810     .                                                                    
018811     EJECT                                                                
018812 IMS-GHNP-WDJ911 SECTION.                                                 
018813                                                                          
018814     STRING 'WDJ911  (WDJ911KY =' W-WDJ911KY-X ')'                        
018815          DELIMITED BY SIZE INTO SSA1                                     
018816     MOVE '  GE'              TO GOOD-STATUSCODES                         
018817     CALL CBLTDLI          USING GHNP WDJ9-PCB DLI-IO-WDJ911 SSA1         
018818     MOVE WDJ9-STATUS-CODE    TO STATUS-WS                                
018819     PERFORM IMS-STATUSCHECK                                              
018820     .                                                                    
018821     SKIP3                                                                
018822 IMS-DLET-WDJ911 SECTION.                                                 
018823                                                                          
018824     MOVE '  '                TO GOOD-STATUSCODES                         
018825     CALL CBLTDLI          USING DLET WDJ9-PCB DLI-IO-WDJ911              
018826     MOVE WDJ9-STATUS-CODE    TO STATUS-WS                                
018827     PERFORM IMS-STATUSCHECK                                              
018830     .                                                                    
018900     EJECT                                                                
019000 IMS-RESTART SECTION.                                                     
019100     SKIP2                                                                
019200     MOVE SPACE               TO CHKP-MSG-IO-AREA                         
019300     MOVE '  '                TO GOOD-STATUSCODES                         
019400     CALL CBLTDLI          USING XRST MSG-PCB                             
019500                                 CHKP-MSG-IO-AREA-LENGTH                  
019510                                 CHKP-MSG-IO-AREA                         
019600                                 CHKP-AREA-LENGTH CHKP-AREA               
019700     MOVE MSG-STATUS-CODE     TO STATUS-WS                                
019800     PERFORM IMS-STATUSCHECK                                              
019900     .                                                                    
020000     SKIP3                                                                
020100 IMS-CHECKPOINT SECTION.                                                  
020200     SKIP2                                                                
020300     MOVE SPACE               TO CHKP-MSG-IO-AREA                         
020400     MOVE '  XD'              TO GOOD-STATUSCODES                         
020500     CALL CBLTDLI          USING CHKP MSG-PCB                             
020600                                 CHKP-MSG-IO-AREA-LENGTH                  
020610                                 CHKP-MSG-IO-AREA                         
020700                                 CHKP-AREA-LENGTH CHKP-AREA               
020800     MOVE MSG-STATUS-CODE     TO STATUS-WS                                
020900     PERFORM IMS-STATUSCHECK                                              
021000                                                                          
021100     IF IMS-NOT-OK                                                        
021200       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE'                        
021210                              TO ERROR-TEXT-STR                           
021300       DISPLAY ERROR-TEXT                                                 
021400       CALL FELLOG                                                        
021500     END-IF                                                               
021600     .                                                                    
021700     EJECT                                                                
021800 IMS-STATUSCHECK SECTION.                                                 
021900     SKIP2                                                                
022000     SET STATUS-IX             TO 1                                       
022100     SEARCH GOOD-STATUS                                                   
022200       AT END                                                             
022300         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
022400           DELIMITED BY SIZE INTO ERROR-TEXT                              
022500         DISPLAY ERROR-TEXT                                               
022600         CALL FELLOG                                                      
022700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
022800         CONTINUE                                                         
022900     END-SEARCH                                                           
023000     .                                                                    
