001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     W6118B00.                                                
001400 AUTHOR.         UMESH JAIN.                                              
001500 DATE-WRITTEN.   09/12/08.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800                                                                          
001900*    FUNCTION:                                                            
002000*        TO DELETE SUPERCEEDED AND EXPIRED PART NUMBERS FROM              
002100*        WDT211                                                           
002200*                                                                         
002310*        THE PROGRAM UPDATES   WDT2                                       
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003201     SKIP2                                                                
003202*          --- PART NUMBERS TO BE DELETED FROM WDT211                     
003210     SELECT W6118H                     ASSIGN TO W6118BD1.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003801     SKIP3                                                                
003802 FD  W6118H                                                               
003803     RECORDING       F                                                    
003804     BLOCK CONTAINS  0.                                                   
003805                                                                          
003810*01  -COPY W6118H01      -L.                                              
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200 77  IDPGM                       PIC X(8)    VALUE 'W6118B00'.            
004300 01  CHKP-VAR.                                                            
004400     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004500     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004600     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004700     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004800     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004900     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
005000 77  YES                         PIC X       VALUE 'J'.                   
005100 77  NOO                         PIC X       VALUE 'N'.                   
005110 77  WS-ADLAGOMR-ST              PIC X       VALUE 'N'.                   
005120 77  WS-BEFT-ST                  PIC X       VALUE 'N'.                   
005130 77  WS-IDARTNR-ST               PIC X       VALUE 'N'.                   
005200     SKIP2                                                                
005300 01  ERROR-TEXT.                                                          
005400     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
005500     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
005701                                                                          
005702 77  W6118H-EOF-SW               PIC X       VALUE 'N'.                   
005710     88  END-OF-W6118H                       VALUE 'Y'.                   
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
007502 01  W6118H-AREA-START           PIC X(24)   VALUE                        
007503                                             'W6118H-AREA-START'.         
007504     SKIP2                                                                
007505                                                                          
007510*01  AREA -COPY W6118H01     -PRE W6118H-                                 
007600*                                                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008000 01  KEYS-TILL-DLI.                                                       
008010     03  W-IDDC-X.                                                        
008020         05  W-IDDC              PIC 9(2)    VALUE ZERO.                  
008100     03  W-BEFT-X.                                                        
008101         05  W-BEFT              PIC 9(2)    VALUE ZERO.                  
008120     03  W-IDARTNR-X.                                                     
008130         05  W-IDARTNR           PIC S9(9) COMP-3 VALUE ZERO.             
008131     03  W-ADLAGOMR-X.                                                    
008132         05  W-ADLAGOMR            PIC S9(3)    VALUE ZERO COMP-3.        
008140     03  W-TIAAVV1-FOM-X.                                                 
008150         05  W-TIAAVV1-FOM         PIC S9(4)    VALUE 9999 COMP-3.        
008180     03  W-TIAAVV2-FOM-X.                                                 
008190         05  W-TIAAVV2-FOM         PIC S9(4)    VALUE 9999 COMP-3.        
008193     03  W-TIAAVV3-FOM-X.                                                 
008194         05  W-TIAAVV3-FOM         PIC S9(4)    VALUE 9999 COMP-3.        
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
010301 01  FILLER         PIC X(33) VALUE 'DLI-IO-WDT201'.                      
010302 01  DLI-IO-WDT201.                                                       
010310*    03  -COPY WDT201                                                     
010400                                                                          
010500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT211'.                      
010600 01  DLI-IO-WDT211.                                                       
010700*    03  -COPY WDT211                                                     
010720 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT212'.                      
010730 01  DLI-IO-WDT212.                                                       
010740*    03  -COPY WDT212                                                     
010741 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT213'.                      
010742 01  DLI-IO-WDT213.                                                       
010743*    03  -COPY WDT213                                                     
010750                                                                          
010800     EJECT                                                                
010900 LINKAGE SECTION.                                                         
011000                                                                          
011100*01  -COPY W0009  -PRE MSG-                                               
011201                                                                          
011202*01  -COPY W0008  -PRE WDT2-                                              
011210     05  FILLER                  PIC X.                                   
011500     EJECT                                                                
011601 PROCEDURE DIVISION  USING MSG-PCB WDT2-PCB.                              
011602 MAIN SECTION.                                                            
011610     ENTRY 'DLITCBL' USING MSG-PCB WDT2-PCB.                              
011700                                                                          
011900     SKIP2                                                                
012000     PERFORM A-INIT                                                       
012110     PERFORM S01-READ-W6118H                                              
012200     PERFORM UNTIL END-OF-W6118H                                          
012300       IF CHKP-ANT > CHKP-MAX                                             
012400         PERFORM X-TAKE-CHECKPOINT                                        
012500       END-IF                                                             
012600       MOVE LOW-VALUES  TO  W-ADLAGOMR-X                                  
012601                            W-BEFT-X                                      
012602                            W-IDARTNR-X                                   
012603       MOVE 'N'         TO  WS-ADLAGOMR-ST                                
012604                            WS-BEFT-ST                                    
012605                            WS-IDARTNR-ST                                 
012606                                                                          
012607       IF W6118H-ADLAGOMR IS NUMERIC                                      
012610          MOVE W6118H-ADLAGOMR TO W-ADLAGOMR                              
012611          MOVE 'J'             TO WS-ADLAGOMR-ST                          
012612       END-IF                                                             
012613                                                                          
012614       IF W6118H-BEFT IS NUMERIC                                          
012620          MOVE W6118H-BEFT     TO W-BEFT                                  
012621          MOVE 'J'             TO WS-BEFT-ST                              
012622       END-IF                                                             
012623                                                                          
012630       IF W6118H-IDARTNR IS NUMERIC                                       
012700          MOVE W6118H-IDARTNR  TO W-IDARTNR                               
012701          MOVE 'J'             TO WS-IDARTNR-ST                           
012702       END-IF                                                             
012703                                                                          
012710       MOVE 11              TO W-IDDC                                     
012800       PERFORM IMS-GHU-WDT201                                             
012801       IF SEGMENT-FOUND                                                   
012870*** CHECK IF WDT211                                                       
012880         IF WS-ADLAGOMR-ST = 'J'                                          
012890            MOVE W6118H-TIAAVV-FOM  TO W-TIAAVV1-FOM                      
012900            PERFORM IMS-GHNP-WDT211                                       
013000            IF SEGMENT-FOUND                                              
013100               PERFORM IMS-DLET-WDT211                                    
013200               ADD +1 TO CHKP-ANT                                         
013201            END-IF                                                        
013202          END-IF                                                          
013203                                                                          
013204*** CHECK IF WDT212                                                       
013205         IF WS-BEFT-ST = 'J'                                              
013206            MOVE W6118H-TIAAVV-FOM  TO W-TIAAVV2-FOM                      
013207            PERFORM IMS-GHNP-WDT212                                       
013208            IF SEGMENT-FOUND                                              
013209               PERFORM IMS-DLET-WDT212                                    
013210               ADD +1 TO CHKP-ANT                                         
013211            END-IF                                                        
013212          END-IF                                                          
013213                                                                          
013214*** CHECK IF WDT213                                                       
013215         IF WS-IDARTNR-ST = 'J'                                           
013216            MOVE W6118H-TIAAVV-FOM  TO W-TIAAVV3-FOM                      
013217            PERFORM IMS-GHNP-WDT213                                       
013218            IF SEGMENT-FOUND                                              
013219               PERFORM IMS-DLET-WDT213                                    
013220               ADD +1 TO CHKP-ANT                                         
013221            END-IF                                                        
013222          END-IF                                                          
013223       END-IF                                                             
013230       PERFORM S01-READ-W6118H                                            
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
014710     OPEN INPUT W6118H                                                    
015410     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015700     .                                                                    
015900     EJECT                                                                
016000 Z-FINIT SECTION.                                                         
016510     CLOSE W6118H                                                         
016701     SKIP2                                                                
016702     MOVE 'S' TO POSTSUM-OPKOD                                            
016710     CALL POSTSUM USING POSTSUM-PARM                                      
016900     .                                                                    
017001     EJECT                                                                
017002 S01-READ-W6118H  SECTION.                                                
017003     SKIP2                                                                
017004     READ W6118H INTO W6118H-AREA                                         
017005     AT END                                                               
017007        SET END-OF-W6118H TO TRUE                                         
017008                                                                          
017009     NOT AT END                                                           
017010        MOVE 'W6118H'   TO POSTSUM-FDNAMN                                 
017011        MOVE 'W6118BD1' TO POSTSUM-DDNAMN2                                
017012        MOVE SPACE      TO POSTSUM-TRANSTYP                               
017013        CALL POSTSUM USING POSTSUM-PARM                                   
017016     END-READ                                                             
017020     .                                                                    
017300     EJECT                                                                
017400 X-TAKE-CHECKPOINT   SECTION.                                             
017600* --- AT CHECKPOINT YOU LOSE GN-POSITION IN THE BASE                      
017700* --- SAVE DATABASE KEYS IF NECESSARY                                     
018100     PERFORM IMS-CHECKPOINT                                               
018200     MOVE ZERO TO CHKP-ANT                                                
018300* --- REREAD DATABASE IF NECESSARY                                        
018400     .                                                                    
018500     EJECT                                                                
018600* --- IMS SECTIONS  ---                                                   
018700                                                                          
018801     EJECT                                                                
018802 IMS-GHU-WDT201 SECTION.                                                  
018803     STRING 'WDT201  (IDDC     =' W-IDDC-X ')'                            
018804          DELIMITED BY SIZE INTO SSA1                                     
018809     MOVE '  GE' TO GOOD-STATUSCODES                                      
018810     CALL CBLTDLI USING GHU WDT2-PCB DLI-IO-WDT201 SSA1                   
018811     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
018812     PERFORM IMS-STATUSCHECK                                              
018813     .                                                                    
018814     SKIP3                                                                
018815 IMS-GHNP-WDT211 SECTION.                                                 
018816     STRING 'WDT211  *F(ADLAGOMR =' W-ADLAGOMR-X                          
018817                      '&TIAAVVF1 =' W-TIAAVV1-FOM-X ')'                   
018819          DELIMITED BY SIZE INTO SSA1                                     
018820     MOVE '  GE' TO GOOD-STATUSCODES                                      
018821     CALL CBLTDLI USING GHNP WDT2-PCB DLI-IO-WDT211 SSA1                  
018822     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
018823     PERFORM IMS-STATUSCHECK                                              
018824     .                                                                    
018825     SKIP3                                                                
018826 IMS-DLET-WDT211 SECTION.                                                 
018827     MOVE '  ' TO GOOD-STATUSCODES                                        
018828     CALL CBLTDLI USING DLET WDT2-PCB DLI-IO-WDT211                       
018829     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
018830     PERFORM IMS-STATUSCHECK                                              
018840     .                                                                    
018900     EJECT                                                                
019010 IMS-GHNP-WDT212 SECTION.                                                 
019020     STRING 'WDT212  *F(BEFT     =' W-BEFT-X                              
019030                      '&TIAAVVF2 =' W-TIAAVV2-FOM-X ')'                   
019040          DELIMITED BY SIZE INTO SSA1                                     
019050     MOVE '  GE' TO GOOD-STATUSCODES                                      
019060     CALL CBLTDLI USING GHNP WDT2-PCB DLI-IO-WDT212 SSA1                  
019070     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
019080     PERFORM IMS-STATUSCHECK                                              
019090     .                                                                    
019091     SKIP3                                                                
019092 IMS-DLET-WDT212 SECTION.                                                 
019093     MOVE '  ' TO GOOD-STATUSCODES                                        
019094     CALL CBLTDLI USING DLET WDT2-PCB DLI-IO-WDT212                       
019095     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
019096     PERFORM IMS-STATUSCHECK                                              
019097     .                                                                    
019098     EJECT                                                                
019099 IMS-GHNP-WDT213 SECTION.                                                 
019100     STRING 'WDT213  *F(IDARTNR  =' W-IDARTNR-X                           
019101                      '&TIAAVVF3 =' W-TIAAVV3-FOM-X ')'                   
019102          DELIMITED BY SIZE INTO SSA1                                     
019103     MOVE '  GE' TO GOOD-STATUSCODES                                      
019104     CALL CBLTDLI USING GHNP WDT2-PCB DLI-IO-WDT213 SSA1                  
019105     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
019106     PERFORM IMS-STATUSCHECK                                              
019107     .                                                                    
019108     SKIP3                                                                
019109 IMS-DLET-WDT213 SECTION.                                                 
019110     MOVE '  ' TO GOOD-STATUSCODES                                        
019111     CALL CBLTDLI USING DLET WDT2-PCB DLI-IO-WDT213                       
019112     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
019113     PERFORM IMS-STATUSCHECK                                              
019114     .                                                                    
019115     EJECT                                                                
019116 IMS-RESTART SECTION.                                                     
019120     SKIP2                                                                
019200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
019300     MOVE '  ' TO GOOD-STATUSCODES                                        
019400     CALL CBLTDLI USING XRST MSG-PCB                                      
019500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
019600                        CHKP-AREA-LENGTH CHKP-AREA                        
019700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
019800     PERFORM IMS-STATUSCHECK                                              
019900     .                                                                    
020000     SKIP3                                                                
020100 IMS-CHECKPOINT SECTION.                                                  
020200     SKIP2                                                                
020300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020400     MOVE '  XD' TO GOOD-STATUSCODES                                      
020500     CALL CBLTDLI USING CHKP MSG-PCB                                      
020600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
020700                        CHKP-AREA-LENGTH CHKP-AREA                        
020800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
020900     PERFORM IMS-STATUSCHECK                                              
021000                                                                          
021100     IF IMS-NOT-OK                                                        
021200       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE'                        
021210                                       TO ERROR-TEXT-STR                  
021300       DISPLAY ERROR-TEXT                                                 
021400       CALL FELLOG                                                        
021500     END-IF                                                               
021600     .                                                                    
021700     EJECT                                                                
021800 IMS-STATUSCHECK SECTION.                                                 
021900     SKIP2                                                                
022000     SET STATUS-IX TO 1                                                   
022100     SEARCH GOOD-STATUS                                                   
022200       AT END                                                             
022300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
022400           DELIMITED BY SIZE INTO ERROR-TEXT                              
022500         DISPLAY ERROR-TEXT                                               
022600         CALL FELLOG                                                      
022700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
022800         CONTINUE                                                         
022900     END-SEARCH                                                           
023000     .                                                                    
