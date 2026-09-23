001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     W6132600.                                                
001400 AUTHOR.         UMESH JAIN.                                              
001500 DATE-WRITTEN.   08/11/17.                                                
001600 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNCTION:                                                            
002000*        TO INSERT ALL THE RECORDS FROM W613.W613S2.W61321(+0)            
002100*        INTO WDT1 DATABASE                                               
002200*                                                                         
002310*        THE PROGRAM UPDATES   WDT1                                       
002400*                                                                         
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003201     SKIP2                                                                
003202*          --- SEGMENTS TO BE INSERTED INTO WDT1                          
003210     SELECT W61321                     ASSIGN TO W61326D1.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003801     SKIP3                                                                
003802 FD  W61321                                                               
003803     RECORDING       F                                                    
003804     BLOCK CONTAINS  0.                                                   
003805                                                                          
003810*01  -COPY WDT101      -L.                                                
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200 77  IDPGM                       PIC X(8)    VALUE 'W6132600'.            
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
005300 01  ERRTEXT.                                                             
005400     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
005500     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
005701                                                                          
005702 77  W61321-EOF-SW               PIC X       VALUE 'N'.                   
005710     88  END-OF-W61321                       VALUE 'Y'.                   
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
007502 01  W61321-AREA-START           PIC X(24)   VALUE                        
007503                                             'W61321-AREA-START'.         
007504     SKIP2                                                                
007505                                                                          
007510*01  AREA -COPY WDT101     -PRE W61321-                                   
007600*                                                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008000 01  KEYS-TILL-DLI.                                                       
008120     03  W-WDT101KY-X.                                                    
008130         05 W-PF-IDDC            PIC X(2)    VALUE SPACE.                 
008140         05 W-PF-IDARTNR         PIC S9(9)   COMP-3 VALUE ZERO.           
008150         05 W-PF-TIORDTIME       PIC 9(12)   VALUE ZERO.                  
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
010301 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT101'.                      
010302 01  DLI-IO-WDT101.                                                       
010310*    03  -COPY WDT101                                                     
010400                                                                          
010800     EJECT                                                                
010900 LINKAGE SECTION.                                                         
011000                                                                          
011100*01  -COPY W0009   -PRE MSG-                                              
011201                                                                          
011202*01  -COPY W0008  -PRE WDT1-                                              
011210     05  FILLER                  PIC X.                                   
011500     EJECT                                                                
011601 PROCEDURE DIVISION  USING MSG-PCB WDT1-PCB.                              
011602 MAIN SECTION.                                                            
011610     ENTRY 'DLITCBL' USING MSG-PCB WDT1-PCB.                              
011700                                                                          
011900     SKIP2                                                                
012000     PERFORM A-INIT                                                       
012110     PERFORM S01-READ-W61321                                              
012200     PERFORM UNTIL END-OF-W61321                                          
012300       IF CHKP-ANT > CHKP-MAX                                             
012400         PERFORM X-TAKE-CHECKPOINT                                        
012500       END-IF                                                             
012600                                                                          
012821       MOVE W61321-PF-WDT101 TO PF-WDT101                                 
012830       PERFORM IMS-ISRT-WDT101                                            
012832       PERFORM UNTIL SEGMENT-FOUND                                        
012833         ADD +1 TO PF-TIORDTIME                                           
012834         PERFORM IMS-ISRT-WDT101                                          
012836       END-PERFORM                                                        
012837       IF SEGMENT-FOUND                                                   
012840          ADD +3 TO CHKP-ANT                                              
012841       END-IF                                                             
012860                                                                          
013210       PERFORM S01-READ-W61321                                            
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
014400                                                                          
014500     PERFORM IMS-RESTART                                                  
014710     OPEN INPUT W61321                                                    
015410     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015700     .                                                                    
015900     EJECT                                                                
016000 Z-FINIT SECTION.                                                         
016501                                                                          
016510     CLOSE W61321                                                         
016701     SKIP2                                                                
016702     MOVE 'S' TO POSTSUM-OPKOD                                            
016710     CALL POSTSUM USING POSTSUM-PARM                                      
016900     .                                                                    
017001     EJECT                                                                
017002 S01-READ-W61321  SECTION.                                                
017003     SKIP2                                                                
017004     READ W61321 INTO W61321-AREA                                         
017005     AT END                                                               
017007        SET END-OF-W61321 TO TRUE                                         
017008                                                                          
017009     NOT AT END                                                           
017010        MOVE 'W61321' TO POSTSUM-FDNAMN                                   
017011        MOVE 'W61326D1' TO POSTSUM-DDNAMN2                                
017012        MOVE SPACES     TO POSTSUM-TRANSTYP                               
017013        CALL POSTSUM USING POSTSUM-PARM                                   
017016     END-READ                                                             
017020     .                                                                    
017300     EJECT                                                                
017400 X-TAKE-CHECKPOINT   SECTION.                                             
017500                                                                          
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
018802 IMS-GHU-WDT101 SECTION.                                                  
018803                                                                          
018804     STRING 'WDT101  (WDT101KY =' W-WDT101KY-X ')'                        
018805          DELIMITED BY SIZE INTO SSA1                                     
018806     MOVE '  GE' TO GOOD-STATUSCODES                                      
018807     CALL CBLTDLI USING GHU WDT1-PCB DLI-IO-WDT101 SSA1                   
018808     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
018809     PERFORM IMS-STATUSCHECK                                              
018810     .                                                                    
018811     SKIP3                                                                
018812 IMS-ISRT-WDT101 SECTION.                                                 
018813                                                                          
018814     MOVE 'WDT101 ' TO SSA1                                               
018815     MOVE '  IINI' TO GOOD-STATUSCODES                                    
018816     CALL CBLTDLI USING ISRT WDT1-PCB DLI-IO-WDT101 SSA1                  
018817     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
018818     PERFORM IMS-STATUSCHECK                                              
018820     .                                                                    
018900     EJECT                                                                
019000 IMS-RESTART SECTION.                                                     
019100     SKIP2                                                                
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
021200       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE' TO ERRTEXT-STR         
021300       DISPLAY ERRTEXT                                                    
021400       CALL FELLOG                                                        
021500     END-IF                                                               
021600     .                                                                    
021700     EJECT                                                                
021800 IMS-STATUSCHECK SECTION.                                                 
021900     SKIP2                                                                
022000     SET STATUS-IX TO 1                                                   
022100     SEARCH GOOD-STATUS                                                   
022200       AT END                                                             
022300         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
022400           DELIMITED BY SIZE INTO ERRTEXT                                 
022500         DISPLAY ERRTEXT                                                  
022600         CALL FELLOG                                                      
022700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
022800         CONTINUE                                                         
022900     END-SEARCH                                                           
023000     .                                                                    
