001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     W4769700.                                                
001400 AUTHOR.         STINA MOGREN.                                            
001500 DATE-WRITTEN.   03/02/11.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800                                                                          
001900*    FUNCTION:                                                            
002000*        IT READS W47696 FILE AND DELETES IDSHIPM. FILE W47696 IS         
002100*        CREATED BY W4769600 PROGRAM FOR WDE2 WITH DISTRICT               
002110*        AND IT'S 'DAYS' ARE EXCEEDED                                     
002200*                                                                         
002310*        THE PROGRAM UPDATES   WDE2                                       
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003201     SKIP2                                                                
003202*          --- I/P FILE FOR WDE2 FOR DELETE                               
003210     SELECT W47696                     ASSIGN TO W47697D1.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003801     SKIP3                                                                
003802 FD  W47696                                                               
003803     RECORDING       F                                                    
003804     BLOCK CONTAINS  0.                                                   
003805                                                                          
003810*01  -COPY W4769001      -L.                                              
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200 77  IDPGM                       PIC X(8)    VALUE 'W4769700'.            
004300 01  CHKP-VAR.                                                            
004400     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004500     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004600     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004700     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004800     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004900     03 CHKP-MAX                 PIC S9(3)   VALUE +400 COMP-3.           
005000 77  YES                         PIC X       VALUE 'J'.                   
005100 77  NOO                         PIC X       VALUE 'N'.                   
005200     SKIP2                                                                
005300 01  ERRTEXT.                                                             
005400     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
005500     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
005701                                                                          
005702 77  W47696-EOF-SW               PIC X       VALUE 'N'.                   
005710     88  END-OF-W47696                       VALUE 'Y'.                   
006000     EJECT                                                                
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
007502 01  IN1-AREA-START              PIC X(24)   VALUE                        
007503                                             'IN1-AREA-START'.            
007504     SKIP2                                                                
007505                                                                          
007510*01  AREA -COPY W4769001     -PRE IN1-                                    
007600*                                                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008000 01  KEYS-TILL-DLI.                                                       
008101     03  W-IDSHIPM-X.                                                     
008110         05  W-IDSHIPM           PIC 9(7)   VALUE ZERO.                   
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
010301 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDE201'.             
010302 01  DLI-IO-WDE201.                                                       
010310*    03  -COPY WDE201                                                     
010400                                                                          
010800     EJECT                                                                
010900 LINKAGE SECTION.                                                         
011000                                                                          
011100*01  -COPY W0009   -PRE MSG-                                              
011201                                                                          
011202*01  -COPY W0008   -PRE WDE2-                                             
011210     05  FILLER                  PIC X.                                   
011500     EJECT                                                                
011600                                                                          
011601 PROCEDURE DIVISION  USING MSG-PCB WDE2-PCB.                              
011602 MAIN SECTION.                                                            
011610     ENTRY 'DLITCBL' USING MSG-PCB WDE2-PCB.                              
011700                                                                          
011900     SKIP2                                                                
012000     PERFORM A-INIT                                                       
012110     PERFORM S01-READ-W47696                                              
012200     PERFORM UNTIL END-OF-W47696                                          
012300       IF CHKP-ANT > CHKP-MAX                                             
012310         PERFORM IMS-CHECKPOINT                                           
012320         MOVE ZERO TO CHKP-ANT                                            
012500       END-IF                                                             
012510       MOVE IN1-DEL-IDSHIPM  TO W-IDSHIPM                                 
012600       PERFORM IMS-GHU-WDE201                                             
012700       IF SEGMENT-FOUND                                                   
012800         ADD 1               TO CHKP-ANT                                  
012900         PERFORM IMS-DLET-WDE201                                          
013000       END-IF                                                             
013100                                                                          
013210       PERFORM S01-READ-W47696                                            
013300     END-PERFORM                                                          
013400                                                                          
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
014701                                                                          
014710     OPEN INPUT W47696                                                    
015000                                                                          
015300                                                                          
015410     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015700     .                                                                    
015900     EJECT                                                                
016000 Z-FINIT SECTION.                                                         
016100                                                                          
016501                                                                          
016510     CLOSE W47696                                                         
016701     SKIP2                                                                
016702     MOVE 'S' TO POSTSUM-OPKOD                                            
016710     CALL POSTSUM USING POSTSUM-PARM                                      
016900     .                                                                    
017001     EJECT                                                                
017002 S01-READ-W47696  SECTION.                                                
017003     SKIP2                                                                
017004     READ W47696 INTO IN1-AREA                                            
017005     AT END                                                               
017006        MOVE HIGH-VALUE TO IN1-AREA                                       
017007        SET END-OF-W47696 TO TRUE                                         
017008                                                                          
017009     NOT AT END                                                           
017010        MOVE 'W47696'   TO POSTSUM-FDNAMN                                 
017011        MOVE 'W47697D1' TO POSTSUM-DDNAMN2                                
017013        MOVE SPACE      TO POSTSUM-TRANSTYP                               
017014        CALL POSTSUM USING POSTSUM-PARM                                   
017017     END-READ                                                             
017020     .                                                                    
017300     EJECT                                                                
018600* --- IMS SECTIONS  ---                                                   
018700                                                                          
018801     EJECT                                                                
018802 IMS-GHU-WDE201 SECTION.                                                  
018803                                                                          
018804     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
018805          DELIMITED BY SIZE INTO SSA1                                     
018806     MOVE '  GE' TO GOOD-STATUSCODES                                      
018807     CALL CBLTDLI USING GHU WDE2-PCB DLI-IO-WDE201 SSA1                   
018808     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
018809     PERFORM IMS-STATUSCHECK                                              
018810     .                                                                    
018811     SKIP3                                                                
018812 IMS-DLET-WDE201 SECTION.                                                 
018813                                                                          
018814     MOVE '  ' TO GOOD-STATUSCODES                                        
018815     CALL CBLTDLI USING DLET WDE2-PCB DLI-IO-WDE201                       
018816     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
018817     PERFORM IMS-STATUSCHECK                                              
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
022300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
022400           DELIMITED BY SIZE INTO ERRTEXT                                 
022500         DISPLAY ERRTEXT                                                  
022600         CALL FELLOG                                                      
022700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
022800         CONTINUE                                                         
022900     END-SEARCH                                                           
023000     .                                                                    
