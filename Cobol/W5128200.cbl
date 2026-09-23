000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5128200.                                                
000300 AUTHOR.         SARASWATHY S.                                            
000400 DATE-WRITTEN.   19/11/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        CREATES FILE WITH STOCK VALUE FOR NON VCC DCS                    
000900*                                                                         
001000*        THE PROGRAM READS  DB WDB6 AND WDD3                              
001010*        THE PROGRAM READS  FILE W01184                                   
001100*                                                                         
001200*    ABENDCODES:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- W01184 - WDK7 EXTRACT FILE                                 
002500     SELECT W01184                     ASSIGN TO W51282D1.                
002510     SKIP2                                                                
003000*          --- STOCK VALUE FILE                                           
003100     SELECT W51282                     ASSIGN TO W51282D2.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP2                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W01184                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  -COPY W01184      -L.                                                
004110     SKIP3                                                                
004900 FD  W51282                                                               
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200                                                                          
005300*01  RECORD -COPY W51282  -PRE  UT-  -L.                                  
005400     EJECT                                                                
005500 WORKING-STORAGE SECTION.                                                 
005600                                                                          
005700 77  IDPGM                       PIC X(8)    VALUE 'W5128200'.            
005800 77  YES                         PIC X       VALUE 'J'.                   
005900 77  NOO                         PIC X       VALUE 'N'.                   
006000                                                                          
006100 77  W01184-EOF-SW               PIC X       VALUE 'N'.                   
006200     88  END-OF-W01184                       VALUE 'J'.                   
006300                                                                          
006700 01  WS-KDTRADP                  PIC X(4)    VALUE SPACES.                
006800 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006900 01  FILLER REDEFINES TODAYS-DATE.                                        
007000     03  TODAYS-DATE-YEAR        PIC 9(2).                                
007100     03  TODAYS-DATE-MONTH       PIC 9(2).                                
007200     03  TODAYS-DATE-DAY         PIC 9(2).                                
007300     EJECT                                                                
007400 01  GENERAL-SUBPROGRAMS.                                                 
007500*                                                                         
007600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008000     SKIP2                                                                
008100*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
008200                                                                          
008300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008600     SKIP2                                                                
008700 01  ERROR-TEXT.                                                          
008800     03  FILLER                  PIC X(10)    VALUE 'ERROR-TEXT'.         
008900     03  ERROR-TEXT-STR          PIC X(70)   VALUE SPACE.                 
009000     EJECT                                                                
009010*    --- DC COPYBOOK                                                      
009020*                                                                         
009030*01  -COPY WWDC99                                                         
009040     EJECT                                                                
009100*    --- PARAMETRAR TILL POSTSUM                                          
009200*                                                                         
009300*01  -COPY W0005   -PRE  POSTSUM-                                         
009400     EJECT                                                                
009500 01  IN1-AREA-START              PIC X(24)   VALUE                        
009600                                 'IN1-AREA-START  '.                      
009700     SKIP2                                                                
009800                                                                          
010500*01  AREA -COPY W01184     -PRE IN1-                                      
010660     EJECT                                                                
010700 01  UT-AREA-START               PIC X(24)   VALUE                        
010800                                 'UT-AREA-START  '.                       
010900     SKIP2                                                                
011000                                                                          
011100*01  AREA -COPY W51282     -PRE UT-                                       
011200     EJECT                                                                
011300*    --- AREAS FOR IMS-SECTIONS                                           
011400*                                                                         
011500     EJECT                                                                
011600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011700     SKIP3                                                                
011800 01  KEYS-FOR-DLI.                                                        
011900     03  W-IDDC-X.                                                        
012000         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
012010     03  W-IDARTNR-X.                                                     
012021         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
012030     03  W-IDSKYLT-X.                                                     
012040         05  W-IDSKYLT           PIC X(3)    VALUE 'GB '.                 
012050     SKIP2                                                                
012100     SKIP2                                                                
012200*    --- STATUS-KOD FRÅN IMS                                              
012300 01  STATUS-WS                   PIC XX.                                  
012400     88  SEGMENT-FOUND                       VALUE '  '.                  
012500     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
012600     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012700     SKIP2                                                                
012800 01  GOOD-STATUSCODES.                                                    
012900     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013000     SKIP3                                                                
013100 01  SSA1                        PIC X(64).                               
013200 01  SSA2                        PIC X(64).                               
013300     EJECT                                                                
013400*    --- IMS FUNCTION CODES                                               
013500*01  -COPY W0003                                                          
013600     EJECT                                                                
013700*    ---  DLI INPUT-OUTPUT AREA                                           
013800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
013900 01  DLI-IO-WDB601.                                                       
014000*    03  -COPY WDB601                                                     
014100     EJECT                                                                
014110 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD301'.                      
014120 01  DLI-IO-WDD301.                                                       
014130*    03  -COPY WDD301                                                     
014140     EJECT                                                                
014150 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
014160 01  DLI-IO-WDD311.                                                       
014170*    03  -COPY WDD311                                                     
014180     EJECT                                                                
014200 LINKAGE SECTION.                                                         
014300                                                                          
014400                                                                          
014500*01  -COPY W0008  -PRE WDB6-                                              
014600     05  FILLER                  PIC X.                                   
014700     EJECT                                                                
014710*01  -COPY W0008  -PRE WDD3-                                              
014720     05  FILLER                  PIC X.                                   
014730     EJECT                                                                
014800 PROCEDURE DIVISION  USING WDB6-PCB WDD3-PCB.                             
014900 MAIN SECTION.                                                            
015000     ENTRY 'DLITCBL' USING WDB6-PCB WDD3-PCB.                             
015100                                                                          
015300     PERFORM A-INIT                                                       
015310     PERFORM S01-READ-W01184                                              
015330                                                                          
015340     PERFORM UNTIL END-OF-W01184                                          
015341       MOVE IN1-SLAG-IDDC       TO W-IDDC                                 
015342                                   WS-IDDC                                
015346       PERFORM S12-GET-KDTRADP                                            
015350       PERFORM B-PROCESS-DATA                                             
015398       PERFORM S01-READ-W01184                                            
015399     END-PERFORM                                                          
015400                                                                          
017000     PERFORM Z-FINIT                                                      
017100                                                                          
017200     MOVE ZERO TO RETURN-CODE                                             
017300     GOBACK                                                               
017400     .                                                                    
017500     EJECT                                                                
017600 A-INIT SECTION.                                                          
017700                                                                          
017800     OPEN INPUT  W01184                                                   
018000                                                                          
018100     OPEN OUTPUT W51282                                                   
018200                                                                          
018300     ACCEPT TODAYS-DATE  FROM DATE                                        
018400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018500     .                                                                    
018600     EJECT                                                                
018700 B-PROCESS-DATA SECTION.                                                  
018800                                                                          
018810     IF XDC-NON-VCC-OWNED                                                 
018820       MOVE IN1-SLAG-IDARTNR    TO UT-IDARTNR                             
018830                                   W-IDARTNR                              
018840       PERFORM S13-GET-PARTDESC                                           
019000       MOVE IN1-SLAG-IDDC       TO UT-IDDC                                
019200       MOVE IN1-SLAG-KVEFRS     TO UT-KVEFRS                              
019300       MOVE IN1-SLAG-KVLS       TO UT-KVLS                                
019400       MOVE IN1-SLAG-PRAVCOST   TO UT-PRAVCOST                            
019500       MOVE WS-KDTRADP          TO UT-KDTRADP                             
019600       PERFORM S11-WRITE-W51282                                           
019610     END-IF                                                               
019700     .                                                                    
019800     EJECT                                                                
019891     .                                                                    
019892     EJECT                                                                
019900 Z-FINIT SECTION.                                                         
020000     CLOSE W01184                                                         
020210           W51282                                                         
020300     SKIP2                                                                
020400     MOVE 'S' TO POSTSUM-OPKOD                                            
020500     CALL POSTSUM USING POSTSUM-PARM                                      
020600     .                                                                    
020700     EJECT                                                                
020900 S01-READ-W01184  SECTION.                                                
021000     READ W01184 INTO IN1-AREA                                            
021100     AT END                                                               
021200        MOVE HIGH-VALUE TO IN1-AREA                                       
021300        MOVE 'J'        TO W01184-EOF-SW                                  
021500     NOT AT END                                                           
021501        MOVE IN1-SLAG-IDDC   TO W-IDDC                                    
021502                                WS-IDDC                                   
021600        MOVE 'W01184' TO POSTSUM-FDNAMN                                   
021700        MOVE 'W51282D1' TO POSTSUM-DDNAMN2                                
021800*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
021900        MOVE SPACES     TO POSTSUM-TRANSTYP                               
022000        CALL POSTSUM USING POSTSUM-PARM                                   
022100     END-READ                                                             
022200     .                                                                    
022300     EJECT                                                                
023900 S11-WRITE-W51282  SECTION.                                               
024000                                                                          
024100     WRITE UT-RECORD FROM UT-AREA                                         
024200                                                                          
024300     MOVE SPACES    TO POSTSUM-TRANSTYP                                   
024400     MOVE 'W51282'  TO POSTSUM-FDNAMN                                     
024500     MOVE 'W51282D2' TO POSTSUM-DDNAMN2                                   
024600     CALL POSTSUM USING POSTSUM-PARM                                      
024700     .                                                                    
024800     EJECT                                                                
024900 S12-GET-KDTRADP SECTION.                                                 
025100     PERFORM IMS-GU-WDB601                                                
025200     IF SEGMENT-FOUND                                                     
025300       MOVE DCS-KDTRADP         TO WS-KDTRADP                             
025400     ELSE                                                                 
025500       MOVE SPACES              TO WS-KDTRADP                             
025600     END-IF                                                               
025700     .                                                                    
025800     EJECT                                                                
025810 S13-GET-PARTDESC SECTION.                                                
025813     PERFORM IMS-GU-WDD311                                                
025814     IF SEGMENT-FOUND                                                     
025816       MOVE TEXT-BEART          TO UT-BEART                               
025817     ELSE                                                                 
025818       MOVE SPACE               TO UT-BEART                               
025819     END-IF                                                               
025880     .                                                                    
025890     EJECT                                                                
025900 S99-ABEND SECTION.                                                       
026000                                                                          
026100     SKIP2                                                                
026200     MOVE 'S' TO POSTSUM-OPKOD                                            
026300     CALL POSTSUM USING POSTSUM-PARM                                      
026400     CALL ABEND USING RKOD-ABEND                                          
026500     .                                                                    
026600     EJECT                                                                
026700* --- IMS SECTIONS  ---                                                   
026800                                                                          
026900     EJECT                                                                
027000 IMS-GU-WDB601 SECTION.                                                   
027100                                                                          
027200     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
027300          DELIMITED BY SIZE INTO SSA1                                     
027400     MOVE '  GE' TO GOOD-STATUSCODES                                      
027500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
027600     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
027700     PERFORM IMS-STATUSCHECK                                              
027800     .                                                                    
027900     EJECT                                                                
027910 IMS-GU-WDD311 SECTION.                                                   
027920                                                                          
027930     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X  ')'                        
027940          DELIMITED BY SIZE INTO SSA1                                     
027950     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X  ')'                        
027960          DELIMITED BY SIZE INTO SSA2                                     
027970     MOVE '  GE' TO GOOD-STATUSCODES                                      
027980     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
027990     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
027991     PERFORM IMS-STATUSCHECK                                              
027992     .                                                                    
027993     EJECT                                                                
028000 IMS-STATUSCHECK SECTION.                                                 
028100                                                                          
028200     SET STATUS-IX TO 1                                                   
028300     SEARCH GOOD-STATUS                                                   
028400       AT END                                                             
028500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
028600           DELIMITED BY SIZE INTO ERROR-TEXT                              
028700         DISPLAY ERROR-TEXT                                               
028800         CALL FELLOG                                                      
028900       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
029000         CONTINUE                                                         
029100     END-SEARCH                                                           
029200     .                                                                    
