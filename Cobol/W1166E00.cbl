000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1166E00.                                                
000300 AUTHOR.         CHESTER COUCH.                                           
000400 DATE-WRITTEN.   21/04/06.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        THIS PROGRAM READS THE TOTAL PART INFO FILE PRODUCED BY          
001000*        PROGRAM W1166D FOR VSIM, RECORD BY RECORD. FOR EACH RECOR        
001100*        THIS PROGRAM LOOKS UP THE PARTNER ID, I.E. IDLEVNR-EMB,          
001200*        IN SEGMENT WDB601 FOR THE CONCERNED DC.                          
001300*                                                                         
001400*        THE INPUT FILE IS ASSUMED TO BE SORTED IN                        
001500*        ASCENDING DC ORDER TO GIVE GOOD PERFORMANCE.                     
001600*                                                                         
001700*        THE PROGRAM READS     WDB6                                       
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- THE TOTAL PART INFO FILE FOR VSIM                          
002800     SELECT W1166D                     ASSIGN TO W1166ED1.                
002900     SKIP2                                                                
003000*          --- THE TOTAL VSIM PART INFO FILE WITH PARTNER ID ADDED        
003100     SELECT W1166E                     ASSIGN TO W1166ED2.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W1166D                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  -COPY W1166D      -L.                                                
004200     SKIP3                                                                
004300 FD  W1166E                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  RECORD -COPY W1166E -PRE  OUT-  -L.                                  
004800     EJECT                                                                
004900 WORKING-STORAGE SECTION.                                                 
005000                                                                          
005100 77  IDPGM                       PIC X(8)    VALUE 'W1166E00'.            
005200 77  YES                         PIC X       VALUE 'J'.                   
005300 77  NOO                         PIC X       VALUE 'N'.                   
005400     SKIP2                                                                
005500 01  ERROR-TEXT.                                                          
005600     03  FILLER                  PIC X(8)    VALUE 'ERRORTXT'.            
005700     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
005800                                                                          
005900 77  W1166D-EOF-SW               PIC X       VALUE 'N'.                   
006000     88  END-OF-W1166D                       VALUE 'Y'.                   
006100     EJECT                                                                
006200 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006300 01  FILLER REDEFINES TODAYS-DATE.                                        
006400     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006500     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006600     03  TODAYS-DATE-DAY         PIC 9(2).                                
006700     EJECT                                                                
006800 01  GENERAL-SUBPROGRAMS.                                                 
006900*                                                                         
007000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007300     EJECT                                                                
007400*    --- PARAMETRAR TILL POSTSUM                                          
007500*                                                                         
007600*01  -COPY W0005   -PRE  POSTSUM-                                         
007700     EJECT                                                                
007800 01  IN-AREA-START               PIC X(24)   VALUE                        
007900                                             'IN-AREA-START'.             
008000     SKIP2                                                                
008100                                                                          
008200*01  IN-AREA -COPY W1166D                                                 
008300     EJECT                                                                
008400 01  OUT-AREA-START              PIC X(24)   VALUE                        
008500                                             'OUT-AREA-START'.            
008600     SKIP2                                                                
008700                                                                          
008800*01  OUT-AREA -COPY W1166E                                                
008900*                                                                         
009000     EJECT                                                                
009100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009200     SKIP3                                                                
009300 01  KEYS-TILL-DLI.                                                       
009400     03  W-IDDC-X.                                                        
009500         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
009600     SKIP2                                                                
009700*    --- STATUS-KOD FRÅN IMS                                              
009800 01  STATUS-WS                   PIC XX.                                  
009900     88  SEGMENT-FOUND                       VALUE '  '.                  
010000     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
010100     88  SEGMENT-MISSING                     VALUE 'GE'.                  
010200     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
010300     88  IMS-NOT-OK                          VALUE 'XD'.                  
010400     SKIP2                                                                
010500 01  GOOD-STATUSCODES.                                                    
010600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010700     SKIP3                                                                
010800 01  SSA1                        PIC X(64).                               
010900 01  SSA2                        PIC X(64).                               
011000     EJECT                                                                
011100*    --- IMS FUNCTION CODES                                               
011200*01  -COPY W0003                                                          
011300     EJECT                                                                
011400*    ---  DLI INPUT-OUTPUT AREA                                           
011500                                                                          
011600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
011700 01  DLI-IO-WDB601.                                                       
011800*    03  -COPY WDB601                                                     
011900                                                                          
012000     EJECT                                                                
012100 LINKAGE SECTION.                                                         
012200                                                                          
012500*01  -COPY W0008  -PRE WDB6-                                              
012600     05  FILLER                  PIC X.                                   
012700     EJECT                                                                
012800 PROCEDURE DIVISION  USING WDB6-PCB.                                      
012900 MAIN SECTION.                                                            
013000     ENTRY 'DLITCBL' USING WDB6-PCB.                                      
013100                                                                          
013200     SKIP2                                                                
013300     PERFORM A-INIT                                                       
013400     PERFORM S01-READ-W1166D                                              
013500     PERFORM UNTIL END-OF-W1166D                                          
013600       IF IDDC IN IN-AREA     NOT = W-IDDC                                
013700         MOVE IDDC IN IN-AREA    TO W-IDDC                                
013800         PERFORM IMS-GU-WDB601                                            
013900         IF SEGMENT-MISSING                                               
014000           MOVE 'DC'             TO DCS-IDLEVNR-EMB                       
014100           MOVE IDDC IN IN-AREA  TO DCS-IDLEVNR-EMB(3:2)                  
014200         END-IF                                                           
014300       END-IF                                                             
014400       PERFORM UNTIL END-OF-W1166D                                        
014500                  OR IDDC IN IN-AREA  NOT = W-IDDC                        
014600                                                                          
014700         MOVE CORR W1166D-CTX     TO W1166E-CTX                           
014900         MOVE DCS-IDLEVNR-EMB     TO IDLEVNR-EMB IN OUT-AREA              
014910         PERFORM S11-WRITE-W1166E                                         
015000                                                                          
015100         PERFORM S01-READ-W1166D                                          
015200       END-PERFORM                                                        
015300     END-PERFORM                                                          
015400                                                                          
015500     PERFORM Z-FINIT                                                      
015600                                                                          
015700     MOVE ZERO TO RETURN-CODE                                             
015800     GOBACK                                                               
015900     .                                                                    
016000     EJECT                                                                
016100 A-INIT SECTION.                                                          
016200     SKIP2                                                                
016300                                                                          
016400     OPEN INPUT W1166D                                                    
016500                                                                          
016600     OPEN OUTPUT W1166E                                                   
016700                                                                          
016800                                                                          
016900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017000     .                                                                    
017100     EJECT                                                                
017200 Z-FINIT SECTION.                                                         
017300                                                                          
017400                                                                          
017500     CLOSE W1166D                                                         
017600                                                                          
017700           W1166E                                                         
017800     SKIP2                                                                
017900     MOVE 'S' TO POSTSUM-OPKOD                                            
018000     CALL POSTSUM USING POSTSUM-PARM                                      
018100     .                                                                    
018200     EJECT                                                                
018300 S01-READ-W1166D  SECTION.                                                
018400     SKIP2                                                                
018500     READ W1166D INTO IN-AREA                                             
018600     AT END                                                               
018700        MOVE HIGH-VALUE TO IDDC IN IN-AREA                                
018800        SET END-OF-W1166D TO TRUE                                         
018900                                                                          
019000     NOT AT END                                                           
019100        MOVE 'W1166D' TO POSTSUM-FDNAMN                                   
019200        MOVE 'W1166ED1' TO POSTSUM-DDNAMN2                                
019300        MOVE SPACE     TO POSTSUM-TRANSTYP                                
019400        CALL POSTSUM USING POSTSUM-PARM                                   
019500     END-READ                                                             
019600     .                                                                    
019700     EJECT                                                                
019800 S11-WRITE-W1166E SECTION.                                                
019900     SKIP2                                                                
020000     WRITE OUT-RECORD  FROM OUT-AREA                                      
020100                                                                          
020200     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
020300     MOVE 'W1166E ' TO POSTSUM-FDNAMN                                     
020400     MOVE 'W1166ED2' TO POSTSUM-DDNAMN2                                   
020500     CALL POSTSUM USING POSTSUM-PARM                                      
020600     .                                                                    
020700     EJECT                                                                
020800 IMS-GU-WDB601 SECTION.                                                   
020900                                                                          
021000     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
021100          DELIMITED BY SIZE INTO SSA1                                     
021200     MOVE '  GE' TO GOOD-STATUSCODES                                      
021300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
021400     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
021500     PERFORM IMS-STATUSCHECK                                              
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
