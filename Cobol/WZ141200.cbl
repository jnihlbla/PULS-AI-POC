000100*COMPOPT DB2BIND=YES                                                      
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WZ141200.                                                
000400 AUTHOR.         ANDRE KJELL.                                             
000500 DATE-WRITTEN.   02/09/16.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNCTION:                                                            
001000*        GENERAL PROGRAM TO PROCESS A REPORT FILE                         
001100*        IN DISTRIBUTION & PRINT.                                         
001200*                                                                         
001300*        A CONTROL FILE CONTAINING D&P RULE DATA IS ALSO READ.            
001400*                                                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- INPUT DATA                                                 
002400     SELECT INDATA                     ASSIGN TO WZ1412D1.                
002500     SKIP2                                                                
002600*          --- CONTROL FILE                                               
002700     SELECT CTLDATA                    ASSIGN TO WZ1412D2.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  INDATA                                                               
003400     RECORDING       V                                                    
003500     RECORD IS VARYING FROM 1 TO 3000 CHARACTERS                          
003600            DEPENDING ON IN-KVDLEN                                        
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900 01  FILLER                     PIC X(3000).                              
004000     SKIP3                                                                
004100 FD  CTLDATA                                                              
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500 01  FILLER                     PIC X(80).                                
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900 77  IDPGM                       PIC X(8)    VALUE 'WZ141200'.            
005000 77  YES                         PIC X       VALUE 'J'.                   
005100 77  NOO                         PIC X       VALUE 'N'.                   
005200     SKIP2                                                                
005300 01  ERRTEXT.                                                             
005400     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
005500     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
005600 77  KDRC-DISPLAY                PIC Z(3)9.                               
005700                                                                          
005800 77  INDATA-EOF-SW               PIC X       VALUE 'N'.                   
005900     88  END-OF-INDATA                       VALUE 'Y'.                   
006000                                                                          
006100 77  CTLDATA-EOF-SW              PIC X       VALUE 'N'.                   
006200     88  END-OF-CTLDATA                      VALUE 'Y'.                   
006300                                                                          
006400     EJECT                                                                
006500 01  GENERAL-SUBPROGRAMS.                                                 
006600*                                                                         
006700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
006900     03  WZ04DAP                 PIC X(8)    VALUE 'WZ04DAP '.            
007000     SKIP3                                                                
007100*    --- PARAMETERS TO ABEND                                              
007200                                                                          
007300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007600     EJECT                                                                
007700*    --- PARAMETERS TO POSTSUM                                            
007800*                                                                         
007900*01  -COPY W0005   -PRE  POSTSUM-                                         
008000     EJECT                                                                
008100 01  IN-AREA-START               PIC X(16)   VALUE  'IN-AREA'.            
008200                                                                          
008300 01  IN-KVDLEN                   PIC 9(9)   BINARY.                       
008400                                                                          
008500 01  IN-AREA                     PIC X(3000).                             
008600     EJECT                                                                
008700 01  CTL-AREA-START              PIC X(16)   VALUE 'CTL-AREA'.            
008800                                                                          
008900     SKIP2                                                                
009000 01  CTL-AREA                    PIC X(80).                               
009100     EJECT                                                                
009200 01  FILLER                      PIC X(16)   VALUE 'DAP-AREA '.           
009300     SKIP3                                                                
009400 01  -COPY WZ04DAP                                                        
009500     EJECT                                                                
009600                                                                          
009700 PROCEDURE DIVISION.                                                      
009800 MAIN SECTION.                                                            
009900                                                                          
010000     SKIP2                                                                
010100     PERFORM A-INIT                                                       
010200     PERFORM S04-DAP-OPEN                                                 
010300     IF DAP-KDRC = 0                                                      
010400       PERFORM S01-READ-INDATA                                            
010500       PERFORM UNTIL END-OF-INDATA                                        
010600         PERFORM S04-DAP-PUT-FROM-INAREA                                  
010700         PERFORM S01-READ-INDATA                                          
010800       END-PERFORM                                                        
010900     END-IF                                                               
011000                                                                          
011100     PERFORM S04-DAP-CLOSE                                                
011200     PERFORM Z-FINIT                                                      
011300                                                                          
011400     MOVE ZERO TO RETURN-CODE                                             
011500     GOBACK                                                               
011600     .                                                                    
011700     EJECT                                                                
011800 A-INIT SECTION.                                                          
011900                                                                          
012000     OPEN INPUT INDATA CTLDATA                                            
012100                                                                          
012200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
012300                                                                          
012400*    -- FETCH DATA FOR D&P OPEN                                           
012500     PERFORM S02-READ-CTLDATA                                             
012600     IF END-OF-CTLDATA                                                    
012700       STRING 'WZ1412  OUTPUT TYPE MISSING'                               
012800       DELIMITED BY SIZE INTO ERRTEXT                                     
012900     ELSE                                                                 
013000       MOVE CTL-AREA  TO DAP-IDOUTTYPE                                    
013100       PERFORM S02-READ-CTLDATA                                           
013200     END-IF                                                               
013300                                                                          
013400     IF END-OF-CTLDATA                                                    
013500       MOVE SPACE     TO DAP-IDOUTREC                                     
013600     ELSE                                                                 
013700       MOVE CTL-AREA  TO DAP-IDOUTREC                                     
013800       PERFORM S02-READ-CTLDATA                                           
013900     END-IF                                                               
014000                                                                          
014100     IF END-OF-CTLDATA                                                    
014200       MOVE FUNCTION CURRENT-DATE(3:12) TO DAP-IDLIST                     
014300     ELSE                                                                 
014400       MOVE CTL-AREA  TO DAP-IDLIST                                       
014500     END-IF                                                               
014600                                                                          
014700     IF ERRTEXT-STR NOT = SPACE                                           
014800       DISPLAY  ERRTEXT                                                   
014900       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
015000     END-IF                                                               
015100     .                                                                    
015200     EJECT                                                                
015300 Z-FINIT SECTION.                                                         
015400                                                                          
015500     CLOSE INDATA CTLDATA                                                 
015600                                                                          
015700     SKIP2                                                                
015800     MOVE 'S' TO POSTSUM-OPKOD                                            
015900     CALL POSTSUM USING POSTSUM-PARM                                      
016000     .                                                                    
016100     EJECT                                                                
016200 S01-READ-INDATA  SECTION.                                                
016300     SKIP2                                                                
016400     READ INDATA INTO IN-AREA                                             
016500     AT END                                                               
016600        SET END-OF-INDATA TO TRUE                                         
016700                                                                          
016800     NOT AT END                                                           
016900        MOVE 'INDATA'   TO POSTSUM-FDNAMN                                 
017000        MOVE 'WZ1412D1' TO POSTSUM-DDNAMN2                                
017100        MOVE SPACE      TO POSTSUM-TRANSTYP                               
017200        CALL POSTSUM USING POSTSUM-PARM                                   
017300     END-READ                                                             
017400     .                                                                    
017500     EJECT                                                                
017600 S02-READ-CTLDATA  SECTION.                                               
017700     SKIP2                                                                
017800     READ CTLDATA INTO CTL-AREA                                           
017900     AT END                                                               
018000        SET END-OF-CTLDATA TO TRUE                                        
018100                                                                          
018200     NOT AT END                                                           
018300        MOVE 'CTLDATA'  TO POSTSUM-FDNAMN                                 
018400        MOVE 'WZ1412D2' TO POSTSUM-DDNAMN2                                
018500        MOVE SPACE      TO POSTSUM-TRANSTYP                               
018600        CALL POSTSUM USING POSTSUM-PARM                                   
018700     END-READ                                                             
018800     .                                                                    
018900     EJECT                                                                
019000 S04-DAP-OPEN SECTION.                                                    
019100                                                                          
019200     MOVE 'OPEN'                     TO DAP-KDFUNC                        
019300     CALL WZ04DAP USING DAP-WZ04DAP                                       
019400                                                                          
019500     IF DAP-KDRC > 0                                                      
019600       MOVE DAP-KDRC TO KDRC-DISPLAY                                      
019700       STRING 'WZ04DAP "OPEN" ERROR RC=' KDRC-DISPLAY                     
019800              '. ' DAP-BEFEL                                              
019900       DELIMITED BY SIZE INTO ERRTEXT                                     
020000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
020100     END-IF                                                               
020200     .                                                                    
020300     SKIP3                                                                
020400 S04-DAP-PUT-FROM-INAREA SECTION.                                         
020500                                                                          
020600     MOVE 'PUT'                      TO DAP-KDFUNC                        
020700     MOVE IN-KVDLEN TO DAP-KVDLEN                                         
020800     MOVE IN-AREA(1:IN-KVDLEN) TO DAP-TEOUTDATA                           
020900     CALL WZ04DAP USING DAP-WZ04DAP                                       
021000                                                                          
021100     IF DAP-KDRC > 0                                                      
021200       MOVE DAP-KDRC TO KDRC-DISPLAY                                      
021300       STRING 'WZ04DAP "PUT" ERROR RC=' KDRC-DISPLAY                      
021400              '. ' DAP-BEFEL                                              
021500       DELIMITED BY SIZE INTO ERRTEXT                                     
021600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
021700     END-IF                                                               
021800     .                                                                    
021900     SKIP3                                                                
022000 S04-DAP-CLOSE SECTION.                                                   
022100                                                                          
022200     MOVE 'CLOSE'                    TO DAP-KDFUNC                        
022300     CALL WZ04DAP USING DAP-WZ04DAP                                       
022400                                                                          
022500     IF DAP-KDRC > 0                                                      
022600       MOVE DAP-KDRC TO KDRC-DISPLAY                                      
022700       STRING 'WZ04DAP "CLOSE" ERROR RC=' KDRC-DISPLAY                    
022800              '. ' DAP-BEFEL                                              
022900       DELIMITED BY SIZE INTO ERRTEXT                                     
023000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
023100     END-IF                                                               
023200     .                                                                    
