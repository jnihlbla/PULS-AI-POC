000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4183E00.                                                
000300 AUTHOR.         ANDERS HENRIKSSON.                                       
000400 DATE-WRITTEN.   2010-01-21.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*    THE PGM                                                              
000900*    - READS FILE WITH DATA TO VIPS                                       
001000*    - SENDS DATA TO VIPS FOR HANDLING FEE BY:                            
001100*      . D&P                                                              
001200                                                                          
001300 ENVIRONMENT DIVISION.                                                    
001400 INPUT-OUTPUT SECTION.                                                    
001500 FILE-CONTROL.                                                            
001600*          --- FEEDBACK DATA RECORDS                                      
001700     SELECT W4183C                     ASSIGN TO W4183ED1.                
001800     EJECT                                                                
001900 DATA DIVISION.                                                           
002000 FILE SECTION.                                                            
002100 FD  W4183C                                                               
002200     RECORDING       F                                                    
002300     BLOCK CONTAINS  0.                                                   
002400                                                                          
002500*01  -COPY W4183C      -L.                                                
002600     EJECT                                                                
002700                                                                          
002800 WORKING-STORAGE SECTION.                                                 
002900*** - CONSTANTS                                                           
003000 77  IDPGM                       PIC X(8)    VALUE 'W4183E00'.            
003100 77  SYSIN-EOF                   PIC X       VALUE 'N'.                   
003200 77  YES                         PIC X       VALUE 'J'.                   
003300 77  NOO                         PIC X       VALUE 'N'.                   
003400 77  OK-SW                       PIC X       VALUE 'N'.                   
003500 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
003600 77  FIRST-TIME-SW               PIC X       VALUE 'J'.                   
003700 77  W4183C-EOF-SW               PIC X       VALUE 'N'.                   
003800     88  END-OF-W4183C                       VALUE 'J'.                   
003900 77  WS-ADRESS                   PIC X(50)                                
004000                           VALUE 'CARPARTS.DAP.DISTRDOC'.                 
004100     EJECT                                                                
004200                                                                          
004300 01  WS-IDPARTNR                 PIC X(9)       VALUE SPACE.              
004400                                                                          
004500 01  ERRTEXT.                                                             
004600     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
004700     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
004800 01  KDRC-DISPLAY                PIC Z(5).                                
004900     EJECT                                                                
005000                                                                          
005100 01  GENERAL-SUBPROGRAMS.                                                 
005200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005300     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
005400     EJECT                                                                
005500                                                                          
005600*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
005700                                                                          
005800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006100     EJECT                                                                
006200                                                                          
006300*    --- AREOR FÖR KOMMUNIKATION                                          
006400 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
006500*01  -COPY WZ01SEND                                                       
006600     EJECT                                                                
006700                                                                          
006800 01  INPUT-AREA                 PIC X(24)   VALUE                         
006900                                'INPUT-AREA     '.                        
007000                                                                          
007100 01  IN-AREA-START            PIC X(24)   VALUE 'IN-AREA-START'.          
007200 01  IN-AREA.                                                             
007300*    03  -COPY W4183C     -PRE IN-                                        
007400     EJECT                                                                
007500                                                                          
007600*    --- UT-AREOR                                                         
007700 01  OUTPUT-AREA                 PIC X(24)   VALUE                        
007800                                'OUTPUT-AREA     '.                       
007900 01  HDR-AREA.                                                            
008000*    03  -COPY WZ01REQU                                                   
008100*    03  -COPY WZ04HDR                                                    
008200     EJECT                                                                
008300                                                                          
008400 01  LINE-AREA.                                                           
008500*    03  -COPY WZ01REQU         -PRE VIPSUT-                              
008600*    03  -COPY W4183C           -PRE VIPSUT-                              
008700     EJECT                                                                
008800                                                                          
008900 LINKAGE SECTION.                                                         
009000*01  -COPY W0009   -PRE MSG-                                              
009100     EJECT                                                                
009200                                                                          
009300 PROCEDURE DIVISION  USING MSG-PCB.                                       
009400 MAIN SECTION.                                                            
009500     ENTRY 'DLITCBL' USING MSG-PCB.                                       
009600                                                                          
009700     PERFORM A-INIT                                                       
009800     PERFORM B-EXECUTE                                                    
009900     PERFORM Z-FINIT                                                      
010000                                                                          
010100     MOVE ZERO TO RETURN-CODE                                             
010200     GOBACK                                                               
010300     .                                                                    
010400 A-INIT SECTION.                                                          
010500     OPEN INPUT W4183C                                                    
010600     MOVE YES TO FIRST-TIME-SW                                            
010700     .                                                                    
010800     EJECT                                                                
010900                                                                          
011000 B-EXECUTE SECTION.                                                       
011100     PERFORM S01-READ-W4183C                                              
011200                                                                          
011300     IF END-OF-W4183C                                                     
011400       CONTINUE                                                           
011500     ELSE                                                                 
011600       PERFORM UNTIL END-OF-W4183C                                        
011700         PERFORM BA-SEND-TO-VIPS                                          
011800         PERFORM S01-READ-W4183C                                          
011900       END-PERFORM                                                        
012000     END-IF                                                               
012100     .                                                                    
012200                                                                          
012300 BA-SEND-TO-VIPS  SECTION.                                                
012400     IF IN-IDPARTNR = WS-IDPARTNR                                         
012500       CONTINUE                                                           
012600     ELSE                                                                 
012700       IF FIRST-TIME-SW = YES                                             
012800         PERFORM S90-SEND-OPEN                                            
012900         MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                               
013000         MOVE NOO TO FIRST-TIME-SW                                        
013100         MOVE ZERO            TO WS-IDPARTNR                              
013200         PERFORM BAA-HANDLE-HEADER                                        
013300         PERFORM S90-PUT-HEADER                                           
013400       ELSE                                                               
013500         PERFORM S90-SEND-CLOSE                                           
013600         MOVE ZERO            TO WZ04-SEND-IDCOM                          
013700         PERFORM S90-SEND-OPEN                                            
013800         MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                               
013900         MOVE ZERO            TO WS-IDPARTNR                              
014000         PERFORM BAA-HANDLE-HEADER                                        
014100         PERFORM S90-PUT-HEADER                                           
014200       END-IF                                                             
014300     END-IF                                                               
014400                                                                          
014500     IF IN-IDPARTNR   NOT = WS-IDPARTNR                                   
014600       MOVE IN-IDPARTNR         TO WS-IDPARTNR                            
014700       PERFORM BAB-SEND-LINE-TO-VIPS                                      
014800     ELSE                                                                 
014900       PERFORM BAB-SEND-LINE-TO-VIPS                                      
015000     END-IF                                                               
015100     .                                                                    
015200                                                                          
015300 BAA-HANDLE-HEADER SECTION.                                               
015400     MOVE 1                          TO REQU-IDMSGVER                     
015500     MOVE 'R'                        TO REQU-KDPGMACT                     
015600     MOVE IDPGM                      TO REQU-IDUSER                       
015700                                                                          
015800     MOVE 'W4183C-001'               TO HDR-IDOUTTYPE                     
015900     MOVE SPACE                      TO HDR-IDOUTREC                      
016000     MOVE IN-IDPARTNR                TO HDR-IDOUTREC(1:9)                 
016100     MOVE IN-IDFINDOC                TO HDR-IDLIST                        
016200     .                                                                    
016300                                                                          
016400 BAB-SEND-LINE-TO-VIPS SECTION.                                           
016500     MOVE 1                     TO VIPSUT-REQU-IDMSGVER                   
016600     MOVE 'R'                   TO VIPSUT-REQU-KDPGMACT                   
016700     MOVE IDPGM                 TO VIPSUT-REQU-IDUSER                     
016800                                                                          
016900     MOVE IN-IDPTYP             TO VIPSUT-IDPTYP                          
017000     MOVE IN-IDFINDOC           TO VIPSUT-IDFINDOC                        
017100     MOVE IN-DAFINDOC           TO VIPSUT-DAFINDOC                        
017200     MOVE IN-IDPARTNR           TO VIPSUT-IDPARTNR                        
017300     MOVE IN-IDDISTR            TO VIPSUT-IDDISTR                         
017400     MOVE IN-IDKUNDNR           TO VIPSUT-IDKUNDNR                        
017500     MOVE IN-BEART              TO VIPSUT-BEART                           
017600     MOVE IN-KVLEVART-VIPS      TO VIPSUT-KVLEVART-VIPS                   
017700     MOVE IN-PRARTNTO-VIPS      TO VIPSUT-PRARTNTO-VIPS                   
017800     MOVE IN-SUNTO-VIPS         TO VIPSUT-SUNTO-VIPS                      
017900     MOVE IN-SUVAT-VIPS         TO VIPSUT-SUVAT-VIPS                      
018000     MOVE IN-SUBTO-VIPS         TO VIPSUT-SUBTO-VIPS                      
018100     MOVE IN-KDVAT              TO VIPSUT-KDVAT                           
018200     MOVE IN-KDVALISO-LOC       TO VIPSUT-KDVALISO-LOC                    
018300     MOVE IN-PRKURS             TO VIPSUT-PRKURS                          
018400     MOVE IN-IDREF              TO VIPSUT-IDREF                           
018500                                                                          
018600     PERFORM S90-PUT-LINE                                                 
018700     .                                                                    
018800                                                                          
018900 Z-FINIT SECTION.                                                         
019000     IF FIRST-TIME-SW = NOO                                               
019100       PERFORM S90-SEND-CLOSE                                             
019200     END-IF                                                               
019300     .                                                                    
019400     EJECT                                                                
019500                                                                          
019600 S01-READ-W4183C  SECTION.                                                
019700     READ W4183C INTO IN-AREA                                             
019800       AT END                                                             
019900         SET END-OF-W4183C TO TRUE                                        
020000     END-READ                                                             
020100     .                                                                    
020200     EJECT                                                                
020300                                                                          
020400 S90-SEND-OPEN SECTION.                                                   
020500     MOVE WS-ADRESS                       TO SEND-ADDISPABS               
020600     MOVE 'OPEN'                          TO SEND-KDFUNC                  
020700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
020800                         SEND-OPEN-AREA                                   
020900     IF SEND-KDRC > ZERO                                                  
021000       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
021100       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
021200       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
021300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
021400     END-IF                                                               
021500     .                                                                    
021600                                                                          
021700 S90-PUT-HEADER SECTION.                                                  
021800     MOVE 'PUT'                           TO SEND-KDFUNC                  
021900     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
022000     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
022100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
022200                         SEND-KVDLEN                                      
022300                         HDR-AREA                                         
022400     IF SEND-KDRC > ZERO                                                  
022500       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
022600       STRING 'WZ01SEND PUT HDR ERROR RC=' KDRC-DISPLAY                   
022700       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
022800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
022900     END-IF                                                               
023000     .                                                                    
023100                                                                          
023200 S90-PUT-LINE SECTION.                                                    
023300     MOVE 'PUT'                           TO SEND-KDFUNC                  
023400     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
023500     MOVE LENGTH OF LINE-AREA             TO SEND-KVDLEN                  
023600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
023700                         SEND-KVDLEN                                      
023800                         LINE-AREA                                        
023900     IF SEND-KDRC > ZERO                                                  
024000       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
024100       STRING 'WZ01SEND PUT LINE ERROR RC=' KDRC-DISPLAY                  
024200       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
024300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
024400     END-IF                                                               
024500     .                                                                    
024600                                                                          
024700 S90-SEND-CLOSE SECTION.                                                  
024800     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
024900     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
025000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
025100     .                                                                    
