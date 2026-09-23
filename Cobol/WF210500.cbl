000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WF210500.                                                
000300 AUTHOR.         BERNT LUNDH.                                             
000400 DATE-WRITTEN.   OCTOBER 2002.                                            
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*    THE PGM                                                              
000900*    - READS FILE WITH FEEDBACK DATA RECORDS                              
001000*    - SENDS FEEDBACK DATA RECORDS FOR VCCS TO:                           
001100*      PULS (W418)        BY WZ01SEND (CARPARTS.PULS.FBCREDIT)            
001200                                                                          
001300 ENVIRONMENT DIVISION.                                                    
001400                                                                          
001500 INPUT-OUTPUT SECTION.                                                    
001600                                                                          
001700 FILE-CONTROL.                                                            
001800*          --- FEEDBACK DATA RECORDS                                      
001900     SELECT WF2017                     ASSIGN TO WF2105D1.                
002000     EJECT                                                                
002100 DATA DIVISION.                                                           
002200                                                                          
002300 FILE SECTION.                                                            
002400 FD  WF2017                                                               
002500     RECORDING       F                                                    
002600     BLOCK CONTAINS  0.                                                   
002700                                                                          
002800*01  -COPY WF2017      -L.                                                
002900     EJECT                                                                
003000                                                                          
003100 WORKING-STORAGE SECTION.                                                 
003200*** - CONSTANTS                                                           
003300 77  IDPGM                       PIC X(8)    VALUE 'WF210500'.            
003600 77  WS-ADRESS-W418              PIC X(50)                                
003700                                VALUE 'CARPARTS.PULS.FBCREDIT'.           
003800 77  WS-W418                     PIC X(4)    VALUE 'W418'.                
003900                                                                          
004000 77  WF2017-EOF-SW               PIC X       VALUE 'N'.                   
004100     88  END-OF-WF2017                       VALUE 'J'.                   
004200                                                                          
004300 01  W418-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
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
006800 01  IN-AREA-START            PIC X(24)   VALUE 'IN-AREA-START'.          
006900 01  IN-AREA.                                                             
007000*    03  -COPY WF2017                                                     
007100     EJECT                                                                
007200                                                                          
007300 01  W418UT-AREA-START            PIC X(24)   VALUE                       
007400                                              'W418UT-AREA-START'.        
007500 01  W418UT-AREA.                                                         
007600*    03  -COPY WZ01REQU         -PRE W418UT-                              
007700*    03  -COPY WF2105           -PRE W418UT-                              
007800     EJECT                                                                
007900                                                                          
008000 LINKAGE SECTION.                                                         
008100*01  -COPY W0009   -PRE MSG-                                              
008200     EJECT                                                                
008300                                                                          
008400 PROCEDURE DIVISION  USING MSG-PCB.                                       
008500 MAIN SECTION.                                                            
008600     ENTRY 'DLITCBL' USING MSG-PCB.                                       
008700                                                                          
008800     PERFORM A-INIT                                                       
008900     PERFORM B-EXECUTE                                                    
009000     PERFORM Z-FINIT                                                      
009100                                                                          
009200     MOVE ZERO TO RETURN-CODE                                             
009300     GOBACK                                                               
009400     .                                                                    
009500     EJECT                                                                
009600                                                                          
009700 A-INIT SECTION.                                                          
009800     MOVE 1                TO W418UT-REQU-IDMSGVER                        
009900     MOVE SPACE            TO W418UT-REQU-KDPGMACT                        
010000     MOVE IDPGM            TO W418UT-REQU-IDUSER                          
010100                                                                          
010200     OPEN INPUT WF2017                                                    
010300     .                                                                    
010400     EJECT                                                                
010500                                                                          
010600 B-EXECUTE SECTION.                                                       
010700     PERFORM S01-READ-WF2017                                              
010800                                                                          
010900     IF END-OF-WF2017                                                     
011000       CONTINUE                                                           
011100     ELSE                                                                 
011200       PERFORM UNTIL END-OF-WF2017                                        
011500         IF FEED-IDSYSTEM-REC = WS-W418                                   
011600           PERFORM BA-HANDLE-W418                                         
011700         END-IF                                                           
011900         PERFORM S01-READ-WF2017                                          
012000       END-PERFORM                                                        
012100     END-IF                                                               
012200     .                                                                    
012300                                                                          
012400 BA-HANDLE-W418 SECTION.                                                  
012500     IF W418-SEND-IDCOM = ZERO                                            
012600       PERFORM S70-OPEN-W418                                              
012700       MOVE SEND-IDCOM TO W418-SEND-IDCOM                                 
012800     END-IF                                                               
012900                                                                          
013000     MOVE FEED-DAFINDOC         TO W418UT-DAFINDOC                        
013100     MOVE FEED-IDFINDOC         TO W418UT-IDFINDOC                        
013200     MOVE FEED-KDVALISO         TO W418UT-KDVALISO                        
013300     MOVE FEED-PRKURS           TO W418UT-PRKURS                          
013400     MOVE FEED-IDDC             TO W418UT-IDDC                            
013500     MOVE FEED-SUNTO-TOT        TO W418UT-SUNTO-TOT                       
013600     MOVE FEED-SUVAT-BILLIT-TOT TO W418UT-SUVAT-BILLIT-TOT                
013700     MOVE FEED-SUBTO-TOT        TO W418UT-SUBTO-TOT                       
013800     MOVE FEED-IDEXCUST(1)      TO W418UT-IDEXCUST-1                      
013900     MOVE FEED-IDEXCUST(2)      TO W418UT-IDEXCUST-2                      
014000     MOVE FEED-IDREF            TO W418UT-IDREF                           
014100     MOVE FEED-IDARTNR-FINANCE  TO W418UT-IDARTNR-FINANCE                 
014200     MOVE FEED-BEART            TO W418UT-BEART                           
014300     MOVE FEED-PRARTNTO         TO W418UT-PRARTNTO                        
014400     MOVE FEED-IDOPTION(1)      TO W418UT-IDOPTION-1                      
014500     MOVE FEED-IDOPTION(2)      TO W418UT-IDOPTION-2                      
014600     MOVE FEED-KVLEVART         TO W418UT-KVLEVART                        
014700     MOVE FEED-KDVAT            TO W418UT-KDVAT                           
014800     MOVE FEED-SUNTO            TO W418UT-SUNTO                           
014900     MOVE FEED-SUVAT-BILLIT     TO W418UT-SUVAT-BILLIT                    
015000     MOVE FEED-SUBTO            TO W418UT-SUBTO                           
015100     MOVE FEED-KDTRADP          TO W418UT-KDTRADP                         
015200     MOVE FEED-KDVALISO-BET     TO W418UT-KDVALISO-BET                    
015300     MOVE FEED-PRKURS-BET       TO W418UT-PRKURS-BET                      
015400     MOVE FEED-PRKURS-FAKBET    TO W418UT-PRKURS-FAKBET                   
015500                                                                          
015600     PERFORM S80-PUT-W418                                                 
015700     .                                                                    
015800     EJECT                                                                
015900                                                                          
016000 Z-FINIT SECTION.                                                         
016100     CLOSE WF2017                                                         
016200                                                                          
016300     IF W418-SEND-IDCOM > ZERO                                            
016400       PERFORM S90-CLOSE-W418                                             
016500     END-IF                                                               
016600     .                                                                    
016700     EJECT                                                                
016800                                                                          
016900 S01-READ-WF2017  SECTION.                                                
017000     READ WF2017 INTO IN-AREA                                             
017100       AT END                                                             
017200         SET END-OF-WF2017 TO TRUE                                        
017300     END-READ                                                             
017400     .                                                                    
017500     EJECT                                                                
017600                                                                          
017700 S70-OPEN-W418 SECTION.                                                   
017800     MOVE WS-ADRESS-W418                  TO SEND-ADDISPABS               
017900     MOVE 'OPEN'                          TO SEND-KDFUNC                  
018000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
018100                         SEND-OPEN-AREA                                   
018200     IF SEND-KDRC > ZERO                                                  
018300       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
018400       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
018500       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
018600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
018700     END-IF                                                               
018800     .                                                                    
018900                                                                          
019000 S80-PUT-W418 SECTION.                                                    
019100     MOVE 'PUT'                           TO SEND-KDFUNC                  
019200     MOVE W418-SEND-IDCOM                 TO SEND-IDCOM                   
019300     MOVE LENGTH OF W418UT-AREA           TO SEND-KVDLEN                  
019400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
019500                         SEND-KVDLEN                                      
019600                         W418UT-AREA                                      
019700     IF SEND-KDRC > ZERO                                                  
019800       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
019900       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
020000       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
020100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
020200     END-IF                                                               
020300     .                                                                    
020400                                                                          
020500 S90-CLOSE-W418 SECTION.                                                  
020600     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
020700     MOVE W418-SEND-IDCOM                 TO SEND-IDCOM                   
020800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
020900     .                                                                    
