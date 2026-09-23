000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WF211200.                                                
000300 AUTHOR.         ANDERS HENRIKSSON                                        
000400 DATE-WRITTEN.   2008-09-01.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*    THE PGM                                                              
000900*    - READS FILE WITH FEEDBACK DATA RECORDS                              
001000*    - SENDS FEEDBACK DATA RECORDS FOR VCCS TO:                           
001100*      PULS (W418)(INVKRE) BY WZ01SEND (CARPARTS.PULS.FBINVKRE)           
001200                                                                          
001300 ENVIRONMENT DIVISION.                                                    
001400                                                                          
001500 INPUT-OUTPUT SECTION.                                                    
001600                                                                          
001700 FILE-CONTROL.                                                            
001800*          --- FEEDBACK DATA RECORDS                                      
001900     SELECT WF2017                     ASSIGN TO WF2112D1.                
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
003300 77  IDPGM                       PIC X(8)    VALUE 'WF211200'.            
003400 77  WS-IDLEGSEL-VCCS            PIC X(4)    VALUE 'VCCS'.                
003500 77  WS-ADRESS-W418              PIC X(50)                                
003600                                VALUE 'CARPARTS.PULS.FBINVKRE'.           
003700 77  WS-W41K                     PIC X(4)    VALUE 'W41K'.                
003800                                                                          
003900 77  WF2017-EOF-SW               PIC X       VALUE 'N'.                   
004000     88  END-OF-WF2017                       VALUE 'J'.                   
004100                                                                          
004200 01  W418-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
004300                                                                          
004400 01  ERRTEXT.                                                             
004500     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
004600     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
004700 01  KDRC-DISPLAY                PIC Z(5).                                
004800     EJECT                                                                
004900                                                                          
005000 01  GENERAL-SUBPROGRAMS.                                                 
005100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005200     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
005300     EJECT                                                                
005400                                                                          
005500*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
005600                                                                          
005700 77  RKOD-ABEND                  PIC S9(4)  COMP VALUE +0.                
005800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)  COMP VALUE +16.               
005900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)  COMP VALUE +1000.             
006000                                                                          
006100 77  SAVE-DAFINDOC               PIC X(8)   VALUE  SPACE.                 
006200 77  SAVE-IDFINDOC               PIC S9(9)  COMP-3 VALUE +0.              
006300 77  SAVE-IDOPTION-1             PIC X(8)   VALUE  SPACE.                 
006400 77  SAVE-IDOPTION-2             PIC X(8)   VALUE  SPACE.                 
006410 77  SAVE-IDREF                  PIC X(15)  VALUE  SPACE.                 
006500     EJECT                                                                
006600                                                                          
006700*    --- AREOR FÖR KOMMUNIKATION                                          
006800 01  FILLER                      PIC X(16)  VALUE 'SEND-CONTROL'.         
006900*01  -COPY WZ01SEND                                                       
007000     EJECT                                                                
007100                                                                          
007200 01  IN-AREA-START               PIC X(24)  VALUE 'IN-AREA-START'.        
007300 01  IN-AREA.                                                             
007400*    03  -COPY WF2017                                                     
007500     EJECT                                                                
007600                                                                          
007700 01  W418UT-AREA-START            PIC X(24) VALUE                         
007800                                            'W418UT-AREA-START'.          
007900 01  W418UT-AREA.                                                         
008000*    03  -COPY WZ01REQU         -PRE W418UT-                              
008100*    03  -COPY WF2112I1         -PRE W418UT-                              
008200     EJECT                                                                
008300                                                                          
008400 LINKAGE SECTION.                                                         
008500*01  -COPY W0009   -PRE MSG-                                              
008600                                                                          
008700*01  -COPY W0009   -PRE SAVEIT-                                           
008800     EJECT                                                                
008900                                                                          
009000 PROCEDURE DIVISION  USING MSG-PCB SAVEIT-PCB.                            
009100 MAIN SECTION.                                                            
009200                                                                          
009300     ENTRY 'DLITCBL' USING MSG-PCB SAVEIT-PCB.                            
009400                                                                          
009500     PERFORM A-INIT                                                       
009600     PERFORM B-EXECUTE                                                    
009700     PERFORM Z-FINIT                                                      
009800                                                                          
009900     MOVE ZERO TO RETURN-CODE                                             
010000     GOBACK                                                               
010100     .                                                                    
010200     EJECT                                                                
010300                                                                          
010400 A-INIT SECTION.                                                          
010500     MOVE 1                TO W418UT-REQU-IDMSGVER                        
010600     MOVE SPACE            TO W418UT-REQU-KDPGMACT                        
010700     MOVE IDPGM            TO W418UT-REQU-IDUSER                          
010800                                                                          
010900     OPEN INPUT WF2017                                                    
011000     .                                                                    
011100     EJECT                                                                
011200                                                                          
011300 B-EXECUTE SECTION.                                                       
011400     PERFORM S01-READ-WF2017                                              
011600     IF END-OF-WF2017                                                     
011700       CONTINUE                                                           
011800     ELSE                                                                 
011900       PERFORM UNTIL END-OF-WF2017                                        
012000         IF FEED-IDLEGSEL = WS-IDLEGSEL-VCCS                              
012100           IF FEED-IDSYSTEM-REC = WS-W41K                                 
012200             PERFORM BB-HANDLE-W418                                       
012300           END-IF                                                         
012400         END-IF                                                           
012500         PERFORM S01-READ-WF2017                                          
012600       END-PERFORM                                                        
012700     END-IF                                                               
012800     .                                                                    
012900     EJECT                                                                
013000                                                                          
013100 BB-HANDLE-W418 SECTION.                                                  
013200     IF W418-SEND-IDCOM = ZERO                                            
013300       PERFORM S71-OPEN-W418                                              
013400       MOVE SEND-IDCOM TO W418-SEND-IDCOM                                 
013500*      MOVE FEED-DAFINDOC       TO SAVE-DAFINDOC                          
013600*      MOVE FEED-IDFINDOC       TO SAVE-IDFINDOC                          
013700*      MOVE FEED-IDOPTION (1)   TO SAVE-IDOPTION-1                        
013800*      MOVE FEED-IDOPTION (2)   TO SAVE-IDOPTION-2                        
013810*      MOVE FEED-IDREF          TO SAVE-IDREF                             
013900     END-IF                                                               
014000                                                                          
014300*    IF FEED-DAFINDOC     = SAVE-DAFINDOC   AND                           
014400*       FEED-IDFINDOC     = SAVE-IDFINDOC   AND                           
014500*       FEED-IDOPTION (1) = SAVE-IDOPTION-1 AND                           
014600*       FEED-IDOPTION (2) = SAVE-IDOPTION-2 AND                           
014610*       FEED-IDREF        = SAVE-IDREF                                    
014611       PERFORM BBA-FLYTTA                                                 
014800*    ELSE                                                                 
014900**   IF NEW INVOICE/PROD.NO/CASE                                          
015000**   THEN CLOSE TRANSACTION AND START NEW TRANSACTION                     
015100*      IF FEED-DAFINDOC     NOT = SAVE-DAFINDOC   OR                      
015200*         FEED-IDFINDOC     NOT = SAVE-IDFINDOC                           
015700*         PERFORM S91-CLOSE-W418                                          
015800*         PERFORM S71-OPEN-W418                                           
015810*         PERFORM BBA-FLYTTA                                              
015910*      ELSE                                                               
015911*        PERFORM BBA-FLYTTA                                               
015912*      END-IF                                                             
016000*      MOVE FEED-DAFINDOC       TO SAVE-DAFINDOC                          
016100*      MOVE FEED-IDFINDOC       TO SAVE-IDFINDOC                          
016200*      MOVE FEED-IDOPTION (1)   TO SAVE-IDOPTION-1                        
016300*      MOVE FEED-IDOPTION (2)   TO SAVE-IDOPTION-2                        
016310*      MOVE FEED-IDREF          TO SAVE-IDREF                             
016400*    END-IF                                                               
016500     .                                                                    
016600     EJECT                                                                
016700 BBA-FLYTTA   SECTION.                                                    
016800                                                                          
017100     MOVE FEED-DAFINDOC         TO W418UT-DAFINDOC                        
017200     MOVE FEED-IDFINDOC         TO W418UT-IDFINDOC                        
017300     MOVE FEED-KDVALISO         TO W418UT-KDVALISO                        
017400     MOVE FEED-IDPARTNR         TO W418UT-IDPARTNR                        
017500     MOVE FEED-KDVAT            TO W418UT-KDVAT                           
017600     MOVE FEED-IDREF            TO W418UT-IDREF                           
017700     MOVE FEED-SUVAT-BILLIT-TOT TO W418UT-SUVAT-BILLIT-TOT                
017800     MOVE FEED-SUBTO-TOT        TO W418UT-SUBTO-TOT                       
017900     MOVE FEED-PRARTNTO         TO W418UT-PRARTNTO                        
017910     MOVE FEED-KVLEVART         TO W418UT-KVLEVART                        
018000     MOVE FEED-IDEXCUST(1)      TO W418UT-IDEXCUST-1                      
018100     MOVE FEED-IDEXCUST(2)      TO W418UT-IDEXCUST-2                      
018200     MOVE FEED-IDOPTION(1)      TO W418UT-IDOPTION-1                      
018300     MOVE FEED-IDOPTION(2)      TO W418UT-IDOPTION-2                      
018400     MOVE FEED-IDOPTION(3)      TO W418UT-IDOPTION-3                      
018700     MOVE FEED-IDARTNR-FINANCE  TO W418UT-IDARTNR-FINANCE                 
018800     MOVE FEED-BEART            TO W418UT-BEART                           
019000     MOVE FEED-SUBTO            TO W418UT-SUBTO                           
019100     MOVE FEED-SUNTO            TO W418UT-SUNTO                           
019200     MOVE FEED-SUVAT-BILLIT     TO W418UT-SUVAT-BILLIT                    
019300     MOVE FEED-KDVALISO-BET     TO W418UT-KDVALISO-BET                    
019400     MOVE FEED-PRKURS-BET       TO W418UT-PRKURS-BET                      
019500     MOVE FEED-PRKURS           TO W418UT-PRKURS                          
019600     MOVE FEED-PRKURS-FAKBET    TO W418UT-PRKURS-FAKBET                   
019601                                                                          
019610     PERFORM S81-PUT-W418                                                 
019700     .                                                                    
019800     EJECT                                                                
019900                                                                          
020000 Z-FINIT SECTION.                                                         
020100     CLOSE WF2017                                                         
020200                                                                          
020300     IF W418-SEND-IDCOM > ZERO                                            
020600       PERFORM S91-CLOSE-W418                                             
020700     END-IF                                                               
020800     .                                                                    
020900     EJECT                                                                
021000                                                                          
021100 S01-READ-WF2017  SECTION.                                                
021200     READ WF2017 INTO IN-AREA                                             
021300       AT END                                                             
021400         SET END-OF-WF2017 TO TRUE                                        
021500     END-READ                                                             
021600     .                                                                    
021700     EJECT                                                                
021800                                                                          
021900 S71-OPEN-W418 SECTION.                                                   
022000     MOVE WS-ADRESS-W418                  TO SEND-ADDISPABS               
022100     MOVE 'OPEN'                          TO SEND-KDFUNC                  
022200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
022300                         SEND-OPEN-AREA                                   
022400     IF SEND-KDRC > ZERO                                                  
022500       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
022600       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
022700       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
022800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
022900     END-IF                                                               
023000     .                                                                    
023100     EJECT                                                                
023200                                                                          
023300 S81-PUT-W418 SECTION.                                                    
023400     MOVE 'PUT'                           TO SEND-KDFUNC                  
023500     MOVE W418-SEND-IDCOM                 TO SEND-IDCOM                   
023600     MOVE LENGTH OF W418UT-AREA           TO SEND-KVDLEN                  
023700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
023800                         SEND-KVDLEN                                      
023900                         W418UT-AREA                                      
024000     IF SEND-KDRC > ZERO                                                  
024100       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
024200       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
024300       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
024400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
024500     END-IF                                                               
024600     .                                                                    
024700     EJECT                                                                
024800                                                                          
024900 S91-CLOSE-W418 SECTION.                                                  
025000                                                                          
025100     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
025200     MOVE W418-SEND-IDCOM                 TO SEND-IDCOM                   
025300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
025400     .                                                                    
