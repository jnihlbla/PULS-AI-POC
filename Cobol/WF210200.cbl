000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WF210200.                                                
000300 AUTHOR.         BERNT LUNDH.                                             
000400 DATE-WRITTEN.   2002-04-29.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        THE PGM                                                          
001000*        - READS FILE WITH INTRASTAT DATA RECORDS                         
001100*        - SENDS INTRASTAT DATA RECORDS FOR VCCS TO:                      
001200*          . PULS BY USING WZ01SEND (CARPARTS.PULS.RECEIVEI)              
001300*                                                                         
001400 ENVIRONMENT DIVISION.                                                    
001500                                                                          
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900*          --- INTRASTAT DATA RECORDS                                     
002000     SELECT WF2012                     ASSIGN TO WF2102D1.                
002100     EJECT                                                                
002200 DATA DIVISION.                                                           
002300                                                                          
002400 FILE SECTION.                                                            
002500 FD  WF2012                                                               
002600     RECORDING       F                                                    
002700     BLOCK CONTAINS  0.                                                   
002800                                                                          
002900*01  -COPY WF2012      -L.                                                
003000     EJECT                                                                
003100                                                                          
003200 WORKING-STORAGE SECTION.                                                 
003300 77  IDPGM                       PIC X(8)    VALUE 'WF210200'.            
003400 77  WS-IDLEGSEL-VCCS            PIC X(4)    VALUE 'VCCS'.                
003500 77  WS-IDPTYP                   PIC X(3)    VALUE 'INT'.                 
003600 77  WS-ADRESS                   PIC X(50)                                
003700                                  VALUE 'CARPARTS.PULS.RECEIVEI'.         
003800                                                                          
003900 77  WF2012-EOF-SW               PIC X       VALUE 'N'.                   
004000     88  END-OF-WF2012                       VALUE 'J'.                   
004100                                                                          
004200 01  ERRTEXT.                                                             
004300     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
004400     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
004500 01  KDRC-DISPLAY                PIC Z(5).                                
004600     EJECT                                                                
004700                                                                          
004800 01  GENERAL-SUBPROGRAMS.                                                 
004900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005000     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
005100     EJECT                                                                
005200                                                                          
005300*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
005400                                                                          
005500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005800     EJECT                                                                
005900                                                                          
006000*    --- AREOR FÖR KOMMUNIKATION                                          
006100 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
006200*01  -COPY WZ01SEND                                                       
006300     EJECT                                                                
006400                                                                          
006500 01  IN-AREA-START            PIC X(24)   VALUE 'IN-AREA-START'.          
006600 01  IN-AREA.                                                             
006700*    03  -COPY WF2012                                                     
006800     EJECT                                                                
006900                                                                          
007000 01  INTUT-AREA-START            PIC X(24)   VALUE                        
007100                                               'INTUT-AREA-START'.        
007200 01  INTUT-AREA.                                                          
007300*    03  -COPY WZ01REQU         -PRE INTUT-                               
007400*    03  -COPY WF2102I1         -PRE INTUT-                               
007500     EJECT                                                                
007600                                                                          
007700 LINKAGE SECTION.                                                         
007800*01  -COPY W0009   -PRE MSG-                                              
007900                                                                          
008000*01  -COPY W0009   -PRE RECEIVEI-                                         
008100     EJECT                                                                
008200                                                                          
008300 PROCEDURE DIVISION  USING MSG-PCB RECEIVEI-PCB.                          
008400 MAIN SECTION.                                                            
008500                                                                          
008600     ENTRY 'DLITCBL' USING MSG-PCB RECEIVEI-PCB.                          
008700                                                                          
008800     PERFORM A-INIT                                                       
008900     PERFORM B-EXECUTE                                                    
009000     PERFORM Z-FINIT                                                      
009100                                                                          
009200     MOVE ZERO TO RETURN-CODE                                             
009300     GOBACK                                                               
009400     .                                                                    
009500 A-INIT SECTION.                                                          
009600                                                                          
009700     OPEN INPUT WF2012                                                    
009800     .                                                                    
009900 B-EXECUTE SECTION.                                                       
010000                                                                          
010100     PERFORM S01-READ-WF2012                                              
010200                                                                          
010300     IF END-OF-WF2012                                                     
010400       CONTINUE                                                           
010500     ELSE                                                                 
010600       PERFORM S90-SEND-OPEN                                              
010700                                                                          
010800       PERFORM UNTIL END-OF-WF2012                                        
010900         IF INT-IDLEGSEL = WS-IDLEGSEL-VCCS                               
011000           PERFORM BA-MOVE-TO-OUTPUT                                      
011100           PERFORM S91-SEND-PUT                                           
011200         END-IF                                                           
011300         PERFORM S01-READ-WF2012                                          
011400       END-PERFORM                                                        
011500                                                                          
011600       PERFORM S99-SEND-CLOSE                                             
011700     END-IF                                                               
011800     .                                                                    
011900 BA-MOVE-TO-OUTPUT SECTION.                                               
012000     MOVE 1                   TO INTUT-REQU-IDMSGVER                      
012100     MOVE SPACE               TO INTUT-REQU-KDPGMACT                      
012200     MOVE 'WF210200'          TO INTUT-REQU-IDUSER                        
012300                                                                          
012400     MOVE INT-DAEXDAT         TO INTUT-DAEXDAT                            
012500     COMPUTE INT-TIEXTID = INT-TIEXTID / 10                               
012600     END-COMPUTE                                                          
012700     MOVE INT-TIEXTID         TO INTUT-TIEXTID                            
012800     MOVE WS-IDPTYP           TO INTUT-IDPTYP                             
012900     MOVE INT-IDLANDX3-SEND   TO INTUT-IDLANDX3-SEND                      
013000     MOVE INT-IDLANDX3-BET    TO INTUT-IDLANDX3-BET                       
013100     MOVE INT-IDLANDX3-REC    TO INTUT-IDLANDX3-REC                       
013200     MOVE INT-KDVALISO        TO INTUT-KDVALISO                           
013300     MOVE INT-PRKURS          TO INTUT-PRKURS                             
013400     MOVE INT-IDPARTNR        TO INTUT-IDPARTNR                           
013500     MOVE INT-KDFINDOC        TO INTUT-KDFINDOC                           
013600     MOVE INT-DAFINDOC        TO INTUT-DAFINDOC                           
013700     MOVE INT-IDFINDOC        TO INTUT-IDFINDOC                           
013800     MOVE INT-IDEXCUST-1      TO INTUT-IDEXCUST-1                         
013900     MOVE INT-IDEXCUST-2      TO INTUT-IDEXCUST-2                         
014000     MOVE INT-IDARTNR-FINANCE TO INTUT-IDARTNR-FINANCE                    
014100     MOVE INT-BEART           TO INTUT-BEART                              
014200     MOVE INT-IDSTATNR        TO INTUT-IDSTATNR                           
014300     COMPUTE INTUT-VKORDNTO ROUNDED = INT-VKARTNTO *                      
014400                                      INT-KVLEVART                        
014500     END-COMPUTE                                                          
014600     COMPUTE INTUT-VKORDNTO-3DEC  ROUNDED = INT-VKARTNTO *                
014601                                      INT-KVLEVART                        
014602     END-COMPUTE                                                          
014610     MOVE INT-KVLEVART        TO INTUT-KVLEVART                           
014700     MOVE INT-SUNTO           TO INTUT-SUNTO                              
014800     MOVE INT-KDVALISO-SEND   TO INTUT-KDVALISO-SEND                      
014900     MOVE INT-PRKURS-SEND     TO INTUT-PRKURS-SEND                        
015000     MOVE INT-KDARTURS        TO INTUT-KDARTURS                           
015100     MOVE INT-KDFRAKT         TO INTUT-KDFRAKT                            
015200     MOVE INT-BELEVVIL        TO INTUT-BELEVVIL                           
015300     MOVE INT-IDVAT-LEG       TO INTUT-IDVAT-LEG                          
015310     MOVE INT-IDVAT-BET       TO INTUT-IDVAT-BET                          
015320     MOVE INT-IDVAT-RESP      TO INTUT-IDVAT-RESP                         
015330     MOVE INT-IDVAT-AGENT     TO INTUT-IDVAT-AGENT                        
015400     .                                                                    
015500 Z-FINIT SECTION.                                                         
015600                                                                          
015700     CLOSE WF2012                                                         
015800     .                                                                    
015900 S01-READ-WF2012  SECTION.                                                
016000                                                                          
016100     READ WF2012 INTO IN-AREA                                             
016200       AT END                                                             
016300         SET END-OF-WF2012 TO TRUE                                        
016400     END-READ                                                             
016500     .                                                                    
016600 S90-SEND-OPEN SECTION.                                                   
016700                                                                          
016800     MOVE WS-ADRESS                       TO SEND-ADDISPABS               
016900     MOVE 'OPEN'                          TO SEND-KDFUNC                  
017000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
017100                         SEND-OPEN-AREA                                   
017200     IF SEND-KDRC > ZERO                                                  
017300       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
017400       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
017500       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
017600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
017700     END-IF                                                               
017800     .                                                                    
017900 S91-SEND-PUT SECTION.                                                    
018000                                                                          
018100     MOVE 'PUT'                           TO SEND-KDFUNC                  
018200     MOVE LENGTH OF INTUT-AREA            TO SEND-KVDLEN                  
018300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
018400                         SEND-KVDLEN                                      
018500                         INTUT-AREA                                       
018600     IF SEND-KDRC > ZERO                                                  
018700       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
018800       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
018900       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
019000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
019100     END-IF                                                               
019200     .                                                                    
019300 S99-SEND-CLOSE SECTION.                                                  
019400                                                                          
019500     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
019600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
019700     .                                                                    
