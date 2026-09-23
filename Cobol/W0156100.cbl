000100 ID DIVISION.                                                             
000200 PROGRAM-ID.    W0156100.                                                 
000300 AUTHOR.        RICHARD THÖRNGREN.                                        
000400 DATE-WRITTEN.  JULI 1990.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*    GENERELLT PGM FÖR DATABASLISTNINGAR                                  
000900*    LÄSER DATABASER MED SB.                                              
001000                                                                          
001100 ENVIRONMENT DIVISION.                                                    
001200 INPUT-OUTPUT SECTION.                                                    
001300 FILE-CONTROL.                                                            
001400                                                                          
001500     SELECT STYRIN           ASSIGN TO      SYSIN.                        
001600     SELECT INDATA           ASSIGN TO      W01561D1.                     
001700     SELECT UTDATA           ASSIGN TO      W01561D2.                     
001800     EJECT                                                                
001900 DATA DIVISION.                                                           
002000 FILE SECTION.                                                            
002100                                                                          
002200 FD  STYRIN                                                               
002300     LABEL RECORD STANDARD                                                
002400     RECORDING F                                                          
002500     BLOCK CONTAINS 0.                                                    
002600 01  STYRPOST                    PIC X(80).                               
002700     SKIP3                                                                
002800 FD  INDATA                                                               
002900     LABEL RECORD STANDARD                                                
003000     RECORDING F                                                          
003100     BLOCK CONTAINS 0.                                                    
003200 01  INPOST                      PIC X(80).                               
003300     SKIP3                                                                
003400 FD  UTDATA                                                               
003500     LABEL RECORD STANDARD                                                
003600     RECORDING V                                                          
003700     BLOCK CONTAINS 0                                                     
003800     RECORD VARYING FROM 1 TO 8188 DEPENDING ON W-SEGL.                   
003900 01  UTPOST                      PIC X(8188).                             
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300                                                                          
004400*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(8)    VALUE 'W0156100'.            
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800 77  W-PREFIX                    PIC S9(4)   VALUE +46  COMP SYNC.        
004900 77  W-KEYL                      PIC S9(4)   VALUE +0   COMP SYNC.        
005000 77  W-SEGL                      PIC 9(4)    VALUE 0    COMP SYNC.        
005100 77  W-STYRANTAL                 PIC S9(3)   VALUE +0   COMP-3.           
005200 77  W-INANTAL                   PIC S9(5)   VALUE +0   COMP-3.           
005300 77  W-UTANTAL                   PIC S9(7)   VALUE +0   COMP-3.           
005400 77  W-SLUT                      PIC X(20)   VALUE HIGH-VALUE.            
005500 77  IN-EOF                      PIC X       VALUE 'N'.                   
005600 77  STYR-EOF                    PIC X       VALUE 'N'.                   
005700     SKIP3                                                                
005800 01  DYNAMISKA-SUBPROGRAM.                                                
005900   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
006000   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
006100     EJECT                                                                
006200 01  FILLER                      PIC X(8)    VALUE 'STYRAREA'.            
006300                                                                          
006400 01  STYRAREA.                                                            
006500   03  STYR-SSA                  PIC X(19).                               
006600   03  FILLER REDEFINES STYR-SSA.                                         
006700     05  STYR-SEGNAME            PIC X(8).                                
006800     05  FILLER                  PIC X(11).                               
006900   03  FILLER                    PIC X(61).                               
007000     SKIP3                                                                
007100 01  FILLER                      PIC X(8)    VALUE 'INAREA'.              
007200                                                                          
007300 01  INAREA.                                                              
007400   03  IN-NYCKEL                 PIC X(80).                               
007500     SKIP3                                                                
007600 01  FILLER                      PIC X(8)    VALUE 'UTAREA'.              
007700                                                                          
007800 01  UTAREA.                                                              
007900   03  UT-SEG-NAME               PIC X(8).                                
008000   03  UT-SEGKEY                 PIC X(38).                               
008100   03  UT-SEGMENT                PIC X(8142).                             
008200     EJECT                                                                
008300 01  FILLER                      PIC X(8)   VALUE 'IMS-WS  '.             
008400                                                                          
008500 01  STATUS-WS                   PIC XX.                                  
008600     88  SEGMENT-FINNS                      VALUE '  ' 'GA' 'GK'.         
008700     88  SEGMENT-SAKNAS                     VALUE 'GE'.                   
008800     SKIP3                                                                
008900 01  GODK-STATUSKODER.                                                    
009000   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
009100     SKIP3                                                                
009200 01  SSA1                        PIC X(64).                               
009300     SKIP3                                                                
009400 01  FILLER                      PIC X(16)  VALUE                         
009500                                            'DLI-IO-AREA'.                
009600 01  DLI-IO-AREA                 PIC X(8142).                             
009700     EJECT                                                                
009800*01      -COPY W0003.                                                     
009900     EJECT                                                                
010000 LINKAGE SECTION.                                                         
010100*    -COPY W0008      -PRE DB-                                            
010200    05  FILLER                   PIC X(256).                              
010300     EJECT                                                                
010400 PROCEDURE DIVISION  USING DB-PCB.                                        
010500 MAIN SECTION.                                                            
010600     ENTRY 'DLITCBL' USING DB-PCB.                                        
010700                                                                          
010800     PERFORM A-INIT                                                       
010900     PERFORM B-LAES-STYR                                                  
011000     PERFORM IMS-GET-FIRST                                                
011100     MOVE DB-LENGTH-FB-KEY TO W-KEYL                                      
011200     PERFORM C-LAES-INPOST                                                
011300     PERFORM UNTIL IN-EOF = JA                                            
011400       MOVE HIGH-VALUE TO DLI-IO-AREA                                     
011500       PERFORM IMS-GET-ROT                                                
011600       IF SEGMENT-FINNS                                                   
011700         PERFORM D-SKRIV-UT-POST                                          
011800         MOVE HIGH-VALUE TO DLI-IO-AREA                                   
011900         PERFORM IMS-GET-BARN                                             
012000         PERFORM UNTIL SEGMENT-SAKNAS                                     
012100           PERFORM D-SKRIV-UT-POST                                        
012200           MOVE HIGH-VALUE TO DLI-IO-AREA                                 
012300           PERFORM IMS-GET-BARN                                           
012400         END-PERFORM                                                      
012500       ELSE                                                               
012600         DISPLAY INAREA ' SAKNAS POST NR ' W-INANTAL                      
012700       END-IF                                                             
012800       PERFORM C-LAES-INPOST                                              
012900     END-PERFORM                                                          
013000     PERFORM Z-FINIT                                                      
013100                                                                          
013200     MOVE ZERO TO RETURN-CODE                                             
013300     GOBACK                                                               
013400     .                                                                    
013500     EJECT                                                                
013600 A-INIT SECTION.                                                          
013700                                                                          
013800     OPEN INPUT STYRIN INDATA                                             
013900          OUTPUT UTDATA                                                   
014000     .                                                                    
014100     EJECT                                                                
014200 B-LAES-STYR SECTION.                                                     
014300                                                                          
014400     READ STYRIN INTO STYRAREA                                            
014500       AT END                                                             
014600         MOVE JA TO STYR-EOF                                              
014700       NOT AT END                                                         
014800         ADD +1 TO W-STYRANTAL                                            
014900     END-READ                                                             
015000     .                                                                    
015100     EJECT                                                                
015200 C-LAES-INPOST SECTION.                                                   
015300                                                                          
015400     READ INDATA INTO INAREA                                              
015500       AT END                                                             
015600         MOVE JA TO IN-EOF                                                
015700       NOT AT END                                                         
015800         ADD +1 TO W-INANTAL                                              
015900     END-READ                                                             
016000     .                                                                    
016100     EJECT                                                                
016200 D-SKRIV-UT-POST SECTION.                                                 
016300                                                                          
016400     MOVE ZERO TO W-SEGL                                                  
016500     MOVE DB-SEG-NAME-FB TO UT-SEG-NAME                                   
016600     MOVE LOW-VALUE TO UT-SEGKEY                                          
016700     MOVE DB-KEY-FB-AREA(1:DB-LENGTH-FB-KEY) TO UT-SEGKEY                 
016800     MOVE DLI-IO-AREA TO UT-SEGMENT                                       
016900     INSPECT DLI-IO-AREA TALLYING W-SEGL FOR CHARACTERS                   
017000       BEFORE INITIAL W-SLUT                                              
017100     ADD W-PREFIX TO W-SEGL                                               
017200     WRITE UTPOST FROM UTAREA                                             
017300     ADD +1 TO W-UTANTAL                                                  
017400     .                                                                    
017500     EJECT                                                                
017600 Z-FINIT SECTION.                                                         
017700                                                                          
017800     CLOSE STYRIN                                                         
017900           INDATA                                                         
018000           UTDATA                                                         
018100     DISPLAY ' STYR = ' W-STYRANTAL                                       
018200     DISPLAY ' IN = ' W-INANTAL                                           
018300     DISPLAY ' UT = ' W-UTANTAL                                           
018400     .                                                                    
018500     EJECT                                                                
018600*    ---- IMS SEKTIONER                                                   
018700 IMS-GET-FIRST SECTION.                                                   
018800                                                                          
018900     MOVE STYR-SEGNAME TO SSA1                                            
019000     MOVE '  ' TO GODK-STATUSKODER                                        
019100     CALL CBLTDLI USING GN DB-PCB DLI-IO-AREA SSA1                        
019200     MOVE DB-STATUS-CODE TO STATUS-WS                                     
019300     PERFORM IMS-STATUSKONTROLL                                           
019400     .                                                                    
019500                                                                          
019600 IMS-GET-ROT SECTION.                                                     
019700                                                                          
019800     STRING STYR-SSA IN-NYCKEL(1:W-KEYL) ') '                             
019900            DELIMITED BY SIZE INTO SSA1                                   
020000     MOVE '  GE' TO GODK-STATUSKODER                                      
020100     CALL CBLTDLI USING GU DB-PCB DLI-IO-AREA SSA1                        
020200     MOVE DB-STATUS-CODE TO STATUS-WS                                     
020300     PERFORM IMS-STATUSKONTROLL                                           
020400     .                                                                    
020500                                                                          
020600 IMS-GET-BARN SECTION.                                                    
020700                                                                          
020800     MOVE '  GAGKGE' TO GODK-STATUSKODER                                  
020900     CALL CBLTDLI USING GNP DB-PCB DLI-IO-AREA                            
021000     MOVE DB-STATUS-CODE TO STATUS-WS                                     
021100     PERFORM IMS-STATUSKONTROLL                                           
021200     .                                                                    
021300                                                                          
021400 IMS-STATUSKONTROLL SECTION.                                              
021500                                                                          
021600     SET STATUS-IX TO 1                                                   
021700     SEARCH GODK-STATUS                                                   
021800       AT END                                                             
021900         CALL FELLOG                                                      
022000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
022100         CONTINUE                                                         
022200     END-SEARCH                                                           
022300     .                                                                    
