000100 ID DIVISION.                                                             
000200 PROGRAM-ID.       W0158500.                                              
000300 AUTHOR.           THOMAS NILSSON.                                        
000400 DATE-WRITTEN.     JUNI 1990.                                             
000410 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION.                                                            
000900*    GENERELLT PGM FÖR DATABASLISTNINGAR                                  
001000*    LÄSER DATABASER MED SB.                                              
001100                                                                          
001200 ENVIRONMENT DIVISION.                                                    
001300 INPUT-OUTPUT SECTION.                                                    
001400 FILE-CONTROL.                                                            
001500                                                                          
001600     SELECT INDATA           ASSIGN TO      SYSIN.                        
001700     SELECT UTDATA           ASSIGN TO      W01585D1.                     
001800                                                                          
001810 I-O-CONTROL.                                                             
001820     APPLY WRITE-ONLY ON UTDATA.                                          
001830                                                                          
001900 DATA DIVISION.                                                           
002000 FILE SECTION.                                                            
002100                                                                          
002200 FD  INDATA                                                               
002300     LABEL RECORD STANDARD                                                
002400     RECORDING F                                                          
002500     BLOCK CONTAINS 0.                                                    
002600 01  INDATAPOST                  PIC X(80).                               
002700                                                                          
002800 FD  UTDATA                                                               
002900     LABEL RECORD STANDARD                                                
003000     RECORDING V                                                          
003100     BLOCK CONTAINS 0                                                     
003200     RECORD VARYING FROM 1 TO 8188 DEPENDING ON SEGL.                     
003300 01  UTPOST                      PIC X(8188).                             
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003510                                                                          
003600*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(8)    VALUE 'W0158500'.            
003710 77  JA                          PIC X       VALUE 'J'.                   
003800 77  NEJ                         PIC X       VALUE 'N'.                   
003900 77  SEGL                        PIC 9(4)    COMP SYNC.                   
004000 77  PREFIX                      PIC 9(4)    VALUE 46 COMP SYNC.          
004100 77  SKRAEP                      PIC X(10)   VALUE HIGH-VALUE.            
004200 77  NYCKEL-KOLL                 PIC X       VALUE 'N'.                   
004300 77  NYCKEL-SKRIV                PIC X       VALUE 'J'.                   
004400 77  SEG-KOLL                    PIC X       VALUE 'N'.                   
004500 77  SEG-SKRIV                   PIC X       VALUE 'J'.                   
004600 77  IN-EOF                      PIC X       VALUE 'N'.                   
004700                                                                          
004710                                                                          
004720                                                                          
004800 01  DYNAMISKA-SUBPROGRAM.                                                
004900   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
005000   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
005100   03  HEXCONV                   PIC X(8)    VALUE 'HEXCONV '.            
005300                                                                          
005310     EJECT                                                                
005400 01  FILLER                      PIC X(8)    VALUE 'INAREA'.              
005500                                                                          
005600 01  INAREA1.                                                             
005700     03  SEGTABELL OCCURS 7 INDEXED BY IX.                                
005810       05  SEGM                  PIC X(4)    VALUE '**  '.                
005900     03  FILLER                  PIC X(52)   VALUE SPACE.                 
006000 01  INAREA2.                                                             
006100     03  NYCKELLAENGD            PIC 9(2)    VALUE 00.                    
006200     03  FILLER                  PIC X(78)   VALUE SPACE.                 
006300 01  INAREA3.                                                             
006400     03  NYCKELFOM               PIC X(35)   VALUE SPACE.                 
006500     03  FILLER                  PIC X(35)   VALUE SPACE.                 
006600 01  INAREA4.                                                             
006700     03  NYCKELTOM               PIC X(35)   VALUE SPACE.                 
006800     03  FILLER                  PIC X(35)   VALUE SPACE.                 
006900                                                                          
006910                                                                          
006920                                                                          
007000 01  FILLER                      PIC X(8)    VALUE 'UTAREA'.              
007100                                                                          
007200 01  UTAREA.                                                              
007300     03  SEGNAMN.                                                         
007400       05 FILLER                 PIC X(4).                                
007500       05 SEG-TYP                PIC X(4).                                
007700     03  SEGKEY                  PIC X(38).                               
007800     03  SEGDATA                 PIC X(8142).                             
007900                                                                          
008000                                                                          
008100                                                                          
008200 01  HEXPARAMETRAR.                                                       
008300     03  HEXCONV-TYPE            PIC X.                                   
008400     03  HEXCONV-LENGTH          PIC S9(4)  COMP SYNC.                    
008500     03  HEXCONV-FROM            PIC X(35).                               
008600     03  HEXCONV-TO              PIC X(35).                               
008700                                                                          
008800 01  HEXAREA.                                                             
008900     03  KEYL                    PIC 9(2).                                
009000     03  KEYFOM                  PIC X(35).                               
009100     03  KEYTOM                  PIC X(35).                               
009200                                                                          
009300     EJECT                                                                
009400 01  FILLER                      PIC X(8)   VALUE 'IMS-WS  '.             
009500                                                                          
009600 01  STATUS-WS                   PIC XX.                                  
009700     88  SEGMENT-FINNS                      VALUE '  '.                   
009800     88  SEGMENT-SLUT                       VALUE 'GB'.                   
009900                                                                          
010000 01  GODK-STATUSKODER.                                                    
010100   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
010200     SKIP3                                                                
010300 01  SSA1                        PIC X(32).                               
010400                                                                          
010500     EJECT                                                                
010600*01      -COPY W0003.                                                     
011000     EJECT                                                                
011100 01  FILLER                      PIC X(16)  VALUE                         
011200                                            'DLI-IO-AREA'.                
011300 01  DLI-IO-AREA                 PIC X(8142).                             
011400     EJECT                                                                
011500 LINKAGE SECTION.                                                         
011700*    -COPY W0008                                                          
011900       05  SEG-KFB               PIC X(100).                              
012000     EJECT                                                                
012100 PROCEDURE DIVISION  USING PCB.                                           
012110 MAIN SECTION.                                                            
012200     ENTRY 'DLITCBL' USING PCB.                                           
012300                                                                          
012400     PERFORM A-INIT                                                       
012500     MOVE HIGH-VALUE                 TO DLI-IO-AREA                       
012600     PERFORM IMS-GET                                                      
012700     PERFORM UNTIL SEGMENT-SLUT                                           
012800       MOVE ZERO                     TO SEGL                              
012900       MOVE SEG-NAME-FB              TO SEGNAMN                           
012910       MOVE LOW-VALUE                TO SEGKEY                            
013000       IF LENGTH-FB-KEY > 38                                              
013001         MOVE 38 TO LENGTH-FB-KEY                                         
013002       END-IF                                                             
013010       MOVE SEG-KFB(1:LENGTH-FB-KEY) TO SEGKEY(1:LENGTH-FB-KEY)           
013100       MOVE DLI-IO-AREA              TO SEGDATA                           
013200       INSPECT DLI-IO-AREA TALLYING SEGL FOR CHARACTERS                   
013300               BEFORE INITIAL SKRAEP                                      
013410       ADD PREFIX                    TO SEGL                              
013500       IF SEG-TYP                    =  '01  '                            
013600           AND NYCKEL-KOLL           =  JA                                
013700         PERFORM B-KOLLA-NYCKEL                                           
013800       END-IF                                                             
013900       IF SEG-KOLL                   =  JA                                
014000         PERFORM C-KOLLA-SEG                                              
014100       END-IF                                                             
014200       IF NYCKEL-SKRIV               =  JA                                
014300           AND SEG-SKRIV             =  JA                                
014400          WRITE UTPOST FROM UTAREA                                        
014500       END-IF                                                             
014600       MOVE HIGH-VALUE               TO DLI-IO-AREA                       
014700       PERFORM IMS-GET                                                    
014800     END-PERFORM                                                          
014900     CLOSE INDATA UTDATA                                                  
015000                                                                          
015100     MOVE ZERO TO RETURN-CODE                                             
015200     GOBACK                                                               
015300     .                                                                    
015400     EJECT                                                                
015500 A-INIT       SECTION.                                                    
015600                                                                          
015700     OPEN INPUT  INDATA                                                   
015800          OUTPUT UTDATA                                                   
015900     READ INDATA INTO                 INAREA1                             
016000       AT END MOVE '1'               TO IN-EOF                            
016100     END-READ                                                             
016200     READ INDATA INTO                 INAREA2                             
016300       AT END MOVE '2'               TO IN-EOF                            
016400     END-READ                                                             
016500     READ INDATA INTO                 INAREA3                             
016600       AT END MOVE '3'               TO IN-EOF                            
016700     END-READ                                                             
016800     READ INDATA INTO                 INAREA4                             
016900       AT END MOVE '4'               TO IN-EOF                            
017000     END-READ                                                             
017100     IF NYCKELLAENGD > ZERO                                               
017200       MOVE JA                    TO NYCKEL-KOLL                          
017300       MOVE NYCKELLAENGD          TO HEXCONV-LENGTH                       
017400       MOVE NYCKELFOM             TO HEXCONV-FROM                         
017500       MOVE '1'                   TO HEXCONV-TYPE                         
017600       CALL HEXCONV USING HEXCONV-TYPE HEXCONV-LENGTH                     
017700                          HEXCONV-FROM HEXCONV-TO                         
017800       MOVE HEXCONV-TO            TO KEYFOM                               
017900       MOVE HEXCONV-LENGTH        TO KEYL                                 
018000       MOVE NYCKELLAENGD          TO HEXCONV-LENGTH                       
018100       MOVE NYCKELTOM             TO HEXCONV-FROM                         
018200       MOVE '1'                   TO HEXCONV-TYPE                         
018300       CALL HEXCONV USING HEXCONV-TYPE HEXCONV-LENGTH                     
018400                          HEXCONV-FROM HEXCONV-TO                         
018500       MOVE HEXCONV-TO            TO KEYTOM                               
018600     ELSE                                                                 
018700       MOVE NEJ                   TO NYCKEL-KOLL                          
018900     END-IF                                                               
018901                                                                          
018910     INSPECT INAREA1 REPLACING ALL '****' BY '  **'                       
019000     IF SEGM (1) = '**  ' OR '  **'                                       
019100       MOVE NEJ                   TO SEG-KOLL                             
019300     ELSE                                                                 
019400       MOVE JA                    TO SEG-KOLL                             
019500     END-IF                                                               
019600     .                                                                    
019700     EJECT                                                                
019800 B-KOLLA-NYCKEL SECTION.                                                  
019900                                                                          
020000     IF SEGKEY(1:KEYL)          >= KEYFOM(1:KEYL) AND                     
020100        SEGKEY(1:KEYL)          <= KEYTOM(1:KEYL)                         
020200        MOVE JA                 TO NYCKEL-SKRIV                           
020300     ELSE                                                                 
020400        MOVE NEJ                TO NYCKEL-SKRIV                           
020500     END-IF                                                               
020600     .                                                                    
020700     EJECT                                                                
020800 C-KOLLA-SEG SECTION.                                                     
020900                                                                          
021000     IF SEG-TYP = SEGM (1)                                                
021010             OR   SEGM (2)                                                
021020             OR   SEGM (3)                                                
021030             OR   SEGM (4)                                                
021040             OR   SEGM (5)                                                
021050             OR   SEGM (6)                                                
021060             OR   SEGM (7)                                                
021100       MOVE JA  TO SEG-SKRIV                                              
021200     ELSE                                                                 
021210       MOVE NEJ TO SEG-SKRIV                                              
021500     END-IF                                                               
021600     .                                                                    
021700     EJECT                                                                
021800*    ---- IMS SEKTIONER                                                   
021900 IMS-GET      SECTION.                                                    
022000                                                                          
022100     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
022200     CALL CBLTDLI USING GN PCB DLI-IO-AREA                                
022300     MOVE STATUS-CODE TO STATUS-WS                                        
022400     PERFORM IMS-STATUSKONTROLL                                           
022500     .                                                                    
022600                                                                          
022610                                                                          
022620                                                                          
022700 IMS-STATUSKONTROLL SECTION.                                              
022800                                                                          
022900     SET STATUS-IX TO 1                                                   
023000     SEARCH GODK-STATUS                                                   
023100       AT END                                                             
023110         CALL FELLOG                                                      
023200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
023210         CONTINUE                                                         
023300     END-SEARCH                                                           
023400     .                                                                    
