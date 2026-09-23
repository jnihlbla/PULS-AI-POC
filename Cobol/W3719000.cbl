000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3719000.                                                
000300 AUTHOR.         HÅKAN BOHLIN.                                            
000400 DATE-WRITTEN.   14/10/20.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        REMANUFACTOR ORDER IN NES.                                       
001000*        CREATE AN OUTPUT FILE WITH CORE PARTS WHICH SHOULD BE            
001100*        ORDERED FOR A REMANUFACTOR.                                      
001200*        FILE IS CREATED IN CSV FORMAT AND SHOULD BE SENT BY MAIL         
001300*        TO SITTARD.                                                      
001400*        SYMBOLIC PARAMETERS IDUSER/IDDISTR.                              
001500*                                                                         
001600*        PROGRAM    READ     WDR2 (WDGX3152/WDGX3154)                     
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- INPUT PARAMETERS FROM NES ORDER SCREEN (WEB)               
002700     SELECT INDATA                     ASSIGN TO W37190D1.                
002800     SKIP2                                                                
002900*          --- OUTPUT FILE                                                
003000     SELECT W37191                     ASSIGN TO W37190D2.                
003100                                                                          
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  INDATA                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900     SKIP2                                                                
004000 01  IN-RECORD            PIC X(80).                                      
004100     SKIP3                                                                
004200 FD  W37191                                                               
004300     RECORDING       V                                                    
004400     BLOCK CONTAINS  0.                                                   
004500     SKIP2                                                                
004600 01  OUT-RECORD           PIC X(30).                                      
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900                                                                          
005000                                                                          
005100 77  IDPGM                       PIC X(8)    VALUE 'W3719000'.            
005200 77  YES                         PIC X       VALUE 'J'.                   
005300 77  NOO                         PIC X       VALUE 'N'.                   
005400 77  FIRST-SW                    PIC X       VALUE 'J'.                   
005500     88  FIRST-OK                            VALUE 'J'.                   
005600     SKIP2                                                                
005700 01  ERRTEXT.                                                             
005800     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
005900     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
006000     EJECT                                                                
006100                                                                          
006200 01  DYNAMISKA-SUBPROGRAM.                                                
006300*                                                                         
006400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006700     EJECT                                                                
006800*    --- PARAMETRAR TILL POSTSUM                                          
006900*                                                                         
007000*01  -COPY W0005   -PRE  POSTSUM-                                         
007100     EJECT                                                                
007200                                                                          
007300 01  IN-AREA-START               PIC X(16)   VALUE                        
007400                                 'IN-AREA-START  '.                       
007500     SKIP2                                                                
007600 01  IN-AREA-1.                                                           
007700     03  IN-IDUSER-OREG          PIC X(8).                                
007800     03  FILLER                  PIC X(72).                               
007900 01  IN-AREA-2.                                                           
008000     03  IN-IDDISTR              PIC 9(4).                                
008100     03  FILLER                  PIC X(76).                               
008200     EJECT                                                                
008300                                                                          
008400 01  OUT-AREA-START              PIC X(24)   VALUE                        
008500                                 'OUT-AREA-START'.                        
008600 01  OUT-AREA.                                                            
008700     03  OUT-IDDISTR             PIC Z(3)9.                               
008800     03  FILLER                  PIC X(1)  VALUE ';'.                     
008900     03  OUT-IDARTNR             PIC Z(7)9.                               
009000     03  FILLER                  PIC X(1)  VALUE ';'.                     
009100     03  OUT-KVBEART             PIC Z(5)9.                               
009300     SKIP2                                                                
009400                                                                          
009500 01  WS-HEADER.                                                           
009600     03  WS-HEADER-IDDISTR       PIC X(12) VALUE 'REMAN DISTR.'.          
009700     03  FILLER                  PIC X(1)  VALUE ';'.                     
009800     03  WS-HEADER-IDARTNR       PIC X(7)  VALUE 'CORE NO'.               
009900     03  FILLER                  PIC X(1)  VALUE ';'.                     
010000     03  WS-HEADER-KVBEART       PIC X(9)  VALUE 'ORDER QTY'.             
010200                                                                          
010300     EJECT                                                                
010400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010500     SKIP3                                                                
010600 01  KEYS-TO-DLI.                                                         
010700     03   W-WDGXKEY-3151-X.                                               
010800         05  W-IDHTYP            PIC X(4)   VALUE '3151'.                 
010900         05  FILLER              PIC X(26)  VALUE LOW-VALUE.              
011000                                                                          
011100     03  W-WDGXKEY-3152-X.                                                
011200         05 W-IDUSER-OREG        PIC X(8)  VALUE SPACE.                   
011300         05 W-IDDISTR            PIC S9(5) VALUE ZERO COMP-3.             
011400     EJECT                                                                
011500*    --- STATUS-CODE FROM IMS                                             
011600 01  STATUS-WS                   PIC XX.                                  
011700     88  SEGMENT-EXIST                       VALUE '  '.                  
011800     88  SEGMENT-MISSING                     VALUE 'GE'.                  
011900     88  END-OF-DATABASE                     VALUE 'GB'.                  
012000     SKIP2                                                                
012100 01  GOOD-STATUSCODES.                                                    
012200     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012300     SKIP3                                                                
012400 01  SSA1                        PIC X(64).                               
012500 01  SSA2                        PIC X(64).                               
012600     EJECT                                                                
012700*    --- IMS RETURNCODES                                                  
012800*01  -COPY W0003                                                          
012900     EJECT                                                                
013000*    ---  DLI INPUT-OUTPUT AREA                                           
013100 01  FILLER                PIC X(16)   VALUE 'DLI-IO-3152'.               
013200 01  DLI-IO-3152.                                                         
013300*    03  -COPY WDGX3152                                                   
013400     EJECT                                                                
013500 01  FILLER                PIC X(16)   VALUE 'DLI-IO-3154'.               
013600 01  DLI-IO-3154.                                                         
013700*    03  -COPY WDGX3154                                                   
013800     EJECT                                                                
013900 LINKAGE SECTION.                                                         
014000                                                                          
014100*01  -COPY W0008  -PRE 3151-                                              
014200     05  FILLER                  PIC X.                                   
014300     EJECT                                                                
014400 PROCEDURE DIVISION  USING 3151-PCB.                                      
014500 MAIN SECTION.                                                            
014600     ENTRY 'DLITCBL' USING 3151-PCB.                                      
014700                                                                          
014800     SKIP2                                                                
014900     PERFORM A-INIT                                                       
015000                                                                          
015100     MOVE IN-IDUSER-OREG    TO W-IDUSER-OREG                              
015200     MOVE IN-IDDISTR        TO W-IDDISTR                                  
015300     PERFORM IMS-GU-WDGX3152                                              
015310     IF SEGMENT-EXIST                                                     
015400        PERFORM IMS-GNP-WDGX3154                                          
015500        PERFORM UNTIL SEGMENT-MISSING OR END-OF-DATABASE                  
015600           PERFORM B-BUILD-MAIL-RECORDS                                   
015700           PERFORM IMS-GNP-WDGX3154                                       
015800        END-PERFORM                                                       
015810     END-IF                                                               
015900                                                                          
016000     PERFORM Z-FINIT                                                      
016100     MOVE ZERO TO RETURN-CODE                                             
016200     GOBACK                                                               
016300     .                                                                    
016400     EJECT                                                                
016500 A-INIT SECTION.                                                          
016600                                                                          
016700     OPEN INPUT INDATA                                                    
016800     READ INDATA INTO IN-AREA-1                                           
016900     READ INDATA INTO IN-AREA-2                                           
017000     END-READ                                                             
017100     CLOSE INDATA                                                         
017200                                                                          
017300     OPEN OUTPUT W37191                                                   
017400                                                                          
017500     MOVE IDPGM             TO POSTSUM-PROGNAMN                           
017600     .                                                                    
017700     EJECT                                                                
017800 B-BUILD-MAIL-RECORDS SECTION.                                            
017900                                                                          
018000     IF FIRST-OK                                                          
018100        PERFORM S11-WRITE-W37191                                          
018200        MOVE NOO TO FIRST-SW                                              
018300     END-IF                                                               
018400     MOVE 3152-IDDISTR          TO OUT-IDDISTR                            
018500     MOVE 3154-IDARTNR-OBJ      TO OUT-IDARTNR                            
018600     MOVE 3154-KVBEART          TO OUT-KVBEART                            
018700     PERFORM S11-WRITE-W37191                                             
018800     .                                                                    
018900     EJECT                                                                
019000 Z-FINIT SECTION.                                                         
019100                                                                          
019200     CLOSE W37191                                                         
019300                                                                          
019400     SKIP2                                                                
019500     MOVE 'S' TO POSTSUM-OPKOD                                            
019600     CALL POSTSUM USING POSTSUM-PARM                                      
019700     .                                                                    
019800     EJECT                                                                
019900 S11-WRITE-W37191 SECTION.                                                
020000     SKIP2                                                                
020100     IF FIRST-OK                                                          
020200        WRITE OUT-RECORD FROM WS-HEADER                                   
020300     ELSE                                                                 
020400        WRITE OUT-RECORD FROM OUT-AREA                                    
020500     END-IF                                                               
020600                                                                          
020700     MOVE 'W37191 ' TO POSTSUM-FDNAMN                                     
020800     MOVE 'W37190D2' TO POSTSUM-DDNAMN2                                   
020900     CALL POSTSUM USING POSTSUM-PARM                                      
021000     .                                                                    
021100     EJECT                                                                
021200* --- IMS SECTIONS ---                                                    
021300                                                                          
021400 IMS-GU-WDGX3152 SECTION.                                                 
021500                                                                          
021600     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-3151-X ')'                    
021700          DELIMITED BY SIZE INTO SSA1                                     
021800     STRING 'WDGX3152(KY3152   =' W-WDGXKEY-3152-X ')'                    
021900          DELIMITED BY SIZE INTO SSA2                                     
022000     MOVE '  GE' TO GOOD-STATUSCODES                                      
022100     CALL CBLTDLI USING GU 3151-PCB DLI-IO-3152 SSA1 SSA2                 
022200     MOVE 3151-STATUS-CODE TO STATUS-WS                                   
022300     PERFORM IMS-STATUSCHECK                                              
022400     .                                                                    
022500     SKIP3                                                                
022600 IMS-GNP-WDGX3154 SECTION.                                                
022700                                                                          
022800     MOVE 'WDGX3154' TO SSA1                                              
022900     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
023000     CALL CBLTDLI USING GNP 3151-PCB DLI-IO-3154 SSA1                     
023100     MOVE 3151-STATUS-CODE TO STATUS-WS                                   
023200     PERFORM IMS-STATUSCHECK                                              
023300     .                                                                    
023400     SKIP2                                                                
023500 IMS-STATUSCHECK SECTION.                                                 
023600     SKIP2                                                                
023700     SET STATUS-IX TO 1                                                   
023800     SEARCH GOOD-STATUS                                                   
023900       AT END                                                             
024000         STRING ' WRONG STATUS CODE FROM IMS: ' STATUS-WS                 
024100           DELIMITED BY SIZE INTO ERRTEXT-STR                             
024200         DISPLAY ERRTEXT                                                  
024300         CALL FELLOG                                                      
024400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
024500         CONTINUE                                                         
024600     END-SEARCH                                                           
024700     .                                                                    
