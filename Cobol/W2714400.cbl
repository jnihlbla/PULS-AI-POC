000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2714400.                                                
000300 AUTHOR.         REDDY RAHUL.                                             
000400 DATE-WRITTEN.   13/11/13.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        PROGRAM TO FETCH RECORDS TO BE UPDATED WHEN REFILL DC IS         
001000*        UPDATED ON 4403.                                                 
001100*        WRITE THE RECORDS TO FILE FOR FURTHER PROCESSING                 
001200*        IN W2714600.                                                     
001300*                                                                         
001400*        THE PROGRAM READS     WDB6 WDK7B                                 
001500*                                                                         
001600*    CHANGE LOG:                                                          
001700*      13/11/13 - REDDY RAHUL     - INITIAL VERSION                       
001800*                                   SCR 10205394                          
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- OUTPUT FILE WITH RECS TO BE UPDATED                        
002800     SELECT W27144                     ASSIGN TO W27144D1.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W27144                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  POST -COPY W27144 -PRE  UT-  -L.                                     
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200 01  PARM-SYSIN                  PIC X(80).                               
004300 01  PARM-IDDC                   PIC X(2).                                
004400 01  PARM-IDDC-REF-OLD           PIC X(2).                                
004500 01  PARM-IDDC-REF-NEW           PIC X(2).                                
004600                                                                          
004700 77  IDPGM                       PIC X(8)    VALUE 'W2714400'.            
004800 77  YES                         PIC X       VALUE 'J'.                   
004900 77  NOO                         PIC X       VALUE 'N'.                   
005000     SKIP2                                                                
005100 01  ERROR-TEXT.                                                          
005200     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
005300     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
005400     EJECT                                                                
005500 77  WS-IDLEVNR-NEW              PIC X(5).                                
005600 01  GENERAL-SUBPROGRAMS.                                                 
005700*                                                                         
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006100     EJECT                                                                
006200*    --- PARAMETRAR TILL POSTSUM                                          
006300*                                                                         
006400*01  -COPY W0005   -PRE  POSTSUM-                                         
006500     EJECT                                                                
006600 01  UT-AREA-START               PIC X(24)   VALUE                        
006700                                             'UT-AREA-START'.             
006800     SKIP2                                                                
006900*01  AREA -COPY W27144      -PRE UT-                                      
007000*                                                                         
007100     EJECT                                                                
007200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007300     SKIP3                                                                
007400 01  KEYS-TILL-DLI.                                                       
007500     03  W-WDK7B1KY-MIN-X.                                                
007600         05  W-IDDC-REF-B1-MIN   PIC X(2)    VALUE SPACE.                 
007700         05  W-IDDC-B1-MIN       PIC X(2)    VALUE SPACE.                 
007800         05  W-IDARTNR-B1-MIN    PIC S9(9)   COMP-3 VALUE ZERO.           
007900     03  W-WDK7B1KY-MAX-X.                                                
008000         05  W-IDDC-REF-B1-MAX   PIC X(2)    VALUE SPACE.                 
008100         05  W-IDDC-B1-MAX       PIC X(2)    VALUE SPACE.                 
008200         05  W-IDARTNR-B1-MAX    PIC S9(9)                                
008300                                         VALUE +999999999 COMP-3.         
008400     03  W-IDDC-X.                                                        
008500         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
008600     SKIP2                                                                
008700*    --- STATUS-KOD FRÅN IMS                                              
008800 01  STATUS-WS                   PIC XX.                                  
008900     88  SEGMENT-FOUND                       VALUE '  '.                  
009000     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
009100     88  SEGMENT-MISSING                     VALUE 'GE'.                  
009200     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
009300     88  IMS-NOT-OK                          VALUE 'XD'.                  
009400     SKIP2                                                                
009500 01  GOOD-STATUSCODES.                                                    
009600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009700     SKIP3                                                                
009800 01  SSA1                        PIC X(128).                              
009900 01  SSA2                        PIC X(64).                               
010000     EJECT                                                                
010100*    --- IMS FUNCTION CODES                                               
010200*01  -COPY W0003                                                          
010300     EJECT                                                                
010400*    ---  DLI INPUT-OUTPUT AREA                                           
010500                                                                          
010600 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK7B1'.         
010700 01  DLI-IO-WDK7B1.                                                       
010800*    03  -COPY WDK7B1                                                     
010900 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB601'.         
011000 01  DLI-IO-WDB601.                                                       
011100*    03  -COPY WDB601                                                     
011200                                                                          
011300     EJECT                                                                
011400 LINKAGE SECTION.                                                         
011500                                                                          
011600*01  -COPY W0008  -PRE WDK7B-                                             
011700     05  FILLER                  PIC X.                                   
011800                                                                          
011900*01  -COPY W0008  -PRE WDB6-                                              
012000     05  FILLER                  PIC X.                                   
012100                                                                          
012200     EJECT                                                                
012300 PROCEDURE DIVISION  USING WDK7B-PCB WDB6-PCB.                            
012400                                                                          
012500 MAIN SECTION.                                                            
012600     ENTRY 'DLITCBL' USING WDK7B-PCB WDB6-PCB.                            
012700     SKIP2                                                                
012800                                                                          
012900     PERFORM A-INIT                                                       
013000     PERFORM B-PROCESS                                                    
013100     PERFORM Z-FINIT                                                      
013200     MOVE ZERO                   TO RETURN-CODE                           
013300     GOBACK                                                               
013400     .                                                                    
013500     EJECT                                                                
013600 A-INIT SECTION.                                                          
013700     SKIP2                                                                
013800                                                                          
013900     MOVE IDPGM                  TO POSTSUM-PROGNAMN                      
014000     OPEN OUTPUT W27144                                                   
014100                                                                          
014200     ACCEPT PARM-SYSIN         FROM SYSIN                                 
014300     UNSTRING PARM-SYSIN  DELIMITED BY ','                                
014400                               INTO PARM-IDDC                             
014500                                    PARM-IDDC-REF-OLD                     
014600                                    PARM-IDDC-REF-NEW                     
014700                                                                          
014800     DISPLAY 'PARM IDDC/OLD REF DC/NEW REF DC :'                          
014900                              PARM-IDDC '/'                               
015000                              PARM-IDDC-REF-OLD '/'                       
015100                              PARM-IDDC-REF-NEW '.'                       
015200     .                                                                    
015300     EJECT                                                                
015400 B-PROCESS SECTION.                                                       
015500                                                                          
015600     MOVE PARM-IDDC-REF-NEW      TO W-IDDC                                
015700                                                                          
015800     PERFORM IMS-GET-WDB601                                               
015900     IF SEGMENT-FOUND                                                     
016000       MOVE DCS-IDLEVNR-DC       TO UT-IDLEVNR                            
016100     END-IF                                                               
016200                                                                          
016300     MOVE PARM-IDDC-REF-OLD      TO W-IDDC-REF-B1-MIN                     
016400                                    W-IDDC-REF-B1-MAX                     
016500     MOVE PARM-IDDC              TO W-IDDC-B1-MIN                         
016600                                    W-IDDC-B1-MAX                         
016700                                    W-IDDC                                
016800     PERFORM IMS-GN-WDK7B                                                 
016900     PERFORM UNTIL SEGMENT-MISSING OR                                     
017000                   SEGMENT-NOMORE                                         
017100       MOVE PARM-IDDC            TO UT-IDDC                               
017200       MOVE PARM-IDDC-REF-NEW    TO UT-IDDC-REF                           
017300       MOVE SEQB-IDARTNR         TO UT-IDARTNR                            
017400       PERFORM S11-WRITE-W27144                                           
017500       PERFORM IMS-GN-WDK7B                                               
017600     END-PERFORM                                                          
017700     .                                                                    
017800     EJECT                                                                
017900 Z-FINIT SECTION.                                                         
018000     SKIP2                                                                
018100                                                                          
018200     CLOSE W27144                                                         
018300     MOVE 'S'                    TO POSTSUM-OPKOD                         
018400     CALL POSTSUM             USING POSTSUM-PARM                          
018500     .                                                                    
018600     EJECT                                                                
018700 S11-WRITE-W27144 SECTION.                                                
018800     SKIP2                                                                
018900     WRITE UT-POST             FROM UT-AREA                               
019000     MOVE 'UT'                   TO POSTSUM-TRANSTYP                      
019100     MOVE 'W27144 '              TO POSTSUM-FDNAMN                        
019200     MOVE 'W27144D1'             TO POSTSUM-DDNAMN2                       
019300     CALL POSTSUM             USING POSTSUM-PARM                          
019400     .                                                                    
019500     EJECT                                                                
019600* --- IMS SECTIONS  ---                                                   
019700                                                                          
019800     EJECT                                                                
019900 IMS-GN-WDK7B SECTION.                                                    
020000                                                                          
020100     STRING 'WDK7B1  (WDK7B1KY>=' W-WDK7B1KY-MIN-X                        
020200                    '&WDK7B1KY<=' W-WDK7B1KY-MAX-X ')'                    
020300             DELIMITED BY SIZE INTO SSA1                                  
020400     MOVE '  GE'                 TO GOOD-STATUSCODES                      
020500     CALL CBLTDLI USING GN WDK7B-PCB DLI-IO-WDK7B1 SSA1                   
020600     MOVE WDK7B-STATUS-CODE      TO STATUS-WS                             
020700     PERFORM IMS-STATUSCHECK                                              
020800     .                                                                    
020900     SKIP3                                                                
021000 IMS-GET-WDB601 SECTION.                                                  
021100                                                                          
021200     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
021300             DELIMITED BY SIZE INTO SSA1                                  
021400     MOVE '  GE'                 TO GOOD-STATUSCODES                      
021500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
021600     MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
021700     PERFORM IMS-STATUSCHECK                                              
021800     .                                                                    
021900     EJECT                                                                
022000 IMS-STATUSCHECK SECTION.                                                 
022100     SKIP2                                                                
022200     SET STATUS-IX               TO 1                                     
022300     SEARCH GOOD-STATUS                                                   
022400       AT END                                                             
022500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
022600             DELIMITED BY SIZE INTO ERROR-TEXT                            
022700         DISPLAY ERROR-TEXT                                               
022800         CALL FELLOG                                                      
022900       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
023000         CONTINUE                                                         
023100     END-SEARCH                                                           
023200     .                                                                    
