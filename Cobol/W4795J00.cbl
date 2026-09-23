000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W4795J00.                                                 
000400 AUTHOR.        PER FREDRIKSSON.                                          
000500 DATE-WRITTEN.  NOV 1995.                                                 
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*                                                                         
001100*        PROGRAMMET LÄSER WDE6 MED SB.                                    
001200*        SUGER UT INFORMATION OCH SKAPAR EN FIL.                          
001300                                                                          
001400     EJECT                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800*                                                                         
001900 FILE-CONTROL.                                                            
002000                                                                          
002100*                                                                         
002500     SELECT W4795J    ASSIGN TO W4795JD1.                                 
002800*                                                                         
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP2                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
005500 FD  W4795J                                                               
005600     LABEL RECORD    STANDARD                                             
005700     RECORDING       F                                                    
005800     BLOCK CONTAINS 0.                                                    
005900                                                                          
006000*01  E64-AREA  -COPY W4795J    -L                                         
006100     SKIP3                                                                
007500     EJECT                                                                
007600 WORKING-STORAGE SECTION.                                                 
007601                                                                          
007610*    -- CHECKED BY WY2000                                                 
007700*                                                                         
007800 77  PROGRAM-NAMN                PIC X(6) VALUE 'W4795J'.                 
007900                                                                          
008000 77  JA                          PIC X(1)    VALUE 'J'.                   
008100 77  NEJ                         PIC X(1)    VALUE 'N'.                   
008600                                                                          
008700 01  DAGENS-DATUM.                                                        
008800   03  DAGENS-AAR                PIC 9(2).                                
008900   03  DAGENS-MAANAD             PIC 9(2).                                
009000   03  DAGENS-DAG                PIC 9(2).                                
009010 01  DAGENS-DATUM-N REDEFINES DAGENS-DATUM                                
009011                                 PIC 9(6).                                
009012                                                                          
010200                                                                          
010300 01  FAKT-DATUM-E64              PIC 9(6).                                
010600                                                                          
011500     SKIP3                                                                
011600 01  DYNAMISKA-SUBPROGRAM.                                                
011700*                                                                         
011800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
012000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
012100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012300                                                                          
012700 01  FILLER                      PIC X(8)    VALUE 'UT-AREA '.            
013400     EJECT                                                                
013500*01          -COPY W4795J   -PRE E64-                                     
014220     EJECT                                                                
014700*   ---- PARAMETER TILL DATUMKORT                                         
014800                                                                          
014900 01  DATUMKORT-ID                PIC X(06) VALUE '000001'.                
015000                                                                          
015100*01  -COPY WDATKORT                                                       
015200     EJECT                                                                
015300 01  FILLER                      PIC X(8)    VALUE 'IMS-WS  '.            
015400                                                                          
015500 01  W-WDE4F1KY-MIN-X.                                                    
015600     03  W-IDPRODNR-MIN            PIC S9(7) VALUE +0 COMP-3.             
015610     03  W-IDKOLLI-MIN             PIC S9(5) VALUE +0 COMP-3.             
015700     03  FILLER                    PIC X(22) VALUE LOW-VALUE.             
015701                                                                          
015702 01  W-WDE4F1KY-MAX-X.                                                    
015703     03  W-IDPRODNR-MAX            PIC S9(7) VALUE +0 COMP-3.             
015704     03  W-IDKOLLI-MAX             PIC S9(5) VALUE +0 COMP-3.             
015705     03  FILLER                    PIC X(22) VALUE HIGH-VALUE.            
015706                                                                          
015710 01  IMS-WS.                                                              
015720                                                                          
015730     03  STATUS-WS                 PIC X(2).                              
015800        88  SEGMENT-FINNS                    VALUE '  '.                  
015900        88  SEGMENT-SAKNAS                   VALUE 'GE'.                  
016000        88  SEGMENT-SLUT                     VALUE 'GB'.                  
016100                                                                          
016200     03  GODK-STATUSKODER.                                                
016300         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
016400                                                                          
016710     SKIP3                                                                
016711 01  SSA1                        PIC X(150).                              
016713     EJECT                                                                
016714*    --- IMS FUNKTIONSKODER                                               
016715*01  -COPY W0003                                                          
016720     EJECT                                                                
016800*01  -COPY W0005    -PRE POSTSUM-                                         
016900     EJECT                                                                
017000 01  DLI-IO-AREA1.                                                        
017100     03  IO-AREA             PIC X(352).                                  
017200     SKIP3                                                                
017300*    03  WDE601    -COPY WDE601    -RED IO-AREA                           
017400     EJECT                                                                
017500*    03  WDE611    -COPY WDE611    -RED IO-AREA                           
017600     EJECT                                                                
017610 01  DLI-IO-E4F1.                                                         
017700*    03  -COPY WDE4F1                                                     
017800     EJECT                                                                
017900 LINKAGE SECTION.                                                         
018000     SKIP3                                                                
018100*01  -COPY W0008   -PRE WDE6-                                             
018200         05  FILLER          PIC X(1).                                    
018330     EJECT                                                                
018340*01  -COPY W0008   -PRE WDE4F-                                            
018350         05  FILLER          PIC X(1).                                    
018360     EJECT                                                                
018400 PROCEDURE DIVISION  USING WDE6-PCB WDE4F-PCB.                            
018500     ENTRY 'DLITCBL' USING WDE6-PCB WDE4F-PCB.                            
018600                                                                          
018700     PERFORM A-INIT                                                       
018800     PERFORM IMS-GET-WDE6                                                 
018900                                                                          
019000     PERFORM UNTIL SEGMENT-SLUT                                           
019100                                                                          
019200       EVALUATE WDE6-SEG-NAME-FB                                          
019300         WHEN 'WDE601'                                                    
019700           PERFORM WDE601-SKAPA-E64                                       
020200                                                                          
020300         WHEN 'WDE611'                                                    
020700           PERFORM WDE611-SKAPA-E64                                       
020900                                                                          
021400           PERFORM B-SKRIV-E64                                            
021600       END-EVALUATE                                                       
021700                                                                          
021800       PERFORM IMS-GET-WDE6                                               
021900     END-PERFORM                                                          
022000                                                                          
022100     PERFORM Z-FINIT                                                      
022200     MOVE ZERO TO RETURN-CODE                                             
022300     GOBACK                                                               
022310     .                                                                    
022400     EJECT                                                                
022500 A-INIT SECTION.                                                          
022600                                                                          
022700     OPEN OUTPUT W4795J                                                   
023300                                                                          
023400     MOVE PROGRAM-NAMN     TO POSTSUM-PROGNAMN                            
023600                                                                          
023700     CALL DATKORT  USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT              
023800                                                                          
023900     MOVE D-AAR            TO DAGENS-AAR                                  
024000     MOVE D-MAANAD         TO DAGENS-MAANAD                               
024010     MOVE D-DAG            TO DAGENS-DAG                                  
024100     .                                                                    
024200     EJECT                                                                
024300                                                                          
031100 WDE601-SKAPA-E64       SECTION.                                          
031200                                                                          
031300     MOVE VORD-IDPRODNR             TO E64-IDPRODNR                       
031301                                       W-IDPRODNR-MIN                     
031302                                       W-IDPRODNR-MAX                     
031310     .                                                                    
031400     EJECT                                                                
031500                                                                          
050400 WDE611-SKAPA-E64       SECTION.                                          
050500                                                                          
050600     MOVE KOLLI-TIFAKT          TO FAKT-DATUM-E64                         
051500                                                                          
051600     IF FAKT-DATUM-E64 = DAGENS-DATUM-N                                   
051930       MOVE KOLLI-IDKOLLI         TO E64-IDKOLLI                          
051940       MOVE KOLLI-TIPACKN         TO E64-TIPACKN                          
051950       MOVE KOLLI-TIFAKT          TO E64-TIFAKT                           
051960       MOVE KOLLI-TILASTN         TO E64-TILASTN                          
052100       MOVE KOLLI-IDFAKT          TO E64-IDFAKT                           
052110       MOVE KOLLI-KDKOLLI         TO E64-KDKOLLI                          
052120       MOVE KOLLI-KVORDRAD        TO E64-KVORDRAD                         
052130       MOVE KOLLI-SUORDV-KOLLI    TO E64-SUORDV-KOLLI                     
052131       MOVE KOLLI-SUORDV-LOC      TO E64-SUORDV-LOC                       
052132       MOVE KOLLI-SUORDV-LOCPREL  TO E64-SUORDV-LOCPREL                   
052133       MOVE KOLLI-KDVALISO        TO E64-KDVALISO                         
052140       MOVE KOLLI-VKORDBTO-KOLLI  TO E64-VKORDBTO-KOLLI                   
052150       MOVE KOLLI-VKORDNTO-KOLLI  TO E64-VKORDNTO-KOLLI                   
052200     END-IF                                                               
052210     .                                                                    
052300     EJECT                                                                
052400                                                                          
059100 B-SKRIV-E64              SECTION.                                        
059200                                                                          
059300     IF FAKT-DATUM-E64 = DAGENS-DATUM-N                                   
059301       MOVE KOLLI-IDKOLLI       TO W-IDKOLLI-MIN W-IDKOLLI-MAX            
059310       PERFORM IMS-GU-WDE4F1                                              
059320       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                       
059400         MOVE 'E64'                TO E64-IDPTYP                          
059500         MOVE SEQF-IDDISTR         TO E64-IDDISTR                         
059600         MOVE SEQF-IDKUNDNR        TO E64-IDKUNDNR                        
059700         MOVE SEQF-IDKUNDRF        TO E64-IDKUNDRF                        
059800         MOVE SEQF-IDPURAD         TO E64-IDRADNR-KO                      
059900         MOVE SEQF-KVLEVART        TO E64-KVLEVART2                       
060000                                                                          
060100         WRITE E64-AREA FROM E64-W4795J                                   
060200                                                                          
060300         MOVE 'W4795J'   TO POSTSUM-FDNAMN                                
060400         MOVE 'W4795JD1' TO POSTSUM-DDNAMN2                               
060500         MOVE 'E64'      TO POSTSUM-TRANSTYP                              
060600                                                                          
060700         CALL POSTSUM USING POSTSUM-PARM                                  
060710         PERFORM IMS-GN-WDE4F1                                            
060720       END-PERFORM                                                        
060800     END-IF                                                               
060810     .                                                                    
060900     EJECT                                                                
061000                                                                          
061100 Z-FINIT  SECTION.                                                        
061200                                                                          
061300     CLOSE W4795J                                                         
061900                                                                          
062000     MOVE 'S'          TO POSTSUM-OPKOD                                   
062100     CALL POSTSUM USING POSTSUM-PARM                                      
062110     .                                                                    
062200     EJECT                                                                
062300                                                                          
065400     EJECT                                                                
065500                                                                          
065630*         * I M S  S E C T I O N                                          
065700                                                                          
065800 IMS-GET-WDE6             SECTION.                                        
065900                                                                          
066000     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
066100     CALL CBLTDLI USING GN WDE6-PCB DLI-IO-AREA1                          
066200     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
066300     PERFORM IMS-STATUSKONTROLL                                           
066310     .                                                                    
066400     SKIP3                                                                
066410 IMS-GU-WDE4F1                 SECTION.                                   
066420     STRING 'WDE4F1  (WDE4F1KY>=' W-WDE4F1KY-MIN-X                        
066430                    '&WDE4F1KY<=' W-WDE4F1KY-MAX-X ')'                    
066440            DELIMITED BY SIZE INTO SSA1                                   
066450     MOVE '  GE' TO GODK-STATUSKODER                                      
066460     CALL CBLTDLI USING GU WDE4F-PCB DLI-IO-E4F1 SSA1                     
066470     MOVE WDE4F-STATUS-CODE TO STATUS-WS                                  
066480     PERFORM IMS-STATUSKONTROLL                                           
066490     .                                                                    
066491     SKIP3                                                                
066492 IMS-GN-WDE4F1                 SECTION.                                   
066493     STRING 'WDE4F1  (WDE4F1KY>=' W-WDE4F1KY-MIN-X                        
066494                    '&WDE4F1KY<=' W-WDE4F1KY-MAX-X ')'                    
066495            DELIMITED BY SIZE INTO SSA1                                   
066496     MOVE '  GEGB' TO GODK-STATUSKODER                                    
066497     CALL CBLTDLI USING GN WDE4F-PCB DLI-IO-E4F1 SSA1                     
066498     MOVE WDE4F-STATUS-CODE TO STATUS-WS                                  
066499     PERFORM IMS-STATUSKONTROLL                                           
066500     .                                                                    
066510     SKIP3                                                                
066600                                                                          
066610 IMS-STATUSKONTROLL       SECTION.                                        
066700                                                                          
066800     SET STATUS-IX TO 1                                                   
066900     SEARCH GODK-STATUS AT END CALL FELLOG                                
067000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
067100     END-SEARCH.                                                          
