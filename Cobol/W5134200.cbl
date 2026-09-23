000100 ID  DIVISION.                                                            
000200 PROGRAM-ID.    W5134200.                                                 
000300 AUTHOR.        CHRISTINA BRUHN                                           
000400 DATE-WRITTEN.  JUNI     1988.                                            
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000900*        SB-PGM SOM LÄSER NER WDH7.                                       
001000*        FIL TILL REVISORER M FL.                                         
001100*                                                                         
001200     EJECT                                                                
001300 ENVIRONMENT DIVISION.                                                    
001400     SKIP2                                                                
001500 INPUT-OUTPUT SECTION.                                                    
001600                                                                          
001700 FILE-CONTROL.                                                            
001800     SKIP2                                                                
001900*- - - - - - - - - UTFIL                                                  
002000     SELECT W51343               ASSIGN TO UT-S-W51342D1.                 
002100     EJECT                                                                
002200 DATA DIVISION.                                                           
002300     SKIP2                                                                
002400 FILE SECTION.                                                            
002500     SKIP2                                                                
002600 FD  W51343                                                               
002700     RECORDING F                                                          
002800     BLOCK CONTAINS 0.                                                    
002900     SKIP2                                                                
003000*01  POST -COPY  W51343  -PRE UTFIL-  -L.                                 
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400     SKIP3                                                                
003500*    -COPY WY2000W1                                                       
003600     SKIP3                                                                
003700 77  IDPGM                       PIC X(8)  VALUE 'W5134200'.              
003800 77  JA                          PIC X(1)    VALUE 'J'.                   
003900 77  NEJ                         PIC X(1)    VALUE 'N'.                   
004000 77  SKRIV-UTFIL                 PIC X(1)    VALUE 'N'.                   
004100                                                                          
004200 01  DATUM-TIAAMMDD              PIC 9(7).                                
004300                                                                          
004400 01  DYNAMISKA-SUBPROGRAM.                                                
004500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
004800     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
004900     EJECT                                                                
005000 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
005100                                                                          
005200*01  -COPY WDATKORT                                                       
005300     EJECT                                                                
005400*01  -COPY W0005  -PRE POSTSUM-.                                          
005500*                                                                         
005600*    --- VALID IDDC CODES                                                 
005700*                                                                         
005800*01  -COPY WWDC99                                                         
005900*                                                                         
006000     EJECT                                                                
006100 01  FILLER                     PIC X(16)   VALUE 'UTAREA'.               
006200     SKIP2                                                                
006300 01  W51343-POST.                                                         
006400*    03  -COPY W51343  -PRE UT-.                                          
006500     EJECT                                                                
006600 01  FILLER                     PIC X(8)    VALUE 'IMS-WS  '.             
006700                                                                          
006800 01  IMS-WS.                                                              
006900                                                                          
007000     03  STATUS-WS               PIC X(2).                                
007100        88  SEGMENT-FINNS                    VALUE '  '.                  
007200        88  SEGMENT-SAKNAS                   VALUE 'GE'.                  
007210        88  SEGMENT-SLUT                     VALUE 'GB'.                  
007300                                                                          
007400     03  GODK-STATUSKODER.                                                
007500         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
007600     SKIP2                                                                
007700 01  SSA1                     PIC X(32).                                  
007800*01  -COPY W0003                                                          
007900     EJECT                                                                
007901 01  W-IDDC-B6-X.                                                         
007902     03 W-IDDC-B6             PIC X(2).                                   
007903                                                                          
007910     EJECT                                                                
008000 01  DLI-IO-AREA.                                                         
008100     03  IO-AREA              PIC X(300).                                 
008200                                                                          
008300*    03  FILLER -COPY WDH701              -RED IO-AREA                    
008400     EJECT                                                                
008500*    03  FILLER -COPY WDH711              -RED IO-AREA                    
008501     EJECT                                                                
008510                                                                          
008520 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
008530 01   DLI-IO-AREA-B601.                                                   
008540*     03  -COPY WDB601                                                    
008550                                                                          
008600     EJECT                                                                
008700 LINKAGE SECTION.                                                         
008800     SKIP3                                                                
008900*01  -COPY W0008   -PRE WDH7-                                             
009000         05  FILLER           PIC X(1).                                   
009010 SKIP3                                                                    
009020*01  -COPY W0008   -PRE WDB6-                                             
009030     05  FILLER               PIC X.                                      
009040                                                                          
009100     EJECT                                                                
009200 PROCEDURE DIVISION USING  WDH7-PCB WDB6-PCB.                             
009300     ENTRY 'DLITCBL' USING WDH7-PCB WDB6-PCB.                             
009400                                                                          
009500     PERFORM A-INIT                                                       
009600                                                                          
009700     PERFORM IMS-GET-WDH7                                                 
009800                                                                          
009900     PERFORM UNTIL SEGMENT-SLUT                                           
010000                                                                          
010100        EVALUATE WDH7-SEG-NAME-FB                                         
010200                                                                          
010300           WHEN 'WDH701  '                                                
010400             MOVE INVA-IDARTNR             TO UT-IDARTNR                  
010500                                                                          
010600           WHEN 'WDH711  '                                                
010700                                                                          
010800             MOVE INVH-DAREGDAT-CLO(3:6)   TO TMP1-YYMMDD                 
010900             MOVE DATUM-TIAAMMDD           TO TMP2-YYMMDD                 
011000             PERFORM WY2000P1                                             
011100                                                                          
011200             IF  TMP1-YYMMDD > TMP2-YYMMDD - 30000                        
011300              MOVE INVH-IDDC               TO WS-IDDC                     
011400                                              W-IDDC-B6                   
011410              PERFORM IMS-GU-WDB601                                       
011420                                                                          
011430              IF SEGMENT-FINNS AND                                        
011500                DCS-CDC OR DCS-CDC-TR OR DCS-SDC OR DCS-NDC               
011600               MOVE ZERO                   TO UT-KVINVS                   
011700                                              UT-TIINVDAT                 
011720               MOVE INVH-PRARTSTD          TO UT-PRARTSTD                 
011810               MOVE INVH-IDDC              TO UT-IDDC                     
011900               MOVE INVH-DAREGDAT-CLO(3:6) TO UT-TIJUSTDA                 
012000               MOVE INVH-KVJUSTKV          TO UT-KVJUSTKV                 
012100               MOVE INVH-KDJUSTYP          TO UT-KDJUSTYP                 
012200               PERFORM S01-SKRIV-UTFIL                                    
012310              END-IF                                                      
012400             END-IF                                                       
012500                                                                          
012600         END-EVALUATE                                                     
012700                                                                          
012800         PERFORM IMS-GET-WDH7                                             
012900     END-PERFORM                                                          
013000                                                                          
013100     PERFORM Z-FINIT                                                      
013200                                                                          
013300     MOVE ZERO TO RETURN-CODE                                             
013400     GOBACK                                                               
013500     .                                                                    
013600     EJECT                                                                
013700 A-INIT SECTION.                                                          
013800                                                                          
013900     OPEN OUTPUT W51343                                                   
014000                                                                          
014100     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
014200                                                                          
014300     MOVE ZERO            TO DATUM-TIAAMMDD                               
014400     MOVE D-AAR           TO DATUM-TIAAMMDD(2:2)                          
014500     MOVE D-MAANAD        TO DATUM-TIAAMMDD(4:2)                          
014600     MOVE D-DAG           TO DATUM-TIAAMMDD(6:2)                          
014700     .                                                                    
014800     SKIP2                                                                
014900 Z-FINIT SECTION.                                                         
015000                                                                          
015100     CLOSE W51343                                                         
015200                                                                          
015300     MOVE 'S' TO POSTSUM-OPKOD                                            
015400     CALL POSTSUM USING POSTSUM-PARM                                      
015500     .                                                                    
015600     SKIP3                                                                
015700 S01-SKRIV-UTFIL   SECTION.                                               
015800                                                                          
015900     WRITE UTFIL-POST FROM W51343-POST                                    
016000                                                                          
016100     MOVE 'W51343'    TO POSTSUM-FDNAMN                                   
016200     MOVE 'W51343D1'  TO POSTSUM-DDNAMN2                                  
016300     MOVE 'UT '       TO POSTSUM-TRANSTYP                                 
016400     CALL POSTSUM USING POSTSUM-PARM                                      
016500     .                                                                    
016600     EJECT                                                                
016700*         * I M S  S E C T I O N                                          
016800                                                                          
016900 IMS-GET-WDH7         SECTION.                                            
017000                                                                          
017100     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
017200     CALL CBLTDLI USING GN WDH7-PCB IO-AREA                               
017300     MOVE WDH7-STATUS-CODE TO STATUS-WS                                   
017400     PERFORM IMS-STATUSKONTROLL                                           
017500     .                                                                    
017510     SKIP3                                                                
017520 IMS-GU-WDB601    SECTION.                                                
017530     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
017540          DELIMITED BY SIZE INTO SSA1                                     
017550     MOVE '  GE' TO GODK-STATUSKODER                                      
017560     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
017570     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
017580     PERFORM IMS-STATUSKONTROLL                                           
017590     .                                                                    
017600     SKIP3                                                                
017700 IMS-STATUSKONTROLL   SECTION.                                            
017800                                                                          
017900     SET STATUS-IX TO 1                                                   
018000     SEARCH GODK-STATUS                                                   
018100       AT END                                                             
018200       CALL FELLOG                                                        
018300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
018400     END-SEARCH                                                           
018500     .                                                                    
018800*    -COPY WY2000P1                                                       
