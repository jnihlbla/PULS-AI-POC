000100 PROCESS DYNAM                                                            
000200*        - THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM             
000300*                                                                         
001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     W5220400.                                                
001400 AUTHOR.         HAMMARIN BO.                                             
001500 DATE-WRITTEN.   NOV 2003.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001900*                                                                         
002000*    FUNCTION:                                                            
002100*        SELECTS DATA FROM DB2-TABLE T01IVW AND                           
002200*        CREATES A SEQUENCE-FILE FOR CUSTOMS                              
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSACTION: W52204T                                             
002800*                                                                         
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     SKIP2                                                                
003500 INPUT-OUTPUT SECTION.                                                    
003600                                                                          
003700 FILE-CONTROL.                                                            
003801     SKIP2                                                                
003802*          --- SEQUENCEFILE CUSTOMS                                       
003803     SELECT W52204                     ASSIGN TO W52204D1.                
003804     SKIP2                                                                
004100 DATA DIVISION.                                                           
004200     SKIP3                                                                
004300 FILE SECTION.                                                            
004401     SKIP3                                                                
004402 FD  W52204                                                               
004403     RECORDING       F                                                    
004404     BLOCK CONTAINS  0.                                                   
004405                                                                          
004406*01  POST    -COPY W522CUS    -L.                                         
004407     SKIP3                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700 77  IDPGM                       PIC X(08)   VALUE 'W5220400'.            
004800                                                                          
004900*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND                
005000 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
005100 77  KDRC-DISPLAY                PIC Z(5).                                
005200                                                                          
005300 77  YES                         PIC X       VALUE 'J'.                   
005400 77  NOO                         PIC X       VALUE 'N'.                   
005500                                                                          
006200 77  KEYS-SW                     PIC X       VALUE 'J'.                   
006300     88  KEYS-OK                             VALUE 'J'.                   
006400     88  KEYS-WRONG                          VALUE 'N'.                   
006410     EJECT                                                                
006412                                                                          
006413 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006414 01  FILLER REDEFINES DAGENS-DATUM.                                       
006415     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006416     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006417     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006418     EJECT                                                                
006419                                                                          
006420*    --- WS-AREA FOR SEQUENCEFILE                                         
006450 01  WS-DAREGDAT                 PIC X(8)    VALUE SPACE.                 
006460 01  WS-TIREGTID                 PIC S9(7)   VALUE ZERO COMP-3.           
006470 01  WS-IDLOPNR                  PIC S9(5)   VALUE ZERO COMP-3.           
006480 01  WS-IDPTYP                   PIC X(3)    VALUE 'CUS'.                 
006490 01  WS-TIRP                     PIC S9(2)   VALUE ZERO COMP-3.           
006491 01  WS-TIRP-DISPLAY             PIC  9(2).                               
006500 01  WS-FLKLAR                   PIC X       VALUE SPACE.                 
006510 01  WS-IV-DATA                  PIC X(200)  VALUE SPACE.                 
006595*                                                                         
006596 01  WS-IDPTYP-HEAD              PIC X(3)    VALUE '2  '.                 
006597*                                                                         
006610*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006700 01  GENERAL-SUBPROGRAMS.                                                 
006900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
007102     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007200                                                                          
007300*    --- PARAMETERS TO ABEND                                              
007400                                                                          
007500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007810 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
007820                                                                          
007830*    --- PARAMETRAR TILL DATKORT                                          
007840*                                                                         
007850 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W52204'.              
007860                                                                          
007870 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
007880                                                                          
007890*01  -COPY WDATKORT                                                       
007891     EJECT                                                                
007892                                                                          
008000 01  MESSAGE-CODES.                                                       
008200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
008301     EJECT                                                                
008302                                                                          
008502*01  -COPY WDATAREA                                                       
008503     EJECT                                                                
008602 01  CUS-AREA                    PIC X(24)   VALUE 'CUS-AREA'.            
008605                                                                          
008606*01  -COPY W522CUS -PRE CUS-                                              
008607     EJECT                                                                
008608                                                                          
009902 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
009903       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
009904                                                                          
009905 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
009906 01  DB2-WS.                                                              
009907     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
009908         88  CURSOR-OK                       VALUE 000.                   
009909         88  LINES-FOUND                     VALUE 000.                   
009910         88  LINES-MISSING                   VALUE 100.                   
009911         88  DOUBLE-LINES                    VALUE 811.                   
009912         88  RESOURCE-WRONG                  VALUE 904.                   
009913     03  GOOD-SQLCODECODES.                                               
009914         05  GOOD-SQLCODE OCCURS 5                                        
009920             INDEXED BY SQLCODE-IX PIC 9(3).                              
010000     EJECT                                                                
010100                                                                          
010301     EJECT                                                                
010302 01  FILLER                      PIC X(16)  VALUE 'T01IVW-AREA'.          
010303                                                                          
010310*01  -COPY T01IVW   -PRE T01IVW-                                          
010401     EJECT                                                                
010410     EXEC SQL INCLUDE T01IVW END-EXEC.                                    
010500     EJECT                                                                
010600 LINKAGE SECTION.                                                         
011001 PROCEDURE DIVISION.                                                      
011002                                                                          
011003 MAIN SECTION.                                                            
011600     PERFORM A-INIT                                                       
011700     PERFORM B-BEHANDLA-RADER                                             
012900     PERFORM Z-FINIT                                                      
013000     MOVE ZERO TO RETURN-CODE                                             
013100     GOBACK                                                               
013200     .                                                                    
013300     EJECT                                                                
013310                                                                          
013400 A-INIT SECTION.                                                          
013702     OPEN OUTPUT W52204                                                   
013712                                                                          
013715     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
013716     MOVE D-AAR           TO DAGENS-DATUM-AAR                             
013718     MOVE D-MAANAD        TO DAGENS-DATUM-MAANAD                          
013720     MOVE D-DAG           TO DAGENS-DATUM-DAG                             
013730                                                                          
013732     MOVE 'AAMMDD'        TO DAT-KDDATFORM                                
013733     MOVE DAGENS-DATUM    TO DAT-I-TIDATUM                                
013740     CALL WDATKONV USING                                                  
013750          DAT-KDDATFORM                                                   
013760          DAT-I-TIDATUM                                                   
013761          DAT-O-TIDATUM                                                   
013770          DAT-KDSVAR                                                      
013781     MOVE DAT-TIVV        TO WS-TIRP                                      
013782                             WS-TIRP-DISPLAY                              
013783     DISPLAY 'PERIOD='       WS-TIRP-DISPLAY                              
013901                                                                          
013920     INITIALIZE GOOD-SQLCODECODES                                         
014200     .                                                                    
014300     EJECT                                                                
014400                                                                          
018109 B-BEHANDLA-RADER SECTION.                                                
018111     PERFORM DB2-DCL-OPEN-CRS                                             
018113     PERFORM DB2-FETCH-CRS                                                
018114     PERFORM UNTIL LINES-MISSING                                          
018115       MOVE T01IVW-IV-DATA TO CUS-W522CUS                                 
018116       PERFORM S11-WRITE-W52204                                           
018117       PERFORM DB2-UPDATE-T01IVW                                          
018118       PERFORM DB2-FETCH-CRS                                              
018120     END-PERFORM                                                          
018121     PERFORM DB2-CLOSE-CRS                                                
018122     .                                                                    
018123     EJECT                                                                
018124                                                                          
018700 Z-FINIT SECTION.                                                         
018900     CLOSE W52204                                                         
019400     .                                                                    
019410                                                                          
019500     EJECT                                                                
023002 S11-WRITE-W52204 SECTION.                                                
023045     WRITE POST FROM CUS-W522CUS                                          
023046     .                                                                    
023050     EJECT                                                                
023060                                                                          
023546 DB2-DCL-OPEN-CRS SECTION.                                                
023550     MOVE 000100 TO GOOD-SQLCODECODES                                     
023560                                                                          
023570     EXEC SQL DECLARE T01IVW-CRS CURSOR WITH HOLD FOR                     
023580         SELECT DAREGDAT                                                  
023581              , TIREGTID                                                  
023582              , IDLOPNR                                                   
023583              , IDPTYP                                                    
023584              , TIRP                                                      
023585              , FLKLAR                                                    
023586              , IV_DATA                                                   
023587              , IV_DATA2                                                  
023588                                                                          
023589         FROM T01IVW                                                      
023590                                                                          
023591         WHERE TIRP   = :WS-TIRP                                          
023592           AND IDPTYP = :WS-IDPTYP                                        
023593           AND FLKLAR = 'N'                                               
023594                                                                          
023595         FOR UPDATE OF FLKLAR                                             
023596                                                                          
023597     END-EXEC                                                             
023598                                                                          
023599     MOVE 000100 TO GOOD-SQLCODECODES                                     
023600                                                                          
023601     EXEC SQL                                                             
023602        OPEN T01IVW-CRS                                                   
023603     END-EXEC                                                             
023604                                                                          
023605     MOVE SQLCODE TO SQLCODE-WS                                           
023607     PERFORM DB2-STATUS-CHECK                                             
023608     .                                                                    
023609     EJECT                                                                
023611                                                                          
023612 DB2-FETCH-CRS SECTION.                                                   
023613     MOVE 000100  TO GOOD-SQLCODECODES                                    
023614     EXEC SQL                                                             
023615         FETCH T01IVW-CRS                                                 
023616         INTO   :T01IVW-DAREGDAT                                          
023617              , :T01IVW-TIREGTID                                          
023618              , :T01IVW-IDLOPNR                                           
023619              , :T01IVW-IDPTYP                                            
023620              , :T01IVW-TIRP                                              
023621              , :T01IVW-FLKLAR                                            
023622              , :T01IVW-IV-DATA                                           
023623              , :T01IVW-IV-DATA2                                          
023624     END-EXEC                                                             
023625                                                                          
023626     MOVE SQLCODE TO SQLCODE-WS                                           
023627     PERFORM DB2-STATUS-CHECK                                             
023628     .                                                                    
023629     EJECT                                                                
023631                                                                          
023632 DB2-UPDATE-T01IVW SECTION.                                               
023633     MOVE 000     TO GOOD-SQLCODECODES                                    
023634     EXEC SQL                                                             
023635         UPDATE T01IVW                                                    
023636         SET FLKLAR = 'J'                                                 
023651         WHERE   CURRENT OF T01IVW-CRS                                    
023652     END-EXEC                                                             
023653                                                                          
023654     MOVE SQLCODE TO SQLCODE-WS                                           
023656     PERFORM DB2-STATUS-CHECK                                             
023657     .                                                                    
023658     EJECT                                                                
023660                                                                          
023661 DB2-CLOSE-CRS SECTION.                                                   
023662     EXEC SQL                                                             
023663         CLOSE T01IVW-CRS                                                 
023664     END-EXEC                                                             
023665     .                                                                    
023666     EJECT                                                                
023782                                                                          
023783 DB2-STATUS-CHECK SECTION.                                                
023784     SET SQLCODE-IX TO 1                                                  
023785     SEARCH GOOD-SQLCODE                                                  
023786       AT END                                                             
023787          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
023788          DELIMITED BY SIZE INTO ERROR-TEXT                               
023789          CALL ABEND USING RKOD-ABEND-DB2                                 
023790       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
023791           CONTINUE                                                       
023792     END-SEARCH                                                           
023800     .                                                                    
