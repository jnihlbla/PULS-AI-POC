000100 PROCESS DYNAM                                                            
000200*        - THE OPTION ABOVE IS NEEDED TO LINK A BMP-DB2-PGM               
000400*                                                                         
001150 ID DIVISION.                                                             
001200 PROGRAM-ID.     WF101700.                                                
001300 AUTHOR.         ANDERS HENRIKSSON                                        
001400 DATE-WRITTEN.   JUN 2006.                                                
001500 DATE-COMPILED.                                                           
001700                                                                          
001800*   PGM                                                                   
001900*   -SELECTS ROWS FROM COUNTRY CODE TABLE                                 
001901*      T01COCO                                                            
001904                                                                          
002010*   - SENDS DOCUMENT DATA RECORDS FOR VCCS TO                             
002020*      PULS BY USING WZ01SEND                                             
002410*                                                                         
002500                                                                          
002700 ENVIRONMENT DIVISION.                                                    
002800                                                                          
002900 INPUT-OUTPUT SECTION.                                                    
003011 FILE-CONTROL.                                                            
003500 DATA DIVISION.                                                           
003700 FILE SECTION.                                                            
003800                                                                          
004000 WORKING-STORAGE SECTION.                                                 
004200 77  IDPGM                     PIC X(8)    VALUE 'WF101700'.              
004430                                                                          
004670 01  WS-CURRENT-DATE           PIC X(8)  VALUE SPACE.                     
004687     EJECT                                                                
004688                                                                          
004689 01  ERRTEXT.                                                             
004690     03  FILLER                PIC X(8)    VALUE 'ERRTEXT'.               
004691     03  ERRTEXT-STR           PIC X(72)   VALUE SPACE.                   
004692 01  KDRC-DISPLAY              PIC Z(5).                                  
004693     EJECT                                                                
004700                                                                          
006000 01  DYNAMISKA-SUBPROGRAM.                                                
006400     03  ABEND                 PIC X(8)    VALUE 'ABEND   '.              
006500     03  WZ01SEND              PIC X(8)    VALUE 'WZ01SEND'.              
006501     EJECT                                                                
006502                                                                          
006513*    --- PARAMETRAR TILL ABEND                                            
006514*                                                                         
006515 01  RKOD-ABEND-DB2            PIC S9(4)   VALUE +998 COMP SYNC.          
006516 01  RKOD-ABEND-WITH-DUMP      PIC S9(4)   VALUE +999 COMP SYNC.          
006517     EJECT                                                                
006518                                                                          
006519*    --- AREOR FÖR KOMMUNIKATION                                          
006520 01  FILLER                    PIC X(16)   VALUE 'SEND-CONTROL'.          
006530*01  -COPY WZ01SEND                                                       
006540     EJECT                                                                
006541                                                                          
006547 01  UT-AREA-START             PIC X(24)   VALUE 'UT-AREA-START'.         
006548                                                                          
006549 01  UT02-AREA.                                                           
006550*    03  -COPY WZ01REQU                                                   
006551     03  UT-BELAND             PIC X(35)   VALUE SPACE.                   
006552     03  UT-IDLANDX3           PIC X(3)    VALUE SPACE.                   
006553*                                                                         
010602*        WORK-AREAS FOR DB2-SECTIONS                                      
010603*                                                                         
010604 01  FILLER                    PIC X(16)   VALUE 'DB2-COCO   '.           
010605*01  -COPY T01COCO        -PRE COCO-                                      
010610     EJECT                                                                
010611                                                                          
010620 01  FILLER                    PIC X(16)   VALUE 'COCO-AREA'.             
010621       EXEC SQL INCLUDE T01COCO END-EXEC.                                 
010622                                                                          
010631 01  FILLER                    PIC X(16)   VALUE 'SQLCA-AREA'.            
010632       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
010633*                        **** STATUS-CODE FROM DB2                        
010634                                                                          
010635 01  FILLER                    PIC X(16)   VALUE 'SQLCODE-WS'.            
010636 01  DB2-WS.                                                              
010637   03  SQLCODE-WS              PIC S9(3)   VALUE ZERO.                    
010638     88  ROW-FOUND                         VALUE +000.                    
010639     88  ROW-MISSING                       VALUE +100.                    
010640   03  GOOD-SQLCODES.                                                     
010641     05  GOOD-SQLCODE OCCURS 5                                            
010642         INDEXED BY SQLCODE-IX PIC 999.                                   
010643     EJECT                                                                
010644                                                                          
010645 LINKAGE SECTION.                                                         
010646*01  -COPY W0009   -PRE MSG-                                              
010647     EJECT                                                                
010650                                                                          
010717 PROCEDURE DIVISION  USING MSG-PCB.                                       
010719 MAIN SECTION.                                                            
010720     ENTRY 'DLITCBL' USING MSG-PCB.                                       
010800                                                                          
011100     PERFORM A-INIT                                                       
011200                                                                          
011600     PERFORM B-EXECUTE                                                    
011900                                                                          
012400     PERFORM Z-FINISH                                                     
012600     MOVE ZERO TO RETURN-CODE                                             
012700     GOBACK                                                               
012800     .                                                                    
012900     EJECT                                                                
012910                                                                          
013000 A-INIT SECTION.                                                          
014042     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
014100     .                                                                    
014200     EJECT                                                                
014210                                                                          
014985 B-EXECUTE SECTION.                                                       
014986     PERFORM S90-COUNTRY-OPEN                                             
014987                                                                          
015003     PERFORM DB2-OPEN-CRS-COCO                                            
015004     PERFORM DB2-FETCH-CRS-COCO                                           
015005     PERFORM UNTIL ROW-MISSING                                            
015007       PERFORM BA-BUILD-OUTPUT-COCO                                       
015008       PERFORM S90-COUNTRY-PUT                                            
015009       PERFORM DB2-FETCH-CRS-COCO                                         
015010     END-PERFORM                                                          
015011     PERFORM DB2-CLOSE-CRS-COCO                                           
015012                                                                          
015168     PERFORM S90-COUNTRY-CLOSE                                            
015169     .                                                                    
015170     EJECT                                                                
015171                                                                          
015172 BA-BUILD-OUTPUT-COCO          SECTION.                                   
015175     MOVE COCO-BELAND           TO UT-BELAND                              
015176     MOVE COCO-IDLANDX3         TO UT-IDLANDX3                            
015190     .                                                                    
015195     EJECT                                                                
015196                                                                          
015262 Z-FINISH SECTION.                                                        
015263     CONTINUE                                                             
015264     .                                                                    
015265     EJECT                                                                
015266                                                                          
015267 S90-COUNTRY-OPEN SECTION.                                                
015268     MOVE 'CARPARTS.PULS.RECCOUNTRY'     TO SEND-ADDISPABS                
015269     MOVE 'OPEN'                          TO SEND-KDFUNC                  
015270     CALL WZ01SEND USING SEND-CONTROL-AREA                                
015280                         SEND-OPEN-AREA                                   
015290     IF SEND-KDRC > ZERO                                                  
015300       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
015310       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
015311       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
015312       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
015313     END-IF                                                               
015314     .                                                                    
015315                                                                          
015322 S90-COUNTRY-PUT SECTION.                                                 
015323     MOVE 'PUT'                           TO SEND-KDFUNC                  
015324     MOVE LENGTH OF UT02-AREA             TO SEND-KVDLEN                  
015325     CALL WZ01SEND USING SEND-CONTROL-AREA                                
015326                         SEND-KVDLEN                                      
015327                         UT02-AREA                                        
015329     IF SEND-KDRC > 1                                                     
015330       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
015331       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
015332       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
015333       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
015334     END-IF                                                               
015335     .                                                                    
015336                                                                          
015337 S90-COUNTRY-CLOSE SECTION.                                               
015338     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
015339     CALL WZ01SEND USING SEND-CONTROL-AREA                                
015340     .                                                                    
015341     EJECT                                                                
015342                                                                          
015343* --- DB2 SECTIONS  ---                                                   
015344*                                                                         
015350****** COUNTRY CODES ISO      ********                                    
015400*                                                                         
017454 DB2-OPEN-CRS-COCO SECTION.                                               
017456     EXEC SQL DECLARE COCO-CRS CURSOR FOR                                 
017457     SELECT   BELAND                                                      
017458             ,IDLANDX3                                                    
017463                                                                          
017464     FROM     T01COCO                                                     
017465                                                                          
017473     ORDER BY BELAND                                                      
017474             ,IDLANDX3                                                    
017475                                                                          
017476     FOR FETCH ONLY                                                       
017477     END-EXEC                                                             
017478                                                                          
017479     MOVE 000            TO GOOD-SQLCODES                                 
017480     EXEC SQL OPEN COCO-CRS                                               
017481     END-EXEC                                                             
017482     MOVE SQLCODE        TO SQLCODE-WS                                    
017483     PERFORM DB2-STATUS-CHECK                                             
017484     .                                                                    
017485     EJECT                                                                
017486                                                                          
017487 DB2-FETCH-CRS-COCO SECTION.                                              
017500     EXEC SQL FETCH COCO-CRS INTO                                         
017600            :COCO-BELAND                                                  
017701           ,:COCO-IDLANDX3                                                
017900     END-EXEC                                                             
017910                                                                          
017920     MOVE 000100         TO GOOD-SQLCODES                                 
018000     MOVE SQLCODE        TO SQLCODE-WS                                    
018200     PERFORM DB2-STATUS-CHECK                                             
018300     .                                                                    
019088     EJECT                                                                
019089                                                                          
019090 DB2-CLOSE-CRS-COCO SECTION.                                              
019092     EXEC SQL CLOSE COCO-CRS                                              
019093     END-EXEC                                                             
019094     .                                                                    
019116     EJECT                                                                
019117                                                                          
019560 DB2-STATUS-CHECK SECTION.                                                
019561     SET SQLCODE-IX         TO 1                                          
019562     SEARCH GOOD-SQLCODE AT END                                           
019563           CALL ABEND USING RKOD-ABEND-DB2                                
019564        WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
019565           CONTINUE                                                       
019570     END-SEARCH                                                           
019600     .                                                                    
