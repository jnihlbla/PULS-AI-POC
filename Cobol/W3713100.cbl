000100 PROCESS DYNAM                                                            
000200*        - OVANSTÅENDE BEHÖVS FÖR LÄNKNING AV ETT BATCH-DB2-PGM           
000400*                                                                         
001150 ID DIVISION.                                                             
001200 PROGRAM-ID.     W3713100.                                                
001300 AUTHOR.         BO HAMMARIN.                                             
001400 DATE-WRITTEN.   SEPT 2000.                                               
001500 DATE-COMPILED.                                                           
001700                                                                          
001800*    FUNKTION:                                                            
001900*        PROGRAM READS BYLRAD (DB2-TABLE).                                
002000*        PROGRAM WRITES SELECTED OUTPUT TO A FILE                         
002400*        FOR LATER LOADING OF BYLACK (DB2-TABLE)                          
002410*                                                                         
002500                                                                          
002700 ENVIRONMENT DIVISION.                                                    
002800                                                                          
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003010                                                                          
003011 FILE-CONTROL.                                                            
003020*          --- OUTPUTFILE LOADING-DATA                                    
003030     SELECT W37130                     ASSIGN TO W37131D1.                
003400                                                                          
003500 DATA DIVISION.                                                           
003600                                                                          
003700 FILE SECTION.                                                            
003800 FD  W37130                                                               
003810     RECORDING       F                                                    
003820     BLOCK CONTAINS  0.                                                   
003830                                                                          
003840*01  POST -COPY W37130 -PRE UT-   -L.                                     
003900     EJECT                                                                
003910                                                                          
004000 WORKING-STORAGE SECTION.                                                 
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'W3713100'.            
004430                                                                          
004440*    --- WORKFIELDS                                                       
004450 01  W-AREA-DATUM.                                                        
004491     03  W-DAREGDAT-KONV         PIC 9(8).                                
004492     03  W-TIKLOCK-KONV          PIC 9(9).                                
004493     03  W-TIKLOCK               PIC S9(9)   COMP-3.                      
004494     EJECT                                                                
004580                                                                          
004659*    ---KEYS FOR SELECTION OF ROWS IN TABLE BYLRAD                        
004665 01  WS-DAAAVV-ALFA.                                                      
004666     03  WS-DAAA                 PIC 9(4)    VALUE 2000.                  
004667     03  WS-DAVV                 PIC 9(2)    VALUE 00.                    
004668 01  WS-DAAAVV-KEY               PIC X(6).                                
004669 01  WS-RET                      PIC X(3)    VALUE 'RET'.                 
004670 01  WS-FAK                      PIC X(3)    VALUE 'FAK'.                 
004680 01  WS-KRE                      PIC X(3)    VALUE 'KRE'.                 
004686     EJECT                                                                
004687                                                                          
005400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005500 01  FILLER REDEFINES DAGENS-DATUM.                                       
005600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005900     EJECT                                                                
005910                                                                          
006000 01  DYNAMISKA-SUBPROGRAM.                                                
006100*                                                                         
006300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006310     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
006400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006501     EJECT                                                                
006502*                                                                         
006503*    --- PARAMETRAR TILL DATKORT                                          
006504*                                                                         
006505 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W37131'.              
006506                                                                          
006507 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
006508                                                                          
006509*01  -COPY WDATKORT                                                       
006510     EJECT                                                                
006530*                                                                         
006531*    --- PARAMETRAR TILL DATKONV                                          
006532*                                                                         
006534*01  -COPY WDATAREA                                                       
006535     EJECT                                                                
006536*                                                                         
006537*    --- PARAMETRAR TILL POSTSUM                                          
006538*                                                                         
006539*01  -COPY W0005   -PRE  POSTSUM-                                         
006540     EJECT                                                                
006541                                                                          
006542 01  W37130-AREA-START            PIC X(24)   VALUE                       
006543                                             'W371-AREA-START'.           
006544*01  -COPY W37130                  -PRE UT-                               
006545*                                                                         
006546     EJECT                                                                
006550*                                                                         
010602*        WORK-AREAS FOR DB2-SECTIONS                                      
010603*                                                                         
010604 01  FILLER                       PIC X(16)   VALUE 'DB2-WS     '.        
010605*01  -COPY BYLRAD     -PRE BYLRAD-                                        
010606     EJECT                                                                
010607                                                                          
010608 01  FILLER                       PIC X(16)   VALUE 'BYLRAD-AREA'.        
010609       EXEC SQL INCLUDE BYLRAD END-EXEC.                                  
010610                                                                          
010611 01  FILLER                       PIC X(16)   VALUE 'SQLCA-AREA'.         
010612       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
010613*                        **** STATUS-CODE FROM DB2                        
010614                                                                          
010615 01  FILLER                       PIC X(16)   VALUE 'SQLCODE-WS'.         
010616 01  DB2-WS.                                                              
010617   03  SQLCODE-WS                 PIC S9(3)   VALUE ZERO.                 
010619     88  ROW-FOUND                            VALUE +000.                 
010620     88  ROW-MISSING                          VALUE +100.                 
010622   03  GOOD-SQLCODES.                                                     
010623     05  GOOD-SQLCODE OCCURS 5                                            
010624         INDEXED BY SQLCODE-IX    PIC 999.                                
010625     EJECT                                                                
010630                                                                          
010717 PROCEDURE DIVISION.                                                      
010718 MAIN SECTION.                                                            
010720     ENTRY 'DLITCBL'.                                                     
010800                                                                          
011100     PERFORM A-INIT                                                       
011200                                                                          
011600     PERFORM B-EXECUTE                                                    
011900                                                                          
012400     PERFORM Z-FINISH                                                     
012500                                                                          
012600     MOVE ZERO TO RETURN-CODE                                             
012700     GOBACK                                                               
012800     .                                                                    
012900     EJECT                                                                
012910                                                                          
013000 A-INIT SECTION.                                                          
013010     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
013020     MOVE D-AAR                         TO DAGENS-DATUM-AAR               
013030     MOVE D-MAANAD                      TO DAGENS-DATUM-MAANAD            
013040     MOVE D-DAG                         TO DAGENS-DATUM-DAG               
013060                                                                          
013100     MOVE 'AAMMDD'                      TO DAT-KDDATFORM                  
013200     MOVE DAGENS-DATUM                  TO DAT-I-TIDATUM                  
013300                                                                          
013400     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
013500                         DAT-O-TIDATUM DAT-KDSVAR                         
013600                                                                          
013700     IF DAT-KDSVAR-OK                                                     
013800       ADD DAT-TIAA-VECKA               TO   WS-DAAA                      
013900       ADD DAT-TIVV                     TO   WS-DAVV                      
013920       MOVE WS-DAAAVV-ALFA              TO   WS-DAAAVV-KEY                
013930       DISPLAY 'DATE=' WS-DAAAVV-KEY                                      
014000     ELSE                                                                 
014010       DISPLAY 'WRONG DATE'                                               
014020       CALL FELLOG                                                        
014030     END-IF                                                               
014031                                                                          
014040     OPEN OUTPUT W37130                                                   
014050     MOVE IDPGM                         TO POSTSUM-PROGNAMN               
014100     .                                                                    
014200     EJECT                                                                
014210                                                                          
014985 B-EXECUTE SECTION.                                                       
014989     PERFORM DB2-OPEN-CRS-BYLRAD                                          
014991                                                                          
014992     PERFORM DB2-FETCH-CRS-BYLRAD                                         
014993     PERFORM UNTIL ROW-MISSING                                            
014994       MOVE BYLRAD-DAAAVV        TO UT-ACK-DAAAVV                         
014997       MOVE BYLRAD-IDARTNR       TO UT-ACK-IDARTNR                        
014999       MOVE BYLRAD-IDPTYP        TO UT-ACK-IDPTYP                         
015000       MOVE BYLRAD-KVANTAL       TO UT-ACK-KVANTAL                        
015001       PERFORM S11-WRITE-W37130                                           
015002                                                                          
015003       PERFORM DB2-FETCH-CRS-BYLRAD                                       
015004     END-PERFORM                                                          
015005                                                                          
015010     PERFORM DB2-CLOSE-CRS-BYLRAD                                         
015100     .                                                                    
015101     EJECT                                                                
015102                                                                          
015103 Z-FINISH SECTION.                                                        
015104     CLOSE W37130                                                         
015105     MOVE 'S' TO POSTSUM-OPKOD                                            
015106     CALL POSTSUM USING POSTSUM-PARM                                      
015107     .                                                                    
015108     EJECT                                                                
015109                                                                          
015110 S11-WRITE-W37130 SECTION.                                                
015111     WRITE UT-POST FROM UT-ACK-W37130                                     
015112                                                                          
015113     MOVE 'ACK'      TO POSTSUM-TRANSTYP                                  
015114     MOVE 'W37130 '  TO POSTSUM-FDNAMN                                    
015115     MOVE 'W37131D1' TO POSTSUM-DDNAMN2                                   
015116     CALL POSTSUM USING POSTSUM-PARM                                      
015117     .                                                                    
015118     EJECT                                                                
015119                                                                          
015120* --- DB2 SECTIONS  ---                                                   
015130*                                                                         
015140 DB2-OPEN-CRS-BYLRAD SECTION.                                             
015150                                                                          
015160     EXEC SQL DECLARE BYLRAD-CRS CURSOR FOR                               
015170     SELECT   IDARTNR,                                                    
015180              DAAAVV,                                                     
015181              IDPTYP,                                                     
015190              SUM(KVANTAL)                                                
015200                                                                          
015300     FROM     BYLRAD                                                      
015400                                                                          
015600     WHERE    DAAAVV  = :WS-DAAAVV-KEY                                    
015700       AND   (IDPTYP  = :WS-RET                                           
015800       OR     IDPTYP  = :WS-FAK                                           
015900       OR     IDPTYP  = :WS-KRE)                                          
016000                                                                          
016100     GROUP BY IDARTNR,                                                    
016200              DAAAVV,                                                     
016210              IDPTYP                                                      
016300     ORDER BY IDARTNR,                                                    
016400              DAAAVV,                                                     
016410              IDPTYP                                                      
016411                                                                          
016420     FOR FETCH ONLY                                                       
016500     END-EXEC                                                             
016600                                                                          
016700     MOVE 000            TO GOOD-SQLCODES                                 
016800     EXEC SQL OPEN BYLRAD-CRS END-EXEC                                    
016900     MOVE SQLCODE        TO SQLCODE-WS                                    
017000     PERFORM DB2-STATUS-CHECK                                             
017100     .                                                                    
017200     EJECT                                                                
017210                                                                          
017300 DB2-FETCH-CRS-BYLRAD SECTION.                                            
017400                                                                          
017500     EXEC SQL FETCH BYLRAD-CRS INTO                                       
017600            :BYLRAD-IDARTNR,                                              
017700            :BYLRAD-DAAAVV,                                               
017710            :BYLRAD-IDPTYP,                                               
017800            :BYLRAD-KVANTAL                                               
017900     END-EXEC                                                             
017910                                                                          
017920     MOVE 000100         TO GOOD-SQLCODES                                 
018000     MOVE SQLCODE        TO SQLCODE-WS                                    
018200     PERFORM DB2-STATUS-CHECK                                             
018300     .                                                                    
018400     EJECT                                                                
019011                                                                          
019012 DB2-CLOSE-CRS-BYLRAD SECTION.                                            
019013                                                                          
019014     EXEC SQL CLOSE BYLRAD-CRS                                            
019015     END-EXEC                                                             
019016     .                                                                    
019017     EJECT                                                                
019018                                                                          
019020 DB2-STATUS-CHECK SECTION.                                                
019100     SET SQLCODE-IX         TO 1                                          
019200     SEARCH GOOD-SQLCODE AT END CALL FELLOG                               
019300        WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
019400           CONTINUE                                                       
019500     END-SEARCH                                                           
019600     .                                                                    
