000100 PROCESS DYNAM                                                            
000200*        - THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM             
000400*                                                                         
001150 ID DIVISION.                                                             
001200 PROGRAM-ID.     WF101100.                                                
001300 AUTHOR.         BO HAMMARIN.                                             
001400 DATE-WRITTEN.   DEC 2001.                                                
001500 DATE-COMPILED.                                                           
001700                                                                          
001800*   PGM                                                                   
001900*   -SELECTS ROWS FROM PERMANENT VAT-TABLE                                
001910*   -CREATES FILE CONTAINING                                              
002000*    1 NEW VAT DATA                                                       
002401*    2 DELETED VAT DATA                                                   
002402*    3 CHANGED VAT DATA                                                   
002410*                                                                         
002500                                                                          
002700 ENVIRONMENT DIVISION.                                                    
002800                                                                          
002900 INPUT-OUTPUT SECTION.                                                    
003011 FILE-CONTROL.                                                            
003020*          --- DISTRIBUTION-DATA                                          
003030     SELECT WF1017                     ASSIGN TO WF1011D1.                
003400                                                                          
003500 DATA DIVISION.                                                           
003600                                                                          
003700 FILE SECTION.                                                            
003800 FD  WF1017                                                               
003810     RECORDING       F                                                    
003820     BLOCK CONTAINS  0.                                                   
003830                                                                          
003840*01  POST -COPY WF10M17   -PRE UT-   -L.                                  
003900     EJECT                                                                
003910                                                                          
004000 WORKING-STORAGE SECTION.                                                 
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'WF101100'.            
004430                                                                          
004659*    ---KEYS FOR SELECTION OF ROWS IN TABLE T01ONUM                       
004668 01  WS-IDLEGSEL                 PIC X(4).                                
004670 01  WS-IDLAND                   PIC X(2).                                
004680 01  WS-KDVAT                    PIC X(2).                                
004686     EJECT                                                                
004687                                                                          
006000 01  DYNAMISKA-SUBPROGRAM.                                                
006100*                                                                         
006300     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
006400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006501     EJECT                                                                
006502                                                                          
006503 01  WS-DATUM.                                                            
006504     03  WS-DAGENS-DATUM         PIC X(8).                                
006505     EJECT                                                                
006507                                                                          
006508*    --- PARAMETRAR TILL ABEND                                            
006509*                                                                         
006510 01  RKOD-ABEND-DB2              PIC S9(4)   VALUE +998 COMP SYNC.        
006511     EJECT                                                                
006512                                                                          
006513*    --- PARAMETRAR TILL DATKORT                                          
006514*                                                                         
006515 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
006516                                                                          
006517*01  -COPY WDATKORT                                                       
006540     EJECT                                                                
006541                                                                          
006542 01  WF1017-AREA-START           PIC X(24)   VALUE                        
006543                                             'WF10-AREA-START'.           
006544*01  -COPY WF10M17        -PRE UT-                                        
006546     EJECT                                                                
006550*                                                                         
010602*        WORK-AREAS FOR DB2-SECTIONS                                      
010603*                                                                         
010604 01  FILLER                       PIC X(16)   VALUE 'DB2-WS1    '.        
010605*01  -COPY T01VAT         -PRE VAT-                                       
010610     EJECT                                                                
010611                                                                          
010612 01  FILLER                       PIC X(16)   VALUE 'VAT-AREA'.           
010613       EXEC SQL INCLUDE T01VAT   END-EXEC.                                
010615                                                                          
010618     EJECT                                                                
010619                                                                          
010620 01  FILLER                       PIC X(16)   VALUE 'SQLCA-AREA'.         
010621       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
010622*                        **** STATUS-CODE FROM DB2                        
010623                                                                          
010624 01  FILLER                       PIC X(16)   VALUE 'SQLCODE-WS'.         
010625 01  DB2-WS.                                                              
010626   03  SQLCODE-WS                 PIC S9(3)   VALUE ZERO.                 
010627     88  ROW-FOUND                            VALUE +000.                 
010628     88  ROW-MISSING                          VALUE +100.                 
010629   03  GOOD-SQLCODES.                                                     
010630     05  GOOD-SQLCODE OCCURS 5                                            
010631         INDEXED BY SQLCODE-IX    PIC 999.                                
010632     EJECT                                                                
010640                                                                          
010717 PROCEDURE DIVISION.                                                      
010718                                                                          
010719 MAIN SECTION.                                                            
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
014040     OPEN OUTPUT WF1017                                                   
014041                                                                          
014042     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
014043     MOVE 20           TO WS-DAGENS-DATUM(1:2)                            
014044     MOVE D-AAR        TO WS-DAGENS-DATUM(3:2)                            
014045     MOVE D-MAANAD     TO WS-DAGENS-DATUM(5:2)                            
014046     MOVE D-DAG        TO WS-DAGENS-DATUM(7:2)                            
014100     .                                                                    
014200     EJECT                                                                
014210                                                                          
014985 B-EXECUTE SECTION.                                                       
014989     PERFORM DB2-OPEN-CRS-VAT                                             
014994     PERFORM DB2-FETCH-CRS-VAT                                            
014995     PERFORM UNTIL ROW-MISSING                                            
014996       PERFORM BA-BUILD-OUTPUT-VAT                                        
015004       PERFORM S11-WRITE-WF1017                                           
015006       PERFORM DB2-FETCH-CRS-VAT                                          
015007     END-PERFORM                                                          
015009     PERFORM DB2-CLOSE-CRS-VAT                                            
015103     .                                                                    
015104     EJECT                                                                
015105                                                                          
015106 BA-BUILD-OUTPUT-VAT SECTION.                                             
015109     MOVE VAT-IDLEGSEL           TO UT-IDLEGSEL                           
015111     MOVE VAT-IDLANDX2           TO UT-IDLAND                             
015112     MOVE VAT-KDVAT              TO UT-KDVAT                              
015115     MOVE VAT-REVAT              TO UT-REVAT                              
015116     MOVE VAT-BEVAT              TO UT-BEVAT                              
015117     MOVE VAT-DAREGDAT           TO UT-DAREGDAT                           
015118     MOVE VAT-DAUPPDAT           TO UT-DAUPPDAT                           
015119     MOVE VAT-DADELDAT           TO UT-DADELDAT                           
015144     .                                                                    
015195     EJECT                                                                
015196                                                                          
015197 Z-FINISH SECTION.                                                        
015198     CLOSE WF1017                                                         
015202     .                                                                    
015203     EJECT                                                                
015204                                                                          
015205 S11-WRITE-WF1017 SECTION.                                                
015206     WRITE UT-POST   FROM UT-WF10M17                                      
015212     .                                                                    
015213     EJECT                                                                
015214                                                                          
015215* --- DB2 SECTIONS  ---                                                   
015216*                                                                         
015217 DB2-OPEN-CRS-VAT SECTION.                                                
015219     EXEC SQL DECLARE VAT-CRS CURSOR FOR                                  
015220     SELECT   T01VAT.IDLEGSEL,                                            
015221              T01VAT.IDLANDX2,                                            
015222              T01VAT.KDVAT,                                               
015223              T01VAT.REVAT,                                               
015224              T01VAT.BEVAT,                                               
015225              T01VAT.DAREGDAT,                                            
015226              T01VAT.DAUPPDAT,                                            
015227              T01VAT.DADELDAT                                             
015230                                                                          
015300     FROM     T01VAT                                                      
015400                                                                          
015600     WHERE    T01VAT.DAREGDAT = :WS-DAGENS-DATUM OR                       
015710              T01VAT.DAUPPDAT = :WS-DAGENS-DATUM OR                       
015720              T01VAT.DADELDAT = :WS-DAGENS-DATUM                          
015800                                                                          
016300     ORDER BY T01VAT.IDLEGSEL,                                            
016410              T01VAT.IDLANDX2,                                            
016411              T01VAT.KDVAT                                                
016412                                                                          
016420     FOR FETCH ONLY                                                       
016500     END-EXEC                                                             
016600                                                                          
016700     MOVE 000            TO GOOD-SQLCODES                                 
016800     EXEC SQL OPEN VAT-CRS                                                
016810     END-EXEC                                                             
016900     MOVE SQLCODE        TO SQLCODE-WS                                    
017000     PERFORM DB2-STATUS-CHECK                                             
017100     .                                                                    
017452     EJECT                                                                
017453                                                                          
017454 DB2-FETCH-CRS-VAT SECTION.                                               
017500     EXEC SQL FETCH VAT-CRS INTO                                          
017600            :VAT-IDLEGSEL,                                                
017701            :VAT-IDLANDX2,                                                
017702            :VAT-KDVAT,                                                   
017703            :VAT-REVAT,                                                   
017704            :VAT-BEVAT,                                                   
017800            :VAT-DAREGDAT,                                                
017810            :VAT-DAUPPDAT,                                                
017820            :VAT-DADELDAT                                                 
017900     END-EXEC                                                             
017910                                                                          
017920     MOVE 000100         TO GOOD-SQLCODES                                 
018000     MOVE SQLCODE        TO SQLCODE-WS                                    
018200     PERFORM DB2-STATUS-CHECK                                             
018300     .                                                                    
019088     EJECT                                                                
019089                                                                          
019090 DB2-CLOSE-CRS-VAT SECTION.                                               
019092     EXEC SQL CLOSE VAT-CRS                                               
019093     END-EXEC                                                             
019094     .                                                                    
019116     EJECT                                                                
019117                                                                          
019118 DB2-STATUS-CHECK SECTION.                                                
019120     SET SQLCODE-IX         TO 1                                          
019200     SEARCH GOOD-SQLCODE AT END                                           
019210           CALL ABEND USING RKOD-ABEND-DB2                                
019300        WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
019400           CONTINUE                                                       
019500     END-SEARCH                                                           
019600     .                                                                    
