040093 PROCESS DYNAM                                                            
115071 ID DIVISION.                                                             
120099 PROGRAM-ID.     W3713200.                                                
130070 AUTHOR.         BO HAMMARIN.                                             
140099 DATE-WRITTEN.   JUNI 2000.                                               
150000 DATE-COMPILED.                                                           
170000                                                                          
180000*    FUNKTION:                                                            
190099*        PROGRAM CLEANS OUT LINE-TABLE FOR EXCHANGE-POINTS                
230092*                                                                         
240099*        PROGRAM UPDATES BYLRAD (DB2-TABLE)                               
241092*                                                                         
250000                                                                          
270000 ENVIRONMENT DIVISION.                                                    
280070                                                                          
290000 INPUT-OUTPUT SECTION.                                                    
300000                                                                          
310000 FILE-CONTROL.                                                            
340070                                                                          
350000 DATA DIVISION.                                                           
360070                                                                          
370000 FILE SECTION.                                                            
390000     EJECT                                                                
391070                                                                          
400000 WORKING-STORAGE SECTION.                                                 
411002*    -- CHECKED BY WY2000                                                 
420099 77  IDPGM                       PIC X(8)    VALUE 'W3713200'.            
443000                                                                          
444099*    --- WORKFIELDS                                                       
445003 01  W-AREA-DATUM.                                                        
449106     03  W-DAREGDAT-KONV         PIC 9(8).                                
449203     03  W-TIKLOCK-KONV          PIC 9(9).                                
449341     03  W-TIKLOCK               PIC S9(9)   COMP-3.                      
449470     EJECT                                                                
458000                                                                          
465999*    ---KEY FOR DELETION OF ROWS IN TABLE BYLRAD                          
466599 01  WS-DAAAVV-ALFA.                                                      
466699     03  WS-DAAA                 PIC 9(4)  VALUE 2000.                    
466799     03  WS-DAVV                 PIC X(2)  VALUE '01'.                    
466899 01  WS-DAAAVV-KEY               PIC X(6).                                
468671     EJECT                                                                
468771                                                                          
469099 01  ERRORTEXT.                                                           
470099     03  FILLER                  PIC X(16)   VALUE 'ERRORTEXT'.           
480099     03  ERRORTEXT-STR           PIC X(72)   VALUE SPACE.                 
531070                                                                          
540000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
550000 01  FILLER REDEFINES DAGENS-DATUM.                                       
560000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
570000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
580000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
590000     EJECT                                                                
591070                                                                          
600000 01  DYNAMISKA-SUBPROGRAM.                                                
610000*                                                                         
620000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
630000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
631099     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
640099     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
650100     EJECT                                                                
650299*                                                                         
650399*    --- PARAMETRAR TILL DATKORT                                          
650499*                                                                         
650599 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W37132'.              
650699                                                                          
650799 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
650899                                                                          
650999*01  -COPY WDATKORT                                                       
651099     EJECT                                                                
653099*                                                                         
653199*    --- PARAMETRAR TILL DATKONV                                          
653299*                                                                         
653499*01  -COPY WDATAREA                                                       
653599     EJECT                                                                
653899*                                                                         
060299*        WORK-AREAS FOR DB2-SECTIONS                                      
060392*                                                                         
060492 01  FILLER                       PIC X(16)   VALUE 'DB2-WS     '.        
060592*01  -COPY BYLRAD     -PRE BYLRAD-                                        
060692     EJECT                                                                
060792                                                                          
060892 01  FILLER                       PIC X(16) VALUE 'BYLRAD-AREA'.          
060992       EXEC SQL INCLUDE BYLRAD END-EXEC.                                  
061092                                                                          
061192 01  FILLER                       PIC X(16) VALUE 'SQLCA-AREA'.           
061292       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
061399*                        **** STATUS-CODE FROM DB2                        
061492                                                                          
061592 01  FILLER                       PIC X(16) VALUE 'SQLCODE-WS'.           
061692 01  DB2-WS.                                                              
061799   03  SQLCODE-WS                 PIC S9(3) VALUE ZERO.                   
061999     88  ROW-FOUND                          VALUE +000.                   
062099     88  ROW-MISSING                        VALUE +100.                   
062299   03  GOOD-SQLCODESKODER.                                                
062399     05  GOOD-SQLCODE OCCURS 5                                            
062499         INDEXED BY SQLCODE-IX    PIC 999.                                
062592     EJECT                                                                
063086                                                                          
071771 PROCEDURE DIVISION.                                                      
071800 MAIN SECTION.                                                            
072071     ENTRY 'DLITCBL'.                                                     
080000                                                                          
110000     PERFORM A-INIT                                                       
120004                                                                          
160099     PERFORM DB2-DELETE-BYLRAD                                            
190000                                                                          
240000     PERFORM Z-FINIT                                                      
250000                                                                          
260000     MOVE ZERO TO RETURN-CODE                                             
270000     GOBACK                                                               
280000     .                                                                    
290000     EJECT                                                                
291070                                                                          
300000 A-INIT SECTION.                                                          
301099     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
302099     MOVE D-AAR                         TO DAGENS-DATUM-AAR               
303099     MOVE D-MAANAD                      TO DAGENS-DATUM-MAANAD            
304099     MOVE D-DAG                         TO DAGENS-DATUM-DAG               
306099                                                                          
310099     MOVE 'AAMMDD'                      TO DAT-KDDATFORM                  
320099     MOVE DAGENS-DATUM                  TO DAT-I-TIDATUM                  
330099                                                                          
340099     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
350099                         DAT-O-TIDATUM DAT-KDSVAR                         
360099                                                                          
370099     IF DAT-KDSVAR-OK                                                     
380099       ADD DAT-TIAA-VECKA               TO   WS-DAAA                      
390099       SUBTRACT 2                       FROM WS-DAAA                      
392099       MOVE WS-DAAAVV-ALFA              TO   WS-DAAAVV-KEY                
400099     ELSE                                                                 
401099       DISPLAY 'WRONG DATE'                                               
402099       CALL FELLOG                                                        
403099     END-IF                                                               
410000     .                                                                    
420000     EJECT                                                                
421070                                                                          
498568 Z-FINIT SECTION.                                                         
498899     DISPLAY 'DELETED FROM TABLE BYLRAD: YYYYWW < ' WS-DAAAVV-ALFA        
500000     .                                                                    
510100     EJECT                                                                
510299                                                                          
511399* --- DB2 SECTIONS  ---                                                   
511492*                                                                         
891099 DB2-DELETE-BYLRAD SECTION.                                               
893099                                                                          
894099     EXEC SQL                                                             
895099       DELETE FROM                                                        
895199          BYLRAD                                                          
895299                                                                          
896099       WHERE                                                              
897099          DAAAVV         < :WS-DAAAVV-KEY                                 
899099     END-EXEC.                                                            
899199                                                                          
899299     MOVE ZERO    TO GOOD-SQLCODE (1)                                     
899399     MOVE +100    TO GOOD-SQLCODE (2)                                     
899499     MOVE SQLCODE TO SQLCODE-WS                                           
899599     STRING 'DELETE AGAINST TABLE BYLRAD'                                 
899699            DELIMITED BY SIZE INTO ERRORTEXT                              
899799     PERFORM DB2-STATUS-CHECK                                             
901099     .                                                                    
901199                                                                          
902099 DB2-STATUS-CHECK SECTION.                                                
910092     SET SQLCODE-IX         TO 1                                          
920099     SEARCH GOOD-SQLCODE AT END CALL FELLOG                               
930099        WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
940092           CONTINUE                                                       
950092     END-SEARCH                                                           
960092     .                                                                    
