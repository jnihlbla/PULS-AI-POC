010000*COMPOPT VRTEREUS=YES                                                     
020000 ID DIVISION.                                                             
030000 PROGRAM-ID.   W010WDI1.                                                  
040000 AUTHOR.         LASSI OLGRENER.                                          
050000 DATE-WRITTEN.   14-09-12.                                                
060000 DATE-COMPILED.                                                           
070000                                                                          
080000*   RENSAR ÄLDRE ÄN 6 MÅNADER GAMLA SEGMENT                               
090000*                                                                         
100000 DATA DIVISION.                                                           
110000                                                                          
120000 WORKING-STORAGE SECTION.                                                 
140000 77 IDPGM     PIC X(8) VALUE 'W010WDI1'.                                  
140100 77 FELTEXT           PIC X(32) VALUE SPACE.                              
140200 01 SW-FIRST          PIC X     VALUE 'J'.                                
140300*                                                                         
140400 01  DYNAMISKA-SUBPROGRAM.                                                
140500     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
140600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
140700                                                                          
140800*    --- PARAMETRAR TILL WDAGKONV:                                        
140900 01  FILLER                      PIC X(16) VALUE 'WDAGAREA'.              
141000*01  -COPY WDAGAREA                                                       
141100     EJECT                                                                
141200 LINKAGE SECTION.                                                         
141300 01  SEGNAMN          PIC X(8).                                           
141400 01 OLD.                                                                  
141500    03 -COPY  WDI101                                                      
141600     EJECT                                                                
141700 PROCEDURE DIVISION USING SEGNAMN OLD.                                    
141800                                                                          
141900     MOVE ZERO TO RETURN-CODE                                             
142000     IF SW-FIRST = 'J'                                                    
142100       MOVE 003                  TO DAG-KDCALL                            
142200       ACCEPT DAG-TIAAMMDD-TOM   FROM DATE                                
142300       MOVE 365                  TO DAG-KVKALDAG                          
142400                                                                          
142500       CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA                      
142600                           DAG-KDSVAR                                     
142700       IF DAG-KDSVAR = 'F'                                                
142800         MOVE 'FEL FRÅN DAGKONV ' TO FELTEXT                              
142900         CALL FELLOG                                                      
143000       END-IF                                                             
143100       MOVE 'N' TO SW-FIRST                                               
143200       DISPLAY 'RENSA ÄLDRE ÄN = ' DAG-TIAAMMDD-FOM                       
143300     END-IF                                                               
143400     IF SEGNAMN =  'WDI101'                                               
143500       IF PIED-TIREGDAT < DAG-TIAAMMDD-FOM                                
143600         MOVE +12 TO RETURN-CODE                                          
143700       END-IF                                                             
143800     END-IF                                                               
400000     GOBACK                                                               
410000      .                                                                   
