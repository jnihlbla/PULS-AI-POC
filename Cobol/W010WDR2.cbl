000101*COMPOPT VRTEREUS=YES                                                     
000201 ID DIVISION.                                                             
000301 PROGRAM-ID.   W010WDR2.                                                  
000401 AUTHOR.         LASSI OLGRENER.                                          
000501 DATE-WRITTEN.   19-09-23.                                                
000601 DATE-COMPILED.                                                           
000701                                                                          
000801*   RENSAR ÄLDRE ÄN 2 ÅR WDGX2264/WDGX2266 SEGMENT                        
000901*                                                                         
001001 DATA DIVISION.                                                           
001101                                                                          
001201 WORKING-STORAGE SECTION.                                                 
001301                                                                          
001401 77 IDPGM             PIC X(8)  VALUE 'W010WDR2'.                         
001501 77 FELTEXT           PIC X(32) VALUE SPACE.                              
001601 01 RENSA             PIC X     VALUE 'N'.                                
001701 01 SW-FIRST          PIC X     VALUE 'J'.                                
001801 01 DLET-X2264        PIC 9(5) VALUE 0.                                   
001901 01 DLET-X2266        PIC 9(5) VALUE 0.                                   
002101*                                                                         
002201 01  DYNAMISKA-SUBPROGRAM.                                                
002301     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
002302     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
002401     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
002501                                                                          
002601*    --- PARAMETRAR TILL WDAGKONV:                                        
002701 01  FILLER                      PIC X(16) VALUE 'WDAGAREA'.              
002801*01  -COPY WDAGAREA                                                       
002802*    --- PARAMETRAR TILL WDATKONV:                                        
002803 01  FILLER                      PIC X(16) VALUE 'WDATAREA'.              
002804*01  -COPY WDATAREA                                                       
002901     EJECT                                                                
003001 LINKAGE SECTION.                                                         
003101 01  SEGNAMN          PIC X(8).                                           
003201 01 OLD.                                                                  
003301    03 -COPY  WDGX2264                                                    
003401     EJECT                                                                
003501 PROCEDURE DIVISION USING SEGNAMN OLD.                                    
003601                                                                          
003701     MOVE ZERO TO RETURN-CODE                                             
003801     IF SW-FIRST = 'J'                                                    
003802       PERFORM A-CALC-2YEARS-BACK                                         
030001       MOVE 'N' TO SW-FIRST                                               
040001       DISPLAY 'RENSA ÄLDRE ÄN = ' DAT-TIAAVVD                            
050001     END-IF                                                               
060001     IF SEGNAMN =  'WDGX2264'                                             
070001       IF 2264-TISOP < DAT-TIAAVVD                                        
080001         MOVE +8 TO RETURN-CODE                                           
090001         MOVE 'J' TO RENSA                                                
100001         ADD 1 TO DLET-X2264                                              
110001       ELSE                                                               
120001         MOVE 'N' TO RENSA                                                
130001       END-IF                                                             
140001     END-IF                                                               
150001     IF SEGNAMN =  'WDGX2266'                                             
151001       IF RENSA = 'J'                                                     
152001         MOVE +8 TO RETURN-CODE                                           
153001         ADD 1 TO DLET-X2266                                              
154001       END-IF                                                             
155001     END-IF                                                               
156001     IF SEGNAMN =  '        '                                             
157001       DISPLAY 'ANT DLET 2264 = ' DLET-X2264                              
158001       DISPLAY 'ANT DLET 2266 = ' DLET-X2266                              
159001     END-IF                                                               
160001     GOBACK                                                               
170001      .                                                                   
170002 A-CALC-2YEARS-BACK SECTION.                                              
170003     MOVE 003                  TO DAG-KDCALL                              
170004     ACCEPT DAG-TIAAMMDD-TOM   FROM DATE                                  
170005     MOVE 730                  TO DAG-KVKALDAG                            
170006                                                                          
170007     CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA                        
170008                         DAG-KDSVAR                                       
170009     IF DAG-KDSVAR = 'F'                                                  
170010       MOVE 'FEL FRÅN DAGKONV ' TO FELTEXT                                
170020       CALL FELLOG                                                        
170030     ELSE                                                                 
170040       MOVE "AAMMDD"         TO DAT-KDDATFORM                             
170050       MOVE DAG-TIAAMMDD-FOM TO DAT-I-TIDATUM                             
170060       CALL WDATKONV USING                                                
170070            DAT-KDDATFORM,                                                
170080            DAT-I-TIDATUM,                                                
170090            DAT-O-TIDATUM,                                                
170100            DAT-KDSVAR                                                    
170200       IF DAT-KDSVAR-FEL                                                  
170300         MOVE 'FEL FRÅN DATKONV' TO FELTEXT                               
170400         CALL FELLOG                                                      
170500       END-IF                                                             
170600     END-IF                                                               
170700      .                                                                   
