000101*COMPOPT VRTEREUS=YES                                                     
000201 ID DIVISION.                                                             
000301 PROGRAM-ID.   W010WDE7.                                                  
000401 AUTHOR.         LASSI OLGRENER.                                          
000501 DATE-WRITTEN.   17-01-31.                                                
000601 DATE-COMPILED.                                                           
000701                                                                          
000801*   RENSAR ÄLDRE ÄN 1 MÅNAD GAMLA E711 (+E721) SEGMENT                    
000901*   WDE701 RENSAS ALDRIG. DE MOTSVARAR STRECKKODS FLAGGORNA               
001001*   SOM FINNS UPPSATTA I LAGREN PÅ VO-STATIONER FÖR SAMLINGSKOLLIN        
001101*                                                                         
001201 DATA DIVISION.                                                           
001301                                                                          
001401 WORKING-STORAGE SECTION.                                                 
001501                                                                          
001601 77 IDPGM             PIC X(8)  VALUE 'W010WDE7'.                         
001701 77 FELTEXT           PIC X(32) VALUE SPACE.                              
001801 01 RENSA             PIC X     VALUE 'N'.                                
001802 01 SW-FIRST          PIC X     VALUE 'J'.                                
001803 01 DLET-E711         PIC 9(5) VALUE 0.                                   
001804 01 DLET-E721         PIC 9(5) VALUE 0.                                   
001901*                                                                         
002001 01  DYNAMISKA-SUBPROGRAM.                                                
002101     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
002201     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
002301                                                                          
002401*    --- PARAMETRAR TILL WDAGKONV:                                        
002501 01  FILLER                      PIC X(16) VALUE 'WDAGAREA'.              
002601*01  -COPY WDAGAREA                                                       
002701     EJECT                                                                
002801 LINKAGE SECTION.                                                         
002901 01  SEGNAMN          PIC X(8).                                           
003001 01 OLD.                                                                  
003101    03 -COPY  WDE711                                                      
003201     EJECT                                                                
004001 PROCEDURE DIVISION USING SEGNAMN OLD.                                    
005001                                                                          
006001     MOVE ZERO TO RETURN-CODE                                             
007001     IF SW-FIRST = 'J'                                                    
008001       MOVE 003                  TO DAG-KDCALL                            
009001       ACCEPT DAG-TIAAMMDD-TOM   FROM DATE                                
010001       MOVE 14                   TO DAG-KVKALDAG                          
020001                                                                          
030001       CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA                      
040001                           DAG-KDSVAR                                     
050001       IF DAG-KDSVAR = 'F'                                                
060001         MOVE 'FEL FRÅN DAGKONV ' TO FELTEXT                              
070001         CALL FELLOG                                                      
080001       END-IF                                                             
090001       MOVE 'N' TO SW-FIRST                                               
100001       DISPLAY 'RENSA ÄLDRE ÄN = ' DAG-TIAAMMDD-FOM                       
110001     END-IF                                                               
120001     IF SEGNAMN =  'WDE711'                                               
130001       IF SKLI-TIREGDAT < DAG-TIAAMMDD-FOM AND                            
130002          SKLI-KDSTASKLI = 'S'                                            
140001         MOVE +8 TO RETURN-CODE                                           
140002         MOVE 'J' TO RENSA                                                
140003         ADD 1 TO DLET-E711                                               
150001       ELSE                                                               
150002         MOVE 'N' TO RENSA                                                
150003       END-IF                                                             
160001     END-IF                                                               
160002     IF SEGNAMN =  'WDE721'                                               
160003       IF RENSA = 'J'                                                     
160004         MOVE +8 TO RETURN-CODE                                           
160006         ADD 1 TO DLET-E721                                               
160007       END-IF                                                             
160008     END-IF                                                               
160009     IF SEGNAMN =  '        '                                             
160010       DISPLAY 'ANT DLET E711 = ' DLET-E711                               
160011       DISPLAY 'ANT DLET E721 = ' DLET-E721                               
160020     END-IF                                                               
170001     GOBACK                                                               
180001      .                                                                   
