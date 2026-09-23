000100*COMPOPT VRTEREUS=YES                                                     
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.   W010WDI2.                                                  
000400 AUTHOR.         LASSI OLGRENER.                                          
000500 DATE-WRITTEN.   15-01-30.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*   RENSAR ÄLDRE ÄN 1 MÅNAD GAMLA SEGMENT                                 
000900*                                                                         
001000 DATA DIVISION.                                                           
001100                                                                          
001200 WORKING-STORAGE SECTION.                                                 
001300                                                                          
001400 77 IDPGM             PIC X(8)  VALUE 'W010WDI2'.                         
001410 77 FELTEXT           PIC X(32) VALUE SPACE.                              
001500 01 SW-FIRST          PIC X     VALUE 'J'.                                
001506*                                                                         
001510 01  DYNAMISKA-SUBPROGRAM.                                                
001560     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
001561     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
001562                                                                          
001570*    --- PARAMETRAR TILL WDAGKONV:                                        
001580 01  FILLER                      PIC X(16) VALUE 'WDAGAREA'.              
001590*01  -COPY WDAGAREA                                                       
001591     EJECT                                                                
175000 LINKAGE SECTION.                                                         
176000 01  SEGNAMN          PIC X(8).                                           
177000 01 OLD.                                                                  
178000    03 -COPY  WDI201                                                      
179000     EJECT                                                                
180000 PROCEDURE DIVISION USING SEGNAMN OLD.                                    
190000                                                                          
200000     MOVE ZERO TO RETURN-CODE                                             
210000     IF SW-FIRST = 'J'                                                    
210100       MOVE 003                  TO DAG-KDCALL                            
210110       ACCEPT DAG-TIAAMMDD-TOM   FROM DATE                                
210200       MOVE 30                   TO DAG-KVKALDAG                          
210400                                                                          
210500       CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA                      
210600                           DAG-KDSVAR                                     
210800       IF DAG-KDSVAR = 'F'                                                
210900         MOVE 'FEL FRÅN DAGKONV ' TO FELTEXT                              
211000         CALL FELLOG                                                      
211700       END-IF                                                             
310000       MOVE 'N' TO SW-FIRST                                               
320001       DISPLAY 'RENSA ÄLDRE ÄN = ' DAG-TIAAMMDD-FOM                       
330000     END-IF                                                               
340000     IF SEGNAMN =  'WDI201'                                               
350001       IF TAKF-TIREGDAT < DAG-TIAAMMDD-FOM                                
370000         MOVE +8 TO RETURN-CODE                                           
380000       END-IF                                                             
390000     END-IF                                                               
400000     GOBACK                                                               
410000      .                                                                   
