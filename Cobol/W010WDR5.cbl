000010*COMPOPT INCLMOD=VRTEUOPT                                                 
000020 ID DIVISION.                                                             
000030 PROGRAM-ID.     W010WDR5.                                                
000040 AUTHOR.         LASSI.                                                   
000050 DATE-WRITTEN.   SEP  20.                                                 
000060 DATE-COMPILED.                                                           
000070                                                                          
000080*    RENSNINGAR VIA EXIT I W010V9.                                        
000090                                                                          
000100 DATA DIVISION.                                                           
000200                                                                          
000300 WORKING-STORAGE SECTION.                                                 
000400                                                                          
000500*    -- CHECKED BY WY2000                                                 
000600 77  IDPGM                     PIC X(8)    VALUE 'W010WDR5'.              
000611 77  WS-FIRST                  PIC X       VALUE 'J'.                     
000612 77  WS-3MONTH-OLD             PIC 9(6).                                  
000613 77  WS-6MONTH-OLD             PIC 9(6).                                  
000614 77  FELTEXT                   PIC X(32).                                 
000615 77  JA                        PIC X       VALUE 'J'.                     
000616 77  NEJ                       PIC X       VALUE 'N'.                     
000617*                                                                         
000618 01  DYNAMISKA-SUBPROGRAM.                                                
000619     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
000620     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000621                                                                          
000622*    --- PARAMETRAR TILL WDAGKONV:                                        
000623 01  FILLER                      PIC X(16) VALUE 'WDAGAREA'.              
000624*01  -COPY WDAGAREA                                                       
000625     EJECT                                                                
000626 LINKAGE SECTION.                                                         
000627 01  SEGNAMN         PIC X(8).                                            
000628 01  OLD.                                                                 
000629    03 -COPY WDGX4564                                                     
000630    03 -COPY WDGX2510 -RED 4564-WDGX4564-CTX                              
000631     EJECT                                                                
000632 PROCEDURE DIVISION USING SEGNAMN OLD.                                    
000633 MAIN SECTION.                                                            
000640                                                                          
000650     MOVE 0                   TO RETURN-CODE                              
000660     IF WS-FIRST = JA                                                     
000670       MOVE 003                  TO DAG-KDCALL                            
000680       ACCEPT DAG-TIAAMMDD-TOM   FROM DATE                                
000681* OLDER THAN 3 MONTH                                                      
000690       MOVE 92                   TO DAG-KVKALDAG                          
000800       CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA                      
000900                           DAG-KDSVAR                                     
001000       IF DAG-KDSVAR = 'F'                                                
001100         MOVE 'FEL FRÅN DAGKONV ' TO FELTEXT                              
001200         CALL FELLOG                                                      
001210       ELSE                                                               
001220         MOVE DAG-TIAAMMDD-FOM  TO WS-3MONTH-OLD                          
001230         DISPLAY 'RENSA X4564 ÄLDRE ÄN = ' DAG-TIAAMMDD-FOM               
001300       END-IF                                                             
001301* OLDER THAN 6 MONTH                                                      
001310       MOVE 183                  TO DAG-KVKALDAG                          
001330       CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA                      
001340                           DAG-KDSVAR                                     
001350       IF DAG-KDSVAR = 'F'                                                
001360         MOVE 'FEL FRÅN DAGKONV ' TO FELTEXT                              
001370         CALL FELLOG                                                      
001380       ELSE                                                               
001390         MOVE DAG-TIAAMMDD-FOM  TO WS-6MONTH-OLD                          
001391         DISPLAY 'RENSA X2510 ÄLDRE ÄN = ' DAG-TIAAMMDD-FOM               
001392       END-IF                                                             
001400       MOVE NEJ TO WS-FIRST                                               
001501     END-IF                                                               
001502     IF SEGNAMN =  'WDGX4564'                                             
001503       IF 4564-TIREPDAT < WS-3MONTH-OLD                                   
001504         MOVE 8             TO RETURN-CODE                                
001505       END-IF                                                             
001506     END-IF                                                               
001507     IF SEGNAMN =  'WDGX2510'                                             
001508       IF 2510-FLREFERAL = NEJ AND                                        
001509          2510-TIREGDAT < WS-6MONTH-OLD                                   
001510         MOVE 8             TO RETURN-CODE                                
001511       END-IF                                                             
001512     END-IF                                                               
001513                                                                          
001514     GOBACK                                                               
001520     .                                                                    
