000100*COMPOPT VRTEREUS=YES                                                     
000110 ID DIVISION.                                                             
000200 PROGRAM-ID.     W010WDE6.                                                
000300 AUTHOR.         LASSI OLGRENER.                                          
000400 DATE-WRITTEN.   DEC   99.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000610*            UNLOAD RENSNING WDE6.                                        
000620                                                                          
000621 ENVIRONMENT DIVISION.                                                    
000622     SKIP2                                                                
000623 INPUT-OUTPUT SECTION.                                                    
000624                                                                          
000625 FILE-CONTROL.                                                            
000626     SKIP2                                                                
000627*          --- INFIL - RENSNINGSPOSTER                                    
000628     SELECT INFIL                      ASSIGN TO W010WDE6.                
000629     SKIP2                                                                
000633 DATA DIVISION.                                                           
000634     SKIP2                                                                
000635 FILE SECTION.                                                            
000636     SKIP3                                                                
000637 FD  INFIL                                                                
000638     RECORDING       F                                                    
000639     BLOCK CONTAINS  0.                                                   
000640                                                                          
000641 01  FILLER.                                                              
000643     03 -COPY W479E6      -L.                                             
000644     SKIP3                                                                
000650 WORKING-STORAGE SECTION.                                                 
000651                                                                          
000652*    -- CHECKED BY WY2000                                                 
000660 77  IDPGM                     PIC X(8)    VALUE 'W010WDE6'.              
000661 77  W-ANTAL-INFIL             PIC 9(7)    VALUE ZERO.                    
000662 77  SPAR-IDRANDOM             PIC X(4)    VALUE SPACE.                   
000672 77  LAES                      PIC X(1)    VALUE 'J'.                     
000673 77  FOERSTA                   PIC X(1)    VALUE 'J'.                     
000674 77  JA                        PIC X(1)    VALUE 'J'.                     
000675 77  NEJ                       PIC X(1)    VALUE 'N'.                     
000676 77  END-OF-INFIL-SW           PIC X(1)    VALUE 'N'.                     
000677     88 END-OF-INFIL                       VALUE 'J'.                     
000680                                                                          
000681 01  TABELL.                                                              
000683     03  TAB-IDPRODNR OCCURS 10 INDEXED BY IX PIC S9(7) COMP-3.           
000684 01  IN-AREA.                                                             
000685*    03  -COPY W479E6  -PRE IN-                                           
000686     EJECT                                                                
000690 LINKAGE SECTION.                                                         
000691 01  SEGNAMN         PIC X(8).                                            
000693 01  OLD.                                                                 
000694*    03  -COPY WDE611                                                     
000695*    03  -COPY WDE601    -RED KOLLI-WDE611                                
000697     EJECT                                                                
000698 01  NEW.                                                                 
000699*    03  -COPY WDE611                                                     
000700*    03  -COPY WDE601    -RED KOLLI-WDE611                                
000704     EJECT                                                                
000708 PROCEDURE DIVISION USING SEGNAMN OLD NEW.                                
000709 STYR SECTION.                                                            
000710                                                                          
000711     IF FOERSTA = JA                                                      
000712       MOVE NEJ  TO FOERSTA                                               
000713       INITIALIZE TABELL                                                  
000715       OPEN INPUT INFIL                                                   
000717       PERFORM S01-LAES-INFIL                                             
000728     END-IF                                                               
000729                                                                          
000732     MOVE 0            TO RETURN-CODE                                     
000733     IF SEGNAMN =  'WDE601  '                                             
000734       SET IX TO 1                                                        
000735       SEARCH TAB-IDPRODNR                                                
000736         AT END                                                           
000737           MOVE JA TO LAES                                                
000738         WHEN TAB-IDPRODNR (IX) NOT = ZERO                                
000739           MOVE NEJ TO LAES                                               
000740       END-SEARCH                                                         
000741                                                                          
000742       IF LAES = JA                                                       
000743         MOVE NEJ TO LAES                                                 
000744         IF NOT END-OF-INFIL                                              
000745           SET IX TO 1                                                    
000746           MOVE IN-IDRANDOM    TO SPAR-IDRANDOM                           
000747           PERFORM UNTIL IN-IDRANDOM NOT = SPAR-IDRANDOM                  
000749                         OR (END-OF-INFIL)                                
000750             MOVE IN-IDPRODNR TO TAB-IDPRODNR (IX)                        
000751             SET IX UP BY 1                                               
000752                                                                          
000753             PERFORM S01-LAES-INFIL                                       
000754           END-PERFORM                                                    
000755         END-IF                                                           
000756       END-IF                                                             
000757                                                                          
000758       SET IX TO 1                                                        
000759       SEARCH TAB-IDPRODNR                                                
000760         AT END                                                           
000761           MOVE 0             TO RETURN-CODE                              
000763         WHEN TAB-IDPRODNR (IX) = VORD-IDPRODNR IN OLD                    
000765           MOVE 12            TO RETURN-CODE                              
000766           MOVE ZERO          TO TAB-IDPRODNR(IX)                         
000767       END-SEARCH                                                         
000779     ELSE                                                                 
000780       IF SEGNAMN =  '        '                                           
000784         DISPLAY 'ANTAL INFIL     = ' W-ANTAL-INFIL                       
000785         CLOSE INFIL                                                      
000793       END-IF                                                             
000794     END-IF                                                               
000795                                                                          
000796     GOBACK                                                               
000797     .                                                                    
000798 S01-LAES-INFIL SECTION.                                                  
000799                                                                          
000800     ADD +1 TO W-ANTAL-INFIL                                              
000805     READ INFIL INTO IN-AREA                                              
000810     AT END                                                               
000900        SET END-OF-INFIL TO TRUE                                          
001000     .                                                                    
