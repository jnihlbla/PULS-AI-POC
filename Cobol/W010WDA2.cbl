000100*COMPOPT VRTEREUS=YES                                                     
000110 ID DIVISION.                                                             
000200 PROGRAM-ID.     W010WDA2.                                                
000300 AUTHOR.         MÅNS.                                                    
000400 DATE-WRITTEN.   FEB   97.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000610*            UNLOAD RENSNING AV WDA2.                                     
000620                                                                          
000621 ENVIRONMENT DIVISION.                                                    
000622     SKIP2                                                                
000623 INPUT-OUTPUT SECTION.                                                    
000624                                                                          
000625 FILE-CONTROL.                                                            
000626     SKIP2                                                                
000627*          --- INFIL RENSADE LEVANM.                                      
000628     SELECT INFIL                      ASSIGN TO W010WDA2.                
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
000642     03 FILLER      PIC X(4).                                             
000643     03 -COPY W41880      -L.                                             
000644     SKIP3                                                                
000650 WORKING-STORAGE SECTION.                                                 
000651                                                                          
000652*    -- CHECKED BY WY2000                                                 
000660 77  IDPGM                     PIC X(8)    VALUE 'W010WDA2'.              
000661 77  W-ANTAL-LAES              PIC 9(7)    VALUE ZERO.                    
000662 77  W-ANTAL-RENSA-01          PIC 9(7)    VALUE ZERO.                    
000663 77  W-ANTAL-RENSA-OVR         PIC 9(7)    VALUE ZERO.                    
000664 77  W-ANTAL-POSTER            PIC 9(7)    VALUE ZERO.                    
000670 77  RENSA                     PIC X(1)    VALUE 'N'.                     
000671 77  LAES                      PIC X(1)    VALUE 'J'.                     
000672 77  FOERSTA                   PIC X(1)    VALUE 'J'.                     
000673 77  JA                        PIC X(1)    VALUE 'J'.                     
000674 77  NEJ                       PIC X(1)    VALUE 'N'.                     
000675 77  SPAR-RANDOMKEY            PIC X(4).                                  
000676 77  END-OF-INFIL-SW           PIC X(1)    VALUE 'N'.                     
000677     88 END-OF-INFIL                       VALUE 'J'.                     
000680                                                                          
000681 01  IN-AREA.                                                             
000682     03  IN-RANDOMKEY          PIC X(4).                                  
000683*    03  -COPY W41880 -PRE IN-                                            
000684     EJECT                                                                
000685 01  TABELL.                                                              
000687     05  TAB-IDLEVANM OCCURS 10 INDEXED BY IX PIC X(14).                  
000688     EJECT                                                                
000690 LINKAGE SECTION.                                                         
000691 01  SEGNAMN         PIC X(8).                                            
000693 01  OLD.                                                                 
000694*    03  -COPY WDA221                    -PRE OLD-                        
000695*    03  -COPY WDA211    -RED OLD-TXT-WDA221 -PRE OLD-                    
000696*    03  -COPY WDA201    -RED OLD-TXT-WDA221 -PRE OLD-                    
000697     EJECT                                                                
000698 01  NEW.                                                                 
000699*    03  -COPY WDA221                    -PRE NEW-                        
000700*    03  -COPY WDA211    -RED NEW-TXT-WDA221 -PRE NEW-                    
000701*    03  -COPY WDA201    -RED NEW-TXT-WDA221 -PRE NEW-                    
000702     EJECT                                                                
000703*01  -COPY W0008  -PRE WDA2-                                              
000704     05  FILLER                  PIC X.                                   
000706     EJECT                                                                
000707 PROCEDURE DIVISION USING SEGNAMN OLD NEW WDA2-PCB.                       
000708 STYR SECTION.                                                            
000709     IF FOERSTA = JA                                                      
000711       MOVE SPACE TO TABELL                                               
000715       OPEN INPUT INFIL                                                   
000716       MOVE NEJ TO FOERSTA                                                
000718       PERFORM S01-LAES-INFIL                                             
000728     END-IF                                                               
000730     MOVE 0            TO RETURN-CODE                                     
000731     ADD +1 TO W-ANTAL-POSTER                                             
000732     IF SEGNAMN =  'WDA201  '                                             
000734       SET IX TO 1                                                        
000735       SEARCH TAB-IDLEVANM                                                
000736         AT END                                                           
000738           MOVE JA TO LAES                                                
000739         WHEN TAB-IDLEVANM (IX) NOT = SPACE                               
000741           MOVE NEJ TO LAES                                               
000742       END-SEARCH                                                         
000745       IF LAES = JA                                                       
000747         MOVE NEJ TO LAES                                                 
000748         IF NOT END-OF-INFIL                                              
000749           SET IX TO 1                                                    
000750           MOVE IN-RANDOMKEY TO SPAR-RANDOMKEY                            
000751           PERFORM UNTIL IN-RANDOMKEY NOT = SPAR-RANDOMKEY                
000753             MOVE IN-IDLEVANM  TO TAB-IDLEVANM (IX)                       
000754             SET IX UP BY 1                                               
000756             PERFORM S01-LAES-INFIL                                       
000757             IF END-OF-INFIL                                              
000758               MOVE SPACE TO SPAR-RANDOMKEY                               
000761             END-IF                                                       
000762           END-PERFORM                                                    
000763         END-IF                                                           
000764       END-IF                                                             
000765       SET IX TO 1                                                        
000766       SEARCH TAB-IDLEVANM                                                
000767         AT END                                                           
000769          MOVE NEJ TO RENSA                                               
000770          MOVE 0             TO RETURN-CODE                               
000771         WHEN TAB-IDLEVANM (IX) = OLD-ANM-IDLEVANM                        
000773          MOVE JA TO RENSA                                                
000774          MOVE 8             TO RETURN-CODE                               
000775          ADD +1 TO W-ANTAL-RENSA-01                                      
000777          MOVE SPACE         TO TAB-IDLEVANM(IX)                          
000778       END-SEARCH                                                         
000779     ELSE                                                                 
000780       IF SEGNAMN =  '        '                                           
000781         DISPLAY 'ANTAL POSTER = ' W-ANTAL-POSTER                         
000782         DISPLAY 'ANTAL RENSA01= ' W-ANTAL-RENSA-01                       
000783         DISPLAY 'ANTAL RENSA-OVR = ' W-ANTAL-RENSA-OVR                   
000784         DISPLAY 'ANTAL LAES   = ' W-ANTAL-LAES                           
000785         CLOSE INFIL                                                      
000786       ELSE                                                               
000787         IF RENSA = JA                                                    
000788            MOVE 8             TO RETURN-CODE                             
000789            ADD +1 TO W-ANTAL-RENSA-OVR                                   
000790         ELSE                                                             
000791            MOVE 0             TO RETURN-CODE                             
000792         END-IF                                                           
000793       END-IF                                                             
000794     END-IF                                                               
000795                                                                          
000796     GOBACK                                                               
000797     .                                                                    
000798 S01-LAES-INFIL SECTION.                                                  
000799                                                                          
000800     ADD +1 TO W-ANTAL-LAES                                               
000801     READ INFIL INTO IN-AREA                                              
000810     AT END                                                               
000900        SET END-OF-INFIL TO TRUE                                          
001000     .                                                                    
