000100*COMPOPT VRTEREUS=YES                                                     
000110 ID DIVISION.                                                             
000200 PROGRAM-ID.     W010WDE4.                                                
000300 AUTHOR.         LASSI OLGRENER.                                          
000400 DATE-WRITTEN.   DEC   99.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000610*            UNLOAD RENSNING WDE4.                                        
000620                                                                          
000621 ENVIRONMENT DIVISION.                                                    
000622     SKIP2                                                                
000623 INPUT-OUTPUT SECTION.                                                    
000624                                                                          
000625 FILE-CONTROL.                                                            
000626     SKIP2                                                                
000627*          --- INFIL - RENSNINGSPOSTER                                    
000628     SELECT INFIL                      ASSIGN TO W010WDE4.                
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
000643     03 -COPY W479E4      -L.                                             
000644     SKIP3                                                                
000650 WORKING-STORAGE SECTION.                                                 
000651                                                                          
000652*    -- CHECKED BY WY2000                                                 
000660 77  IDPGM                     PIC X(8)    VALUE 'W010WDE4'.              
000661 77  W-ANTAL-INFIL             PIC 9(7)    VALUE ZERO.                    
000666 77  SPAR-IDRANDOM             PIC X(4)    VALUE SPACE.                   
000671 77  LAES                      PIC X(1)    VALUE 'J'.                     
000672 77  FOERSTA                   PIC X(1)    VALUE 'J'.                     
000673 77  JA                        PIC X(1)    VALUE 'J'.                     
000674 77  NEJ                       PIC X(1)    VALUE 'N'.                     
000676 77  END-OF-INFIL-SW           PIC X(1)    VALUE 'N'.                     
000677     88 END-OF-INFIL                       VALUE 'J'.                     
000680                                                                          
000681 01  TABELL.                                                              
000682     03  TAB-WDE401KY OCCURS 10 INDEXED BY IX.                            
000684       05  TAB-IDGMTREF    PIC X(17).                                     
000685       05  TAB-IDPRODNR    PIC S9(7)  COMP-3.                             
000686       05  TAB-IDPLKLST    PIC S9(3)  COMP-3.                             
000687     EJECT                                                                
000688 01  IN-AREA.                                                             
000689*    03  -COPY W479E4  -PRE IN-                                           
000690     EJECT                                                                
000691 LINKAGE SECTION.                                                         
000692 01  SEGNAMN         PIC X(8).                                            
000693 01  OLD.                                                                 
000694*    03  -COPY WDE411                                                     
000695*    03  -COPY WDE401    -RED ORAD-WDE411                                 
000697     EJECT                                                                
000698 01  NEW.                                                                 
000699*    03  -COPY WDE411                                                     
000700*    03  -COPY WDE401    -RED ORAD-WDE411                                 
000704     EJECT                                                                
000708 PROCEDURE DIVISION USING SEGNAMN OLD NEW.                                
000709 STYR SECTION.                                                            
000710                                                                          
000711     IF FOERSTA = JA                                                      
000712       MOVE NEJ   TO FOERSTA                                              
000713       MOVE SPACE TO TABELL                                               
000715       OPEN INPUT INFIL                                                   
000717       PERFORM S01-LAES-INFIL                                             
000728     END-IF                                                               
000729                                                                          
000741     MOVE 0            TO RETURN-CODE                                     
000742     IF SEGNAMN =  'WDE401  '                                             
000743       SET IX TO 1                                                        
000744       SEARCH TAB-WDE401KY                                                
000745         AT END                                                           
000746           MOVE JA TO LAES                                                
000747         WHEN TAB-IDGMTREF (IX) NOT = SPACE                               
000748           MOVE NEJ TO LAES                                               
000749       END-SEARCH                                                         
000750                                                                          
000751       IF LAES = JA                                                       
000752         MOVE NEJ TO LAES                                                 
000753         IF NOT END-OF-INFIL                                              
000754           SET IX TO 1                                                    
000755           MOVE IN-IDRANDOM    TO SPAR-IDRANDOM                           
000756           PERFORM UNTIL IN-IDRANDOM NOT = SPAR-IDRANDOM                  
000757             MOVE IN-IDGMTREF TO TAB-IDGMTREF (IX)                        
000758             MOVE IN-IDPRODNR TO TAB-IDPRODNR (IX)                        
000759             MOVE IN-IDPLKLST TO TAB-IDPLKLST (IX)                        
000760             SET IX UP BY 1                                               
000761                                                                          
000762             PERFORM S01-LAES-INFIL                                       
000766           END-PERFORM                                                    
000767         END-IF                                                           
000768       END-IF                                                             
000769                                                                          
000770       SET IX TO 1                                                        
000771       SEARCH TAB-WDE401KY                                                
000772         AT END                                                           
000774           MOVE 0             TO RETURN-CODE                              
000775         WHEN TAB-IDGMTREF (IX) = KORD-IDGMTREF IN OLD AND                
000776              TAB-IDPRODNR (IX) = KORD-IDPRODNR IN OLD AND                
000777              TAB-IDPLKLST (IX) = KORD-IDPLKLST IN OLD                    
000778           MOVE 12            TO RETURN-CODE                              
000780           MOVE SPACE         TO TAB-WDE401KY(IX)                         
000781       END-SEARCH                                                         
000808     ELSE                                                                 
000809       IF SEGNAMN = SPACE                                                 
000814         CLOSE INFIL                                                      
000815         DISPLAY 'ANTAL IN-POSTER: ' W-ANTAL-INFIL                        
000819       END-IF                                                             
000820     END-IF                                                               
000821                                                                          
000822     GOBACK                                                               
000823     .                                                                    
000824 S01-LAES-INFIL SECTION.                                                  
000825                                                                          
000826     ADD +1 TO W-ANTAL-INFIL                                              
000827     READ INFIL INTO IN-AREA                                              
000830     AT END                                                               
000900        SET END-OF-INFIL TO TRUE                                          
000910        MOVE SPACE       TO SPAR-IDRANDOM                                 
001000     .                                                                    
