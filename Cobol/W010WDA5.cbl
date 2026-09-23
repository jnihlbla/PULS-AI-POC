000100*COMPOPT VRTEREUS=YES                                                     
000110 ID DIVISION.                                                             
000200 PROGRAM-ID.     W010WDA5.                                                
000300 AUTHOR.         LASSI OLGRENER.                                          
000400 DATE-WRITTEN.   DEC   99.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000610*            UNLOAD RENSNING WDA5.                                        
000620                                                                          
000621 ENVIRONMENT DIVISION.                                                    
000622     SKIP2                                                                
000623 INPUT-OUTPUT SECTION.                                                    
000624                                                                          
000625 FILE-CONTROL.                                                            
000626     SKIP2                                                                
000627*          --- INFIL - RENSNINGSPOSTER                                    
000628     SELECT INFIL                      ASSIGN TO W010WDA5.                
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
000643     03 -COPY W479A5      -L.                                             
000644     SKIP3                                                                
000650 WORKING-STORAGE SECTION.                                                 
000651                                                                          
000652*    -- CHECKED BY WY2000                                                 
000660 77  IDPGM                     PIC X(8)    VALUE 'W010WDA5'.              
000661 77  W-ANTAL-INFIL             PIC 9(7)    VALUE ZERO.                    
000672 77  FOERSTA                   PIC X(1)    VALUE 'J'.                     
000673 77  JA                        PIC X(1)    VALUE 'J'.                     
000674 77  NEJ                       PIC X(1)    VALUE 'N'.                     
000676 77  END-OF-INFIL-SW           PIC X(1)    VALUE 'N'.                     
000677     88 END-OF-INFIL                       VALUE 'J'.                     
000680                                                                          
000681 01  IN-AREA.                                                             
000683*    03  -COPY W479A5  -PRE IN-                                           
000684     EJECT                                                                
000690 LINKAGE SECTION.                                                         
000691 01  SEGNAMN         PIC X(8).                                            
000693 01  OLD.                                                                 
000694*    03  -COPY WDA501                                                     
000697     EJECT                                                                
000698 01  NEW.                                                                 
000699*    03  -COPY WDA501                                                     
000704     EJECT                                                                
000708 PROCEDURE DIVISION USING SEGNAMN OLD NEW.                                
000709 STYR SECTION.                                                            
000710                                                                          
000711     IF FOERSTA = JA                                                      
000715       OPEN INPUT INFIL                                                   
000716       MOVE NEJ TO FOERSTA                                                
000717       PERFORM S01-LAES-INFIL                                             
000728     END-IF                                                               
000729                                                                          
000732     MOVE 0            TO RETURN-CODE                                     
000733     IF SEGNAMN =  'WDA501  '                                             
000765       PERFORM UNTIL END-OF-INFIL OR                                      
000767            (IN-W479A5 (1:24) NOT < RAD-WDA501 IN OLD (1:24) )            
000768         PERFORM S01-LAES-INFIL                                           
000775           IF RAD-WDA501 IN OLD (1:24) > IN-W479A5 (1:24)                 
000776             DISPLAY 'E4-RO SAKNAS PÅ A5: '                               
000777                                  IN-IDDISTR                              
000778                                  IN-IDKUNDNR                             
000779                                  IN-IDKUNDRF                             
000780                                  IN-IDARTNR                              
000781                                  IN-IDLOPNR                              
000782           END-IF                                                         
000784       END-PERFORM                                                        
000785       IF NOT END-OF-INFIL                                                
000786         IF RAD-IDDISTR  IN OLD = IN-IDDISTR  AND                         
000787            RAD-IDKUNDNR IN OLD = IN-IDKUNDNR AND                         
000788            RAD-IDKUNDRF IN OLD = IN-IDKUNDRF AND                         
000789            RAD-IDARTNR  IN OLD = IN-IDARTNR  AND                         
000790            RAD-IDLOPNR  IN OLD = IN-IDLOPNR                              
000791                                                                          
000792           IF RAD-KDSTARAD IN OLD = '4'                                   
000794             MOVE 8         TO RETURN-CODE                                
000795           ELSE                                                           
000796             DISPLAY 'RO-KDSTARAD < 4: '                                  
000797                                  IN-IDDISTR                              
000798                                  IN-IDKUNDNR                             
000799                                  IN-IDKUNDRF                             
000800                                  IN-IDARTNR                              
000801                                  IN-IDLOPNR                              
000802           END-IF                                                         
000803         END-IF                                                           
000804       END-IF                                                             
000805     ELSE                                                                 
000806       IF SEGNAMN =  '        '                                           
000807         DISPLAY 'ANTAL INFIL     = ' W-ANTAL-INFIL                       
000808         CLOSE INFIL                                                      
000809       END-IF                                                             
000810     END-IF                                                               
000811                                                                          
000812     GOBACK                                                               
000813     .                                                                    
000814 S01-LAES-INFIL SECTION.                                                  
000815                                                                          
000816     ADD +1 TO W-ANTAL-INFIL                                              
000817     READ INFIL INTO IN-AREA                                              
000820     AT END                                                               
000900        SET END-OF-INFIL TO TRUE                                          
001000     .                                                                    
