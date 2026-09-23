000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W3357500.                                                
000400*AUTHOR.         INGVAR SKJELBRED.                                        
000500*DATE-WRITTEN.   95/10/09.                                                
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        SLÅR I HOP FILERNA W33574 OCH W33570                             
001000*                                                                         
001100*                                                                         
001200*    ABENDKODER:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002301     SKIP2                                                                
002302*          --- SORTERAD FIL W33570                                        
002303     SELECT W33570                     ASSIGN TO W33575D1.                
002304     SKIP2                                                                
002305*          --- SORTAD INFIL W33574                                        
002306     SELECT W33574                     ASSIGN TO W33575D2.                
002307     SKIP2                                                                
002308*          --- IHOPSLAGEN FIL AV W33570 O W33574                          
002310     SELECT W33575                     ASSIGN TO W33575D3.                
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP3                                                                
002800 FILE SECTION.                                                            
002901     SKIP3                                                                
002902 FD  W33570                                                               
002903     RECORDING       F                                                    
002904     BLOCK CONTAINS  0.                                                   
002905                                                                          
002906*01  -COPY W33570      -L.                                                
002907     SKIP3                                                                
002908 FD  W33574                                                               
002909     RECORDING       F                                                    
002910     BLOCK CONTAINS  0.                                                   
002911                                                                          
002912*01  -COPY W33574      -L.                                                
002913     SKIP3                                                                
002914 FD  W33575                                                               
002915     RECORDING       F                                                    
002916     BLOCK CONTAINS  0.                                                   
002917                                                                          
002920*01  POST -COPY W33575 -PRE  UT-  -L.                                     
003000     EJECT                                                                
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003201                                                                          
003210*    -- CHECKED BY WY2000                                                 
003300 77  IDPGM                       PIC X(8)    VALUE 'W3357500'.            
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003705                                                                          
003706 77  W33570-EOF-SW               PIC X       VALUE 'N'.                   
003707     88  END-OF-W33570                       VALUE 'J'.                   
003708                                                                          
003709 77  W33574-EOF-SW               PIC X       VALUE 'N'.                   
003710     88  END-OF-W33574                       VALUE 'J'.                   
004410     EJECT                                                                
004500 01  DYNAMISKA-SUBPROGRAM.                                                
004600*                                                                         
004700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004810     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
004900     SKIP2                                                                
005000*    --- PARAMETRAR TILL ABEND                                            
005100                                                                          
005200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
005400     SKIP2                                                                
005500 01  FELTEXT.                                                             
005600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005801     EJECT                                                                
005802*    --- PARAMETRAR TILL POSTSUM                                          
005803*                                                                         
005810*01  -COPY W0005   -PRE  POSTSUM-                                         
006001     EJECT                                                                
006002 01  IN-AREA-START               PIC X(24)   VALUE                        
006003                                 'IN-AREA-START  '.                       
006004     SKIP2                                                                
006005                                                                          
006006*01  AREA -COPY W33570     -PRE IN1-                                      
006007     EJECT                                                                
006008 01  IN-AREA-START               PIC X(24)   VALUE                        
006009                                 'IN-AREA-START  '.                       
006010     SKIP2                                                                
006011                                                                          
006012*01  AREA -COPY W33574     -PRE IN2-                                      
006013     EJECT                                                                
006014 01  UT-AREA-START               PIC X(24)   VALUE                        
006015                                 'UT-AREA-START  '.                       
006016     SKIP2                                                                
006017                                                                          
006020*01  AREA -COPY W33575     -PRE UT-                                       
006100     EJECT                                                                
006200 PROCEDURE DIVISION.                                                      
006400     SKIP2                                                                
006500                                                                          
006600     PERFORM A-INIT                                                       
006701     PERFORM S01-LAES-W33570                                              
006702     PERFORM S02-LAES-W33574                                              
006800     PERFORM UNTIL END-OF-W33570                                          
006810              AND  END-OF-W33574                                          
006900          PERFORM B-KONTRL-IN-FILER                                       
007600     END-PERFORM                                                          
007700                                                                          
007800                                                                          
007900     PERFORM Z-FINIT                                                      
008000                                                                          
008100     MOVE ZERO TO RETURN-CODE                                             
008200     GOBACK                                                               
008300     .                                                                    
008400     EJECT                                                                
008500 A-INIT SECTION.                                                          
008601                                                                          
008602     OPEN INPUT  W33570                                                   
008610                 W33574                                                   
008701                                                                          
008710     OPEN OUTPUT W33575                                                   
008800                                                                          
009010     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009100     .                                                                    
009200     EJECT                                                                
009202 B-KONTRL-IN-FILER SECTION.                                               
009205                                                                          
009206     IF END-OF-W33570                                                     
009208        PERFORM BB-SKAPA-FEL-FIL                                          
009209        PERFORM S02-LAES-W33574                                           
009210     ELSE                                                                 
009211       IF END-OF-W33574                                                   
009213          PERFORM BC-SKAPA-FEL-FIL                                        
009214          PERFORM S01-LAES-W33570                                         
009215       ELSE                                                               
009216         IF IN1-IDPARTNR = IN2-IDPARTNR                                   
009220           PERFORM BA-SKAPA-NY-FIL                                        
009221           PERFORM S01-LAES-W33570                                        
009222           PERFORM S02-LAES-W33574                                        
009223         ELSE                                                             
009224           IF IN1-IDPARTNR > IN2-IDPARTNR                                 
009226              PERFORM BB-SKAPA-FEL-FIL                                    
009227              PERFORM S02-LAES-W33574                                     
009228           ELSE                                                           
009230              PERFORM BC-SKAPA-FEL-FIL                                    
009231              PERFORM S01-LAES-W33570                                     
009232           END-IF                                                         
009233         END-IF                                                           
009258       END-IF                                                             
009259     END-IF                                                               
009260     .                                                                    
009261     EJECT                                                                
009262 BA-SKAPA-NY-FIL SECTION.                                                 
009263                                                                          
009264     MOVE IN1-IDPARTNR TO UT-IDPARTNR                                     
009267     MOVE IN2-KDKREDSP TO UT-KDKREDSP                                     
009270     MOVE IN1-IDPROMR  TO UT-IDPROMR                                      
009291                                                                          
009292     PERFORM S11-SKRIV-W33575                                             
009293     .                                                                    
009294     EJECT                                                                
009295 BB-SKAPA-FEL-FIL SECTION.                                                
009297                                                                          
009298     MOVE IN2-IDPARTNR TO UT-IDPARTNR                                     
009301     MOVE IN2-KDKREDSP TO UT-KDKREDSP                                     
009302     MOVE SPACE        TO UT-IDPROMR                                      
009303                                                                          
009304     PERFORM S11-SKRIV-W33575                                             
009305     .                                                                    
009310     EJECT                                                                
009320 BC-SKAPA-FEL-FIL SECTION.                                                
009330                                                                          
009340     MOVE IN1-IDPARTNR TO UT-IDPARTNR                                     
009380     MOVE IN1-IDPROMR  TO UT-IDPROMR                                      
009381     MOVE SPACE        TO UT-KDKREDSP                                     
009390                                                                          
009391     PERFORM S11-SKRIV-W33575                                             
009392     .                                                                    
009393     EJECT                                                                
009400 Z-FINIT SECTION.                                                         
009401     CLOSE W33570                                                         
009402           W33574                                                         
009410           W33575                                                         
009501     SKIP2                                                                
009502     MOVE 'S' TO POSTSUM-OPKOD                                            
009510     CALL POSTSUM USING POSTSUM-PARM                                      
009600     .                                                                    
009701     EJECT                                                                
009702 S01-LAES-W33570  SECTION.                                                
009703                                                                          
009704     READ W33570 INTO IN1-AREA                                            
009705     AT END                                                               
009706        SET END-OF-W33570 TO TRUE                                         
009708     NOT AT END                                                           
009709        MOVE 'W33570'   TO POSTSUM-FDNAMN                                 
009710        MOVE 'W33575D1' TO POSTSUM-DDNAMN2                                
009711        MOVE 'INF1'     TO POSTSUM-TRANSTYP                               
009712        CALL POSTSUM USING POSTSUM-PARM                                   
009716     END-READ                                                             
009717     .                                                                    
009718     EJECT                                                                
009719 S02-LAES-W33574  SECTION.                                                
009720                                                                          
009721     READ W33574 INTO IN2-AREA                                            
009722     AT END                                                               
009723        SET END-OF-W33574 TO TRUE                                         
009725     NOT AT END                                                           
009726        MOVE 'W33574'   TO POSTSUM-FDNAMN                                 
009727        MOVE 'W33575D2' TO POSTSUM-DDNAMN2                                
009728        MOVE 'INF2'     TO POSTSUM-TRANSTYP                               
009729        CALL POSTSUM USING POSTSUM-PARM                                   
009732     END-READ                                                             
009740     .                                                                    
009801     EJECT                                                                
009802 S11-SKRIV-W33575 SECTION.                                                
009803                                                                          
009804     WRITE UT-POST FROM UT-AREA                                           
009806                                                                          
009807     MOVE 'W335'     TO POSTSUM-TRANSTYP                                  
009808     MOVE 'W33575'   TO POSTSUM-FDNAMN                                    
009809     MOVE 'W33575D3' TO POSTSUM-DDNAMN2                                   
009810     CALL POSTSUM USING POSTSUM-PARM                                      
009820     .                                                                    
