000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     WDMW4600.                                                
000400*AUTHOR.         KJELL ANDRE.                                             
000500*DATE-WRITTEN.   92/02/27.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*      FÖDNING AV REPORT-INFO TILL DATAMANAGER.                           
001100*      RAPPORTER SKA INNEHÅLLA REFERENS TILL DDNAMNET SOM LISTAN          
001200*      SKRIVS FRÅN.                                                       
001300*                                                                         
001400*      MATCHAR TVÅ GENERATIONER AV EN FIL MED INFO OM RAPPORTER           
001500*      OCH DDNAMN OCH GENERERAR EN UTFIL MED DE POSTER SOM                
001600*      FÖRÄNDRATS PÅ DEN SENASTE GENERATIONEN.                            
001700*                                                                         
001800*      DENNA UTFIL SKA SEN ANVÄNDAS FÖR ATT SKAPA/UPPDATERA               
001900*      RAPPORT-MEDLEMMAR I DATAMANAGER.                                   
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- GAMMAL LIST-INFO                                           
003000     SELECT OL-WDMR45                  ASSIGN TO WDMR46D1.                
003100     SKIP2                                                                
003200*          --- NY LIST-INFO                                               
003300     SELECT NL-WDMR45                  ASSIGN TO WDMR46D2.                
003400     SKIP2                                                                
003500*          --- FÖRÄNDRINGAR                                               
003600     SELECT UT-WDMR46                  ASSIGN TO WDMR46D3.                
003700     SKIP2                                                                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  OL-WDMR45                                                            
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600     SKIP2                                                                
004700 01  FILLER          PIC X(50).                                           
004800     SKIP3                                                                
004900 FD  NL-WDMR45                                                            
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200     SKIP2                                                                
005300 01  FILLER          PIC X(50).                                           
005400     EJECT                                                                
005500 FD  UT-WDMR46                                                            
005600     RECORDING       F                                                    
005700     BLOCK CONTAINS  0.                                                   
005800     SKIP2                                                                
005900 01  UT-POST         PIC X(80).                                           
006000     EJECT                                                                
006100 WORKING-STORAGE SECTION.                                                 
006200     SKIP2                                                                
006201                                                                          
006210*    -- CHECKED BY WY2000                                                 
006300 77  IDPGM                       PIC X(8)    VALUE 'WDMW4600'.            
006400 77  JA                          PIC X       VALUE 'J'.                   
006500 77  NEJ                         PIC X       VALUE 'N'.                   
006600                                                                          
006700 77  OL-WDMR45-EOF-SW            PIC X       VALUE 'N'.                   
006800     88  END-OF-OL-WDMR45                    VALUE 'J'.                   
006900                                                                          
007000 77  NL-WDMR45-EOF-SW            PIC X       VALUE 'N'.                   
007100     88  END-OF-NL-WDMR45                    VALUE 'J'.                   
007200                                                                          
007300 77  LIST-AENDRING-SW            PIC X       VALUE 'N'.                   
007400     88  LISTA-AENDRAD                       VALUE 'J'.                   
007500     88  LISTA-EJ-AENDRAD                    VALUE 'N'.                   
007600                                                                          
007700     EJECT                                                                
007800 01  MIN-ID.                                                              
007900     03 MIN-LISTNAMN             PIC X(12)   VALUE LOW-VALUE.             
008000     03 MIN-DDNAMN               PIC X(8)    VALUE LOW-VALUE.             
008100                                                                          
008200 77  SENASTE-LISTNAMN            PIC X(12)   VALUE LOW-VALUE.             
008300                                                                          
008400 77  LIX                         PIC S9(9)   COMP VALUE ZERO.             
008500 77  LIX-MAX                     PIC S9(9)   COMP VALUE 10.               
008600 77  LIX-TOP                     PIC S9(9)   COMP VALUE ZERO.             
008700                                                                          
008800 01  TAB.                                                                 
008900     03  TAB-LISTINFO        OCCURS 10.                                   
009000       05  TAB-LISTBESKR           PIC X(30).                             
009100       05  TAB-DDNAMN              PIC X(8).                              
009200                                                                          
009300     EJECT                                                                
009400 01  OLDL-AREA-START             PIC X(24)   VALUE                        
009500                                 'OLDL-AREA-START  '.                     
009600     SKIP2                                                                
009700 01  OLDL-AREA.                                                           
009800     03   OLDL-LISTNAMN          PIC X(12).                               
009900     03   OLDL-LISTBESKR         PIC X(30).                               
010000     03   OLDL-DDNAMN            PIC X(8).                                
010100                                                                          
010200 01  OLDL-ID.                                                             
010300     03   OLDL-ID-LISTNAMN       PIC X(12).                               
010400     03   OLDL-ID-DDNAMN         PIC X(8).                                
010500                                                                          
010600     EJECT                                                                
010700 01  NEWL-AREA-START             PIC X(24)   VALUE                        
010800                                 'NEWL-AREA-START  '.                     
010900     SKIP2                                                                
011000 01  NEWL-AREA.                                                           
011100     03   NEWL-LISTNAMN          PIC X(12).                               
011200     03   NEWL-LISTBESKR         PIC X(30).                               
011300     03   NEWL-DDNAMN            PIC X(8).                                
011400                                                                          
011500 01  NEWL-ID.                                                             
011600     03   NEWL-ID-LISTNAMN       PIC X(12).                               
011700     03   NEWL-ID-DDNAMN         PIC X(8).                                
011800                                                                          
011900     EJECT                                                                
012000 01  UT-AREA-START               PIC X(24)   VALUE                        
012100                                 'UT-AREA-START   '.                      
012200     SKIP2                                                                
012300 01  UT-AREA.                                                             
012400     03   UT-LISTNAMN            PIC X(12).                               
012500     03   UT-LISTBESKR           PIC X(30).                               
012600     03   UT-DDNAMN              PIC X(8).                                
012700                                                                          
012800 01  UT-DMRRAD                   PIC X(80).                               
012900                                                                          
013000 01  WTEXT                       PIC X(4).                                
013010 01  WPGMNAMN                    PIC X(9).                                
013100     EJECT                                                                
013200 PROCEDURE DIVISION.                                                      
013300     SKIP2                                                                
013400                                                                          
013500     PERFORM A-INIT                                                       
013600                                                                          
013700     PERFORM S01-LAES-OL-WDMR45                                           
013800     PERFORM S02-LAES-NL-WDMR45                                           
013900                                                                          
014000     PERFORM S20-MINSTA-ID                                                
014100     MOVE MIN-LISTNAMN TO SENASTE-LISTNAMN                                
014200     SET LISTA-EJ-AENDRAD TO TRUE                                         
014300                                                                          
014400     PERFORM UNTIL MIN-LISTNAMN = HIGH-VALUE                              
014500                                                                          
014600       IF NEWL-ID-LISTNAMN = MIN-LISTNAMN                                 
014700         PERFORM B-SPARA-NEWL                                             
014800       END-IF                                                             
014900       IF  NEWL-ID NOT = OLDL-ID                                          
015000         SET LISTA-AENDRAD TO TRUE                                        
015100       END-IF                                                             
015200       IF  OLDL-ID <= MIN-ID                                              
015300         PERFORM S01-LAES-OL-WDMR45                                       
015400       END-IF                                                             
015500       IF  NEWL-ID <= MIN-ID                                              
015600         PERFORM S02-LAES-NL-WDMR45                                       
015700       END-IF                                                             
015800                                                                          
015900       PERFORM S20-MINSTA-ID                                              
016000                                                                          
016100       IF MIN-LISTNAMN > SENASTE-LISTNAMN                                 
016200                                                                          
016300         IF LISTA-AENDRAD                                                 
016400           PERFORM D-SKRIV-LIST-INFO                                      
016500           SET LISTA-EJ-AENDRAD TO TRUE                                   
016600         END-IF                                                           
016700         MOVE MIN-LISTNAMN TO SENASTE-LISTNAMN                            
016800         MOVE ZERO TO LIX-TOP                                             
016900                                                                          
017000       END-IF                                                             
017100                                                                          
017200     END-PERFORM                                                          
017300                                                                          
017400     PERFORM Z-FINIT                                                      
017500                                                                          
017600     MOVE ZERO TO RETURN-CODE                                             
017700     GOBACK                                                               
017800     .                                                                    
017900     EJECT                                                                
018000 A-INIT SECTION.                                                          
018100                                                                          
018200     OPEN INPUT  OL-WDMR45                                                
018300                 NL-WDMR45                                                
018400                                                                          
018500     OPEN OUTPUT UT-WDMR46                                                
018600     .                                                                    
018700     EJECT                                                                
018800 B-SPARA-NEWL  SECTION.                                                   
018900     SKIP2                                                                
019000     MOVE 1 TO LIX                                                        
019100     PERFORM UNTIL LIX > LIX-TOP OR                                       
019200       TAB-DDNAMN (LIX) = NEWL-DDNAMN                                     
019300       ADD 1 TO LIX                                                       
019400     END-PERFORM                                                          
019500     IF LIX > LIX-TOP                                                     
019600       IF LIX-TOP < LIX-MAX                                               
019700         ADD 1 TO LIX-TOP                                                 
019800         MOVE NEWL-LISTBESKR TO TAB-LISTBESKR (LIX-TOP)                   
019900         MOVE NEWL-DDNAMN    TO TAB-DDNAMN (LIX-TOP)                      
020000       ELSE                                                               
020100         DISPLAY 'TOO MANY DDNAMES IN REPORT ' SENASTE-LISTNAMN           
020200       END-IF                                                             
020300     END-IF                                                               
020400     .                                                                    
020500     EJECT                                                                
020600 D-SKRIV-LIST-INFO SECTION.                                               
020700     SKIP2                                                                
020800     MOVE SENASTE-LISTNAMN TO UT-LISTNAMN                                 
020900                                                                          
021000     IF LIX-TOP > ZERO                                                    
021100                                                                          
021200       MOVE SPACE TO UT-DMRRAD                                            
021300       STRING 'REPLACE "' DELIMITED BY SIZE                               
021400           UT-LISTNAMN DELIMITED BY SPACE                                 
021500           '".' DELIMITED BY SIZE                                         
021600           INTO UT-DMRRAD                                                 
021700       PERFORM S11-SKRIV-UT-WDMR46                                        
021800                                                                          
021900       MOVE 'REPORT' TO UT-DMRRAD                                         
022000       PERFORM S11-SKRIV-UT-WDMR46                                        
022100                                                                          
022200       MOVE SPACE  TO UT-DMRRAD                                           
022300       PERFORM S11-SKRIV-UT-WDMR46                                        
022400                                                                          
022500       MOVE 'CATALOG' TO UT-DMRRAD                                        
022600       PERFORM S11-SKRIV-UT-WDMR46                                        
022700                                                                          
022800       MOVE SPACE TO UT-DMRRAD                                            
022900       STRING                                                             
023000          '    ' QUOTE 'REPORT' QUOTE                                     
023100          DELIMITED BY SIZE                                               
023200          INTO UT-DMRRAD                                                  
023300       PERFORM S11-SKRIV-UT-WDMR46                                        
023400                                                                          
023500       MOVE SPACE  TO UT-DMRRAD                                           
023600       PERFORM S11-SKRIV-UT-WDMR46                                        
023700                                                                          
023800       MOVE 'DESCRIPTION' TO UT-DMRRAD                                    
023900       PERFORM S11-SKRIV-UT-WDMR46                                        
024000                                                                          
024100       MOVE 1 TO LIX                                                      
024200       PERFORM UNTIL LIX > LIX-TOP                                        
024300          MOVE TAB-LISTBESKR (LIX) TO UT-LISTBESKR                        
024400                                                                          
024500          STRING QUOTE UT-LISTBESKR QUOTE                                 
024600            DELIMITED BY SIZE                                             
024700            INTO UT-DMRRAD                                                
024800          PERFORM S11-SKRIV-UT-WDMR46                                     
024900                                                                          
025000          ADD 1 TO LIX                                                    
025100       END-PERFORM                                                        
025200                                                                          
025300       MOVE SPACE TO UT-DMRRAD                                            
025400       PERFORM S11-SKRIV-UT-WDMR46                                        
025500                                                                          
025600       MOVE 'CONTAINS' TO UT-DMRRAD                                       
025700       PERFORM S11-SKRIV-UT-WDMR46                                        
025800                                                                          
025900       MOVE SPACE TO WTEXT                                                
026000       MOVE 1 TO LIX                                                      
026100       PERFORM UNTIL LIX > LIX-TOP                                        
026200          MOVE TAB-DDNAMN    (LIX) TO UT-DDNAMN                           
026300                                                                          
026400          MOVE SPACE TO UT-DMRRAD                                         
026500          IF UT-DDNAMN (1:1) = 'W'                                        
026510          AND UT-DDNAMN (7:1) = 'D' OR 'E'                                
026600            STRING                                                        
026700              WTEXT     DELIMITED BY SIZE                                 
026800              UT-DDNAMN DELIMITED BY SPACE                                
026900              '-DD'     DELIMITED BY SIZE                                 
027000              INTO UT-DMRRAD                                              
027100          ELSE                                                            
027101            UNSTRING UT-LISTNAMN DELIMITED BY '-' OR SPACE                
027102              INTO WPGMNAMN                                               
027110            STRING                                                        
027120              WTEXT     DELIMITED BY SIZE                                 
027121              WPGMNAMN  DELIMITED BY SPACE                                
027122              '-'       DELIMITED BY SIZE                                 
027130              UT-DDNAMN DELIMITED BY SPACE                                
027140              '-DD'     DELIMITED BY SIZE                                 
027150              INTO UT-DMRRAD                                              
027160          END-IF                                                          
027200          PERFORM S11-SKRIV-UT-WDMR46                                     
027300          MOVE '   ,' TO WTEXT                                            
027400                                                                          
027500          ADD 1 TO LIX                                                    
027600       END-PERFORM                                                        
027700                                                                          
027800       MOVE '.'  TO UT-DMRRAD                                             
027900       PERFORM S11-SKRIV-UT-WDMR46                                        
028000                                                                          
028100     ELSE                                                                 
028200       MOVE SPACE   TO UT-DDNAMN                                          
028300       MOVE SPACE   TO UT-LISTBESKR                                       
028400       MOVE SPACE   TO UT-DMRRAD                                          
028500       STRING 'REMOVE "' DELIMITED BY SIZE                                
028600           UT-LISTNAMN DELIMITED BY SPACE                                 
028700           '".' DELIMITED BY SIZE                                         
028800           INTO UT-DMRRAD                                                 
028900        PERFORM S11-SKRIV-UT-WDMR46                                       
028901                                                                          
028960                                                                          
029000     END-IF                                                               
029100                                                                          
029200     .                                                                    
029300     EJECT                                                                
029400 Z-FINIT SECTION.                                                         
029500     SKIP2                                                                
029600     CLOSE OL-WDMR45                                                      
029700           NL-WDMR45                                                      
029800           UT-WDMR46                                                      
029900     .                                                                    
030000     EJECT                                                                
030100 S01-LAES-OL-WDMR45 SECTION.                                              
030200     SKIP2                                                                
030300     READ OL-WDMR45 INTO OLDL-AREA                                        
030400     AT END                                                               
030500        MOVE HIGH-VALUE TO OLDL-ID                                        
030600        SET END-OF-OL-WDMR45 TO TRUE                                      
030700     NOT AT END                                                           
030800        MOVE OLDL-LISTNAMN  TO OLDL-ID-LISTNAMN                           
030900        MOVE OLDL-DDNAMN    TO OLDL-ID-DDNAMN                             
031000     END-READ                                                             
031100     .                                                                    
031200     EJECT                                                                
031300 S02-LAES-NL-WDMR45 SECTION.                                              
031400     SKIP2                                                                
031500     READ NL-WDMR45 INTO NEWL-AREA                                        
031600     AT END                                                               
031700        MOVE HIGH-VALUE TO NEWL-ID                                        
031800        SET END-OF-NL-WDMR45 TO TRUE                                      
031900     NOT AT END                                                           
032000        MOVE NEWL-LISTNAMN  TO NEWL-ID-LISTNAMN                           
032100        MOVE NEWL-DDNAMN    TO NEWL-ID-DDNAMN                             
032200     END-READ                                                             
032300     .                                                                    
032400     EJECT                                                                
032500 S11-SKRIV-UT-WDMR46 SECTION.                                             
032600     SKIP2                                                                
032700     WRITE UT-POST FROM UT-DMRRAD                                         
032800     .                                                                    
032900                                                                          
033000     EJECT                                                                
033100 S20-MINSTA-ID    SECTION.                                                
033200     SKIP2                                                                
033300                                                                          
033400     MOVE OLDL-ID TO MIN-ID                                               
033500     IF NEWL-ID < MIN-ID                                                  
033600       MOVE NEWL-ID TO MIN-ID                                             
033700     END-IF                                                               
033800     .                                                                    
