000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     WDMW4300.                                                
000400*AUTHOR.         KJELL ANDRE.                                             
000500*DATE-WRITTEN.   92/02/13.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*      FÖDNING AV JOBB- OCH REPORT-INFO TILL DATAMANAGER.                 
001100*      JOBB SKA INNEHÅLLA REFERENS TILL ANROPADE PROCEDURER               
001200*      ELLER PROGRAM, OCH RAPPORTER/LISTOR SOM SKAPAS I JOBBET.           
001300*                                                                         
001400*      MATCHAR TVÅ GENERATIONER AV EN FIL MED INFO OM JOBB OCH            
001500*      PROCEDUREANROP (FRÅN JCL-BIBLIOTEKET) OCH GENERERAR EN             
001600*      UTFIL MED DE POSTER SOM FÖRÄNDRATS PÅ DEN SENASTE                  
001700*      GENERATIONEN.                                                      
001800*                                                                         
001900*      SAMTIDIGT MATCHAS TVÅ GENERATIONER AV EN ANNAN FIL MED             
002000*      INFO OM JOBB OCH LISTOR (FRÅN EXPRESS-DELIVERY) OCH PÅ             
002100*      SAMMA UTFIL FÖR DE SKILLNADER SOM FINNS.                           
002200*                                                                         
002300*      DENNA UTFIL SKA SEN ANVÄNDAS FÖR ATT UPPDATERA                     
002400*      JOBB-MEDLEMMAR I DATAMANAGER.                                      
002500*                                                                         
002600*      YTTERLIGARE EN UTFIL MED INFO OM LISTOR SKRIVS LIKNANDE            
002700*      DEN EXPRESS-DELIVERY FIL SOM LÄSES, FAST DÄR DDNAMN                
002800*      ALLTID FINNS IFYLLT (DETTA SAKNAS FÖR STACKADE LISTOR).            
002900*      DEN SKA SEN ANVÄNDAS FÖR ATT UPPDATERA REPORT-MEDLEMMAR            
003000*      I DATAMANAGER                                                      
003100*                                                                         
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     SKIP2                                                                
003600 INPUT-OUTPUT SECTION.                                                    
003700                                                                          
003800 FILE-CONTROL.                                                            
003900     SKIP2                                                                
004000*          --- GAMMAL JOBB-EXEC INFO                                      
004100     SELECT OX-WDMR42                  ASSIGN TO WDMR43D1.                
004200     SKIP2                                                                
004300*          --- NY JOBB-EXEC INFO                                          
004400     SELECT NX-WDMR42                  ASSIGN TO WDMR43D2.                
004500     SKIP2                                                                
004600*          --- GAMMAL JOBB-LIST INFO                                      
004700     SELECT OL-WDMR40                  ASSIGN TO WDMR43D3.                
004800     SKIP2                                                                
004900*          --- NY JOBB-LIST INFO                                          
005000     SELECT NL-WDMR40                  ASSIGN TO WDMR43D4.                
005100     SKIP2                                                                
005200*          --- FÖRÄNDRADE JOBB TILL DMR                                   
005300     SELECT UT-WDMR43                  ASSIGN TO WDMR43D5.                
005400     SKIP2                                                                
005500*          --- KOMPLETTERAD LIST-INFO                                     
005600     SELECT UT2-WDMR44                 ASSIGN TO WDMR43D6.                
005700     EJECT                                                                
005800 DATA DIVISION.                                                           
005900     SKIP3                                                                
006000 FILE SECTION.                                                            
006100     SKIP3                                                                
006200 FD  OX-WDMR42                                                            
006300     RECORDING       F                                                    
006400     BLOCK CONTAINS  0.                                                   
006500     SKIP2                                                                
006600 01  FILLER          PIC X(17).                                           
006700     SKIP3                                                                
006800 FD  NX-WDMR42                                                            
006900     RECORDING       F                                                    
007000     BLOCK CONTAINS  0.                                                   
007100     SKIP2                                                                
007200 01  FILLER          PIC X(17).                                           
007300     SKIP3                                                                
007400 FD  OL-WDMR40                                                            
007500     RECORDING       F                                                    
007600     BLOCK CONTAINS  0.                                                   
007700     SKIP2                                                                
007800 01  FILLER          PIC X(58).                                           
007900     SKIP3                                                                
008000 FD  NL-WDMR40                                                            
008100     RECORDING       F                                                    
008200     BLOCK CONTAINS  0.                                                   
008300     SKIP2                                                                
008400 01  FILLER          PIC X(58).                                           
008500     EJECT                                                                
008600 FD  UT-WDMR43                                                            
008700     RECORDING       F                                                    
008800     BLOCK CONTAINS  0.                                                   
008900     SKIP2                                                                
009000 01  UT-POST         PIC X(80).                                           
009100     SKIP3                                                                
009200 FD  UT2-WDMR44                                                           
009300     RECORDING       F                                                    
009400     BLOCK CONTAINS  0.                                                   
009500     SKIP2                                                                
009600 01  UT2-POST         PIC X(50).                                          
009700     EJECT                                                                
009800 WORKING-STORAGE SECTION.                                                 
009900     SKIP2                                                                
009901                                                                          
009910*    -- CHECKED BY WY2000                                                 
010000 77  IDPGM                       PIC X(8)    VALUE 'WDMW4300'.            
010100 77  JA                          PIC X       VALUE 'J'.                   
010200 77  NEJ                         PIC X       VALUE 'N'.                   
010300                                                                          
010400 77  OX-WDMR42-EOF-SW            PIC X       VALUE 'N'.                   
010500     88  END-OF-OX-WDMR42                    VALUE 'J'.                   
010600                                                                          
010700 77  NX-WDMR42-EOF-SW            PIC X       VALUE 'N'.                   
010800     88  END-OF-NX-WDMR42                    VALUE 'J'.                   
010900                                                                          
011000 77  OL-WDMR40-EOF-SW            PIC X       VALUE 'N'.                   
011100     88  END-OF-OL-WDMR40                    VALUE 'J'.                   
011200                                                                          
011300 77  NL-WDMR40-EOF-SW            PIC X       VALUE 'N'.                   
011400     88  END-OF-NL-WDMR40                    VALUE 'J'.                   
011500                                                                          
011600 77  JOBB-AENDRING-SW            PIC X       VALUE 'N'.                   
011700     88  JOBB-AENDRAT                        VALUE 'J'.                   
011800     88  JOBB-EJ-AENDRAT                     VALUE 'N'.                   
011900                                                                          
012000     EJECT                                                                
012100 77  MIN-JOBBNAMN                PIC X(8)    VALUE LOW-VALUE.             
012200 77  SENASTE-JOBBNAMN            PIC X(8)    VALUE LOW-VALUE.             
012300 77  SPARO-DDNAMN                PIC X(8)    VALUE LOW-VALUE.             
012400 77  SPARN-DDNAMN                PIC X(8)    VALUE LOW-VALUE.             
012500                                                                          
012600 01  MINX-ID.                                                             
012700     03 MINX-JOBBNAMN            PIC X(8)    VALUE LOW-VALUE.             
012800     03 MINX-EXECNAMN            PIC X(8)    VALUE LOW-VALUE.             
012900                                                                          
013000 01  MINL-ID.                                                             
013100     03 MINL-JOBBNAMN            PIC X(8)    VALUE LOW-VALUE.             
013200     03 MINL-DDNAMN              PIC X(8)    VALUE LOW-VALUE.             
013300     03 MINL-LISTNAMN            PIC X(12)   VALUE LOW-VALUE.             
013400                                                                          
013500     EJECT                                                                
013600 77  XIX                         PIC S9(9)   COMP VALUE ZERO.             
013700 77  XIX-MAX                     PIC S9(9)   COMP VALUE 500.              
013800 77  XIX-TOP                     PIC S9(9)   COMP VALUE ZERO.             
013900                                                                          
014000 77  LIX                         PIC S9(9)   COMP VALUE ZERO.             
014100 77  LIX-MAX                     PIC S9(9)   COMP VALUE 500.              
014200 77  LIX-TOP                     PIC S9(9)   COMP VALUE ZERO.             
014300                                                                          
014400 01  TAB.                                                                 
014500     03  TAB-EXECINFO        OCCURS 500.                                  
014600       05  TAB-EXECNAMN            PIC X(8).                              
014610       05  TAB-EXECTYP             PIC X(1).                              
014700     03  TAB-LISTINFO        OCCURS 500.                                  
014800       05  TAB-LISTNAMN            PIC X(12).                             
014900                                                                          
015000     EJECT                                                                
015100 01  OLDX-AREA-START             PIC X(24)   VALUE                        
015200                                 'OLDX-AREA-START  '.                     
015300     SKIP2                                                                
015400 01  OLDX-AREA.                                                           
015500     03  OLDX-ID.                                                         
015600       05  OLDX-JOBBNAMN         PIC X(8).                                
015700       05  OLDX-EXECNAMN         PIC X(8).                                
015800     03  OLDX-EXECTYP            PIC X(1).                                
015900     EJECT                                                                
016000 01  NEWX-AREA-START             PIC X(24)   VALUE                        
016100                                 'NEWX-AREA-START  '.                     
016200     SKIP2                                                                
016300 01  NEWX-AREA.                                                           
016400     03  NEWX-ID.                                                         
016500       05  NEWX-JOBBNAMN         PIC X(8).                                
016600       05  NEWX-EXECNAMN         PIC X(8).                                
016700     03  NEWX-EXECTYP            PIC X(1).                                
016800                                                                          
016900     EJECT                                                                
017000 01  OLDL-AREA-START             PIC X(24)   VALUE                        
017100                                 'OLDL-AREA-START  '.                     
017200     SKIP2                                                                
017300 01  OLDL-AREA.                                                           
017400     03   OLDL-ID.                                                        
017500       05  OLDL-JOBBNAMN         PIC X(8).                                
017600       05  OLDL-DDNAMN           PIC X(8).                                
017700       05  OLDL-LISTNAMN         PIC X(12).                               
017800     03  OLDL-LISTBESKR          PIC X(30).                               
017900                                                                          
018000     EJECT                                                                
018100 01  NEWL-AREA-START             PIC X(24)   VALUE                        
018200                                 'NEWL-AREA-START  '.                     
018300     SKIP2                                                                
018400 01  NEWL-AREA.                                                           
018500     03   NEWL-ID.                                                        
018600       05   NEWL-JOBBNAMN        PIC X(8).                                
018700       05   NEWL-DDNAMN          PIC X(8).                                
018800       05   NEWL-LISTNAMN        PIC X(12).                               
018900     03   NEWL-LISTBESKR         PIC X(30).                               
019000                                                                          
019100     EJECT                                                                
019200 01  UT-AREA-START               PIC X(24)   VALUE                        
019300                                 'UT-AREA-START  '.                       
019400     SKIP2                                                                
019500 01  UT-AREA.                                                             
019600     03   UT-JOBBNAMN            PIC X(8).                                
019700     03   UT-NAMN                PIC X(12).                               
019710     03   UT-EXECTYP             PIC X(1).                                
019800                                                                          
019900 01  UT-DMRRAD                   PIC X(80).                               
020000                                                                          
020100 01  WRADNR                      PIC 9(5).                                
020200 01  WTEXT                       PIC X(5).                                
020300                                                                          
020400     EJECT                                                                
020500 01  UT2-AREA-START              PIC X(24)   VALUE                        
020600                                 'UT2-AREA-START  '.                      
020700     SKIP2                                                                
020800 01  UT2-AREA.                                                            
020900     03   UT2-LISTNAMN           PIC X(12).                               
021000     03   UT2-LISTBESKR          PIC X(30).                               
021100     03   UT2-DDNAMN             PIC X(8).                                
021200     EJECT                                                                
021300 PROCEDURE DIVISION.                                                      
021400     SKIP2                                                                
021500                                                                          
021600     PERFORM A-INIT                                                       
021700                                                                          
021800     PERFORM S01-LAES-OX-WDMR42                                           
021900     PERFORM S02-LAES-NX-WDMR42                                           
022000     PERFORM S03-LAES-OL-WDMR40                                           
022100     PERFORM S04-LAES-NL-WDMR40                                           
022200                                                                          
022300     PERFORM S20-MINSTA-ID                                                
022400     MOVE MIN-JOBBNAMN TO SENASTE-JOBBNAMN                                
022500     SET JOBB-EJ-AENDRAT TO TRUE                                          
022600                                                                          
022700     PERFORM UNTIL MIN-JOBBNAMN = HIGH-VALUE                              
022800       IF MIN-JOBBNAMN = OLDX-JOBBNAMN OR NEWX-JOBBNAMN                   
022900                                                                          
023000         IF  NEWX-JOBBNAMN = MIN-JOBBNAMN                                 
023100           PERFORM B-SPARA-NEWX                                           
023200         END-IF                                                           
023300         IF  NEWX-ID NOT = OLDX-ID                                        
023400           SET JOBB-AENDRAT TO TRUE                                       
023500         END-IF                                                           
023600         IF  OLDX-ID <= MINX-ID                                           
023700           PERFORM S01-LAES-OX-WDMR42                                     
023800         END-IF                                                           
023900         IF  NEWX-ID <= MINX-ID                                           
024000           PERFORM S02-LAES-NX-WDMR42                                     
024100         END-IF                                                           
024200                                                                          
024300       END-IF                                                             
024400                                                                          
024500       IF MIN-JOBBNAMN = OLDL-JOBBNAMN OR NEWL-JOBBNAMN                   
024600                                                                          
024700         IF  NEWL-JOBBNAMN = MIN-JOBBNAMN                                 
024800           PERFORM C-SPARA-NEWL                                           
024900         END-IF                                                           
025000         IF  NEWL-ID NOT = OLDL-ID                                        
025100           SET JOBB-AENDRAT TO TRUE                                       
025200         END-IF                                                           
025300         IF  OLDL-ID <= MINL-ID                                           
025400           PERFORM S03-LAES-OL-WDMR40                                     
025500         END-IF                                                           
025600         IF  NEWL-ID <= MINL-ID                                           
025700           PERFORM E-SKRIV-KOMPLETTERAD-LIST-INFO                         
025800           PERFORM S04-LAES-NL-WDMR40                                     
025900         END-IF                                                           
026000                                                                          
026100       END-IF                                                             
026200                                                                          
026300       PERFORM S20-MINSTA-ID                                              
026400                                                                          
026500       IF MIN-JOBBNAMN > SENASTE-JOBBNAMN                                 
026600                                                                          
026700         IF JOBB-AENDRAT                                                  
026800           PERFORM D-SKRIV-JOBB-INFO                                      
026900           SET JOBB-EJ-AENDRAT TO TRUE                                    
027000         END-IF                                                           
027100         MOVE MIN-JOBBNAMN TO SENASTE-JOBBNAMN                            
027200         MOVE ZERO TO XIX-TOP LIX-TOP                                     
027300                                                                          
027400       END-IF                                                             
027500                                                                          
027600     END-PERFORM                                                          
027700                                                                          
027800     PERFORM Z-FINIT                                                      
027900                                                                          
028000     MOVE ZERO TO RETURN-CODE                                             
028100     GOBACK                                                               
028200     .                                                                    
028300     EJECT                                                                
028400 A-INIT SECTION.                                                          
028500                                                                          
028600     OPEN INPUT  OX-WDMR42                                                
028700                 NX-WDMR42                                                
028800                 OL-WDMR40                                                
028900                 NL-WDMR40                                                
029000                                                                          
029100     OPEN OUTPUT UT-WDMR43                                                
029200                 UT2-WDMR44                                               
029300     .                                                                    
029400     EJECT                                                                
029500 B-SPARA-NEWX  SECTION.                                                   
029600     SKIP2                                                                
029700     MOVE 1 TO XIX                                                        
029800     PERFORM UNTIL XIX > XIX-TOP OR                                       
029900       TAB-EXECNAMN (XIX) = NEWX-EXECNAMN                                 
030000       ADD 1 TO XIX                                                       
030100     END-PERFORM                                                          
030200     IF XIX > XIX-TOP                                                     
030300       IF XIX-TOP < XIX-MAX                                               
030400         ADD 1 TO XIX-TOP                                                 
030500         MOVE NEWX-EXECNAMN TO TAB-EXECNAMN (XIX-TOP)                     
030510         MOVE NEWX-EXECTYP  TO TAB-EXECTYP  (XIX-TOP)                     
030600       ELSE                                                               
030700         DISPLAY 'TOO MANY EXEC CARDS IN JOB ' SENASTE-JOBBNAMN           
030800       END-IF                                                             
030900     END-IF                                                               
031000     .                                                                    
031100     EJECT                                                                
031200 C-SPARA-NEWL  SECTION.                                                   
031300     SKIP2                                                                
031400     MOVE 1 TO LIX                                                        
031500     PERFORM UNTIL LIX > LIX-TOP OR                                       
031600       TAB-LISTNAMN (LIX) = NEWL-LISTNAMN                                 
031700       ADD 1 TO LIX                                                       
031800     END-PERFORM                                                          
031900     IF LIX > LIX-TOP                                                     
032000       IF LIX-TOP < LIX-MAX                                               
032100         ADD 1 TO LIX-TOP                                                 
032200         MOVE NEWL-LISTNAMN  TO TAB-LISTNAMN (LIX-TOP)                    
032300       ELSE                                                               
032400         DISPLAY 'TOO MANY REPORTS IN JOB ' SENASTE-JOBBNAMN              
032500       END-IF                                                             
032600     END-IF                                                               
032700     .                                                                    
032800     EJECT                                                                
032900 D-SKRIV-JOBB-INFO SECTION.                                               
033000     SKIP2                                                                
033100     MOVE SENASTE-JOBBNAMN TO UT-JOBBNAMN                                 
033200                                                                          
033300     IF XIX-TOP > ZERO OR LIX-TOP > ZERO                                  
033400                                                                          
033500       MOVE SPACE TO UT-DMRRAD                                            
033600       STRING 'ADD ' UT-JOBBNAMN '.'                                      
033700         DELIMITED BY SIZE                                                
033800         INTO UT-DMRRAD                                                   
033900       PERFORM S11-SKRIV-UT-WDMR43                                        
034000                                                                          
034400       MOVE 'JOB' TO UT-DMRRAD                                            
034500       PERFORM S11-SKRIV-UT-WDMR43                                        
034600                                                                          
034610       MOVE '.'   TO UT-DMRRAD                                            
034620       PERFORM S11-SKRIV-UT-WDMR43                                        
034630                                                                          
034700       MOVE SPACE TO UT-DMRRAD                                            
034800       STRING 'MODIFY ' UT-JOBBNAMN '.'                                   
034900         DELIMITED BY SIZE                                                
035000         INTO UT-DMRRAD                                                   
035100       PERFORM S11-SKRIV-UT-WDMR43                                        
035200                                                                          
035300       MOVE 'DELETE 90000 TO 99999 ' TO UT-DMRRAD                         
035400       PERFORM S11-SKRIV-UT-WDMR43                                        
035500       MOVE 90000 TO WRADNR                                               
035600       MOVE SPACE TO UT-DMRRAD                                            
035700       STRING WRADNR DELIMITED BY SIZE                                    
035800           INTO UT-DMRRAD                                                 
035900       PERFORM S11-SKRIV-UT-WDMR43                                        
036000                                                                          
036100       IF XIX-TOP > ZERO                                                  
036200         MOVE SPACE TO UT-DMRRAD                                          
036300         ADD 10 TO WRADNR                                                 
036400         STRING WRADNR ' CONTAINS' DELIMITED BY SIZE                      
036500           INTO UT-DMRRAD                                                 
036600         PERFORM S11-SKRIV-UT-WDMR43                                      
036700                                                                          
036800         MOVE SPACE TO WTEXT                                              
036900         MOVE 1 TO XIX                                                    
037000         PERFORM UNTIL XIX > XIX-TOP                                      
037100            MOVE TAB-EXECNAMN (XIX) TO UT-NAMN                            
037110            MOVE TAB-EXECTYP  (XIX) TO UT-EXECTYP                         
037200                                                                          
037300            ADD 10 TO WRADNR                                              
037400            MOVE SPACE TO UT-DMRRAD                                       
037410            IF UT-EXECTYP = 'P'                                           
037500              STRING WRADNR WTEXT DELIMITED BY SIZE                       
037510                UT-NAMN DELIMITED BY SPACE                                
037520                '-PGM' DELIMITED BY SIZE                                  
037700                INTO UT-DMRRAD                                            
037801            ELSE                                                          
037820              STRING                                                      
037821                WRADNR WTEXT    DELIMITED BY SIZE                         
037823                UT-NAMN         DELIMITED BY SPACE                        
037824                '-PROC' DELIMITED BY SIZE                                 
037830                INTO UT-DMRRAD                                            
037831            END-IF                                                        
037840            PERFORM S11-SKRIV-UT-WDMR43                                   
037900                                                                          
038000            MOVE '    ,' TO WTEXT                                         
038100            ADD 1 TO XIX                                                  
038200         END-PERFORM                                                      
038300                                                                          
038400       END-IF                                                             
038500                                                                          
038600       IF LIX-TOP > ZERO                                                  
038700                                                                          
038800         ADD 10 TO WRADNR                                                 
038900         MOVE SPACE TO UT-DMRRAD                                          
039000         STRING WRADNR DELIMITED BY SIZE                                  
039100             INTO UT-DMRRAD                                               
039200         PERFORM S11-SKRIV-UT-WDMR43                                      
039300         ADD 10 TO WRADNR                                                 
039400         MOVE SPACE TO UT-DMRRAD                                          
039500         STRING WRADNR ' OUTPUTS' DELIMITED BY SIZE                       
039600             INTO UT-DMRRAD                                               
039700         PERFORM S11-SKRIV-UT-WDMR43                                      
039800                                                                          
039900         MOVE SPACE TO WTEXT                                              
040000         MOVE 1 TO LIX                                                    
040100         PERFORM UNTIL LIX > LIX-TOP                                      
040200            MOVE TAB-LISTNAMN (LIX)  TO UT-NAMN                           
040300                                                                          
040400            ADD 10 TO WRADNR                                              
040500            MOVE SPACE TO UT-DMRRAD                                       
040600            STRING WRADNR WTEXT '"' DELIMITED BY SIZE                     
040700              UT-NAMN DELIMITED BY SPACE                                  
040800              '"'     DELIMITED BY SIZE                                   
040900              INTO UT-DMRRAD                                              
041000            PERFORM S11-SKRIV-UT-WDMR43                                   
041100                                                                          
041200            MOVE '    ,' TO WTEXT                                         
041300            ADD 1 TO LIX                                                  
041400         END-PERFORM                                                      
041500       END-IF                                                             
041600                                                                          
041700       MOVE '.' TO UT-DMRRAD                                              
041800       PERFORM S11-SKRIV-UT-WDMR43                                        
041900     END-IF                                                               
042000                                                                          
042100     IF XIX-TOP = ZERO AND LIX-TOP = ZERO                                 
042200       MOVE SPACE   TO UT-NAMN                                            
042300                                                                          
042400       MOVE SPACE   TO UT-DMRRAD                                          
042801                                                                          
042802       STRING 'REMOVE "' DELIMITED BY SIZE                                
042803           UT-JOBBNAMN DELIMITED BY SPACE                                 
042804           '".' DELIMITED BY SIZE                                         
042805           INTO UT-DMRRAD                                                 
042806       PERFORM S11-SKRIV-UT-WDMR43                                        
042807                                                                          
043000     END-IF                                                               
043100     .                                                                    
043200     EJECT                                                                
043300 E-SKRIV-KOMPLETTERAD-LIST-INFO SECTION.                                  
043400     SKIP2                                                                
043500     MOVE NEWL-LISTNAMN    TO UT2-LISTNAMN                                
043600     MOVE NEWL-LISTBESKR   TO UT2-LISTBESKR                               
043700     MOVE NEWL-DDNAMN      TO UT2-DDNAMN                                  
043800     PERFORM S12-SKRIV-UT2-WDMR44                                         
043900     .                                                                    
044000     EJECT                                                                
044100 Z-FINIT SECTION.                                                         
044200     SKIP2                                                                
044300     CLOSE OX-WDMR42                                                      
044400           NX-WDMR42                                                      
044500           OL-WDMR40                                                      
044600           NL-WDMR40                                                      
044700           UT-WDMR43                                                      
044800           UT2-WDMR44                                                     
044900     .                                                                    
045000     EJECT                                                                
045100 S01-LAES-OX-WDMR42 SECTION.                                              
045200     SKIP2                                                                
045300     READ OX-WDMR42 INTO OLDX-AREA                                        
045400     AT END                                                               
045500        MOVE HIGH-VALUE TO OLDX-JOBBNAMN                                  
045600        SET END-OF-OX-WDMR42 TO TRUE                                      
045700     END-READ                                                             
045800     .                                                                    
045900     EJECT                                                                
046000 S02-LAES-NX-WDMR42 SECTION.                                              
046100     SKIP2                                                                
046200     READ NX-WDMR42 INTO NEWX-AREA                                        
046300     AT END                                                               
046400        MOVE HIGH-VALUE TO NEWX-JOBBNAMN                                  
046500        SET END-OF-NX-WDMR42 TO TRUE                                      
046600     .                                                                    
046700     EJECT                                                                
046800 S03-LAES-OL-WDMR40 SECTION.                                              
046900     SKIP2                                                                
047000     READ OL-WDMR40 INTO OLDL-AREA                                        
047100     AT END                                                               
047200        MOVE HIGH-VALUE TO OLDL-JOBBNAMN                                  
047300        SET END-OF-OL-WDMR40 TO TRUE                                      
047400     NOT AT END                                                           
047500        IF OLDL-DDNAMN = SPACE                                            
047600          MOVE SPARO-DDNAMN TO OLDL-DDNAMN                                
047700        END-IF                                                            
047800        MOVE OLDL-DDNAMN TO SPARO-DDNAMN                                  
047900     END-READ                                                             
048000     .                                                                    
048100     EJECT                                                                
048200 S04-LAES-NL-WDMR40 SECTION.                                              
048300     SKIP2                                                                
048400     READ NL-WDMR40 INTO NEWL-AREA                                        
048500     AT END                                                               
048600        MOVE HIGH-VALUE TO NEWL-JOBBNAMN                                  
048700        SET END-OF-NL-WDMR40 TO TRUE                                      
048800     NOT AT END                                                           
048900        IF NEWL-DDNAMN = SPACE                                            
049000          MOVE SPARN-DDNAMN TO NEWL-DDNAMN                                
049100        END-IF                                                            
049200        MOVE NEWL-DDNAMN TO SPARN-DDNAMN                                  
049300     END-READ                                                             
049400     .                                                                    
049500     EJECT                                                                
049600 S11-SKRIV-UT-WDMR43 SECTION.                                             
049700     SKIP2                                                                
049800     WRITE UT-POST FROM UT-DMRRAD                                         
049900     .                                                                    
050000                                                                          
050100     EJECT                                                                
050200 S12-SKRIV-UT2-WDMR44 SECTION.                                            
050300     SKIP2                                                                
050400     WRITE UT2-POST FROM UT2-AREA                                         
050500     .                                                                    
050600                                                                          
050700     EJECT                                                                
050800 S20-MINSTA-ID    SECTION.                                                
050900     SKIP2                                                                
051000                                                                          
051100     MOVE OLDX-ID TO MINX-ID                                              
051200     IF NEWX-ID < MINX-ID                                                 
051300       MOVE NEWX-ID TO MINX-ID                                            
051400     END-IF                                                               
051500                                                                          
051600     MOVE OLDL-ID TO MINL-ID                                              
051700     IF NEWL-ID < MINL-ID                                                 
051800       MOVE NEWL-ID TO MINL-ID                                            
051900     END-IF                                                               
052000                                                                          
052100     MOVE MINX-JOBBNAMN TO MIN-JOBBNAMN                                   
052200     IF MINL-JOBBNAMN < MIN-JOBBNAMN                                      
052300       MOVE MINL-JOBBNAMN TO MIN-JOBBNAMN                                 
052400     END-IF                                                               
052500     .                                                                    
