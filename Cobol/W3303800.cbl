000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W3303800.                                                
000400*AUTHOR.         RONNY STENHOLM.                                          
000500*DATE-WRITTEN.   93/12/30.                                                
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        SPLITTA STATISTIKEN MELLAN MARKNADSBOLAGEN                       
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
002300     SKIP2                                                                
002400*          --- STATISTIK TILL ALLA MB                                     
002500     SELECT W33038                     ASSIGN TO W33038D1.                
002600     SKIP2                                                                
002700*          --- MB A >>>>  SVERIGE                                         
002800     SELECT W3303A                     ASSIGN TO W33038D2.                
002900     SKIP2                                                                
003000*          --- MB B >>>>  VCEM                                            
003100     SELECT W3303B                     ASSIGN TO W33038D3.                
003200     SKIP2                                                                
003300*          --- MB C >>>>  ASIA PACFIC                                     
003400     SELECT W3303C                     ASSIGN TO W33038D4.                
003500     SKIP2                                                                
003600*          --- MB D >>>>  VCSA SOUTH AMERICA                              
003700     SELECT W3303D                     ASSIGN TO W33038D5.                
003800     SKIP2                                                                
003900*          --- MB E >>>>  VCNA                                            
004000     SELECT W3303E                     ASSIGN TO W33038D6.                
004100     SKIP2                                                                
004110*          --- MB F >>>>  VCAS                                            
004120     SELECT W3303F                     ASSIGN TO W33038D8.                
004130     SKIP2                                                                
004200*          --- MB G >>>>  VCI                                             
004300     SELECT W3303G                     ASSIGN TO W33038D7.                
004400     SKIP2                                                                
004800*          --- MB A >>>>  1:A POST TILL VCOM.                             
004900     SELECT W3304A                     ASSIGN TO W33038D9.                
005000     SKIP2                                                                
005100*          --- MB B >>>>  1:A POST TILL VCOM.                             
005200     SELECT W3304B                     ASSIGN TO W33038DA.                
005300     SKIP2                                                                
005400*          --- MB C >>>>  1:A POST TILL VCOM.                             
005500     SELECT W3304C                     ASSIGN TO W33038DB.                
005600     SKIP2                                                                
005700*          --- MB D >>>>  1:A POST TILL VCOM.                             
005800     SELECT W3304D                     ASSIGN TO W33038DC.                
005900     SKIP2                                                                
006000*          --- MB E >>>>  1:A POST TILL VCOM.                             
006100     SELECT W3304E                     ASSIGN TO W33038DD.                
006200     SKIP2                                                                
006300*          --- MB F >>>>  1:A POST TILL VCOM.                             
006400     SELECT W3304F                     ASSIGN TO W33038DF.                
006500     SKIP2                                                                
006510*          --- MB G >>>>  1:A POST TILL VCOM.                             
006520     SELECT W3304G                     ASSIGN TO W33038DE.                
006530     SKIP2                                                                
006600 DATA DIVISION.                                                           
006700     SKIP3                                                                
006800 FILE SECTION.                                                            
006900     SKIP3                                                                
007000 FD  W33038                                                               
007100     RECORDING       F                                                    
007200     BLOCK CONTAINS  0.                                                   
007300     SKIP2                                                                
007400*01  -COPY W33038      -L.                                                
007500     SKIP3                                                                
007600 FD  W3303A                                                               
007700     RECORDING       F                                                    
007800     BLOCK CONTAINS  0.                                                   
007900     SKIP2                                                                
008000*01  POST -COPY W33038 -PRE  A-  -L.                                      
008100     SKIP3                                                                
008200 FD  W3303B                                                               
008300     RECORDING       F                                                    
008400     BLOCK CONTAINS  0.                                                   
008500     SKIP2                                                                
008600*01  POST -COPY W33038 -PRE  B-  -L.                                      
008700     SKIP3                                                                
008800 FD  W3303C                                                               
008900     RECORDING       F                                                    
009000     BLOCK CONTAINS  0.                                                   
009100     SKIP2                                                                
009200*01  POST -COPY W33038 -PRE  C-  -L.                                      
009300     SKIP3                                                                
009400 FD  W3303D                                                               
009500     RECORDING       F                                                    
009600     BLOCK CONTAINS  0.                                                   
009700     SKIP2                                                                
009800*01  POST -COPY W33038 -PRE  D-  -L.                                      
009900     SKIP3                                                                
010000 FD  W3303E                                                               
010100     RECORDING       F                                                    
010200     BLOCK CONTAINS  0.                                                   
010300     SKIP2                                                                
010400*01  POST -COPY W33038 -PRE  E-  -L.                                      
010500     SKIP3                                                                
010600 FD  W3303F                                                               
010700     RECORDING       F                                                    
010800     BLOCK CONTAINS  0.                                                   
010900     SKIP2                                                                
011000*01  POST -COPY W33038 -PRE  F-  -L.                                      
011100     SKIP3                                                                
011200 FD  W3303G                                                               
011300     RECORDING       F                                                    
011400     BLOCK CONTAINS  0.                                                   
011500     SKIP2                                                                
011600*01  POST -COPY W33038 -PRE  G-  -L.                                      
011700     SKIP3                                                                
011800 FD  W3304A                                                               
011900     RECORDING       F                                                    
012000     BLOCK CONTAINS  0.                                                   
012100     SKIP2                                                                
012200*01  POST -COPY W330380A -PRE ANTAL-A-  -L.                               
012300     SKIP3                                                                
012400 FD  W3304B                                                               
012500     RECORDING       F                                                    
012600     BLOCK CONTAINS  0.                                                   
012700     SKIP2                                                                
012800*01  POST -COPY W330380A -PRE ANTAL-B-  -L.                               
012900     SKIP3                                                                
013000 FD  W3304C                                                               
013100     RECORDING       F                                                    
013200     BLOCK CONTAINS  0.                                                   
013300     SKIP2                                                                
013400*01  POST -COPY W330380A -PRE ANTAL-C-  -L.                               
013500     SKIP3                                                                
013600 FD  W3304D                                                               
013700     RECORDING       F                                                    
013800     BLOCK CONTAINS  0.                                                   
013900     SKIP2                                                                
014000*01  POST -COPY W330380A -PRE ANTAL-D-  -L.                               
014100     SKIP3                                                                
014200 FD  W3304E                                                               
014300     RECORDING       F                                                    
014400     BLOCK CONTAINS  0.                                                   
014500     SKIP2                                                                
014600*01  POST -COPY W330380A -PRE ANTAL-E-  -L.                               
014700     SKIP3                                                                
014800 FD  W3304F                                                               
014900     RECORDING       F                                                    
015000     BLOCK CONTAINS  0.                                                   
015100     SKIP2                                                                
015200*01  POST -COPY W330380A -PRE ANTAL-F-  -L.                               
015300     SKIP3                                                                
015310 FD  W3304G                                                               
015320     RECORDING       F                                                    
015330     BLOCK CONTAINS  0.                                                   
015340     SKIP2                                                                
015350*01  POST -COPY W330380A -PRE ANTAL-G-  -L.                               
015360     SKIP3                                                                
015400 WORKING-STORAGE SECTION.                                                 
015500     SKIP2                                                                
015501                                                                          
015510*    -- CHECKED BY WY2000                                                 
015600 77  IDPGM                       PIC X(8)    VALUE 'W3303800'.            
015700 77  JA                          PIC X       VALUE 'J'.                   
015800 77  NEJ                         PIC X       VALUE 'N'.                   
015900 77  A-RAKNARE                   PIC S9(7)   COMP SYNC VALUE ZERO.        
016000 77  B-RAKNARE                   PIC S9(7)   COMP SYNC VALUE ZERO.        
016100 77  C-RAKNARE                   PIC S9(7)   COMP SYNC VALUE ZERO.        
016200 77  D-RAKNARE                   PIC S9(7)   COMP SYNC VALUE ZERO.        
016300 77  E-RAKNARE                   PIC S9(7)   COMP SYNC VALUE ZERO.        
016310 77  F-RAKNARE                   PIC S9(7)   COMP SYNC VALUE ZERO.        
016400 77  G-RAKNARE                   PIC S9(7)   COMP SYNC VALUE ZERO.        
016500                                                                          
016600 77  W33038-EOF-SW               PIC X       VALUE 'N'.                   
016700     88  END-OF-W33038                       VALUE 'J'.                   
016800     EJECT                                                                
016900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
017000 01  FILLER REDEFINES DAGENS-DATUM.                                       
017100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
017200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
017300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
017400     EJECT                                                                
017500 01  DYNAMISKA-SUBPROGRAM.                                                
017600*                                                                         
017700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
017800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
017900     SKIP2                                                                
018000*    --- PARAMETRAR TILL ABEND                                            
018100                                                                          
018200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
018300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
018400     SKIP2                                                                
018500 01  FELTEXT.                                                             
018600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
018700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
018800     EJECT                                                                
018900*    --- PARAMETRAR TILL POSTSUM                                          
019000*                                                                         
019100*01  -COPY W0005   -PRE  POSTSUM-                                         
019200     EJECT                                                                
019300 01  IN-AREA-START               PIC X(24)   VALUE                        
019400                                 'IN-AREA-START  '.                       
019500     SKIP2                                                                
019600                                                                          
019700*01  AREA -COPY W33038     -PRE IN-                                       
019800     EJECT                                                                
019900 01  UT-AREA-START                PIC X(24)   VALUE                       
020000                                 'UT-AREA-START  '.                       
020100     SKIP2                                                                
020200                                                                          
020300*01  AREA -COPY W33038     -PRE UT-                                       
020400     EJECT                                                                
020500 01  UT-ANTAL-START                PIC X(24)   VALUE                      
020600                                 'UT-ANTAL-AREA-START  '.                 
020700     SKIP2                                                                
020800                                                                          
020900*01  AREA -COPY W330380A    -PRE ANTAL-                                   
021000     EJECT                                                                
021100 PROCEDURE DIVISION.                                                      
021200     SKIP2                                                                
021300                                                                          
021400     PERFORM A-INIT                                                       
021500     PERFORM S01-LAES-W33038                                              
021600     PERFORM UNTIL END-OF-W33038                                          
021700       MOVE IN-AREA TO UT-AREA                                            
021800       EVALUATE IN-BEST-IDMARKBO                                          
021900                                                                          
022000         WHEN 'A' PERFORM S11-SKRIV-W3303A                                
022100                  ADD +1 TO A-RAKNARE                                     
022200                                                                          
022300         WHEN 'B' PERFORM S12-SKRIV-W3303B                                
022400                  ADD +1 TO B-RAKNARE                                     
022500                                                                          
022600         WHEN 'C' PERFORM S13-SKRIV-W3303C                                
022700                  ADD +1 TO C-RAKNARE                                     
022800                                                                          
022900         WHEN 'D' PERFORM S14-SKRIV-W3303D                                
023000                  ADD +1 TO D-RAKNARE                                     
023100                                                                          
023200         WHEN 'E' PERFORM S15-SKRIV-W3303E                                
023300                  ADD +1 TO E-RAKNARE                                     
023400                                                                          
023500         WHEN 'G' PERFORM S16-SKRIV-W3303G                                
023600                  ADD +1 TO G-RAKNARE                                     
023601                                                                          
023610         WHEN 'F' PERFORM S17-SKRIV-W3303F                                
023620                  ADD +1 TO F-RAKNARE                                     
023700                                                                          
024100       END-EVALUATE                                                       
024200       PERFORM S01-LAES-W33038                                            
024300     END-PERFORM                                                          
024400     PERFORM B-SKRIV-ANTALS-FILERNA                                       
024500                                                                          
024600     PERFORM Z-FINIT                                                      
024700                                                                          
024800     MOVE ZERO TO RETURN-CODE                                             
024900     GOBACK                                                               
025000     .                                                                    
025100     EJECT                                                                
025200 A-INIT SECTION.                                                          
025300                                                                          
025400     OPEN INPUT  W33038                                                   
025500                                                                          
025600     OPEN OUTPUT W3303A      W3304A                                       
025700                 W3303B      W3304B                                       
025800                 W3303C      W3304C                                       
025900                 W3303D      W3304D                                       
026000                 W3303E      W3304E                                       
026100                 W3303F      W3304F                                       
026110                 W3303G      W3304G                                       
026200                                                                          
026300     SKIP2                                                                
026400     ACCEPT DAGENS-DATUM  FROM DATE                                       
026500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
026600     .                                                                    
026700     EJECT                                                                
026800 B-SKRIV-ANTALS-FILERNA  SECTION.                                         
026900     MOVE '380'     TO ANTAL-IDPTYP                                       
027000     MOVE  'A'      TO ANTAL-IDVTYP                                       
027100                                                                          
027200                                                                          
027300     IF A-RAKNARE NOT = ZERO                                              
027400       MOVE A-RAKNARE TO ANTAL-KVPOST                                     
027500       PERFORM S21-SKRIV-W3304A                                           
027600     END-IF                                                               
027610     IF B-RAKNARE NOT = ZERO                                              
027620       MOVE B-RAKNARE TO ANTAL-KVPOST                                     
027630       PERFORM S22-SKRIV-W3304B                                           
027640     END-IF                                                               
027650     IF C-RAKNARE NOT = ZERO                                              
027660       MOVE C-RAKNARE TO ANTAL-KVPOST                                     
027670       PERFORM S23-SKRIV-W3304C                                           
027680     END-IF                                                               
027690     IF D-RAKNARE NOT = ZERO                                              
027691       MOVE D-RAKNARE TO ANTAL-KVPOST                                     
027692       PERFORM S24-SKRIV-W3304D                                           
027693     END-IF                                                               
027694     IF E-RAKNARE NOT = ZERO                                              
027695       MOVE E-RAKNARE TO ANTAL-KVPOST                                     
027696       PERFORM S25-SKRIV-W3304E                                           
027697     END-IF                                                               
027698     IF F-RAKNARE NOT = ZERO                                              
027699       MOVE F-RAKNARE TO ANTAL-KVPOST                                     
027700       PERFORM S27-SKRIV-W3304F                                           
027701     END-IF                                                               
027702     IF G-RAKNARE NOT = ZERO                                              
027703       MOVE G-RAKNARE TO ANTAL-KVPOST                                     
027704       PERFORM S26-SKRIV-W3304G                                           
027705     END-IF                                                               
027710     SKIP2                                                                
027800     .                                                                    
027900     EJECT                                                                
028000 Z-FINIT SECTION.                                                         
028100     CLOSE W33038                                                         
028200           W3303A      W3304A                                             
028300           W3303B      W3304B                                             
028400           W3303C      W3304C                                             
028500           W3303D      W3304D                                             
028600           W3303E      W3304E                                             
028700           W3303F      W3304F                                             
028710           W3303G      W3304G                                             
028800     SKIP2                                                                
028900     MOVE 'S' TO POSTSUM-OPKOD                                            
029000     CALL POSTSUM USING POSTSUM-PARM                                      
029100     .                                                                    
029200     EJECT                                                                
029300 S01-LAES-W33038  SECTION.                                                
029400     SKIP2                                                                
029500     READ W33038 INTO IN-AREA                                             
029600     AT END                                                               
029700        SET END-OF-W33038 TO TRUE                                         
029800                                                                          
029900     NOT AT END                                                           
030000        MOVE 'W33038'   TO POSTSUM-FDNAMN                                 
030100        MOVE 'W33038D1' TO POSTSUM-DDNAMN2                                
030200        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
030300        CALL POSTSUM USING POSTSUM-PARM                                   
030400     END-READ                                                             
030500     .                                                                    
030600     EJECT                                                                
030700 S11-SKRIV-W3303A SECTION.                                                
030800     SKIP2                                                                
030900     WRITE A-POST FROM UT-AREA                                            
031000                                                                          
031100     MOVE 'A'      TO POSTSUM-TRANSTYP                                    
031200     MOVE 'W3303A' TO POSTSUM-FDNAMN                                      
031300     MOVE 'W33038D2' TO POSTSUM-DDNAMN2                                   
031400     CALL POSTSUM USING POSTSUM-PARM                                      
031500     .                                                                    
031600     EJECT                                                                
031700 S12-SKRIV-W3303B SECTION.                                                
031800     SKIP2                                                                
031900     WRITE B-POST FROM UT-AREA                                            
032000                                                                          
032100     MOVE 'B'      TO POSTSUM-TRANSTYP                                    
032200     MOVE 'W3303B' TO POSTSUM-FDNAMN                                      
032300     MOVE 'W33038D3' TO POSTSUM-DDNAMN2                                   
032400     CALL POSTSUM USING POSTSUM-PARM                                      
032500     .                                                                    
032600     EJECT                                                                
032700 S13-SKRIV-W3303C SECTION.                                                
032800     SKIP2                                                                
032900     WRITE C-POST FROM UT-AREA                                            
033000                                                                          
033100     MOVE 'C'      TO POSTSUM-TRANSTYP                                    
033200     MOVE 'W3303C' TO POSTSUM-FDNAMN                                      
033300     MOVE 'W33038D4' TO POSTSUM-DDNAMN2                                   
033400     CALL POSTSUM USING POSTSUM-PARM                                      
033500     .                                                                    
033600     EJECT                                                                
033700 S14-SKRIV-W3303D SECTION.                                                
033800     SKIP2                                                                
033900     WRITE D-POST FROM UT-AREA                                            
034000                                                                          
034100     MOVE 'D'      TO POSTSUM-TRANSTYP                                    
034200     MOVE 'W3303E' TO POSTSUM-FDNAMN                                      
034300     MOVE 'W33038D5' TO POSTSUM-DDNAMN2                                   
034400     CALL POSTSUM USING POSTSUM-PARM                                      
034500     .                                                                    
034600     EJECT                                                                
034700 S15-SKRIV-W3303E SECTION.                                                
034800     SKIP2                                                                
034900     WRITE E-POST FROM UT-AREA                                            
035000                                                                          
035100     MOVE 'E'      TO POSTSUM-TRANSTYP                                    
035200     MOVE 'W3303E' TO POSTSUM-FDNAMN                                      
035300     MOVE 'W33038D6' TO POSTSUM-DDNAMN2                                   
035400     CALL POSTSUM USING POSTSUM-PARM                                      
035500     .                                                                    
035600     EJECT                                                                
035700 S16-SKRIV-W3303G SECTION.                                                
035800     SKIP2                                                                
035900     WRITE G-POST FROM UT-AREA                                            
036000                                                                          
036100     MOVE 'G'      TO POSTSUM-TRANSTYP                                    
036200     MOVE 'W3303G' TO POSTSUM-FDNAMN                                      
036300     MOVE 'W33038D7' TO POSTSUM-DDNAMN2                                   
036400     CALL POSTSUM USING POSTSUM-PARM                                      
036500     .                                                                    
036510 S17-SKRIV-W3303F SECTION.                                                
036520     SKIP2                                                                
036530     WRITE F-POST FROM UT-AREA                                            
036540                                                                          
036550     MOVE 'F'      TO POSTSUM-TRANSTYP                                    
036560     MOVE 'W3303F' TO POSTSUM-FDNAMN                                      
036570     MOVE 'W33038D8' TO POSTSUM-DDNAMN2                                   
036580     CALL POSTSUM USING POSTSUM-PARM                                      
036590     .                                                                    
036600 S21-SKRIV-W3304A SECTION.                                                
036700     SKIP2                                                                
036800     WRITE ANTAL-A-POST FROM ANTAL-AREA                                   
036900                                                                          
037000     MOVE 'A'      TO POSTSUM-TRANSTYP                                    
037100     MOVE 'W3304A' TO POSTSUM-FDNAMN                                      
037200     MOVE 'W33038D9' TO POSTSUM-DDNAMN2                                   
037300     CALL POSTSUM USING POSTSUM-PARM                                      
037400     .                                                                    
037500     EJECT                                                                
037600 S22-SKRIV-W3304B SECTION.                                                
037700     SKIP2                                                                
037800     WRITE ANTAL-B-POST FROM ANTAL-AREA                                   
037900                                                                          
038000     MOVE 'B'      TO POSTSUM-TRANSTYP                                    
038100     MOVE 'W3304B' TO POSTSUM-FDNAMN                                      
038200     MOVE 'W33038DA' TO POSTSUM-DDNAMN2                                   
038300     CALL POSTSUM USING POSTSUM-PARM                                      
038400     .                                                                    
038500     EJECT                                                                
038600 S23-SKRIV-W3304C SECTION.                                                
038700     SKIP2                                                                
038800     WRITE ANTAL-C-POST FROM ANTAL-AREA                                   
038900                                                                          
039000     MOVE 'C'      TO POSTSUM-TRANSTYP                                    
039100     MOVE 'W3304C' TO POSTSUM-FDNAMN                                      
039200     MOVE 'W33038DB' TO POSTSUM-DDNAMN2                                   
039300     CALL POSTSUM USING POSTSUM-PARM                                      
039400     .                                                                    
039500     EJECT                                                                
039600 S24-SKRIV-W3304D SECTION.                                                
039700     SKIP2                                                                
039800     WRITE ANTAL-D-POST FROM ANTAL-AREA                                   
039900                                                                          
040000     MOVE 'D'      TO POSTSUM-TRANSTYP                                    
040100     MOVE 'W3304E' TO POSTSUM-FDNAMN                                      
040200     MOVE 'W33038DC' TO POSTSUM-DDNAMN2                                   
040300     CALL POSTSUM USING POSTSUM-PARM                                      
040400     .                                                                    
040500     EJECT                                                                
040600 S25-SKRIV-W3304E SECTION.                                                
040700     SKIP2                                                                
040800     WRITE ANTAL-E-POST FROM ANTAL-AREA                                   
040900                                                                          
041000     MOVE 'E'      TO POSTSUM-TRANSTYP                                    
041100     MOVE 'W3304E' TO POSTSUM-FDNAMN                                      
041200     MOVE 'W33038DD' TO POSTSUM-DDNAMN2                                   
041300     CALL POSTSUM USING POSTSUM-PARM                                      
041400     .                                                                    
041500     EJECT                                                                
041600 S26-SKRIV-W3304G SECTION.                                                
041700     SKIP2                                                                
041800     WRITE ANTAL-G-POST FROM ANTAL-AREA                                   
041900                                                                          
042000     MOVE 'G'      TO POSTSUM-TRANSTYP                                    
042100     MOVE 'W3304G' TO POSTSUM-FDNAMN                                      
042200     MOVE 'W33038DE' TO POSTSUM-DDNAMN2                                   
042300     CALL POSTSUM USING POSTSUM-PARM                                      
042400     .                                                                    
042500 S27-SKRIV-W3304F SECTION.                                                
042600     SKIP2                                                                
042700     WRITE ANTAL-F-POST FROM ANTAL-AREA                                   
042800                                                                          
042900     MOVE 'F'      TO POSTSUM-TRANSTYP                                    
043000     MOVE 'W3304F' TO POSTSUM-FDNAMN                                      
043100     MOVE 'W33038DF' TO POSTSUM-DDNAMN2                                   
043200     CALL POSTSUM USING POSTSUM-PARM                                      
043300     .                                                                    
