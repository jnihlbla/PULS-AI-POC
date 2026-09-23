000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W3354400.                                                
000400 AUTHOR.         RONNY STENHOLM.                                          
000500 DATE-WRITTEN.   96/10/28.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PGM ÄR SNARLIKT W33566                                           
001100*        PGMMET RENSAR ALLA ARTIKLAR SOM INTE FINNS PÅ NÅGOT              
001200*        NDC DET BERÄKNAR OCKSÅ DEN LÄMPLIGASTE "SJÄLVKOSTEN".            
001600*        DET SOM SKILJER ÄR ATT DET INTE FINNS NÅGON                      
001700*        ANTALSFIL I W33566.                                              
001900*                                                                         
002000*ETRACKER 3449719 ÄNDRING 060710/EÖ                                       
002001*                 USA SKA EJ LÄNGRE HA LOKAL SJÄLVKOST PÅ SINA            
002002*                 LOKALA ARTIKLAR.                                        
002010*                                                                         
002100*    ABENDKODER:                                                          
002200*        U0016 -  . . . .                                                 
002300*        U1000 -  . . . .                                                 
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     SKIP2                                                                
003600*          --- URVALET FRÅN 91042                                         
003700     SELECT W33542                     ASSIGN TO W33544D1.                
003800     SKIP2                                                                
004200*          --- ANTAL I INFO FIL (EN REC)                                  
004300     SELECT W33543                     ASSIGN TO W33544D2.                
004400     SKIP2                                                                
004610*          --- RENSAD PÅ ICKE VCNA ARTIKLAR                               
004620     SELECT W33544                     ASSIGN TO W33544D3.                
004700     EJECT                                                                
004800 DATA DIVISION.                                                           
004900     SKIP3                                                                
005000 FILE SECTION.                                                            
005100     SKIP3                                                                
005800 FD  W33542                                                               
005900     RECORDING       V                                                    
006000     BLOCK CONTAINS  0.                                                   
006100                                                                          
006200*01  -COPY W335401A      -L.                                              
006300                                                                          
006400*01  -COPY W335402A      -L.                                              
006500                                                                          
006600*01  -COPY W335403A      -L.                                              
006700     SKIP3                                                                
006800 FD  W33544                                                               
006900     RECORDING       V                                                    
007000     BLOCK CONTAINS  0.                                                   
007100                                                                          
007200*01  POST -COPY W335404A -PRE  UT1-     -L.                               
007300                                                                          
007400*01  POST -COPY W335402A -PRE  UT2-     -L.                               
007500                                                                          
007600*01  POST -COPY W335403A -PRE  UT3-     -L.                               
007700     SKIP3                                                                
007800 FD  W33543                                                               
007900     RECORDING       F                                                    
008000     BLOCK CONTAINS  0.                                                   
008100                                                                          
008200*01  POST -COPY W335400A -PRE  ANTAL-  -L.                                
008900     EJECT                                                                
009000 WORKING-STORAGE SECTION.                                                 
009100                                                                          
009101                                                                          
009110*    -- CHECKED BY WY2000                                                 
009200 77  IDPGM                       PIC X(8)    VALUE 'W3354400'.            
009300 77  JA                          PIC X       VALUE 'J'.                   
009400 77  NEJ                         PIC X       VALUE 'N'.                   
009500 77  WS-RAKNARE                  PIC S9(7) VALUE ZERO.                    
009700                                                                          
010400 77  W33542-EOF-SW               PIC X       VALUE 'N'.                   
010500     88  END-OF-W33542                       VALUE 'J'.                   
010600     EJECT                                                                
010610                                                                          
010640 77  WS-IDMARKBO                 PIC X      VALUE SPACE.                  
010660 77  WS-KDVALISO-COST            PIC X(3)   VALUE 'SEK'.                  
010661 77  WS-DATUM                    PIC 9(6)   VALUE ZERO.                   
010670                                                                          
010700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
010800 01  FILLER REDEFINES DAGENS-DATUM.                                       
010900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
011000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
011100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
011110 01  WS-DAGENS-DATUM             PIC 9(8)    VALUE ZERO.                  
011120 01  WS-PRISDATUM                PIC 9(8)    VALUE ZERO.                  
011200     EJECT                                                                
011300 01  DYNAMISKA-SUBPROGRAM.                                                
011400*                                                                         
011500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
011510     03  FELLOG                  PIC X(8)    VALUE 'FELLOG'.              
011600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
011700     SKIP2                                                                
011800*    --- PARAMETRAR TILL ABEND                                            
011900                                                                          
012000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
012100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
012200     SKIP2                                                                
012300 01  FELTEXT.                                                             
012400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
012500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
012600     EJECT                                                                
012700*    --- PARAMETRAR TILL POSTSUM                                          
012800*                                                                         
012900*01  -COPY W0005   -PRE  POSTSUM-                                         
013000     EJECT                                                                
013800     EJECT                                                                
014700 01  INFO-AREA-START             PIC X(24)   VALUE                        
014800                                 'INFO-AREA-START  '.                     
014900     SKIP2                                                                
015000 01  INFO-AREA.                                                           
015100     03  INFO-AREA-0.                                                     
015200       05  INFO-IDPTYP           PIC X(3).                                
015300       05  FILLER                PIC X(400).                              
015400*   03  FILLER -COPY W335401A  -PRE INFO1-  -RED  INFO-AREA-0             
015500*   03  FILLER -COPY W335402A  -PRE INFO2-  -RED  INFO-AREA-0             
015600*   03  FILLER -COPY W335403A  -PRE INFO3-  -RED  INFO-AREA-0             
015700     EJECT                                                                
015800 01  UTINFO-AREA-START           PIC X(24)   VALUE                        
015900                                 'UTINFO-AREA-START  '.                   
016000     SKIP2                                                                
016100 01  UTINFO-AREA.                                                         
016200     03  UTINFO-AREA-0.                                                   
016300       05  UTINFO-IDPTYP         PIC X(3).                                
016400       05  FILLER                PIC X(400).                              
016500*   03  FILLER -COPY W335404A  -PRE UT1-  -RED  UTINFO-AREA-0             
016600*   03  FILLER -COPY W335402A  -PRE UT2-  -RED  UTINFO-AREA-0             
016700*   03  FILLER -COPY W335403A  -PRE UT3-  -RED  UTINFO-AREA-0             
016800     EJECT                                                                
016900 01  ANTAL-AREA-START            PIC X(24)   VALUE                        
017000                                 'ANTAL-AREA-START  '.                    
017100     SKIP2                                                                
017200                                                                          
017300*01  AREA -COPY W335400A     -PRE ANTAL-                                  
017400     EJECT                                                                
017501                                                                          
017525 LINKAGE SECTION.                                                         
017531     EJECT                                                                
017532 PROCEDURE DIVISION.                                                      
017533                                                                          
017534 MAIN SECTION.                                                            
017540                                                                          
017800                                                                          
017900     PERFORM A-INIT                                                       
018000*      MARKNADSBOLAG ÄR ALLTID M5 = E                                     
018010     MOVE 'E'               TO WS-IDMARKBO                                
018100     PERFORM S01-LAES-W33542                                              
018300     PERFORM UNTIL END-OF-W33542                                          
018600       PERFORM B-FLYTTA-OCH-SKRIV                                         
019420       PERFORM UNTIL INFO1-IDPTYP = '401' OR END-OF-W33542                
019430         PERFORM S01-LAES-W33542                                          
019440       END-PERFORM                                                        
019900     END-PERFORM                                                          
020000                                                                          
020100     PERFORM C-SKRIV-W33543                                               
020200     PERFORM Z-FINIT                                                      
020300                                                                          
020400     MOVE ZERO TO RETURN-CODE                                             
020500     GOBACK                                                               
020600     .                                                                    
020700     EJECT                                                                
020800 A-INIT SECTION.                                                          
020900                                                                          
021000     OPEN INPUT  W33542                                                   
021300                                                                          
021400     OPEN OUTPUT W33544                                                   
021500                 W33543                                                   
021600     SKIP2                                                                
021700     ACCEPT DAGENS-DATUM  FROM DATE                                       
021800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
021810     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DAGENS-DATUM                   
021900     .                                                                    
022000     EJECT                                                                
022100 B-FLYTTA-OCH-SKRIV  SECTION.                                             
022200     SKIP2                                                                
022300* --- OBS PRARTVNA BLIR HÄR SJÄLVKOST- KAN BLI BESTPRIS SE BB-...         
022310* --- OBS SJÄLVKOST I INFIL ÄR ÄNDRARD I TIDIGARE STEG(W33543)            
022320     MOVE INFO1-ARTINFO TO UT1-ARTINFO                                    
022505                                                                          
022719     PERFORM S11A-SKRIV-W33544-401                                        
022720     PERFORM BA-SKRIV-402-403-POSTER                                      
022910                                                                          
023000     .                                                                    
023010     EJECT                                                                
023011                                                                          
023020 BA-SKRIV-402-403-POSTER SECTION.                                         
023030                                                                          
023100     PERFORM S01-LAES-W33542                                              
023101     IF NOT END-OF-W33542                                                 
023200       IF INFO-IDPTYP = '402'                                             
023300         MOVE INFO2-ARTTEXT  TO UT2-ARTTEXT                               
023400         PERFORM S11B-SKRIV-W33544-402                                    
023500         PERFORM S01-LAES-W33542                                          
023600       END-IF                                                             
023610     END-IF                                                               
023620     IF NOT END-OF-W33542                                                 
023700       IF INFO-IDPTYP = '403'                                             
023800         MOVE INFO3-ARTBEN   TO UT3-ARTBEN                                
023900         PERFORM S11C-SKRIV-W33544-403                                    
024000         PERFORM S01-LAES-W33542                                          
024100       END-IF                                                             
024110     END-IF                                                               
024200     .                                                                    
024300     EJECT                                                                
024310                                                                          
026500 C-SKRIV-W33543  SECTION.                                                 
026600     SKIP2                                                                
026700     MOVE '400' TO ANTAL-IDPTYP                                           
026800     MOVE 'A'   TO ANTAL-IDVTYP                                           
026900     MOVE WS-RAKNARE TO ANTAL-KVPOST                                      
027000                                                                          
027100     PERFORM S12-SKRIV-W33543                                             
027200     .                                                                    
027300     EJECT                                                                
027400 Z-FINIT SECTION.                                                         
027500     CLOSE W33542                                                         
027700           W33543                                                         
027800           W33544                                                         
028000     SKIP2                                                                
028100     MOVE 'S' TO POSTSUM-OPKOD                                            
028200     CALL POSTSUM USING POSTSUM-PARM                                      
028300     .                                                                    
028400     EJECT                                                                
029900 S01-LAES-W33542  SECTION.                                                
030000     SKIP2                                                                
030100     READ W33542 INTO INFO-AREA                                           
030200     AT END                                                               
030300        SET END-OF-W33542 TO TRUE                                         
030310        MOVE +999999999   TO INFO1-IDARTNR                                
030400                                                                          
030500     NOT AT END                                                           
030600        MOVE 'W33542'   TO POSTSUM-FDNAMN                                 
030700        MOVE 'W33544D1' TO POSTSUM-DDNAMN2                                
030800        MOVE '33542'    TO POSTSUM-TRANSTYP                               
030900        CALL POSTSUM USING POSTSUM-PARM                                   
031000     END-READ                                                             
031100     .                                                                    
031200                                                                          
032740 S11A-SKRIV-W33544-401 SECTION.                                           
032800                                                                          
032900     ADD +1 TO WS-RAKNARE                                                 
033000     WRITE UT1-POST FROM UTINFO-AREA                                      
033100                                                                          
033200     MOVE '400'    TO POSTSUM-TRANSTYP                                    
033300     MOVE 'W33544' TO POSTSUM-FDNAMN                                      
033400     MOVE 'W33544D3' TO POSTSUM-DDNAMN2                                   
033500     CALL POSTSUM USING POSTSUM-PARM                                      
033600     .                                                                    
033700     EJECT                                                                
033800 S11B-SKRIV-W33544-402 SECTION.                                           
033900                                                                          
034000     ADD +1 TO WS-RAKNARE                                                 
034100     WRITE UT2-POST FROM UTINFO-AREA                                      
034200                                                                          
034300     MOVE '402'    TO POSTSUM-TRANSTYP                                    
034400     MOVE 'W33544' TO POSTSUM-FDNAMN                                      
034500     MOVE 'W33544D3' TO POSTSUM-DDNAMN2                                   
034600     CALL POSTSUM USING POSTSUM-PARM                                      
034700     .                                                                    
034800     EJECT                                                                
034900 S11C-SKRIV-W33544-403 SECTION.                                           
035000                                                                          
035100     ADD +1 TO WS-RAKNARE                                                 
035200     WRITE UT3-POST FROM UTINFO-AREA                                      
035300                                                                          
035400     MOVE '403'    TO POSTSUM-TRANSTYP                                    
035500     MOVE 'W33544' TO POSTSUM-FDNAMN                                      
035600     MOVE 'W33544D3' TO POSTSUM-DDNAMN2                                   
035700     CALL POSTSUM USING POSTSUM-PARM                                      
035800     .                                                                    
035900     EJECT                                                                
036000 S12-SKRIV-W33543 SECTION.                                                
036100                                                                          
036200     WRITE ANTAL-POST FROM ANTAL-AREA                                     
036300                                                                          
036400     MOVE ANTAL-IDPTYP TO POSTSUM-TRANSTYP                                
036500     MOVE 'W33543' TO POSTSUM-FDNAMN                                      
036600     MOVE 'W33544D2' TO POSTSUM-DDNAMN2                                   
036700     CALL POSTSUM USING POSTSUM-PARM                                      
036800     .                                                                    
036900     EJECT                                                                
