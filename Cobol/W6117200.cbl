000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6117200.                                                
000400*AUTHOR.         BODIL LINDAHL.                                           
000500*DATE-WRITTEN.   95/01/26.                                                
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        LÄSER FIL W61171 OCH SKAPAR W61172 INTRASTAT-POSTER              
001000*        TILL VOLVO TRANSPORT.                                            
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDK6                                       
001300*                                                                         
001400*    CHANGE LOG:                                                          
001500*                                                                         
001600*      YY/MM/DD - NAME            - CHANGE DESCRIPTION                    
001700*      ----------------------------------------------------------         
001800*      15/10/13 - REDDY RAHUL     - ADD STAT NUMBER AND WEIGHT            
001900*                                   IN INTRASTE FILES.                    
002000*                                   E'TRACKER 10265098                    
002100*                                                                         
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- INFIL                                                      
003000     SELECT W61171                     ASSIGN TO W61172D1.                
003100     SKIP2                                                                
003200*          --- INTRASTAT-POSTER VOLVO TRANSPORT                           
003300     SELECT W61172                     ASSIGN TO W61172D2.                
003400     SKIP2                                                                
003500*          --- SORTERINGSFIL                                              
003600     SELECT SORTFIL                    ASSIGN TO W61172DS.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP3                                                                
004000 FILE SECTION.                                                            
004100     SKIP3                                                                
004200 FD  W61171                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600*01  -COPY W61171      -L.                                                
004700     SKIP3                                                                
004800 FD  W61172                                                               
004900     RECORDING       F                                                    
005000     BLOCK CONTAINS  0.                                                   
005100                                                                          
005200*01  UT-POST    -COPY A7290A01      -L.                                   
005300     SKIP2                                                                
005400 SD  SORTFIL.                                                             
005500                                                                          
005600*01  POST -COPY W61171      -PRE SORT-                                    
005700     EJECT                                                                
005800 WORKING-STORAGE SECTION.                                                 
005900                                                                          
006000*    -- CHECKED BY WY2000                                                 
006100                                                                          
006200                                                                          
006300 77  IDPGM                       PIC X(8)    VALUE 'W6117200'.            
006400 77  JA                          PIC X       VALUE 'J'.                   
006500 77  NEJ                         PIC X       VALUE 'N'.                   
006600 77  IX                          PIC S9(3)   VALUE ZERO.                  
006700 77  IX-MAX                      PIC S9(3)   VALUE +10.                   
006800 77  SPAR-IDLANDX2               PIC X(2)    VALUE SPACE.                 
006900 77  SPAR-IDARTNR                PIC S9(9)   VALUE ZERO COMP-3.           
007000                                                                          
007100 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
007200     88  END-OF-SORTFIL                      VALUE 'J'.                   
007300                                                                          
007400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007500 01  FILLER REDEFINES DAGENS-DATUM.                                       
007600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007900                                                                          
008000 01  WS-DATUM                    PIC X(8)    VALUE SPACE.                 
008100 01  FILLER REDEFINES WS-DATUM.                                           
008200     03  WS-AAR1                 PIC X(2).                                
008300     03  WS-AAR2                 PIC X(2).                                
008400     03  WS-MM                   PIC X(2).                                
008500     03  WS-DD                   PIC X(2).                                
008600                                                                          
008700 01  WS-GAELL-DATUM              PIC X(6)    VALUE SPACE.                 
008800 01  FILLER REDEFINES WS-GAELL-DATUM.                                     
008900     03  WS-GAELL-AAR1           PIC X(2).                                
009000     03  WS-GAELL-AAR2           PIC X(2).                                
009100     03  WS-GAELL-MM             PIC X(2).                                
009200                                                                          
009300 01  KDSORT-TABELL.                                                       
009400     03  KDSORT-TAB.                                                      
009500         05  FILLER             PIC X(6)  VALUE 'ST PCE'.                 
009600         05  FILLER             PIC X(6)  VALUE 'SA PCE'.                 
009700         05  FILLER             PIC X(6)  VALUE 'KG KGM'.                 
009800         05  FILLER             PIC X(6)  VALUE 'M  MTR'.                 
009900         05  FILLER             PIC X(6)  VALUE 'L  DMQ'.                 
010000         05  FILLER             PIC X(6)  VALUE 'ML MMT'.                 
010100         05  FILLER             PIC X(6)  VALUE 'G  GRM'.                 
010200         05  FILLER             PIC X(6)  VALUE 'C2 CMK'.                 
010300         05  FILLER             PIC X(6)  VALUE 'M2 MTK'.                 
010400         05  FILLER             PIC X(6)  VALUE 'MM CMQ'.                 
010500     03 SORT-TAB REDEFINES KDSORT-TAB.                                    
010600         05  TAB OCCURS 10.                                               
010700             07  KDSORT-PARTS   PIC X(2).                                 
010800             07  FILLER         PIC X.                                    
010900             07  KDSORT-TR      PIC X(3).                                 
011000     EJECT                                                                
011100 01  DYNAMISKA-SUBPROGRAM.                                                
011200*                                                                         
011300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
011400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011600     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
011700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
011800     03  W009CIA                 PIC X(8)    VALUE 'W009CIA'.             
011900     SKIP2                                                                
012000*    --- PARAMETRAR TILL ABEND                                            
012100                                                                          
012200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
012300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
012400     SKIP2                                                                
012500 01  FELTEXT.                                                             
012600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
012700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
012800     EJECT                                                                
012900*    --- PARAMETRAR TILL DATKORT                                          
013000*                                                                         
013100 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W61172'.              
013200     SKIP2                                                                
013300 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
013400     SKIP2                                                                
013500*01  -COPY WDATKORT                                                       
013600     EJECT                                                                
013700*01  -COPY W0005   -PRE  POSTSUM-                                         
013800     EJECT                                                                
013900*01  -COPY W009CIA                                                        
014000     EJECT                                                                
014100 01  SORT-AREA-START             PIC X(24)   VALUE                        
014200                                 'IN-AREA-START  '.                       
014300     SKIP2                                                                
014400                                                                          
014500*01  AREA -COPY W61171     -PRE IN-                                       
014600     EJECT                                                                
014700 01  UT-AREA-START               PIC X(24)   VALUE                        
014800                                 'UT-AREA-START  '.                       
014900*01  AREA -COPY A7290A01     -PRE UT-                                     
015000     EJECT                                                                
015100 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
015200                                  'SORTWS-AREA-START  '.                  
015300*01  AREA -COPY W61171      -PRE SORTWS-                                  
015400 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
015500     EJECT                                                                
015600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015700*                                                                         
015800     EJECT                                                                
015900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016000     SKIP3                                                                
016100 01  NYCKLAR-TILL-DLI.                                                    
016200     03  W-IDARTNR-X.                                                     
016300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
016400     03  W-KDSEGKEY-X.                                                    
016500         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
016600     SKIP2                                                                
016700*    --- STATUS-KOD FRÅN IMS                                              
016800 01  STATUS-WS                   PIC XX.                                  
016900     88  SEGMENT-FINNS                       VALUE '  '.                  
017000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017200     SKIP2                                                                
017300 01  GODK-STATUSKODER.                                                    
017400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017500     SKIP3                                                                
017600 01  SSA1                        PIC X(64).                               
017700 01  SSA2                        PIC X(64).                               
017800     EJECT                                                                
017900*    --- IMS FUNKTIONSKODER                                               
018000*01  -COPY W0003                                                          
018100     EJECT                                                                
018200*    ---  DLI INPUT-OUTPUT AREA                                           
018300                                                                          
018400 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK601'.         
018500 01  DLI-IO-WDK601.                                                       
018600*    03  -COPY WDK601  -PRE WDK6-                                         
018700     EJECT                                                                
018800                                                                          
018900 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK611'.         
019000 01  DLI-IO-WDK611.                                                       
019100*    03  -COPY WDK611  -PRE WDK6-                                         
019200     EJECT                                                                
019300                                                                          
019400     EJECT                                                                
019500 LINKAGE SECTION.                                                         
019600                                                                          
019700     EJECT                                                                
019800*01  -COPY W0008  -PRE WDK6-                                              
019900     05  FILLER                  PIC X.                                   
020000     EJECT                                                                
020100 PROCEDURE DIVISION  USING WDK6-PCB.                                      
020200     ENTRY 'DLITCBL' USING WDK6-PCB.                                      
020300                                                                          
020400     PERFORM A-INIT                                                       
020500                                                                          
020600     SORT SORTFIL ASCENDING KEY SORT-IDLANDX2 SORT-IDARTNR                
020700                  USING W61171                                            
020800                  OUTPUT PROCEDURE B-SORT-OUTPUT                          
020900                                                                          
021000     IF SORT-RETURN NOT = 0                                               
021100       MOVE SORT-RETURN TO SORT-RETURN-X                                  
021200       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
021300       DELIMITED BY SIZE INTO FELTEXT-STR                                 
021400       DISPLAY FELTEXT                                                    
021500       PERFORM S99-ABEND                                                  
021600     ELSE                                                                 
021700       PERFORM Z-FINIT                                                    
021800       MOVE ZERO TO RETURN-CODE                                           
021900       GOBACK                                                             
022000     END-IF                                                               
022100     .                                                                    
022200     EJECT                                                                
022300 A-INIT SECTION.                                                          
022400                                                                          
022500     OPEN OUTPUT W61172                                                   
022600                                                                          
022700     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
022800     MOVE D-AAR     TO  DAGENS-DATUM-AAR                                  
022900                        WS-AAR2                                           
023000     MOVE D-MAANAD  TO  DAGENS-DATUM-MAANAD                               
023100                        WS-MM                                             
023200     MOVE D-DAG     TO  DAGENS-DATUM-DAG                                  
023300                        WS-DD                                             
023400                                                                          
023500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
023600                                                                          
023700     MOVE SPACE TO UT-FIL-AREA                                            
023800     .                                                                    
023900     EJECT                                                                
024000 B-SORT-OUTPUT SECTION.                                                   
024100                                                                          
024200     PERFORM S31-SORT-RETURN                                              
024300                                                                          
024400     IF NOT END-OF-SORTFIL                                                
024500       PERFORM BA-SKAPA-SKRIV-PT01                                        
024600       PERFORM BB-SKAPA-PT02                                              
024700       MOVE SORTWS-IDLANDX2 TO SPAR-IDLANDX2                              
024800       MOVE SORTWS-IDARTNR TO SPAR-IDARTNR                                
024900                                                                          
025000       PERFORM UNTIL END-OF-SORTFIL                                       
025100         IF (SORTWS-IDLANDX2 = SPAR-IDLANDX2) AND                         
025200           (SORTWS-IDARTNR = SPAR-IDARTNR)                                
025300            ADD SORTWS-KVANTMOT TO UT-ANTAL                               
025400            ADD SORTWS-SUARTBES TO UT-VARDE IN UT-ARTIKEL-AREA            
025500         ELSE                                                             
025600            PERFORM BC-GET-STATNR-VIKT                                    
025700            PERFORM S01-SKRIV-UTPOST                                      
025800            MOVE ZERO TO UT-VARDE IN UT-ARTIKEL-AREA                      
025900                         UT-ANTAL                                         
026000            PERFORM BB-SKAPA-PT02                                         
026100            ADD SORTWS-KVANTMOT TO UT-ANTAL                               
026200            ADD SORTWS-SUARTBES TO UT-VARDE IN UT-ARTIKEL-AREA            
026300            MOVE SORTWS-IDLANDX2 TO SPAR-IDLANDX2                         
026400            MOVE SORTWS-IDARTNR TO SPAR-IDARTNR                           
026500         END-IF                                                           
026600         PERFORM S31-SORT-RETURN                                          
026700       END-PERFORM                                                        
026800                                                                          
026900       PERFORM S01-SKRIV-UTPOST                                           
027000     END-IF                                                               
027100     .                                                                    
027200     EJECT                                                                
027300 BA-SKAPA-SKRIV-PT01 SECTION.                                             
027400                                                                          
027500     MOVE '01'              TO UT-POSTTYP                                 
027600     MOVE '01441003'        TO UT-FILAVSID                                
027700     MOVE 'VOLVO CAR PARTS                  '                             
027800                            TO UT-FILAVSNAMN                              
027900     MOVE '5560743089'      TO UT-AVSORGNR                                
028000                                                                          
028100     IF DAGENS-DATUM-AAR < 94                                             
028200        MOVE '20' TO WS-AAR1                                              
028300     ELSE                                                                 
028400        MOVE '19' TO WS-AAR1                                              
028500     END-IF                                                               
028600     MOVE WS-DATUM          TO UT-FILDATUM                                
028700                                                                          
028800     MOVE '2000'            TO UT-FILTID                                  
028900     MOVE 'I'               TO UT-FORSELKOD                               
029000                                                                          
029100     MOVE SORTWS-TIAAMMDD-GAELL TO DAGENS-DATUM                           
029200     IF DAGENS-DATUM-AAR < 94                                             
029300        MOVE '20' TO WS-GAELL-AAR1                                        
029400     ELSE                                                                 
029500        MOVE '19' TO WS-GAELL-AAR1                                        
029600     END-IF                                                               
029700     MOVE DAGENS-DATUM-AAR     TO  WS-GAELL-AAR2                          
029800     MOVE DAGENS-DATUM-MAANAD  TO  WS-GAELL-MM                            
029900     MOVE WS-GAELL-DATUM       TO UT-PERIOD                               
030000                                                                          
030100     PERFORM S01-SKRIV-UTPOST                                             
030200     MOVE SPACE TO UT-FIL-AREA                                            
030300     MOVE ZERO TO UT-ANTAL                                                
030400                  UT-VARDE IN UT-ARTIKEL-AREA                             
030500     .                                                                    
030600     EJECT                                                                
030700 BB-SKAPA-PT02 SECTION.                                                   
030800                                                                          
030900     MOVE '02'              TO UT-POSTTYP                                 
031000     MOVE SORTWS-IDLANDX2   TO UT-LANDKOD IN UT-ARTIKEL-AREA              
031100     MOVE SPACE             TO UT-TRANSPORTSATT IN UT-ARTIKEL-AREA        
031200                                                                          
031300     MOVE 'VO ' TO CIA-IDARTPRE-IN                                        
031400     MOVE SORTWS-IDARTNR TO CIA-IDARTBET-IN                               
031500     CALL W009CIA USING CIA-W009CIA                                       
031600     MOVE CIA-IDARTBET-UT   TO UT-ARTIKELNR                               
031700                                                                          
031800     MOVE 'PCE'             TO UT-ANTALSTYP                               
031900     MOVE +1 TO IX                                                        
032000     PERFORM UNTIL IX > IX-MAX                                            
032100       IF KDSORT-PARTS(IX) = SORTWS-KDSORT                                
032200          MOVE KDSORT-TR(IX) TO UT-ANTALSTYP                              
032300       END-IF                                                             
032400       ADD +1 TO IX                                                       
032500     END-PERFORM                                                          
032600                                                                          
032700     .                                                                    
032800     EJECT                                                                
032900 BC-GET-STATNR-VIKT SECTION.                                              
033000                                                                          
033100     MOVE SPAR-IDARTNR              TO W-IDARTNR                          
033200                                                                          
033300     PERFORM IMS-GET-WDK611                                               
033400     IF SEGMENT-FINNS                                                     
033500*      IDSTATNR FOR BELGIUM                                               
033600       IF WDK6-CLAG-IDSTATNR(3) > ZERO                                    
033700         MOVE WDK6-CLAG-IDSTATNR(3) TO UT-IDSTATNR                        
033800       ELSE                                                               
033900         MOVE ZERO                  TO UT-IDSTATNR                        
034000       END-IF                                                             
034100*      TOTAL WEIGHT                                                       
034200       IF WDK6-CLAG-VKART > ZERO                                          
034300         COMPUTE UT-VKARTTOT = (UT-ANTAL * WDK6-CLAG-VKART)               
034400       ELSE                                                               
034500         MOVE ZERO                  TO UT-VKARTTOT                        
034600       END-IF                                                             
034700     ELSE                                                                 
034800       MOVE ZERO                    TO UT-IDSTATNR                        
034900                                       UT-VKARTTOT                        
035000     END-IF                                                               
035100                                                                          
035200     .                                                                    
035300     EJECT                                                                
035400 Z-FINIT SECTION.                                                         
035500                                                                          
035600     CLOSE W61172                                                         
035700                                                                          
035800     MOVE 'S' TO POSTSUM-OPKOD                                            
035900     CALL POSTSUM USING POSTSUM-PARM                                      
036000     .                                                                    
036100     EJECT                                                                
036200 S01-SKRIV-UTPOST SECTION.                                                
036300                                                                          
036400     WRITE UT-POST FROM UT-AREA                                           
036500                                                                          
036600     MOVE SPACE        TO POSTSUM-TRANSTYP                                
036700     MOVE 'W61172'     TO POSTSUM-FDNAMN                                  
036800     MOVE 'W61172D2'   TO POSTSUM-DDNAMN2                                 
036900     CALL POSTSUM USING POSTSUM-PARM                                      
037000     .                                                                    
037100     EJECT                                                                
037200 S31-SORT-RETURN  SECTION.                                                
037300                                                                          
037400     RETURN SORTFIL INTO SORTWS-AREA                                      
037500     AT END                                                               
037600         SET END-OF-SORTFIL TO TRUE                                       
037700     .                                                                    
037800     EJECT                                                                
037900 S99-ABEND  SECTION.                                                      
038000                                                                          
038100     MOVE 'S' TO POSTSUM-OPKOD                                            
038200     CALL POSTSUM USING POSTSUM-PARM                                      
038300     CALL ABEND  USING RKOD-ABEND-UTAN-DUMP                               
038400     .                                                                    
038500     EJECT                                                                
038600* --- IMS SEKTIONER ---                                                   
038700     SKIP3                                                                
038800     EJECT                                                                
038900 IMS-GET-WDK611 SECTION.                                                  
039000                                                                          
039100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
039200          DELIMITED BY SIZE INTO SSA1                                     
039300     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
039400          DELIMITED BY SIZE INTO SSA2                                     
039500     MOVE '  GE' TO GODK-STATUSKODER                                      
039600     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
039700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
039800     PERFORM IMS-STATUSKONTROLL                                           
039900     .                                                                    
040000     EJECT                                                                
040100 IMS-STATUSKONTROLL SECTION.                                              
040200                                                                          
040300     SET STATUS-IX TO 1                                                   
040400     SEARCH GODK-STATUS                                                   
040500       AT END                                                             
040600         MOVE 'FEL PÅ LÄSNING' TO FELTEXT-STR                             
040700         DISPLAY FELTEXT                                                  
040800         CALL FELLOG                                                      
040900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
041000         CONTINUE                                                         
041100     END-SEARCH                                                           
041200     .                                                                    
