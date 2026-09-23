000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W0117400.                                                
000400 AUTHOR.         STEFAN KIHLBERG.                                         
000500 DATE-WRITTEN.   94/10/25.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER BENÄMNINGSBASEN WDD3 MED SB.                               
001000*        SKAPAR FILER MED SAMTLIGA BENÄMNINGAR PER ARTIKEL.               
001100*        W01174   BENÄMNINGAR FRÅN WDD3                                   
001200*        SKAPAR FIL W01178 FÖR TURKISKA RYSKA KINESISKA                   
001300*        THAILÄNDSKA TJECKISKA UNGERSKA.                                  
001400*                                                                         
001500*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001600*                                                                         
001700*    ÄNDRINGAR:                                                           
001800*        2002-06-10: I SAMBAND MED NEVIS ÄR ALLA ICKE-LATIN1 SPRÅK        
001900*        LAGRADE MED UNICODE UTF8 I BASEN.                                
002000*        DÄRFÖR MÅSTE DE KONVERTERAS MED WCNVUNTR INNAN DE KAN            
002100*        SKRIVAS UT PÅ FILEN W01178.  /C.E.                               
002200*                                                                         
002300*        2018-02-05: ADD THAI LANGUAGES TO THE VIPS SYSTEM                
002400*        JIRA REFERENCE -  PULS-813. /S.A                                 
002500*                                                                         
002600*        2018-05-31: ADD CZECH REPUBLIC (TJECKISKA) LANGUAGES             
002700*                    TO THE VIPS SYSTEM.                                  
002800*        JIRA REFERENCE -  PULS-2536. /S.A                                
002900*                                                                         
003000*        2018-11-26: ADD HUNGARIAN (UNGERSKA) LANGUAGES                   
003100*                    TO THE VIPS SYSTEM.                                  
003200*        JIRA REFERENCE -  PULS-2589. /HB                                 
003300*                                                                         
003400*                                                                         
003500*    ABENDKODER:                                                          
003600*        U0016 -  . . . .                                                 
003700*        U1000 -  . . . .                                                 
003800*                                                                         
003900                                                                          
004000     SKIP3                                                                
004100 ENVIRONMENT DIVISION.                                                    
004200     SKIP2                                                                
004300 INPUT-OUTPUT SECTION.                                                    
004400                                                                          
004500 FILE-CONTROL.                                                            
004600     SKIP2                                                                
004700*          --- BENÄMNINGAR PER ARTIKEL                                    
004800     SELECT W01174                     ASSIGN TO W01174D1.                
004900*          --- BENÄMNING MED ARTIKEL EJ LATINSKA SPRÅK                    
005000     SELECT W01178                     ASSIGN TO W01174D2.                
005100*          --- BENÄMNING MED ARTIKEL EJ LATINSKA SPRÅK                    
005200     SELECT W01171                     ASSIGN TO W01174D3.                
005300*          --- IDBENNR+IDARTNR                                            
005400*              TILL AZURE DATALAKE                                        
005500     SELECT W01174X1                   ASSIGN TO W01174D4.                
005600*          --- IDBENNR+IDSKYLT+BEARTEXT                                   
005700*              TILL AZURE DATALAKE                                        
005800     SELECT W01174X2                   ASSIGN TO W01174D5.                
005810     SELECT W01174X3                   ASSIGN TO W01174D6.                
005900     EJECT                                                                
006000 DATA DIVISION.                                                           
006100     SKIP2                                                                
006200 FILE SECTION.                                                            
006300     SKIP3                                                                
006400 FD  W01174                                                               
006500     RECORDING       F                                                    
006600     BLOCK CONTAINS  0.                                                   
006700                                                                          
006800*01  POST -COPY W01174 -PRE  W01174-  -L.                                 
006900     SKIP3                                                                
007000 FD  W01178                                                               
007100     RECORDING       F                                                    
007200     BLOCK CONTAINS  0.                                                   
007300                                                                          
007400*01  POST -COPY W11602 -PRE  W11602-  -L.                                 
007500     SKIP3                                                                
007600 FD  W01171                                                               
007700     RECORDING       F                                                    
007800     BLOCK CONTAINS  0.                                                   
007900                                                                          
008000*01  POST -COPY W01171 -PRE  W01171-  -L.                                 
008100                                                                          
008200     SKIP3                                                                
008300 FD  W01174X1                                                             
008400     RECORDING       F                                                    
008500     BLOCK CONTAINS  0.                                                   
008600                                                                          
008700*01  POST -COPY W01174X1 -PRE  W01174X1- -L.                              
008800                                                                          
008900 FD  W01174X2                                                             
009000     RECORDING       F                                                    
009100     BLOCK CONTAINS  0.                                                   
009200                                                                          
009300*01  POST -COPY W01174X2 -PRE  W01174X2- -L.                              
009400                                                                          
009500     EJECT                                                                
009510 FD  W01174X3                                                             
009520     RECORDING       F                                                    
009530     BLOCK CONTAINS  0.                                                   
009540                                                                          
009550*01  POST -COPY W01174X3 -PRE  W01174X3- -L.                              
009560                                                                          
009570     EJECT                                                                
009600 WORKING-STORAGE SECTION.                                                 
009700                                                                          
009800                                                                          
009900*    -- CHECKED BY WY2000                                                 
010000 77  IDPGM                       PIC X(8)    VALUE 'W0117400'.            
010100 77  JA                          PIC X       VALUE 'J'.                   
010200 77  NEJ                         PIC X       VALUE 'N'.                   
010300 77  X20                         PIC X       VALUE X'20'.                 
010400                                                                          
010500 77  WS-IDSKYLT                  PIC X(3).                                
010600*    --- ALLA SPRÅK ENLIGT ISO-LATIN1                                     
010700     88  GODK-IDSKYLT                        VALUE 'D  '                  
010800                                                   'E  '                  
010900                                                   'F  '                  
011000                                                   'GB '                  
011100                                                   'I  '                  
011200                                                   'NL '                  
011300                                                   'P  '                  
011400                                                   'S  '                  
011500                                                   'SF '                  
011600                                                   'USA'.                 
011700     88  GODK-IDSKYLT-W01171                 VALUE 'CZ '                  
011800                                                   'GR '                  
011900                                                   'H  '                  
012000                                                   'IR '                  
012100                                                   'J  '                  
012200                                                   'KOR'                  
012300                                                   'PL '                  
012400                                                   'RC '                  
012500                                                   'RCN'                  
012600                                                   'RO '                  
012700                                                   'RUS'                  
012800                                                   'T  '                  
012900                                                   'TR '                  
013000                                                   'YU '.                 
013010 77  WS-IDSKYLT-AZURE            PIC X(3).                                
013100*    --- ALLA SPRÅK FÖR AZURE DATALAKE                                    
013110     88  GODK-IDSKYLT-W01174X2               VALUE 'D  '                  
013120                                                   'DK '                  
013130                                                   'E  '                  
013140                                                   'F  '                  
013150                                                   'GB '                  
013160                                                   'I  '                  
013170                                                   'MAL'                  
013180                                                   'NL '                  
013190                                                   'P  '                  
013192                                                   'S  '                  
013193                                                   'SF '                  
013194                                                   'USA'.                 
013195                                                                          
013196     88  GODK-IDSKYLT-W01174X3               VALUE 'CZ '                  
013197                                                   'GR '                  
013198                                                   'H  '                  
013199                                                   'IR '                  
013200                                                   'J  '                  
013201                                                   'KOR'                  
013202                                                   'PL '                  
013203                                                   'RC '                  
013204                                                   'RCN'                  
013205                                                   'RO '                  
013206                                                   'RUS'                  
013207                                                   'T  '                  
013208                                                   'TR '                  
013209                                                   'YU '.                 
013223                                                                          
013230 01  ARBETSAREOR.                                                         
013300     03  IX                      PIC S9(9)   VALUE ZERO COMP-3.           
013400     03  IX2                     PIC S9(9)   VALUE ZERO COMP-3.           
013500     03  SPRAK-IX                PIC S9(9)   VALUE ZERO COMP-3.           
013600                                                                          
013700 01  FILLER                      PIC X(8)    VALUE 'SPARAREA'.            
013800 01  WS-SPAR-IDBENNR             PIC 9(7)    VALUE ZERO.                  
013900 01  WS-SPAR-GB-BEART            PIC X(25)   VALUE SPACE.                 
014000 01  BEARTEXT-UTF8               PIC X(100)  VALUE SPACE.                 
014100 77  START-POS                   PIC S9(3)   VALUE ZERO.                  
014200 77  ANT-SPACE                   PIC S9(3)   VALUE ZERO.                  
014300 77  WS-IDARTNR                  PIC S9(9)   VALUE ZERO.                  
014400                                                                          
014500                                                                          
014600     EJECT                                                                
014700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
014800 01  FILLER REDEFINES DAGENS-DATUM.                                       
014900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
015000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
015100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
015200     EJECT                                                                
015300 01  DYNAMISKA-SUBPROGRAM.                                                
015400*                                                                         
015500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
015600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
015700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
015900     03  WCNVUNTR                PIC X(8)    VALUE 'WCNVUNTR'.            
016000     03  WCNVUNRU                PIC X(8)    VALUE 'WCNVUNRU'.            
016100     03  WCNVUNTH                PIC X(8)    VALUE 'WCNVUNTH'.            
016200     03  WCNVUNL2                PIC X(8)    VALUE 'WCNVUNL2'.            
016210     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
016300     SKIP2                                                                
016310 01  FILLER                      PIC X(16) VALUE 'WTRAUTF8-AREA'.         
016320*01  -COPY WTRAUTF8                                                       
016330     SKIP3                                                                
016340 01  FILLER                      PIC X(16)   VALUE 'WWOMVAND '.           
016350*   -COPY WWOMVAND                                                        
016360     SKIP3                                                                
016400*    --- PARAMETRAR TILL ABEND                                            
016500                                                                          
016600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
016700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
016800     SKIP2                                                                
016900 01  FELTEXT.                                                             
017000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
017100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
017200     EJECT                                                                
017300*    --- PARAMETRAR TILL POSTSUM                                          
017400*                                                                         
017500*01  -COPY W0005   -PRE  POSTSUM-                                         
017600     EJECT                                                                
017700 01  W01171-AREA-START           PIC X(24)   VALUE                        
017800                                 'W01171-AREA-START  '.                   
017900     SKIP2                                                                
018000                                                                          
018100*01  AREA -COPY W01171     -PRE W01171-                                   
018200     EJECT                                                                
018300 01  W01174-AREA-START           PIC X(24)   VALUE                        
018400                                 'W01174-AREA-START  '.                   
018500     SKIP2                                                                
018600                                                                          
018700*01  AREA -COPY W01174     -PRE W01174-                                   
018800     EJECT                                                                
018900 01  W01174X1-AREA-START         PIC X(24)   VALUE                        
019000                                 'W01174X1-AREA-START  '.                 
019100                                                                          
019200*01  AREA -COPY W01174X1   -PRE W01174X1-                                 
019300     EJECT                                                                
019400 01  W01174X2-AREA-START         PIC X(24)   VALUE                        
019500                                 'W01174X2-AREA-START  '.                 
019600                                                                          
019700*01  AREA -COPY W01174X2   -PRE W01174X2-                                 
019800     EJECT                                                                
019810 01  W01174X3-AREA-START         PIC X(24)   VALUE                        
019820                                 'W01174X3-AREA-START  '.                 
019830                                                                          
019840*01  AREA -COPY W01174X3   -PRE W01174X3-                                 
019850     EJECT                                                                
019900 01  W01178-AREA-START           PIC X(24)   VALUE                        
020000                                 'W01178-AREA-START  '.                   
020100     SKIP2                                                                
020200                                                                          
020300*01  AREA -COPY W11602     -PRE W11602-                                   
020400     EJECT                                                                
020500     SKIP2                                                                
020600                                                                          
020700*01   -COPY WCNVAREA       -PRE CNV-                                      
020800     EJECT                                                                
020900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021000*                                                                         
021100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
021200     SKIP3                                                                
021300 01  NYCKLAR-TILL-DLI.                                                    
021400     03  W-IDSKYLT-X.                                                     
021500         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
021600     03  W-IDARTNR-X.                                                     
021700         05  W-IDARTNR           PIC X(9)    VALUE SPACE.                 
021800     SKIP2                                                                
021900*    --- STATUS-KOD FRÅN IMS                                              
022000 01  STATUS-WS                   PIC XX.                                  
022100     88  SEGMENT-FINNS                       VALUE '  '.                  
022200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
022300     SKIP2                                                                
022400 01  GODK-STATUSKODER.                                                    
022500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022600     SKIP3                                                                
022700 01  SSA1                        PIC X(64).                               
022800 01  SSA2                        PIC X(64).                               
022900     EJECT                                                                
023000*    --- IMS FUNKTIONSKODER                                               
023100*01  -COPY W0003                                                          
023200     EJECT                                                                
023300*    ---  DLI INPUT-OUTPUT AREA                                           
023400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
023500     SKIP3                                                                
023600 01  DLI-IO-AREA.                                                         
023700     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
023800     SKIP3                                                                
023900     03  WLBENA01 REDEFINES IO-AREA.                                      
024000*        05  -COPY WDD301  -PRE WDD301-                                   
024100     SKIP3                                                                
024200     03  WLBENA11 REDEFINES IO-AREA.                                      
024300*        05  -COPY WDD311  -PRE WDD311-                                   
024400     SKIP3                                                                
024500     03  WLBENA12 REDEFINES IO-AREA.                                      
024600*        05  -COPY WDD312  -PRE WDD312-                                   
024700     EJECT                                                                
024800 LINKAGE SECTION.                                                         
024900*                                                                         
025000*01  -COPY W0008  -PRE WDD3-                                              
025100     05  FILLER                  PIC X.                                   
025200     EJECT                                                                
025300 PROCEDURE DIVISION  USING WDD3-PCB.                                      
025400     ENTRY 'DLITCBL' USING WDD3-PCB.                                      
025500                                                                          
025600     PERFORM A-INIT                                                       
025700     PERFORM IMS-GET-WDD3                                                 
025800     PERFORM UNTIL SEGMENT-SLUT                                           
025900        EVALUATE WDD3-SEG-NAME-FB                                         
026000           WHEN 'WDD301  '                                                
026100              PERFORM B-NOLLSTALL                                         
026200           WHEN 'WDD311  '                                                
026300              PERFORM C-FLYTTA-WDD311                                     
026400              PERFORM E-FLYTTA-WDD311-W01171                              
026500           WHEN 'WDD312  '                                                
026600              PERFORM D-FLYTTA-WDD312                                     
026700              PERFORM S11-SKRIV-W01174                                    
026800              PERFORM S12-SKRIV-W01178                                    
026900              PERFORM S13-SKRIV-W01171                                    
027000         END-EVALUATE                                                     
027100         PERFORM IMS-GET-WDD3                                             
027200     END-PERFORM                                                          
027300     PERFORM Z-FINIT                                                      
027400     MOVE ZERO TO RETURN-CODE                                             
027500     GOBACK                                                               
027600     .                                                                    
027700     EJECT                                                                
027800                                                                          
027900                                                                          
028000 A-INIT SECTION.                                                          
028100                                                                          
028200     OPEN OUTPUT W01174                                                   
028300                 W01178                                                   
028400                 W01171                                                   
028500                 W01174X1                                                 
028600                 W01174X2                                                 
028610                 W01174X3                                                 
028700                                                                          
028800     ACCEPT DAGENS-DATUM  FROM DATE                                       
028900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
029000     .                                                                    
029100     EJECT                                                                
029200                                                                          
029300                                                                          
029400 B-NOLLSTALL SECTION.                                                     
029500                                                                          
029600     MOVE ZERO         TO IX                                              
029700     MOVE +1           TO IX2                                             
029800     PERFORM UNTIL IX2 > 5                                                
029900        MOVE ZERO         TO W11602-IDARTNR                               
030000        MOVE SPACE        TO W11602-BEART(IX2)                            
030100                             W11602-BEARTEXT                              
030200                             W11602-IDSKYLT(IX2)                          
030300                             W11602-IDSKYLT-RCN                           
030400        ADD +1 TO IX2                                                     
030500     END-PERFORM                                                          
030600     MOVE SPACE TO WS-SPAR-GB-BEART                                       
030700     MOVE ZERO  TO IX2                                                    
030800     MOVE ZERO  TO SPRAK-IX                                               
030900                                                                          
031000     MOVE WDD301-BEN-IDBENNR TO WS-SPAR-IDBENNR                           
031100     .                                                                    
031200     EJECT                                                                
031300                                                                          
031400                                                                          
031500 C-FLYTTA-WDD311 SECTION.                                                 
031600                                                                          
031710                                                                          
031800     MOVE WDD311-TEXT-IDSKYLT         TO WS-IDSKYLT                       
031900     IF GODK-IDSKYLT                                                      
032000        ADD +1         TO IX                                              
032100        MOVE WDD311-TEXT-IDSKYLT      TO W01174-IDSKYLT(IX)               
032200        MOVE WDD311-TEXT-BEART        TO W01174-BEART(IX)                 
032300        IF WDD311-TEXT-IDSKYLT = 'GB '                                    
032400           MOVE WDD311-TEXT-BEART TO WS-SPAR-GB-BEART                     
032500        END-IF                                                            
032600     END-IF                                                               
032700                                                                          
032730                                                                          
032800     IF WDD311-TEXT-IDSKYLT = 'TR '                                       
032900        ADD +1 TO IX2                                                     
033000        MOVE WDD311-TEXT-IDSKYLT      TO W11602-IDSKYLT(IX2)              
033100*       MOVE LOW-VALUE                TO CNV-TECONV-FROM                  
033200        MOVE SPACE                    TO CNV-TECONV-FROM                  
033300        STRING WDD311-TEXT-BEARTEXT                                       
033400                  DELIMITED BY SIZE INTO CNV-TECONV-FROM (1:100)          
033500        MOVE 25  TO CNV-KVMAXTL                                           
033600        MOVE 'J' TO CNV-FLUTF8                                            
033700        MOVE 'N' TO CNV-FLTXTENT                                          
033800        CALL WCNVUNTR  USING CNV-WCNVAREA                                 
033900        IF CNV-KDSVAR = 'F'                                               
034000           MOVE WS-SPAR-GB-BEART      TO W11602-BEART(IX2)                
034100        ELSE                                                              
034200           MOVE CNV-TECONV-TO         TO W11602-BEART(IX2)                
034300        END-IF                                                            
034400                                                                          
034700     END-IF                                                               
034800                                                                          
034900     IF WDD311-TEXT-IDSKYLT = 'RUS'                                       
035000        ADD +1 TO IX2                                                     
035100        MOVE WDD311-TEXT-IDSKYLT      TO W11602-IDSKYLT(IX2)              
035200*       MOVE LOW-VALUE                TO CNV-TECONV-FROM                  
035300        MOVE SPACE                    TO CNV-TECONV-FROM                  
035400        STRING WDD311-TEXT-BEARTEXT                                       
035500                  DELIMITED BY SIZE INTO CNV-TECONV-FROM (1:100)          
035600        MOVE 25  TO CNV-KVMAXTL                                           
035700        MOVE 'J' TO CNV-FLUTF8                                            
035800        MOVE 'N' TO CNV-FLTXTENT                                          
035900        CALL WCNVUNRU  USING CNV-WCNVAREA                                 
036000        IF CNV-KDSVAR = 'F'                                               
036100           MOVE WS-SPAR-GB-BEART      TO W11602-BEART(IX2)                
036200        ELSE                                                              
036300           MOVE CNV-TECONV-TO         TO W11602-BEART(IX2)                
036400        END-IF                                                            
036500                                                                          
036800     END-IF                                                               
036900                                                                          
037000* JIRA -813  - ADD THAI FOR LANGUAGE CONVERSION - START                   
037100                                                                          
037200     IF WDD311-TEXT-IDSKYLT = 'T  '                                       
037300        ADD +1 TO IX2                                                     
037400        MOVE WDD311-TEXT-IDSKYLT      TO W11602-IDSKYLT(IX2)              
037500        MOVE SPACE                    TO CNV-TECONV-FROM                  
037600        STRING WDD311-TEXT-BEARTEXT                                       
037700                  DELIMITED BY SIZE INTO CNV-TECONV-FROM (1:100)          
037800        MOVE 25  TO CNV-KVMAXTL                                           
037900        MOVE 'J' TO CNV-FLUTF8                                            
038000        MOVE 'N' TO CNV-FLTXTENT                                          
038100        CALL WCNVUNTH  USING CNV-WCNVAREA                                 
038200        IF CNV-KDSVAR = 'F'                                               
038300           MOVE WS-SPAR-GB-BEART      TO W11602-BEART(IX2)                
038400        ELSE                                                              
038500           MOVE CNV-TECONV-TO         TO W11602-BEART(IX2)                
038600        END-IF                                                            
038700                                                                          
039000     END-IF                                                               
039100* JIRA -813  - ADD THAI FOR LANGUAGE CONVERSION - END                     
039200                                                                          
039300* JIRA -2536 - ADD CZECH FOR LANGUAGE CONVERSION - START                  
039400                                                                          
039500     IF WDD311-TEXT-IDSKYLT = 'CZ '                                       
039600        ADD +1 TO IX2                                                     
039700        MOVE WDD311-TEXT-IDSKYLT      TO W11602-IDSKYLT(IX2)              
039800        MOVE SPACE                    TO CNV-TECONV-FROM                  
039900        STRING WDD311-TEXT-BEARTEXT                                       
040000                  DELIMITED BY SIZE INTO CNV-TECONV-FROM (1:100)          
040100        MOVE 25  TO CNV-KVMAXTL                                           
040200        MOVE 'J' TO CNV-FLUTF8                                            
040300        MOVE 'N' TO CNV-FLTXTENT                                          
040400        CALL WCNVUNL2  USING CNV-WCNVAREA                                 
040500        IF CNV-KDSVAR = 'F'                                               
040600           MOVE WS-SPAR-GB-BEART      TO W11602-BEART(IX2)                
040700        ELSE                                                              
040800           MOVE CNV-TECONV-TO         TO W11602-BEART(IX2)                
040900        END-IF                                                            
041000                                                                          
041300     END-IF                                                               
041400* JIRA -2536 - ADD CZECH FOR LANGUAGE CONVERSION - END                    
041500                                                                          
041600* JIRA -2589 - ADD HUNGARIAN FOR LANGUAGE CONVERSION - START              
041700     IF WDD311-TEXT-IDSKYLT = 'H  '                                       
041800        ADD +1 TO IX2                                                     
041900        MOVE WDD311-TEXT-IDSKYLT      TO W11602-IDSKYLT(IX2)              
042000        MOVE SPACE                    TO CNV-TECONV-FROM                  
042100        STRING WDD311-TEXT-BEARTEXT                                       
042200                  DELIMITED BY SIZE INTO CNV-TECONV-FROM (1:100)          
042300        MOVE 25  TO CNV-KVMAXTL                                           
042400        MOVE 'J' TO CNV-FLUTF8                                            
042500        MOVE 'N' TO CNV-FLTXTENT                                          
042600        CALL WCNVUNL2  USING CNV-WCNVAREA                                 
042700        IF CNV-KDSVAR = 'F'                                               
042800           MOVE WS-SPAR-GB-BEART      TO W11602-BEART(IX2)                
042900        ELSE                                                              
043000           MOVE CNV-TECONV-TO         TO W11602-BEART(IX2)                
043100        END-IF                                                            
043200                                                                          
043500     END-IF                                                               
043600* JIRA -2589 - ADD HUNGARIAN FOR LANGUAGE CONVERSION - END                
043700                                                                          
043800     IF WDD311-TEXT-IDSKYLT = 'RCN'                                       
043900        IF WDD311-TEXT-BEARTEXT = SPACE                                   
044000           MOVE SPACE TO BEARTEXT-UTF8                                    
044100           INSPECT BEARTEXT-UTF8 REPLACING ALL SPACE BY X20               
044200        ELSE                                                              
044300           MOVE WDD311-TEXT-BEARTEXT  TO BEARTEXT-UTF8                    
044400           MOVE ZERO TO ANT-SPACE                                         
044500           INSPECT FUNCTION REVERSE(BEARTEXT-UTF8)                        
044600               TALLYING ANT-SPACE FOR LEADING SPACE                       
044700                                                                          
044800           COMPUTE START-POS = 100 - ANT-SPACE + 1                        
044900                                                                          
045000           IF START-POS > 100                                             
045100             CONTINUE                                                     
045200           ELSE                                                           
045300             INSPECT BEARTEXT-UTF8( START-POS : )                         
045400                 REPLACING ALL SPACE BY X20                               
045500           END-IF                                                         
045600        END-IF                                                            
045700        MOVE BEARTEXT-UTF8            TO W11602-BEARTEXT                  
045800        MOVE WDD311-TEXT-IDSKYLT      TO W11602-IDSKYLT-RCN               
045900                                                                          
046200     END-IF                                                               
046300                                                                          
046400     IF WDD311-TEXT-IDSKYLT = 'RC '                                       
046500        IF WDD311-TEXT-BEARTEXT = SPACE                                   
046600           MOVE SPACE TO BEARTEXT-UTF8                                    
046700           INSPECT BEARTEXT-UTF8 REPLACING ALL SPACE BY X20               
046800        ELSE                                                              
046900           MOVE WDD311-TEXT-BEARTEXT  TO BEARTEXT-UTF8                    
047000           MOVE ZERO TO ANT-SPACE                                         
047100           INSPECT FUNCTION REVERSE(BEARTEXT-UTF8)                        
047200               TALLYING ANT-SPACE FOR LEADING SPACE                       
047300                                                                          
047400           COMPUTE START-POS = 100 - ANT-SPACE + 1                        
047500                                                                          
047600           IF START-POS > 100                                             
047700             CONTINUE                                                     
047800           ELSE                                                           
047900             INSPECT BEARTEXT-UTF8( START-POS : )                         
048000                 REPLACING ALL SPACE BY X20                               
048100           END-IF                                                         
048200        END-IF                                                            
048300        MOVE BEARTEXT-UTF8            TO W11602-BEARTEXT-RC               
048400        MOVE WDD311-TEXT-IDSKYLT      TO W11602-IDSKYLT-RC                
048500                                                                          
048800     END-IF                                                               
048900                                                                          
049110     MOVE WDD311-TEXT-IDSKYLT         TO WS-IDSKYLT-AZURE                 
049120     IF GODK-IDSKYLT-W01174X2                                             
049200        MOVE WS-SPAR-IDBENNR          TO W01174X2-IDBENNR                 
049210        MOVE WDD311-TEXT-IDSKYLT      TO W01174X2-IDSKYLT                 
049220        MOVE WDD311-TEXT-BEARTEXT     TO W01174X2-BEARTEXT                
049300        PERFORM S15-SKRIV-W01174X2                                        
049400     ELSE                                                                 
049520        IF GODK-IDSKYLT-W01174X3                                          
049530           INITIALIZE W01174X3-POST                                       
049571           MOVE '278'                 TO TRAUTF8-KDCP                     
049572           MOVE WS-SPAR-IDBENNR       TO TRAUTF8-TECONV-FROM              
049573           CALL WTRAUTF8 USING TRAUTF8-AREA                               
049574           MOVE TRAUTF8-TECONV-TO     TO W01174X3-IDBENNR                 
049575                                                                          
049576           MOVE '278'                 TO TRAUTF8-KDCP                     
049577           MOVE WDD311-TEXT-IDSKYLT   TO TRAUTF8-TECONV-FROM              
049578           CALL WTRAUTF8 USING TRAUTF8-AREA                               
049579           MOVE TRAUTF8-TECONV-TO     TO W01174X3-IDSKYLT                 
049580                                                                          
049584           MOVE 'UTF8'                TO TRAUTF8-KDCP                     
049586           MOVE WDD311-TEXT-BEARTEXT  TO TRAUTF8-TECONV-FROM              
049590           CALL WTRAUTF8 USING TRAUTF8-AREA                               
049591           MOVE TRAUTF8-TECONV-TO     TO W01174X3-BEARTEXT                
049592                                                                          
049593           MOVE X'0D0A'               TO W01174X3-KDCRLF                  
049594                                                                          
049608           PERFORM S16-SKRIV-W01174X3                                     
049609        END-IF                                                            
049610     END-IF                                                               
049620     .                                                                    
049700     EJECT                                                                
049800 D-FLYTTA-WDD312 SECTION.                                                 
049900                                                                          
050000     MOVE WDD312-ART-IDARTNR          TO W01174-IDARTNR                   
050100                                         W11602-IDARTNR                   
050200                                         W01171-IDARTNR                   
050300                                         WS-IDARTNR                       
050400     MOVE ZERO  TO SPRAK-IX                                               
050500                                                                          
050600     MOVE WS-SPAR-IDBENNR             TO W01174X1-IDBENNR                 
050700     MOVE WDD312-ART-IDARTNR          TO W01174X1-IDARTNR                 
050800     PERFORM S14-SKRIV-W01174X1                                           
050900     .                                                                    
051000     EJECT                                                                
051100                                                                          
051200 E-FLYTTA-WDD311-W01171      SECTION.                                     
051300                                                                          
051400*LÄGGER 14 SPRÅK / POSTER PÅ UTFIL W01171                                 
051500                                                                          
051600     MOVE WDD311-TEXT-IDSKYLT  TO WS-IDSKYLT                              
051700                                                                          
051800     IF GODK-IDSKYLT-W01171                                               
051900        IF WDD311-TEXT-BEARTEXT = SPACE                                   
052000           MOVE SPACE TO BEARTEXT-UTF8                                    
052100           INSPECT BEARTEXT-UTF8 REPLACING ALL SPACE BY X20               
052200        ELSE                                                              
052300           MOVE WDD311-TEXT-BEARTEXT TO BEARTEXT-UTF8                     
052400           MOVE ZERO TO ANT-SPACE                                         
052500           INSPECT FUNCTION REVERSE(BEARTEXT-UTF8)                        
052600               TALLYING ANT-SPACE FOR LEADING SPACE                       
052700                                                                          
052800           COMPUTE START-POS = 100 - ANT-SPACE + 1                        
052900                                                                          
053000           IF START-POS > 100                                             
053100             CONTINUE                                                     
053200           ELSE                                                           
053300             INSPECT BEARTEXT-UTF8( START-POS : )                         
053400                 REPLACING ALL SPACE BY X20                               
053500           END-IF                                                         
053600        END-IF                                                            
053700        ADD  +1                  TO SPRAK-IX                              
053800        MOVE BEARTEXT-UTF8       TO W01171-BEARTEXT (SPRAK-IX)            
053900        MOVE WDD311-TEXT-IDSKYLT TO W01171-IDSKYLT(SPRAK-IX)              
054000     END-IF                                                               
054100     .                                                                    
054200     EJECT                                                                
054300 Z-FINIT SECTION.                                                         
054400     CLOSE W01174                                                         
054500           W01178                                                         
054600           W01171                                                         
054700           W01174X1                                                       
054800           W01174X2                                                       
054810           W01174X3                                                       
054900     SKIP2                                                                
055000     MOVE 'S' TO POSTSUM-OPKOD                                            
055100     CALL POSTSUM USING POSTSUM-PARM                                      
055200     .                                                                    
055300     EJECT                                                                
055400                                                                          
055500                                                                          
055600 S11-SKRIV-W01174 SECTION.                                                
055700                                                                          
055800     WRITE W01174-POST FROM W01174-AREA                                   
055900                                                                          
056000     MOVE 'W01174' TO POSTSUM-FDNAMN                                      
056100     MOVE 'W01174D1' TO POSTSUM-DDNAMN2                                   
056200     CALL POSTSUM USING POSTSUM-PARM                                      
056300     .                                                                    
056400     EJECT                                                                
056500                                                                          
056600                                                                          
056700 S12-SKRIV-W01178 SECTION.                                                
056800                                                                          
056900     WRITE W11602-POST FROM W11602-AREA                                   
057000                                                                          
057100     MOVE 'W01178' TO POSTSUM-FDNAMN                                      
057200     MOVE 'W01174D2' TO POSTSUM-DDNAMN2                                   
057300     CALL POSTSUM USING POSTSUM-PARM                                      
057400     .                                                                    
057500     EJECT                                                                
057600                                                                          
057700 S13-SKRIV-W01171 SECTION.                                                
057800                                                                          
057900     WRITE W01171-POST FROM W01171-AREA                                   
058000                                                                          
058100     MOVE 'W01171' TO POSTSUM-FDNAMN                                      
058200     MOVE 'W01174D3' TO POSTSUM-DDNAMN2                                   
058300     CALL POSTSUM USING POSTSUM-PARM                                      
058400     .                                                                    
058500     EJECT                                                                
058600                                                                          
058700 S14-SKRIV-W01174X1 SECTION.                                              
058800                                                                          
058900     WRITE W01174X1-POST FROM W01174X1-AREA                               
059000                                                                          
059100     MOVE 'W01174X1' TO POSTSUM-FDNAMN                                    
059200     MOVE 'W01174D4' TO POSTSUM-DDNAMN2                                   
059300     CALL POSTSUM USING POSTSUM-PARM                                      
059400     .                                                                    
059500     EJECT                                                                
059600 S15-SKRIV-W01174X2 SECTION.                                              
059700                                                                          
059800     WRITE W01174X2-POST FROM W01174X2-AREA                               
059900                                                                          
060000     MOVE 'W01174X2' TO POSTSUM-FDNAMN                                    
060100     MOVE 'W01174D5' TO POSTSUM-DDNAMN2                                   
060200     CALL POSTSUM USING POSTSUM-PARM                                      
060300     .                                                                    
060301                                                                          
060310 S16-SKRIV-W01174X3 SECTION.                                              
060320                                                                          
060330     WRITE W01174X3-POST FROM W01174X3-AREA                               
060340                                                                          
060350     MOVE 'W01174X3' TO POSTSUM-FDNAMN                                    
060360     MOVE 'W01174D6' TO POSTSUM-DDNAMN2                                   
060370     CALL POSTSUM USING POSTSUM-PARM                                      
060380     .                                                                    
060400     EJECT                                                                
060500* --- IMS SEKTIONER ---                                                   
060600     SKIP3                                                                
060700                                                                          
060800                                                                          
060900 IMS-GET-WDD3   SECTION.                                                  
061000                                                                          
061100     CALL CBLTDLI USING GN WDD3-PCB DLI-IO-AREA                           
061200     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
061300     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
061400     PERFORM IMS-STATUSKONTROLL                                           
061500     .                                                                    
061600     EJECT                                                                
061700                                                                          
061800                                                                          
061900 IMS-STATUSKONTROLL SECTION.                                              
062000                                                                          
062100     SET STATUS-IX TO 1                                                   
062200     SEARCH GODK-STATUS                                                   
062300       AT END                                                             
062400         MOVE 'FEL IMS STATUS' TO FELTEXT-STR                             
062500         DISPLAY FELTEXT                                                  
062600         CALL FELLOG                                                      
062700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
062800         CONTINUE                                                         
062900     END-SEARCH                                                           
063000     .                                                                    
