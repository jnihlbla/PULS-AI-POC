000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W6122600.                                                
000400 AUTHOR.         BODIL LINDAHL.                                           
000500 DATE-WRITTEN.   97/01/09.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET LÄSER FIL W61223                                      
001100*                   SKAPAR D&P-FIL W61226, TRANSFER DEVIATION NDC         
001200*                                                                         
001300                                                                          
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*          --- LEV ANM TRANSFER                                           
002200     SELECT W61223                     ASSIGN TO W61226D1.                
002300     SKIP2                                                                
002400*          --- LISTA TRANSFER DEVIATION                                   
002500     SELECT W61226                     ASSIGN TO W61226D2.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003000                                                                          
003100 FD  W61223                                                               
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400                                                                          
003500*01  -COPY W61223      -L.                                                
003600     SKIP3                                                                
003700 FD  W61226                                                               
003800     RECORDING       V                                                    
003900     BLOCK CONTAINS  0.                                                   
004000     SKIP2                                                                
004100 01  LIST-POST                   PIC X(121).                              
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004500                                                                          
004600*    -- CHECKED BY WY2000                                                 
004700 77  IDPGM                       PIC X(8)    VALUE 'W6122600'.            
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  NEJ                         PIC X       VALUE 'N'.                   
004910 77  SPAR-IDDC                   PIC X(2)    VALUE SPACE.                 
005000 77  SPAR-IDFAKT                 PIC 9(7)    VALUE ZERO.                  
005100                                                                          
005200 77  W61223-EOF-SW               PIC X       VALUE 'N'.                   
005300     88  END-OF-W61223                       VALUE 'J'.                   
005400                                                                          
005500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005600 01  FILLER REDEFINES DAGENS-DATUM.                                       
005700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006000     EJECT                                                                
006100 01  DYNAMISKA-SUBPROGRAM.                                                
006200*                                                                         
006300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006500     SKIP2                                                                
006600*    --- PARAMETRAR TILL ABEND                                            
006700                                                                          
006800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007000     SKIP2                                                                
007100 01  FELTEXT.                                                             
007200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007400     EJECT                                                                
007500*    --- PARAMETRAR TILL POSTSUM                                          
007600*                                                                         
007700*01  -COPY W0005   -PRE  POSTSUM-                                         
007800     EJECT                                                                
007900 01  IN-AREA-START               PIC X(24)   VALUE                        
008000                                 'IN-AREA-START  '.                       
008100*01  AREA -COPY W61223     -PRE IN-                                       
008200     EJECT                                                                
008300 01  W001-AREA-START             PIC X(24)   VALUE                        
008400                                 'W001-AREA-START  '.                     
009700                                                                          
009800     EJECT                                                                
010300                                                                          
010400 01  W001-DAP.                                                            
010500     03  FILLER                  PIC X(165)  VALUE SPACE.                 
010600                                                                          
010700 01  FILLER                      PIC X(118)                               
010800                                 VALUE ' RUBRIKRAD1 '.                    
010900 01  W001-RUBRIK1.                                                        
011000     03  FILLER                  PIC X(3)    VALUE SPACE.                 
011100     03  FILLER                  PIC X(21)                                
011200                          VALUE 'VOLVO CAR PARTS      '.                  
011300     03  FILLER                  PIC X(12)   VALUE SPACE.                 
011400     03  W001-LISTID             PIC X(6)    VALUE SPACE.                 
011500     03  FILLER                  PIC X(2)    VALUE SPACE.                 
011510     03  W001-IDDC               PIC X(2)    VALUE SPACE.                 
011520     03  FILLER                  PIC X(10)   VALUE SPACE.                 
011600     03  W001-DATUM              PIC XXBXXBXX.                            
011700     03  FILLER                  PIC X(10)   VALUE SPACE.                 
012200     EJECT                                                                
012300 01  FILLER                      PIC X(118)                               
012400                                 VALUE ' RUBRIKRAD2 '.                    
012500 01  W001-RUBRIK2.                                                        
012600     03  FILLER                  PIC X(3)    VALUE SPACE.                 
012700     03  FILLER                  PIC X(21)                                
012800                          VALUE 'TRANSFER DEVIATIONS  '.                  
013600     03  FILLER                  PIC X(4)                                 
013700                          VALUE 'DC: '.                                   
013800     03  W001-IDDC-SEND          PIC X(2).                                
013900     03  FILLER                  PIC X(44)   VALUE SPACE.                 
014000     03  FILLER                  PIC X(67)   VALUE SPACE.                 
014100                                                                          
014200     EJECT                                                                
014300 01  FILLER                      PIC X(118)                               
014400                                 VALUE ' RUBRIKRAD4 '.                    
014500 01  W001-RUBRIK4.                                                        
014600     03  FILLER                  PIC X(3)    VALUE SPACE.                 
014700     03  FILLER                  PIC X(12)                                
014800                          VALUE 'DISTRICT:   '.                           
014900     03  W001-IDDISTR            PIC Z(4)9.                               
015000     03  FILLER                  PIC X(93)   VALUE SPACE.                 
015100     03  FILLER                  PIC X(7)    VALUE SPACE.                 
015200                                                                          
015300     EJECT                                                                
015400 01  FILLER                      PIC X(118)                               
015500                                 VALUE ' RUBRIKRAD5 '.                    
015600 01  W001-RUBRIK5.                                                        
015700     03  FILLER                  PIC X(3)    VALUE SPACE.                 
015800     03  FILLER                  PIC X(10)                                
015900                          VALUE 'CUSTOMER: '.                             
016000     03  W001-IDKUNDNR           PIC Z(6)9.                               
016100     03  FILLER                  PIC X(100)  VALUE SPACE.                 
016200                                                                          
016300     EJECT                                                                
016400 01  FILLER                      PIC X(118)                               
016500                                 VALUE ' RUBRIKRAD6 '.                    
016600 01  W001-RUBRIK6.                                                        
016700     03  FILLER                  PIC X(3)    VALUE SPACE.                 
016800     03  FILLER                  PIC X(21)                                
016900                          VALUE 'PULS INVOICE NUMBER: '.                  
017000     03  W001-IDFAKT             PIC Z(6)9.                               
017100     03  FILLER                  PIC X(15)   VALUE SPACE.                 
017200     03  FILLER                  PIC X(32)                                
017300                     VALUE 'VIPS INVOICE NUMBER:_ _ _ _ _ _ '.            
017400     03  FILLER                  PIC X(34)   VALUE SPACE.                 
017600                                                                          
017700     EJECT                                                                
017800 01  FILLER                      PIC X(118)                               
017900                                 VALUE ' RUBRIKRAD7 '.                    
018000 01  W001-RUBRIK7.                                                        
018100     03  FILLER                  PIC X(3)    VALUE SPACE.                 
018200     03  FILLER                  PIC X(22)                                
018300                          VALUE 'PULS INVOICE DATE..:'.                   
018400     03  W001-TIFAKT             PIC 9(6).                                
018600                                                                          
018700     EJECT                                                                
018800 01  FILLER                      PIC X(118)                               
018900                                 VALUE ' RUBRIKRAD8 '.                    
019000 01  W001-RUBRIK8.                                                        
019100     03  FILLER                  PIC X(3)    VALUE SPACE.                 
019200     03  FILLER                  PIC X(2)    VALUE SPACE.                 
019300     03  FILLER                  PIC X(5)    VALUE 'ORDER'.               
019400     03  FILLER                  PIC X(4)    VALUE SPACE.                 
019500     03  FILLER                  PIC X(6)    VALUE 'CASENO'.              
019600     03  FILLER                  PIC X(4)    VALUE SPACE.                 
019700     03  FILLER                  PIC X(11)   VALUE 'PART NUMBER'.         
019800     03  FILLER                  PIC X(4)    VALUE SPACE.                 
019900     03  FILLER                  PIC X(8)    VALUE 'QUANTITY'.            
020000     03  FILLER                  PIC X(4)    VALUE SPACE.                 
020100     03  FILLER                  PIC X(11)   VALUE 'REASON CODE'.         
020200     03  FILLER                  PIC X(4)    VALUE SPACE.                 
020300     03  FILLER                  PIC X(12)   VALUE 'PACKING CODE'.        
020400     03  FILLER                  PIC X(42)   VALUE SPACE.                 
020500                                                                          
020600     EJECT                                                                
020700 01  W001-DETALJ1.                                                        
020800     03  FILLER                  PIC X(3)    VALUE SPACE.                 
020900     03  W001-IDORDNR7           PIC Z(6)9.                               
021000     03  FILLER                  PIC X(5)    VALUE SPACE.                 
021100     03  W001-IDKOLLI            PIC Z(4)9.                               
021200     03  FILLER                  PIC X(6)    VALUE SPACE.                 
021300     03  W001-IDARTNR            PIC Z(8)9.                               
021400     03  FILLER                  PIC X(5)    VALUE SPACE.                 
021500     03  W001-KVLEVANM           PIC Z(6)9.                               
021600     03  FILLER                  PIC X(13)   VALUE SPACE.                 
021700     03  W001-KDANMORS           PIC X(2).                                
021800     03  FILLER                  PIC X(13)   VALUE SPACE.                 
021900     03  W001-KDEMBLEV           PIC 9.                                   
022000     03  FILLER                  PIC X(44)   VALUE SPACE.                 
022100     EJECT                                                                
022200 PROCEDURE DIVISION.                                                      
022300 MAIN SECTION.                                                            
022400                                                                          
022500     PERFORM A-INIT                                                       
022600                                                                          
022700     PERFORM S01-LAES-W61223                                              
022800     PERFORM UNTIL END-OF-W61223                                          
022900                                                                          
023000        IF IN-IDDC-SEND NOT = SPAR-IDDC                                   
023100          MOVE IN-IDDC-SEND  TO SPAR-IDDC                                 
023200          PERFORM S10-SKRIV-DAP1                                          
023300          PERFORM S11-SKRIV-DAP2                                          
023400          PERFORM B-SKRIV-DC-RUBRIK                                       
023500          MOVE ZERO TO SPAR-IDFAKT                                        
023600        END-IF                                                            
023700        IF IN-IDFAKT NOT = SPAR-IDFAKT                                    
023800           PERFORM C-SKRIV-FAKTURA-RUBRIK                                 
024000           MOVE IN-IDFAKT TO SPAR-IDFAKT                                  
024100        END-IF                                                            
024200                                                                          
024300        PERFORM D-SKAPA-DETALJRAD                                         
024400        PERFORM S01-LAES-W61223                                           
024500                                                                          
024600     END-PERFORM                                                          
024700                                                                          
024800     PERFORM Z-FINIT                                                      
024900                                                                          
025000     MOVE ZERO TO RETURN-CODE                                             
025100     GOBACK                                                               
025200     .                                                                    
025300     EJECT                                                                
025400 A-INIT SECTION.                                                          
025500                                                                          
025600     OPEN INPUT  W61223                                                   
025700     OPEN OUTPUT W61226                                                   
025800                                                                          
025900     MOVE ZERO TO SPAR-IDFAKT                                             
026000     ACCEPT DAGENS-DATUM FROM DATE                                        
026100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
026200     .                                                                    
026300     EJECT                                                                
026310 B-SKRIV-DC-RUBRIK SECTION.                                               
026320                                                                          
026330     MOVE 'W61226'        TO W001-LISTID                                  
026340     MOVE SPAR-IDDC       TO W001-IDDC                                    
026350     MOVE DAGENS-DATUM    TO W001-DATUM                                   
026360     MOVE IN-IDDC-SEND    TO W001-IDDC-SEND                               
026370                                                                          
026380     MOVE W001-RUBRIK1    TO W001-DAP                                     
026390     WRITE LIST-POST FROM W001-DAP                                        
026391     MOVE SPACE           TO W001-DAP                                     
026392     WRITE LIST-POST FROM W001-DAP                                        
026393     MOVE W001-RUBRIK2    TO W001-DAP                                     
026394     WRITE LIST-POST FROM W001-DAP                                        
026395     MOVE SPACE           TO W001-DAP                                     
026396     WRITE LIST-POST FROM W001-DAP                                        
026399     .                                                                    
026400     EJECT                                                                
026401 C-SKRIV-FAKTURA-RUBRIK SECTION.                                          
026402                                                                          
026403     MOVE IN-IDDISTR      TO W001-IDDISTR                                 
026404     MOVE IN-IDKUNDNR     TO W001-IDKUNDNR                                
026405     MOVE IN-IDFAKT       TO W001-IDFAKT                                  
026406     MOVE IN-TIFAKT       TO W001-TIFAKT                                  
026407                                                                          
026408     MOVE SPACE           TO W001-DAP                                     
026413     WRITE LIST-POST FROM W001-DAP                                        
026414     WRITE LIST-POST FROM W001-DAP                                        
026415     MOVE W001-RUBRIK4    TO W001-DAP                                     
026416     WRITE LIST-POST FROM W001-DAP                                        
026418                                                                          
026419     MOVE W001-RUBRIK5    TO W001-DAP                                     
026420     WRITE LIST-POST FROM W001-DAP                                        
026423                                                                          
026424     MOVE W001-RUBRIK6    TO W001-DAP                                     
026425     WRITE LIST-POST FROM W001-DAP                                        
026428                                                                          
026429     MOVE W001-RUBRIK7    TO W001-DAP                                     
026430     WRITE LIST-POST FROM W001-DAP                                        
026431     MOVE SPACE           TO W001-DAP                                     
026432     WRITE LIST-POST FROM W001-DAP                                        
026433                                                                          
026434     MOVE W001-RUBRIK8    TO W001-DAP                                     
026435     WRITE LIST-POST FROM W001-DAP                                        
026436     MOVE SPACE           TO W001-DAP                                     
026437     WRITE LIST-POST FROM W001-DAP                                        
026443     .                                                                    
026444                                                                          
026450 D-SKAPA-DETALJRAD SECTION.                                               
026500                                                                          
026600     MOVE IN-IDORDNR5     TO W001-IDORDNR7                                
026700     MOVE IN-IDKOLLI      TO W001-IDKOLLI                                 
026800     MOVE IN-IDARTNR      TO W001-IDARTNR                                 
026900     MOVE IN-KVLEVANM     TO W001-KVLEVANM                                
027000     MOVE IN-KDANMORS     TO W001-KDANMORS                                
027100     IF IN-KDANMORS = '43'                                                
027200        MOVE 2            TO W001-KDEMBLEV                                
027300     ELSE                                                                 
027400        MOVE ZERO         TO W001-KDEMBLEV                                
027500     END-IF                                                               
027510     MOVE W001-DETALJ1    TO W001-DAP                                     
027520     WRITE LIST-POST FROM W001-DAP                                        
027700     .                                                                    
027800     EJECT                                                                
029200 Z-FINIT SECTION.                                                         
029300                                                                          
029400     CLOSE W61223                                                         
029500           W61226                                                         
029600                                                                          
029700     MOVE 'S' TO POSTSUM-OPKOD                                            
029800     CALL POSTSUM USING POSTSUM-PARM                                      
029900     .                                                                    
030000     EJECT                                                                
030100 S01-LAES-W61223  SECTION.                                                
030200                                                                          
030300     READ W61223 INTO IN-AREA                                             
030400     AT END                                                               
030500        SET END-OF-W61223 TO TRUE                                         
030600                                                                          
030700     NOT AT END                                                           
030800        MOVE 'W61223'   TO POSTSUM-FDNAMN                                 
030900        MOVE 'W61226D1' TO POSTSUM-DDNAMN2                                
031000        MOVE SPACE      TO POSTSUM-TRANSTYP                               
031100        CALL POSTSUM USING POSTSUM-PARM                                   
031200     END-READ                                                             
031300     .                                                                    
031400     EJECT                                                                
031500 S10-SKRIV-DAP1 SECTION.                                                  
031510                                                                          
031520     MOVE ' ¤DAPW61226' TO W001-DAP                                       
031530     WRITE LIST-POST FROM W001-DAP                                        
031540                                                                          
031550     MOVE SPACE TO W001-DAP                                               
031560     .                                                                    
031570                                                                          
031580 S11-SKRIV-DAP2 SECTION.                                                  
031590                                                                          
031591     STRING ' ¤DAP' SPAR-IDDC                                             
031592            DELIMITED BY SIZE INTO W001-DAP                               
031593     WRITE LIST-POST FROM W001-DAP                                        
031594                                                                          
031595     MOVE SPACE TO W001-DAP                                               
031596     .                                                                    
