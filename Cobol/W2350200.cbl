000100*                  * CONVERTED BY VILMAII *                               
000200*                  * TO PURE COBOLCODE    *                               
000300     SKIP3                                                                
000400 ID DIVISION.                                                             
000500*                                                                         
000600 PROGRAM-ID.     W2350200.                                                
000700*              PROGRAM CONVERTED BY                                       
000800*              COBOL CONVERSION AID PO 5785-ABJ                           
000900*              CONVERSION DATE 05/25/91 19:59:46.                         
001000*AUTHOR.         ANDERSON RALPH.                                          
001100*DATE-WRITTEN.   DEC - 79.                                                
001200*REMARKS.        LÄSER EN FIL FRÅN LEV. BEVAKNINGSSYSTEMET (W236)         
001300*                SORTERAR DEN PÅ KDPRODSL,IDLEVNR,TIPER (COBOLSORT        
001400*                OCH   UPPDATERA   LEV. BEDÖMNINGSREGISTRET.  TAR         
001500*                BORT POSTER  ÄLDRE ÄN 8 PERIODER  FRÅN REGISTRET         
001600*                SAMT SKRIVER  EN LISTTRANS PER PERIOD , PRODUKT-         
001700*                SLAG OCH LEVERANTÖR.                                     
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000*                                                                         
002010 CONFIGURATION SECTION.                                                   
002020 SPECIAL-NAMES.                                                           
002030     ALPHABET Y2000 IS X'50' THRU X'99' X'00' THRU X'49'.                 
002040*                                                                         
002100 INPUT-OUTPUT SECTION.                                                    
002200 FILE-CONTROL.                                                            
002300     SELECT REG-IN  ASSIGN TO UT-S-W23502D1.                              
002400     SELECT W23603  ASSIGN TO UT-S-W23502D2.                              
002500     SELECT REG-UT  ASSIGN TO UT-S-W23502D3.                              
002600     SELECT SORTERA ASSIGN TO UT-S-W23502DS.                              
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  REG-IN                                                               
003200     BLOCK CONTAINS 0 RECORDS                                             
003300     RECORDING MODE IS F.                                                 
003400*                                                                         
003500*01  -COPY W235001    -L                                                  
003700     SKIP3                                                                
003800 FD  REG-UT                                                               
003900     BLOCK CONTAINS 0 RECORDS                                             
004000     RECORDING MODE IS F.                                                 
004100*                                                                         
004200 01  REG-UT-POST.                                                         
004300*    03  -COPY W235001    -L                                              
004500     EJECT                                                                
004600 FD  W23603                                                               
004700     BLOCK CONTAINS 0 RECORDS                                             
004800     RECORDING MODE IS F.                                                 
004900*                                                                         
005000*01  -COPY W236003   -L                                                   
005200     EJECT                                                                
005300 SD  SORTERA                                                              
005400                        .                                                 
005500*                                                                         
005600 01  SORTPOST.                                                            
005700     03  SORT-KDPRODSL     PIC S9(3)   COMP-3.                            
005800     03  SORT-IDLEVNR      PIC X(5).                                      
005900     03  SORT-TIAARP       PIC S9(5)   COMP-3.                            
006000     03  SORT-DATA         PIC X(24).                                     
006030                                                                          
006100     EJECT                                                                
006200 WORKING-STORAGE SECTION.                                                 
006210*    -COPY WY2000W7                                                       
006300     SKIP3                                                                
006400 77  INDENT-I PIC X(40) VALUE                                             
006500     'W2350200 91/05/25 TIME 13.15 VILMAII'.                              
006600***  STATEMENT ABOVE GENERATED BY VILMAII CONVERTER                       
006700*                                                                         
006800     SKIP3                                                                
006900 01  A-AREOR.                                                             
007000     03  JA                PIC X       VALUE 'J'.                         
007100     03  NEJ               PIC X       VALUE 'N'.                         
007200     03  TIO               PIC S9(3) COMP-3  VALUE +10.                   
007300     03   ABEND-CODE       PIC S9(4)   COMP  SYNC.                        
007400*                                                                         
007500     03   REG-ID-SW        PIC X       VALUE 'N'.                         
007600       88 SAMMA-REG-ID                 VALUE 'J'.                         
007700*                                                                         
007800     03   TRANS-ID-SW      PIC X       VALUE 'N'.                         
007900       88 SAMMA-TRANS-ID               VALUE 'J'.                         
008000*                                                                         
008100     03  AKTUELL-TIAARP-X.                                                
008200         05  WS-AAR        PIC 99.                                        
008300         05  WS-PER        PIC 99..                                       
008400     03  AKTUELL-TIAARP    REDEFINES AKTUELL-TIAARP-X    PIC 9(4).        
008500                                                                          
008600                                                                          
008700     03  GRANS-TIAARP      PIC 9(4).                                      
008800*                                                                         
008900     03  GAMMALT-REG-ID.                                                  
009000         05  FILLER        PIC S9(3)  COMP-3.                             
009100         05  FILLER        PIC X(5).                                      
009200*                                                                         
009300     03  GAMMALT-TRANS-ID.                                                
009400         05  FILLER        PIC S9(3)  COMP-3.                             
009500         05  FILLER        PIC X(5).                                      
009510                                                                          
009520     03  WS-AAVV                 PIC  9(4)   VALUE ZERO.                  
009530     03  FILLER    REDEFINES WS-AAVV.                                     
009550         05  WS-AA               PIC  9(2).                               
009560         05  WS-VV               PIC  9(2).                               
009600     SKIP2                                                                
009700 01  DYNAMISKA-SUBPROGRAM.                                                
009800     03  DATKORT           PIC X(8)    VALUE 'DATKORT '.                  
009900     03  POSTSUM           PIC X(8)    VALUE 'POSTSUM '.                  
010000     03  ABEND             PIC X(8)    VALUE 'ABEND   '.                  
010010     03  WDATKONV          PIC X(8)    VALUE 'WDATKONV'.                  
010100     EJECT                                                                
010110*01    -COPY WDATAREA                                                     
010120                                                                          
010130     EJECT                                                                
010200 01  EOF-SW.                                                              
010300     88  REGISTER-OCH-TRANS-SLUT       VALUE 'JJ'.                        
010400*                                                                         
010500     03  REG-SW            PIC X       VALUE 'N'.                         
010600         88  EOF-REG                   VALUE 'J'.                         
010700     03  TRANS-SW          PIC X       VALUE 'N'.                         
010800         88  EOF-TRANS                 VALUE 'J'.                         
010900     EJECT                                                                
011000 01  INPOST.                                                              
011100*    03  -COPY W236003     -PRE TRANS-                                    
011300*                                                                         
011400 01  FILLER  REDEFINES INPOST.                                            
011500         05  TRANS-ID.                                                    
011600             07  FILLER    PIC S9(3)  COMP-3.                             
011700             07  FILLER    PIC X(5).                                      
011800     EJECT                                                                
011900*01  -COPY W235001         -PRE REG-                                      
012100     EJECT                                                                
012200*01  -COPY W235001         -PRE SUTRAN-                                   
012400 01  NOLLOR.                                                              
012500     03  FILLER            PIC S9(3)   COMP-3    VALUE ZERO.              
012600     03  FILLER            PIC X(5)              VALUE SPACE.             
012700     03  FILLER            PIC S9(5)   COMP-3    VALUE ZERO.              
012800*                                                                         
012900     03  FILLER            PIC S9(5)   COMP-3    VALUE ZERO.              
013000     03  FILLER            PIC S9(5)   COMP-3    VALUE ZERO.              
013100     03  FILLER            PIC S9(5)   COMP-3    VALUE ZERO.              
013200*                                                                         
013300     03  FILLER            PIC S9V99   COMP-3    VALUE ZERO.              
013400*                                                                         
013500     03  FILLER            PIC S9(9)V99  COMP-3  VALUE ZERO.              
013600     03  FILLER            PIC S9(9)V99  COMP-3  VALUE ZERO.              
013700     03  FILLER            PIC S9(9)V99  COMP-3  VALUE ZERO.              
013800     EJECT                                                                
013900 01  PARAM-TILL-DATUMKORT.                                                
014000     03  FLTA              PIC X(6)    VALUE 'W23502'.                    
014100     03  FLTB              PIC X(6)    VALUE 'WDATUM'.                    
014200     03  FLTC.                                                            
014300*        05  -COPY WDATKORT                                               
014500     EJECT                                                                
014600*01  -COPY W0005     -PRE POSTSUM-                                        
014800     SKIP2                                                                
014900     03  REG-IN-TRANSID.                                                  
015000         05  REG-IN-FDNAMN    PIC X(6) VALUE 'REG-IN'.                    
015100         05  REG-IN-DDNAMN    PIC X(8) VALUE 'W23502D1'.                  
015200         05  REG-IN-TRANSTYP  PIC X(4) VALUE 'R-IN'.                      
015300                                                                          
015400     03  REG-UT-TRANSID.                                                  
015500         05  REG-UT-FDNAMN    PIC X(6) VALUE 'REG-UT'.                    
015600         05  REG-UT-DDNAMN    PIC X(8) VALUE 'W23502D3'.                  
015700         05  REG-UT-TRANSTYP  PIC X(4) VALUE 'R-UT'.                      
015800                                                                          
015900     03  W23603-TRANSID.                                                  
016000         05  W23603-FDNAMN    PIC X(6) VALUE 'W23603'.                    
016100         05  W23603-DDNAMN    PIC X(8) VALUE 'W23502D2'.                  
016200         05  W23603-TRANSTYP  PIC X(4) VALUE 'TRAN'.                      
016300                                                                          
016400     03  W23503-TRANSID.                                                  
016500         05  W23503-FDNAMN    PIC X(6) VALUE 'W23502'.                    
016600         05  W23503-DDNAMN    PIC X(8) VALUE 'W23302D4'.                  
016700         05  W23503-TRANSTYP  PIC X(4) VALUE 'LIST'.                      
016800     EJECT                                                                
016900 PROCEDURE DIVISION.                                                      
017000     CONTINUE.                                                            
017100     SKIP2                                                                
017200 STYR SECTION.                                                            
017300*                                                                         
017400     OPEN INPUT REG-IN                                                    
017500         OUTPUT REG-UT                                                    
017600*                                                                         
017700     SORT SORTERA ON ASCENDING KEY                                        
017800                       SORT-KDPRODSL                                      
017900                       SORT-IDLEVNR                                       
018000                       SORT-TIAARP                                        
018100     USING W23603                                                         
018200*                                                                         
018300     OUTPUT PROCEDURE BEARBETA                                            
018400     SKIP3                                                                
018500     IF SORT-RETURN = +16                                                 
018600       DISPLAY 'W23502 SORTERINGSFEL'                                     
018700       MOVE +20 TO ABEND-CODE                                             
018800       CALL ABEND USING ABEND-CODE                                        
018900     END-IF                                                               
019000     MOVE +0 TO RETURN-CODE                                               
019100     GOBACK                                                               
019200     CONTINUE.                                                            
019300     EJECT                                                                
019400 BEARBETA SECTION.                                                        
019500*                                                                         
019600     PERFORM A-INIT                                                       
019700     PERFORM S01-LAS-REGISTER                                             
019800     PERFORM S02-LAS-TRANSFIL                                             
019900*                                                                         
020000     MOVE TRANS-KDPRODSL TO SUTRAN-KDPRODSL                               
020100     MOVE TRANS-IDLEVNR TO SUTRAN-IDLEVNR                                 
020200     MOVE TRANS-TIAARP  TO SUTRAN-TIAARP                                  
020300     SKIP2                                                                
020400     PERFORM UNTIL                                                        
020500      ( REGISTER-OCH-TRANS-SLUT )                                         
020600       IF REG-ID = TRANS-ID                                               
020700         PERFORM C-BEHANDLA-REGPOST                                       
020800         PERFORM D-BEHANDLA-TRANSPOST                                     
020900       ELSE                                                               
021000         IF REG-ID < TRANS-ID                                             
021100           PERFORM C-BEHANDLA-REGPOST                                     
021200         ELSE                                                             
021300           PERFORM D-BEHANDLA-TRANSPOST                                   
021400         END-IF                                                           
021500       END-IF                                                             
021600     END-PERFORM                                                          
021700     PERFORM B-FINIT                                                      
021800     CONTINUE.                                                            
021900     EJECT                                                                
022000 A-INIT SECTION.                                                          
022100*                                                                         
022200     CALL DATKORT USING FLTA FLTB FLTC                                    
022300*                                                                         
022400     MOVE D-AAR    TO WS-AAR WS-AA                                        
022401     MOVE D-VECKA  TO WS-VV                                               
022402                                                                          
022410     MOVE WS-AAVV     TO DAT-I-TIDATUM                                    
022420     MOVE 'AAVV  '    TO DAT-KDDATFORM                                    
022430     CALL WDATKONV USING DAT-KDDATFORM                                    
022440                         DAT-I-TIDATUM                                    
022450                         DAT-O-TIDATUM                                    
022460                         DAT-KDSVAR                                       
022470     IF DAT-KDSVAR-OK                                                     
022480        MOVE DAT-TIRP    TO WS-PER                                        
022490     ELSE                                                                 
022491        DISPLAY ' FEL I DATKONV  TIRP AAVV ' WS-AAVV                      
022492        MOVE 20          TO ABEND-CODE                                    
022493        CALL ABEND USING ABEND-CODE                                       
022494     END-IF                                                               
022495                                                                          
022600     COMPUTE GRANS-TIAARP ROUNDED = AKTUELL-TIAARP - 100                  
022700*                                                                         
022800     MOVE ZERO TO REG-KDPRODSL                                            
022810     MOVE SPACE  TO REG-IDLEVNR                                           
022900     MOVE NOLLOR TO SUTRAN-W235001                                        
023000     MOVE AKTUELL-TIAARP  TO  TRANS-TIAARP                                
023100     MOVE 'W23502'  TO  POSTSUM-PROGNAMN                                  
023200     CONTINUE.                                                            
023300*                                                                         
023400 A999-EXIT.                                                               
023500     EXIT.                                                                
023600     CONTINUE.                                                            
023700     EJECT                                                                
023800 B-FINIT SECTION.                                                         
023900*                                                                         
024000     MOVE 'S' TO POSTSUM-OPKOD                                            
024100*                                                                         
024200     MOVE REG-IN-TRANSID TO POSTSUM-TRANSID                               
024300     CALL POSTSUM USING POSTSUM-PARM                                      
024400*                                                                         
024500     MOVE REG-UT-TRANSID TO POSTSUM-TRANSID                               
024600     CALL POSTSUM USING POSTSUM-PARM                                      
024700*                                                                         
024800     MOVE W23603-TRANSID TO POSTSUM-TRANSID                               
024900     CALL POSTSUM USING POSTSUM-PARM                                      
025000*                                                                         
025100     CLOSE REG-IN                                                         
025200           REG-UT                                                         
025300     CONTINUE.                                                            
025400*                                                                         
025500 B999-EXIT.                                                               
025600     EXIT.                                                                
025700     CONTINUE.                                                            
025800     EJECT                                                                
025900 C-BEHANDLA-REGPOST SECTION.                                              
026000*                                                                         
026100     MOVE REG-ID TO GAMMALT-REG-ID                                        
026200     PERFORM UNTIL                                                        
026300      NOT ( REG-ID = GAMMALT-REG-ID AND NOT EOF-REG )                     
026301       MOVE REG-TIAARP    TO TMP1-YYRP                                    
026302       MOVE GRANS-TIAARP  TO TMP2-YYRP                                    
026310       PERFORM WY2000P7                                                   
026400       IF TMP1-YYRP > TMP2-YYRP                                           
026401         MOVE REG-TIAARP    TO TMP1-YYRP                                  
026402         MOVE TRANS-TIAARP  TO TMP2-YYRP                                  
026410         PERFORM WY2000P7                                                 
026500         IF TMP1-YYRP < TMP2-YYRP                                         
026600           WRITE REG-UT-POST FROM REG-W235001                             
026700           MOVE REG-UT-TRANSID TO POSTSUM-TRANSID                         
026800           CALL POSTSUM USING POSTSUM-PARM                                
026900         ELSE                                                             
027000           DISPLAY 'FEL TIAARP PÅ REGISTER ELLER TRANSFIL'                
027100           DISPLAY 'TRANS-TIAARP = ' TRANS-TIAARP                         
027200           DISPLAY 'REG-TIAARP = ' REG-TIAARP                             
027300           MOVE 66 TO ABEND-CODE                                          
027400           CALL ABEND USING ABEND-CODE                                    
027500         END-IF                                                           
027600       END-IF                                                             
027700       PERFORM S01-LAS-REGISTER                                           
027800     END-PERFORM                                                          
027900     CONTINUE.                                                            
028000 C999-EXIT.                                                               
028100     EXIT.                                                                
028200     CONTINUE.                                                            
028300     EJECT                                                                
028400 D-BEHANDLA-TRANSPOST SECTION.                                            
028500*                                                                         
028600     MOVE TRANS-ID  TO  GAMMALT-TRANS-ID                                  
028700*                                                                         
028800     PERFORM UNTIL                                                        
028900      NOT ( TRANS-ID = GAMMALT-TRANS-ID AND NOT EOF-TRANS )               
029000       IF TRANS-TYP = +1                                                  
029100         ADD +1 TO SUTRAN-KVSENLEV                                        
029200         ADD TRANS-VARDE TO SUTRAN-SUSENLEV                               
029300       ELSE                                                               
029400         IF TRANS-TYP = +2                                                
029500           ADD +1 TO SUTRAN-KVTIDLEV                                      
029600           ADD TRANS-VARDE TO SUTRAN-SUTIDLEV                             
029700         ELSE                                                             
029800           ADD TRANS-ANTAL TO SUTRAN-KVLEV                                
029900           ADD TRANS-VARDE TO SUTRAN-SULEV                                
030000         END-IF                                                           
030100       END-IF                                                             
030200       PERFORM S02-LAS-TRANSFIL                                           
030300     END-PERFORM                                                          
030400     IF SUTRAN-KVLEV > 0                                                  
030500       COMPUTE SUTRAN-MATVARDE ROUNDED = 1 -                              
030600       ((SUTRAN-KVSENLEV + SUTRAN-KVTIDLEV) / SUTRAN-KVLEV)               
030700     ELSE                                                                 
030800       MOVE 1 TO SUTRAN-MATVARDE                                          
030900     END-IF                                                               
031000     WRITE REG-UT-POST FROM SUTRAN-W235001                                
031100     MOVE REG-UT-TRANSID TO POSTSUM-TRANSID                               
031200     CALL POSTSUM USING POSTSUM-PARM                                      
031300*                                                                         
031400     MOVE NOLLOR TO SUTRAN-W235001                                        
031500*                                                                         
031600     MOVE TRANS-KDPRODSL TO SUTRAN-KDPRODSL                               
031700     MOVE TRANS-IDLEVNR TO SUTRAN-IDLEVNR                                 
031800     MOVE TRANS-TIAARP  TO SUTRAN-TIAARP                                  
031900     CONTINUE.                                                            
032000*                                                                         
032100 D999-EXIT.                                                               
032200     EXIT.                                                                
032300     CONTINUE.                                                            
032400     EJECT                                                                
032500 S01-LAS-REGISTER SECTION.                                                
032600*                                                                         
032700     MOVE REG-ID TO GAMMALT-REG-ID                                        
032800*                                                                         
032900     READ REG-IN INTO REG-W235001 AT END MOVE JA TO REG-SW                
033000                                    MOVE 999 TO REG-KDPRODSL              
033100                                MOVE '99999' TO REG-IDLEVNR               
033200     END-READ                                                             
033300     IF NOT EOF-REG                                                       
033400       MOVE REG-IN-TRANSID TO POSTSUM-TRANSID                             
033500       CALL POSTSUM USING POSTSUM-PARM                                    
033600     END-IF                                                               
033700     CONTINUE.                                                            
033800 S998-EXIT.                                                               
033900     EXIT.                                                                
034000     CONTINUE.                                                            
034100     EJECT                                                                
034200 S02-LAS-TRANSFIL SECTION.                                                
034300*                                                                         
034400     MOVE TRANS-ID TO GAMMALT-TRANS-ID                                    
034500*                                                                         
034600     RETURN SORTERA INTO INPOST AT END MOVE JA TO TRANS-SW                
034700                               MOVE 999   TO TRANS-KDPRODSL               
034800                               MOVE '99999'  TO TRANS-IDLEVNR             
034900                               MOVE 9999  TO TRANS-TIAARP                 
035000     END-RETURN                                                           
035100     IF NOT EOF-TRANS                                                     
035200       MOVE W23603-TRANSID TO POSTSUM-TRANSID                             
035300       CALL POSTSUM USING POSTSUM-PARM                                    
035400     END-IF                                                               
035500     .                                                                    
035510     EJECT                                                                
035600*    -COPY WY2000P7                                                       
