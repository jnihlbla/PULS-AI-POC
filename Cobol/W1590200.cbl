000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1590200.                                                
000300 AUTHOR.         CONNY EGHOLT.                                            
000400 DATE-WRITTEN.   02/04/11.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        MATCHAR DET DAGLIGA ARTIKELEXTRAKTET FÖR NEVIS, OCH              
000900*        MATCHAR MED NEVIS ARTIKELREGISTER FÖR ATT ENBART SKICKA          
001000*        FÖRÄNDRINGARNA.                                                  
001100*        ALLA NYA, ÄNDRADE OCH BORTTAGNA ARTIKLAR I PULS SKALL            
001200*        RAPPORTERAS TILL NEVIS.                                          
001300*        FÖR ALLA UTPOSTER SKRIVES EN NYCKELFIL MED IDARTNR               
001400*        SOM GÅR TILL EFTERFÖLJANDE BMP.  DETTA FÖR ATT SÄTTA             
001500*        EN FLAGGA PÅ WDD301 FÖR ATT FÅ UT BENÄMNINGEN TILL NEVIS.        
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- EXTRAKT FRÅN W1590100                                      
003000     SELECT W15901                     ASSIGN TO W15902D1.                
003100     SKIP2                                                                
003200*          --- NEVIS ARTIKELREGISTER IN                                   
003300     SELECT W159K6I                    ASSIGN TO W15902D2.                
003400     SKIP2                                                                
003500*          --- NEVIS ARTIKELREGISTER UT                                   
003600     SELECT W159K6U                    ASSIGN TO W15902D3.                
003700     SKIP2                                                                
003800*          --- ARTIKLAR TILL NEVIS                                        
003900     SELECT W15902                     ASSIGN TO W15902D4.                
004000     SKIP2                                                                
004100*          --- ARTIKELNUMMER TILL BMP W1590500                            
004200     SELECT W15905                     ASSIGN TO W15902D5.                
004300     EJECT                                                                
004400 DATA DIVISION.                                                           
004500     SKIP2                                                                
004600 FILE SECTION.                                                            
004700     SKIP3                                                                
004800 FD  W15901                                                               
004900     RECORDING       F                                                    
005000     BLOCK CONTAINS  0.                                                   
005100*01  -COPY W15907      -L.                                                
005200                                                                          
005300     SKIP3                                                                
005400 FD  W159K6I                                                              
005500     RECORDING       F                                                    
005600     BLOCK CONTAINS  0.                                                   
005700*01  -COPY W15907      -L.                                                
005800                                                                          
005900     SKIP3                                                                
006000 FD  W159K6U                                                              
006100     RECORDING       F                                                    
006200     BLOCK CONTAINS  0.                                                   
006300*01  POST -COPY W15907 -PRE UT-REG-     -L.                               
006400                                                                          
006500     SKIP3                                                                
006600 FD  W15902                                                               
006700     RECORDING       F                                                    
006800     BLOCK CONTAINS  0.                                                   
006900*01  POST -COPY W15902 -PRE W15902- -L.                                   
007000                                                                          
007100     SKIP3                                                                
007200 FD  W15905                                                               
007300     RECORDING       F                                                    
007400     BLOCK CONTAINS  0.                                                   
007500 01  W15905-POST                   PIC X(15).                             
007600                                                                          
007700     EJECT                                                                
007800 WORKING-STORAGE SECTION.                                                 
007900                                                                          
008000 77  IDPGM                       PIC X(8)    VALUE 'W1590200'.            
008100 77  FL-NOSALDO                  PIC X       VALUE 'N'.                   
008200 77  TEST-FLNOSTOCK              PIC X       VALUE 'N'.                   
008300 77  WS-KVAKS-SDC                PIC S9(7)   VALUE ZERO COMP-3.           
008400 77  WS-RETUR                    PIC S9(7)   VALUE ZERO COMP-3.           
008500                                                                          
008600 77  W15901-EOF-SW               PIC X       VALUE 'N'.                   
008700     88  END-OF-W15901                       VALUE 'J'.                   
008800                                                                          
008900 77  W159K6-EOF-SW               PIC X       VALUE 'N'.                   
009000     88  END-OF-W159K6                       VALUE 'J'.                   
009100     EJECT                                                                
009200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009300 01  FILLER REDEFINES DAGENS-DATUM.                                       
009400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009700     EJECT                                                                
009800 01  -COPY WWDC99                                                         
009900                                                                          
010000 01  DYNAMISKA-SUBPROGRAM.                                                
010100*                                                                         
010200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG'.              
010400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010600     SKIP2                                                                
010700*    --- PARAMETRAR TILL ABEND                                            
010800                                                                          
010900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
011100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
011200     SKIP2                                                                
011300 01  FELTEXT.                                                             
011400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011600     EJECT                                                                
011700*    --- PARAMETRAR TILL POSTSUM                                          
011800*                                                                         
011900*01  -COPY W0005   -PRE  POSTSUM-                                         
012000     EJECT                                                                
012100*      --- VALID IDDC CODES                                               
012200*01    -COPY WWDCKONS                                                     
012300       EJECT                                                              
012400*                                                                         
012500 01  W15901-AREA-START           PIC X(24)   VALUE 'W15901-AREA'.         
012600*01  AREA -COPY W15907   -PRE W15901-                                     
012700     EJECT                                                                
012800                                                                          
012900 01  IN-REG-AREA-START           PIC X(24)   VALUE 'IN-REG-AREA'.         
013000*01  AREA -COPY W15907   -PRE IN-REG-                                     
013100     EJECT                                                                
013200                                                                          
013300 01  UT-REG-AREA-START           PIC X(24)   VALUE 'UT-REG-AREA'.         
013400*01  AREA -COPY W15907   -PRE UT-REG-                                     
013500     EJECT                                                                
013600                                                                          
013700 01  W15902-AREA-START           PIC X(24)   VALUE 'W15902-AREA'.         
013800*01  AREA -COPY W15902   -PRE W15902- .                                   
013900     EJECT                                                                
014000*                                                                         
014100 01  FILLER                      PIC X(24)   VALUE 'W15902-AREA'.         
014200 01  W15905-AREA.                                                         
014300     03  W15905-IDARTNR          PIC 9(9).                                
014400     03  FILLER                  PIC X(6)    VALUE SPACE.                 
014500*                                                                         
014600     EJECT                                                                
014700*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
014800*                                                                         
014900 01  IMS-WS.                                                              
015000     03     FILLER         PIC X(8)    VALUE 'IMS-WS  '.                  
015100*                            *** STATUSKOD FRÅN IMS                       
015200     03  STATUS-WS         PIC XX.                                        
015300         88  SEGMENT-FINNS             VALUE '  '.                        
015400         88  SEGMENT-SAKNAS            VALUE 'GE'.                        
015500*                                                                         
015600*                                                                         
015700 01  NYCKLAR.                                                             
015800     03  W-IDARTNR-X.                                                     
015900       05  W-IDARTNR         PIC S9(9) VALUE ZERO COMP-3.                 
016000     03  W-IDDC-X.                                                        
016100       05  W-IDDC            PIC X(2)  VALUE SPACE.                       
016200     03  W-IDPTYP-X.                                                      
016300         05  W-IDPTYP            PIC X(3)    VALUE '310'.                 
016400     EJECT                                                                
016500     03    SSA1            PIC X(64).                                     
016600     03    SSA2            PIC X(64).                                     
016700     SKIP3                                                                
016800     03    GODK-STATUSKODER.                                              
016900         05    GODK-STATUS OCCURS 3  INDEXED BY STATUS-IX PIC XX.         
017000     EJECT                                                                
017100*01      -COPY W0003                                                      
017200     EJECT                                                                
017300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
017400 01  DLI-IO-WDK601.                                                       
017500     03  -COPY WDK601                                                     
017600     EJECT                                                                
017700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK701'.                      
017800 01  DLI-IO-WDK701.                                                       
017900     03  -COPY WDK701                                                     
018000     EJECT                                                                
018100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711'.                      
018200 01  DLI-IO-WDK711.                                                       
018300     03  -COPY WDK711                                                     
018400     EJECT                                                                
018500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL601'.                      
018600 01  DLI-IO-WDL601.                                                       
018700*    03  -COPY WDL601                                                     
018800     EJECT                                                                
018900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL611'.                      
019000 01  DLI-IO-WDL611.                                                       
019100*    03  -COPY WDL611                                                     
019200     EJECT                                                                
019300 LINKAGE SECTION.                                                         
019400     SKIP3                                                                
019500*01      -COPY W0008     -PRE WDK6-                                       
019600         05  FILLER       PIC X.                                          
019700     EJECT                                                                
019800*01      -COPY W0008     -PRE WDK7-                                       
019900         05  FILLER       PIC X.                                          
020000     EJECT                                                                
020100*01      -COPY W0008     -PRE WDL6-                                       
020200         05  FILLER       PIC X.                                          
020300     EJECT                                                                
020400 PROCEDURE DIVISION  USING  WDK6-PCB WDK7-PCB WDL6-PCB.                   
020500     ENTRY 'DLITCBL' USING  WDK6-PCB WDK7-PCB WDL6-PCB.                   
020600     EJECT                                                                
020700 MAIN SECTION.                                                            
020800                                                                          
020900     PERFORM A-INIT                                                       
021000     MOVE 'P01' TO W15902-IDPTYP                                          
021100                                                                          
021200     PERFORM S01-LAES-W15901                                              
021300     PERFORM S02-LAES-W159K6I                                             
021400     PERFORM UNTIL END-OF-W15901                                          
021500       IF NOT END-OF-W15901                                               
021600         IF NOT END-OF-W159K6                                             
021700           IF W15901-IDARTNR = IN-REG-IDARTNR                             
021800*            --                       MATCH, FINNS I NEVIS !              
021900             IF W15901-W15901 NOT = IN-REG-W15901                         
022000*              --                     DEN ÄR FÖRÄNDRAD I PULS.            
022100               MOVE W15901-W15901 TO W15902-W15901                        
022200                                   UT-REG-W15901                          
022300               MOVE W15901-IDARTNR TO W-IDARTNR                           
022400               MOVE IN-REG-FLNOSTOCK TO TEST-FLNOSTOCK                    
022500               PERFORM D-KOLLA-KDERS                                      
022600               MOVE TEST-FLNOSTOCK TO W15902-FLNOSTOCK                    
022700                                      UT-REG-FLNOSTOCK                    
022800*              --                     SKRIV UT REG-POST                   
022900               PERFORM S11-SKRIV-W159K6U                                  
023000*              --                     SKICKA "UPDATE" KOD                 
023100*              --                     SKRIV UTFIL TILL NEVIS              
023200               MOVE 'U' TO W15902-KDUPPD                                  
023300               PERFORM S12-SKRIV-W15902                                   
023400             ELSE                                                         
023500*              --                     INGEN FÖRÄNDRING ALLS               
023600*              --                     SKRIV UT REG-POSTEN                 
023700               MOVE W15901-W15901 TO W15902-W15901                        
023800                                   UT-REG-W15901                          
023900               MOVE W15901-IDARTNR TO W-IDARTNR                           
024000               MOVE IN-REG-FLNOSTOCK TO TEST-FLNOSTOCK                    
024100               PERFORM D-KOLLA-KDERS                                      
024200               MOVE TEST-FLNOSTOCK TO W15902-FLNOSTOCK                    
024300                                      UT-REG-FLNOSTOCK                    
024400               IF IN-REG-FLNOSTOCK = TEST-FLNOSTOCK                       
024500                  PERFORM S11-SKRIV-W159K6U                               
024600               ELSE                                                       
024700*              --                  SKRIV UTFIL TILL NEVIS                 
024800                  MOVE 'U' TO W15902-KDUPPD                               
024900                  PERFORM S12-SKRIV-W15902                                
025000*              --                     SKRIV UT REG-POSTEN                 
025100                  PERFORM S11-SKRIV-W159K6U                               
025200               END-IF                                                     
025300             END-IF                                                       
025400*            --                       LÄS IN NYA POSTER                   
025500             PERFORM S01-LAES-W15901                                      
025600             PERFORM S02-LAES-W159K6I                                     
025700           ELSE                                                           
025800*            --                       OMATCH, FINNS INTE I NEVIS!         
025900             IF W15901-IDARTNR < IN-REG-IDARTNR                           
026000*              --                     ARTIKEL IN LÄGRE ÄN IN-REG.         
026100               MOVE W15901-W15901 TO W15902-W15901                        
026200                                     UT-REG-W15901                        
026300               MOVE W15901-IDARTNR TO W-IDARTNR                           
026400               MOVE W15901-FLNOSTOCK TO TEST-FLNOSTOCK                    
026500               PERFORM D-KOLLA-KDERS                                      
026600               MOVE TEST-FLNOSTOCK TO W15902-FLNOSTOCK                    
026700                                      UT-REG-FLNOSTOCK                    
026800*              --                     SKRIV UT-REG                        
026900               PERFORM S11-SKRIV-W159K6U                                  
027000*              --                     SKICKA "ADD" KOD                    
027100*              --                     SKRIV UTFIL TILL NEVIS              
027200               MOVE 'A' TO W15902-KDUPPD                                  
027300               PERFORM S12-SKRIV-W15902                                   
027400*              --                     SKRIV ARTIKELPOST TILL BMP          
027500               PERFORM S13-SKRIV-W15905                                   
027600*              --                     LÄS IN-ARTIKEL                      
027700               PERFORM S01-LAES-W15901                                    
027800             ELSE                                                         
027900*              --                     ARTIKEL IN, HÖGRE ÄN IN-REG.        
028000*              --                     REG-ART ÄR ALLTSÅ BORTTAGEN         
028100               MOVE IN-REG-W15901 TO W15902-W15901                        
028200               MOVE IN-REG-FLNOSTOCK TO W15902-FLNOSTOCK                  
028300*              --                     SKRIV INTE UT-REG                   
028400*              --                     SKRIV UTFIL TILL NEVIS              
028500*              --                     SKICKA "DELETE" KOD                 
028600               MOVE 'D'       TO W15902-KDUPPD                            
028700               PERFORM S12-SKRIV-W15902                                   
028800*              --                     LÄS IN-REG                          
028900               PERFORM S02-LAES-W159K6I                                   
029000             END-IF                                                       
029100           END-IF                                                         
029200         ELSE                                                             
029300*          --                         ARTIKEL IN HÖGRE ÄN                 
029400*          --                         DEN HÖGSTA PÅ IN-REG.               
029500           MOVE W15901-W15901 TO W15902-W15901                            
029600                                 UT-REG-W15901                            
029700           MOVE W15901-IDARTNR TO W-IDARTNR                               
029800           MOVE W15901-FLNOSTOCK TO TEST-FLNOSTOCK                        
029900           PERFORM D-KOLLA-KDERS                                          
030000           MOVE TEST-FLNOSTOCK TO W15902-FLNOSTOCK                        
030100                                  UT-REG-FLNOSTOCK                        
030200*          --                         SKRIV UT REG-POST                   
030300           PERFORM S11-SKRIV-W159K6U                                      
030400*          --                         SKICKA "ADD" KOD                    
030500*          --                         SKRIV UTFIL TILL NEVIS              
030600           MOVE 'A' TO W15902-KDUPPD                                      
030700           PERFORM S12-SKRIV-W15902                                       
030800*          --                         SKRIV ARTIKELPOST TILL BMP          
030900           PERFORM S13-SKRIV-W15905                                       
031000*          --                         LÄS IN-ARTIKEL                      
031100           PERFORM S01-LAES-W15901                                        
031200         END-IF                                                           
031300       ELSE                                                               
031400         IF NOT END-OF-W159K6                                             
031500*          --                         REG-ARTIKEL HÖGRE ÄN                
031600*          --                         DEN HÖGSTA PÅ IN-FIL.               
031700*          --                         REG-ART BORTTAGEN I PULS            
031800           MOVE IN-REG-W15901    TO W15902-W15901                         
031900           MOVE IN-REG-FLNOSTOCK TO W15902-FLNOSTOCK                      
032000*          --                         SKRIV INTE UT-REG                   
032100*          --                         SKRIV UTFIL TILL NEVIS              
032200*          --                         SKICKA "DELETE" KOD                 
032300           MOVE 'D'       TO W15902-KDUPPD                                
032400           PERFORM S12-SKRIV-W15902                                       
032500*          --                         LÄS IN-REG                          
032600           PERFORM S02-LAES-W159K6I                                       
032700         END-IF                                                           
032800       END-IF                                                             
032900     END-PERFORM                                                          
033000                                                                          
033100     PERFORM Z-FINIT                                                      
033200                                                                          
033300     MOVE ZERO TO RETURN-CODE                                             
033400     GOBACK                                                               
033500     .                                                                    
033600     EJECT                                                                
033700 A-INIT SECTION.                                                          
033800                                                                          
033900     OPEN INPUT  W15901                                                   
034000                 W159K6I                                                  
034100                                                                          
034200     OPEN OUTPUT W15902                                                   
034300                 W159K6U                                                  
034400                 W15905                                                   
034500                                                                          
034600     INITIALIZE W15902-AREA                                               
034700                                                                          
034800     ACCEPT DAGENS-DATUM  FROM DATE                                       
034900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
035000     .                                                                    
035100     EJECT                                                                
035200 D-KOLLA-KDERS SECTION.                                                   
035300                                                                          
035400     IF W15902-KDERS = 00 OR 01 OR 02 OR 03 OR 04 OR 05 OR 06 OR          
035500                       09 OR 07 OR 08 OR 27 OR 28 OR                      
035600                       11 OR 14 OR 19                                     
035700        MOVE 'F' TO TEST-FLNOSTOCK                                        
035800     ELSE                                                                 
035900        IF W15902-KDERS = 22 OR 23 OR 25 OR 26 OR 52                      
036000           MOVE 'T' TO TEST-FLNOSTOCK                                     
036100        ELSE                                                              
036200           IF (W15902-KDERS = 21 OR 24 OR 29) AND                         
036300           (TEST-FLNOSTOCK = 'F' OR SPACE)                                
036400              PERFORM DA-KOLLA-SALDO-NDC                                  
036500           END-IF                                                         
036600        END-IF                                                            
036700     END-IF                                                               
036800     .                                                                    
036900     EJECT                                                                
037000 DA-KOLLA-SALDO-NDC SECTION.                                              
037100                                                                          
037200     MOVE 'T' TO TEST-FLNOSTOCK                                           
037300                                                                          
037400     PERFORM IMS-GET-WDK701                                               
037500     IF SEGMENT-FINNS                                                     
037600        PERFORM IMS-GNP-WDK711                                            
037700        PERFORM UNTIL SEGMENT-SAKNAS                                      
037800                   OR TEST-FLNOSTOCK NOT = 'T'                            
037900           MOVE SLAG-IDDC TO WS-IDDC                                      
038000           IF NDC                                                         
038300              PERFORM DAA-KOLLA-SALDO                                     
038400           END-IF                                                         
038500           PERFORM IMS-GNP-WDK711                                         
038600        END-PERFORM                                                       
038700     ELSE                                                                 
038800        MOVE 'T' TO TEST-FLNOSTOCK                                        
038900     END-IF                                                               
039000     .                                                                    
039100     EJECT                                                                
039200 DAA-KOLLA-SALDO SECTION.                                                 
039300                                                                          
039400     MOVE SLAG-KVAKS-SDC TO WS-KVAKS-SDC                                  
039500     IF SLAG-KVAKS-SDC > 0                                                
039600        PERFORM DAAA-KOLLA-KVAKS-SDC                                      
039700     END-IF                                                               
039800     IF WS-KVAKS-SDC > ZERO                                               
039900        MOVE 'F' TO TEST-FLNOSTOCK                                        
040000     ELSE                                                                 
040100        IF SLAG-KVLS + SLAG-KVAKS-PAV +                                   
040200              SLAG-KVBEART > 0                                            
040300            MOVE 'F' TO TEST-FLNOSTOCK                                    
040400        END-IF                                                            
040500     END-IF                                                               
040600     .                                                                    
040700     EJECT                                                                
040800 DAAA-KOLLA-KVAKS-SDC SECTION.                                            
040900                                                                          
041000     PERFORM IMS-GET-WDL601                                               
041100     IF SEGMENT-FINNS                                                     
041200        PERFORM IMS-GET-WDL611                                            
041300        PERFORM UNTIL SEGMENT-SAKNAS                                      
041400           IF INL-KDRT = 07                                               
041500              COMPUTE WS-RETUR = INL-KVAVIS - INL-KVANTMOT                
041600              SUBTRACT WS-RETUR FROM WS-KVAKS-SDC                         
041700           END-IF                                                         
041800           PERFORM IMS-GET-WDL611                                         
041900        END-PERFORM                                                       
042000     END-IF                                                               
042100     .                                                                    
042200     EJECT                                                                
042300 Z-FINIT SECTION.                                                         
042400     CLOSE W15901                                                         
042500           W159K6I                                                        
042600           W159K6U                                                        
042700           W15902                                                         
042800           W15905                                                         
042900     SKIP2                                                                
043000     MOVE 'S' TO POSTSUM-OPKOD                                            
043100     CALL POSTSUM USING POSTSUM-PARM                                      
043200     .                                                                    
043300     EJECT                                                                
043400 S01-LAES-W15901  SECTION.                                                
043500     READ W15901 INTO W15901-AREA                                         
043600     AT END                                                               
043700        MOVE HIGH-VALUE   TO W15901-AREA                                  
043800        MOVE 999999999    TO W15901-IDARTNR                               
043900        SET END-OF-W15901 TO TRUE                                         
044000                                                                          
044100     NOT AT END                                                           
044200        MOVE 'W15901'   TO POSTSUM-FDNAMN                                 
044300        MOVE 'W15902D1' TO POSTSUM-DDNAMN2                                
044400        MOVE SPACE      TO POSTSUM-TRANSTYP                               
044500        CALL POSTSUM USING POSTSUM-PARM                                   
044600     END-READ                                                             
044700     .                                                                    
044800     EJECT                                                                
044900 S02-LAES-W159K6I SECTION.                                                
045000     READ W159K6I INTO IN-REG-AREA                                        
045100     AT END                                                               
045200        MOVE HIGH-VALUE   TO IN-REG-AREA                                  
045300        MOVE 999999999    TO IN-REG-IDARTNR                               
045400        SET END-OF-W159K6 TO TRUE                                         
045500                                                                          
045600     NOT AT END                                                           
045700        MOVE 'IN-R'     TO POSTSUM-TRANSTYP                               
045800        MOVE 'W159K6'   TO POSTSUM-FDNAMN                                 
045900        MOVE 'W15902D2' TO POSTSUM-DDNAMN2                                
046000        CALL POSTSUM USING POSTSUM-PARM                                   
046100     END-READ                                                             
046200     .                                                                    
046300     EJECT                                                                
046400 S11-SKRIV-W159K6U SECTION.                                               
046500                                                                          
046600     WRITE UT-REG-POST FROM UT-REG-AREA                                   
046700                                                                          
046800     MOVE 'UT-R'     TO POSTSUM-TRANSTYP                                  
046900     MOVE 'W159K6U'  TO POSTSUM-FDNAMN                                    
047000     MOVE 'W15902D3' TO POSTSUM-DDNAMN2                                   
047100     CALL POSTSUM USING POSTSUM-PARM                                      
047200     .                                                                    
047300     EJECT                                                                
047400 S12-SKRIV-W15902 SECTION.                                                
047500                                                                          
047600     WRITE W15902-POST FROM W15902-AREA                                   
047700                                                                          
047800     MOVE W15902-IDPTYP TO POSTSUM-TRANSTYP                               
047900     MOVE 'W15902'      TO POSTSUM-FDNAMN                                 
048000     MOVE 'W15902D4'    TO POSTSUM-DDNAMN2                                
048100     CALL POSTSUM USING POSTSUM-PARM                                      
048200     .                                                                    
048300     EJECT                                                                
048400 S13-SKRIV-W15905 SECTION.                                                
048500     SKIP2                                                                
048600     MOVE W15902-IDARTNR TO W15905-IDARTNR                                
048700                                                                          
048800     WRITE W15905-POST FROM W15905-AREA                                   
048900                                                                          
049000     MOVE SPACE           TO POSTSUM-TRANSTYP                             
049100     MOVE 'W15905'        TO POSTSUM-FDNAMN                               
049200     MOVE 'W15902D5'      TO POSTSUM-DDNAMN2                              
049300     CALL POSTSUM USING POSTSUM-PARM                                      
049400                                                                          
049500     .                                                                    
049600*   IMS SEKTIONER                                                         
049700     SKIP2                                                                
049800 IMS-GET-WDK701   SECTION.                                                
049900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
050000          DELIMITED BY SIZE INTO SSA1                                     
050100     MOVE '  GE' TO GODK-STATUSKODER                                      
050200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
050300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
050400     PERFORM IMS-STATUSKONTROLL                                           
050500     .                                                                    
050600     EJECT                                                                
050700 IMS-GNP-WDK711   SECTION.                                                
050800     MOVE 'WDK711 ' TO SSA1                                               
050900     MOVE '  GE' TO GODK-STATUSKODER                                      
051000     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
051100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
051200     PERFORM IMS-STATUSKONTROLL                                           
051300     .                                                                    
051400     EJECT                                                                
051500 IMS-GET-WDL601 SECTION.                                                  
051600     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
051700          DELIMITED BY SIZE INTO SSA1                                     
051800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
051900     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-WDL601 SSA1                    
052000     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
052100     PERFORM IMS-STATUSKONTROLL                                           
052200     .                                                                    
052300     SKIP2                                                                
052400 IMS-GET-WDL611 SECTION.                                                  
052500     STRING 'WDL611  (IDDC     =' W-IDDC-X                                
052600                    '&IDPTYP   =' W-IDPTYP-X ')'                          
052700            DELIMITED BY SIZE INTO SSA1                                   
052800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
052900     CALL CBLTDLI USING GNP WDL6-PCB DLI-IO-WDL611 SSA1                   
053000     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
053100     PERFORM IMS-STATUSKONTROLL                                           
053200     .                                                                    
053300     EJECT                                                                
053400 IMS-STATUSKONTROLL SECTION.                                              
053500                                                                          
053600     SET STATUS-IX TO 1                                                   
053700     SEARCH GODK-STATUS                                                   
053800        AT END                                                            
053900           CALL FELLOG                                                    
054000        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
054100           CONTINUE                                                       
054200     END-SEARCH                                                           
054300     .                                                                    
