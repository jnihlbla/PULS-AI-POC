000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2368000.                                                
000300 AUTHOR.         JOHAN NIHLBLAD.                                          
000400 DATE-WRITTEN.   03/08/26.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        NEDLÄSNING WDD9 FÖR SPFU / VSIM / NU10                           
000900*                                                                         
001000*        PROGRAMMET LÄSER WDD9                                            
001100*                                                                         
001200                                                                          
001300     SKIP3                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000*          --- NEDLÄST WDD9 FÖR SPFU/VSIM                                 
002100     SELECT W23680                     ASSIGN TO W23680D1.                
002200     SKIP2                                                                
002300 DATA DIVISION.                                                           
002400     SKIP2                                                                
002500 FILE SECTION.                                                            
002600     SKIP3                                                                
002700 FD  W23680                                                               
002800     RECORDING       F                                                    
002900     BLOCK CONTAINS  0.                                                   
003000*01  POST -COPY W23680      -PRE  UT-  -L.                                
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400 77  IDPGM                       PIC X(8)    VALUE 'W2368000'.            
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003700 77  IX                          PIC S9(3)   VALUE ZERO.                  
003800 77  WS-SUMMA                    PIC 9(7)    VALUE ZERO.                  
003900 77  WS-KVANTMOT-TOT             PIC 9(7)    VALUE ZERO.                  
004000 77  SKRIV-SW                    PIC X       VALUE 'N'.                   
004100 77  WDD906-SW                   PIC X       VALUE 'N'.                   
004200 77  IDAG                        PIC X       VALUE 'D'.                   
004300 77  BAKAAT                      PIC X       VALUE 'B'.                   
004400 77  FRAMAAT                     PIC X       VALUE 'F'.                   
004500                                                                          
004600 01  SPARADE-IDLOPNRM-PL.                                                 
004700     03  SPARAD-IDLOPNRM-PL OCCURS 100.                                   
004800        05  SPAR-IDLOPNRM-PL    PIC 9(7).                                 
004900        05  SPAR-KVANTMOT       PIC 9(7).                                 
005000                                                                          
005100 01  WS-TYP                      PIC X.                                   
005200     88  WS-IDAG                   VALUE 'D'.                             
005300     88  WS-BAKAAT                 VALUE 'B'.                             
005400     88  WS-FRAMAAT                VALUE 'F'.                             
005500                                                                          
005600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005700 01  FILLER REDEFINES DAGENS-DATUM.                                       
005800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006100                                                                          
006200 01  DAGENS-DATUM-2DAG           PIC 9(6)    VALUE ZERO.                  
006300                                                                          
006400 01  AKTUELL-VVD                 PIC 9(3)    VALUE ZERO.                  
006500 01  FILLER REDEFINES AKTUELL-VVD.                                        
006600     03  AKTUELL-VV              PIC 9(2).                                
006700     03  AKTUELL-D               PIC 9(1).                                
006800                                                                          
006900 01  AVS-AAAAVV                  PIC 9(6).                                
007000 01  FILLER REDEFINES AVS-AAAAVV.                                         
007100     03  AVS-SEKEL               PIC 9(2).                                
007200     03  AVS-AAVV                PIC 9(4).                                
007300     03  FILLER REDEFINES AVS-AAVV.                                       
007400         05  AVS-AA              PIC 9(2).                                
007500         05  AVS-VV              PIC 9(2).                                
007600                                                                          
007700 01  WS-IDLOPNRM-PL              PIC 9(9).                                
007800 01  FILLER REDEFINES WS-IDLOPNRM-PL.                                     
007900     03  WS-AA                   PIC 9(2).                                
008000     03  WS-VVD                  PIC 9(3).                                
008100     03  WS-LLLL                 PIC 9(4).                                
008200                                                                          
008300 01  W-IDLOPNRM-PL               PIC 9(7).                                
008400 01  FILLER REDEFINES W-IDLOPNRM-PL.                                      
008500     03  W-VVD                   PIC 9(3).                                
008600     03  W-LLLL                  PIC 9(4).                                
008700                                                                          
008800 01  OM-AAMMDD                   PIC 9(6).                                
008900 01  FILLER REDEFINES OM-AAMMDD.                                          
009000     03  OM-AA                   PIC 9(2).                                
009100     03  OM-MM                   PIC 9(2).                                
009200     03  OM-DD                   PIC 9(2).                                
009300                                                                          
009400 01  OMV-AAVVD                   PIC 9(5).                                
009500 01  FILLER REDEFINES OMV-AAVVD.                                          
009600     03  OMV-AA                  PIC 9(2).                                
009700     03  OMV-VV                  PIC 9(2).                                
009800     03  OMV-D                   PIC 9(1).                                
009900                                                                          
010000 01  WS-TIAVRDAT-INL             PIC 9(6).                                
010100 01  WS-IDARTNR                  PIC S9(9)   COMP-3.                      
010200 01  WS-IDLEVNR                  PIC X(5).                                
010210 01  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
010300     EJECT                                                                
010400                                                                          
010500 01  DYNAMISKA-SUBPROGRAM.                                                
010600*                                                                         
010700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
011100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
011200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011300     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
011400     SKIP2                                                                
011500 01  FELTEXT.                                                             
011600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011800     EJECT                                                                
011900*    --- PARAMETRAR TILL DATKORT                                          
012000*                                                                         
012100 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W23680'.              
012200     SKIP2                                                                
012300 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
012400     SKIP2                                                                
012500*01  -COPY WDATKORT                                                       
012600     EJECT                                                                
012700*01  -COPY WORKAREA                                                       
012800     EJECT                                                                
012900*    --- PARAMETRAR TILL POSTSUM                                          
013000*                                                                         
013100*01  -COPY W0005   -PRE  POSTSUM-                                         
013200     EJECT                                                                
013300*01  -COPY WDATAREA                                                       
013400     EJECT                                                                
013401*01  -COPY WWDCKONS                                                       
013410     EJECT                                                                
013500 01  UT-AREA-START               PIC X(24)   VALUE                        
013600                                 'UT-AREA-START  '.                       
013700*01  AREA -COPY W23680     -PRE UT-                                       
013800     EJECT                                                                
013900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014000*                                                                         
014100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014200*    --- STATUS-KOD FRÅN IMS                                              
014300 01  STATUS-WS                   PIC XX.                                  
014400     88  SEGMENT-FINNS                       VALUE '  '.                  
014500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
014700     SKIP2                                                                
014800 01  GODK-STATUSKODER.                                                    
014900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015000     SKIP3                                                                
015100 01  SSA1                        PIC X(64).                               
015200 01  SSA2                        PIC X(64).                               
015300     EJECT                                                                
015400*    --- IMS FUNKTIONSKODER                                               
015500*01  -COPY W0003                                                          
015600     EJECT                                                                
015700*    ---  DLI INPUT-OUTPUT AREA                                           
015800 01  FILLER         PIC X(16) VALUE 'DLI-IO-AREA'.                        
015900 01  DLI-IO-AREA.                                                         
016000     03  IO-AREA    PIC X(150) VALUE SPACE.                               
016100     SKIP2                                                                
016200     03  WDD901 REDEFINES IO-AREA.                                        
016300*        05  -COPY WDD901  -PRE IN-                                       
016400     EJECT                                                                
016500     03  WDD902 REDEFINES IO-AREA.                                        
016600*        05  -COPY WDD902  -PRE IN-                                       
016700     EJECT                                                                
016800     03  WDD905 REDEFINES IO-AREA.                                        
016900*        05  -COPY WDD905  -PRE IN-                                       
017000     EJECT                                                                
017100     03  WDD906 REDEFINES IO-AREA.                                        
017200*        05  -COPY WDD906  -PRE IN-                                       
017300     EJECT                                                                
017400 LINKAGE SECTION.                                                         
017500*01  -COPY W0008  -PRE WDD9-                                              
017600     05  FILLER                  PIC X.                                   
017700     EJECT                                                                
017800 PROCEDURE DIVISION  USING WDD9-PCB.                                      
017900 MAIN SECTION.                                                            
018000     ENTRY 'DLITCBL' USING WDD9-PCB.                                      
018100                                                                          
018200     PERFORM A-INIT                                                       
018300                                                                          
018400     PERFORM IMS-GET-WDD9                                                 
018500     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
018600        EVALUATE WDD9-SEG-NAME-FB                                         
018700           WHEN 'WDD901'                                                  
018800              IF SKRIV-SW = JA                                            
018900                PERFORM S01-SKRIV-POST                                    
019000                MOVE NEJ TO SKRIV-SW                                      
019100              END-IF                                                      
019200              MOVE IN-IDARTNR TO WS-IDARTNR                               
019210              MOVE IN-IDDC    TO WS-IDDC                                  
019300           WHEN 'WDD902'                                                  
019400              IF SKRIV-SW = JA                                            
019500                PERFORM S01-SKRIV-POST                                    
019600                MOVE NEJ TO SKRIV-SW                                      
019700              END-IF                                                      
019800              MOVE IN-IDLEVNR TO WS-IDLEVNR                               
019900           WHEN 'WDD905'                                                  
020000              MOVE NEJ TO WDD906-SW                                       
020100              IF SKRIV-SW = JA                                            
020200                 PERFORM S01-SKRIV-POST                                   
020300                 MOVE NEJ TO SKRIV-SW                                     
020400              END-IF                                                      
020500              MOVE +1 TO IX                                               
020600              PERFORM UNTIL IX > 100                                      
020700                 MOVE ZERO TO SPAR-IDLOPNRM-PL(IX)                        
020800                              SPAR-KVANTMOT(IX)                           
020900                 ADD +1 TO IX                                             
021000              END-PERFORM                                                 
021100              MOVE +1 TO IX                                               
021200              MOVE ZERO TO WS-KVANTMOT-TOT                                
021300              PERFORM B-DATUM-2MAAN-BAK                                   
021400***** OM-AAMMDD = DAGENS-DATUM - 2 MÅN                                    
021500              IF IN-KDAVROP = 2 OR 9                                      
021600                                                                          
021700***** ALLA MED FÖRVÄNTAD INLEVERANS 2 MÅN BAKÅT + ALLA FRAMÅT             
021800***** TIAVRDAT = PLANERAD INLEVERANS                                      
021900                 MOVE IN-TIAVRDAT-INL TO WS-TIAVRDAT-INL                  
022000*****            IF WS-TIAVRDAT-INL >= OM-AAMMDD                          
022100***** OM FÖRVÄNTAD INLEVERANS IDAG                                        
022200                    IF WS-TIAVRDAT-INL = DAGENS-DATUM                     
022300                       MOVE IDAG TO WS-TYP                                
022400                       PERFORM C-OMVANDLA-DATUM                           
022500***** FÖRVÄNTAD AVSÄNDNINGSDAG FRÅN LEV TILL UT-TIAAMMDD                  
022600                       MOVE 'A' TO UT-STAT                                
022700                       MOVE IN-KVAVROP TO WS-SUMMA                        
022800                       MOVE JA TO SKRIV-SW                                
022900                       MOVE JA TO WDD906-SW                               
023000                    ELSE                                                  
023100*****OM FÖRVÄNTAD INLEVERANS FRAMÅT I TIDEN                               
023200                       IF WS-TIAVRDAT-INL > DAGENS-DATUM                  
023400                          MOVE FRAMAAT TO WS-TYP                          
023500                          MOVE 'A' TO UT-STAT                             
023600                          PERFORM C-OMVANDLA-DATUM                        
023700                          MOVE IN-KVAVROP TO WS-SUMMA                     
023800                          MOVE JA TO WDD906-SW                            
023900                       ELSE                                               
024000***** OM FÖRVÄNTAD INLEVERANS PASSERAD                                    
024100                          MOVE BAKAAT TO WS-TYP                           
024200                          MOVE 'U' TO UT-STAT                             
024300                          PERFORM C-OMVANDLA-DATUM                        
024400                          MOVE IN-KVAVROP TO WS-SUMMA                     
024500                          MOVE JA TO WDD906-SW                            
024600                       END-IF                                             
024700                    END-IF                                                
024800******           END-IF                                                   
024900              END-IF                                                      
025000           WHEN 'WDD906'                                                  
025100              IF WDD906-SW = JA                                           
025200                 PERFORM D-DATUM-KONTROLL                                 
025300***** DAGENS-DATUM I AKTUELL-VVD                                          
025400                 MOVE IN-IDLOPNRM-PL TO WS-IDLOPNRM-PL                    
025500***** INLEVERANS DATUM I WS-VVD                                           
025600                 MOVE WS-VVD TO W-VVD                                     
025700                 MOVE WS-LLLL TO W-LLLL                                   
025800                                                                          
025900                 IF WS-VVD < AKTUELL-VVD                                  
026000                    CONTINUE                                              
026100                 ELSE                                                     
026200                    MOVE W-IDLOPNRM-PL  TO SPAR-IDLOPNRM-PL(IX)           
026300                    MOVE IN-KVAVROP-AVB TO SPAR-KVANTMOT(IX)              
026400                    ADD +1 TO IX                                          
026500                 END-IF                                                   
026600                                                                          
026700                 IF WS-VVD NOT = AKTUELL-VVD                              
026800                    MOVE 'U' TO UT-STAT                                   
026900                 END-IF                                                   
027000                 ADD IN-KVAVROP-AVB TO WS-SUMMA                           
027100                                       WS-KVANTMOT-TOT                    
027300                 IF WS-VVD = AKTUELL-VVD                                  
027400                   IF WS-BAKAAT                                           
027500                      MOVE JA TO SKRIV-SW                                 
027600                   ELSE                                                   
027700                      IF WS-FRAMAAT                                       
027800                         MOVE JA TO SKRIV-SW                              
027900                      END-IF                                              
028000                   END-IF                                                 
028100                 END-IF                                                   
028300              END-IF                                                      
028400         END-EVALUATE                                                     
028500         PERFORM IMS-GET-WDD9                                             
028600     END-PERFORM                                                          
028700     IF SKRIV-SW = JA                                                     
028800        PERFORM S01-SKRIV-POST                                            
028900        MOVE NEJ TO SKRIV-SW                                              
029000     END-IF                                                               
029100                                                                          
029200     PERFORM Z-FINIT                                                      
029300     MOVE ZERO TO RETURN-CODE                                             
029400     GOBACK                                                               
029500     .                                                                    
029600     EJECT                                                                
029700 A-INIT SECTION.                                                          
029800                                                                          
029900     OPEN OUTPUT W23680                                                   
030000                                                                          
030100     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
030200     MOVE D-AAR    TO DAGENS-DATUM-AAR                                    
030300     MOVE D-MAANAD TO DAGENS-DATUM-MAANAD                                 
030400     MOVE D-DAG    TO DAGENS-DATUM-DAG                                    
030500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
030600                                                                          
030700     MOVE ZERO TO WS-SUMMA                                                
030800     MOVE NEJ TO SKRIV-SW                                                 
030900     PERFORM AA-NOLLSTAELL-VARDEN                                         
031000     .                                                                    
031100     EJECT                                                                
031200 AA-NOLLSTAELL-VARDEN SECTION.                                            
031300                                                                          
031400     MOVE ZERO   TO UT-TIAAMMDD                                           
031500                    UT-KVAVROP                                            
031600                    UT-KVANTMOT-TOT                                       
031700                    UT-IDLOPNRM-PL                                        
031800                    UT-TIAVRDAT-INL                                       
031900     MOVE SPACE  TO UT-STAT                                               
032000     MOVE +1 TO IX                                                        
032100     PERFORM UNTIL IX > 100                                               
032200        MOVE ZERO TO SPAR-IDLOPNRM-PL(IX)                                 
032300                     SPAR-KVANTMOT(IX)                                    
032400        ADD +1 TO IX                                                      
032500     END-PERFORM                                                          
032600     .                                                                    
032700     EJECT                                                                
032800 B-DATUM-2MAAN-BAK SECTION.                                               
032900                                                                          
033000     MOVE DAGENS-DATUM    TO OM-AAMMDD                                    
033100     IF OM-MM = 1 OR 2                                                    
033200        IF OM-MM = 1                                                      
033300           MOVE 11 TO OM-MM                                               
033400           ADD -1  TO OM-AA                                               
033500        END-IF                                                            
033600        IF OM-MM = 2                                                      
033700           MOVE 12 TO OM-MM                                               
033800           ADD -1  TO OM-AA                                               
033900        END-IF                                                            
034000     ELSE                                                                 
034100        ADD -2     TO OM-MM                                               
034200     END-IF                                                               
034300     .                                                                    
034400     EJECT                                                                
034500 C-OMVANDLA-DATUM SECTION.                                                
034600                                                                          
034700     MOVE IN-DAAVROP-AVS TO AVS-AAAAVV                                    
034800     MOVE AVS-AA      TO OMV-AA                                           
034900     MOVE AVS-VV      TO OMV-VV                                           
035000     MOVE IN-TILEVDAG TO OMV-D                                            
035100                                                                          
035200     MOVE 'AAVVD' TO DAT-KDDATFORM                                        
035300     MOVE OMV-AAVVD TO DAT-I-TIDATUM                                      
035400                                                                          
035500     CALL WDATKONV USING DAT-KDDATFORM                                    
035600                       DAT-I-TIDATUM DAT-O-TIDATUM DAT-KDSVAR             
035700                                                                          
035800     IF DAT-KDSVAR-OK                                                     
035900        MOVE DAT-TIAAMMDD TO UT-TIAAMMDD                                  
036000     ELSE                                                                 
036100        MOVE NEJ TO SKRIV-SW                                              
036200     END-IF                                                               
036300     .                                                                    
036400     EJECT                                                                
036500 D-DATUM-KONTROLL SECTION.                                                
036600                                                                          
036700     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
036800     MOVE DAGENS-DATUM      TO DAT-I-TIDATUM                              
036900                                                                          
037000     CALL WDATKONV USING DAT-KDDATFORM                                    
037100                       DAT-I-TIDATUM DAT-O-TIDATUM DAT-KDSVAR             
037200                                                                          
037300     IF DAT-KDSVAR-OK                                                     
037400        MOVE DAT-TIVV    TO AKTUELL-VV                                    
037500        MOVE DAT-TID     TO AKTUELL-D                                     
037600     ELSE                                                                 
037700        MOVE NEJ TO SKRIV-SW                                              
037800     END-IF                                                               
037900     .                                                                    
038000     EJECT                                                                
038100 Z-FINIT SECTION.                                                         
038200                                                                          
038300     CLOSE W23680                                                         
038400                                                                          
038500     MOVE 'S' TO POSTSUM-OPKOD                                            
038600     CALL POSTSUM USING POSTSUM-PARM                                      
038700     .                                                                    
038800     EJECT                                                                
038900 S01-SKRIV-POST SECTION.                                                  
039000                                                                          
039100     INSPECT WS-IDLEVNR                                                   
039200                  REPLACING ALL SPACE BY ZERO                             
039400     IF WS-IDLEVNR = 'AEFW8' OR 'AD0QT'                                   
039410       IF WS-IDDC = WC-CDC-SE                                             
039500         PERFORM S11-SKAPA-SKRIV-UTPOST                                   
039600       END-IF                                                             
039610     END-IF                                                               
039700     MOVE NEJ TO SKRIV-SW                                                 
039800     MOVE ZERO TO WS-SUMMA                                                
039900                  WS-KVANTMOT-TOT                                         
040000                  W-IDLOPNRM-PL                                           
040100     .                                                                    
040200     EJECT                                                                
040300 S11-SKAPA-SKRIV-UTPOST SECTION.                                          
040400                                                                          
040500     IF IX > 1                                                            
040600        MOVE +1 TO IX                                                     
040700        PERFORM UNTIL IX > 100                                            
040800           IF SPAR-IDLOPNRM-PL(IX) NOT = ZERO                             
040900              MOVE WS-SUMMA TO UT-KVAVROP                                 
041000              MOVE WS-IDARTNR TO UT-IDARTNR                               
041100              MOVE WS-IDLEVNR TO UT-IDLEVNR                               
041200              MOVE WS-TIAVRDAT-INL TO UT-TIAVRDAT-INL                     
041300              MOVE WS-KVANTMOT-TOT TO UT-KVANTMOT-TOT                     
041400              MOVE SPAR-IDLOPNRM-PL(IX) TO UT-IDLOPNRM-PL                 
041500              MOVE SPAR-KVANTMOT(IX) TO UT-KVANTMOT                       
041600              WRITE UT-POST FROM UT-AREA                                  
041700                                                                          
041800              MOVE SPACE      TO POSTSUM-TRANSTYP                         
041900              MOVE 'W23680'   TO POSTSUM-FDNAMN                           
042000              MOVE 'W23680D1' TO POSTSUM-DDNAMN2                          
042100              CALL POSTSUM USING POSTSUM-PARM                             
042200           END-IF                                                         
042300           ADD +1 TO IX                                                   
042400        END-PERFORM                                                       
042500     ELSE                                                                 
042600           MOVE WS-SUMMA TO UT-KVAVROP                                    
042700           MOVE WS-IDARTNR TO UT-IDARTNR                                  
042800           MOVE WS-IDLEVNR TO UT-IDLEVNR                                  
042900           MOVE WS-TIAVRDAT-INL TO UT-TIAVRDAT-INL                        
043000           MOVE WS-KVANTMOT-TOT TO UT-KVANTMOT-TOT                        
043100           MOVE SPAR-IDLOPNRM-PL(1) TO UT-IDLOPNRM-PL                     
043200           MOVE SPAR-KVANTMOT(1) TO UT-KVANTMOT                           
043300           WRITE UT-POST FROM UT-AREA                                     
043400                                                                          
043500           MOVE SPACE      TO POSTSUM-TRANSTYP                            
043600           MOVE 'W23680'   TO POSTSUM-FDNAMN                              
043700           MOVE 'W23680D1' TO POSTSUM-DDNAMN2                             
043800           CALL POSTSUM USING POSTSUM-PARM                                
043900     END-IF                                                               
044000                                                                          
044100     PERFORM AA-NOLLSTAELL-VARDEN                                         
044200     .                                                                    
044300     EJECT                                                                
044400* --- IMS SEKTIONER ---                                                   
044500                                                                          
044600 IMS-GET-WDD9   SECTION.                                                  
044700                                                                          
044800     CALL CBLTDLI USING GN WDD9-PCB DLI-IO-AREA                           
044900     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
045000     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
045100     PERFORM IMS-STATUSKONTROLL                                           
045200     .                                                                    
045300     SKIP3                                                                
045400 IMS-STATUSKONTROLL SECTION.                                              
045500                                                                          
045600     SET STATUS-IX TO 1                                                   
045700     SEARCH GODK-STATUS                                                   
045800       AT END                                                             
045900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
046000           DELIMITED BY SIZE INTO FELTEXT                                 
046100         DISPLAY FELTEXT                                                  
046200         CALL FELLOG                                                      
046300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
046400         CONTINUE                                                         
046500     END-SEARCH                                                           
046600     .                                                                    
