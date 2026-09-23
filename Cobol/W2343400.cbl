000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2343400.                                                
000400*AUTHOR.         HENRIK ARONSSON.                                         
000500*DATE-WRITTEN.   APRIL 93.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET JÄMFÖR FIL INNEHÅLLANDE AVROP FÖR ARTIKLAR            
001010*        MED LV-LEVERANTÖRNR SOM FÅTT GODKÄND LEV.PLAN MED                
001100*        ETT INTERNT REGISTER INNEHÅLLANDE FÖRRA VERSIONEN                
001200*        AV LEVERANSPLANER OCH UPPDATERAR NYTT INTERNREGISTER             
001210*        SAMT SKAPAR ORDERPOSTER TILL LV:S ORDERSYSTEM.                   
001220*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  SVAR FRÅN WDATKONV EJ OK                                
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- FIL MED AVROP FÖR ARTIKLAR MED                             
002510*          --- LV-LEVARANTÖRNR SOM FÅTT GODK. LEVERANSPLAN.               
002600     SELECT W23430                     ASSIGN TO W23434D1.                
002700     SKIP2                                                                
002800*          --- GAMMALT INTERNREGISTER                                     
002900*          --- INTERNREGISTER LEVERANSPLANER FÖR                          
003000*          --- ARTIKLAR MED LV-LEVERANTÖRNUMMER                           
003100     SELECT W23434-GL                  ASSIGN TO W23434D2.                
003200     SKIP2                                                                
003300*          --- NYTT INTERNREGISTER                                        
003400*          --- INTERNREGISTER LEVERANSPLANER FÖR                          
003500*          --- ARTIKLAR MED LV-LEVERANTÖRNUMMER                           
003600     SELECT W23434-NY                  ASSIGN TO W23434D3.                
003700     SKIP2                                                                
003800*          --- FIL TILL LV:S ORDERSYSTEM                                  
003900     SELECT W23436                     ASSIGN TO W23434D4.                
004000     EJECT                                                                
004100 DATA DIVISION.                                                           
004200     SKIP3                                                                
004300 FILE SECTION.                                                            
004400     SKIP3                                                                
004500 FD  W23430                                                               
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800     SKIP2                                                                
004900*01  -COPY W23430      -L.                                                
005000     SKIP3                                                                
005100 FD  W23434-GL                                                            
005200     RECORDING       F                                                    
005300     BLOCK CONTAINS  0.                                                   
005400     SKIP2                                                                
005500*01  -COPY W23434      -L.                                                
005600     SKIP3                                                                
005700 FD  W23434-NY                                                            
005800     RECORDING       F                                                    
005900     BLOCK CONTAINS  0.                                                   
006000     SKIP2                                                                
006100*01  POST -COPY W23434 -PRE  NY-  -L.                                     
006200     SKIP3                                                                
006300 FD  W23436                                                               
006400     RECORDING       F                                                    
006500     BLOCK CONTAINS  0.                                                   
006600     SKIP2                                                                
006700*01  POST -COPY W412RX5 -PRE  UT-  -L.                                    
006800     EJECT                                                                
006900 WORKING-STORAGE SECTION.                                                 
007000     SKIP2                                                                
007001*    -COPY WY2000W3                                                       
007010     SKIP3                                                                
007100 77  IDPGM                       PIC X(8)    VALUE 'W2343400'.            
007200 77  JA                          PIC X       VALUE 'J'.                   
007300 77  NEJ                         PIC X       VALUE 'N'.                   
007310 77  SPAR-IDARTNR                PIC S9(9)   VALUE ZERO COMP-3.           
007320 77  DISTR-PV                    PIC 9(4)    VALUE 55.                    
007400                                                                          
007500 77  W23430-IN-EOF-SW            PIC X       VALUE 'N'.                   
007600     88  END-OF-W23430-IN                    VALUE 'J'.                   
007700                                                                          
007800 77  W23434-GL-EOF-SW            PIC X       VALUE 'N'.                   
007900     88  END-OF-W23434-GL                    VALUE 'J'.                   
008000     EJECT                                                                
008100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008200 01  FILLER REDEFINES DAGENS-DATUM.                                       
008300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008600                                                                          
008700 01  INNEV-TIAAVV                PIC  9(4)   VALUE ZERO.                  
008800     EJECT                                                                
008900 01  DYNAMISKA-SUBPROGRAM.                                                
009000*                                                                         
009100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009200     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
009300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009500     SKIP2                                                                
009600*    --- PARAMETRAR TILL ABEND                                            
009700                                                                          
009800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010000     SKIP2                                                                
010100 01  FELTEXT.                                                             
010200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010400     EJECT                                                                
010500*    --- PARAMETRAR TILL DATKORT                                          
010600*                                                                         
010700 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W23434'.              
010800     SKIP2                                                                
010900 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
011000     SKIP2                                                                
011100*01  -COPY WDATKORT                                                       
011200     EJECT                                                                
011300*    --- PARAMETRAR TILL WDATKONV                                         
011400*                                                                         
011500*01  -COPY WDATAREA                                                       
011600     EJECT                                                                
011700*    --- PARAMETRAR TILL POSTSUM                                          
011800*                                                                         
011900*01  -COPY W0005   -PRE  POSTSUM-                                         
012000     EJECT                                                                
012100 01  IN-AREA-START               PIC X(24)   VALUE                        
012200                                 'IN-AREA-START  '.                       
012300     SKIP2                                                                
012400                                                                          
012500*01  AREA -COPY W23430     -PRE IN-                                       
012600     EJECT                                                                
012700 01  GL-AREA-START               PIC X(24)   VALUE                        
012800                                 'GL-AREA-START  '.                       
012900     SKIP2                                                                
013000                                                                          
013100*01  AREA -COPY W23434     -PRE GL-                                       
013200     EJECT                                                                
013300 01  NY-AREA-START                 PIC X(24)   VALUE                      
013400                                 'NY-AREA-START  '.                       
013500     SKIP2                                                                
013600                                                                          
013700*01  AREA -COPY W23434     -PRE NY-                                       
013800     EJECT                                                                
013900 01  UT-AREA-START               PIC X(24)   VALUE                        
014000                                 'UT-AREA-START  '.                       
014100     SKIP2                                                                
014200                                                                          
014300*01  AREA -COPY W412RX5    -PRE UT-                                       
014400     EJECT                                                                
014500 PROCEDURE DIVISION.                                                      
014600                                                                          
014700     PERFORM A-INIT                                                       
014800     PERFORM S01-LAES-W23430-IN                                           
014900     PERFORM S02-LAES-W23434-GL                                           
015000     PERFORM UNTIL END-OF-W23430-IN AND                                   
015100                   END-OF-W23434-GL                                       
015200                                                                          
015300       IF IN-IDARTNR = GL-IDARTNR                                         
015400* ----   ARTIKELN FINNS PÅ INFIL + INTERNREG                              
015500         PERFORM B-KOLLA-AVROP                                            
015600       ELSE                                                               
015700         IF IN-IDARTNR < GL-IDARTNR                                       
015800* ----     ARTIKELN SAKNAS PÅ INTERNREG                                   
015900           PERFORM C-BEHANDLA-IN-INFO                                     
016000         ELSE                                                             
016100* ----     ARTIKELN SAKNAS PÅ INFIL                                       
016200           PERFORM D-BEHANDLA-GL-INFO                                     
016300         END-IF                                                           
016400       END-IF                                                             
016500                                                                          
016800     END-PERFORM                                                          
016900                                                                          
017000     PERFORM Z-FINIT                                                      
017100                                                                          
017200     MOVE ZERO TO RETURN-CODE                                             
017300     GOBACK                                                               
017400     .                                                                    
017500     EJECT                                                                
017600 A-INIT SECTION.                                                          
017700                                                                          
017800     OPEN INPUT  W23430                                                   
017900                 W23434-GL                                                
018000                                                                          
018100     OPEN OUTPUT W23434-NY                                                
018200                 W23436                                                   
018300                                                                          
018400     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
018500     MOVE D-AAR    TO DAGENS-DATUM-AAR                                    
018600     MOVE D-MAANAD TO DAGENS-DATUM-MAANAD                                 
018700     MOVE D-DAG    TO DAGENS-DATUM-DAG                                    
018800     MOVE IDPGM    TO POSTSUM-PROGNAMN                                    
018900                                                                          
019000     MOVE 'AAMMDD'     TO DAT-KDDATFORM                                   
019100     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
019200                                                                          
019300     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
019400                         DAT-O-TIDATUM DAT-KDSVAR                         
019500                                                                          
019600     IF DAT-KDSVAR-OK                                                     
019700       MOVE DAT-TIAAVV-GRP TO INNEV-TIAAVV                                
019800     ELSE                                                                 
019900       MOVE 'SVAR 1 FRÅN WDATKONV EJ OK' TO FELTEXT-STR                   
020000       DISPLAY FELTEXT                                                    
020100       PERFORM S99-ABEND                                                  
020200     END-IF                                                               
020300     .                                                                    
020400     EJECT                                                                
020500 B-KOLLA-AVROP SECTION.                                                   
020600******************************************************************        
020700* KONTROLLERA FÖRÄNDRINGAR/TILLÄGG/BORTTAG AV AVROP FÖR ARTIKELN *        
020800******************************************************************        
020900                                                                          
020910     MOVE IN-IDARTNR TO SPAR-IDARTNR                                      
020920                                                                          
021000     PERFORM UNTIL IN-IDARTNR NOT = GL-IDARTNR  OR                        
021100                   END-OF-W23430-IN             OR                        
021200                   END-OF-W23434-GL                                       
021300                                                                          
021400       IF IN-TIAVROP-AVS = GL-TIAVROP-AVS                                 
021500* ----   AVROP FINNS PÅ INFIL + INTERNREG                                 
021501                                                                          
021502         MOVE IN-TIAVROP-AVS   TO TMP1-YYWW                               
021503         MOVE INNEV-TIAAVV     TO TMP2-YYWW                               
021504         PERFORM WY2000P3                                                 
021510         IF TMP1-YYWW <= TMP2-YYWW                                        
021520           CONTINUE                                                       
021600         ELSE                                                             
021700           IF IN-KVAVROP = GL-KVAVROP                                     
021800             PERFORM S21-SKRIV-SAMMA-POST-INTERNREG                       
021900           ELSE                                                           
022000             PERFORM S31-SKAPA-ORDERPOST                                  
022100             PERFORM S22-SKRIV-AENDR-POST-INTERNREG                       
022200           END-IF                                                         
022210         END-IF                                                           
022300         PERFORM S01-LAES-W23430-IN                                       
022400         PERFORM S02-LAES-W23434-GL                                       
022500       ELSE                                                               
022501         MOVE IN-TIAVROP-AVS   TO TMP1-YYWW                               
022502         MOVE GL-TIAVROP-AVS   TO TMP2-YYWW                               
022503         PERFORM WY2000P3                                                 
022600         IF TMP1-YYWW < TMP2-YYWW                                         
022700* ----     AVROP SAKNAS PÅ INTERNREG                                      
022800* ----     (DVS. AVROP HAR TILLKOMMIT)                                    
022801                                                                          
022802           MOVE IN-TIAVROP-AVS   TO TMP1-YYWW                             
022803           MOVE INNEV-TIAAVV     TO TMP2-YYWW                             
022804           PERFORM WY2000P3                                               
022810           IF TMP1-YYWW <= TMP2-YYWW                                      
022820             CONTINUE                                                     
022830           ELSE                                                           
023000             PERFORM S31-SKAPA-ORDERPOST                                  
023100             PERFORM S23-SKRIV-NY-POST-INTERNREG                          
023110           END-IF                                                         
023200           PERFORM S01-LAES-W23430-IN                                     
023300         ELSE                                                             
023400* ----     AVROP SAKNAS PÅ INFIL                                          
023500* ----     (DVS. AVROP HAR FÖRSVUNNIT)                                    
023501                                                                          
023502           MOVE GL-TIAVROP-AVS   TO TMP1-YYWW                             
023503           MOVE INNEV-TIAAVV     TO TMP2-YYWW                             
023504           PERFORM WY2000P3                                               
023510           IF TMP1-YYWW <= TMP2-YYWW                                      
023520             CONTINUE                                                     
023530           ELSE                                                           
023710             PERFORM S32-SKAPA-ORDERPOST-NOLL                             
023720           END-IF                                                         
023800           PERFORM S02-LAES-W23434-GL                                     
023900         END-IF                                                           
024000       END-IF                                                             
024100     END-PERFORM                                                          
024110                                                                          
024111* ---- KOLLA OM DET FINNS AVROP KVAR PÅ INTERNREG                         
024112                                                                          
024120     IF IN-IDARTNR > GL-IDARTNR                                           
024121* ----  AVROP SAKNAS PÅ INFIL                                             
024122* ----  (DVS. AVROP HAR FÖRSVUNNIT)                                       
024123                                                                          
024124       PERFORM UNTIL GL-IDARTNR > SPAR-IDARTNR OR                         
024125                     END-OF-W23434-GL                                     
024126         MOVE GL-TIAVROP-AVS   TO TMP1-YYWW                               
024127         MOVE INNEV-TIAAVV     TO TMP2-YYWW                               
024128         PERFORM WY2000P3                                                 
024130         IF TMP1-YYWW <= TMP2-YYWW                                        
024131           CONTINUE                                                       
024132         ELSE                                                             
024133           PERFORM S32-SKAPA-ORDERPOST-NOLL                               
024134         END-IF                                                           
024135         PERFORM S02-LAES-W23434-GL                                       
024136       END-PERFORM                                                        
024137     END-IF                                                               
024138                                                                          
024139* ---- KOLLA OM DET FINNS AVROP KVAR PÅ INFIL                             
024140                                                                          
024141     IF IN-IDARTNR < GL-IDARTNR                                           
024142* ---  AVROP SAKNAS PÅ INTERNREG                                          
024143* ---  (DVS. AVROP HAR TILLKOMMIT)                                        
024144                                                                          
024145       PERFORM UNTIL IN-IDARTNR > SPAR-IDARTNR OR                         
024146                     END-OF-W23430-IN                                     
024147         MOVE IN-TIAVROP-AVS   TO TMP1-YYWW                               
024148         MOVE INNEV-TIAAVV     TO TMP2-YYWW                               
024149         PERFORM WY2000P3                                                 
024151         IF TMP1-YYWW <= TMP2-YYWW OR                                     
024152            IN-TIAVROP-AVS  = +9999                                       
024153           CONTINUE                                                       
024154         ELSE                                                             
024155           PERFORM S31-SKAPA-ORDERPOST                                    
024156           PERFORM S23-SKRIV-NY-POST-INTERNREG                            
024157         END-IF                                                           
024158         PERFORM S01-LAES-W23430-IN                                       
024159       END-PERFORM                                                        
024160     END-IF                                                               
024200     .                                                                    
024300     EJECT                                                                
030400 C-BEHANDLA-IN-INFO SECTION.                                              
030500******************************************************************        
030600* SKAPA ORDERPOSTER SAMT UPPDATERA INTERNREGISTRET MED NY INFO   *        
030700******************************************************************        
030800                                                                          
030900     PERFORM UNTIL IN-IDARTNR >= GL-IDARTNR OR                            
031000                   END-OF-W23430-IN                                       
031100                                                                          
031101       MOVE IN-TIAVROP-AVS   TO TMP1-YYWW                                 
031102       MOVE INNEV-TIAAVV     TO TMP2-YYWW                                 
031103       PERFORM WY2000P3                                                   
031200       IF TMP1-YYWW <= TMP2-YYWW OR                                       
031210          IN-TIAVROP-AVS  = +9999                                         
031300         CONTINUE                                                         
031400       ELSE                                                               
031500         PERFORM S31-SKAPA-ORDERPOST                                      
031600         PERFORM S23-SKRIV-NY-POST-INTERNREG                              
031700       END-IF                                                             
031800                                                                          
031900       PERFORM S01-LAES-W23430-IN                                         
032000     END-PERFORM                                                          
032100     .                                                                    
032200     EJECT                                                                
032300 D-BEHANDLA-GL-INFO SECTION.                                              
032400******************************************************************        
032500* SKRIV ALLA AVROP SOM EJ ÄR PASSERADE PÅ INTERNREGISTRET        *        
032600******************************************************************        
032700                                                                          
032800     PERFORM UNTIL GL-IDARTNR >= IN-IDARTNR OR                            
032900                   END-OF-W23434-GL                                       
033000                                                                          
033001       MOVE GL-TIAVROP-AVS   TO TMP1-YYWW                                 
033002       MOVE INNEV-TIAAVV     TO TMP2-YYWW                                 
033003       PERFORM WY2000P3                                                   
033100       IF TMP1-YYWW <= TMP2-YYWW                                          
033200         CONTINUE                                                         
033300       ELSE                                                               
033400         MOVE GL-AREA TO NY-AREA                                          
033500         PERFORM S11-SKRIV-W23434-NY                                      
033600       END-IF                                                             
033700                                                                          
033800       PERFORM S02-LAES-W23434-GL                                         
033900     END-PERFORM                                                          
034000     .                                                                    
034100     EJECT                                                                
034200 Z-FINIT SECTION.                                                         
034300                                                                          
034400     CLOSE W23430                                                         
034500           W23434-GL                                                      
034600           W23434-NY                                                      
034700           W23436                                                         
034800                                                                          
034900     MOVE 'S' TO POSTSUM-OPKOD                                            
035000     CALL POSTSUM USING POSTSUM-PARM                                      
035100     .                                                                    
035200     EJECT                                                                
035300 S01-LAES-W23430-IN SECTION.                                              
035400                                                                          
035500     READ W23430 INTO IN-AREA                                             
035600     AT END                                                               
035700        MOVE +999999999 TO IN-IDARTNR                                     
035800        SET END-OF-W23430-IN TO TRUE                                      
035900                                                                          
036000     NOT AT END                                                           
036100        MOVE SPACE      TO POSTSUM-TRANSTYP                               
036200        MOVE 'W23430'   TO POSTSUM-FDNAMN                                 
036300        MOVE 'W23434D1' TO POSTSUM-DDNAMN2                                
036400        CALL POSTSUM USING POSTSUM-PARM                                   
036500     END-READ                                                             
036600     .                                                                    
036700     EJECT                                                                
036800 S02-LAES-W23434-GL SECTION.                                              
036900                                                                          
037000     READ W23434-GL INTO GL-AREA                                          
037100     AT END                                                               
037200        MOVE +999999999 TO GL-IDARTNR                                     
037300        SET END-OF-W23434-GL TO TRUE                                      
037400                                                                          
037500     NOT AT END                                                           
037600        MOVE SPACE      TO POSTSUM-TRANSTYP                               
037700        MOVE 'W23434'   TO POSTSUM-FDNAMN                                 
037800        MOVE 'W23434D2' TO POSTSUM-DDNAMN2                                
037900        CALL POSTSUM USING POSTSUM-PARM                                   
038000     END-READ                                                             
038100     .                                                                    
038200     EJECT                                                                
038300 S11-SKRIV-W23434-NY SECTION.                                             
038400                                                                          
038500     WRITE NY-POST FROM NY-AREA                                           
038600                                                                          
038700     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
038800     MOVE 'W23434'   TO POSTSUM-FDNAMN                                    
038900     MOVE 'W23434D3' TO POSTSUM-DDNAMN2                                   
039000     CALL POSTSUM USING POSTSUM-PARM                                      
039100     .                                                                    
039200     EJECT                                                                
039300 S12-SKRIV-W23436-UT SECTION.                                             
039400                                                                          
039500     WRITE UT-POST FROM UT-AREA                                           
039600                                                                          
039700     MOVE UT-IDTYP   TO POSTSUM-TRANSTYP                                  
039800     MOVE 'W23436'   TO POSTSUM-FDNAMN                                    
039900     MOVE 'W23434D4' TO POSTSUM-DDNAMN2                                   
040000     CALL POSTSUM USING POSTSUM-PARM                                      
040100     .                                                                    
040200     EJECT                                                                
040210 S21-SKRIV-SAMMA-POST-INTERNREG SECTION.                                  
040220******************************************************************        
040230* UPPDATERA INTERNREGISTRET MED OFÖRÄNDRAD POST                  *        
040240******************************************************************        
040250                                                                          
040260     MOVE GL-AREA TO NY-AREA                                              
040270     PERFORM S11-SKRIV-W23434-NY                                          
040280     .                                                                    
040290     EJECT                                                                
040291 S22-SKRIV-AENDR-POST-INTERNREG SECTION.                                  
040292******************************************************************        
040293* UPPDATERA INTERNREGISTRET MED FÖRÄNDRAD POST                   *        
040294******************************************************************        
040295                                                                          
040296     MOVE IN-IDARTNR     TO NY-IDARTNR                                    
040297     MOVE IN-TIAVROP-AVS TO NY-TIAVROP-AVS                                
040298     MOVE IN-KVAVROP     TO NY-KVAVROP                                    
040299     MOVE GL-IDORDNR7    TO NY-IDORDNR7                                   
040300     MOVE GL-TITPO       TO NY-TITPO                                      
040301     MOVE DAGENS-DATUM   TO NY-TIUPPDAT                                   
040302                                                                          
040303     PERFORM S11-SKRIV-W23434-NY                                          
040304     .                                                                    
040305     EJECT                                                                
040306 S23-SKRIV-NY-POST-INTERNREG SECTION.                                     
040307******************************************************************        
040308* UPPDATERA INTERNREGISTET MED NY POST                           *        
040309******************************************************************        
040310                                                                          
040311     MOVE IN-IDARTNR     TO NY-IDARTNR                                    
040312     MOVE IN-TIAVROP-AVS TO NY-TIAVROP-AVS                                
040313     MOVE IN-KVAVROP     TO NY-KVAVROP                                    
040314     MOVE UT-IDORDNR7    TO NY-IDORDNR7                                   
040315     MOVE UT-TITPO       TO NY-TITPO                                      
040316     MOVE DAGENS-DATUM   TO NY-TIUPPDAT                                   
040317                                                                          
040318     PERFORM S11-SKRIV-W23434-NY                                          
040319     .                                                                    
040320     EJECT                                                                
040330 S31-SKAPA-ORDERPOST SECTION.                                             
040400******************************************************************        
040500* SKAPA ORDERPOST TILL LV:S ORDERSYSTEM                          *        
040600******************************************************************        
040700                                                                          
040710     MOVE SPACE         TO UT-AREA                                        
040720                                                                          
040800     MOVE 'RX1'         TO UT-IDTYP                                       
040900     MOVE DISTR-PV      TO UT-IDDISTR                                     
041000     MOVE +20000        TO UT-IDORDNR7                                    
041010     ADD IN-TIAVROP-AVS TO UT-IDORDNR7                                    
041100     MOVE +4            TO UT-KDORDKL                                     
041200     MOVE ZERO          TO UT-KDVRINFO                                    
041300     MOVE +1            TO UT-KDTPOTYP                                    
041400     MOVE IN-IDARTNR    TO UT-IDARTNR                                     
041500     MOVE ZERO          TO UT-REKSIFFR                                    
041600     MOVE IN-KVAVROP    TO UT-KVBEART                                     
041700     MOVE ZERO          TO UT-KDKVBRYT                                    
041900     MOVE SPACE         TO UT-BERADREF                                    
042000     MOVE NEJ           TO UT-FLSLATT                                     
042100     MOVE SPACE         TO UT-FTGKOD                                      
042200                           UT-KDMASK                                      
042300                                                                          
042311     MOVE 'AAVV'         TO DAT-KDDATFORM                                 
042312     MOVE IN-TIAVROP-AVS TO DAT-I-TIDATUM                                 
042313                                                                          
042314     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
042315                         DAT-O-TIDATUM DAT-KDSVAR                         
042316                                                                          
042317     IF DAT-KDSVAR-OK                                                     
042318       MOVE DAT-TIAAMMDD TO UT-TITPO                                      
042319     ELSE                                                                 
042320       MOVE 'SVAR 2 FRÅN WDATKONV EJ OK' TO FELTEXT-STR                   
042321       DISPLAY FELTEXT                                                    
042322       PERFORM S99-ABEND                                                  
042330     END-IF                                                               
042340                                                                          
042400     PERFORM S12-SKRIV-W23436-UT                                          
042500     .                                                                    
042600     EJECT                                                                
042610 S32-SKAPA-ORDERPOST-NOLL SECTION.                                        
042620******************************************************************        
042630* SKAPA ORDERPOST MED NOLL I KVANT TILL LV:S ORDERSYSTEM         *        
042640******************************************************************        
042650                                                                          
042651     MOVE SPACE       TO UT-AREA                                          
042652                                                                          
042660     MOVE 'RX1'       TO UT-IDTYP                                         
042670     MOVE DISTR-PV    TO UT-IDDISTR                                       
042680     MOVE ZERO        TO UT-IDKUNDNR                                      
042690     MOVE GL-IDORDNR7 TO UT-IDORDNR7                                      
042693     MOVE +4          TO UT-KDORDKL                                       
042694     MOVE ZERO        TO UT-KDVRINFO                                      
042695     MOVE +1          TO UT-KDTPOTYP                                      
042696     MOVE GL-IDARTNR  TO UT-IDARTNR                                       
042697     MOVE ZERO        TO UT-REKSIFFR                                      
042698                         UT-KVBEART                                       
042699                         UT-KDKVBRYT                                      
042700     MOVE SPACE       TO UT-BERADREF                                      
042701     MOVE NEJ         TO UT-FLSLATT                                       
042703     MOVE SPACE       TO UT-FTGKOD                                        
042704                         UT-KDMASK                                        
042705                                                                          
042706     MOVE 'AAVV'         TO DAT-KDDATFORM                                 
042707     MOVE GL-TIAVROP-AVS TO DAT-I-TIDATUM                                 
042708                                                                          
042709     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
042710                         DAT-O-TIDATUM DAT-KDSVAR                         
042711                                                                          
042712     IF DAT-KDSVAR-OK                                                     
042713       MOVE DAT-TIAAMMDD TO UT-TITPO                                      
042714     ELSE                                                                 
042715       MOVE 'SVAR 3 FRÅN WDATKONV EJ OK' TO FELTEXT-STR                   
042716       DISPLAY FELTEXT                                                    
042717       PERFORM S99-ABEND                                                  
042718     END-IF                                                               
042719                                                                          
042720     PERFORM S12-SKRIV-W23436-UT                                          
042721     .                                                                    
042722     EJECT                                                                
042730 S99-ABEND SECTION.                                                       
042800                                                                          
042900     SKIP2                                                                
043000     MOVE 'S' TO POSTSUM-OPKOD                                            
043100     CALL POSTSUM USING POSTSUM-PARM                                      
043200     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
043300     .                                                                    
043310     EJECT                                                                
043400*    -COPY WY2000P3                                                       
