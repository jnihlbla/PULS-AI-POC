000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4759A00.                                                
000400 AUTHOR.         MARGARETA GABRIELSON.                                    
000500 DATE-WRITTEN.   95/01/17.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PROGRAMMET LÄSER DAGLIGA INTRASTATUTSUG FRÅN                     
001000*        KREDITERINGEN OCH FAKTURERINGEN OCH SKAPAR FIL                   
001100*        TILL VOLVO TRANSPORT MED MÅNADSINTRASTATINFO                     
001200*                                                                         
001300*        PROGRAMMET LÄSER      WDK6                                       
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900*    CHANGE LOG:                                                          
002000*                                                                         
002100*      YY/MM/DD - NAME            - CHANGE DESCRIPTION                    
002200*      ----------------------------------------------------------         
002300*      15/10/09 - REDDY RAHUL     - ADD STAT NUMBER AND WEIGHT            
002400*                                   IN INTRASTE FILES.                    
002500*                                   E'TRACKER 10265098                    
002600*                                                                         
002700*      21/12/27 - CAMELIA O.      - ADD VAT CODE, COUNTRY OF ORIG.        
002800*                                   AND TRANSACTIONS CODE                 
002900*                                   IN INTRASTE FILES.                    
003000*                                   STORY 2523293                         
003100*                                                                         
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     SKIP2                                                                
003600 INPUT-OUTPUT SECTION.                                                    
003700                                                                          
003800 FILE-CONTROL.                                                            
003900     SKIP2                                                                
004000*          --- DAGLIGA FAKTURERINGSFILER OCH KREDITERINGSFILER            
004100     SELECT W4758N                     ASSIGN TO W4759AD1.                
004200     SKIP2                                                                
004300*          --- FIL TILL VOLVO TRANSPORT                                   
004400     SELECT W4759A                     ASSIGN TO W4759AD2.                
004500     EJECT                                                                
004600 DATA DIVISION.                                                           
004700     SKIP2                                                                
004800 FILE SECTION.                                                            
004900     SKIP3                                                                
005000 FD  W4758N                                                               
005100     RECORDING       F                                                    
005200     BLOCK CONTAINS  0.                                                   
005300                                                                          
005400*01  -COPY W475INT      -L.                                               
005500     SKIP3                                                                
005600 FD  W4759A                                                               
005700     RECORDING       F                                                    
005800     BLOCK CONTAINS  0.                                                   
005900*01  UT-POST   -COPY A7290A01   -L.                                       
006000     EJECT                                                                
006100 WORKING-STORAGE SECTION.                                                 
006200                                                                          
006300*    -- CHECKED BY WY2000                                                 
006400     SKIP3                                                                
006500 77  IDPGM                       PIC X(8)    VALUE 'W4759A00'.            
006600 77  JA                          PIC X       VALUE 'J'.                   
006700 77  NEJ                         PIC X       VALUE 'N'.                   
006800                                                                          
006900 77  W4758N-EOF-SW               PIC X       VALUE 'N'.                   
007000     88  INFIL-EOF                           VALUE 'J'.                   
007100     EJECT                                                                
007200 01  SPAR-AREOR.                                                          
007300     03  SPAR-IDLANDX2           PIC X(2)         VALUE SPACE.            
007400     03  SPAR-IDARTNR            PIC S9(9) COMP-3 VALUE ZERO.             
007500     03  SPAR-IDSTATNR           PIC S9(9) COMP-3 VALUE ZERO.             
007600     03  SPAR-KDINTTYP           PIC S9(2) COMP-3 VALUE ZERO.             
007700     03  SPAR-KDARTURS           PIC X(2)         VALUE SPACE.            
007800     03  SPAR-IDVAT              PIC X(17)        VALUE SPACE.            
007900     SKIP2                                                                
008000 01  SUM-AREOR.                                                           
008100     03  SUM-KVLEVART            PIC S9(7) COMP-3 VALUE ZERO.             
008200     03  SUM-SUFKTBEL            PIC S9(9)V9(2) COMP-3                    
008300                                 VALUE ZERO.                              
008400 01  WS-VKARTTOT                 PIC S9(15)V9(2)  VALUE ZERO.             
008500     SKIP2                                                                
008600 01  HJALP-AREOR.                                                         
008700     03  W-KDSORT                PIC X(2)  VALUE SPACE.                   
008800     EJECT                                                                
008900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009000 01  FILLER REDEFINES DAGENS-DATUM.                                       
009100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009400     EJECT                                                                
009500 01  DYNAMISKA-SUBPROGRAM.                                                
009600*                                                                         
009700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
010100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010200     03  W009CIA                 PIC X(8)    VALUE 'W009CIA'.             
010300     SKIP2                                                                
010400*    --- PARAMETRAR TILL ABEND                                            
010500                                                                          
010600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010800     SKIP2                                                                
010900 01  FELTEXT.                                                             
011000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011200     EJECT                                                                
011300*    --- PARAMETRAR TILL DATKORT                                          
011400*                                                                         
011500 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W4759A'.              
011600     SKIP2                                                                
011700 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
011800     SKIP2                                                                
011900*01  -COPY WDATKORT                                                       
012000     EJECT                                                                
012100*    --- PARAMETRAR TILL POSTSUM                                          
012200*                                                                         
012300*01  -COPY W0005   -PRE  POSTSUM-                                         
012400     EJECT                                                                
012500 01  W009-AREA-START             PIC X(24)   VALUE                        
012600                                 'W009CIA-AREA   '.                       
012700     SKIP2                                                                
012800                                                                          
012900*01  -COPY W009CIA                                                        
013000     EJECT                                                                
013100 01  IN-AREA-START               PIC X(24)   VALUE                        
013200                                 'IN-AREA-START  '.                       
013300     SKIP2                                                                
013400                                                                          
013500*01  AREA -COPY W475INT     -PRE IN-                                      
013600     EJECT                                                                
013700 01  UT-AREA-START               PIC X(24)   VALUE                        
013800                                 'UT-AREA-START  '.                       
013900     SKIP2                                                                
014000                                                                          
014100*01  AREA -COPY A7290A01    -PRE UT-                                      
014200     EJECT                                                                
014300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014400*                                                                         
014500     EJECT                                                                
014600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014700     SKIP3                                                                
014800 01  NYCKLAR-TILL-DLI.                                                    
014900     03  W-IDARTNR-X.                                                     
015000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
015100     03  W-KDSEGKEY-X.                                                    
015200         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
015300     SKIP2                                                                
015400*    --- STATUS-KOD FRÅN IMS                                              
015500 01  STATUS-WS                   PIC XX.                                  
015600     88  SEGMENT-FINNS                       VALUE '  '.                  
015700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015900     SKIP2                                                                
016000 01  GODK-STATUSKODER.                                                    
016100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016200     SKIP3                                                                
016300 01  SSA1                        PIC X(64).                               
016400 01  SSA2                        PIC X(64).                               
016500     EJECT                                                                
016600*    --- IMS FUNKTIONSKODER                                               
016700*01  -COPY W0003                                                          
016800     EJECT                                                                
016900*    ---  DLI INPUT-OUTPUT AREA                                           
017000                                                                          
017100 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK601'.         
017200 01  DLI-IO-WDK601.                                                       
017300*    03  -COPY WDK601  -PRE WDK6-                                         
017400     EJECT                                                                
017500                                                                          
017600 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK611'.         
017700 01  DLI-IO-WDK611.                                                       
017800*    03  -COPY WDK611  -PRE WDK6-                                         
017900     EJECT                                                                
018000                                                                          
018100     EJECT                                                                
018200 LINKAGE SECTION.                                                         
018300                                                                          
018400     EJECT                                                                
018500*01  -COPY W0008  -PRE WDK6-                                              
018600     05  FILLER                  PIC X.                                   
018700     EJECT                                                                
018800 PROCEDURE DIVISION  USING WDK6-PCB.                                      
018900     ENTRY 'DLITCBL' USING WDK6-PCB.                                      
019000                                                                          
019100     PERFORM A-INIT                                                       
019200     PERFORM S01-LAES-W4758N                                              
019300     MOVE IN-IDLANDX2     TO SPAR-IDLANDX2                                
019400     MOVE IN-IDARTNR      TO SPAR-IDARTNR                                 
019500     MOVE IN-IDSTATNR     TO SPAR-IDSTATNR                                
019600     MOVE IN-KDINTTYP     TO SPAR-KDINTTYP                                
019700     MOVE IN-KDARTURS     TO SPAR-KDARTURS                                
019800     MOVE IN-IDVAT        TO SPAR-IDVAT                                   
019900     PERFORM UNTIL INFIL-EOF                                              
019910**** WE SHALL NOT ADD ANY GB VAT REG TO THE OUTPUT FILE                   
020000       IF IN-IDVAT(1:2) = 'GB'                                            
020010         CONTINUE                                                         
020020       ELSE                                                               
020100         IF IN-IDLANDX2 = SPAR-IDLANDX2 AND                               
020200            IN-IDARTNR = SPAR-IDARTNR AND                                 
020300            IN-KDARTURS = SPAR-KDARTURS AND                               
020400            IN-KDINTTYP = SPAR-KDINTTYP                                   
020500           PERFORM B-ADDERA-SUMMOR                                        
020600         ELSE                                                             
020700           IF SUM-SUFKTBEL = ZERO OR                                      
020800              SUM-KVLEVART = ZERO                                         
020900             CONTINUE                                                     
021000           ELSE                                                           
021100             PERFORM C-FLYTTA-TILL-UTFIL                                  
021200             IF W-KDSORT = 'SW'                                           
021300               CONTINUE                                                   
021400             ELSE                                                         
021500               PERFORM S11-SKRIV-W4759A                                   
021600             END-IF                                                       
021700           END-IF                                                         
021800           PERFORM D-NOLLA-SUMMOR                                         
021900           PERFORM B-ADDERA-SUMMOR                                        
022000         END-IF                                                           
022100       END-IF                                                             
022200       PERFORM S01-LAES-W4758N                                            
022300     END-PERFORM                                                          
022400     IF SUM-SUFKTBEL    = ZERO                                            
022500        OR SUM-KVLEVART = ZERO                                            
022600        CONTINUE                                                          
022700     ELSE                                                                 
022800        PERFORM C-FLYTTA-TILL-UTFIL                                       
022900        PERFORM S11-SKRIV-W4759A                                          
023000     END-IF                                                               
023100                                                                          
023200     PERFORM Z-FINIT                                                      
023300                                                                          
023400     MOVE ZERO TO RETURN-CODE                                             
023500     GOBACK                                                               
023600     .                                                                    
023700     EJECT                                                                
023800 A-INIT SECTION.                                                          
023900                                                                          
024000     OPEN INPUT  W4758N                                                   
024100                                                                          
024200     OPEN OUTPUT W4759A                                                   
024300                                                                          
024400     MOVE ZERO        TO SUM-KVLEVART                                     
024500                         SUM-SUFKTBEL                                     
024600     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
024700     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
024800     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
024900     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
025000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
025100                                                                          
025200     MOVE SPACE                   TO UT-FORSEL-AREA                       
025300     MOVE '01'                    TO UT-POSTTYP                           
025400     MOVE '01441001'              TO UT-FILAVSID                          
025500     MOVE 'VOLVO CAR PARTS'       TO UT-FILAVSNAMN                        
025600     MOVE '5560743089'            TO UT-AVSORGNR                          
025700     MOVE ZERO                    TO UT-FILTID                            
025800     IF DAGENS-DATUM-AAR       < '90'                                     
025900        MOVE '20'                 TO UT-PERIOD (1:2)                      
026000                                     UT-FILDATUM (1:2)                    
026100     ELSE                                                                 
026200        MOVE '19'                 TO UT-PERIOD (1:2)                      
026300                                     UT-FILDATUM (1:2)                    
026400     END-IF                                                               
026500     MOVE DAGENS-DATUM-AAR        TO UT-PERIOD (3:2)                      
026600                                     UT-FILDATUM (3:2)                    
026700     MOVE DAGENS-DATUM-MAANAD     TO UT-PERIOD (5:2)                      
026800                                     UT-FILDATUM (5:2)                    
026900     MOVE DAGENS-DATUM-DAG        TO UT-FILDATUM (7:2)                    
027000     MOVE 'U'                     TO UT-FORSELKOD                         
027100                                                                          
027200     PERFORM S11-SKRIV-W4759A                                             
027300     .                                                                    
027400     EJECT                                                                
027500 B-ADDERA-SUMMOR SECTION.                                                 
027600                                                                          
027700     ADD IN-KVLEVART      TO SUM-KVLEVART                                 
027800     ADD IN-SUFKTBEL      TO SUM-SUFKTBEL                                 
027900                                                                          
028000     .                                                                    
028100     EJECT                                                                
028200 C-FLYTTA-TILL-UTFIL SECTION.                                             
028300                                                                          
028400     MOVE SPACE           TO UT-FORSEL-AREA                               
028500     MOVE '02'            TO UT-POSTTYP                                   
028600     MOVE SPAR-IDLANDX2   TO UT-LANDKOD IN UT-ARTIKEL-AREA                
028700     MOVE SPACE           TO UT-TRANSPORTSATT IN UT-ARTIKEL-AREA          
028800     MOVE SUM-KVLEVART    TO UT-ANTAL                                     
028900     MOVE SUM-SUFKTBEL    TO UT-VARDE IN UT-ARTIKEL-AREA                  
029000     MOVE SPAR-IDSTATNR   TO UT-IDSTATNR IN UT-ARTIKEL-AREA               
029100     MOVE SPAR-KDINTTYP   TO UT-KDINTTYP IN UT-ARTIKEL-AREA               
029200     MOVE SPAR-KDARTURS   TO UT-KDARTURS IN UT-ARTIKEL-AREA               
029300     MOVE SPAR-IDVAT      TO UT-IDVAT    IN UT-ARTIKEL-AREA               
029400                                                                          
029500                                                                          
029600     MOVE SPAR-IDARTNR    TO W-IDARTNR                                    
029700                             CIA-IDARTBET-IN                              
029800                                                                          
029900     MOVE 'VO'            TO CIA-IDARTPRE-IN                              
030000     CALL W009CIA   USING CIA-W009CIA                                     
030100     MOVE CIA-IDARTBET-UT TO UT-ARTIKELNR                                 
030200                                                                          
030300     PERFORM IMS-GET-WDK601                                               
030400     IF SEGMENT-FINNS                                                     
030500       MOVE WDK6-ART-KDSORT   TO W-KDSORT                                 
030600       EVALUATE W-KDSORT                                                  
030700         WHEN 'ST'                                                        
030800           MOVE 'PCE '    TO UT-ANTALSTYP                                 
030900         WHEN 'SA'                                                        
031000           MOVE 'PCE '    TO UT-ANTALSTYP                                 
031100         WHEN 'KG'                                                        
031200           MOVE 'KGM '    TO UT-ANTALSTYP                                 
031300         WHEN 'M '                                                        
031400           MOVE 'MTR '    TO UT-ANTALSTYP                                 
031500         WHEN ' M'                                                        
031600           MOVE 'MTR '    TO UT-ANTALSTYP                                 
031700         WHEN 'L '                                                        
031800           MOVE 'DMQ '    TO UT-ANTALSTYP                                 
031900         WHEN ' L'                                                        
032000           MOVE 'DMQ '    TO UT-ANTALSTYP                                 
032100         WHEN 'MM'                                                        
032200           MOVE 'MMT '    TO UT-ANTALSTYP                                 
032300         WHEN 'G '                                                        
032400           MOVE 'GRM '    TO UT-ANTALSTYP                                 
032500         WHEN ' G'                                                        
032600           MOVE 'GRM '    TO UT-ANTALSTYP                                 
032700         WHEN 'C2'                                                        
032800           MOVE 'CMK '    TO UT-ANTALSTYP                                 
032900         WHEN 'M2'                                                        
033000           MOVE 'MTK '    TO UT-ANTALSTYP                                 
033100         WHEN 'ML'                                                        
033200           MOVE 'CMQ '    TO UT-ANTALSTYP                                 
033300         WHEN 'SW'                                                        
033400           MOVE 'SW  '    TO UT-ANTALSTYP                                 
033500         WHEN OTHER                                                       
033600           MOVE 'PCE '    TO UT-ANTALSTYP                                 
033700       END-EVALUATE                                                       
033800                                                                          
033900       PERFORM IMS-GET-WDK611                                             
034000       IF SEGMENT-FINNS                                                   
034100*        IDSTATNR FOR BELGIUM                                             
034200         IF UT-IDSTATNR = ZERO                                            
034300           IF WDK6-CLAG-IDSTATNR(3) > ZERO                                
034400             MOVE WDK6-CLAG-IDSTATNR(3) TO UT-IDSTATNR                    
034500           ELSE                                                           
034600             MOVE ZERO                TO UT-IDSTATNR                      
034700           END-IF                                                         
034800         END-IF                                                           
034900*        TOTAL WEIGHT                                                     
035000         IF WDK6-CLAG-VKART > ZERO                                        
035100           COMPUTE UT-VKARTTOT = (SUM-KVLEVART * WDK6-CLAG-VKART)         
035200*                                                                         
035300*---       IN THE NEXT PGM. W4759D THIS TOTAL, WHICH IS IN GRAMS          
035400*  -       WILL BE DEVIDED BY 1000 TO MAKE IT IN KG.                      
035500*          THE DEVIDED TOTAL CAN BE 0 IF THE GRAMS ARE VERY FEW.          
035600*          IN THIS SITUATION, WE'LL MAKE IT 0,01 BY PUTTING 10            
035700*          IN THE UT-VKARTTOT IN THIS PGM.                                
035800*                                                                         
035900           COMPUTE WS-VKARTTOT = UT-VKARTTOT / 1000                       
036000           IF WS-VKARTTOT = ZERO                                          
036100             MOVE 10                  TO UT-VKARTTOT                      
036200           END-IF                                                         
036300         ELSE                                                             
036400           MOVE 10                    TO UT-VKARTTOT                      
036500         END-IF                                                           
036600       ELSE                                                               
036700         MOVE 10                      TO UT-VKARTTOT                      
036800       END-IF                                                             
036900     ELSE                                                                 
037000*---   INITIATE TIS-PARTS WITH A MIN. WEIGHT AS THERE ARE                 
037100*      NOT ON THE WDK6 IN PULS AND THEY NEED TO HAVE SOME                 
037200*      WEIGHT ON THE FINALE FILE - ACCORDING TO HD 27/4 '22               
037300*                                                                         
037400       MOVE 'PCE '                    TO UT-ANTALSTYP                     
037500       MOVE 10                        TO UT-VKARTTOT                      
037600     END-IF                                                               
037700     .                                                                    
037800     EJECT                                                                
037900 D-NOLLA-SUMMOR SECTION.                                                  
038000                                                                          
038100     MOVE ZERO            TO SUM-KVLEVART                                 
038200                             SUM-SUFKTBEL                                 
038300     MOVE IN-IDLANDX2     TO SPAR-IDLANDX2                                
038400     MOVE IN-IDARTNR      TO SPAR-IDARTNR                                 
038500     MOVE IN-IDSTATNR     TO SPAR-IDSTATNR                                
038600     MOVE IN-KDINTTYP     TO SPAR-KDINTTYP                                
038700     MOVE IN-KDARTURS     TO SPAR-KDARTURS                                
038800     MOVE IN-IDVAT        TO SPAR-IDVAT                                   
038900                                                                          
039000     .                                                                    
039100     EJECT                                                                
039200 Z-FINIT SECTION.                                                         
039300     CLOSE W4758N                                                         
039400           W4759A                                                         
039500     SKIP2                                                                
039600     MOVE 'S' TO POSTSUM-OPKOD                                            
039700     CALL POSTSUM USING POSTSUM-PARM                                      
039800     .                                                                    
039900     EJECT                                                                
040000 S01-LAES-W4758N  SECTION.                                                
040100     READ W4758N INTO IN-AREA                                             
040200     AT END                                                               
040300        MOVE HIGH-VALUE TO IN-AREA                                        
040400        MOVE JA TO W4758N-EOF-SW                                          
040500                                                                          
040600     NOT AT END                                                           
040700        MOVE 'W4758N' TO POSTSUM-FDNAMN                                   
040800        MOVE 'W4759AD1' TO POSTSUM-DDNAMN2                                
040900        MOVE IN-IDPTYP TO POSTSUM-TRANSTYP                                
041000        CALL POSTSUM USING POSTSUM-PARM                                   
041100     END-READ                                                             
041200     .                                                                    
041300     EJECT                                                                
041400 S11-SKRIV-W4759A SECTION.                                                
041500                                                                          
041600     WRITE UT-POST FROM UT-AREA                                           
041700                                                                          
041800     MOVE 'INT'     TO POSTSUM-TRANSTYP                                   
041900     MOVE 'W4759A' TO POSTSUM-FDNAMN                                      
042000     MOVE 'W4759AD2' TO POSTSUM-DDNAMN2                                   
042100     CALL POSTSUM USING POSTSUM-PARM                                      
042200     .                                                                    
042300     EJECT                                                                
042400 S99-ABEND SECTION.                                                       
042500                                                                          
042600     SKIP2                                                                
042700     MOVE 'S' TO POSTSUM-OPKOD                                            
042800     CALL POSTSUM USING POSTSUM-PARM                                      
042900     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
043000     .                                                                    
043100     EJECT                                                                
043200* --- IMS SEKTIONER ---                                                   
043300     SKIP3                                                                
043400     EJECT                                                                
043500 IMS-GET-WDK601 SECTION.                                                  
043600                                                                          
043700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
043800          DELIMITED BY SIZE INTO SSA1                                     
043900     MOVE '  GE' TO GODK-STATUSKODER                                      
044000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
044100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
044200     PERFORM IMS-STATUSKONTROLL                                           
044300     .                                                                    
044400     EJECT                                                                
044500 IMS-GET-WDK611 SECTION.                                                  
044600                                                                          
044700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
044800          DELIMITED BY SIZE INTO SSA1                                     
044900     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
045000          DELIMITED BY SIZE INTO SSA2                                     
045100     MOVE '  GE' TO GODK-STATUSKODER                                      
045200     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
045300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
045400     PERFORM IMS-STATUSKONTROLL                                           
045500     .                                                                    
045600     EJECT                                                                
045700 IMS-STATUSKONTROLL SECTION.                                              
045800                                                                          
045900     SET STATUS-IX TO 1                                                   
046000     SEARCH GODK-STATUS                                                   
046100       AT END                                                             
046200         MOVE 'FEL PÅ LÄSNING' TO FELTEXT-STR                             
046300         DISPLAY FELTEXT                                                  
046400         CALL FELLOG                                                      
046500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
046600         CONTINUE                                                         
046700     END-SEARCH                                                           
046800     .                                                                    
