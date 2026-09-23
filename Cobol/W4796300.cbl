000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4796300.                                                
000400 AUTHOR.         GÖRAN KJELLSON   GUIDE                                   
000500 DATE-WRITTEN.   NOVEMBER  2005                                           
000600 DATE-COMPILED.                                                           
000710                                                                          
000833*                                                                         
000840*    FUNKTION:                                                            
000900*        SKAPAR EXTRAKTFIL FRÅN WXTR.FAKTRAD I EXCEL-FORMAT               
001000*        URVALSREGLER:                                                    
001100*        KDSTAURV PÅ WDB6                                                 
001200*        BLANK = INGET EXTRAKT                                            
001300*            A = EXTRAKT FÖR SAMTLIGA DISTRIKT OCH KUNDER                 
001400*            C = EXTRAKT OM FLLDCKND (WDB2) = JA                          
001500*                                                                         
001600*        INFILEN ÄR SORTERAD PÅ DC, DISTRIKT, KUND, ORDERKLASS            
001610*                               ORDERNUMMER OCH KOLLIID                   
001700*                                                                         
002200*    ABENDKODER:                                                          
002300*        U0016 -  . . . .                                                 
002400*        U1000 -  . . . .                                                 
002500*                                                                         
002600*                                                                         
002700                                                                          
002800                                                                          
002900 ENVIRONMENT DIVISION.                                                    
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003400     SKIP2                                                                
003500*          --- VECKANS FAKTURERADE RADER (WXTR-FAKTRAD)                   
003600     SELECT W47965A                    ASSIGN TO W47963D1.                
003700                                                                          
003800*          --- EXTRAKT FÖR VÖR VALDA DC/KUNDER (W479??)                   
003900     SELECT EXCEL-EXTRAKT              ASSIGN TO W47963D2.                
004000                                                                          
004001*          --- EXTRAKT FÖR VÖR VALDA DC/KUNDER RADPOSTER                  
004002     SELECT W47968B                    ASSIGN TO W47963D3.                
004003                                                                          
008340     EJECT                                                                
008350 DATA DIVISION.                                                           
008500 FILE SECTION.                                                            
008600                                                                          
008700 FD  W47965A                                                              
008800     RECORDING       F                                                    
008900     BLOCK CONTAINS  0.                                                   
009000                                                                          
009100*01  -COPY W47965A     -L.                                                
009200                                                                          
009210                                                                          
009300 FD  EXCEL-EXTRAKT                                                        
009400     RECORDING       F                                                    
009500     BLOCK CONTAINS  0.                                                   
009600                                                                          
009700 01  EXCEL-POST PIC X(82).                                                
009800                                                                          
009900                                                                          
009901 FD  W47968B                                                              
009902     RECORDING       F                                                    
009903     BLOCK CONTAINS  0.                                                   
009904                                                                          
009905*01  POST  -COPY W47968B    -PRE 68B-  -L.                                
009906                                                                          
009907                                                                          
020252 WORKING-STORAGE SECTION.                                                 
020260                                                                          
020500 77  IDPGM                       PIC X(8)    VALUE 'W4796300'.            
020510 77  CURRENT-SECTION             PIC X(16)   VALUE 'MAIN'.                
020520 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
020530 77  FILLER                      PIC X(8)    VALUE 'ERRORTEX'.            
020531 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
020532                                                                          
020533                                                                          
020600 77  JA                          PIC X       VALUE 'J'.                   
020700 77  NEJ                         PIC X       VALUE 'N'.                   
029600                                                                          
029601 77  WS-KDMFUP                   PIC X(2)    VALUE SPACE.                 
029602 77  WS-ADCITY                   PIC X(20)   VALUE SPACE.                 
029603                                                                          
029610 77  EOF-W47965A-SW              PIC X       VALUE 'N'.                   
029620     88  EOF-W47965A                         VALUE 'J'.                   
029621                                                                          
029622 77  POSTER-SW                   PIC X       VALUE 'N'.                   
029623     88  POSTER-SKRIVNA                     VALUE 'J'.                    
029625                                                                          
029626 77  EXTRAKT-SW                  PIC X       VALUE 'N'.                   
029627     88  SKRIV-EXTRAKT                       VALUE 'J'.                   
029630                                                                          
029640 01  BRYTBEGREPP.                                                         
029650     03  BRYT-IDDC-LEV           PIC X(2)    VALUE SPACE.                 
029651     03  BRYT-IDDISTR            PIC 9(5)    VALUE ZERO.                  
029652     03  BRYT-IDKUNDNR           PIC 9(7)    VALUE ZERO.                  
029653     03  BRYT-KDORDKL            PIC 9(1)    VALUE ZERO.                  
029654     03  BRYT-IDORDNR5           PIC 9(5)    VALUE ZERO.                  
029655     03  BRYT-IDKOLLI            PIC 9(5)    VALUE ZERO.                  
029656                                                                          
029657 01  ARBETSVARIABLER.                                                     
029658     03  WS-KVLEVART2            PIC 9(7)    VALUE ZERO.                  
029659     03  WS-KVRADER              PIC 9(7)    VALUE ZERO.                  
029660     03  WS-KVKOLLI              PIC 9(7)    VALUE ZERO.                  
029661     03  WS-KVORDER              PIC 9(7)    VALUE ZERO.                  
029662     03  WS-VKKOLLI              PIC 9(7)V9  VALUE ZERO.                  
029663     03  WS-PRARTNTO             PIC 9(9)V99 VALUE ZERO.                  
029664                                                                          
029665     03  WS-KVLEVART2-CUSTSUM    PIC 9(8)    VALUE ZERO.                  
029666     03  WS-KVRADER-CUSTSUM      PIC 9(7)    VALUE ZERO.                  
029667     03  WS-KVKOLLI-CUSTSUM      PIC 9(6)    VALUE ZERO.                  
029668     03  WS-KVORDER-CUSTSUM      PIC 9(6)    VALUE ZERO.                  
029669     03  WS-VKKOLLI-CUSTSUM      PIC 9(7)V9  VALUE ZERO.                  
029670                                                                          
029671     03  WS-KVLEVART2-DISTSUM    PIC 9(8)    VALUE ZERO.                  
029672     03  WS-KVRADER-DISTSUM      PIC 9(7)    VALUE ZERO.                  
029673     03  WS-KVKOLLI-DISTSUM      PIC 9(6)    VALUE ZERO.                  
029674     03  WS-KVORDER-DISTSUM      PIC 9(6)    VALUE ZERO.                  
029675     03  WS-VKKOLLI-DISTSUM      PIC 9(7)V9  VALUE ZERO.                  
029680                                                                          
029681 01  HELPVARIABLER.                                                       
029682     03  HELP-VKKOLLI            PIC 9(7)    VALUE ZERO.                  
029683     03  HELP-PRARTNTO           PIC 9(9)    VALUE ZERO.                  
029684                                                                          
029700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
029800 01  FILLER REDEFINES DAGENS-DATUM.                                       
029900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
030000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
030100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
030110 01  DAGENS-DATUM-VECKA          PIC 9(2)    VALUE ZERO.                  
030200                                                                          
033740                                                                          
033800 01  DYNAMISKA-SUBPROGRAM.                                                
033900*                                                                         
034000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
034100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
034200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
034300     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
034400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
034500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
034600     03  WL10WBDC                PIC X(8)    VALUE 'WL10WBDC'.            
034800     SKIP2                                                                
034900*    --- PARAMETRAR TILL ABEND                                            
035000                                                                          
035100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
035200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
035300                                                                          
035400 01  FELTEXT.                                                             
035500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT '.            
035600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
035700     SKIP2                                                                
038200*    --- PARAMETRAR TILL DATKORT                                          
038300*                                                                         
038400 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W47963'.              
038600 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
038700                                                                          
038800*01  -COPY WDATKORT                                                       
038900                                                                          
038901*    --- PARAMETRAR TILL WDATKONV                                         
038902 01  FILLER                      PIC X(16) VALUE 'WDATAREA'.              
038903*01  -COPY WDATAREA                                                       
038904                                                                          
038905                                                                          
038906*    --- PARAMETRAR TILL WL10WBDC                                         
038907 01  FILLER                      PIC X(16) VALUE 'WL10WBDC'.              
038908*01  -COPY WL10WBDC                                                       
038909                                                                          
038910                                                                          
039000*    --- PARAMETRAR TILL POSTSUM                                          
039100*                                                                         
039200*01  -COPY W0005   -PRE  POSTSUM-                                         
039300                                                                          
039310                                                                          
039400 01  FILLER          PIC X(24)   VALUE 'W47965A-AREA'.                    
039700                                                                          
039710 01  W47965A-AREA.                                                        
039800*    03  AREA -COPY W47965A    -PRE IN-                                   
039900                                                                          
039910                                                                          
039911 01  FILLER          PIC X(24)   VALUE 'W47968B-AREA'.                    
039912                                                                          
039913 01  W47968B-AREA.                                                        
039914*    03  AREA -COPY W47968B    -PRE 68B-                                  
039915                                                                          
039916                                                                          
040000 01  FILLER          PIC X(24)   VALUE 'EXCEL-H1-AREA'.                   
040400 01  EXCEL-HEAD-1-AREA.                                                   
040401     03 EXCEL-H1-IDDC            PIC X(2).                                
040410     03 EXCEL-H1-LISTNR          PIC X(13)  VALUE 'W47963-001'.           
040411     03 FILLER                   PIC X(3)   VALUE 'DC'.                   
040412     03 EXCEL-H1-IDDC-LEV        PIC X(2).                                
040413     03 FILLER                   PIC X(16)  VALUE                         
040414                                            '  INVOICED WEEK '.           
040415     03 EXCEL-H1-AAR             PIC 9(2).                                
040416     03 EXCEL-H1-VV              PIC 9(2).                                
040429                                                                          
040430                                                                          
040431 01  FILLER          PIC X(24)   VALUE 'EXCEL-H2-AREA'.                   
040432 01  EXCEL-HEAD-2-AREA.                                                   
040433     03 EXCEL-H2-IDDC            PIC X(2).                                
040434     03 FILLER                   PIC X(8)   VALUE 'DISTRICT'.             
040435     03 FILLER                   PIC X(1)   VALUE X'05'.                  
040436     03 FILLER                   PIC X(8)   VALUE '  DEALER'.             
040437     03 FILLER                   PIC X(1)   VALUE X'05'.                  
040438     03 FILLER                   PIC X(9)   VALUE '    CLASS'.            
040439     03 FILLER                   PIC X(1)   VALUE X'05'.                  
040440     03 FILLER                   PIC X(8)   VALUE '  PIECES'.             
040441     03 FILLER                   PIC X(1)   VALUE X'05'.                  
040442     03 FILLER                   PIC X(10)  VALUE '     LINES'.           
040443     03 FILLER                   PIC X(1)   VALUE X'05'.                  
040444     03 FILLER                   PIC X(9)   VALUE '     CASE'.            
040445     03 FILLER                   PIC X(1)   VALUE X'05'.                  
040446     03 FILLER                   PIC X(8)   VALUE '   ORDER'.             
040447     03 FILLER                   PIC X(1)   VALUE X'05'.                  
040448     03 FILLER                   PIC X(11)  VALUE 'CASE WEIGHT'.          
040449     03 FILLER                   PIC X(1)   VALUE X'05'.                  
040450                                                                          
040451                                                                          
040452 01  FILLER          PIC X(24)   VALUE 'EXCEL-LINE-AREA'.                 
040460 01  EXCEL-LINE-AREA.                                                     
040461     03 EXCEL-L1-IDDC            PIC X(2).                                
040462     03 EXCEL-IDDISTR            PIC Z(4)9.                               
040463     03 FILLER                   PIC X(1)   VALUE X'05'.                  
040464     03 EXCEL-IDKUNDNR           PIC Z(5)9.                               
040465     03 FILLER                   PIC X(1)   VALUE X'05'.                  
040466     03 EXCEL-KDORDKL            PIC Z(2)9.                               
040467     03 FILLER                   PIC X(1)   VALUE X'05'.                  
040468     03 EXCEL-KVLEVART2          PIC Z(7)9.                               
040469     03 FILLER                   PIC X(1)   VALUE X'05'.                  
040470     03 EXCEL-KVRADER            PIC Z(6)9.                               
040471     03 FILLER                   PIC X(1)   VALUE X'05'.                  
040472     03 EXCEL-KVKOLLI            PIC Z(5)9.                               
040473     03 FILLER                   PIC X(1)   VALUE X'05'.                  
040474     03 EXCEL-KVORDER            PIC Z(5)9.                               
040475     03 FILLER                   PIC X(1)   VALUE X'05'.                  
040476     03 EXCEL-VKKOLLI            PIC Z(7)9V9.                             
040477     03 FILLER                   PIC X(1)   VALUE X'05'.                  
040478     03 FILLER                   PIC X(1)   VALUE X'05'.                  
040479                                                                          
040480                                                                          
040481 01  FILLER          PIC X(24)   VALUE 'EXCEL-CUST-AREA'.                 
040482 01  EXCEL-CUST-AREA.                                                     
040483     03 EXCEL-CUST-IDDC          PIC X(2).                                
040484     03 EXCEL-CUST-IDDISTR       PIC Z(4)9.                               
040485     03 FILLER                   PIC X(1)   VALUE X'05'.                  
040486     03 EXCEL-CUST-IDKUNDNR      PIC Z(5)9.                               
040487     03 FILLER                   PIC X(5)   VALUE X'0540404005'.          
040488     03 EXCEL-CUST-KVLEVART2     PIC Z(7)9.                               
040489     03 FILLER                   PIC X(1)   VALUE X'05'.                  
040490     03 EXCEL-CUST-KVRADER       PIC Z(6)9.                               
040491     03 FILLER                   PIC X(1)   VALUE X'05'.                  
040492     03 EXCEL-CUST-KVKOLLI       PIC Z(5)9.                               
040493     03 FILLER                   PIC X(1)   VALUE X'05'.                  
040494     03 EXCEL-CUST-KVORDER       PIC Z(5)9.                               
040495     03 FILLER                   PIC X(1)   VALUE X'05'.                  
040496     03 EXCEL-CUST-VKKOLLI       PIC Z(7)9V9.                             
040497     03 FILLER                   PIC X(1)   VALUE X'05'.                  
040498                                                                          
040499                                                                          
040500 01  FILLER          PIC X(24)   VALUE 'EXCEL-DIST-AREA'.                 
040501 01  EXCEL-DIST-AREA.                                                     
040502     03 EXCEL-DIST-IDDC          PIC X(2).                                
040503     03 EXCEL-DIST-IDDISTR       PIC Z(4)9.                               
040504     03 FILLER                   PIC X(1)   VALUE X'05'.                  
040506     03 FILLER                   PIC X(5)   VALUE X'40054005'.            
040507     03 EXCEL-DIST-KVLEVART2     PIC Z(7)9.                               
040508     03 FILLER                   PIC X(1)   VALUE X'05'.                  
040509     03 EXCEL-DIST-KVRADER       PIC Z(6)9.                               
040510     03 FILLER                   PIC X(1)   VALUE X'05'.                  
040511     03 EXCEL-DIST-KVKOLLI       PIC Z(5)9.                               
040512     03 FILLER                   PIC X(1)   VALUE X'05'.                  
040513     03 EXCEL-DIST-KVORDER       PIC Z(5)9.                               
040514     03 FILLER                   PIC X(1)   VALUE X'05'.                  
040515     03 EXCEL-DIST-VKKOLLI       PIC Z(7)9V9.                             
040516     03 FILLER                   PIC X(1)   VALUE X'05'.                  
040517                                                                          
040518                                                                          
040520                                                                          
040600 01  FILLER          PIC X(24)   VALUE 'EXCEL-TOMM-RAD '.                 
040700                                                                          
040800 01  EXCEL-TOMRAD-AREA.                                                   
040810     03 EXCEL-T1-IDDC            PIC X(2).                                
040820     03 FILLER                   PIC X(7)   VALUE X'05'.                  
040830     03 FILLER                   PIC X(7)   VALUE X'05'.                  
041000     03 FILLER                   PIC X(8)   VALUE X'05'.                  
041100     03 FILLER                   PIC X(8)   VALUE X'05'.                  
041200     03 FILLER                   PIC X(8)   VALUE X'05'.                  
041300     03 FILLER                   PIC X(8)   VALUE X'05'.                  
041400     03 FILLER                   PIC X(8)   VALUE X'05'.                  
041600     03 FILLER                   PIC X(3)   VALUE X'05'.                  
041610     03 FILLER                   PIC X(3)   VALUE X'05'.                  
041700     03 FILLER                   PIC X(1)   VALUE X'05'.                  
051376                                                                          
051378*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
051379*                                                                         
051380 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
051400                                                                          
051500 01  NYCKLAR-TILL-DLI.                                                    
051600     03  W-IDGMT-X.                                                       
051700         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
051800         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
051900                                                                          
058820     03  W-IDDC-B6-X.                                                     
058830         05 W-IDDC-B6            PIC X(2).                                
058840                                                                          
058896                                                                          
059700*    --- STATUS-KOD FRÅN IMS                                              
059800 01  STATUS-WS                   PIC XX.                                  
059900     88  SEGMENT-FINNS                       VALUE '  '.                  
060100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
060300                                                                          
060400 01  GODK-STATUSKODER.                                                    
060500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
060600                                                                          
060700 01  SSA1                        PIC X(128).                              
060900                                                                          
060910                                                                          
061000*    --- IMS FUNKTIONSKODER                                               
061100*01  -COPY W0003                                                          
061200                                                                          
061300*    ---  DLI INPUT-OUTPUT AREA                                           
061400 01  FILLER               PIC X(16)   VALUE 'WDB201 AREA'.                
069300 01  DLI-IO-WDB201.                                                       
069700*    03  -COPY WDB201                                                     
070987                                                                          
070988 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
070989 01  DLI-IO-WDB601.                                                       
070990*    03  -COPY WDB601                                                     
070991                                                                          
070992                                                                          
071800 LINKAGE SECTION.                                                         
071900                                                                          
075592*01  -COPY W0008   -PRE WDB2-                                             
075593     05  FILLER                  PIC X.                                   
075594                                                                          
075595*01  -COPY W0008   -PRE WDB6-                                             
075596     05  FILLER                  PIC X.                                   
075597                                                                          
075600 PROCEDURE DIVISION  USING WDB2-PCB WDB6-PCB.                             
075700 MAIN SECTION.                                                            
076200     ENTRY 'DLITCBL' USING WDB2-PCB WDB6-PCB.                             
076900                                                                          
076920                                                                          
077000     PERFORM A-INIT                                                       
077100     PERFORM S01-LAES-W47965A                                             
077200     PERFORM UNTIL EOF-W47965A                                            
077300                                                                          
077310        PERFORM B-KOLLA-OM-EXTRAKT                                        
077311                                                                          
077320        IF SKRIV-EXTRAKT                                                  
077400           PERFORM C-REDIGERA-SKRIV-EXTRAKT                               
077401        END-IF                                                            
077410                                                                          
077500        PERFORM S01-LAES-W47965A                                          
077510     END-PERFORM                                                          
079400                                                                          
079500     PERFORM Z-FINIT                                                      
079600                                                                          
079700     MOVE ZERO TO RETURN-CODE                                             
079800     GOBACK                                                               
079900     .                                                                    
080000                                                                          
080100                                                                          
080200 A-INIT SECTION.                                                          
080210     MOVE 'A-INIT          ' TO CURRENT-SECTION.                          
080300                                                                          
080400     OPEN INPUT  W47965A                                                  
080500                                                                          
080600     OPEN OUTPUT EXCEL-EXTRAKT                                            
080700                 W47968B                                                  
082300                                                                          
083000     CALL DATKORT    USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT            
083100     MOVE D-AAR         TO DAGENS-DATUM-AAR                               
083200     MOVE D-MAANAD      TO DAGENS-DATUM-MAANAD                            
083300     MOVE D-DAG         TO DAGENS-DATUM-DAG                               
083400     MOVE D-VECKA       TO DAGENS-DATUM-VECKA                             
084700     MOVE IDPGM         TO POSTSUM-PROGNAMN                               
084710                                                                          
084720     MOVE DAGENS-DATUM  TO DAT-I-TIDATUM                                  
084730     MOVE 'AAMMDD'      TO DAT-KDDATFORM                                  
084740     CALL WDATKONV USING   DAT-KDDATFORM,                                 
084750                           DAT-I-TIDATUM                                  
084760                           DAT-O-TIDATUM,                                 
084770                           DAT-KDSVAR                                     
084780                                                                          
084790     IF NOT DAT-KDSVAR-OK                                                 
084800        MOVE 'FEL FRÅN WDATKONV I A-INIT'                                 
084900                        TO ERROR-TEXT                                     
085000        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
085100     END-IF                                                               
085101                                                                          
085102     MOVE SPACE        TO DCS-IDDC                                        
085103     MOVE ZERO         TO GMT-IDDISTR                                     
085104                          GMT-IDKUNDNR                                    
085105     .                                                                    
085106                                                                          
085107 B-KOLLA-OM-EXTRAKT SECTION.                                              
085108     MOVE 'B-KOLLA-OM-EXTRA' TO CURRENT-SECTION                           
085109                                                                          
085110     MOVE NEJ TO EXTRAKT-SW                                               
085111                                                                          
085112     IF IN-IDDC-LEV NOT = DCS-IDDC                                        
085113        MOVE IN-IDDC-LEV  TO W-IDDC-B6                                    
085114                             WBDC-IDDC                                    
085115        PERFORM IMS-01-GU-WDB601                                          
085116        CALL WL10WBDC USING WBDC-AREA                                     
085117        IF WBDC-KDMFUP = SPACE                                            
085118*         -- DETTA KAN HÄNDA OM EN LDC-KUND SERVAS                        
085119*         -- FRÅN ETT LAGER SOM INTE ÄR "WEB-DC"                          
085122*         -- DETTA SKER BARA OM URSPRUNGLIGT LAGER VAR ETT                
085123*         -- LDC I EUROPA. FIXA SÅ ATT KDMFUP = "MA" (= EUROPA)           
085124*         -- ÄVEN "FOLLOW-UP" LISTOR KOMMER DÅ ATT SKAPAS FÖR             
085125*         -- DESSA LAGER ÄVEN OM DE INTE KAN HITTAS PÅ NORMAL             
085126*         -- VÄG! (KJELL FIX. 2013-01-04)                                 
085127          MOVE 'MA' TO WBDC-KDMFUP                                        
085128        END-IF                                                            
085129     END-IF                                                               
085130                                                                          
085131     IF DCS-KDFAKTDC NOT = SPACE AND                                      
085132        IN-FLDIRLEV = NEJ                                                 
085133                                                                          
085134        IF DCS-KDFAKTDC = 'A'                                             
085135           MOVE JA TO EXTRAKT-SW                                          
085136        ELSE                                                              
085137           IF IN-IDDISTR  NOT = GMT-IDDISTR OR                            
085138              IN-IDKUNDNR NOT = GMT-IDKUNDNR                              
085139                                                                          
085140              MOVE IN-IDDISTR  TO W-IDDISTR                               
085141              MOVE IN-IDKUNDNR TO W-IDKUNDNR                              
085142              PERFORM IMS-02-GU-WDB201                                    
085143           END-IF                                                         
085144           IF GMT-FLLDCKND = JA                                           
085145              MOVE JA TO EXTRAKT-SW                                       
085146           END-IF                                                         
085147        END-IF                                                            
085150     END-IF                                                               
085160                                                                          
085200     .                                                                    
085300                                                                          
085400 C-REDIGERA-SKRIV-EXTRAKT SECTION.                                        
085500     MOVE 'C-REDIGERA-SKRIV' TO CURRENT-SECTION                           
085501*    DISPLAY ' '                                                          
085510*    DISPLAY 'DC    ' IN-IDDC-LEV                                         
085520*    DISPLAY 'DIST  ' IN-IDDISTR                                          
085521*    DISPLAY 'KUND  ' IN-IDKUNDNR                                         
085522*    DISPLAY 'KLASS ' IN-KDORDKL                                          
085530*    DISPLAY 'ORDER ' IN-IDORDNR5                                         
085540*    DISPLAY 'KOLL  ' IN-IDKOLLI                                          
085550*    DISPLAY 'ANTAL ' IN-KVLEVART2                                        
085560*    DISPLAY 'VIKT  ' IN-VKORDBTO-KOLLI                                   
085600                                                                          
085610     IF IN-IDDC-LEV NOT = BRYT-IDDC-LEV                                   
085612* NYTT DC                                                                 
085613        PERFORM CA-NYTT-DC                                                
085615     END-IF                                                               
085628                                                                          
085629     IF IN-IDDISTR  = BRYT-IDDISTR                                        
085630        IF IN-IDKUNDNR = BRYT-IDKUNDNR                                    
085631           IF IN-KDORDKL = BRYT-KDORDKL                                   
085640              IF IN-IDORDNR5 = BRYT-IDORDNR5                              
085650                 IF IN-IDKOLLI = BRYT-IDKOLLI                             
085651                    CONTINUE                                              
085652                 ELSE                                                     
085653* NYTT KOLLI                                                              
085654                    ADD 1           TO WS-KVKOLLI                         
085655                    ADD 1           TO WS-KVKOLLI-CUSTSUM                 
085656                    ADD 1           TO WS-KVKOLLI-DISTSUM                 
085660                    MOVE IN-IDKOLLI TO BRYT-IDKOLLI                       
085661                 END-IF                                                   
085662              ELSE                                                        
085663* NY ORDER                                                                
085664                 ADD 1            TO WS-KVKOLLI                           
085665                 ADD 1            TO WS-KVKOLLI-CUSTSUM                   
085666                 ADD 1            TO WS-KVKOLLI-DISTSUM                   
085668                 ADD 1            TO WS-KVORDER                           
085669                 ADD 1            TO WS-KVORDER-CUSTSUM                   
085670                 ADD 1            TO WS-KVORDER-DISTSUM                   
085671                 MOVE IN-IDORDNR5 TO BRYT-IDORDNR5                        
085672                 MOVE IN-IDKOLLI  TO BRYT-IDKOLLI                         
085673              END-IF                                                      
085674           ELSE                                                           
085675* NY ORDERKLASS                                                           
085676              ADD 1            TO WS-KVKOLLI-CUSTSUM                      
085677              ADD 1            TO WS-KVKOLLI-DISTSUM                      
085678              ADD 1            TO WS-KVORDER-CUSTSUM                      
085679              ADD 1            TO WS-KVORDER-DISTSUM                      
085680              PERFORM CB-NY-ORDERKLASS                                    
085681              MOVE IN-KDORDKL  TO BRYT-KDORDKL                            
085682              MOVE IN-IDORDNR5 TO BRYT-IDORDNR5                           
085683              MOVE IN-IDKOLLI  TO BRYT-IDKOLLI                            
085684           END-IF                                                         
085685        ELSE                                                              
085686* NY KUND                                                                 
085687           ADD 1            TO WS-KVKOLLI-DISTSUM                         
085688           ADD 1            TO WS-KVORDER-DISTSUM                         
085689           PERFORM CC-NY-KUND                                             
085690           MOVE IN-IDKUNDNR TO BRYT-IDKUNDNR                              
085691           MOVE IN-KDORDKL  TO BRYT-KDORDKL                               
085692           MOVE IN-IDORDNR5 TO BRYT-IDORDNR5                              
085693           MOVE IN-IDKOLLI  TO BRYT-IDKOLLI                               
085694        END-IF                                                            
085695     ELSE                                                                 
085696* NYTT DISTRIKT                                                           
085697        PERFORM CD-NYTT-DISTRIKT                                          
085698        MOVE IN-IDDISTR  TO BRYT-IDDISTR                                  
085699        MOVE IN-IDKUNDNR TO BRYT-IDKUNDNR                                 
085700        MOVE IN-KDORDKL  TO BRYT-KDORDKL                                  
085701        MOVE IN-IDORDNR5 TO BRYT-IDORDNR5                                 
085702        MOVE IN-IDKOLLI  TO BRYT-IDKOLLI                                  
085710     END-IF                                                               
085800                                                                          
085810     ADD IN-KVLEVART2      TO WS-KVLEVART2                                
085820                              WS-KVLEVART2-CUSTSUM                        
085821                              WS-KVLEVART2-DISTSUM                        
085830     ADD 1                 TO WS-KVRADER                                  
085840                              WS-KVRADER-CUSTSUM                          
085841                              WS-KVRADER-DISTSUM                          
085850     ADD IN-VKORDBTO-KOLLI TO WS-VKKOLLI                                  
085860                              WS-VKKOLLI-CUSTSUM                          
085870                              WS-VKKOLLI-DISTSUM                          
085880     IF DCS-IDLANDX2 NOT = 'CN'                                           
085890        COMPUTE WS-PRARTNTO ROUNDED = WS-PRARTNTO +                       
085900               (IN-KVLEVART2 * IN-PRARTSTD)                               
085901     END-IF                                                               
085902     .                                                                    
086000                                                                          
086100 CA-NYTT-DC  SECTION.                                                     
086200     MOVE 'CA-NYTT-DC      ' TO CURRENT-SECTION                           
086300                                                                          
086301     IF POSTER-SKRIVNA                                                    
086302        PERFORM S11-SKRIV-SISTA-RAD-O-SUMMOR                              
086303     ELSE                                                                 
086310        MOVE JA              TO POSTER-SW                                 
086311     END-IF                                                               
086320                                                                          
086400     MOVE IN-IDDC-LEV        TO BRYT-IDDC-LEV                             
086500     MOVE IN-IDDISTR         TO BRYT-IDDISTR                              
086510     MOVE IN-IDKUNDNR        TO BRYT-IDKUNDNR                             
086600     MOVE IN-KDORDKL         TO BRYT-KDORDKL                              
086700     MOVE IN-IDORDNR5        TO BRYT-IDORDNR5                             
086800     MOVE IN-IDKOLLI         TO BRYT-IDKOLLI                              
086900                                                                          
087000     MOVE WBDC-KDMFUP        TO WS-KDMFUP                                 
087100     MOVE DCS-ADCITY IN DCS-ADPOST-PNRORT                                 
087200                             TO WS-ADCITY                                 
087300*    DISPLAY '*******DC ' BRYT-IDDC-LEV                                   
087401*    DISPLAY '***ADCITY ' WS-ADCITY                                       
087402                                                                          
087403     MOVE ZERO               TO WS-KVLEVART2                              
087404                                WS-KVRADER                                
087405                                WS-VKKOLLI                                
087406                                WS-PRARTNTO                               
087407                                                                          
087408                                WS-KVLEVART2-CUSTSUM                      
087409                                WS-KVRADER-CUSTSUM                        
087410                                WS-VKKOLLI-CUSTSUM                        
087411                                                                          
087412                                WS-KVLEVART2-DISTSUM                      
087413                                WS-KVRADER-DISTSUM                        
087414                                WS-VKKOLLI-DISTSUM                        
087415                                                                          
087416     MOVE 1                  TO WS-KVKOLLI                                
087417                                WS-KVORDER                                
087418                                WS-KVKOLLI-CUSTSUM                        
087419                                WS-KVORDER-CUSTSUM                        
087420                                WS-KVKOLLI-DISTSUM                        
087421                                WS-KVORDER-DISTSUM                        
087422                                                                          
087423     MOVE IN-IDDC-LEV        TO EXCEL-H1-IDDC                             
087424                                EXCEL-H2-IDDC                             
087425                                EXCEL-L1-IDDC                             
087426                                EXCEL-CUST-IDDC                           
087427                                EXCEL-DIST-IDDC                           
087428                                EXCEL-T1-IDDC                             
087429                                68B-IDDC-LEV                              
087430                                                                          
087431     MOVE BRYT-IDDC-LEV      TO EXCEL-H1-IDDC-LEV                         
087432     MOVE DAGENS-DATUM-AAR   TO EXCEL-H1-AAR                              
087433                                68B-TIAAVV(1:2)                           
087434     MOVE DAGENS-DATUM-VECKA TO EXCEL-H1-VV                               
087435                                68B-TIAAVV(3:2)                           
087436                                                                          
087437     PERFORM S02-SKRIV-H1-EXCEL                                           
087500     PERFORM S03-SKRIV-TOMRAD-EXCEL                                       
087600     PERFORM S04-SKRIV-H2-EXCEL                                           
093400     .                                                                    
093500                                                                          
093600 CB-NY-ORDERKLASS SECTION.                                                
093700     MOVE 'CB-NY-ORDERKLASS' TO CURRENT-SECTION                           
093800                                                                          
093810     MOVE BRYT-IDDISTR       TO EXCEL-IDDISTR                             
093811                                  68B-IDDISTR                             
093820     MOVE BRYT-IDKUNDNR      TO EXCEL-IDKUNDNR                            
093821                                  68B-IDKUNDNR                            
093830     MOVE BRYT-KDORDKL       TO EXCEL-KDORDKL                             
093840                                  68B-KDORDKL                             
093850     MOVE WS-KVLEVART2       TO EXCEL-KVLEVART2                           
093860                                  68B-KVLEVART2                           
093870     MOVE WS-KVRADER         TO EXCEL-KVRADER                             
093880                                  68B-KVRADER                             
093890     MOVE WS-KVKOLLI         TO EXCEL-KVKOLLI                             
093891                                  68B-KVKOLLI                             
093892     MOVE WS-KVORDER         TO EXCEL-KVORDER                             
093893                                  68B-KVORDER                             
093894     MOVE WS-VKKOLLI         TO EXCEL-VKKOLLI                             
093895     COMPUTE HELP-VKKOLLI ROUNDED =                                       
093896               WS-VKKOLLI * 1                                             
093897     MOVE HELP-VKKOLLI       TO   68B-VKORDBTO                            
093898                                                                          
093899     MOVE WS-KDMFUP          TO 68B-KDMFUP                                
093900     MOVE WS-ADCITY          TO 68B-ADCITY                                
094000     MOVE DAT-TIAARP         TO 68B-TIAARP                                
094100     COMPUTE HELP-PRARTNTO ROUNDED =                                      
094200               WS-PRARTNTO * 1                                            
094201     MOVE HELP-PRARTNTO      TO 68B-PRARTNTO                              
094202                                                                          
095600     PERFORM S05-SKRIV-LINE-EXCEL                                         
095700     PERFORM S05B-SKRIV-W47968B                                           
095800                                                                          
095810     MOVE ZERO               TO WS-KVLEVART2                              
095820                                WS-KVRADER                                
095821                                WS-VKKOLLI                                
095822                                WS-PRARTNTO                               
095830     MOVE 1                  TO WS-KVKOLLI                                
095840                                WS-KVORDER                                
095900     .                                                                    
096000                                                                          
096100 CC-NY-KUND  SECTION.                                                     
096200     MOVE 'CC-NY-KUND      ' TO CURRENT-SECTION                           
096300                                                                          
096301     MOVE BRYT-IDDISTR       TO EXCEL-IDDISTR                             
096302                                  68B-IDDISTR                             
096303     MOVE BRYT-IDKUNDNR      TO EXCEL-IDKUNDNR                            
096304                                  68B-IDKUNDNR                            
096305     MOVE BRYT-KDORDKL       TO EXCEL-KDORDKL                             
096306                                  68B-KDORDKL                             
096307     MOVE WS-KVLEVART2       TO EXCEL-KVLEVART2                           
096308                                  68B-KVLEVART2                           
096309     MOVE WS-KVRADER         TO EXCEL-KVRADER                             
096310                                  68B-KVRADER                             
096311     MOVE WS-KVKOLLI         TO EXCEL-KVKOLLI                             
096312                                  68B-KVKOLLI                             
096313     MOVE WS-KVORDER         TO EXCEL-KVORDER                             
096314                                  68B-KVORDER                             
096315     MOVE WS-VKKOLLI         TO EXCEL-VKKOLLI                             
096316     COMPUTE HELP-VKKOLLI ROUNDED =                                       
096317               WS-VKKOLLI * 1                                             
096318     MOVE HELP-VKKOLLI       TO   68B-VKORDBTO                            
096319                                                                          
096320     MOVE WS-KDMFUP          TO 68B-KDMFUP                                
096330     MOVE WS-ADCITY          TO 68B-ADCITY                                
096340     MOVE DAT-TIAARP         TO 68B-TIAARP                                
096350     COMPUTE HELP-PRARTNTO ROUNDED =                                      
096360               WS-PRARTNTO * 1                                            
096361     MOVE HELP-PRARTNTO      TO 68B-PRARTNTO                              
096362                                                                          
096363     PERFORM S05-SKRIV-LINE-EXCEL                                         
096364     PERFORM S05B-SKRIV-W47968B                                           
098601                                                                          
098602     MOVE BRYT-IDDISTR         TO EXCEL-CUST-IDDISTR                      
098603     MOVE BRYT-IDKUNDNR        TO EXCEL-CUST-IDKUNDNR                     
098604     MOVE WS-KVLEVART2-CUSTSUM TO EXCEL-CUST-KVLEVART2                    
098605     MOVE WS-KVRADER-CUSTSUM   TO EXCEL-CUST-KVRADER                      
098606     MOVE WS-KVKOLLI-CUSTSUM   TO EXCEL-CUST-KVKOLLI                      
098607     MOVE WS-KVORDER-CUSTSUM   TO EXCEL-CUST-KVORDER                      
098608     MOVE WS-VKKOLLI-CUSTSUM   TO EXCEL-CUST-VKKOLLI                      
098609                                                                          
098610     PERFORM S06-SKRIV-CUSTSUM-EXCEL                                      
098611     PERFORM S03-SKRIV-TOMRAD-EXCEL                                       
098612                                                                          
098620     MOVE ZERO               TO WS-KVLEVART2                              
098630                                WS-KVRADER                                
098660                                WS-VKKOLLI                                
098661                                WS-PRARTNTO                               
098670                                                                          
098680                                WS-KVLEVART2-CUSTSUM                      
098690                                WS-KVRADER-CUSTSUM                        
098693                                WS-VKKOLLI-CUSTSUM                        
098699                                                                          
098700     MOVE 1                  TO WS-KVKOLLI                                
098701                                WS-KVORDER                                
098702                                WS-KVKOLLI-CUSTSUM                        
098703                                WS-KVORDER-CUSTSUM                        
098710     .                                                                    
098800                                                                          
098900 CD-NYTT-DISTRIKT  SECTION.                                               
099000     MOVE 'CD-NYTT-DISTRIKT' TO CURRENT-SECTION                           
099100                                                                          
099101     MOVE BRYT-IDDISTR       TO EXCEL-IDDISTR                             
099102                                  68B-IDDISTR                             
099103     MOVE BRYT-IDKUNDNR      TO EXCEL-IDKUNDNR                            
099104                                  68B-IDKUNDNR                            
099105     MOVE BRYT-KDORDKL       TO EXCEL-KDORDKL                             
099106                                  68B-KDORDKL                             
099107     MOVE WS-KVLEVART2       TO EXCEL-KVLEVART2                           
099108                                  68B-KVLEVART2                           
099109     MOVE WS-KVRADER         TO EXCEL-KVRADER                             
099110                                  68B-KVRADER                             
099111     MOVE WS-KVKOLLI         TO EXCEL-KVKOLLI                             
099112                                  68B-KVKOLLI                             
099113     MOVE WS-KVORDER         TO EXCEL-KVORDER                             
099114                                  68B-KVORDER                             
099115     MOVE WS-VKKOLLI         TO EXCEL-VKKOLLI                             
099116     COMPUTE HELP-VKKOLLI ROUNDED =                                       
099117               WS-VKKOLLI * 1                                             
099118     MOVE HELP-VKKOLLI       TO   68B-VKORDBTO                            
099119                                                                          
099120     MOVE WS-KDMFUP          TO 68B-KDMFUP                                
099130     MOVE WS-ADCITY          TO 68B-ADCITY                                
099140     MOVE DAT-TIAARP         TO 68B-TIAARP                                
099150     COMPUTE HELP-PRARTNTO ROUNDED =                                      
099160               WS-PRARTNTO * 1                                            
099161     MOVE HELP-PRARTNTO      TO 68B-PRARTNTO                              
099162                                                                          
099163     PERFORM S05-SKRIV-LINE-EXCEL                                         
099164     PERFORM S05B-SKRIV-W47968B                                           
100100                                                                          
100110     MOVE BRYT-IDDISTR         TO EXCEL-CUST-IDDISTR                      
100200     MOVE BRYT-IDKUNDNR        TO EXCEL-CUST-IDKUNDNR                     
100300     MOVE WS-KVLEVART2-CUSTSUM TO EXCEL-CUST-KVLEVART2                    
100400     MOVE WS-KVRADER-CUSTSUM   TO EXCEL-CUST-KVRADER                      
100500     MOVE WS-KVKOLLI-CUSTSUM   TO EXCEL-CUST-KVKOLLI                      
100600     MOVE WS-KVORDER-CUSTSUM   TO EXCEL-CUST-KVORDER                      
100700     MOVE WS-VKKOLLI-CUSTSUM   TO EXCEL-CUST-VKKOLLI                      
100800                                                                          
100900     PERFORM S06-SKRIV-CUSTSUM-EXCEL                                      
100910                                                                          
100920     MOVE BRYT-IDDISTR         TO EXCEL-DIST-IDDISTR                      
100930     MOVE WS-KVLEVART2-DISTSUM TO EXCEL-DIST-KVLEVART2                    
100940     MOVE WS-KVRADER-DISTSUM   TO EXCEL-DIST-KVRADER                      
100950     MOVE WS-KVKOLLI-DISTSUM   TO EXCEL-DIST-KVKOLLI                      
100960     MOVE WS-KVORDER-DISTSUM   TO EXCEL-DIST-KVORDER                      
100970     MOVE WS-VKKOLLI-DISTSUM   TO EXCEL-DIST-VKKOLLI                      
100980                                                                          
100990     PERFORM S07-SKRIV-DISTSUM-EXCEL                                      
101000     PERFORM S03-SKRIV-TOMRAD-EXCEL                                       
101100                                                                          
101200     MOVE ZERO               TO WS-KVLEVART2                              
101300                                WS-KVRADER                                
101600                                WS-VKKOLLI                                
101610                                WS-PRARTNTO                               
101700                                                                          
101800                                WS-KVLEVART2-CUSTSUM                      
101900                                WS-KVRADER-CUSTSUM                        
102200                                WS-VKKOLLI-CUSTSUM                        
102210                                                                          
102220                                WS-KVLEVART2-DISTSUM                      
102230                                WS-KVRADER-DISTSUM                        
102260                                WS-VKKOLLI-DISTSUM                        
102830                                                                          
102900     MOVE 1                  TO WS-KVKOLLI                                
103000                                WS-KVORDER                                
103100                                WS-KVKOLLI-CUSTSUM                        
103200                                WS-KVORDER-CUSTSUM                        
103210                                WS-KVKOLLI-DISTSUM                        
103220                                WS-KVORDER-DISTSUM                        
103300     .                                                                    
103400                                                                          
243600 Z-FINIT SECTION.                                                         
243610     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
243800                                                                          
243810     IF POSTER-SKRIVNA                                                    
243811        PERFORM S11-SKRIV-SISTA-RAD-O-SUMMOR                              
243840     END-IF                                                               
243850                                                                          
243900     CLOSE W47965A                                                        
244000           EXCEL-EXTRAKT                                                  
244100           W47968B                                                        
245700                                                                          
245800     MOVE 'S' TO POSTSUM-OPKOD                                            
245900     CALL POSTSUM USING POSTSUM-PARM                                      
246000     .                                                                    
246100                                                                          
246200 S01-LAES-W47965A SECTION.                                                
246210     MOVE 'S01-LAES-W47965A' TO CURRENT-SECTION                           
246400                                                                          
246500     READ W47965A   INTO W47965A-AREA                                     
246600                                                                          
246610     AT END                                                               
246620        MOVE JA TO EOF-W47965A-SW                                         
246630                                                                          
246700     NOT AT END                                                           
246800        MOVE 'W47965A'                    TO POSTSUM-FDNAMN               
246900        MOVE 'W47963D1'                   TO POSTSUM-DDNAMN2              
247000        MOVE 'FAKT'                       TO POSTSUM-TRANSTYP             
247100        CALL POSTSUM USING POSTSUM-PARM                                   
247200     END-READ                                                             
247300     .                                                                    
247400                                                                          
248520 S02-SKRIV-H1-EXCEL  SECTION.                                             
248530     MOVE 'S02-SKRIV-H1-EXC' TO CURRENT-SECTION                           
248700                                                                          
248800     WRITE EXCEL-POST FROM EXCEL-HEAD-1-AREA                              
248900                                                                          
249000     MOVE 'HEAD'                          TO POSTSUM-TRANSTYP             
249100     MOVE 'EXCEL'                         TO POSTSUM-FDNAMN               
249200     MOVE 'W47963D2'                      TO POSTSUM-DDNAMN2              
249300     CALL POSTSUM USING POSTSUM-PARM                                      
249400     .                                                                    
249410                                                                          
249411 S03-SKRIV-TOMRAD-EXCEL  SECTION.                                         
249412     MOVE 'S03-SKRIV-TOMRAD' TO CURRENT-SECTION                           
249413                                                                          
249414     WRITE EXCEL-POST FROM EXCEL-TOMRAD-AREA                              
249420     .                                                                    
249421                                                                          
249422 S04-SKRIV-H2-EXCEL  SECTION.                                             
249423     MOVE 'S04-SKRIV-H2-EXC' TO CURRENT-SECTION                           
249424                                                                          
249425     WRITE EXCEL-POST FROM EXCEL-HEAD-2-AREA                              
249431     .                                                                    
249432                                                                          
249439 S05-SKRIV-LINE-EXCEL  SECTION.                                           
249440     MOVE 'S05-SKRIV-LINE-E' TO CURRENT-SECTION                           
249441                                                                          
249450     WRITE EXCEL-POST FROM EXCEL-LINE-AREA                                
249460                                                                          
249470     MOVE 'LINE'                          TO POSTSUM-TRANSTYP             
249480     MOVE 'EXCEL'                         TO POSTSUM-FDNAMN               
249490     MOVE 'W47963D2'                      TO POSTSUM-DDNAMN2              
249491     CALL POSTSUM USING POSTSUM-PARM                                      
249492     .                                                                    
249500                                                                          
249501 S05B-SKRIV-W47968B     SECTION.                                          
249502     MOVE 'S05B-SK-W47968B ' TO CURRENT-SECTION                           
249503                                                                          
249504*    IF 68B-IDDISTR = 778                                                 
249505     WRITE 68B-POST    FROM W47968B-AREA                                  
249506                                                                          
249507     MOVE 'LINE'                          TO POSTSUM-TRANSTYP             
249508     MOVE 'W47968B'                       TO POSTSUM-FDNAMN               
249509     MOVE 'W47963D3'                      TO POSTSUM-DDNAMN2              
249510     CALL POSTSUM USING POSTSUM-PARM                                      
249511*    END-IF                                                               
249512     .                                                                    
249513                                                                          
249600 S06-SKRIV-CUSTSUM-EXCEL  SECTION.                                        
249700     MOVE 'S06-SKRIV-CUSTSU' TO CURRENT-SECTION                           
249800                                                                          
249900     WRITE EXCEL-POST FROM EXCEL-CUST-AREA                                
250000     .                                                                    
250100                                                                          
250200 S07-SKRIV-DISTSUM-EXCEL  SECTION.                                        
250300     MOVE 'S07-SKRIV-DISTSU' TO CURRENT-SECTION                           
250400                                                                          
250500     WRITE EXCEL-POST FROM EXCEL-DIST-AREA                                
250600     .                                                                    
250700                                                                          
250803 S11-SKRIV-SISTA-RAD-O-SUMMOR SECTION.                                    
250804     MOVE 'S11-SKRIV-SISTA-' TO CURRENT-SECTION                           
250805                                                                          
250806     MOVE BRYT-IDDISTR     TO EXCEL-IDDISTR                               
250807                                68B-IDDISTR                               
250808     MOVE BRYT-IDKUNDNR    TO EXCEL-IDKUNDNR                              
250809                                68B-IDKUNDNR                              
250810     MOVE BRYT-KDORDKL     TO EXCEL-KDORDKL                               
250811                                68B-KDORDKL                               
250812     MOVE WS-KVLEVART2     TO EXCEL-KVLEVART2                             
250813                                68B-KVLEVART2                             
250814     MOVE WS-KVRADER       TO EXCEL-KVRADER                               
250815                                68B-KVRADER                               
250816     MOVE WS-KVKOLLI       TO EXCEL-KVKOLLI                               
250817                                68B-KVKOLLI                               
250818     MOVE WS-KVORDER       TO EXCEL-KVORDER                               
250819                                68B-KVORDER                               
250820     MOVE WS-VKKOLLI       TO EXCEL-VKKOLLI                               
250830     COMPUTE HELP-VKKOLLI ROUNDED =                                       
250831               WS-VKKOLLI * 1                                             
250832     MOVE HELP-VKKOLLI     TO   68B-VKORDBTO                              
250833                                                                          
250834     MOVE WS-KDMFUP        TO 68B-KDMFUP                                  
250835     MOVE WS-ADCITY        TO 68B-ADCITY                                  
250836     MOVE DAT-TIAARP       TO 68B-TIAARP                                  
250837     COMPUTE HELP-PRARTNTO ROUNDED =                                      
250838               WS-PRARTNTO * 1                                            
250839     MOVE HELP-PRARTNTO    TO 68B-PRARTNTO                                
250840                                                                          
250841     PERFORM S05-SKRIV-LINE-EXCEL                                         
250842     PERFORM S05B-SKRIV-W47968B                                           
250843                                                                          
250844     MOVE BRYT-IDDISTR         TO EXCEL-CUST-IDDISTR                      
250845     MOVE BRYT-IDKUNDNR        TO EXCEL-CUST-IDKUNDNR                     
250846     MOVE WS-KVLEVART2-CUSTSUM TO EXCEL-CUST-KVLEVART2                    
250847     MOVE WS-KVRADER-CUSTSUM   TO EXCEL-CUST-KVRADER                      
250848     MOVE WS-KVKOLLI-CUSTSUM   TO EXCEL-CUST-KVKOLLI                      
250849     MOVE WS-KVORDER-CUSTSUM   TO EXCEL-CUST-KVORDER                      
250850     MOVE WS-VKKOLLI-CUSTSUM   TO EXCEL-CUST-VKKOLLI                      
250851                                                                          
250852     PERFORM S06-SKRIV-CUSTSUM-EXCEL                                      
250853                                                                          
250854     MOVE BRYT-IDDISTR         TO EXCEL-DIST-IDDISTR                      
250855     MOVE WS-KVLEVART2-DISTSUM TO EXCEL-DIST-KVLEVART2                    
250856     MOVE WS-KVRADER-DISTSUM   TO EXCEL-DIST-KVRADER                      
250857     MOVE WS-KVKOLLI-DISTSUM   TO EXCEL-DIST-KVKOLLI                      
250858     MOVE WS-KVORDER-DISTSUM   TO EXCEL-DIST-KVORDER                      
250859     MOVE WS-VKKOLLI-DISTSUM   TO EXCEL-DIST-VKKOLLI                      
250860                                                                          
250861     PERFORM S07-SKRIV-DISTSUM-EXCEL                                      
250862     .                                                                    
250863                                                                          
250900                                                                          
385500* --- IMS SEKTIONER ---                                                   
385600                                                                          
400800                                                                          
403065 IMS-01-GU-WDB601    SECTION.                                             
403066     MOVE 'IMS-01' TO CURRENT-IMS-SECTION                                 
403067                                                                          
403068     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
403070          DELIMITED BY SIZE INTO SSA1                                     
403071                                                                          
403072     MOVE '  GE'              TO GODK-STATUSKODER                         
403073     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
403074     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
403075     PERFORM IMS-STATUSKONTROLL                                           
403076     IF SEGMENT-SAKNAS                                                    
403077         MOVE SPACE TO DCS-KDDC                                           
403078                       DCS-KDFAKTDC                                       
403079     END-IF                                                               
403080     .                                                                    
403081                                                                          
403082 IMS-02-GU-WDB201      SECTION.                                           
403083     MOVE 'IMS-02' TO CURRENT-IMS-SECTION                                 
403084                                                                          
403085     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
403086          DELIMITED BY SIZE INTO SSA1                                     
403087                                                                          
403088     MOVE '  GE'              TO GODK-STATUSKODER                         
403089     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
403090     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
403091     PERFORM IMS-STATUSKONTROLL                                           
403092     IF SEGMENT-SAKNAS                                                    
403093         MOVE SPACE TO GMT-FLLDCKND                                       
403094     END-IF                                                               
403095     .                                                                    
403096 IMS-STATUSKONTROLL SECTION.                                              
403100                                                                          
403200     SET STATUS-IX TO 1                                                   
403300     SEARCH GODK-STATUS                                                   
403400       AT END                                                             
403500         MOVE 'FEL STATUSKOD FRÅN IMS ' TO FELTEXT-STR                    
403600         DISPLAY FELTEXT                                                  
403700         CALL FELLOG                                                      
403800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
403900         CONTINUE                                                         
404000     END-SEARCH                                                           
404100     .                                                                    
