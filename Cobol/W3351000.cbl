000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3351000.                                                
000300 AUTHOR.         RONNY STENHOLM.                                          
000400 DATE-WRITTEN.   93/10/14.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PRISGRUPPSINFORMATION                                            
000900*        UPPDATERAR KUNDREGISTRET MED EN FIL FRÅN RESP                    
001000*        MARKNAD.                                                         
001100*        FILEN INNEHÅLLER RABATTSTRUKTURERNA FÖR                          
001200*        NORMALPRISSÄTTNINGEN PÅ PRISGRUPPSNIVÅ.                          
001300*                                                                         
001400*        PROGRAMMET UPPDATERAR WLPRIB (WDC2)                              
001500*                                                                         
001600*        SKICKAR MEMO TILL DET MB SOM LÄGGER UPP NYTT ELLER               
001700*        ÄNDRAT PRISOMR                                                   
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- INFILER                                                    
002800*          --- NYA RABATTSTRUKTURER                                       
002900     SELECT W33508                     ASSIGN TO W33510D1.                
003000*          --- UTFILER                                                    
003100*          --- MEMO TILL MF                                               
003200     SELECT W33511                     ASSIGN TO W33510D2.                
003300*          --- MEMO TILL MB A SVERIGE                                     
003400     SELECT W33512                     ASSIGN TO W33510D3.                
003500*          --- MEMO TILL MB B VCEM                                        
003600     SELECT W33513                     ASSIGN TO W33510D4.                
003700*          --- MEMO TILL MB C ASIA PACFIC                                 
003800     SELECT W33514                     ASSIGN TO W33510D5.                
003900*          --- MEMO TILL MB D SOUTH AMERICA                               
004000     SELECT W33515                     ASSIGN TO W33510D6.                
004100*          --- MEMO TILL MB E VCNA                                        
004200     SELECT W33516                     ASSIGN TO W33510D7.                
004300*          --- MEMO TILL MB F VCAS VCI                                    
004400     SELECT W33517                     ASSIGN TO W33510D8.                
004500*          --- MEMO TILL MB G VCI-EAST                                    
004600     SELECT W33518                     ASSIGN TO W33510D9.                
004700     EJECT                                                                
004800 DATA DIVISION.                                                           
004900     SKIP3                                                                
005000 FILE SECTION.                                                            
005100     SKIP3                                                                
005200 FD  W33508                                                               
005300     RECORDING       V                                                    
005400     BLOCK CONTAINS  0.                                                   
005500     SKIP2                                                                
005600*01  -COPY W335101B  -L.                                                  
005700     EJECT                                                                
005800************MEMOFIL MARKNADSFÖRING*****************************           
005900 FD  W33511                                                               
006000     RECORDING      V                                                     
006100     BLOCK CONTAINS 0.                                                    
006200 01  UT-MF                       PIC X(80).                               
006300     SKIP3                                                                
006400************MEMOFIL MARKNADSBOLAG A SVERIGE*********************          
006500 FD  W33512                                                               
006600     RECORDING      V                                                     
006700     BLOCK CONTAINS 0.                                                    
006800 01  UT-A                        PIC X(80).                               
006900     SKIP2                                                                
007000************MEMOFIL MARKNADSBOLAG B VCEM ***********************          
007100 FD  W33513                                                               
007200     RECORDING      V                                                     
007300     BLOCK CONTAINS 0.                                                    
007400 01  UT-B                        PIC X(80).                               
007500     SKIP2                                                                
007600************MEMOFIL MARKNADSBOLAG C ASIA PACFIC*****************          
007700 FD  W33514                                                               
007800     RECORDING      V                                                     
007900     BLOCK CONTAINS 0.                                                    
008000 01  UT-C                        PIC X(80).                               
008100     SKIP2                                                                
008200************MEMOFIL MARKNADSBOLAG D SOUTH AMERICA***************          
008300 FD  W33515                                                               
008400     RECORDING      V                                                     
008500     BLOCK CONTAINS 0.                                                    
008600 01  UT-D                        PIC X(80).                               
008700     SKIP2                                                                
008800************MEMOFIL MARKNADSBOLAG E VCNA  **********************          
008900 FD  W33516                                                               
009000     RECORDING      V                                                     
009100     BLOCK CONTAINS 0.                                                    
009200 01  UT-E                        PIC X(80).                               
009300     SKIP2                                                                
009400************MEMOFIL MARKNADSBOLAG F VCAS  **********************          
009500 FD  W33517                                                               
009600     RECORDING      V                                                     
009700     BLOCK CONTAINS 0.                                                    
009800 01  UT-F                        PIC X(80).                               
009900     SKIP2                                                                
010000************MEMOFIL MARKNADSBOLAG G VCI   **********************          
010100 FD  W33518                                                               
010200     RECORDING      V                                                     
010300     BLOCK CONTAINS 0.                                                    
010400 01  UT-G                        PIC X(80).                               
010500     EJECT                                                                
010600 WORKING-STORAGE SECTION.                                                 
010700                                                                          
010800*    -- CHECKED BY WY2000                                                 
010900 77  IDPGM                       PIC X(8)    VALUE 'W3351000'.            
011000 77  JA                          PIC X       VALUE 'J'.                   
011100 77  NEJ                         PIC X       VALUE 'N'.                   
011200 77  FOERSTA-UT-A                PIC X       VALUE 'J'.                   
011300 77  FOERSTA-UT-B                PIC X       VALUE 'J'.                   
011400 77  FOERSTA-UT-C                PIC X       VALUE 'J'.                   
011500 77  FOERSTA-UT-D                PIC X       VALUE 'J'.                   
011600 77  FOERSTA-UT-E                PIC X       VALUE 'J'.                   
011700 77  FOERSTA-UT-F                PIC X       VALUE 'J'.                   
011800 77  FOERSTA-UT-G                PIC X       VALUE 'J'.                   
011900 77  FOERSTA-UT-H                PIC X       VALUE 'J'.                   
012000 77  IX                          PIC 9(3)    COMP SYNC.                   
012100 77  SPAR-IDPROMR                PIC X(3)    VALUE SPACE.                 
012200     SKIP2                                                                
012300 01  FELTEXT.                                                             
012400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
012500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
012600                                                                          
012700 77  W33508-EOF-SW               PIC X       VALUE 'N'.                   
012800     88  END-OF-W33508                       VALUE 'J'.                   
012900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
013000                                                                          
013100 77  NY-TABELL-SW                PIC X       VALUE 'N'.                   
013200     88  NY-TABELL                           VALUE 'J'.                   
013300     EJECT                                                                
013400 01  CHKP-VAR.                                                            
013500 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
013600 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
013700 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
013800 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
013900 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
014000 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
014100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
014200                                                                          
014300                                                                          
014400 01  FILLER                      PIC X(24)  VALUE                         
014500                                            'MEMO-COPYTEXT      '.        
014600     SKIP2                                                                
014700*01  -COPY WMEMAREA                                                       
014800     EJECT                                                                
014900 01  UT-MEMO-AREA.                                                        
015000*                                                                         
015100     03  MEMO-MARKNADSFOERING.                                            
015200         05 RUBRIKRAD-0      PIC X(44)                                    
015300           VALUE 'MEMO FRÅN RUTIN W335D4 NORMALRABATTEN.      '.          
015400         05 RUBRIKRAD-1      PIC X(47)                                    
015500      VALUE 'FOLLOWING PRICE-AREAS HAS BEEN CREATED/UPDATED.'.            
015600         05 RUBRIKRAD-2      PIC X(51)                                    
015700      VALUE 'PLEASE, CHECK THAT THE PRICE-AREA HAVE AT LEAST ONE'.        
015800         05 RUBRIKRAD-3      PIC X(16)                                    
015900      VALUE 'PAYER CONNECTED.'.                                           
016000         05 RUBRIKRAD-4      PIC X(51)                                    
016100      VALUE 'IF NOT, THE PRICE-AREA WILL BE DELETED AT THE END  '.        
016200         05 RUBRIKRAD-5      PIC X(13)                                    
016300      VALUE 'OF THIS WEEK.'.                                              
016400         05 RUBRIKRAD-6      PIC X(50)                                    
016500      VALUE ' PRICE-AREA  SC START DATE UPDATE START DATUM'.              
016600*            --------XXX--XX-----XXXXXX-------------XXXXXX                
016700         05 UPPGIFTSRAD-1.                                                
016800           07 FILLER           PIC X(08)  VALUE  SPACE.                   
016900           07 MEMO-IDPROMR     PIC X(03)  VALUE  SPACE.                   
017000           07 FILLER           PIC X(02)  VALUE  SPACE.                   
017100           07 MEMO-IDLANDX2    PIC X(02)  VALUE  SPACE.                   
017200           07 FILLER           PIC X(05)  VALUE  SPACE.                   
017300           07 MEMO-DATUM       PIC 9(6)   VALUE  ZERO.                    
017400           07 FILLER           PIC X(13)  VALUE  SPACE.                   
017500           07 UPG-DATUM        PIC 9(6)   VALUE  ZERO.                    
017600           07 FILLER           PIC X(16)  VALUE  SPACE.                   
017700*                                                                         
017800*        05 UPPGIFTSRAD-2.                                                
017900*          07 FILLER           PIC X(10)  VALUE  SPACE.                   
018000*                                  VALUE 'UPDATE START DATUM'.            
018100*          07 FILLER           PIC X(2)   VALUE  SPACE.                   
018200*          07 UPG-DATUM        PIC 9(6)   VALUE  ZERO.                    
018300*          07 FILLER           PIC X(6)   VALUE  SPACE.                   
018400*          07 FILLER           PIC X(3)   VALUE  SPACE.                   
018500*          07 FILLER           PIC X(2)   VALUE  SPACE.                   
018600*          07 FILLER           PIC X(6)   VALUE  SPACE.                   
018700*          07 FILLER           PIC X(9)   VALUE  SPACE.                   
018800*          07 FILLER           PIC X(18)  VALUE  SPACE.                   
018900*                                                                         
019000     EJECT                                                                
019100 01  DYNAMISKA-SUBPROGRAM.                                                
019200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
019300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
019400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
019500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
019600     EJECT                                                                
019700*    --- PARAMETRAR TILL POSTSUM                                          
019800*                                                                         
019900*01  -COPY W0005   -PRE  POSTSUM-                                         
020000     EJECT                                                                
020100 01  AMC-AREA-START              PIC X(24)   VALUE                        
020200                                             'AMC-AREA-START'.            
020300     SKIP2                                                                
020400                                                                          
020500*01  AREA -COPY W335101B -PRE AMC-                                        
020600*                                                                         
020700     EJECT                                                                
020800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
020900     SKIP3                                                                
021000 01  NYCKLAR-TILL-DLI.                                                    
021100     03  W-IDPROMR-X.                                                     
021200         05  W-IDPROMR           PIC X(3)    VALUE SPACE.                 
021300     03  W-DASTADAT-X.                                                    
021400         05  W-DASTADAT          PIC 9(8)   VALUE ZERO.                   
021500                                                                          
021600     03  W-WDB1ASEQ-X.                                                    
021700         05  W-IDPROMR-B1A       PIC X(3)    VALUE SPACE.                 
021800                                                                          
022300     SKIP2                                                                
022400*    --- STATUS-KOD FRÅN IMS                                              
022500 01  STATUS-WS                   PIC XX.                                  
022600     88  SEGMENT-FINNS                       VALUE '  '.                  
022700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
022800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
022900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
023000     88  IMS-EJ-OK                           VALUE 'XD'.                  
023100     SKIP2                                                                
023200 01  GODK-STATUSKODER.                                                    
023300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023400     SKIP3                                                                
023500 01  SSA1                        PIC X(64).                               
023600 01  SSA2                        PIC X(64).                               
023700     EJECT                                                                
023800*    --- IMS FUNKTIONSKODER                                               
023900*01  -COPY W0003                                                          
024000     EJECT                                                                
024100*    ---  DLI INPUT-OUTPUT AREA                                           
024200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
024300                                                                          
024400 01  DLI-IO-PRIB01.                                                       
024500*    03  -COPY WDC201  -PRE PRIB-                                         
024600     EJECT                                                                
024700 01  DLI-IO-PRIB13.                                                       
024800*    03  -COPY WDC213  -PRE NORM-                                         
024900     EJECT                                                                
025000 01  DLI-IO-WDB101.                                                       
025100*    03  -COPY WDB101                                                     
025200     EJECT                                                                
025300 LINKAGE SECTION.                                                         
025400*01  -COPY W0009   -PRE MSG-                                              
025500     EJECT                                                                
025600*01  -COPY W0008  -PRE PRIB-                                              
025700     05  FILLER                  PIC X.                                   
025800*01  -COPY W0008  -PRE WDB1A-                                             
025900     05  FILLER                  PIC X.                                   
026000     EJECT                                                                
026100 PROCEDURE DIVISION  USING MSG-PCB  PRIB-PCB WDB1A-PCB.                   
026200 MAIN SECTION.                                                            
026300     ENTRY 'DLITCBL' USING MSG-PCB  PRIB-PCB WDB1A-PCB.                   
026400                                                                          
026500     PERFORM A-INIT                                                       
026600     PERFORM S01-LAES-W33508                                              
026700     PERFORM UNTIL END-OF-W33508                                          
026800       IF CHKP-ANT > CHKP-MAX                                             
026900         PERFORM X-TAG-CHECKPOINT                                         
027000       END-IF                                                             
027100       PERFORM B-UPPDATERA-PRIB-NORM                                      
027200       PERFORM C-BEHANDLA-MEMO                                            
027300       PERFORM S01-LAES-W33508                                            
027400       ADD +1 TO CHKP-ANT                                                 
027500     END-PERFORM                                                          
027600                                                                          
027700     PERFORM Z-FINIT                                                      
027800*     DISPLAY ' ******* SLUT     ********* '                              
027900*     DISPLAY ' ************************** '                              
028000                                                                          
028100     MOVE ZERO TO RETURN-CODE                                             
028200     GOBACK                                                               
028300     .                                                                    
028400     EJECT                                                                
028500 A-INIT SECTION.                                                          
028600                                                                          
028700     ACCEPT DAGENS-DATUM FROM DATE                                        
028800     OPEN INPUT W33508                                                    
028900     OPEN OUTPUT W33511                                                   
029000                 W33512                                                   
029100                 W33513                                                   
029200                 W33514                                                   
029300                 W33515                                                   
029400                 W33516                                                   
029500                 W33517                                                   
029600                 W33518                                                   
029700                                                                          
029800     PERFORM IMS-RESTART                                                  
029900                                                                          
030000     MOVE RUBRIKRAD-0  TO MEMO-TEXT                                       
030100     WRITE UT-MF FROM MEMO-TEXT                                           
030200     MOVE RUBRIKRAD-1  TO MEMO-TEXT                                       
030300     WRITE UT-MF FROM MEMO-TEXT                                           
030400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
030500*     DISPLAY ' ******* W3351000 ********* '                              
030600     .                                                                    
030700     EJECT                                                                
030800 B-UPPDATERA-PRIB-NORM SECTION.                                           
030900                                                                          
031000     IF AMC-IDMARKBO = 'A' OR 'B' OR 'C' OR 'E' OR 'F'                    
031100       MOVE AMC-IDPROMR TO W-IDPROMR-X                                    
031200       MOVE AMC-IDPROMR TO SPAR-IDPROMR                                   
031300       PERFORM IMS-GET-PRIB-PRO                                           
031400       IF SEGMENT-FINNS                                                   
031500         CONTINUE                                                         
031600       ELSE                                                               
031700         PERFORM BA-SKAPA-NY-PRISGRUPP                                    
031800       END-IF                                                             
031900       IF SEGMENT-FINNS                                                   
032000         MOVE AMC-TISTADAT TO NORM-RAB-DASTADAT                           
032100*---Y2K-FIX*******                                                        
032200         IF AMC-TISTADAT NOT = ZERO                                       
032300           IF AMC-TISTADAT < 500000                                       
032400             MOVE 20          TO NORM-RAB-DASTADAT(1:2)                   
032500           ELSE                                                           
032600             IF AMC-TISTADAT < 999999                                     
032700               MOVE 19        TO NORM-RAB-DASTADAT(1:2)                   
032800             ELSE                                                         
032900               MOVE 99999999  TO NORM-RAB-DASTADAT                        
033000             END-IF                                                       
033100           END-IF                                                         
033200         END-IF                                                           
033300         PERFORM BB-FYLL-NORM-RAB-AREA                                    
033400         PERFORM IMS-ISRT-PRIB-NORM                                       
033500         IF SEGMENT-FINNS-REDAN                                           
033600           MOVE AMC-TISTADAT TO W-DASTADAT                                
033700*---Y2K-FIX*******                                                        
033800         IF AMC-TISTADAT NOT = ZERO                                       
033900           IF AMC-TISTADAT < 500000                                       
034000             MOVE 20          TO W-DASTADAT(1:2)                          
034100           ELSE                                                           
034200             IF AMC-TISTADAT < 999999                                     
034300               MOVE 19        TO W-DASTADAT(1:2)                          
034400             ELSE                                                         
034500               MOVE 99999999  TO W-DASTADAT                               
034600             END-IF                                                       
034700           END-IF                                                         
034800         END-IF                                                           
034900           PERFORM IMS-GHU-NORM                                           
035000           PERFORM BB-FYLL-NORM-RAB-AREA                                  
035100           PERFORM IMS-REPL-PRIB-NORM                                     
035200         END-IF                                                           
035300       ELSE                                                               
035400         DISPLAY 'PRISOMR SAKNAS'                                         
035500         DISPLAY 'PRISOMR '  SPAR-IDPROMR                                 
035600         CALL ABEND USING                                                 
035700              RKOD-ABEND-UTAN-DUMP                                        
035800       END-IF                                                             
035900     ELSE                                                                 
036000       DISPLAY 'FEL MARKNADSBOLAGSKOD>' AMC-IDMARKBO '<'                  
036100       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
036200     END-IF                                                               
036300     .                                                                    
036400     EJECT                                                                
036500 BB-FYLL-NORM-RAB-AREA SECTION.                                           
036600     MOVE +1 TO IX                                                        
036700     PERFORM UNTIL IX = +100                                              
036800       MOVE AMC-KDRABATT(IX)      TO NORM-RAB-KDARTRAB(IX)                
036900       MOVE AMC-REARTRAB-DO(IX)   TO NORM-RAB-REARTRAB-DO(IX)             
037000       MOVE AMC-REARTRAB-BULK(IX) TO NORM-RAB-REARTRAB-BULK(IX)           
037100       IF IX = AMC-KDRABATT(IX)                                           
037200         ADD +1 TO IX                                                     
037300       ELSE                                                               
037400         DISPLAY 'FEL I FILEN DEN INNEHÅLLER EJ 99 KODER'                 
037500         DISPLAY 'ELLER SÅ ÄR ORDNINGEN FEL             '                 
037600         CALL ABEND USING                                                 
037700              RKOD-ABEND-UTAN-DUMP                                        
037800       END-IF                                                             
037900     END-PERFORM                                                          
038000     .                                                                    
038100     EJECT                                                                
038200 BA-SKAPA-NY-PRISGRUPP SECTION.                                           
038300                                                                          
038400     MOVE AMC-IDPROMR TO PRIB-PRO-IDPROMR                                 
038500     MOVE +1 TO PRIB-PRO-KDORDKL-DOG                                      
038600     PERFORM IMS-ISRT-PRIB-PRIB                                           
038700     .                                                                    
038800     EJECT                                                                
038900 C-BEHANDLA-MEMO SECTION.                                                 
039000                                                                          
039100     MOVE AMC-IDPROMR TO MEMO-IDPROMR                                     
039200                         W-IDPROMR-B1A                                    
039600     MOVE AMC-TISTADAT TO UPG-DATUM                                       
039700     MOVE AMC-TISTADAT TO MEMO-DATUM                                      
039800     PERFORM IMS-GU-WDB101                                                
039900     IF SEGMENT-FINNS                                                     
040200       MOVE BET-IDLANDX2 TO MEMO-IDLANDX2                                 
040300     ELSE                                                                 
040500       MOVE SPACE        TO MEMO-IDLANDX2                                 
040600     END-IF                                                               
040700                                                                          
040800     IF AMC-IDMARKBO = 'A'                                                
040900       IF FOERSTA-UT-A = JA                                               
041000                                                                          
041100         MOVE NEJ TO FOERSTA-UT-A                                         
041200         MOVE RUBRIKRAD-1  TO MEMO-TEXT                                   
041300         WRITE UT-A FROM MEMO-TEXT                                        
041400         MOVE RUBRIKRAD-2  TO MEMO-TEXT                                   
041500         WRITE UT-A FROM MEMO-TEXT                                        
041600         MOVE RUBRIKRAD-3  TO MEMO-TEXT                                   
041700         WRITE UT-A FROM MEMO-TEXT                                        
041800         MOVE RUBRIKRAD-4  TO MEMO-TEXT                                   
041900         WRITE UT-A FROM MEMO-TEXT                                        
042000         MOVE RUBRIKRAD-5  TO MEMO-TEXT                                   
042100         WRITE UT-A FROM MEMO-TEXT                                        
042200         MOVE RUBRIKRAD-6  TO MEMO-TEXT                                   
042300         WRITE UT-A FROM MEMO-TEXT                                        
042400       END-IF                                                             
042500       MOVE UPPGIFTSRAD-1  TO MEMO-TEXT                                   
042600       WRITE UT-A FROM MEMO-TEXT                                          
042700       WRITE UT-MF FROM MEMO-TEXT                                         
042800*      MOVE UPPGIFTSRAD-2  TO MEMO-TEXT                                   
042900*      WRITE UT-MF FROM MEMO-TEXT                                         
043000     END-IF                                                               
043100     IF AMC-IDMARKBO = 'B'                                                
043200       IF FOERSTA-UT-B = JA                                               
043300                                                                          
043400         MOVE NEJ TO FOERSTA-UT-B                                         
043500         MOVE RUBRIKRAD-1  TO MEMO-TEXT                                   
043600         WRITE UT-B FROM MEMO-TEXT                                        
043700         MOVE RUBRIKRAD-2  TO MEMO-TEXT                                   
043800         WRITE UT-B FROM MEMO-TEXT                                        
043900         MOVE RUBRIKRAD-3  TO MEMO-TEXT                                   
044000         WRITE UT-B FROM MEMO-TEXT                                        
044100         MOVE RUBRIKRAD-4  TO MEMO-TEXT                                   
044200         WRITE UT-B FROM MEMO-TEXT                                        
044300         MOVE RUBRIKRAD-5  TO MEMO-TEXT                                   
044400         WRITE UT-B FROM MEMO-TEXT                                        
044500         MOVE RUBRIKRAD-6  TO MEMO-TEXT                                   
044600         WRITE UT-B FROM MEMO-TEXT                                        
044700       END-IF                                                             
044800       MOVE UPPGIFTSRAD-1  TO MEMO-TEXT                                   
044900       WRITE UT-B FROM MEMO-TEXT                                          
045000       WRITE UT-MF FROM MEMO-TEXT                                         
045100*      MOVE UPPGIFTSRAD-2  TO MEMO-TEXT                                   
045200*      WRITE UT-MF FROM MEMO-TEXT                                         
045300     END-IF                                                               
045400     IF AMC-IDMARKBO = 'C'                                                
045500       IF FOERSTA-UT-C = JA                                               
045600         MOVE NEJ TO FOERSTA-UT-C                                         
045700                                                                          
045800         MOVE RUBRIKRAD-1  TO MEMO-TEXT                                   
045900         WRITE UT-C FROM MEMO-TEXT                                        
046000         MOVE RUBRIKRAD-2  TO MEMO-TEXT                                   
046100         WRITE UT-C FROM MEMO-TEXT                                        
046200         MOVE RUBRIKRAD-3  TO MEMO-TEXT                                   
046300         WRITE UT-C FROM MEMO-TEXT                                        
046400         MOVE RUBRIKRAD-4  TO MEMO-TEXT                                   
046500         WRITE UT-C FROM MEMO-TEXT                                        
046600         MOVE RUBRIKRAD-5  TO MEMO-TEXT                                   
046700         WRITE UT-C FROM MEMO-TEXT                                        
046800         MOVE RUBRIKRAD-6  TO MEMO-TEXT                                   
046900         WRITE UT-C FROM MEMO-TEXT                                        
047000       END-IF                                                             
047100       MOVE UPPGIFTSRAD-1  TO MEMO-TEXT                                   
047200       WRITE UT-C FROM MEMO-TEXT                                          
047300       WRITE UT-MF FROM MEMO-TEXT                                         
047400*      MOVE UPPGIFTSRAD-2  TO MEMO-TEXT                                   
047500*      WRITE UT-MF FROM MEMO-TEXT                                         
047600     END-IF                                                               
047700     IF AMC-IDMARKBO = 'E'                                                
047800       IF FOERSTA-UT-F = JA                                               
047900         MOVE NEJ TO FOERSTA-UT-E                                         
048000                                                                          
048100         MOVE RUBRIKRAD-1  TO MEMO-TEXT                                   
048200         WRITE UT-E FROM MEMO-TEXT                                        
048300         MOVE RUBRIKRAD-2  TO MEMO-TEXT                                   
048400         WRITE UT-E FROM MEMO-TEXT                                        
048500         MOVE RUBRIKRAD-3  TO MEMO-TEXT                                   
048600         WRITE UT-E FROM MEMO-TEXT                                        
048700         MOVE RUBRIKRAD-4  TO MEMO-TEXT                                   
048800         WRITE UT-E FROM MEMO-TEXT                                        
048900         MOVE RUBRIKRAD-5  TO MEMO-TEXT                                   
049000         WRITE UT-E FROM MEMO-TEXT                                        
049100         MOVE RUBRIKRAD-6  TO MEMO-TEXT                                   
049200         WRITE UT-E FROM MEMO-TEXT                                        
049300       END-IF                                                             
049400       MOVE UPPGIFTSRAD-1  TO MEMO-TEXT                                   
049500       WRITE UT-E FROM MEMO-TEXT                                          
049600       WRITE UT-MF FROM MEMO-TEXT                                         
049700*      MOVE UPPGIFTSRAD-2  TO MEMO-TEXT                                   
049800*      WRITE UT-MF FROM MEMO-TEXT                                         
049900     END-IF                                                               
050000     IF AMC-IDMARKBO = 'F'                                                
050100       IF FOERSTA-UT-F = JA                                               
050200         MOVE NEJ TO FOERSTA-UT-F                                         
050300                                                                          
050400         MOVE RUBRIKRAD-1  TO MEMO-TEXT                                   
050500         WRITE UT-F FROM MEMO-TEXT                                        
050600         MOVE RUBRIKRAD-2  TO MEMO-TEXT                                   
050700         WRITE UT-F FROM MEMO-TEXT                                        
050800         MOVE RUBRIKRAD-3  TO MEMO-TEXT                                   
050900         WRITE UT-F FROM MEMO-TEXT                                        
051000         MOVE RUBRIKRAD-4  TO MEMO-TEXT                                   
051100         WRITE UT-F FROM MEMO-TEXT                                        
051200         MOVE RUBRIKRAD-5  TO MEMO-TEXT                                   
051300         WRITE UT-F FROM MEMO-TEXT                                        
051400         MOVE RUBRIKRAD-6  TO MEMO-TEXT                                   
051500         WRITE UT-F FROM MEMO-TEXT                                        
051600       END-IF                                                             
051700       MOVE UPPGIFTSRAD-1  TO MEMO-TEXT                                   
051800       WRITE UT-F FROM MEMO-TEXT                                          
051900       WRITE UT-MF FROM MEMO-TEXT                                         
052000*      MOVE UPPGIFTSRAD-2  TO MEMO-TEXT                                   
052100*      WRITE UT-MF FROM MEMO-TEXT                                         
052200     END-IF                                                               
052300*   HÄMTA FLER IDLANDX2                                                   
052400*    DISPLAY 'HÄMTA FLER IDLANDX2 '                                       
052500     PERFORM IMS-GN-WDB101                                                
052600     PERFORM UNTIL SEGMENT-SAKNAS                                         
052700       IF MEMO-IDLANDX2 NOT = BET-IDLANDX2                                
052800         MOVE BET-IDLANDX2      TO MEMO-IDLANDX2                          
052900         MOVE UPPGIFTSRAD-1     TO MEMO-TEXT                              
053000         IF AMC-IDMARKBO = 'A'                                            
053100           WRITE UT-A         FROM MEMO-TEXT                              
053200           WRITE UT-MF        FROM MEMO-TEXT                              
053300         END-IF                                                           
053400         IF AMC-IDMARKBO = 'B'                                            
053500           WRITE UT-B         FROM MEMO-TEXT                              
053600           WRITE UT-MF        FROM MEMO-TEXT                              
053700         END-IF                                                           
053800         IF AMC-IDMARKBO = 'C'                                            
053900           WRITE UT-C         FROM MEMO-TEXT                              
054000           WRITE UT-MF        FROM MEMO-TEXT                              
054100         END-IF                                                           
054200         IF AMC-IDMARKBO = 'E'                                            
054300           WRITE UT-E         FROM MEMO-TEXT                              
054400           WRITE UT-MF        FROM MEMO-TEXT                              
054500         END-IF                                                           
054600         IF AMC-IDMARKBO = 'F'                                            
054700           WRITE UT-F         FROM MEMO-TEXT                              
054800           WRITE UT-MF        FROM MEMO-TEXT                              
054900         END-IF                                                           
055000       END-IF                                                             
055100       PERFORM IMS-GN-WDB101                                              
055200     END-PERFORM                                                          
055300     .                                                                    
055400     EJECT                                                                
055500 X-TAG-CHECKPOINT   SECTION.                                              
055600                                                                          
055700     PERFORM IMS-CHECKPOINT                                               
055800     MOVE ZERO TO CHKP-ANT                                                
055900     .                                                                    
056000     EJECT                                                                
056100 Z-FINIT SECTION.                                                         
056200                                                                          
056300     CLOSE W33508                                                         
056400           W33511                                                         
056500           W33512                                                         
056600           W33513                                                         
056700           W33514                                                         
056800           W33515                                                         
056900           W33516                                                         
057000           W33517                                                         
057100           W33518                                                         
057200                                                                          
057300     MOVE 'S' TO POSTSUM-OPKOD                                            
057400     CALL POSTSUM USING POSTSUM-PARM                                      
057500     .                                                                    
057600     EJECT                                                                
057700 S01-LAES-W33508  SECTION.                                                
057800                                                                          
057900     READ W33508 INTO AMC-AREA                                            
058000     AT END                                                               
058100     MOVE JA TO W33508-EOF-SW                                             
058200                                                                          
058300     NOT AT END                                                           
058400        MOVE 'W33508' TO POSTSUM-FDNAMN                                   
058500        MOVE 'W33510D1' TO POSTSUM-DDNAMN2                                
058600        MOVE 'AMC'      TO POSTSUM-TRANSTYP                               
058700        CALL POSTSUM USING POSTSUM-PARM                                   
058800     END-READ                                                             
058900     .                                                                    
059000     EJECT                                                                
059100* --- IMS SEKTIONER ---                                                   
059200                                                                          
059300 IMS-GET-PRIB-PRO SECTION.                                                
059400     STRING 'WLPRIB01(IDPROMR  =' W-IDPROMR-X ')'                         
059500          DELIMITED BY SIZE INTO SSA1                                     
059600     MOVE '  GE' TO GODK-STATUSKODER                                      
059700     CALL CBLTDLI USING GHU PRIB-PCB DLI-IO-PRIB01 SSA1                   
059800     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
059900     PERFORM IMS-STATUSKONTROLL                                           
060000     .                                                                    
060100     SKIP3                                                                
060200 IMS-GHU-NORM SECTION.                                                    
060300     STRING 'WLPRIB01(IDPROMR  =' W-IDPROMR-X ')'                         
060400          DELIMITED BY SIZE INTO SSA1                                     
060500     STRING 'WLPRIB13(DASTADAT =' W-DASTADAT-X ')'                        
060600          DELIMITED BY SIZE INTO SSA2                                     
060700     MOVE '  GE' TO GODK-STATUSKODER                                      
060800     CALL CBLTDLI USING GHU PRIB-PCB DLI-IO-PRIB13 SSA1 SSA2              
060900     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
061000     PERFORM IMS-STATUSKONTROLL                                           
061100     .                                                                    
061200     SKIP3                                                                
061300 IMS-ISRT-PRIB-NORM SECTION.                                              
061400                                                                          
061500     STRING 'WLPRIB01(IDPROMR  =' W-IDPROMR-X ')'                         
061600          DELIMITED BY SIZE INTO SSA1                                     
061700     MOVE 'WLPRIB13 ' TO SSA2                                             
061800     MOVE '  II' TO GODK-STATUSKODER                                      
061900     CALL CBLTDLI USING ISRT PRIB-PCB DLI-IO-PRIB13 SSA1 SSA2             
062000     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
062100     PERFORM IMS-STATUSKONTROLL                                           
062200     .                                                                    
062300     EJECT                                                                
062400 IMS-REPL-PRIB-NORM SECTION.                                              
062500                                                                          
062600     MOVE '  ' TO GODK-STATUSKODER                                        
062700     CALL CBLTDLI USING REPL PRIB-PCB DLI-IO-PRIB13                       
062800     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
062900     PERFORM IMS-STATUSKONTROLL                                           
063000     .                                                                    
063100     EJECT                                                                
063200 IMS-ISRT-PRIB-PRIB SECTION.                                              
063300                                                                          
063400     MOVE 'WLPRIB01 ' TO SSA1                                             
063500     MOVE '  ' TO GODK-STATUSKODER                                        
063600     CALL CBLTDLI USING ISRT PRIB-PCB DLI-IO-PRIB01 SSA1                  
063700     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
063800     PERFORM IMS-STATUSKONTROLL                                           
063900     .                                                                    
064000     EJECT                                                                
064100 IMS-GU-WDB101 SECTION.                                                   
064200*    DISPLAY 'IMS-GU-WDB101 SECTION.'                                     
064300     STRING 'WDB101  (WDB1ASEQ =' W-WDB1ASEQ-X ')'                        
064400                                                                          
064500          DELIMITED BY SIZE INTO SSA1                                     
064600     MOVE '  GE' TO GODK-STATUSKODER                                      
064700     CALL CBLTDLI USING GU WDB1A-PCB DLI-IO-WDB101 SSA1                   
064800     MOVE WDB1A-STATUS-CODE TO STATUS-WS                                  
064900     PERFORM IMS-STATUSKONTROLL                                           
065000     .                                                                    
065100     EJECT                                                                
065200 IMS-GN-WDB101 SECTION.                                                   
065300*    DISPLAY 'IMS-GN-WDB101 SECTION.'                                     
065400     STRING 'WDB101  (WDB1ASEQ =' W-WDB1ASEQ-X ')'                        
065500                                                                          
065600          DELIMITED BY SIZE INTO SSA1                                     
065700     MOVE '  GE' TO GODK-STATUSKODER                                      
065800     CALL CBLTDLI USING GN WDB1A-PCB DLI-IO-WDB101 SSA1                   
065900     MOVE WDB1A-STATUS-CODE TO STATUS-WS                                  
066000     PERFORM IMS-STATUSKONTROLL                                           
066100     .                                                                    
066200     EJECT                                                                
066300 IMS-RESTART SECTION.                                                     
066400                                                                          
066500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
066600     MOVE '  ' TO GODK-STATUSKODER                                        
066700     CALL CBLTDLI USING XRST MSG-PCB                                      
066800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
066900                        CHKP-AREA-LENGTH CHKP-AREA                        
067000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
067100     PERFORM IMS-STATUSKONTROLL                                           
067200     .                                                                    
067300     EJECT                                                                
067400 IMS-CHECKPOINT SECTION.                                                  
067500                                                                          
067600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
067700     MOVE '  XD' TO GODK-STATUSKODER                                      
067800     CALL CBLTDLI USING CHKP MSG-PCB                                      
067900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
068000                        CHKP-AREA-LENGTH CHKP-AREA                        
068100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
068200     PERFORM IMS-STATUSKONTROLL                                           
068300                                                                          
068400     IF IMS-EJ-OK                                                         
068500       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
068600       DISPLAY FELTEXT                                                    
068700       CALL FELLOG                                                        
068800     END-IF                                                               
068900     .                                                                    
069000     EJECT                                                                
069100 IMS-STATUSKONTROLL SECTION.                                              
069200                                                                          
069300     SET STATUS-IX TO 1                                                   
069400     SEARCH GODK-STATUS                                                   
069500       AT END                                                             
069600         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
069700         DISPLAY FELTEXT                                                  
069800         CALL FELLOG                                                      
069900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
070000         CONTINUE                                                         
070100     END-SEARCH                                                           
070200     .                                                                    
