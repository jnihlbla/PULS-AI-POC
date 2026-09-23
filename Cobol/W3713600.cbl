000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W3713600.                                                
000400*AUTHOR.         RONNY STENHOLM.                                          
000500*DATE-WRITTEN.   92/08/27.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER PÅ WDM6 ALLA RAPPORTER MED STATUS 9                        
001100*        SKRIVER HISTORIKFILER FÖR USA OCH ÖVRIGA VÄRLDEN                 
001200*        FÖR RAPPORTER SOM BLIVIT GODKÄNDA UNDER VECKAN                   
001300*        SKAPAR RENSNINGSFILER FÖR WDM6                                   
001400*        FÖR RAPPORTER SOM LEGAT PÅ REGISTRET I MINST 12 VECKOR           
001500*        RAPPORTERNA HAR LEGAT DÄR MINST 12 VECKOR                        
001600*                                                                         
001700*        PROGRAMMET LÄSER      WDM6                                       
001800*        PROGRAMMET LÄSER      WDK6                                       
001900*        PROGRAMMET LÄSER      WDD3                                       
002000*        PROGRAMMET LÄSER      WDR2                                       
002100*                                                                         
002200*    ABENDKODER:                                                          
002300*        U0016 -  . . . .                                                 
002400*        U1000 -  . . . .                                                 
002500*                                                                         
002600                                                                          
002700                                                                          
002800 ENVIRONMENT DIVISION.                                                    
002900                                                                          
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300                                                                          
003400*          --- RENSNING AV WDM6                                           
003500     SELECT W37137                     ASSIGN TO W37136D1.                
003600                                                                          
003700*          --- HISTFIL ÖVER GODKÄNDA RAPPORTER                            
003800     SELECT W37138                     ASSIGN TO W37136D2.                
003900                                                                          
004000*          --- HISTFIL ÖVER GODKÄNDA RAPPORTER NDC                        
004100     SELECT W37153                     ASSIGN TO W37136D3.                
004110                                                                          
004200*          --- UPPDATERING AV KDBYTBEK TILL KLAR I WDM6                   
004300     SELECT W37154                     ASSIGN TO W37136D4.                
004400     EJECT                                                                
004500 DATA DIVISION.                                                           
004600                                                                          
004700 FILE SECTION.                                                            
004800                                                                          
004900 FD  W37137                                                               
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200                                                                          
005300*01  POST -COPY W371STAT -PRE  STAT-  -L.                                 
005400                                                                          
005500 FD  W37138                                                               
005600     RECORDING       F                                                    
005700     BLOCK CONTAINS  0.                                                   
005800                                                                          
005900*01  POST -COPY W37138 -PRE  HIST-  -L.                                   
006000 FD  W37153                                                               
006100     RECORDING       F                                                    
006200     BLOCK CONTAINS  0.                                                   
006300                                                                          
006400*01  POST -COPY W37138 -PRE  HNDC-  -L.                                   
006500 FD  W37154                                                               
006600     RECORDING       F                                                    
006700     BLOCK CONTAINS  0.                                                   
006800                                                                          
006900*01  POST -COPY W37154 -PRE  KDBT-  -L.                                   
007000     EJECT                                                                
007100 WORKING-STORAGE SECTION.                                                 
007200*    -- CHECKED BY WY2000                                                 
007300                                                                          
007400 77  IDPGM                       PIC X(8)    VALUE 'W3713600'.            
007500 77  JA                          PIC X       VALUE 'J'.                   
007600 77  NEJ                         PIC X       VALUE 'N'.                   
007700 77  KLART                       PIC X       VALUE 'N'.                   
007800 77  OK                          PIC X       VALUE ' '.                   
007900 77  SKAPA-HISTORIK              PIC X       VALUE 'N'.                   
008000 77  WS-KDBYTBEK                 PIC X       VALUE ' '.                   
008100 01  WS-IDARTNR                  PIC  X(9)   VALUE ZERO.                  
008200 01  FILLER REDEFINES WS-IDARTNR.                                         
008300     03 FILLER                   PIC 9(5).                                
008400     03 WS-ARTSIFFRA             PIC 9(1).                                
008500        88 ART-0                 VALUE 6.                                 
008510        88 ART-1                 VALUE 4  7.                              
008600        88 ART-2                 VALUE 5  8.                              
008700        88 ART-3                 VALUE 9.                                 
008800     03 FILLER                   PIC  9(3).                               
008930                                                                          
009000 01  TEST-IDARTNR                PIC 9(9)   COMP-3.                       
009100*01  FILLER -COPY WWBYT16   -RED TEST-IDARTNR                             
009200     EJECT                                                                
009300                                                                          
009400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009500 01  FILLER REDEFINES DAGENS-DATUM.                                       
009600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009900                                                                          
010000 01  DAGENS-VECKA                PIC 9(6)    VALUE ZERO.                  
010100 01  FILLER REDEFINES DAGENS-VECKA.                                       
010200     03  DATKORT-DATUM-SEKEL     PIC 9(2).                                
010300     03  DATKORT-DATUM-AAR       PIC 9(2).                                
010400     03  DATKORT-DATUM-VECKA     PIC 9(2).                                
010500                                                                          
010600                                                                          
010700 01  GODKAND-VECKA               PIC 9(6)    VALUE ZERO.                  
010800 01  FILLER REDEFINES GODKAND-VECKA.                                      
010900     03  DATUM-SEKEL             PIC 9(2).                                
011000     03  DATUM-AAR               PIC 9(2).                                
011100     03  DATUM-VECKA             PIC 9(2).                                
011200                                                                          
011300 01  DAGENS-AAVV                 PIC 9(4)    VALUE ZERO.                  
011400 01  FILLER REDEFINES DAGENS-AAVV.                                        
011500     03 DAGENS-AA                PIC 9(2).                                
011600     03 DAGENS-VV                PIC 9(2).                                
011700*                                                                         
011800 01  FILLER                  PIC X(16)   VALUE 'WS-SEKTION'.              
011900 01  WS-SEKTION                  PIC X(30)   VALUE SPACE.                 
012000 01  FILLER                  PIC X(16)   VALUE 'WS-IMS-SEKTION'.          
012100 01  WS-IMS-SEKTION              PIC X(30)   VALUE SPACE.                 
012200 01  FILLER                  PIC X(16)   VALUE 'WS-FIL-SEKTION'.          
012300 01  WS-FIL-SEKTION              PIC X(30)   VALUE SPACE.                 
012400                                                                          
012500*                                                                         
012600 01  WS-RENSNINGS-DATUM          PIC 9(8)    VALUE ZERO.                  
012700*                                                                         
012800 01  WS-DAREGDAT-GODK            PIC 9(8)    VALUE ZERO.                  
012900*                                                                         
013000     EJECT                                                                
013100 01  DYNAMISKA-SUBPROGRAM.                                                
013200*                                                                         
013300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
013400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
013700     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
013800     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
013900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
014000                                                                          
014100*    --- PARAMETRAR TILL ABEND                                            
014200                                                                          
014300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
014400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
014500                                                                          
014600 01  FELTEXT.                                                             
014700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
014800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
014900     EJECT                                                                
015000*    --- PARAMETRAR TILL POSTSUM                                          
015100*                                                                         
015200*01  -COPY W0005   -PRE  POSTSUM-                                         
015300     EJECT                                                                
015400*    --- VALID IDDC CODES                                                 
015500*                                                                         
015600*01  -COPY WWDC99                                                         
015700*                                                                         
015800*01  -COPY WDAGAREA                                                       
015900     EJECT                                                                
016000*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKORT                     
016100                                                                          
016200 01  FILLER                   PIC X(16) VALUE 'DATKORT'.                  
016300 01  DATUMKORT-ID             PIC X(6)  VALUE 'WDATUM'.                   
016400*01  -COPY WDATKORT                                                       
016500*- - - - - - - - - - - - - -  PARAMETRAR TILL WDATKONV                    
016600 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
016700                                                                          
016800*01  -COPY WDATAREA.                                                      
016900*                                                                         
017000     EJECT                                                                
017100 01  STAT-AREA-START             PIC X(24)   VALUE                        
017200                                 'STAT-AREA-START  '.                     
017300                                                                          
017400                                                                          
017500*01  AREA -COPY W371STAT     -PRE STAT-                                   
017600     EJECT                                                                
017700 01  HIST-AREA-START             PIC X(24)   VALUE                        
017800                                 'HIST-AREA-START  '.                     
017900                                                                          
018000                                                                          
018100*01  AREA -COPY W37138     -PRE HIST-                                     
018200 01  HNDC-AREA-START             PIC X(24)   VALUE                        
018300                                 'HNDC-AREA-START '.                      
018400                                                                          
018500                                                                          
018600*01  AREA -COPY W37138     -PRE HNDC-                                     
018700                                                                          
018800 01  KDBT-AREA-START             PIC X(24)   VALUE                        
018900                                 'KDBT-AREA-START '.                      
019000*01  AREA -COPY W37154     -PRE  KDBT-                                    
019100                                                                          
019200     EJECT                                                                
019300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019400 01  NYCKLAR-TILL-DLI.                                                    
019500                                                                          
019600     03  W-WDM6ASEQ-MIN.                                                  
019700         05 W-WDM6A-KDBYTSTA-MIN PIC X      VALUE '9'.                    
019800         05 W-WDM6A-IDDC-MIN     PIC X(2)   VALUE SPACE.                  
019900         05 W-WDM6A-DAREGDAT-MIN PIC 9(8)   VALUE ZERO.                   
020000         05 W-WDM6A-IDDISTR-MIN  PIC S9(5)  VALUE ZERO   COMP-3.          
020100         05 W-WDM6A-IDBYTRAP-MIN PIC S9(7)  VALUE ZERO   COMP-3.          
020110                                                                          
020120     03  W-WDM6ASEQ-MAX.                                                  
020130         05 W-WDM6A-KDBYTSTA-MAX PIC X      VALUE '9'.                    
020140         05 W-WDM6A-IDDC-MAX     PIC X(2)   VALUE HIGH-VALUE.             
020150         05 W-WDM6A-DAREGDAT-MAX PIC 9(8)   VALUE 99999999.               
020160         05 W-WDM6A-IDDISTR-MAX  PIC S9(5)  VALUE +99999   COMP-3.        
020170         05 W-WDM6A-IDBYTRAP-MAX PIC S9(7)  VALUE +9999999 COMP-3.        
020200                                                                          
020700     03  W-WDK6-IDARTNR-X.                                                
020800         05  W-WDK6-IDARTNR      PIC S9(9)   VALUE ZERO COMP-3.           
020900                                                                          
021000     03  W-WDD3-IDARTNR-X.                                                
021100         05  W-WDD3-IDARTNR      PIC S9(9)   VALUE ZERO COMP-3.           
021200     03  W-WDD3-IDSKYLT-X.                                                
021300         05  W-WDD3-IDSKYLT      PIC X(3)    VALUE SPACE.                 
021400                                                                          
021500     03  W-WDR2-WDGX30-X.                                                 
021600         05  FILLER              PIC X(4)    VALUE '3139'.                
021700         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
021800     03  W-WDR2-IDTABNR-X.                                                
021900         05  W-WDR2-IDTABNR       PIC S9(3)  VALUE ZERO COMP-3.           
022000                                                                          
022100*    --- STATUS-KOD FRÅN IMS                                              
022200 01  STATUS-WS                   PIC XX.                                  
022300     88  SEGMENT-FINNS                       VALUE '  '.                  
022400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
022500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
022600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
022700                                                                          
022800 01  GODK-STATUSKODER.                                                    
022900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023000                                                                          
023100 01  SSA1                        PIC X(256).                              
023200 01  SSA2                        PIC X(256).                              
023300     EJECT                                                                
023400*    --- IMS FUNKTIONSKODER                                               
023500*01  -COPY W0003                                                          
023600     EJECT                                                                
023700*    ---  DLI INPUT-OUTPUT AREA                                           
023800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
023900                                                                          
024000 01  DLI-IO-AREA.                                                         
024100     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
024200                                                                          
024300     03  WDM601 REDEFINES IO-AREA.                                        
024400*        05  -COPY WDM601                                                 
024500                                                                          
024600     03  WDM611 REDEFINES IO-AREA.                                        
024700*        05  -COPY WDM611                                                 
024800 01  DLI-IO-AREA2.                                                        
024900     03  IO-AREA2                 PIC X(150)  VALUE SPACE.                
025000                                                                          
025100     03  WDK601 REDEFINES IO-AREA2.                                       
025200*        05  -COPY WDK601                                                 
025300     EJECT                                                                
025400     03  WDD311 REDEFINES IO-AREA2.                                       
025500*        05  -COPY WDD311                                                 
025600     EJECT                                                                
025700     03  WDR230 REDEFINES IO-AREA2.                                       
025800*        05  -COPY WDGX3140                                               
025900     EJECT                                                                
026000 LINKAGE SECTION.                                                         
026100                                                                          
026200     EJECT                                                                
026500     EJECT                                                                
026600*01  -COPY W0008  -PRE WDM6-                                              
026700     05  FILLER                  PIC X.                                   
026800     EJECT                                                                
026900*01  -COPY W0008  -PRE WDK6-                                              
027000     05  FILLER                  PIC X.                                   
027100     EJECT                                                                
027200*01  -COPY W0008  -PRE WDD3-                                              
027300     05  FILLER                  PIC X.                                   
027400                                                                          
027500*01  -COPY W0008  -PRE WDR2-                                              
027600     05  FILLER                  PIC X.                                   
027700     EJECT                                                                
027900 PROCEDURE DIVISION  USING WDM6-PCB WDK6-PCB WDD3-PCB WDR2-PCB.           
028100     ENTRY 'DLITCBL' USING WDM6-PCB WDK6-PCB WDD3-PCB WDR2-PCB.           
028200                                                                          
028300     PERFORM A-INIT                                                       
028400                                                                          
028500     PERFORM IMS-GU-WDM601                                                
028600     PERFORM UNTIL NOT SEGMENT-FINNS                                      
028700       PERFORM B-BEHANDLA-WDM601                                          
028800       IF SKAPA-HISTORIK = JA                                             
028900                                                                          
029000         PERFORM IMS-GNP-WDM611                                           
029100         PERFORM UNTIL NOT SEGMENT-FINNS                                  
029200             PERFORM C-FLYTTA-TILL-UTFILER                                
029300             PERFORM IMS-GNP-WDM611                                       
029400         END-PERFORM                                                      
029500                                                                          
029600       END-IF                                                             
029700       PERFORM IMS-GN-WDM601                                              
029800     END-PERFORM                                                          
029900                                                                          
030000     PERFORM Z-FINIT                                                      
030100                                                                          
030200     MOVE ZERO                     TO RETURN-CODE                         
030300     GOBACK                                                               
030400     .                                                                    
030500     EJECT                                                                
030600 A-INIT SECTION.                                                          
030700     MOVE 'A-INIT'                 TO WS-SEKTION                          
030800                                                                          
030900     OPEN OUTPUT W37137                                                   
031000                 W37138                                                   
031100                 W37153                                                   
031200                 W37154                                                   
031300                                                                          
031400     ACCEPT DAGENS-DATUM         FROM DATE                                
031500     MOVE IDPGM                    TO POSTSUM-PROGNAMN                    
031600     PERFORM AB-BORTTAGSDATUM                                             
031700     PERFORM AC-HAMTA-DATKORT                                             
031800     .                                                                    
031900     EJECT                                                                
032000 AB-BORTTAGSDATUM SECTION.                                                
032100     MOVE 'AB-BORTTAGSDATUM'       TO WS-SEKTION                          
032200*************************************************************             
032300**** SKAPAR ETT DATUM FÖR BORTRENSINGEN AV RAPPORTER ********             
032400**** FRÅN WDM6 SOM LEGAT MINST 12 VECKOR PÅ REGISTRET********             
032500**** 7 * 12 = 84 DVS 84 DAGAR                        ********             
032600*************************************************************             
032700                                                                          
032800     ACCEPT DAG-TIAAMMDD-TOM     FROM DATE                                
032900     MOVE 364                      TO DAG-KVKALDAG                        
033000     MOVE 003                      TO DAG-KDCALL                          
033100                                                                          
033200     CALL WDAGKONV              USING DAG-KDCALL                          
033300                                      DAG-DATUM-AREA                      
033400                                      DAG-KDSVAR                          
033500                                                                          
033600     IF DAG-KDSVAR = OK                                                   
033700       MOVE DAG-TIAAMMDD-FOM       TO WS-RENSNINGS-DATUM (3:6)            
033800       MOVE DAG-TISEKEL-FOM        TO WS-RENSNINGS-DATUM (1:2)            
033900       DISPLAY '*** RENSNINGSDATUM FÖR WDM6 ****** '                      
034000       DISPLAY 'RENSNINGSDATUM ' WS-RENSNINGS-DATUM                       
034100     ELSE                                                                 
034200       MOVE 'FEL I DATUMKONVERTERING'                                     
034300                                   TO FELTEXT-STR                         
034400       PERFORM S99-ABEND                                                  
034500     END-IF                                                               
034600     .                                                                    
034700                                                                          
034800     EJECT                                                                
034900 AC-HAMTA-DATKORT SECTION.                                                
035000     MOVE 'AC-HAMTA-DATKORT'       TO WS-SEKTION                          
035100                                                                          
035200     CALL DATKORT               USING IDPGM                               
035300                                      DATUMKORT-ID                        
035400                                      DATUMKORT                           
035500                                                                          
035600     MOVE D-AAR                    TO DATKORT-DATUM-AAR                   
035700     MOVE D-VECKA                  TO DATKORT-DATUM-VECKA                 
035800                                                                          
035900     IF D-AAR > 60                                                        
036000        MOVE 19                    TO DATKORT-DATUM-SEKEL                 
036100     ELSE                                                                 
036200        MOVE 20                    TO DATKORT-DATUM-SEKEL                 
036300     END-IF                                                               
036400                                                                          
036500     DISPLAY 'ALLA RAPPORTER SOM CLIVIT GODKÄNDA '                        
036600     DISPLAY 'DENNA VECKA SKALL SKAPAS HISTORIK  '                        
036700     DISPLAY 'DAGENS-VECKA ' DAGENS-VECKA                                 
036800     .                                                                    
036900     EJECT                                                                
037000 B-BEHANDLA-WDM601    SECTION.                                            
037100     MOVE 'B-BEHANDLA-WDM601'      TO WS-SEKTION                          
037200                                                                          
037300     MOVE 'N'                      TO SKAPA-HISTORIK                      
037400                                                                          
037500     MOVE RAPP-DAREGDAT-GODK       TO WS-DAREGDAT-GODK                    
037600     IF WS-RENSNINGS-DATUM > WS-DAREGDAT-GODK                             
037700*......SKAPA RENSNINGSFIL FÖR WDM6                                        
037800                                                                          
037900       MOVE RAPP-IDDISTR           TO STAT-IDDISTR                        
038000       MOVE RAPP-IDBYTRAP          TO STAT-IDBYTRAP                       
038100       PERFORM S11-SKRIV-W37137                                           
038200     ELSE                                                                 
038300       PERFORM BA-LAES-WDATKONV                                           
038400                                                                          
038500       IF DAGENS-VECKA = GODKAND-VECKA                                    
038600*........SKAPAR HISTORIK POSTER TILL ÖVRIGA VÄRLDEN OCH USA               
038700*        POSTERN PÅ DATABASEN WDM6 SKA HA SAMMA VECKA SOM                 
038800*        DAGENS VECKA   (PROGRAMMET KÖRS I SLUTET AV EN VECKA OCH         
038900*        SKAPAR HISTORIK   AV ALLA BYTESRAPPORTER SOM HAR BLIVIT          
039000*        GODKÄNDA DENNA VECKA)                                            
039100                                                                          
039200         MOVE JA                   TO SKAPA-HISTORIK                      
039300                                                                          
039700         MOVE RAPP-KDBYTBEK        TO WS-KDBYTBEK                         
039800                                                                          
039900         MOVE RAPP-IDDISTR         TO KDBT-IDDISTR                        
040000         MOVE RAPP-IDBYTRAP        TO KDBT-IDBYTRAP                       
040100         MOVE RAPP-KDBYTBEK        TO KDBT-KDBYTBEK                       
040200         IF WS-KDBYTBEK NOT = 'K'                                         
040300            PERFORM S14-SKRIV-W37154                                      
040400         END-IF                                                           
040500                                                                          
040600         MOVE RAPP-IDDISTR         TO HIST-IDDISTR                        
040700         MOVE RAPP-IDKUNDNR        TO HIST-IDKUNDNR                       
040800         MOVE RAPP-IDBYTRAP        TO HIST-IDBYTRAP                       
040900         MOVE RAPP-DAREGDAT (3:6)  TO HIST-TIREGDAT-DEALER                
041000         MOVE RAPP-IDFAKT          TO HIST-IDFAKT                         
041100         MOVE RAPP-DAANKDAG (3:6)  TO HIST-TIANKDAG                       
041200         MOVE RAPP-DAREGDAT-GODK(3:6)                                     
041300                                   TO HIST-TIREGDAT-GODK                  
041400         MOVE RAPP-IDUSER          TO HIST-IDUSER                         
041500         MOVE RAPP-IDDC            TO HIST-IDDC                           
041600                                                                          
041700         MOVE RAPP-IDDISTR         TO HNDC-IDDISTR                        
041800         MOVE RAPP-IDKUNDNR        TO HNDC-IDKUNDNR                       
041900         MOVE RAPP-IDBYTRAP        TO HNDC-IDBYTRAP                       
042000         MOVE RAPP-DAREGDAT (3:6)  TO HNDC-TIREGDAT-DEALER                
042100         MOVE RAPP-IDFAKT          TO HNDC-IDFAKT                         
042200         MOVE RAPP-DAANKDAG (3:6)  TO HNDC-TIANKDAG                       
042300         MOVE RAPP-DAREGDAT-GODK(3:6)                                     
042400                                   TO HNDC-TIREGDAT-GODK                  
042500         MOVE RAPP-IDUSER          TO HNDC-IDUSER                         
042600         MOVE RAPP-IDDC            TO HNDC-IDDC                           
042700       END-IF                                                             
042800     END-IF                                                               
042900     .                                                                    
043000     EJECT                                                                
043100 BA-LAES-WDATKONV       SECTION.                                          
043200     MOVE 'BA-LAS-WDATKONV'        TO WS-SEKTION                          
043300                                                                          
043400     MOVE 'AAMMDD'                 TO DAT-KDDATFORM                       
043500     MOVE RAPP-DAREGDAT-GODK(3:6)  TO DAT-I-TIDATUM                       
043600                                                                          
043700     CALL WDATKONV              USING DAT-KDDATFORM                       
043800                                      DAT-I-TIDATUM                       
043900                                      DAT-O-TIDATUM                       
044000                                      DAT-KDSVAR                          
044100                                                                          
044200     IF DAT-KDSVAR-OK                                                     
044300       MOVE DAT-TIAA-VECKA         TO DATUM-AAR                           
044400       MOVE DAT-TIVV               TO DATUM-VECKA                         
044500       MOVE RAPP-DAREGDAT-GODK (1:2)                                      
044600                                   TO DATUM-SEKEL                         
044700     ELSE                                                                 
044800       MOVE 'FEL I WDATKONV'       TO FELTEXT-STR                         
044900       PERFORM S99-ABEND                                                  
045000     END-IF                                                               
045100     .                                                                    
045200     EJECT                                                                
045300                                                                          
045400 C-FLYTTA-TILL-UTFILER SECTION.                                           
045500     MOVE 'C-FLYTTA-TILL-UTFILER'  TO WS-SEKTION                          
045600                                                                          
045700     MOVE OBJ-IDARTNR-OBJ          TO W-WDK6-IDARTNR                      
045800     IF W-WDK6-IDARTNR = ZERO                                             
045900       PERFORM CA-HAEMTA-KVITTAB-ARTNR                                    
046000     END-IF                                                               
046100     PERFORM IMS-GET-WDK601                                               
046200                                                                          
046300     IF SEGMENT-FINNS                                                     
046400       MOVE ART-IDFKNGRP           TO HIST-IDFKNGRP                       
046500                                      HNDC-IDFKNGRP                       
046600     ELSE                                                                 
046700       MOVE ZERO                   TO HIST-IDFKNGRP                       
046800                                      HNDC-IDFKNGRP                       
046900     END-IF                                                               
047000                                                                          
047100     PERFORM CB-HAEMTA-BENAEMNING                                         
047200                                                                          
047300     MOVE OBJ-IDORDER              TO HIST-IDORDNR7                       
047400     MOVE OBJ-IDBYTRAD             TO HIST-IDBYTRAD                       
047500     MOVE OBJ-IDARTNR-OBJ          TO HIST-IDARTNR-OBJ                    
047600     MOVE OBJ-IDTABNR              TO HIST-IDTABNR                        
047700     MOVE OBJ-KVRETUR-URSP         TO HIST-KVRETUR-URSP                   
047800     MOVE OBJ-KVRETUR-GODK         TO HIST-KVRETUR-GODK                   
047900     MOVE OBJ-KDBYTSTA-OBJ         TO HIST-KDBYTSTA-OBJ                   
048000     MOVE OBJ-BERADREF             TO HIST-BERADREF                       
048100     MOVE OBJ-KDBYTREF             TO HIST-KDBYTREF                       
048200                                                                          
048300     MOVE OBJ-IDORDER              TO HNDC-IDORDNR7                       
048400     MOVE OBJ-IDBYTRAD             TO HNDC-IDBYTRAD                       
048500     MOVE OBJ-IDARTNR-OBJ          TO HNDC-IDARTNR-OBJ                    
048600     MOVE OBJ-IDTABNR              TO HNDC-IDTABNR                        
048700     MOVE OBJ-KVRETUR-URSP         TO HNDC-KVRETUR-URSP                   
048800     MOVE OBJ-KVRETUR-GODK         TO HNDC-KVRETUR-GODK                   
048900     MOVE OBJ-KDBYTSTA-OBJ         TO HNDC-KDBYTSTA-OBJ                   
049000     MOVE OBJ-BERADREF             TO HNDC-BERADREF                       
049100     MOVE OBJ-KDBYTREF             TO HNDC-KDBYTREF                       
049200                                                                          
049300     IF CDC OR SDC OR NDC-PACIFIC                                         
049400        IF WS-KDBYTBEK NOT = 'K'                                          
049500           PERFORM S12-SKRIV-W37138                                       
049600        END-IF                                                            
049700     ELSE                                                                 
049800        IF WS-KDBYTBEK NOT = 'K'                                          
049900           PERFORM S13-SKRIV-W37153                                       
050000        END-IF                                                            
050100     END-IF                                                               
050200     .                                                                    
050300     EJECT                                                                
050400 CA-HAEMTA-KVITTAB-ARTNR SECTION.                                         
050500     MOVE 'CA-HAEMTA-KVITTAB-ARTNR' TO WS-SEKTION                         
050600                                                                          
050700     PERFORM  IMS-GU-WDR201                                               
050800     MOVE OBJ-IDTABNR              TO W-WDR2-IDTABNR                      
050900     PERFORM IMS-GNP-WDR230                                               
051000     IF SEGMENT-FINNS                                                     
051100       MOVE 3140-IDARTNR-BYT       TO WS-IDARTNR                          
051200                                      TEST-IDARTNR                        
051300*..... FRÅN HA VARIT RENOVERADE ARTIKLAR BLIR DE NU BRA OBJEKT            
051400       IF BYT16-RADIO                                                     
051500          MOVE 4                   TO WS-ARTSIFFRA                        
051600       ELSE                                                               
051610          IF WS-ARTSIFFRA = 0                                             
051620            MOVE 6                   TO WS-ARTSIFFRA                      
051630          ELSE                                                            
051700            IF WS-ARTSIFFRA = 1                                           
051800              MOVE 7                 TO WS-ARTSIFFRA                      
051900            ELSE                                                          
052000              IF WS-ARTSIFFRA = 2                                         
052100                MOVE 8               TO WS-ARTSIFFRA                      
052200              ELSE                                                        
052300                MOVE 9               TO WS-ARTSIFFRA                      
052400              END-IF                                                      
052500            END-IF                                                        
052510          END-IF                                                          
052600       END-IF                                                             
052700       MOVE WS-IDARTNR             TO W-WDK6-IDARTNR                      
052800     END-IF                                                               
052900     .                                                                    
053000     EJECT                                                                
053100 CB-HAEMTA-BENAEMNING    SECTION.                                         
053200     MOVE 'CB-HAEMTA-BENAMNING'    TO WS-SEKTION                          
053300                                                                          
053400     MOVE OBJ-IDARTNR-OBJ          TO W-WDD3-IDARTNR                      
053500     MOVE 'S  '                    TO W-WDD3-IDSKYLT                      
053600     PERFORM IMS-GU-WDD311                                                
053700     IF SEGMENT-FINNS                                                     
053800        MOVE TEXT-BEART            TO HIST-BEART-SVE                      
053900                                      HNDC-BEART-SVE                      
054000     ELSE                                                                 
054100        MOVE SPACE                 TO HIST-BEART-SVE                      
054200                                      HNDC-BEART-SVE                      
054300     END-IF                                                               
054400                                                                          
054500     MOVE OBJ-IDARTNR-OBJ          TO W-WDD3-IDARTNR                      
054600     MOVE 'GB '                    TO W-WDD3-IDSKYLT                      
054700     PERFORM IMS-GU-WDD311                                                
054800     IF SEGMENT-FINNS                                                     
054900        MOVE TEXT-BEART            TO HIST-BEART-ENG                      
055000                                      HNDC-BEART-ENG                      
055100     ELSE                                                                 
055200        MOVE SPACE                 TO HIST-BEART-ENG                      
055300                                      HNDC-BEART-ENG                      
055400     END-IF                                                               
055500     .                                                                    
055600                                                                          
055700     EJECT                                                                
055800 Z-FINIT SECTION.                                                         
055900     MOVE 'Z-FINIT'                TO WS-SEKTION                          
056000                                                                          
056100     CLOSE W37137                                                         
056200           W37138                                                         
056300           W37153                                                         
056400           W37154                                                         
056500                                                                          
056600     MOVE 'S'                      TO POSTSUM-OPKOD                       
056700     CALL POSTSUM               USING POSTSUM-PARM                        
056800     .                                                                    
056900     EJECT                                                                
057000 S11-SKRIV-W37137 SECTION.                                                
057100     MOVE 'S11-SKRIV-W37137'       TO WS-FIL-SEKTION                      
057200                                                                          
057300     WRITE STAT-POST             FROM STAT-AREA                           
057400                                                                          
057500     MOVE 'STAT'                   TO POSTSUM-TRANSTYP                    
057600     MOVE 'W37137'                 TO POSTSUM-FDNAMN                      
057700     MOVE 'W37136D1'               TO POSTSUM-DDNAMN2                     
057800     CALL POSTSUM               USING POSTSUM-PARM                        
057900     .                                                                    
058000     EJECT                                                                
058100 S12-SKRIV-W37138 SECTION.                                                
058200     MOVE 'S12-SKRIV-W37138'       TO WS-FIL-SEKTION                      
058300                                                                          
058400     WRITE HIST-POST             FROM HIST-AREA                           
058500                                                                          
058600     MOVE 'HIST'                   TO POSTSUM-TRANSTYP                    
058700     MOVE 'W37138'                 TO POSTSUM-FDNAMN                      
058800     MOVE 'W37136D2'               TO POSTSUM-DDNAMN2                     
058900     CALL POSTSUM               USING POSTSUM-PARM                        
059000     .                                                                    
059100     EJECT                                                                
059200 S13-SKRIV-W37153 SECTION.                                                
059300     MOVE 'S13-SKRIV-W37153'       TO WS-FIL-SEKTION                      
059400                                                                          
059500     WRITE HNDC-POST             FROM HNDC-AREA                           
059600                                                                          
059700     MOVE 'HNDC'                   TO POSTSUM-TRANSTYP                    
059800     MOVE 'W37153'                 TO POSTSUM-FDNAMN                      
059900     MOVE 'W37136D3'               TO POSTSUM-DDNAMN2                     
060000     CALL POSTSUM USING POSTSUM-PARM                                      
060100     .                                                                    
060200     EJECT                                                                
060300 S14-SKRIV-W37154 SECTION.                                                
060400     MOVE 'S14-SKRIV-W37154'       TO WS-FIL-SEKTION                      
060500                                                                          
060600     WRITE KDBT-POST             FROM KDBT-AREA                           
060700                                                                          
060800     MOVE 'KDBT'                   TO POSTSUM-TRANSTYP                    
060900     MOVE 'W37154'                 TO POSTSUM-FDNAMN                      
061000     MOVE 'W37136D4'               TO POSTSUM-DDNAMN2                     
061100     CALL POSTSUM               USING POSTSUM-PARM                        
061200     .                                                                    
061300     EJECT                                                                
061400 S99-ABEND SECTION.                                                       
061500                                                                          
061600     MOVE 'S'                      TO POSTSUM-OPKOD                       
061700     CALL POSTSUM               USING POSTSUM-PARM                        
061800     CALL ABEND                 USING RKOD-ABEND-UTAN-DUMP                
061900     .                                                                    
062000     EJECT                                                                
062100* --- IMS SEKTIONER ---                                                   
062200                                                                          
062300 IMS-GU-WDM601     SECTION.                                               
062400     MOVE 'IMS-GU-WDM601'          TO WS-IMS-SEKTION                      
062500                                                                          
062600     STRING 'WDM601  (WDM6ASEQ>=' W-WDM6ASEQ-MIN                          
062700                    '&WDM6ASEQ<=' W-WDM6ASEQ-MAX ')'                      
062800          DELIMITED BY SIZE      INTO SSA1                                
062900     MOVE '  GE'                   TO GODK-STATUSKODER                    
063000     CALL CBLTDLI USING GU WDM6-PCB DLI-IO-AREA SSA1                      
063100     MOVE WDM6-STATUS-CODE         TO STATUS-WS                           
063200     PERFORM IMS-STATUSKONTROLL                                           
063300     .                                                                    
063400 IMS-GN-WDM601     SECTION.                                               
063500     MOVE 'IMS-GN-WDM601'          TO WS-IMS-SEKTION                      
063600                                                                          
063700     STRING 'WDM601  (WDM6ASEQ>=' W-WDM6ASEQ-MIN                          
063800                    '&WDM6ASEQ<=' W-WDM6ASEQ-MAX ')'                      
063900          DELIMITED BY SIZE      INTO SSA1                                
064000     MOVE '  GEGB'                 TO GODK-STATUSKODER                    
064100     CALL CBLTDLI USING GN WDM6-PCB DLI-IO-AREA SSA1                      
064200     MOVE WDM6-STATUS-CODE     TO STATUS-WS                               
064300     PERFORM IMS-STATUSKONTROLL                                           
064400     .                                                                    
064500 IMS-GNP-WDM611 SECTION.                                                  
064600     MOVE 'IMS-GNP-WDM611'         TO WS-IMS-SEKTION                      
064700                                                                          
065000     MOVE   'WDM611'               TO SSA1                                
065100     MOVE '  GEGB'                 TO GODK-STATUSKODER                    
065200     CALL CBLTDLI USING GNP WDM6-PCB DLI-IO-AREA SSA1                     
065300     MOVE WDM6-STATUS-CODE         TO STATUS-WS                           
065400     PERFORM IMS-STATUSKONTROLL                                           
065500     .                                                                    
065600     EJECT                                                                
065700 IMS-GET-WDK601 SECTION.                                                  
065800     MOVE 'IMS-GET-WDK601'         TO WS-IMS-SEKTION                      
065900                                                                          
066000     STRING 'WDK601  (IDARTNR  =' W-WDK6-IDARTNR-X ')'                    
066100          DELIMITED BY SIZE      INTO SSA1                                
066200     MOVE '  GE'                   TO GODK-STATUSKODER                    
066300     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA2 SSA1                     
066400     MOVE WDK6-STATUS-CODE         TO STATUS-WS                           
066500     PERFORM IMS-STATUSKONTROLL                                           
066600     .                                                                    
066700     EJECT                                                                
066800 IMS-GU-WDD311 SECTION.                                                   
066900     MOVE 'IMS-GU-WDD311'          TO WS-IMS-SEKTION                      
067000                                                                          
067100     STRING 'WDD301  (WDD3BSEQ =' W-WDD3-IDARTNR-X ')'                    
067200          DELIMITED BY SIZE      INTO SSA1                                
067300     STRING 'WDD311  (IDSKYLT  =' W-WDD3-IDSKYLT-X ')'                    
067400          DELIMITED BY SIZE      INTO SSA2                                
067500     MOVE '  GE'                   TO GODK-STATUSKODER                    
067600     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-AREA2 SSA1 SSA2                
067700     MOVE WDD3-STATUS-CODE         TO STATUS-WS                           
067800     PERFORM IMS-STATUSKONTROLL                                           
067900     .                                                                    
068000     EJECT                                                                
068100 IMS-GU-WDR201   SECTION.                                                 
068200     MOVE 'IMS-GU-WDR201'          TO WS-IMS-SEKTION                      
068300                                                                          
068400     STRING 'WDR201  (WDGXKEY  =' W-WDR2-WDGX30-X ')'                     
068500          DELIMITED BY SIZE      INTO SSA1                                
068600     MOVE '  '                     TO GODK-STATUSKODER                    
068700     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-AREA2 SSA1                     
068800     MOVE WDR2-STATUS-CODE         TO STATUS-WS                           
068900     PERFORM IMS-STATUSKONTROLL                                           
069000     .                                                                    
069100     EJECT                                                                
069200 IMS-GNP-WDR230 SECTION.                                                  
069300     MOVE 'IMS-GNP-WDR230'         TO WS-IMS-SEKTION                      
069400                                                                          
069500     STRING 'WDR201  (WDGXKEY  =' W-WDR2-WDGX30-X ')'                     
069600          DELIMITED BY SIZE      INTO SSA1                                
069700     STRING 'WDR230  (IDTABNR  =' W-WDR2-IDTABNR-X ')'                    
069800          DELIMITED BY SIZE      INTO SSA2                                
069900     MOVE '  GE'                   TO GODK-STATUSKODER                    
070000     CALL CBLTDLI USING GN WDR2-PCB DLI-IO-AREA2 SSA1 SSA2                
070100     MOVE WDR2-STATUS-CODE         TO STATUS-WS                           
070200     PERFORM IMS-STATUSKONTROLL                                           
070300     .                                                                    
070400     EJECT                                                                
070500 IMS-STATUSKONTROLL SECTION.                                              
070600                                                                          
070700     SET STATUS-IX TO 1                                                   
070800     SEARCH GODK-STATUS                                                   
070900       AT END                                                             
071000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
071100           DELIMITED BY SIZE INTO FELTEXT                                 
071200         DISPLAY FELTEXT                                                  
071300         DISPLAY 'SECTION:' WS-IMS-SEKTION                                
071400         DISPLAY 'SSA1   :' SSA1                                          
071500         DISPLAY 'SSA2   :' SSA2                                          
071600         CALL FELLOG                                                      
071700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
071800         CONTINUE                                                         
071900     END-SEARCH                                                           
072000     .                                                                    
