000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.      W3714600.                                               
000400 AUTHOR.          INGVAR SKJELBRED                                        
000500 DATE-WRITTEN.    AUGUSTI  1997.                                          
000600                                                                          
000700*    REMARKS.                                                             
000800**                                                                        
000900*                                                                         
001000*    FUNKTION:                                                            
001100*       PROGRAMET LÄSER IGENOM WDM6 (HISTORIKBAS FÖR S-LAGER) OCH         
001200*       SKRIVER EN FIL MED DATA TILL LIFO SYSTEMET.                       
001300*    ABENDKODER:                                                          
001400*            U0016 - OM FELAKTIG RETURKOD FRÅN WORKDAY                    
001500*                                                                         
001600*    CHANGE LOG:                                                          
001700*                                                                         
001800*      YY/MM/DD - NAME            - CHANGE DESCRIPTION                    
001900*      ----------------------------------------------------------         
002000*      15/04/17 - REDDY RAHUL     - CHINA EXCHANGE PHASE 2.               
002100*                                   E'TRACKER 10252358                    
002200*                                   ADD DISPLAY FOR CHINA DC'S.           
002300*                                                                         
002400     EJECT                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP3                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     SKIP2                                                                
003100     SELECT W3714A                     ASSIGN TO W37146D1.                
003200     SELECT W3714B                     ASSIGN TO W37146D2.                
003300     SELECT W3714C                     ASSIGN TO W37146D3.                
003400     SELECT W3714D                     ASSIGN TO W37146D4.                
003500     SELECT W3714E                     ASSIGN TO W37146D5.                
003600     SELECT W3714F                     ASSIGN TO W37146D6.                
003700     SELECT W3714G                     ASSIGN TO W37146D7.                
003800     SELECT W3714J                     ASSIGN TO W37146D8.                
003900     SELECT W3714N                     ASSIGN TO W37146D9.                
004000     SELECT W3714R                     ASSIGN TO W37146DA.                
004010     SELECT W3714X                     ASSIGN TO W37146DB.                
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP2                                                                
004400 FILE SECTION.                                                            
004500     SKIP3                                                                
004600 FD  W3714A                                                               
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000*01  POST -COPY W3714A -PRE UT1-  -L.                                     
005100                                                                          
005200     EJECT                                                                
005300 FD  W3714B                                                               
005400     RECORDING       F                                                    
005500     BLOCK CONTAINS  0.                                                   
005600                                                                          
005700*01  POST -COPY W3714B -PRE UT2-  -L.                                     
005800                                                                          
005900     EJECT                                                                
006000 FD  W3714C                                                               
006100     RECORDING       F                                                    
006200     BLOCK CONTAINS  0.                                                   
006300                                                                          
006400*01  POST -COPY W3714C -PRE UT3-  -L.                                     
006500                                                                          
006600     EJECT                                                                
006700 FD  W3714D                                                               
006800     RECORDING       F                                                    
006900     BLOCK CONTAINS  0.                                                   
007000                                                                          
007100*01  POST -COPY W3714D -PRE UT4-  -L.                                     
007200                                                                          
007300     EJECT                                                                
007400 FD  W3714E                                                               
007500     RECORDING       F                                                    
007600     BLOCK CONTAINS  0.                                                   
007700                                                                          
007800*01  POST -COPY W3714E -PRE UT5-  -L.                                     
007900                                                                          
008000     EJECT                                                                
008100 FD  W3714F                                                               
008200     RECORDING       F                                                    
008300     BLOCK CONTAINS  0.                                                   
008400                                                                          
008500*01  POST -COPY W3714F -PRE UT6-  -L.                                     
008600                                                                          
008700 FD  W3714G                                                               
008800     RECORDING       F                                                    
008900     BLOCK CONTAINS  0.                                                   
009000                                                                          
009100*01  POST -COPY W3714G -PRE UT7-  -L.                                     
009200                                                                          
009300     EJECT                                                                
009400                                                                          
009500 FD  W3714J                                                               
009600     RECORDING       F                                                    
009700     BLOCK CONTAINS  0.                                                   
009800                                                                          
009900*01  POST -COPY W3714J -PRE UT8-  -L.                                     
010000                                                                          
010100     EJECT                                                                
010200     SKIP3                                                                
010300 FD  W3714N                                                               
010400     RECORDING       F                                                    
010500     BLOCK CONTAINS  0.                                                   
010600                                                                          
010700*01  POST -COPY W3714N -PRE UT9-  -L.                                     
010800                                                                          
010900     EJECT                                                                
011000                                                                          
011100 FD  W3714R                                                               
011200     RECORDING       F                                                    
011300     BLOCK CONTAINS  0.                                                   
011400                                                                          
011500*01  POST -COPY W3714J -PRE UTA-  -L.                                     
011600                                                                          
011700     EJECT                                                                
011710 FD  W3714X                                                               
011720     RECORDING       F                                                    
011730     BLOCK CONTAINS  0.                                                   
011740                                                                          
011750*01  POST -COPY W3714X -PRE UTB-  -L.                                     
011760                                                                          
011770     EJECT                                                                
011800 WORKING-STORAGE SECTION.                                                 
011900                                                                          
012000                                                                          
012100*    -- CHECKED BY WY2000                                                 
012200 77   IDPGM                      PIC X(8)    VALUE 'W3714600'.            
012300                                                                          
012400 77  KLART-SW                    PIC X       VALUE 'N'.                   
012500     88  KLART-SLUT                          VALUE 'J'.                   
012600                                                                          
012700                                                                          
012800                                                                          
012900 01  FILLER                  PIC X(16)   VALUE 'WS-SEKTION'.              
013000 01  WS-SEKTION              PIC X(30)   VALUE SPACE.                     
013100 01  FILLER                  PIC X(16)   VALUE 'WS-SEKTION2'.             
013200 01  WS-SEKTION2             PIC X(30)   VALUE SPACE.                     
013300 01  FILLER                  PIC X(16)   VALUE 'WS-IMS-SEKTION'.          
013400 01  WS-IMS-SEKTION              PIC X(30)   VALUE SPACE.                 
013500 01  FILLER                  PIC X(16)   VALUE 'WS-FIL-SEKTION'.          
013600 01  WS-FIL-SEKTION              PIC X(30)   VALUE SPACE.                 
013700                                                                          
013800 01  JA                          PIC X       VALUE 'J'.                   
013900 01  NEJ                         PIC X       VALUE 'N'.                   
014000 01  W-IDDC                      PIC X(2)    VALUE SPACE.                 
014100 01  SPAR-IDARTNR                PIC S9(9) COMP-3 VALUE +0.               
014200 01  SPAR-IDDISTR                PIC S9(5) COMP-3 VALUE +0.               
014300 01  SPAR-IDBYTRAP               PIC S9(7) COMP-3 VALUE +0.               
014400 01  SPAR-IDBYTRAP2              PIC S9(7) COMP-3 VALUE +0.               
016200 01  SPAR-IDDC                   PIC X(2)    VALUE SPACE.                 
016210 01  SPAR-IDUSER                 PIC X(8)    VALUE SPACE.                 
016300 01  WS-IDDC-WORKDAY             PIC X(2)    VALUE SPACE.                 
016400 01  WS-IDDC2-WORKDAY            PIC X(2)    VALUE SPACE.                 
016500 01  SPAR-IDKUNDNR               PIC S9(7) COMP-3 VALUE +0.               
016600 01  SPAR-TIANKDAG               PIC 9(8)    VALUE ZERO.                  
016700 01  WS-TIANKDAG                 PIC 9(8)    VALUE ZERO.                  
016800 01  SPAR-TIREGDAT               PIC 9(8)    VALUE ZERO.                  
016900 01  SPAR-TIREGDAT-GODK          PIC 9(8)    VALUE ZERO.                  
017000 01  WS-TIREGDAT-GODK            PIC 9(8)    VALUE ZERO.                  
017100 01  SPAR-KDBYTSTA               PIC X       VALUE SPACE.                 
017200 01  SPAR-KVRETUR                PIC S9(7) COMP-3 VALUE +0.               
017400 01  SPAR-FLBYTGAR               PIC X.                                   
017500 01  SPAR-FLBYGODK               PIC X       VALUE SPACE.                 
017600 01  POST-SKRIVEN                PIC X       VALUE 'J'.                   
017700 01  WDATUM                      PIC X(6)    VALUE 'WDATUM'.              
017800 01  WS-DAGENS-DATUM             PIC 9(8)   VALUE ZERO.                   
017900 01  DAGENS-DATUM                PIC 9(8).                                
018000 01  RED-DATUM                   REDEFINES   DAGENS-DATUM.                
018100     03   DAGENS-DATUM-SEKEL     PIC 9(2).                                
018200     03   DAGENS-DATUM-AR        PIC 9(2).                                
018300     03   DAGENS-DATUM-MANAD     PIC 9(2).                                
018400     03   DAGENS-DATUM-DAG       PIC 9(2).                                
018500 01  WS-DATUM                    PIC 9(8).                                
018600 01  WS-DATUM-RED       REDEFINES WS-DATUM.                               
018700     03   DATUM-SEKEL            PIC 9(2).                                
018800     03   DATUM-AR               PIC 9(2).                                
018900     03   DATUM-MANAD            PIC 9(2).                                
019000     03   DATUM-DAG              PIC 9(2).                                
019010     EJECT                                                                
019110*01  -COPY WWDC99                                                         
019111     EJECT                                                                
019200 01  DYNAMISKA-SUBPROGRAM.                                                
019300   03  WORKDAY                   PIC X(8)    VALUE 'WORKDAY'.             
019400   03  DATKORT                   PIC X(8)    VALUE 'DATKORT'.             
019500   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
019600   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
019700   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
019800   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
019900     EJECT                                                                
020000*- - - - - - -  - - - - - - - RETURKODER                                  
020100 01  RETURKODER.                                                          
020200     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4) COMP VALUE +16.                
020300                                                                          
020400*- - - - - - - - - - - - - -  MEDDELANDE                                  
020500 01  MEDDELANDE.                                                          
020600     03  MEDDELANDE-1           PIC X(29)   VALUE                         
020700         'FELAKTIGT SVAR FRÅN WORKDAY  '.                                 
020800*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKORT                     
020900                                                                          
021000 01  FILLER                   PIC X(16) VALUE 'DATKORT'.                  
021100 01  DATUMKORT-ID             PIC X(6)  VALUE '000001'.                   
021200*01  -COPY WDATKORT                                                       
021300     EJECT                                                                
021400*    ----  PARAMETRAR TILL WORKDAY                                        
021500                                                                          
021600*01  -COPY WORKAREA.                                                      
021700     EJECT                                                                
021800*                                                                         
021900 01  FILLER                      PIC X(8)    VALUE 'IMS-WS  '.            
022000                                                                          
022100 01  NYCKLAR-TILL-DLI.                                                    
022200     03  W-IDDC-B6-X.                                                     
022300         05 W-IDDC-B6                  PIC X(2).                          
022400                                                                          
022500 01  IMS-WS.                                                              
022600                                                                          
022700   03  STATUS-WS                 PIC X(2).                                
022800      88  SEGMENT-FINNS                      VALUE '  ' 'GA'              
022900                                                   'GK'.                  
023000      88  SEGMENT-SAKNAS                     VALUE 'GE'.                  
023100      88  SEGMENT-SLUT                       VALUE 'GB'.                  
023200                                                                          
023300   03 GODK-STATUSKODER.                                                   
023400      05 GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).              
023500                                                                          
023600   03 SSA1                       PIC X(64)  VALUE SPACE.                  
023700     EJECT                                                                
023800*01        -COPY W0003                                                    
023900     EJECT                                                                
024000 01  FILLER                      PIC X(16) VALUE 'POSTSUM'.               
024100                                                                          
024200*    -COPY W0005       -PRE POSTSUM-                                      
024300     EJECT                                                                
024400 01  FILLER                    PIC X(16)  VALUE 'UT1-AREA-START'.         
024500                                                                          
024600*01  AREA  -COPY W3714A   -PRE UT1-                                       
024700     EJECT                                                                
024800                                                                          
024900 01  FILLER                    PIC X(16)  VALUE 'UT2-AREA-START'.         
025000                                                                          
025100*01  AREA  -COPY W3714B   -PRE UT2-                                       
025200     EJECT                                                                
025300                                                                          
025400 01  FILLER                    PIC X(16)  VALUE 'UT3-AREA-START'.         
025500                                                                          
025600*01  AREA  -COPY W3714C   -PRE UT3-                                       
025700     EJECT                                                                
025800                                                                          
025900 01  FILLER                    PIC X(16)  VALUE 'UT4-AREA-START'.         
026000                                                                          
026100*01  AREA  -COPY W3714D   -PRE UT4-                                       
026200     EJECT                                                                
026300                                                                          
026400 01  FILLER                    PIC X(16)  VALUE 'UT5-AREA-START'.         
026500                                                                          
026600*01  AREA  -COPY W3714E   -PRE UT5-                                       
026700     EJECT                                                                
026800                                                                          
026900                                                                          
027000 01  FILLER                    PIC X(16)  VALUE 'UT6-AREA-START'.         
027100                                                                          
027200*01  AREA  -COPY W3714F   -PRE UT6-                                       
027300     EJECT                                                                
027400                                                                          
027500 01  FILLER                    PIC X(16)  VALUE 'UT7-AREA-START'.         
027600                                                                          
027700*01  AREA  -COPY W3714G   -PRE UT7-                                       
027800     EJECT                                                                
027900                                                                          
028000                                                                          
028100 01  FILLER                    PIC X(16)  VALUE 'UT8-AREA-START'.         
028200                                                                          
028300*01  AREA  -COPY W3714J   -PRE UT8-                                       
028400     EJECT                                                                
028500                                                                          
028600 01  FILLER                    PIC X(16)  VALUE 'UT9-AREA-START'.         
028700                                                                          
028800*01  AREA  -COPY W3714N   -PRE UT9-                                       
028900     EJECT                                                                
029000                                                                          
029100 01  FILLER                    PIC X(16)  VALUE 'UTA-AREA-START'.         
029200                                                                          
029300*01  AREA  -COPY W3714J   -PRE UTA-                                       
029400     EJECT                                                                
029410 01  FILLER                    PIC X(16)  VALUE 'UTB-AREA-START'.         
029420                                                                          
029430*01  AREA  -COPY W3714X   -PRE UTB-                                       
029440     EJECT                                                                
029500                                                                          
029600 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA'.          
029700     SKIP3                                                                
029800 01  DLI-IO-AREA.                                                         
029900     03  IO-AREA                 PIC X(150) VALUE SPACE.                  
030000     03  IO-M601                 REDEFINES IO-AREA.                       
030100*        05  -COPY WDM601                                                 
030200     03  IO-M611                 REDEFINES IO-AREA.                       
030300*        05  -COPY WDM611                                                 
030400     EJECT                                                                
030500                                                                          
030600 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
030700 01   DLI-IO-AREA-B601.                                                   
030800*     03  -COPY WDB601                                                    
030900                                                                          
031000 LINKAGE SECTION.                                                         
031100                                                                          
031200*01  -COPY W0008       -PRE BYTF-                                         
031300       05 FILLER                 PIC X(1).                                
031400                                                                          
031500*01  -COPY W0008       -PRE WDB6-                                         
031600       05 FILLER                 PIC X(1).                                
031700     EJECT                                                                
031800 PROCEDURE DIVISION USING BYTF-PCB WDB6-PCB.                              
031900     ENTRY 'DLITCBL' USING BYTF-PCB WDB6-PCB.                             
032000                                                                          
032100     PERFORM A-INIT                                                       
032200     PERFORM IMS-GET-WDM6                                                 
032300     IF BYTF-SEG-NAME-FB = 'WDM601'                                       
032400        MOVE ZERO             TO SPAR-IDBYTRAP2                           
032500        MOVE RAPP-IDDC        TO SPAR-IDDC                                
032501                                 W-IDDC                                   
032510        MOVE RAPP-IDUSER      TO SPAR-IDUSER                              
032700     END-IF                                                               
032800                                                                          
032900     PERFORM UNTIL SEGMENT-SLUT                                           
033000                                                                          
033100      EVALUATE BYTF-SEG-NAME-FB                                           
033200        WHEN  'WDM601'                                                    
033300            MOVE RAPP-IDDC           TO SPAR-IDDC                         
033400                                        W-IDDC                            
033410            MOVE RAPP-IDUSER         TO SPAR-IDUSER                       
033500                                                                          
033600            MOVE SPACE               TO  SPAR-FLBYGODK                    
033700            IF RAPP-FLBYGODK = 'N'                                        
033800               MOVE 'N'              TO  SPAR-FLBYGODK                    
033900               MOVE RAPP-FLBYTGAR    TO SPAR-FLBYTGAR                     
034000               MOVE RAPP-IDDISTR     TO SPAR-IDDISTR                      
034100               MOVE RAPP-IDBYTRAP    TO SPAR-IDBYTRAP                     
034200               MOVE RAPP-IDDC        TO SPAR-IDDC                         
034300                                        W-IDDC                            
035100               MOVE RAPP-IDKUNDNR    TO SPAR-IDKUNDNR                     
035200               MOVE RAPP-DAANKDAG    TO SPAR-TIANKDAG                     
035300               IF RAPP-DAREGDAT = 99999999                                
035400                MOVE RAPP-DAANKDAG  TO RAPP-DAREGDAT                      
035500               END-IF                                                     
035600               MOVE RAPP-DAREGDAT    TO SPAR-TIREGDAT                     
035700               MOVE RAPP-DAREGDAT-GODK                                    
035800                                     TO SPAR-TIREGDAT-GODK                
035900               MOVE RAPP-KDBYTSTA-RAPP TO SPAR-KDBYTSTA                   
036000               MOVE RAPP-KVRETUR-TOT  TO SPAR-KVRETUR                     
036100               PERFORM B-BEARBETA                                         
036200            END-IF                                                        
036300                                                                          
036400        WHEN  'WDM611'                                                    
036500                                                                          
036600               IF SPAR-FLBYGODK = 'N'                                     
036700                  PERFORM BA-BEARBETA                                     
036800               END-IF                                                     
036900                                                                          
037000       END-EVALUATE                                                       
037100                                                                          
037200       PERFORM IMS-GET-WDM6                                               
037300                                                                          
037400     END-PERFORM                                                          
037500                                                                          
037600     PERFORM C-AVSLUTA                                                    
037700                                                                          
037800     PERFORM Z-FINIT                                                      
037900                                                                          
038000     MOVE ZERO TO RETURN-CODE                                             
038100                                                                          
038200     GOBACK                                                               
038300     .                                                                    
038400     SKIP3                                                                
038500 A-INIT SECTION.                                                          
038600     MOVE 'A-INIT'      TO WS-SEKTION                                     
038700                                                                          
038800     OPEN OUTPUT W3714A                                                   
038900                 W3714B                                                   
039000                 W3714C                                                   
039100                 W3714D                                                   
039200                 W3714E                                                   
039300                 W3714F                                                   
039400                 W3714G                                                   
039500                 W3714J                                                   
039600                 W3714N                                                   
039700                 W3714R                                                   
039710                 W3714X                                                   
039800                                                                          
039900     MOVE IDPGM              TO POSTSUM-PROGNAMN                          
040000                                                                          
040100     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
040200     MOVE D-AAR      TO DAGENS-DATUM-AR                                   
040300     MOVE D-MAANAD   TO DAGENS-DATUM-MANAD                                
040400     MOVE D-DAG      TO DAGENS-DATUM-DAG                                  
040500                                                                          
040600     IF DAGENS-DATUM-AR  > 60                                             
040700        MOVE 19      TO DATUM-SEKEL                                       
040800                        DAGENS-DATUM-SEKEL                                
040900     ELSE                                                                 
041000        MOVE 20      TO DATUM-SEKEL                                       
041100                        DAGENS-DATUM-SEKEL                                
041200     END-IF                                                               
041300     MOVE DAGENS-DATUM-AR     TO DATUM-AR                                 
041400     MOVE DAGENS-DATUM-MANAD  TO DATUM-MANAD                              
041500     MOVE DAGENS-DATUM-DAG    TO DATUM-DAG                                
041600     MOVE DAGENS-DATUM        TO WS-DAGENS-DATUM                          
041700     DISPLAY 'DAGENS-DATUM ' WS-DAGENS-DATUM                              
041800                                                                          
041900                                                                          
042000     .                                                                    
042100     EJECT                                                                
042200                                                                          
042300 B-BEARBETA SECTION.                                                      
042400     MOVE 'B-BEARBETA'      TO WS-SEKTION                                 
042500                                                                          
042600     IF SPAR-KDBYTSTA = '2'                                               
042700******************************************************************        
042800*** NÄR STATUS = 2 DÅ ÄR BYTESRAPPORTERNA PÅ VÄG               ***        
042900******************************************************************        
043000        MOVE SPAR-IDDISTR     TO UT1-IDDISTR                              
043100        MOVE SPAR-IDBYTRAP    TO UT1-IDBYTRAP                             
043200        MOVE SPAR-IDDC        TO UT1-IDDC                                 
043300        MOVE SPAR-IDKUNDNR    TO UT1-IDKUNDNR                             
043400        MOVE SPAR-KVRETUR     TO UT1-KVRETUR                              
043500        MOVE SPAR-KDBYTSTA    TO UT1-KDBYTSTA                             
043600        MOVE SPAR-TIREGDAT    TO UT1-TIREGDAT                             
043700        PERFORM S01-SKRIV-W3714A                                          
043800     ELSE                                                                 
043900        IF SPAR-KDBYTSTA = '3'                                            
044000        OR SPAR-KDBYTSTA = '4'                                            
044100        OR SPAR-KDBYTSTA = '9'                                            
044200           IF SPAR-TIANKDAG = WS-DAGENS-DATUM                             
044300******************************************************************        
044400*** UNDERLAG TILL PERIODLISTAN SOM MÄTER TIDEN DET TAR FÖR EN  ***        
044500*** BYTESRAPPORT ATT GÅ FRÅN STATUS 2 TILL STATUS 3            ***        
044600******************************************************************        
044700              MOVE SPAR-IDDISTR     TO UT5-IDDISTR                        
044800              MOVE SPAR-IDDC        TO UT5-IDDC                           
044900                                       WS-IDDC-WORKDAY                    
045000                                                                          
045100              MOVE SPAR-IDKUNDNR    TO UT5-IDKUNDNR                       
045200              IF SPAR-TIREGDAT < 19970101                                 
045300                 MOVE 19970101        TO WORK-TIAAMMDD-FOM                
045400              ELSE                                                        
045500                 MOVE SPAR-TIREGDAT    TO WORK-TIAAMMDD-FOM               
045600              END-IF                                                      
045700              MOVE SPAR-TIANKDAG    TO WORK-TIAAMMDD-TOM                  
045800              PERFORM BD-BERAKNA-ANTAL-ARBDAGAR                           
045900              MOVE WORK-KVWORKD      TO UT5-KVARBDAG                      
046000              PERFORM S05-SKRIV-W3714E                                    
046100           END-IF                                                         
046200           IF SPAR-KDBYTSTA = '4'                                         
046300           OR SPAR-KDBYTSTA = '9'                                         
046400              IF SPAR-TIREGDAT-GODK = WS-DAGENS-DATUM                     
046500                 IF W-IDDC NOT = W-IDDC-B6                                
046600                    MOVE W-IDDC TO W-IDDC-B6                              
046700                    PERFORM IMS-GU-WDB601                                 
046800                 END-IF                                                   
046900                 IF DCS-CDC OR (DCS-SDC AND DCS-IDLANDX2 = 'NL')          
047000                    MOVE SPAR-IDDC        TO UT9-IDDC                     
047100                    MOVE SPAR-IDDISTR     TO UT9-IDDISTR                  
047200                    MOVE SPAR-IDBYTRAP    TO UT9-IDBYTRAP                 
047300                    PERFORM S09-SKRIV-W3714N                              
047400                 END-IF                                                   
047500              END-IF                                                      
047600           END-IF                                                         
047700        END-IF                                                            
047800     END-IF                                                               
047900     .                                                                    
048000     EJECT                                                                
048100                                                                          
048200 BA-BEARBETA SECTION.                                                     
048300     MOVE 'BA-BEARBETA'      TO WS-SEKTION                                
048400                                                                          
048500     IF SPAR-KDBYTSTA = '3'                                               
048600******************************************************************        
048700*** NÄR STATUS = 3 DÅ ÄR BYTESRAPPORTERNA MOTTAGNA             ***        
048800******************************************************************        
048900        PERFORM BB-SKAPA-FIL-STATUS-3                                     
049000     ELSE                                                                 
049100        IF SPAR-KDBYTSTA = '4'                                            
049200        OR SPAR-KDBYTSTA = '9'                                            
049300******************************************************************        
049400*** NÄR STATUS = 4 ELLER 9 DÅ ÄR BYTESRAPPORTERNA KLARA(GODKÄND)**        
049500******************************************************************        
049600           PERFORM BC-SKAPA-FIL-STATUS-4                                  
049700           IF SPAR-FLBYTGAR = 'J'                                         
049800              IF SPAR-TIREGDAT-GODK = WS-DAGENS-DATUM                     
049900                 IF OBJ-KDBYTREF = '210'                                  
050000                    PERFORM BE-SKAPA-GARANTI-RAPPORT                      
050100                 ELSE                                                     
050200                    PERFORM BF-SKAPA-GARANTI-RAPPORT                      
050300                 END-IF                                                   
050400              END-IF                                                      
050500           END-IF                                                         
050600        END-IF                                                            
050700     END-IF                                                               
050800                                                                          
050900     .                                                                    
051000     EJECT                                                                
051100                                                                          
051200 BB-SKAPA-FIL-STATUS-3 SECTION.                                           
051300     MOVE 'BB-SKAPA-FIL-STATUS-3'  TO WS-SEKTION                          
051400                                                                          
051500****************************************************************          
051600*** MAASTRICHT ÄR INTE INTRESERAD AV 019 (OBJEKT SAKNAS)     ***          
051700*** ELLER 220 ( GARANTI UTAN SALDO PÅVERKAN)                 ***          
051800****************************************************************          
051900     IF OBJ-KDBYTSTA-OBJ = ' '                                            
052000       MOVE SPAR-IDDC        TO UT2-IDDC                                  
052100       MOVE SPAR-IDDC        TO W-IDDC                                    
052200       MOVE SPAR-IDDISTR     TO UT2-IDDISTR                               
052300       MOVE SPAR-KDBYTSTA    TO UT2-KDBYTSTA                              
052400       MOVE SPAR-FLBYTGAR    TO UT2-FLBYTGAR                              
052500       MOVE OBJ-KVRETUR-URSP TO UT2-KVRETUR                               
053400       IF W-IDDC NOT = W-IDDC-B6                                          
053500         MOVE W-IDDC TO W-IDDC-B6                                         
053600         PERFORM IMS-GU-WDB601                                            
053700       END-IF                                                             
053800       IF DCS-SDC AND DCS-IDLANDX2 = 'NL'                                 
053900                                                                          
054000         IF OBJ-KDBYTREF  = '220' OR                                      
054100            OBJ-KDBYTREF  = '019'                                         
054200            CONTINUE                                                      
054300         ELSE                                                             
054500            PERFORM S02-SKRIV-W3714B                                      
054600         END-IF                                                           
054700       ELSE                                                               
054800         PERFORM S02-SKRIV-W3714B                                         
054900       END-IF                                                             
055400     END-IF                                                               
055500*                                                                         
055600* OBJEKTETS STATUS ' ', N, C, 4, E.                                       
055700* NY FIL TILLAGD 070917/EÖ                                                
055800                                                                          
055900     IF OBJ-KDBYTSTA-OBJ = 'C' AND                                        
056000          OBJ-KVRETUR-GODK NOT > 0                                        
056100          CONTINUE                                                        
056200     ELSE                                                                 
056300        IF SPAR-TIANKDAG = WS-DAGENS-DATUM                                
056400                                                                          
056500          IF W-IDDC NOT = W-IDDC-B6                                       
056600            MOVE W-IDDC TO W-IDDC-B6                                      
056700            PERFORM IMS-GU-WDB601                                         
056800          END-IF                                                          
056900                                                                          
057000          IF DCS-CDC OR (DCS-SDC AND DCS-IDLANDX2 ='NL')                  
057100            IF OBJ-KDBYTREF NOT = '220'                                   
057200              IF OBJ-KDBYTREF NOT = '019'                                 
057300                 MOVE SPAR-IDDISTR      TO UTA-IDDISTR                    
057400                 MOVE SPAR-IDDC         TO UTA-IDDC                       
057500                 IF OBJ-KDBYTSTA-OBJ = ' '                                
057600                   MOVE OBJ-KVRETUR-URSP  TO UTA-KVRETUR                  
057700                 ELSE                                                     
057800                   MOVE OBJ-KVRETUR-GODK  TO UTA-KVRETUR                  
057900                 END-IF                                                   
058000                 MOVE OBJ-IDARTNR-OBJ   TO UTA-IDARTNR                    
058100                 MOVE SPACE             TO UTA-BEART                      
058200                 MOVE ZERO              TO UTA-IDFKNGRP                   
058300                 PERFORM S0A-SKRIV-W3714R                                 
058400              END-IF                                                      
058500            END-IF                                                        
058600          END-IF                                                          
058700        END-IF                                                            
058800     END-IF                                                               
058900                                                                          
059000     .                                                                    
059100     EJECT                                                                
059200                                                                          
059300 BC-SKAPA-FIL-STATUS-4 SECTION.                                           
059400     MOVE 'BC-SKAPA-FIL-STATUS-4'  TO WS-SEKTION                          
059500                                                                          
059600     IF SPAR-TIREGDAT-GODK = WS-DAGENS-DATUM                              
059700        MOVE SPAR-IDDC        TO UT3-IDDC                                 
059800        MOVE SPAR-IDDISTR     TO UT3-IDDISTR                              
059900        MOVE SPAR-KDBYTSTA    TO UT3-KDBYTSTA                             
060000        MOVE OBJ-KVRETUR-GODK TO UT3-KVRETUR                              
060100        PERFORM S03-SKRIV-W3714C                                          
060200******************************************************************        
060300**** BERÄKNAR ANTAL DAGAR MELLAN ANKOMSTDAGEN OCH            *****        
060400**** SLUTREGISTRERINGSDAGEN                                  *****        
060500**** DVS TIDEN DET TAR FRÅN STATUS 3 TILL STATUS 4           *****        
060600******************************************************************        
060700        IF SPAR-IDBYTRAP2 = ZERO                                          
060800******************************************************************        
060900***** SPAR-IDBYTRAPP2 ÄR LIKA MED NOLL FÖRSTA GÅNGEN *************        
061000******************************************************************        
061100           MOVE ZERO             TO UT4-KVRETUR                           
061200           MOVE SPAR-IDBYTRAP    TO SPAR-IDBYTRAP2                        
061300           MOVE SPAR-IDDC        TO UT4-IDDC                              
061400                                    WS-IDDC2-WORKDAY                      
061500                                    WS-IDDC-WORKDAY                       
061600           MOVE SPAR-KDBYTSTA    TO UT4-KDBYTSTA                          
061700           MOVE SPAR-FLBYTGAR    TO UT4-FLBYTGAR                          
061800           MOVE SPAR-TIANKDAG    TO WS-TIANKDAG                           
061900           MOVE SPAR-TIREGDAT-GODK TO WS-TIREGDAT-GODK                    
062000           MOVE 'N'              TO POST-SKRIVEN                          
062100        END-IF                                                            
062200                                                                          
062300        IF SPAR-IDBYTRAP NOT = SPAR-IDBYTRAP2                             
062400           IF WS-TIANKDAG < 19970101                                      
062500              MOVE 19970101         TO WORK-TIAAMMDD-FOM                  
062600           ELSE                                                           
062700              MOVE WS-TIANKDAG      TO WORK-TIAAMMDD-FOM                  
062800           END-IF                                                         
062900           MOVE WS-TIREGDAT-GODK TO WORK-TIAAMMDD-TOM                     
063000           PERFORM BG-BERAKNA-ANTAL-ARBDAGAR                              
063100           MOVE WORK-KVWORKD      TO UT4-KVARBDAG                         
063200           PERFORM S04-SKRIV-W3714D                                       
063300           MOVE ZERO             TO UT4-KVRETUR                           
063400           MOVE 'N'              TO POST-SKRIVEN                          
063500           MOVE  SPAR-IDBYTRAP   TO SPAR-IDBYTRAP2                        
063600           ADD  OBJ-KVRETUR-GODK TO UT4-KVRETUR                           
063700           MOVE SPAR-IDDC        TO UT4-IDDC                              
063800                                    WS-IDDC2-WORKDAY                      
063900                                    WS-IDDC-WORKDAY                       
064000           MOVE SPAR-KDBYTSTA    TO UT4-KDBYTSTA                          
064100           MOVE SPAR-TIANKDAG    TO WS-TIANKDAG                           
064200           MOVE SPAR-TIREGDAT-GODK TO WS-TIREGDAT-GODK                    
064300        ELSE                                                              
064400           ADD  OBJ-KVRETUR-GODK TO UT4-KVRETUR                           
064500        END-IF                                                            
064600                                                                          
064700        IF DCS-CDC OR (DCS-SDC AND DCS-IDLANDX2 = 'NL')                   
064800           IF OBJ-KVRETUR-GODK > ZERO                                     
064900              IF OBJ-KDBYTREF NOT = '220'                                 
065000                IF OBJ-KDBYTREF NOT = '300'                               
065100                  IF OBJ-KDBYTREF NOT = '019'                             
065200                     MOVE SPAR-IDDISTR   TO UT8-IDDISTR                   
065300                     MOVE SPAR-IDDC      TO UT8-IDDC                      
065400                     MOVE OBJ-KVRETUR-GODK TO UT8-KVRETUR                 
065500                     MOVE OBJ-IDARTNR-OBJ TO UT8-IDARTNR                  
065600                     MOVE SPACE          TO UT8-BEART                     
065700                     MOVE ZERO           TO UT8-IDFKNGRP                  
065800                     PERFORM S08-SKRIV-W3714J                             
065900                  END-IF                                                  
066000                END-IF                                                    
066100              END-IF                                                      
066200           END-IF                                                         
066300        END-IF                                                            
066301                                                                          
066303        MOVE SPAR-IDDC                   TO WS-IDDC                       
066310        IF SDC-NL-ET                                                      
066312           MOVE OBJ-IDARTNR-OBJ          TO UTB-IDARTNR-OBJ               
066313           MOVE SPAR-IDDC                TO UTB-IDDC                      
066314           MOVE SPAR-IDDISTR             TO UTB-IDDISTR                   
066315           MOVE SPAR-IDKUNDNR            TO UTB-IDKUNDNR                  
066316           MOVE SPAR-IDBYTRAP            TO UTB-IDBYTRAP                  
066317           MOVE SPAR-TIREGDAT-GODK       TO UTB-DAREGDAT-GODK             
066318           MOVE OBJ-KVRETUR-GODK         TO UTB-KVRETUR-GODK              
066319           MOVE OBJ-KVRETUR-URSP         TO UTB-KVRETUR-URSP              
066320           MOVE OBJ-KDBYTREF             TO UTB-KDBYTREF                  
066321           MOVE OBJ-FLSKROT              TO UTB-FLSKROT                   
066322           MOVE SPAR-IDUSER              TO UTB-IDUSER                    
066323           PERFORM S11-SKRIV-W3714X                                       
066330        END-IF                                                            
066400     END-IF                                                               
066500                                                                          
066600     .                                                                    
066700     EJECT                                                                
066800                                                                          
066900 BD-BERAKNA-ANTAL-ARBDAGAR SECTION.                                       
067000     MOVE 'BD-BERAKNA-ANTAL-ARBDAGAR'  TO WS-SEKTION                      
067100                                                                          
067200     MOVE WS-IDDC-WORKDAY    TO WORK-IDDC                                 
067300     MOVE 001                TO WORK-KDCALL                               
067400     CALL WORKDAY  USING WORK-KDCALL                                      
067500                         WORK-DATE-AREA                                   
067600                         WORK-KDSVAR                                      
067700                                                                          
067800     IF WORK-KDSVAR-OK                                                    
067900        CONTINUE                                                          
068000     ELSE                                                                 
068100        DISPLAY '*** FEL I WORKDAY, PGM W37146'                           
068200        DISPLAY 'WORK-KVWORKD ' WORK-KVWORKD                              
068300        DISPLAY 'DAT FOM ' WORK-TIAAMMDD-FOM                              
068400        DISPLAY 'DAT TOM ' WORK-TIAAMMDD-TOM                              
068500        DISPLAY 'IDBYTRAPP ' RAPP-IDBYTRAP                                
068600        DISPLAY 'IDDISTR ' RAPP-IDDISTR                                   
068700        DISPLAY 'IDDC2 ' WS-IDDC2-WORKDAY                                 
068800        DISPLAY 'WS-IDDC-WORKDAY ' WS-IDDC-WORKDAY                        
068900        DISPLAY MEDDELANDE-1                                              
069000        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
069100        MOVE ZERO            TO WORK-KVWORKD                              
069200     END-IF                                                               
069300                                                                          
069400     .                                                                    
069500     EJECT                                                                
069600                                                                          
069700 BE-SKAPA-GARANTI-RAPPORT SECTION.                                        
069800     MOVE 'BE-SKAPA-GARANTI-RAPPORT'  TO WS-SEKTION                       
069900                                                                          
070000     MOVE SPAR-IDDC        TO UT6-IDDC                                    
070100     MOVE SPAR-IDDISTR     TO UT6-IDDISTR                                 
070200     MOVE SPAR-IDKUNDNR    TO UT6-IDKUNDNR                                
070300     MOVE SPAR-IDBYTRAP    TO UT6-IDBYTRAP                                
070400     MOVE SPAR-KDBYTSTA    TO UT6-KDBYTSTA                                
070500     MOVE OBJ-KVRETUR-GODK TO UT6-KVRETUR                                 
070600     MOVE OBJ-IDARTNR-OBJ  TO UT6-IDARTNR-OBJ                             
070700     PERFORM S06-SKRIV-W3714F                                             
070800                                                                          
070900                                                                          
071000     .                                                                    
071100     EJECT                                                                
071200                                                                          
071300 BF-SKAPA-GARANTI-RAPPORT SECTION.                                        
071400     MOVE 'BF-SKAPA-GARANTI-RAPPORT'  TO WS-SEKTION                       
071500                                                                          
071600     IF OBJ-KDBYTREF = '220'                                              
071700        MOVE SPAR-IDDC        TO UT7-IDDC                                 
071800        MOVE SPAR-IDDISTR     TO UT7-IDDISTR                              
071900        MOVE SPAR-IDKUNDNR    TO UT7-IDKUNDNR                             
072000        MOVE SPAR-IDBYTRAP    TO UT7-IDBYTRAP                             
072100        MOVE SPAR-KDBYTSTA    TO UT7-KDBYTSTA                             
072200        MOVE OBJ-KVRETUR-GODK TO UT7-KVRETUR                              
072300        PERFORM S07-SKRIV-W3714G                                          
072400     END-IF                                                               
072500                                                                          
072600     .                                                                    
072700     EJECT                                                                
072800                                                                          
072900 BG-BERAKNA-ANTAL-ARBDAGAR SECTION.                                       
073000     MOVE 'BG-BERAKNA-ANTAL-ARBDAGAR'  TO WS-SEKTION                      
073100                                                                          
073200     MOVE WS-IDDC2-WORKDAY   TO WORK-IDDC                                 
073300     MOVE 001                TO WORK-KDCALL                               
073400     CALL WORKDAY  USING WORK-KDCALL                                      
073500                         WORK-DATE-AREA                                   
073600                         WORK-KDSVAR                                      
073700                                                                          
073800     IF WORK-KDSVAR-OK                                                    
073900        CONTINUE                                                          
074000     ELSE                                                                 
074100        DISPLAY '*** FEL I WORKDAY, PGM W37146'                           
074200        DISPLAY 'WORK-KVWORKD ' WORK-KVWORKD                              
074300        DISPLAY 'DAT FOM ' WORK-TIAAMMDD-FOM                              
074400        DISPLAY 'DAT TOM ' WORK-TIAAMMDD-TOM                              
074500        DISPLAY 'IDBYTRAPP ' RAPP-IDBYTRAP                                
074600        DISPLAY 'IDDISTR ' RAPP-IDDISTR                                   
074700        DISPLAY 'IDDC2 ' WS-IDDC2-WORKDAY                                 
074800        DISPLAY 'WS-IDDC-WORKDAY ' WS-IDDC-WORKDAY                        
074900        DISPLAY MEDDELANDE-1                                              
075000        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
075100        MOVE ZERO            TO WORK-KVWORKD                              
075200     END-IF                                                               
075300                                                                          
075400     .                                                                    
075500     EJECT                                                                
075600                                                                          
075700 C-AVSLUTA SECTION.                                                       
075800     MOVE 'C-AVSLUTA'   TO WS-SEKTION                                     
075900                                                                          
076000     IF POST-SKRIVEN = 'N'                                                
076100        IF WS-TIANKDAG < 19970101                                         
076200          MOVE 19970101         TO WORK-TIAAMMDD-FOM                      
076300        ELSE                                                              
076400          MOVE WS-TIANKDAG      TO WORK-TIAAMMDD-FOM                      
076500        END-IF                                                            
076600        MOVE WS-TIREGDAT-GODK TO WORK-TIAAMMDD-TOM                        
076700        PERFORM BD-BERAKNA-ANTAL-ARBDAGAR                                 
076800        MOVE WORK-KVWORKD      TO UT4-KVARBDAG                            
076900        PERFORM S04-SKRIV-W3714D                                          
077000     END-IF                                                               
077100                                                                          
077200     .                                                                    
077300     EJECT                                                                
077400                                                                          
077500 Z-FINIT SECTION.                                                         
077600     MOVE 'Z-FINIT'     TO WS-SEKTION                                     
077700                                                                          
077800     CLOSE W3714A                                                         
077900           W3714B                                                         
078000           W3714C                                                         
078100           W3714D                                                         
078200           W3714E                                                         
078300           W3714F                                                         
078400           W3714G                                                         
078500           W3714J                                                         
078600           W3714N                                                         
078700           W3714R                                                         
078710           W3714X                                                         
078800                                                                          
078900     MOVE 'S' TO POSTSUM-OPKOD                                            
079000     CALL POSTSUM USING POSTSUM-PARM                                      
081200     .                                                                    
081300     SKIP2                                                                
081400 S01-SKRIV-W3714A SECTION.                                                
081500     MOVE 'S01-SKRIV-W3714A' TO WS-FIL-SEKTION                            
081600     WRITE UT1-POST FROM UT1-AREA                                         
081700                                                                          
081800     MOVE 'W3714A' TO POSTSUM-FDNAMN                                      
081900     MOVE 'W37146D1' TO POSTSUM-DDNAMN2                                   
082000     MOVE 'UT1 '       TO POSTSUM-TRANSTYP                                
082100     CALL POSTSUM USING POSTSUM-PARM                                      
082200     .                                                                    
082300     EJECT                                                                
082400 S02-SKRIV-W3714B SECTION.                                                
082500     MOVE 'S02-SKRIV-W3714B' TO WS-FIL-SEKTION                            
082600     WRITE UT2-POST FROM UT2-AREA                                         
082700                                                                          
082800     MOVE 'W3714B' TO POSTSUM-FDNAMN                                      
082900     MOVE 'W37146D2' TO POSTSUM-DDNAMN2                                   
083000     MOVE 'UT2 '       TO POSTSUM-TRANSTYP                                
083100     CALL POSTSUM USING POSTSUM-PARM                                      
083200     .                                                                    
083300     EJECT                                                                
083400 S03-SKRIV-W3714C SECTION.                                                
083500     MOVE 'S03-SKRIV-W3714C' TO WS-FIL-SEKTION                            
083600     WRITE UT3-POST FROM UT3-AREA                                         
083700                                                                          
083800     MOVE 'W3714C' TO POSTSUM-FDNAMN                                      
083900     MOVE 'W37146D3' TO POSTSUM-DDNAMN2                                   
084000     MOVE 'UT3 '       TO POSTSUM-TRANSTYP                                
084100     CALL POSTSUM USING POSTSUM-PARM                                      
084200     .                                                                    
084300     EJECT                                                                
084400 S04-SKRIV-W3714D SECTION.                                                
084500     MOVE 'S04-SKRIV-W3714D' TO WS-FIL-SEKTION                            
084600     WRITE UT4-POST FROM UT4-AREA                                         
084700                                                                          
084800     MOVE 'W3714D' TO POSTSUM-FDNAMN                                      
084900     MOVE 'W37146D4' TO POSTSUM-DDNAMN2                                   
085000     MOVE 'UT4 '       TO POSTSUM-TRANSTYP                                
085100     CALL POSTSUM USING POSTSUM-PARM                                      
085200     .                                                                    
085300     EJECT                                                                
085400 S05-SKRIV-W3714E SECTION.                                                
085500     MOVE 'S05-SKRIV-W3714E' TO WS-FIL-SEKTION                            
085600     WRITE UT5-POST FROM UT5-AREA                                         
085700                                                                          
085800     MOVE 'W3714E' TO POSTSUM-FDNAMN                                      
085900     MOVE 'W37146D5' TO POSTSUM-DDNAMN2                                   
086000     MOVE 'UT5 '       TO POSTSUM-TRANSTYP                                
086100     CALL POSTSUM USING POSTSUM-PARM                                      
086200     .                                                                    
086300     EJECT                                                                
086400 S06-SKRIV-W3714F SECTION.                                                
086500     MOVE 'S06-SKRIV-W3714F' TO WS-FIL-SEKTION                            
086600     WRITE UT6-POST FROM UT6-AREA                                         
086700                                                                          
086800     MOVE 'W3714F' TO POSTSUM-FDNAMN                                      
086900     MOVE 'W37146D6' TO POSTSUM-DDNAMN2                                   
087000     MOVE 'UT6 '       TO POSTSUM-TRANSTYP                                
087100     CALL POSTSUM USING POSTSUM-PARM                                      
087200     .                                                                    
087300     EJECT                                                                
087400 S07-SKRIV-W3714G SECTION.                                                
087500     MOVE 'S07-SKRIV-W3714G' TO WS-FIL-SEKTION                            
087600     WRITE UT7-POST FROM UT7-AREA                                         
087700                                                                          
087800     MOVE 'W3714G' TO POSTSUM-FDNAMN                                      
087900     MOVE 'W37146D7' TO POSTSUM-DDNAMN2                                   
088000     MOVE 'UT7 '       TO POSTSUM-TRANSTYP                                
088100     CALL POSTSUM USING POSTSUM-PARM                                      
088200     .                                                                    
088300     EJECT                                                                
088400 S08-SKRIV-W3714J SECTION.                                                
088500     MOVE 'S08-SKRIV-W3714J' TO WS-FIL-SEKTION                            
088600     WRITE UT8-POST FROM UT8-AREA                                         
088700                                                                          
088800     MOVE 'W3714J' TO POSTSUM-FDNAMN                                      
088900     MOVE 'W37146D8' TO POSTSUM-DDNAMN2                                   
089000     MOVE 'UT8 '       TO POSTSUM-TRANSTYP                                
089100     CALL POSTSUM USING POSTSUM-PARM                                      
089200     .                                                                    
089300     EJECT                                                                
089400 S09-SKRIV-W3714N SECTION.                                                
089500     MOVE 'S09-SKRIV-W3714N' TO WS-FIL-SEKTION                            
089600     WRITE UT9-POST FROM UT9-AREA                                         
089700                                                                          
089800     MOVE 'W3714N' TO POSTSUM-FDNAMN                                      
089900     MOVE 'W37146D9' TO POSTSUM-DDNAMN2                                   
090000     MOVE 'UT9 '       TO POSTSUM-TRANSTYP                                
090100     CALL POSTSUM USING POSTSUM-PARM                                      
090200     .                                                                    
090300     EJECT                                                                
090400 S0A-SKRIV-W3714R SECTION.                                                
090500     MOVE 'S0A-SKRIV-W3714R' TO WS-FIL-SEKTION                            
090600     WRITE UTA-POST FROM UTA-AREA                                         
090700                                                                          
090800     MOVE 'W3714R' TO POSTSUM-FDNAMN                                      
090900     MOVE 'W37146DA' TO POSTSUM-DDNAMN2                                   
091000     MOVE 'UTA '       TO POSTSUM-TRANSTYP                                
091100     CALL POSTSUM USING POSTSUM-PARM                                      
091200     .                                                                    
091300     EJECT                                                                
091310 S11-SKRIV-W3714X SECTION.                                                
091320     MOVE 'S11-WRITE-W3714X       ' TO WS-FIL-SEKTION                     
091330                                                                          
091340     WRITE UTB-POST FROM UTB-AREA                                         
091350                                                                          
091370     MOVE 'W3714X  '  TO POSTSUM-FDNAMN                                   
091380     MOVE 'W37146DB'  TO POSTSUM-DDNAMN2                                  
091381     MOVE 'UTX '       TO POSTSUM-TRANSTYP                                
091390     CALL POSTSUM  USING POSTSUM-PARM                                     
091391     .                                                                    
091392     EJECT                                                                
091400 IMS-GET-WDM6 SECTION.                                                    
091500     MOVE 'IMS-GET-WDM6' TO WS-IMS-SEKTION                                
091600                                                                          
091700     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
091800     CALL CBLTDLI USING GN BYTF-PCB DLI-IO-AREA                           
091900     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
092000     PERFORM IMS-STATUSKONTROLL                                           
095500     .                                                                    
095600     SKIP2                                                                
095700 IMS-GU-WDB601    SECTION.                                                
095800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
095900          DELIMITED BY SIZE INTO SSA1                                     
096000     MOVE '  GE' TO GODK-STATUSKODER                                      
096100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
096200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
096300     PERFORM IMS-STATUSKONTROLL                                           
096400     IF SEGMENT-SAKNAS                                                    
096500         MOVE SPACE TO DCS-KDDC                                           
096600     END-IF                                                               
096700     .                                                                    
096800                                                                          
096900 IMS-STATUSKONTROLL SECTION.                                              
097000                                                                          
097100     SET STATUS-IX TO 1                                                   
097200     SEARCH GODK-STATUS AT END CALL FELLOG                                
097300     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
097400     END-SEARCH                                                           
097500     .                                                                    
