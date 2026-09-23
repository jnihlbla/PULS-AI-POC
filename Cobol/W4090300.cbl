000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4090300.                                                
000400 AUTHOR.         FRANK THORBURN                                           
000500 DATE-WRITTEN.   SEPT 88.                                                 
000600                                                                          
000800*                                                                         
000900*    FUNKTION.                                                            
001000*    FRÅGEPROGRAM, RESTORDER-RELEASE 2.                                   
001100*                                                                         
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W4T903                                              
001500*        MID:         W4I90301                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         W4O90301                                            
001900     EJECT                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP3                                                                
002200 DATA DIVISION.                                                           
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002401                                                                          
002410*    -- CHECKED BY WY2000                                                 
002500 77   PROGRAM-NAMN           VALUE 'W4090300'                             
002600                                 PIC X(8).                                
002700 77  JA                          PIC X(1)   VALUE 'J'.                    
002800 77  NEJ                         PIC X(1)   VALUE 'N'.                    
002900 77  DAG-IX                      PIC S9(9)  VALUE +0   COMP SYNC.         
003000 77  RAD-IX                      PIC S9(9)  VALUE +0   COMP SYNC.         
003100 77  RAD-MAX                     PIC S9(9)  VALUE +14  COMP SYNC.         
003200 77  RAD-MAX-PLUS-1              PIC S9(9)  VALUE +15  COMP SYNC.         
003300 77  SPRAK-IX                    PIC S9(9)  VALUE +0   COMP SYNC.         
003400 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +1080 COMP SYNC.        
003500 77  WS-IDDISTR                  PIC X(4)   VALUE ZERO.                   
003600 77  WS-IDKUNDNR                 PIC X(6)   VALUE ZERO.                   
003700 77  WS-KDFRAKT                  PIC X(2)   VALUE ZERO.                   
003800 77  WS-KDORDKL                  PIC X(1)   VALUE ZERO.                   
003900 77  WS-IDTRANS                  PIC X(4).                                
004000     88  EGEN-BILD                          VALUE '4903'.                 
004100     88  WS-GODKAEND-BILD                   VALUE '4903' '4904'.          
004200 77  SW-NYCKLAR-OK               PIC X(1).                                
004300     88  NYCKLAR-OK                         VALUE 'J'.                    
004400                                                                          
004500     EJECT                                                                
004510                                                                          
004520 01  DYNAMISKA-SUBPROGRAM.                                                
004530     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004540     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004550                                                                          
004600 01  NYCKLAR-TILL-DLI.                                                    
004700                                                                          
004800     03  W-WDB401KEY-X.                                                   
004900       05  W-IDDISTR          PIC S9(5) COMP-3  VALUE ZERO.               
005000       05  W-IDKUNDNR         PIC S9(7) COMP-3  VALUE ZERO.               
005100                                                                          
005200     03  W-WDB414KEY-MIN-X.                                               
005300       05  W-KDFRAKT-MIN      PIC S9(3) COMP-3  VALUE ZERO.               
005400       05  W-KDORDKL-MIN      PIC S9(1) COMP-3  VALUE ZERO.               
005500                                                                          
005600     03  W-WDB414KEY-MAX-X.                                               
005700       05  W-KDFRAKT-MAX      PIC S9(3) COMP-3  VALUE ZERO.               
005800       05  W-KDORDKL-MAX      PIC S9(1) COMP-3  VALUE ZERO.               
005900                                                                          
006000     EJECT                                                                
006100 01  ARBETS-FALT.                                                         
006200     03  FRAKT-X              PIC X(3)          VALUE ZERO.               
006300     03  ARB-FRAKT-X REDEFINES FRAKT-X.                                   
006400       05  KDFRAKT-1          PIC X(1).                                   
006500       05  KDFRAKT-2          PIC X(2).                                   
006600     EJECT                                                                
006700*01  -COPY WWTEXT01.                                                      
006900                                                                          
007000     EJECT                                                                
007100 01    FILLER              PIC X(16)   VALUE 'MFS-WS'.                    
007200     SKIP3                                                                
007300 01    FILLER              PIC X(16)   VALUE 'MID W4I903 MID'.            
007400*01  MID -COPY W4I90301.                                                  
007600     EJECT                                                                
007700*01  -COPY WMSGAREA                                                       
007900     EJECT                                                                
008000*    03  MOD -COPY W4O90301  -RED MSG-AREA.                               
008200     EJECT                                                                
008300*01  -COPY WMFSAREA                                                       
008500     EJECT                                                                
008600 01  IMS-WS.                                                              
008700     03  FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
008800                                                                          
008900*                        **** STATUS-KOD FRÅN IMS                         
009000     03  STATUS-WS               PIC X(2).                                
009100         88  SEGMENT-FINNS                   VALUE '  '.                  
009200         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
009300                                                                          
009400     03  GODK-STATUSKODER.                                                
009500         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
009600                                                                          
009700 01  SSA1                        PIC X(64).                               
009800 01  SSA2                        PIC X(64).                               
009900     EJECT                                                                
010000*                            IMS FUNKTIONSKODER                           
010100*01  -COPY W0003                                                          
010300     EJECT                                                                
010400*                            DLI INPUT-OUTPUT AREA                        
010500 01  DLI-IO-AREA.                                                         
010600     03  IO-AREA                 PIC X(192)  VALUE SPACE.                 
010700                                                                          
010800*    03  WLKNDD01 -COPY WDB401   -RED IO-AREA.                            
011000                                                                          
011100     EJECT                                                                
011200*    03  WLKNDD14 -COPY WDB414   -RED IO-AREA.                            
011400                                                                          
011500     EJECT                                                                
011600 LINKAGE SECTION.                                                         
011700*01  -COPY W0009     -PRE MSG-                                            
011900     EJECT                                                                
012000*01  -COPY W0008     -PRE WDB4-                                           
012200     05  FILLER                  PIC X.                                   
012300     EJECT                                                                
012400 PROCEDURE DIVISION USING MSG-PCB WDB4-PCB.                               
012500     ENTRY 'DLITCBL' USING MSG-PCB WDB4-PCB.                              
012600                                                                          
012700     PERFORM IMS-GET-MSG                                                  
012800     IF SEGMENT-FINNS                                                     
012900       PERFORM A-INIT                                                     
013000       IF WS-GODKAEND-BILD                                                
013100         PERFORM B-KOLLA-NYCKLAR                                          
013200         IF NYCKLAR-OK                                                    
013300           IF MFS-IDPFK = '7'                                             
013400             PERFORM C-FLYTTA-FIRST-NYCKLAR                               
013500           ELSE                                                           
013600             IF MFS-IDPFK = '8'                                           
013700                 PERFORM D-FLYTTA-NEXT-NYCKLAR                            
013800             ELSE                                                         
013900                 PERFORM E-FLYTTA-SAME-NYCKLAR                            
014000             END-IF                                                       
014100           END-IF                                                         
014200           PERFORM F-HAEMTA-ORDERHUVUD-INFO                               
014300         ELSE                                                             
014400           MOVE TEXT-0401 (SPRAK-IX)       TO MOD-TEMFSFEL                
014500*          FEL NYCKEL                                                     
014600         END-IF                                                           
014700       ELSE                                                               
014800         PERFORM MFS-RENSA-NYCKLAR                                        
014900       END-IF                                                             
015000       PERFORM IMS-INSERT-MSG                                             
015100     END-IF                                                               
015200     MOVE ZERO TO RETURN-CODE                                             
015300     GOBACK                                                               
015400                                                                          
015500                                                                          
015600     .                                                                    
015700     EJECT                                                                
015800 A-INIT SECTION.                                                          
015900                                                                          
016000     IF MSG-DUBBLA-TRANSKODER                                             
016100         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I90301               
016200         MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                
016300         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                              
016400         MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                         
016500         MOVE MSG-IDPFK TO MFS-IDPFK                                      
016600     ELSE                                                                 
016700         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I90301                
016800         MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                
016900         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                              
017000         MOVE SPACE TO MFS-KDTRTYP        MFS-IDPFK                       
017100     END-IF                                                               
017200     MOVE MFS-IDTRANS TO WS-IDTRANS                                       
017300                                                                          
017400     MOVE LOW-VALUE      TO MSG-AREA                                      
017500     MOVE 'W4O90301'     TO MFS-IDMOD                                     
017600     MOVE '4903'         TO MOD-IDTRANS                                   
017700     MOVE MAX-MOD-LAENGD TO MSG-KVLL                                      
017800                                                                          
017900     PERFORM MFS-RENSA-BILD                                               
018000                                                                          
018100     MOVE LOW-VALUE                   TO W-WDB414KEY-MIN-X                
018200     MOVE HIGH-VALUE                  TO W-WDB414KEY-MAX-X                
018300                                                                          
018400     IF NOT EGEN-BILD                                                     
018500         MOVE SPACE TO MFS-KDTRTYP                                        
018600         MOVE '7' TO MFS-IDPFK                                            
018700     END-IF                                                               
018800                                                                          
018900     IF ENGLISH-TEXT                                                      
019000       MOVE +2                        TO SPRAK-IX                         
019100     ELSE                                                                 
019200       MOVE +1                        TO SPRAK-IX                         
019300     END-IF                                                               
019400                                                                          
019500                                                                          
019600     .                                                                    
019700     EJECT                                                                
019800 B-KOLLA-NYCKLAR SECTION.                                                 
019900                                                                          
020000     IF MID-IDDISTR-IN = ALL '+'                                          
020100       MOVE MID-IDDISTR-UT TO WS-IDDISTR                                  
020200       INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                 
020300     ELSE                                                                 
020400       MOVE MID-IDDISTR-IN TO WS-IDDISTR                                  
020500       MOVE '7'            TO MFS-IDPFK                                   
020600     END-IF                                                               
020700                                                                          
020800     MOVE WS-IDDISTR         TO MOD-IDDISTR-UT                            
020900     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
021000                                                                          
021100     IF MID-IDKUNDNR-IN = ALL '+'                                         
021200       MOVE MID-IDKUNDNR-UT TO WS-IDKUNDNR                                
021300       INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
021400     ELSE                                                                 
021500       MOVE MID-IDKUNDNR-IN TO WS-IDKUNDNR                                
021600       MOVE '7'             TO MFS-IDPFK                                  
021700     END-IF                                                               
021800                                                                          
021900     MOVE WS-IDKUNDNR        TO MOD-IDKUNDNR-UT                           
022000     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
022100                                                                          
022200     IF MID-KDFRAKT-IN = ALL '+'                                          
022300       MOVE MID-KDFRAKT-UT  TO WS-KDFRAKT                                 
022400       INSPECT WS-KDFRAKT REPLACING LEADING SPACE BY ZERO                 
022500     ELSE                                                                 
022600       MOVE MID-KDFRAKT-IN TO WS-KDFRAKT                                  
022700       MOVE '7'            TO MFS-IDPFK                                   
022800     END-IF                                                               
022900                                                                          
023000     MOVE WS-KDFRAKT TO MOD-KDFRAKT-UT                                    
023100     INSPECT MOD-KDFRAKT-UT REPLACING LEADING ZERO BY SPACE               
023200                                                                          
023300     IF MID-KDORDKL-IN = ALL '+'                                          
023400       MOVE MID-KDORDKL-UT  TO WS-KDORDKL                                 
023500       INSPECT WS-KDORDKL REPLACING LEADING SPACE BY ZERO                 
023600     ELSE                                                                 
023700       MOVE MID-KDORDKL-IN TO WS-KDORDKL                                  
023800       MOVE '7'            TO MFS-IDPFK                                   
023900     END-IF                                                               
024000                                                                          
024100     MOVE WS-KDORDKL TO MOD-KDORDKL-UT                                    
024200     INSPECT MOD-KDORDKL-UT REPLACING LEADING ZERO BY SPACE               
024300                                                                          
024400     PERFORM BA-KONTROLL-INPUT                                            
024500                                                                          
024600     EJECT                                                                
024700                                                                          
024800                                                                          
024900     .                                                                    
025000 BA-KONTROLL-INPUT SECTION.                                               
025100                                                                          
025200     IF (WS-IDDISTR NUMERIC AND WS-IDDISTR > ZERO) AND                    
025300         WS-IDKUNDNR               NUMERIC AND                            
025400         WS-KDFRAKT                NUMERIC AND                            
025500         WS-KDORDKL                NUMERIC                                
025600       MOVE JA                    TO SW-NYCKLAR-OK                        
025700     ELSE                                                                 
025800       MOVE NEJ                   TO SW-NYCKLAR-OK                        
025900     END-IF                                                               
026000                                                                          
026100                                                                          
026200     .                                                                    
026300     EJECT                                                                
026400 C-FLYTTA-FIRST-NYCKLAR SECTION.                                          
026500                                                                          
026600     IF WS-KDFRAKT > ZERO                                                 
026700       MOVE WS-KDFRAKT                   TO W-KDFRAKT-MIN                 
026800                                            W-KDFRAKT-MAX                 
026900     END-IF                                                               
027000     IF WS-KDORDKL > ZERO                                                 
027100       MOVE WS-KDORDKL                   TO W-KDORDKL-MIN                 
027200                                            W-KDORDKL-MAX                 
027300     END-IF                                                               
027400                                                                          
027500     .                                                                    
027600     EJECT                                                                
027700 D-FLYTTA-NEXT-NYCKLAR SECTION.                                           
027800                                                                          
027900     IF WS-KDFRAKT NUMERIC AND WS-KDFRAKT > ZERO                          
028000       MOVE WS-KDFRAKT                   TO W-KDFRAKT-MIN                 
028100                                            W-KDFRAKT-MAX                 
028200     ELSE                                                                 
028300       INSPECT MID-KDFRAKT-MAX REPLACING LEADING SPACE BY ZERO            
028400       IF MID-KDFRAKT-MAX NUMERIC                                         
028500         MOVE MID-KDFRAKT-MAX              TO W-KDFRAKT-MIN               
028600       END-IF                                                             
028700     END-IF                                                               
028800                                                                          
028900     IF WS-KDORDKL NUMERIC AND WS-KDORDKL > ZERO                          
029000       MOVE WS-KDORDKL                   TO W-KDORDKL-MIN                 
029100                                            W-KDORDKL-MAX                 
029200     ELSE                                                                 
029300       INSPECT MID-KDORDKL-MAX REPLACING LEADING SPACE BY ZERO            
029400       IF MID-KDORDKL-MAX NUMERIC                                         
029500         MOVE MID-KDORDKL-MAX              TO W-KDORDKL-MIN               
029600       END-IF                                                             
029700     END-IF                                                               
029800                                                                          
029900     .                                                                    
030000     EJECT                                                                
030100 E-FLYTTA-SAME-NYCKLAR SECTION.                                           
030200                                                                          
030300     IF WS-KDFRAKT NUMERIC AND WS-KDFRAKT > ZERO                          
030400       MOVE WS-KDFRAKT                   TO W-KDFRAKT-MIN                 
030500                                            W-KDFRAKT-MAX                 
030600     ELSE                                                                 
030700       INSPECT MID-KDFRAKT-MIN REPLACING LEADING SPACE BY ZERO            
030800       IF MID-KDFRAKT-MIN NUMERIC                                         
030900         MOVE MID-KDFRAKT-MIN              TO W-KDFRAKT-MIN               
031000       END-IF                                                             
031100     END-IF                                                               
031200     IF WS-KDORDKL NUMERIC AND WS-KDORDKL > ZERO                          
031300       MOVE WS-KDORDKL                     TO W-KDORDKL-MIN               
031400                                              W-KDORDKL-MAX               
031500     ELSE                                                                 
031600       INSPECT MID-KDORDKL-MIN REPLACING LEADING SPACE BY ZERO            
031700       IF MID-KDORDKL-MIN NUMERIC                                         
031800         MOVE MID-KDORDKL-MIN              TO W-KDORDKL-MIN               
031900       END-IF                                                             
032000     END-IF                                                               
032100                                                                          
032200     .                                                                    
032300     EJECT                                                                
032400 F-HAEMTA-ORDERHUVUD-INFO SECTION.                                        
032500                                                                          
032600     MOVE MFS-RENSA-FAELT          TO MOD-KDFRAKT-MIN                     
032700                                      MOD-KDORDKL-MIN                     
032800                                      MOD-KDFRAKT-MAX                     
032900                                      MOD-KDORDKL-MAX                     
033000     MOVE WS-IDDISTR               TO W-IDDISTR                           
033100     MOVE WS-IDKUNDNR              TO W-IDKUNDNR                          
033200     PERFORM IMS-LAS-GU-WDB401                                            
033300     IF SEGMENT-FINNS                                                     
033400       PERFORM IMS-LAS-GNP-WDB414                                         
033500       IF SEGMENT-FINNS                                                   
033600         MOVE ORD-KDFRAKT              TO FRAKT-X                         
033700         MOVE KDFRAKT-2                TO MOD-KDFRAKT-MIN                 
033800         MOVE ORD-KDORDKL              TO MOD-KDORDKL-MIN                 
033900         MOVE +1                       TO RAD-IX                          
034000         PERFORM UNTIL SEGMENT-SAKNAS OR RAD-IX > RAD-MAX                 
034100           PERFORM FA-LAGG-UT-RAD                                         
034200           ADD +1                      TO RAD-IX                          
034300           PERFORM IMS-LAS-GNP-WDB414                                     
034400         END-PERFORM                                                      
034500         IF SEGMENT-FINNS                                                 
034600           MOVE ZERO                   TO FRAKT-X                         
034700           MOVE ORD-KDFRAKT            TO FRAKT-X                         
034800           MOVE KDFRAKT-2              TO MOD-KDFRAKT-MAX                 
034900           MOVE ORD-KDORDKL            TO MOD-KDORDKL-MAX                 
035000           MOVE TEXT-0402 (SPRAK-IX)   TO MOD-TEMFSINF                    
035100*          FLER RADER FINNS                                               
035200         END-IF                                                           
035300       ELSE                                                               
035400         MOVE TEXT-0413 (SPRAK-IX)     TO MOD-TEMFSFEL                    
035500*        INFORMATION SAKNAS                                               
035600       END-IF                                                             
035700     ELSE                                                                 
035800       MOVE TEXT-0417 (SPRAK-IX)     TO MOD-TEMFSFEL                      
035900*      ORDERHUVUD SAKNAS                                                  
036000     END-IF                                                               
036100                                                                          
036200                                                                          
036300     EJECT                                                                
036400                                                                          
036500     .                                                                    
036600 FA-LAGG-UT-RAD SECTION.                                                  
036700                                                                          
036800     MOVE WS-IDKUNDNR              TO MOD-IDKUNDNR (RAD-IX)               
036900     INSPECT MOD-IDKUNDNR (RAD-IX)                                        
037000             REPLACING LEADING ZERO BY SPACE                              
037100     MOVE ORD-KDFRAKT              TO MOD-KDFRAKT (RAD-IX)                
037200     MOVE ORD-KDORDKL              TO MOD-KDORDKL (RAD-IX)                
037300     MOVE ORD-KDROPACK (2)         TO MOD-KDROPACK (RAD-IX)               
037400     MOVE ORD-TISTADAT             TO MOD-TISTADAT (RAD-IX)               
037500                                                                          
037600     PERFORM VARYING DAG-IX FROM 1 BY 1 UNTIL DAG-IX > 10                 
037700       IF ORD-TID (DAG-IX) > ZERO                                         
037800         EVALUATE DAG-IX                                                  
037900           WHEN 1 MOVE '1'         TO MOD-TID-EVEN (RAD-IX, 1)            
038000           WHEN 2 MOVE '2'         TO MOD-TID-EVEN (RAD-IX, 2)            
038100           WHEN 3 MOVE '3'         TO MOD-TID-EVEN (RAD-IX, 3)            
038200           WHEN 4 MOVE '4'         TO MOD-TID-EVEN (RAD-IX, 4)            
038300           WHEN 5 MOVE '5'         TO MOD-TID-EVEN (RAD-IX, 5)            
038400           WHEN 6 MOVE '1'         TO MOD-TID-ODD (RAD-IX, 1)             
038500           WHEN 7 MOVE '2'         TO MOD-TID-ODD (RAD-IX, 2)             
038600           WHEN 8 MOVE '3'         TO MOD-TID-ODD (RAD-IX, 3)             
038700           WHEN 9 MOVE '4'         TO MOD-TID-ODD (RAD-IX, 4)             
038800           WHEN 10 MOVE '5'        TO MOD-TID-ODD (RAD-IX, 5)             
038900         END-EVALUATE                                                     
039000       END-IF                                                             
039100     END-PERFORM                                                          
039200                                                                          
039300                                                                          
039400     .                                                                    
039500     EJECT                                                                
039600 MFS-RENSA-NYCKLAR SECTION.                                               
039700                                                                          
039800     MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR-UT                         
039900                                   MOD-IDKUNDNR-UT                        
040000                                   MOD-KDFRAKT-UT                         
040100                                   MOD-KDORDKL-UT                         
040200                                                                          
040300                                                                          
040400                                                                          
040500     .                                                                    
040600 MFS-RENSA-BILD SECTION.                                                  
040700                                                                          
040800     MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR-IN                         
040900                                   MOD-IDKUNDNR-IN                        
041000                                   MOD-KDFRAKT-IN                         
041100                                   MOD-KDORDKL-IN                         
041200                                   MOD-TEMFSFEL                           
041300                                   MOD-TEMFSINF                           
041400     PERFORM VARYING RAD-IX FROM 1 BY 1 UNTIL RAD-IX > RAD-MAX            
041500       MOVE MFS-RENSA-FAELT     TO MOD-RAD-INFO (RAD-IX)                  
041600     END-PERFORM                                                          
041700                                                                          
041800     EJECT                                                                
041900                                                                          
042000                                                                          
042100                                                                          
042200     .                                                                    
042300 IMS-GET-MSG SECTION.                                                     
042400                                                                          
042500     MOVE '  QC' TO GODK-STATUSKODER                                      
042600     CALL CBLTDLI USING GU                                                
042700                          MSG-PCB                                         
042800                          MSG-IO-AREA                                     
042900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
043000     PERFORM IMS-STATUSKONTROLL                                           
043100                                                                          
043200                                                                          
043300                                                                          
043400     .                                                                    
043500 IMS-INSERT-MSG SECTION.                                                  
043600                                                                          
043700     IF ENGLISH-TEXT                                                      
043800       MOVE 'N' TO MFS-KDHUVOMR                                           
043900     END-IF                                                               
044000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
044100     MOVE SPACE TO GODK-STATUSKODER                                       
044200     CALL CBLTDLI USING ISRT                                              
044300                          MSG-PCB                                         
044400                          MSG-IO-AREA                                     
044500                          MFS-IDMOD                                       
044600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
044700     PERFORM IMS-STATUSKONTROLL                                           
044800                                                                          
044900     EJECT                                                                
045000                                                                          
045100     .                                                                    
045200 IMS-LAS-GU-WDB401 SECTION.                                               
045300                                                                          
045400     STRING 'WDB401  (WDB401KY =' W-WDB401KEY-X ')'                       
045500            DELIMITED BY SIZE INTO SSA1                                   
045600     MOVE '  GE' TO GODK-STATUSKODER                                      
045700     CALL CBLTDLI USING GU WDB4-PCB DLI-IO-AREA SSA1                      
045800     MOVE WDB4-STATUS-CODE TO STATUS-WS                                   
045900     PERFORM IMS-STATUSKONTROLL                                           
046000                                                                          
046100     .                                                                    
046200 IMS-LAS-GNP-WDB414 SECTION.                                              
046300                                                                          
046400     STRING 'WDB414  (WDB414KY=>' W-WDB414KEY-MIN-X                       
046500                    '&WDB414KY=<' W-WDB414KEY-MAX-X ')'                   
046600            DELIMITED BY SIZE INTO SSA1                                   
046700     MOVE '  GE' TO GODK-STATUSKODER                                      
046800     CALL CBLTDLI USING GNP WDB4-PCB DLI-IO-AREA SSA1                     
046900     MOVE WDB4-STATUS-CODE TO STATUS-WS                                   
047000     PERFORM IMS-STATUSKONTROLL                                           
047100                                                                          
047200     .                                                                    
047300 IMS-STATUSKONTROLL SECTION.                                              
047400                                                                          
047500     SET STATUS-IX TO 1                                                   
047600     SEARCH GODK-STATUS                                                   
047610       AT END                                                             
047620         CALL FELLOG                                                      
047700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
047800     END-SEARCH                                                           
047900                                                                          
048000     .                                                                    
