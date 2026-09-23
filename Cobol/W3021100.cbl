000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W3021100.                                                
000400 AUTHOR.         RONNY STENHOLM.                                          
000500 DATE-WRITTEN.   AUG   89.                                                
000600                                                                          
000700     REMARKS.                                                             
000800                                                                          
000900*    FUNKTION.                                                            
001000*        REGISTRERA JUSTERING I ARTIKELSYSTEMET.                          
001100*        OM SJÄLVKOSTNAD ANGES SKER JUSTERINGEN WORLD-WIDE                
001200*        PÅ DEN ARTIKEL/VECKA SOM ANGIVITS. ÄR DET EJ SJÄLV-              
001300*        KOSTNAD SOM ANGIVITS SÅ KRÄVS FÖLJANDE FÄLT:                     
001400*                          ARTIKELNUMMER                                  
001500*                          DISTRIKT                                       
001600*                          VECKA                                          
001700*                          PRIS OCH/ELLER KVANT                           
001800*        IDUSER REGISTRERAS AV PROGRAMMET.                                
001900*                                                                         
002000*        COBOL II                                                         
002100*                                                                         
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: W3T211                                              
002500*        MID:         W3I21101                                            
002600*                                                                         
002700*    UTDATA.                                                              
002800*        MOD:         W3O21101                                            
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP3                                                                
003200 DATA DIVISION.                                                           
003300     EJECT                                                                
003400*                                                                         
003500****************************************************************          
003600*         WORKING-STORAGE SECTION                                         
003700****************************************************************          
003800*                                                                         
003900 WORKING-STORAGE SECTION.                                                 
004000*                                                                         
004010*    -- CHECKED BY WY2000                                                 
004020     SKIP3                                                                
004100 01  FILLER                  PIC X(16)   VALUE '77-OR'.                   
004200*                                                                         
004300 77  PROGRAM-NAMN            PIC X(8)    VALUE 'W3021100'.                
004400 77  JA                      PIC X       VALUE 'J'.                       
004500 77  NEJ                     PIC X       VALUE 'N'.                       
004600 77  INMATNINGS-FEL          PIC X       VALUE 'N'.                       
004700 77  UPPDATERING-OENSKAS     PIC X       VALUE 'N'.                       
004800 77  NY-JUST                 PIC X       VALUE 'N'.                       
004900 77  BORTTAG                 PIC X       VALUE 'B'.                       
005000 77  PFK8                    PIC X       VALUE '8'.                       
005100 77  PFK7                    PIC X       VALUE '7'.                       
005200 77  PFK11                   PIC X       VALUE 'U'.                       
005300 77  RADIX                   PIC S9(3)   VALUE +1 COMP-3.                 
005400 77  MAX-MOD-LAENGD          PIC S9(4)   VALUE +699 COMP SYNC.            
005500 77  WS-IDTRANS              PIC X(04).                                   
005600     88 EGEN-BILD                        VALUE '3211'.                    
005700 01  WS-PRARTNTO             PIC S9(7)V9(2).                              
005800 01  WS-PRARTSJK             PIC S9(7)V9(2).                              
005900 01  WS-DAFSGVV              PIC 9(6).                                    
006000 01  WS-KVLEVART             PIC X(7)    VALUE SPACE.                     
006100 01  FILLER         REDEFINES WS-KVLEVART.                                
006200     03  KEY-KVLEVART             PIC S9(7).                              
006300 01  WS-AAAAVVD.                                                          
006310     03 WS-SEKEL             PIC 9(2).                                    
006400     03 WS-AAVVD             PIC 9(5).                                    
006411 01  FILLER REDEFINES WS-AAAAVVD.                                         
006420     03 WS-AAAAVV            PIC 9(6).                                    
006500     03 WS-DAG               PIC 9.                                       
006600   EJECT                                                                  
006700*                                                                         
006800******************************************************************        
006900*          SUBPROGRAM                                                     
007000******************************************************************        
007100*                                                                         
007200 01  FILLER                  PIC X(16)   VALUE 'SUBPGM'.                  
007300*                                                                         
007400 01  SUBPGM.                                                              
007500   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
007600   03  FELLOG                PIC X(8)    VALUE 'FELLOG '.                 
007700   03  WDECEDIT              PIC X(8)    VALUE 'WDECEDIT'.                
007800   03  WDATKONV              PIC X(8)    VALUE 'WDATKONV'.                
007900   03  WMEDKONV              PIC X(8)    VALUE 'WMEDKONV'.                
008000   EJECT                                                                  
008100*                                                                         
008200******************************************************************        
008300*          NYCKLAR TILL DLI                                               
008400******************************************************************        
008500*                                                                         
008600 01    FILLER                  PIC X(16) VALUE 'DLI-NYCKLAR'.             
008800 01    NYCKLAR-TILL-DLI.                                                  
009000   03   W-3131-X.                                                         
009100     05    W-HTYP              PIC X(4)  VALUE '3131'.                    
009200     05    FILLER              PIC X(26) VALUE LOW-VALUE.                 
009300     SKIP3                                                                
009500   03   W-KY3132-X.                                                       
009600     05    W-IDARTNR           PIC S9(9) VALUE ZERO  COMP-3.              
009700     05    W-IDDISTR           PIC S9(5) VALUE ZERO  COMP-3.              
009800     05    W-DAFSGVV           PIC 9(6)  VALUE ZERO.                      
010100   EJECT                                                                  
010200*                                                                         
010300******************************************************************        
010400*            FELMEDDELANDEMODUL                                           
010500******************************************************************        
010600*                                                                         
010700 01   FILLER                 PIC X(16)   VALUE 'WMEDAREA'.                
010800*01  -COPY WMEDAREA.                                                      
011000     EJECT                                                                
011100*                                                                         
011200******************************************************************        
011300*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
011400******************************************************************        
011500*                                                                         
011600 01    FILLER                 PIC X(16)   VALUE 'MIDCOPYTEXT'.            
011700     SKIP3                                                                
011800*01    MID -COPY W3I21101.                                                
012000     EJECT                                                                
012100 01    FILLER                 PIC X(16)   VALUE 'MSG-IO-AREA'.            
012200*01    -COPY WMSGAREA                                                     
012400     EJECT                                                                
012500*  03    MOD -COPY W3O21101  -RED MSG-AREA.                               
012700     EJECT                                                                
012800*01    -COPY WMFSAREA                                                     
013000     EJECT                                                                
013100*01    -COPY WDECAREA                                                     
013300     EJECT                                                                
013400*01    -COPY WDATAREAC0                                                   
013600     EJECT                                                                
013700******************************************************************        
013800                                                                          
013900*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014000******************************************************************        
014100*                                                                         
014200 01    IMS-WS.                                                            
014300   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
014400     SKIP3                                                                
014500*                        **** STATUS-KOD FRÅN IMS                         
014600   03    STATUS-WS               PIC XX.                                  
014700     88    SEGMENT-FINNS                     VALUE '  '.                  
014800     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
014900     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
015000     SKIP3                                                                
015100   03    GODK-STATUSKODER.                                                
015200     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
015300     SKIP3                                                                
015400 01    SSA1                      PIC X(64).                               
015500 01    SSA2                      PIC X(64).                               
015600     EJECT                                                                
015700*                            IMS FUNKTIONSKODER                           
015800*01    -COPY W0003                                                        
016000     EJECT                                                                
016100*                            DLI INPUT-OUTPUT AREA                        
016200 01    DLI-IO-AREA.                                                       
016500*  03  WL313111 -COPY WDGX3132                                            
016700     EJECT                                                                
016800 LINKAGE SECTION.                                                         
016900*01    -COPY W0009     -PRE MSG-                                          
017100     EJECT                                                                
017200*01    -COPY W0008     -PRE 3131-                                         
017400     05  FILLER                  PIC X.                                   
017500     EJECT                                                                
017600 PROCEDURE DIVISION USING MSG-PCB 3131-PCB.                               
017700     ENTRY 'DLITCBL' USING MSG-PCB 3131-PCB.                              
017800 STYR SECTION.                                                            
017900     PERFORM IMS-GET-MSG                                                  
018000     IF SEGMENT-FINNS                                                     
018100       PERFORM A-INIT-SPARA-INPUT                                         
018200       IF EGEN-BILD                                                       
018300         IF MFS-KDTRTYP        = PFK11                                    
018400           PERFORM B-INDATA-KOLL                                          
018500           IF INMATNINGS-FEL = NEJ                                        
018600             MOVE MID-IDARTNR TO  W-IDARTNR                               
018700             MOVE MID-IDDISTR TO  W-IDDISTR                               
018800             MOVE MID-TIFSGVV TO  W-DAFSGVV                               
018810             IF MID-TIFSGVV NOT = ZERO                                    
018820               IF MID-TIFSGVV < 5000                                      
018830                 MOVE 20      TO  W-DAFSGVV (1:2)                         
018840               ELSE                                                       
018850                 IF MID-TIFSGVV < 9999                                    
018860                   MOVE 19    TO  W-DAFSGVV (1:2)                         
018870                 ELSE                                                     
018880                   MOVE 999999 TO  W-DAFSGVV                              
018890                 END-IF                                                   
018891               END-IF                                                     
018892             END-IF                                                       
018900             IF MID-KDSVAR = NY-JUST                                      
019000               PERFORM C-SKAPA-NYTT-SEG                                   
019100             ELSE                                                         
019200               IF MID-KDSVAR = BORTTAG                                    
019300                 PERFORM D-TAG-BORT-SEG                                   
019400               END-IF                                                     
019500             END-IF                                                       
019600           ELSE                                                           
019700             MOVE '001' TO MED-IDMFSFEL                                   
019800             CALL WMEDKONV USING MED-WMEDAREA                             
019900             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
020000             PERFORM S02-MFS-ROER-EJ-FAELT                                
020100           END-IF                                                         
020200         ELSE                                                             
020300           PERFORM E-KOLLA-OM-UPPDAT-OENSKAS                              
020400           IF UPPDATERING-OENSKAS = JA                                    
020500             MOVE '003' TO MED-IDMFSFEL                                   
020600             CALL WMEDKONV USING MED-WMEDAREA                             
020700             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
020800             PERFORM S06-MFS-LAES-IN-FAELT                                
020900             PERFORM S02-MFS-ROER-EJ-FAELT                                
021000             PERFORM H-VISA-SAMMA-SIDA                                    
021100           ELSE                                                           
021200             IF MFS-IDPFK = PFK8                                          
021300               PERFORM F-VISA-NAESTA-SIDA                                 
021400             ELSE                                                         
021500               IF MFS-IDPFK = PFK7                                        
021600                 PERFORM G-VISA-FOERSTA-SIDA                              
021700               ELSE                                                       
021800                 PERFORM H-VISA-SAMMA-SIDA                                
021900               END-IF                                                     
022000             END-IF                                                       
022100           END-IF                                                         
022200         END-IF                                                           
022300       ELSE                                                               
022400         PERFORM G-VISA-FOERSTA-SIDA                                      
022500       END-IF                                                             
022600       IF INMATNINGS-FEL = JA                                             
022700         PERFORM S02-MFS-ROER-EJ-FAELT                                    
022800         PERFORM H-VISA-SAMMA-SIDA                                        
022900         MOVE SPACE TO MOD-TEMFSINF                                       
023000       END-IF                                                             
023100       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
023200       PERFORM IMS-INSERT-MSG                                             
023300     END-IF                                                               
023400     MOVE ZERO TO RETURN-CODE                                             
023500     GOBACK.                                                              
023600     EJECT                                                                
023700                                                                          
023800 A-INIT-SPARA-INPUT SECTION.                                              
023900     IF MSG-DUBBLA-TRANSKODER                                             
024000        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I21101                
024100        MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                 
024200        MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                               
024300        MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                          
024400        MOVE MSG-IDPFK TO MFS-IDPFK                                       
024500     ELSE                                                                 
024600        MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W3I21101                 
024700        MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                 
024800        MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                               
024900        MOVE ' ' TO MFS-KDTRTYP                                           
025000        MOVE ' ' TO MFS-IDPFK                                             
025100     END-IF                                                               
025200                                                                          
025300     MOVE MFS-IDTRANS TO WS-IDTRANS                                       
025400     MOVE LOW-VALUE TO MSG-AREA                                           
025500     MOVE 'W3O211N1' TO MFS-IDMOD                                         
025600     MOVE '3211' TO MOD-IDTRANS                                           
025700                                                                          
025800*    IF SWEDISH-TEXT                                                      
025900*       MOVE 1                       TO SPAR-TEXT-IND                     
026000*    ELSE                                                                 
026100*       MOVE 2                       TO SPAR-TEXT-IND                     
026200*    END-IF.                                                              
026300     .                                                                    
026400    EJECT                                                                 
026500                                                                          
026600                                                                          
026700                                                                          
026800 B-INDATA-KOLL SECTION.                                                   
026900     IF MID-KDSVAR = BORTTAG OR NY-JUST                                   
027000       MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSVAR-ATTR                       
027100     ELSE                                                                 
027200       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSVAR-ATTR                         
027300       MOVE JA TO INMATNINGS-FEL                                          
027400     END-IF                                                               
027500     IF MID-IDARTNR NUMERIC                                               
027600       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDARTNR-ATTR                      
027700     ELSE                                                                 
027800       MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-ATTR                        
027900       MOVE JA TO INMATNINGS-FEL                                          
028000     END-IF                                                               
028100     IF MID-IDDISTR NUMERIC                                               
028200       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDDISTR-ATTR                      
028300     ELSE                                                                 
028400       IF MID-IDDISTR = ALL '+'                                           
028500         MOVE '0' TO MID-IDDISTR                                          
028600       ELSE                                                               
028700         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDISTR-ATTR                      
028800         MOVE JA TO INMATNINGS-FEL                                        
028900       END-IF                                                             
029000     END-IF                                                               
029100     IF MID-TIFSGVV NOT = ALL '+'                                         
029200       MOVE 'AAVV  ' TO DAT-KDDATFORM                                     
029300       MOVE MID-TIFSGVV TO DAT-I-TIDATUM                                  
029400       CALL WDATKONV USING DAT-KDDATFORM,                                 
029500                           DAT-I-TIDATUM,                                 
029600                           DAT-O-TIDATUM,                                 
029700                           DAT-KDSVAR                                     
029800       IF DAT-KDSVAR-OK                                                   
029900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-TIFSGVV-ATTR                    
030000       ELSE                                                               
030100         MOVE MFS-ALFA-FAELT-FEL TO MOD-TIFSGVV-ATTR                      
030200         MOVE JA TO INMATNINGS-FEL                                        
030300       END-IF                                                             
030400     END-IF                                                               
030500     IF MID-KDSVAR NOT = BORTTAG                                          
030600       IF MID-PRARTNTO = ALL '+'                                          
030700         CONTINUE                                                         
030800       ELSE                                                               
030900         INSPECT MID-PRARTNTO REPLACING LEADING ZEROS BY SPACE            
031000         MOVE MID-PRARTNTO TO DEC-IDFRIDATA                               
031100         PERFORM S05-OMVANDLA-ALFA-TILL-NUM                               
031200         IF DEC-KDSVAR-OK                                                 
031300           MOVE DEC-IDEDITDATA TO WS-PRARTNTO                             
031400           MOVE MFS-ALFA-FAELT-RAETT TO MOD-PRARTNTO-ATTR                 
031500         ELSE                                                             
031600           MOVE MFS-ALFA-FAELT-FEL TO MOD-PRARTNTO-ATTR                   
031700           MOVE JA TO INMATNINGS-FEL                                      
031800         END-IF                                                           
031900       END-IF                                                             
032000       IF MID-KVLEVART = ALL '+'                                          
032100         CONTINUE                                                         
032200       ELSE                                                               
032300         INSPECT MID-KVLEVART REPLACING LEADING ZEROS BY SPACE            
032400         MOVE MID-KVLEVART TO DEC-IDFRIDATA                               
032500         PERFORM S09-OMVANDLA-ALFA-TILL-NUM                               
032600         IF DEC-KDSVAR-OK                                                 
032700           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVLEVART-ATTR                 
032800           MOVE DEC-IDEDITDATA TO KEY-KVLEVART                            
032900         ELSE                                                             
033000           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVLEVART-ATTR                   
033100           MOVE JA TO INMATNINGS-FEL                                      
033200         END-IF                                                           
033300       END-IF                                                             
033400       IF MID-PRARTSJK = ALL '+'                                          
033500         CONTINUE                                                         
033600       ELSE                                                               
033700         MOVE MID-PRARTSJK TO DEC-IDFRIDATA                               
033800         PERFORM S05-OMVANDLA-ALFA-TILL-NUM                               
033900         IF DEC-KDSVAR-OK                                                 
034000           MOVE DEC-IDEDITDATA TO WS-PRARTSJK                             
034100           MOVE MFS-ALFA-FAELT-RAETT TO MOD-PRARTSJK-ATTR                 
034200         ELSE                                                             
034300           MOVE MFS-ALFA-FAELT-FEL TO MOD-PRARTSJK-ATTR                   
034400           MOVE JA TO INMATNINGS-FEL                                      
034500         END-IF                                                           
034600       END-IF                                                             
034700     END-IF.                                                              
034800     EJECT                                                                
034900                                                                          
035000 C-SKAPA-NYTT-SEG SECTION.                                                
035100                                                                          
035200     PERFORM IMS-GHU-313101                                               
035300     PERFORM IMS-GHU-SEG                                                  
035400     IF SEGMENT-SAKNAS                                                    
035500       PERFORM CA-KOLLA-DATUM-MAX-TVA-AAR                                 
035600       IF INMATNINGS-FEL = NEJ                                            
035700         PERFORM CB-FLYTTA-TILL-DBAREA                                    
035800         IF INMATNINGS-FEL = NEJ                                          
035900           PERFORM IMS-ISRT-SEG                                           
036000           IF SEGMENT-FINNS                                               
036100             PERFORM S01-RENSA-FAELT                                      
036200             PERFORM S07-FORMATETS-ATTRIBUT                               
036300             PERFORM S03-FLYTTA-SIDA-TILL-MOD                             
036400             MOVE '101' TO MED-IDMFSINF                                   
036500             CALL WMEDKONV USING MED-WMEDAREA                             
036600             MOVE MED-MFSINF TO MOD-TEMFSINF                              
036700           END-IF                                                         
036800         END-IF                                                           
036900       END-IF                                                             
037000     ELSE                                                                 
037100       MOVE JA TO INMATNINGS-FEL                                          
037200       MOVE '007' TO MED-IDMFSFEL                                         
037300       CALL WMEDKONV USING MED-WMEDAREA                                   
037400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
037500       PERFORM S06-MFS-LAES-IN-FAELT                                      
037600     END-IF.                                                              
037700     EJECT                                                                
037800                                                                          
037900 CA-KOLLA-DATUM-MAX-TVA-AAR SECTION.                                      
038000                                                                          
038100     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
038200     CALL WDATKONV USING DAT-KDDATFORM,                                   
038300                         DAT-I-TIDATUM,                                   
038400                         DAT-O-TIDATUM,                                   
038500                         DAT-KDSVAR                                       
038600     IF DAT-KDSVAR-OK                                                     
038700       MOVE DAT-TIAAVVD TO WS-AAVVD                                       
038710       MOVE DAT-TISEKEL TO WS-SEKEL                                       
038800     END-IF                                                               
038900     MOVE MID-TIFSGVV TO WS-DAFSGVV                                       
038910     IF MID-TIFSGVV NOT = ZERO                                            
038920       IF MID-TIFSGVV < 5000                                              
038930         MOVE 20      TO WS-DAFSGVV (1:2)                                 
038940       ELSE                                                               
038950         IF MID-TIFSGVV < 9999                                            
038960           MOVE 19    TO WS-DAFSGVV (1:2)                                 
038970         ELSE                                                             
038980           MOVE 999999 TO WS-DAFSGVV                                      
038990         END-IF                                                           
038991       END-IF                                                             
038992     END-IF                                                               
039000*-----DAGENS-ÅRVECKA-MINUS-TVÅ-ÅR-------------------------                
039100     COMPUTE WS-AAAAVV = WS-AAAAVV - 200                                  
039200     IF WS-DAFSGVV < WS-AAAAVV                                            
039300       MOVE MFS-ALFA-FAELT-FEL TO MOD-TIFSGVV-ATTR                        
039400       MOVE JA TO INMATNINGS-FEL                                          
039500       MOVE '009' TO MED-IDMFSFEL                                         
039600       CALL WMEDKONV USING MED-WMEDAREA                                   
039700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
039800     END-IF.                                                              
039900     EJECT                                                                
040000                                                                          
040100 CB-FLYTTA-TILL-DBAREA SECTION.                                           
040200                                                                          
040300     MOVE MID-IDARTNR       TO 3132-IDARTNR                               
040400     MOVE MID-IDDISTR       TO 3132-IDDISTR                               
040500     MOVE MID-TIFSGVV       TO 3132-DAFSGVV                               
040510     IF MID-TIFSGVV NOT = ZERO                                            
040520       IF MID-TIFSGVV < 5000                                              
040530         MOVE 20            TO 3132-DAFSGVV (1:2)                         
040540       ELSE                                                               
040550         IF MID-TIFSGVV < 9999                                            
040560           MOVE 19          TO 3132-DAFSGVV (1:2)                         
040570         ELSE                                                             
040580           MOVE 999999      TO 3132-DAFSGVV                               
040590         END-IF                                                           
040591       END-IF                                                             
040592     END-IF                                                               
040593                                                                          
040600     MOVE MSG-SIGNON-USERID TO 3132-IDUSER                                
040700*FIX--------------FÖR-BTS-KÖRNING-----------------------------            
040800*    MOVE 'W012345 ' TO 3132-IDUSER                                       
040900*FIX----------------------------------------------------------            
041000     IF MID-IDDISTR > ZERO                                                
041100       IF WS-PRARTNTO NUMERIC OR                                          
041200          KEY-KVLEVART NUMERIC                                            
041300         MOVE ZERO TO 3132-PRARTSJK                                       
041400         PERFORM CBA-KOLLA-DATUM-FOER-FAKT                                
041500         IF INMATNINGS-FEL = NEJ                                          
041600           IF WS-PRARTNTO NUMERIC                                         
041700             MOVE WS-PRARTNTO TO 3132-PRARTNTO                            
041800           ELSE                                                           
041900             MOVE ZERO TO 3132-PRARTNTO                                   
042000           END-IF                                                         
042100           IF KEY-KVLEVART NUMERIC                                        
042200             MOVE KEY-KVLEVART TO 3132-KVLEVART                           
042300           ELSE                                                           
042400             MOVE ZERO TO 3132-KVLEVART                                   
042500           END-IF                                                         
042600         END-IF                                                           
042700       ELSE                                                               
042800         MOVE '001' TO MED-IDMFSFEL                                       
042900         CALL WMEDKONV USING MED-WMEDAREA                                 
043000         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
043100         PERFORM S06-MFS-LAES-IN-FAELT                                    
043200         MOVE MFS-ALFA-FAELT-FEL TO MOD-PRARTNTO-ATTR                     
043300         MOVE MFS-ALFA-FAELT-FEL TO MOD-KVLEVART-ATTR                     
043400         MOVE JA TO INMATNINGS-FEL                                        
043500       END-IF                                                             
043600       IF MID-PRARTSJK NOT = ALL '+'                                      
043700         MOVE '001' TO MED-IDMFSFEL                                       
043800         CALL WMEDKONV USING MED-WMEDAREA                                 
043900         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
044000         MOVE JA TO INMATNINGS-FEL                                        
044100         PERFORM S06-MFS-LAES-IN-FAELT                                    
044200         MOVE MFS-ALFA-FAELT-FEL TO MOD-PRARTSJK-ATTR                     
044300       END-IF                                                             
044400     ELSE                                                                 
044500       PERFORM CBB-KOLLA-DATUM-FOER-JUST                                  
044600       IF INMATNINGS-FEL = NEJ                                            
044700         IF WS-PRARTSJK NUMERIC AND WS-PRARTSJK > ZERO                    
044800           MOVE WS-PRARTSJK TO 3132-PRARTSJK                              
044900           MOVE ZERO         TO 3132-PRARTNTO                             
045000                                3132-KVLEVART                             
045100         ELSE                                                             
045200           MOVE '001' TO MED-IDMFSFEL                                     
045300           CALL WMEDKONV USING MED-WMEDAREA                               
045400           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
045500           PERFORM S06-MFS-LAES-IN-FAELT                                  
045600           MOVE MFS-ALFA-FAELT-FEL TO MOD-PRARTSJK-ATTR                   
045700           MOVE JA TO INMATNINGS-FEL                                      
045800         END-IF                                                           
045900         IF MID-PRARTNTO NOT = ALL '+'                                    
046000           MOVE '001' TO MED-IDMFSFEL                                     
046100           CALL WMEDKONV USING MED-WMEDAREA                               
046200           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
046300           PERFORM S06-MFS-LAES-IN-FAELT                                  
046400           MOVE MFS-ALFA-FAELT-FEL TO MOD-PRARTNTO-ATTR                   
046500           MOVE JA TO INMATNINGS-FEL                                      
046600         END-IF                                                           
046700         IF MID-KVLEVART NOT = ALL '+'                                    
046800           MOVE '001' TO MED-IDMFSFEL                                     
046900           CALL WMEDKONV USING MED-WMEDAREA                               
047000           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
047100           PERFORM S06-MFS-LAES-IN-FAELT                                  
047200           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVLEVART-ATTR                   
047300           MOVE JA TO INMATNINGS-FEL                                      
047400         END-IF                                                           
047500       END-IF                                                             
047600     END-IF                                                               
047700     .                                                                    
047800     EJECT                                                                
047900                                                                          
048000 CBA-KOLLA-DATUM-FOER-FAKT SECTION.                                       
048100                                                                          
048200*---FÖR-ATT-ÅTERSTÄLLA WS-AAAAVV TILL DAGENS ÅÅÅÅVV                       
048300     COMPUTE WS-AAAAVV = WS-AAAAVV + 200                                  
048400     IF WS-DAFSGVV > WS-AAAAVV                                            
048500       MOVE MFS-ALFA-FAELT-FEL TO MOD-TIFSGVV-ATTR                        
048600       MOVE JA TO INMATNINGS-FEL                                          
048700       MOVE '008' TO MED-IDMFSFEL                                         
048800       CALL WMEDKONV USING MED-WMEDAREA                                   
048900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
049000     END-IF.                                                              
049100     EJECT                                                                
049200                                                                          
049300 CBB-KOLLA-DATUM-FOER-JUST SECTION.                                       
049400                                                                          
049500*---FÖR-ATT-ÅTERSTÄLLA WS-AAAAVV TILL DAGENS ÅÅÅÅVV                       
049600     COMPUTE WS-AAAAVV = WS-AAAAVV + 200                                  
049700     IF WS-DAFSGVV >= WS-AAAAVV                                           
049800       MOVE MFS-ALFA-FAELT-FEL TO MOD-TIFSGVV-ATTR                        
049900       MOVE JA TO INMATNINGS-FEL                                          
050000       MOVE '008' TO MED-IDMFSFEL                                         
050100       CALL WMEDKONV USING MED-WMEDAREA                                   
050200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
050300     END-IF.                                                              
050400     EJECT                                                                
050500                                                                          
050600                                                                          
050700 D-TAG-BORT-SEG SECTION.                                                  
050800                                                                          
050900     PERFORM IMS-GHU-SEG                                                  
051000     IF SEGMENT-FINNS                                                     
051100       PERFORM IMS-DLET-SEG                                               
051200       PERFORM S01-RENSA-FAELT                                            
051300       PERFORM S07-FORMATETS-ATTRIBUT                                     
051400       MOVE MID-IDARTNR-SAMMA TO W-IDARTNR                                
051500       MOVE MID-IDDISTR-SAMMA TO W-IDDISTR                                
051600       MOVE MID-TIFSGVV-SAMMA TO W-DAFSGVV                                
051610       IF MID-TIFSGVV-SAMMA NOT = ZERO                                    
051620         IF MID-TIFSGVV-SAMMA < 5000                                      
051630           MOVE 20            TO W-DAFSGVV (1:2)                          
051640         ELSE                                                             
051650           IF MID-TIFSGVV-SAMMA < 9999                                    
051660             MOVE 19          TO W-DAFSGVV (1:2)                          
051670           ELSE                                                           
051680             MOVE 999999      TO W-DAFSGVV                                
051690           END-IF                                                         
051691         END-IF                                                           
051692       END-IF                                                             
051693                                                                          
051700       PERFORM S03-FLYTTA-SIDA-TILL-MOD                                   
051800       MOVE '101' TO MED-IDMFSINF                                         
051900       CALL WMEDKONV USING MED-WMEDAREA                                   
052000       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
052100     ELSE                                                                 
052200       PERFORM S06-MFS-LAES-IN-FAELT                                      
052300       PERFORM S02-MFS-ROER-EJ-FAELT                                      
052400       PERFORM H-VISA-SAMMA-SIDA                                          
052500       MOVE '010' TO MED-IDMFSFEL                                         
052600       CALL WMEDKONV USING MED-WMEDAREA                                   
052700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
052800*      MOVE JA TO INMATNINGS-FEL                                          
052900     END-IF.                                                              
053000     EJECT                                                                
053100                                                                          
053200 E-KOLLA-OM-UPPDAT-OENSKAS SECTION.                                       
053300                                                                          
053400     IF MID-IDARTNR = ALL '+' OR MID-IDARTNR = 0                          
053500       IF MID-TIFSGVV = ALL '+' OR MID-TIFSGVV = 0                        
053600         CONTINUE                                                         
053700       ELSE                                                               
053800         MOVE JA TO UPPDATERING-OENSKAS                                   
053900       END-IF                                                             
054000     ELSE                                                                 
054100         MOVE JA TO UPPDATERING-OENSKAS                                   
054200     END-IF                                                               
054300     .                                                                    
054400     EJECT                                                                
054500                                                                          
054600 F-VISA-NAESTA-SIDA SECTION.                                              
054700                                                                          
054800     IF MID-IDARTNR-NAESTA NUMERIC AND                                    
054900        MID-IDDISTR-NAESTA NUMERIC AND                                    
055000        MID-TIFSGVV-NAESTA NUMERIC                                        
055100       MOVE MID-IDARTNR-NAESTA TO W-IDARTNR                               
055200       MOVE MID-IDDISTR-NAESTA TO W-IDDISTR                               
055300       MOVE MID-TIFSGVV-NAESTA TO W-DAFSGVV                               
055310       IF MID-TIFSGVV-NAESTA NOT = ZERO                                   
055320         IF MID-TIFSGVV-NAESTA < 5000                                     
055330           MOVE 20             TO W-DAFSGVV (1:2)                         
055340         ELSE                                                             
055350           IF MID-TIFSGVV-NAESTA < 9999                                   
055360             MOVE 19           TO W-DAFSGVV (1:2)                         
055370           ELSE                                                           
055380             MOVE 999999       TO W-DAFSGVV                               
055390           END-IF                                                         
055391         END-IF                                                           
055392       END-IF                                                             
055400     ELSE                                                                 
055500       MOVE ZERO TO W-IDARTNR                                             
055600                    W-IDDISTR                                             
055700                    W-DAFSGVV                                             
055800     END-IF                                                               
055900     PERFORM S03-FLYTTA-SIDA-TILL-MOD                                     
056000     .                                                                    
056100     EJECT                                                                
056200                                                                          
056300 G-VISA-FOERSTA-SIDA SECTION.                                             
056400                                                                          
056500     MOVE LOW-VALUE TO WL313111                                           
056600     PERFORM S03-FLYTTA-SIDA-TILL-MOD                                     
056700     .                                                                    
056800     EJECT                                                                
056900                                                                          
057000 H-VISA-SAMMA-SIDA SECTION.                                               
057100                                                                          
057200     IF MID-IDARTNR-SAMMA NUMERIC AND                                     
057300        MID-IDDISTR-SAMMA NUMERIC AND                                     
057400        MID-TIFSGVV-SAMMA NUMERIC                                         
057500       MOVE MID-IDARTNR-SAMMA TO W-IDARTNR                                
057600       MOVE MID-IDDISTR-SAMMA TO W-IDDISTR                                
057700       MOVE MID-TIFSGVV-SAMMA TO W-DAFSGVV                                
057710       IF MID-TIFSGVV-SAMMA  NOT = ZERO                                   
057720         IF MID-TIFSGVV-SAMMA  < 5000                                     
057730           MOVE 20             TO W-DAFSGVV (1:2)                         
057740         ELSE                                                             
057750           IF MID-TIFSGVV-SAMMA  < 9999                                   
057760             MOVE 19           TO W-DAFSGVV (1:2)                         
057770           ELSE                                                           
057780             MOVE 999999       TO W-DAFSGVV                               
057790           END-IF                                                         
057791         END-IF                                                           
057792       END-IF                                                             
057800     ELSE                                                                 
057900       MOVE ZERO TO W-IDARTNR                                             
058000                    W-IDDISTR                                             
058100                    W-DAFSGVV                                             
058200     END-IF                                                               
058300     PERFORM S03-FLYTTA-SIDA-TILL-MOD                                     
058400     .                                                                    
058500     SKIP2                                                                
058600                                                                          
058700 S01-RENSA-FAELT SECTION.                                                 
058800                                                                          
058900     MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR                             
059000                                  MOD-IDDISTR                             
059100                                  MOD-TIFSGVV                             
059200                                  MOD-KVLEVART                            
059300                                  MOD-KDSVAR                              
059400                                  MOD-PRARTNTO                            
059500                                  MOD-PRARTSJK.                           
059600    SKIP2                                                                 
059700                                                                          
059800 S02-MFS-ROER-EJ-FAELT SECTION.                                           
059900                                                                          
060000     MOVE MFS-ROER-EJ-FAELT    TO MOD-IDARTNR                             
060100                                  MOD-IDDISTR                             
060200                                  MOD-TIFSGVV                             
060300                                  MOD-KVLEVART                            
060400                                  MOD-KDSVAR                              
060500                                  MOD-PRARTNTO                            
060600                                  MOD-PRARTSJK.                           
060700    EJECT                                                                 
060800                                                                          
060900 S03-FLYTTA-SIDA-TILL-MOD SECTION.                                        
061000                                                                          
061100     PERFORM IMS-GHU-313101                                               
061200     PERFORM IMS-GNP-313111                                               
061300     IF SEGMENT-FINNS                                                     
061400       MOVE 1 TO RADIX                                                    
061500       PERFORM UNTIL RADIX = 13 OR SEGMENT-SAKNAS                         
061600         PERFORM S04-FLYTTA-RAD-TILL-MOD                                  
061700         IF RADIX = 1                                                     
061800           MOVE 3132-IDARTNR TO MOD-IDARTNR-SAMMA                         
061900           MOVE 3132-IDDISTR TO MOD-IDDISTR-SAMMA                         
062000           MOVE 3132-DAFSGVV (3:4) TO MOD-TIFSGVV-SAMMA                   
062100         END-IF                                                           
062200         PERFORM IMS-GNP-313111                                           
062300         ADD 1 TO RADIX                                                   
062400       END-PERFORM                                                        
062500                                                                          
062600       IF SEGMENT-SAKNAS                                                  
062700*---------------------------------------------INGA FLER SIDOR             
062800       MOVE '106' TO MED-IDMFSINF                                         
062900       CALL WMEDKONV USING MED-WMEDAREA                                   
063000       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
063100       END-IF                                                             
063200     ELSE                                                                 
063300       MOVE '107' TO MED-IDMFSINF                                         
063400       CALL WMEDKONV USING MED-WMEDAREA                                   
063500       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
063600     END-IF                                                               
063700                                                                          
063800     IF RADIX = 13 AND SEGMENT-FINNS                                      
063900       MOVE 3132-IDARTNR TO MOD-IDARTNR-NAESTA                            
064000       MOVE 3132-IDDISTR TO MOD-IDDISTR-NAESTA                            
064100       MOVE 3132-DAFSGVV (3:4) TO MOD-TIFSGVV-NAESTA                      
064200       MOVE '105' TO MED-IDMFSINF                                         
064300       CALL WMEDKONV USING MED-WMEDAREA                                   
064400       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
064500     ELSE                                                                 
064600       MOVE ZERO TO MOD-IDARTNR-NAESTA                                    
064700                    MOD-IDDISTR-NAESTA                                    
064800                    MOD-TIFSGVV-NAESTA                                    
064900                                                                          
065000     END-IF                                                               
065100     .                                                                    
065200     EJECT                                                                
065300                                                                          
065400 S04-FLYTTA-RAD-TILL-MOD SECTION.                                         
065500                                                                          
065600     MOVE 3132-IDARTNR          TO MOD-UTRAD-IDARTNR(RADIX)               
065700     MOVE 3132-IDDISTR          TO MOD-UTRAD-IDDISTR(RADIX)               
065800     MOVE 3132-DAFSGVV (3:4)    TO MOD-UTRAD-TIFSGVV(RADIX)               
065900     MOVE 3132-PRARTNTO         TO WS-PRARTNTO                            
066000     MOVE WS-PRARTNTO           TO MOD-UTRAD-PRARTNTO(RADIX)              
066100     MOVE 3132-KVLEVART         TO MOD-UTRAD-KVLEVART(RADIX)              
066200     MOVE 3132-PRARTSJK         TO MOD-UTRAD-PRARTSJK(RADIX)              
066300     .                                                                    
066400     SKIP2                                                                
066500                                                                          
066600 S05-OMVANDLA-ALFA-TILL-NUM SECTION.                                      
066700                                                                          
066800     MOVE 7            TO DEC-KVHELTAL                                    
066900     MOVE 2            TO DEC-KVDECIMAL                                   
067000     CALL WDECEDIT USING DEC-IDFRIDATA                                    
067100                         DEC-IDEDITDATA                                   
067200                         DEC-KVHELTAL                                     
067300                         DEC-KVDECIMAL                                    
067400                         DEC-KDSVAR                                       
067500     .                                                                    
067600     SKIP2                                                                
067700                                                                          
067800 S06-MFS-LAES-IN-FAELT SECTION.                                           
067900                                                                          
068000     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDSVAR-ATTR                        
068100                                   MOD-IDARTNR-ATTR                       
068200                                   MOD-IDDISTR-ATTR                       
068300                                   MOD-TIFSGVV-ATTR                       
068400                                   MOD-PRARTNTO-ATTR                      
068500                                   MOD-KVLEVART-ATTR                      
068600                                   MOD-PRARTSJK-ATTR                      
068700     .                                                                    
068800     EJECT                                                                
068900                                                                          
069000 S07-FORMATETS-ATTRIBUT SECTION.                                          
069100                                                                          
069200     MOVE MFS-FORMATETS-ATTR    TO MOD-KDSVAR-ATTR                        
069300                                   MOD-IDARTNR-ATTR                       
069400                                   MOD-IDDISTR-ATTR                       
069500                                   MOD-TIFSGVV-ATTR                       
069600                                   MOD-PRARTNTO-ATTR                      
069700                                   MOD-KVLEVART-ATTR                      
069800                                   MOD-PRARTSJK-ATTR                      
069900     .                                                                    
070000     SKIP2                                                                
070100                                                                          
070200 S09-OMVANDLA-ALFA-TILL-NUM SECTION.                                      
070300                                                                          
070400     MOVE 7            TO DEC-KVHELTAL                                    
070500     MOVE 0            TO DEC-KVDECIMAL                                   
070600     CALL WDECEDIT USING DEC-IDFRIDATA                                    
070700                         DEC-IDEDITDATA                                   
070800                         DEC-KVHELTAL                                     
070900                         DEC-KVDECIMAL                                    
071000                         DEC-KDSVAR                                       
071100     .                                                                    
071200     EJECT                                                                
071300                                                                          
071400*                                                                         
071500******************************************************************        
071600* IMS SEKTIONER                                                           
071700******************************************************************        
071800*                                                                         
071900    SKIP3                                                                 
072000 IMS-GET-MSG SECTION.                                                     
072100    SKIP2                                                                 
072200     MOVE '  QC' TO GODK-STATUSKODER                                      
072300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
072400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
072500     PERFORM IMS-STATUSKONTROLL.                                          
072600   EJECT                                                                  
072700 IMS-INSERT-MSG SECTION.                                                  
072800    SKIP2                                                                 
072810     IF NOT ENGLISH-TEXT                                                  
072820       MOVE '0' TO MFS-KDHUVOMR                                           
072830     END-IF                                                               
072900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
073000     MOVE SPACE TO GODK-STATUSKODER                                       
073100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
073200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
073300     PERFORM IMS-STATUSKONTROLL.                                          
073400    EJECT                                                                 
073500                                                                          
073600 IMS-GNP-313111 SECTION.                                                  
073700*                                              SÖKN KST BARN              
073800     SKIP2                                                                
073900     STRING 'WL313111(KY3132  =>' W-KY3132-X ')'                          
074000            DELIMITED BY SIZE INTO SSA1                                   
074100     MOVE '  GE' TO GODK-STATUSKODER                                      
074200     CALL CBLTDLI USING GNP 3131-PCB DLI-IO-AREA SSA1                     
074300     MOVE 3131-STATUS-CODE TO STATUS-WS                                   
074400     PERFORM IMS-STATUSKONTROLL.                                          
074500                                                                          
074600 IMS-GHU-313101 SECTION.                                                  
074700*                                           1:A SÖKN KST FÖRÄLDER         
074800     SKIP2                                                                
074900     STRING 'WL313101(WDGXKEY  =' W-3131-X  ')'                           
075000            DELIMITED BY SIZE INTO SSA1                                   
075100     MOVE '  ' TO GODK-STATUSKODER                                        
075200     CALL CBLTDLI USING GHU 3131-PCB DLI-IO-AREA SSA1                     
075300     MOVE 3131-STATUS-CODE TO STATUS-WS                                   
075400     PERFORM IMS-STATUSKONTROLL.                                          
075500     EJECT                                                                
075600                                                                          
075700 IMS-GHU-SEG SECTION.                                                     
075800     SKIP2                                                                
075900     STRING 'WL313111(KY3132   =' W-KY3132-X ')'                          
076000            DELIMITED BY SIZE INTO SSA1                                   
076100     MOVE '  GE' TO GODK-STATUSKODER                                      
076200     CALL CBLTDLI USING GHU 3131-PCB DLI-IO-AREA SSA1                     
076300     MOVE 3131-STATUS-CODE TO STATUS-WS                                   
076400     PERFORM IMS-STATUSKONTROLL.                                          
076500                                                                          
076600 IMS-ISRT-SEG SECTION.                                                    
076700     STRING 'WL313101(WDGXKEY  =' W-3131-X ')'                            
076800            DELIMITED BY SIZE INTO SSA1                                   
076900     MOVE   'WL313111 ' TO SSA2                                           
077000     MOVE '  ' TO GODK-STATUSKODER                                        
077100     CALL CBLTDLI USING ISRT 3131-PCB DLI-IO-AREA SSA1 SSA2               
077200     MOVE 3131-STATUS-CODE TO STATUS-WS                                   
077300     PERFORM IMS-STATUSKONTROLL.                                          
077400     EJECT                                                                
077500                                                                          
077600 IMS-DLET-SEG SECTION.                                                    
077700                                                                          
077800     MOVE '  ' TO GODK-STATUSKODER                                        
077900     CALL CBLTDLI USING DLET 3131-PCB DLI-IO-AREA                         
078000     MOVE 3131-STATUS-CODE TO STATUS-WS                                   
078100     PERFORM IMS-STATUSKONTROLL.                                          
078200     SKIP2                                                                
078300                                                                          
078400 IMS-STATUSKONTROLL SECTION.                                              
078500     SKIP2                                                                
078600     SET STATUS-IX TO 1                                                   
078700     SEARCH GODK-STATUS AT END CALL FELLOG                                
078800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
078900     END-SEARCH.                                                          
079000   EJECT                                                                  
