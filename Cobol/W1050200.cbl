000300     SKIP3                                                                
000400 ID DIVISION.                                                             
000500     SKIP2                                                                
000600 PROGRAM-ID.     W1050200.                                                
001000*AUTHOR.         BODIL LINDAHL.                                           
001100*DATE-WRITTEN.   DECEMBER 1983.                                           
001200                                                                          
001400*                                                                         
001500*    FUNKTION.                                                            
001600*        SÖKNING RUBRIKNUMMER                                             
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W1T502                                              
002000*        MID:         W1I50201                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        MOD:         W1O50201                                            
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP3                                                                
002700 DATA DIVISION.                                                           
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
002901                                                                          
002910*    -- CHECKED BY WY2000                                                 
003000 77  IDPGM                       PIC X(8)    VALUE 'W1050200'.            
003700 77    JA                        PIC X       VALUE 'J'.                   
003800 77    NEJ                       PIC X       VALUE 'N'.                   
003810 77    SVENSK                    PIC X       VALUE 'S'.                   
003900 77    WS-IDRUBNR                PIC X(5)    VALUE SPACE.                 
004000 77    WS-IDSKYLT                PIC X(3)    VALUE SPACE.                 
004100 77    NYCKLAR-RETT              PIC X       VALUE 'J'.                   
004200 77    RAD-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
004300 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
004400 77    MAX-RAD-PLUS-1            PIC S9(3)   VALUE +8.                    
004500 77    MAX-MOD-LAENGD            PIC S9(4)  VALUE +949  COMP SYNC.        
004600 77    MIN-MOD-LAENGD            PIC S9(4)  VALUE +130  COMP SYNC.        
004700     SKIP2                                                                
004710                                                                          
004720 01  DYNAMISKA-SUBPROGRAM.                                                
004730   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
004740   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
004750                                                                          
004800 01    WS-IDRUBNR-MIN            PIC X(5).                                
004900 01    IDRUBNR-MIN-WS REDEFINES WS-IDRUBNR-MIN   PIC 9(5).                
005000     SKIP2                                                                
005100 01    WS-IDRUBNR-MAX            PIC X(5).                                
005200 01    IDRUBNR-MAX-WS REDEFINES WS-IDRUBNR-MAX   PIC 9(5).                
005300     SKIP2                                                                
005400 01    LITEN-BOKSTAV        PIC X(28)                                     
005500                       VALUE 'abcdefghijklmnopqrstuvxyzåäö'.              
005600     SKIP2                                                                
005700 01    STOR-BOKSTAV         PIC X(28)                                     
005800                       VALUE 'ABCDEFGHIJKLMNOPQRSTUVXYZÅÄÖ'.              
005900     EJECT                                                                
006000 01  WS-IDTRANS             PIC X(4).                                     
006100     88   EGEN-BILD         VALUE '1502'.                                 
006200     88   GODKAEND-BILD     VALUE '1501' '1502' '1503'                    
006300                                  '1505' '1506'                           
006400                                  '1508' '1509'.                          
006500     EJECT                                                                
006600*01    -COPY WWLAND03C0                                                   
006800     EJECT                                                                
006900 01    NYCKLAR-TILL-DLI.                                                  
007000   03    W-IDRUBNR-X.                                                     
007100     05    W-IDRUBNR             PIC S9(5)  COMP-3 VALUE ZERO.            
007200   03    W-IDRUBNR-MAX-X.                                                 
007300     05    W-IDRUBNR-MAX         PIC S9(5)  COMP-3 VALUE ZERO.            
007400   03    W-IDRUBNR-MIN-X.                                                 
007500     05    W-IDRUBNR-MIN         PIC S9(5)  COMP-3 VALUE ZERO.            
007600   03    W-IDSKYLT-X.                                                     
007700     05    W-IDSKYLT             PIC X(3)   VALUE SPACE.                  
007800   03    W-IDSEGMNR-LOW-X.                                                
007900     05    W-IDSEGMNR-LOW        PIC S9(1)  VALUE ZERO COMP-3.            
008000   03    W-IDSEGMNR-HIGH-X.                                               
008100     05    W-IDSEGMNR-HIGH       PIC S9(1)  VALUE +9   COMP-3.            
008200     SKIP3                                                                
008300 01  MEDDELANDE-AREA.                                                     
008400*                                                                         
008500   03  MEDD-1.                                                            
008600      05 FILLER                  PIC X(26)   VALUE                        
008700              'TRYCK PF8 FÖR FLER RADER  '.                               
008800      05 FILLER                  PIC X(26)   VALUE                        
008900              'PRESS PF8 FOR MORE LINES  '.                               
009000   03  FILLER REDEFINES MEDD-1.                                           
009100      05 MED-1 OCCURS 2    PIC X(26).                                     
009200*                                                                         
009300   03  MEDD-2.                                                            
009400      05 FILLER                  PIC X(26)   VALUE                        
009500                'FÖRSTA SIDAN VISAS        '.                             
009600      05 FILLER                  PIC X(26)   VALUE                        
009700                'FIRST PAGE IS SHOWN       '.                             
009800   03  FILLER REDEFINES MEDD-2.                                           
009900      05 MED-2 OCCURS 2    PIC X(26).                                     
010000*                                                                         
010100   03  FEL-01.                                                            
010200      05 FILLER                  PIC X(26)   VALUE                        
010300             'NYCKLAR FEL               '.                                
010400      05 FILLER                  PIC X(26)   VALUE                        
010500             'WRONG KEY(S)              '.                                
010600   03  FILLER REDEFINES FEL-01.                                           
010700      05 FEL-1 OCCURS 2    PIC X(26).                                     
010800*                                                                         
010900   03  FEL-02.                                                            
011000      05 FILLER                  PIC X(26)   VALUE                        
011100             'RUBRIKNUMMER SAKNAS       '.                                
011200      05 FILLER                  PIC X(26)   VALUE                        
011300             'HEADING-NUMBER IS MISSING '.                                
011400   03  FILLER REDEFINES FEL-02.                                           
011500      05 FEL-2 OCCURS 2    PIC X(26).                                     
011600*                                                                         
011700   03  FEL-03.                                                            
011800      05 FILLER                  PIC X(26)   VALUE                        
011900             'MATA IN NYCKLAR           '.                                
012000      05 FILLER                  PIC X(26)   VALUE                        
012100             'PLEASE, TYPE IN KEYS      '.                                
012200   03  FILLER REDEFINES FEL-03.                                           
012300      05 FEL-3 OCCURS 2    PIC X(26).                                     
012400*                                                                         
012500     EJECT                                                                
012600******************************************************************        
012700*                                                                         
012800*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
012900*                                                                         
013000 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
013100     SKIP3                                                                
013200*01    MID -COPY W1I50201.                                                
013400     EJECT                                                                
013500*01    -COPY WMSGAREA                                                     
013700     EJECT                                                                
013800*  03    MOD -COPY W1O50201 -RED MSG-AREA.                                
014000     EJECT                                                                
014100*01    -COPY WMFSAREA                                                     
014300     EJECT                                                                
014400******************************************************************        
014500*                                                                         
014600*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014700*                                                                         
014800 01    IMS-WS.                                                            
014900   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
015000     SKIP3                                                                
015100*                        **** STATUS-KOD FRÅN IMS                         
015200   03    STATUS-WS               PIC XX.                                  
015300     88    SEGMENT-FINNS                     VALUE '  '.                  
015400     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
015500     SKIP3                                                                
015600   03    GODK-STATUSKODER.                                                
015700     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
015800     SKIP3                                                                
015900 01    SSA1                      PIC X(64).                               
016000     EJECT                                                                
016100*                            IMS FUNKTIONSKODER                           
016200*01    -COPY W0003                                                        
016400     EJECT                                                                
016500*                            DLI INPUT-OUTPUT AREA                        
016600 01    DLI-IO-AREA.                                                       
016700   03    IO-AREA                 PIC X(50)  VALUE SPACE.                  
016800     SKIP3                                                                
016900*  03    WLKATB01 -COPY WDN201  -PRE KATB-  -RED IO-AREA.                 
017100     EJECT                                                                
017200*  03    WLKATC01 -COPY WDN2A1  -PRE KATC-  -RED IO-AREA.                 
017400     EJECT                                                                
017500 LINKAGE SECTION.                                                         
017600*01    -COPY W0009     -PRE MSG-                                          
017800     EJECT                                                                
017900*01    -COPY W0008     -PRE KATB-                                         
018100     05  FILLER                  PIC X.                                   
018200     EJECT                                                                
018300*01    -COPY W0008     -PRE KATC-                                         
018500     05  FILLER                  PIC X.                                   
018600     EJECT                                                                
018700 PROCEDURE DIVISION USING MSG-PCB KATB-PCB KATC-PCB.                      
018800     ENTRY 'DLITCBL' USING MSG-PCB KATB-PCB KATC-PCB.                     
018900     SKIP2                                                                
019000     PERFORM IMS-GET-MSG                                                  
019100     IF SEGMENT-FINNS                                                     
019200       PERFORM A-INIT-SPARA-INPUT                                         
019300       IF GODKAEND-BILD                                                   
019400         PERFORM B-KOLLA-NYCKLAR                                          
019500         IF NYCKLAR-RETT = JA                                             
019600           IF EGEN-BILD                                                   
019700             PERFORM D-KOLLA-PFTANGENTER                                  
019800           ELSE                                                           
019900             MOVE WS-IDRUBNR TO WS-IDRUBNR-MIN                            
020000             INSPECT WS-IDRUBNR-MIN                                       
020100                               REPLACING ALL SPACE BY ZERO                
020200           END-IF                                                         
020300           PERFORM C-LAS-BASEN-FLYTTA-TILL-MOD                            
020400         ELSE                                                             
020500           MOVE FEL-1(INDX) TO MOD-TEMFSFEL                               
020600         END-IF                                                           
020700         MOVE MAX-MOD-LAENGD TO MSG-KVLL                                  
020800       ELSE                                                               
020900         MOVE FEL-3(INDX) TO MOD-TEMFSFEL                                 
021000         PERFORM E-RENSA-MOD                                              
021100       END-IF                                                             
021200       PERFORM IMS-INSERT-MSG                                             
021300     END-IF                                                               
021400     MOVE ZERO TO RETURN-CODE                                             
021500     GOBACK                                                               
021600     CONTINUE.                                                            
021700     EJECT                                                                
021800 A-INIT-SPARA-INPUT SECTION.                                              
021900     SKIP2                                                                
022000     IF MSG-DUBBLA-TRANSKODER                                             
022100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I50201                 
022200       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
022300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
022400       MOVE MSG-IDPFK TO MFS-IDPFK                                        
022500     ELSE                                                                 
022600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I50201                  
022700       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
022800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
022900       MOVE ' ' TO MFS-IDPFK                                              
023000     END-IF                                                               
023100     MOVE MFS-IDTRANS TO WS-IDTRANS                                       
023200                                                                          
023300     MOVE LOW-VALUE TO MSG-AREA                                           
023400     MOVE 'W1O50201' TO MFS-IDMOD                                         
023500     MOVE '1502' TO MOD-IDTRANS                                           
023600     IF ENGLISH-TEXT                                                      
023700       MOVE +2 TO INDX                                                    
023800     ELSE                                                                 
023900       MOVE +1 TO INDX                                                    
024000     END-IF                                                               
024100     MOVE MFS-RENSA-FAELT TO MOD-IDRUBNR-IN                               
024200                             MOD-IDSKYLT-IN                               
024500                             MOD-BERUBTXT-IN                              
024600                             MOD-IDRUBNR-MIN                              
024700                             MOD-IDRUBNR-MAX                              
024800                             MOD-TEMFSFEL                                 
024900                             MOD-TEMFSINF                                 
025000     CONTINUE.                                                            
025100     EJECT                                                                
025200 B-KOLLA-NYCKLAR SECTION.                                                 
025300     SKIP2                                                                
025400     MOVE JA TO NYCKLAR-RETT                                              
025500                                                                          
025600     IF MID-IDSKYLT-IN = ALL '+'                                          
025610       IF MID-IDSKYLT-UT = SPACE                                          
025620          MOVE SVENSK TO WS-IDSKYLT                                       
025630          MOVE '7' TO MFS-IDPFK                                           
025640       else                                                               
025700          MOVE MID-IDSKYLT-UT TO WS-IDSKYLT                               
025710       end-if                                                             
025800     ELSE                                                                 
025900       MOVE MID-IDSKYLT-IN TO WS-IDSKYLT                                  
026000       INSPECT WS-IDSKYLT CONVERTING LITEN-BOKSTAV TO STOR-BOKSTAV        
026100       MOVE '7' TO MFS-IDPFK                                              
026200     END-IF                                                               
026300     MOVE WS-IDSKYLT TO MOD-IDSKYLT-UT                                    
026400                                                                          
026500     SET WWLAND03-IX TO +1                                                
026600     SEARCH WWLAND03-IDSKYLT-RAD                                          
026700             AT END MOVE NEJ TO NYCKLAR-RETT                              
026800             WHEN WWLAND03-IDSKYLT(WWLAND03-IX) = WS-IDSKYLT              
026900             CONTINUE                                                     
027000     END-SEARCH                                                           
027100                                                                          
027200     IF MID-IDRUBNR-IN = ALL '+'                                          
027300       MOVE MID-IDRUBNR-UT TO WS-IDRUBNR                                  
027400     ELSE                                                                 
027500       MOVE MID-IDRUBNR-IN TO WS-IDRUBNR                                  
027600       MOVE '7' TO MFS-IDPFK                                              
027700     END-IF                                                               
027800     MOVE WS-IDRUBNR TO WS-IDRUBNR-MAX                                    
027900     INSPECT WS-IDRUBNR-MAX REPLACING ALL SPACE BY '9'                    
028000     IF WS-IDRUBNR-MAX NUMERIC                                            
028200       CONTINUE                                                           
028300     ELSE                                                                 
028400       MOVE NEJ TO NYCKLAR-RETT                                           
028500     END-IF                                                               
028600     MOVE WS-IDRUBNR TO MOD-IDRUBNR-UT                                    
028700                                                                          
030100     IF MID-BERUBTXT-IN = ALL '+'                                         
030200       MOVE MID-BERUBTXT-UT TO MOD-BERUBTXT-UT                            
030300     ELSE                                                                 
030400       MOVE MID-BERUBTXT-IN TO MOD-BERUBTXT-UT                            
030500     END-IF                                                               
030600     CONTINUE.                                                            
030700     EJECT                                                                
030800 C-LAS-BASEN-FLYTTA-TILL-MOD SECTION.                                     
030900     SKIP2                                                                
031000     MOVE +1 TO RAD-IX                                                    
031100                                                                          
031200     MOVE IDRUBNR-MAX-WS TO W-IDRUBNR-MAX                                 
031300     MOVE IDRUBNR-MIN-WS TO W-IDRUBNR-MIN                                 
031400     MOVE WS-IDSKYLT TO W-IDSKYLT                                         
031500                                                                          
031600     PERFORM IMS-GET-KATC01                                               
031700     IF SEGMENT-FINNS                                                     
031800       MOVE KATC-RUBA-IDRUBNR TO MOD-IDRUBNR-MIN                          
031900       PERFORM UNTIL                                                      
032000        NOT ( RAD-IX < MAX-RAD-PLUS-1 )                                   
032100         IF SEGMENT-FINNS                                                 
032200           IF KATC-RUBA-IDSEGMNR = +1 OR ZERO                             
032300             MOVE KATC-RUBA-IDRUBNR TO MOD-IDRUBNR(RAD-IX)                
032400                                       W-IDRUBNR                          
032500             MOVE KATC-RUBA-BERUBTXT TO MOD-BERUBTXT                      
032600                 (RAD-IX 1)                                               
032700             MOVE MFS-RENSA-FAELT TO MOD-BERUBTXT (RAD-IX 2)              
032800                                     MOD-BERUBTXT (RAD-IX 3)              
032900             PERFORM IMS-GET-KATB01                                       
033000             MOVE KATB-RUB-TIUPPDAT TO MOD-TIUPPDAT(RAD-IX)               
033100                                                                          
033200             IF ENGLISH-TEXT                                              
033300               IF KATB-RUB-FLKOMBINERAS = 'J'                             
033400                 MOVE 'Y' TO MOD-FLKOMBINERAS(RAD-IX)                     
033500               ELSE                                                       
033600                 MOVE KATB-RUB-FLKOMBINERAS TO                            
033700                     MOD-FLKOMBINERAS(RAD-IX)                             
033800                                                                          
033900               END-IF                                                     
034000               MOVE 'DATE ' TO MOD-DATUM-RUBRIK(RAD-IX)                   
034100             ELSE                                                         
034200               MOVE KATB-RUB-FLKOMBINERAS TO                              
034300                     MOD-FLKOMBINERAS(RAD-IX)                             
034400                                                                          
034500               MOVE 'DATUM' TO MOD-DATUM-RUBRIK(RAD-IX)                   
034600             END-IF                                                       
034700           ELSE                                                           
034800             MOVE KATC-RUBA-BERUBTXT TO MOD-BERUBTXT                      
034900                  (RAD-IX KATC-RUBA-IDSEGMNR)                             
035000           END-IF                                                         
035100           PERFORM IMS-GET-KATC01                                         
035200           IF SEGMENT-FINNS                                               
035300             IF KATC-RUBA-IDRUBNR = W-IDRUBNR                             
035400*                   CONTINUE                                              
035500               CONTINUE                                                   
035600             ELSE                                                         
035700               ADD +1 TO RAD-IX                                           
035800             END-IF                                                       
035900           ELSE                                                           
036000             ADD +1 TO RAD-IX                                             
036100           END-IF                                                         
036200         ELSE                                                             
036300           MOVE MFS-RENSA-FAELT TO MOD-UTRAD(RAD-IX)                      
036400           ADD +1 TO RAD-IX                                               
036500         END-IF                                                           
036600       END-PERFORM                                                        
036700     ELSE                                                                 
036800       MOVE FEL-2(INDX) TO MOD-TEMFSFEL                                   
036900     END-IF                                                               
037000     IF SEGMENT-FINNS                                                     
037100       MOVE MED-1(INDX) TO MOD-TEMFSINF                                   
037200       MOVE KATC-RUBA-IDRUBNR TO MOD-IDRUBNR-MAX                          
037300     END-IF                                                               
037400     CONTINUE.                                                            
037500     EJECT                                                                
037600  D-KOLLA-PFTANGENTER SECTION.                                            
037700     SKIP2                                                                
037800     IF MFS-IDPFK = '7'                                                   
037900       MOVE WS-IDRUBNR TO WS-IDRUBNR-MIN                                  
038000       MOVE MED-2(INDX) TO MOD-TEMFSINF                                   
038100     ELSE                                                                 
038200       EVALUATE TRUE                                                      
038300       WHEN MFS-IDPFK = '8'                                               
038400         IF MID-IDRUBNR-MAX NUMERIC                                       
038500           MOVE MID-IDRUBNR-MAX TO WS-IDRUBNR-MIN                         
038600         ELSE                                                             
038700           MOVE WS-IDRUBNR TO WS-IDRUBNR-MIN                              
038800           MOVE MED-2(INDX) TO MOD-TEMFSINF                               
038900         END-IF                                                           
039000        WHEN OTHER                                                        
039100         IF MID-IDRUBNR-MIN NUMERIC                                       
039200           MOVE MID-IDRUBNR-MIN TO WS-IDRUBNR-MIN                         
039300         ELSE                                                             
039400           MOVE WS-IDRUBNR TO WS-IDRUBNR-MIN                              
039500           MOVE MED-2(INDX) TO MOD-TEMFSINF                               
039600         END-IF                                                           
039700       END-EVALUATE                                                       
039800     END-IF                                                               
039900     INSPECT WS-IDRUBNR-MIN REPLACING ALL SPACE BY ZERO                   
040000     CONTINUE.                                                            
040100     EJECT                                                                
040200 E-RENSA-MOD SECTION.                                                     
040300     SKIP2                                                                
040400     MOVE MIN-MOD-LAENGD TO MSG-KVLL                                      
040500     MOVE MFS-RENSA-FAELT TO MOD-BERUBTXT-UT                              
040800                             MOD-IDSKYLT-UT                               
040900                             MOD-IDRUBNR-UT                               
041000     CONTINUE.                                                            
041100     EJECT                                                                
041200* IMS SEKTIONER                                                           
041300     SKIP3                                                                
041400 IMS-GET-MSG SECTION.                                                     
041500     MOVE '  QC' TO GODK-STATUSKODER                                      
041600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
041700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
041800     PERFORM IMS-STATUSKONTROLL                                           
041900     CONTINUE.                                                            
042000     SKIP3                                                                
042100 IMS-INSERT-MSG SECTION.                                                  
042200     IF ENGLISH-TEXT                                                      
042300       MOVE 'N' TO MFS-KDHUVOMR                                           
042400     END-IF                                                               
042500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
042600     MOVE SPACE TO GODK-STATUSKODER                                       
042700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
042800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
042900     PERFORM IMS-STATUSKONTROLL                                           
043000     CONTINUE.                                                            
043100     EJECT                                                                
043200 IMS-GET-KATC01 SECTION.                                                  
043300     STRING 'WLKATC01(WDN2A1KY=>' W-IDSKYLT-X  W-IDRUBNR-MIN-X            
043400                     W-IDSEGMNR-LOW-X                                     
043500                    '&WDN2A1KY <' W-IDSKYLT-X  W-IDRUBNR-MAX-X            
043600                     W-IDSEGMNR-HIGH-X ')'                                
043700            DELIMITED BY SIZE INTO SSA1                                   
043800     MOVE '  GBGE' TO GODK-STATUSKODER                                    
043900     CALL CBLTDLI USING GN KATC-PCB DLI-IO-AREA SSA1                      
044000     MOVE KATC-STATUS-CODE TO STATUS-WS                                   
044100     PERFORM IMS-STATUSKONTROLL                                           
044200     CONTINUE.                                                            
044300     SKIP3                                                                
044400 IMS-GET-KATB01 SECTION.                                                  
044500     STRING 'WLKATB01(IDRUBNR  =' W-IDRUBNR-X ')'                         
044600            DELIMITED BY SIZE INTO SSA1                                   
044700     MOVE '  ' TO GODK-STATUSKODER                                        
044800     CALL CBLTDLI USING GU KATB-PCB DLI-IO-AREA SSA1                      
044900     MOVE KATB-STATUS-CODE TO STATUS-WS                                   
045000     PERFORM IMS-STATUSKONTROLL                                           
045100     CONTINUE.                                                            
045200     SKIP3                                                                
045300 IMS-STATUSKONTROLL SECTION.                                              
045400     SET STATUS-IX TO 1                                                   
045500     SEARCH GODK-STATUS                                                   
045510       AT END                                                             
045520         CALL FELLOG                                                      
045600     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
045700     END-SEARCH                                                           
045800     CONTINUE                                                             
045900            CONTINUE.                                                     
046000 IMS-STATUSKONTROLL-EXIT. EXIT.                                           
046100     CONTINUE.                                                            
