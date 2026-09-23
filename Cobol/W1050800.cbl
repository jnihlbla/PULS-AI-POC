000400 ID DIVISION.                                                             
000500     SKIP2                                                                
000600 PROGRAM-ID.     W1050800.                                                
001000*AUTHOR.         BODIL LINDAHL.                                           
001100*DATE-WRITTEN.   OKTOBER 1984.                                            
001200                                                                          
001400*                                                                         
001500*    FUNKTION.                                                            
001600*        SÖKNING FOTNOTSTEXT ELLER DEL AV FOTNOTSTEXT                     
001700*        PÅ VALT SPRÅK OCH FORDONSSLAG SAMT OM FOTNOTEN                   
001800*        FÅR ÖVERSÄTTAS ELLER INTE.                                       
001900*                                                                         
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSAKTION: W1T508                                              
002300*        MID:         W1I50801                                            
002400*                                                                         
002500*    UTDATA.                                                              
002600*        MOD:         W1O50801                                            
002700*                                                                         
002800*    SUBPROGRAM.                                                          
002900*        CBLTDLI                                                          
003000*        FELLOG                                                           
003100*        W009LTXT                                                         
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     SKIP3                                                                
003500 DATA DIVISION.                                                           
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003710                                                                          
003800*    -- CHECKED BY WY2000                                                 
003900 77  IDPGM                       PIC X(8)    VALUE 'W1050800'.            
004500 77    JA                        PIC X       VALUE 'J'.                   
004600 77    NEJ                       PIC X       VALUE 'N'.                   
004700 77    SVENSK                    PIC X(3)    VALUE 'S  '.                 
004800 77    WS-BEFOTNOT               PIC X(25)   VALUE SPACE.                 
004900 77    WS-IDSKYLT                PIC X(3)    VALUE SPACE.                 
005000 77    SPAR-IDSKYLT              PIC X(3)    VALUE SPACE.                 
005300 77    WS-IDFOTNR-SPAR           PIC S9(5)   VALUE ZERO  COMP-3.          
005400 77    WS-IDFOTNR                PIC X(5)    VALUE SPACE.                 
005500 77    SOEK-IDFOTNR              PIC 9(5)    VALUE ZERO.                  
005600 77    WS-RADSUM                 PIC S9(5)   VALUE ZERO  COMP-3.          
005700 77    NYCKLAR-RETT              PIC X       VALUE 'J'.                   
005800 77    LAS-VIDARE                PIC X       VALUE 'J'.                   
005900 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
006000 77    RAD-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
006100 77    SPAR-IX                   PIC S9(9)   VALUE +0   COMP SYNC.        
006200 77    MAX-RADER                 PIC S9(3)   VALUE +14.                   
006300 77    MAX-RADER-PLUS-1          PIC S9(3)   VALUE +15.                   
006400 77    MAX-RADER-PLUS-2          PIC S9(3)   VALUE +16.                   
006500 77    SPAR-IX-PLUS-1            PIC S9(3)   VALUE +7.                    
006600 77    MAX-MOD-LAENGD            PIC S9(4)  VALUE +1014 COMP SYNC.        
006610 77    FAELT-LAENGD              PIC S9(4)  VALUE +330 COMP.              
006700     SKIP2                                                                
006800 01    WS-IDTRANS                PIC X(4).                                
006900       88  EGEN-BILD             VALUE '1508'.                            
007000       88  GODKAEND-BILD         VALUE '1501' '1502' '1503'               
007100                                       '1505' '1506'                      
007200                                       '1508' '1509'.                     
007300     EJECT                                                                
007400 01    SPARAD-BEFOTNOT.                                                   
007500       03  SPAR-BEFOTNOT         PIC X(55)   OCCURS 6.                    
007600     SKIP3                                                                
007700 01    LITEN-BOKSTAV             PIC X(28)                                
007800             VALUE 'abcdefghijklmnopqrstuvxyzåäö'.                        
007900     SKIP3                                                                
008000 01    STOR-BOKSTAV              PIC X(28)                                
008100             VALUE 'ABCDEFGHIJKLMNOPQRSTUVXYZÅÄÖ'.                        
008200     SKIP3                                                                
008300 01  DYNAMISKA-SUBPROGRAM.                                                
008400     03  W009LTXT            PIC X(8)  VALUE 'W009LTXT'.                  
008430     03  CBLTDLI             PIC X(8)  VALUE 'CBLTDLI '.                  
008440     03  FELLOG              PIC X(8)  VALUE 'FELLOG  '.                  
008500     EJECT                                                                
008600*01  -COPY WWLAND03                                                       
008800     EJECT                                                                
008900*01  -COPY W009W041                                                       
009100     EJECT                                                                
009200 01    NYCKLAR-TILL-DLI.                                                  
009300   03    W-IDSKYLT-X.                                                     
009400     05    W-IDSKYLT             PIC X(3)    VALUE SPACE.                 
009500   03    W-KDFORDON-X.                                                    
009600     05    W-KDFORDON            PIC X(2)    VALUE 'PV'.                  
009900   03    W-IDFOTNR-X.                                                     
010000     05    W-IDFOTNR             PIC S9(5)   VALUE ZERO  COMP-3.          
010100   03    W-IDFOTNR-MAX-X.                                                 
010200     05    FILLER                PIC S9(5)   VALUE +99999 COMP-3.         
010300   03    W-IDSEGMNR-LOW-X.                                                
010400     05    FILLER                PIC S9(1)   VALUE ZERO   COMP-3.         
010500   03    W-IDSEGMNR-HIGH-X.                                               
010600     05    FILLER                PIC S9(1)   VALUE +9     COMP-3.         
010700     SKIP3                                                                
010800*                                                                         
010900 01  FILLER                 PIC X(16) VALUE  'MEDDELANDE'.                
011000 01  MEDDELANDE-AREA.                                                     
011100   03 MEDD-1.                                                             
011200     05  FILLER                  PIC X(26)   VALUE                        
011300             'TRYCK PF8 FÖR FLER RADER  '.                                
011400     05  FILLER                  PIC X(26)   VALUE                        
011500             'PRESS PF8 FOR MORE LINES  '.                                
011600   03 FILLER REDEFINES MEDD-1.                                            
011700     04  MED-1 OCCURS 2 PIC X(26).                                        
011800*                                                                         
011900   03 MEDD-2.                                                             
012000     05  FILLER                  PIC X(26)   VALUE                        
012100             'FÖRSTA SIDAN VISAS        '.                                
012200     05  FILLER                  PIC X(26)   VALUE                        
012300             'FIRST PAGE IS SHOWN       '.                                
012400   03 FILLER REDEFINES MEDD-2.                                            
012500     04  MED-2 OCCURS 2    PIC X(26).                                     
012600*                                                                         
012700   03 FEL-01.                                                             
012800     05  FILLER                  PIC X(26)   VALUE                        
012900             'NYCKLAR FEL !             '.                                
013000     05  FILLER                  PIC X(26)   VALUE                        
013100             'WRONG KEYS  !             '.                                
013200   03 FILLER REDEFINES FEL-01.                                            
013300     04  FEL-1 OCCURS 2    PIC X(26).                                     
013400*                                                                         
013500   03 FEL-03.                                                             
013600     05  FILLER                  PIC X(26)   VALUE                        
013700             'TEXT SAKNAS               '.                                
013800     05  FILLER                  PIC X(26)   VALUE                        
013900             'NO TEXT FOUND             '.                                
014000   03 FILLER REDEFINES FEL-03.                                            
014100     04  FEL-3 OCCURS 2    PIC X(26).                                     
014200*                                                                         
014300   03 FEL-04.                                                             
014400     05  FILLER                  PIC X(26)   VALUE                        
014500             'MATA IN NYCKLAR !         '.                                
014600     05  FILLER                  PIC X(26)   VALUE                        
014700             'TYPE IN KEYS !            '.                                
014800   03 FILLER REDEFINES FEL-04.                                            
014900     04  FEL-4 OCCURS 2    PIC X(26).                                     
015000*                                                                         
015100     EJECT                                                                
015200******************************************************************        
015300*                                                                         
015400*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
015500*                                                                         
015600 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
015700     SKIP3                                                                
015800*01    MID -COPY W1I50801.                                                
016000     EJECT                                                                
016100*01    -COPY WMSGAREA                                                     
016300     EJECT                                                                
016400*  03    MOD -COPY W1O50801  -RED MSG-AREA.                               
016600     EJECT                                                                
016700*01    -COPY WMFSAREA                                                     
016900     EJECT                                                                
017000******************************************************************        
017100*                                                                         
017200*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017300*                                                                         
017400 01    IMS-WS.                                                            
017500   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
017600     SKIP3                                                                
017700*                        **** STATUS-KOD FRÅN IMS                         
017800   03    STATUS-WS               PIC XX.                                  
017900     88    SEGMENT-FINNS                     VALUE '  '.                  
018000     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
018100     88    BASEN-SLUT                        VALUE 'GB'.                  
018200     SKIP3                                                                
018300   03    GODK-STATUSKODER.                                                
018400     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
018500     SKIP3                                                                
018600 01    SSA1                      PIC X(64).                               
018700     EJECT                                                                
018800*                            IMS FUNKTIONSKODER                           
018900*01    -COPY W0003                                                        
019100     EJECT                                                                
019200*                            DLI INPUT-OUTPUT AREA                        
019300 01    DLI-IO-AREA.                                                       
019400   03    IO-AREA                 PIC X(100)  VALUE SPACE.                 
019500     SKIP3                                                                
019600*  03    WLKATG01 -COPY WDN3A1  -PRE KATG-  -RED IO-AREA.                 
019800     EJECT                                                                
019900 LINKAGE SECTION.                                                         
020000*01    -COPY W0009     -PRE MSG-                                          
020200     EJECT                                                                
020300*01    -COPY W0008     -PRE KATG-                                         
020500     05  FILLER                  PIC X.                                   
020600     EJECT                                                                
020700 PROCEDURE DIVISION USING MSG-PCB KATG-PCB.                               
020800     ENTRY 'DLITCBL' USING MSG-PCB KATG-PCB.                              
020900     SKIP2                                                                
021000     PERFORM IMS-GET-MSG                                                  
021100     IF SEGMENT-FINNS                                                     
021200       PERFORM A-INIT-SPARA-INPUT                                         
021300       IF GODKAEND-BILD                                                   
021400         PERFORM B-KOLLA-NYCKLAR                                          
021500         IF NYCKLAR-RETT = JA                                             
021600           IF EGEN-BILD                                                   
021700             PERFORM D-KOLLA-PFTANGENTER                                  
021800           ELSE                                                           
021900             MOVE ZERO TO WS-IDFOTNR-SPAR                                 
022000           END-IF                                                         
022100           PERFORM C-LAS-BASEN                                            
022200         ELSE                                                             
022300           MOVE FEL-1(INDX) TO MOD-TEMFSFEL                               
022400         END-IF                                                           
022500       ELSE                                                               
022600         MOVE FEL-4(INDX) TO MOD-TEMFSFEL                                 
022700         MOVE SPACE TO WS-BEFOTNOT                                        
022800                       WS-IDSKYLT                                         
023100         MOVE MFS-RENSA-FAELT TO MOD-IDFOTNR-SPAR                         
023200       END-IF                                                             
023300       MOVE WS-BEFOTNOT TO MOD-BEFOTNOT-SOEK-UT                           
024100       IF WS-IDFOTNR-SPAR = ZERO                                          
024200         MOVE MFS-RENSA-FAELT TO MOD-IDFOTNR-SPAR                         
024300       ELSE                                                               
024400         MOVE WS-IDFOTNR-SPAR TO MOD-IDFOTNR-SPAR                         
024500       END-IF                                                             
024600       MOVE WS-IDFOTNR TO MOD-IDFOTNR-UT                                  
024700       INSPECT MOD-IDFOTNR-UT REPLACING LEADING ZERO BY SPACE             
024710       MOVE WS-IDSKYLT TO MOD-IDSKYLT-UT                                  
024800       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
024900       PERFORM IMS-INSERT-MSG                                             
025000     END-IF                                                               
025100     MOVE ZERO TO RETURN-CODE                                             
025200     GOBACK                                                               
025300     CONTINUE.                                                            
025400     EJECT                                                                
025500 A-INIT-SPARA-INPUT SECTION.                                              
025600     SKIP2                                                                
025700     IF MSG-DUBBLA-TRANSKODER                                             
025800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I50801                 
025900       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
026000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
026100       MOVE MSG-IDPFK TO MFS-IDPFK                                        
026200     ELSE                                                                 
026300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I50801                  
026400       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
026500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
026600       MOVE ' ' TO MFS-IDPFK                                              
026700     END-IF                                                               
026800     MOVE MFS-IDTRANS TO WS-IDTRANS                                       
026900                                                                          
027000     MOVE LOW-VALUE TO MSG-AREA                                           
027100     MOVE 'W1O50801' TO MFS-IDMOD                                         
027200     MOVE '1508' TO MOD-IDTRANS                                           
027300     IF SWEDISH-TEXT                                                      
027400       MOVE +1 TO INDX                                                    
027500     ELSE                                                                 
027600       MOVE +2 TO INDX                                                    
028000     END-IF                                                               
028100     MOVE MFS-RENSA-FAELT TO MOD-BEFOTNOT-SOEK-IN                         
028200                             MOD-IDSKYLT-IN                               
028500                             MOD-IDFOTNR-IN                               
028600                             MOD-TEMFSINF                                 
028700                             MOD-TEMFSFEL                                 
028800     CONTINUE.                                                            
028900     EJECT                                                                
029000 B-KOLLA-NYCKLAR SECTION.                                                 
029100                                                                          
029200     MOVE JA TO NYCKLAR-RETT                                              
029300                                                                          
029400     IF MID-IDSKYLT-IN = ALL '+'                                          
029500       IF MID-IDSKYLT-UT = SPACE                                          
029600         MOVE SVENSK TO WS-IDSKYLT                                        
029700         MOVE '7' TO MFS-IDPFK                                            
029800       ELSE                                                               
029900         MOVE MID-IDSKYLT-UT TO WS-IDSKYLT                                
030000       END-IF                                                             
030100     ELSE                                                                 
030200       MOVE MID-IDSKYLT-IN TO WS-IDSKYLT                                  
030300       MOVE '7' TO MFS-IDPFK                                              
030400     END-IF                                                               
030600     INSPECT WS-IDSKYLT CONVERTING LITEN-BOKSTAV TO STOR-BOKSTAV          
030700     SET WWLAND03-IX TO +1                                                
030800     SEARCH WWLAND03-IDSKYLT-RAD                                          
030900     AT END MOVE NEJ TO NYCKLAR-RETT                                      
031000     WHEN WWLAND03-IDSKYLT(WWLAND03-IX) = WS-IDSKYLT                      
031100     MOVE JA TO NYCKLAR-RETT                                              
031200     END-SEARCH                                                           
031300                                                                          
031400     IF MID-BEFOTNOT-SOEK-IN = ALL '+'                                    
031500       MOVE MID-BEFOTNOT-SOEK-UT TO WS-BEFOTNOT                           
031600     ELSE                                                                 
031700       MOVE MID-BEFOTNOT-SOEK-IN TO WS-BEFOTNOT                           
031800       MOVE '7'  TO MFS-IDPFK                                             
031900     END-IF                                                               
032000     IF WS-BEFOTNOT = SPACE                                               
032100       MOVE NEJ TO NYCKLAR-RETT                                           
032200     END-IF                                                               
033200                                                                          
036200     IF MID-IDFOTNR-IN = ALL '+'                                          
036300       MOVE MID-IDFOTNR-UT TO WS-IDFOTNR                                  
036400     ELSE                                                                 
036500       MOVE MID-IDFOTNR-IN TO WS-IDFOTNR                                  
036600       INSPECT WS-IDFOTNR REPLACING LEADING SPACE BY ZERO                 
036700     END-IF                                                               
036800     .                                                                    
036900     EJECT                                                                
037000 C-LAS-BASEN SECTION.                                                     
037100     SKIP2                                                                
037200     MOVE SPACE TO SPARAD-BEFOTNOT                                        
037300     MOVE JA TO LAS-VIDARE                                                
037400     MOVE 1 TO RAD-IX                                                     
037500     MOVE 1 TO SPAR-IX                                                    
037600                                                                          
037700     MOVE WS-IDFOTNR-SPAR TO W-IDFOTNR                                    
037800     MOVE WS-IDSKYLT TO W-IDSKYLT                                         
038900     PERFORM IMS-GET-KATG01                                               
039000     PERFORM UNTIL                                                        
039100      NOT ( SEGMENT-FINNS AND LAS-VIDARE = JA )                           
039200       MOVE KATG-FOTA-BEFOTNOT TO SPAR-BEFOTNOT(SPAR-IX)                  
039300       MOVE KATG-FOTA-IDFOTNR TO SOEK-IDFOTNR                             
039400       ADD +1 TO SPAR-IX                                                  
039500       PERFORM IMS-GET-KATG01                                             
039600       PERFORM UNTIL                                                      
039700        NOT ( SEGMENT-FINNS AND KATG-FOTA-IDFOTNR = SOEK-IDFOTNR          
039800         )                                                                
039900         MOVE KATG-FOTA-BEFOTNOT TO SPAR-BEFOTNOT(SPAR-IX)                
040000         ADD +1 TO SPAR-IX                                                
040100         PERFORM IMS-GET-KATG01                                           
040200       END-PERFORM                                                        
040300       PERFORM CC-SOEK-TEXT                                               
040400       IF W041-OK                                                         
040500         COMPUTE WS-RADSUM = RAD-IX + SPAR-IX                             
040600         IF WS-RADSUM > MAX-RADER-PLUS-2                                  
040700           MOVE MED-1(INDX) TO MOD-TEMFSINF                               
040800           MOVE SOEK-IDFOTNR TO WS-IDFOTNR-SPAR                           
040900           MOVE NEJ TO LAS-VIDARE                                         
041000         ELSE                                                             
041100           PERFORM CA-FLYTTA-TILL-MOD                                     
041200           MOVE ZERO TO WS-IDFOTNR-SPAR                                   
041300         END-IF                                                           
041400       ELSE                                                               
041500         MOVE ZERO TO WS-IDFOTNR-SPAR                                     
041600       END-IF                                                             
041700       MOVE SPACE TO SPARAD-BEFOTNOT                                      
041800       MOVE +1 TO SPAR-IX                                                 
041900     END-PERFORM                                                          
042000     IF SEGMENT-SAKNAS OR BASEN-SLUT                                      
042100       IF RAD-IX = 1                                                      
042200         MOVE FEL-3(INDX) TO MOD-TEMFSFEL                                 
042300         MOVE ZERO TO WS-IDFOTNR-SPAR                                     
042400       END-IF                                                             
042500       PERFORM CB-RENSA-FAELT                                             
042600     END-IF                                                               
042700     CONTINUE.                                                            
042800     EJECT                                                                
042900 CA-FLYTTA-TILL-MOD SECTION.                                              
043000     SKIP2                                                                
043100     MOVE +1 TO SPAR-IX                                                   
043200     MOVE SOEK-IDFOTNR TO MOD-IDFOTNR(RAD-IX)                             
043300     PERFORM UNTIL                                                        
043400      NOT ( SPAR-IX < SPAR-IX-PLUS-1 )                                    
043500       IF SPAR-BEFOTNOT(SPAR-IX) NOT = SPACE                              
043600         MOVE SPAR-BEFOTNOT(SPAR-IX) TO MOD-BEFOTNOT(RAD-IX)              
043700         ADD +1 TO RAD-IX                                                 
043800       END-IF                                                             
043900       ADD +1 TO SPAR-IX                                                  
044000     END-PERFORM                                                          
044100     CONTINUE.                                                            
044200     EJECT                                                                
044300 CB-RENSA-FAELT SECTION.                                                  
044400     SKIP2                                                                
044500     PERFORM UNTIL                                                        
044600      NOT ( RAD-IX < MAX-RADER-PLUS-1 )                                   
044700       MOVE MFS-RENSA-FAELT TO MOD-IDFOTNR(RAD-IX)                        
044800       MOVE MFS-RENSA-FAELT TO MOD-BEFOTNOT(RAD-IX)                       
044900       ADD +1 TO RAD-IX                                                   
045000     END-PERFORM                                                          
045100     CONTINUE.                                                            
045200     EJECT                                                                
045300 CC-SOEK-TEXT SECTION.                                                    
045400     SKIP2                                                                
045500     MOVE WS-BEFOTNOT TO W041-BESORD                                      
045510     MOVE FAELT-LAENGD TO W041-DIFAELT                                    
045600     MOVE SPARAD-BEFOTNOT TO W041-BESTEXT                                 
045700     CALL W009LTXT USING W041-W009W041                                    
045800     CONTINUE.                                                            
045900     EJECT                                                                
046000 D-KOLLA-PFTANGENTER SECTION.                                             
046100     SKIP2                                                                
046200     IF MFS-IDPFK = '7'                                                   
046300       MOVE ZERO TO WS-IDFOTNR-SPAR                                       
046400     ELSE                                                                 
046500       EVALUATE TRUE                                                      
046600       WHEN MFS-IDPFK = '8'                                               
046700         IF MID-IDFOTNR-SPAR NUMERIC                                      
046800           IF MID-IDFOTNR-SPAR > ZERO                                     
046900             CONTINUE                                                     
047000           ELSE                                                           
047100             MOVE MED-2(INDX) TO MOD-TEMFSINF                             
047200           END-IF                                                         
047300           MOVE MID-IDFOTNR-SPAR TO WS-IDFOTNR-SPAR                       
047400         ELSE                                                             
047500           MOVE ZERO TO WS-IDFOTNR-SPAR                                   
047600           MOVE MED-2(INDX) TO MOD-TEMFSINF                               
047700         END-IF                                                           
047800        WHEN OTHER                                                        
047900         IF MID-IDFOTNR NUMERIC                                           
048000           MOVE MID-IDFOTNR TO WS-IDFOTNR-SPAR                            
048100         ELSE                                                             
048200           MOVE ZERO TO WS-IDFOTNR-SPAR                                   
048300           MOVE MED-2(INDX) TO MOD-TEMFSINF                               
048400         END-IF                                                           
048500       END-EVALUATE                                                       
048600     END-IF                                                               
048700     CONTINUE.                                                            
048800     EJECT                                                                
048900* IMS SEKTIONER                                                           
049000     SKIP3                                                                
049100 IMS-GET-MSG SECTION.                                                     
049200     MOVE '  QC' TO GODK-STATUSKODER                                      
049300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
049400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
049500     PERFORM IMS-STATUSKONTROLL                                           
049600     CONTINUE.                                                            
049700     SKIP3                                                                
049800 IMS-INSERT-MSG SECTION.                                                  
049900     IF ENGLISH-TEXT                                                      
050000       MOVE 'N' TO MFS-KDHUVOMR                                           
050100     END-IF                                                               
050200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
050300     MOVE SPACE TO GODK-STATUSKODER                                       
050400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
050500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
050600     PERFORM IMS-STATUSKONTROLL                                           
050700     CONTINUE.                                                            
050800     EJECT                                                                
050900 IMS-GET-KATG01 SECTION.                                                  
051000     STRING 'WLKATG01(WDN3A1KY=>' W-IDSKYLT-X  W-KDFORDON-X               
051100                      W-IDFOTNR-X W-IDSEGMNR-LOW-X                        
051300                     '&WDN3A1KY <' W-IDSKYLT-X W-KDFORDON-X               
051500                      W-IDFOTNR-MAX-X  W-IDSEGMNR-HIGH-X ')'              
051600            DELIMITED BY SIZE INTO SSA1                                   
051700     MOVE '  GBGE' TO GODK-STATUSKODER                                    
051800     CALL CBLTDLI USING GN KATG-PCB DLI-IO-AREA SSA1                      
051900     MOVE KATG-STATUS-CODE TO STATUS-WS                                   
052000     PERFORM IMS-STATUSKONTROLL                                           
052100     CONTINUE.                                                            
052200     SKIP3                                                                
052300 IMS-STATUSKONTROLL SECTION.                                              
052400     SET STATUS-IX TO 1                                                   
052500     SEARCH GODK-STATUS                                                   
052510       AT END                                                             
052520         CALL FELLOG                                                      
052600     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
052700     END-SEARCH                                                           
052900     CONTINUE.                                                            
