000821 ID DIVISION.                                                             
000900 PROGRAM-ID.     W1050100.                                                
001000 AUTHOR.         BODIL LINDAHL.                                           
001100 DATE-WRITTEN.   NOVEMBER 1983.                                           
001110 DATE-compiled.                                                           
001200                                                                          
001400*                                                                         
001500*    FUNKTION.                                                            
001600*        SÖKNING RUBRIKTEXT                                               
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSAKTION: W1T501                                              
002300*        MID:         W1I50101                                            
002400*                                                                         
002500*    UTDATA.                                                              
002600*        MOD:         W1O50101                                            
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
004210 77  IDPGM                     PIC X(8)    VALUE 'W1050100'.              
004500 77  JA                        PIC X       VALUE 'J'.                     
004600 77  NEJ                       PIC X       VALUE 'N'.                     
004700 77  SVENSK                    PIC X(3)    VALUE 'S  '.                   
004800 77  WS-BERUBTXT               PIC X(25)   VALUE SPACE.                   
004900 77  WS-IDSKYLT                PIC X(3)    VALUE SPACE.                   
005000 77  WS-IDRUBNR                PIC X(5)    VALUE SPACE.                   
005100 77  WS-FLKOMBINERAS           PIC X(1)    VALUE SPACE.                   
005300 77  WS-IDRUBNR-SPAR           PIC 9(5)    VALUE ZERO.                    
005400 77  SOEK-IDRUBNR              PIC 9(5)    VALUE ZERO.                    
005500 77  SPAR-STATUS-WS            PIC X(2)    VALUE SPACE.                   
005600 77  LAS-VIDARE                PIC X       VALUE 'J'.                     
005700 77  NYCKLAR-RETT              PIC X       VALUE 'J'.                     
005800 77  INDX                      PIC S9(9)   VALUE +0   COMP SYNC.          
005900 77  RAD-IX                    PIC S9(9)   VALUE +0   COMP SYNC.          
006000 77  TEXT-IX                   PIC S9(9)   VALUE +0   COMP SYNC.          
006100 77  SPAR-IX                   PIC S9(9)   VALUE +0   COMP SYNC.          
006200 77  MAX-RADER                 PIC S9(3)   VALUE +7.                      
006300 77  MAX-RADER-PLUS-1          PIC S9(3)   VALUE +8.                      
006400 77  MAX-TEXT                  PIC S9(3)   VALUE +3.                      
006500 77  MAX-TEXT-PLUS-1           PIC S9(3)   VALUE +4.                      
006600 77  MAX-SPAR                  PIC S9(3)   VALUE +3.                      
006700 77  MAX-SPAR-PLUS-1           PIC S9(3)   VALUE +4.                      
006800 77  MAX-MOD-LAENGD            PIC S9(4)   VALUE +930  COMP SYNC.         
006810 77  RUBTXT-LAENGD             PIC S9(4)   VALUE +90  COMP.               
006900     EJECT                                                                
007000 01    WS-IDTRANS           PIC X(4).                                     
007100   88  EGEN-BILD            VALUE '1501'.                                 
007200   88  GODKAEND-BILD        VALUE '1501' '1502' '1503'                    
007300                                  '1505' '1506'                           
007400                                  '1508' '1509'.                          
007500     SKIP2                                                                
007600 01    LITEN-BOKSTAV        PIC X(28)                                     
007700               VALUE 'abcdefghijklmnopqrstuvxyzåäö'.                      
007800 01    STOR-BOKSTAV         PIC X(28)                                     
007900               VALUE 'ABCDEFGHIJKLMNOPQRSTUVXYZÅÄÖ'.                      
008000     SKIP2                                                                
008100 01  SPARAD-BERUBTXT.                                                     
008200     03  SPAR-BERUBTXT         PIC X(30)   OCCURS 3.                      
008300     SKIP3                                                                
008400 01  DYNAMISKA-SUBPROGRAM.                                                
008500     03  W009LTXT              PIC X(8)    VALUE 'W009LTXT'.              
008530     03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.              
008540     03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.              
008600     EJECT                                                                
008700*01  -COPY WWLAND03                                                       
008900     EJECT                                                                
009000*01  -COPY W009W041                                                       
009200     EJECT                                                                
009300 01    NYCKLAR-TILL-DLI.                                                  
009400   03    W-IDSKYLT-X.                                                     
009500     05    W-IDSKYLT             PIC X(3)    VALUE SPACE.                 
009600   03    W-IDRUBNR-X.                                                     
009700     05    W-IDRUBNR             PIC S9(5)   VALUE ZERO  COMP-3.          
009800   03    W-IDRUBNR-MAX-X.                                                 
009900     05    FILLER                PIC S9(5)   VALUE +99999 COMP-3.         
010000   03    W-IDSEGMNR-LOW-X.                                                
010100     05    FILLER                PIC S9(1)   VALUE ZERO   COMP-3.         
010200   03    W-IDSEGMNR-HIGH-X.                                               
010300     05    FILLER                PIC S9(1)   VALUE +9     COMP-3.         
010400     SKIP3                                                                
010500 01  MEDDELANDE-AREA.                                                     
010600*                                                                         
010700     03 MEDD-1.                                                           
010800        05 FILLER                PIC X(26)   VALUE                        
010900                  'TRYCK PF8 FÖR FLER RADER  '.                           
011000        05 FILLER                PIC X(26)   VALUE                        
011100                  'PRESS PF8 FOR MORE LINES  '.                           
011200     03 FILLER REDEFINES MEDD-1.                                          
011300        05 MED-1 OCCURS 2        PIC X(26).                               
011400*                                                                         
011500     03 MEDD-2.                                                           
011600        05 FILLER                PIC X(26)   VALUE                        
011700                  'FÖRSTA SIDAN VISAS        '.                           
011800        05 FILLER                PIC X(26)   VALUE                        
011900                  'FIRST PAGE IS SHOWN       '.                           
012000     03 FILLER REDEFINES MEDD-2.                                          
012100        05 MED-2 OCCURS 2        PIC X(26).                               
012200*                                                                         
012300     EJECT                                                                
012400*                                                                         
012500     03 FEL-01.                                                           
012600        05 FILLER                PIC X(26)   VALUE                        
012700                  'NYCKLAR FEL               '.                           
012800        05 FILLER                PIC X(26)   VALUE                        
012900                  'WRONG KEY(S)              '.                           
013000     03 FILLER REDEFINES FEL-01.                                          
013100        05 FEL-1 OCCURS 2        PIC X(26).                               
013200*                                                                         
013300     03 FEL-02.                                                           
013400        05 FILLER                PIC X(26)   VALUE                        
013500                  'TEXT SAKNAS               '.                           
013600        05 FILLER                PIC X(26)   VALUE                        
013700                  'TEXT IS MISSING           '.                           
013800     03 FILLER REDEFINES FEL-02.                                          
013900        05 FEL-2 OCCURS 2        PIC X(26).                               
014000*                                                                         
014100     03 FEL-03.                                                           
014200        05 FILLER                PIC X(26)   VALUE                        
014300                  'MATA IN NYCKLAR           '.                           
014400        05 FILLER                PIC X(26)   VALUE                        
014500                  'PLEASE, TYPE IN KEYS      '.                           
014600     03 FILLER REDEFINES FEL-03.                                          
014700        05 FEL-3 OCCURS 2        PIC X(26).                               
014800*                                                                         
014900     EJECT                                                                
015000******************************************************************        
015100*                                                                         
015200*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
015300*                                                                         
015400 01  FILLER                    PIC X(16)   VALUE 'MFS-WS'.                
015500     SKIP3                                                                
015600*01    MID -COPY W1I50101.                                                
015800     EJECT                                                                
015900*01    -COPY WMSGAREA                                                     
016100     EJECT                                                                
016200*    03  MOD -COPY W1O50101  -RED MSG-AREA.                               
016400     EJECT                                                                
016500*01    -COPY WMFSAREA                                                     
016700     EJECT                                                                
016800******************************************************************        
016900*                                                                         
017000*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017100*                                                                         
017200 01    IMS-WS.                                                            
017300   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
017400     SKIP3                                                                
017500*                        **** STATUS-KOD FRÅN IMS                         
017600   03    STATUS-WS               PIC XX.                                  
017700     88    SEGMENT-FINNS                     VALUE '  '.                  
017800     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
017900     88    BASEN-SLUT                        VALUE 'GB'.                  
018000     SKIP3                                                                
018100   03    GODK-STATUSKODER.                                                
018200     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
018300     SKIP3                                                                
018400 01    SSA1                      PIC X(120).                              
018500 01    SSA2                      PIC X(120).                              
018600     EJECT                                                                
018700*                            IMS FUNKTIONSKODER                           
018800*01    -COPY W0003                                                        
019000     EJECT                                                                
019100*                            DLI INPUT-OUTPUT AREA                        
019200 01    DLI-IO-AREA-1.                                                     
019300   03    IO-AREA-1               PIC X(200)  VALUE SPACE.                 
019400     SKIP3                                                                
019500*  03    WLKATC01 -COPY WDN2A1  -PRE KATC-  -RED IO-AREA-1.               
019700     EJECT                                                                
019800 01    DLI-IO-AREA-2.                                                     
019900   03    IO-AREA-2               PIC X(200)  VALUE SPACE.                 
020000     SKIP3                                                                
020100*  03    WLKATB01 -COPY WDN201  -PRE KATB-  -RED IO-AREA-2.               
020300     EJECT                                                                
020400 LINKAGE SECTION.                                                         
020500*01    -COPY W0009     -PRE MSG-                                          
020700     EJECT                                                                
020800*01    -COPY W0008     -PRE KATB-                                         
021000     05  FILLER                  PIC X.                                   
021100     EJECT                                                                
021200*01    -COPY W0008     -PRE KATC-                                         
021400     05  FILLER                  PIC X.                                   
021500     EJECT                                                                
021600 PROCEDURE DIVISION USING MSG-PCB KATB-PCB KATC-PCB.                      
021610 MAIN SECTION.                                                            
021700     ENTRY 'DLITCBL' USING MSG-PCB KATB-PCB KATC-PCB.                     
021800                                                                          
021900     PERFORM IMS-GET-MSG                                                  
022000     IF SEGMENT-FINNS                                                     
022100       PERFORM A-INIT-SPARA-INPUT                                         
022200       IF GODKAEND-BILD                                                   
022300         PERFORM B-KOLLA-NYCKLAR                                          
022400         IF NYCKLAR-RETT = JA                                             
022500           IF EGEN-BILD                                                   
022600             PERFORM D-KOLLA-PFTANGENTER                                  
022700           ELSE                                                           
022800             MOVE ZERO TO WS-IDRUBNR-SPAR                                 
022900           END-IF                                                         
023000           PERFORM C-LAS-BASEN                                            
023100         ELSE                                                             
023200           MOVE FEL-1(INDX) TO MOD-TEMFSFEL                               
023300         END-IF                                                           
023400       ELSE                                                               
023500         MOVE FEL-3(INDX) TO MOD-TEMFSFEL                                 
023600         MOVE SPACE TO WS-BERUBTXT                                        
023900                       WS-IDSKYLT                                         
024000         MOVE MFS-RENSA-FAELT TO MOD-IDRUBNR-SPAR                         
024100       END-IF                                                             
024200       MOVE WS-BERUBTXT TO MOD-BERUBTXT-SOEK-UT                           
024300       MOVE WS-IDSKYLT TO MOD-IDSKYLT-UT                                  
024400       MOVE WS-IDRUBNR TO MOD-IDRUBNR-UT                                  
024500       INSPECT MOD-IDRUBNR-UT REPLACING LEADING ZERO BY SPACE             
024800       IF WS-IDRUBNR-SPAR = ZERO                                          
024900         MOVE MFS-RENSA-FAELT TO MOD-IDRUBNR-SPAR                         
025000       ELSE                                                               
025100         MOVE WS-IDRUBNR-SPAR TO MOD-IDRUBNR-SPAR                         
025200       END-IF                                                             
025300       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
025400       PERFORM IMS-INSERT-MSG                                             
025500     END-IF                                                               
025600     MOVE ZERO TO RETURN-CODE                                             
025700     GOBACK                                                               
025800     .                                                                    
025900     EJECT                                                                
026000 A-INIT-SPARA-INPUT SECTION.                                              
026100     SKIP2                                                                
026200     IF MSG-DUBBLA-TRANSKODER                                             
026300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I50101                 
026400       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
026500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
026600       MOVE MSG-IDPFK TO MFS-IDPFK                                        
026700     ELSE                                                                 
026800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I50101                  
026900       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
027000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
027100       MOVE ' ' TO MFS-IDPFK                                              
027200     END-IF                                                               
027300     MOVE MFS-IDTRANS TO WS-IDTRANS                                       
027400                                                                          
027500     MOVE LOW-VALUE TO MSG-AREA                                           
027600     MOVE 'W1O50101' TO MFS-IDMOD                                         
027700     MOVE '1501' TO MOD-IDTRANS                                           
027800     IF SWEDISH-TEXT                                                      
027900       MOVE +1 TO INDX                                                    
028000     ELSE                                                                 
028100       MOVE +2 TO INDX                                                    
028200     END-IF                                                               
028300     MOVE MFS-RENSA-FAELT TO MOD-BERUBTXT-SOEK-IN                         
028400                             MOD-IDSKYLT-IN                               
028700                             MOD-IDRUBNR-IN                               
028800                             MOD-TEMFSFEL                                 
028900                             MOD-TEMFSINF                                 
029000     .                                                                    
029100     EJECT                                                                
029200 B-KOLLA-NYCKLAR SECTION.                                                 
029300     SKIP2                                                                
029400     MOVE JA TO NYCKLAR-RETT                                              
029500                                                                          
029600     IF MID-IDSKYLT-IN = ALL '+'                                          
029700       IF MID-IDSKYLT-UT = SPACE                                          
029800         MOVE SVENSK TO WS-IDSKYLT                                        
029900         MOVE '7' TO MFS-IDPFK                                            
030000       ELSE                                                               
030100         MOVE MID-IDSKYLT-UT TO WS-IDSKYLT                                
030200       END-IF                                                             
030300     ELSE                                                                 
030400       MOVE MID-IDSKYLT-IN TO WS-IDSKYLT                                  
030500       INSPECT WS-IDSKYLT CONVERTING LITEN-BOKSTAV TO STOR-BOKSTAV        
030600       MOVE '7' TO MFS-IDPFK                                              
030700     END-IF                                                               
030800     SET WWLAND03-IX TO +1                                                
030900     SEARCH WWLAND03-IDSKYLT-RAD                                          
031000                  AT END MOVE NEJ TO NYCKLAR-RETT                         
031100                  WHEN WWLAND03-IDSKYLT(WWLAND03-IX)                      
031200                  = WS-IDSKYLT                                            
031300                  MOVE JA TO NYCKLAR-RETT                                 
031400     END-SEARCH                                                           
031500                                                                          
031600     IF MID-BERUBTXT-SOEK-IN = ALL '+'                                    
031700       MOVE MID-BERUBTXT-SOEK-UT TO WS-BERUBTXT                           
031800     ELSE                                                                 
031900       MOVE MID-BERUBTXT-SOEK-IN TO WS-BERUBTXT                           
032000       MOVE '7' TO MFS-IDPFK                                              
032100     END-IF                                                               
032200     IF WS-BERUBTXT = SPACE                                               
032300       MOVE NEJ TO NYCKLAR-RETT                                           
032400     END-IF                                                               
034200     IF MID-IDRUBNR-IN = ALL '+'                                          
034300       MOVE MID-IDRUBNR-UT TO WS-IDRUBNR                                  
034400     ELSE                                                                 
034500       MOVE MID-IDRUBNR-IN TO WS-IDRUBNR                                  
034600       INSPECT WS-IDRUBNR REPLACING LEADING SPACE BY ZERO                 
034700     END-IF                                                               
034800     .                                                                    
034900     EJECT                                                                
035000 C-LAS-BASEN SECTION.                                                     
035100     SKIP2                                                                
035200     MOVE SPACE TO SPARAD-BERUBTXT                                        
035300     MOVE JA TO LAS-VIDARE                                                
035400     MOVE 1 TO RAD-IX                                                     
035500     MOVE 1 TO TEXT-IX                                                    
035600     MOVE 1 TO SPAR-IX                                                    
035700                                                                          
035800     MOVE WS-IDRUBNR-SPAR TO W-IDRUBNR                                    
035900     MOVE WS-IDSKYLT TO W-IDSKYLT                                         
036000     PERFORM IMS-GET-KATC01                                               
036100     PERFORM UNTIL                                                        
036200      NOT ( SEGMENT-FINNS AND LAS-VIDARE = JA )                           
036300       MOVE KATC-RUBA-BERUBTXT TO SPAR-BERUBTXT(SPAR-IX)                  
036400       MOVE KATC-RUBA-IDRUBNR TO SOEK-IDRUBNR                             
036500       ADD +1 TO SPAR-IX                                                  
036600       PERFORM IMS-GET-KATC01                                             
036700       PERFORM UNTIL                                                      
036800        NOT ( SEGMENT-FINNS AND KATC-RUBA-IDRUBNR = SOEK-IDRUBNR          
036900         )                                                                
037000         MOVE KATC-RUBA-BERUBTXT TO SPAR-BERUBTXT(SPAR-IX)                
037100         ADD +1 TO SPAR-IX                                                
037200         PERFORM IMS-GET-KATC01                                           
037300       END-PERFORM                                                        
037400       PERFORM CC-SOEK-TEXT                                               
037500       IF W041-OK                                                         
037600         IF RAD-IX = MAX-RADER-PLUS-1                                     
037700           MOVE MED-1(INDX) TO MOD-TEMFSINF                               
037800           MOVE SOEK-IDRUBNR TO WS-IDRUBNR-SPAR                           
037900           MOVE NEJ TO LAS-VIDARE                                         
038000         ELSE                                                             
038100           MOVE STATUS-WS TO SPAR-STATUS-WS                               
038200           PERFORM CA-LAS-FLYTTA-TILL-MOD                                 
038300           MOVE SPAR-STATUS-WS TO STATUS-WS                               
038400           ADD +1 TO RAD-IX                                               
038500           MOVE 1 TO TEXT-IX                                              
038600           MOVE ZERO TO WS-IDRUBNR-SPAR                                   
038700         END-IF                                                           
038800       ELSE                                                               
038900         MOVE ZERO TO WS-IDRUBNR-SPAR                                     
039000       END-IF                                                             
039100       MOVE SPACE TO SPARAD-BERUBTXT                                      
039200       MOVE +1 TO SPAR-IX                                                 
039300     END-PERFORM                                                          
039400     IF SEGMENT-SAKNAS OR BASEN-SLUT                                      
039500       IF RAD-IX > 1                                                      
039600         CONTINUE                                                         
039700       ELSE                                                               
039800         MOVE FEL-2(INDX) TO MOD-TEMFSFEL                                 
039900         MOVE ZERO TO WS-IDRUBNR-SPAR                                     
040000       END-IF                                                             
040100       PERFORM CB-RENSA-FAELT                                             
040200     END-IF                                                               
040300     .                                                                    
040400     EJECT                                                                
040500 CA-LAS-FLYTTA-TILL-MOD SECTION.                                          
040600     SKIP2                                                                
040700     MOVE +1 TO SPAR-IX                                                   
040800     MOVE SOEK-IDRUBNR TO W-IDRUBNR                                       
040900     PERFORM IMS-GET-KATB01                                               
041000                                                                          
041100     IF ENGLISH-TEXT                                                      
041200       IF KATB-RUB-FLKOMBINERAS = 'J'                                     
041300         MOVE 'Y' TO MOD-FLKOMBINERAS(RAD-IX)                             
041400       ELSE                                                               
041500         MOVE KATB-RUB-FLKOMBINERAS TO MOD-FLKOMBINERAS(RAD-IX)           
041600       END-IF                                                             
041700       MOVE 'DATE ' TO MOD-DATUM-RUBRIK(RAD-IX)                           
041800     ELSE                                                                 
041900       MOVE KATB-RUB-FLKOMBINERAS TO MOD-FLKOMBINERAS(RAD-IX)             
042000       MOVE 'DATUM' TO MOD-DATUM-RUBRIK(RAD-IX)                           
042100     END-IF                                                               
042200     MOVE KATB-RUB-TIUPPDAT TO MOD-TIUPPDAT(RAD-IX)                       
042300     MOVE KATB-RUB-IDRUBNR TO MOD-IDRUBNR(RAD-IX)                         
042400     PERFORM UNTIL                                                        
042500      NOT ( SPAR-IX < MAX-SPAR-PLUS-1 )                                   
042600       MOVE SPAR-BERUBTXT(SPAR-IX)                                        
042700         TO MOD-BERUBTXT(RAD-IX TEXT-IX)                                  
042800       ADD +1 TO TEXT-IX                                                  
042900       ADD +1 TO SPAR-IX                                                  
043000     END-PERFORM                                                          
043100     .                                                                    
043200     EJECT                                                                
043300  CB-RENSA-FAELT SECTION.                                                 
043400     SKIP2                                                                
043500     PERFORM UNTIL                                                        
043600      NOT ( RAD-IX > 0 AND < MAX-RADER-PLUS-1 )                           
043700       MOVE MFS-RENSA-FAELT TO MOD-IDRUBNR(RAD-IX)                        
043800                               MOD-TIUPPDAT(RAD-IX)                       
043900                               MOD-FLKOMBINERAS(RAD-IX)                   
044000                               MOD-DATUM-RUBRIK(RAD-IX)                   
044100       PERFORM UNTIL                                                      
044200        NOT ( TEXT-IX > 0 AND < MAX-TEXT-PLUS-1 )                         
044300         MOVE MFS-RENSA-FAELT                                             
044400                    TO MOD-BERUBTXT(RAD-IX TEXT-IX)                       
044500         ADD +1 TO TEXT-IX                                                
044600       END-PERFORM                                                        
044700       ADD +1 TO RAD-IX                                                   
044800       MOVE +1 TO TEXT-IX                                                 
044900     END-PERFORM                                                          
045000     .                                                                    
045100     EJECT                                                                
045200 CC-SOEK-TEXT SECTION.                                                    
045300     SKIP2                                                                
045400     MOVE WS-BERUBTXT TO W041-BESORD                                      
045500     MOVE SPARAD-BERUBTXT TO W041-BESTEXT                                 
045510     MOVE RUBTXT-LAENGD TO W041-DIFAELT                                   
045600     CALL W009LTXT USING W041-W009W041                                    
045700     .                                                                    
045800     EJECT                                                                
045900 D-KOLLA-PFTANGENTER SECTION.                                             
046000     SKIP2                                                                
046100     IF MFS-IDPFK = '7'                                                   
046200       MOVE ZERO TO WS-IDRUBNR-SPAR                                       
046300     ELSE                                                                 
046400       EVALUATE TRUE                                                      
046500       WHEN MFS-IDPFK = '8'                                               
046600         IF MID-IDRUBNR-SPAR NUMERIC                                      
046700           IF MID-IDRUBNR-SPAR > ZERO                                     
046800*             CONTINUE                                                    
046900             CONTINUE                                                     
047000           ELSE                                                           
047100             MOVE MED-2(INDX) TO MOD-TEMFSINF                             
047200           END-IF                                                         
047300           MOVE MID-IDRUBNR-SPAR TO WS-IDRUBNR-SPAR                       
047400         ELSE                                                             
047500           MOVE ZERO TO WS-IDRUBNR-SPAR                                   
047600           MOVE MED-2(INDX) TO MOD-TEMFSINF                               
047700         END-IF                                                           
047800        WHEN OTHER                                                        
047900         IF MID-IDRUBNR NUMERIC                                           
048000           MOVE MID-IDRUBNR TO WS-IDRUBNR-SPAR                            
048100         ELSE                                                             
048200           MOVE ZERO TO WS-IDRUBNR-SPAR                                   
048300           MOVE MED-2(INDX) TO MOD-TEMFSINF                               
048400         END-IF                                                           
048500       END-EVALUATE                                                       
048600     END-IF                                                               
048700     .                                                                    
048800     EJECT                                                                
048900* IMS SEKTIONER                                                           
049000     SKIP3                                                                
049100 IMS-GET-MSG SECTION.                                                     
049200     MOVE '  QC' TO GODK-STATUSKODER                                      
049300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
049400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
049500     PERFORM IMS-STATUSKONTROLL                                           
049600     .                                                                    
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
050700     .                                                                    
050800     EJECT                                                                
050900 IMS-GET-KATC01 SECTION.                                                  
051000     STRING 'WLKATC01(WDN2A1KY=>' W-IDSKYLT-X  W-IDRUBNR-X                
051100                      W-IDSEGMNR-LOW-X                                    
051200                     '&WDN2A1KY <' W-IDSKYLT-X                            
051300                      W-IDRUBNR-MAX-X  W-IDSEGMNR-HIGH-X ')'              
051400            DELIMITED BY SIZE INTO SSA1                                   
051500     MOVE '  GBGE' TO GODK-STATUSKODER                                    
051600     CALL CBLTDLI USING GN KATC-PCB DLI-IO-AREA-1 SSA1                    
051700     MOVE KATC-STATUS-CODE TO STATUS-WS                                   
051800     PERFORM IMS-STATUSKONTROLL                                           
051900     .                                                                    
052000     SKIP3                                                                
052100 IMS-GET-KATB01 SECTION.                                                  
052200     STRING 'WLKATB01(IDRUBNR  =' W-IDRUBNR-X ')'                         
052300            DELIMITED BY SIZE INTO SSA1                                   
052400     MOVE '  ' TO GODK-STATUSKODER                                        
052500     CALL CBLTDLI USING GU KATB-PCB DLI-IO-AREA-2 SSA1                    
052600     MOVE KATB-STATUS-CODE TO STATUS-WS                                   
052700     PERFORM IMS-STATUSKONTROLL                                           
052800     .                                                                    
052900     SKIP3                                                                
053000 IMS-STATUSKONTROLL SECTION.                                              
053100     SET STATUS-IX TO 1                                                   
053200     SEARCH GODK-STATUS                                                   
053210       AT END                                                             
053220         CALL FELLOG                                                      
053300     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
053310       CONTINUE                                                           
053400     END-SEARCH                                                           
053600     .                                                                    
