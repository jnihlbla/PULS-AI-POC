000100 ID DIVISION.                                                             
000300 PROGRAM-ID.     W1227000.                                                
000400 AUTHOR.         BODIL LINDAHL.                                           
000500 DATE-WRITTEN.   91/02/14.                                                
000510 DATE-COMPILED.                                                           
000600                                                                          
000900*    FUNKTION:                                                            
001000*        RENSNING WDJ1 AV                                                 
001100*        - STRUKTURER SOM FINNS PÅ ARTREG OCH VARIT DEF ERSATTA           
001200*          MER ÄN 6 ÅR.                                                   
001300*        - STRUKTURER SOM SAKNAS ARTREG OCH INTE ÄNDRATS PÅ 6 ÅR.         
001400*                                                                         
001500*        PROGRAMMET UPPATERAR WLSATB (WDJ1)                               
001600*        PROGRAMMET LÄSER     WLARTC (WDK6)                               
001700*        PROGRAMMET LÄSER     WLBENA (WDD3)                               
001800*                                                                         
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- HISTORIKFIL RASA                                           
002700     SELECT UTFIL                      ASSIGN TO W12270D1.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  UTFIL                                                                
003400     LABEL RECORD    STANDARD                                             
003500     RECORDING       V                                                    
003600     BLOCK CONTAINS  0.                                                   
003700     SKIP2                                                                
003800*01  POST -COPY W11251     -PRE  UT01-  -L.                               
004000                                                                          
004100     SKIP2                                                                
004200*01  POST -COPY W11252     -PRE  UT11-  -L.                               
004400                                                                          
004500     SKIP2                                                                
004600*01  POST -COPY W11253     -PRE  UT22-  -L.                               
004800                                                                          
004900     EJECT                                                                
005000 WORKING-STORAGE SECTION.                                                 
005100     SKIP2                                                                
005101                                                                          
005102*    -- CHECKED BY WY2000                                                 
005200 77  IDPGM                       PIC X(8)    VALUE 'W1227000'.            
005300 77  JA                          PIC X       VALUE 'J'.                   
005400 77  NEJ                         PIC X       VALUE 'N'.                   
005500 77  STRUKTUR-FINNS-ARTREG       PIC X       VALUE SPACE.                 
005600 77  W-DIFF                      PIC S9(3)   VALUE ZERO COMP-3.           
005600 77  W-DLET-WDJ101               PIC 9(7)    VALUE ZERO.                  
005600 77  W-REPL-WDJ111               PIC 9(7)    VALUE ZERO.                  
005700     SKIP3                                                                
005800 01  WS-DAGENS-AAAVVD            PIC 9(6)    VALUE ZERO.                  
005900 01  FILLER REDEFINES WS-DAGENS-AAAVVD.                                   
006000     03  WS-DAGENS-AAA           PIC 9(3).                                
006100     03  FILLER                  PIC 9(3).                                
006200     SKIP3                                                                
006300 01  WS-AAAVVD                   PIC 9(6)    VALUE ZERO.                  
006400 01  FILLER REDEFINES WS-AAAVVD.                                          
006500     03  WS-AAA                  PIC 9(3).                                
006600     03  FILLER                  PIC 9(3).                                
006700     SKIP3                                                                
006800 01  WS-SPAR-KDSORT              PIC X(2)    VALUE SPACE.                 
006900 01  WS-SPAR-BEART-SVE           PIC X(30)   VALUE SPACE.                 
007000 01  WS-SPAR-KDHOM               PIC S9      VALUE ZERO COMP-3.           
007100     EJECT                                                                
007200 01  DYNAMISKA-SUBPROGRAM.                                                
007300*                                                                         
007400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007800     EJECT                                                                
007900*    --- PARAMETRAR TILL POSTSUM                                          
008000*                                                                         
008100*01  -COPY W0005      -PRE  POSTSUM-                                      
008300     EJECT                                                                
008400*01  -COPY WDATAREA                                                       
008600     EJECT                                                                
008700 01  UT-AREA                   PIC X(225)  VALUE SPACE.                   
008800     SKIP2                                                                
008900*01  FILLER -COPY W11251          -PRE UT01-   -RED  UT-AREA              
009100     EJECT                                                                
009200*01  FILLER -COPY W11252          -PRE UT11-   -RED  UT-AREA              
009400     EJECT                                                                
009500*01  FILLER -COPY W11253          -PRE UT22-   -RED  UT-AREA              
009700     EJECT                                                                
009800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009900*                                                                         
010000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010100     SKIP3                                                                
010200 01  NYCKLAR-TILL-DLI.                                                    
010300     03  W-IDARTNR-X.                                                     
010400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010500     03  W-IDSKYLT-X.                                                     
010600         05  W-IDSKYLT           PIC X(3)    VALUE 'S  '.                 
010700     03  W-WDJ111KY-X.                                                    
010800         05  W-KDSTRRAD          PIC X(1)    VALUE SPACE.                 
010900         05  W-IDRADNR           PIC S9(5)   COMP-3 VALUE ZERO.           
011000     03  W-WDJ1CSEQ-X.                                                    
011100         05  W-IDLEVNR-S         PIC X(5)    VALUE SPACE.                 
011200         05  W-BELEVART-S        PIC X(30)   VALUE SPACE.                 
011300         05  W-IDARTNR-S         PIC S9(9)   VALUE ZERO COMP-3.           
011400     SKIP2                                                                
011500*    --- STATUS-KOD FRÅN IMS                                              
011600 01  STATUS-WS                   PIC XX.                                  
011700     88  SEGMENT-FINNS                       VALUE '  '.                  
011800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011900     88  BASEN-SLUT                          VALUE 'GB'.                  
012000     SKIP2                                                                
012100 01  GODK-STATUSKODER.                                                    
012200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012300     SKIP3                                                                
012400 01  SSA1                        PIC X(64).                               
012500 01  SSA2                        PIC X(64).                               
012600     EJECT                                                                
012700*    --- IMS FUNKTIONSKODER                                               
012800*01  -COPY W0003                                                          
013000     EJECT                                                                
013100*    ---  DLI INPUT-OUTPUT AREA                                           
013200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013300     SKIP3                                                                
013400 01  DLI-IO-AREA.                                                         
013500     03  IO-AREA                 PIC X(250)  VALUE SPACE.                 
013600     SKIP3                                                                
013700     03  WLSATB01 REDEFINES IO-AREA.                                      
013800*        05  -COPY WDJ101     -PRE SATB-                                  
014000     EJECT                                                                
014100     03  WLSATB11 REDEFINES IO-AREA.                                      
014200*        05  -COPY WDJ111     -PRE SATB-                                  
014400     EJECT                                                                
014500     03  WLSATB22 REDEFINES IO-AREA.                                      
014600*        05  -COPY WDJ122     -PRE SATB-                                  
014800     EJECT                                                                
015300     03  WLARTC01 REDEFINES IO-AREA.                                      
015400*        05  -COPY WDK601                                                 
015600     EJECT                                                                
015700     03  WLBENA01 REDEFINES IO-AREA.                                      
015800*        05  -COPY WDD301     -PRE BENA-                                  
016000     EJECT                                                                
016100     03  WLBENA11 REDEFINES IO-AREA.                                      
016200*        05  -COPY WDD311     -PRE BENA-                                  
016400     EJECT                                                                
016500 01  DLI-IO-AREA-2.                                                       
016600     03  IO-AREA-2               PIC X(350)  VALUE SPACE.                 
016700     SKIP3                                                                
016800     03  WDJ1CSEQ REDEFINES IO-AREA-2.                                    
016900*        05  -COPY WDJ111     -PRE SATE-                                  
017100*        05  -COPY WDJ101     -PRE SATE-                                  
017300     EJECT                                                                
017400 LINKAGE SECTION.                                                         
017600*01  -COPY W0008      -PRE SATB-                                          
017800     05  FILLER                  PIC X.                                   
017900                                                                          
018400*01  -COPY W0008      -PRE BENA-                                          
018600     05  FILLER                  PIC X.                                   
018700     EJECT                                                                
018800*01  -COPY W0008      -PRE SATB2-                                         
019000     05  FILLER                  PIC X.                                   
019100                                                                          
019200*01  -COPY W0008      -PRE ARTC-                                          
019400     05  FILLER                  PIC X.                                   
019500     EJECT                                                                
019600*01  -COPY W0008      -PRE SATE-                                          
019800     05  FILLER                  PIC X.                                   
019900     EJECT                                                                
020000 PROCEDURE DIVISION  USING SATB-PCB BENA-PCB SATB2-PCB                    
020100                     ARTC-PCB SATE-PCB.                                   
020110 MAIN SECTION.                                                            
020200     ENTRY 'DLITCBL' USING SATB-PCB BENA-PCB SATB2-PCB                    
020300                           ARTC-PCB SATE-PCB.                             
020500     PERFORM A-INIT                                                       
020600                                                                          
020700     PERFORM IMS-GET-SATB01                                               
020800     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
              MOVE DAT-TIAAVVD TO WS-DAGENS-AAAVVD                              
020900        MOVE SATB-STR-IDARTNR TO W-IDARTNR                                
021000        PERFORM IMS-GET-ARTC01                                            
021100        IF SEGMENT-FINNS                                                  
021200           MOVE JA TO STRUKTUR-FINNS-ARTREG                               
021300           PERFORM B-BEHANDLA-ERSAETTNING                                 
021400        ELSE                                                              
021500           IF SATB-STR-TIUPPDAT > ZERO                                    
021600               MOVE NEJ TO STRUKTUR-FINNS-ARTREG                          
021700               PERFORM C-BEHANDLA-TIUPPDAT                                
021800           END-IF                                                         
021900        END-IF                                                            
022000      PERFORM IMS-GET-SATB01                                              
022100     END-PERFORM                                                          
022200                                                                          
022300     PERFORM Z-FINIT                                                      
022400                                                                          
022500     MOVE ZERO TO RETURN-CODE                                             
022600     GOBACK                                                               
022700     .                                                                    
022800     EJECT                                                                
022900 A-INIT SECTION.                                                          
023000     SKIP2                                                                
023100     OPEN OUTPUT UTFIL                                                    
023200                                                                          
023300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
023400     MOVE 'IDAG  '        TO DAT-KDDATFORM                                
023500                                                                          
023600     CALL WDATKONV USING DAT-KDDATFORM                                    
023700                         DAT-I-TIDATUM                                    
023800                         DAT-O-TIDATUM                                    
023900                         DAT-KDSVAR                                       
024000     MOVE DAT-TIAAVVD TO WS-DAGENS-AAAVVD                                 
024100     .                                                                    
024200                                                                          
024300     EJECT                                                                
024400 B-BEHANDLA-ERSAETTNING SECTION.                                          
024500     SKIP2                                                                
024600******************************************************************        
024700* BEHANDLAR STRUKTURNR SOM FINNS PÅ ARTREG.                      *        
024800* KDERS-UTG PÅ ARTC01 SÄTTS TILL >0 NÄR ARTIKELN VARIT DEF       *        
024900* ERSATT 3 ÅR.                                                   *        
025000* OM STRUKTUREN VARIT DEF ERSATT MER ÄN 6 ÅR RENSAS DEN FRÅN     *        
025100* WDJ1 OCH SPARAS PÅ HISTORIKFIL.                                *        
025200******************************************************************        
025300     IF ART-KDERS-UTG > 0                                                 
025400        MOVE ART-TIERSDAT TO WS-AAAVVD                                    
025500        PERFORM S10-KONTROLLERA-DATUM                                     
025600     END-IF                                                               
025700     .                                                                    
025800     EJECT                                                                
025900 C-BEHANDLA-TIUPPDAT SECTION.                                             
026000     SKIP2                                                                
026100******************************************************************        
026200* BEHANDLAR STRUKTURNR SOM SAKNAS PÅ ARTREG.                     *        
026300* TIUPPDAT UPPDATERAS NÄR STRUKTUREN ÄNDRAS.                     *        
026400* OM STRUKTUREN INTE ÄNDRATS PÅ 6 ÅR RENSAS DEN FRÅN WDJ1 OCH    *        
026500* SPARAS PÅ HISTORIKFIL.                                         *        
026600******************************************************************        
026700     SKIP2                                                                
026800     MOVE SATB-STR-TIUPPDAT TO DAT-I-TIDATUM                              
026900     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
027000                                                                          
027100     CALL WDATKONV USING DAT-KDDATFORM                                    
027200                         DAT-I-TIDATUM                                    
027300                         DAT-O-TIDATUM                                    
027400                         DAT-KDSVAR                                       
027500     IF DAT-KDSVAR-OK                                                     
027600        MOVE DAT-TIAAVVD TO WS-AAAVVD                                     
027700        PERFORM S10-KONTROLLERA-DATUM                                     
027800     END-IF                                                               
027900     .                                                                    
028000     EJECT                                                                
028100 Z-FINIT SECTION.                                                         
028200     SKIP2                                                                
           DISPLAY 'ANTAL BORTTAGNA WDJ101: ' W-DLET-WDJ101                     
           DISPLAY 'ANTAL ÄNDRADE WDJ111: ' W-REPL-WDJ111                       
028300     CLOSE UTFIL                                                          
028400     MOVE 'S' TO POSTSUM-OPKOD                                            
028500     CALL POSTSUM USING POSTSUM-PARM                                      
028600     .                                                                    
028700     EJECT                                                                
028800 S01-SKRIV-UTPOST SECTION.                                                
028900     SKIP2                                                                
029000     EVALUATE TRUE                                                        
029100        WHEN UT01-IDPTYP = '001'                                          
029200           WRITE UT01-POST FROM UT01-W11251                               
029300        WHEN UT01-IDPTYP = '011'                                          
029400           WRITE UT11-POST FROM UT11-W11252                               
029500        WHEN UT01-IDPTYP = '022'                                          
029600           WRITE UT22-POST FROM UT22-W11253                               
029700     END-EVALUATE                                                         
029800                                                                          
029900     MOVE UT01-IDPTYP TO POSTSUM-TRANSTYP                                 
030000     MOVE 'UTFIL' TO POSTSUM-FDNAMN                                       
030100     MOVE 'W12270D1' TO POSTSUM-DDNAMN2                                   
030200     CALL POSTSUM USING POSTSUM-PARM                                      
030300     .                                                                    
030400     EJECT                                                                
030500 S10-KONTROLLERA-DATUM SECTION.                                           
030600     SKIP2                                                                
030700     ADD +6 TO WS-AAA                                                     
030800                                                                          
030900     MOVE ZERO TO W-DIFF                                                  
031000     COMPUTE W-DIFF = WS-AAA - WS-DAGENS-AAA                              
031100     IF W-DIFF > 50                                                       
031200        ADD 100 TO WS-DAGENS-AAA                                          
031300     END-IF                                                               
031400                                                                          
031500     IF WS-DAGENS-AAAVVD > WS-AAAVVD                                      
031600        PERFORM IMS-GHU-SATB01                                            
031700        PERFORM S101-RENSA-STRUKTUREN                                     
031800        PERFORM S102-KOLLA-RAD                                            
031900     END-IF                                                               
032000     .                                                                    
032100     EJECT                                                                
032200 S101-RENSA-STRUKTUREN SECTION.                                           
032300     SKIP2                                                                
032400******************************************************************        
032500* STRUKTUREN RENSAS FRÅN WDJ1 OCH SPARAS PÅ HISTORIKFIL                   
032600******************************************************************        
032700     SKIP2                                                                
032800     MOVE '001'                 TO UT01-IDPTYP                            
032900     MOVE SATB-STR-IDARTNR      TO UT01-IDARTNR                           
033000     MOVE SATB-STR-BEART-SVE    TO UT01-BEART-SVE                         
033100     MOVE SATB-STR-FLEXFORP     TO UT01-FLEXFORP                          
033200     MOVE SATB-STR-FLFORPQ      TO UT01-FLFORPQ                           
033300     MOVE SATB-STR-IDLEVNR      TO UT01-IDLEVNR                           
033400     MOVE SATB-STR-IDSTRTYP     TO UT01-IDSTRTYP                          
033500     MOVE SATB-STR-IDFKNGRP     TO UT01-IDFKNGRP                          
033600     MOVE SATB-STR-IDTSPEC      TO UT01-IDTSPEC                           
033700     MOVE SATB-STR-IDUSER       TO UT01-IDUSER                            
033800     MOVE SATB-STR-KDBENHOM     TO UT01-KDBENHOM                          
033900     MOVE SATB-STR-KDPRODSL     TO UT01-KDPRODSL                          
034000     MOVE SATB-STR-TIBORT       TO UT01-TIBORT                            
034100     MOVE SATB-STR-TIREGDAT     TO UT01-TIREGDAT                          
034200     MOVE SATB-STR-TIUPPDAT     TO UT01-TIUPPDAT                          
034300     MOVE SATB-STR-TESTRNOT(1)  TO UT01-TESTRNOT(1)                       
034400     MOVE SATB-STR-TESTRNOT(2)  TO UT01-TESTRNOT(2)                       
034500                                                                          
034600     IF STRUKTUR-FINNS-ARTREG = JA                                        
034700         PERFORM IMS-GET-ARTC01                                           
034800         IF SEGMENT-FINNS                                                 
034900            MOVE ART-KDPRODSL    TO UT01-KDPRODSL                         
035000            MOVE ART-IDFKNGRP    TO UT01-IDFKNGRP                         
035100            MOVE ART-KDSORT      TO WS-SPAR-KDSORT                        
035200         ELSE                                                             
035300            MOVE ZERO            TO UT01-KDPRODSL                         
035400            MOVE ZERO            TO UT01-IDFKNGRP                         
035500            MOVE SPACE           TO WS-SPAR-KDSORT                        
035600         END-IF                                                           
035700                                                                          
035800         PERFORM IMS-GET-BENA01                                           
035900         IF SEGMENT-FINNS                                                 
036000             MOVE BENA-BEN-KDHOMONYM TO UT01-KDBENHOM                     
036100                                        WS-SPAR-KDHOM                     
036200             PERFORM IMS-GET-BENA11                                       
036300             MOVE BENA-TEXT-BEART    TO UT01-BEART-SVE                    
036400                                        WS-SPAR-BEART-SVE                 
036500         ELSE                                                             
036600            MOVE ZERO                TO UT01-KDBENHOM                     
036700                                        WS-SPAR-KDHOM                     
036800                                                                          
036900            MOVE SPACE               TO UT01-BEART-SVE                    
037000                                        WS-SPAR-BEART-SVE                 
037100         END-IF                                                           
037200     END-IF                                                               
037300                                                                          
037400     PERFORM S01-SKRIV-UTPOST                                             
037500                                                                          
037600     PERFORM IMS-GET-SATB11                                               
037700     PERFORM UNTIL SEGMENT-SAKNAS                                         
037800       MOVE '011'               TO UT11-IDPTYP                            
037900       MOVE SATB-RAD-KDSTRRAD   TO UT11-KDSTRRAD                          
038000       MOVE SATB-RAD-IDRADNR    TO UT11-IDRADNR                           
038100       MOVE SATB-RAD-IDLEVNR    TO UT11-IDLEVNR                           
038200       MOVE SATB-RAD-BELEVART   TO UT11-BELEVART                          
038300       MOVE SATB-RAD-IDARTNR    TO UT11-IDARTNR                           
038400       MOVE SATB-RAD-BEART-SVE  TO UT11-BEART-SVE                         
038500       MOVE SATB-RAD-IDAO-STA   TO UT11-IDAO-STA                          
038600       MOVE SATB-RAD-IDAO-STO   TO UT11-IDAO-STO                          
038700       MOVE SATB-RAD-IDSTRTYP   TO UT11-IDSTRTYP                          
038800       MOVE SATB-RAD-KDBENHOM   TO UT11-KDBENHOM                          
038900       MOVE SATB-RAD-KDISATS    TO UT11-KDISATS                           
039000       MOVE SATB-RAD-KDSORT     TO UT11-KDSORT                            
039100       MOVE SATB-RAD-REANTPSA   TO UT11-REANTPSA                          
039200       MOVE SATB-RAD-TIREGDAT   TO UT11-TIREGDAT                          
039300       MOVE SATB-RAD-TISTADAT   TO UT11-TISTADAT                          
039400       MOVE SATB-RAD-TISTODAT   TO UT11-TISTODAT                          
039500                                                                          
039600       PERFORM S01-SKRIV-UTPOST                                           
039700                                                                          
039800       MOVE SATB-RAD-KDSTRRAD   TO W-KDSTRRAD                             
039900       MOVE SATB-RAD-IDRADNR    TO W-IDRADNR                              
040000       PERFORM IMS-GET-SATB22                                             
040100       IF SEGMENT-FINNS                                                   
040200          MOVE '022'                TO UT22-IDPTYP                        
040300          MOVE SATB-NOT-IDSTRNOT    TO UT22-IDSTRNOT                      
040400          MOVE SATB-NOT-TESTRNOT(1) TO UT22-TESTRNOT(1)                   
040500          MOVE SATB-NOT-TESTRNOT(2) TO UT22-TESTRNOT(2)                   
040600          PERFORM S01-SKRIV-UTPOST                                        
040700       END-IF                                                             
040800                                                                          
040900       PERFORM IMS-GET-SATB11                                             
041000     END-PERFORM                                                          
041100                                                                          
041200     PERFORM IMS-GHU-SATB01                                               
041300     DISPLAY SATB-STR-IDARTNR 'RENSAD FRÅN RASA'                          
041400     PERFORM IMS-DLET-SATB                                                
           ADD 1 TO W-DLET-WDJ101                                               
041500     .                                                                    
041600     EJECT                                                                
041700 S102-KOLLA-RAD SECTION.                                                  
041800     SKIP2                                                                
041900******************************************************************        
042000* KONTROLL OM STRUKTUREN INGÅR SOM RAD I ANNAN STRUKTUR          *        
042100******************************************************************        
042200     SKIP2                                                                
042300     MOVE W-IDARTNR TO W-IDARTNR-S                                        
042400     MOVE SPACE     TO W-IDLEVNR-S                                        
042500     MOVE SPACE     TO W-BELEVART-S                                       
042600                                                                          
042700     PERFORM IMS-GU-SATB11-CSEQ                                           
042800     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
042900        MOVE SATE-STR-IDARTNR  TO W-IDARTNR                               
043000        MOVE SATE-RAD-IDRADNR  TO W-IDRADNR                               
043100        MOVE SATE-RAD-KDSTRRAD TO W-KDSTRRAD                              
043200        PERFORM IMS-GHU-SATB11                                            
043300                                                                          
043400        MOVE SPACE                TO SATB-RAD-IDSTRTYP                    
043500        IF STRUKTUR-FINNS-ARTREG = JA                                     
043600           MOVE WS-SPAR-KDSORT    TO SATB-RAD-KDSORT                      
043700           MOVE WS-SPAR-BEART-SVE TO SATB-RAD-BEART-SVE                   
043800           MOVE WS-SPAR-KDHOM     TO SATB-RAD-KDBENHOM                    
043900        END-IF                                                            
044000        PERFORM IMS-REPL-SATB                                             
              ADD 1 TO W-REPL-WDJ111                                            
044100        DISPLAY SATB-RAD-IDARTNR 'RAD UPPDATERAD'                         
044200                                                                          
044300        PERFORM IMS-GN-SATB11-CSEQ                                        
044400     END-PERFORM                                                          
044500     .                                                                    
044600     EJECT                                                                
044700* --- IMS SEKTIONER ---                                                   
044800*                                                                         
044900 IMS-GET-SATB01 SECTION.                                                  
045000     MOVE 'WLSATB01 ' TO SSA1                                             
045100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
045200     CALL CBLTDLI USING GN SATB-PCB DLI-IO-AREA SSA1                      
045300     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
045400     PERFORM IMS-STATUSKONTROLL                                           
045500     .                                                                    
045600     SKIP3                                                                
045700 IMS-GHU-SATB11 SECTION.                                                  
045800     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
045900          DELIMITED BY SIZE INTO SSA1                                     
046000     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-X ')'                        
046100          DELIMITED BY SIZE INTO SSA2                                     
046200     MOVE '  ' TO GODK-STATUSKODER                                        
046300     CALL CBLTDLI USING GHU SATB2-PCB DLI-IO-AREA SSA1 SSA2               
046400     MOVE SATB2-STATUS-CODE TO STATUS-WS                                  
046500     PERFORM IMS-STATUSKONTROLL                                           
046600     .                                                                    
046700     EJECT                                                                
046800 IMS-GHU-SATB01 SECTION.                                                  
046900     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
047000          DELIMITED BY SIZE INTO SSA1                                     
047100     MOVE '  GE' TO GODK-STATUSKODER                                      
047200     CALL CBLTDLI USING GHU SATB2-PCB DLI-IO-AREA SSA1                    
047300     MOVE SATB2-STATUS-CODE TO STATUS-WS                                  
047400     PERFORM IMS-STATUSKONTROLL                                           
047500     .                                                                    
047600     SKIP3                                                                
047700 IMS-GET-SATB11 SECTION.                                                  
047800     MOVE 'WLSATB11 ' TO SSA1                                             
047900     MOVE '  GE' TO GODK-STATUSKODER                                      
048000     CALL CBLTDLI USING GNP SATB2-PCB DLI-IO-AREA SSA1                    
048100     MOVE SATB2-STATUS-CODE TO STATUS-WS                                  
048200     PERFORM IMS-STATUSKONTROLL                                           
048300     .                                                                    
048400     SKIP3                                                                
048500 IMS-GET-SATB22 SECTION.                                                  
048600     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-X ')'                        
048700          DELIMITED BY SIZE INTO SSA1                                     
048800     MOVE 'WLSATB22 ' TO SSA2                                             
048900     MOVE '  GE' TO GODK-STATUSKODER                                      
049000     CALL CBLTDLI USING GNP SATB2-PCB DLI-IO-AREA SSA1 SSA2               
049100     MOVE SATB2-STATUS-CODE TO STATUS-WS                                  
049200     PERFORM IMS-STATUSKONTROLL                                           
049300     .                                                                    
049400     EJECT                                                                
049500 IMS-DLET-SATB SECTION.                                                   
049600     MOVE '  ' TO GODK-STATUSKODER                                        
049700     CALL CBLTDLI USING DLET SATB2-PCB DLI-IO-AREA                        
049800     MOVE SATB2-STATUS-CODE TO STATUS-WS                                  
049900     PERFORM IMS-STATUSKONTROLL                                           
050000     .                                                                    
050100     SKIP3                                                                
050200 IMS-REPL-SATB SECTION.                                                   
050300     MOVE '  ' TO GODK-STATUSKODER                                        
050400     CALL CBLTDLI USING REPL SATB2-PCB DLI-IO-AREA                        
050500     MOVE SATB2-STATUS-CODE TO STATUS-WS                                  
050600     PERFORM IMS-STATUSKONTROLL                                           
050700     .                                                                    
051700     EJECT                                                                
051800 IMS-GET-ARTC01 SECTION.                                                  
051900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
052000          DELIMITED BY SIZE INTO SSA1                                     
052100     MOVE '  GE' TO GODK-STATUSKODER                                      
052200     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
052300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
052400     PERFORM IMS-STATUSKONTROLL                                           
052500     .                                                                    
052600     SKIP3                                                                
052700 IMS-GET-BENA01 SECTION.                                                  
052800     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
052900          DELIMITED BY SIZE INTO SSA1                                     
053000     MOVE '  GE' TO GODK-STATUSKODER                                      
053100     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1                      
053200     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
053300     PERFORM IMS-STATUSKONTROLL                                           
053400     .                                                                    
053500     EJECT                                                                
053600 IMS-GET-BENA11 SECTION.                                                  
053700     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
053800          DELIMITED BY SIZE INTO SSA1                                     
053900     MOVE '  ' TO GODK-STATUSKODER                                        
054000     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA SSA1                     
054100     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
054200     PERFORM IMS-STATUSKONTROLL                                           
054300     .                                                                    
054400     EJECT                                                                
054500 IMS-GU-SATB11-CSEQ SECTION.                                              
054600     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
054700          DELIMITED BY SIZE INTO SSA1                                     
054800     MOVE 'WLSATB01 ' TO SSA2                                             
054900     MOVE '  GBGE' TO GODK-STATUSKODER                                    
055000     CALL CBLTDLI USING GU SATE-PCB DLI-IO-AREA-2 SSA1 SSA2               
055100     MOVE SATE-STATUS-CODE TO STATUS-WS                                   
055200     PERFORM IMS-STATUSKONTROLL                                           
055300     .                                                                    
055400     SKIP3                                                                
055500 IMS-GN-SATB11-CSEQ SECTION.                                              
055600     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
055700          DELIMITED BY SIZE INTO SSA1                                     
055800     MOVE 'WLSATB01 ' TO SSA2                                             
055900     MOVE '  GBGE' TO GODK-STATUSKODER                                    
056000     CALL CBLTDLI USING GN SATE-PCB DLI-IO-AREA-2 SSA1 SSA2               
056100     MOVE SATE-STATUS-CODE TO STATUS-WS                                   
056200     PERFORM IMS-STATUSKONTROLL                                           
056300     .                                                                    
056400     SKIP3                                                                
056500 IMS-STATUSKONTROLL SECTION.                                              
056600     SKIP2                                                                
056700     SET STATUS-IX TO 1                                                   
056800     SEARCH GODK-STATUS                                                   
056900       AT END                                                             
056910         CALL FELLOG                                                      
057000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
057010         CONTINUE                                                         
057100     END-SEARCH                                                           
057200     .                                                                    
