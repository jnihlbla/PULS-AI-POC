000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6016600.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   98/05/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        UPPDATERING ETIKETTDATABAS FÖR BARCODE.                          
000900*                                                                         
001000*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001100*                              WLBENA (WDD3)                              
001200*        PROGRAMMET UPPDATERAR WLETIA (WDK3)                              
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W6T166                                              
001600*        MID:         W6I16601                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W6O16601                                            
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'W6016600'.            
003000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003100 77  JA                          PIC X       VALUE 'J'.                   
003200 77  NEJ                         PIC X       VALUE 'N'.                   
003300 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
003400 77  DAGENS-AAAAMMDD             PIC 9(8)    VALUE ZERO.                  
003500                                                                          
003600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
003700     88  INDATA-OK                           VALUE 'J'.                   
003800     88  INDATA-FEL                          VALUE 'N'.                   
003900                                                                          
004000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004100     88  NYCKLAR-OK                          VALUE 'J'.                   
004200     88  NYCKLAR-FEL                         VALUE 'N'.                   
004300                                                                          
004400 77  IDLAYOUT-SW                 PIC X       VALUE 'J'.                   
004500     88  IDLAYOUT-OK                         VALUE 'J'.                   
004600     88  IDLAYOUT-FEL                        VALUE 'N'.                   
004700                                                                          
004800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004900     88  EGEN-MID                            VALUE '6166'.                
005000     88  GODK-MID                            VALUE '6166' '6167'          
005100                                                   '6168'.                
005200     88  HELP-MID                            VALUE '0551'.                
005300                                                                          
005400 01  WS-ARTNR-ETIK                PIC 9(9).                               
005500 01  FILLER REDEFINES WS-ARTNR-ETIK.                                      
005600     03  WS-ARTNR-ETIK-POS1-2     PIC X(2).                               
005700     03  WS-ARTNR-ETIK-POS3-6     PIC X(4).                               
005800     03  FILLER                   PIC X(3).                               
005900                                                                          
006000 01 WS-IDLAYOUT-TEST.                                                     
006100     03 WS-STORLEK-KOD     PIC X(1).                                      
006200     03 WS-TYP-KOD         PIC X(4).                                      
006300     03 WS-LOEPNUMMER      PIC X(2).                                      
006400     03 WS-SKRIVARE-KOD PIC X.                                            
006500     03 WS-LOGO-KOD        PIC X.                                         
006600     03 WS-TOMRUM          PIC X(1).                                      
006700     EJECT                                                                
006800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006900 01  GENERELLA-SUBPROGRAM.                                                
007000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007400     EJECT                                                                
007500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007600*01 -COPY WMEDAREA                                                        
007700     SKIP3                                                                
007800 01  MESSAGE-CODES.                                                       
007900     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
008000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008100     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008200     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008400     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
008500     03  LAYOUT-FELAKTIG         PIC X(3)    VALUE '729'.                 
008600     EJECT                                                                
008700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
009000     SKIP3                                                                
009100*01 -COPY WMSGINIT                                                        
009200     EJECT                                                                
009300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009400*                                                                         
009500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009600     SKIP3                                                                
009700*01  MID -COPY W6I16601                                                   
009800     EJECT                                                                
009900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010000     SKIP3                                                                
010100*01  -COPY WMSGAREA                                                       
010200     EJECT                                                                
010300     03  MOD REDEFINES MSG-AREA.                                          
010400*      05  -COPY W6O16601                                                 
010500     EJECT                                                                
010600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010700     SKIP3                                                                
010800*01  -COPY WMFSAREA                                                       
010900     EJECT                                                                
011000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011100*                                                                         
011200     EJECT                                                                
011300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011400     SKIP3                                                                
011500 01  NYCKLAR-TILL-DLI.                                                    
011600     03  W-IDARTNR-X.                                                     
011700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011800     03  W-IDSKYLT-X.                                                     
011900         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
012000     SKIP2                                                                
012100*    --- STATUS-KOD FRÅN IMS                                              
012200 01  STATUS-WS                   PIC XX.                                  
012300     88  SEGMENT-FINNS                       VALUE '  '.                  
012400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012600     SKIP2                                                                
012700 01  GODK-STATUSKODER.                                                    
012800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012900     SKIP3                                                                
013000 01  SSA1                        PIC X(64).                               
013100 01  SSA2                        PIC X(64).                               
013200     EJECT                                                                
013300*    --- IMS FUNKTIONSKODER                                               
013400*01  -COPY W0003                                                          
013500     EJECT                                                                
013600*    ---  DLI INPUT-OUTPUT AREA                                           
013700                                                                          
013800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC01'.                    
013900 01  DLI-IO-WLARTC01.                                                     
014000*    03  -COPY WDK601                                                     
014100     EJECT                                                                
014200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBENA11'.                    
014300 01  DLI-IO-WLBENA11.                                                     
014400*    03  -COPY WDD311                                                     
014500     EJECT                                                                
014600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLETIA01'.                    
014700 01  DLI-IO-WLETIA01.                                                     
014800*    03  -COPY WDK301                                                     
014900     EJECT                                                                
015000 LINKAGE SECTION.                                                         
015100*01  -COPY W0009   -PRE MSG-                                              
015200     EJECT                                                                
015300*01  -COPY W0008   -PRE USEA-                                             
015400     05  FILLER                  PIC X.                                   
015500     EJECT                                                                
015600*01  -COPY W0008   -PRE ARTC-                                             
015700     05  FILLER                  PIC X.                                   
015800     EJECT                                                                
015900*01  -COPY W0008   -PRE BENA-                                             
016000     05  FILLER                  PIC X.                                   
016100     EJECT                                                                
016200*01  -COPY W0008   -PRE ETIA-                                             
016300     05  FILLER                  PIC X.                                   
016400     EJECT                                                                
016500 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB ARTC-PCB BENA-PCB             
016600                           ETIA-PCB.                                      
016700 MAIN SECTION.                                                            
016800     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB ARTC-PCB BENA-PCB             
016900                           ETIA-PCB.                                      
017000                                                                          
017100     PERFORM IMS-GET-MSG                                                  
017200                                                                          
017300     IF SEGMENT-FINNS                                                     
017400       PERFORM A-INIT                                                     
017500       PERFORM B-KOLLA-NYCKLAR                                            
017600       IF NYCKLAR-OK                                                      
017700         IF MFS-UPDATE                                                    
017800           PERFORM G-KOLLA-INPUT                                          
017900           IF INDATA-OK                                                   
018000             PERFORM H-UPPDATERA                                          
018100           END-IF                                                         
018200         ELSE                                                             
018300           IF MFS-FIRST                                                   
018400             PERFORM C-FOERSTA-SIDA                                       
018500           ELSE                                                           
018600             PERFORM E-SAMMA-SIDA                                         
018700           END-IF                                                         
018800         END-IF                                                           
018900         PERFORM F-LAES-VISA-INFO                                         
019000       END-IF                                                             
019100                                                                          
019200       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O16601 + 4                      
019300       PERFORM IMS-INSERT-MSG                                             
019400     END-IF                                                               
019500                                                                          
019600     MOVE ZERO TO RETURN-CODE                                             
019700     GOBACK                                                               
019800     .                                                                    
019900     EJECT                                                                
020000 A-INIT SECTION.                                                          
020100                                                                          
020200     IF MSG-DUBBLA-TRANSKODER                                             
020300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I16601                 
020400       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
020500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
020600     ELSE                                                                 
020700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I16601                  
020800       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
020900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
021000     END-IF                                                               
021100                                                                          
021200     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
021300     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
021400     MOVE MFS-IDTRANS TO W-IDTRANS                                        
021500                                                                          
021600     MOVE LOW-VALUE TO MSG-AREA                                           
021700     MOVE 'W6O166N1' TO MFS-IDMOD                                         
021800     MOVE '6166' TO MOD-IDTRANS                                           
021900     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
022000                                                                          
022100     IF MSGI-IDLAND-SPR = 'SE'                                            
022200        MOVE '0' TO MFS-KDHUVOMR                                          
022300     END-IF                                                               
022400                                                                          
022500     IF EGEN-MID OR HELP-MID                                              
022600       CONTINUE                                                           
022700     ELSE                                                                 
022800       MOVE SPACE TO MFS-KDTRTYP                                          
022900       MOVE '7' TO MFS-IDPFK                                              
023000     END-IF                                                               
023100                                                                          
023200     ACCEPT DAGENS-DATUM FROM DATE                                        
023300     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-AAAAMMDD                   
023400     .                                                                    
023500     EJECT                                                                
023600 B-KOLLA-NYCKLAR SECTION.                                                 
023700                                                                          
023800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
023900     MOVE '001'             TO MSGI-KDCALL                                
024000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
024100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
024200     MOVE '6166'            TO MSGI-IDTRANS                               
024300                                                                          
024400     IF GODK-MID                                                          
024500        MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                               
024600     END-IF                                                               
024700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
024800     IF MSGI-IDLAND-SPR = 'SE'                                            
024900        MOVE 'S  ' TO W-IDSKYLT                                           
025000                      MED-IDSKYLT                                         
025100     ELSE                                                                 
025200        MOVE'GB '  TO W-IDSKYLT                                           
025300                      MED-IDSKYLT                                         
025400     END-IF                                                               
025500                                                                          
025600     MOVE JA TO NYCKLAR-SW                                                
025700                                                                          
025800     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
025900     IF MID-IDARTNR-IN NOT = ALL '+'                                      
026000       MOVE '7'         TO MFS-IDPFK                                      
026100       MOVE SPACE       TO MFS-KDTRTYP                                    
026200     END-IF                                                               
026300     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
026400     IF MSGI-IDARTNR NUMERIC                                              
026500       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
026600     ELSE                                                                 
026700       MOVE NEJ TO NYCKLAR-SW                                             
026800     END-IF                                                               
026900                                                                          
027000     IF GODK-MID OR NYCKLAR-OK                                            
027100       MOVE MSGI-IDARTNR    TO MOD-IDARTNR-UT                             
027200       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
027300     ELSE                                                                 
027400       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
027500     END-IF                                                               
027600                                                                          
027700     IF NYCKLAR-FEL                                                       
027800       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
027900       CALL WMEDKONV USING MED-WMEDAREA                                   
028000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
028100       PERFORM MFS-RENSA-FAELT-IN                                         
028200       PERFORM MFS-RENSA-FAELT-UT                                         
028300     END-IF                                                               
028400     .                                                                    
028500     EJECT                                                                
028600 C-FOERSTA-SIDA SECTION.                                                  
028700                                                                          
028800     PERFORM MFS-RENSA-FAELT-IN                                           
028900     .                                                                    
029000     EJECT                                                                
029100 E-SAMMA-SIDA SECTION.                                                    
029200                                                                          
029300     IF EGEN-MID OR HELP-MID                                              
029400       IF MID-IDLAYOUT = ALL '+'                                          
029500       AND MID-IDARTNR-ETIK = ALL '+'                                     
029600       AND MID-TEETIK-INT = ALL '+'                                       
029700         PERFORM MFS-RENSA-FAELT-IN                                       
029800       ELSE                                                               
029900         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
030000         CALL WMEDKONV USING MED-WMEDAREA                                 
030100         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
030200         PERFORM EA-MID-INDATA-TILL-MOD                                   
030300       END-IF                                                             
030400     ELSE                                                                 
030500       PERFORM MFS-RENSA-FAELT-IN                                         
030600     END-IF                                                               
030700     .                                                                    
030800     EJECT                                                                
030900 EA-MID-INDATA-TILL-MOD SECTION.                                          
031000                                                                          
031100     IF MID-IDLAYOUT NOT = ALL '+'                                        
031200        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLAYOUT-ATTR                   
031300        MOVE MFS-ROER-EJ-FAELT     TO MOD-IDLAYOUT-IN                     
031400     ELSE                                                                 
031500        MOVE MFS-RENSA-FAELT TO MOD-IDLAYOUT-IN                           
031600     END-IF                                                               
031700                                                                          
031800     IF MID-IDARTNR-ETIK NOT = ALL '+'                                    
031900        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDARTNR-ETIK-ATTR               
032000        MOVE MFS-ROER-EJ-FAELT     TO MOD-IDARTNR-ETIK-IN                 
032100     ELSE                                                                 
032200        MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-ETIK-IN                       
032300     END-IF                                                               
032400                                                                          
032500     IF MID-TEETIK-INT NOT = ALL '+'                                      
032600        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEETIK-INT-ATTR                 
032700        MOVE MFS-ROER-EJ-FAELT     TO MOD-TEETIK-INT-IN                   
032800     ELSE                                                                 
032900        MOVE MFS-RENSA-FAELT TO MOD-TEETIK-INT-IN                         
033000     END-IF                                                               
033100     .                                                                    
033200     EJECT                                                                
033300 F-LAES-VISA-INFO SECTION.                                                
033400                                                                          
033500     PERFORM FA-LAES-GRUNDDATA                                            
033600                                                                          
033700     IF SEGMENT-SAKNAS                                                    
033800        IF MFS-UPDATE                                                     
033900           CONTINUE                                                       
034000        ELSE                                                              
034100           MOVE ARTIKEL-SAKNAS TO MED-IDMFSFEL                            
034200           CALL WMEDKONV USING MED-WMEDAREA                               
034300           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
034400        END-IF                                                            
034500        PERFORM MFS-RENSA-FAELT-UT                                        
034600     ELSE                                                                 
034700        PERFORM FB-VISA-ARTINF                                            
034800     END-IF                                                               
034900     .                                                                    
035000     EJECT                                                                
035100 FA-LAES-GRUNDDATA SECTION.                                               
035200                                                                          
035300     PERFORM IMS-GET-ETIA01                                               
035400     .                                                                    
035500     EJECT                                                                
035600 FB-VISA-ARTINF SECTION.                                                  
035700                                                                          
035800     MOVE ETI-DAREGDAT     TO MOD-DAREGDAT                                
035900     MOVE ETI-IDUSER       TO MOD-IDUSER                                  
036000     MOVE ETI-TIUPPDAT     TO MOD-TIUPPDAT                                
036100     MOVE ETI-IDARTNR-ETIK TO MOD-IDARTNR-ETIK-UT                         
036200     MOVE ETI-IDLAYOUT     TO MOD-IDLAYOUT-UT                             
036300     MOVE ETI-TEETIK-INT   TO MOD-TEETIK-INT-UT                           
036400                                                                          
036500     PERFORM IMS-GET-BENA11                                               
036600     MOVE TEXT-BEART       TO MOD-BEART                                   
036700     .                                                                    
036800     EJECT                                                                
036900 G-KOLLA-INPUT SECTION.                                                   
037000                                                                          
037100     MOVE JA  TO INDATA-SW                                                
037200     IF MID-IDLAYOUT = ALL '+'                                            
037300     AND MID-IDARTNR-ETIK = ALL '+'                                       
037400     AND MID-TEETIK-INT = ALL '+'                                         
037500        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
037600        CALL WMEDKONV USING MED-WMEDAREA                                  
037700        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
037800        PERFORM MFS-ROER-EJ-FAELT-IN                                      
037900        PERFORM MFS-ROER-EJ-FAELT-UT                                      
038000        MOVE NEJ TO INDATA-SW                                             
038100     ELSE                                                                 
038200       IF MID-IDLAYOUT NOT = ALL '+'                                      
038300          MOVE JA TO IDLAYOUT-SW                                          
038400          MOVE MID-IDLAYOUT TO WS-IDLAYOUT-TEST                           
038410                                                                          
038500          IF WS-STORLEK-KOD = '1' AND WS-LOGO-KOD = 'L'                   
038600             MOVE NEJ TO IDLAYOUT-SW                                      
038700          END-IF                                                          
038710                                                                          
038800          IF WS-STORLEK-KOD = '2' AND WS-LOGO-KOD = 'L'                   
038900             MOVE NEJ TO IDLAYOUT-SW                                      
039000          END-IF                                                          
039010                                                                          
039100          IF WS-STORLEK-KOD = '3' AND WS-LOGO-KOD = 'U'                   
039200             MOVE NEJ TO IDLAYOUT-SW                                      
039300          END-IF                                                          
039400                                                                          
039500          IF WS-LOEPNUMMER IS NOT NUMERIC                                 
039600             MOVE NEJ TO IDLAYOUT-SW                                      
039700          END-IF                                                          
039800                                                                          
039900          IF WS-TYP-KOD = 'STAN' OR 'SPEC' OR 'SATS' OR 'TEST'            
040000             CONTINUE                                                     
040100          ELSE                                                            
040200             MOVE NEJ TO IDLAYOUT-SW                                      
040300          END-IF                                                          
040301                                                                          
040302          IF WS-TOMRUM NOT = SPACE                                        
040310             MOVE NEJ TO IDLAYOUT-SW                                      
040320          END-IF                                                          
040330                                                                          
040400          IF IDLAYOUT-OK                                                  
040500             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLAYOUT-ATTR               
040600          ELSE                                                            
040800             MOVE NEJ TO INDATA-SW                                        
040810             MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDLAYOUT-ATTR               
040900          END-IF                                                          
041000       END-IF                                                             
041100                                                                          
041200       IF MID-IDARTNR-ETIK NOT = ALL '+'                                  
041300         IF MID-IDARTNR-ETIK NOT NUMERIC                                  
041400           MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ETIK-ATTR                
041500           MOVE NEJ TO INDATA-SW                                          
041600         ELSE                                                             
041700           MOVE MID-IDARTNR-ETIK TO WS-ARTNR-ETIK                         
041800           IF WS-ARTNR-ETIK-POS1-2 = ZERO                                 
041900           AND WS-ARTNR-ETIK-POS3-6 = '6768'                              
042000              MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-ETIK-ATTR           
042100           ELSE                                                           
042200              MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ETIK-ATTR             
042300              MOVE NEJ TO INDATA-SW                                       
042400           END-IF                                                         
042500         END-IF                                                           
042600       END-IF                                                             
042700                                                                          
042800       IF MID-TEETIK-INT NOT = ALL '+'                                    
042900          MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEETIK-INT-ATTR                
043000       END-IF                                                             
043100                                                                          
043200       IF INDATA-FEL                                                      
043210          IF IDLAYOUT-OK                                                  
043300             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
043301          ELSE                                                            
043310             MOVE LAYOUT-FELAKTIG  TO MED-IDMFSFEL                        
043320          END-IF                                                          
043400          CALL WMEDKONV USING MED-WMEDAREA                                
043500          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
043600          PERFORM MFS-ROER-EJ-FAELT-UT                                    
043700          PERFORM MFS-ROER-EJ-FAELT-IN                                    
043800       ELSE                                                               
043900          PERFORM IMS-GET-ARTC01                                          
044000          IF SEGMENT-FINNS                                                
044100            IF ART-KDERS-UTG > 0                                          
044200               MOVE NEJ TO INDATA-SW                                      
044300               MOVE ARTIKEL-SAKNAS TO MED-IDMFSFEL                        
044400               CALL WMEDKONV USING MED-WMEDAREA                           
044500               MOVE MED-MFSFEL TO MOD-TEMFSFEL                            
044600               PERFORM MFS-RENSA-FAELT-IN                                 
044700               PERFORM MFS-RENSA-FAELT-UT                                 
044800            END-IF                                                        
044900          ELSE                                                            
045000             MOVE NEJ TO INDATA-SW                                        
045100             MOVE ARTIKEL-SAKNAS TO MED-IDMFSFEL                          
045200             CALL WMEDKONV USING MED-WMEDAREA                             
045300             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
045400             PERFORM MFS-RENSA-FAELT-IN                                   
045500             PERFORM MFS-RENSA-FAELT-UT                                   
045600          END-IF                                                          
045700       END-IF                                                             
045800     END-IF                                                               
045900     .                                                                    
046000     EJECT                                                                
046100 H-UPPDATERA SECTION.                                                     
046200     PERFORM IMS-GET-ETIA01                                               
046300                                                                          
046400     IF SEGMENT-FINNS                                                     
046500        MOVE MSG-SIGNON-USERID   TO ETI-IDUSER                            
046600        MOVE DAGENS-DATUM        TO ETI-TIUPPDAT                          
046700                                                                          
046800        IF MID-IDLAYOUT NOT = ALL '+'                                     
046900           MOVE MID-IDLAYOUT     TO ETI-IDLAYOUT                          
047000           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDLAYOUT-ATTR                
047100        END-IF                                                            
047200                                                                          
047300        IF MID-IDARTNR-ETIK NOT = ALL '+'                                 
047400           MOVE MID-IDARTNR-ETIK TO ETI-IDARTNR-ETIK                      
047500           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDARTNR-ETIK-ATTR            
047600        END-IF                                                            
047700                                                                          
047800        IF MID-TEETIK-INT NOT = ALL '+'                                   
047900           MOVE MID-TEETIK-INT   TO ETI-TEETIK-INT                        
048000           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEETIK-INT-ATTR              
048100        END-IF                                                            
048200                                                                          
048300        PERFORM IMS-REPL-ETIA                                             
048400     ELSE                                                                 
048500        MOVE W-IDARTNR         TO ETI-IDARTNR                             
048600        MOVE DAGENS-AAAAMMDD   TO ETI-DAREGDAT                            
048700        MOVE MSG-SIGNON-USERID TO ETI-IDUSER                              
048800        MOVE ZERO              TO ETI-TIUPPDAT                            
048900        MOVE 'VO '             TO ETI-IDSORTIM                            
049000                                                                          
049100        IF MID-IDLAYOUT NOT = ALL '+'                                     
049200           MOVE MID-IDLAYOUT   TO ETI-IDLAYOUT                            
049300           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDLAYOUT-ATTR                
049400        ELSE                                                              
049500           MOVE SPACE          TO ETI-IDLAYOUT                            
049600        END-IF                                                            
049700                                                                          
049800        IF MID-IDARTNR-ETIK NOT = ALL '+'                                 
049900           MOVE MID-IDARTNR-ETIK TO ETI-IDARTNR-ETIK                      
050000           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDARTNR-ETIK-ATTR            
050100        ELSE                                                              
050200           MOVE ZERO             TO ETI-IDARTNR-ETIK                      
050300        END-IF                                                            
050400                                                                          
050500        IF MID-TEETIK-INT NOT = ALL '+'                                   
050600           MOVE MID-TEETIK-INT   TO ETI-TEETIK-INT                        
050700           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEETIK-INT-ATTR              
050800        ELSE                                                              
050900           MOVE SPACE            TO ETI-TEETIK-INT                        
051000        END-IF                                                            
051100                                                                          
051200        PERFORM IMS-ISRT-ETIA                                             
051300     END-IF                                                               
051400                                                                          
051500     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
051600     CALL WMEDKONV USING MED-WMEDAREA                                     
051700     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
051800     PERFORM MFS-FORM-ATTR                                                
051900     PERFORM MFS-RENSA-FAELT-IN                                           
052000     .                                                                    
052100     EJECT                                                                
052200 MFS-RENSA-FAELT-UT SECTION.                                              
052300                                                                          
052400     MOVE MFS-RENSA-FAELT TO MOD-IDLAYOUT-UT                              
052500                             MOD-IDARTNR-ETIK-UT                          
052600                             MOD-TEETIK-INT-UT                            
052700                             MOD-BEART                                    
052800                             MOD-IDUSER                                   
052900                             MOD-TIUPPDAT                                 
053000                             MOD-DAREGDAT                                 
053100     .                                                                    
053200     SKIP3                                                                
053300 MFS-RENSA-FAELT-IN SECTION.                                              
053400                                                                          
053500     MOVE MFS-RENSA-FAELT TO MOD-IDLAYOUT-IN                              
053600                             MOD-IDARTNR-ETIK-IN                          
053700                             MOD-TEETIK-INT-IN                            
053800     .                                                                    
053900     EJECT                                                                
054000 MFS-ROER-EJ-FAELT-UT SECTION.                                            
054100                                                                          
054200     MOVE MFS-ROER-EJ-FAELT TO MOD-IDLAYOUT-UT                            
054300                               MOD-IDARTNR-ETIK-UT                        
054400                               MOD-TEETIK-INT-UT                          
054500                               MOD-BEART                                  
054600                               MOD-IDUSER                                 
054700                               MOD-TIUPPDAT                               
054800                               MOD-DAREGDAT                               
054900     .                                                                    
055000     SKIP3                                                                
055100 MFS-ROER-EJ-FAELT-IN SECTION.                                            
055200                                                                          
055300     MOVE MFS-ROER-EJ-FAELT TO MOD-IDLAYOUT-IN                            
055400                               MOD-IDARTNR-ETIK-IN                        
055500                               MOD-TEETIK-INT-IN                          
055600     .                                                                    
055700     EJECT                                                                
055800 MFS-FORM-ATTR SECTION.                                                   
055900                                                                          
056000     MOVE MFS-FORMATETS-ATTR TO MOD-IDLAYOUT-ATTR                         
056100                                MOD-IDARTNR-ETIK-ATTR                     
056200                                MOD-TEETIK-INT-ATTR                       
056300     .                                                                    
056400     SKIP2                                                                
056500 MFS-LAES-IN-IGEN SECTION.                                                
056600                                                                          
056700     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLAYOUT-ATTR                      
056800                                   MOD-IDARTNR-ETIK-ATTR                  
056900                                   MOD-TEETIK-INT-ATTR                    
057000     .                                                                    
057100     EJECT                                                                
057200* --- IMS SEKTIONER ---                                                   
057300     SKIP3                                                                
057400 IMS-GET-MSG SECTION.                                                     
057500     MOVE '  QC' TO GODK-STATUSKODER                                      
057600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
057700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
057800     PERFORM IMS-STATUSKONTROLL                                           
057900     .                                                                    
058000     SKIP3                                                                
058100 IMS-INSERT-MSG SECTION.                                                  
058200     IF MSGI-IDLAND-SPR = 'SE'                                            
058300       MOVE '0' TO MFS-KDHUVOMR                                           
058400     END-IF                                                               
058500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
058600     MOVE SPACE TO GODK-STATUSKODER                                       
058700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
058800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
058900     PERFORM IMS-STATUSKONTROLL                                           
059000     .                                                                    
059100     EJECT                                                                
059200 IMS-GET-ARTC01 SECTION.                                                  
059300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
059400          DELIMITED BY SIZE INTO SSA1                                     
059500     MOVE '  GE' TO GODK-STATUSKODER                                      
059600     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC01 SSA1                  
059700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
059800     PERFORM IMS-STATUSKONTROLL                                           
059900     .                                                                    
060000     SKIP2                                                                
060100 IMS-GET-BENA11 SECTION.                                                  
060200     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
060300             DELIMITED BY SIZE INTO SSA1                                  
060400     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
060500              DELIMITED BY SIZE INTO SSA2                                 
060600     MOVE '  GE' TO GODK-STATUSKODER                                      
060700     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WLBENA11 SSA1 SSA2             
060800     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
060900     PERFORM IMS-STATUSKONTROLL                                           
061000     .                                                                    
061100     EJECT                                                                
061200 IMS-GET-ETIA01 SECTION.                                                  
061300     STRING 'WLETIA01(IDARTNR  =' W-IDARTNR-X ')'                         
061400          DELIMITED BY SIZE INTO SSA1                                     
061500     MOVE '  GE' TO GODK-STATUSKODER                                      
061600     CALL CBLTDLI USING GHU ETIA-PCB DLI-IO-WLETIA01 SSA1                 
061700     MOVE ETIA-STATUS-CODE TO STATUS-WS                                   
061800     PERFORM IMS-STATUSKONTROLL                                           
061900     .                                                                    
062000     SKIP3                                                                
062100 IMS-ISRT-ETIA SECTION.                                                   
062200     MOVE 'WLETIA01 ' TO SSA1                                             
062300     MOVE '    ' TO GODK-STATUSKODER                                      
062400     CALL CBLTDLI USING ISRT ETIA-PCB DLI-IO-WLETIA01 SSA1                
062500     MOVE ETIA-STATUS-CODE TO STATUS-WS                                   
062600     PERFORM IMS-STATUSKONTROLL                                           
062700     .                                                                    
062800     SKIP3                                                                
062900 IMS-REPL-ETIA SECTION.                                                   
063000     MOVE '  ' TO GODK-STATUSKODER                                        
063100     CALL CBLTDLI USING REPL ETIA-PCB DLI-IO-WLETIA01                     
063200     MOVE ETIA-STATUS-CODE TO STATUS-WS                                   
063300     PERFORM IMS-STATUSKONTROLL                                           
063400     .                                                                    
063500     EJECT                                                                
063600 IMS-STATUSKONTROLL SECTION.                                              
063700                                                                          
063800     SET STATUS-IX TO 1                                                   
063900     SEARCH GODK-STATUS                                                   
064000       AT END                                                             
064100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
064200         DELIMITED BY SIZE INTO FELTEXT                                   
064300         CALL FELLOG                                                      
064400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
064500         CONTINUE                                                         
064600     END-SEARCH                                                           
064700     .                                                                    
