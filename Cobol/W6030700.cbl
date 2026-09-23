000100*                                                                         
000200******************************************************************        
000300*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0107      *        
000400******************************************************************        
000500*                                                                         
000600 ID DIVISION.                                                             
000700 PROGRAM-ID.     W6030700.                                                
000800 AUTHOR.         KENT JEBSEN.                                             
000900 DATE-WRITTEN.   96/11/21.                                                
001000 DATE-COMPILED.                                                           
001100                                                                          
001200*    FUNKTION:                                                            
001300*        GODSMOTTAGNINGSHISTORIK SDC/NDC                                  
001400*                                                                         
001500*        PROGRAMMET LÄSER      WDL6                                       
001600*                              WLBENA (WDD3)                              
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W6T307                                              
002000*        MID:         W6I30701                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        MOD:         W6O30701                                            
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100*    -- CHECKED BY WY2000                                                 
003200 77  IDPGM                       PIC X(08)   VALUE 'W6030700'.            
003300                                                                          
003400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003600                                                                          
003700 77  JA                          PIC X       VALUE 'J'.                   
003800 77  NEJ                         PIC X       VALUE 'N'.                   
003900 77  YES                         PIC X       VALUE 'Y'.                   
004000                                                                          
004100*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004200 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004300 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
004400*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004500                                                                          
004600 77  WS-TIREGDAT                 PIC 9(6)    VALUE ZERO.                  
004700 77  WS-IDINLEV-REGDAT           PIC 9(6)    VALUE ZERO.                  
004800 77  WS-DAINLEV                  PIC 9(16)   VALUE ZERO.                  
004900 77  WS-FLDC                     PIC X       VALUE SPACE.                 
004910 77  FOERSTA-GAANG               PIC X(1)    VALUE 'N'.                   
004920 77  W-NDEL-KVRAPP               PIC S9(7)   VALUE ZERO  COMP-3.          
005000                                                                          
005100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005200     88  NYCKLAR-OK                          VALUE 'J'.                   
005300     88  NYCKLAR-FEL                         VALUE 'N'.                   
005400                                                                          
005500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005600     88  EGEN-MID                            VALUE '6307'.                
005700     88  GODK-MID                            VALUE '6301' '6302'          
005800                                                   '6303' '6304'          
005900                                                   '6305' '6306'          
006000                                                   '6307' '6308'          
006100                                                   '6309' '6397'.         
006200     88  HELP-MID                            VALUE '0551'.                
006300     88  HOPP-MID                            VALUE '6397'.                
006400                                                                          
006500 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006600     88  INDATA-OK                           VALUE 'J'.                   
006700     88  INDATA-FEL                          VALUE 'N'.                   
006800     EJECT                                                                
006900*      --- VALID IDDC CODES                                               
007000*                                                                         
007100*01    -COPY WWDC99                                                       
007200*01    -COPY WWDC99 -PRE INL-                                             
007300       EJECT                                                              
007400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007500 01  GENERELLA-SUBPROGRAM.                                                
007600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008000     EJECT                                                                
008100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008200*01 -COPY WMEDAREA                                                        
008300     SKIP3                                                                
008400 01  MESSAGE-CODES.                                                       
008500     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008600     03  PARTNO-MISSING          PIC X(3)    VALUE '017'.                 
008700     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008900     03  INF-CHANGE-SCREEN       PIC X(3)    VALUE '127'.                 
009000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009100     EJECT                                                                
009200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
009300*                                                                         
009400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
009500     SKIP3                                                                
009600*01 -COPY WMSGINIT                                                        
009700     EJECT                                                                
009800*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
009900*                                                                         
010000 01  SPAR-AREA.                                                           
010100     03  SPAR-IDTRANS         PIC X(4)     VALUE '6307'.                  
010200     03  SPAR-DAINLEV-ENTER   PIC 9(16)   VALUE ZERO.                     
010300     03  SPAR-DAINLEV-NEXT    PIC 9(16)   VALUE ZERO.                     
010400     03  SPAR-HOPP-DAINLEV-ENTER   PIC 9(16)   VALUE ZERO.                
010500     EJECT                                                                
010600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
010700*                                                                         
010800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010900     SKIP3                                                                
011000*01  MID -COPY W6I30701                                                   
011100     EJECT                                                                
011200 01  W-PROG-TO-PROG-SW-6397.                                              
011300     03  M-SW-LL-6397            PIC S9(4)   VALUE +79 COMP SYNC.         
011400     03  M-SW-Z1-Z2-6397         PIC X(2)    VALUE LOW-VALUE.             
011500***  03  M-SW-KDTRANS-6397       PIC X(8)    VALUE 'W6T397X '.            
011600     03  M-SW-KDTRANS-6397       PIC X(8)    VALUE 'W6T397  '.            
011700     03  M-SW-IDTRANS-6397       PIC X(4)    VALUE '6307'.                
011800     03  M-SW-KDMFSTYP-6397      PIC X(1)    VALUE '2'.                   
011900                                                                          
012000*    03  MID -COPY W6I39701 -PRE 6397-                                    
012100     EJECT                                                                
012200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012300     SKIP3                                                                
012400*01  -COPY WMSGAREA                                                       
012500     EJECT                                                                
012600     03  MOD REDEFINES MSG-AREA.                                          
012700*      05  -COPY W6O30701                                                 
012800     EJECT                                                                
012900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013000     SKIP3                                                                
013100*01  -COPY WMFSAREA                                                       
013200     EJECT                                                                
013300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013400*                                                                         
013500     EJECT                                                                
013600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013700     SKIP3                                                                
013800 01  NYCKLAR-TILL-DLI.                                                    
013900*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
014000     03  W-DAINLEV-MIN-X.                                                 
014100         05  W-DAINLEV-MIN       PIC 9(16)  VALUE ZERO.                   
014200                                                                          
014210     03  W-DAINLEV-X.                                                     
014220         05  W-DAINLEV           PIC 9(16)  VALUE ZERO.                   
014230                                                                          
014300     03  W-IDARTNR-X.                                                     
014400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
014500                                                                          
014600     03  W-IDSKYLT-X.                                                     
014700         05  W-IDSKYLT           PIC X(3)     VALUE 'GB '.                
014800     SKIP2                                                                
014900*    --- STATUS-KOD FRÅN IMS                                              
015000 01  STATUS-WS                   PIC XX.                                  
015100     88  SEGMENT-FINNS                       VALUE '  '.                  
015200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015400     SKIP2                                                                
015500 01  GODK-STATUSKODER.                                                    
015600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015700     SKIP3                                                                
015800 01  SSA1                        PIC X(64).                               
015900 01  SSA2                        PIC X(64).                               
016000     EJECT                                                                
016100*    --- IMS FUNKTIONSKODER                                               
016200*01  -COPY W0003                                                          
016300     EJECT                                                                
016400*    ---  DLI INPUT-OUTPUT AREA                                           
016500                                                                          
016600 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL601'.                      
016700 01  DLI-IO-WDL601.                                                       
016800*    03  -COPY WDL601                                                     
016900     EJECT                                                                
017000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL611'.                      
017100 01  DLI-IO-WDL611.                                                       
017200*    03  -COPY WDL611                                                     
017300     EJECT                                                                
017310 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL621'.                      
017320 01  DLI-IO-WDL621.                                                       
017330*    03  -COPY WDL621                                                     
017340     EJECT                                                                
017400 01  DLI-IO-AREA2.                                                        
017500     03  WLBENA11.                                                        
017600*        05  -COPY WDD311  -PRE BENA-                                     
017700     EJECT                                                                
017800 LINKAGE SECTION.                                                         
017900*01  -COPY W0009   -PRE MSG-                                              
018000     EJECT                                                                
018100*01  -COPY W0009   -PRE ALT-                                              
018200     EJECT                                                                
018300*01  -COPY W0008   -PRE USEA-                                             
018400     05  FILLER                  PIC X.                                   
018500     EJECT                                                                
018600*01  -COPY W0008  -PRE WDL6-                                              
018700     05  FILLER                  PIC X.                                   
018800     EJECT                                                                
018900*01  -COPY W0008  -PRE BENA-                                              
019000     05  FILLER                  PIC X.                                   
019100     EJECT                                                                
019200 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB WDL6-PCB              
019300                           BENA-PCB.                                      
019400 MAIN SECTION.                                                            
019500     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB WDL6-PCB              
019600                           BENA-PCB.                                      
019700                                                                          
019800     PERFORM IMS-GET-MSG                                                  
019900     IF SEGMENT-FINNS                                                     
020000       PERFORM A-INIT                                                     
020100       PERFORM B-KOLLA-NYCKLAR                                            
020200       IF NYCKLAR-OK                                                      
020300          PERFORM G-KOLLA-INPUT                                           
020400          IF INDATA-OK                                                    
020500             IF MFS-FIRST                                                 
020600                PERFORM C-FOERSTA-SIDA                                    
020700             ELSE                                                         
020800                IF MFS-NEXT                                               
020900                   PERFORM D-NAESTA-SIDA                                  
021000                ELSE                                                      
021100                   IF MFS-SPLIT                                           
021200                      PERFORM H-BYT-BILD                                  
021300                   ELSE                                                   
021400                      PERFORM E-SAMMA-SIDA                                
021500                   END-IF                                                 
021600                END-IF                                                    
021700             END-IF                                                       
021800          ELSE                                                            
021900             MOVE MSGI-SPAR-AREA TO SPAR-AREA                             
022000             IF SPAR-IDTRANS = 6307                                       
022100                MOVE SPAR-HOPP-DAINLEV-ENTER TO W-DAINLEV-MIN             
022200             END-IF                                                       
022300          END-IF                                                          
022400          IF MFS-SPLIT AND INDATA-OK                                      
022500             CONTINUE                                                     
022600          ELSE                                                            
022700             PERFORM F-LAES-VISA-INFO                                     
022800          END-IF                                                          
022900       END-IF                                                             
023000       IF MFS-SPLIT AND INDATA-OK                                         
023100          CONTINUE                                                        
023200       ELSE                                                               
023300          COMPUTE MSG-KVLL = LENGTH OF MOD-W6O30701 + 4                   
023400          PERFORM IMS-INSERT-MSG                                          
023500       END-IF                                                             
023600     END-IF                                                               
023700                                                                          
023800     MOVE ZERO TO RETURN-CODE                                             
023900     GOBACK                                                               
024000     .                                                                    
024100     EJECT                                                                
024200 A-INIT SECTION.                                                          
024300                                                                          
024400     IF MSG-DUBBLA-TRANSKODER                                             
024500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I30701                 
024600       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
024700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
024800     ELSE                                                                 
024900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I30701                  
025000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
025100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
025200     END-IF                                                               
025300                                                                          
025400     MOVE MSG-KDTRTYP     TO MFS-KDTRTYP                                  
025500     MOVE MSG-IDPFK       TO MFS-IDPFK                                    
025600     MOVE MFS-IDTRANS     TO W-IDTRANS                                    
025700                                                                          
025800     MOVE LOW-VALUE       TO MSG-AREA                                     
025900     MOVE 'W6O307N1'      TO MFS-IDMOD                                    
026000     MOVE '6307'          TO MOD-IDTRANS                                  
026100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
026200                                                                          
026300     IF EGEN-MID OR HELP-MID                                              
026400       CONTINUE                                                           
026500     ELSE                                                                 
026600       MOVE SPACE TO MFS-KDTRTYP                                          
026700       MOVE '7'   TO MFS-IDPFK                                            
026800       MOVE ALL '+' TO MID-FLMORE                                         
026900     END-IF                                                               
027000     .                                                                    
027100     EJECT                                                                
027200 B-KOLLA-NYCKLAR SECTION.                                                 
027300                                                                          
027400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
027500     MOVE '001'             TO MSGI-KDCALL                                
027600     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
027700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
027800     MOVE '6307'            TO MSGI-IDTRANS                               
027900     IF EGEN-MID                                                          
028000       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
028100     END-IF                                                               
028200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
028300     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
028400                                                                          
028500     MOVE JA TO NYCKLAR-SW                                                
028600                                                                          
028700                                                                          
028800*    -- KONTROLL AV IDARTNR OCH IDDC                                      
028900     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
029000                                                                          
029100     IF MID-IDARTNR-IN  NOT = ALL '+'                                     
029200       MOVE '7'         TO MFS-IDPFK                                      
029300       MOVE SPACE       TO MFS-KDTRTYP                                    
029400       MOVE ALL '+' TO MID-FLMORE                                         
029500     END-IF                                                               
029600     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
029700     IF MSGI-IDARTNR NUMERIC                                              
029800       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
029900     ELSE                                                                 
030000       MOVE NEJ TO NYCKLAR-SW                                             
030100     END-IF                                                               
030200                                                                          
030300*    -- KONTROLL AV IDDC                                                  
030400     MOVE MFS-RENSA-FAELT             TO MOD-IDDC-IN                      
030500                                                                          
030600     IF EGEN-MID OR HOPP-MID                                              
030700       IF MID-IDDC-IN                 NOT = ALL '+'                       
030800         MOVE MID-IDDC-IN             TO WS-IDDC                          
030900         MOVE '7'                     TO MFS-IDPFK                        
031000         MOVE SPACE                   TO MFS-KDTRTYP                      
031100         MOVE ALL '+' TO MID-FLMORE                                       
031200       ELSE                                                               
031300         MOVE MID-IDDC-UT             TO WS-IDDC                          
031400       END-IF                                                             
031500     ELSE                                                                 
031600       MOVE MSGI-IDDC-KEY             TO WS-IDDC                          
031700       MOVE ALL '+' TO MID-FLMORE                                         
031800     END-IF                                                               
031900                                                                          
032000*    INSPECT WS-IDDC REPLACING LEADING SPACE BY ZERO                      
032100*    IF WS-IDDC NUMERIC                                                   
032200     IF GOOD-DC                                                           
032300     OR WS-IDDC NUMERIC                                                   
032400        CONTINUE                                                          
032500     ELSE                                                                 
032600        MOVE NEJ                    TO NYCKLAR-SW                         
032700     END-IF                                                               
032800                                                                          
032900                                                                          
033000     IF GODK-MID OR NYCKLAR-OK                                            
033100       MOVE MSGI-IDARTNR        TO MOD-IDARTNR-UT                         
033200       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
033300       MOVE WS-IDDC             TO MOD-IDDC-UT                            
033400       INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE                
033500     ELSE                                                                 
033600       MOVE MFS-RENSA-FAELT     TO MOD-IDARTNR-UT                         
033700                                   MOD-IDDC-UT                            
033800     END-IF                                                               
033900                                                                          
034000     IF NYCKLAR-FEL                                                       
034100       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
034200       CALL WMEDKONV USING MED-WMEDAREA                                   
034300       MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                 
034400       PERFORM MFS-RENSA-FAELT-IN                                         
034500       PERFORM MFS-RENSA-FAELT-UT                                         
034600     END-IF                                                               
034700     .                                                                    
034800     EJECT                                                                
034900 C-FOERSTA-SIDA SECTION.                                                  
035000                                                                          
035100     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
035200     CALL WMEDKONV USING MED-WMEDAREA                                     
035300     MOVE MED-MFSINF     TO MOD-TEMFSFEL                                  
035400     PERFORM MFS-RENSA-FAELT-IN                                           
035500     MOVE MFS-FORMATETS-ATTR TO MOD-FLMORE-ATTR                           
035600     .                                                                    
035700     EJECT                                                                
035800 D-NAESTA-SIDA SECTION.                                                   
035900                                                                          
036000     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
036100     IF SPAR-IDTRANS = 6307                                               
036200        MOVE SPAR-DAINLEV-NEXT TO W-DAINLEV-MIN                           
036300     END-IF                                                               
036400     .                                                                    
036500     EJECT                                                                
036600 E-SAMMA-SIDA SECTION.                                                    
036700                                                                          
036800     IF SPAR-IDTRANS = 6307                                               
036900        MOVE SPAR-DAINLEV-ENTER TO W-DAINLEV-MIN                          
037000        IF MID-IDARTNR-IN = ALL '+' AND WS-IDDC = ALL '+'                 
037100          PERFORM MFS-RENSA-FAELT-IN                                      
037200        ELSE                                                              
037300          MOVE INF-FIRST-PAGE TO MED-IDMFSINF                             
037400          CALL WMEDKONV USING MED-WMEDAREA                                
037500          MOVE MED-MFSINF     TO MOD-TEMFSFEL                             
037600        END-IF                                                            
037700     END-IF                                                               
037800     .                                                                    
037900     EJECT                                                                
038000 F-LAES-VISA-INFO SECTION.                                                
038100                                                                          
038200     PERFORM FA-LAES-GRUNDDATA                                            
038300                                                                          
038400     IF SEGMENT-SAKNAS                                                    
038500        MOVE PARTNO-MISSING TO MED-IDMFSFEL                               
038600        CALL WMEDKONV USING MED-WMEDAREA                                  
038700        MOVE MED-MFSFEL     TO MOD-TEMFSFEL                               
038800        PERFORM MFS-RENSA-FAELT-UT                                        
038900     ELSE                                                                 
039000                                                                          
039100       PERFORM IMS-GU-WLBENA11                                            
039200       IF SEGMENT-FINNS                                                   
039300         MOVE BENA-TEXT-BEART TO MOD-BEART                                
039400       ELSE                                                               
039500         MOVE SPACE           TO MOD-BEART                                
039600       END-IF                                                             
039700                                                                          
039800       PERFORM FB-LAES-RADDATA                                            
039900                                                                          
040000       IF SEGMENT-FINNS                                                   
040100         MOVE INL-DAINLEV   TO SPAR-DAINLEV-ENTER                         
040110                               W-DAINLEV                                  
040200         MOVE INL-IDDC      TO INL-WS-IDDC                                
040300         IF (WS-IDDC = '20' AND INL-SDC)                                  
040400         OR (WS-IDDC = '40' AND INL-NDC-NA)                               
040500         OR (WS-IDDC = '60' AND INL-NDC-PACIFIC)                          
040600         OR WS-IDDC  = INL-IDDC                                           
040700            MOVE 'J' TO WS-FLDC                                           
040800         ELSE                                                             
040900            MOVE 'N' TO WS-FLDC                                           
041000         END-IF                                                           
041100         IF WS-FLDC = 'J'                                                 
041200            MOVE +1 TO INDX                                               
041300         ELSE                                                             
041400            MOVE +0 TO INDX                                               
041500         END-IF                                                           
041600       ELSE                                                               
041700         MOVE W-DAINLEV-MIN TO SPAR-DAINLEV-ENTER                         
041800       END-IF                                                             
041900                                                                          
042000       PERFORM UNTIL INDX > MAX-INDX                                      
042100         IF SEGMENT-FINNS                                                 
042300           IF WS-FLDC = 'J'                                               
042400             IF INDX = +1                                                 
042500               MOVE INL-DAINLEV    TO SPAR-HOPP-DAINLEV-ENTER             
042600             END-IF                                                       
042700             MOVE INL-IDPTYP       TO MOD-IDPTYP   (INDX)                 
042800             MOVE INL-IDDC         TO MOD-IDDC     (INDX)                 
042900             MOVE INL-IDLEVNR      TO MOD-IDLEVNR  (INDX)                 
043000             MOVE INL-KDRT         TO MOD-KDRT     (INDX)                 
043100                                                                          
043200             IF INL-IDPTYP = 'R30' OR '310'                               
043300               MOVE INL-IDFAKT     TO MOD-IDLOPNRM (INDX)                 
043400             END-IF                                                       
043500             IF INL-IDPTYP = 'R31'                                        
043600               MOVE INL-IDLOPNRM   TO MOD-IDLOPNRM (INDX)                 
043700             END-IF                                                       
043800             IF INL-IDPTYP = 'R32' OR 'W33' OR 'R34' OR 'R40'             
043900               IF INL-IDLOPNRM = 0                                        
044000                 MOVE INL-IDFAKT   TO MOD-IDLOPNRM (INDX)                 
044100               ELSE                                                       
044200                 MOVE INL-IDLOPNRM TO MOD-IDLOPNRM (INDX)                 
044300               END-IF                                                     
044400             END-IF                                                       
044500                                                                          
044600             MOVE INL-IDKUNDRF     TO MOD-IDKUNDRF (INDX)                 
044700                                                                          
044800             IF INL-TIAVIDAT > 0                                          
044820               MOVE INL-TIAVIDAT   TO MOD-TIREGDAT (INDX)                 
044830             ELSE                                                         
044900               MOVE INL-DAINLEV    TO WS-DAINLEV                          
045000               MOVE WS-DAINLEV (3:6)                                      
045010                                   TO WS-IDINLEV-REGDAT                   
045100               COMPUTE WS-TIREGDAT = 999999 - WS-IDINLEV-REGDAT           
045200               MOVE WS-TIREGDAT    TO MOD-TIREGDAT (INDX)                 
045210             END-IF                                                       
045300                                                                          
045400             MOVE INL-TIINLINL     TO MOD-TIINLINL (INDX)                 
045500             MOVE INL-KVAVIS       TO MOD-KVAVIS   (INDX)                 
045600             MOVE INL-KVANTMOT     TO MOD-KVANTMOT (INDX)                 
045700             MOVE INL-KVART-SKROT  TO MOD-KVART-SKROT (INDX)              
045800                                                                          
045900             IF INL-FLMAKUL = 'J'                                         
046000               MOVE 'Y'            TO MOD-FLMAKUL  (INDX)                 
046100             ELSE                                                         
046200               MOVE INL-FLMAKUL    TO MOD-FLMAKUL  (INDX)                 
046300             END-IF                                                       
046500           END-IF                                                         
046600         ELSE                                                             
046610           IF INDX = 0                                                    
046620             MOVE 1 TO INDX                                               
046630           END-IF                                                         
046640           PERFORM MFS-RENSA-RAD-FAELT-UT                                 
046800         END-IF                                                           
046900                                                                          
046901**       READ WDL621 SEGMENTS FOR P32                                     
046902         IF WS-FLDC = 'J'                                                 
046903           PERFORM FC-GET-P32-SEGMENTS                                    
046904         END-IF                                                           
046905                                                                          
046910**       CONTINUE WITH WDL611 READING                                     
046920         IF INDX > MAX-INDX                                               
046930           CONTINUE                                                       
046940         ELSE                                                             
047000           PERFORM FB-LAES-RADDATA                                        
047200           IF SEGMENT-FINNS                                               
047400             MOVE INL-IDDC        TO INL-WS-IDDC                          
047500             MOVE INL-DAINLEV     TO W-DAINLEV                            
047600             IF (WS-IDDC = '20' AND INL-SDC)                              
047700             OR (WS-IDDC = '40' AND INL-NDC-NA)                           
047800             OR (WS-IDDC = '60' AND INL-NDC-PACIFIC)                      
047900             OR WS-IDDC      = INL-IDDC                                   
048000                MOVE 'J' TO WS-FLDC                                       
048100             ELSE                                                         
048200                MOVE 'N' TO WS-FLDC                                       
048300             END-IF                                                       
048400           END-IF                                                         
048500           IF WS-FLDC = 'J' OR SEGMENT-SAKNAS                             
048600             ADD 1 TO INDX                                                
048700           END-IF                                                         
048800         END-IF                                                           
048900       END-PERFORM                                                        
049000                                                                          
049100       IF SEGMENT-FINNS                                                   
049200         MOVE INL-DAINLEV          TO SPAR-DAINLEV-NEXT                   
049300         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
049400         CALL WMEDKONV             USING MED-WMEDAREA                     
049500         MOVE MED-TEMFSINF         TO MOD-TEMFSINF                        
049600       ELSE                                                               
049700         MOVE SPAR-DAINLEV-ENTER   TO SPAR-DAINLEV-NEXT                   
049800       END-IF                                                             
049900                                                                          
050000       MOVE '002'      TO MSGI-KDCALL                                     
050100       MOVE '6307'     TO MSGI-IDTRANS                                    
050200       MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                  
050300       CALL W005INIT   USING MSGI-WMSGINIT USEA-PCB                       
050400     END-IF                                                               
050500     .                                                                    
050600     EJECT                                                                
050700 FA-LAES-GRUNDDATA SECTION.                                               
050800                                                                          
050900     PERFORM IMS-GU-WDL601                                                
051000     .                                                                    
051100     EJECT                                                                
051200 FB-LAES-RADDATA SECTION.                                                 
051400     PERFORM IMS-GNP-WDL611                                               
051500     .                                                                    
051510 FC-GET-P32-SEGMENTS  SECTION.                                            
051511     MOVE JA   TO FOERSTA-GAANG                                           
051520     PERFORM IMS-GNP-WDL621                                               
051524     IF SEGMENT-FINNS                                                     
051527       ADD +1 TO INDX                                                     
051528     END-IF                                                               
051529*                                                                         
051530     PERFORM UNTIL NOT (SEGMENT-FINNS AND INDX <= MAX-INDX)               
051531       IF FOERSTA-GAANG = JA                                              
051532         MOVE ZERO           TO W-NDEL-KVRAPP                             
051533         PERFORM FCA-FLYTTA-SPAR                                          
051534         MOVE NEJ            TO FOERSTA-GAANG                             
051535       END-IF                                                             
051536       COMPUTE W-NDEL-KVRAPP = W-NDEL-KVRAPP + NDEL-KVRAPP                
051537       MOVE W-NDEL-KVRAPP    TO MOD-KVANTMOT(INDX)                        
051538       PERFORM IMS-GNP-WDL621                                             
051539     END-PERFORM                                                          
051540*                                                                         
051547     IF SEGMENT-FINNS                                                     
051549       MOVE W-DAINLEV (2:15) TO SPAR-DAINLEV-NEXT                         
051552     END-IF                                                               
051553     .                                                                    
051554     EJECT                                                                
051555 FCA-FLYTTA-SPAR SECTION.                                                 
051556     SKIP2                                                                
051557     MOVE 'P32'             TO MOD-IDPTYP     (INDX)                      
051558     MOVE NDEL-TIREGDAT     TO MOD-TIINLINL   (INDX)                      
051559     MOVE MFS-RENSA-FAELT   TO MOD-IDLOPNRM   (INDX)                      
051560                               MOD-IDDC       (INDX)                      
051561                               MOD-IDLEVNR    (INDX)                      
051562                               MOD-KDRT       (INDX)                      
051563                               MOD-IDKUNDRF   (INDX)                      
051564                               MOD-TIREGDAT   (INDX)                      
051565                               MOD-KVANTMOT   (INDX)                      
051566                               MOD-KVART-SKROT(INDX)                      
051567                               MOD-FLMAKUL    (INDX)                      
051568                                                                          
051578     .                                                                    
051579     EJECT                                                                
051700 G-KOLLA-INPUT SECTION.                                                   
051900     MOVE JA TO INDATA-SW                                                 
052000                                                                          
052100     IF MID-FLMORE = ALL '+' OR SPACE                                     
052200        IF MFS-SPLIT                                                      
052300           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLMORE-ATTR                     
052400           MOVE NEJ TO INDATA-SW                                          
052500        ELSE                                                              
052600           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLMORE-ATTR                   
052700        END-IF                                                            
052800     ELSE                                                                 
052900        IF MFS-SPLIT                                                      
053000           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLMORE-ATTR                   
053100        ELSE                                                              
053200           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLMORE-ATTR                     
053300           MOVE NEJ TO INDATA-SW                                          
053400           MOVE INF-CHANGE-SCREEN TO MED-IDMFSFEL                         
053500           CALL WMEDKONV USING MED-WMEDAREA                               
053600           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
053700        END-IF                                                            
053800        IF MID-FLMORE = JA OR YES                                         
053900           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLMORE-ATTR                   
054000        ELSE                                                              
054100           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLMORE-ATTR                     
054200           MOVE NEJ TO INDATA-SW                                          
054300        END-IF                                                            
054400     END-IF                                                               
054500                                                                          
054600     IF INDATA-FEL                                                        
054700        IF MOD-TEMFSFEL > SPACE                                           
054800           CONTINUE                                                       
054900        ELSE                                                              
055000           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
055100           CALL WMEDKONV USING MED-WMEDAREA                               
055200           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
055300        END-IF                                                            
055400        MOVE MFS-ROER-EJ-FAELT TO MOD-FLMORE                              
055500     END-IF                                                               
055600     .                                                                    
055700     EJECT                                                                
055800 H-BYT-BILD SECTION.                                                      
055900                                                                          
056000     MOVE LOW-VALUE    TO 6397-MID-W6I39701                               
056100     MOVE MSGI-IDARTNR TO 6397-MID-IDARTNR-IN                             
056200     MOVE WS-IDDC      TO 6397-MID-IDDC-IN                                
056300     PERFORM IMS-INSERT-ALT-MSG-6397                                      
056400     .                                                                    
056500     EJECT                                                                
056600 MFS-RENSA-FAELT-UT SECTION.                                              
056700                                                                          
056800     MOVE 0 TO SPAR-DAINLEV-ENTER                                         
056900               SPAR-DAINLEV-NEXT                                          
057000               W-DAINLEV-MIN                                              
057100               SPAR-HOPP-DAINLEV-ENTER                                    
057200                                                                          
057300     MOVE +1 TO INDX                                                      
057400     PERFORM UNTIL INDX > MAX-INDX                                        
057500       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
057600       ADD +1 TO INDX                                                     
057700     END-PERFORM                                                          
057800     .                                                                    
057900     SKIP3                                                                
058000 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
058100                                                                          
058200*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
058300     MOVE MFS-RENSA-FAELT TO MOD-IDPTYP      (INDX)                       
058400                             MOD-IDDC        (INDX)                       
058500                             MOD-IDLEVNR     (INDX)                       
058600                             MOD-KDRT        (INDX)                       
058700                             MOD-IDLOPNRM    (INDX)                       
058800                             MOD-IDKUNDRF    (INDX)                       
058900                             MOD-TIREGDAT    (INDX)                       
059000                             MOD-TIINLINL    (INDX)                       
059100                             MOD-KVAVIS      (INDX)                       
059200                             MOD-KVANTMOT    (INDX)                       
059300                             MOD-KVART-SKROT (INDX)                       
059400                             MOD-FLMAKUL     (INDX)                       
059500     .                                                                    
059600     EJECT                                                                
059700     SKIP3                                                                
059800 MFS-RENSA-FAELT-IN SECTION.                                              
059900                                                                          
060000*    --- ALLA INDATA-FÄLT                                                 
060100     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
060200                             MOD-IDDC-IN                                  
060300                             MOD-FLMORE                                   
060400     .                                                                    
060500     EJECT                                                                
060600* --- IMS SEKTIONER ---                                                   
060700     SKIP3                                                                
060800 IMS-GET-MSG SECTION.                                                     
060900                                                                          
061000     MOVE '  QC' TO GODK-STATUSKODER                                      
061100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
061200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
061300     PERFORM IMS-STATUSKONTROLL                                           
061400     .                                                                    
061500     SKIP3                                                                
061600 IMS-INSERT-MSG SECTION.                                                  
061700                                                                          
061800     MOVE LOW-VALUE       TO MSG-KDZ1 MSG-KDZ2                            
061900     MOVE SPACE           TO GODK-STATUSKODER                             
062000     CALL CBLTDLI         USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD        
062100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
062200     PERFORM IMS-STATUSKONTROLL                                           
062300     .                                                                    
062400     SKIP3                                                                
062500 IMS-INSERT-ALT-MSG-6397 SECTION.                                         
062600     MOVE SPACE TO GODK-STATUSKODER                                       
062700     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW-6397               
062800     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
062900     PERFORM IMS-STATUSKONTROLL                                           
063000     .                                                                    
063100     EJECT                                                                
063200 IMS-GU-WDL601    SECTION.                                                
063300                                                                          
063400     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
063500          DELIMITED BY SIZE INTO SSA1                                     
063600     MOVE '  GE'           TO GODK-STATUSKODER                            
063700     CALL CBLTDLI          USING GU WDL6-PCB DLI-IO-WDL601 SSA1           
063800     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
063900     PERFORM IMS-STATUSKONTROLL                                           
064000     .                                                                    
064100     EJECT                                                                
064200 IMS-GNP-WDL611   SECTION.                                                
064400     STRING 'WDL611  (DAINLEV =>' W-DAINLEV-MIN-X ')'                     
064500          DELIMITED BY SIZE INTO SSA1                                     
064600     MOVE '  GE'           TO GODK-STATUSKODER                            
064700     CALL CBLTDLI          USING GNP WDL6-PCB DLI-IO-WDL611   SSA1        
064800     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
064900     PERFORM IMS-STATUSKONTROLL                                           
065000     .                                                                    
065100     EJECT                                                                
065110 IMS-GNP-WDL621   SECTION.                                                
065120     STRING 'WDL611  (DAINLEV = ' W-DAINLEV-X ')'                         
065130          DELIMITED BY SIZE INTO SSA1                                     
065131     MOVE 'WDL621  '       TO SSA2                                        
065140     MOVE '  GE'           TO GODK-STATUSKODER                            
065150     CALL CBLTDLI USING GNP WDL6-PCB DLI-IO-WDL621 SSA1 SSA2              
065160     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
065170     PERFORM IMS-STATUSKONTROLL                                           
065180     .                                                                    
065190     EJECT                                                                
065200 IMS-GU-WLBENA11 SECTION.                                                 
065300                                                                          
065400     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
065500          DELIMITED BY SIZE INTO SSA1                                     
065600     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
065700          DELIMITED BY SIZE INTO SSA2                                     
065800     MOVE '  GE' TO GODK-STATUSKODER                                      
065900     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA2 SSA1 SSA2                
066000     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
066100     PERFORM IMS-STATUSKONTROLL                                           
066200     .                                                                    
066300     EJECT                                                                
066400 IMS-STATUSKONTROLL SECTION.                                              
066500                                                                          
066600     SET STATUS-IX TO 1                                                   
066700     SEARCH GODK-STATUS                                                   
066800       AT END                                                             
066900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
067000         DELIMITED BY SIZE INTO FELTEXT                                   
067100         CALL FELLOG                                                      
067200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
067300         CONTINUE                                                         
067400     END-SEARCH                                                           
067500     .                                                                    
