000100*                                                                         
000200******************************************************************        
000300*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0106      *        
000400******************************************************************        
000500*                                                                         
000600 ID DIVISION.                                                             
000700                                                                          
000800 PROGRAM-ID.     W6030600.                                                
000900 AUTHOR.         MARTIEN HOMPES.                                          
001000 DATE-WRITTEN.   96/03/12.                                                
001100 DATE-COMPILED.                                                           
001200                                                                          
001300*    FUNKTION:                                                            
001400*        LOCATION ENQUIRY SDC AND CDC                                     
001500*        PROGRAM READS FOLLOWING DATABASE WLARTR = WDK7A                  
001600*                                         WLBENA = WDD311                 
001700*                                         WDB601                          
001710*                                         WDK6E1 - FOR CDC                
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W6T306                                              
002100*        MID:         W6I30601                                            
002200*                                                                         
002300*    UTDATA.                                                              
002400*        MOD:         W6O30601                                            
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800 DATA DIVISION.                                                           
002900     EJECT                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003200*    -- CHECKED BY WY2000                                                 
003300 77  IDPGM                       PIC X(08)   VALUE 'W6030600'.            
003400                                                                          
003500*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003600 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003700                                                                          
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004000                                                                          
004100 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004200 77  MAX-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
004300                                                                          
004400 77  WS-ADLAGOMR                 PIC 9(3)   VALUE ZERO.                   
004500 77  WS-ADGANG                   PIC 9(3)   VALUE ZERO.                   
004600 77  WS-ADPLATS                  PIC 9(5)   VALUE ZERO.                   
004610 77  WS-IDARTNR                  PIC S9(9)  VALUE ZERO  COMP-3.           
004700                                                                          
004800*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004900 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005000     88  ALLT-OK                             VALUE 'J'.                   
005100                                                                          
005200 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005300     88  INDATA-OK                           VALUE 'J'.                   
005400     88  INDATA-FEL                          VALUE 'N'.                   
005500                                                                          
005600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005700     88  NYCKLAR-OK                          VALUE 'J'.                   
005800     88  NYCKLAR-FEL                         VALUE 'N'.                   
005900                                                                          
006000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006100     88  EGEN-MID                            VALUE '6306'.                
006200     88  GODK-MID                            VALUE '6306'.                
006300     88  HELP-MID                            VALUE '0551'.                
006400                                                                          
006500     EJECT                                                                
006600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006700 01  GENERELLA-SUBPROGRAM.                                                
006800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007200     EJECT                                                                
007300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
007400*01 -COPY WMSGINIT                                                        
007500     EJECT                                                                
007600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007700*01 -COPY WMEDAREA                                                        
007701     SKIP3                                                                
007710*    --- VALID DC CODES                                                   
007720*01 -COPY WWDC99                                                          
007800     SKIP3                                                                
007900 01  MESSAGE-CODES.                                                       
008000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
008100     03  KEYS-MISSING            PIC X(3)    VALUE '005'.                 
008200     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008300     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008400     03  INF-LAST-PAGE-SHOWN     PIC X(3)    VALUE '115'.                 
008500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008600     03                          PIC X(3)    VALUE '105'.                 
008700     EJECT                                                                
008800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008900*                                                                         
009000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009100     SKIP3                                                                
009200*01  MID -COPY W6I30601                                                   
009300     EJECT                                                                
009400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009500     SKIP3                                                                
009600*01  -COPY WMSGAREA                                                       
009700     EJECT                                                                
009800     03  MOD REDEFINES MSG-AREA.                                          
009900*      05  -COPY W6O30601                                                 
010000     EJECT                                                                
010100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010200*01  -COPY WMFSAREA                                                       
010300     EJECT                                                                
010400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010500*                                                                         
010600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010700     SKIP3                                                                
010800 01  NYCKLAR-TILL-DLI.                                                    
010900   03   W-IDARTNR-X.                                                      
011000     05 W-IDARTNR             PIC S9(9)  COMP-3  VALUE ZERO.              
011100                                                                          
011200   03   W-IDSKYLT-X.                                                      
011300     05 W-IDSKYLT             PIC X(3).                                   
011400                                                                          
011500   03  WDK7A1KY-MIN-X.                                                    
011600     05  W-IDDC-MIN          PIC X(2)           VALUE SPACE.              
011700     05  W-ADART-MIN.                                                     
011800       07  W-ADLAGOMR-MIN    PIC S9(3)  COMP-3  VALUE ZERO.               
011900       07  W-ADGANG-MIN      PIC S9(3)  COMP-3  VALUE ZERO.               
012000       07  W-ADPLATS-MIN     PIC S9(5)  COMP-3  VALUE ZERO.               
012100     05 W-IDARTNR-MIN        PIC S9(9)  COMP-3  VALUE ZERO.               
012200                                                                          
012300   03  WDK7A1KY-MAX-X.                                                    
012400     05  W-IDDC-MAX          PIC X(2)           VALUE SPACE.              
012500     05  W-ADART-MAX.                                                     
012600       07  W-ADLAGOMR-MAX    PIC S9(3)  COMP-3  VALUE ZERO.               
012700       07  W-ADGANG-MAX      PIC S9(3)  COMP-3  VALUE ZERO.               
012800       07  W-ADPLATS-MAX     PIC S9(5)  COMP-3  VALUE ZERO.               
012900     05 W-IDARTNR-MAX        PIC S9(9)  COMP-3  VALUE ZERO.               
013000                                                                          
013100   03  W-IDDC-B6-X.                                                       
013200       05 W-IDDC-B6            PIC X(2).                                  
013300                                                                          
013400   03  WDK6E1KY-MIN-X.                                                    
013410     05  W-ADART-CDC-MIN.                                                 
013420       07  W-ADLAGOMR-CDC-MIN  PIC S9(3)  COMP-3  VALUE ZERO.             
013430       07  W-ADGANG-CDC-MIN    PIC S9(3)  COMP-3  VALUE ZERO.             
013440       07  W-ADPLATS-CDC-MIN   PIC S9(5)  COMP-3  VALUE ZERO.             
013450     05 W-IDARTNR-CDC-MIN      PIC S9(9)  COMP-3  VALUE ZERO.             
013460                                                                          
013470   03  WDK6E1KY-MAX-X.                                                    
013480     05  W-ADART-CDC-MAX.                                                 
013490       07  W-ADLAGOMR-CDC-MAX  PIC S9(3)  COMP-3  VALUE ZERO.             
013491       07  W-ADGANG-CDC-MAX    PIC S9(3)  COMP-3  VALUE ZERO.             
013492       07  W-ADPLATS-CDC-MAX   PIC S9(5)  COMP-3  VALUE ZERO.             
013493     05 W-IDARTNR-CDC-MAX      PIC S9(9)  COMP-3  VALUE 99999999.         
013494                                                                          
013500     SKIP2                                                                
013600*    --- STATUS-KOD FRÅN IMS                                              
013700 01  STATUS-WS                   PIC XX.                                  
013800     88  STATUS-OK                           VALUE '  '.                  
013900     88  SEGMENT-FINNS                       VALUE '  '.                  
014000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014100     88  BASEN-SLUT                          VALUE 'GB'.                  
014200     88  TRANSKOD-FEL                        VALUE 'A1'.                  
014300     88  SECURITY-FEL                        VALUE 'A4'.                  
014400     SKIP2                                                                
014500 01  GODK-STATUSKODER.                                                    
014600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014700     SKIP3                                                                
014800 01  SSA1                        PIC X(64).                               
014900 01  SSA2                        PIC X(64).                               
015000     EJECT                                                                
015100*    --- IMS FUNKTIONSKODER                                               
015200*01  -COPY W0003                                                          
015300     EJECT                                                                
015400*    ---  DLI INPUT-OUTPUT AREA                                           
015500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
015600     SKIP3                                                                
015700 01  DLI-IO-AREA.                                                         
015800     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
015900     SKIP3                                                                
016000     03  WLARTR   REDEFINES IO-AREA.                                      
016100*        05  -COPY WDK7A1                                                 
016200     EJECT                                                                
016300     03  WLBENA     REDEFINES IO-AREA.                                    
016400*        05  -COPY WDD311                                                 
016500                                                                          
016600 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
016700 01   DLI-IO-AREA-B601.                                                   
016800*     03  -COPY WDB601                                                    
016900                                                                          
016910 01  FILLER               PIC X(16)   VALUE 'WDK6E1-AREA'.                
016920 01  DLI-IO-AREA-WDK6E1.                                                  
016930*    03  -COPY WDK6E1                                                     
016940                                                                          
017000     EJECT                                                                
017100 LINKAGE SECTION.                                                         
017200                                                                          
017300*01  -COPY W0009   -PRE MSG-                                              
017400     EJECT                                                                
017500*01  -COPY W0008  -PRE USEA-                                              
017600     05  FILLER                  PIC X.                                   
017700                                                                          
017800*01  -COPY W0008  -PRE ARTR-                                              
017900     05  FILLER                  PIC X.                                   
018000                                                                          
018100*01  -COPY W0008  -PRE BENA-                                              
018200     05  FILLER                  PIC X.                                   
018300                                                                          
018400*01  -COPY W0008  -PRE WDB6-                                              
018500     05  FILLER                  PIC X.                                   
018501                                                                          
018510*01  -COPY W0008  -PRE WDK6E-                                             
018520     05  FILLER                  PIC X.                                   
018600                                                                          
018700     EJECT                                                                
018800 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB ARTR-PCB BENA-PCB             
018900                           WDB6-PCB WDK6E-PCB.                            
019000     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB ARTR-PCB BENA-PCB             
019100                           WDB6-PCB WDK6E-PCB.                            
019200 MAIN SECTION.                                                            
019300                                                                          
019400     PERFORM IMS-GET-MSG                                                  
019500     IF SEGMENT-FINNS                                                     
019600       PERFORM A-INIT                                                     
019700       IF GODK-MID OR HELP-MID                                            
019800          PERFORM B-KOLLA-NYCKLAR                                         
019900          IF NYCKLAR-OK                                                   
020000             IF MFS-FIRST                                                 
020100                PERFORM C-FOERSTA-SIDA                                    
020200             ELSE                                                         
020300               IF MFS-NEXT                                                
020400                  PERFORM D-NAESTA-SIDA                                   
020500                ELSE                                                      
020600                  PERFORM E-SAMMA-SIDA                                    
020700               END-IF                                                     
020800             END-IF                                                       
020900             IF ALLT-OK                                                   
021000                PERFORM F-LAES-VISA-INFO                                  
021100             END-IF                                                       
021200          END-IF                                                          
021300       END-IF                                                             
021400       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O30601 + 4                      
021500       PERFORM IMS-INSERT-MSG                                             
021600     END-IF                                                               
021700                                                                          
021800     MOVE ZERO TO RETURN-CODE                                             
021900     GOBACK                                                               
022000     .                                                                    
022100     EJECT                                                                
022200 A-INIT SECTION.                                                          
022300                                                                          
022400     IF MSG-DUBBLA-TRANSKODER                                             
022500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I30601                 
022600       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
022700       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
022800     ELSE                                                                 
022900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I30601                  
023000       MOVE MSG-IDTRANS-1                TO MFS-IDTRANS                   
023100       MOVE MSG-KDMFSFOR-1               TO MFS-KDMFSFOR                  
023200     END-IF                                                               
023300                                                                          
023400     MOVE MSG-KDTRTYP     TO MFS-KDTRTYP                                  
023500     MOVE MSG-IDPFK       TO MFS-IDPFK                                    
023600     MOVE MFS-IDTRANS     TO W-IDTRANS                                    
023700                                                                          
023800     MOVE LOW-VALUE       TO MSG-AREA                                     
023900     MOVE 'W6O306N1'      TO MFS-IDMOD                                    
024000     MOVE '6306'          TO MOD-IDTRANS                                  
024100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
024200                                                                          
024300                                                                          
024400     IF EGEN-MID OR HELP-MID                                              
024500       CONTINUE                                                           
024600     ELSE                                                                 
024700       MOVE SPACE               TO MFS-KDTRTYP                            
024800       MOVE '7'                 TO MFS-IDPFK                              
024900       PERFORM MFS-RENSA-FAELT-IN                                         
025000     END-IF                                                               
025100                                                                          
025200     .                                                                    
025300     EJECT                                                                
025400 B-KOLLA-NYCKLAR SECTION.                                                 
025500                                                                          
025600     MOVE JA TO NYCKLAR-SW                                                
025700                                                                          
025800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
025900     MOVE '001'             TO MSGI-KDCALL                                
026000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
026100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
026200     MOVE '6306'            TO MSGI-IDTRANS                               
026300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
026400                                                                          
026500     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
026600                                                                          
026700                                                                          
026800     MOVE JA              TO NYCKLAR-SW                                   
026900     MOVE MFS-RENSA-FAELT TO MOD-ADLAGOMR-IN                              
027000                             MOD-ADGANG-IN                                
027100                             MOD-ADPLATS-IN                               
027200                             MOD-IDDC-IN                                  
027300*                                                                         
027400*    -- KONTROL  ON PART LOCATION                                         
027500*                                                                         
027600     IF MID-ADLAGOMR-IN = ALL '+'                                         
027700       INSPECT MID-ADLAGOMR-UT REPLACING LEADING SPACE BY ZERO            
027800       MOVE MID-ADLAGOMR-UT TO WS-ADLAGOMR                                
027900     ELSE                                                                 
028000       IF MID-ADLAGOMR-IN NUMERIC                                         
028100          MOVE    '7'          TO MFS-IDPFK                               
028200          MOVE    SPACE        TO MFS-KDTRTYP                             
028300          MOVE MID-ADLAGOMR-IN TO WS-ADLAGOMR                             
028400       ELSE                                                               
028500          MOVE NEJ             TO NYCKLAR-SW                              
028600       END-IF                                                             
028700     END-IF                                                               
028800*                                                                         
028900     IF MID-ADGANG-IN = ALL '+'                                           
029000       INSPECT MID-ADGANG-UT REPLACING LEADING SPACE BY ZERO              
029100       MOVE MID-ADGANG-UT   TO WS-ADGANG                                  
029200     ELSE                                                                 
029300       IF MID-ADGANG-IN NUMERIC                                           
029400          MOVE    '7'          TO MFS-IDPFK                               
029500          MOVE    SPACE        TO MFS-KDTRTYP                             
029600          MOVE MID-ADGANG-IN   TO WS-ADGANG                               
029700       ELSE                                                               
029800          MOVE NEJ             TO NYCKLAR-SW                              
029900       END-IF                                                             
030000     END-IF                                                               
030100*                                                                         
030200     IF MID-ADPLATS-IN = ALL '+'                                          
030300       INSPECT MID-ADPLATS-UT REPLACING LEADING SPACE BY ZERO             
030400       MOVE MID-ADPLATS-UT  TO WS-ADPLATS                                 
030500     ELSE                                                                 
030600       IF MID-ADPLATS-IN NUMERIC                                          
030700          MOVE    '7'          TO MFS-IDPFK                               
030800          MOVE    SPACE        TO MFS-KDTRTYP                             
030900          MOVE MID-ADPLATS-IN  TO WS-ADPLATS                              
031000       ELSE                                                               
031100          MOVE NEJ             TO NYCKLAR-SW                              
031200       END-IF                                                             
031300     END-IF                                                               
031400                                                                          
031500*    -- KONTROLL AV IDDC                                                  
031600     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
031700                                                                          
031800     MOVE MSGI-IDDC       TO W-IDDC-B6                                    
031900     PERFORM IMS-GU-WDB601                                                
032000*                                                                         
032100     MOVE WS-ADLAGOMR     TO MOD-ADLAGOMR-UT                              
032200     MOVE WS-ADGANG       TO MOD-ADGANG-UT                                
032300     MOVE WS-ADPLATS      TO MOD-ADPLATS-UT                               
032400     MOVE W-IDDC-B6       TO MOD-IDDC-UT                                  
032500                                                                          
032600                                                                          
032700     IF NYCKLAR-FEL OR                                                    
032800        DCS-KDDC = SPACE OR                                               
032900        DCS-DDC                                                           
033000        MOVE NEJ             TO NYCKLAR-SW                                
033100     ELSE                                                                 
033200        MOVE WS-ADLAGOMR     TO W-ADLAGOMR-MIN                            
033300                                W-ADLAGOMR-MAX                            
033310                                W-ADLAGOMR-CDC-MIN                        
033320                                W-ADLAGOMR-CDC-MAX                        
033400        MOVE WS-ADGANG       TO W-ADGANG-MIN                              
033500                                W-ADGANG-MAX                              
033510                                W-ADGANG-CDC-MIN                          
033520                                W-ADGANG-CDC-MAX                          
033600        MOVE WS-ADPLATS      TO W-ADPLATS-MIN                             
033700                                W-ADPLATS-MAX                             
033710                                W-ADPLATS-CDC-MIN                         
033720                                W-ADPLATS-CDC-MAX                         
033800        MOVE DCS-IDDC        TO W-IDDC-MIN                                
033900                                W-IDDC-MAX                                
034000        MOVE 99999999        TO W-IDARTNR-MAX                             
034100     END-IF                                                               
034200                                                                          
034300                                                                          
034400     IF NYCKLAR-FEL                                                       
034500       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
034600       CALL WMEDKONV USING MED-WMEDAREA                                   
034700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
034800     END-IF                                                               
034900     .                                                                    
035000     EJECT                                                                
035100 C-FOERSTA-SIDA SECTION.                                                  
035200                                                                          
035300     MOVE    INF-FIRST-PAGE TO    MED-IDMFSINF                            
035400     CALL    WMEDKONV       USING MED-WMEDAREA                            
035500     MOVE    MED-MFSINF     TO    MOD-TEMFSFEL                            
035600                                                                          
035700*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
035800     MOVE    ZERO           TO    MOD-IDARTNR-ENTER                       
035900                                  MOD-IDARTNR-NEXT                        
036000     .                                                                    
036100     EJECT                                                                
036200 D-NAESTA-SIDA SECTION.                                                   
036300                                                                          
036400     INSPECT MID-IDARTNR-NEXT REPLACING LEADING SPACE BY ZERO             
036500     IF MID-IDARTNR-NEXT > ZERO                                           
036600       MOVE    MID-IDARTNR-NEXT    TO    W-IDARTNR-MIN                    
036610                                         W-IDARTNR-CDC-MIN                
036700     ELSE                                                                 
036800       MOVE    INF-LAST-PAGE-SHOWN TO    MED-IDMFSFEL                     
036900       CALL    WMEDKONV            USING MED-WMEDAREA                     
037000       MOVE    MED-MFSFEL          TO    MOD-TEMFSFEL                     
037100       PERFORM MFS-ROER-EJ-BILD                                           
037200       MOVE    NEJ                 TO    ALLT-SW                          
037300     END-IF                                                               
037400     .                                                                    
037500     EJECT                                                                
037600 E-SAMMA-SIDA SECTION.                                                    
037700                                                                          
037800     INSPECT MID-IDARTNR-ENTER REPLACING LEADING SPACE BY ZERO            
037900     IF EGEN-MID OR HELP-MID                                              
038000       MOVE MID-IDARTNR-ENTER    TO W-IDARTNR-MIN                         
038010                                    W-IDARTNR-CDC-MIN                     
038100     ELSE                                                                 
038200       PERFORM MFS-RENSA-FAELT-IN                                         
038300     END-IF                                                               
038400     .                                                                    
038500     EJECT                                                                
038600 F-LAES-VISA-INFO SECTION.                                                
038700                                                                          
038710     MOVE MSGI-IDDC               TO WS-IDDC                              
038711                                                                          
038720     IF CDC-SE                                                            
038721* READ WDK6E FOR CDC                                                      
038730        PERFORM IMS-GU-WDK6E1                                             
038731        IF SEGMENT-FINNS                                                  
038732           MOVE SEQE-IDARTNR      TO WS-IDARTNR                           
038733        END-IF                                                            
038740     ELSE                                                                 
038741* READ WDK7A FOR ALL OTHER DC                                             
038750        PERFORM IMS-GU-ARTR01                                             
038751        IF SEGMENT-FINNS                                                  
038752           MOVE SEQA-IDARTNR      TO WS-IDARTNR                           
038753        END-IF                                                            
038760     END-IF                                                               
038770                                                                          
039000     IF SEGMENT-SAKNAS                                                    
039100       IF MFS-NEXT                                                        
039200         MOVE INF-LAST-PAGE-SHOWN TO    MED-IDMFSFEL                      
039300       ELSE                                                               
039400         MOVE KEYS-MISSING        TO    MED-IDMFSFEL                      
039500       END-IF                                                             
039600       CALL    WMEDKONV  USING MED-WMEDAREA                               
039700       MOVE    MED-MFSFEL    TO    MOD-TEMFSFEL                           
039800       PERFORM MFS-RENSA-FAELT-UT                                         
039900     ELSE                                                                 
040000       MOVE WS-IDARTNR       TO MOD-IDARTNR-ENTER                         
040100       MOVE +1               TO INDX                                      
040200                                                                          
040300       PERFORM UNTIL SEGMENT-SAKNAS  OR                                   
040400                     BASEN-SLUT      OR                                   
040500                     INDX > MAX-INDX                                      
040600                                                                          
040700           PERFORM FA-BUILD-LINES                                         
040800                                                                          
040900           ADD 1      TO    INDX                                          
041000                                                                          
041010           IF CDC-SE                                                      
041020              PERFORM IMS-GN-WDK6E1                                       
041030              IF SEGMENT-FINNS                                            
041040                 MOVE SEQE-IDARTNR      TO WS-IDARTNR                     
041050              END-IF                                                      
041060           ELSE                                                           
041070              PERFORM IMS-GN-ARTR01                                       
041080              IF SEGMENT-FINNS                                            
041090                 MOVE SEQA-IDARTNR      TO WS-IDARTNR                     
041091              END-IF                                                      
041092           END-IF                                                         
041200       END-PERFORM                                                        
041300                                                                          
041400       IF SEGMENT-FINNS                                                   
041500         MOVE WS-IDARTNR           TO    MOD-IDARTNR-NEXT                 
041600         MOVE INF-MORE-INFO-EXISTS TO    MED-IDMFSFEL                     
041700         CALL WMEDKONV  USING MED-WMEDAREA                                
041800         MOVE MED-MFSFEL           TO    MOD-TEMFSINF                     
041900       ELSE                                                               
042000         MOVE    ZERO              TO    MOD-IDARTNR-NEXT                 
042100       END-IF                                                             
042200                                                                          
042300     END-IF                                                               
042400     .                                                                    
042500     EJECT                                                                
042600 FA-BUILD-LINES SECTION.                                                  
042700                                                                          
042800     MOVE WS-IDARTNR      TO MOD-IDARTNR(INDX)                            
042900                             W-IDARTNR                                    
043000                                                                          
043100     MOVE MED-IDSKYLT     TO W-IDSKYLT                                    
043200                                                                          
043300     PERFORM IMS-GET-BENA-TEXT                                            
043400                                                                          
043500     IF SEGMENT-FINNS                                                     
043600       MOVE TEXT-BEART    TO MOD-BEART-ENG(INDX)                          
043700     ELSE                                                                 
043800       MOVE SPACES        TO MOD-BEART-ENG(INDX)                          
043900     END-IF                                                               
044000     .                                                                    
044100     EJECT                                                                
044200 MFS-RENSA-FAELT-IN SECTION.                                              
044300                                                                          
044400     MOVE MFS-RENSA-FAELT TO MOD-ADLAGOMR-UT                              
044500                             MOD-ADGANG-UT                                
044600                             MOD-ADPLATS-UT                               
044700                             MOD-ADLAGOMR-IN                              
044800                             MOD-ADGANG-IN                                
044900                             MOD-ADPLATS-IN                               
045000     .                                                                    
045100     EJECT                                                                
045200 MFS-RENSA-FAELT-UT SECTION.                                              
045300                                                                          
045400*    --- ALLA UTDATA-FÄLT                                                 
045500*    --- INKL. BLÄDDRINGSNYCKLAR                                          
045600     MOVE +1 TO INDX                                                      
045700     PERFORM UNTIL INDX > MAX-INDX                                        
045800       MOVE MFS-RENSA-FAELT   TO MOD-IDARTNR   (INDX)                     
045900                                 MOD-BEART-ENG (INDX)                     
046000       ADD +1 TO INDX                                                     
046100     END-PERFORM                                                          
046200     .                                                                    
046300     EJECT                                                                
046400 MFS-ROER-EJ-BILD SECTION.                                                
046500                                                                          
046600     MOVE MFS-ROER-EJ-FAELT   TO MOD-IDARTNR-ENTER                        
046700     MOVE +1                  TO INDX                                     
046800     PERFORM UNTIL INDX                > MAX-INDX OR                      
046900                   MOD-IDARTNR (INDX) = SPACE                             
047000       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR   (INDX)                     
047100                                 MOD-BEART-ENG (INDX)                     
047200       ADD +1 TO INDX                                                     
047300     END-PERFORM                                                          
047400     .                                                                    
047500     EJECT                                                                
047600***               ---- IMS SEKTIONER ----                                 
047700     SKIP3                                                                
047800 IMS-GET-MSG SECTION.                                                     
047900                                                                          
048000     MOVE '  QC' TO GODK-STATUSKODER                                      
048100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
048200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
048300     PERFORM IMS-STATUS-KONTROLL                                          
048400     .                                                                    
048500     SKIP3                                                                
048600 IMS-INSERT-MSG SECTION.                                                  
048700                                                                          
048800     IF SWEDISH-TEXT                                                      
048900        IF MSGI-IDLAND-SPR NOT = 'GB'                                     
049000           MOVE '0' TO MFS-KDHUVOMR                                       
049100        END-IF                                                            
049200     END-IF                                                               
049300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
049400     MOVE SPACE TO GODK-STATUSKODER                                       
049500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
049600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
049700     PERFORM IMS-STATUS-KONTROLL                                          
049800     .                                                                    
049900     EJECT                                                                
050000 IMS-GU-ARTR01 SECTION.                                                   
050100                                                                          
050200     STRING 'WLARTR01(WDK7A1KY=>' WDK7A1KY-MIN-X                          
050300                    '&WDK7A1KY=<' WDK7A1KY-MAX-X ')'                      
050400          DELIMITED BY SIZE INTO SSA1                                     
050500     MOVE '  GE' TO GODK-STATUSKODER                                      
050600     CALL CBLTDLI USING GU ARTR-PCB DLI-IO-AREA SSA1                      
050700     MOVE ARTR-STATUS-CODE TO STATUS-WS                                   
050800     PERFORM IMS-STATUS-KONTROLL                                          
050900     .                                                                    
051000     SKIP3                                                                
051100 IMS-GN-ARTR01 SECTION.                                                   
051200                                                                          
051300     STRING 'WLARTR01(WDK7A1KY=>' WDK7A1KY-MIN-X                          
051400                    '&WDK7A1KY=<' WDK7A1KY-MAX-X ')'                      
051500          DELIMITED BY SIZE INTO SSA1                                     
051600     MOVE '  GE' TO GODK-STATUSKODER                                      
051700     CALL CBLTDLI USING GN ARTR-PCB DLI-IO-AREA SSA1                      
051800     MOVE ARTR-STATUS-CODE TO STATUS-WS                                   
051900     PERFORM IMS-STATUS-KONTROLL                                          
052000     .                                                                    
052100     SKIP3                                                                
052200 IMS-GET-BENA-TEXT SECTION.                                               
052300                                                                          
052400     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
052500            DELIMITED BY SIZE INTO SSA1                                   
052600     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
052700            DELIMITED BY SIZE INTO SSA2                                   
052800     MOVE '  GE' TO GODK-STATUSKODER                                      
052900     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
053000     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
053100     PERFORM IMS-STATUS-KONTROLL                                          
053200     .                                                                    
053300     SKIP3                                                                
053400 IMS-GU-WDB601    SECTION.                                                
053500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
053600          DELIMITED BY SIZE INTO SSA1                                     
053700     MOVE '  GE' TO GODK-STATUSKODER                                      
053800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
053900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
054000     PERFORM IMS-STATUS-KONTROLL                                          
054100     IF SEGMENT-SAKNAS                                                    
054200         MOVE SPACE TO DCS-KDDC                                           
054300     END-IF                                                               
054400     .                                                                    
054410 IMS-GU-WDK6E1 SECTION.                                                   
054420                                                                          
054430     STRING 'WDK6E1  (WDK6E1KY=>' WDK6E1KY-MIN-X                          
054440                    '&WDK6E1KY=<' WDK6E1KY-MAX-X ')'                      
054450          DELIMITED BY SIZE INTO SSA1                                     
054460     MOVE '  GE'              TO GODK-STATUSKODER                         
054470     CALL CBLTDLI USING GU WDK6E-PCB DLI-IO-AREA-WDK6E1 SSA1              
054480     MOVE WDK6E-STATUS-CODE TO STATUS-WS                                  
054490     PERFORM IMS-STATUS-KONTROLL                                          
054491     .                                                                    
054492 IMS-GN-WDK6E1 SECTION.                                                   
054493                                                                          
054494     STRING 'WDK6E1  (WDK6E1KY=>' WDK6E1KY-MIN-X                          
054495                    '&WDK6E1KY=<' WDK6E1KY-MAX-X ')'                      
054496          DELIMITED BY SIZE INTO SSA1                                     
054497     MOVE '  GE'              TO GODK-STATUSKODER                         
054498     CALL CBLTDLI USING GN WDK6E-PCB DLI-IO-AREA-WDK6E1 SSA1              
054499     MOVE WDK6E-STATUS-CODE TO STATUS-WS                                  
054500     PERFORM IMS-STATUS-KONTROLL                                          
054501     .                                                                    
054510 IMS-STATUS-KONTROLL SECTION.                                             
054600                                                                          
054700     SET STATUS-IX TO 1                                                   
054800     SEARCH GODK-STATUS                                                   
054900       AT END                                                             
055000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
055100         DELIMITED BY SIZE INTO FELTEXT                                   
055200         CALL FELLOG                                                      
055300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
055400         CONTINUE                                                         
055500     END-SEARCH                                                           
055600     .                                                                    
