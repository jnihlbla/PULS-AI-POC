000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6031600.                                                
000300 AUTHOR.         MARTIEN HOMPES.                                          
000400 DATE-WRITTEN.   97/04/03.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        ADD LOCATIONS                                                    
000900*                                                                         
001000*        THE PROGRAM READS     WLLOCA (WDJ8)                              
001100*        THE PROGRAM READS     WLARTS (WDK7A)                             
001200*        THE PROGRAM READS     WLARTD (WDD8A)                             
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSACTION: W6T316                                              
001600*        MID:         W6I31601                                            
001700*                                                                         
001800*    OUTDATA.                                                             
001900*        MOD:         W6O31601                                            
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700*    -- CHECKED BY WY2000                                                 
002800 77  IDPGM                       PIC X(08)   VALUE 'W6031600'.            
002900                                                                          
003000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003200                                                                          
003300 77  YES                         PIC X       VALUE 'Y'.                   
003400 77  NOO                         PIC X       VALUE 'N'.                   
003500                                                                          
003600 77  IO-COUNT                   PIC S9(4)  VALUE +0    COMP SYNC.         
003700 77  MAX-IO-COUNT               PIC S9(4)  VALUE +3    COMP SYNC.         
003800 77  LNG-P-TO-P-PREFIX          PIC S9(4)  VALUE +17   COMP SYNC.         
003900*    --- INDEX FOR SCROLL LINES                                           
004000 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004100 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
004200*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004300                                                                          
004400 77  WS-ADLAGOMR                 PIC 9(2)   VALUE ZERO.                   
004500 77  WS-ADGANG-FOM               PIC 9(2)   VALUE ZERO.                   
004600 77  WS-ADGANG-TOM               PIC 9(2)   VALUE ZERO.                   
004700 77  WS-ADGANG                   PIC 9(2)   VALUE ZERO.                   
004800 77  WS-ADSEC-FOM                PIC 9(2)   VALUE ZERO.                   
004900 77  WS-ADSEC-TOM                PIC 9(2)   VALUE ZERO.                   
005000 77  WS-ADSEC                    PIC 9(2)   VALUE ZERO.                   
005100 77  WS-ADLEVEL-FOM              PIC 9(2)   VALUE ZERO.                   
005200 77  WS-ADLEVEL-TOM              PIC 9(2)   VALUE ZERO.                   
005300 77  WS-ADLEVEL                  PIC 9(2)   VALUE ZERO.                   
005400 77  WS-ADSEQ                    PIC 9(2)   VALUE ZERO.                   
005500 77  WS-KDLOC                    PIC X(1)   VALUE SPACE.                  
005600 77  WS-KDFREQ                   PIC 9(2)   VALUE ZERO.                   
005700 77  WS-TELOC                    PIC X(1)   VALUE SPACE.                  
005800                                                                          
005900 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
006000     88  INDATA-OK                           VALUE 'Y'.                   
006100     88  INDATA-WRONG                        VALUE 'N'.                   
006200                                                                          
006300 77  KEYS-SW                     PIC X       VALUE 'Y'.                   
006400     88  KEYS-OK                             VALUE 'Y'.                   
006500     88  KEYS-WRONG                          VALUE 'N'.                   
006600                                                                          
006700 77  PRINT-SW                    PIC X       VALUE 'Y'.                   
006800     88  PRINT-OK                            VALUE 'Y'.                   
006900     88  PRINT-WRONG                         VALUE 'N'.                   
007000                                                                          
007100 77  OCCU-SW                     PIC X       VALUE 'Y'.                   
007200     88  OCCUPIED                            VALUE 'Y'.                   
007300     88  NOT-OCCUPIED                        VALUE 'N'.                   
007400                                                                          
007500 77  RESTART-SW                  PIC X       VALUE 'N'.                   
007600     88  RESTART                             VALUE 'Y'.                   
007700                                                                          
007800 77  PRIME                       PIC X        VALUE 'P'.                  
007900 77  BUFFER                      PIC X        VALUE 'R'.                  
008000 77  MIXED                       PIC X        VALUE 'M'.                  
008100                                                                          
008200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008300     88  OWN-MID                             VALUE '6316'.                
008400     88  GOOD-MID                            VALUE '6316'.                
008500     88  HELP-MID                            VALUE '0551'.                
008600       EJECT                                                              
008700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
008800 01  GENERAL-SUBPROGRAMS.                                                 
008900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009300     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
009400     EJECT                                                                
009500*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
009600*01 -COPY WMEDAREA                                                        
009700     SKIP3                                                                
009800*    --- PARAMETRAR TILL SUBPROGRAM W006PRT                               
009900*   -COPY W006PRT                                                         
010000     EJECT                                                                
010100 01  MESSAGE-CODES.                                                       
010200     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010300     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
010400     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010500     03  ITEMS-MISSING           PIC X(3)    VALUE '029'.                 
010600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
010700     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
010800     03  INF-LAST-PAGE-SHOWN     PIC X(3)    VALUE '115'.                 
010900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011000     03  WRONG-INTERVAL-INFO     PIC X(3)    VALUE '738'.                 
011100     EJECT                                                                
011200*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
011300*                                                                         
011400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
011500     SKIP3                                                                
011600*01 -COPY WMSGINIT                                                        
011700     EJECT                                                                
011800*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
011900*                                                                         
012000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012100     SKIP3                                                                
012200*01  MID -COPY W6I31601                                                   
012300     EJECT                                                                
012400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012500     SKIP3                                                                
012600*01  -COPY WMSGAREA                                                       
012700     EJECT                                                                
012800     03  MOD REDEFINES MSG-AREA.                                          
012900*      05  -COPY W6O31601                                                 
013000     EJECT                                                                
013100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013200     SKIP3                                                                
013300*01  -COPY WMFSAREA                                                       
013400     EJECT                                                                
013500 01  W-PROG-TO-PROG-SW.                                                   
013600*03 -COPY WMSGSOP                                                         
013700  SKIP3                                                                   
013800 01 PARM-TESYMBV.                                                         
013900     03 FILLER                   PIC X(2)    VALUE 'A('.                  
014000     03 PARM-IDDC                PIC X(2)    VALUE SPACE.                 
014100     03 FILLER                   PIC X(1)    VALUE ')'.                   
014200     03 FILLER                   PIC X(2)    VALUE 'B('.                  
014300     03 PARM-ADLAGOMR            PIC 9(2)    VALUE ZERO.                  
014400     03 FILLER                   PIC X(1)    VALUE ')'.                   
014500     03 FILLER                   PIC X(2)    VALUE 'C('.                  
014600     03 PARM-ADGANG-FOM          PIC 9(2)    VALUE ZERO.                  
014700     03 FILLER                   PIC X(1)    VALUE ')'.                   
014800     03 FILLER                   PIC X(2)    VALUE 'D('.                  
014900     03 PARM-ADGANG-TOM          PIC 9(2)    VALUE ZERO.                  
015000     03 FILLER                   PIC X(1)    VALUE ')'.                   
015100     03 FILLER                   PIC X(2)    VALUE 'E('.                  
015200     03 PARM-ADSEC-FOM           PIC 9(2)    VALUE ZERO.                  
015300     03 FILLER                   PIC X(1)    VALUE ')'.                   
015400     03 FILLER                   PIC X(2)    VALUE 'F('.                  
015500     03 PARM-ADSEC-TOM           PIC 9(2)    VALUE ZERO.                  
015600     03 FILLER                   PIC X(1)    VALUE ')'.                   
015700     03 FILLER                   PIC X(2)    VALUE 'G('.                  
015800     03 PARM-ADLEVEL-FOM         PIC 9(2)    VALUE ZERO.                  
015900     03 FILLER                   PIC X(1)    VALUE ')'.                   
016000     03 FILLER                   PIC X(2)    VALUE 'H('.                  
016100     03 PARM-ADLEVEL-TOM         PIC 9(2)    VALUE ZERO.                  
016200     03 FILLER                   PIC X(1)    VALUE ')'.                   
016300     03 FILLER                   PIC X(2)    VALUE 'I('.                  
016400     03 PARM-KDLOC               PIC X       VALUE SPACE.                 
016500     03 FILLER                   PIC X(1)    VALUE ')'.                   
016600     03 FILLER                   PIC X(2)    VALUE 'J('.                  
016700     03 PARM-KDFREQ              PIC 9(2)    VALUE ZERO.                  
016800     03 FILLER                   PIC X(1)    VALUE ')'.                   
016900     03 FILLER                   PIC X(2)    VALUE 'K('.                  
017000     03 PARM-TELOC               PIC X       VALUE SPACE.                 
017100     03 FILLER                   PIC X(1)    VALUE ')'.                   
017200     03 FILLER                   PIC X(2)    VALUE 'L('.                  
017300     03 PARM-KDPRT               PIC X(3)    VALUE SPACE.                 
017400     03 FILLER                   PIC X(1)    VALUE ')'.                   
017500*    --- WORK-AREAS FOR IMS-SECTIONS                                      
017600*                                                                         
017700     EJECT                                                                
017800 01  WORK-AREA.                                                           
017900     03 WORK-ADLAGOMR            PIC 9(2).                                
018000     03 WORK-ADGANG              PIC 9(2).                                
018100     03 WORK-ADPLATS.                                                     
018200        05 WORK-ADSEC            PIC 9(2).                                
018300        05 WORK-ADLEVEL          PIC 9(2).                                
018400        05 WORK-ADSEQ            PIC 9(1).                                
018500     03 WORK-TELOC               PIC X(1).                                
018600                                                                          
018700 01  SAVE-AREA.                                                           
018800     03 SAVE-IDTRANS             PIC X(4)    VALUE  SPACE.                
018900     03 SAVE-ADLAGOMR-ENTER      PIC 9(2).                                
019000     03 SAVE-ADGANG-ENTER        PIC 9(2).                                
019100     03 SAVE-ADPLATS-ENTER.                                               
019200        05 SAVE-ADSEC-ENTER      PIC 9(2).                                
019300        05 SAVE-ADLEVEL-ENTER    PIC 9(2).                                
019400        05 SAVE-ADSEQ-ENTER      PIC 9(1).                                
019500     03 SAVE-ADLAGOMR-NEXT       PIC 9(2).                                
019600     03 SAVE-ADGANG-NEXT         PIC 9(2).                                
019700     03 SAVE-ADPLATS-NEXT.                                                
019800        05 SAVE-ADSEC-NEXT       PIC 9(2).                                
019900        05 SAVE-ADLEVEL-NEXT     PIC 9(2).                                
020000        05 SAVE-ADSEQ-NEXT       PIC 9(1).                                
020100                                                                          
020200                                                                          
020300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
020400     SKIP3                                                                
020500 01  KEYS-TO-DLI.                                                         
020600                                                                          
020700     03  W-WDJ8KY-MIN-X.                                                  
020800         05  W-LOC-IDDC-MIN       PIC X(2).                               
020900         05  W-LOC-ADLAGOMR-MIN   PIC 9(2).                               
021000         05  W-LOC-ADGANG-MIN     PIC 9(2).                               
021100         05  W-LOC-ADPLATS-MIN.                                           
021200             07 W-LOC-ADSEC-MIN   PIC 9(2).                               
021300             07 W-LOC-ADLEVEL-MIN PIC 9(2).                               
021400             07 W-LOC-ADSEQ-MIN   PIC 9(1).                               
021500                                                                          
021600   03  W-WDJ8KY-MAX-X.                                                    
021700     05  W-LOC-IDDC-MAX      PIC X(2).                                    
021800     05  W-LOC-ADLAGOMR-MAX  PIC 9(2).                                    
021900     05  W-LOC-ADGANG-MAX    PIC 9(2).                                    
022000     05  W-LOC-ADPLATS-MAX.                                               
022100       07  W-LOC-ADSEC-MAX   PIC 9(2).                                    
022200       07  W-LOC-ADLEVEL-MAX PIC 9(2).                                    
022300       07  W-LOC-ADSEQ-MAX   PIC 9(1).                                    
022400                                                                          
022500   03  WDK7A1KY-MIN-X.                                                    
022600     05  W-IDDC-MIN          PIC X(2)           VALUE SPACE.              
022700     05  W-ADART-MIN.                                                     
022800       07  W-ADLAGOMR-MIN    PIC S9(3)  COMP-3  VALUE ZERO.               
022900       07  W-ADGANG-MIN      PIC S9(3)  COMP-3  VALUE ZERO.               
023000       07  W-ADPLATS-MIN     PIC S9(5)  COMP-3  VALUE ZERO.               
023100     05 W-IDARTNR-MIN        PIC S9(9)  COMP-3  VALUE ZERO.               
023200                                                                          
023300   03  WDK7A1KY-MAX-X.                                                    
023400     05  W-IDDC-MAX          PIC X(2)           VALUE SPACE.              
023500     05  W-ADART-MAX.                                                     
023600       07  W-ADLAGOMR-MAX    PIC S9(3)  COMP-3  VALUE ZERO.               
023700       07  W-ADGANG-MAX      PIC S9(3)  COMP-3  VALUE ZERO.               
023800       07  W-ADPLATS-MAX     PIC S9(5)  COMP-3  VALUE ZERO.               
023900     05 W-IDARTNR-MAX     PIC S9(9)  COMP-3  VALUE 99999999.              
024000                                                                          
024100   03  W-WDD8ASEQ-X.                                                      
024200     05  W-IDDC-ASEQ         PIC X(2)           VALUE SPACE.              
024300     05  W-ADBUFFOMR-ASEQ    PIC S9(3)  COMP-3  VALUE ZERO.               
024400     05  W-ADBUFFGANG-ASEQ   PIC S9(3)  COMP-3  VALUE ZERO.               
024500     05  W-ADBUFFPL-ASEQ     PIC S9(5)  COMP-3  VALUE ZERO.               
024600                                                                          
024700     03  W-IDDC-B6-X.                                                     
024800         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
024900                                                                          
025000     SKIP2                                                                
025100*    --- STATUS-KOD FRÅN IMS                                              
025200 01  STATUS-WS                   PIC XX.                                  
025300     88  SEGMENT-FOUND                       VALUE '  '.                  
025400     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
025500     88  SEGMENT-MISSING                     VALUE 'GE'.                  
025600     88  END-OF-DATABASE                     VALUE 'GB'.                  
025700     SKIP2                                                                
025800 01  GOOD-STATUSCODES.                                                    
025900     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
026000     SKIP3                                                                
026100 01  SSA1                        PIC X(64).                               
026200 01  SSA2                        PIC X(64).                               
026300     EJECT                                                                
026400*    --- IMS FUNCTION CODES                                               
026500*01  -COPY W0003                                                          
026600     EJECT                                                                
026700*    ---  DLI INPUT-OUTPUT AREA                                           
026800                                                                          
026900 01  FILLER         PIC X(20) VALUE 'WLLOCA01-AREA'.                      
027000 01  DLI-IO-WLLOCA01.                                                     
027100*    03  -COPY WDJ801                                                     
027200                                                                          
027300 01  FILLER         PIC X(20) VALUE 'WLARTA01-AREA'.                      
027400 01  DLI-IO-WLARTA01.                                                     
027500*    03  -COPY WDK7A1                                                     
027600                                                                          
027700 01  FILLER         PIC X(20) VALUE 'WLARTD11-AREA'.                      
027800 01  DLI-IO-WLARTD11.                                                     
027900*    03  -COPY WDD811                                                     
028000     EJECT                                                                
028100 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
028200 01   DLI-IO-AREA-B601.                                                   
028300*     03  -COPY WDB601                                                    
028400     EJECT                                                                
028500                                                                          
028600     EJECT                                                                
028700 LINKAGE SECTION.                                                         
028800*01  -COPY W0009  -PRE MSG-                                               
028900*01  -COPY W0009  -PRE ALT-                                               
029000     EJECT                                                                
029100*01  -COPY W0008  -PRE USEA-                                              
029200     05  FILLER                  PIC X.                                   
029300     EJECT                                                                
029400*01  -COPY W0008  -PRE LOCA-                                              
029500     05  FILLER                  PIC X.                                   
029600     EJECT                                                                
029700*01  -COPY W0008  -PRE ARTR-                                              
029800     05  FILLER                  PIC X.                                   
029900     EJECT                                                                
030000*01  -COPY W0008  -PRE ARTD-                                              
030100     05  FILLER                  PIC X.                                   
030200     EJECT                                                                
030300*01  -COPY W0008      -PRE WDB6-                                          
030400     05  FILLER                  PIC X.                                   
030500     EJECT                                                                
030600 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB                                
030700           USEA-PCB                                                       
030800           LOCA-PCB ARTR-PCB ARTD-PCB                                     
030900           WDB6-PCB.                                                      
031000 MAIN SECTION.                                                            
031100     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB                                
031200            USEA-PCB                                                      
031300            LOCA-PCB ARTR-PCB ARTD-PCB                                    
031400            WDB6-PCB.                                                     
031500                                                                          
031600     PERFORM IMS-GET-MSG                                                  
031700     IF SEGMENT-FOUND                                                     
031800       PERFORM A-INIT                                                     
031900       IF GOOD-MID OR HELP-MID                                            
032000          PERFORM B-CHECK-KEYS                                            
032100          IF KEYS-OK                                                      
032200             IF MFS-PRINT                                                 
032300                PERFORM G-CHECK-PRINT                                     
032400                IF PRINT-OK                                               
032500                   PERFORM GA-STARTA-JOB                                  
032600                END-IF                                                    
032700             END-IF                                                       
032800             IF MFS-FIRST                                                 
032900                PERFORM C-FIRST-PAGE                                      
033000              ELSE                                                        
033100                IF MFS-NEXT                                               
033200                   PERFORM D-NEXT-PAGE                                    
033300                 ELSE                                                     
033400                   PERFORM E-SAME-PAGE                                    
033500                END-IF                                                    
033600             END-IF                                                       
033700             PERFORM F-READ-SHOW-INFO                                     
033800          END-IF                                                          
033900       END-IF                                                             
034000       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O31601 + 4                      
034100       PERFORM IMS-INSERT-MSG                                             
034200     END-IF                                                               
034300                                                                          
034400     MOVE ZERO TO RETURN-CODE                                             
034500     GOBACK                                                               
034600     .                                                                    
034700     EJECT                                                                
034800 A-INIT SECTION.                                                          
034900                                                                          
035000     IF MSG-DOUBLE-TRANSACTIONS                                           
035100       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W6I31601                 
035200       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
035300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
035400     ELSE                                                                 
035500       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W6I31601                  
035600       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
035700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
035800     END-IF                                                               
035900                                                                          
036000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
036100     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
036200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
036300                                                                          
036400     MOVE LOW-VALUE TO MSG-AREA                                           
036500     MOVE 'W6O316N1' TO MFS-IDMOD                                         
036600     MOVE '6316' TO MOD-IDTRANS                                           
036700     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
036800                                                                          
036900                                                                          
037000     MOVE ALL '+'           TO MSGI-WMSGINIT                              
037100     MOVE '001'             TO MSGI-KDCALL                                
037200     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
037300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
037400     MOVE '6316'            TO MSGI-IDTRANS                               
037500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
037600                                                                          
037700     MOVE MSGI-SPAR-AREA    TO SAVE-AREA                                  
037800     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
037900                                                                          
038000     IF GOOD-MID OR HELP-MID                                              
038100       CONTINUE                                                           
038200     ELSE                                                                 
038300       MOVE SPACE TO MFS-KDTRTYP                                          
038400       MOVE '7' TO MFS-IDPFK                                              
038500       PERFORM MFS-INIT-KEY-FIELD-IN                                      
038600       PERFORM MFS-INIT-KEY-FIELD-OUT                                     
038700       PERFORM MFS-ERASE-LINE-FIELD-OUT                                   
038800     END-IF                                                               
038900     .                                                                    
039000     EJECT                                                                
039100 B-CHECK-KEYS SECTION.                                                    
039200                                                                          
039300                                                                          
039400     MOVE YES               TO KEYS-SW                                    
039500                                                                          
039600     PERFORM MFS-INIT-KEY-FIELD-IN                                        
039700                                                                          
039800*                                                                         
039900*    -- CONTROL  ON WAREHOUSE                                             
040000*                                                                         
040100     MOVE MSGI-IDDC         TO W-IDDC-B6                                  
040200     PERFORM IMS-GU-WDB601                                                
040300     IF SEGMENT-FOUND                                                     
040400     AND (DCS-SDC                                                         
040500     OR   DCS-NDC-NA                                                      
040600     OR   DCS-NDC-PF                                                      
040700     OR   DCS-NDC-OTHERS)                                                 
040800       CONTINUE                                                           
040900     ELSE                                                                 
041000       MOVE NOO                 TO KEYS-SW                                
041100     END-IF                                                               
041200*                                                                         
041300*    -- CONTROL  AREA                                                     
041400*                                                                         
041500     IF MID-ADLAGOMR-IN = ALL '+'                                         
041600       INSPECT MID-ADLAGOMR-UT REPLACING LEADING SPACE BY ZERO            
041700       MOVE MID-ADLAGOMR-UT    TO WS-ADLAGOMR                             
041800     ELSE                                                                 
041900       IF MID-ADLAGOMR-IN NUMERIC                                         
042000          MOVE '7' TO MFS-IDPFK                                           
042100          MOVE    SPACE        TO MFS-KDTRTYP                             
042200          MOVE MID-ADLAGOMR-IN TO WS-ADLAGOMR                             
042300       ELSE                                                               
042400          MOVE NOO             TO KEYS-SW                                 
042500       END-IF                                                             
042600     END-IF                                                               
042700*                                                                         
042800*    -- CONTROL  AISLE FROM                                               
042900*                                                                         
043000     IF MID-ADGANG-FOM-IN = ALL '+'                                       
043100       INSPECT MID-ADGANG-FOM-UT REPLACING LEADING SPACE BY ZERO          
043200       MOVE MID-ADGANG-FOM-UT    TO WS-ADGANG-FOM                         
043300     ELSE                                                                 
043400       IF MID-ADGANG-FOM-IN NUMERIC                                       
043500          MOVE '7' TO MFS-IDPFK                                           
043600          MOVE    SPACE          TO MFS-KDTRTYP                           
043700          MOVE MID-ADGANG-FOM-IN TO WS-ADGANG-FOM                         
043800       ELSE                                                               
043900          MOVE NOO               TO KEYS-SW                               
044000       END-IF                                                             
044100     END-IF                                                               
044200*                                                                         
044300*    -- CONTROL  AISLE THRU                                               
044400*                                                                         
044500     IF MID-ADGANG-TOM-IN = ALL '+'                                       
044600       INSPECT MID-ADGANG-TOM-UT REPLACING LEADING SPACE BY ZERO          
044700       MOVE MID-ADGANG-TOM-UT    TO WS-ADGANG-TOM                         
044800     ELSE                                                                 
044900       IF MID-ADGANG-TOM-IN NUMERIC                                       
045000          MOVE '7' TO MFS-IDPFK                                           
045100          MOVE    SPACE          TO MFS-KDTRTYP                           
045200          MOVE MID-ADGANG-TOM-IN TO WS-ADGANG-TOM                         
045300       ELSE                                                               
045400          MOVE NOO               TO KEYS-SW                               
045500       END-IF                                                             
045600     END-IF                                                               
045700*                                                                         
045800*    -- CONTROL  SECTION FROM                                             
045900*                                                                         
046000     IF MID-ADSEC-FOM-IN = ALL '+'                                        
046100       INSPECT MID-ADSEC-FOM-UT REPLACING LEADING SPACE BY ZERO           
046200       MOVE MID-ADSEC-FOM-UT    TO WS-ADSEC-FOM                           
046300     ELSE                                                                 
046400       IF MID-ADSEC-FOM-IN NUMERIC                                        
046500          MOVE '7' TO MFS-IDPFK                                           
046600          MOVE    SPACE          TO MFS-KDTRTYP                           
046700          MOVE MID-ADSEC-FOM-IN TO WS-ADSEC-FOM                           
046800       ELSE                                                               
046900          MOVE NOO               TO KEYS-SW                               
047000       END-IF                                                             
047100     END-IF                                                               
047200*                                                                         
047300*    -- CONTROL  SECTION THRU                                             
047400*                                                                         
047500     IF MID-ADSEC-TOM-IN = ALL '+'                                        
047600       INSPECT MID-ADSEC-TOM-UT REPLACING LEADING SPACE BY ZERO           
047700       MOVE MID-ADSEC-TOM-UT    TO WS-ADSEC-TOM                           
047800     ELSE                                                                 
047900       IF MID-ADSEC-TOM-IN NUMERIC                                        
048000          MOVE '7' TO MFS-IDPFK                                           
048100          MOVE    SPACE          TO MFS-KDTRTYP                           
048200          MOVE MID-ADSEC-TOM-IN TO WS-ADSEC-TOM                           
048300       ELSE                                                               
048400          MOVE NOO               TO KEYS-SW                               
048500       END-IF                                                             
048600     END-IF                                                               
048700*                                                                         
048800*    -- CONTROL  LEVEL FROM                                               
048900*                                                                         
049000     IF MID-ADLEVEL-FOM-IN = ALL '+'                                      
049100       INSPECT MID-ADLEVEL-FOM-UT REPLACING LEADING SPACE BY ZERO         
049200       MOVE MID-ADLEVEL-FOM-UT    TO WS-ADLEVEL-FOM                       
049300     ELSE                                                                 
049400       IF MID-ADLEVEL-FOM-IN NUMERIC                                      
049500          MOVE '7' TO MFS-IDPFK                                           
049600          MOVE    SPACE           TO MFS-KDTRTYP                          
049700          MOVE MID-ADLEVEL-FOM-IN TO WS-ADLEVEL-FOM                       
049800       ELSE                                                               
049900          MOVE NOO                TO KEYS-SW                              
050000       END-IF                                                             
050100     END-IF                                                               
050200                                                                          
050300*                                                                         
050400*    -- CONTROL  LEVEL THRU                                               
050500*                                                                         
050600     IF MID-ADLEVEL-TOM-IN = ALL '+'                                      
050700       INSPECT MID-ADLEVEL-TOM-UT REPLACING LEADING SPACE BY ZERO         
050800       MOVE MID-ADLEVEL-TOM-UT    TO WS-ADLEVEL-TOM                       
050900     ELSE                                                                 
051000       IF MID-ADLEVEL-TOM-IN NUMERIC                                      
051100          MOVE '7' TO MFS-IDPFK                                           
051200          MOVE    SPACE           TO MFS-KDTRTYP                          
051300          MOVE MID-ADLEVEL-TOM-IN TO WS-ADLEVEL-TOM                       
051400       ELSE                                                               
051500          MOVE NOO                TO KEYS-SW                              
051600       END-IF                                                             
051700     END-IF                                                               
051800*                                                                         
051900*    -- CONTROL  TYPE OF LOCATION                                         
052000*                                                                         
052100     IF MID-KDLOC-IN  = ALL '+'                                           
052200        MOVE MID-KDLOC-UT          TO WS-KDLOC                            
052300      ELSE                                                                
052400        IF MID-KDLOC-IN  =  PRIME OR BUFFER OR MIXED                      
052500           MOVE '7'                TO MFS-IDPFK                           
052600           MOVE SPACE              TO MFS-KDTRTYP                         
052700           MOVE MID-KDLOC-IN       TO WS-KDLOC                            
052800         ELSE                                                             
052900           MOVE NOO                 TO KEYS-SW                            
053000        END-IF                                                            
053100     END-IF                                                               
053200*                                                                         
053300*    -- CONTROL  FREQUENCY TYPE                                           
053400*                                                                         
053500     IF MID-KDFREQ-IN = ALL '+'                                           
053600        MOVE MID-KDFREQ-UT          TO WS-KDFREQ                          
053700     ELSE                                                                 
053800       IF MID-KDFREQ-IN NUMERIC                                           
053900          MOVE '7'                  TO MFS-IDPFK                          
054000          MOVE SPACE                TO MFS-KDTRTYP                        
054100          MOVE MID-KDFREQ-IN        TO WS-KDFREQ                          
054200       ELSE                                                               
054300          MOVE NOO                  TO KEYS-SW                            
054400       END-IF                                                             
054500     END-IF                                                               
054600*                                                                         
054700*    -- FILL IN REMARK FIELD INPUT                                        
054800*                                                                         
054900     IF MID-TELOC-IN  = ALL '+'                                           
055000        MOVE MID-TELOC-UT         TO WS-TELOC                             
055100      ELSE                                                                
055200        MOVE '7'                  TO MFS-IDPFK                            
055300        MOVE SPACE                TO MFS-KDTRTYP                          
055400        MOVE MID-TELOC-IN         TO WS-TELOC                             
055500     END-IF                                                               
055600*                                                                         
055700*    -- FILL MOD KEY-OUTPUT FIELDS                                        
055800*                                                                         
055900     MOVE W-IDDC-B6      TO  MOD-IDDC-UT                                  
056000     MOVE WS-ADLAGOMR    TO  MOD-ADLAGOMR-UT                              
056100     MOVE WS-ADGANG-FOM  TO  MOD-ADGANG-FOM-UT                            
056200     MOVE WS-ADGANG-TOM  TO  MOD-ADGANG-TOM-UT                            
056300     MOVE WS-ADSEC-FOM   TO  MOD-ADSEC-FOM-UT                             
056400     MOVE WS-ADSEC-TOM   TO  MOD-ADSEC-TOM-UT                             
056500     MOVE WS-ADLEVEL-FOM TO  MOD-ADLEVEL-FOM-UT                           
056600     MOVE WS-ADLEVEL-TOM TO  MOD-ADLEVEL-TOM-UT                           
056700     MOVE WS-KDLOC       TO  MOD-KDLOC-UT                                 
056800     MOVE WS-KDFREQ      TO  MOD-KDFREQ-UT                                
056900     MOVE WS-TELOC       TO  MOD-TELOC-UT                                 
057000                                                                          
057100                                                                          
057200     IF KEYS-WRONG                                                        
057300       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
057400       CALL WMEDKONV USING MED-WMEDAREA                                   
057500       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
057600      ELSE                                                                
057700       IF WS-ADGANG-FOM   >  WS-ADGANG-TOM  OR                            
057800          WS-ADSEC-FOM    >  WS-ADSEC-TOM   OR                            
057900          WS-ADLEVEL-FOM  >  WS-ADLEVEL-TOM                               
058000                                                                          
058100          MOVE WRONG-INTERVAL-INFO TO MED-IDMFSFEL                        
058200          CALL WMEDKONV USING MED-WMEDAREA                                
058300          MOVE MED-MFSFEL          TO MOD-TEMFSFEL                        
058400          MOVE NOO                 TO KEYS-SW                             
058500       END-IF                                                             
058600     END-IF                                                               
058700                                                                          
058800     .                                                                    
058900     EJECT                                                                
059000 C-FIRST-PAGE SECTION.                                                    
059100                                                                          
059200                                                                          
059300*                                                                         
059400*    -- FILL START KEY FIELDS                                             
059500*                                                                         
059600     MOVE W-IDDC-B6             TO W-LOC-IDDC-MIN                         
059700     MOVE WS-ADLAGOMR           TO W-LOC-ADLAGOMR-MIN                     
059800     MOVE WS-ADGANG-FOM         TO W-LOC-ADGANG-MIN                       
059900     MOVE WS-ADSEC-FOM          TO W-LOC-ADSEC-MIN                        
060000     MOVE WS-ADLEVEL-FOM        TO W-LOC-ADLEVEL-MIN                      
060100     MOVE ZEROES                TO W-LOC-ADSEQ-MIN                        
060200                                                                          
060300     .                                                                    
060400     EJECT                                                                
060500 E-SAME-PAGE SECTION.                                                     
060600                                                                          
060700*                                                                         
060800*    -- FILL START KEY FIELDS                                             
060900*                                                                         
061000     MOVE W-IDDC-B6             TO W-LOC-IDDC-MIN                         
061100     MOVE SAVE-ADLAGOMR-ENTER   TO W-LOC-ADLAGOMR-MIN                     
061200     MOVE SAVE-ADGANG-ENTER     TO W-LOC-ADGANG-MIN                       
061300     MOVE SAVE-ADSEC-ENTER      TO W-LOC-ADSEC-MIN                        
061400     MOVE SAVE-ADLEVEL-ENTER    TO W-LOC-ADLEVEL-MIN                      
061500     MOVE SAVE-ADSEQ-ENTER      TO W-LOC-ADSEQ-MIN                        
061600     .                                                                    
061700     EJECT                                                                
061800                                                                          
061900 D-NEXT-PAGE SECTION.                                                     
062000                                                                          
062100*                                                                         
062200*    -- FILL START KEY FIELDS                                             
062300*                                                                         
062400     MOVE W-IDDC-B6             TO W-LOC-IDDC-MIN                         
062500     MOVE SAVE-ADLAGOMR-NEXT    TO W-LOC-ADLAGOMR-MIN                     
062600     MOVE SAVE-ADGANG-NEXT      TO W-LOC-ADGANG-MIN                       
062700     MOVE SAVE-ADSEC-NEXT       TO W-LOC-ADSEC-MIN                        
062800     MOVE SAVE-ADLEVEL-NEXT     TO W-LOC-ADLEVEL-MIN                      
062900     MOVE SAVE-ADSEQ-NEXT       TO W-LOC-ADSEQ-MIN                        
063000     .                                                                    
063100     EJECT                                                                
063200                                                                          
063300 F-READ-SHOW-INFO SECTION.                                                
063400                                                                          
063500*                                                                         
063600*    -- FILL FINISH (MAX) KEY FIELDS                                      
063700*                                                                         
063800     MOVE W-IDDC-B6             TO W-LOC-IDDC-MAX                         
063900     MOVE WS-ADLAGOMR           TO W-LOC-ADLAGOMR-MAX                     
064000     MOVE WS-ADGANG-TOM         TO W-LOC-ADGANG-MAX                       
064100     MOVE WS-ADSEC-TOM          TO W-LOC-ADSEC-MAX                        
064200     MOVE WS-ADLEVEL-TOM        TO W-LOC-ADLEVEL-MAX                      
064300     MOVE 9                     TO W-LOC-ADSEQ-MAX                        
064400                                                                          
064500     MOVE 99999999              TO W-IDARTNR-MAX                          
064600                                                                          
064700     PERFORM  IMS-GU-LOCA                                                 
064800                                                                          
064900     IF SEGMENT-MISSING                                                   
065000        IF MFS-NEXT                                                       
065100           MOVE INF-LAST-PAGE-SHOWN TO    MED-IDMFSFEL                    
065200         ELSE                                                             
065300           MOVE ITEMS-MISSING       TO    MED-IDMFSFEL                    
065400        END-IF                                                            
065500        CALL    WMEDKONV    USING MED-WMEDAREA                            
065600        MOVE MED-MFSFEL     TO MOD-TEMFSFEL                               
065700                                                                          
065800        PERFORM MFS-ERASE-LINE-FIELD-OUT                                  
065900                                                                          
066000        MOVE WS-ADLAGOMR    TO SAVE-ADLAGOMR-ENTER                        
066100        MOVE WS-ADGANG-FOM  TO SAVE-ADGANG-ENTER                          
066200        MOVE WS-ADSEC-FOM   TO SAVE-ADSEC-ENTER                           
066300        MOVE WS-ADLEVEL-FOM TO SAVE-ADLEVEL-ENTER                         
066400        MOVE ZEROES         TO SAVE-ADSEQ-ENTER                           
066500                                                                          
066600        MOVE WS-ADLAGOMR    TO SAVE-ADLAGOMR-NEXT                         
066700        MOVE WS-ADGANG-FOM  TO SAVE-ADGANG-NEXT                           
066800        MOVE WS-ADSEC-FOM   TO SAVE-ADSEC-NEXT                            
066900        MOVE WS-ADLEVEL-FOM TO SAVE-ADLEVEL-NEXT                          
067000        MOVE ZEROES         TO SAVE-ADSEQ-NEXT                            
067100                                                                          
067200      ELSE                                                                
067300        MOVE LOC-ADLAGOMR   TO SAVE-ADLAGOMR-ENTER                        
067400        MOVE LOC-ADGANG     TO SAVE-ADGANG-ENTER                          
067500        MOVE LOC-ADPLATS    TO SAVE-ADPLATS-ENTER                         
067600        MOVE +1             TO INDX                                       
067700        PERFORM UNTIL SEGMENT-MISSING OR END-OF-DATABASE OR               
067800                      INDX > MAX-INDX                                     
067900         MOVE NOO                 TO OCCU-SW                              
068000         MOVE LOC-ADPLATS         TO WORK-ADPLATS                         
068100         MOVE LOC-TELOC           TO WORK-TELOC                           
068200         IF WS-ADLEVEL-FOM <= WORK-ADLEVEL              AND               
068300            WS-ADLEVEL-TOM >= WORK-ADLEVEL              AND               
068400            WS-ADSEC-FOM   <= WORK-ADSEC                AND               
068500            WS-ADSEC-TOM   >= WORK-ADSEC                AND               
068600            (WS-KDLOC  = " " OR  WS-KDLOC = LOC-KDLOC)  AND               
068700            (WS-KDFREQ = 0   OR WS-KDFREQ = LOC-KDFREQ) AND               
068800            (WS-TELOC  = " " OR WS-TELOC  = WORK-TELOC)                   
068900            IF LOC-KDLOC     =  PRIME OR MIXED                            
069000               MOVE LOC-IDDC            TO W-IDDC-MIN                     
069100                                           W-IDDC-MAX                     
069200               MOVE LOC-ADLAGOMR        TO W-ADLAGOMR-MIN                 
069300                                           W-ADLAGOMR-MAX                 
069400               MOVE LOC-ADGANG          TO W-ADGANG-MIN                   
069500                                           W-ADGANG-MAX                   
069600               MOVE LOC-ADPLATS         TO W-ADPLATS-MIN                  
069700                                        W-ADPLATS-MAX                     
069800               PERFORM  IMS-GU-ARTR01                                     
069900                                                                          
070000               IF SEGMENT-FOUND                                           
070100                  MOVE  YES              TO OCCU-SW                       
070200               END-IF                                                     
070300            END-IF                                                        
070400            IF LOC-KDLOC  =  (BUFFER OR MIXED) AND NOT-OCCUPIED           
070500               MOVE LOC-IDDC            TO W-IDDC-ASEQ                    
070600               MOVE LOC-ADLAGOMR        TO W-ADBUFFOMR-ASEQ               
070700               MOVE LOC-ADGANG          TO W-ADBUFFGANG-ASEQ              
070800               MOVE LOC-ADPLATS         TO W-ADBUFFPL-ASEQ                
070900                                                                          
071000               PERFORM  IMS-GU-ARTD-ASEQ                                  
071100                                                                          
071200               IF SEGMENT-FOUND                                           
071300                  MOVE  YES              TO OCCU-SW                       
071400               END-IF                                                     
071500            END-IF                                                        
071600            IF NOT-OCCUPIED                                               
071700               MOVE LOC-ADLAGOMR    TO MOD-ADLAGOMR-LINE (INDX)           
071800               MOVE LOC-ADGANG      TO MOD-ADGANG-LINE   (INDX)           
071900               MOVE LOC-ADPLATS     TO MOD-ADPLATS-LINE  (INDX)           
072000               MOVE LOC-KDLOC       TO MOD-KDLOC-LINE    (INDX)           
072100               MOVE LOC-KDFREQ      TO MOD-KDFREQ-LINE   (INDX)           
072200               MOVE LOC-KDSTOR      TO MOD-KDSTOR-LINE   (INDX)           
072300               MOVE LOC-TELOC       TO MOD-TELOC-LINE    (INDX)           
072400               ADD 1 TO INDX                                              
072500            END-IF                                                        
072600         END-IF                                                           
072700         PERFORM IMS-GN-LOCA                                              
072800        END-PERFORM                                                       
072900        IF SEGMENT-FOUND                                                  
073000           MOVE LOC-ADLAGOMR   TO SAVE-ADLAGOMR-NEXT                      
073100           MOVE LOC-ADGANG     TO SAVE-ADGANG-NEXT                        
073200           MOVE LOC-ADPLATS    TO SAVE-ADPLATS-NEXT                       
073300           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
073400           CALL WMEDKONV USING MED-WMEDAREA                               
073500           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
073600         ELSE                                                             
073700           MOVE LOC-ADLAGOMR        TO    SAVE-ADLAGOMR-NEXT              
073800           MOVE LOC-ADGANG          TO    SAVE-ADGANG-NEXT                
073900           MOVE LOC-ADPLATS         TO    SAVE-ADPLATS-NEXT               
074000           MOVE INF-LAST-PAGE-SHOWN TO    MED-IDMFSFEL                    
074100           CALL WMEDKONV            USING MED-WMEDAREA                    
074200           MOVE MED-MFSFEL          TO    MOD-TEMFSFEL                    
074300           PERFORM UNTIL INDX > MAX-INDX                                  
074400             MOVE MFS-ERASE-FIELD   TO  MOD-ADLAGOMR-LINE (INDX)          
074500                                        MOD-ADGANG-LINE   (INDX)          
074600                                        MOD-ADPLATS-LINE  (INDX)          
074700                                        MOD-KDLOC-LINE    (INDX)          
074800                                        MOD-KDFREQ-LINE   (INDX)          
074900                                        MOD-KDSTOR-LINE   (INDX)          
075000                                        MOD-TELOC-LINE    (INDX)          
075100             ADD 1 TO INDX                                                
075200           END-PERFORM                                                    
075300         END-IF                                                           
075400                                                                          
075500       MOVE '002'      TO MSGI-KDCALL                                     
075600       MOVE '6316'     TO SAVE-IDTRANS                                    
075700       MOVE SAVE-AREA  TO MSGI-SPAR-AREA                                  
075800       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
075900     END-IF                                                               
076000     .                                                                    
076100     EJECT                                                                
076200 G-CHECK-PRINT SECTION.                                                   
076300     IF MID-KDPRT = ALL '+'                                               
076400        MOVE NOO TO PRINT-SW                                              
076500        MOVE 'PRINT CODE WRONG'  TO MOD-TEMFSFEL                          
076600     ELSE                                                                 
076700        MOVE '6L'      TO PRT-IDPRTLST(1:2)                               
076800        MOVE MID-KDPRT TO PRT-IDPRTLST(3:3)                               
076900        MOVE SPACE     TO PRT-IDPRTLST(6:3)                               
077000        MOVE 1                 TO PRT-KDCALL                              
077100        CALL W006PRT USING PRT-W006PRT                                    
077200        IF PRT-IDLTERM = 'SAKNAS  '                                       
077300          MOVE NOO TO PRINT-SW                                            
077400          MOVE 'PRINTER MISSING IN W006PRT' TO MOD-TEMFSFEL               
077500        ELSE                                                              
077600          MOVE W-IDDC-B6         TO PARM-IDDC                             
077700          MOVE WS-ADLAGOMR       TO PARM-ADLAGOMR                         
077800          MOVE WS-ADGANG-FOM     TO PARM-ADGANG-FOM                       
077900          MOVE WS-ADGANG-TOM     TO PARM-ADGANG-TOM                       
078000          MOVE WS-ADSEC-FOM      TO PARM-ADSEC-FOM                        
078100          MOVE WS-ADSEC-TOM      TO PARM-ADSEC-TOM                        
078200          MOVE WS-ADLEVEL-FOM    TO PARM-ADLEVEL-FOM                      
078300          MOVE WS-ADLEVEL-TOM    TO PARM-ADLEVEL-TOM                      
078400          MOVE WS-KDLOC          TO PARM-KDLOC                            
078500          MOVE WS-KDFREQ         TO PARM-KDFREQ                           
078600          MOVE WS-TELOC          TO PARM-TELOC                            
078700          MOVE MID-KDPRT         TO PARM-KDPRT                            
078800          MOVE 'PRINT STARTED'   TO MOD-TEMFSFEL                          
078900        END-IF                                                            
079000     END-IF                                                               
079100     MOVE MFS-ERASE-FIELD        TO MOD-KDPRT                             
079200                                                                          
079300     EJECT                                                                
079400     .                                                                    
079500                                                                          
079600*    FLYTTAR PARAMETRAR TILL SOPRUTIN OCH STARTAR UPP                     
079700 GA-STARTA-JOB SECTION.                                                   
079800     MOVE '6316'       TO MSGSOP-IDTRANS                                  
079900     MOVE MFS-KDMFSFOR TO MSGSOP-KDMFSFOR                                 
080000     MOVE 'W612S6    ' TO MSGSOP-IDPROCESS                                
080100     MOVE 'O'          TO MSGSOP-KDSOPFUNK                                
080200     MOVE PARM-TESYMBV TO MSGSOP-TESYMBV                                  
080300     PERFORM IMS-INSERT-ALT-MSG                                           
080400                                                                          
080500     SKIP3                                                                
080600     .                                                                    
080700 MFS-INIT-KEY-FIELD-IN SECTION.                                           
080800                                                                          
080900*    --- ALL INPUT KEY FIELDS                                             
081000                                                                          
081100     MOVE MFS-ERASE-FIELD   TO MOD-IDDC-IN                                
081200                               MOD-ADLAGOMR-IN                            
081300                               MOD-ADGANG-FOM-IN                          
081400                               MOD-ADGANG-TOM-IN                          
081500                               MOD-ADSEC-FOM-IN                           
081600                               MOD-ADSEC-TOM-IN                           
081700                               MOD-ADLEVEL-FOM-IN                         
081800                               MOD-ADLEVEL-TOM-IN                         
081900                               MOD-KDLOC-IN                               
082000                               MOD-KDFREQ-IN                              
082100                               MOD-TELOC-IN                               
082200     .                                                                    
082300     EJECT                                                                
082400 MFS-INIT-KEY-FIELD-OUT SECTION.                                          
082500                                                                          
082600*    --- ALL OUTPUT KEY FIELDS                                            
082700                                                                          
082800     MOVE MSGI-IDDC         TO MOD-IDDC-UT                                
082900     MOVE MFS-ERASE-FIELD   TO MOD-ADLAGOMR-UT                            
083000                               MOD-ADGANG-FOM-UT                          
083100                               MOD-ADGANG-TOM-UT                          
083200                               MOD-ADSEC-FOM-UT                           
083300                               MOD-ADSEC-TOM-UT                           
083400                               MOD-ADLEVEL-FOM-UT                         
083500                               MOD-ADLEVEL-TOM-UT                         
083600                               MOD-KDLOC-UT                               
083700                               MOD-KDFREQ-UT                              
083800                               MOD-TELOC-UT                               
083900     .                                                                    
084000     EJECT                                                                
084100 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
084200                                                                          
084300*    --- OUTDATA-FIELD ON SCROLL KEYS                                     
084400                                                                          
084500     MOVE +1 TO INDX                                                      
084600     PERFORM UNTIL INDX > MAX-INDX                                        
084700      MOVE MFS-ERASE-FIELD     TO MOD-ADLAGOMR-LINE   (INDX)              
084800                                  MOD-ADGANG-LINE     (INDX)              
084900                                  MOD-ADPLATS-LINE    (INDX)              
085000                                  MOD-KDLOC-LINE      (INDX)              
085100                                  MOD-KDFREQ-LINE     (INDX)              
085200                                  MOD-KDSTOR-LINE     (INDX)              
085300                                  MOD-TELOC-LINE      (INDX)              
085400      ADD +1 TO INDX                                                      
085500     END-PERFORM                                                          
085600     .                                                                    
085700     SKIP3                                                                
085800* --- IMS SECTIONS ---                                                    
085900     SKIP3                                                                
086000 IMS-GET-MSG SECTION.                                                     
086100                                                                          
086200     MOVE '  QC' TO GOOD-STATUSCODES                                      
086300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
086400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
086500     PERFORM IMS-STATUSCHECK                                              
086600     .                                                                    
086700     SKIP3                                                                
086800 IMS-INSERT-ALT-MSG SECTION.                                              
086900                                                                          
087000     MOVE SPACE TO GOOD-STATUSCODES                                       
087100     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
087200     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
087300     PERFORM IMS-STATUSCHECK                                              
087400     .                                                                    
087500     EJECT                                                                
087600 IMS-INSERT-MSG SECTION.                                                  
087700                                                                          
087800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
087900     MOVE SPACE TO GOOD-STATUSCODES                                       
088000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
088100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
088200     PERFORM IMS-STATUSCHECK                                              
088300     .                                                                    
088400     EJECT                                                                
088500 IMS-GU-LOCA SECTION.                                                     
088600                                                                          
088700     STRING 'WLLOCA01(WDJ801KY=>' W-WDJ8KY-MIN-X                          
088800                    '&WDJ801KY=<' W-WDJ8KY-MAX-X ')'                      
088900          DELIMITED BY SIZE INTO SSA1                                     
089000     MOVE '  GE' TO GOOD-STATUSCODES                                      
089100     CALL CBLTDLI USING GU LOCA-PCB LOC-WDJ801 SSA1                       
089200     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
089300     PERFORM IMS-STATUSCHECK                                              
089400     .                                                                    
089500     SKIP3                                                                
089600                                                                          
089700 IMS-GN-LOCA SECTION.                                                     
089800                                                                          
089900     STRING 'WLLOCA01(WDJ801KY=>' W-WDJ8KY-MIN-X                          
090000                    '&WDJ801KY=<' W-WDJ8KY-MAX-X ')'                      
090100          DELIMITED BY SIZE INTO SSA1                                     
090200     MOVE '  GE' TO GOOD-STATUSCODES                                      
090300     CALL CBLTDLI USING GN LOCA-PCB LOC-WDJ801 SSA1                       
090400     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
090500     PERFORM IMS-STATUSCHECK                                              
090600     .                                                                    
090700     SKIP3                                                                
090800                                                                          
090900 IMS-GU-ARTR01 SECTION.                                                   
091000                                                                          
091100     STRING 'WLARTR01(WDK7A1KY=>' WDK7A1KY-MIN-X                          
091200                    '&WDK7A1KY=<' WDK7A1KY-MAX-X ')'                      
091300          DELIMITED BY SIZE INTO SSA1                                     
091400     MOVE '  GE' TO GOOD-STATUSCODES                                      
091500     CALL CBLTDLI USING GU ARTR-PCB SEQA-WDK7A1 SSA1                      
091600     MOVE ARTR-STATUS-CODE TO STATUS-WS                                   
091700     PERFORM IMS-STATUSCHECK                                              
091800     .                                                                    
091900     SKIP3                                                                
092000                                                                          
092100 IMS-GU-ARTD-ASEQ SECTION.                                                
092200                                                                          
092300     STRING 'WLARTD11(WDD8ASEQ =' W-WDD8ASEQ-X ')'                        
092400          DELIMITED BY SIZE INTO SSA1                                     
092500     MOVE '  GBGE' TO GOOD-STATUSCODES                                    
092600     CALL CBLTDLI USING GU  ARTD-PCB SALDO-WDD811 SSA1                    
092700     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
092800     PERFORM IMS-STATUSCHECK                                              
092900     .                                                                    
093000     EJECT                                                                
093100                                                                          
093200 IMS-GU-WDB601    SECTION.                                                
093300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
093400          DELIMITED BY SIZE INTO SSA1                                     
093500     MOVE '  GE' TO GOOD-STATUSCODES                                      
093600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
093700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
093800     PERFORM IMS-STATUSCHECK                                              
093900     .                                                                    
094000     EJECT                                                                
094100 IMS-STATUSCHECK SECTION.                                                 
094200                                                                          
094300     SET STATUS-IX TO 1                                                   
094400     SEARCH GOOD-STATUS                                                   
094500       AT END                                                             
094600         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
094700         DELIMITED BY SIZE INTO ERROR-TEXT                                
094800         CALL FELLOG                                                      
094900       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
095000         CONTINUE                                                         
095100     END-SEARCH                                                           
095200     .                                                                    
