000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6031700.                                                
000300 AUTHOR.         MARTIEN HOMPES.                                          
000400 DATE-WRITTEN.   97/04/03.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        LIST ALL LOCATIONS                                               
000900*                                                                         
001000*        THE PROGRAM READS     WLLOCA (WDJ8)                              
001100*        THE PROGRAM READS     WLARTS (WDK7A)                             
001200*        THE PROGRAM READS     WLARTD (WDD8A)                             
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSACTION: W6T317                                              
001600*        MID:         W6I31701                                            
001700*                                                                         
001800*    OUTDATA.                                                             
001900*        MOD:         W6O31701                                            
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700*    -- CHECKED BY WY2000                                                 
002800 77  IDPGM                       PIC X(08)   VALUE 'W6031700'.            
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
004200*    --- INDEX FOR IDARTNR                                                
004300 77  TAB-INDX                    PIC 99     VALUE ZERO.                   
004400 77  TAB-MAX-INDX                PIC 99     VALUE ZERO.                   
004500 01  WORK-IDARTNR-TAB-GRP.                                                
004600     03 WORK-IDARTNR-TAB     PIC X(9)                                     
004700                             OCCURS 99.                                   
004800*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004900                                                                          
005000 77  WS-ADLAGOMR                 PIC 9(2)   VALUE ZERO.                   
005100 77  WS-ADGANG-FOM               PIC 9(2)   VALUE ZERO.                   
005200 77  WS-ADGANG-TOM               PIC 9(2)   VALUE ZERO.                   
005300 77  WS-ADGANG                   PIC 9(2)   VALUE ZERO.                   
005400 77  WS-ADSEC-FOM                PIC 9(2)   VALUE ZERO.                   
005500 77  WS-ADSEC-TOM                PIC 9(2)   VALUE ZERO.                   
005600 77  WS-ADSEC                    PIC 9(2)   VALUE ZERO.                   
005700 77  WS-ADLEVEL-FOM              PIC 9(2)   VALUE ZERO.                   
005800 77  WS-ADLEVEL-TOM              PIC 9(2)   VALUE ZERO.                   
005900 77  WS-ADLEVEL                  PIC 9(2)   VALUE ZERO.                   
006000 77  WS-ADSEQ                    PIC 9(2)   VALUE ZERO.                   
006100 77  WS-KDLOC                    PIC X(1)   VALUE SPACE.                  
006200 77  WS-KDFREQ                   PIC 9(2)   VALUE ZERO.                   
006300 77  WS-TELOC                    PIC X(1)   VALUE SPACE.                  
006400                                                                          
006500 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
006600     88  INDATA-OK                           VALUE 'Y'.                   
006700     88  INDATA-WRONG                        VALUE 'N'.                   
006800                                                                          
006900 77  KEYS-SW                     PIC X       VALUE 'Y'.                   
007000     88  KEYS-OK                             VALUE 'Y'.                   
007100     88  KEYS-WRONG                          VALUE 'N'.                   
007200                                                                          
007300 77  PRINT-SW                    PIC X       VALUE 'Y'.                   
007400     88  PRINT-OK                            VALUE 'Y'.                   
007500     88  PRINT-WRONG                         VALUE 'N'.                   
007600                                                                          
007700 77  RESTART-SW                  PIC X       VALUE 'N'.                   
007800     88  RESTART                             VALUE 'Y'.                   
007900                                                                          
008000 77  PRIME                       PIC X        VALUE 'P'.                  
008100 77  BUFFER                      PIC X        VALUE 'R'.                  
008200 77  MIXED                       PIC X        VALUE 'M'.                  
008300                                                                          
008400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008500     88  OWN-MID                             VALUE '6317'.                
008600     88  GOOD-MID                            VALUE '6317'.                
008700     88  HELP-MID                            VALUE '0551'.                
008800     EJECT                                                                
008900*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
009000 01  GENERAL-SUBPROGRAMS.                                                 
009100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009500     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
009600     EJECT                                                                
009700*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
009800*01 -COPY WMEDAREA                                                        
009900     SKIP3                                                                
010000*    --- PARAMETRAR TILL SUBPROGRAM W006PRT                               
010100*   -COPY W006PRT                                                         
010200     EJECT                                                                
010300 01  MESSAGE-CODES.                                                       
010400     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010500     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
010600     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010700     03  ITEMS-MISSING           PIC X(3)    VALUE '029'.                 
010800     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
010900     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
011000     03  INF-LAST-PAGE-SHOWN     PIC X(3)    VALUE '115'.                 
011100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011200     03  WRONG-INTERVAL-INFO     PIC X(3)    VALUE '738'.                 
011300     EJECT                                                                
011400*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
011500*                                                                         
011600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
011700     SKIP3                                                                
011800*01 -COPY WMSGINIT                                                        
011900     EJECT                                                                
012000*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
012100*                                                                         
012200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012300     SKIP3                                                                
012400*01  MID -COPY W6I31701                                                   
012500     EJECT                                                                
012600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012700     SKIP3                                                                
012800*01  -COPY WMSGAREA                                                       
012900     EJECT                                                                
013000     03  MOD REDEFINES MSG-AREA.                                          
013100*      05  -COPY W6O31701                                                 
013200     EJECT                                                                
013300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013400     SKIP3                                                                
013500*01  -COPY WMFSAREA                                                       
013600     EJECT                                                                
013700*    --- AREA FÖR SOP ANROP                                               
013800 01  W-PROG-TO-PROG-SW.                                                   
013900*03 -COPY WMSGSOP                                                         
014000  SKIP3                                                                   
014100 01 PARM-TESYMBV.                                                         
014200     03 FILLER                   PIC X(2)    VALUE 'A('.                  
014300     03 PARM-IDDC                PIC X(2)    VALUE SPACE.                 
014400     03 FILLER                   PIC X(1)    VALUE ')'.                   
014500     03 FILLER                   PIC X(2)    VALUE 'B('.                  
014600     03 PARM-ADLAGOMR            PIC 9(2)    VALUE ZERO.                  
014700     03 FILLER                   PIC X(1)    VALUE ')'.                   
014800     03 FILLER                   PIC X(2)    VALUE 'C('.                  
014900     03 PARM-ADGANG-FOM          PIC 9(2)    VALUE ZERO.                  
015000     03 FILLER                   PIC X(1)    VALUE ')'.                   
015100     03 FILLER                   PIC X(2)    VALUE 'D('.                  
015200     03 PARM-ADGANG-TOM          PIC 9(2)    VALUE ZERO.                  
015300     03 FILLER                   PIC X(1)    VALUE ')'.                   
015400     03 FILLER                   PIC X(2)    VALUE 'E('.                  
015500     03 PARM-ADSEC-FOM           PIC 9(2)    VALUE ZERO.                  
015600     03 FILLER                   PIC X(1)    VALUE ')'.                   
015700     03 FILLER                   PIC X(2)    VALUE 'F('.                  
015800     03 PARM-ADSEC-TOM           PIC 9(2)    VALUE ZERO.                  
015900     03 FILLER                   PIC X(1)    VALUE ')'.                   
016000     03 FILLER                   PIC X(2)    VALUE 'G('.                  
016100     03 PARM-ADLEVEL-FOM         PIC 9(2)    VALUE ZERO.                  
016200     03 FILLER                   PIC X(1)    VALUE ')'.                   
016300     03 FILLER                   PIC X(2)    VALUE 'H('.                  
016400     03 PARM-ADLEVEL-TOM         PIC 9(2)    VALUE ZERO.                  
016500     03 FILLER                   PIC X(1)    VALUE ')'.                   
016600     03 FILLER                   PIC X(2)    VALUE 'I('.                  
016700     03 PARM-KDLOC               PIC X       VALUE SPACE.                 
016800     03 FILLER                   PIC X(1)    VALUE ')'.                   
016900     03 FILLER                   PIC X(2)    VALUE 'J('.                  
017000     03 PARM-KDFREQ              PIC 9(2)    VALUE ZERO.                  
017100     03 FILLER                   PIC X(1)    VALUE ')'.                   
017200     03 FILLER                   PIC X(2)    VALUE 'K('.                  
017300     03 PARM-TELOC               PIC X       VALUE SPACE.                 
017400     03 FILLER                   PIC X(1)    VALUE ')'.                   
017500     03 FILLER                   PIC X(2)    VALUE 'L('.                  
017600     03 PARM-KDPRT               PIC X(3)    VALUE SPACE.                 
017700     03 FILLER                   PIC X(1)    VALUE ')'.                   
017800     EJECT                                                                
017900 01  WORK-AREA.                                                           
018000     03 WORK-ADLAGOMR            PIC 9(2).                                
018100     03 WORK-ADGANG              PIC 9(2).                                
018200     03 WORK-ADPLATS.                                                     
018300        05 WORK-ADSEC            PIC 9(2).                                
018400        05 WORK-ADLEVEL          PIC 9(2).                                
018500        05 WORK-ADSEQ            PIC 9(1).                                
018600     03 WORK-TELOC               PIC X(1).                                
018700     03 WORK-IDARTNR             PIC X(9).                                
018800   03  WDD8ASEQ-WORK.                                                     
018900     05  W-IDDC-ASEQ-WORK        PIC X(2)           VALUE SPACE.          
019000     05  W-ADBUFFOMR-ASEQ-WORK   PIC S9(3)  COMP-3  VALUE ZERO.           
019100     05  W-ADBUFFGANG-ASEQ-WORK  PIC S9(3)  COMP-3  VALUE ZERO.           
019200     05  W-ADBUFFPL-ASEQ-WORK    PIC S9(5)  COMP-3  VALUE ZERO.           
019300     05  W-DABUFPAF-ASEQ-WORK    PIC  9(8)  COMP-3  VALUE ZERO.           
019400     05  W-IDARTNR-ASEQ-WORK     PIC S9(9)  COMP-3  VALUE ZERO.           
019500                                                                          
019600 01  SAVE-AREA.                                                           
019700     03 SAVE-IDTRANS             PIC X(4)    VALUE  SPACE.                
019800     03 SAVE-ADLAGOMR-ENTER      PIC 9(2).                                
019900     03 SAVE-ADGANG-ENTER        PIC 9(2).                                
020000     03 SAVE-ADPLATS-ENTER.                                               
020100        05 SAVE-ADSEC-ENTER      PIC 9(2).                                
020200        05 SAVE-ADLEVEL-ENTER    PIC 9(2).                                
020300        05 SAVE-ADSEQ-ENTER      PIC 9(1).                                
020400     03 SAVE-ADLAGOMR-NEXT       PIC 9(2).                                
020500     03 SAVE-ADGANG-NEXT         PIC 9(2).                                
020600     03 SAVE-ADPLATS-NEXT.                                                
020700        05 SAVE-ADSEC-NEXT       PIC 9(2).                                
020800        05 SAVE-ADLEVEL-NEXT     PIC 9(2).                                
020900        05 SAVE-ADSEQ-NEXT       PIC 9(1).                                
021000                                                                          
021100                                                                          
021200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
021300     SKIP3                                                                
021400 01  KEYS-TO-DLI.                                                         
021500                                                                          
021600     03  W-WDJ8KY-MIN-X.                                                  
021700         05  W-LOC-IDDC-MIN       PIC X(2).                               
021800         05  W-LOC-ADLAGOMR-MIN   PIC 9(2).                               
021900         05  W-LOC-ADGANG-MIN     PIC 9(2).                               
022000         05  W-LOC-ADPLATS-MIN.                                           
022100             07 W-LOC-ADSEC-MIN   PIC 9(2).                               
022200             07 W-LOC-ADLEVEL-MIN PIC 9(2).                               
022300             07 W-LOC-ADSEQ-MIN   PIC 9(1).                               
022400                                                                          
022500     03  W-WDJ8KY-MAX-X.                                                  
022600         05  W-LOC-IDDC-MAX       PIC X(2).                               
022700         05  W-LOC-ADLAGOMR-MAX   PIC 9(2).                               
022800         05  W-LOC-ADGANG-MAX     PIC 9(2).                               
022900         05  W-LOC-ADPLATS-MAX.                                           
023000             07 W-LOC-ADSEC-MAX   PIC 9(2).                               
023100             07 W-LOC-ADLEVEL-MAX PIC 9(2).                               
023200             07 W-LOC-ADSEQ-MAX   PIC 9(1).                               
023300                                                                          
023400     03  W-WDJ8KEY-X.                                                     
023500         05  W-LOC-IDDC          PIC X(2)    VALUE SPACE.                 
023600         05  W-LOC-ADLAGOMR      PIC 9(2)    VALUE ZERO.                  
023700         05  W-LOC-ADGANG        PIC 9(2)    VALUE ZERO.                  
023800         05  W-LOC-ADPLATS.                                               
023900             07 W-LOC-ADSEC      PIC 9(2).                                
024000             07 W-LOC-ADLEVEL    PIC 9(2).                                
024100             07 W-LOC-ADSEQ      PIC 9(1).                                
024200                                                                          
024300   03  WDK7A1KY-MIN-X.                                                    
024400     05  W-IDDC-MIN          PIC X(2)           VALUE SPACE.              
024500     05  W-ADART-MIN.                                                     
024600       07  W-ADLAGOMR-MIN    PIC S9(3)  COMP-3  VALUE ZERO.               
024700       07  W-ADGANG-MIN      PIC S9(3)  COMP-3  VALUE ZERO.               
024800       07  W-ADPLATS-MIN     PIC S9(5)  COMP-3  VALUE ZERO.               
024900     05 W-IDARTNR-MIN        PIC S9(9)  COMP-3  VALUE ZERO.               
025000                                                                          
025100   03  WDK7A1KY-MAX-X.                                                    
025200     05  W-IDDC-MAX          PIC X(2)           VALUE SPACE.              
025300     05  W-ADART-MAX.                                                     
025400       07  W-ADLAGOMR-MAX    PIC S9(3)  COMP-3  VALUE ZERO.               
025500       07  W-ADGANG-MAX      PIC S9(3)  COMP-3  VALUE ZERO.               
025600       07  W-ADPLATS-MAX     PIC S9(5)  COMP-3  VALUE ZERO.               
025700     05 W-IDARTNR-MAX     PIC S9(9)  COMP-3  VALUE 99999999.              
025800                                                                          
025900   03  W-WDD8ASEQ-X.                                                      
026000     05  W-IDDC-ASEQ         PIC X(2)           VALUE SPACE.              
026100     05  W-ADBUFFOMR-ASEQ    PIC S9(3)  COMP-3  VALUE ZERO.               
026200     05  W-ADBUFFGANG-ASEQ   PIC S9(3)  COMP-3  VALUE ZERO.               
026300     05  W-ADBUFFPL-ASEQ     PIC S9(5)  COMP-3  VALUE ZERO.               
026400                                                                          
026500   03  WDD8ASEQ-MIN-X.                                                    
026600     05  W-IDDC-ASEQ-MIN         PIC X(2)           VALUE SPACE.          
026700     05  W-ADBUFFOMR-ASEQ-MIN    PIC S9(3)  COMP-3  VALUE ZERO.           
026800     05  W-ADBUFFGANG-ASEQ-MIN   PIC S9(3)  COMP-3  VALUE ZERO.           
026900     05  W-ADBUFFPL-ASEQ-MIN     PIC S9(5)  COMP-3  VALUE ZERO.           
027000     05  W-DABUFPAF-ASEQ-MIN     PIC  9(8)  COMP-3  VALUE ZERO.           
027100     05  W-IDARTNR-ASEQ-MIN      PIC S9(9)  COMP-3  VALUE ZERO.           
027200                                                                          
027300   03  WDD8ASEQ-MAX-X.                                                    
027400     05  W-IDDC-ASEQ-MAX       PIC X(2)          VALUE SPACE.             
027500     05  W-ADBUFFOMR-ASEQ-MAX  PIC S9(3)  COMP-3 VALUE ZERO.              
027600     05  W-ADBUFFGANG-ASEQ-MAX PIC S9(3)  COMP-3 VALUE ZERO.              
027700     05  W-ADBUFFPL-ASEQ-MAX   PIC S9(5)  COMP-3 VALUE ZERO.              
027800     05  W-DABUFPAF-ASEQ-MAX   PIC  9(8)  COMP-3 VALUE 99999999.          
027900     05  W-IDARTNR-ASEQ-MAX    PIC S9(9)  COMP-3 VALUE 999999999.         
028000                                                                          
028100     03  W-IDDC-B6-X.                                                     
028200         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
028300                                                                          
028400     SKIP2                                                                
028500*    --- STATUS-KOD FRÅN IMS                                              
028600 01  STATUS-WS                   PIC XX.                                  
028700     88  SEGMENT-FOUND                       VALUE '  '.                  
028800     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
028900     88  SEGMENT-MISSING                     VALUE 'GE'.                  
029000     88  END-OF-DATABASE                     VALUE 'GB'.                  
029100     SKIP2                                                                
029200 01  GOOD-STATUSCODES.                                                    
029300     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
029400     SKIP3                                                                
029500 01  SSA1                        PIC X(64).                               
029600 01  SSA2                        PIC X(64).                               
029700     EJECT                                                                
029800*    --- IMS FUNCTION CODES                                               
029900*01  -COPY W0003                                                          
030000     EJECT                                                                
030100*    ---  DLI INPUT-OUTPUT AREA                                           
030200                                                                          
030300 01  FILLER         PIC X(20) VALUE 'WLLOCA01-AREA'.                      
030400 01  DLI-IO-WLLOCA01.                                                     
030500*    03  -COPY WDJ801                                                     
030600                                                                          
030700 01  FILLER         PIC X(20) VALUE 'WLARTA01-AREA'.                      
030800 01  DLI-IO-WLARTA01.                                                     
030900*    03  -COPY WDK7A1                                                     
031000                                                                          
031100 01  FILLER         PIC X(20) VALUE 'WLARTD11-AREA'.                      
031200 01  DLI-IO-WLARTD11.                                                     
031300*    03  -COPY WDD811                                                     
031400     EJECT                                                                
031500 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
031600 01   DLI-IO-AREA-B601.                                                   
031700*     03  -COPY WDB601                                                    
031800                                                                          
031900     EJECT                                                                
032000 LINKAGE SECTION.                                                         
032100*01  -COPY W0009  -PRE MSG-                                               
032200*01  -COPY W0009  -PRE ALT-                                               
032300     EJECT                                                                
032400*01  -COPY W0008  -PRE USEA-                                              
032500     05  FILLER                  PIC X.                                   
032600     EJECT                                                                
032700*01  -COPY W0008  -PRE LOCA-                                              
032800     05  FILLER                  PIC X.                                   
032900     EJECT                                                                
033000*01  -COPY W0008  -PRE ARTR-                                              
033100     05  FILLER                  PIC X.                                   
033200     EJECT                                                                
033300*01  -COPY W0008  -PRE ARTD-                                              
033400     05  FILLER                  PIC X.                                   
033500     EJECT                                                                
033600*01  -COPY W0008      -PRE WDB6-                                          
033700     05  FILLER                  PIC X.                                   
033800     EJECT                                                                
033900 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB                                
034000           USEA-PCB                                                       
034100           LOCA-PCB ARTR-PCB ARTD-PCB                                     
034200           WDB6-PCB.                                                      
034300 MAIN SECTION.                                                            
034400     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB                                
034500            USEA-PCB                                                      
034600            LOCA-PCB ARTR-PCB ARTD-PCB                                    
034700            WDB6-PCB.                                                     
034800                                                                          
034900     PERFORM IMS-GET-MSG                                                  
035000     IF SEGMENT-FOUND                                                     
035100       PERFORM A-INIT                                                     
035200       IF GOOD-MID OR HELP-MID                                            
035300          PERFORM B-CHECK-KEYS                                            
035400          IF KEYS-OK                                                      
035500             IF MFS-PRINT                                                 
035600                PERFORM G-CHECK-PRINT                                     
035700                IF PRINT-OK                                               
035800                   PERFORM GA-STARTA-JOB                                  
035900                END-IF                                                    
036000             END-IF                                                       
036100             IF MFS-FIRST                                                 
036200                PERFORM C-FIRST-PAGE                                      
036300              ELSE                                                        
036400                IF MFS-NEXT                                               
036500                   PERFORM D-NEXT-PAGE                                    
036600                ELSE                                                      
036700                   PERFORM E-SAME-PAGE                                    
036800                END-IF                                                    
036900             END-IF                                                       
037000             PERFORM F-READ-SHOW-INFO                                     
037100          END-IF                                                          
037200       END-IF                                                             
037300       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O31701 + 4                      
037400       PERFORM IMS-INSERT-MSG                                             
037500     END-IF                                                               
037600                                                                          
037700     MOVE ZERO TO RETURN-CODE                                             
037800     GOBACK                                                               
037900     .                                                                    
038000     EJECT                                                                
038100 A-INIT SECTION.                                                          
038200                                                                          
038300     IF MSG-DOUBLE-TRANSACTIONS                                           
038400       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W6I31701                 
038500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
038600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
038700     ELSE                                                                 
038800       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W6I31701                  
038900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
039000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
039100     END-IF                                                               
039200                                                                          
039300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
039400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
039500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
039600                                                                          
039700     MOVE LOW-VALUE TO MSG-AREA                                           
039800     MOVE 'W6O317N1' TO MFS-IDMOD                                         
039900     MOVE '6317' TO MOD-IDTRANS                                           
040000     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
040100                                                                          
040200                                                                          
040300     MOVE ALL '+'           TO MSGI-WMSGINIT                              
040400     MOVE '001'             TO MSGI-KDCALL                                
040500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
040600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
040700     MOVE '6317'            TO MSGI-IDTRANS                               
040800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
040900                                                                          
041000     MOVE MSGI-SPAR-AREA    TO SAVE-AREA                                  
041100     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
041200                                                                          
041300     IF GOOD-MID OR HELP-MID                                              
041400       CONTINUE                                                           
041500     ELSE                                                                 
041600       MOVE SPACE TO MFS-KDTRTYP                                          
041700       MOVE '7' TO MFS-IDPFK                                              
041800       PERFORM MFS-INIT-KEY-FIELD-IN                                      
041900       PERFORM MFS-INIT-KEY-FIELD-OUT                                     
042000       PERFORM MFS-ERASE-LINE-FIELD-OUT                                   
042100     END-IF                                                               
042200     .                                                                    
042300     EJECT                                                                
042400 B-CHECK-KEYS SECTION.                                                    
042500                                                                          
042600                                                                          
042700     MOVE YES               TO KEYS-SW                                    
042800                                                                          
042900     PERFORM MFS-INIT-KEY-FIELD-IN                                        
043000                                                                          
043100*                                                                         
043200*    -- CONTROL  ON WAREHOUSE                                             
043300*                                                                         
043400     MOVE MSGI-IDDC         TO W-IDDC-B6                                  
043500     PERFORM IMS-GU-WDB601                                                
043600     IF SEGMENT-FOUND                                                     
043700     AND (DCS-SDC                                                         
043800     OR   DCS-NDC-NA                                                      
043900     OR   DCS-NDC-PF                                                      
043910     OR   DCS-NDC-OTHERS)                                                 
044000       CONTINUE                                                           
044100     ELSE                                                                 
044200       MOVE NOO                 TO KEYS-SW                                
044300     END-IF                                                               
044400*                                                                         
044500*    -- CONTROL  AREA                                                     
044600*                                                                         
044700     IF MID-ADLAGOMR-IN = ALL '+'                                         
044800       INSPECT MID-ADLAGOMR-UT REPLACING LEADING SPACE BY ZERO            
044900       MOVE MID-ADLAGOMR-UT    TO WS-ADLAGOMR                             
045000     ELSE                                                                 
045100       IF MID-ADLAGOMR-IN NUMERIC                                         
045200          MOVE '7' TO MFS-IDPFK                                           
045300          MOVE    SPACE        TO MFS-KDTRTYP                             
045400          MOVE MID-ADLAGOMR-IN TO WS-ADLAGOMR                             
045500       ELSE                                                               
045600          MOVE NOO             TO KEYS-SW                                 
045700       END-IF                                                             
045800     END-IF                                                               
045900*                                                                         
046000*    -- CONTROL  AISLE FROM                                               
046100*                                                                         
046200     IF MID-ADGANG-FOM-IN = ALL '+'                                       
046300       INSPECT MID-ADGANG-FOM-UT REPLACING LEADING SPACE BY ZERO          
046400       MOVE MID-ADGANG-FOM-UT    TO WS-ADGANG-FOM                         
046500     ELSE                                                                 
046600       IF MID-ADGANG-FOM-IN NUMERIC                                       
046700          MOVE '7' TO MFS-IDPFK                                           
046800          MOVE    SPACE          TO MFS-KDTRTYP                           
046900          MOVE MID-ADGANG-FOM-IN TO WS-ADGANG-FOM                         
047000       ELSE                                                               
047100          MOVE NOO               TO KEYS-SW                               
047200       END-IF                                                             
047300     END-IF                                                               
047400*                                                                         
047500*    -- CONTROL  AISLE THRU                                               
047600*                                                                         
047700     IF MID-ADGANG-TOM-IN = ALL '+'                                       
047800       INSPECT MID-ADGANG-TOM-UT REPLACING LEADING SPACE BY ZERO          
047900       MOVE MID-ADGANG-TOM-UT    TO WS-ADGANG-TOM                         
048000     ELSE                                                                 
048100       IF MID-ADGANG-TOM-IN NUMERIC                                       
048200          MOVE '7' TO MFS-IDPFK                                           
048300          MOVE    SPACE          TO MFS-KDTRTYP                           
048400          MOVE MID-ADGANG-TOM-IN TO WS-ADGANG-TOM                         
048500       ELSE                                                               
048600          MOVE NOO               TO KEYS-SW                               
048700       END-IF                                                             
048800     END-IF                                                               
048900*                                                                         
049000*    -- CONTROL  ADSEC FROM                                               
049100*                                                                         
049200     IF MID-ADSEC-FOM-IN = ALL '+'                                        
049300       INSPECT MID-ADSEC-FOM-UT REPLACING LEADING SPACE BY ZERO           
049400       MOVE MID-ADSEC-FOM-UT    TO WS-ADSEC-FOM                           
049500     ELSE                                                                 
049600       IF MID-ADSEC-FOM-IN NUMERIC                                        
049700          MOVE '7' TO MFS-IDPFK                                           
049800          MOVE    SPACE          TO MFS-KDTRTYP                           
049900          MOVE MID-ADSEC-FOM-IN TO WS-ADSEC-FOM                           
050000       ELSE                                                               
050100          MOVE NOO               TO KEYS-SW                               
050200       END-IF                                                             
050300     END-IF                                                               
050400*                                                                         
050500*    -- CONTROL  ADSEC THRU                                               
050600*                                                                         
050700     IF MID-ADSEC-TOM-IN = ALL '+'                                        
050800       INSPECT MID-ADSEC-TOM-UT REPLACING LEADING SPACE BY ZERO           
050900       MOVE MID-ADSEC-TOM-UT    TO WS-ADSEC-TOM                           
051000     ELSE                                                                 
051100       IF MID-ADSEC-TOM-IN NUMERIC                                        
051200          MOVE '7' TO MFS-IDPFK                                           
051300          MOVE    SPACE          TO MFS-KDTRTYP                           
051400          MOVE MID-ADSEC-TOM-IN TO WS-ADSEC-TOM                           
051500       ELSE                                                               
051600          MOVE NOO               TO KEYS-SW                               
051700       END-IF                                                             
051800     END-IF                                                               
051900*                                                                         
052000*    -- CONTROL  LEVEL FROM                                               
052100*                                                                         
052200     IF MID-ADLEVEL-FOM-IN = ALL '+'                                      
052300       INSPECT MID-ADLEVEL-FOM-UT REPLACING LEADING SPACE BY ZERO         
052400       MOVE MID-ADLEVEL-FOM-UT    TO WS-ADLEVEL-FOM                       
052500     ELSE                                                                 
052600       IF MID-ADLEVEL-FOM-IN NUMERIC                                      
052700          MOVE '7' TO MFS-IDPFK                                           
052800          MOVE    SPACE           TO MFS-KDTRTYP                          
052900          MOVE MID-ADLEVEL-FOM-IN TO WS-ADLEVEL-FOM                       
053000       ELSE                                                               
053100          MOVE NOO                TO KEYS-SW                              
053200       END-IF                                                             
053300     END-IF                                                               
053400                                                                          
053500*                                                                         
053600*    -- CONTROL  LEVEL THRU                                               
053700*                                                                         
053800     IF MID-ADLEVEL-TOM-IN = ALL '+'                                      
053900       INSPECT MID-ADLEVEL-TOM-UT REPLACING LEADING SPACE BY ZERO         
054000       MOVE MID-ADLEVEL-TOM-UT    TO WS-ADLEVEL-TOM                       
054100     ELSE                                                                 
054200       IF MID-ADLEVEL-TOM-IN NUMERIC                                      
054300          MOVE '7' TO MFS-IDPFK                                           
054400          MOVE    SPACE           TO MFS-KDTRTYP                          
054500          MOVE MID-ADLEVEL-TOM-IN TO WS-ADLEVEL-TOM                       
054600       ELSE                                                               
054700          MOVE NOO                TO KEYS-SW                              
054800       END-IF                                                             
054900     END-IF                                                               
055000*                                                                         
055100*    -- CONTROL  TYPE OF LOCATION                                         
055200*                                                                         
055300     IF MID-KDLOC-IN  = ALL '+'                                           
055400        MOVE MID-KDLOC-UT          TO WS-KDLOC                            
055500      ELSE                                                                
055600        IF MID-KDLOC-IN  =  PRIME OR BUFFER OR MIXED                      
055700           MOVE '7'                TO MFS-IDPFK                           
055800           MOVE SPACE              TO MFS-KDTRTYP                         
055900           MOVE MID-KDLOC-IN       TO WS-KDLOC                            
056000         ELSE                                                             
056100           MOVE NOO                 TO KEYS-SW                            
056200        END-IF                                                            
056300     END-IF                                                               
056400*                                                                         
056500*    -- CONTROL  FREQUENCY TYPE                                           
056600*                                                                         
056700     IF MID-KDFREQ-IN = ALL '+'                                           
056800        MOVE MID-KDFREQ-UT          TO WS-KDFREQ                          
056900     ELSE                                                                 
057000       IF MID-KDFREQ-IN NUMERIC                                           
057100          MOVE '7'                  TO MFS-IDPFK                          
057200          MOVE SPACE                TO MFS-KDTRTYP                        
057300          MOVE MID-KDFREQ-IN        TO WS-KDFREQ                          
057400       ELSE                                                               
057500          MOVE NOO                  TO KEYS-SW                            
057600       END-IF                                                             
057700     END-IF                                                               
057800*                                                                         
057900*    -- FILL IN REMARK FIELD INPUT                                        
058000*                                                                         
058100     IF MID-TELOC-IN  = ALL '+'                                           
058200        MOVE MID-TELOC-UT         TO WS-TELOC                             
058300      ELSE                                                                
058400        MOVE '7'                  TO MFS-IDPFK                            
058500        MOVE SPACE                TO MFS-KDTRTYP                          
058600        MOVE MID-TELOC-IN         TO WS-TELOC                             
058700     END-IF                                                               
058800*                                                                         
058900*    -- FILL MOD KEY-OUTPUT FIELDS                                        
059000*                                                                         
059100     MOVE W-IDDC-B6      TO  MOD-IDDC-UT                                  
059200     MOVE WS-ADLAGOMR    TO  MOD-ADLAGOMR-UT                              
059300     MOVE WS-ADGANG-FOM  TO  MOD-ADGANG-FOM-UT                            
059400     MOVE WS-ADGANG-TOM  TO  MOD-ADGANG-TOM-UT                            
059500     MOVE WS-ADSEC-FOM   TO  MOD-ADSEC-FOM-UT                             
059600     MOVE WS-ADSEC-TOM   TO  MOD-ADSEC-TOM-UT                             
059700     MOVE WS-ADLEVEL-FOM TO  MOD-ADLEVEL-FOM-UT                           
059800     MOVE WS-ADLEVEL-TOM TO  MOD-ADLEVEL-TOM-UT                           
059900     MOVE WS-KDLOC       TO  MOD-KDLOC-UT                                 
060000     MOVE WS-KDFREQ      TO  MOD-KDFREQ-UT                                
060100     MOVE WS-TELOC       TO  MOD-TELOC-UT                                 
060200                                                                          
060300                                                                          
060400     IF KEYS-WRONG                                                        
060500       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
060600       CALL WMEDKONV USING MED-WMEDAREA                                   
060700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
060800      ELSE                                                                
060900       IF WS-ADGANG-FOM   >  WS-ADGANG-TOM  OR                            
061000          WS-ADSEC-FOM    >  WS-ADSEC-TOM   OR                            
061100          WS-ADLEVEL-FOM  >  WS-ADLEVEL-TOM                               
061200                                                                          
061300          MOVE WRONG-INTERVAL-INFO TO MED-IDMFSFEL                        
061400          CALL WMEDKONV USING MED-WMEDAREA                                
061500          MOVE MED-MFSFEL          TO MOD-TEMFSFEL                        
061600          MOVE NOO                 TO KEYS-SW                             
061700       END-IF                                                             
061800     END-IF                                                               
061900                                                                          
062000     .                                                                    
062100     EJECT                                                                
062200 C-FIRST-PAGE SECTION.                                                    
062300                                                                          
062400                                                                          
062500*                                                                         
062600*    -- FILL START KEY FIELDS                                             
062700*                                                                         
062800     MOVE W-IDDC-B6             TO W-LOC-IDDC-MIN                         
062900     MOVE WS-ADLAGOMR           TO W-LOC-ADLAGOMR-MIN                     
063000     MOVE WS-ADGANG-FOM         TO W-LOC-ADGANG-MIN                       
063100     MOVE WS-ADSEC-FOM          TO W-LOC-ADSEC-MIN                        
063200     MOVE WS-ADLEVEL-FOM        TO W-LOC-ADLEVEL-MIN                      
063300     MOVE ZEROES                TO W-LOC-ADSEQ-MIN                        
063400                                                                          
063500     .                                                                    
063600     EJECT                                                                
063700 E-SAME-PAGE SECTION.                                                     
063800                                                                          
063900*                                                                         
064000*    -- FILL START KEY FIELDS                                             
064100*                                                                         
064200     MOVE W-IDDC-B6             TO W-LOC-IDDC-MIN                         
064300     MOVE SAVE-ADLAGOMR-ENTER   TO W-LOC-ADLAGOMR-MIN                     
064400     MOVE SAVE-ADGANG-ENTER     TO W-LOC-ADGANG-MIN                       
064500     MOVE SAVE-ADSEC-ENTER      TO W-LOC-ADSEC-MIN                        
064600     MOVE SAVE-ADLEVEL-ENTER    TO W-LOC-ADLEVEL-MIN                      
064700     MOVE SAVE-ADSEQ-ENTER      TO W-LOC-ADSEQ-MIN                        
064800     .                                                                    
064900     EJECT                                                                
065000                                                                          
065100 D-NEXT-PAGE SECTION.                                                     
065200                                                                          
065300*                                                                         
065400*    -- FILL START KEY FIELDS                                             
065500*                                                                         
065600     MOVE W-IDDC-B6             TO W-LOC-IDDC-MIN                         
065700     MOVE SAVE-ADLAGOMR-NEXT    TO W-LOC-ADLAGOMR-MIN                     
065800     MOVE SAVE-ADGANG-NEXT      TO W-LOC-ADGANG-MIN                       
065900     MOVE SAVE-ADSEC-NEXT       TO W-LOC-ADSEC-MIN                        
066000     MOVE SAVE-ADLEVEL-NEXT     TO W-LOC-ADLEVEL-MIN                      
066100     MOVE SAVE-ADSEQ-NEXT       TO W-LOC-ADSEQ-MIN                        
066200     .                                                                    
066300     EJECT                                                                
066400                                                                          
066500 F-READ-SHOW-INFO SECTION.                                                
066600                                                                          
066700*                                                                         
066800*    -- FILL FINISH (MAX) KEY FIELDS                                      
066900*                                                                         
067000     MOVE W-IDDC-B6             TO W-LOC-IDDC-MAX                         
067100     MOVE WS-ADLAGOMR           TO W-LOC-ADLAGOMR-MAX                     
067200     MOVE WS-ADGANG-TOM         TO W-LOC-ADGANG-MAX                       
067300     MOVE WS-ADSEC-TOM          TO W-LOC-ADSEC-MAX                        
067400     MOVE WS-ADLEVEL-TOM        TO W-LOC-ADLEVEL-MAX                      
067500     MOVE 9                     TO W-LOC-ADSEQ-MAX                        
067600                                                                          
067700     MOVE 99999999              TO W-IDARTNR-MAX                          
067800                                                                          
067900     PERFORM  IMS-GU-LOCA                                                 
068000                                                                          
068100     IF SEGMENT-MISSING                                                   
068200        IF MFS-NEXT                                                       
068300           MOVE INF-LAST-PAGE-SHOWN TO    MED-IDMFSFEL                    
068400         ELSE                                                             
068500           MOVE ITEMS-MISSING       TO    MED-IDMFSFEL                    
068600        END-IF                                                            
068700        CALL    WMEDKONV    USING MED-WMEDAREA                            
068800        MOVE MED-MFSFEL     TO MOD-TEMFSFEL                               
068900                                                                          
069000        PERFORM MFS-ERASE-LINE-FIELD-OUT                                  
069100                                                                          
069200        MOVE WS-ADLAGOMR    TO SAVE-ADLAGOMR-ENTER                        
069300        MOVE WS-ADGANG-FOM  TO SAVE-ADGANG-ENTER                          
069400        MOVE WS-ADSEC-FOM   TO SAVE-ADSEC-ENTER                           
069500        MOVE WS-ADLEVEL-FOM TO SAVE-ADLEVEL-ENTER                         
069600        MOVE ZEROES         TO SAVE-ADSEQ-ENTER                           
069700                                                                          
069800        MOVE WS-ADLAGOMR    TO SAVE-ADLAGOMR-NEXT                         
069900        MOVE WS-ADGANG-FOM  TO SAVE-ADGANG-NEXT                           
070000        MOVE WS-ADSEC-FOM   TO SAVE-ADSEC-NEXT                            
070100        MOVE WS-ADLEVEL-FOM TO SAVE-ADLEVEL-NEXT                          
070200        MOVE ZEROES         TO SAVE-ADSEQ-NEXT                            
070300                                                                          
070400      ELSE                                                                
070500        MOVE LOC-ADLAGOMR   TO SAVE-ADLAGOMR-ENTER                        
070600        MOVE LOC-ADGANG     TO SAVE-ADGANG-ENTER                          
070700        MOVE LOC-ADPLATS    TO SAVE-ADPLATS-ENTER                         
070800        MOVE +1             TO INDX                                       
070900        PERFORM UNTIL SEGMENT-MISSING OR END-OF-DATABASE OR               
071000                      INDX > MAX-INDX                                     
071100         MOVE LOC-ADPLATS         TO WORK-ADPLATS                         
071200         MOVE LOC-TELOC           TO WORK-TELOC                           
071300         MOVE SPACES              TO WORK-IDARTNR                         
071400         IF WS-ADLEVEL-FOM <= WORK-ADLEVEL              AND               
071500            WS-ADLEVEL-TOM >= WORK-ADLEVEL              AND               
071600            WS-ADSEC-FOM <= WORK-ADSEC                  AND               
071700            WS-ADSEC-TOM >= WORK-ADSEC                  AND               
071800            (WS-KDLOC  = " " OR  WS-KDLOC = LOC-KDLOC)  AND               
071900            (WS-KDFREQ = 0   OR WS-KDFREQ = LOC-KDFREQ) AND               
072000            (WS-TELOC  = " " OR WS-TELOC  = WORK-TELOC)                   
072100            MOVE LOC-IDDC               TO W-IDDC-MIN                     
072200                                        W-IDDC-MAX                        
072300            MOVE LOC-ADLAGOMR           TO W-ADLAGOMR-MIN                 
072400                                        W-ADLAGOMR-MAX                    
072500            MOVE LOC-ADGANG             TO W-ADGANG-MIN                   
072600                                        W-ADGANG-MAX                      
072700            MOVE LOC-ADPLATS            TO W-ADPLATS-MIN                  
072800                                        W-ADPLATS-MAX                     
072900            PERFORM     IMS-GU-ARTR01                                     
073000            IF LOC-KDLOC     =  PRIME OR MIXED                            
073100               PERFORM UNTIL SEGMENT-MISSING OR                           
073200                             END-OF-DATABASE OR                           
073300                             TAB-INDX > 98                                
073400                  ADD 1 TO TAB-INDX                                       
073500                           TAB-MAX-INDX                                   
073600                  MOVE SEQA-IDARTNR                                       
073700                                 TO WORK-IDARTNR-TAB (TAB-INDX)           
073800                  INSPECT WORK-IDARTNR-TAB (TAB-INDX)                     
073900                   REPLACING LEADING ZEROES BY SPACE                      
074000                  PERFORM IMS-GN-ARTR01                                   
074100               END-PERFORM                                                
074200            END-IF                                                        
074300                                                                          
074400            IF LOC-KDLOC  =  (BUFFER OR MIXED) AND                        
074500               TAB-MAX-INDX = ZERO                                        
074600                                                                          
074700               MOVE LOC-IDDC            TO W-IDDC-ASEQ                    
074800               MOVE LOC-ADLAGOMR        TO W-ADBUFFOMR-ASEQ               
074900               MOVE LOC-ADGANG          TO W-ADBUFFGANG-ASEQ              
075000               MOVE LOC-ADPLATS         TO W-ADBUFFPL-ASEQ                
075100                                                                          
075200               PERFORM  IMS-GU-ARTD-ASEQ                                  
075300                                                                          
075400               IF SEGMENT-FOUND                                           
075500                  MOVE SEQA-IDARTNR     TO WORK-IDARTNR                   
075600                  INSPECT WORK-IDARTNR REPLACING LEADING                  
075700                     ZEROES BY SPACE                                      
075800               END-IF                                                     
075900            END-IF                                                        
076000            IF TAB-MAX-INDX = ZERO                                        
076100              MOVE LOC-ADLAGOMR      TO MOD-ADLAGOMR-LINE (INDX)          
076200              MOVE LOC-ADGANG        TO MOD-ADGANG-LINE   (INDX)          
076300              MOVE LOC-ADPLATS       TO MOD-ADPLATS-LINE  (INDX)          
076400              MOVE LOC-KDLOC         TO MOD-KDLOC-LINE    (INDX)          
076500              MOVE LOC-KDFREQ        TO MOD-KDFREQ-LINE   (INDX)          
076600              MOVE LOC-KDSTOR        TO MOD-KDSTOR-LINE   (INDX)          
076700              MOVE LOC-TELOC         TO MOD-TELOC-LINE    (INDX)          
076800              MOVE WORK-IDARTNR      TO MOD-IDARTNR-LINE  (INDX)          
076900              ADD 1 TO INDX                                               
077000            ELSE                                                          
077100              MOVE LOC-ADLAGOMR      TO MOD-ADLAGOMR-LINE (INDX)          
077200              MOVE LOC-ADGANG        TO MOD-ADGANG-LINE   (INDX)          
077300              MOVE LOC-ADPLATS       TO MOD-ADPLATS-LINE  (INDX)          
077400              MOVE LOC-KDLOC         TO MOD-KDLOC-LINE    (INDX)          
077500              MOVE LOC-KDFREQ        TO MOD-KDFREQ-LINE   (INDX)          
077600              MOVE LOC-KDSTOR        TO MOD-KDSTOR-LINE   (INDX)          
077700              MOVE LOC-TELOC         TO MOD-TELOC-LINE    (INDX)          
077800              MOVE 1  TO TAB-INDX                                         
077900              PERFORM UNTIL TAB-INDX > TAB-MAX-INDX                       
078000                OR INDX > MAX-INDX                                        
078100                MOVE WORK-IDARTNR-TAB (TAB-INDX)                          
078200                                     TO MOD-IDARTNR-LINE  (INDX)          
078300                ADD 1 TO TAB-INDX                                         
078400                         INDX                                             
078500              END-PERFORM                                                 
078600              MOVE ZERO              TO TAB-INDX                          
078700                                        TAB-MAX-INDX                      
078800                                                                          
078900            END-IF                                                        
079000         END-IF                                                           
079100         PERFORM IMS-GN-LOCA                                              
079200        END-PERFORM                                                       
079300        IF SEGMENT-FOUND                                                  
079400           MOVE LOC-ADLAGOMR   TO SAVE-ADLAGOMR-NEXT                      
079500           MOVE LOC-ADGANG     TO SAVE-ADGANG-NEXT                        
079600           MOVE LOC-ADPLATS    TO SAVE-ADPLATS-NEXT                       
079700           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
079800           CALL WMEDKONV USING MED-WMEDAREA                               
079900           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
080000         ELSE                                                             
080100           MOVE LOC-ADLAGOMR        TO    SAVE-ADLAGOMR-NEXT              
080200           MOVE LOC-ADGANG          TO    SAVE-ADGANG-NEXT                
080300           MOVE LOC-ADPLATS         TO    SAVE-ADPLATS-NEXT               
080400           MOVE INF-LAST-PAGE-SHOWN TO    MED-IDMFSFEL                    
080500           CALL WMEDKONV            USING MED-WMEDAREA                    
080600           MOVE MED-MFSFEL          TO    MOD-TEMFSFEL                    
080700           PERFORM UNTIL INDX > MAX-INDX                                  
080800             MOVE MFS-ERASE-FIELD   TO  MOD-ADLAGOMR-LINE (INDX)          
080900                                        MOD-ADGANG-LINE   (INDX)          
081000                                        MOD-ADPLATS-LINE  (INDX)          
081100                                        MOD-KDLOC-LINE    (INDX)          
081200                                        MOD-KDFREQ-LINE   (INDX)          
081300                                        MOD-KDSTOR-LINE   (INDX)          
081400                                        MOD-TELOC-LINE    (INDX)          
081500                                        MOD-IDARTNR-LINE  (INDX)          
081600             ADD 1 TO INDX                                                
081700           END-PERFORM                                                    
081800         END-IF                                                           
081900                                                                          
082000       MOVE '002'      TO MSGI-KDCALL                                     
082100       MOVE '6317'     TO SAVE-IDTRANS                                    
082200       MOVE SAVE-AREA  TO MSGI-SPAR-AREA                                  
082300       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
082400     END-IF                                                               
082500     .                                                                    
082600     EJECT                                                                
082700 G-CHECK-PRINT SECTION.                                                   
082800     IF MID-KDPRT = ALL '+'                                               
082900        MOVE NOO TO PRINT-SW                                              
083000        MOVE 'PRINT CODE WRONG'  TO MOD-TEMFSFEL                          
083100     ELSE                                                                 
083200        MOVE '6L'      TO PRT-IDPRTLST(1:2)                               
083300        MOVE MID-KDPRT TO PRT-IDPRTLST(3:3)                               
083400        MOVE SPACE     TO PRT-IDPRTLST(6:3)                               
083500        MOVE 1                 TO PRT-KDCALL                              
083600        CALL W006PRT USING PRT-W006PRT                                    
083700        IF PRT-IDLTERM = 'SAKNAS  '                                       
083800          MOVE NOO TO PRINT-SW                                            
083900          MOVE 'PRINTER MISSING IN W006PRT' TO MOD-TEMFSFEL               
084000        ELSE                                                              
084100          MOVE W-IDDC-B6         TO PARM-IDDC                             
084200          MOVE WS-ADLAGOMR       TO PARM-ADLAGOMR                         
084300          MOVE WS-ADGANG-FOM     TO PARM-ADGANG-FOM                       
084400          MOVE WS-ADGANG-TOM     TO PARM-ADGANG-TOM                       
084500          MOVE WS-ADSEC-FOM      TO PARM-ADSEC-FOM                        
084600          MOVE WS-ADSEC-TOM      TO PARM-ADSEC-TOM                        
084700          MOVE WS-ADLEVEL-FOM    TO PARM-ADLEVEL-FOM                      
084800          MOVE WS-ADLEVEL-TOM    TO PARM-ADLEVEL-TOM                      
084900          MOVE WS-KDLOC          TO PARM-KDLOC                            
085000          MOVE WS-KDFREQ         TO PARM-KDFREQ                           
085100          MOVE WS-TELOC          TO PARM-TELOC                            
085200          MOVE MID-KDPRT         TO PARM-KDPRT                            
085300          MOVE 'PRINT STARTED'   TO MOD-TEMFSFEL                          
085400        END-IF                                                            
085500     END-IF                                                               
085600     MOVE MFS-ERASE-FIELD        TO MOD-KDPRT                             
085700                                                                          
085800     EJECT                                                                
085900     .                                                                    
086000                                                                          
086100*    FLYTTAR PARAMETRAR TILL SOPRUTIN OCH STARTAR UPP                     
086200 GA-STARTA-JOB SECTION.                                                   
086300     MOVE '6317'       TO MSGSOP-IDTRANS                                  
086400     MOVE MFS-KDMFSFOR TO MSGSOP-KDMFSFOR                                 
086500     MOVE 'W612S5    ' TO MSGSOP-IDPROCESS                                
086600     MOVE 'O'          TO MSGSOP-KDSOPFUNK                                
086700     MOVE PARM-TESYMBV TO MSGSOP-TESYMBV                                  
086800     PERFORM IMS-INSERT-ALT-MSG                                           
086900                                                                          
087000     SKIP3                                                                
087100     .                                                                    
087200 MFS-INIT-KEY-FIELD-IN SECTION.                                           
087300                                                                          
087400*    --- ALL INPUT KEY FIELDS                                             
087500                                                                          
087600     MOVE MFS-ERASE-FIELD   TO MOD-IDDC-IN                                
087700                               MOD-ADLAGOMR-IN                            
087800                               MOD-ADGANG-FOM-IN                          
087900                               MOD-ADGANG-TOM-IN                          
088000                               MOD-ADSEC-FOM-IN                           
088100                               MOD-ADSEC-TOM-IN                           
088200                               MOD-ADLEVEL-FOM-IN                         
088300                               MOD-ADLEVEL-TOM-IN                         
088400                               MOD-KDLOC-IN                               
088500                               MOD-KDFREQ-IN                              
088600                               MOD-TELOC-IN                               
088700     .                                                                    
088800     EJECT                                                                
088900 MFS-INIT-KEY-FIELD-OUT SECTION.                                          
089000                                                                          
089100*    --- ALL OUTPUT KEY FIELDS                                            
089200                                                                          
089300     MOVE MSGI-IDDC         TO MOD-IDDC-UT                                
089400     MOVE MFS-ERASE-FIELD   TO MOD-ADLAGOMR-UT                            
089500                               MOD-ADGANG-FOM-UT                          
089600                               MOD-ADGANG-TOM-UT                          
089700                               MOD-ADSEC-FOM-UT                           
089800                               MOD-ADSEC-TOM-UT                           
089900                               MOD-ADLEVEL-FOM-UT                         
090000                               MOD-ADLEVEL-TOM-UT                         
090100                               MOD-KDLOC-UT                               
090200                               MOD-KDFREQ-UT                              
090300                               MOD-TELOC-UT                               
090400     .                                                                    
090500     EJECT                                                                
090600 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
090700                                                                          
090800*    --- OUTDATA-FIELD ON SCROLL KEYS                                     
090900                                                                          
091000     MOVE +1 TO INDX                                                      
091100     PERFORM UNTIL INDX > MAX-INDX                                        
091200      MOVE MFS-ERASE-FIELD     TO MOD-ADLAGOMR-LINE   (INDX)              
091300                                  MOD-ADGANG-LINE     (INDX)              
091400                                  MOD-ADPLATS-LINE    (INDX)              
091500                                  MOD-KDLOC-LINE      (INDX)              
091600                                  MOD-KDFREQ-LINE     (INDX)              
091700                                  MOD-KDSTOR-LINE     (INDX)              
091800                                  MOD-TELOC-LINE      (INDX)              
091900                                  MOD-IDARTNR-LINE    (INDX)              
092000      ADD +1 TO INDX                                                      
092100     END-PERFORM                                                          
092200     .                                                                    
092300     SKIP3                                                                
092400* --- IMS SECTIONS ---                                                    
092500     SKIP3                                                                
092600 IMS-GET-MSG SECTION.                                                     
092700                                                                          
092800     MOVE '  QC' TO GOOD-STATUSCODES                                      
092900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
093000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
093100     PERFORM IMS-STATUSCHECK                                              
093200     .                                                                    
093300     SKIP3                                                                
093400 IMS-INSERT-ALT-MSG SECTION.                                              
093500                                                                          
093600     MOVE SPACE TO GOOD-STATUSCODES                                       
093700     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
093800     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
093900     PERFORM IMS-STATUSCHECK                                              
094000     .                                                                    
094100     EJECT                                                                
094200 IMS-INSERT-MSG SECTION.                                                  
094300                                                                          
094400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
094500     MOVE SPACE TO GOOD-STATUSCODES                                       
094600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
094700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
094800     PERFORM IMS-STATUSCHECK                                              
094900     .                                                                    
095000     EJECT                                                                
095100 IMS-GU-LOCA SECTION.                                                     
095200                                                                          
095300     STRING 'WLLOCA01(WDJ801KY=>' W-WDJ8KY-MIN-X                          
095400                    '&WDJ801KY=<' W-WDJ8KY-MAX-X ')'                      
095500          DELIMITED BY SIZE INTO SSA1                                     
095600     MOVE '  GE' TO GOOD-STATUSCODES                                      
095700     CALL CBLTDLI USING GU LOCA-PCB LOC-WDJ801 SSA1                       
095800     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
095900     PERFORM IMS-STATUSCHECK                                              
096000     .                                                                    
096100     SKIP3                                                                
096200                                                                          
096300 IMS-GN-LOCA SECTION.                                                     
096400                                                                          
096500     STRING 'WLLOCA01(WDJ801KY=>' W-WDJ8KY-MIN-X                          
096600                    '&WDJ801KY=<' W-WDJ8KY-MAX-X ')'                      
096700          DELIMITED BY SIZE INTO SSA1                                     
096800     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
096900     CALL CBLTDLI USING GN LOCA-PCB LOC-WDJ801 SSA1                       
097000     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
097100     PERFORM IMS-STATUSCHECK                                              
097200     .                                                                    
097300     SKIP3                                                                
097400                                                                          
097500 IMS-GU-ARTR01 SECTION.                                                   
097600                                                                          
097700     STRING 'WLARTR01(WDK7A1KY=>' WDK7A1KY-MIN-X                          
097800                    '&WDK7A1KY=<' WDK7A1KY-MAX-X ')'                      
097900          DELIMITED BY SIZE INTO SSA1                                     
098000     MOVE '  GE' TO GOOD-STATUSCODES                                      
098100     CALL CBLTDLI USING GU ARTR-PCB SEQA-WDK7A1 SSA1                      
098200     MOVE ARTR-STATUS-CODE TO STATUS-WS                                   
098300     PERFORM IMS-STATUSCHECK                                              
098400     .                                                                    
098500     SKIP3                                                                
098600                                                                          
098700 IMS-GN-ARTR01 SECTION.                                                   
098800                                                                          
098900     STRING 'WLARTR01(WDK7A1KY=>' WDK7A1KY-MIN-X                          
099000                    '&WDK7A1KY=<' WDK7A1KY-MAX-X ')'                      
099100          DELIMITED BY SIZE INTO SSA1                                     
099200     MOVE '  GBGE' TO GOOD-STATUSCODES                                    
099300     CALL CBLTDLI USING GN ARTR-PCB SEQA-WDK7A1 SSA1                      
099400     MOVE ARTR-STATUS-CODE TO STATUS-WS                                   
099500     PERFORM IMS-STATUSCHECK                                              
099600     .                                                                    
099700     SKIP3                                                                
099800                                                                          
099900 IMS-GU-ARTD-ASEQ SECTION.                                                
100000                                                                          
100100     STRING 'WLARTD11(WDD8ASEQ =' W-WDD8ASEQ-X ')'                        
100200          DELIMITED BY SIZE INTO SSA1                                     
100300     MOVE '  GBGE' TO GOOD-STATUSCODES                                    
100400     CALL CBLTDLI USING GU  ARTD-PCB SALDO-WDD811 SSA1                    
100500     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
100600     PERFORM IMS-STATUSCHECK                                              
100700     .                                                                    
100800     EJECT                                                                
100900 IMS-GU-ARTD-ASEQ-B SECTION.                                              
101000                                                                          
101100     STRING 'WLARTR01(WDD8ASEQ=>' WDD8ASEQ-MIN-X                          
101200                    '&WDD8ASEQ=<' WDD8ASEQ-MAX-X ')'                      
101300          DELIMITED BY SIZE INTO SSA1                                     
101400     MOVE '  GBGE' TO GOOD-STATUSCODES                                    
101500     CALL CBLTDLI USING GU  ARTD-PCB SALDO-WDD811 SSA1                    
101600     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
101700     PERFORM IMS-STATUSCHECK                                              
101800     .                                                                    
101900     EJECT                                                                
102000 IMS-GN-ARTD-ASEQ-B SECTION.                                              
102100                                                                          
102200     STRING 'WLARTR01(WDD8ASEQ=>' WDD8ASEQ-MIN-X                          
102300                    '&WDD8ASEQ=<' WDD8ASEQ-MAX-X ')'                      
102400          DELIMITED BY SIZE INTO SSA1                                     
102500     MOVE '  GBGE' TO GOOD-STATUSCODES                                    
102600     CALL CBLTDLI USING GN  ARTD-PCB SALDO-WDD811 SSA1                    
102700     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
102800     PERFORM IMS-STATUSCHECK                                              
102900     .                                                                    
103000     EJECT                                                                
103100                                                                          
103200 IMS-GU-WDB601    SECTION.                                                
103300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
103400          DELIMITED BY SIZE INTO SSA1                                     
103500     MOVE '  GE' TO GOOD-STATUSCODES                                      
103600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
103700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
103800     PERFORM IMS-STATUSCHECK                                              
103900     .                                                                    
104000     EJECT                                                                
104100 IMS-STATUSCHECK SECTION.                                                 
104200                                                                          
104300     SET STATUS-IX TO 1                                                   
104400     SEARCH GOOD-STATUS                                                   
104500       AT END                                                             
104600         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
104700         DELIMITED BY SIZE INTO ERROR-TEXT                                
104800         CALL FELLOG                                                      
104900       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
105000         CONTINUE                                                         
105100     END-SEARCH                                                           
105200     .                                                                    
