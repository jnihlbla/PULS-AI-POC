000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6034700.                                                
000300 AUTHOR.         MARTIEN HOMPES.                                          
000400 DATE-WRITTEN.   97/04/03.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        LIST ALL LOCATIONS                                               
000900*                                                                         
001000*        THE PROGRAM READS     WLLOCA (WDJ8)                              
001100*        THE PROGRAM READS     WDK6E1 (WDK6E)                             
001200*        THE PROGRAM READS     WLARTD (WDD8A)                             
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSACTION: W6T347                                              
001600*        MID:         W6I34701                                            
001700*                                                                         
001800*    OUTDATA.                                                             
001900*        MOD:         W6O34701                                            
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700*    -- CHECKED BY WY2000                                                 
002800 77  IDPGM                       PIC X(08)   VALUE 'W6034700'.            
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
004100 77  MAX-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
004200*    --- INDEX FOR IDARTNR                                                
004300 77  TAB-INDX                    PIC 9(4)   VALUE ZERO.                   
004400 77  TAB-MAX-INDX                PIC 9(4)   VALUE ZERO.                   
004500 01  WORK-IDARTNR-TAB-GRP.                                                
004600     03 WORK-IDARTNR-TAB     PIC X(9)                                     
004700                             OCCURS 99.                                   
004800*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004900                                                                          
005000 77  WS-ADLAGOMR                 PIC 9(2)   VALUE ZERO.                   
005100 77  WS-ADGANG-FOM               PIC 9(2)   VALUE ZERO.                   
005200 77  WS-ADGANG-TOM               PIC 9(2)   VALUE ZERO.                   
005300 77  WS-ADGANG                   PIC 9(2)   VALUE ZERO.                   
005400 77  WS-ADSEC11-FOM              PIC 9(3)   VALUE ZERO.                   
005500 77  WS-ADSEC11-TOM              PIC 9(3)   VALUE ZERO.                   
005600 77  WS-ADSEC11                  PIC 9(3)   VALUE ZERO.                   
005700 77  WS-ADLEVEL11-FOM            PIC 9(1)   VALUE ZERO.                   
005800 77  WS-ADLEVEL11-TOM            PIC 9(1)   VALUE ZERO.                   
005900 77  WS-ADLEVEL11                PIC 9(1)   VALUE ZERO.                   
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
008500     88  OWN-MID                             VALUE '6347'.                
008600     88  GOOD-MID                            VALUE '6347'.                
008700     88  HELP-MID                            VALUE '0551'.                
008800     EJECT                                                                
008900*      --- VALID IDDC CODES                                               
009000*                                                                         
009100*01    -COPY WWDC99                                                       
009200       EJECT                                                              
009300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
009400 01  GENERAL-SUBPROGRAMS.                                                 
009500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009900     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
010000     EJECT                                                                
010100*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
010200*01 -COPY WMEDAREA                                                        
010300     SKIP3                                                                
010400*    --- PARAMETRAR TILL SUBPROGRAM W006PRT                               
010500*   -COPY W006PRT                                                         
010600     EJECT                                                                
010700 01  MESSAGE-CODES.                                                       
010800     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010900     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
011000     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
011100     03  ITEMS-MISSING           PIC X(3)    VALUE '029'.                 
011200     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
011300     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
011400     03  INF-LAST-PAGE-SHOWN     PIC X(3)    VALUE '115'.                 
011500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011600     03  WRONG-INTERVAL-INFO     PIC X(3)    VALUE '738'.                 
011700     EJECT                                                                
011800*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
011900*                                                                         
012000 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
012100     SKIP3                                                                
012200*01 -COPY WMSGINIT                                                        
012300     EJECT                                                                
012400*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
012500*                                                                         
012600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012700     SKIP3                                                                
012800*01  MID -COPY W6I34701                                                   
012900     EJECT                                                                
013000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
013100     SKIP3                                                                
013200*01  -COPY WMSGAREA                                                       
013300     EJECT                                                                
013400     03  MOD REDEFINES MSG-AREA.                                          
013500*      05  -COPY W6O34701                                                 
013600     EJECT                                                                
013700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013800     SKIP3                                                                
013900*01  -COPY WMFSAREA                                                       
014000     EJECT                                                                
014100*    --- AREA FÖR SOP ANROP                                               
014200 01  W-PROG-TO-PROG-SW.                                                   
014300*03 -COPY WMSGSOP                                                         
014400  SKIP3                                                                   
014500 01 PARM-TESYMBV.                                                         
014600     03 FILLER                   PIC X(2)    VALUE 'A('.                  
014700     03 PARM-IDDC                PIC X(2)    VALUE SPACE.                 
014800     03 FILLER                   PIC X(1)    VALUE ')'.                   
014900     03 FILLER                   PIC X(2)    VALUE 'B('.                  
015000     03 PARM-ADLAGOMR            PIC 9(2)    VALUE ZERO.                  
015100     03 FILLER                   PIC X(1)    VALUE ')'.                   
015200     03 FILLER                   PIC X(2)    VALUE 'C('.                  
015300     03 PARM-ADGANG-FOM          PIC 9(2)    VALUE ZERO.                  
015400     03 FILLER                   PIC X(1)    VALUE ')'.                   
015500     03 FILLER                   PIC X(2)    VALUE 'D('.                  
015600     03 PARM-ADGANG-TOM          PIC 9(2)    VALUE ZERO.                  
015700     03 FILLER                   PIC X(1)    VALUE ')'.                   
015800     03 FILLER                   PIC X(2)    VALUE 'E('.                  
015900     03 PARM-ADSEC11-FOM         PIC 9(3)    VALUE ZERO.                  
016000     03 FILLER                   PIC X(1)    VALUE ')'.                   
016100     03 FILLER                   PIC X(2)    VALUE 'F('.                  
016200     03 PARM-ADSEC11-TOM         PIC 9(3)    VALUE ZERO.                  
016300     03 FILLER                   PIC X(1)    VALUE ')'.                   
016400     03 FILLER                   PIC X(2)    VALUE 'G('.                  
016500     03 PARM-ADLEVEL11-FOM       PIC 9(1)    VALUE ZERO.                  
016600     03 FILLER                   PIC X(1)    VALUE ')'.                   
016700     03 FILLER                   PIC X(2)    VALUE 'H('.                  
016800     03 PARM-ADLEVEL11-TOM       PIC 9(1)    VALUE ZERO.                  
016900     03 FILLER                   PIC X(1)    VALUE ')'.                   
017000     03 FILLER                   PIC X(2)    VALUE 'I('.                  
017100     03 PARM-KDLOC               PIC X       VALUE SPACE.                 
017200     03 FILLER                   PIC X(1)    VALUE ')'.                   
017300     03 FILLER                   PIC X(2)    VALUE 'J('.                  
017400     03 PARM-KDFREQ              PIC 9(2)    VALUE ZERO.                  
017500     03 FILLER                   PIC X(1)    VALUE ')'.                   
017600     03 FILLER                   PIC X(2)    VALUE 'K('.                  
017700     03 PARM-TELOC               PIC X       VALUE SPACE.                 
017800     03 FILLER                   PIC X(1)    VALUE ')'.                   
017900     03 FILLER                   PIC X(2)    VALUE 'L('.                  
018000     03 PARM-KDPRT               PIC X(3)    VALUE SPACE.                 
018100     03 FILLER                   PIC X(1)    VALUE ')'.                   
018200     EJECT                                                                
018300 01  WORK-AREA.                                                           
018400     03 WORK-ADLAGOMR            PIC 9(2).                                
018500     03 WORK-ADGANG              PIC 9(2).                                
018600     03 WORK-ADPLATS.                                                     
018700        05 WORK-ADSEC11          PIC 9(3).                                
018800        05 WORK-ADLEVEL11        PIC 9(1).                                
018900        05 WORK-ADSEQ            PIC 9(1).                                
019000     03 WORK-TELOC               PIC X(1).                                
019100     03 WORK-IDARTNR             PIC X(9).                                
019200   03  WDD8ASEQ-WORK.                                                     
019300     05  W-IDDC-ASEQ-WORK        PIC X(2)           VALUE SPACE.          
019400     05  W-ADBUFFOMR-ASEQ-WORK   PIC S9(3)  COMP-3  VALUE ZERO.           
019500     05  W-ADBUFFGANG-ASEQ-WORK  PIC S9(3)  COMP-3  VALUE ZERO.           
019600     05  W-ADBUFFPL-ASEQ-WORK    PIC S9(5)  COMP-3  VALUE ZERO.           
019700     05  W-DABUFPAF-ASEQ-WORK    PIC  9(8)  COMP-3  VALUE ZERO.           
019800     05  W-IDARTNR-ASEQ-WORK     PIC S9(9)  COMP-3  VALUE ZERO.           
019900                                                                          
020000 01  SAVE-AREA.                                                           
020100     03 SAVE-IDTRANS             PIC X(4)    VALUE  SPACE.                
020200     03 SAVE-ADLAGOMR-ENTER      PIC 9(2).                                
020300     03 SAVE-ADGANG-ENTER        PIC 9(2).                                
020400     03 SAVE-ADPLATS-ENTER.                                               
020500        05 SAVE-ADSEC11-ENTER    PIC 9(3).                                
020600        05 SAVE-ADLEVEL11-ENTER  PIC 9(1).                                
020700        05 SAVE-ADSEQ-ENTER      PIC 9(1).                                
020710     03 SAVE-IDARTNR-ENTER       PIC 9(9).                                
020800     03 SAVE-ADLAGOMR-NEXT       PIC 9(2).                                
020900     03 SAVE-ADGANG-NEXT         PIC 9(2).                                
021000     03 SAVE-ADPLATS-NEXT.                                                
021100        05 SAVE-ADSEC11-NEXT     PIC 9(3).                                
021200        05 SAVE-ADLEVEL11-NEXT   PIC 9(1).                                
021300        05 SAVE-ADSEQ-NEXT       PIC 9(1).                                
021310     03 SAVE-IDARTNR-NEXT        PIC 9(9).                                
021400                                                                          
021500                                                                          
021600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
021700     SKIP3                                                                
021800 01  KEYS-TO-DLI.                                                         
021900                                                                          
022000     03  W-WDJ8KY-MIN-X.                                                  
022100         05  W-LOC-IDDC-MIN       PIC X(2).                               
022200         05  W-LOC-ADLAGOMR-MIN   PIC 9(2).                               
022300         05  W-LOC-ADGANG-MIN     PIC 9(2).                               
022400         05  W-LOC-ADPLATS-MIN.                                           
022500             07 W-LOC-ADSEC11-MIN PIC 9(3).                               
022600             07 W-LOC-ADLEVEL11-MIN PIC 9(1).                             
022700             07 W-LOC-ADSEQ-MIN   PIC 9(1).                               
022800                                                                          
022900     03  W-WDJ8KY-MAX-X.                                                  
023000         05  W-LOC-IDDC-MAX       PIC X(2).                               
023100         05  W-LOC-ADLAGOMR-MAX   PIC 9(2).                               
023200         05  W-LOC-ADGANG-MAX     PIC 9(2).                               
023300         05  W-LOC-ADPLATS-MAX.                                           
023400             07 W-LOC-ADSEC11-MAX PIC 9(3).                               
023500             07 W-LOC-ADLEVEL11-MAX PIC 9(1).                             
023600             07 W-LOC-ADSEQ-MAX   PIC 9(1).                               
023700                                                                          
023800     03  W-WDJ8KEY-X.                                                     
023900         05  W-LOC-IDDC          PIC X(2)    VALUE SPACE.                 
024000         05  W-LOC-ADLAGOMR      PIC 9(2)    VALUE ZERO.                  
024100         05  W-LOC-ADGANG        PIC 9(2)    VALUE ZERO.                  
024200         05  W-LOC-ADPLATS.                                               
024300             07 W-LOC-ADSEC11    PIC 9(3).                                
024400             07 W-LOC-ADLEVEL11  PIC 9(1).                                
024500             07 W-LOC-ADSEQ      PIC 9(1).                                
024600                                                                          
024700   03  WDK6E1KY-MIN-X.                                                    
024800     05  W-ADART-MIN.                                                     
024900       07  W-ADLAGOMR-MIN    PIC S9(3)  COMP-3  VALUE ZERO.               
025000       07  W-ADGANG-MIN      PIC S9(3)  COMP-3  VALUE ZERO.               
025100       07  W-ADPLATS-MIN     PIC S9(5)  COMP-3  VALUE ZERO.               
025200     05 W-IDARTNR-MIN        PIC S9(9)  COMP-3  VALUE ZERO.               
025300                                                                          
025400   03  WDK6E1KY-MAX-X.                                                    
025500     05  W-ADART-MAX.                                                     
025600       07  W-ADLAGOMR-MAX    PIC S9(3)  COMP-3  VALUE ZERO.               
025700       07  W-ADGANG-MAX      PIC S9(3)  COMP-3  VALUE ZERO.               
025800       07  W-ADPLATS-MAX     PIC S9(5)  COMP-3  VALUE ZERO.               
025900     05 W-IDARTNR-MAX     PIC S9(9)  COMP-3  VALUE 99999999.              
026000                                                                          
026100   03  W-WDD8ASEQ-X.                                                      
026200     05  W-IDDC-ASEQ         PIC X(2)           VALUE SPACE.              
026300     05  W-ADBUFFOMR-ASEQ    PIC S9(3)  COMP-3  VALUE ZERO.               
026400     05  W-ADBUFFGANG-ASEQ   PIC S9(3)  COMP-3  VALUE ZERO.               
026500     05  W-ADBUFFPL-ASEQ     PIC S9(5)  COMP-3  VALUE ZERO.               
026600                                                                          
026700   03  WDD8ASEQ-MIN-X.                                                    
026800     05  W-IDDC-ASEQ-MIN         PIC X(2)           VALUE SPACE.          
026900     05  W-ADBUFFOMR-ASEQ-MIN    PIC S9(3)  COMP-3  VALUE ZERO.           
027000     05  W-ADBUFFGANG-ASEQ-MIN   PIC S9(3)  COMP-3  VALUE ZERO.           
027100     05  W-ADBUFFPL-ASEQ-MIN     PIC S9(5)  COMP-3  VALUE ZERO.           
027200     05  W-DABUFPAF-ASEQ-MIN     PIC  9(8)  COMP-3  VALUE ZERO.           
027300     05  W-IDARTNR-ASEQ-MIN      PIC S9(9)  COMP-3  VALUE ZERO.           
027400                                                                          
027500   03  WDD8ASEQ-MAX-X.                                                    
027600     05  W-IDDC-ASEQ-MAX       PIC X(2)          VALUE SPACE.             
027700     05  W-ADBUFFOMR-ASEQ-MAX  PIC S9(3)  COMP-3 VALUE ZERO.              
027800     05  W-ADBUFFGANG-ASEQ-MAX PIC S9(3)  COMP-3 VALUE ZERO.              
027900     05  W-ADBUFFPL-ASEQ-MAX   PIC S9(5)  COMP-3 VALUE ZERO.              
028000     05  W-DABUFPAF-ASEQ-MAX   PIC  9(8)  COMP-3 VALUE 99999999.          
028100     05  W-IDARTNR-ASEQ-MAX    PIC S9(9)  COMP-3 VALUE 999999999.         
028200                                                                          
028300     SKIP2                                                                
028400*    --- STATUS-KOD FRÅN IMS                                              
028500 01  STATUS-WS                   PIC XX.                                  
028600     88  SEGMENT-FOUND                       VALUE '  '.                  
028700     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
028800     88  SEGMENT-MISSING                     VALUE 'GE'.                  
028900     88  END-OF-DATABASE                     VALUE 'GB'.                  
029000     SKIP2                                                                
029100 01  GOOD-STATUSCODES.                                                    
029200     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
029300     SKIP3                                                                
029400 01  SSA1                        PIC X(64).                               
029500 01  SSA2                        PIC X(64).                               
029600     EJECT                                                                
029700*    --- IMS FUNCTION CODES                                               
029800*01  -COPY W0003                                                          
029900     EJECT                                                                
030000*    ---  DLI INPUT-OUTPUT AREA                                           
030100                                                                          
030200 01  FILLER         PIC X(20) VALUE 'WLLOCA01-AREA'.                      
030300 01  DLI-IO-WLLOCA01.                                                     
030400*    03  -COPY WDJ801                                                     
030500                                                                          
030600 01  FILLER         PIC X(20) VALUE 'WLARTA01-AREA'.                      
030700 01  DLI-IO-WLARTA01.                                                     
030800*    03  -COPY WDK6E1                                                     
030900                                                                          
031000 01  FILLER         PIC X(20) VALUE 'WLARTD11-AREA'.                      
031100 01  DLI-IO-WLARTD11.                                                     
031200*    03  -COPY WDD811                                                     
031300                                                                          
031400     EJECT                                                                
031500 LINKAGE SECTION.                                                         
031600*01  -COPY W0009  -PRE MSG-                                               
031700*01  -COPY W0009  -PRE ALT-                                               
031800     EJECT                                                                
031900*01  -COPY W0008  -PRE USEA-                                              
032000     05  FILLER                  PIC X.                                   
032100     EJECT                                                                
032200*01  -COPY W0008  -PRE LOCA-                                              
032300     05  FILLER                  PIC X.                                   
032400     EJECT                                                                
032500*01  -COPY W0008  -PRE ARTR-                                              
032600     05  FILLER                  PIC X.                                   
032700     EJECT                                                                
032800*01  -COPY W0008  -PRE ARTD-                                              
032900     05  FILLER                  PIC X.                                   
033000     EJECT                                                                
033100 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB                                
033200           USEA-PCB                                                       
033300           LOCA-PCB ARTR-PCB ARTD-PCB.                                    
033400 MAIN SECTION.                                                            
033500     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB                                
033600            USEA-PCB                                                      
033700            LOCA-PCB ARTR-PCB ARTD-PCB.                                   
033800                                                                          
033900     PERFORM IMS-GET-MSG                                                  
034000     IF SEGMENT-FOUND                                                     
034100       PERFORM A-INIT                                                     
034200       IF GOOD-MID OR HELP-MID                                            
034300          PERFORM B-CHECK-KEYS                                            
034400          IF KEYS-OK                                                      
034500             IF MFS-PRINT                                                 
034600                PERFORM G-CHECK-PRINT                                     
034700                IF PRINT-OK                                               
034800                   PERFORM GA-STARTA-JOB                                  
034900                END-IF                                                    
035000             END-IF                                                       
035100             IF MFS-FIRST                                                 
035200                PERFORM C-FIRST-PAGE                                      
035300              ELSE                                                        
035400                IF MFS-NEXT                                               
035500                   PERFORM D-NEXT-PAGE                                    
035600                ELSE                                                      
035700                   PERFORM E-SAME-PAGE                                    
035800                END-IF                                                    
035900             END-IF                                                       
036000             PERFORM F-READ-SHOW-INFO                                     
036100          END-IF                                                          
036200       END-IF                                                             
036300       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O34701 + 4                      
036400       PERFORM IMS-INSERT-MSG                                             
036500     END-IF                                                               
036600                                                                          
036700     MOVE ZERO TO RETURN-CODE                                             
036800     GOBACK                                                               
036900     .                                                                    
037000     EJECT                                                                
037100 A-INIT SECTION.                                                          
037200                                                                          
037300     IF MSG-DOUBLE-TRANSACTIONS                                           
037400       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W6I34701                 
037500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
037600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
037700     ELSE                                                                 
037800       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W6I34701                  
037900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
038000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
038100     END-IF                                                               
038200                                                                          
038300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
038400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
038500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
038600                                                                          
038700     MOVE LOW-VALUE TO MSG-AREA                                           
038800     MOVE 'W6O347N1' TO MFS-IDMOD                                         
038900     MOVE '6347' TO MOD-IDTRANS                                           
039000     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
039100                                                                          
039200                                                                          
039300     MOVE ALL '+'           TO MSGI-WMSGINIT                              
039400     MOVE '001'             TO MSGI-KDCALL                                
039500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
039600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
039700     MOVE '6347'            TO MSGI-IDTRANS                               
039800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
039900                                                                          
040000     MOVE MSGI-SPAR-AREA    TO SAVE-AREA                                  
040100     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
040200     MOVE MSGI-IDDC         TO WS-IDDC                                    
040300                                                                          
040400     IF GOOD-MID OR HELP-MID                                              
040500       CONTINUE                                                           
040600     ELSE                                                                 
040700       MOVE SPACE TO MFS-KDTRTYP                                          
040800       MOVE '7' TO MFS-IDPFK                                              
040900       PERFORM MFS-INIT-KEY-FIELD-IN                                      
041000       PERFORM MFS-INIT-KEY-FIELD-OUT                                     
041100       PERFORM MFS-ERASE-LINE-FIELD-OUT                                   
041200     END-IF                                                               
041300     .                                                                    
041400     EJECT                                                                
041500 B-CHECK-KEYS SECTION.                                                    
041600                                                                          
041700                                                                          
041800     MOVE YES               TO KEYS-SW                                    
041900                                                                          
042000     PERFORM MFS-INIT-KEY-FIELD-IN                                        
042100                                                                          
042200*                                                                         
042300*    -- CONTROL  ON WAREHOUSE                                             
042400*                                                                         
042500     IF CDC                                                               
042600       CONTINUE                                                           
042700     ELSE                                                                 
042800       MOVE NOO                 TO KEYS-SW                                
042900     END-IF                                                               
043000*                                                                         
043100*    -- CONTROL  AREA                                                     
043200*                                                                         
043300     IF MID-ADLAGOMR-IN = ALL '+'                                         
043400       INSPECT MID-ADLAGOMR-UT REPLACING LEADING SPACE BY ZERO            
043500       MOVE MID-ADLAGOMR-UT    TO WS-ADLAGOMR                             
043600     ELSE                                                                 
043700       IF MID-ADLAGOMR-IN NUMERIC                                         
043800          MOVE '7' TO MFS-IDPFK                                           
043900          MOVE    SPACE        TO MFS-KDTRTYP                             
044000          MOVE MID-ADLAGOMR-IN TO WS-ADLAGOMR                             
044100       ELSE                                                               
044200          MOVE NOO             TO KEYS-SW                                 
044300       END-IF                                                             
044400     END-IF                                                               
044500*                                                                         
044600*    -- CONTROL  AISLE FROM                                               
044700*                                                                         
044800     IF MID-ADGANG-FOM-IN = ALL '+'                                       
044900       INSPECT MID-ADGANG-FOM-UT REPLACING LEADING SPACE BY ZERO          
045000       MOVE MID-ADGANG-FOM-UT    TO WS-ADGANG-FOM                         
045100     ELSE                                                                 
045200       IF MID-ADGANG-FOM-IN NUMERIC                                       
045300          MOVE '7' TO MFS-IDPFK                                           
045400          MOVE    SPACE          TO MFS-KDTRTYP                           
045500          MOVE MID-ADGANG-FOM-IN TO WS-ADGANG-FOM                         
045600       ELSE                                                               
045700          MOVE NOO               TO KEYS-SW                               
045800       END-IF                                                             
045900     END-IF                                                               
046000*                                                                         
046100*    -- CONTROL  AISLE THRU                                               
046200*                                                                         
046300     IF MID-ADGANG-TOM-IN = ALL '+'                                       
046400       INSPECT MID-ADGANG-TOM-UT REPLACING LEADING SPACE BY ZERO          
046500       MOVE MID-ADGANG-TOM-UT    TO WS-ADGANG-TOM                         
046600     ELSE                                                                 
046700       IF MID-ADGANG-TOM-IN NUMERIC                                       
046800          MOVE '7' TO MFS-IDPFK                                           
046900          MOVE    SPACE          TO MFS-KDTRTYP                           
047000          MOVE MID-ADGANG-TOM-IN TO WS-ADGANG-TOM                         
047100       ELSE                                                               
047200          MOVE NOO               TO KEYS-SW                               
047300       END-IF                                                             
047400     END-IF                                                               
047500*                                                                         
047600*    -- CONTROL  ADSEC11 FROM                                             
047700*                                                                         
047800     IF MID-ADSEC11-FOM-IN = ALL '+'                                      
047900       INSPECT MID-ADSEC11-FOM-UT REPLACING LEADING SPACE BY ZERO         
048000       MOVE MID-ADSEC11-FOM-UT  TO WS-ADSEC11-FOM                         
048100     ELSE                                                                 
048200       IF MID-ADSEC11-FOM-IN NUMERIC                                      
048300          MOVE '7' TO MFS-IDPFK                                           
048400          MOVE    SPACE          TO MFS-KDTRTYP                           
048500          MOVE MID-ADSEC11-FOM-IN TO WS-ADSEC11-FOM                       
048600       ELSE                                                               
048700          MOVE NOO               TO KEYS-SW                               
048800       END-IF                                                             
048900     END-IF                                                               
049000*                                                                         
049100*    -- CONTROL  ADSEC11 THRU                                             
049200*                                                                         
049300     IF MID-ADSEC11-TOM-IN = ALL '+'                                      
049400       INSPECT MID-ADSEC11-TOM-UT REPLACING LEADING SPACE BY ZERO         
049500       MOVE MID-ADSEC11-TOM-UT  TO WS-ADSEC11-TOM                         
049600     ELSE                                                                 
049700       IF MID-ADSEC11-TOM-IN NUMERIC                                      
049800          MOVE '7' TO MFS-IDPFK                                           
049900          MOVE    SPACE          TO MFS-KDTRTYP                           
050000          MOVE MID-ADSEC11-TOM-IN TO WS-ADSEC11-TOM                       
050100       ELSE                                                               
050200          MOVE NOO               TO KEYS-SW                               
050300       END-IF                                                             
050400     END-IF                                                               
050500*                                                                         
050600*    -- CONTROL  LEVEL FROM                                               
050700*                                                                         
050800     IF MID-ADLEVEL11-FOM-IN = ALL '+'                                    
050900      INSPECT MID-ADLEVEL11-FOM-UT REPLACING LEADING SPACE BY ZERO        
051000       MOVE MID-ADLEVEL11-FOM-UT  TO WS-ADLEVEL11-FOM                     
051100     ELSE                                                                 
051200       IF MID-ADLEVEL11-FOM-IN NUMERIC                                    
051300          MOVE '7' TO MFS-IDPFK                                           
051400          MOVE    SPACE           TO MFS-KDTRTYP                          
051500          MOVE MID-ADLEVEL11-FOM-IN TO WS-ADLEVEL11-FOM                   
051600       ELSE                                                               
051700          MOVE NOO                TO KEYS-SW                              
051800       END-IF                                                             
051900     END-IF                                                               
052000                                                                          
052100*                                                                         
052200*    -- CONTROL  LEVEL THRU                                               
052300*                                                                         
052400     IF MID-ADLEVEL11-TOM-IN = ALL '+'                                    
052500      INSPECT MID-ADLEVEL11-TOM-UT REPLACING LEADING SPACE BY ZERO        
052600       MOVE MID-ADLEVEL11-TOM-UT  TO WS-ADLEVEL11-TOM                     
052700     ELSE                                                                 
052800       IF MID-ADLEVEL11-TOM-IN NUMERIC                                    
052900          MOVE '7' TO MFS-IDPFK                                           
053000          MOVE    SPACE           TO MFS-KDTRTYP                          
053100          MOVE MID-ADLEVEL11-TOM-IN TO WS-ADLEVEL11-TOM                   
053200       ELSE                                                               
053300          MOVE NOO                TO KEYS-SW                              
053400       END-IF                                                             
053500     END-IF                                                               
053600*                                                                         
053700*    -- CONTROL  TYPE OF LOCATION                                         
053800*                                                                         
053900     IF MID-KDLOC-IN  = ALL '+'                                           
054000        MOVE MID-KDLOC-UT          TO WS-KDLOC                            
054100      ELSE                                                                
054200        IF MID-KDLOC-IN  =  PRIME OR BUFFER OR MIXED                      
054300           MOVE '7'                TO MFS-IDPFK                           
054400           MOVE SPACE              TO MFS-KDTRTYP                         
054500           MOVE MID-KDLOC-IN       TO WS-KDLOC                            
054600         ELSE                                                             
054700           MOVE NOO                 TO KEYS-SW                            
054800        END-IF                                                            
054900     END-IF                                                               
055000*                                                                         
055100*    -- CONTROL  FREQUENCY TYPE                                           
055200*                                                                         
055300     IF MID-KDFREQ-IN = ALL '+'                                           
055400        MOVE MID-KDFREQ-UT          TO WS-KDFREQ                          
055500     ELSE                                                                 
055600       IF MID-KDFREQ-IN NUMERIC                                           
055700          MOVE '7'                  TO MFS-IDPFK                          
055800          MOVE SPACE                TO MFS-KDTRTYP                        
055900          MOVE MID-KDFREQ-IN        TO WS-KDFREQ                          
056000       ELSE                                                               
056100          MOVE NOO                  TO KEYS-SW                            
056200       END-IF                                                             
056300     END-IF                                                               
056400*                                                                         
056500*    -- FILL IN REMARK FIELD INPUT                                        
056600*                                                                         
056700     IF MID-TELOC-IN  = ALL '+'                                           
056800        MOVE MID-TELOC-UT         TO WS-TELOC                             
056900      ELSE                                                                
057000        MOVE '7'                  TO MFS-IDPFK                            
057100        MOVE SPACE                TO MFS-KDTRTYP                          
057200        MOVE MID-TELOC-IN         TO WS-TELOC                             
057300     END-IF                                                               
057400*                                                                         
057500*    -- FILL MOD KEY-OUTPUT FIELDS                                        
057600*                                                                         
057700     MOVE WS-IDDC        TO  MOD-IDDC-UT                                  
057800     MOVE WS-ADLAGOMR    TO  MOD-ADLAGOMR-UT                              
057900     MOVE WS-ADGANG-FOM  TO  MOD-ADGANG-FOM-UT                            
058000     MOVE WS-ADGANG-TOM  TO  MOD-ADGANG-TOM-UT                            
058100     MOVE WS-ADSEC11-FOM TO  MOD-ADSEC11-FOM-UT                           
058200     MOVE WS-ADSEC11-TOM TO  MOD-ADSEC11-TOM-UT                           
058300     MOVE WS-ADLEVEL11-FOM TO MOD-ADLEVEL11-FOM-UT                        
058400     MOVE WS-ADLEVEL11-TOM TO MOD-ADLEVEL11-TOM-UT                        
058500     MOVE WS-KDLOC       TO  MOD-KDLOC-UT                                 
058600     MOVE WS-KDFREQ      TO  MOD-KDFREQ-UT                                
058700     MOVE WS-TELOC       TO  MOD-TELOC-UT                                 
058800                                                                          
058900                                                                          
059000     IF KEYS-WRONG                                                        
059100       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
059200       CALL WMEDKONV USING MED-WMEDAREA                                   
059300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
059400      ELSE                                                                
059500       IF WS-ADGANG-FOM   >  WS-ADGANG-TOM  OR                            
059600          WS-ADSEC11-FOM  >  WS-ADSEC11-TOM OR                            
059700          WS-ADLEVEL11-FOM > WS-ADLEVEL11-TOM                             
059800                                                                          
059900          MOVE WRONG-INTERVAL-INFO TO MED-IDMFSFEL                        
060000          CALL WMEDKONV USING MED-WMEDAREA                                
060100          MOVE MED-MFSFEL          TO MOD-TEMFSFEL                        
060200          MOVE NOO                 TO KEYS-SW                             
060300       END-IF                                                             
060400     END-IF                                                               
060500                                                                          
060600     .                                                                    
060700     EJECT                                                                
060800 C-FIRST-PAGE SECTION.                                                    
060900                                                                          
061000                                                                          
061100*                                                                         
061200*    -- FILL START KEY FIELDS                                             
061300*                                                                         
061400     MOVE WS-IDDC               TO W-LOC-IDDC-MIN                         
061500     MOVE WS-ADLAGOMR           TO W-LOC-ADLAGOMR-MIN                     
061600     MOVE WS-ADGANG-FOM         TO W-LOC-ADGANG-MIN                       
061700     MOVE WS-ADSEC11-FOM        TO W-LOC-ADSEC11-MIN                      
061800     MOVE WS-ADLEVEL11-FOM      TO W-LOC-ADLEVEL11-MIN                    
061900     MOVE ZEROES                TO W-LOC-ADSEQ-MIN                        
061910     MOVE ZEROES                TO W-IDARTNR-MIN                          
062000                                                                          
062100     .                                                                    
062200     EJECT                                                                
062300 E-SAME-PAGE SECTION.                                                     
062400                                                                          
062500*                                                                         
062600*    -- FILL START KEY FIELDS                                             
062700*                                                                         
062800     MOVE WS-IDDC               TO W-LOC-IDDC-MIN                         
062900     MOVE SAVE-ADLAGOMR-ENTER   TO W-LOC-ADLAGOMR-MIN                     
063000     MOVE SAVE-ADGANG-ENTER     TO W-LOC-ADGANG-MIN                       
063100     MOVE SAVE-ADSEC11-ENTER    TO W-LOC-ADSEC11-MIN                      
063200     MOVE SAVE-ADLEVEL11-ENTER  TO W-LOC-ADLEVEL11-MIN                    
063300     MOVE SAVE-ADSEQ-ENTER      TO W-LOC-ADSEQ-MIN                        
063310     MOVE SAVE-IDARTNR-ENTER    TO W-IDARTNR-MIN                          
063400     .                                                                    
063500     EJECT                                                                
063600                                                                          
063700 D-NEXT-PAGE SECTION.                                                     
063800                                                                          
063900*                                                                         
064000*    -- FILL START KEY FIELDS                                             
064100*                                                                         
064200     MOVE WS-IDDC               TO W-LOC-IDDC-MIN                         
064300     MOVE SAVE-ADLAGOMR-NEXT    TO W-LOC-ADLAGOMR-MIN                     
064400     MOVE SAVE-ADGANG-NEXT      TO W-LOC-ADGANG-MIN                       
064500     MOVE SAVE-ADSEC11-NEXT     TO W-LOC-ADSEC11-MIN                      
064600     MOVE SAVE-ADLEVEL11-NEXT   TO W-LOC-ADLEVEL11-MIN                    
064700     MOVE SAVE-ADSEQ-NEXT       TO W-LOC-ADSEQ-MIN                        
064710     MOVE SAVE-IDARTNR-NEXT     TO W-IDARTNR-MIN                          
064800     .                                                                    
064900     EJECT                                                                
065000                                                                          
065100 F-READ-SHOW-INFO SECTION.                                                
065200                                                                          
065300*                                                                         
065400*    -- FILL FINISH (MAX) KEY FIELDS                                      
065500*                                                                         
065600     MOVE WS-IDDC               TO W-LOC-IDDC-MAX                         
065700     MOVE WS-ADLAGOMR           TO W-LOC-ADLAGOMR-MAX                     
065800     MOVE WS-ADGANG-TOM         TO W-LOC-ADGANG-MAX                       
065900     MOVE WS-ADSEC11-TOM        TO W-LOC-ADSEC11-MAX                      
066000     MOVE WS-ADLEVEL11-TOM      TO W-LOC-ADLEVEL11-MAX                    
066100     MOVE 9                     TO W-LOC-ADSEQ-MAX                        
066200                                                                          
066300     MOVE 99999999              TO W-IDARTNR-MAX                          
066400                                                                          
066500     PERFORM  IMS-GU-LOCA                                                 
066600                                                                          
066700     IF SEGMENT-MISSING                                                   
066800        IF MFS-NEXT                                                       
066900           MOVE INF-LAST-PAGE-SHOWN TO    MED-IDMFSFEL                    
067000         ELSE                                                             
067100           MOVE ITEMS-MISSING       TO    MED-IDMFSFEL                    
067200        END-IF                                                            
067300        CALL    WMEDKONV    USING MED-WMEDAREA                            
067400        MOVE MED-MFSFEL     TO MOD-TEMFSFEL                               
067500                                                                          
067600        PERFORM MFS-ERASE-LINE-FIELD-OUT                                  
067700                                                                          
067800        MOVE WS-ADLAGOMR    TO SAVE-ADLAGOMR-ENTER                        
067900        MOVE WS-ADGANG-FOM  TO SAVE-ADGANG-ENTER                          
068000        MOVE WS-ADSEC11-FOM TO SAVE-ADSEC11-ENTER                         
068100        MOVE WS-ADLEVEL11-FOM TO SAVE-ADLEVEL11-ENTER                     
068200        MOVE ZEROES         TO SAVE-ADSEQ-ENTER                           
068210        MOVE ZEROES         TO SAVE-IDARTNR-ENTER                         
068300                                                                          
068400        MOVE WS-ADLAGOMR    TO SAVE-ADLAGOMR-NEXT                         
068500        MOVE WS-ADGANG-FOM  TO SAVE-ADGANG-NEXT                           
068600        MOVE WS-ADSEC11-FOM TO SAVE-ADSEC11-NEXT                          
068700        MOVE WS-ADLEVEL11-FOM TO SAVE-ADLEVEL11-NEXT                      
068800        MOVE ZEROES         TO SAVE-ADSEQ-NEXT                            
068810        MOVE ZEROES         TO SAVE-IDARTNR-NEXT                          
068900                                                                          
069000      ELSE                                                                
069100        MOVE LOC-ADLAGOMR   TO SAVE-ADLAGOMR-ENTER                        
069200        MOVE LOC-ADGANG     TO SAVE-ADGANG-ENTER                          
069300        MOVE LOC-ADPLATS    TO SAVE-ADPLATS-ENTER                         
069400        MOVE +1             TO INDX                                       
069500        PERFORM UNTIL SEGMENT-MISSING OR END-OF-DATABASE OR               
069600                      INDX > MAX-INDX                                     
069700         MOVE LOC-ADPLATS         TO WORK-ADPLATS                         
069800         MOVE LOC-TELOC           TO WORK-TELOC                           
069900         MOVE SPACES              TO WORK-IDARTNR                         
070000         IF WS-ADLEVEL11-FOM <= WORK-ADLEVEL11          AND               
070100            WS-ADLEVEL11-TOM >= WORK-ADLEVEL11          AND               
070200            WS-ADSEC11-FOM <= WORK-ADSEC11              AND               
070300            WS-ADSEC11-TOM >= WORK-ADSEC11              AND               
070400            (WS-KDLOC  = " " OR  WS-KDLOC = LOC-KDLOC)  AND               
070500            (WS-KDFREQ = 0   OR WS-KDFREQ = LOC-KDFREQ) AND               
070600            (WS-TELOC  = " " OR WS-TELOC  = WORK-TELOC)                   
070700*           MOVE LOC-IDDC               TO W-IDDC-MIN                     
070800*                                       W-IDDC-MAX                        
070900            MOVE LOC-ADLAGOMR           TO W-ADLAGOMR-MIN                 
071000                                        W-ADLAGOMR-MAX                    
071100            MOVE LOC-ADGANG             TO W-ADGANG-MIN                   
071200                                        W-ADGANG-MAX                      
071300            MOVE LOC-ADPLATS            TO W-ADPLATS-MIN                  
071400                                        W-ADPLATS-MAX                     
071500            PERFORM     IMS-GU-WDK6E1                                     
071600            IF LOC-KDLOC     =  PRIME OR MIXED                            
071700               PERFORM UNTIL SEGMENT-MISSING OR                           
071800                             END-OF-DATABASE OR                           
071900                             TAB-INDX > 98                                
072000                  ADD 1 TO TAB-INDX                                       
072100                           TAB-MAX-INDX                                   
072200                  MOVE SEQE-IDARTNR                                       
072300                                 TO WORK-IDARTNR-TAB (TAB-INDX)           
072400                  INSPECT WORK-IDARTNR-TAB (TAB-INDX)                     
072500                   REPLACING LEADING ZEROES BY SPACE                      
072600                  PERFORM IMS-GN-WDK6E1                                   
072700               END-PERFORM                                                
072800            END-IF                                                        
072900                                                                          
073000            IF LOC-KDLOC  =  (BUFFER OR MIXED) AND                        
073100               TAB-MAX-INDX = ZERO                                        
073200                                                                          
073300               MOVE LOC-IDDC            TO W-IDDC-ASEQ                    
073400               MOVE LOC-ADLAGOMR        TO W-ADBUFFOMR-ASEQ               
073500               MOVE LOC-ADGANG          TO W-ADBUFFGANG-ASEQ              
073600               MOVE LOC-ADPLATS         TO W-ADBUFFPL-ASEQ                
073700                                                                          
073800               PERFORM  IMS-GU-ARTD-ASEQ                                  
073900                                                                          
074000               IF SEGMENT-FOUND                                           
074100                  MOVE SEQE-IDARTNR     TO WORK-IDARTNR                   
074200                  INSPECT WORK-IDARTNR REPLACING LEADING                  
074300                     ZEROES BY SPACE                                      
074400               END-IF                                                     
074500            END-IF                                                        
074600            IF TAB-MAX-INDX = ZERO                                        
074700              MOVE LOC-ADLAGOMR      TO MOD-ADLAGOMR-LINE (INDX)          
074800              MOVE LOC-ADGANG        TO MOD-ADGANG-LINE   (INDX)          
074900              MOVE LOC-ADPLATS       TO MOD-ADPLATS-LINE  (INDX)          
075000              MOVE LOC-KDLOC         TO MOD-KDLOC-LINE    (INDX)          
075100              MOVE LOC-KDFREQ        TO MOD-KDFREQ-LINE   (INDX)          
075200              MOVE LOC-KDSTOR        TO MOD-KDSTOR-LINE   (INDX)          
075300              MOVE LOC-TELOC         TO MOD-TELOC-LINE    (INDX)          
075400              MOVE WORK-IDARTNR      TO MOD-IDARTNR-LINE  (INDX)          
075500              ADD 1 TO INDX                                               
075600            ELSE                                                          
075700              MOVE LOC-ADLAGOMR      TO MOD-ADLAGOMR-LINE (INDX)          
075800              MOVE LOC-ADGANG        TO MOD-ADGANG-LINE   (INDX)          
075900              MOVE LOC-ADPLATS       TO MOD-ADPLATS-LINE  (INDX)          
076000              MOVE LOC-KDLOC         TO MOD-KDLOC-LINE    (INDX)          
076100              MOVE LOC-KDFREQ        TO MOD-KDFREQ-LINE   (INDX)          
076200              MOVE LOC-KDSTOR        TO MOD-KDSTOR-LINE   (INDX)          
076300              MOVE LOC-TELOC         TO MOD-TELOC-LINE    (INDX)          
076310              MOVE WORK-IDARTNR-TAB (1)                                   
076320                                     TO SAVE-IDARTNR-ENTER                
076400              MOVE 1  TO TAB-INDX                                         
076500              PERFORM UNTIL TAB-INDX > TAB-MAX-INDX                       
076600                         OR INDX > MAX-INDX                               
076700                MOVE WORK-IDARTNR-TAB (TAB-INDX)                          
076800                                     TO MOD-IDARTNR-LINE  (INDX)          
076900                ADD 1 TO TAB-INDX                                         
077000                         INDX                                             
077100              END-PERFORM                                                 
077400                                                                          
077500            END-IF                                                        
077600         END-IF                                                           
077602         IF TAB-MAX-INDX = ZERO OR                                        
077603            (TAB-MAX-INDX > ZERO AND                                      
077604             TAB-INDX > TAB-MAX-INDX)                                     
077605           PERFORM IMS-GN-LOCA                                            
077606           MOVE ZERO                 TO TAB-INDX                          
077607                                        TAB-MAX-INDX                      
077611         END-IF                                                           
077800        END-PERFORM                                                       
077900        IF SEGMENT-FOUND OR                                               
077910           (TAB-INDX <= TAB-MAX-INDX AND                                  
077920            TAB-MAX-INDX > ZERO)                                          
078000           MOVE LOC-ADLAGOMR   TO SAVE-ADLAGOMR-NEXT                      
078100           MOVE LOC-ADGANG     TO SAVE-ADGANG-NEXT                        
078200           MOVE LOC-ADPLATS    TO SAVE-ADPLATS-NEXT                       
078210           IF TAB-INDX <= TAB-MAX-INDX AND TAB-INDX > ZERO                
078221             MOVE WORK-IDARTNR-TAB (TAB-INDX)                             
078230                               TO SAVE-IDARTNR-NEXT                       
078240           ELSE                                                           
078250             MOVE ZERO         TO SAVE-IDARTNR-NEXT                       
078260           END-IF                                                         
078300           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
078400           CALL WMEDKONV USING MED-WMEDAREA                               
078500           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
078600         ELSE                                                             
078700           MOVE LOC-ADLAGOMR        TO    SAVE-ADLAGOMR-NEXT              
078800           MOVE LOC-ADGANG          TO    SAVE-ADGANG-NEXT                
078900           MOVE LOC-ADPLATS         TO    SAVE-ADPLATS-NEXT               
079000           MOVE INF-LAST-PAGE-SHOWN TO    MED-IDMFSFEL                    
079100           CALL WMEDKONV            USING MED-WMEDAREA                    
079200           MOVE MED-MFSFEL          TO    MOD-TEMFSFEL                    
079300           PERFORM UNTIL INDX > MAX-INDX                                  
079400             MOVE MFS-ERASE-FIELD   TO  MOD-ADLAGOMR-LINE (INDX)          
079500                                        MOD-ADGANG-LINE   (INDX)          
079600                                        MOD-ADPLATS-LINE  (INDX)          
079700                                        MOD-KDLOC-LINE    (INDX)          
079800                                        MOD-KDFREQ-LINE   (INDX)          
079900                                        MOD-KDSTOR-LINE   (INDX)          
080000                                        MOD-TELOC-LINE    (INDX)          
080100                                        MOD-IDARTNR-LINE  (INDX)          
080200             ADD 1 TO INDX                                                
080300           END-PERFORM                                                    
080400         END-IF                                                           
080500                                                                          
080600       MOVE '002'      TO MSGI-KDCALL                                     
080700       MOVE '6347'     TO SAVE-IDTRANS                                    
080800       MOVE SAVE-AREA  TO MSGI-SPAR-AREA                                  
080900       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
081000     END-IF                                                               
081100     .                                                                    
081200     EJECT                                                                
081300 G-CHECK-PRINT SECTION.                                                   
081400     IF MID-KDPRT = ALL '+'                                               
081500        MOVE NOO TO PRINT-SW                                              
081600        MOVE 'PRINT CODE WRONG'  TO MOD-TEMFSFEL                          
081700     ELSE                                                                 
081800        MOVE '6L'      TO PRT-IDPRTLST(1:2)                               
081900        MOVE MID-KDPRT TO PRT-IDPRTLST(3:3)                               
082000        MOVE SPACE     TO PRT-IDPRTLST(6:3)                               
082100        MOVE 1                 TO PRT-KDCALL                              
082200        CALL W006PRT USING PRT-W006PRT                                    
082300        IF PRT-IDLTERM = 'SAKNAS  '                                       
082400          MOVE NOO TO PRINT-SW                                            
082500          MOVE 'PRINTER MISSING IN W006PRT' TO MOD-TEMFSFEL               
082600        ELSE                                                              
082700          MOVE WS-IDDC           TO PARM-IDDC                             
082800          MOVE WS-ADLAGOMR       TO PARM-ADLAGOMR                         
082900          MOVE WS-ADGANG-FOM     TO PARM-ADGANG-FOM                       
083000          MOVE WS-ADGANG-TOM     TO PARM-ADGANG-TOM                       
083100          MOVE WS-ADSEC11-FOM    TO PARM-ADSEC11-FOM                      
083200          MOVE WS-ADSEC11-TOM    TO PARM-ADSEC11-TOM                      
083300          MOVE WS-ADLEVEL11-FOM  TO PARM-ADLEVEL11-FOM                    
083400          MOVE WS-ADLEVEL11-TOM  TO PARM-ADLEVEL11-TOM                    
083500          MOVE WS-KDLOC          TO PARM-KDLOC                            
083600          MOVE WS-KDFREQ         TO PARM-KDFREQ                           
083700          MOVE WS-TELOC          TO PARM-TELOC                            
083800          MOVE MID-KDPRT         TO PARM-KDPRT                            
083900          MOVE 'PRINT STARTED'   TO MOD-TEMFSFEL                          
084000        END-IF                                                            
084100     END-IF                                                               
084200     MOVE MFS-ERASE-FIELD        TO MOD-KDPRT                             
084300                                                                          
084400     EJECT                                                                
084500     .                                                                    
084600                                                                          
084700*    FLYTTAR PARAMETRAR TILL SOPRUTIN OCH STARTAR UPP                     
084800 GA-STARTA-JOB SECTION.                                                   
084900     MOVE '6347'       TO MSGSOP-IDTRANS                                  
085000     MOVE MFS-KDMFSFOR TO MSGSOP-KDMFSFOR                                 
085100     MOVE 'W615S2    ' TO MSGSOP-IDPROCESS                                
085200     MOVE 'O'          TO MSGSOP-KDSOPFUNK                                
085300     MOVE PARM-TESYMBV TO MSGSOP-TESYMBV                                  
085400     PERFORM IMS-INSERT-ALT-MSG                                           
085500                                                                          
085600     SKIP3                                                                
085700     .                                                                    
085800 MFS-INIT-KEY-FIELD-IN SECTION.                                           
085900                                                                          
086000*    --- ALL INPUT KEY FIELDS                                             
086100                                                                          
086200     MOVE MFS-ERASE-FIELD   TO MOD-IDDC-IN                                
086300                               MOD-ADLAGOMR-IN                            
086400                               MOD-ADGANG-FOM-IN                          
086500                               MOD-ADGANG-TOM-IN                          
086600                               MOD-ADSEC11-FOM-IN                         
086700                               MOD-ADSEC11-TOM-IN                         
086800                               MOD-ADLEVEL11-FOM-IN                       
086900                               MOD-ADLEVEL11-TOM-IN                       
087000                               MOD-KDLOC-IN                               
087100                               MOD-KDFREQ-IN                              
087200                               MOD-TELOC-IN                               
087300     .                                                                    
087400     EJECT                                                                
087500 MFS-INIT-KEY-FIELD-OUT SECTION.                                          
087600                                                                          
087700*    --- ALL OUTPUT KEY FIELDS                                            
087800                                                                          
087900     MOVE MSGI-IDDC         TO MOD-IDDC-UT                                
088000     MOVE MFS-ERASE-FIELD   TO MOD-ADLAGOMR-UT                            
088100                               MOD-ADGANG-FOM-UT                          
088200                               MOD-ADGANG-TOM-UT                          
088300                               MOD-ADSEC11-FOM-UT                         
088400                               MOD-ADSEC11-TOM-UT                         
088500                               MOD-ADLEVEL11-FOM-UT                       
088600                               MOD-ADLEVEL11-TOM-UT                       
088700                               MOD-KDLOC-UT                               
088800                               MOD-KDFREQ-UT                              
088900                               MOD-TELOC-UT                               
089000     .                                                                    
089100     EJECT                                                                
089200 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
089300                                                                          
089400*    --- OUTDATA-FIELD ON SCROLL KEYS                                     
089500                                                                          
089600     MOVE +1 TO INDX                                                      
089700     PERFORM UNTIL INDX > MAX-INDX                                        
089800      MOVE MFS-ERASE-FIELD     TO MOD-ADLAGOMR-LINE   (INDX)              
089900                                  MOD-ADGANG-LINE     (INDX)              
090000                                  MOD-ADPLATS-LINE    (INDX)              
090100                                  MOD-KDLOC-LINE      (INDX)              
090200                                  MOD-KDFREQ-LINE     (INDX)              
090300                                  MOD-KDSTOR-LINE     (INDX)              
090400                                  MOD-TELOC-LINE      (INDX)              
090500                                  MOD-IDARTNR-LINE    (INDX)              
090600      ADD +1 TO INDX                                                      
090700     END-PERFORM                                                          
090800     .                                                                    
090900     SKIP3                                                                
091000* --- IMS SECTIONS ---                                                    
091100     SKIP3                                                                
091200 IMS-GET-MSG SECTION.                                                     
091300                                                                          
091400     MOVE '  QC' TO GOOD-STATUSCODES                                      
091500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
091600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
091700     PERFORM IMS-STATUSCHECK                                              
091800     .                                                                    
091900     SKIP3                                                                
092000 IMS-INSERT-ALT-MSG SECTION.                                              
092100                                                                          
092200     MOVE SPACE TO GOOD-STATUSCODES                                       
092300     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
092400     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
092500     PERFORM IMS-STATUSCHECK                                              
092600     .                                                                    
092700     EJECT                                                                
092800 IMS-INSERT-MSG SECTION.                                                  
092900                                                                          
093000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
093100     MOVE SPACE TO GOOD-STATUSCODES                                       
093200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
093300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
093400     PERFORM IMS-STATUSCHECK                                              
093500     .                                                                    
093600     EJECT                                                                
093700 IMS-GU-LOCA SECTION.                                                     
093800                                                                          
093900     STRING 'WLLOCA01(WDJ801KY=>' W-WDJ8KY-MIN-X                          
094000                    '&WDJ801KY=<' W-WDJ8KY-MAX-X ')'                      
094100          DELIMITED BY SIZE INTO SSA1                                     
094200     MOVE '  GE' TO GOOD-STATUSCODES                                      
094300     CALL CBLTDLI USING GU LOCA-PCB LOC-WDJ801 SSA1                       
094400     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
094500     PERFORM IMS-STATUSCHECK                                              
094600     .                                                                    
094700     SKIP3                                                                
094800                                                                          
094900 IMS-GN-LOCA SECTION.                                                     
095000                                                                          
095100     STRING 'WLLOCA01(WDJ801KY=>' W-WDJ8KY-MIN-X                          
095200                    '&WDJ801KY=<' W-WDJ8KY-MAX-X ')'                      
095300          DELIMITED BY SIZE INTO SSA1                                     
095400     MOVE '  GE' TO GOOD-STATUSCODES                                      
095500     CALL CBLTDLI USING GN LOCA-PCB LOC-WDJ801 SSA1                       
095600     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
095700     PERFORM IMS-STATUSCHECK                                              
095800     .                                                                    
095900     SKIP3                                                                
096000                                                                          
096100 IMS-GU-WDK6E1 SECTION.                                                   
096200                                                                          
096300     STRING 'WDK6E1  (WDK6E1KY=>' WDK6E1KY-MIN-X                          
096400                    '&WDK6E1KY=<' WDK6E1KY-MAX-X ')'                      
096500          DELIMITED BY SIZE INTO SSA1                                     
096600     MOVE '  GE' TO GOOD-STATUSCODES                                      
096700     CALL CBLTDLI USING GU ARTR-PCB SEQE-WDK6E1 SSA1                      
096800     MOVE ARTR-STATUS-CODE TO STATUS-WS                                   
096900     PERFORM IMS-STATUSCHECK                                              
097000     .                                                                    
097100     SKIP3                                                                
097200                                                                          
097300 IMS-GN-WDK6E1 SECTION.                                                   
097400                                                                          
097500     STRING 'WDK6E1  (WDK6E1KY=>' WDK6E1KY-MIN-X                          
097600                    '&WDK6E1KY=<' WDK6E1KY-MAX-X ')'                      
097700          DELIMITED BY SIZE INTO SSA1                                     
097800     MOVE '  GBGE' TO GOOD-STATUSCODES                                    
097900     CALL CBLTDLI USING GN ARTR-PCB SEQE-WDK6E1 SSA1                      
098000     MOVE ARTR-STATUS-CODE TO STATUS-WS                                   
098100     PERFORM IMS-STATUSCHECK                                              
098200     .                                                                    
098300     SKIP3                                                                
098400                                                                          
098500 IMS-GU-ARTD-ASEQ SECTION.                                                
098600                                                                          
098700     STRING 'WLARTD11(WDD8ASEQ =' W-WDD8ASEQ-X ')'                        
098800          DELIMITED BY SIZE INTO SSA1                                     
098900     MOVE '  GBGE' TO GOOD-STATUSCODES                                    
099000     CALL CBLTDLI USING GU  ARTD-PCB SALDO-WDD811 SSA1                    
099100     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
099200     PERFORM IMS-STATUSCHECK                                              
099300     .                                                                    
099400     EJECT                                                                
099500*IMS-GU-ARTD-ASEQ-B SECTION.                                              
099600*                                                                         
099700*    STRING 'WLARTR01(WDD8ASEQ=>' WDD8ASEQ-MIN-X                          
099800*                   '&WDD8ASEQ=<' WDD8ASEQ-MAX-X ')'                      
099900*         DELIMITED BY SIZE INTO SSA1                                     
100000*    MOVE '  GBGE' TO GOOD-STATUSCODES                                    
100100*    CALL CBLTDLI USING GU  ARTD-PCB SALDO-WDD811 SSA1                    
100200*    MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
100300*    PERFORM IMS-STATUSCHECK                                              
100400*    .                                                                    
100500*    EJECT                                                                
100600 IMS-GN-ARTD-ASEQ-B SECTION.                                              
100700                                                                          
100800     STRING 'WLARTR01(WDD8ASEQ=>' WDD8ASEQ-MIN-X                          
100900                    '&WDD8ASEQ=<' WDD8ASEQ-MAX-X ')'                      
101000          DELIMITED BY SIZE INTO SSA1                                     
101100     MOVE '  GBGE' TO GOOD-STATUSCODES                                    
101200     CALL CBLTDLI USING GN  ARTD-PCB SALDO-WDD811 SSA1                    
101300     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
101400     PERFORM IMS-STATUSCHECK                                              
101500     .                                                                    
101600     EJECT                                                                
101700 IMS-STATUSCHECK SECTION.                                                 
101800                                                                          
101900     SET STATUS-IX TO 1                                                   
102000     SEARCH GOOD-STATUS                                                   
102100       AT END                                                             
102200         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
102300         DELIMITED BY SIZE INTO ERROR-TEXT                                
102400         CALL FELLOG                                                      
102500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
102600         CONTINUE                                                         
102700     END-SEARCH                                                           
102800     .                                                                    
