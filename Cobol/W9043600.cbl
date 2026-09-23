000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9043600.                                                
000300 AUTHOR.         JOHAN LINDKVIST.                                         
000400 DATE-WRITTEN.   99/07/13.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        VISAR OCH/ELLER UPPDATERAR FÖRPACKNINGSTYP                       
000900*                                                                         
001000*        THE PROGRAM UPDATES   WLARTC (WDK6)                              
001100*        THE PROGRAM READS     WLBENA (WDD3)                              
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSACTION: W9T436                                              
001500*        MID:         W90436I1                                            
001600*                                                                         
001700*    OUTDATA.                                                             
001800*        MOD:         W90436O1                                            
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400 WORKING-STORAGE SECTION.                                                 
002500                                                                          
002600*    -- CHECKED BY WY2000                                                 
002700 77  IDPGM                       PIC X(08)   VALUE 'W9043600'.            
002800                                                                          
002900 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
003000 77  DAGENS-AAAAMMDD             PIC 9(8)    VALUE ZERO.                  
003100 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
003200 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
003300 77  P-TO-P-KVLL-CPYTXT          PIC S9(4)           COMP SYNC.           
003400 77  W-TEBEFT-79                 PIC X(79)   VALUE SPACE.                 
003500                                                                          
003600*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003700 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003800                                                                          
003900 77  YES                         PIC X       VALUE 'Y'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004100                                                                          
004200*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004300                                                                          
004400 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
004500     88  INDATA-OK                           VALUE 'Y'.                   
004600     88  INDATA-WRONG                        VALUE 'N'.                   
004700                                                                          
004800 77  BEFT-SW                     PIC X       VALUE 'N'.                   
004900     88  BEFT-RETT                           VALUE 'Y'.                   
005000                                                                          
005100 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005200     88  KEYS-OK                             VALUE 'Y'.                   
005300     88  KEYS-WRONG                          VALUE 'N'.                   
005400                                                                          
005500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005600     88  OWN-MID                             VALUE '9436'.                
005700     88  GOOD-MID                            VALUE '9436' '9435'          
005800                                                   '9436' '9435'          
005900                                                   '9435'.                
006000     88  HELP-MID                            VALUE '0551'.                
006100                                                                          
006200*    --- WORKING STORAGE FIELDS                                           
006300                                                                          
006400 01  HIST-IX                     PIC 99      VALUE ZERO.                  
006500                                                                          
006600 01  WS-TID                      PIC S9(9).                               
006700 01  WS-DAREGDAT                 PIC 9(8).                                
006800 01  WS-DAREGDAT-2 REDEFINES WS-DAREGDAT.                                 
006900     03 FILLER                   PIC 9(2).                                
007000     03 WS-TIREGDAT              PIC 9(6).                                
007100                                                                          
007200     EJECT                                                                
007300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
007400 01  GENERAL-SUBPROGRAMS.                                                 
007500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007900     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
008000     EJECT                                                                
008100*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
008200*01 -COPY WMEDAREA                                                        
008300     SKIP3                                                                
008310*01 -COPY WWLNDKON                                                        
008400 01  MESSAGE-CODES.                                                       
008500     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008700     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008800     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
008900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009000     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
009100     03  ERR-PRESS-PF23          PIC X(3)    VALUE '206'.                 
009200                                                                          
009300*                                                                         
009400   03    W-WDGX2213-X.                                                    
009500     05  FILLER          PIC X(4)    VALUE '2213'.                        
009510     05  W-IDDC          PIC X(2)    VALUE '11'.                          
009600     05  FILLER          PIC X(24)   VALUE LOW-VALUE.                     
009700*                                                                         
009800     EJECT                                                                
009900*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
010000*                                                                         
010100 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
010200     SKIP3                                                                
010300*01 -COPY WMSGINIT                                                        
010400     EJECT                                                                
010500*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
010600*                                                                         
010700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010800     SKIP3                                                                
010900*01  MID -COPY W90436I1                                                   
011000     EJECT                                                                
011100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011200     SKIP3                                                                
011300*01  -COPY WMSGAREA                                                       
011400     EJECT                                                                
011500     03  MOD REDEFINES MSG-AREA.                                          
011600*      05  -COPY W90436O1                                                 
011700     EJECT                                                                
011800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011900     SKIP3                                                                
012000*01  -COPY WMFSAREA                                                       
012100     EJECT                                                                
012200 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
012300     SKIP3                                                                
012400 01  KOM-MSG-IO-AREA.                                                     
012500*03  -COPY WMSGKOM                                                        
012600     EJECT                                                                
012700*    --- WORK-AREAS FOR IMS-SECTIONS                                      
012800*                                                                         
012900     EJECT                                                                
013000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013100     SKIP3                                                                
013200 01  KEYS-TO-DLI.                                                         
013300     03  W-IDARTNR-X.                                                     
013400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
013500     03  W-IDSKYLT-X.                                                     
013600         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
013610     03  W-IDLAND-X.                                                      
013620         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
013700     03  W-WDT311KY-X.                                                    
013800         05  W-DAREGDAT-9KOMPL   PIC 9(8)    VALUE ZERO.                  
013900         05  W-TIKLOCK-9KOMPL    PIC S9(9)   VALUE ZERO COMP-3.           
014000     SKIP2                                                                
014100 01      P-TO-P-SW.                                                       
014200                                                                          
014300  02     P-TO-P-KVLL             PIC S9(4)           COMP SYNC.           
014400  02     P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
014500  02     P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
014600  02     P-TO-P-KDTRANS          PIC X(8).                                
014700  02     P-TO-P-IDTRANS          PIC X(4).                                
014800  02     P-TO-P-KDMFSFOR         PIC X(1).                                
014900  02     P-TO-P-DATA             PIC X(1000).                             
015000     EJECT                                                                
015100 01      FILLER                  PIC X(24)   VALUE                        
015200                                 'MOD619B-MID-W6I19B01'.                  
015300     SKIP2                                                                
015400     -COPY W6I19B01 -PRE MOD619B-                                         
015500     EJECT                                                                
015600*    --- STATUS-KOD FRÅN IMS                                              
015700 01  STATUS-WS                   PIC XX.                                  
015800     88  SEGMENT-FOUND                       VALUE '  '.                  
015900     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
016000     88  SEGMENT-MISSING                     VALUE 'GE'.                  
016100     SKIP2                                                                
016200 01  GOOD-STATUSCODES.                                                    
016300     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016400     SKIP3                                                                
016500 01  SSA1                        PIC X(64).                               
016600 01  SSA2                        PIC X(64).                               
016700     EJECT                                                                
016800*    --- IMS FUNCTION CODES                                               
016900*01  -COPY W0003                                                          
017000     EJECT                                                                
017100*    ---  DLI INPUT-OUTPUT AREA                                           
017200                                                                          
017300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC01'.                    
017400 01  DLI-IO-WLARTC01.                                                     
017500*    03  -COPY WDK601  -PRE ARTC-                                         
017600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC11'.                    
017700 01  DLI-IO-WLARTC11.                                                     
017800*    03  -COPY WDK611  -PRE ARTC-                                         
017900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT301'.                      
018000 01  DLI-IO-WDT301.                                                       
018100*    03  -COPY WDT301                                                     
018110 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT311'.                      
018120 01  DLI-IO-WDT311.                                                       
018130*    03  -COPY WDT311  -PRE ARTC-                                         
018200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBENA11'.                    
018300 01  DLI-IO-WLBENA11.                                                     
018400*    03  -COPY WDD311  -PRE BENA-                                         
018500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2214'.                    
018600 01  DLI-IO-WDGX2214.                                                     
018700*    03  -COPY WDGX2214                                                   
018800     EJECT                                                                
018900 LINKAGE SECTION.                                                         
019000*01  -COPY W0009   -PRE MSG-                                              
019100     EJECT                                                                
019200*01  -COPY W0009  -PRE DISP-                                              
019300     EJECT                                                                
019400*01  -COPY W0008   -PRE USEA-                                             
019500     05  FILLER                  PIC X.                                   
019600                                                                          
019700*01  -COPY W0008  -PRE ARTC-                                              
019800     05  FILLER                  PIC X.                                   
019900                                                                          
020000*01  -COPY W0008  -PRE BENA-                                              
020100     05  FILLER                  PIC X.                                   
020200     EJECT                                                                
020300*01  -COPY W0008  -PRE XXBI-                                              
020400     05 FILLER                   PIC X.                                   
020500                                                                          
020510*01  -COPY W0008  -PRE WDT3-                                              
020520     05 FILLER                   PIC X.                                   
020530                                                                          
020600*    PCB'ER FÖR SUBPGM                                                    
020700                                                                          
020800 01 KOM-KOMA-PCB                 PIC X.                                   
020900     EJECT                                                                
021000 PROCEDURE DIVISION  USING MSG-PCB DISP-PCB USEA-PCB ARTC-PCB             
021100                           BENA-PCB XXBI-PCB WDT3-PCB                     
021110                           KOM-KOMA-PCB.                                  
021200 MAIN SECTION.                                                            
021300     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB USEA-PCB ARTC-PCB             
021310                           BENA-PCB XXBI-PCB WDT3-PCB                     
021320                           KOM-KOMA-PCB.                                  
021500                                                                          
021600     PERFORM IMS-GET-MSG                                                  
021700     IF SEGMENT-FOUND                                                     
021800                                                                          
021900       PERFORM A-INIT                                                     
022000       PERFORM B-CHECK-KEYS                                               
022100                                                                          
022200       IF KEYS-OK                                                         
022300         IF MFS-UPDATE OR MFS-UPD-V                                       
022400           PERFORM G-CHECK-INPUT                                          
022500           IF INDATA-OK                                                   
022600             PERFORM H-UPDATE                                             
022700           END-IF                                                         
022800         ELSE                                                             
022900           IF MFS-FIRST                                                   
023000             PERFORM C-FIRST-PAGE                                         
023100           ELSE                                                           
023200             PERFORM E-SAME-PAGE                                          
023300           END-IF                                                         
023400         END-IF                                                           
023500                                                                          
023600         PERFORM F-READ-SHOW-INFO                                         
023700       END-IF                                                             
023800                                                                          
023900       COMPUTE MSG-KVLL = LENGTH OF MOD-W90436O1 + 4                      
024000       PERFORM IMS-INSERT-MSG                                             
024100     END-IF                                                               
024200                                                                          
024300     MOVE ZERO TO RETURN-CODE                                             
024400     GOBACK                                                               
024500     .                                                                    
024600     EJECT                                                                
024700                                                                          
024800                                                                          
024900                                                                          
025000                                                                          
025100 A-INIT SECTION.                                                          
025200                                                                          
025300     IF MSG-DOUBLE-TRANSACTIONS                                           
025400       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W90436I1                 
025500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
025600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
025700     ELSE                                                                 
025800       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W90436I1                  
025900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
026000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
026100     END-IF                                                               
026200                                                                          
026300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
026400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
026500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
026600                                                                          
026700     MOVE LOW-VALUE TO MSG-AREA                                           
026800*    MOVE 'W90436O1' TO MFS-IDMOD                                         
026900     MOVE '9436' TO MOD-IDTRANS                                           
027000     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
027100                                                                          
027200     IF MSGI-IDLAND-SPR = 'SE'                                            
027300        MOVE '0' TO MFS-KDHUVOMR                                          
027400     END-IF                                                               
027500                                                                          
027600     IF OWN-MID OR HELP-MID                                               
027700       CONTINUE                                                           
027800     ELSE                                                                 
027900       MOVE SPACE TO MFS-KDTRTYP                                          
028000       MOVE '7' TO MFS-IDPFK                                              
028100     END-IF                                                               
028200                                                                          
028300                                                                          
028400     ACCEPT DAGENS-DATUM FROM DATE                                        
028500     ACCEPT DAGENS-TID   FROM TIME                                        
028600     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-AAAAMMDD                   
028700                                                                          
028800     .                                                                    
028900     EJECT                                                                
029000                                                                          
029100                                                                          
029200                                                                          
029300                                                                          
029400 B-CHECK-KEYS SECTION.                                                    
029500     MOVE ALL '+'           TO MSGI-WMSGINIT                              
029600     MOVE '001'             TO MSGI-KDCALL                                
029700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
029800     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
029900     MOVE '9436'            TO MSGI-IDTRANS                               
030000                                                                          
030100     IF GOOD-MID                                                          
030200        MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                               
030300     END-IF                                                               
030400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
030500     MOVE YES TO KEYS-SW                                                  
030600                                                                          
031700     IF MID-IDARTNR-IN NOT = ALL '+'                                      
031800       MOVE '7'         TO MFS-IDPFK                                      
031900       MOVE SPACE       TO MFS-KDTRTYP                                    
032000     END-IF                                                               
032100                                                                          
032200                                                                          
032300     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
032400     IF MSGI-IDARTNR NUMERIC                                              
032500       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
032600     ELSE                                                                 
032700       MOVE NEJ TO KEYS-SW                                                
032800     END-IF                                                               
032900*                                                                         
033000     IF GOOD-MID OR KEYS-OK                                               
033100       MOVE MSGI-IDARTNR TO MOD-IDARTNR-UT                                
033200       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
033300     ELSE                                                                 
033400       MOVE MFS-ERASE-FIELD TO MOD-IDARTNR-UT                             
033500     END-IF                                                               
033600                                                                          
033700     IF KEYS-WRONG                                                        
033800       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
033900       CALL WMEDKONV USING MED-WMEDAREA                                   
034000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
034100       PERFORM MFS-ERASE-FIELD-IN                                         
034200       PERFORM MFS-ERASE-FIELD-OUT                                        
034300     END-IF                                                               
034400     .                                                                    
034500     EJECT                                                                
034600 C-FIRST-PAGE SECTION.                                                    
034700                                                                          
034800     PERFORM MFS-ERASE-FIELD-IN                                           
034900                                                                          
035000     .                                                                    
035100     EJECT                                                                
035200                                                                          
035300                                                                          
035400                                                                          
035500                                                                          
035600                                                                          
035700 E-SAME-PAGE SECTION.                                                     
035800                                                                          
035900     IF OWN-MID OR HELP-MID                                               
036000                                                                          
036100       IF  MID-BEFT          = ALL '+'                                    
036200*      AND MID-KDFORP        = ALL '+'                                    
036300       AND MID-TEBEFT        = ALL '+'                                    
036400       AND MID-TEBEFT-79     = ALL '+'                                    
036500         PERFORM MFS-ERASE-FIELD-IN                                       
036600       ELSE                                                               
036700         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
036800         CALL WMEDKONV USING MED-WMEDAREA                                 
036900         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
037000         PERFORM EA-MID-INDATA-TO-MOD                                     
037100       END-IF                                                             
037200     ELSE                                                                 
037300       PERFORM MFS-ERASE-FIELD-IN                                         
037400     END-IF                                                               
037500                                                                          
037600     .                                                                    
037700     EJECT                                                                
037800                                                                          
037900                                                                          
038000                                                                          
038100                                                                          
038200                                                                          
038300 EA-MID-INDATA-TO-MOD SECTION.                                            
038400                                                                          
038500                                                                          
038600     IF MID-BEFT           NOT = ALL '+'                                  
038700        MOVE MFS-ADD-READ-FIELD     TO MOD-BEFT-IN-ATTR                   
038800        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-BEFT-IN                        
038900     ELSE                                                                 
039000        MOVE MFS-ERASE-FIELD        TO MOD-BEFT-IN                        
039100     END-IF                                                               
039200                                                                          
039300                                                                          
039400*    IF MID-KDFORP      NOT = ALL '+'                                     
039500*       MOVE MFS-ADD-READ-FIELD     TO MOD-KDFORP-IN-ATTR                 
039600*       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDFORP-IN                      
039700*    ELSE                                                                 
039800*       MOVE MFS-ERASE-FIELD        TO MOD-KDFORP-IN                      
039900*    END-IF                                                               
040000                                                                          
040100                                                                          
040200     IF MID-TEBEFT      NOT = ALL '+'                                     
040300        MOVE MFS-ADD-READ-FIELD     TO MOD-TEBEFT-IN-ATTR                 
040400        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-TEBEFT-IN                      
040500     ELSE                                                                 
040600        MOVE MFS-ERASE-FIELD        TO MOD-TEBEFT-IN                      
040700     END-IF                                                               
040800                                                                          
040900     IF MID-TEBEFT-79 NOT = ALL '+'                                       
041000        MOVE MFS-ADD-READ-FIELD     TO MOD-TEBEFT-79-IN-ATTR              
041100        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-TEBEFT-79-IN                   
041200     ELSE                                                                 
041300        MOVE MFS-ERASE-FIELD        TO MOD-TEBEFT-79-IN                   
041400     END-IF                                                               
041800     .                                                                    
041900     EJECT                                                                
042900 F-READ-SHOW-INFO SECTION.                                                
043000                                                                          
043100     PERFORM FA-READ-BASICDATA                                            
043200                                                                          
043300     IF SEGMENT-MISSING                                                   
043400        MOVE ERR-PART-MISSING TO MED-IDMFSFEL                             
043500        CALL WMEDKONV USING MED-WMEDAREA                                  
043600        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
043700        PERFORM MFS-ERASE-FIELD-OUT                                       
043800     ELSE                                                                 
043900        PERFORM FB-SHOW-PART-INFO                                         
044000     END-IF                                                               
044100     .                                                                    
044200     EJECT                                                                
044300 FA-READ-BASICDATA SECTION.                                               
044500     PERFORM IMS-GET-ARTC-ARTC                                            
044600     .                                                                    
044700     EJECT                                                                
044800 FB-SHOW-PART-INFO SECTION.                                               
045100     PERFORM FD-GET-HIST-INFO                                             
045200     .                                                                    
045300     EJECT                                                                
046000 FD-GET-HIST-INFO SECTION.                                                
046300     MOVE ZERO TO HIST-IX                                                 
046310     MOVE WC-LAND-SE           TO   W-IDLAND                              
046400     PERFORM IMS-GU-WDT311                                                
046500                                                                          
046600     IF SEGMENT-FOUND                                                     
046800       MOVE ARTC-FPCK-BEFT      TO  MOD-BEFT-UT                           
047100       MOVE ARTC-FPCK-TEBEFT(1) TO  MOD-TEBEFT-UT                         
047200       MOVE ARTC-FPCK-TEBEFT(2) TO  W-TEBEFT-79(1:40)                     
047300       MOVE ARTC-FPCK-TEBEFT(3) TO  W-TEBEFT-79(41:39)                    
047400       MOVE W-TEBEFT-79         TO  MOD-TEBEFT-79-UT                      
047700       PERFORM FD-CONVERT-FROM-9KOMPL                                     
047800     END-IF                                                               
050200     .                                                                    
050300     EJECT                                                                
050400 FD-CONVERT-FROM-9KOMPL SECTION.                                          
050500                                                                          
050600     COMPUTE WS-DAREGDAT = 999999999 - ARTC-FPCK-DAREGDAT-9KOMPL          
050700     .                                                                    
050800     EJECT                                                                
050900 G-CHECK-INPUT SECTION.                                                   
051000                                                                          
051100     MOVE YES  TO INDATA-SW                                               
051200     IF MID-BEFT     = ALL '+'                                            
051300*    AND MID-KDFORP  = ALL '+'                                            
051400     AND MID-TEBEFT  = ALL '+'                                            
051500     AND MID-TEBEFT-79 = ALL '+'                                          
051600       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
051700       CALL WMEDKONV USING MED-WMEDAREA                                   
051800       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
051900       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
052000       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
052100       MOVE NEJ TO INDATA-SW                                              
052200     ELSE                                                                 
052300       IF MID-BEFT   NOT = ALL '+'                                        
052400          IF MID-BEFT  NOT NUMERIC                                        
052500            MOVE MFS-NUM-FIELD-WRONG TO MOD-BEFT-IN-ATTR                  
052600            MOVE NEJ TO INDATA-SW                                         
052700          ELSE                                                            
052800            MOVE MFS-NUM-FIELD-OK TO MOD-BEFT-IN-ATTR                     
052900          END-IF                                                          
053000       END-IF                                                             
053100*                                                                         
053200*      IF MID-KDFORP NOT = ALL '+'                                        
053300*         IF MID-KDFORP NOT NUMERIC                                       
053400*           MOVE MFS-NUM-FIELD-WRONG TO MOD-KDFORP-IN-ATTR                
053500*           MOVE NEJ TO INDATA-SW                                         
053600*         ELSE                                                            
053700*           MOVE MFS-NUM-FIELD-OK TO MOD-KDFORP-IN-ATTR                   
053800*         END-IF                                                          
053900*      END-IF                                                             
054000                                                                          
054100       IF MID-TEBEFT   NOT = ALL '+'                                      
054200         MOVE MFS-ALPHA-FIELD-OK TO MOD-TEBEFT-IN-ATTR                    
054300       END-IF                                                             
054400                                                                          
054500       IF MID-TEBEFT-79 NOT = ALL '+'                                     
054600         MOVE MFS-ALPHA-FIELD-OK TO MOD-TEBEFT-79-IN-ATTR                 
054700       END-IF                                                             
054800                                                                          
054900       IF INDATA-WRONG                                                    
055000         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
055100         CALL WMEDKONV USING MED-WMEDAREA                                 
055200         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
055300         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
055400         PERFORM MFS-DONT-TOUCH-FIELD-IN                                  
055500       ELSE                                                               
055600                                                                          
055700          PERFORM IMS-GET-ARTC-ARTC                                       
055800          IF SEGMENT-MISSING                                              
055900               MOVE ERR-PART-MISSING TO MED-IDMFSFEL                      
056000               CALL WMEDKONV USING MED-WMEDAREA                           
056100               MOVE MED-MFSFEL TO MOD-TEMFSFEL                            
056200               PERFORM MFS-DONT-TOUCH-FIELD-OUT                           
056300               PERFORM MFS-DONT-TOUCH-FIELD-IN                            
056400          ELSE                                                            
056410             MOVE WC-LAND-SE           TO   W-IDLAND                      
056500             PERFORM IMS-GU-WDT311                                        
056600             PERFORM GA-CHECK-BEFT-UPDATING-RULES                         
056700             IF INDATA-WRONG                                              
056800               MOVE ERR-PRESS-PF23 TO MED-IDMFSFEL                        
056900               CALL WMEDKONV USING MED-WMEDAREA                           
057000               MOVE MED-MFSFEL TO MOD-TEMFSFEL                            
057100               PERFORM MFS-DONT-TOUCH-FIELD-OUT                           
057200               PERFORM MFS-DONT-TOUCH-FIELD-IN                            
057300             ELSE                                                         
057400               CONTINUE                                                   
057500             END-IF                                                       
057600          END-IF                                                          
057700       END-IF                                                             
057800                                                                          
057900     END-IF                                                               
058000     .                                                                    
058100     EJECT                                                                
058200 GA-CHECK-BEFT-UPDATING-RULES SECTION.                                    
058300                                                                          
058400     IF SEGMENT-FOUND                                                     
058500        IF ARTC-FPCK-BEFT = 75                                            
058600            IF NOT MFS-UPD-V                                              
058700               MOVE NEJ TO INDATA-SW                                      
058800            END-IF                                                        
058900        END-IF                                                            
059000     END-IF                                                               
059100                                                                          
059200     IF MID-BEFT         = 79                                             
059300         IF NOT MFS-UPD-V                                                 
059400            MOVE NEJ TO INDATA-SW                                         
059500         END-IF                                                           
059600      END-IF                                                              
059700     .                                                                    
059800     EJECT                                                                
059900 H-UPDATE SECTION.                                                        
060000                                                                          
060100     PERFORM IMS-GET-ARTC-ARTC                                            
060200     IF SEGMENT-FOUND                                                     
060300        MOVE ZEROS TO ARTC-FPCK-BEFT                                      
060400        MOVE ZEROS TO ARTC-FPCK-KDFORP                                    
060500        MOVE SPACE TO ARTC-FPCK-TEBEFT(1)                                 
060600        MOVE SPACE TO ARTC-FPCK-TEBEFT(2)                                 
060700        MOVE SPACE TO ARTC-FPCK-TEBEFT(3)                                 
060701        MOVE SPACE TO ARTC-FPCK-TEBEFT(4)                                 
060702        MOVE SPACE TO ARTC-FPCK-TEBEFT(5)                                 
060710        MOVE WC-LAND-SE           TO   W-IDLAND                           
060800        PERFORM IMS-GU-WDT311                                             
060900*       ---------------------------                <-- DUBBELLAGRA        
061000        PERFORM IMS-GET-ARTC-ARTC                                         
061100        PERFORM IMS-GHUP-ARTC-CLAG                                        
061200*       --------------------------                 <-- DUBBELLAGRA        
061300                                                                          
061400        IF MID-BEFT  NOT = ALL '+'                                        
061500          MOVE MID-BEFT  TO ARTC-FPCK-BEFT  MOD-BEFT-UT                   
061600*       --------------------------                 <-- DUBBELLAGRA        
061700                               ARTC-CLAG-BEFT                             
061800*       --------------------------                 <-- DUBBELLAGRA        
061900          PERFORM HB-STARTA-DISPATCHEN                                    
062000          MOVE MFS-ADD-HILIGHT-FIELD TO MOD-BEFT-IN-ATTR                  
062100          MOVE YES TO BEFT-SW                                             
062200        ELSE                                                              
062300          MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-BEFT-IN-ATTR                 
062400          MOVE NEJ TO BEFT-SW                                             
062500        END-IF                                                            
063600                                                                          
063700        IF MID-TEBEFT NOT = ALL '+'                                       
063800          MOVE MID-TEBEFT TO ARTC-FPCK-TEBEFT(1) MOD-TEBEFT-UT            
063900          MOVE MFS-ADD-HILIGHT-FIELD TO MOD-TEBEFT-IN-ATTR                
064000        ELSE                                                              
064100          MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-TEBEFT-IN-ATTR               
064200        END-IF                                                            
064300                                                                          
064400        IF MID-TEBEFT-79 NOT = ALL '+'                                    
064500          MOVE MID-TEBEFT-79(1:40)  TO ARTC-FPCK-TEBEFT(2)                
064600          MOVE MID-TEBEFT-79(41:39) TO ARTC-FPCK-TEBEFT(3)                
064700          MOVE MID-TEBEFT-79        TO MOD-TEBEFT-79-UT                   
064800          MOVE MFS-ADD-HILIGHT-FIELD TO MOD-TEBEFT-79-IN-ATTR             
064900        ELSE                                                              
065000          MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-TEBEFT-79-IN-ATTR            
065100        END-IF                                                            
065200                                                                          
065300        MOVE MSG-SIGNON-USERID TO ARTC-FPCK-IDUSER                        
065400        PERFORM HA-CONVERT-TO-9KOMPL                                      
065500                                                                          
065600*       --------------------------                 <-- DUBBELLAGRA        
065700        IF SEGMENT-FOUND                                                  
065800           PERFORM IMS-REPL-ARTC-CLAG                                     
065900*       --------------------------                 <-- DUBBELLAGRA        
065910           MOVE W-IDARTNR TO FART-IDARTNR                                 
066000           PERFORM IMS-ISRT-WDT301                                        
066001           MOVE WC-LAND-SE TO ARTC-FPCK-IDLANDX2                          
066002           MOVE SPACE      TO ARTC-FPCK-TEBEFT(4)                         
066003                              ARTC-FPCK-TEBEFT(5)                         
066010           PERFORM IMS-ISRT-WDT311                                        
066100                                                                          
066200           MOVE INF-UPDATE-DONE TO MED-IDMFSINF                           
066300           CALL WMEDKONV USING MED-WMEDAREA                               
066400           MOVE MED-MFSINF TO MOD-TEMFSINF                                
066500           PERFORM MFS-FORM-ATTR                                          
066600           PERFORM MFS-ERASE-FIELD-IN                                     
066700        ELSE                                                              
066800          MOVE 'UPPDATERINGEN MISSLYCKADES' TO MOD-TEMFSFEL               
066900          PERFORM MFS-ERASE-FIELD-OUT                                     
067000        END-IF                                                            
067100*    * * MFS-DO-NOT-TOUCH-FIELD TO LOCKED VALUES                          
067200                                                                          
067300        IF BEFT-RETT                                                      
067400           MOVE   W-IDARTNR    TO 2214-IDARTNR                            
067500           PERFORM IMS-ISRT-2214                                          
067600        END-IF                                                            
067700                                                                          
067800     ELSE                                                                 
067900        MOVE ERR-PART-MISSING TO MED-IDMFSFEL                             
068000        CALL WMEDKONV USING MED-WMEDAREA                                  
068100        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
068200        PERFORM MFS-ERASE-FIELD-OUT                                       
068300     END-IF                                                               
068400     .                                                                    
068500     EJECT                                                                
068600 HA-CONVERT-TO-9KOMPL SECTION.                                            
068700     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DAREGDAT                       
068800     COMPUTE ARTC-FPCK-DAREGDAT-9KOMPL = 999999999 - WS-DAREGDAT          
068900     ACCEPT WS-TID FROM TIME                                              
069000     COMPUTE ARTC-FPCK-TIKLOCK-9KOMPL = 999999999 - WS-TID                
069100                                                                          
069200     MOVE ARTC-FPCK-DAREGDAT-9KOMPL  TO W-DAREGDAT-9KOMPL                 
069300     MOVE ARTC-FPCK-TIKLOCK-9KOMPL  TO W-TIKLOCK-9KOMPL                   
069400                                                                          
069500     .                                                                    
069600     EJECT                                                                
069700 HB-STARTA-DISPATCHEN       SECTION.                                      
069800                                                                          
069900     MOVE SPACE                TO MSG-KOM-WMSGKOM                         
070000     COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                     
070100     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
070200     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
070300     MOVE SPACE                TO MSG-KOM-KDTRANS                         
070400     MOVE 'W6I19B01'           TO MSG-KOM-IDCPYTXT                        
070500     MOVE 'INLEV   '           TO MSG-KOM-IDSNDNOD                        
070600     MOVE 'W9043600'           TO MSG-KOM-IDSNDJOB                        
070700     MOVE DAGENS-DATUM         TO MSG-KOM-TIREGDAT                        
070800     MOVE DAGENS-TID           TO MSG-KOM-TIKLOCK                         
070900     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
071000                                                                          
071100     MOVE ALL '+'              TO MOD619B-MID-W6I19B01                    
071200     MOVE MSGI-IDARTNR         TO MOD619B-MID-IDARTNR                     
071300     MOVE '11'                 TO MOD619B-MID-IDDC                        
071400     MOVE MID-BEFT             TO MOD619B-MID-BEFT                        
071500     COMPUTE P-TO-P-KVLL-CPYTXT = LENGTH OF MOD619B-MID-W6I19B01          
071600     COMPUTE P-TO-P-KVLL        = LNG-P-TO-P-PREFIX +                     
071700                                  P-TO-P-KVLL-CPYTXT                      
071800     MOVE 'W6T19BX '           TO P-TO-P-KDTRANS                          
071900     MOVE '9436'               TO P-TO-P-IDTRANS                          
072000     MOVE MFS-KDMFSFOR         TO P-TO-P-KDMFSFOR                         
072100     MOVE MOD619B-MID-W6I19B01 TO P-TO-P-DATA                             
072200                                                                          
072300     CALL W006KOM USING MSG-PCB                                           
072400                        DISP-PCB                                          
072500                        KOM-KOMA-PCB                                      
072600                        MSG-KOM-WMSGKOM                                   
072700                        P-TO-P-SW                                         
072800     .                                                                    
072900     EJECT                                                                
073000 MFS-ERASE-FIELD-OUT SECTION.                                             
073100*    --- ALLA UTDATA-FÄLT                                                 
073200*    MOVE MFS-ERASE-FIELD TO MOD-BEART-UT                                 
073300     MOVE MFS-ERASE-FIELD TO MOD-BEFT-UT                                  
073400*                            MOD-KDFORP-UT                                
073500*                            MOD-IDUSER-UT                                
073600*                            MOD-TIREGDAT-UT                              
073700                             MOD-TEBEFT-UT                                
073800                             MOD-TEBEFT-79-UT                             
073900*                                                                         
074000*                            MOD-BEFT-HIST(1)                             
074100*                            MOD-KDFORP-HIST(1)                           
074200*                            MOD-IDUSER-HIST(1)                           
074300*                            MOD-TIREGDAT-HIST(1)                         
074400*                            MOD-TEBEFT-HIST(1)                           
074500*                            MOD-TEBEFT-79-HIST(1)                        
074600*                            MOD-BEFT-HIST(2)                             
074700*                            MOD-KDFORP-HIST(2)                           
074800*                            MOD-IDUSER-HIST(2)                           
074900*                            MOD-TIREGDAT-HIST(2)                         
075000*                            MOD-TEBEFT-HIST(2)                           
075100*                            MOD-TEBEFT-79-HIST(2)                        
075200*                            MOD-BEFT-HIST(3)                             
075300*                            MOD-KDFORP-HIST(3)                           
075400*                            MOD-IDUSER-HIST(3)                           
075500*                            MOD-TIREGDAT-HIST(3)                         
075600*                            MOD-TEBEFT-HIST(3)                           
075700*                            MOD-TEBEFT-79-HIST(3)                        
075800*                            MOD-BEFT-HIST(4)                             
075900*                            MOD-KDFORP-HIST(4)                           
076000*                            MOD-IDUSER-HIST(4)                           
076100*                            MOD-TIREGDAT-HIST(4)                         
076200*                            MOD-TEBEFT-HIST(4)                           
076300*                            MOD-TEBEFT-79-HIST(4)                        
076400*                            MOD-BEFT-HIST(5)                             
076500     .                                                                    
076600     SKIP3                                                                
076700 MFS-ERASE-FIELD-IN SECTION.                                              
076800*    --- ALLA INDATA-FÄLT                                                 
076900     MOVE MFS-ERASE-FIELD TO MOD-BEFT-IN                                  
077000*                            MOD-KDFORP-IN                                
077100                             MOD-TEBEFT-IN                                
077200                             MOD-TEBEFT-79-IN                             
077300     .                                                                    
077400     EJECT                                                                
077500 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
077600*    --- ALLA UTDATA-FÄLT                                                 
077700*    MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-BEART-UT                          
077800     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-BEFT-UT                           
077900*                                   MOD-KDFORP-UT                         
078000*                                   MOD-IDUSER-UT                         
078100*                                   MOD-TIREGDAT-UT                       
078200                                    MOD-TEBEFT-UT                         
078300                                    MOD-TEBEFT-79-UT                      
078400*                                                                         
078500*                                   MOD-BEFT-HIST(1)                      
078600*                                   MOD-KDFORP-HIST(1)                    
078700*                                   MOD-IDUSER-HIST(1)                    
078800*                                   MOD-TIREGDAT-HIST(1)                  
078900*                                   MOD-TEBEFT-HIST(1)                    
079000*                                   MOD-TEBEFT-79-HIST(1)                 
079100*                                   MOD-BEFT-HIST(2)                      
079200*                                   MOD-KDFORP-HIST(2)                    
079300*                                   MOD-IDUSER-HIST(2)                    
079400*                                   MOD-TIREGDAT-HIST(2)                  
079500*                                   MOD-TEBEFT-HIST(2)                    
079600*                                   MOD-TEBEFT-79-HIST(2)                 
079700*                                   MOD-BEFT-HIST(3)                      
079800*                                   MOD-KDFORP-HIST(3)                    
079900*                                   MOD-IDUSER-HIST(3)                    
080000*                                   MOD-TIREGDAT-HIST(3)                  
080100*                                   MOD-TEBEFT-HIST(3)                    
080200*                                   MOD-TEBEFT-79-HIST(3)                 
080300*                                   MOD-BEFT-HIST(4)                      
080400*                                   MOD-KDFORP-HIST(4)                    
080500*                                   MOD-IDUSER-HIST(4)                    
080600*                                   MOD-TIREGDAT-HIST(4)                  
080700*                                   MOD-TEBEFT-HIST(4)                    
080800*                                   MOD-TEBEFT-79-HIST(4)                 
080900     .                                                                    
081000     SKIP3                                                                
081100 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
081200*    --- ALLA INDATA-FÄLT                                                 
081300     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-BEFT-IN                           
081400*                                   MOD-KDFORP-IN                         
081500                                    MOD-TEBEFT-IN                         
081600                                    MOD-TEBEFT-79-IN                      
081700                                                                          
081800     .                                                                    
081900     EJECT                                                                
082000 MFS-FORM-ATTR SECTION.                                                   
082100*    --- ALL INDATA-FIELDS                                                
082200     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-BEFT-IN-ATTR                     
082300*                                    MOD-KDFORP-IN-ATTR                   
082400                                     MOD-TEBEFT-IN-ATTR                   
082500                                     MOD-TEBEFT-79-IN-ATTR                
082600                                                                          
082700     .                                                                    
082800     SKIP2                                                                
082900*MFS-READ-IN-AGAIN SECTION.                                               
083000*    --- ALL INDATA-FIELDS                                                
083100*    MOVE MFS-ADD-READ-FIELD TO MOD-BEFT-IN-ATTR                          
083200*                                MOD-KDFORP-IN-ATTR                       
083300*                                MOD-TEBEFT-IN-ATTR                       
083400*                                                                         
083500*     .                                                                   
083600*     EJECT                                                               
083700* --- IMS SECTIONS ---                                                    
083800     SKIP3                                                                
083900 IMS-GET-MSG SECTION.                                                     
084000                                                                          
084100     MOVE '  QC' TO GOOD-STATUSCODES                                      
084200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
084300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
084400     PERFORM IMS-STATUSCHECK                                              
084500     .                                                                    
084600     SKIP3                                                                
084700 IMS-INSERT-MSG SECTION.                                                  
084800                                                                          
084900     IF MSGI-IDLAND-SPR = 'SE'                                            
085000       MOVE '0' TO MFS-KDHUVOMR                                           
085100     END-IF                                                               
085200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
085300     MOVE SPACE TO GOOD-STATUSCODES                                       
085400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
085500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
085600     PERFORM IMS-STATUSCHECK                                              
085700     .                                                                    
085800     EJECT                                                                
085900                                                                          
086000 IMS-GET-ARTC-ARTC SECTION.                                               
086100                                                                          
086200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
086300          DELIMITED BY SIZE INTO SSA1                                     
086400     MOVE '  GE' TO GOOD-STATUSCODES                                      
086500     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC01 SSA1                  
086600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
086700     PERFORM IMS-STATUSCHECK                                              
086800     .                                                                    
086900     EJECT                                                                
087000                                                                          
087100*                                                                         
087200* DUBBELLAGRA                                                             
087300*                                                                         
087400 IMS-GHUP-ARTC-CLAG      SECTION.                                         
087500     MOVE 'WDK611  ' TO SSA1                                              
087600     MOVE '  GE' TO GOOD-STATUSCODES                                      
087700     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-WLARTC11 SSA1                
087800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
087900     PERFORM IMS-STATUSCHECK                                              
088000     SKIP3                                                                
088100     .                                                                    
088200 IMS-REPL-ARTC-CLAG      SECTION.                                         
088300     MOVE '  ' TO GOOD-STATUSCODES                                        
088400     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-WLARTC11                     
088500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
088600     PERFORM IMS-STATUSCHECK                                              
088700     .                                                                    
088800     EJECT                                                                
088900*                                                                         
089000* DUBBELLAGRA                                                             
089100*                                                                         
089200                                                                          
089210 IMS-GU-WDT311 SECTION.                                                   
089220     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-X ')'                         
089230          DELIMITED BY SIZE INTO SSA1                                     
089240     STRING 'WDT311    (IDLAND   =' W-IDLAND-X ')'                        
089250          DELIMITED BY SIZE INTO SSA2                                     
089260     MOVE '  GE' TO GOOD-STATUSCODES                                      
089270     CALL CBLTDLI USING GU WDT3-PCB DLI-IO-WDT311 SSA1 SSA2               
089280     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
089290     PERFORM IMS-STATUSCHECK                                              
089291     .                                                                    
089292     EJECT                                                                
089293 IMS-ISRT-WDT301 SECTION.                                                 
089294     MOVE 'WDT301   ' TO SSA1                                             
089295     MOVE '  II' TO GOOD-STATUSCODES                                      
089296     CALL CBLTDLI USING ISRT WDT3-PCB DLI-IO-WDT301 SSA1                  
089297     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
089298     PERFORM IMS-STATUSCHECK                                              
089299     .                                                                    
089300     SKIP3                                                                
089400 IMS-ISRT-WDT311 SECTION.                                                 
089500     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-X ')'                         
089600          DELIMITED BY SIZE INTO SSA1                                     
089700     MOVE 'WDT311   ' TO SSA2                                             
089800     MOVE '  II' TO GOOD-STATUSCODES                                      
089900     CALL CBLTDLI USING ISRT WDT3-PCB DLI-IO-WDT311 SSA1 SSA2             
090000     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
090100     PERFORM IMS-STATUSCHECK                                              
090200     .                                                                    
090300     SKIP3                                                                
092400 IMS-GET-BENA-TEXT SECTION.                                               
092500     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
092600             DELIMITED BY SIZE INTO SSA1                                  
092700     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
092800              DELIMITED BY SIZE INTO SSA2                                 
092900     MOVE '  ' TO GOOD-STATUSCODES                                        
093000     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WLBENA11 SSA1 SSA2             
093100     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
093200     PERFORM IMS-STATUSCHECK                                              
093300     .                                                                    
093400     EJECT                                                                
093500 IMS-ISRT-2214 SECTION.                                                   
093600*                                                                         
093700     STRING 'WLXXBI01(WDG3KEY  =' W-WDGX2213-X ')'                        
093800             DELIMITED BY SIZE INTO SSA1                                  
093900     MOVE 'WLXXBI11 ' TO SSA2                                             
094000     MOVE '  IIGE' TO GOOD-STATUSCODES                                    
094100     CALL CBLTDLI USING ISRT XXBI-PCB DLI-IO-WDGX2214 SSA1 SSA2           
094200     MOVE XXBI-STATUS-CODE TO STATUS-WS                                   
094300     PERFORM IMS-STATUSCHECK                                              
094400     .                                                                    
094500     EJECT                                                                
094600 IMS-STATUSCHECK SECTION.                                                 
094700                                                                          
094800     SET STATUS-IX TO 1                                                   
094900     SEARCH GOOD-STATUS                                                   
095000       AT END                                                             
095100         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
095200         DELIMITED BY SIZE INTO ERROR-TEXT                                
095300         CALL FELLOG                                                      
095400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
095500         CONTINUE                                                         
095600     END-SEARCH                                                           
095700     .                                                                    
