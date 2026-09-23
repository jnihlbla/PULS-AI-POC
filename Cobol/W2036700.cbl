000100 ID DIVISION.                                                             
000201 PROGRAM-ID.     W2036700.                                                
000301 AUTHOR.         UMESH JAIN.                                              
000401 DATE-WRITTEN.   11/05/03.                                                
000501 DATE-COMPILED.                                                           
000601                                                                          
000701*    FUNCTION:                                                            
000801*        STOCKING STEERING FOR ALL DC'S EXCEPT CDC                        
000901*                                                                         
001001*        THE PROGRAM READS     WDB601                                     
001002*        THE PROGRAM UPDATES   WDB614                                     
001003*                              WDB615                                     
001101*                                                                         
001201*    INDATA.                                                              
001301*        TRANSACTION: W2T367                                              
001401*                     W2T367U                                             
001501*        MID:         W2I36701                                            
001601*                                                                         
001701*    OUTDATA.                                                             
001801*        MOD:         W2O36701                                            
001802*                                                                         
001803*****************************************************************         
001804* 2011-09-07  SO  E'TRACKER 10148137 RÄTTAT FEL VID UPPDATERING           
001805*                                                                         
001806* 2011-09-15  SO  E'TRACKER 10148905 RÄTTAT FEL BILD 2366 + 2367          
001807*                                                                         
001808* 2012-05-23  SO  E'TRACKER 10151888 SEPARATA FÄLT 2366 2367              
001809*                                    IDUSER OCH TIUPPDAT PÅ WDB6.         
001810*                                                                         
001901                                                                          
002001 ENVIRONMENT DIVISION.                                                    
002101                                                                          
002201 DATA DIVISION.                                                           
002301     EJECT                                                                
002401 WORKING-STORAGE SECTION.                                                 
002501 77  IDPGM                       PIC X(08)   VALUE 'W2036700'.            
002601                                                                          
002701*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
002801 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
002901                                                                          
003001 77  YES                         PIC X       VALUE 'Y'.                   
003101 77  NOO                         PIC X       VALUE 'N'.                   
003201 77  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
003301 77  IX                          PIC S9(9)   VALUE ZERO.                  
003401 77  IX1                         PIC S9(9)   VALUE ZERO.                  
003501 77  TAB-IX1                     PIC S9(9)   VALUE ZERO.                  
003601 77  MAX-IX                      PIC S9(9)   VALUE +13.                   
003701 77  MAX-IX1                     PIC S9(9)   VALUE +11.                   
003801 77  W-CMD-CNT-E                 PIC S9(9)   VALUE ZERO.                  
003901 77  W-CMD-CNT-D                 PIC S9(9)   VALUE ZERO.                  
004001 77  W-PAGE-FULL                 PIC X       VALUE 'N'.                   
004101 77  UPDATE-DATA                 PIC X       VALUE 'N'.                   
004201 77  ERR-UPDATE                  PIC X       VALUE 'N'.                   
004202 77  DUP-KDPRODSL                PIC X       VALUE 'N'.                   
004203 77  W-PREV-KDPRODSL             PIC S9(3)   VALUE ZERO.                  
004300                                                                          
004400*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004500                                                                          
004600 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
004700     88  INDATA-OK                           VALUE 'Y'.                   
004800     88  INDATA-WRONG                        VALUE 'N'.                   
004900                                                                          
005000 77  KEYS-SW                     PIC X       VALUE 'Y'.                   
005100     88  KEYS-OK                             VALUE 'Y'.                   
005200     88  KEYS-WRONG                          VALUE 'N'.                   
005300                                                                          
005400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005501     88  OWN-MID                             VALUE '2367'.                
005600     88  GOOD-MID                            VALUE '2361' '2362'          
005700                                                   '2363' '2364'          
005801                                                   '2365' '2366'          
005901                                                   '2367' '2368'          
006001                                                   '2369'.                
006100     88  HELP-MID                            VALUE '0551'.                
006200*                                                                         
006301* ---PARAMETERS FOR SUBPROGRAM WINTSORT                                   
006401* ---TO SORT A TABLE INTERNALLY                                           
006501 01  TABENTRY-PARM.                                                       
006601     03  STEGLAANGD              PIC S9(9) COMP.                          
006701     03  ANTAL                   PIC S9(9) COMP.                          
006801     03  NYCKELLAANGD            PIC S9(9) COMP.                          
006901 01  SORT-TABELL.                                                         
007001     03  TAB-RAD OCCURS 11.                                               
007101        05  TAB-SORT-BEGREPP.                                             
007201            07  TAB-KDPRODSL    PIC 9(2).                                 
007301*                                                                         
007302 01  W-KDPRODSL-TABELL.                                                   
007303     03 W-KDPRODSL         OCCURS 11 TIMES                                
007304                             PIC S9(3)           COMP-3.                  
007305                                                                          
007400*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
007500 01  GENERAL-SUBPROGRAMS.                                                 
007600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008000     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR '.            
008100*                                                                         
008200*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
008300*01 -COPY WMEDAREA                                                        
008400     SKIP3                                                                
008500 01  MESSAGE-CODES.                                                       
008600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
008700     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008800     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008900     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
009000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009101     03  ERR-WRONG-DC            PIC X(3)    VALUE '440'.                 
009200     03  INF-NO-RECORDS          PIC X(3)    VALUE '010'.                 
009301     03  ERR-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '007'.                 
009302     03  ERR-DUPLICATE-KDPRODSL  PIC X(3)    VALUE '241'.                 
009401     03  ERR-DC-MISSING          PIC X(3)    VALUE '026'.                 
009402     03  INF-NO-UPDATE-DONE      PIC X(3)    VALUE '034'.                 
009500*                                                                         
009600*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
009700*                                                                         
009800 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
009900*                                                                         
010000*01 -COPY WMSGINIT                                                        
010100     EJECT                                                                
010200*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
010300*                                                                         
010400 01  SAVE-AREA.                                                           
010501     03  SAVE-IDTRANS           PIC X(4)    VALUE '2367'.                 
010600     EJECT                                                                
010700*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
010800*                                                                         
010900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011000     SKIP3                                                                
011101*01  MID -COPY W2I36701                                                   
011200     EJECT                                                                
011300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011400     SKIP3                                                                
011500*01  -COPY WMSGAREA                                                       
011600     EJECT                                                                
011700     03  MOD REDEFINES MSG-AREA.                                          
011801*      05  -COPY W2O36701                                                 
011900     EJECT                                                                
012000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012100     SKIP3                                                                
012200*01  -COPY WMFSAREA                                                       
012300     EJECT                                                                
012400*    --- WORK-AREAS FOR IMS-SECTIONS                                      
012500*                                                                         
012600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012700*                                                                         
012800 01  KEYS-FOR-DLI.                                                        
012900     03  W-IDDC-X.                                                        
013000         05  W-IDDC              PIC X(02)    VALUE SPACE.                
013100*                                                                         
013200     03  W-WDB614KY-X.                                                    
013302         05  W-AKT-KVAKT             PIC S9(3) VALUE 0 COMP-3.            
013400         05  W-AKT-KVVECKOR-PUBV     PIC S9(3) VALUE 0 COMP-3.            
013500         05  W-AKT-PRARTSTD          PIC 9(7)  VALUE 0.                   
013600         05  W-AKT-VLARTNTO          PIC 9(8)  VALUE 0.                   
013703         05  W-AKT-KVPB-SEP-REF      PIC S9(7) VALUE 0 COMP-3.            
013801         05  W-AKT-KDPRODSL          PIC S9(3) VALUE 0 COMP-3.            
013900*                                                                         
013910     03  W-IDTRANS-B6-X.                                                  
013920         05  W-IDTRANS-B6        PIC X(4)  VALUE '2367'.                  
013930*                                                                         
014000*    --- STATUS CODES FROM IMS                                            
014100 01  STATUS-WS                   PIC XX.                                  
014200     88  SEGMENT-FOUND                       VALUE '  '.                  
014300     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
014400     88  SEGMENT-MISSING                     VALUE 'GE'.                  
014500*                                                                         
014600 01  GOOD-STATUSCODES.                                                    
014700     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014800*                                                                         
014900 01  SSA1                        PIC X(64).                               
015000 01  SSA2                        PIC X(64).                               
015100*                                                                         
015200*    --- IMS FUNCTION CODES                                               
015300*01  -COPY W0003                                                          
015400*                                                                         
015500*    ---  DLI INPUT-OUTPUT AREA                                           
015600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
015700 01  DLI-IO-WDB601.                                                       
015800*    03  -COPY WDB601                                                     
015900*                                                                         
016000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB614'.                      
016100 01  DLI-IO-WDB614.                                                       
016200*    03  -COPY WDB614                                                     
016300*                                                                         
016320 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB615'.                      
016330 01  DLI-IO-WDB615.                                                       
016340*    03  -COPY WDB615                                                     
016350*                                                                         
016400 LINKAGE SECTION.                                                         
016500*01  -COPY W0009   -PRE MSG-                                              
016600                                                                          
016700*01  -COPY W0008   -PRE WDP7-                                             
016800     05  FILLER                  PIC X.                                   
016900                                                                          
017000*01  -COPY W0008  -PRE WDB6-                                              
017100     05  FILLER                  PIC X.                                   
017200     EJECT                                                                
017300 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDB6-PCB.                     
017400 MAIN SECTION.                                                            
017500     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDB6-PCB.                     
017600                                                                          
017700     PERFORM IMS-GET-MSG                                                  
017800     IF SEGMENT-FOUND                                                     
017900       PERFORM A-INIT                                                     
018000       PERFORM B-CHECK-KEYS                                               
018100       IF KEYS-OK                                                         
018200         IF MFS-UPDATE                                                    
018300           PERFORM G-CHECK-INPUT                                          
018400           IF INDATA-OK AND W-PAGE-FULL = 'N'                             
018500             PERFORM H-UPDATE                                             
018600           END-IF                                                         
018700         ELSE                                                             
018800           IF MFS-FIRST                                                   
018900             PERFORM C-FIRST-PAGE                                         
019000           ELSE                                                           
019100             PERFORM E-SAME-PAGE                                          
019200           END-IF                                                         
019300         END-IF                                                           
019400         PERFORM F-READ-SHOW-INFO                                         
019500       END-IF                                                             
019600*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
019700*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
019801       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O36701 + 4                      
019900       PERFORM IMS-INSERT-MSG                                             
020000     END-IF                                                               
020100                                                                          
020200     MOVE ZERO TO RETURN-CODE                                             
020300     GOBACK                                                               
020400     .                                                                    
020500                                                                          
020600 A-INIT SECTION.                                                          
020700     IF MSG-DOUBLE-TRANSACTIONS                                           
020801       MOVE MSG-INDATA-MINUS-2-TRANSACT  TO MID-W2I36701                  
020900       MOVE MSG-IDTRANS-2                TO MFS-IDTRANS                   
021000       MOVE MSG-KDMFSFOR-2               TO MFS-KDMFSFOR                  
021100     ELSE                                                                 
021201       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W2I36701                  
021300       MOVE MSG-IDTRANS-1                TO MFS-IDTRANS                   
021400       MOVE MSG-KDMFSFOR-1               TO MFS-KDMFSFOR                  
021500     END-IF                                                               
021600                                                                          
021700     MOVE MSG-KDTRTYP      TO MFS-KDTRTYP                                 
021800     MOVE MSG-IDPFK        TO MFS-IDPFK                                   
021900     MOVE MFS-IDTRANS      TO W-IDTRANS                                   
022000     MOVE LOW-VALUE        TO MSG-AREA                                    
022101     MOVE 'W2O367N1'       TO MFS-IDMOD                                   
022201     MOVE '2367'           TO MOD-IDTRANS                                 
022300     MOVE MFS-ERASE-FIELD  TO MOD-TEMFSFEL MOD-TEMFSINF                   
022400                                                                          
022500     IF OWN-MID OR HELP-MID                                               
022600       CONTINUE                                                           
022700     ELSE                                                                 
022800       MOVE SPACE TO MFS-KDTRTYP                                          
022900       MOVE '7'   TO MFS-IDPFK                                            
023000     END-IF                                                               
023100                                                                          
023200     ACCEPT TODAYS-DATE FROM DATE                                         
023300     .                                                                    
023310                                                                          
023400 B-CHECK-KEYS SECTION.                                                    
023500     MOVE ALL '+'           TO MSGI-WMSGINIT                              
023600     MOVE '001'             TO MSGI-KDCALL                                
023700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
023800     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
023901     MOVE '2367'            TO MSGI-IDTRANS                               
024010     IF OWN-MID                                                           
024100       MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                              
024200     END-IF                                                               
024300     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
024400                                                                          
024500*    - LANGUAGE TO BE USED BY MEDKONV                                     
024600     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
024700*                                                                         
024800     MOVE YES TO KEYS-SW                                                  
024900*                                                                         
025000*    -- CHECK OF IDDC                                                     
025100     MOVE MFS-ERASE-FIELD   TO MOD-IDDC-IN                                
025200                                                                          
025301     MOVE MSGI-IDDC-KEY     TO W-IDDC                                     
025400     IF MID-IDDC-IN NOT = ALL '+'                                         
025501       PERFORM IMS-GU-WDB601                                              
025601       IF SEGMENT-MISSING OR DCS-CDC-TR OR DCS-DDC                        
025701         MOVE NOO            TO KEYS-SW                                   
025801         MOVE ERR-WRONG-DC   TO MED-IDMFSFEL                              
025901         CALL WMEDKONV USING MED-WMEDAREA                                 
026001         MOVE MED-MFSFEL     TO MOD-TEMFSFEL                              
026101         PERFORM MFS-ERASE-FIELD-IN                                       
026201         PERFORM MFS-ERASE-FIELD-OUT                                      
026301         PERFORM MFS-CLOSE-FIELD-IN                                       
026401       ELSE                                                               
026501         MOVE '7'           TO MFS-IDPFK                                  
026601         MOVE SPACE         TO MFS-KDTRTYP                                
026701       END-IF                                                             
026800     END-IF                                                               
026900                                                                          
027000     IF GOOD-MID OR KEYS-OK                                               
027100       MOVE MSGI-IDDC-KEY   TO MOD-IDDC-UT                                
027200     ELSE                                                                 
027300       MOVE MFS-ERASE-FIELD TO MOD-IDDC-UT                                
027400     END-IF                                                               
028300     .                                                                    
028402                                                                          
028500 C-FIRST-PAGE SECTION.                                                    
028600     PERFORM MFS-ERASE-FIELD-IN                                           
028700     .                                                                    
028802                                                                          
028900 E-SAME-PAGE SECTION.                                                     
029001     IF OWN-MID OR HELP-MID                                               
029101       IF MID-KDCMDVAL (1)  = ALL '+'  AND                                
029201          MID-KDCMDVAL (2)  = ALL '+'  AND                                
029301          MID-KDCMDVAL (3)  = ALL '+'  AND                                
029401          MID-KDCMDVAL (4)  = ALL '+'  AND                                
029501          MID-KDCMDVAL (5)  = ALL '+'  AND                                
029601          MID-KDCMDVAL (6)  = ALL '+'  AND                                
029701          MID-KDCMDVAL (7)  = ALL '+'  AND                                
029801          MID-KDCMDVAL (8)  = ALL '+'  AND                                
029901          MID-KDCMDVAL (9)  = ALL '+'  AND                                
030001          MID-KDCMDVAL (10) = ALL '+'  AND                                
030101          MID-KDCMDVAL (11) = ALL '+'  AND                                
030201          MID-KDCMDVAL (12) = ALL '+'  AND                                
030202          MID-KDCMDVAL (13) = ALL '+'  AND                                
030402          MID-KVAKT-UP           = ALL '+'  AND                           
030501          MID-KVVECKOR-PUBV-UP   = ALL '+'  AND                           
030601          MID-PRARTSTD-UP        = ALL '+'  AND                           
030701          MID-VLARTNTO-UP        = ALL '+'  AND                           
030802          MID-KVPB-SEP-REF-UP    = ALL '+'  AND                           
030902          MID-KDPRODSL-UP (1)    = ALL '+'  AND                           
031002          MID-KDPRODSL-UP (2)    = ALL '+'  AND                           
031102          MID-KDPRODSL-UP (3)    = ALL '+'  AND                           
031202          MID-KDPRODSL-UP (4)    = ALL '+'  AND                           
031302          MID-KDPRODSL-UP (5)    = ALL '+'  AND                           
031402          MID-KDPRODSL-UP (6)    = ALL '+'  AND                           
031502          MID-KDPRODSL-UP (7)    = ALL '+'  AND                           
031602          MID-KDPRODSL-UP (8)    = ALL '+'  AND                           
031702          MID-KDPRODSL-UP (9)    = ALL '+'  AND                           
031802          MID-KDPRODSL-UP (10)   = ALL '+'  AND                           
031902          MID-KDPRODSL-UP (11)   = ALL '+'                                
032002         PERFORM MFS-ERASE-FIELD-IN                                       
032102       ELSE                                                               
032202         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
032302         CALL WMEDKONV USING MED-WMEDAREA                                 
032402         MOVE MED-MFSINF     TO MOD-TEMFSINF                              
032502         PERFORM EA-MID-INDATA-TO-MOD                                     
032602       END-IF                                                             
032702     ELSE                                                                 
032802       PERFORM MFS-ERASE-FIELD-IN                                         
032902     END-IF                                                               
033002     .                                                                    
033102                                                                          
033202 EA-MID-INDATA-TO-MOD SECTION.                                            
033302     MOVE 1 TO IX                                                         
033402     PERFORM UNTIL IX > MAX-IX                                            
033502       IF MID-KVAKT (IX) = ALL '+'                                        
033602         MOVE MFS-ERASE-FIELD     TO MOD-KVAKT (IX)                       
033702       ELSE                                                               
033802         MOVE MID-KVAKT (IX)      TO MOD-KVAKT (IX)                       
033902                                                                          
034002       END-IF                                                             
034102                                                                          
034202       IF MID-KVVECKOR-PUBV (IX) = ALL '+'                                
034302         MOVE MFS-ERASE-FIELD     TO MOD-KVVECKOR-PUBV (IX)               
034402       ELSE                                                               
034502         MOVE MID-KVVECKOR-PUBV (IX)                                      
034602                                  TO MOD-KVVECKOR-PUBV (IX)               
034702       END-IF                                                             
034802                                                                          
034902       IF MID-PRARTSTD (IX) = ALL '+'                                     
035002         MOVE MFS-ERASE-FIELD     TO MOD-PRARTSTD (IX)                    
035102       ELSE                                                               
035202         MOVE MID-PRARTSTD (IX)   TO MOD-PRARTSTD (IX)                    
035302       END-IF                                                             
035402                                                                          
035502       IF MID-VLARTNTO (IX) = ALL '+'                                     
035602         MOVE MFS-ERASE-FIELD     TO MOD-VLARTNTO (IX)                    
035702       ELSE                                                               
035802         MOVE MID-VLARTNTO (IX)   TO MOD-VLARTNTO (IX)                    
035902       END-IF                                                             
036002                                                                          
036102       IF MID-KVPB-SEP-REF (IX) = ALL '+'                                 
036202         MOVE MFS-ERASE-FIELD       TO MOD-KVPB-SEP-REF (IX)              
036302       ELSE                                                               
036402         MOVE MID-KVPB-SEP-REF (IX) TO MOD-KVPB-SEP-REF (IX)              
036502       END-IF                                                             
036602                                                                          
036702       MOVE 1 TO IX1                                                      
036802       PERFORM UNTIL IX1 > MAX-IX1                                        
036902         IF MID-KDPRODSL (IX IX1) = ALL '+'                               
037002           MOVE MFS-ERASE-FIELD   TO MOD-KDPRODSL (IX IX1)                
037102         ELSE                                                             
037202           MOVE MID-KDPRODSL (IX IX1)                                     
037302                                  TO MOD-KDPRODSL (IX IX1)                
037402         END-IF                                                           
037502         ADD 1 TO IX1                                                     
037602       END-PERFORM                                                        
037702                                                                          
037802       ADD 1 TO IX                                                        
037902     END-PERFORM                                                          
038002*                                                                         
038102     MOVE +1 TO IX                                                        
038202     PERFORM UNTIL IX > MAX-IX                                            
038302       IF MID-KDCMDVAL (IX) NOT = ALL '+'                                 
038402         MOVE MID-KDCMDVAL (IX)     TO MOD-KDCMDVAL (IX)                  
038502         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMDVAL-ATTR (IX)             
038602       ELSE                                                               
038702         MOVE MFS-ERASE-FIELD       TO MOD-KDCMDVAL (IX)                  
038802       END-IF                                                             
038902       ADD +1 TO IX                                                       
039002     END-PERFORM                                                          
039102                                                                          
039202     IF MID-KVAKT-UP NOT = ALL '+'                                        
039302       MOVE MID-KVAKT-UP          TO MOD-KVAKT-UP                         
039502       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVAKT-UP-ATTR                    
039602     ELSE                                                                 
039702       MOVE MFS-ERASE-FIELD       TO MOD-KVAKT-UP                         
039802     END-IF                                                               
039902                                                                          
040002     IF MID-KVVECKOR-PUBV-UP NOT = ALL '+'                                
040102       MOVE MID-KVVECKOR-PUBV-UP  TO MOD-KVVECKOR-PUBV-UP                 
040202       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVVECKOR-PUBV-UP-ATTR            
040302     ELSE                                                                 
040402       MOVE MFS-ERASE-FIELD       TO MOD-KVVECKOR-PUBV-UP                 
040502     END-IF                                                               
040602                                                                          
040702     IF MID-PRARTSTD-UP NOT = ALL '+'                                     
040802       MOVE MID-PRARTSTD-UP       TO MOD-PRARTSTD-UP                      
040902       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-PRARTSTD-UP-ATTR                 
041002     ELSE                                                                 
041102       MOVE MFS-ERASE-FIELD       TO MOD-PRARTSTD-UP                      
041202     END-IF                                                               
041302                                                                          
041402     IF MID-VLARTNTO-UP NOT = ALL '+'                                     
041502       MOVE MID-VLARTNTO-UP       TO MOD-VLARTNTO-UP                      
041602       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-VLARTNTO-UP-ATTR                 
041702     ELSE                                                                 
041802       MOVE MFS-ERASE-FIELD       TO MOD-VLARTNTO-UP                      
041902     END-IF                                                               
042002                                                                          
042102     IF MID-KVPB-SEP-REF-UP NOT = ALL '+'                                 
042202       MOVE MID-KVPB-SEP-REF-UP   TO MOD-KVPB-SEP-REF-UP                  
042302       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVPB-SEP-REF-UP-ATTR             
042402     ELSE                                                                 
042502       MOVE MFS-ERASE-FIELD       TO MOD-KVPB-SEP-REF-UP                  
042602     END-IF                                                               
042702                                                                          
042802     MOVE 1 TO IX1                                                        
042902     PERFORM UNTIL IX1 > MAX-IX1                                          
043002       IF MID-KDPRODSL-UP (IX1) NOT = ALL '+'                             
043102         MOVE MID-KDPRODSL-UP (IX1) TO MOD-KDPRODSL-UP (IX1)              
043202         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDPRODSL-UP-ATTR (IX1)         
043302       ELSE                                                               
043402         MOVE MFS-ERASE-FIELD       TO MOD-KDPRODSL-UP (IX1)              
043502       END-IF                                                             
043602       ADD 1 TO IX1                                                       
043702     END-PERFORM                                                          
043802     .                                                                    
043902                                                                          
044002 F-READ-SHOW-INFO SECTION.                                                
044102     PERFORM IMS-GU-WDB601                                                
044202                                                                          
044302     IF SEGMENT-MISSING OR DCS-CDC-TR OR DCS-DDC                          
044402       MOVE ERR-WRONG-DC   TO MED-IDMFSFEL                                
044502       CALL WMEDKONV USING MED-WMEDAREA                                   
044602       MOVE MED-MFSFEL     TO MOD-TEMFSFEL                                
044702       PERFORM MFS-ERASE-FIELD-OUT                                        
044802       PERFORM MFS-CLOSE-FIELD-IN                                         
044902     ELSE                                                                 
045202       PERFORM IMS-GNP-WDB614                                             
045302       MOVE +1 TO IX                                                      
045402       PERFORM UNTIL IX > MAX-IX                                          
045502         IF SEGMENT-FOUND                                                 
045602           PERFORM FA-MOVE-DATA-TO-MOD                                    
045702           PERFORM IMS-GNP-WDB614                                         
045802         ELSE                                                             
045902           PERFORM MFS-CLOSE-ERASE-FIELD                                  
046002         END-IF                                                           
046102         ADD +1 TO IX                                                     
046202       END-PERFORM                                                        
046203                                                                          
046205       PERFORM IMS-GNP-WDB615                                             
046206       IF SEGMENT-FOUND                                                   
046207         MOVE LOGG-TIUPPDAT   TO MOD-TIUPPDAT                             
046208         MOVE LOGG-IDUSER     TO MOD-IDUSER                               
046209       ELSE                                                               
046210         MOVE ZERO            TO MOD-TIUPPDAT                             
046220         MOVE SPACE           TO MOD-IDUSER                               
046230       END-IF                                                             
046302     END-IF                                                               
046402     .                                                                    
046502                                                                          
046602 FA-MOVE-DATA-TO-MOD  SECTION.                                            
046702     MOVE AKT-KVAKT            TO MOD-KVAKT           (IX)                
046802     MOVE AKT-KVVECKOR-PUBV    TO MOD-KVVECKOR-PUBV   (IX)                
046902     MOVE AKT-PRARTSTD         TO MOD-PRARTSTD        (IX)                
047002     MOVE AKT-VLARTNTO         TO MOD-VLARTNTO        (IX)                
047102     MOVE AKT-KVPB-SEP-REF     TO MOD-KVPB-SEP-REF    (IX)                
047202     MOVE +1 TO IX1                                                       
047302     PERFORM UNTIL IX1 > MAX-IX1                                          
047402       MOVE AKT-KDPRODSL (IX1) TO MOD-KDPRODSL   (IX IX1)                 
047502       ADD +1 TO IX1                                                      
047602     END-PERFORM                                                          
047702     .                                                                    
047802                                                                          
047902 G-CHECK-INPUT SECTION.                                                   
048002     MOVE YES  TO INDATA-SW                                               
048102     IF (MID-KVAKT-UP         = ALL '+' ) AND                             
048202        (MID-KVVECKOR-PUBV-UP = ALL '+' ) AND                             
048302        (MID-PRARTSTD-UP      = ALL '+' ) AND                             
048402        (MID-VLARTNTO-UP      = ALL '+' ) AND                             
048502        (MID-KVPB-SEP-REF-UP  = ALL '+' ) AND                             
048602        (MID-KDPRODSL-UP (1)  = ALL '+' ) AND                             
048702        (MID-KDPRODSL-UP (2)  = ALL '+' ) AND                             
048802        (MID-KDPRODSL-UP (3)  = ALL '+' ) AND                             
048902        (MID-KDPRODSL-UP (4)  = ALL '+' ) AND                             
049002        (MID-KDPRODSL-UP (5)  = ALL '+' ) AND                             
049102        (MID-KDPRODSL-UP (6)  = ALL '+' ) AND                             
049202        (MID-KDPRODSL-UP (7)  = ALL '+' ) AND                             
049302        (MID-KDPRODSL-UP (8)  = ALL '+' ) AND                             
049402        (MID-KDPRODSL-UP (9)  = ALL '+' ) AND                             
049502        (MID-KDPRODSL-UP (10) = ALL '+' ) AND                             
049602        (MID-KDPRODSL-UP (11) = ALL '+' ) AND                             
049702        (MID-KDCMDVAL (1)     = ALL '+' ) AND                             
049802        (MID-KDCMDVAL (2)     = ALL '+' ) AND                             
049902        (MID-KDCMDVAL (3)     = ALL '+' ) AND                             
050002        (MID-KDCMDVAL (4)     = ALL '+' ) AND                             
050102        (MID-KDCMDVAL (5)     = ALL '+' ) AND                             
050202        (MID-KDCMDVAL (6)     = ALL '+' ) AND                             
050302        (MID-KDCMDVAL (7)     = ALL '+' ) AND                             
050402        (MID-KDCMDVAL (8)     = ALL '+' ) AND                             
050502        (MID-KDCMDVAL (9)     = ALL '+' ) AND                             
050602        (MID-KDCMDVAL (10)    = ALL '+' ) AND                             
050702        (MID-KDCMDVAL (11)    = ALL '+' ) AND                             
050802        (MID-KDCMDVAL (12)    = ALL '+' ) AND                             
050803        (MID-KDCMDVAL (13)    = ALL '+' )                                 
051002       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
051102       CALL WMEDKONV USING MED-WMEDAREA                                   
051202       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
051302       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
051402       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
051502       MOVE NOO TO INDATA-SW                                              
051602     ELSE                                                                 
051702       PERFORM GA-VALIDATE-INPUT-FIELDS                                   
051802       IF INDATA-WRONG                                                    
051902         IF ERR-UPDATE = YES                                              
052002           MOVE ERR-UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                    
052102         ELSE                                                             
052103           IF DUP-KDPRODSL = YES                                          
052104             MOVE ERR-DUPLICATE-KDPRODSL TO MED-IDMFSFEL                  
052105           ELSE                                                           
052202             MOVE ERR-CORR-HILITE-FLDS   TO MED-IDMFSFEL                  
052303           END-IF                                                         
052304         END-IF                                                           
052402         CALL WMEDKONV USING MED-WMEDAREA                                 
052502         MOVE MED-MFSFEL           TO MOD-TEMFSFEL                        
052602         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
052702         PERFORM MFS-DONT-TOUCH-FIELD-IN                                  
052802       ELSE                                                               
052902         IF W-PAGE-FULL = 'Y'                                             
053002           MOVE ERR-UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                    
053102           CALL WMEDKONV USING MED-WMEDAREA                               
053202           MOVE MED-MFSFEL           TO MOD-TEMFSFEL                      
053302           PERFORM MFS-DONT-TOUCH-FIELD-OUT                               
053402           PERFORM MFS-DONT-TOUCH-FIELD-IN                                
053502         END-IF                                                           
053602       END-IF                                                             
053702     END-IF                                                               
053802     .                                                                    
053902                                                                          
054002 GA-VALIDATE-INPUT-FIELDS SECTION.                                        
054102**   VALIDATE ORDER HITS                                                  
054202     IF INDATA-OK                                                         
054403       IF MID-KVAKT-UP NOT = ALL '+'                                      
054502         IF MID-KVAKT-UP NOT NUMERIC                                      
054602           MOVE MID-KVAKT-UP        TO MOD-KVAKT-UP                       
054802           MOVE MFS-NUM-FIELD-WRONG TO MOD-KVAKT-UP-ATTR                  
055002           MOVE NOO                 TO INDATA-SW                          
055102         ELSE                                                             
055202           IF MID-KVAKT-UP = 0                                            
055402             MOVE MID-KVAKT-UP        TO MOD-KVAKT-UP                     
055602             MOVE MFS-NUM-FIELD-WRONG TO MOD-KVAKT-UP-ATTR                
055802             MOVE NOO                 TO INDATA-SW                        
055902           ELSE                                                           
056002             MOVE MFS-NUM-FIELD-OK TO MOD-KVAKT-UP-ATTR                   
056102           END-IF                                                         
056202         END-IF                                                           
056302       END-IF                                                             
056402     END-IF                                                               
056502                                                                          
056602     MOVE NOO                    TO UPDATE-DATA                           
056702                                    ERR-UPDATE                            
056802**   VALIDATE PUBLICATION WEEK                                            
056902     IF INDATA-OK                                                         
057103       IF MID-KVVECKOR-PUBV-UP NOT = ALL '+'                              
057202         IF MID-KVVECKOR-PUBV-UP NOT NUMERIC                              
057302           MOVE MID-KVVECKOR-PUBV-UP                                      
057402                                 TO MOD-KVVECKOR-PUBV-UP                  
057502           MOVE MFS-NUM-FIELD-WRONG                                       
057602                                 TO MOD-KVVECKOR-PUBV-UP-ATTR             
057702           MOVE NOO              TO INDATA-SW                             
057802         ELSE                                                             
057803           IF MID-KVVECKOR-PUBV-UP = 0                                    
057804             MOVE MID-KVVECKOR-PUBV-UP                                    
057805                                   TO MOD-KVVECKOR-PUBV-UP                
057806             MOVE MFS-NUM-FIELD-WRONG                                     
057807                                   TO MOD-KVVECKOR-PUBV-UP-ATTR           
057808             MOVE NOO              TO INDATA-SW                           
057809           ELSE                                                           
057902             MOVE YES              TO UPDATE-DATA                         
058002             MOVE MFS-NUM-FIELD-OK TO MOD-KVVECKOR-PUBV-UP-ATTR           
058102           END-IF                                                         
058103         END-IF                                                           
058202       END-IF                                                             
058302     END-IF                                                               
058402                                                                          
058502**   VALIDATE STANDARD PRICE                                              
058602     IF INDATA-OK                                                         
058802       IF MID-PRARTSTD-UP NOT = ALL '+'                                   
058902         IF MID-PRARTSTD-UP NOT NUMERIC                                   
059002           MOVE MID-PRARTSTD-UP  TO MOD-PRARTSTD-UP                       
059102           MOVE MFS-NUM-FIELD-WRONG                                       
059202                                 TO MOD-PRARTSTD-UP-ATTR                  
059302           MOVE NOO              TO INDATA-SW                             
059402         ELSE                                                             
059502           MOVE MFS-NUM-FIELD-OK TO MOD-PRARTSTD-UP-ATTR                  
059602           MOVE YES              TO UPDATE-DATA                           
059702         END-IF                                                           
059802       END-IF                                                             
059902     END-IF                                                               
060002                                                                          
060102**   VALIDATE VOLUME                                                      
060202     IF INDATA-OK                                                         
060402       IF MID-VLARTNTO-UP NOT = ALL '+'                                   
060502         IF MID-VLARTNTO-UP NOT NUMERIC                                   
060602           MOVE MID-VLARTNTO-UP  TO MOD-VLARTNTO-UP                       
060702           MOVE MFS-NUM-FIELD-WRONG                                       
060802                                 TO MOD-VLARTNTO-UP-ATTR                  
060902           MOVE NOO              TO INDATA-SW                             
061002         ELSE                                                             
061003           IF MID-VLARTNTO-UP = 0                                         
061004             MOVE MID-VLARTNTO-UP  TO MOD-VLARTNTO-UP                     
061005             MOVE MFS-NUM-FIELD-WRONG                                     
061006                                   TO MOD-VLARTNTO-UP-ATTR                
061007             MOVE NOO              TO INDATA-SW                           
061008           ELSE                                                           
061102             MOVE MFS-NUM-FIELD-OK TO MOD-VLARTNTO-UP-ATTR                
061202             MOVE YES              TO UPDATE-DATA                         
061302           END-IF                                                         
061303         END-IF                                                           
061402       END-IF                                                             
061502     END-IF                                                               
062001                                                                          
062102**   VALIDATE GLOBAL F/C                                                  
062202     IF INDATA-OK                                                         
062402       IF MID-KVPB-SEP-REF-UP NOT = ALL '+'                               
062502         IF MID-KVPB-SEP-REF-UP NOT NUMERIC                               
062602           MOVE MID-KVPB-SEP-REF-UP TO MOD-KVPB-SEP-REF-UP                
062702           MOVE MFS-NUM-FIELD-WRONG                                       
062802                                 TO MOD-KVPB-SEP-REF-UP-ATTR              
062902           MOVE NOO              TO INDATA-SW                             
063002         ELSE                                                             
063003           IF MID-KVPB-SEP-REF-UP = 0                                     
063004             MOVE MID-KVPB-SEP-REF-UP TO MOD-KVPB-SEP-REF-UP              
063005             MOVE MFS-NUM-FIELD-WRONG                                     
063006                                   TO MOD-KVPB-SEP-REF-UP-ATTR            
063007             MOVE NOO              TO INDATA-SW                           
063008           ELSE                                                           
063102             MOVE MFS-NUM-FIELD-OK TO MOD-KVPB-SEP-REF-UP-ATTR            
063202             MOVE YES              TO UPDATE-DATA                         
063302           END-IF                                                         
063303         END-IF                                                           
063402       END-IF                                                             
063502     END-IF                                                               
063602                                                                          
063702**   VALIDATE KDPRODSL                                                    
063802     MOVE +1 TO IX1                                                       
063902     PERFORM UNTIL IX1 > MAX-IX1 OR INDATA-WRONG                          
064102       IF MID-KDPRODSL-UP (IX1) NOT = ALL '+'                             
064202         IF MID-KDPRODSL-UP (IX1) NOT NUMERIC                             
064302           MOVE MID-KDPRODSL-UP (IX1)                                     
064402                                 TO MOD-KDPRODSL-UP (IX1)                 
064502           MOVE MFS-NUM-FIELD-WRONG                                       
064602                                 TO MOD-KDPRODSL-UP-ATTR (IX1)            
064702           MOVE NOO              TO INDATA-SW                             
064802         ELSE                                                             
064803           IF MID-KDPRODSL-UP (IX1) < 11                                  
064804             MOVE MID-KDPRODSL-UP (IX1)                                   
064805                                   TO MOD-KDPRODSL-UP (IX1)               
064806             MOVE MFS-NUM-FIELD-WRONG                                     
064807                                   TO MOD-KDPRODSL-UP-ATTR (IX1)          
064808             MOVE NOO              TO INDATA-SW                           
064809           ELSE                                                           
064902             MOVE MFS-NUM-FIELD-OK TO MOD-KDPRODSL-UP-ATTR (IX1)          
065002             MOVE YES              TO UPDATE-DATA                         
065003           END-IF                                                         
065102         END-IF                                                           
065202       END-IF                                                             
065302       ADD +1 TO IX1                                                      
065402     END-PERFORM                                                          
065502*                                                                         
065503**   CHECK FOR DUPLICATE KDPRODSL                                         
065504     MOVE +1 TO IX1                                                       
065505     PERFORM UNTIL IX1 > MAX-IX1                                          
065506       IF MID-KDPRODSL-UP (IX1) = ALL '+'                                 
065507         MOVE ZERO                  TO TAB-KDPRODSL (IX1)                 
065508       ELSE                                                               
065509         MOVE MID-KDPRODSL-UP (IX1) TO TAB-KDPRODSL (IX1)                 
065510       END-IF                                                             
065512       ADD +1 TO IX1                                                      
065513     END-PERFORM                                                          
065514*                                                                         
065515     PERFORM S01-SORT-TABELL                                              
065516*                                                                         
065517     MOVE +1 TO TAB-IX1                                                   
065518     MOVE +1 TO IX1                                                       
065519     MOVE ZERO TO W-PREV-KDPRODSL                                         
065520     PERFORM UNTIL TAB-IX1 > MAX-IX1                                      
065521       IF TAB-KDPRODSL (TAB-IX1) > 0                                      
065522         MOVE TAB-KDPRODSL (TAB-IX1) TO W-KDPRODSL (IX1)                  
065523         IF W-KDPRODSL (IX1) = W-PREV-KDPRODSL                            
065524           MOVE NOO                 TO INDATA-SW                          
065525           MOVE YES                 TO DUP-KDPRODSL                       
065526           MOVE MFS-ADD-SET-CURSOR  TO MOD-KDPRODSL-UP-ATTR (1)           
065527         END-IF                                                           
065528         MOVE W-KDPRODSL (IX1)      TO W-PREV-KDPRODSL                    
065529         ADD +1 TO IX1                                                    
065530       END-IF                                                             
065531       ADD +1 TO TAB-IX1                                                  
065532     END-PERFORM                                                          
065540*                                                                         
065602     IF INDATA-OK                                                         
065702       IF MID-KDCMDVAL (1)  = ALL '+'  AND                                
065802          MID-KDCMDVAL (2)  = ALL '+'  AND                                
065902          MID-KDCMDVAL (3)  = ALL '+'  AND                                
066002          MID-KDCMDVAL (4)  = ALL '+'  AND                                
066102          MID-KDCMDVAL (5)  = ALL '+'  AND                                
066202          MID-KDCMDVAL (6)  = ALL '+'  AND                                
066302          MID-KDCMDVAL (7)  = ALL '+'  AND                                
066402          MID-KDCMDVAL (8)  = ALL '+'  AND                                
066502          MID-KDCMDVAL (9)  = ALL '+'  AND                                
066602          MID-KDCMDVAL (10) = ALL '+'  AND                                
066702          MID-KDCMDVAL (11) = ALL '+'  AND                                
066802          MID-KDCMDVAL (12) = ALL '+'  AND                                
066803          MID-KDCMDVAL (13) = ALL '+'  AND                                
067002          MID-KVAKT-UP = ALL '+'       AND                                
067102          UPDATE-DATA = YES                                               
067202         MOVE MFS-ALPHA-FIELD-WRONG                                       
067302                             TO MOD-KVAKT-UP-ATTR                         
067402         MOVE NOO            TO INDATA-SW                                 
067502         MOVE YES            TO ERR-UPDATE                                
067602       END-IF                                                             
067702     END-IF                                                               
067802**                                                                        
067902     IF INDATA-OK                                                         
068003       IF (MID-KVAKT-UP NOT = ALL '+') AND                                
068102          MID-KDCMDVAL (1)  = ALL '+'  AND                                
068202          MID-KDCMDVAL (2)  = ALL '+'  AND                                
068302          MID-KDCMDVAL (3)  = ALL '+'  AND                                
068402          MID-KDCMDVAL (4)  = ALL '+'  AND                                
068502          MID-KDCMDVAL (5)  = ALL '+'  AND                                
068602          MID-KDCMDVAL (6)  = ALL '+'  AND                                
068702          MID-KDCMDVAL (7)  = ALL '+'  AND                                
068802          MID-KDCMDVAL (8)  = ALL '+'  AND                                
068902          MID-KDCMDVAL (9)  = ALL '+'  AND                                
069002          MID-KDCMDVAL (10) = ALL '+'  AND                                
069102          MID-KDCMDVAL (11) = ALL '+'  AND                                
069103          MID-KDCMDVAL (12) = ALL '+'  AND                                
069202          MID-KDCMDVAL (13) = ALL '+'                                     
069402**        TO CHECK IF THE PAGE IS ALREADY FULL AND NO MORE                
069502**        INSERTS CAN BE ALLOWED                                          
069602          IF MID-KVAKT (MAX-IX) > SPACES                                  
069702            MOVE YES                 TO W-PAGE-FULL                       
069802          ELSE                                                            
069902            IF MID-KVAKT-UP = ALL '+'                                     
070002              MOVE MFS-ALPHA-FIELD-WRONG                                  
070102                                  TO MOD-KVAKT-UP-ATTR                    
070202              MOVE NOO            TO INDATA-SW                            
070302            END-IF                                                        
070402          END-IF                                                          
070502       END-IF                                                             
070602     END-IF                                                               
070702*                                                                         
070802**   VALIDATE COMMAND CODES                                               
070902     MOVE +1 TO IX                                                        
071002     PERFORM UNTIL IX > MAX-IX OR INDATA-WRONG                            
071102                                                                          
071202       IF MID-KDCMDVAL (IX) NOT = ALL '+'                                 
071402         IF MID-KDCMDVAL (IX) = 'E' OR 'B' OR 'D'                         
072002                                                                          
072003           IF (((MID-KDCMDVAL (IX) = 'B' OR 'D') AND                      
072004                (MID-KVAKT-UP NOT = ALL '+')) OR                          
072005               (MID-KDCMDVAL (IX)     = 'E' AND                           
072006                MID-KVAKT-UP          = ALL '+' AND                       
072007                MID-KVVECKOR-PUBV-UP  = ALL '+' AND                       
072008                MID-PRARTSTD-UP       = ALL '+' AND                       
072009                MID-VLARTNTO-UP       = ALL '+' AND                       
072010                MID-KVPB-SEP-REF-UP   = ALL '+' AND                       
072011                MID-KDPRODSL-UP (01)  = ALL '+' AND                       
072012                MID-KDPRODSL-UP (02)  = ALL '+' AND                       
072013                MID-KDPRODSL-UP (03)  = ALL '+' AND                       
072014                MID-KDPRODSL-UP (04)  = ALL '+' AND                       
072015                MID-KDPRODSL-UP (05)  = ALL '+' AND                       
072016                MID-KDPRODSL-UP (06)  = ALL '+' AND                       
072017                MID-KDPRODSL-UP (07)  = ALL '+' AND                       
072018                MID-KDPRODSL-UP (08)  = ALL '+' AND                       
072019                MID-KDPRODSL-UP (09)  = ALL '+' AND                       
072020                MID-KDPRODSL-UP (10)  = ALL '+' AND                       
072021                MID-KDPRODSL-UP (11)  = ALL '+' ))                        
072023               MOVE MID-KDCMDVAL (IX) TO MOD-KDCMDVAL (IX)                
072024               MOVE MFS-ALPHA-FIELD-WRONG                                 
072025                                      TO MOD-KDCMDVAL-ATTR (IX)           
072026               MOVE NOO               TO INDATA-SW                        
072030           ELSE                                                           
072102             IF MID-KDCMDVAL (IX) = 'E'                                   
072202               ADD +1 TO W-CMD-CNT-E                                      
072302             ELSE                                                         
072402               IF MID-KDCMDVAL (IX) = 'D' OR 'B'                          
072502                 ADD +1 TO W-CMD-CNT-D                                    
072602               END-IF                                                     
072702             END-IF                                                       
072802             IF W-CMD-CNT-E > 1 OR                                        
072902                (W-CMD-CNT-E >= 1 AND W-CMD-CNT-D >= 1)                   
073002               MOVE MID-KDCMDVAL (IX) TO MOD-KDCMDVAL (IX)                
073102               MOVE MFS-ALPHA-FIELD-WRONG                                 
073202                                      TO MOD-KDCMDVAL-ATTR (IX)           
073302               MOVE NOO               TO INDATA-SW                        
073402             ELSE                                                         
073502               MOVE MFS-ALPHA-FIELD-OK TO MOD-KDCMDVAL-ATTR (IX)          
073602             END-IF                                                       
073603           END-IF                                                         
073604         ELSE                                                             
073605           MOVE MID-KDCMDVAL (IX)   TO MOD-KDCMDVAL (IX)                  
073606           MOVE MFS-ALPHA-FIELD-WRONG                                     
073607                                    TO MOD-KDCMDVAL-ATTR (IX)             
073608           MOVE NOO                 TO INDATA-SW                          
073702         END-IF                                                           
073802       END-IF                                                             
073902       ADD +1 TO IX                                                       
074002     END-PERFORM                                                          
074102     .                                                                    
074103                                                                          
074202 H-UPDATE SECTION.                                                        
074302**   CHECK FOR DIFFERENT COMMAND CODES                                    
074402     IF (MID-KVAKT-UP NOT = ALL '+') AND                                  
074502        MID-KDCMDVAL (1)  = ALL '+'  AND                                  
074602        MID-KDCMDVAL (2)  = ALL '+'  AND                                  
074702        MID-KDCMDVAL (3)  = ALL '+'  AND                                  
074802        MID-KDCMDVAL (4)  = ALL '+'  AND                                  
074902        MID-KDCMDVAL (5)  = ALL '+'  AND                                  
075002        MID-KDCMDVAL (6)  = ALL '+'  AND                                  
075102        MID-KDCMDVAL (7)  = ALL '+'  AND                                  
075202        MID-KDCMDVAL (8)  = ALL '+'  AND                                  
075302        MID-KDCMDVAL (9)  = ALL '+'  AND                                  
075402        MID-KDCMDVAL (10) = ALL '+'  AND                                  
075502        MID-KDCMDVAL (11) = ALL '+'  AND                                  
075503        MID-KDCMDVAL (12) = ALL '+'  AND                                  
075602        MID-KDCMDVAL (13) = ALL '+'                                       
075802       PERFORM HA-INSERT-NEW-RULE                                         
075902     ELSE                                                                 
076002       MOVE +1 TO IX                                                      
076103       PERFORM UNTIL IX > MAX-IX                                          
076202         IF MID-KDCMDVAL (IX) = 'D' OR 'B'                                
076302           PERFORM HB-DELETE-RULE-LINE                                    
076402         ELSE                                                             
076502           IF MID-KDCMDVAL (IX) = 'E'                                     
076602             PERFORM HB-DELETE-RULE-LINE                                  
076702             PERFORM HC-UPDATE-RULE-LINE                                  
076802           END-IF                                                         
076902         END-IF                                                           
077002         ADD +1 TO IX                                                     
077102       END-PERFORM                                                        
077202     END-IF                                                               
077302                                                                          
077303     IF MED-IDMFSINF = '034'                                              
077304       CONTINUE                                                           
077305     ELSE                                                                 
077402       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
077403     END-IF                                                               
077502     CALL WMEDKONV USING MED-WMEDAREA                                     
077602     MOVE MED-MFSINF      TO MOD-TEMFSINF                                 
077702     PERFORM MFS-FORM-ATTR                                                
077802     PERFORM MFS-ERASE-FIELD-IN                                           
077902* * * MFS-DO-NOT-TOUCH-FIELD TO LOCKED VALUES                             
078002     .                                                                    
078003                                                                          
078102 HA-INSERT-NEW-RULE  SECTION.                                             
078202     MOVE MID-KVAKT-UP             TO AKT-KVAKT                           
078302                                                                          
078402     IF MID-KVVECKOR-PUBV-UP = ALL '+'                                    
078502       MOVE ZERO                   TO AKT-KVVECKOR-PUBV                   
078602     ELSE                                                                 
078702       MOVE MID-KVVECKOR-PUBV-UP   TO AKT-KVVECKOR-PUBV                   
078802     END-IF                                                               
078902                                                                          
079002     IF MID-PRARTSTD-UP = ALL '+'                                         
079102       MOVE ZERO                   TO AKT-PRARTSTD                        
079202     ELSE                                                                 
079302       MOVE MID-PRARTSTD-UP        TO AKT-PRARTSTD                        
079402     END-IF                                                               
079502                                                                          
079602     IF MID-VLARTNTO-UP = ALL '+'                                         
079702       MOVE ZERO                   TO AKT-VLARTNTO                        
079802     ELSE                                                                 
079902       MOVE MID-VLARTNTO-UP        TO AKT-VLARTNTO                        
080002     END-IF                                                               
080102                                                                          
080202     IF MID-KVPB-SEP-REF-UP = ALL '+'                                     
080302       MOVE ZERO                   TO AKT-KVPB-SEP-REF                    
080402     ELSE                                                                 
080502       MOVE MID-KVPB-SEP-REF-UP    TO AKT-KVPB-SEP-REF                    
080602     END-IF                                                               
080702                                                                          
080802     MOVE +1 TO IX1                                                       
080902     PERFORM UNTIL IX1 > MAX-IX1                                          
081002       IF MID-KDPRODSL-UP (IX1) = ALL '+'                                 
081102         MOVE ZERO                  TO TAB-KDPRODSL (IX1)                 
081202       ELSE                                                               
081302         MOVE MID-KDPRODSL-UP (IX1) TO TAB-KDPRODSL (IX1)                 
081402       END-IF                                                             
081502       MOVE ZERO                    TO AKT-KDPRODSL (IX1)                 
081602       ADD +1 TO IX1                                                      
081702     END-PERFORM                                                          
081802*                                                                         
081902     PERFORM S01-SORT-TABELL                                              
081903     MOVE +1 TO TAB-IX1                                                   
081904     MOVE +1 TO IX1                                                       
081905     PERFORM UNTIL TAB-IX1 > MAX-IX1                                      
081906       IF TAB-KDPRODSL (TAB-IX1) > 0                                      
081907         MOVE TAB-KDPRODSL (TAB-IX1)    TO AKT-KDPRODSL (IX1)             
081908         ADD +1 TO IX1                                                    
081909       END-IF                                                             
081910       ADD +1 TO TAB-IX1                                                  
081920     END-PERFORM                                                          
082002*                                                                         
082102     PERFORM IMS-ISRT-WDB614                                              
082103                                                                          
082704     PERFORM IMS-GHU-WDB615                                               
082705     IF SEGMENT-FOUND                                                     
082706       MOVE TODAYS-DATE TO LOGG-TIUPPDAT                                  
082707       MOVE MSGI-IDUSER TO LOGG-IDUSER                                    
082708       PERFORM IMS-REPL-WDB615                                            
082709     ELSE                                                                 
082710       MOVE '2367'      TO LOGG-IDTRANS                                   
082711       MOVE SPACE       TO LOGG-IDDC-REF                                  
082720       MOVE TODAYS-DATE TO LOGG-TIUPPDAT                                  
082730       MOVE MSGI-IDUSER TO LOGG-IDUSER                                    
082740       PERFORM IMS-ISRT-WDB615                                            
082750     END-IF                                                               
082802     .                                                                    
082803                                                                          
082902 HB-DELETE-RULE-LINE SECTION.                                             
083002     INSPECT MID-KVAKT (IX) REPLACING                                     
083102                        LEADING SPACES BY ZEROS                           
083202     MOVE MID-KVAKT (IX) TO AKT-KVAKT                                     
083302                            W-AKT-KVAKT                                   
083402*                                                                         
083502     INSPECT MID-KVVECKOR-PUBV (IX) REPLACING                             
083602                      LEADING SPACES BY ZEROS                             
083702     MOVE MID-KVVECKOR-PUBV   (IX) TO AKT-KVVECKOR-PUBV                   
083802                                      W-AKT-KVVECKOR-PUBV                 
083902*                                                                         
084002     INSPECT MID-PRARTSTD (IX) REPLACING                                  
084102                 LEADING SPACES BY ZEROS                                  
084202     MOVE MID-PRARTSTD        (IX) TO AKT-PRARTSTD                        
084302                                      W-AKT-PRARTSTD                      
084402*                                                                         
084502     INSPECT MID-VLARTNTO (IX) REPLACING                                  
084602                 LEADING SPACES BY ZEROS                                  
084702     MOVE MID-VLARTNTO        (IX) TO AKT-VLARTNTO                        
084802                                      W-AKT-VLARTNTO                      
084902*                                                                         
085002     INSPECT MID-KVPB-SEP-REF (IX) REPLACING                              
085102                 LEADING SPACES BY ZEROS                                  
085202     MOVE MID-KVPB-SEP-REF    (IX) TO AKT-KVPB-SEP-REF                    
085302                                      W-AKT-KVPB-SEP-REF                  
085402*                                                                         
085503     INSPECT MID-KDPRODSL (IX 1) REPLACING                                
085602                      LEADING SPACES BY ZEROS                             
085702     MOVE MID-KDPRODSL (IX 1) TO AKT-KDPRODSL (1)                         
085802                                 W-AKT-KDPRODSL                           
085902                                                                          
086002     PERFORM IMS-GHU-WDB614                                               
086102     IF SEGMENT-FOUND                                                     
086202       PERFORM IMS-DLET-WDB614                                            
086203     ELSE                                                                 
086204       MOVE INF-NO-UPDATE-DONE TO MED-IDMFSINF                            
086302     END-IF                                                               
086402                                                                          
087004     PERFORM IMS-GHU-WDB615                                               
087005     IF SEGMENT-FOUND                                                     
087006       MOVE TODAYS-DATE  TO LOGG-TIUPPDAT                                 
087007       MOVE MSGI-IDUSER  TO LOGG-IDUSER                                   
087008       PERFORM IMS-REPL-WDB615                                            
087009     ELSE                                                                 
087010       MOVE '2367'       TO LOGG-IDTRANS                                  
087011       MOVE SPACE        TO LOGG-IDDC-REF                                 
087020       MOVE TODAYS-DATE  TO LOGG-TIUPPDAT                                 
087030       MOVE MSGI-IDUSER  TO LOGG-IDUSER                                   
087040       PERFORM IMS-ISRT-WDB615                                            
087050     END-IF                                                               
087102     .                                                                    
087103                                                                          
087202 HC-UPDATE-RULE-LINE SECTION.                                             
087302     IF MID-KVAKT-UP NOT = ALL '+'                                        
087402       MOVE MID-KVAKT-UP             TO AKT-KVAKT                         
087502     ELSE                                                                 
087602       INSPECT MID-KVAKT (IX) REPLACING                                   
087702                          LEADING SPACES BY ZEROS                         
087802       MOVE MID-KVAKT (IX) TO AKT-KVAKT                                   
087902     END-IF                                                               
088002                                                                          
088102     IF MID-KVVECKOR-PUBV-UP NOT = ALL '+'                                
088202       MOVE MID-KVVECKOR-PUBV-UP   TO AKT-KVVECKOR-PUBV                   
088302     ELSE                                                                 
088402       INSPECT MID-KVVECKOR-PUBV (IX) REPLACING                           
088502                        LEADING SPACES BY ZEROS                           
088602       MOVE MID-KVVECKOR-PUBV (IX) TO AKT-KVVECKOR-PUBV                   
088702     END-IF                                                               
088802                                                                          
088902     IF MID-PRARTSTD-UP NOT = ALL '+'                                     
089002       MOVE MID-PRARTSTD-UP        TO AKT-PRARTSTD                        
089102     ELSE                                                                 
089202       INSPECT MID-PRARTSTD (IX) REPLACING                                
089302                   LEADING SPACES BY ZEROS                                
089402       MOVE MID-PRARTSTD (IX)      TO AKT-PRARTSTD                        
089502     END-IF                                                               
089602                                                                          
089702     IF MID-VLARTNTO-UP NOT = ALL '+'                                     
089802       MOVE MID-VLARTNTO-UP        TO AKT-VLARTNTO                        
089902     ELSE                                                                 
090002       INSPECT MID-VLARTNTO (IX) REPLACING                                
090102                   LEADING SPACES BY ZEROS                                
090202       MOVE MID-VLARTNTO (IX)      TO AKT-VLARTNTO                        
090302     END-IF                                                               
090402                                                                          
090502     IF MID-KVPB-SEP-REF-UP NOT = ALL '+'                                 
090602       MOVE MID-KVPB-SEP-REF-UP    TO AKT-KVPB-SEP-REF                    
090702     ELSE                                                                 
090802       INSPECT MID-KVPB-SEP-REF (IX) REPLACING                            
090902                   LEADING SPACES BY ZEROS                                
091002       MOVE MID-KVPB-SEP-REF (IX)  TO AKT-KVPB-SEP-REF                    
091102     END-IF                                                               
091202                                                                          
091302     MOVE +1 TO IX1                                                       
091402     PERFORM UNTIL IX1 > MAX-IX1                                          
091502       IF MID-KDPRODSL-UP (IX1) NOT = ALL '+'                             
091602         MOVE MID-KDPRODSL-UP (IX1)   TO TAB-KDPRODSL (IX1)               
091702       ELSE                                                               
091802         INSPECT MID-KDPRODSL (IX IX1) REPLACING                          
091902                         LEADING SPACES BY ZEROS                          
092002         MOVE MID-KDPRODSL (IX IX1)   TO TAB-KDPRODSL (IX1)               
092102       END-IF                                                             
092202       MOVE ZERO                      TO AKT-KDPRODSL (IX1)               
092302       ADD +1 TO IX1                                                      
092402     END-PERFORM                                                          
092502*                                                                         
092602     PERFORM S01-SORT-TABELL                                              
092603     MOVE +1 TO TAB-IX1                                                   
092604     MOVE +1 TO IX1                                                       
092605     PERFORM UNTIL TAB-IX1 > MAX-IX1                                      
092606       IF TAB-KDPRODSL (TAB-IX1) > 0                                      
092607         MOVE TAB-KDPRODSL (TAB-IX1)    TO AKT-KDPRODSL (IX1)             
092608         ADD +1 TO IX1                                                    
092609       END-IF                                                             
092610       ADD +1 TO TAB-IX1                                                  
092620     END-PERFORM                                                          
092702*                                                                         
092902     PERFORM IMS-ISRT-WDB614                                              
092903                                                                          
093504     PERFORM IMS-GHU-WDB615                                               
093505     IF SEGMENT-FOUND                                                     
093506       MOVE TODAYS-DATE  TO LOGG-TIUPPDAT                                 
093507       MOVE MSGI-IDUSER  TO LOGG-IDUSER                                   
093508       PERFORM IMS-REPL-WDB615                                            
093509     ELSE                                                                 
093510       MOVE '2367'       TO LOGG-IDTRANS                                  
093511       MOVE SPACE        TO LOGG-IDDC-REF                                 
093520       MOVE TODAYS-DATE  TO LOGG-TIUPPDAT                                 
093530       MOVE MSGI-IDUSER  TO LOGG-IDUSER                                   
093540       PERFORM IMS-ISRT-WDB615                                            
093550     END-IF                                                               
093702     .                                                                    
093703                                                                          
093802 S01-SORT-TABELL SECTION.                                                 
093902     MOVE +2    TO STEGLAANGD                                             
094002     MOVE +11   TO ANTAL                                                  
094102     MOVE +2    TO NYCKELLAANGD                                           
094202                                                                          
094302     CALL WINTSOR USING SORT-TABELL STEGLAANGD ANTAL                      
094402     TAB-SORT-BEGREPP(1) NYCKELLAANGD                                     
094502                                                                          
095502     .                                                                    
095503                                                                          
095602 MFS-ERASE-FIELD-OUT SECTION.                                             
095702*    --- ALLA UTDATA-FÄLT                                                 
095802     MOVE MFS-ERASE-FIELD TO MOD-KVAKT-UP                                 
095902                             MOD-KVVECKOR-PUBV-UP                         
096002                             MOD-PRARTSTD-UP                              
096102                             MOD-VLARTNTO-UP                              
096203                             MOD-KVPB-SEP-REF-UP                          
096303     MOVE +1 TO IX1                                                       
096403     PERFORM UNTIL IX1 > MAX-IX1                                          
096503       MOVE MFS-ERASE-FIELD TO MOD-KDPRODSL-UP(IX1)                       
096603       ADD +1 TO IX1                                                      
096703     END-PERFORM                                                          
096903     .                                                                    
096904                                                                          
097003 MFS-CLOSE-FIELD-IN SECTION.                                              
097103*    --- ALLA INDATA-FÄLT                                                 
097203     MOVE MFS-CLOSE-FIELD TO MOD-KVAKT-UP-ATTR                            
097303                             MOD-KVVECKOR-PUBV-UP-ATTR                    
097403                             MOD-PRARTSTD-UP-ATTR                         
097503                             MOD-VLARTNTO-UP-ATTR                         
097603                             MOD-KVPB-SEP-REF-UP-ATTR                     
097703     MOVE +1 TO IX1                                                       
097803     PERFORM UNTIL IX1 > MAX-IX1                                          
097903       MOVE MFS-CLOSE-FIELD TO MOD-KDPRODSL-UP-ATTR (IX1)                 
098003       ADD +1 TO IX1                                                      
098103     END-PERFORM                                                          
098203                                                                          
098303     MOVE +1 TO IX                                                        
098403     PERFORM UNTIL IX > MAX-IX                                            
098503       MOVE MFS-CLOSE-FIELD TO MOD-KDCMDVAL-ATTR (IX)                     
098603       ADD +1 TO IX                                                       
098703     END-PERFORM                                                          
098803     .                                                                    
098804                                                                          
098903 MFS-ERASE-FIELD-IN SECTION.                                              
099003*    --- ALLA INDATA-FÄLT                                                 
099103     MOVE MFS-ERASE-FIELD TO MOD-IDDC-IN                                  
099203     MOVE MFS-ERASE-FIELD TO MOD-KVAKT-UP                                 
099303                             MOD-KVVECKOR-PUBV-UP                         
099403                             MOD-PRARTSTD-UP                              
099503                             MOD-VLARTNTO-UP                              
099603                             MOD-KVPB-SEP-REF-UP                          
099703     MOVE +1 TO IX1                                                       
099803     PERFORM UNTIL IX1 > MAX-IX1                                          
099903       MOVE MFS-ERASE-FIELD TO MOD-KDPRODSL-UP (IX1)                      
100003       ADD +1 TO IX1                                                      
100103     END-PERFORM                                                          
100203                                                                          
100303     MOVE +1 TO IX                                                        
100403     PERFORM UNTIL IX > MAX-IX                                            
100503       MOVE MFS-ERASE-FIELD TO MOD-KDCMDVAL (IX)                          
100603       ADD +1 TO IX                                                       
100703     END-PERFORM                                                          
100803     .                                                                    
100804                                                                          
100903 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
101003*    --- ALLA UTDATA-FÄLT                                                 
101103     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDDC-UT                           
101203                                                                          
101303     MOVE +1 TO IX                                                        
101403     PERFORM UNTIL IX > MAX-IX                                            
101503       PERFORM MFS-DONT-TOUCH-LINE-FIELD-OUT                              
101603       ADD +1 TO IX                                                       
101703     END-PERFORM                                                          
101803     .                                                                    
101804                                                                          
101903 MFS-DONT-TOUCH-LINE-FIELD-OUT  SECTION.                                  
102003     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDCMDVAL       (IX)               
102103                                    MOD-KVAKT(IX)                         
102203                                    MOD-KVVECKOR-PUBV  (IX)               
102303                                    MOD-PRARTSTD       (IX)               
102403                                    MOD-VLARTNTO       (IX)               
102503                                    MOD-KVPB-SEP-REF   (IX)               
102603     MOVE +1 TO IX1                                                       
102703     PERFORM UNTIL IX1 > MAX-IX1                                          
102803       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDPRODSL (IX IX1)               
102903       ADD +1 TO IX1                                                      
103003     END-PERFORM                                                          
103103     .                                                                    
103104                                                                          
103203 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
103303*    --- ALLA INDATA-FÄLT                                                 
103403     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDDC-IN                           
103503                                    MOD-KVAKT-UP                          
103603                                    MOD-KVVECKOR-PUBV-UP                  
103703                                    MOD-PRARTSTD-UP                       
103803                                    MOD-VLARTNTO-UP                       
103903                                    MOD-KVPB-SEP-REF-UP                   
104003     MOVE +1 TO IX1                                                       
104103     PERFORM UNTIL IX1 > MAX-IX1                                          
104203       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDPRODSL-UP (IX1)               
104303       ADD +1 TO IX1                                                      
104403     END-PERFORM                                                          
104503                                                                          
104603     MOVE +1 TO IX                                                        
104703     PERFORM UNTIL IX > MAX-IX                                            
104803       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDCMDVAL (IX)                   
104903       ADD +1 TO IX                                                       
105003     END-PERFORM                                                          
105103     .                                                                    
105104                                                                          
105203 MFS-FORM-ATTR SECTION.                                                   
105303*    --- ALL INDATA-FIELDS                                                
105403     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-IDDC-IN-ATTR                     
105503                                    MOD-KVAKT-UP-ATTR                     
105603                                    MOD-KVVECKOR-PUBV-UP-ATTR             
105703                                    MOD-PRARTSTD-UP-ATTR                  
105803                                    MOD-VLARTNTO-UP-ATTR                  
105903                                    MOD-KVPB-SEP-REF-UP-ATTR              
106003     MOVE +1 TO IX1                                                       
106103     PERFORM UNTIL IX1 > MAX-IX1                                          
106203       MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-KDPRODSL-UP-ATTR(IX1)          
106303       ADD +1 TO IX1                                                      
106403     END-PERFORM                                                          
106503                                                                          
106603     MOVE +1 TO IX                                                        
106703     PERFORM UNTIL IX > MAX-IX                                            
106803       MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-KDCMDVAL-ATTR (IX)             
106903       ADD +1 TO IX                                                       
107003     END-PERFORM                                                          
107103     .                                                                    
107104                                                                          
107203 MFS-CLOSE-ERASE-FIELD SECTION.                                           
107303     MOVE MFS-CLOSE-FIELD TO MOD-KDCMDVAL-ATTR  (IX)                      
107403     MOVE MFS-ERASE-FIELD TO MOD-KDCMDVAL       (IX)                      
107503                             MOD-KVAKT(IX)                                
107603                             MOD-KVVECKOR-PUBV  (IX)                      
107703                             MOD-PRARTSTD       (IX)                      
107803                             MOD-VLARTNTO       (IX)                      
107903                             MOD-KVPB-SEP-REF   (IX)                      
108003     MOVE +1 TO IX1                                                       
108103     PERFORM UNTIL IX1 > MAX-IX1                                          
108203       MOVE MFS-ERASE-FIELD TO MOD-KDPRODSL (IX IX1)                      
108303       ADD +1 TO IX1                                                      
108403     END-PERFORM                                                          
108503     .                                                                    
108504                                                                          
108603* --- IMS SECTIONS ---                                                    
108703 IMS-GET-MSG SECTION.                                                     
108803     MOVE '  QC' TO GOOD-STATUSCODES                                      
108903     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
109003     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
109103     PERFORM IMS-STATUSCHECK                                              
109203     .                                                                    
109204                                                                          
109303 IMS-INSERT-MSG SECTION.                                                  
109403**   IF MSGI-IDLAND-SPR = 'SE'                                            
109503**     MOVE '0' TO MFS-KDHUVOMR                                           
109603**   END-IF                                                               
109703     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
109803     MOVE SPACE TO GOOD-STATUSCODES                                       
109903     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
110003     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
110103     PERFORM IMS-STATUSCHECK                                              
110203     .                                                                    
110204                                                                          
110303 IMS-GHU-WDB601 SECTION.                                                  
110403     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
110503          DELIMITED BY SIZE INTO SSA1                                     
110603     MOVE '  GE' TO GOOD-STATUSCODES                                      
110703     CALL CBLTDLI USING GHU WDB6-PCB DLI-IO-WDB601 SSA1                   
110803     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
110903     PERFORM IMS-STATUSCHECK                                              
111003     .                                                                    
111004                                                                          
111103 IMS-REPL-WDB601 SECTION.                                                 
111203     MOVE '  ' TO GOOD-STATUSCODES                                        
111303     CALL CBLTDLI USING REPL WDB6-PCB DLI-IO-WDB601                       
111403     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
111503     PERFORM IMS-STATUSCHECK                                              
111603     .                                                                    
111604                                                                          
111703 IMS-GU-WDB601 SECTION.                                                   
111803     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
111903          DELIMITED BY SIZE INTO SSA1                                     
112003     MOVE '  GE' TO GOOD-STATUSCODES                                      
112103     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
112203     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
112303     PERFORM IMS-STATUSCHECK                                              
112403     .                                                                    
112404                                                                          
112503 IMS-GNP-WDB614 SECTION.                                                  
112603     MOVE 'WDB614  '       TO SSA1                                        
112703     MOVE '  GE' TO GOOD-STATUSCODES                                      
112803     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB614 SSA1                   
112903     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
113003     PERFORM IMS-STATUSCHECK                                              
113103     .                                                                    
113104                                                                          
113203 IMS-GHU-WDB614 SECTION.                                                  
113303     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
113403          DELIMITED BY SIZE INTO SSA1                                     
113503     STRING 'WDB614  (WDB614KY =' W-WDB614KY-X ')'                        
113603          DELIMITED BY SIZE INTO SSA2                                     
113703     MOVE '  GE' TO GOOD-STATUSCODES                                      
113803     CALL CBLTDLI USING GHU WDB6-PCB DLI-IO-WDB614 SSA1 SSA2              
113903     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
114003     PERFORM IMS-STATUSCHECK                                              
114103     .                                                                    
114104                                                                          
114203 IMS-ISRT-WDB614 SECTION.                                                 
114303     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
114403          DELIMITED BY SIZE INTO SSA1                                     
114503     MOVE 'WDB614 ' TO SSA2                                               
114603     MOVE '  II' TO GOOD-STATUSCODES                                      
114703     CALL CBLTDLI USING ISRT WDB6-PCB DLI-IO-WDB614 SSA1 SSA2             
114803     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
114903     PERFORM IMS-STATUSCHECK                                              
115003     .                                                                    
115004                                                                          
115103 IMS-DLET-WDB614 SECTION.                                                 
115203     MOVE '  ' TO GOOD-STATUSCODES                                        
115303     CALL CBLTDLI USING DLET WDB6-PCB DLI-IO-WDB614                       
115403     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
115503     PERFORM IMS-STATUSCHECK                                              
115603     .                                                                    
115604     EJECT                                                                
115605 IMS-GNP-WDB615  SECTION.                                                 
115606                                                                          
115608     STRING 'WDB615  (IDTRANS  =' W-IDTRANS-B6-X ')'                      
115609     DELIMITED BY SIZE INTO SSA1                                          
115610     MOVE '  GE' TO GOOD-STATUSCODES                                      
115611     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB615 SSA1                   
115620     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
115630     PERFORM IMS-STATUSCHECK                                              
115640     .                                                                    
115650     EJECT                                                                
115660 IMS-GHU-WDB615 SECTION.                                                  
115670                                                                          
115680     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
115690          DELIMITED BY SIZE INTO SSA1                                     
115700     STRING 'WDB615  (IDTRANS  =' W-IDTRANS-B6-X ')'                      
115701          DELIMITED BY SIZE INTO SSA2                                     
115702     MOVE '  GE' TO GOOD-STATUSCODES                                      
115703     CALL CBLTDLI USING GHU WDB6-PCB DLI-IO-WDB615 SSA1 SSA2              
115704     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
115705     PERFORM IMS-STATUSCHECK                                              
115706     .                                                                    
115707     EJECT                                                                
115708 IMS-REPL-WDB615 SECTION.                                                 
115709                                                                          
115710     MOVE '  ' TO GOOD-STATUSCODES                                        
115711     CALL CBLTDLI USING REPL WDB6-PCB DLI-IO-WDB615                       
115712     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
115713     PERFORM IMS-STATUSCHECK                                              
115714     .                                                                    
115715     EJECT                                                                
115716 IMS-ISRT-WDB615 SECTION.                                                 
115717                                                                          
115718     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
115719          DELIMITED BY SIZE INTO SSA1                                     
115720     MOVE 'WDB615  ' TO SSA2                                              
115721     MOVE '    ' TO GOOD-STATUSCODES                                      
115722     CALL CBLTDLI USING ISRT WDB6-PCB DLI-IO-WDB615 SSA1 SSA2             
115723     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
115724     PERFORM IMS-STATUSCHECK                                              
115725     .                                                                    
115726     EJECT                                                                
115730 IMS-STATUSCHECK SECTION.                                                 
115803     SET STATUS-IX TO 1                                                   
115903     SEARCH GOOD-STATUS                                                   
116003       AT END                                                             
116103         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
116203         DELIMITED BY SIZE INTO ERROR-TEXT                                
116303         CALL FELLOG                                                      
116403       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
116503         CONTINUE                                                         
116603     END-SEARCH                                                           
117003     .                                                                    
117004                                                                          
