000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2036600.                                                
000300 AUTHOR.         UMESH JAIN.                                              
000400 DATE-WRITTEN.   10/11/25.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        DESTOCKING STEERING FOR ALL DC'S EXCEPT CDC                      
000900*                                                                         
001000*        THE PROGRAM UPDATES   WDB6                                       
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSACTION: W2T366                                              
001400*                     W2T366U                                             
001500*        MID:         W2I36601                                            
001600*                                                                         
001700*    OUTDATA.                                                             
001800*        MOD:         W2O36601                                            
001810*                                                                         
001820* *************                                                           
001830* 2011-09-08   SO  E'TRACKER 10148137 RÄTTA FEL VID UPPDATERING.          
001840*                                                                         
001850* 2011-09-15   SO  E'TRACKER 10148905 RÄTTA FEL 2366 OCH 2367.            
001860*                                                                         
001870* 2012-05-23   SO  E'TRACKER 10151888 SEPARATA FÄLT 2366 2367             
001880*                                     FÖR IDUSER OCH TIUPPDAT.            
001890*                                                                         
001900                                                                          
002100 ENVIRONMENT DIVISION.                                                    
002200                                                                          
002300 DATA DIVISION.                                                           
002400     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600 77  IDPGM                       PIC X(08)   VALUE 'W2036600'.            
002700                                                                          
002800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
002900 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003000                                                                          
003100 77  YES                         PIC X       VALUE 'Y'.                   
003200 77  NOO                         PIC X       VALUE 'N'.                   
003300 77  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
003400 77  IX                          PIC S9(9)   VALUE ZERO.                  
003500 77  IX1                         PIC S9(9)   VALUE ZERO.                  
003510 77  TAB-IX1                     PIC S9(9)   VALUE ZERO.                  
003600 77  MAX-IX                      PIC S9(9)   VALUE +13.                   
003700 77  MAX-IX1                     PIC S9(9)   VALUE +10.                   
003800 77  W-CMD-CNT-E                 PIC S9(9)   VALUE ZERO.                  
003900 77  W-CMD-CNT-D                 PIC S9(9)   VALUE ZERO.                  
003910 77  W-PAGE-FULL                 PIC X       VALUE 'N'.                   
003920 77  UPDATE-DATA                 PIC X       VALUE 'N'.                   
003930 77  ERR-UPDATE                  PIC X       VALUE 'N'.                   
004000                                                                          
004100*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004200                                                                          
004300 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
004400     88  INDATA-OK                           VALUE 'Y'.                   
004500     88  INDATA-WRONG                        VALUE 'N'.                   
004600                                                                          
004700 77  KEYS-SW                     PIC X       VALUE 'Y'.                   
004800     88  KEYS-OK                             VALUE 'Y'.                   
004900     88  KEYS-WRONG                          VALUE 'N'.                   
005000                                                                          
005100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005200     88  OWN-MID                             VALUE '2366'.                
005300     88  GOOD-MID                            VALUE '2361' '2362'          
005400                                                   '2363' '2364'          
005500                                                   '2365' '2366'          
005600                                                   '2367' '2368'          
005700                                                   '2369'.                
005800     88  HELP-MID                            VALUE '0551'.                
005900*                                                                         
005901* ---PARAMETERS FOR SUBPROGRAM WINTSORT                                   
005902* ---TO SORT A TABLE INTERNALLY                                           
005910 01  TABENTRY-PARM.                                                       
005920     03  STEGLAANGD              PIC S9(9) COMP.                          
005930     03  ANTAL                   PIC S9(9) COMP.                          
005940     03  NYCKELLAANGD            PIC S9(9) COMP.                          
005950 01  SORT-TABELL.                                                         
005960     03  TAB-RAD OCCURS 10.                                               
005970        05  TAB-SORT-BEGREPP.                                             
005980            07  TAB-ADLAGOMR    PIC 9(2).                                 
005990*                                                                         
006000*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006100 01  GENERAL-SUBPROGRAMS.                                                 
006200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006600     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR '.            
006700*                                                                         
006800*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
006900*01 -COPY WMEDAREA                                                        
007000     SKIP3                                                                
007100 01  MESSAGE-CODES.                                                       
007200     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007300     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007400     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007500     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007610     03  ERR-WRONG-DC            PIC X(3)    VALUE '440'.                 
007700     03  INF-NO-RECORDS          PIC X(3)    VALUE '010'.                 
007710     03  ERR-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '007'.                 
007720     03  ERR-DC-MISSING          PIC X(3)    VALUE '026'.                 
007800*                                                                         
007900*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
008000*                                                                         
008100 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008200*                                                                         
008300*01 -COPY WMSGINIT                                                        
008400     EJECT                                                                
008500*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
008600*                                                                         
008700 01  SAVE-AREA.                                                           
008800     03  SAVE-IDTRANS           PIC X(4)    VALUE '2366'.                 
008900     EJECT                                                                
009000*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
009100*                                                                         
009200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009300     SKIP3                                                                
009400*01  MID -COPY W2I36601                                                   
009500     EJECT                                                                
009600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009700     SKIP3                                                                
009800*01  -COPY WMSGAREA                                                       
009900     EJECT                                                                
010000     03  MOD REDEFINES MSG-AREA.                                          
010100*      05  -COPY W2O36601                                                 
010200     EJECT                                                                
010300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010400     SKIP3                                                                
010500*01  -COPY WMFSAREA                                                       
010600     EJECT                                                                
010700*    --- WORK-AREAS FOR IMS-SECTIONS                                      
010800*                                                                         
010900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011000*                                                                         
011100 01  KEYS-FOR-DLI.                                                        
011200     03  W-IDDC-X.                                                        
011300         05  W-IDDC              PIC X(02)   VALUE SPACE.                 
011400*                                                                         
011500     03  W-WDB613KY-X.                                                    
011600         05  W-PASS-KVVECKOR-LSALES  PIC S9(3) VALUE 0 COMP-3.            
011700         05  W-PASS-KVVECKOR-PUBV    PIC S9(3) VALUE 0 COMP-3.            
011800         05  W-PASS-PRARTSTD         PIC 9(7)  VALUE 0.                   
011900         05  W-PASS-VLARTNTO         PIC 9(8)  VALUE 0.                   
012000         05  W-PASS-KDPRODSL         PIC S9(3) VALUE 0 COMP-3.            
012100         05  W-PASS-ADLAGOMR         PIC S9(3) VALUE 0 COMP-3.            
012200*                                                                         
012210     03  W-IDTRANS-B6-X.                                                  
012220         05  W-IDTRANS-B6        PIC X(4)    VALUE '2366'.                
012230*                                                                         
012300*    --- STATUS CODES FROM IMS                                            
012400 01  STATUS-WS                   PIC XX.                                  
012500     88  SEGMENT-FOUND                       VALUE '  '.                  
012600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
012700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012800*                                                                         
012900 01  GOOD-STATUSCODES.                                                    
013000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013100*                                                                         
013200 01  SSA1                        PIC X(64).                               
013300 01  SSA2                        PIC X(64).                               
013400*                                                                         
013500*    --- IMS FUNCTION CODES                                               
013600*01  -COPY W0003                                                          
013700*                                                                         
013800*    ---  DLI INPUT-OUTPUT AREA                                           
013900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
014000 01  DLI-IO-WDB601.                                                       
014100*    03  -COPY WDB601                                                     
014200*                                                                         
014300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB613'.                      
014400 01  DLI-IO-WDB613.                                                       
014500*    03  -COPY WDB613                                                     
014600*                                                                         
014620 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB615'.                      
014630 01  DLI-IO-WDB615.                                                       
014640*    03  -COPY WDB615                                                     
014650*                                                                         
014700 LINKAGE SECTION.                                                         
014800*01  -COPY W0009   -PRE MSG-                                              
014900                                                                          
015000*01  -COPY W0008   -PRE WDP7-                                             
015100     05  FILLER                  PIC X.                                   
015200                                                                          
015300*01  -COPY W0008  -PRE WDB6-                                              
015400     05  FILLER                  PIC X.                                   
015500     EJECT                                                                
015600 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDB6-PCB.                     
015700 MAIN SECTION.                                                            
015800     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDB6-PCB.                     
015900                                                                          
016000     PERFORM IMS-GET-MSG                                                  
016100     IF SEGMENT-FOUND                                                     
016200       PERFORM A-INIT                                                     
016300       PERFORM B-CHECK-KEYS                                               
016400       IF KEYS-OK                                                         
016500         IF MFS-UPDATE                                                    
016600           PERFORM G-CHECK-INPUT                                          
016700           IF INDATA-OK AND W-PAGE-FULL = 'N'                             
016800             PERFORM H-UPDATE                                             
016900           END-IF                                                         
017000         ELSE                                                             
017100           IF MFS-FIRST                                                   
017200             PERFORM C-FIRST-PAGE                                         
017300           ELSE                                                           
017400             PERFORM E-SAME-PAGE                                          
017500           END-IF                                                         
017600         END-IF                                                           
017700         PERFORM F-READ-SHOW-INFO                                         
017800       END-IF                                                             
018100       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O36601 + 4                      
018200       PERFORM IMS-INSERT-MSG                                             
018300     END-IF                                                               
018400                                                                          
018500     MOVE ZERO TO RETURN-CODE                                             
018600     GOBACK                                                               
018700     .                                                                    
018800     EJECT                                                                
018900 A-INIT SECTION.                                                          
019000     IF MSG-DOUBLE-TRANSACTIONS                                           
019100       MOVE MSG-INDATA-MINUS-2-TRANSACT  TO MID-W2I36601                  
019200       MOVE MSG-IDTRANS-2                TO MFS-IDTRANS                   
019300       MOVE MSG-KDMFSFOR-2               TO MFS-KDMFSFOR                  
019400     ELSE                                                                 
019500       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W2I36601                  
019600       MOVE MSG-IDTRANS-1                TO MFS-IDTRANS                   
019700       MOVE MSG-KDMFSFOR-1               TO MFS-KDMFSFOR                  
019800     END-IF                                                               
019900                                                                          
020000     MOVE MSG-KDTRTYP      TO MFS-KDTRTYP                                 
020100     MOVE MSG-IDPFK        TO MFS-IDPFK                                   
020200     MOVE MFS-IDTRANS      TO W-IDTRANS                                   
020300     MOVE LOW-VALUE        TO MSG-AREA                                    
020400     MOVE 'W2O366N1'       TO MFS-IDMOD                                   
020500     MOVE '2366'           TO MOD-IDTRANS                                 
020600     MOVE MFS-ERASE-FIELD  TO MOD-TEMFSFEL MOD-TEMFSINF                   
020700                                                                          
020800     IF OWN-MID OR HELP-MID                                               
020900       CONTINUE                                                           
021000     ELSE                                                                 
021100       MOVE SPACE TO MFS-KDTRTYP                                          
021200       MOVE '7'   TO MFS-IDPFK                                            
021300     END-IF                                                               
021400                                                                          
021500     ACCEPT TODAYS-DATE FROM DATE                                         
021600     .                                                                    
021700 B-CHECK-KEYS SECTION.                                                    
021800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
021900     MOVE '001'             TO MSGI-KDCALL                                
022000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
022100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
022200     MOVE '2366'            TO MSGI-IDTRANS                               
022300     IF GOOD-MID                                                          
022400       MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                              
022500     END-IF                                                               
022600     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
022800                                                                          
022900*    - LANGUAGE TO BE USED BY MEDKONV                                     
023000     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
023100*                                                                         
023200     MOVE YES TO KEYS-SW                                                  
023300*                                                                         
023400*    -- CHECK OF IDDC                                                     
023500     MOVE MFS-ERASE-FIELD   TO MOD-IDDC-IN                                
023600                                                                          
023610     MOVE MSGI-IDDC-KEY     TO W-IDDC                                     
023700     IF MID-IDDC-IN NOT = ALL '+'                                         
023910       PERFORM IMS-GU-WDB601                                              
023930       IF SEGMENT-MISSING OR DCS-CDC-TR OR DCS-DDC                        
023931         MOVE NOO            TO KEYS-SW                                   
023940         MOVE ERR-WRONG-DC   TO MED-IDMFSFEL                              
023950         CALL WMEDKONV USING MED-WMEDAREA                                 
023960         MOVE MED-MFSFEL     TO MOD-TEMFSFEL                              
023961         PERFORM MFS-ERASE-FIELD-IN                                       
023970         PERFORM MFS-ERASE-FIELD-OUT                                      
023980         PERFORM MFS-CLOSE-FIELD-IN                                       
023981       ELSE                                                               
023982         MOVE '7'           TO MFS-IDPFK                                  
023983         MOVE SPACE         TO MFS-KDTRTYP                                
023984       END-IF                                                             
024400     END-IF                                                               
024600                                                                          
024700     IF GOOD-MID OR KEYS-OK                                               
024800       MOVE MSGI-IDDC-KEY   TO MOD-IDDC-UT                                
024900     ELSE                                                                 
025000       MOVE MFS-ERASE-FIELD TO MOD-IDDC-UT                                
025100     END-IF                                                               
025200                                                                          
026000     .                                                                    
026100 C-FIRST-PAGE SECTION.                                                    
026200     PERFORM MFS-ERASE-FIELD-IN                                           
026300     .                                                                    
026400 E-SAME-PAGE SECTION.                                                     
026526                                                                          
026530     IF OWN-MID OR HELP-MID                                               
026610       IF MID-KDCMDVAL (1)  = ALL '+'  AND                                
026620          MID-KDCMDVAL (2)  = ALL '+'  AND                                
026630          MID-KDCMDVAL (3)  = ALL '+'  AND                                
026640          MID-KDCMDVAL (4)  = ALL '+'  AND                                
026650          MID-KDCMDVAL (5)  = ALL '+'  AND                                
026660          MID-KDCMDVAL (6)  = ALL '+'  AND                                
026670          MID-KDCMDVAL (7)  = ALL '+'  AND                                
026680          MID-KDCMDVAL (8)  = ALL '+'  AND                                
026690          MID-KDCMDVAL (9)  = ALL '+'  AND                                
026691          MID-KDCMDVAL (10) = ALL '+'  AND                                
026692          MID-KDCMDVAL (11) = ALL '+'  AND                                
026693          MID-KDCMDVAL (12) = ALL '+'  AND                                
026694          MID-KDCMDVAL (13) = ALL '+'  AND                                
026696          MID-KVVECKOR-LSALES-UP = ALL '+'  AND                           
026697          MID-KVVECKOR-PUBV-UP   = ALL '+'  AND                           
026698          MID-PRARTSTD-UP        = ALL '+'  AND                           
026699          MID-VLARTNTO-UP        = ALL '+'  AND                           
026700          MID-KDPRODSL-UP        = ALL '+'  AND                           
026701          MID-ADLAGOMR-UP (1)    = ALL '+'  AND                           
026702          MID-ADLAGOMR-UP (2)    = ALL '+'  AND                           
026703          MID-ADLAGOMR-UP (3)    = ALL '+'  AND                           
026704          MID-ADLAGOMR-UP (4)    = ALL '+'  AND                           
026705          MID-ADLAGOMR-UP (5)    = ALL '+'  AND                           
026706          MID-ADLAGOMR-UP (6)    = ALL '+'  AND                           
026707          MID-ADLAGOMR-UP (7)    = ALL '+'  AND                           
026708          MID-ADLAGOMR-UP (8)    = ALL '+'  AND                           
026709          MID-ADLAGOMR-UP (9)    = ALL '+'  AND                           
026710          MID-ADLAGOMR-UP (10)   = ALL '+'                                
026720         PERFORM MFS-ERASE-FIELD-IN                                       
026800       ELSE                                                               
026900         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
027000         CALL WMEDKONV USING MED-WMEDAREA                                 
027100         MOVE MED-MFSINF     TO MOD-TEMFSINF                              
027200         PERFORM EA-MID-INDATA-TO-MOD                                     
027300       END-IF                                                             
027400     ELSE                                                                 
027500       PERFORM MFS-ERASE-FIELD-IN                                         
027600     END-IF                                                               
027700     .                                                                    
027800 EA-MID-INDATA-TO-MOD SECTION.                                            
027900     MOVE 1 TO IX                                                         
028000     PERFORM UNTIL IX > MAX-IX                                            
028100       IF MID-KVVECKOR-LSALES (IX) = ALL '+'                              
028200         MOVE MFS-ERASE-FIELD     TO MOD-KVVECKOR-LSALES (IX)             
028300       ELSE                                                               
028400         MOVE MID-KVVECKOR-LSALES (IX)                                    
028500                                  TO MOD-KVVECKOR-LSALES (IX)             
028600       END-IF                                                             
028700                                                                          
028800       IF MID-KVVECKOR-PUBV (IX) = ALL '+'                                
028900         MOVE MFS-ERASE-FIELD     TO MOD-KVVECKOR-PUBV (IX)               
029000       ELSE                                                               
029100         MOVE MID-KVVECKOR-PUBV (IX)                                      
029200                                  TO MOD-KVVECKOR-PUBV (IX)               
029300       END-IF                                                             
029400                                                                          
029500       IF MID-PRARTSTD (IX) = ALL '+'                                     
029600         MOVE MFS-ERASE-FIELD     TO MOD-PRARTSTD (IX)                    
029700       ELSE                                                               
029800         MOVE MID-PRARTSTD (IX)   TO MOD-PRARTSTD (IX)                    
029900       END-IF                                                             
030000                                                                          
030100       IF MID-VLARTNTO (IX) = ALL '+'                                     
030200         MOVE MFS-ERASE-FIELD     TO MOD-VLARTNTO (IX)                    
030300       ELSE                                                               
030400         MOVE MID-VLARTNTO (IX)   TO MOD-VLARTNTO (IX)                    
030500       END-IF                                                             
030600                                                                          
030700       IF MID-KDPRODSL (IX) = ALL '+'                                     
030800         MOVE MFS-ERASE-FIELD     TO MOD-KDPRODSL (IX)                    
030900       ELSE                                                               
031000         MOVE MID-KDPRODSL (IX)   TO MOD-KDPRODSL (IX)                    
031100       END-IF                                                             
031200                                                                          
031300       MOVE 1 TO IX1                                                      
031400       PERFORM UNTIL IX1 > MAX-IX1                                        
031500         IF MID-ADLAGOMR (IX IX1) = ALL '+'                               
031600           MOVE MFS-ERASE-FIELD   TO MOD-ADLAGOMR (IX IX1)                
031700         ELSE                                                             
031800           MOVE MID-ADLAGOMR (IX IX1)                                     
031900                                  TO MOD-ADLAGOMR (IX IX1)                
032000         END-IF                                                           
032100         ADD 1 TO IX1                                                     
032200       END-PERFORM                                                        
032300                                                                          
032400       ADD 1 TO IX                                                        
032500     END-PERFORM                                                          
032510***                                                                       
032511     MOVE +1 TO IX                                                        
032512     PERFORM UNTIL IX > MAX-IX                                            
032513       IF MID-KDCMDVAL (IX) NOT = ALL '+'                                 
032514         MOVE MID-KDCMDVAL (IX)     TO MOD-KDCMDVAL (IX)                  
032515         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMDVAL-ATTR (IX)             
032516       ELSE                                                               
032517         MOVE MFS-ERASE-FIELD       TO MOD-KDCMDVAL (IX)                  
032519       END-IF                                                             
032520       ADD +1 TO IX                                                       
032521     END-PERFORM                                                          
032522                                                                          
032581     IF MID-KVVECKOR-LSALES-UP NOT = ALL '+'                              
032582       MOVE MID-KVVECKOR-LSALES-UP                                        
032583                                  TO MOD-KVVECKOR-LSALES-UP               
032584       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVVECKOR-LSALES-UP-ATTR          
032585     ELSE                                                                 
032586       MOVE MFS-ERASE-FIELD       TO MOD-KVVECKOR-LSALES-UP               
032587     END-IF                                                               
032588                                                                          
032590     IF MID-KVVECKOR-PUBV-UP NOT = ALL '+'                                
032593       MOVE MID-KVVECKOR-PUBV-UP  TO MOD-KVVECKOR-PUBV-UP                 
032595       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVVECKOR-PUBV-UP-ATTR            
032596     ELSE                                                                 
032597       MOVE MFS-ERASE-FIELD       TO MOD-KVVECKOR-PUBV-UP                 
032598     END-IF                                                               
032599                                                                          
032600     IF MID-PRARTSTD-UP NOT = ALL '+'                                     
032603       MOVE MID-PRARTSTD-UP       TO MOD-PRARTSTD-UP                      
032604       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-PRARTSTD-UP-ATTR                 
032605     ELSE                                                                 
032606       MOVE MFS-ERASE-FIELD       TO MOD-PRARTSTD-UP                      
032607     END-IF                                                               
032608                                                                          
032609     IF MID-VLARTNTO-UP NOT = ALL '+'                                     
032612       MOVE MID-VLARTNTO-UP       TO MOD-VLARTNTO-UP                      
032613       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-VLARTNTO-UP-ATTR                 
032614     ELSE                                                                 
032615       MOVE MFS-ERASE-FIELD       TO MOD-VLARTNTO-UP                      
032616     END-IF                                                               
032617                                                                          
032618     IF MID-KDPRODSL-UP NOT = ALL '+'                                     
032621       MOVE MID-KDPRODSL-UP       TO MOD-KDPRODSL-UP                      
032622       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDPRODSL-UP-ATTR                 
032623     ELSE                                                                 
032624       MOVE MFS-ERASE-FIELD       TO MOD-KDPRODSL-UP                      
032625     END-IF                                                               
032626                                                                          
032627     MOVE 1 TO IX1                                                        
032628     PERFORM UNTIL IX1 > MAX-IX1                                          
032629       IF MID-ADLAGOMR-UP (IX1) NOT = ALL '+'                             
032632         MOVE MID-ADLAGOMR-UP (IX1) TO MOD-ADLAGOMR-UP (IX1)              
032633         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADLAGOMR-UP-ATTR (IX1)         
032634       ELSE                                                               
032635         MOVE MFS-ERASE-FIELD       TO MOD-ADLAGOMR-UP (IX1)              
032636       END-IF                                                             
032637       ADD 1 TO IX1                                                       
032638     END-PERFORM                                                          
032640     .                                                                    
032700 F-READ-SHOW-INFO SECTION.                                                
032710                                                                          
032800     PERFORM IMS-GU-WDB601                                                
032900                                                                          
033000     IF SEGMENT-MISSING OR DCS-CDC-TR OR DCS-DDC                          
033100       MOVE ERR-WRONG-DC   TO MED-IDMFSFEL                                
033200       CALL WMEDKONV USING MED-WMEDAREA                                   
033300       MOVE MED-MFSFEL     TO MOD-TEMFSFEL                                
033400       PERFORM MFS-ERASE-FIELD-OUT                                        
033410       PERFORM MFS-CLOSE-FIELD-IN                                         
033500     ELSE                                                                 
033800       PERFORM IMS-GNP-WDB613                                             
033900       MOVE +1 TO IX                                                      
034000       PERFORM UNTIL IX > MAX-IX                                          
034100         IF SEGMENT-FOUND                                                 
034200           PERFORM FA-MOVE-DATA-TO-MOD                                    
034300           PERFORM IMS-GNP-WDB613                                         
034400         ELSE                                                             
034500           PERFORM MFS-CLOSE-ERASE-FIELD                                  
034600         END-IF                                                           
034610         ADD +1 TO IX                                                     
034700       END-PERFORM                                                        
034800                                                                          
034802       PERFORM IMS-GNP-WDB615                                             
034803       IF SEGMENT-FOUND                                                   
034804         MOVE LOGG-TIUPPDAT   TO MOD-TIUPPDAT                             
034805         MOVE LOGG-IDUSER     TO MOD-IDUSER                               
034806       ELSE                                                               
034807         MOVE ZERO            TO MOD-TIUPPDAT                             
034808         MOVE SPACE           TO MOD-IDUSER                               
034809       END-IF                                                             
034810     END-IF                                                               
034900     .                                                                    
035000 FA-MOVE-DATA-TO-MOD  SECTION.                                            
035100     MOVE PASS-KVVECKOR-LSALES TO MOD-KVVECKOR-LSALES (IX)                
035200     MOVE PASS-KVVECKOR-PUBV   TO MOD-KVVECKOR-PUBV   (IX)                
035300     MOVE PASS-PRARTSTD        TO MOD-PRARTSTD        (IX)                
035400     MOVE PASS-VLARTNTO        TO MOD-VLARTNTO        (IX)                
035500     MOVE PASS-KDPRODSL        TO MOD-KDPRODSL        (IX)                
035600     MOVE +1 TO IX1                                                       
035700     PERFORM UNTIL IX1 > MAX-IX1                                          
035800       MOVE PASS-ADLAGOMR (IX1) TO MOD-ADLAGOMR  (IX IX1)                 
035900       ADD +1 TO IX1                                                      
036000     END-PERFORM                                                          
036200     .                                                                    
036300 G-CHECK-INPUT SECTION.                                                   
036400     MOVE YES  TO INDATA-SW                                               
036600     IF (MID-KVVECKOR-LSALES-UP = ALL '+' ) AND                           
036700        (MID-KVVECKOR-PUBV-UP = ALL '+' ) AND                             
036800        (MID-PRARTSTD-UP      = ALL '+' ) AND                             
036900        (MID-VLARTNTO-UP      = ALL '+' ) AND                             
037000        (MID-KDPRODSL-UP      = ALL '+' ) AND                             
037100        (MID-ADLAGOMR-UP (1)  = ALL '+' ) AND                             
037200        (MID-ADLAGOMR-UP (2)  = ALL '+' ) AND                             
037300        (MID-ADLAGOMR-UP (3)  = ALL '+' ) AND                             
037400        (MID-ADLAGOMR-UP (4)  = ALL '+' ) AND                             
037500        (MID-ADLAGOMR-UP (5)  = ALL '+' ) AND                             
037600        (MID-ADLAGOMR-UP (6)  = ALL '+' ) AND                             
037700        (MID-ADLAGOMR-UP (7)  = ALL '+' ) AND                             
037800        (MID-ADLAGOMR-UP (8)  = ALL '+' ) AND                             
037900        (MID-ADLAGOMR-UP (9)  = ALL '+' ) AND                             
038000        (MID-ADLAGOMR-UP (10) = ALL '+' ) AND                             
038100        (MID-KDCMDVAL (1)     = ALL '+' ) AND                             
038200        (MID-KDCMDVAL (2)     = ALL '+' ) AND                             
038300        (MID-KDCMDVAL (3)     = ALL '+' ) AND                             
038400        (MID-KDCMDVAL (4)     = ALL '+' ) AND                             
038500        (MID-KDCMDVAL (5)     = ALL '+' ) AND                             
038600        (MID-KDCMDVAL (6)     = ALL '+' ) AND                             
038700        (MID-KDCMDVAL (7)     = ALL '+' ) AND                             
038800        (MID-KDCMDVAL (8)     = ALL '+' ) AND                             
038900        (MID-KDCMDVAL (9)     = ALL '+' ) AND                             
039000        (MID-KDCMDVAL (10)    = ALL '+' ) AND                             
039100        (MID-KDCMDVAL (11)    = ALL '+' ) AND                             
039200        (MID-KDCMDVAL (12)    = ALL '+' ) AND                             
039300        (MID-KDCMDVAL (13)    = ALL '+' )                                 
039500       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
039600       CALL WMEDKONV USING MED-WMEDAREA                                   
039700       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
039800       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
039900       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
040000       MOVE NOO TO INDATA-SW                                              
040100     ELSE                                                                 
040200       PERFORM GA-VALIDATE-INPUT-FIELDS                                   
040300       IF INDATA-WRONG                                                    
040310         IF ERR-UPDATE = YES                                              
040320           MOVE ERR-UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                    
040330         ELSE                                                             
040400           MOVE ERR-CORR-HILITE-FLDS   TO MED-IDMFSFEL                    
040401         END-IF                                                           
040402         CALL WMEDKONV USING MED-WMEDAREA                                 
040403         MOVE MED-MFSFEL           TO MOD-TEMFSFEL                        
040404         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
040405         PERFORM MFS-DONT-TOUCH-FIELD-IN                                  
040410       ELSE                                                               
040420         IF W-PAGE-FULL = 'Y'                                             
040430           MOVE ERR-UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                    
040500           CALL WMEDKONV USING MED-WMEDAREA                               
040600           MOVE MED-MFSFEL           TO MOD-TEMFSFEL                      
040700           PERFORM MFS-DONT-TOUCH-FIELD-OUT                               
040800           PERFORM MFS-DONT-TOUCH-FIELD-IN                                
040900         END-IF                                                           
040910       END-IF                                                             
041000     END-IF                                                               
041100     .                                                                    
041300 GA-VALIDATE-INPUT-FIELDS SECTION.                                        
041400**   VALIDATE LAST SALES WEEK                                             
041500     IF INDATA-OK                                                         
041800       IF MID-KVVECKOR-LSALES-UP NOT = ALL '+'                            
042000         IF MID-KVVECKOR-LSALES-UP NOT NUMERIC                            
042010           MOVE MID-KVVECKOR-LSALES-UP                                    
042020                                 TO MOD-KVVECKOR-LSALES-UP                
042100           MOVE MFS-NUM-FIELD-WRONG                                       
042200                                 TO MOD-KVVECKOR-LSALES-UP-ATTR           
042300           MOVE NOO              TO INDATA-SW                             
042400         ELSE                                                             
042410           IF MID-KVVECKOR-LSALES-UP = 0 OR                               
042420              MID-KVVECKOR-LSALES-UP > 260                                
042430             MOVE MID-KVVECKOR-LSALES-UP                                  
042440                                      TO MOD-KVVECKOR-LSALES-UP           
042450             MOVE MFS-NUM-FIELD-WRONG TO                                  
042460                                      MOD-KVVECKOR-LSALES-UP-ATTR         
042470             MOVE NOO                 TO INDATA-SW                        
042480           ELSE                                                           
042500             MOVE MFS-NUM-FIELD-OK TO MOD-KVVECKOR-LSALES-UP-ATTR         
042600           END-IF                                                         
042610         END-IF                                                           
042700       END-IF                                                             
042800     END-IF                                                               
042801                                                                          
042802     MOVE NOO                    TO UPDATE-DATA                           
042803                                    ERR-UPDATE                            
043000**   VALIDATE PUBLICATION WEEK                                            
043100     IF INDATA-OK                                                         
043200       IF MID-KVVECKOR-PUBV-UP > SPACES AND                               
043300          MID-KVVECKOR-PUBV-UP NOT = ALL '+'                              
043600         IF MID-KVVECKOR-PUBV-UP NOT NUMERIC                              
043700           MOVE MID-KVVECKOR-PUBV-UP                                      
043701                                 TO MOD-KVVECKOR-PUBV-UP                  
043710           MOVE MFS-NUM-FIELD-WRONG                                       
043800                                 TO MOD-KVVECKOR-PUBV-UP-ATTR             
043900           MOVE NOO              TO INDATA-SW                             
044000         ELSE                                                             
044010           MOVE YES              TO UPDATE-DATA                           
044100           MOVE MFS-NUM-FIELD-OK TO MOD-KVVECKOR-PUBV-UP-ATTR             
044200         END-IF                                                           
044300       END-IF                                                             
044400     END-IF                                                               
044500                                                                          
044600**   VALIDATE STANDARD PRICE                                              
044700     IF INDATA-OK                                                         
044800       IF MID-PRARTSTD-UP > SPACES AND                                    
044900          MID-PRARTSTD-UP NOT = ALL '+'                                   
045200         IF MID-PRARTSTD-UP NOT NUMERIC                                   
045300           MOVE MID-PRARTSTD-UP  TO MOD-PRARTSTD-UP                       
045310           MOVE MFS-NUM-FIELD-WRONG                                       
045400                                 TO MOD-PRARTSTD-UP-ATTR                  
045500           MOVE NOO              TO INDATA-SW                             
045600         ELSE                                                             
045700           MOVE MFS-NUM-FIELD-OK TO MOD-PRARTSTD-UP-ATTR                  
045710           MOVE YES              TO UPDATE-DATA                           
045800         END-IF                                                           
045900       END-IF                                                             
046000     END-IF                                                               
046100                                                                          
046200**   VALIDATE VOLUME                                                      
046300     IF INDATA-OK                                                         
046400       IF MID-VLARTNTO-UP > SPACES AND                                    
046500          MID-VLARTNTO-UP NOT = ALL '+'                                   
046800         IF MID-VLARTNTO-UP NOT NUMERIC                                   
046900           MOVE MID-VLARTNTO-UP  TO MOD-VLARTNTO-UP                       
046910           MOVE MFS-NUM-FIELD-WRONG                                       
047000                                 TO MOD-VLARTNTO-UP-ATTR                  
047100           MOVE NOO              TO INDATA-SW                             
047200         ELSE                                                             
047300           MOVE MFS-NUM-FIELD-OK TO MOD-VLARTNTO-UP-ATTR                  
047310           MOVE YES              TO UPDATE-DATA                           
047400         END-IF                                                           
047500       END-IF                                                             
047600     END-IF                                                               
047700                                                                          
047800**   VALIDATE PRODUCT GROUP                                               
047900     IF INDATA-OK                                                         
048000       IF MID-KDPRODSL-UP > SPACES AND                                    
048100          MID-KDPRODSL-UP NOT = ALL '+'                                   
048400         IF MID-KDPRODSL-UP NOT NUMERIC                                   
048500           MOVE MID-KDPRODSL-UP  TO MOD-KDPRODSL-UP                       
048510           MOVE MFS-NUM-FIELD-WRONG                                       
048600                                 TO MOD-KDPRODSL-UP-ATTR                  
048700           MOVE NOO              TO INDATA-SW                             
048800         ELSE                                                             
048900           MOVE MFS-NUM-FIELD-OK TO MOD-KDPRODSL-UP-ATTR                  
048910           MOVE YES              TO UPDATE-DATA                           
049000         END-IF                                                           
049100       END-IF                                                             
049200     END-IF                                                               
049300                                                                          
049400**   VALIDATE ADLAGOMR                                                    
049500     MOVE +1 TO IX1                                                       
049600     PERFORM UNTIL IX1 > MAX-IX1 OR INDATA-WRONG                          
049700       IF MID-ADLAGOMR-UP (IX1) > SPACES AND                              
049800          MID-ADLAGOMR-UP (IX1) NOT = ALL '+'                             
050100         IF MID-ADLAGOMR-UP (IX1) NOT NUMERIC                             
050200           MOVE MID-ADLAGOMR-UP (IX1)                                     
050201                                 TO MOD-ADLAGOMR-UP (IX1)                 
050210           MOVE MFS-NUM-FIELD-WRONG                                       
050300                                 TO MOD-ADLAGOMR-UP-ATTR (IX1)            
050400           MOVE NOO              TO INDATA-SW                             
050500         ELSE                                                             
050600           MOVE MFS-NUM-FIELD-OK TO MOD-ADLAGOMR-UP-ATTR (IX1)            
050610           MOVE YES              TO UPDATE-DATA                           
050700         END-IF                                                           
050800       END-IF                                                             
050900       ADD +1 TO IX1                                                      
051000     END-PERFORM                                                          
051100*                                                                         
051110     IF INDATA-OK                                                         
051140       IF MID-KDCMDVAL (1)  = ALL '+'  AND                                
051150          MID-KDCMDVAL (2)  = ALL '+'  AND                                
051160          MID-KDCMDVAL (3)  = ALL '+'  AND                                
051170          MID-KDCMDVAL (4)  = ALL '+'  AND                                
051180          MID-KDCMDVAL (5)  = ALL '+'  AND                                
051190          MID-KDCMDVAL (6)  = ALL '+'  AND                                
051191          MID-KDCMDVAL (7)  = ALL '+'  AND                                
051192          MID-KDCMDVAL (8)  = ALL '+'  AND                                
051193          MID-KDCMDVAL (9)  = ALL '+'  AND                                
051194          MID-KDCMDVAL (10) = ALL '+'  AND                                
051195          MID-KDCMDVAL (11) = ALL '+'  AND                                
051196          MID-KDCMDVAL (12) = ALL '+'  AND                                
051197          MID-KDCMDVAL (13) = ALL '+'  AND                                
051198          MID-KVVECKOR-LSALES-UP = ALL '+' AND                            
051199          UPDATE-DATA = YES                                               
051200         MOVE MFS-ALPHA-FIELD-WRONG                                       
051201                             TO MOD-KVVECKOR-LSALES-UP-ATTR               
051202         MOVE NOO            TO INDATA-SW                                 
051203         MOVE YES            TO ERR-UPDATE                                
051204       END-IF                                                             
051205     END-IF                                                               
051210**                                                                        
051300     IF INDATA-OK                                                         
051600       IF MID-KVVECKOR-LSALES-UP > 0 AND                                  
051700          MID-KDCMDVAL (1)  = ALL '+'  AND                                
051800          MID-KDCMDVAL (2)  = ALL '+'  AND                                
051900          MID-KDCMDVAL (3)  = ALL '+'  AND                                
051901          MID-KDCMDVAL (4)  = ALL '+'  AND                                
051902          MID-KDCMDVAL (5)  = ALL '+'  AND                                
051903          MID-KDCMDVAL (6)  = ALL '+'  AND                                
051904          MID-KDCMDVAL (7)  = ALL '+'  AND                                
051905          MID-KDCMDVAL (8)  = ALL '+'  AND                                
051906          MID-KDCMDVAL (9)  = ALL '+'  AND                                
051907          MID-KDCMDVAL (10) = ALL '+'  AND                                
051908          MID-KDCMDVAL (11) = ALL '+'  AND                                
051909          MID-KDCMDVAL (12) = ALL '+'  AND                                
051910          MID-KDCMDVAL (13) = ALL '+'                                     
051911**        TO CHECK IF THE PAGE IS ALREADY FULL AND NO MORE                
051912**        INSERTS CAN BE ALLOWED                                          
051913          IF MID-KVVECKOR-LSALES (MAX-IX) > SPACES                        
051920            MOVE YES                 TO W-PAGE-FULL                       
051930          ELSE                                                            
051940            IF MID-KVVECKOR-LSALES-UP = ALL '+'                           
051941              MOVE MFS-ALPHA-FIELD-WRONG                                  
051942                                  TO MOD-KVVECKOR-LSALES-UP-ATTR          
051943              MOVE NOO            TO INDATA-SW                            
051950            END-IF                                                        
052810          END-IF                                                          
053000       END-IF                                                             
053100     END-IF                                                               
053200*                                                                         
053300**   VALIDATE COMMAND CODES                                               
053400     MOVE +1 TO IX                                                        
053500     PERFORM UNTIL IX > MAX-IX OR INDATA-WRONG                            
053700                                                                          
053800       IF MID-KDCMDVAL (IX) NOT = ALL '+' AND                             
053900          MID-KDCMDVAL (IX) > SPACE                                       
054000         IF MID-KDCMDVAL (IX) NOT = 'E' AND                               
054100            MID-KDCMDVAL (IX) NOT = 'D' AND 'B'                           
054200           MOVE MID-KDCMDVAL (IX)   TO MOD-KDCMDVAL (IX)                  
054210           MOVE MFS-ALPHA-FIELD-WRONG                                     
054300                                    TO MOD-KDCMDVAL-ATTR (IX)             
054400           MOVE NOO                 TO INDATA-SW                          
054500         ELSE                                                             
054600           IF MID-KDCMDVAL (IX) = 'E'                                     
054700             ADD +1 TO W-CMD-CNT-E                                        
054800           ELSE                                                           
054810             IF MID-KDCMDVAL (IX) = 'D' OR 'B'                            
054900               ADD +1 TO W-CMD-CNT-D                                      
055000             END-IF                                                       
055010           END-IF                                                         
055100           IF W-CMD-CNT-E > 1 OR                                          
055200              (W-CMD-CNT-E >= 1 AND W-CMD-CNT-D >= 1)                     
055210             MOVE MID-KDCMDVAL (IX) TO MOD-KDCMDVAL (IX)                  
055300             MOVE MFS-ALPHA-FIELD-WRONG                                   
055400                                    TO MOD-KDCMDVAL-ATTR (IX)             
055500             MOVE NOO               TO INDATA-SW                          
055600           ELSE                                                           
055700             MOVE MFS-ALPHA-FIELD-OK TO MOD-KDCMDVAL-ATTR (IX)            
055800           END-IF                                                         
055900         END-IF                                                           
056000       END-IF                                                             
056100       ADD +1 TO IX                                                       
056200     END-PERFORM                                                          
056300     .                                                                    
056400 H-UPDATE SECTION.                                                        
056500**   CHECK FOR DIFFERENT COMMAND CODES                                    
056600     IF MID-KVVECKOR-LSALES-UP > 0 AND                                    
056610        MID-KDCMDVAL (1)  = ALL '+'  AND                                  
056620        MID-KDCMDVAL (2)  = ALL '+'  AND                                  
056630        MID-KDCMDVAL (3)  = ALL '+'  AND                                  
056640        MID-KDCMDVAL (4)  = ALL '+'  AND                                  
056650        MID-KDCMDVAL (5)  = ALL '+'  AND                                  
056660        MID-KDCMDVAL (6)  = ALL '+'  AND                                  
056670        MID-KDCMDVAL (7)  = ALL '+'  AND                                  
056680        MID-KDCMDVAL (8)  = ALL '+'  AND                                  
056690        MID-KDCMDVAL (9)  = ALL '+'  AND                                  
056691        MID-KDCMDVAL (10) = ALL '+'  AND                                  
056692        MID-KDCMDVAL (11) = ALL '+'  AND                                  
056693        MID-KDCMDVAL (12) = ALL '+'  AND                                  
056694        MID-KDCMDVAL (13) = ALL '+'                                       
056700       PERFORM HA-INSERT-NEW-RULE                                         
056800     ELSE                                                                 
056900       MOVE +1 TO IX                                                      
057000       PERFORM UNTIL IX > MAX-IX OR INDATA-WRONG                          
057100         IF MID-KDCMDVAL (IX) = 'D' OR 'B'                                
057200           PERFORM HB-DELETE-RULE-LINE                                    
057300         ELSE                                                             
057400           IF MID-KDCMDVAL (IX) = 'E'                                     
057410             PERFORM HB-DELETE-RULE-LINE                                  
057500             PERFORM HC-UPDATE-RULE-LINE                                  
057600           END-IF                                                         
057700         END-IF                                                           
057800         ADD +1 TO IX                                                     
057900       END-PERFORM                                                        
058000     END-IF                                                               
058100                                                                          
058200     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
058300     CALL WMEDKONV USING MED-WMEDAREA                                     
058400     MOVE MED-MFSINF      TO MOD-TEMFSINF                                 
058500     PERFORM MFS-FORM-ATTR                                                
058600     PERFORM MFS-ERASE-FIELD-IN                                           
058700* * * MFS-DO-NOT-TOUCH-FIELD TO LOCKED VALUES                             
058800     .                                                                    
059000 HA-INSERT-NEW-RULE  SECTION.                                             
059101     MOVE MID-KVVECKOR-LSALES-UP   TO PASS-KVVECKOR-LSALES                
059102                                                                          
059110     IF MID-KVVECKOR-PUBV-UP = ALL '+'                                    
059120       MOVE ZERO                   TO PASS-KVVECKOR-PUBV                  
059130     ELSE                                                                 
059200       MOVE MID-KVVECKOR-PUBV-UP   TO PASS-KVVECKOR-PUBV                  
059210     END-IF                                                               
059220                                                                          
059230     IF MID-PRARTSTD-UP = ALL '+'                                         
059240       MOVE ZERO                   TO PASS-PRARTSTD                       
059250     ELSE                                                                 
059300       MOVE MID-PRARTSTD-UP        TO PASS-PRARTSTD                       
059301     END-IF                                                               
059310                                                                          
059320     IF MID-VLARTNTO-UP = ALL '+'                                         
059330       MOVE ZERO                   TO PASS-VLARTNTO                       
059340     ELSE                                                                 
059400       MOVE MID-VLARTNTO-UP        TO PASS-VLARTNTO                       
059401     END-IF                                                               
059410                                                                          
059420     IF MID-KDPRODSL-UP = ALL '+'                                         
059430       MOVE ZERO                   TO PASS-KDPRODSL                       
059440     ELSE                                                                 
059500       MOVE MID-KDPRODSL-UP        TO PASS-KDPRODSL                       
059501     END-IF                                                               
059510                                                                          
059600     MOVE +1 TO IX1                                                       
059700     PERFORM UNTIL IX1 > MAX-IX1                                          
059710       IF MID-ADLAGOMR-UP (IX1) = ALL '+'                                 
059720         MOVE ZERO                  TO TAB-ADLAGOMR (IX1)                 
059730       ELSE                                                               
059800         MOVE MID-ADLAGOMR-UP (IX1) TO TAB-ADLAGOMR (IX1)                 
059810       END-IF                                                             
059820       MOVE ZERO                    TO PASS-ADLAGOMR (IX1)                
059900       ADD +1 TO IX1                                                      
060000     END-PERFORM                                                          
060100*                                                                         
060101     PERFORM S01-SORT-TABELL                                              
060110*                                                                         
060200     PERFORM IMS-ISRT-WDB613                                              
060201                                                                          
060210     PERFORM IMS-GHU-WDB615                                               
060220     IF SEGMENT-FOUND                                                     
060230       MOVE TODAYS-DATE TO LOGG-TIUPPDAT                                  
060240       MOVE MSGI-IDUSER TO LOGG-IDUSER                                    
060250       PERFORM IMS-REPL-WDB615                                            
060251     ELSE                                                                 
060252       MOVE '2366'      TO LOGG-IDTRANS                                   
060253       MOVE SPACE       TO LOGG-IDDC-REF                                  
060254       MOVE TODAYS-DATE TO LOGG-TIUPPDAT                                  
060255       MOVE MSGI-IDUSER TO LOGG-IDUSER                                    
060256       PERFORM IMS-ISRT-WDB615                                            
060260     END-IF                                                               
060300     .                                                                    
060400 HB-DELETE-RULE-LINE SECTION.                                             
060410     INSPECT MID-KVVECKOR-LSALES (IX) REPLACING                           
060420                        LEADING SPACES BY ZEROS                           
060500     MOVE MID-KVVECKOR-LSALES (IX) TO PASS-KVVECKOR-LSALES                
060510                                      W-PASS-KVVECKOR-LSALES              
060520*                                                                         
060530     INSPECT MID-KVVECKOR-PUBV (IX) REPLACING                             
060540                      LEADING SPACES BY ZEROS                             
060600     MOVE MID-KVVECKOR-PUBV   (IX) TO PASS-KVVECKOR-PUBV                  
060610                                      W-PASS-KVVECKOR-PUBV                
060620*                                                                         
060730     INSPECT MID-PRARTSTD (IX) REPLACING                                  
060740                 LEADING SPACES BY ZEROS                                  
060741     MOVE MID-PRARTSTD        (IX) TO PASS-PRARTSTD                       
060742                                      W-PASS-PRARTSTD                     
060750*                                                                         
060760     INSPECT MID-VLARTNTO (IX) REPLACING                                  
060770                 LEADING SPACES BY ZEROS                                  
060800     MOVE MID-VLARTNTO        (IX) TO PASS-VLARTNTO                       
060810                                      W-PASS-VLARTNTO                     
060820*                                                                         
060830     INSPECT MID-KDPRODSL (IX) REPLACING                                  
060840                 LEADING SPACES BY ZEROS                                  
060900     MOVE MID-KDPRODSL        (IX) TO PASS-KDPRODSL                       
060910                                      W-PASS-KDPRODSL                     
060920*                                                                         
060931     INSPECT MID-ADLAGOMR (IX 1) REPLACING                                
060940                      LEADING SPACES BY ZEROS                             
061000     MOVE MID-ADLAGOMR      (IX 1) TO PASS-ADLAGOMR (1)                   
061010                                      W-PASS-ADLAGOMR                     
061020                                                                          
061100     PERFORM IMS-GHU-WDB613                                               
061110     IF SEGMENT-FOUND                                                     
061200       PERFORM IMS-DLET-WDB613                                            
061210     END-IF                                                               
061211                                                                          
061213     PERFORM IMS-GHU-WDB615                                               
061214     IF SEGMENT-FOUND                                                     
061240       MOVE TODAYS-DATE  TO LOGG-TIUPPDAT                                 
061250       MOVE MSGI-IDUSER  TO LOGG-IDUSER                                   
061260       PERFORM IMS-REPL-WDB615                                            
061261     ELSE                                                                 
061262       MOVE '2366'       TO LOGG-IDTRANS                                  
061263       MOVE SPACE        TO LOGG-IDDC-REF                                 
061264       MOVE TODAYS-DATE  TO LOGG-TIUPPDAT                                 
061265       MOVE MSGI-IDUSER  TO LOGG-IDUSER                                   
061266       PERFORM IMS-ISRT-WDB615                                            
061270     END-IF                                                               
061300     .                                                                    
061400 HC-UPDATE-RULE-LINE SECTION.                                             
061420     IF MID-KVVECKOR-LSALES-UP NOT = ALL '+'                              
061500       MOVE MID-KVVECKOR-LSALES-UP   TO PASS-KVVECKOR-LSALES              
061510     ELSE                                                                 
061511       INSPECT MID-KVVECKOR-LSALES (IX) REPLACING                         
061512                          LEADING SPACES BY ZEROS                         
061520       MOVE MID-KVVECKOR-LSALES (IX) TO PASS-KVVECKOR-LSALES              
061530     END-IF                                                               
061600                                                                          
061700     IF MID-KVVECKOR-PUBV-UP NOT = ALL '+'                                
061800       MOVE MID-KVVECKOR-PUBV-UP   TO PASS-KVVECKOR-PUBV                  
061900     ELSE                                                                 
061920       INSPECT MID-KVVECKOR-PUBV (IX) REPLACING                           
061930                        LEADING SPACES BY ZEROS                           
062000       MOVE MID-KVVECKOR-PUBV (IX) TO PASS-KVVECKOR-PUBV                  
062100     END-IF                                                               
062200                                                                          
062300     IF MID-PRARTSTD-UP NOT = ALL '+'                                     
062400       MOVE MID-PRARTSTD-UP        TO PASS-PRARTSTD                       
062500     ELSE                                                                 
062510       INSPECT MID-PRARTSTD (IX) REPLACING                                
062520                   LEADING SPACES BY ZEROS                                
062600       MOVE MID-PRARTSTD (IX)      TO PASS-PRARTSTD                       
062700     END-IF                                                               
062800                                                                          
062900     IF MID-VLARTNTO-UP NOT = ALL '+'                                     
063000       MOVE MID-VLARTNTO-UP        TO PASS-VLARTNTO                       
063100     ELSE                                                                 
063110       INSPECT MID-VLARTNTO (IX) REPLACING                                
063120                   LEADING SPACES BY ZEROS                                
063200       MOVE MID-VLARTNTO (IX)      TO PASS-VLARTNTO                       
063300     END-IF                                                               
063400                                                                          
063500     IF MID-KDPRODSL-UP NOT = ALL '+'                                     
063600       MOVE MID-KDPRODSL-UP        TO PASS-KDPRODSL                       
063700     ELSE                                                                 
063710       INSPECT MID-KDPRODSL (IX) REPLACING                                
063720                   LEADING SPACES BY ZEROS                                
063800       MOVE MID-KDPRODSL (IX)      TO PASS-KDPRODSL                       
063900     END-IF                                                               
064000                                                                          
064100     MOVE +1 TO IX1                                                       
064200     PERFORM UNTIL IX1 > MAX-IX1                                          
064300       IF MID-ADLAGOMR-UP (IX1) NOT = ALL '+'                             
064400         MOVE MID-ADLAGOMR-UP (IX1)   TO TAB-ADLAGOMR (IX1)               
064500       ELSE                                                               
064510         INSPECT MID-ADLAGOMR (IX IX1) REPLACING                          
064520                         LEADING SPACES BY ZEROS                          
064600         MOVE MID-ADLAGOMR (IX IX1)   TO TAB-ADLAGOMR (IX1)               
064700       END-IF                                                             
064710       MOVE ZERO                      TO PASS-ADLAGOMR (IX1)              
064800       ADD +1 TO IX1                                                      
064900     END-PERFORM                                                          
065100*                                                                         
065101     PERFORM S01-SORT-TABELL                                              
065102*                                                                         
065110     IF SEGMENT-FOUND                                                     
065300       PERFORM IMS-ISRT-WDB613                                            
065301                                                                          
065309       PERFORM IMS-GHU-WDB615                                             
065310       IF SEGMENT-FOUND                                                   
065311         MOVE TODAYS-DATE  TO LOGG-TIUPPDAT                               
065312         MOVE MSGI-IDUSER  TO LOGG-IDUSER                                 
065313         PERFORM IMS-REPL-WDB615                                          
065314       ELSE                                                               
065315         MOVE '2366'       TO LOGG-IDTRANS                                
065316         MOVE SPACE        TO LOGG-IDDC-REF                               
065317         MOVE TODAYS-DATE  TO LOGG-TIUPPDAT                               
065318         MOVE MSGI-IDUSER  TO LOGG-IDUSER                                 
065319         PERFORM IMS-ISRT-WDB615                                          
065320       END-IF                                                             
065330     END-IF                                                               
065400     .                                                                    
065500 S01-SORT-TABELL SECTION.                                                 
065600     MOVE +2    TO STEGLAANGD                                             
065700     MOVE +10   TO ANTAL                                                  
065800     MOVE +2    TO NYCKELLAANGD                                           
065900                                                                          
066000     CALL WINTSOR USING SORT-TABELL STEGLAANGD ANTAL                      
066100     TAB-SORT-BEGREPP(1) NYCKELLAANGD                                     
066110                                                                          
066120     MOVE +1 TO TAB-IX1                                                   
066121     MOVE +1 TO IX1                                                       
066130     PERFORM UNTIL TAB-IX1 > MAX-IX1                                      
066140       IF TAB-ADLAGOMR (TAB-IX1) > 0                                      
066170         MOVE TAB-ADLAGOMR (TAB-IX1)    TO PASS-ADLAGOMR (IX1)            
066190         ADD +1 TO IX1                                                    
066191       END-IF                                                             
066192       ADD +1 TO TAB-IX1                                                  
066193     END-PERFORM                                                          
066200     .                                                                    
066300 MFS-ERASE-FIELD-OUT SECTION.                                             
066400*    --- ALLA UTDATA-FÄLT                                                 
066510     MOVE MFS-ERASE-FIELD TO MOD-KVVECKOR-LSALES-UP                       
066520                             MOD-KVVECKOR-PUBV-UP                         
066530                             MOD-PRARTSTD-UP                              
066540                             MOD-VLARTNTO-UP                              
066550                             MOD-KDPRODSL-UP                              
066593     MOVE +1 TO IX1                                                       
066595     PERFORM UNTIL IX1 > MAX-IX1                                          
066596       MOVE MFS-ERASE-FIELD TO MOD-ADLAGOMR-UP(IX1)                       
066597       ADD +1 TO IX1                                                      
066598     END-PERFORM                                                          
066599                                                                          
066610     .                                                                    
066700 MFS-CLOSE-FIELD-IN SECTION.                                              
066800*    --- ALLA INDATA-FÄLT                                                 
066920     MOVE MFS-CLOSE-FIELD TO MOD-KVVECKOR-LSALES-UP-ATTR                  
066930                             MOD-KVVECKOR-PUBV-UP-ATTR                    
066940                             MOD-PRARTSTD-UP-ATTR                         
066950                             MOD-VLARTNTO-UP-ATTR                         
066960                             MOD-KDPRODSL-UP-ATTR                         
066970     MOVE +1 TO IX1                                                       
066980     PERFORM UNTIL IX1 > MAX-IX1                                          
066990       MOVE MFS-CLOSE-FIELD TO MOD-ADLAGOMR-UP-ATTR (IX1)                 
066991       ADD +1 TO IX1                                                      
066992     END-PERFORM                                                          
066993                                                                          
066994     MOVE +1 TO IX                                                        
066995     PERFORM UNTIL IX > MAX-IX                                            
066996       MOVE MFS-CLOSE-FIELD TO MOD-KDCMDVAL-ATTR (IX)                     
066997       ADD +1 TO IX                                                       
066998     END-PERFORM                                                          
067000     .                                                                    
067100 MFS-ERASE-FIELD-IN SECTION.                                              
067101*    --- ALLA INDATA-FÄLT                                                 
067102     MOVE MFS-ERASE-FIELD TO MOD-IDDC-IN                                  
067104     MOVE MFS-ERASE-FIELD TO MOD-KVVECKOR-LSALES-UP                       
067105                             MOD-KVVECKOR-PUBV-UP                         
067106                             MOD-PRARTSTD-UP                              
067107                             MOD-VLARTNTO-UP                              
067108                             MOD-KDPRODSL-UP                              
067109     MOVE +1 TO IX1                                                       
067110     PERFORM UNTIL IX1 > MAX-IX1                                          
067111       MOVE MFS-ERASE-FIELD TO MOD-ADLAGOMR-UP (IX1)                      
067112       ADD +1 TO IX1                                                      
067113     END-PERFORM                                                          
067114                                                                          
067115     MOVE +1 TO IX                                                        
067116     PERFORM UNTIL IX > MAX-IX                                            
067117       MOVE MFS-ERASE-FIELD TO MOD-KDCMDVAL (IX)                          
067119       ADD +1 TO IX                                                       
067120     END-PERFORM                                                          
067121     .                                                                    
067130 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
067200*    --- ALLA UTDATA-FÄLT                                                 
067300     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDDC-UT                           
067310                                                                          
067400     MOVE +1 TO IX                                                        
067500     PERFORM UNTIL IX > MAX-IX                                            
067600       PERFORM MFS-DONT-TOUCH-LINE-FIELD-OUT                              
067700       ADD +1 TO IX                                                       
067800     END-PERFORM                                                          
067900     .                                                                    
068000 MFS-DONT-TOUCH-LINE-FIELD-OUT  SECTION.                                  
068100     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDCMDVAL       (IX)               
068200                                    MOD-KVVECKOR-LSALES(IX)               
068300                                    MOD-KVVECKOR-PUBV  (IX)               
068400                                    MOD-PRARTSTD       (IX)               
068500                                    MOD-VLARTNTO       (IX)               
068600                                    MOD-KDPRODSL       (IX)               
068700     MOVE +1 TO IX1                                                       
068800     PERFORM UNTIL IX1 > MAX-IX1                                          
068900       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-ADLAGOMR (IX IX1)               
069000       ADD +1 TO IX1                                                      
069100     END-PERFORM                                                          
069200     .                                                                    
069300 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
069400*    --- ALLA INDATA-FÄLT                                                 
069500     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDDC-IN                           
069520                                    MOD-KVVECKOR-LSALES-UP                
069530                                    MOD-KVVECKOR-PUBV-UP                  
069540                                    MOD-PRARTSTD-UP                       
069550                                    MOD-VLARTNTO-UP                       
069560                                    MOD-KDPRODSL-UP                       
069570     MOVE +1 TO IX1                                                       
069580     PERFORM UNTIL IX1 > MAX-IX1                                          
069590       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-ADLAGOMR-UP (IX1)               
069591       ADD +1 TO IX1                                                      
069592     END-PERFORM                                                          
069593                                                                          
069594     MOVE +1 TO IX                                                        
069595     PERFORM UNTIL IX > MAX-IX                                            
069596       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDCMDVAL (IX)                   
069597       ADD +1 TO IX                                                       
069598     END-PERFORM                                                          
069600     .                                                                    
069700 MFS-FORM-ATTR SECTION.                                                   
069800*    --- ALL INDATA-FIELDS                                                
069900     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-IDDC-IN-ATTR                     
069920                                    MOD-KVVECKOR-LSALES-UP-ATTR           
069930                                    MOD-KVVECKOR-PUBV-UP-ATTR             
069940                                    MOD-PRARTSTD-UP-ATTR                  
069950                                    MOD-VLARTNTO-UP-ATTR                  
069960                                    MOD-KDPRODSL-UP-ATTR                  
069970     MOVE +1 TO IX1                                                       
069980     PERFORM UNTIL IX1 > MAX-IX1                                          
069990       MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-ADLAGOMR-UP-ATTR(IX1)          
069991       ADD +1 TO IX1                                                      
069992     END-PERFORM                                                          
069993                                                                          
069994     MOVE +1 TO IX                                                        
069995     PERFORM UNTIL IX > MAX-IX                                            
069996       MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-KDCMDVAL-ATTR (IX)             
069997       ADD +1 TO IX                                                       
069998     END-PERFORM                                                          
070000     .                                                                    
070600 MFS-CLOSE-ERASE-FIELD SECTION.                                           
070700     MOVE MFS-CLOSE-FIELD TO MOD-KDCMDVAL-ATTR  (IX)                      
070710     MOVE MFS-ERASE-FIELD TO MOD-KDCMDVAL       (IX)                      
070800                             MOD-KVVECKOR-LSALES(IX)                      
070900                             MOD-KVVECKOR-PUBV  (IX)                      
071000                             MOD-PRARTSTD       (IX)                      
071100                             MOD-VLARTNTO       (IX)                      
071200                             MOD-KDPRODSL       (IX)                      
071300     MOVE +1 TO IX1                                                       
071400     PERFORM UNTIL IX1 > MAX-IX1                                          
071500       MOVE MFS-ERASE-FIELD TO MOD-ADLAGOMR (IX IX1)                      
071600       ADD +1 TO IX1                                                      
071700     END-PERFORM                                                          
071800     .                                                                    
071900* --- IMS SECTIONS ---                                                    
072000 IMS-GET-MSG SECTION.                                                     
072100     MOVE '  QC' TO GOOD-STATUSCODES                                      
072200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
072300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
072400     PERFORM IMS-STATUSCHECK                                              
072500     .                                                                    
072600 IMS-INSERT-MSG SECTION.                                                  
073000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
073100     MOVE SPACE TO GOOD-STATUSCODES                                       
073200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
073300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
073400     PERFORM IMS-STATUSCHECK                                              
073500     .                                                                    
073600 IMS-GHU-WDB601 SECTION.                                                  
073700     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
073800          DELIMITED BY SIZE INTO SSA1                                     
073900     MOVE '  GE' TO GOOD-STATUSCODES                                      
074000     CALL CBLTDLI USING GHU WDB6-PCB DLI-IO-WDB601 SSA1                   
074100     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
074200     PERFORM IMS-STATUSCHECK                                              
074300     .                                                                    
074301 IMS-REPL-WDB601 SECTION.                                                 
074304     MOVE '  ' TO GOOD-STATUSCODES                                        
074305     CALL CBLTDLI USING REPL WDB6-PCB DLI-IO-WDB601                       
074306     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
074307     PERFORM IMS-STATUSCHECK                                              
074308     .                                                                    
074310 IMS-GU-WDB601 SECTION.                                                   
074320     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
074330          DELIMITED BY SIZE INTO SSA1                                     
074340     MOVE '  GE' TO GOOD-STATUSCODES                                      
074350     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
074360     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
074370     PERFORM IMS-STATUSCHECK                                              
074380     .                                                                    
074400 IMS-GNP-WDB613 SECTION.                                                  
074700     MOVE 'WDB613  '       TO SSA1                                        
074800     MOVE '  GE' TO GOOD-STATUSCODES                                      
074900     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB613 SSA1                   
075000     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
075100     PERFORM IMS-STATUSCHECK                                              
075200     .                                                                    
075300 IMS-GHU-WDB613 SECTION.                                                  
075400     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
075500          DELIMITED BY SIZE INTO SSA1                                     
075510     STRING 'WDB613  (WDB613KY =' W-WDB613KY-X ')'                        
075520          DELIMITED BY SIZE INTO SSA2                                     
075600     MOVE '  GE' TO GOOD-STATUSCODES                                      
075700     CALL CBLTDLI USING GHU WDB6-PCB DLI-IO-WDB613 SSA1 SSA2              
075800     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
075900     PERFORM IMS-STATUSCHECK                                              
076000     .                                                                    
076900 IMS-ISRT-WDB613 SECTION.                                                 
077000     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
077100          DELIMITED BY SIZE INTO SSA1                                     
077210     MOVE 'WDB613 ' TO SSA2                                               
077300     MOVE '  II' TO GOOD-STATUSCODES                                      
077400     CALL CBLTDLI USING ISRT WDB6-PCB DLI-IO-WDB613 SSA1 SSA2             
077500     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
077600     PERFORM IMS-STATUSCHECK                                              
077700     .                                                                    
077800 IMS-DLET-WDB613 SECTION.                                                 
077900     MOVE '  ' TO GOOD-STATUSCODES                                        
078000     CALL CBLTDLI USING DLET WDB6-PCB DLI-IO-WDB613                       
078100     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
078200     PERFORM IMS-STATUSCHECK                                              
078300     .                                                                    
078301     EJECT                                                                
078310 IMS-GNP-WDB615 SECTION.                                                  
078311                                                                          
078321     STRING 'WDB615  (IDTRANS  =' W-IDTRANS-B6-X ')'                      
078322     DELIMITED BY SIZE INTO SSA1                                          
078330     MOVE '  GE' TO GOOD-STATUSCODES                                      
078340     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB615 SSA1                   
078350     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
078360     PERFORM IMS-STATUSCHECK                                              
078370     .                                                                    
078380     EJECT                                                                
078381 IMS-GHU-WDB615 SECTION.                                                  
078382                                                                          
078383     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
078384          DELIMITED BY SIZE INTO SSA1                                     
078385     STRING 'WDB615  (IDTRANS  =' W-IDTRANS-B6-X ')'                      
078386          DELIMITED BY SIZE INTO SSA2                                     
078387     MOVE '  GE' TO GOOD-STATUSCODES                                      
078388     CALL CBLTDLI USING GHU WDB6-PCB DLI-IO-WDB615 SSA1 SSA2              
078389     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
078390     PERFORM IMS-STATUSCHECK                                              
078391     .                                                                    
078392     EJECT                                                                
078393 IMS-REPL-WDB615 SECTION.                                                 
078394                                                                          
078395     MOVE '  ' TO GOOD-STATUSCODES                                        
078396     CALL CBLTDLI USING REPL WDB6-PCB DLI-IO-WDB615                       
078397     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
078398     PERFORM IMS-STATUSCHECK                                              
078399     .                                                                    
078400     EJECT                                                                
078401 IMS-ISRT-WDB615 SECTION.                                                 
078402                                                                          
078403     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
078404          DELIMITED BY SIZE INTO SSA1                                     
078405     MOVE 'WDB615  ' TO SSA2                                              
078406     MOVE '    ' TO GOOD-STATUSCODES                                      
078407     CALL CBLTDLI USING ISRT WDB6-PCB DLI-IO-WDB615 SSA1 SSA2             
078408     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
078409     PERFORM IMS-STATUSCHECK                                              
078410     .                                                                    
078411     EJECT                                                                
078420 IMS-STATUSCHECK SECTION.                                                 
078500     SET STATUS-IX TO 1                                                   
078600     SEARCH GOOD-STATUS                                                   
078700       AT END                                                             
078800         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
078900         DELIMITED BY SIZE INTO ERROR-TEXT                                
079000         CALL FELLOG                                                      
079100       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
079200         CONTINUE                                                         
079300     END-SEARCH                                                           
079400     .                                                                    
