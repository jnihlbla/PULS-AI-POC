001400 ID DIVISION.                                                             
001500 PROGRAM-ID.     W4022800.                                                
001600 AUTHOR.         HENRIKSSON ANDERS.                                       
001700 DATE-WRITTEN.   03/03/17.                                                
001800 DATE-COMPILED.                                                           
001900                                                                          
002000*    FUNCTION:                                                            
002100*        VOR QUESTION                                                     
002200*                                                                         
002310*        THE PROGRAM READS     WDA6                                       
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSACTION: W4T228                                              
002610*        TRANSACTION: W4T278                                              
002700*        MID:         W4I22801                                            
002800*                                                                         
002900*    OUTDATA.                                                             
003000*        MOD:         W4O22801                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400                                                                          
003500 DATA DIVISION.                                                           
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W4022800'.            
003900                                                                          
004000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004200                                                                          
004300 77  YES                         PIC X       VALUE 'J'.                   
004400 77  NOO                         PIC X       VALUE 'N'.                   
004410 77  OBEHORIG                    PIC X       VALUE 'F'.                   
004500                                                                          
004601*    --- INDEX FOR SCROLL LINES                                           
004602 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004613 77  MAX-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
004620 77  COUNTER1                    PIC S9(4)  VALUE +0    COMP-3.           
004630 77  COUNTER2                    PIC S9(4)  VALUE +0    COMP-3.           
004640 77  COUNTER3                    PIC S9(4)  VALUE +0    COMP-3.           
004700*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004900                                                                          
005100                                                                          
005200 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005300     88  KEYS-OK                             VALUE 'J'.                   
005400     88  KEYS-WRONG                          VALUE 'N'.                   
005500                                                                          
005600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005700     88  OWN-MID                             VALUE '4228'.                
006300     88  HELP-MID                            VALUE '0551'.                
006301     88  SWAP-MID                            VALUE '4278'.                
006310                                                                          
006311 77  SEARCH-IDORDER-SW           PIC X       VALUE 'N'.                   
006312     88  SEARCH-IDORDER                      VALUE 'J'.                   
006313     88  DONT-SEARCH-IDORDER                 VALUE 'N'.                   
006314                                                                          
006315 77  SEARCH-LEVORDER-SW          PIC X       VALUE 'N'.                   
006316     88  SEARCH-LEVORDER                     VALUE 'J'.                   
006317     88  DONT-SEARCH-LEVORDER                VALUE 'N'.                   
006318                                                                          
006320 01  W-IDKUNDNR                 PIC 9(6)     VALUE ZERO.                  
006330 01  W-IDORDNR7                 PIC 9(7)     VALUE ZERO.                  
006340 01  W-IDORDNR                  PIC 9(7)     VALUE ZERO.                  
006341 01  WS-IDDISTR                 PIC 9(4)     VALUE ZERO.                  
006350 01  WS-KDBEHX-EDIT             PIC X(1)     VALUE SPACE.                 
006360 01  WS-IDKUNDRF                PIC X(10).                                
006370 01  WS-IDORDNR7-FILLER REDEFINES WS-IDKUNDRF.                            
006380     03 WS-IDORDNR7             PIC 9(7).                                 
006390     03 WS-FILL-IDORDNR7        PIC X(3).                                 
006391 01  WS-REGTID                  PIC 9(8).                                 
006392 01  WS-TIHHMM-FILLER REDEFINES WS-REGTID.                                
006393     03 WS-TIHHMM               PIC 9(2)V9(2).                            
006394     03 FILLER                  PIC 9(4).                                 
006395 01  WS-TIKLATID                PIC 9(6).                                 
006396 01  WS-TIKLATID-FILLER REDEFINES WS-TIKLATID.                            
006397     03 WS-TIKLARTID            PIC 9(2)V9(2).                            
006398     03 FILLER                  PIC 9(4).                                 
006400     EJECT                                                                
006410                                                                          
006500*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006600 01  GENERAL-SUBPROGRAMS.                                                 
006610     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
006700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007200     EJECT                                                                
007210                                                                          
007300*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
007400*01 -COPY WMEDAREA                                                        
007401                                                                          
007410*    --- PARAMETRAR TILL SUBPROGRAM WSECURIT                              
007420*   -COPY WSECAREA                                                        
007500     SKIP3                                                                
007510                                                                          
007600 01  MESSAGE-CODES.                                                       
007801     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007802     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007803     03  INF-PRESS-PF11          PIC X(3)    VALUE '111'.                 
007810     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008000     03  ERR-OBEHORIG            PIC X(3)    VALUE '405'.                 
008100     EJECT                                                                
008200*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
008300*                                                                         
008400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008500     SKIP3                                                                
008600*01 -COPY WMSGINIT                                                        
008700     EJECT                                                                
008800*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
008900*                                                                         
009000 01  SAVE-AREA.                                                           
009100     03  SAVE-IDTRANS             PIC X(4)  VALUE SPACE.                  
009200     03  SAVE-IDORDNR7-MID        PIC S9(7) VALUE ZERO COMP-3.            
009201     03  SAVE-IDORDNR7-JUMP       PIC S9(7) VALUE ZERO COMP-3.            
009202     03  SAVE-FLORDLEV-ENTER        PIC X(1) VALUE SPACE.                 
009203                                                                          
009204     03  SAVE-IDDISTR-ENTER       PIC S9(5) VALUE ZERO COMP-3.            
009205     03  SAVE-IDKUNDNR-ENTER      PIC S9(7) VALUE ZERO COMP-3.            
009206     03  SAVE-IDORDNR7-ENTER      PIC S9(7) VALUE ZERO COMP-3.            
009208     03  SAVE-IDORDNR-ENTER       PIC S9(7) VALUE ZERO COMP-3.            
009209     03  SAVE-TIREGDAT-ENTER      PIC S9(7) VALUE ZERO COMP-3.            
009210     03  SAVE-TILEVDAT-ENTER      PIC S9(7) VALUE ZERO COMP-3.            
009211     03  SAVE-IDARTNR-ENTER       PIC S9(9) VALUE ZERO COMP-3.            
009212     03  SAVE-TIREGTID-ENTER      PIC S9(9) VALUE ZERO COMP-3.            
009213     03  SAVE-TILEVTID-ENTER      PIC S9(9) VALUE ZERO COMP-3.            
009214     03  SAVE-TIREGDAT-AVV-ENTER  PIC S9(7) VALUE ZERO COMP-3.            
009215     03  SAVE-TIREGTID-AVV-ENTER  PIC S9(9) VALUE ZERO COMP-3.            
009216                                                                          
009217     03  SAVE-IDDISTR-NEXT        PIC S9(5) VALUE ZERO COMP-3.            
009218     03  SAVE-IDKUNDNR-NEXT       PIC S9(7) VALUE ZERO COMP-3.            
009219     03  SAVE-IDORDNR7-NEXT       PIC S9(7) VALUE ZERO COMP-3.            
009220     03  SAVE-IDORDNR-NEXT        PIC S9(7) VALUE ZERO COMP-3.            
009221     03  SAVE-TIREGDAT-NEXT       PIC S9(7) VALUE ZERO COMP-3.            
009222     03  SAVE-TILEVDAT-NEXT       PIC S9(7) VALUE ZERO COMP-3.            
009223     03  SAVE-IDARTNR-NEXT        PIC S9(9) VALUE ZERO COMP-3.            
009230     03  SAVE-TIREGTID-NEXT       PIC S9(9) VALUE ZERO COMP-3.            
009231     03  SAVE-TILEVTID-NEXT       PIC S9(9) VALUE ZERO COMP-3.            
009240     03  SAVE-TIREGDAT-AVV-NEXT   PIC S9(7) VALUE ZERO COMP-3.            
009250     03  SAVE-TIREGTID-AVV-NEXT   PIC S9(9) VALUE ZERO COMP-3.            
009300     EJECT                                                                
009400*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
009500*                                                                         
009600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009700     SKIP3                                                                
009800*01  MID -COPY W4I22801                                                   
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010100     SKIP3                                                                
010200*01  -COPY WMSGAREA                                                       
010300     EJECT                                                                
010301     03  MOD REDEFINES MSG-AREA.                                          
010302*      05  -COPY W4O22801                                                 
010303                                                                          
010310*    --- FÖR HOPP TILL 4278-TEXTBILD                                      
010320*01  -COPY W4I27801 -PRE 4278-                                            
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010800     SKIP3                                                                
010900*01  -COPY WMFSAREA                                                       
011000     EJECT                                                                
011100*    --- WORK-AREAS FOR IMS-SECTIONS                                      
011200*                                                                         
011300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011400     SKIP3                                                                
011500 01  KEYS-TO-DLI.                                                         
011601*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
011602     03  W-IDORDNR7-MIN-X.                                                
011603         05  W-IDORDNR7-MIN      PIC S9(7) VALUE ZERO COMP-3.             
011604                                                                          
011605     03  W-IDORDNR-MIN-X.                                                 
011606         05  W-IDORDNR-MIN       PIC S9(7) VALUE ZERO COMP-3.             
011607                                                                          
011608     03  W-IDDISTR-X.                                                     
011609         05  W-IDDISTR           PIC S9(4) VALUE ZERO COMP-3.             
011610                                                                          
011611     03  W-WDA612KY-X.                                                    
011620         05  W-WDA612-KDSEGKEY   PIC X(1)  VALUE SPACE.                   
011630                                                                          
011640     03  W-WDA613KY-X.                                                    
011650         05  W-WDA613-KDSEGKEY   PIC X(1)  VALUE SPACE.                   
011660                                                                          
011700     SKIP2                                                                
011701* THE TWO SEARCHPATHS USE THE SAME LENGHT ON KEYS                         
011710   03    W-WDA601KY-MIN-X.                                                
011720     05    W-A601KY-MIN-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
011730     05    W-A601KY-MIN-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
011740     05    W-A601KY-MIN-IDKUNDRF     PIC X(10) VALUE SPACE.               
011750     05    W-A601KY-MIN-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
011760     05    W-A601KY-MIN-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
011770     05    W-A601KY-MIN-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
011780     05    W-A601KY-MIN-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
011790     05    W-A601KY-MIN-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
011791     SKIP2                                                                
011792   03    W-WDA601KY-MAX-X.                                                
011793     05    W-A601KY-MAX-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
011794     05    W-A601KY-MAX-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
011795     05    W-A601KY-MAX-IDKUNDRF     PIC X(10) VALUE SPACE.               
011796     05    W-A601KY-MAX-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
011797     05    W-A601KY-MAX-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
011798     05    W-A601KY-MAX-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
011799     05    W-A601KY-MAX-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
011800     05    W-A601KY-MAX-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
011801                                                                          
011810*    --- STATUS-KOD FRÅN IMS                                              
011900 01  STATUS-WS                   PIC XX.                                  
012000     88  SEGMENT-FOUND                       VALUE '  '.                  
012100     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
012200     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012300     SKIP2                                                                
012400 01  GOOD-STATUSCODES.                                                    
012500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012600     SKIP3                                                                
012700 01  SSA1                        PIC X(128).                              
012800 01  SSA2                        PIC X(128).                              
012900     EJECT                                                                
012910                                                                          
013000*    --- IMS FUNCTION CODES                                               
013100*01  -COPY W0003                                                          
013300     EJECT                                                                
013310                                                                          
013400*    ---  DLI INPUT-OUTPUT AREA                                           
013601 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA6'.                        
013602 01  DLI-IO-WDA6.                                                         
013603*    03  -COPY WDA601                                                     
013604     EJECT                                                                
013605 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA612'.                      
013606 01  DLI-IO-WDA612.                                                       
013607*    03  -COPY WDA612                                                     
013608     EJECT                                                                
013609 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA613'.                      
013610 01  DLI-IO-WDA613.                                                       
013620*    03  -COPY WDA613                                                     
013940     EJECT                                                                
013950                                                                          
014000 LINKAGE SECTION.                                                         
014100*01  -COPY W0009   -PRE MSG-                                              
014110                                                                          
014120*01  -COPY W0009  -PRE 4278-                                              
014130     05  FILLER                  PIC X.                                   
014140                                                                          
014200*01  -COPY W0008   -PRE WDP7-                                             
014300     05  FILLER                  PIC X.                                   
014401                                                                          
014402*01  -COPY W0008  -PRE WDA6A-                                             
014410     05  FILLER                  PIC X.                                   
014411                                                                          
014420*01  -COPY W0008  -PRE WDA6B-                                             
014430     05  FILLER                  PIC X.                                   
014500     EJECT                                                                
014600                                                                          
014601 PROCEDURE DIVISION  USING MSG-PCB 4278-PCB                               
014602                                   WDP7-PCB WDA6A-PCB WDA6B-PCB.          
014603 MAIN SECTION.                                                            
014610     ENTRY 'DLITCBL' USING MSG-PCB 4278-PCB                               
014620                                   WDP7-PCB WDA6A-PCB WDA6B-PCB.          
014700                                                                          
014900     PERFORM IMS-GET-MSG                                                  
015000     IF SEGMENT-FOUND                                                     
015100       PERFORM A-INIT                                                     
015200       PERFORM B-CHECK-KEYS                                               
015300       IF KEYS-OK                                                         
015400         IF MFS-SPLIT                                                     
015500           PERFORM G-CHECK-INDATA                                         
015502         ELSE                                                             
015503           IF MFS-FIRST                                                   
015504             PERFORM C-FIRST-PAGE                                         
015505           ELSE                                                           
015506             IF MFS-NEXT                                                  
015507               PERFORM D-NEXT-PAGE                                        
015508             ELSE                                                         
015509               PERFORM E-SAME-PAGE                                        
015510             END-IF                                                       
015511           END-IF                                                         
015516           PERFORM F-READ-SHOW-INFO                                       
015520         END-IF                                                           
015900       END-IF                                                             
016206       IF MFS-SPLIT  AND KEYS-OK                                          
016210         COMPUTE MSG-KVLL = LENGTH OF 4278-MID-W4I27801-CTX + 17          
016211         PERFORM IMS-PURG-ALTMSG-4278                                     
016212       ELSE                                                               
016213         COMPUTE MSG-KVLL = LENGTH OF MOD-W4O22801 + 4                    
016214         PERFORM IMS-INSERT-MSG                                           
016220       END-IF                                                             
016400     END-IF                                                               
016700     MOVE ZERO TO RETURN-CODE                                             
016800     GOBACK                                                               
016900     .                                                                    
017000     EJECT                                                                
017010                                                                          
017100 A-INIT SECTION.                                                          
017300     IF MSG-DOUBLE-TRANSACTIONS                                           
017400       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W4I22801                 
017500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017700     ELSE                                                                 
017800       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W4I22801                  
017900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
018000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018100     END-IF                                                               
018200                                                                          
018300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018600                                                                          
018700     MOVE LOW-VALUE  TO MSG-AREA                                          
018710     MOVE LOW-VALUE  TO W-WDA601KY-MIN-X.                                 
018720     MOVE HIGH-VALUE TO W-WDA601KY-MAX-X.                                 
018800     MOVE 'W4O228N1' TO MFS-IDMOD                                         
018900     MOVE '4228' TO MOD-IDTRANS                                           
019000     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
019100                                                                          
019200     IF OWN-MID OR HELP-MID                                               
019300       CONTINUE                                                           
019400     ELSE                                                                 
019410       IF SWAP-MID                                                        
019420         MOVE SPACE TO MFS-KDTRTYP                                        
019600         MOVE ' ' TO MFS-IDPFK                                            
019610       ELSE                                                               
019620         MOVE SPACE TO MFS-KDTRTYP                                        
019630         MOVE '7' TO MFS-IDPFK                                            
019640       END-IF                                                             
019700     END-IF                                                               
020000     .                                                                    
020100     EJECT                                                                
020110                                                                          
020200 B-CHECK-KEYS SECTION.                                                    
020400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
020500     MOVE '001'             TO MSGI-KDCALL                                
020600     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020800     MOVE '4228'            TO MSGI-IDTRANS                               
020810                                                                          
020900     IF OWN-MID                                                           
021000       IF MID-IDDISTR-IN NOT = ALL '+'                                    
021010         MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                             
021020       END-IF                                                             
021021       IF MID-IDKUNDNR-IN NOT = ALL '+'                                   
021022         MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                            
021023       END-IF                                                             
021024       IF MID-IDORDNR7-IN NOT = ALL '+'                                   
021030         MOVE MID-IDORDNR7-IN TO MSGI-IDORDNR7                            
021031       END-IF                                                             
021032       IF MID-FLORDLEV-IN NOT = ALL '+'                                   
021040         MOVE MID-FLORDLEV-IN TO MSGI-FLORDLEV                            
021041       END-IF                                                             
021042                                                                          
021050       IF MID-IDDISTR-IN = ALL '+' AND MID-IDKUNDNR-IN = ALL '+'          
021060       AND MID-IDORDNR7-IN = ALL '+' AND MID-FLORDLEV-IN = ALL '+'        
021070         CONTINUE                                                         
021080       ELSE                                                               
021081         MOVE SPACE TO MFS-KDTRTYP                                        
021082         MOVE '7' TO MFS-IDPFK                                            
021090       END-IF                                                             
021100     END-IF                                                               
021110                                                                          
021200     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
021300     MOVE MSGI-SPAR-AREA TO SAVE-AREA                                     
021400                                                                          
021500*    - LANGUAGE TO BE USED BY MEDKONV                                     
021600     MOVE MSGI-IDSPRAK    TO MED-IDSKYLT                                  
021700                                                                          
021800     MOVE YES TO KEYS-SW                                                  
021900                                                                          
022062     MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-IN                               
022063     MOVE MFS-ERASE-FIELD TO MOD-IDKUNDNR-IN                              
022064     MOVE MFS-ERASE-FIELD TO MOD-IDORDNR7-IN                              
022065     MOVE MFS-ERASE-FIELD TO MOD-FLORDLEV-IN                              
022066                                                                          
022070*    -- CHECK OF IDDISTR                                                  
022106     INSPECT MSGI-IDDISTR REPLACING LEADING SPACE BY ZERO                 
022108     IF MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > ZERO                      
022109       MOVE MSGI-IDDISTR TO WS-IDDISTR                                    
022110       MOVE MSGI-IDDISTR TO W-IDDISTR                                     
022111       MOVE MSGI-IDDISTR TO W-A601KY-MIN-IDDISTR                          
022112       MOVE MSGI-IDDISTR TO W-A601KY-MAX-IDDISTR                          
022113     ELSE                                                                 
022118       MOVE NOO TO KEYS-SW                                                
022119     END-IF                                                               
022147                                                                          
022148*    -- CHECK OF IDKUNDNR                                                 
022151     INSPECT MSGI-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
022153     IF MSGI-IDKUNDNR NUMERIC                                             
022154       MOVE MSGI-IDKUNDNR TO W-IDKUNDNR                                   
022155       MOVE MSGI-IDKUNDNR TO W-A601KY-MIN-IDKUNDNR                        
022156       MOVE MSGI-IDKUNDNR TO W-A601KY-MAX-IDKUNDNR                        
022157     ELSE                                                                 
022163       MOVE NOO TO KEYS-SW                                                
022164     END-IF                                                               
022192                                                                          
022300     IF KEYS-WRONG                                                        
022420       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
022500       CALL WMEDKONV USING MED-WMEDAREA                                   
022501       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
022610       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
022900     END-IF                                                               
022910                                                                          
022920     IF KEYS-OK                                                           
022951*    -- CHECK OF IDORDNR7                                                 
022952       INSPECT MSGI-IDORDNR7 REPLACING LEADING SPACE BY ZERO              
022953       IF MSGI-IDORDNR7 NUMERIC                                           
022954         IF MSGI-IDORDNR7 > ZERO                                          
022955           MOVE MSGI-IDORDNR7 TO WS-IDORDNR7                              
022956                                 MOD-IDORDNR7-UT                          
022957           MOVE SPACE         TO WS-FILL-IDORDNR7                         
022958           MOVE WS-IDKUNDRF   TO W-A601KY-MIN-IDKUNDRF                    
022959           MOVE WS-IDKUNDRF   TO W-A601KY-MAX-IDKUNDRF                    
022960           MOVE YES TO SEARCH-IDORDER-SW                                  
022961         ELSE                                                             
022962           MOVE NOO TO SEARCH-IDORDER-SW                                  
022963         END-IF                                                           
022964       ELSE                                                               
022965         MOVE NOO TO KEYS-SW                                              
022966         MOVE ZERO TO MSGI-IDORDNR7                                       
022967       END-IF                                                             
022968                                                                          
022969*    -- CHECK OF FLORDLEV                                                 
022970       IF MSGI-FLORDLEV = 'J' OR MSGI-FLORDLEV = 'Y'                      
022971         MOVE YES TO SEARCH-LEVORDER-SW                                   
022972         MOVE 'Y' TO MOD-FLORDLEV-UT                                      
022973         MOVE 'Y' TO SAVE-FLORDLEV-ENTER                                  
022974       ELSE                                                               
022975         IF MSGI-FLORDLEV = 'N' OR MSGI-FLORDLEV = ' '                    
022976           MOVE NOO TO SEARCH-LEVORDER-SW                                 
022977           MOVE ' ' TO MOD-FLORDLEV-UT                                    
022978           MOVE 'N' TO SAVE-FLORDLEV-ENTER                                
022979         ELSE                                                             
022980           MOVE 'ONLY Y/N IS VALID TO CHOOSE IN LEVORDER'                 
022981             TO MOD-TEMFSFEL                                              
022982           MOVE NOO TO KEYS-SW                                            
022983         END-IF                                                           
022984       END-IF                                                             
022985                                                                          
022986*    -- CHECK OF KDBEHX-EDIT AND TABLE-CONTROLL                           
022987       IF SWAP-MID                                                        
022988         CONTINUE                                                         
022989       ELSE                                                               
022990         MOVE ZERO TO COUNTER1                                            
022991         MOVE ZERO TO COUNTER2                                            
022992         MOVE ZERO TO COUNTER3                                            
022993         MOVE +1 TO INDX                                                  
022994         IF MID-KDBEHX-EDIT(INDX) = 'T'                                   
022995           IF MFS-SPLIT                                                   
022996             MOVE 'T' TO WS-KDBEHX-EDIT                                   
022997             MOVE INDX TO COUNTER1                                        
022998             ADD +1 TO COUNTER3                                           
022999           ELSE                                                           
023000             MOVE 'DONT FORGET TO PRESS PF9'                              
023001               TO MOD-TEMFSFEL                                            
023002             PERFORM MFS-DONT-TOUCH-FIELD-OUT                             
023003             PERFORM MFS-DONT-TOUCH-FIELD-IN                              
023004             ADD +20 TO INDX                                              
023005           END-IF                                                         
023006         ELSE                                                             
023007           IF MID-KDBEHX-EDIT(INDX) = SPACE OR                            
023008              MID-KDBEHX-EDIT(INDX) = ALL '+'                             
023009             CONTINUE                                                     
023010           ELSE                                                           
023011             MOVE 'ONLY T IS VALID TO CHOOSE IN CMD'                      
023012               TO MOD-TEMFSFEL                                            
023013             MOVE NOO TO KEYS-SW                                          
023014             PERFORM MFS-DONT-TOUCH-FIELD-OUT                             
023015             PERFORM MFS-DONT-TOUCH-FIELD-IN                              
023016             ADD +20 TO INDX                                              
023017           END-IF                                                         
023018         END-IF                                                           
023019         PERFORM UNTIL INDX > MAX-INDX                                    
023020           ADD +1 TO INDX                                                 
023021           IF MID-KDBEHX-EDIT(INDX) = 'T'                                 
023022             IF MFS-SPLIT AND COUNTER3 = ZERO                             
023023               MOVE 'T' TO WS-KDBEHX-EDIT                                 
023024               MOVE INDX TO COUNTER1                                      
023025             ELSE                                                         
023026               IF MFS-SPLIT AND COUNTER3 > ZERO                           
023027                 MOVE 'ONLY ONE T IN CMD IS VALID'                        
023028                   TO MOD-TEMFSFEL                                        
023029                 MOVE NOO TO KEYS-SW                                      
023030                 PERFORM MFS-DONT-TOUCH-FIELD-OUT                         
023031                 PERFORM MFS-DONT-TOUCH-FIELD-IN                          
023032                 ADD +20 TO INDX                                          
023033               ELSE                                                       
023034                 MOVE 'DONT FORGET TO PRESS PF9'                          
023035                   TO MOD-TEMFSFEL                                        
023036                 MOVE NOO TO KEYS-SW                                      
023037                 PERFORM MFS-DONT-TOUCH-FIELD-OUT                         
023038                 PERFORM MFS-DONT-TOUCH-FIELD-IN                          
023039                 ADD +20 TO INDX                                          
023040               END-IF                                                     
023041             END-IF                                                       
023042           ELSE                                                           
023043             IF MID-KDBEHX-EDIT(INDX) = SPACE OR                          
023044                MID-KDBEHX-EDIT(INDX) = ALL '+'                           
023045               CONTINUE                                                   
023046             ELSE                                                         
023047               MOVE 'ONLY T IS VALID TO CHOOSE IN CMD'                    
023048                 TO MOD-TEMFSFEL                                          
023049               MOVE NOO TO KEYS-SW                                        
023050               PERFORM MFS-DONT-TOUCH-FIELD-OUT                           
023051               PERFORM MFS-DONT-TOUCH-FIELD-IN                            
023052               ADD +20 TO INDX                                            
023053             END-IF                                                       
023054           END-IF                                                         
023055         END-PERFORM                                                      
023056       END-IF                                                             
023057     END-IF                                                               
023058                                                                          
023059     IF KEYS-OK                                                           
023060       PERFORM BB-CHECK-SECURIT                                           
023061       IF KEYS-WRONG                                                      
023062         PERFORM MFS-DONT-TOUCH-FIELD-IN                                  
023063       END-IF                                                             
023065                                                                          
023080       MOVE MSGI-IDDISTR   TO MOD-IDDISTR-UT                              
023091       MOVE MSGI-IDKUNDNR  TO MOD-IDKUNDNR-UT                             
023092     END-IF                                                               
023093     IF MSGI-IDORDNR7 = ZERO                                              
023094       MOVE ZERO             TO MOD-IDORDNR7-UT                           
023095     ELSE                                                                 
023096       MOVE MSGI-IDORDNR7    TO MOD-IDORDNR7-UT                           
023097     END-IF                                                               
023098     IF MSGI-FLORDLEV = 'N' OR MSGI-FLORDLEV = ' '                        
023099       MOVE SPACE TO MOD-FLORDLEV-UT                                      
023100     ELSE                                                                 
023101       MOVE MSGI-FLORDLEV  TO MOD-FLORDLEV-UT                             
023102     END-IF                                                               
023103                                                                          
023104     IF KEYS-WRONG                                                        
023105       MOVE ZERO              TO SAVE-IDORDNR7-MID                        
023106       MOVE ZERO              TO SAVE-IDORDNR7-JUMP                       
023107       MOVE SPACE             TO SAVE-FLORDLEV-ENTER                      
023108                                                                          
023109       MOVE ZERO              TO SAVE-IDDISTR-ENTER                       
023110       MOVE ZERO              TO SAVE-IDKUNDNR-ENTER                      
023111       MOVE ZERO              TO SAVE-IDORDNR7-ENTER                      
023112       MOVE ZERO              TO SAVE-IDORDNR-ENTER                       
023113       MOVE ZERO              TO SAVE-TIREGDAT-ENTER                      
023114       MOVE ZERO              TO SAVE-TILEVDAT-ENTER                      
023115       MOVE ZERO              TO SAVE-IDARTNR-ENTER                       
023116       MOVE ZERO              TO SAVE-TIREGTID-ENTER                      
023117       MOVE ZERO              TO SAVE-TILEVTID-ENTER                      
023118       MOVE ZERO              TO SAVE-TIREGDAT-AVV-ENTER                  
023119       MOVE ZERO              TO SAVE-TIREGTID-AVV-ENTER                  
023120                                                                          
023121       MOVE ZERO              TO SAVE-IDDISTR-NEXT                        
023122       MOVE ZERO              TO SAVE-IDKUNDNR-NEXT                       
023123       MOVE ZERO              TO SAVE-IDORDNR7-NEXT                       
023124       MOVE ZERO              TO SAVE-IDORDNR-NEXT                        
023125       MOVE ZERO              TO SAVE-TIREGDAT-NEXT                       
023126       MOVE ZERO              TO SAVE-TILEVDAT-NEXT                       
023127       MOVE ZERO              TO SAVE-IDARTNR-NEXT                        
023128       MOVE ZERO              TO SAVE-TIREGTID-NEXT                       
023129       MOVE ZERO              TO SAVE-TILEVTID-NEXT                       
023130       MOVE ZERO              TO SAVE-TIREGDAT-AVV-NEXT                   
023131       MOVE ZERO              TO SAVE-TIREGTID-AVV-NEXT                   
023132                                                                          
023133       MOVE '002'      TO MSGI-KDCALL                                     
023134       MOVE '4228'     TO SAVE-IDTRANS                                    
023135       MOVE SAVE-AREA  TO MSGI-SPAR-AREA                                  
023136       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
023141     END-IF                                                               
023142     .                                                                    
023143     EJECT                                                                
023150                                                                          
023301 BB-CHECK-SECURIT SECTION.                                                
023302     MOVE MSG-SIGNON-USERID TO    SEC-IDUSER                              
023303     MOVE '4228'            TO    SEC-IDTRANS                             
023304     MOVE WS-IDDISTR        TO    SEC-IDKEY                               
023306     CALL WSECURIT          USING SEC-IDUSER                              
023307                                  SEC-IDTRANS                             
023308                                  SEC-IDKEY                               
023309                                  SEC-KDSVAR                              
023310     IF SEC-KDSVAR = OBEHORIG                                             
023311       MOVE NOO             TO KEYS-SW                                    
023312       MOVE ERR-OBEHORIG    TO MED-IDMFSFEL                               
023313       CALL WMEDKONV        USING MED-WMEDAREA                            
023314       MOVE MED-MFSFEL      TO MOD-TEMFSFEL                               
023315     END-IF                                                               
023316     .                                                                    
023317     EJECT                                                                
023318                                                                          
023319 C-FIRST-PAGE SECTION.                                                    
023320     MOVE INF-FIRST-PAGE    TO MED-IDMFSINF                               
023321     CALL WMEDKONV          USING MED-WMEDAREA                            
023322     MOVE MED-MFSINF        TO MOD-TEMFSFEL                               
023323                                                                          
023324     PERFORM MFS-ERASE-FIELD-IN                                           
023325     .                                                                    
023326     EJECT                                                                
023327                                                                          
023328 D-NEXT-PAGE SECTION.                                                     
023329     IF SAVE-IDTRANS = '4228'                                             
023330       MOVE SAVE-IDDISTR-NEXT      TO W-A601KY-MIN-IDDISTR                
023331       MOVE SAVE-IDDISTR-NEXT      TO W-A601KY-MAX-IDDISTR                
023332       MOVE SAVE-IDKUNDNR-NEXT     TO W-A601KY-MIN-IDKUNDNR               
023333       MOVE SAVE-IDKUNDNR-NEXT     TO W-A601KY-MAX-IDKUNDNR               
023334       MOVE SAVE-IDORDNR7-NEXT     TO WS-IDORDNR7                         
023335       MOVE SPACE                  TO WS-FILL-IDORDNR7                    
023336       MOVE WS-IDKUNDRF            TO W-A601KY-MIN-IDKUNDRF               
023337       MOVE SAVE-IDORDNR7-NEXT     TO W-IDORDNR7-MIN                      
023338       MOVE SAVE-IDORDNR-NEXT      TO W-IDORDNR-MIN                       
023339       MOVE SAVE-TIREGDAT-NEXT     TO W-A601KY-MIN-TIREGDAT               
023340       MOVE SAVE-IDARTNR-NEXT      TO W-A601KY-MIN-IDARTNR                
023341       MOVE SAVE-TIREGTID-NEXT     TO W-A601KY-MIN-TIREGTID               
023342       MOVE SAVE-TIREGDAT-AVV-NEXT TO W-A601KY-MIN-TIREGDAT-AVV           
023343       MOVE SAVE-TIREGTID-AVV-NEXT TO W-A601KY-MIN-TIREGTID-AVV           
023344       IF MSGI-FLORDLEV = 'Y'                                             
023345         MOVE SAVE-IDORDNR-NEXT    TO SAVE-IDORDNR7-NEXT                  
023346         MOVE SAVE-IDORDNR7-NEXT   TO WS-IDORDNR7                         
023347         MOVE SPACE                TO WS-FILL-IDORDNR7                    
023348         MOVE WS-IDKUNDRF          TO W-A601KY-MIN-IDKUNDRF               
023349         MOVE SAVE-TILEVDAT-NEXT   TO W-A601KY-MIN-TIREGDAT               
023350         MOVE SAVE-TILEVTID-NEXT   TO W-A601KY-MIN-TIREGTID               
023351       END-IF                                                             
023352     ELSE                                                                 
023353       PERFORM MFS-ERASE-FIELD-IN                                         
023354     END-IF                                                               
023355     .                                                                    
023356     EJECT                                                                
023357                                                                          
023358 E-SAME-PAGE SECTION.                                                     
023359     IF SWAP-MID                                                          
023362       MOVE MSGI-IDDISTR            TO W-A601KY-MIN-IDDISTR               
023363       MOVE MSGI-IDDISTR            TO W-A601KY-MAX-IDDISTR               
023366       MOVE MSGI-IDKUNDNR           TO W-A601KY-MIN-IDKUNDNR              
023367       MOVE MSGI-IDKUNDNR           TO W-A601KY-MAX-IDKUNDNR              
023368       MOVE SAVE-IDORDNR7-MID       TO WS-IDORDNR7                        
023369       MOVE SPACE                   TO WS-FILL-IDORDNR7                   
023370       MOVE WS-IDKUNDRF             TO W-A601KY-MIN-IDKUNDRF              
023371       MOVE SAVE-IDORDNR7-ENTER     TO W-IDORDNR7-MIN                     
023372       MOVE SAVE-IDORDNR-ENTER      TO W-IDORDNR-MIN                      
023373       MOVE SAVE-TIREGDAT-ENTER     TO W-A601KY-MIN-TIREGDAT              
023374       MOVE SAVE-IDARTNR-ENTER      TO W-A601KY-MIN-IDARTNR               
023375       MOVE SAVE-TIREGTID-ENTER     TO W-A601KY-MIN-TIREGTID              
023376       MOVE SAVE-TIREGDAT-AVV-ENTER TO W-A601KY-MIN-TIREGDAT-AVV          
023377       MOVE SAVE-TIREGTID-AVV-ENTER TO W-A601KY-MIN-TIREGTID-AVV          
023378       IF MSGI-FLORDLEV = 'Y'                                             
023379         MOVE SAVE-IDORDNR-ENTER    TO SAVE-IDORDNR7-MID                  
023380         MOVE SAVE-IDORDNR7-MID     TO WS-IDORDNR7                        
023381         MOVE SPACE                 TO WS-FILL-IDORDNR7                   
023382         MOVE WS-IDKUNDRF           TO W-A601KY-MIN-IDKUNDRF              
023383         MOVE SAVE-TILEVDAT-ENTER   TO W-A601KY-MIN-TIREGDAT              
023384         MOVE SAVE-TILEVTID-ENTER   TO W-A601KY-MIN-TIREGTID              
023385       END-IF                                                             
023386     ELSE                                                                 
023387       IF SAVE-IDTRANS = '4228' OR '0551'                                 
023388         MOVE SAVE-IDDISTR-ENTER      TO W-A601KY-MIN-IDDISTR             
023389         MOVE SAVE-IDDISTR-ENTER      TO W-A601KY-MAX-IDDISTR             
023390         MOVE SAVE-IDKUNDNR-ENTER     TO W-A601KY-MIN-IDKUNDNR            
023391         MOVE SAVE-IDKUNDNR-ENTER     TO W-A601KY-MAX-IDKUNDNR            
023392         MOVE SAVE-IDORDNR7-ENTER     TO WS-IDORDNR7                      
023393         MOVE SPACE                   TO WS-FILL-IDORDNR7                 
023394         MOVE WS-IDKUNDRF             TO W-A601KY-MIN-IDKUNDRF            
023395         MOVE SAVE-IDORDNR7-ENTER     TO W-IDORDNR7-MIN                   
023396         MOVE SAVE-IDORDNR-ENTER      TO W-IDORDNR-MIN                    
023397         MOVE SAVE-TIREGDAT-ENTER     TO W-A601KY-MIN-TIREGDAT            
023398         MOVE SAVE-IDARTNR-ENTER      TO W-A601KY-MIN-IDARTNR             
023399         MOVE SAVE-TIREGTID-ENTER     TO W-A601KY-MIN-TIREGTID            
023400         MOVE SAVE-TIREGDAT-AVV-ENTER TO W-A601KY-MIN-TIREGDAT-AVV        
023401         MOVE SAVE-TIREGTID-AVV-ENTER TO W-A601KY-MIN-TIREGTID-AVV        
023402         IF MSGI-FLORDLEV = 'Y'                                           
023403           MOVE SAVE-IDORDNR-ENTER    TO SAVE-IDORDNR7-ENTER              
023404           MOVE SAVE-IDORDNR7-ENTER   TO WS-IDORDNR7                      
023405           MOVE SPACE                 TO WS-FILL-IDORDNR7                 
023406           MOVE WS-IDKUNDRF           TO W-A601KY-MIN-IDKUNDRF            
023407           MOVE SAVE-TILEVDAT-ENTER   TO W-A601KY-MIN-TIREGDAT            
023408           MOVE SAVE-TILEVTID-ENTER   TO W-A601KY-MIN-TIREGTID            
023409         END-IF                                                           
023410         IF WS-KDBEHX-EDIT = SPACE                                        
023411           PERFORM MFS-DONT-TOUCH-FIELD-OUT                               
023412           PERFORM MFS-DONT-TOUCH-FIELD-IN                                
023413           MOVE 'T IS THE RIGHT CMD AND PRESS PF9' TO MOD-TEMFSFEL        
023414           IF WS-KDBEHX-EDIT = 'T'                                        
023415             PERFORM MFS-DONT-TOUCH-FIELD-OUT                             
023416             PERFORM MFS-DONT-TOUCH-FIELD-IN                              
023417             MOVE 'DONT FORGET TO PRESS PF9'                              
023418             TO MOD-TEMFSFEL                                              
023419           ELSE                                                           
023420             PERFORM MFS-DONT-TOUCH-FIELD-OUT                             
023421             PERFORM MFS-DONT-TOUCH-FIELD-IN                              
023422             MOVE 'T IS THE RIGHT CMD AND PRESS PF9'                      
023423             TO MOD-TEMFSFEL                                              
023424           END-IF                                                         
023425         END-IF                                                           
023426       ELSE                                                               
023427         MOVE INF-FIRST-PAGE TO MED-IDMFSINF                              
023428         CALL WMEDKONV     USING MED-WMEDAREA                             
023429         MOVE MED-MFSINF   TO MOD-TEMFSFEL                                
023430         PERFORM MFS-ERASE-FIELD-IN                                       
023431       END-IF                                                             
023432     END-IF                                                               
023433     .                                                                    
023434     EJECT                                                                
023435                                                                          
023440 F-READ-SHOW-INFO SECTION.                                                
023500     MOVE +1 TO INDX                                                      
023594                                                                          
023600     PERFORM FA-READ-BASICDATA                                            
023700                                                                          
023800     IF SEGMENT-MISSING                                                   
024200       MOVE 'WRONG KEYS' TO MOD-TEMFSFEL                                  
024210       MOVE ZERO              TO SAVE-IDORDNR7-MID                        
024220       MOVE ZERO              TO SAVE-IDORDNR7-JUMP                       
024230       MOVE SPACE             TO SAVE-FLORDLEV-ENTER                      
024240                                                                          
024250       MOVE ZERO              TO SAVE-IDDISTR-ENTER                       
024260       MOVE ZERO              TO SAVE-IDKUNDNR-ENTER                      
024270       MOVE ZERO              TO SAVE-IDORDNR7-ENTER                      
024280       MOVE ZERO              TO SAVE-IDORDNR-ENTER                       
024290       MOVE ZERO              TO SAVE-TIREGDAT-ENTER                      
024291       MOVE ZERO              TO SAVE-TILEVDAT-ENTER                      
024292       MOVE ZERO              TO SAVE-IDARTNR-ENTER                       
024293       MOVE ZERO              TO SAVE-TIREGTID-ENTER                      
024294       MOVE ZERO              TO SAVE-TILEVTID-ENTER                      
024295       MOVE ZERO              TO SAVE-TIREGDAT-AVV-ENTER                  
024296       MOVE ZERO              TO SAVE-TIREGTID-AVV-ENTER                  
024297                                                                          
024298       MOVE ZERO              TO SAVE-IDDISTR-NEXT                        
024299       MOVE ZERO              TO SAVE-IDKUNDNR-NEXT                       
024300       MOVE ZERO              TO SAVE-IDORDNR7-NEXT                       
024310       MOVE ZERO              TO SAVE-IDORDNR-NEXT                        
024320       MOVE ZERO              TO SAVE-TIREGDAT-NEXT                       
024330       MOVE ZERO              TO SAVE-TILEVDAT-NEXT                       
024340       MOVE ZERO              TO SAVE-IDARTNR-NEXT                        
024350       MOVE ZERO              TO SAVE-TIREGTID-NEXT                       
024360       MOVE ZERO              TO SAVE-TILEVTID-NEXT                       
024370       MOVE ZERO              TO SAVE-TIREGDAT-AVV-NEXT                   
024380       MOVE ZERO              TO SAVE-TIREGTID-AVV-NEXT                   
024390                                                                          
024391       MOVE '002'      TO MSGI-KDCALL                                     
024392       MOVE '4228'     TO SAVE-IDTRANS                                    
024393       MOVE SAVE-AREA  TO MSGI-SPAR-AREA                                  
024394       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
024400     ELSE                                                                 
024403       MOVE W-IDORDNR7-MIN  TO W-IDORDNR7                                 
024404       MOVE W-IDORDNR-MIN   TO W-IDORDNR                                  
024405                                                                          
024408       IF SEGMENT-FOUND                                                   
024409         MOVE VOR-IDDISTR       TO SAVE-IDDISTR-ENTER                     
024410         MOVE VOR-IDKUNDNR      TO SAVE-IDKUNDNR-ENTER                    
024411         MOVE VOR-IDORDNR7      TO SAVE-IDORDNR7-ENTER                    
024412         MOVE VOR-IDORDNR7-LEV  TO SAVE-IDORDNR-ENTER                     
024413         MOVE VOR-TIREGDAT-URSP TO SAVE-TIREGDAT-ENTER                    
024414         MOVE VOR-TIREGDAT-LEV  TO SAVE-TILEVDAT-ENTER                    
024415         MOVE VOR-IDARTNR       TO SAVE-IDARTNR-ENTER                     
024416         MOVE VOR-TIREGTID-URSP TO SAVE-TIREGTID-ENTER                    
024417         MOVE VOR-TIREGTID-LEV  TO SAVE-TILEVTID-ENTER                    
024418         MOVE VOR-TIREGDAT-AVV  TO SAVE-TIREGDAT-AVV-ENTER                
024419         MOVE VOR-TIREGTID-AVV  TO SAVE-TIREGTID-AVV-ENTER                
024420       ELSE                                                               
024421         MOVE W-IDORDNR7-MIN TO SAVE-IDORDNR7-ENTER                       
024422         MOVE W-IDORDNR-MIN  TO SAVE-IDORDNR-ENTER                        
024423       END-IF                                                             
024424                                                                          
024425       PERFORM UNTIL INDX > MAX-INDX                                      
024426         IF SEGMENT-FOUND                                                 
024427           PERFORM FB-READ-LINEDATA                                       
024428           ADD 1 TO INDX                                                  
024430           PERFORM FA-READ-NEXT                                           
024432         ELSE                                                             
024433           PERFORM FB-ERASE-LINEDATA                                      
024434           ADD 1 TO INDX                                                  
024435         END-IF                                                           
024437       END-PERFORM                                                        
024438                                                                          
024439       IF SEGMENT-FOUND                                                   
024441         MOVE VOR-IDDISTR       TO SAVE-IDDISTR-NEXT                      
024443         MOVE VOR-IDKUNDNR      TO SAVE-IDKUNDNR-NEXT                     
024446         MOVE VOR-IDORDNR7      TO SAVE-IDORDNR7-NEXT                     
024449         MOVE VOR-IDORDNR7-LEV  TO SAVE-IDORDNR-NEXT                      
024450         MOVE VOR-TIREGDAT-URSP TO SAVE-TIREGDAT-NEXT                     
024451         MOVE VOR-TIREGDAT-LEV  TO SAVE-TILEVDAT-NEXT                     
024452         MOVE VOR-IDARTNR       TO SAVE-IDARTNR-NEXT                      
024453         MOVE VOR-TIREGTID-URSP TO SAVE-TIREGTID-NEXT                     
024454         MOVE VOR-TIREGTID-LEV  TO SAVE-TILEVTID-NEXT                     
024455         MOVE VOR-TIREGDAT-AVV  TO SAVE-TIREGDAT-AVV-NEXT                 
024456         MOVE VOR-TIREGTID-AVV  TO SAVE-TIREGTID-AVV-NEXT                 
024464         MOVE 'MORE INFO EXIST, PRESS PF8 TO SEE' TO MOD-TEMFSINF         
024465       ELSE                                                               
024466         PERFORM FB-ERASE-LINEDATA                                        
024467         MOVE VOR-IDDISTR             TO SAVE-IDDISTR-NEXT                
024468         MOVE VOR-IDKUNDNR            TO SAVE-IDKUNDNR-NEXT               
024469         MOVE SAVE-IDORDNR7-ENTER     TO SAVE-IDORDNR7-NEXT               
024470         MOVE SAVE-IDORDNR-ENTER      TO SAVE-IDORDNR-NEXT                
024471         MOVE SAVE-TIREGDAT-ENTER     TO SAVE-TIREGDAT-NEXT               
024472         MOVE SAVE-TILEVDAT-ENTER     TO SAVE-TILEVDAT-NEXT               
024473         MOVE SAVE-IDARTNR-ENTER      TO SAVE-IDARTNR-NEXT                
024474         MOVE SAVE-TIREGTID-ENTER     TO SAVE-TIREGTID-NEXT               
024475         MOVE SAVE-TILEVTID-ENTER     TO SAVE-TILEVTID-NEXT               
024476         MOVE SAVE-TIREGDAT-AVV-ENTER TO SAVE-TIREGDAT-AVV-NEXT           
024477         MOVE SAVE-TIREGTID-AVV-ENTER TO SAVE-TIREGTID-AVV-NEXT           
024478         IF NOT MFS-SPLIT  AND MFS-NEXT                                   
024479           MOVE 'LAST PAGE' TO MOD-TEMFSFEL                               
024480         END-IF                                                           
024481       END-IF                                                             
024482                                                                          
024483       IF MID-IDORDNR7-IN = ALL '+' OR MID-IDORDNR7-IN = ZERO             
024484         MOVE SAVE-IDORDNR7-ENTER TO SAVE-IDORDNR7-MID                    
024485         MOVE ZERO                TO SAVE-IDORDNR7-JUMP                   
024486       ELSE                                                               
024487         MOVE MID-IDORDNR7-IN     TO SAVE-IDORDNR7-MID                    
024488         MOVE MID-IDORDNR7-IN     TO SAVE-IDORDNR7-JUMP                   
024489       END-IF                                                             
024490                                                                          
024491       MOVE '002'      TO MSGI-KDCALL                                     
024492       MOVE '4228'     TO SAVE-IDTRANS                                    
024493       MOVE SAVE-AREA  TO MSGI-SPAR-AREA                                  
024494       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
024500     END-IF                                                               
024600     .                                                                    
024700     EJECT                                                                
024710                                                                          
024800 FA-READ-BASICDATA SECTION.                                               
024810     IF SEARCH-LEVORDER                                                   
024811       IF SEARCH-IDORDER                                                  
024820         PERFORM IMS-GET-WDA6B                                            
024830       ELSE                                                               
024831         PERFORM IMS-GET-WDA6B                                            
024832       END-IF                                                             
024840     ELSE                                                                 
024900       IF SEARCH-IDORDER                                                  
025000         PERFORM IMS-GET-WDA6A                                            
025002       ELSE                                                               
025004         PERFORM IMS-GET-WDA6A                                            
025006       END-IF                                                             
025007     END-IF                                                               
025400     .                                                                    
025501     EJECT                                                                
025502                                                                          
025503 FA-READ-NEXT SECTION.                                                    
025504     IF SEARCH-LEVORDER                                                   
025505       IF SEARCH-IDORDER                                                  
025506         PERFORM IMS-GN-WDA6B                                             
025507       ELSE                                                               
025508         PERFORM IMS-GN-WDA6B                                             
025509       END-IF                                                             
025510     ELSE                                                                 
025511       IF SEARCH-IDORDER                                                  
025512         PERFORM IMS-GN-WDA6A                                             
025513       ELSE                                                               
025514         PERFORM IMS-GN-WDA6A                                             
025515       END-IF                                                             
025516     END-IF                                                               
025517     .                                                                    
025518     EJECT                                                                
025519                                                                          
025520 FB-READ-LINEDATA SECTION.                                                
025521     MOVE VOR-IDORDNR7           TO MOD-IDORDNR7-INFO(INDX)               
025522     MOVE VOR-IDARTNR            TO MOD-IDARTNR(INDX)                     
025523     IF VOR-TIREGDAT-AVV = ZERO                                           
025524       MOVE VOR-TIREGDAT-URSP    TO MOD-TIREGDAT(INDX)                    
025525       MOVE VOR-TIREGTID-URSP    TO WS-REGTID                             
025526     ELSE                                                                 
025527       MOVE VOR-TIREGDAT-AVV     TO MOD-TIREGDAT(INDX)                    
025528       MOVE VOR-TIREGTID-AVV     TO WS-REGTID                             
025529     END-IF                                                               
025530     MOVE WS-TIHHMM              TO MOD-TIHHMM(INDX)                      
025531     MOVE VOR-KVBEART-URSP       TO MOD-KVBEART-URSP(INDX)                
025532     MOVE VOR-KVPREAVB           TO MOD-KVBEART-Q(INDX)                   
025533     MOVE VOR-KDORDBEK           TO MOD-KDORDBEK(INDX)                    
025534     MOVE VOR-IDORDNR7-LEV       TO MOD-IDORDNR-LEV(INDX)                 
025535     MOVE VOR-TIKLAR             TO MOD-TIKLAR(INDX)                      
025536     MOVE VOR-TIKLATID           TO WS-TIKLATID                           
025537     MOVE WS-TIKLARTID           TO MOD-TIKLATID(INDX)                    
025538                                                                          
025539     IF SEARCH-LEVORDER                                                   
025540       PERFORM IMS-GNP-WDA612B                                            
025541       IF SEGMENT-FOUND                                                   
025542         MOVE 'T' TO MOD-KDBEHX-INFO(INDX)                                
025543       ELSE                                                               
025544         PERFORM IMS-GNP-WDA613B                                          
025545         IF SEGMENT-FOUND                                                 
025546           MOVE 'T' TO MOD-KDBEHX-INFO(INDX)                              
025547         ELSE                                                             
025548           MOVE SPACE TO MOD-KDBEHX-INFO(INDX)                            
025549         END-IF                                                           
025550       END-IF                                                             
025551     ELSE                                                                 
025552       PERFORM IMS-GNP-WDA612                                             
025553       IF SEGMENT-FOUND                                                   
025554         MOVE 'T' TO MOD-KDBEHX-INFO(INDX)                                
025555       ELSE                                                               
025556         PERFORM IMS-GNP-WDA613                                           
025557         IF SEGMENT-FOUND                                                 
025558           MOVE 'T' TO MOD-KDBEHX-INFO(INDX)                              
025559         ELSE                                                             
025560           MOVE SPACE TO MOD-KDBEHX-INFO(INDX)                            
025561         END-IF                                                           
025562       END-IF                                                             
025563     END-IF                                                               
025570     .                                                                    
025700     EJECT                                                                
025900                                                                          
025904 FB-ERASE-LINEDATA SECTION.                                               
025905     MOVE MFS-ERASE-FIELD TO MOD-IDORDNR7-INFO(INDX)                      
025906                             MOD-IDARTNR(INDX)                            
025907                             MOD-TIREGDAT(INDX)                           
025908                             MOD-TIHHMM(INDX)                             
025909                             MOD-KVBEART-URSP(INDX)                       
025910                             MOD-KVBEART-Q(INDX)                          
025911                             MOD-KDORDBEK(INDX)                           
025912                             MOD-IDORDNR-LEV(INDX)                        
025913                             MOD-TIKLAR(INDX)                             
025914                             MOD-TIKLATID(INDX)                           
025915                             MOD-KDBEHX-INFO(INDX)                        
025916     .                                                                    
025917     EJECT                                                                
025918                                                                          
025919 G-CHECK-INDATA SECTION.                                                  
025920*TAKE THE RIGHT VALUES TO USE ON THE DATABASE                             
025921     MOVE MSGI-IDDISTR            TO W-A601KY-MIN-IDDISTR                 
025922                                     W-A601KY-MAX-IDDISTR                 
025923                                     SAVE-IDDISTR-ENTER                   
025924     MOVE MSGI-IDKUNDNR           TO W-A601KY-MIN-IDKUNDNR                
025925                                     W-A601KY-MAX-IDKUNDNR                
025926                                     SAVE-IDKUNDNR-ENTER                  
025928     MOVE SAVE-IDORDNR7-ENTER     TO SAVE-IDORDNR7-MID                    
025930     MOVE SAVE-IDORDNR7-ENTER     TO WS-IDORDNR7                          
025931     MOVE SPACE                   TO WS-FILL-IDORDNR7                     
025932     MOVE WS-IDKUNDRF             TO W-A601KY-MIN-IDKUNDRF                
025933     MOVE SAVE-TIREGDAT-ENTER     TO W-A601KY-MIN-TIREGDAT                
025934     MOVE SAVE-IDARTNR-ENTER      TO W-A601KY-MIN-IDARTNR                 
025935     MOVE SAVE-TIREGTID-ENTER     TO W-A601KY-MIN-TIREGTID                
025936     MOVE SAVE-TIREGDAT-AVV-ENTER TO W-A601KY-MIN-TIREGDAT-AVV            
025937     MOVE SAVE-TIREGTID-AVV-ENTER TO W-A601KY-MIN-TIREGTID-AVV            
025944     IF MSGI-FLORDLEV = 'Y'                                               
025945       MOVE SAVE-IDORDNR-ENTER    TO SAVE-IDORDNR7-MID                    
025946       MOVE SAVE-IDORDNR7-MID     TO WS-IDORDNR7                          
025947       MOVE SPACE                 TO WS-FILL-IDORDNR7                     
025948       MOVE WS-IDKUNDRF           TO W-A601KY-MIN-IDKUNDRF                
025949       MOVE SAVE-TILEVDAT-ENTER   TO W-A601KY-MIN-TIREGDAT                
025950       MOVE SAVE-TILEVTID-ENTER   TO W-A601KY-MIN-TIREGTID                
025951     END-IF                                                               
025952                                                                          
025953     MOVE +0 TO INDX                                                      
025954     PERFORM UNTIL INDX > MAX-INDX OR COUNTER1 = COUNTER2                 
025955       IF INDX = ZERO                                                     
025956         PERFORM GA-READ-BASICDATA                                        
025957       ELSE                                                               
025958         PERFORM GA-READ-NEXT                                             
025959       END-IF                                                             
025960       IF SEGMENT-FOUND                                                   
025961         ADD 1 TO INDX                                                    
025962         PERFORM GB-READ-LINEDATA                                         
025963       ELSE                                                               
025964         ADD 1 TO INDX                                                    
025965       END-IF                                                             
025966     END-PERFORM                                                          
025969                                                                          
025976     IF COUNTER1 > ZERO AND COUNTER1 = COUNTER2                           
025977       MOVE MSGI-IDDISTR       TO 4278-MID-IDDISTR                        
025978       MOVE MSGI-IDKUNDNR      TO 4278-MID-IDKUNDNR                       
025979       MOVE VOR-IDORDNR7       TO 4278-MID-IDORDNR7                       
025981       IF MID-IDORDNR7-IN = ALL '+' OR MID-IDORDNR7-IN = ZERO             
025982         MOVE ZERO            TO SAVE-IDORDNR7-ENTER                      
025983         MOVE ZERO            TO SAVE-IDORDNR7-JUMP                       
025984       ELSE                                                               
025985         MOVE VOR-IDORDNR7    TO SAVE-IDORDNR7-ENTER                      
025986         MOVE MID-IDORDNR7-IN TO SAVE-IDORDNR7-JUMP                       
025987       END-IF                                                             
025988       MOVE VOR-TIREGDAT-URSP TO 4278-MID-TIREGDAT-URSP                   
025990       MOVE VOR-IDARTNR       TO 4278-MID-IDARTNR                         
025992       MOVE VOR-TIREGTID-URSP TO 4278-MID-TIREGTID-URSP                   
025994       MOVE VOR-TIREGDAT-AVV  TO 4278-MID-TIREGDAT-AVV                    
025996       MOVE VOR-TIREGTID-AVV  TO 4278-MID-TIREGTID-AVV                    
025999                                                                          
026000       MOVE 'W4T278  '            TO MSG-KDTRANS-1                        
026001       MOVE '4228'                TO MSG-IDTRANS-1                        
026002       MOVE '1'                   TO MSG-KDMFSFOR-1                       
026003       MOVE 4278-MID-W4I27801-CTX TO MSG-INDATA-MINUS-1-TRANSKOD          
026004                                                                          
026005       MOVE YES TO KEYS-SW                                                
026006     ELSE                                                                 
026007       MOVE ZERO TO SAVE-IDORDNR7-ENTER                                   
026008       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
026009       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
026010       MOVE 'ADD T TO THE RIGHT ROW' TO MOD-TEMFSFEL                      
026011       MOVE NOO TO KEYS-SW                                                
026012     END-IF                                                               
026013                                                                          
026014     MOVE '002'        TO MSGI-KDCALL                                     
026015     MOVE '4228'       TO SAVE-IDTRANS                                    
026016     MOVE SAVE-AREA    TO MSGI-SPAR-AREA                                  
026017     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
026018     .                                                                    
026019     EJECT                                                                
026020                                                                          
026021 GA-READ-BASICDATA SECTION.                                               
026022     IF SEARCH-LEVORDER                                                   
026023       IF SEARCH-IDORDER                                                  
026024         PERFORM IMS-GET-WDA6B                                            
026025       ELSE                                                               
026026         PERFORM IMS-GET-WDA6B                                            
026027       END-IF                                                             
026028     ELSE                                                                 
026029       IF SEARCH-IDORDER                                                  
026030         PERFORM IMS-GET-WDA6A                                            
026031       ELSE                                                               
026032         PERFORM IMS-GET-WDA6A                                            
026033       END-IF                                                             
026034     END-IF                                                               
026035     .                                                                    
026036     EJECT                                                                
026037                                                                          
026038 GA-READ-NEXT SECTION.                                                    
026039     IF SEARCH-LEVORDER                                                   
026040       IF SEARCH-IDORDER                                                  
026041         PERFORM IMS-GN-WDA6B                                             
026042       ELSE                                                               
026043         PERFORM IMS-GN-WDA6B                                             
026044       END-IF                                                             
026045     ELSE                                                                 
026046       IF SEARCH-IDORDER                                                  
026047         PERFORM IMS-GN-WDA6A                                             
026048       ELSE                                                               
026049         PERFORM IMS-GN-WDA6A                                             
026050       END-IF                                                             
026051     END-IF                                                               
026052     .                                                                    
026053     EJECT                                                                
026054                                                                          
026055 GB-READ-LINEDATA SECTION.                                                
026056     IF SEARCH-LEVORDER                                                   
026057       PERFORM IMS-GNP-WDA612B                                            
026058       IF SEGMENT-FOUND                                                   
026059         MOVE 'T' TO MOD-KDBEHX-INFO(INDX)                                
026060         MOVE INDX TO COUNTER2                                            
026061       ELSE                                                               
026062         PERFORM IMS-GNP-WDA613B                                          
026063         IF SEGMENT-FOUND                                                 
026064           MOVE 'T' TO MOD-KDBEHX-INFO(INDX)                              
026065           MOVE INDX TO COUNTER2                                          
026066         ELSE                                                             
026067           MOVE SPACE TO MOD-KDBEHX-INFO(INDX)                            
026068         END-IF                                                           
026069       END-IF                                                             
026070     ELSE                                                                 
026071       PERFORM IMS-GNP-WDA612                                             
026072       IF SEGMENT-FOUND                                                   
026073         MOVE 'T' TO MOD-KDBEHX-INFO(INDX)                                
026074         MOVE INDX TO COUNTER2                                            
026075       ELSE                                                               
026076         PERFORM IMS-GNP-WDA613                                           
026077         IF SEGMENT-FOUND                                                 
026078           MOVE 'T' TO MOD-KDBEHX-INFO(INDX)                              
026079           MOVE INDX TO COUNTER2                                          
026080         ELSE                                                             
026081           MOVE SPACE TO MOD-KDBEHX-INFO(INDX)                            
026082         END-IF                                                           
026083       END-IF                                                             
026084     END-IF                                                               
026085     .                                                                    
026086     EJECT                                                                
026090                                                                          
026810 MFS-ERASE-FIELD-IN SECTION.                                              
026900*    --- ALLA INDATA-FÄLT                                                 
027000     MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-IN                               
027100                             MOD-IDKUNDNR-IN                              
027110                             MOD-IDORDNR7-IN                              
027120                             MOD-FLORDLEV-IN                              
027200     .                                                                    
027300     EJECT                                                                
027500                                                                          
027600 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
027700*    --- ALLA UTDATA-FÄLT                                                 
027800*    --- INCL SCROLL KEYS AND LINEDATA                                    
028200     MOVE +1 TO INDX                                                      
028300     PERFORM UNTIL INDX > MAX-INDX                                        
028400       PERFORM MFS-DONT-TOUCH-LINE-FIELD-OUT                              
028500       ADD +1 TO INDX                                                     
028600     END-PERFORM                                                          
028700     SKIP2                                                                
028800     .                                                                    
028900                                                                          
029000 MFS-DONT-TOUCH-LINE-FIELD-OUT  SECTION.                                  
029100*    --- OUTDATA FIELD ON SCROLL KEYS                                     
029200     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDBEHX-EDIT(INDX)                 
029300                                    MOD-IDORDNR7-INFO(INDX)               
029310                                    MOD-IDARTNR(INDX)                     
029320                                    MOD-TIREGDAT(INDX)                    
029330                                    MOD-TIHHMM(INDX)                      
029340                                    MOD-KVBEART-URSP(INDX)                
029350                                    MOD-KVBEART-Q(INDX)                   
029360                                    MOD-KDORDBEK(INDX)                    
029370                                    MOD-IDORDNR-LEV(INDX)                 
029380                                    MOD-TIKLAR(INDX)                      
029390                                    MOD-TIKLATID(INDX)                    
029391                                    MOD-KDBEHX-INFO(INDX)                 
029400     .                                                                    
029500     SKIP3                                                                
029600                                                                          
029700 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
029800*    --- ALLA INDATA-FÄLT                                                 
029900     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDDISTR-IN                        
029901                                    MOD-IDDISTR-UT                        
029910                                    MOD-IDKUNDNR-IN                       
029911                                    MOD-IDKUNDNR-UT                       
029920                                    MOD-IDORDNR7-IN                       
029921                                    MOD-IDORDNR7-UT                       
029930                                    MOD-FLORDLEV-IN                       
029940                                    MOD-FLORDLEV-UT                       
030000     .                                                                    
030100     EJECT                                                                
030200                                                                          
030400* --- IMS SECTIONS ---                                                    
030500     SKIP3                                                                
030710 IMS-GET-MSG SECTION.                                                     
030800     MOVE '  QC' TO GOOD-STATUSCODES                                      
030900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
031000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031100     PERFORM IMS-STATUSCHECK                                              
031200     .                                                                    
031300     SKIP3                                                                
031500                                                                          
031510 IMS-INSERT-MSG SECTION.                                                  
031900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
032000     MOVE SPACE TO GOOD-STATUSCODES                                       
032100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
032200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032300     PERFORM IMS-STATUSCHECK                                              
032400     .                                                                    
032501     EJECT                                                                
032503                                                                          
032580 IMS-PURG-ALTMSG-4278 SECTION.                                            
032600     MOVE    '  '             TO   GOOD-STATUSCODES                       
032700     CALL    CBLTDLI         USING PURG 4278-PCB MSG-IO-AREA              
032800     MOVE    4278-STATUS-CODE TO   STATUS-WS                              
032810     PERFORM IMS-STATUSCHECK                                              
032820     .                                                                    
032821     EJECT                                                                
032822                                                                          
032830 IMS-GET-WDA6A SECTION.                                                   
032831     STRING 'WDA601  (WDA6ASEQ>=' W-WDA601KY-MIN-X                        
032832                    '&WDA6ASEQ<=' W-WDA601KY-MAX-X ')'                    
032833            DELIMITED BY SIZE INTO SSA1                                   
032834     MOVE '  GEGB'               TO GOOD-STATUSCODES                      
032835     CALL  CBLTDLI  USING GHN   WDA6A-PCB DLI-IO-WDA6 SSA1                
032836     MOVE WDA6A-STATUS-CODE TO STATUS-WS                                  
032837     PERFORM IMS-STATUSCHECK                                              
032838     .                                                                    
032839 IMS-GN-WDA6A SECTION.                                                    
032840     STRING 'WDA601  (WDA6ASEQ>=' W-WDA601KY-MIN-X                        
032841                    '&WDA6ASEQ<=' W-WDA601KY-MAX-X ')'                    
032842            DELIMITED BY SIZE INTO SSA1                                   
032843     MOVE '  GEGB'               TO GOOD-STATUSCODES                      
032844     CALL  CBLTDLI  USING GHN   WDA6A-PCB DLI-IO-WDA6 SSA1                
032845     MOVE WDA6A-STATUS-CODE TO STATUS-WS                                  
032846     PERFORM IMS-STATUSCHECK                                              
032847     .                                                                    
032848 IMS-GET-WDA6B SECTION.                                                   
032849     STRING 'WDA601  (WDA6BSEQ>=' W-WDA601KY-MIN-X                        
032850                    '&WDA6BSEQ<=' W-WDA601KY-MAX-X ')'                    
032851            DELIMITED BY SIZE INTO SSA1                                   
032852     MOVE '  GEGB'               TO GOOD-STATUSCODES                      
032853     CALL  CBLTDLI  USING GHN   WDA6B-PCB DLI-IO-WDA6 SSA1                
032854     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
032855     PERFORM IMS-STATUSCHECK                                              
032856     .                                                                    
032857 IMS-GN-WDA6B SECTION.                                                    
032858     STRING 'WDA601  (WDA6BSEQ>=' W-WDA601KY-MIN-X                        
032859                    '&WDA6BSEQ<=' W-WDA601KY-MAX-X ')'                    
032860            DELIMITED BY SIZE INTO SSA1                                   
032861     MOVE '  GEGB'               TO GOOD-STATUSCODES                      
032862     CALL  CBLTDLI  USING GHN   WDA6B-PCB DLI-IO-WDA6 SSA1                
032863     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
032864     PERFORM IMS-STATUSCHECK                                              
032865     .                                                                    
032866 IMS-GNP-WDA612 SECTION.                                                  
032867     MOVE   'WDA612   ' TO SSA1                                           
032868     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
032869     CALL CBLTDLI USING GNP WDA6A-PCB DLI-IO-WDA612 SSA1                  
032870     MOVE WDA6A-STATUS-CODE TO STATUS-WS                                  
032871     PERFORM IMS-STATUSCHECK                                              
032872     .                                                                    
032873     EJECT                                                                
032874                                                                          
032875 IMS-GNP-WDA613 SECTION.                                                  
032876     MOVE   'WDA613   ' TO SSA1                                           
032877     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
032878     CALL CBLTDLI USING GNP WDA6A-PCB DLI-IO-WDA613 SSA1                  
032879     MOVE WDA6A-STATUS-CODE TO STATUS-WS                                  
032880     PERFORM IMS-STATUSCHECK                                              
032881     .                                                                    
032882     EJECT                                                                
032883                                                                          
032884 IMS-GNP-WDA612B SECTION.                                                 
032885     MOVE   'WDA612   ' TO SSA1                                           
032886     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
032887     CALL CBLTDLI USING GNP WDA6B-PCB DLI-IO-WDA612 SSA1                  
032888     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
032889     PERFORM IMS-STATUSCHECK                                              
032890     .                                                                    
032891     EJECT                                                                
032892                                                                          
032893 IMS-GNP-WDA613B SECTION.                                                 
032894     MOVE   'WDA613   ' TO SSA1                                           
032895     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
032896     CALL CBLTDLI USING GNP WDA6B-PCB DLI-IO-WDA613 SSA1                  
032897     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
032898     PERFORM IMS-STATUSCHECK                                              
032899     .                                                                    
032900     EJECT                                                                
032901                                                                          
032902 IMS-STATUSCHECK SECTION.                                                 
032910     SET STATUS-IX TO 1                                                   
033000     SEARCH GOOD-STATUS                                                   
033100       AT END                                                             
033200         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
033300         DELIMITED BY SIZE INTO ERROR-TEXT                                
033400         CALL FELLOG                                                      
033500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
033600         CONTINUE                                                         
033700     END-SEARCH                                                           
033800     .                                                                    
