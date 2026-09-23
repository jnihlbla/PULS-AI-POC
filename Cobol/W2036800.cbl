000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2036800.                                                
000300 AUTHOR.         SATHISH THIRUVENGADAM.                                   
000400 DATE-WRITTEN.   03/OCT/22.                                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*    DC TABLE STEERING.                                                   
000801*                                                                         
000810*    TO BE ABLE TO STEER SAFETY STOCK OF PARTS EFFECTIVELY,               
000820*    CREATED MORE SEGMENTS (BUYERS). THE BUYER SHOULD BE A                
000830*    THREE-DIGIT CODE.                                                    
000831*                                                                         
000840*    IN WHICH EACH BUYER CAN BE STEERED INTO ONE OF TEN EXISTING          
000850*    SAFETY STOCK TABLES, BASED ON PICKS AND FORECAST.                    
000860*    - SEGMENT ON MAJOR CATEGORIES SUCH AS BUSINESS CRITICAL,             
000870*    ALWAYS AIR, ACCESSORIES, ETC (1ST DIGIT, 0-9)                        
000890*    - SEGMENT ON BULKINESS, TO OPTIMIZE AIR VOLUME / STOCK               
000891*    HOLDING COST (2ND DIGIT, 0-1)                                        
000893*    - USE LIFE CYCLE FOR STOCK DEPTH, TO IMPROVE SERVICE FOR             
000894*    YOUNG PARTS AND DECREASE STOCK VALUE FOR OLDER PARTS (3RD            
000895*    DIGIT, 0-2)                                                          
000900*                                                                         
001000*    PROGRAMMET LÄSER      WDB6                                           
001100*    PROGRAMME  UPDATES    WDB6                                           
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W2T368                                              
001500*                     W2T368U                                             
001600*        MID:         W2I36801                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W2O36801                                            
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300                                                                          
002400 DATA DIVISION.                                                           
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002700 77  IDPGM                       PIC X(08)   VALUE 'W2036800'.            
002800                                                                          
002900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003100*                                                                         
003200 77  JA                          PIC X       VALUE 'J'.                   
003300 77  NEJ                         PIC X       VALUE 'N'.                   
003400 77  FL-UPD                      PIC X       VALUE 'N'.                   
003500 77  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
003600 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
003700 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
003800*                                                                         
005200                                                                          
005300**BUYER DESCRIPTION                                                       
005400*    -COPY WBEBUYER                                                       
005401**                                                                        
005402*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005403 01  WORK.                                                                
005500     03 IX-RAD                   PIC S9(3)  COMP-3  VALUE ZERO.           
005600     03 RAD-MAX                  PIC S9(3)  COMP-3  VALUE 12.             
006100                                                                          
006200 01  SWITCHAR.                                                            
006300     03  SW-INPUT-RAETT          PIC X       VALUE 'J'.                   
006900*                                                                         
007000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007100     88  NYCKLAR-OK                          VALUE 'J'.                   
007200     88  NYCKLAR-FEL                         VALUE 'N'.                   
007300                                                                          
008200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008300     88  EGEN-MID                            VALUE '2368'.                
008400     88  GODK-MID                            VALUE '2361' '2362'          
008500                                                   '2363' '2364'          
008600                                                   '2365' '2366'          
008700                                                   '2367' '2368'.         
008900     88  HELP-MID                            VALUE '0551'.                
009000     EJECT                                                                
009100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009200 01  GENERELLA-SUBPROGRAM.                                                
009300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009700     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
009900     EJECT                                                                
010000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
010100*01 -COPY WMEDAREA                                                        
010200     SKIP3                                                                
006910*    --- COPYTEXT TILL SUBPROGRAM WDECEDIT                                
006920*01  -COPY WDECAREA                                                       
006930     EJECT                                                                
010300 01  MESSAGE-CODES.                                                       
010400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010410     03  ERR-WRONG-DC            PIC X(3)    VALUE '440'.                 
010500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010600     03  CONFLICT                PIC X(3)    VALUE '002'.                 
010700     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
010800     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
010900     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
011000     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
011100     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
011200     03  UPDATING-NOT-ALLOWED    PIC X(3)    VALUE '777'.                 
011300     03  ERR-LAST-PAGE-SHOWN     PIC X(3)    VALUE '115'.                 
011400     EJECT                                                                
011500 01  FELTEXTER.                                                           
011600     03  MED-1                  PIC X(40)                                 
011700         VALUE 'ENTER D TO DELETE             '.                          
011701     03  MED-2                  PIC X(40)                                 
011702         VALUE 'BUYER ALREADY EXISTS          '.                          
011703     03  MED-3                  PIC X(40)                                 
011704         VALUE 'ONLY LOCAL BUYERS CAN BE INSERTED  '.                     
011705     03  MED-4                  PIC X(40)                                 
011706         VALUE 'ONLY LOCAL BUYERS CAN BE DELETED   '.                     
011707     03  MED-5                  PIC X(40)                                 
011708         VALUE 'MAX-LF SHOULD BE >= MIN-HF    '.                          
012800                                                                          
012900     EJECT                                                                
013600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
013700*                                                                         
013800 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
013900     SKIP3                                                                
014000*01 -COPY WMSGINIT                                                        
014100     EJECT                                                                
014200*FIELDS FOR USER DATABASE                                                 
014600 01  SPAR-AREA.                                                           
014700     03  SPAR-IDDC                  PIC X(2)    VALUE SPACE.              
014900     03  SPAR-IDPERSON-BUY          PIC S9(3)   VALUE +0 COMP-3.          
015000     03  SPAR-IDPERSON-BUY-FIRST    PIC S9(3)   VALUE +0 COMP-3.          
015100*                                                                         
027901                                                                          
027912 01  W-KVPB-LIM-LF                  PIC 9(06)V9(01).                      
027901                                                                          
027912 01  W-KVPB-LIM-HF                  PIC 9(06)V9(01).                      
027914                                                                          
027915 01  W-COPY-BUYER-TAB.                                                    
027916     03  W-COPY-BUYER-RAD OCCURS 200.                                     
027917       05  W-COPY-IDPERSON-BUY   PIC S9(3)   VALUE +0 COMP-3.             
027918       05  W-COPY-IDREFTAB-LF-UT PIC X(01) VALUE SPACE.                   
027919       05  W-COPY-IDREFTAB-HF-UT PIC X(01) VALUE SPACE.                   
027920*                                                                         
027921     EJECT                                                                
027922 01  FILLER                      PIC X(16)   VALUE 'DAT-AREA'.            
027923     SKIP3                                                                
027924*01 -COPY WDATAREA                                                        
027925     EJECT                                                                
027926*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
027927*                                                                         
027928 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
027929     SKIP3                                                                
027930*01  MID -COPY W2I36801                                                   
027931     EJECT                                                                
027932 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
027933     SKIP3                                                                
027934*01  -COPY WMSGAREA                                                       
027935     EJECT                                                                
027936     03  MOD REDEFINES MSG-AREA.                                          
027937*      05  -COPY W2O36801                                                 
027938     EJECT                                                                
027939 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
027940     SKIP3                                                                
027941*01  -COPY WMFSAREA                                                       
027942     EJECT                                                                
027943*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
027944*                                                                         
027945 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
027946     SKIP3                                                                
027947 01  NYCKLAR-TILL-DLI.                                                    
027948     03  W-IDPERSON-BUY-X.                                                
027949         05  W-IDPERSON-BUY      PIC S9(3) VALUE +0 COMP-3.               
027950     03  W-IDDC-X.                                                        
027951         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
027952     03  W-IDDC-COPY-X.                                                   
027953         05  W-IDDC-COPY         PIC X(2)    VALUE SPACE.                 
027954     03  W-IDTRANS-B6-X.                                                  
027955         05  W-IDTRANS-B6        PIC X(4)    VALUE '2368'.                
027956     SKIP2                                                                
027957*    --- STATUS-KOD FRÅN IMS                                              
027958 01  STATUS-WS                   PIC XX.                                  
027959     88  SEGMENT-FINNS                       VALUE '  '.                  
027960     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
027961     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
027962     88  SEGMENT-SLUT                        VALUE 'GB'.                  
027963     SKIP2                                                                
027964 01  GODK-STATUSKODER.                                                    
027965     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
027966     SKIP3                                                                
027967 01  SSA1                        PIC X(64).                               
027968 01  SSA2                        PIC X(64).                               
027969 01  SSA3                        PIC X(64).                               
027970     EJECT                                                                
027971*    --- IMS FUNKTIONSKODER                                               
027972*01  -COPY W0003                                                          
027973     EJECT                                                                
027974*    ---  DLI INPUT-OUTPUT AREA                                           
027975                                                                          
027976 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
027977 01  DLI-IO-WDB601.                                                       
027978*    03  -COPY WDB601                                                     
027979     EJECT                                                                
027980 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB615'.                      
027981 01  DLI-IO-WDB615.                                                       
027982*    03  -COPY WDB615                                                     
027983     EJECT                                                                
027984 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB619'.                      
027985 01  DLI-IO-WDB619.                                                       
027986*    03  -COPY WDB619                                                     
027987     EJECT                                                                
027988 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB629'.                      
027989 01  DLI-IO-WDB629.                                                       
027990*    03  -COPY WDB629                                                     
027991     EJECT                                                                
027992 LINKAGE SECTION.                                                         
027993*01  -COPY W0009   -PRE MSG-                                              
027994*01  -COPY W0008   -PRE USEA-                                             
027995     05  FILLER                  PIC X.                                   
027996                                                                          
027997*01  -COPY W0008  -PRE WDB6-                                              
027998     05  FILLER                  PIC X.                                   
027999     EJECT                                                                
028000 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDB6-PCB.                     
028001 MAIN SECTION.                                                            
028002     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDB6-PCB.                     
028003                                                                          
028004     PERFORM IMS-GET-MSG                                                  
028005     IF SEGMENT-FINNS                                                     
028006       PERFORM A-INIT                                                     
028007       PERFORM B-KOLLA-NYCKLAR                                            
028008       IF NYCKLAR-OK                                                      
028009           IF MFS-UPDATE                                                  
028010              PERFORM G-KOLLA-INPUT                                       
028011              IF SW-INPUT-RAETT = JA                                      
028012                 PERFORM H-UPPDATERA                                      
028013              END-IF                                                      
028014           ELSE                                                           
028015             IF MFS-FIRST                                                 
028016                PERFORM C-FOERSTA-SIDA                                    
028017             ELSE                                                         
028018                IF MFS-NEXT                                               
028019                   PERFORM D-NEXT-PAGE                                    
028020                ELSE                                                      
028021                   PERFORM E-SAMMA-SIDA                                   
028022                END-IF                                                    
028023             END-IF                                                       
028024           END-IF                                                         
028025           PERFORM F-LAES-VISA-INFO                                       
028026*WRITE INFO TO USER DATABASE,WHICH CAN BE USED FOR F8                     
028027           MOVE SPAR-AREA         TO MSGI-SPAR-AREA                       
028028           MOVE '002'             TO MSGI-KDCALL                          
028029           MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                    
028030           MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                          
028031           MOVE '2368'            TO MSGI-IDTRANS                         
028032           CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                     
028033*                                                                         
028034       END-IF                                                             
028035       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O36801 + 4                      
028036       PERFORM IMS-INSERT-MSG                                             
028037     END-IF                                                               
028038                                                                          
028039     MOVE ZERO TO RETURN-CODE                                             
028040     GOBACK                                                               
028041     .                                                                    
028042     EJECT                                                                
028043 A-INIT SECTION.                                                          
028044     MOVE 'A-INIT     ' TO CURRENT-SECTION                                
028045                                                                          
028046     IF MSG-DUBBLA-TRANSKODER                                             
028047       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I36801                 
028048       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
028049       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
028050     ELSE                                                                 
028051       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I36801                  
028052       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
028053       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
028054     END-IF                                                               
028055                                                                          
028056     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
028057     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
028058     MOVE MFS-IDTRANS TO W-IDTRANS                                        
028059                                                                          
028060     MOVE LOW-VALUE TO MSG-AREA                                           
028061     MOVE 'W2O368N1' TO MFS-IDMOD                                         
028062     MOVE '2368' TO MOD-IDTRANS                                           
028063                                                                          
028070     MOVE MFS-RENSA-FAELT        TO MOD-TEMFSFEL                          
028080                                    MOD-TEMFSINF                          
028090                                    MOD-KVOT-IN                           
028100                                    MOD-KVPB-LIM-HF-IN                    
028101                                    MOD-KVPB-LIM-LF-IN                    
028102                                    MOD-NEW-IDPERSON-BUY                  
028103     IF EGEN-MID OR HELP-MID                                              
028200       CONTINUE                                                           
028300     ELSE                                                                 
028400       MOVE SPACE TO MFS-KDTRTYP                                          
028500       MOVE '7' TO MFS-IDPFK                                              
028600     END-IF                                                               
028700                                                                          
028800                                                                          
028900     ACCEPT TODAYS-DATE FROM DATE                                         
029000                                                                          
029700     .                                                                    
029800     EJECT                                                                
029900 B-KOLLA-NYCKLAR SECTION.                                                 
029910     MOVE 'B-KOLLA    ' TO CURRENT-SECTION                                
030000                                                                          
030100     MOVE ALL '+'                TO MSGI-WMSGINIT                         
030200     MOVE '001'                  TO MSGI-KDCALL                           
030300     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
030400     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
030500     MOVE '2368'                 TO MSGI-IDTRANS                          
031000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
031010     MOVE MSGI-SPAR-AREA  TO SPAR-AREA                                    
031020     IF SPAR-IDPERSON-BUY NOT NUMERIC                                     
031030        MOVE ZERO TO SPAR-IDPERSON-BUY                                    
031040     END-IF                                                               
031050     IF SPAR-IDPERSON-BUY-FIRST NOT NUMERIC                               
031060        MOVE ZERO TO SPAR-IDPERSON-BUY-FIRST                              
031070     END-IF                                                               
031100                                                                          
031200                                                                          
031300*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
031400     MOVE 'GB  '                 TO MED-IDSKYLT                           
031500*                                                                         
031600     MOVE JA TO NYCKLAR-SW                                                
031700*                                                                         
098300     IF MID-IDDC-2368-IN = ALL '+'                                        
098400       IF MID-IDDC-2368-UT = ALL '+'                                      
098500          MOVE SPACE             TO W-IDDC                                
098501       ELSE                                                               
098502          MOVE MID-IDDC-2368-UT  TO W-IDDC                                
098503       END-IF                                                             
098600     ELSE                                                                 
098700       MOVE MID-IDDC-2368-IN     TO W-IDDC                                
099000     END-IF                                                               
099100                                                                          
099200*    -- CHECK OF IDDC                                                     
099300                                                                          
099400     IF W-IDDC NOT = SPACES                                               
099500       PERFORM IMS-GU-WDB601                                              
099600       IF SEGMENT-SAKNAS                                                  
099700         MOVE NEJ            TO NYCKLAR-SW                                
099800         MOVE ERR-WRONG-DC   TO MED-IDMFSFEL                              
099900         CALL WMEDKONV USING MED-WMEDAREA                                 
100000         MOVE MED-MFSFEL     TO MOD-TEMFSFEL                              
100100         PERFORM MFS-RENSA-FAELT-IN                                       
100200         PERFORM MFS-RENSA-FAELT-UT                                       
100300         PERFORM MFS-CLOSE-FIELD-IN                                       
100400       END-IF                                                             
100500     END-IF                                                               
100501     .                                                                    
100502     EJECT                                                                
100503 C-FOERSTA-SIDA SECTION.                                                  
100504     MOVE 'C-FOERSTA  ' TO CURRENT-SECTION                                
100505                                                                          
100506     PERFORM MFS-RENSA-FAELT-IN                                           
100507     .                                                                    
100508     EJECT                                                                
100509 D-NEXT-PAGE SECTION.                                                     
100510     MOVE 'D-NEXT     ' TO CURRENT-SECTION                                
100520                                                                          
100530     IF SPAR-IDDC NOT = SPACE                                             
100540       MOVE SPAR-IDDC         TO W-IDDC                                   
100550       IF SPAR-IDPERSON-BUY > ZERO                                        
100560          MOVE SPAR-IDPERSON-BUY TO W-IDPERSON-BUY                        
100570       ELSE                                                               
100580          MOVE SPAR-IDPERSON-BUY-FIRST                                    
100590                                     TO W-IDPERSON-BUY                    
100600          MOVE ERR-LAST-PAGE-SHOWN   TO MED-IDMFSFEL                      
100700          CALL WMEDKONV USING MED-WMEDAREA                                
100800          MOVE MED-MFSFEL            TO MOD-TEMFSFEL                      
100900       END-IF                                                             
101000     END-IF                                                               
101001     .                                                                    
101002     EJECT                                                                
101003 E-SAMMA-SIDA SECTION.                                                    
101004     MOVE 'E-SAMMA    ' TO CURRENT-SECTION                                
101005                                                                          
101006     IF SPAR-IDDC NOT = SPACE AND                                         
101007        ( MID-IDDC-2368-IN = ALL '+' OR SPACES)                           
101008        MOVE SPAR-IDDC         TO W-IDDC                                  
101009        MOVE SPAR-IDPERSON-BUY-FIRST                                      
101010                               TO W-IDPERSON-BUY                          
101011     END-IF                                                               
101012     .                                                                    
101013     EJECT                                                                
101014                                                                          
101015 F-LAES-VISA-INFO SECTION.                                                
101016     MOVE 'F-LAES     ' TO CURRENT-SECTION                                
101017                                                                          
101018     MOVE W-IDDC              TO MOD-IDDC-UT                              
101019     PERFORM IMS-GU-WDB601                                                
101020                                                                          
101030     IF SEGMENT-FINNS                                                     
101040*DATA FROM WDB601                                                         
101050        MOVE DCS-KVPB-LIM-LF  TO MOD-KVPB-LIM-LF-UT                       
101060        MOVE DCS-KVPB-LIM-HF  TO MOD-KVPB-LIM-HF-UT                       
101070        MOVE DCS-KVOT         TO MOD-KVOT-UT                              
101080*TO SHOW LAST UPDATED TIME,PERSON DETAILS.                                
101090        PERFORM FA-LAST-UPDATED-DETAILS                                   
101100*                                                                         
101200                                                                          
101300        PERFORM IMS-GNP-WDB619-FIRST                                      
101400                                                                          
101500        IF SEGMENT-FINNS                                                  
101600*WILL BE USED TO LOAD THE SAME PAGE AGAIN,WHEN PRESS ENTER AND            
101700*AFTER REACH THE LAST PAGE                                                
101800           MOVE BUYT-IDPERSON-BUY   TO SPAR-IDPERSON-BUY-FIRST            
101900        END-IF                                                            
102000                                                                          
102100        MOVE +1               TO IX-RAD                                   
102200        PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                      
102300          OR IX-RAD > 12                                                  
102400          IF SEGMENT-FINNS                                                
102500*                                                                         
102600             MOVE BUYT-IDPERSON-BUY TO MOD-IDPERSON-BUY (IX-RAD)          
102610                                                                          
102700             INSPECT MOD-IDPERSON-BUY (IX-RAD)                            
102800                              REPLACING LEADING SPACE BY ZERO             
102900             MOVE BUYT-IDREFTAB-LF  TO MOD-IDREFTAB-LF-UT (IX-RAD)        
103000             MOVE BUYT-IDREFTAB-HF  TO MOD-IDREFTAB-HF-UT (IX-RAD)        
103100*                                                                         
103200             IF BUYT-IDPERSON-BUY > 40 AND                                
103210                BUYT-IDPERSON-BUY < 100                                   
103300                PERFORM IMS-GNP-WDB629                                    
103400                IF SEGMENT-FINNS                                          
103500                   MOVE BUYD-BEBUYER   TO MOD-BEBUYER (IX-RAD)            
103600                END-IF                                                    
103700             ELSE                                                         
103800*                                                                         
103900                SEARCH ALL BUYER-TAB-RECORD                               
104000                WHEN BUYER-ID (BUY-IX) = BUYT-IDPERSON-BUY                
104100                   MOVE BUYER-DESCRIPTION (BUY-IX)                        
104200                                        TO MOD-BEBUYER (IX-RAD)           
104300                END-SEARCH                                                
104400*                                                                         
104500             END-IF                                                       
104600          END-IF                                                          
104700          PERFORM IMS-GNP-WDB619                                          
104800          ADD  +1                  TO IX-RAD                              
104900        END-PERFORM                                                       
105000                                                                          
105100        IF SEGMENT-SAKNAS OR SEGMENT-SLUT                                 
105200*                                                                         
105300           MOVE W-IDDC               TO SPAR-IDDC                         
105400           MOVE ZERO                 TO SPAR-IDPERSON-BUY                 
105500*                                                                         
105600           MOVE INF-LAST-PAGE        TO MED-IDMFSINF                      
105700        ELSE                                                              
105800*THESE VALUES WILL BE USED FOR F8 FUNCTION                                
105900           MOVE W-IDDC               TO SPAR-IDDC                         
106000           MOVE BUYT-IDPERSON-BUY    TO SPAR-IDPERSON-BUY                 
106100*                                                                         
106200           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
106300        END-IF                                                            
106301        IF FL-UPD = JA                                                    
106302           MOVE INF-UPDATE-DONE TO MED-IDMFSINF                           
106303        END-IF                                                            
106304        CALL WMEDKONV USING MED-WMEDAREA                                  
106305        MOVE MED-TEMFSINF TO MOD-TEMFSINF                                 
106306                                                                          
106307     END-IF                                                               
106308     .                                                                    
106309     EJECT                                                                
106310 FA-LAST-UPDATED-DETAILS SECTION.                                         
106320        MOVE 'FA-LAST    ' TO CURRENT-SECTION                             
106330                                                                          
106340*TO SHOW LAST UPDATED TIME,PERSON DETAILS.                                
106350        PERFORM IMS-GNP-WDB615                                            
106360        IF SEGMENT-FINNS                                                  
106370          MOVE LOGG-TIUPPDAT  TO MOD-TIUPPDAT                             
106380          MOVE LOGG-IDUSER    TO MOD-IDUSER                               
106390        ELSE                                                              
106400          MOVE ZERO           TO MOD-TIUPPDAT                             
106500          MOVE SPACE          TO MOD-IDUSER                               
106600        END-IF                                                            
106700*                                                                         
106800     .                                                                    
106900     EJECT                                                                
141000 G-KOLLA-INPUT SECTION.                                                   
141010     MOVE 'G-KOLLA-   ' TO CURRENT-SECTION                                
141100     SKIP2                                                                
141300     MOVE JA                      TO SW-INPUT-RAETT                       
141400                                                                          
141500*KVOT VALIDATION                                                          
141600     IF ( NOT (MID-KVOT = ALL '+' ))                                      
141700        INSPECT MID-KVOT REPLACING LEADING SPACE BY ZERO                  
141800        IF MID-KVOT NUMERIC                                               
141900           MOVE MFS-NUM-FAELT-RAETT                                       
142000                                  TO MOD-KVOT-IN-ATTR                     
142100        ELSE                                                              
142200           MOVE MFS-NUM-FAELT-FEL TO MOD-KVOT-IN-ATTR                     
142300           MOVE NEJ               TO SW-INPUT-RAETT                       
142400        END-IF                                                            
142500        MOVE MFS-ROER-EJ-FAELT    TO MOD-KVOT-IN                          
142600     END-IF                                                               
142700*MINIMUM FORECAST HF VALIDATION                                           
142800     IF ( NOT (MID-KVPB-LIM-HF = ALL '+' ))                               
142900        INSPECT MID-KVPB-LIM-HF REPLACING LEADING SPACE BY ZERO           
              MOVE MID-KVPB-LIM-HF          TO DEC-IDFRIDATA                    
              MOVE 6                        TO DEC-KVHELTAL                     
              MOVE 1                        TO DEC-KVDECIMAL                    
              CALL WDECEDIT USING DEC-WDECAREA                                  
              IF DEC-KDSVAR-OK                                                  
                 MOVE DEC-IDEDITDATA    TO W-KVPB-LIM-HF                        
144000        ELSE                                                              
144100           MOVE MFS-NUM-FAELT-FEL TO MOD-KVPB-LIM-HF-IN-ATTR              
144200           MOVE NEJ               TO SW-INPUT-RAETT                       
144300        END-IF                                                            
144400        MOVE MFS-ROER-EJ-FAELT    TO MOD-KVPB-LIM-HF-IN                   
144500     END-IF                                                               
                                                                                
142700*MAXIMUM FORECAST LF VALIDATION                                           
142800     IF ( NOT (MID-KVPB-LIM-LF = ALL '+' ))                               
142900        INSPECT MID-KVPB-LIM-LF REPLACING LEADING SPACE BY ZERO           
              MOVE MID-KVPB-LIM-LF          TO DEC-IDFRIDATA                    
              MOVE 6                        TO DEC-KVHELTAL                     
              MOVE 1                        TO DEC-KVDECIMAL                    
              CALL WDECEDIT USING DEC-WDECAREA                                  
              IF DEC-KDSVAR-OK                                                  
                 MOVE DEC-IDEDITDATA    TO W-KVPB-LIM-LF                        
144000        ELSE                                                              
144100           MOVE MFS-NUM-FAELT-FEL TO MOD-KVPB-LIM-LF-IN-ATTR              
144200           MOVE NEJ               TO SW-INPUT-RAETT                       
144300        END-IF                                                            
144400        MOVE MFS-ROER-EJ-FAELT    TO MOD-KVPB-LIM-LF-IN                   
144500     END-IF                                                               
                                                                                
146500*NEW BUYER VALIDATION                                                     
146600*IDPERSON-BUY VALIDATION                                                  
146700     IF ( NOT (MID-NEW-IDPERSON-BUY  = ALL '+' OR SPACES ))               
146800        INSPECT MID-NEW-IDPERSON-BUY                                      
146900                         REPLACING LEADING SPACE BY ZERO                  
147000        MOVE MID-NEW-IDPERSON-BUY TO W-IDPERSON-BUY                       
147100     END-IF                                                               
147200*                                                                         
147300     IF SW-INPUT-RAETT = NEJ AND MOD-TEMFSFEL = SPACES                    
147400        MOVE ERR-WRONG-KEY  TO MED-IDMFSFEL                               
147500        CALL WMEDKONV USING MED-WMEDAREA                                  
147600        MOVE MED-MFSFEL     TO MOD-TEMFSFEL                               
147601     END-IF                                                               
147602     .                                                                    
156000     EJECT                                                                
156100 H-UPPDATERA SECTION.                                                     
156101     MOVE 'H-UPPDATERA' TO CURRENT-SECTION                                
156102                                                                          
156103     MOVE W-IDDC              TO MOD-IDDC-UT                              
156104                                                                          
156105     IF MID-COPY-IDDC = ALL '+' OR SPACES                                 
156106        PERFORM HA-UPDATE-FREQUENCY                                       
156107        PERFORM HB-UPDATE-TABLE                                           
156108        PERFORM HC-INSERT-LOCAL-BUYERS                                    
156109        PERFORM HD-DELETE-LOCAL-BUYERS                                    
156110*TO STAY IN THE SAME POSITION AFTER UPDATE                                
156120        MOVE MID-IDPERSON-BUY (1)   TO W-IDPERSON-BUY                     
156130*                                                                         
156140     ELSE                                                                 
156150        PERFORM HE-COPY-DC                                                
156160     END-IF                                                               
156170                                                                          
156180     IF FL-UPD = JA                                                       
156190*TO CAPTURE UPDATED DATE, PERSON DETAILS.                                 
156200        PERFORM IMS-GHU-WDB615                                            
156300        IF SEGMENT-FINNS                                                  
156400          MOVE TODAYS-DATE  TO LOGG-TIUPPDAT                              
156500          MOVE MSGI-IDUSER  TO LOGG-IDUSER                                
156600          PERFORM IMS-REPL-WDB615                                         
156700        ELSE                                                              
156800          MOVE '2368'       TO LOGG-IDTRANS                               
156900          MOVE SPACE        TO LOGG-IDDC-REF                              
157000          MOVE TODAYS-DATE  TO LOGG-TIUPPDAT                              
157100          MOVE MSGI-IDUSER  TO LOGG-IDUSER                                
157200          PERFORM IMS-ISRT-WDB615                                         
157300        END-IF                                                            
      *CLEAR THE SCREEN AFTER SUCCESSFUL UPDATE.                                
157501        PERFORM MFS-RENSA-FAELT-IN                                        
      *                                                                         
157400     END-IF                                                               
157500*                                                                         
157502     .                                                                    
157503 HA-UPDATE-FREQUENCY SECTION.                                             
157504     MOVE 'HA-UPDATE  ' TO CURRENT-SECTION                                
157505                                                                          
157506*UPDATE WDB601                                                            
157507     IF ( NOT (MID-KVOT = ALL '+' )) OR                                   
157508        ( NOT (MID-KVPB-LIM-LF = ALL '+' )) OR                            
157509        ( NOT (MID-KVPB-LIM-HF = ALL '+' ))                               
157510                                                                          
157520        PERFORM IMS-GHU-WDB601                                            
157530                                                                          
157540        IF SEGMENT-FINNS                                                  
157550                                                                          
157560           IF ( NOT (MID-KVPB-LIM-LF = ALL '+' ))                         
157570              MOVE W-KVPB-LIM-LF                                          
157580                                 TO DCS-KVPB-LIM-LF                       
157590           END-IF                                                         
157600           IF ( NOT (MID-KVPB-LIM-HF = ALL '+' ))                         
157700              MOVE W-KVPB-LIM-HF                                          
157800                                 TO DCS-KVPB-LIM-HF                       
157900           END-IF                                                         
158000           IF ( NOT (MID-KVOT = ALL '+' ))                                
158100              MOVE MID-KVOT      TO DCS-KVOT                              
158200           END-IF                                                         
158300*                                                                         
158400           IF DCS-KVPB-LIM-LF   >= DCS-KVPB-LIM-HF                        
158401              PERFORM IMS-REPL-WDB601                                     
158402              MOVE JA            TO FL-UPD                                
158403           ELSE                                                           
158404              MOVE MED-5         TO MOD-TEMFSFEL                          
158405           END-IF                                                         
158406*                                                                         
158500        END-IF                                                            
158501     END-IF                                                               
158502     .                                                                    
158503     EJECT                                                                
158504 HB-UPDATE-TABLE SECTION.                                                 
158505     MOVE 'HB-UPDATE  ' TO CURRENT-SECTION                                
158506                                                                          
158507*UPDATE WDB619                                                            
158508*UPDATE TABLE LF,TABLE HF VALUES                                          
158509     MOVE +1                  TO  IX-RAD                                  
158510     PERFORM UNTIL IX-RAD      > RAD-MAX                                  
158520        IF MID-IDREFTAB-LF (IX-RAD) NUMERIC OR                            
158521           MID-IDREFTAB-HF (IX-RAD) NUMERIC                               
158522           MOVE MID-IDPERSON-BUY (IX-RAD)   TO W-IDPERSON-BUY             
158523           PERFORM IMS-GHU-WDB619                                         
158524           IF MID-IDREFTAB-LF (IX-RAD) NUMERIC                            
158525              MOVE MID-IDREFTAB-LF (IX-RAD) TO BUYT-IDREFTAB-LF           
158526           END-IF                                                         
158527           IF MID-IDREFTAB-HF (IX-RAD) NUMERIC                            
158528              MOVE MID-IDREFTAB-HF (IX-RAD) TO BUYT-IDREFTAB-HF           
158529           END-IF                                                         
158530           PERFORM IMS-REPL-WDB619                                        
158540           MOVE JA TO FL-UPD                                              
158541        END-IF                                                            
158542        ADD +1                 TO IX-RAD                                  
158543     END-PERFORM                                                          
158544     .                                                                    
158545     EJECT                                                                
158546 HC-INSERT-LOCAL-BUYERS SECTION.                                          
158547     MOVE 'HC-INSERT  ' TO CURRENT-SECTION                                
158548                                                                          
158549                                                                          
158550*INSERT WDB619,WDB629                                                     
158560     IF MID-NEW-IDPERSON-BUY NUMERIC AND                                  
158561        MID-NEW-IDPERSON-BUY > 40    AND                                  
158562        MID-NEW-IDPERSON-BUY < 100                                        
158580        MOVE MID-NEW-IDPERSON-BUY    TO BUYT-IDPERSON-BUY                 
158590                                                                          
158600        IF MID-NEW-IDREFTAB-LF NUMERIC                                    
158700           MOVE MID-NEW-IDREFTAB-LF  TO BUYT-IDREFTAB-LF                  
158800        ELSE                                                              
158900           MOVE ZERO                 TO BUYT-IDREFTAB-LF                  
159000        END-IF                                                            
159100                                                                          
159200        IF MID-NEW-IDREFTAB-HF NUMERIC                                    
159300           MOVE MID-NEW-IDREFTAB-HF  TO BUYT-IDREFTAB-HF                  
159400        ELSE                                                              
159500           MOVE ZERO                 TO BUYT-IDREFTAB-HF                  
159600        END-IF                                                            
159700                                                                          
159800        PERFORM IMS-ISRT-WDB619                                           
159900        IF SEGMENT-FINNS-REDAN                                            
160000           MOVE MED-2              TO MOD-TEMFSFEL                        
160100        ELSE                                                              
160200           IF (NOT ( MID-NEW-BEBUYER    = ALL '+' OR SPACES))             
160300              MOVE MID-NEW-BEBUYER TO BUYD-BEBUYER                        
160400           ELSE                                                           
160410              MOVE SPACES          TO BUYD-BEBUYER                        
160500           END-IF                                                         
160510           PERFORM IMS-ISRT-WDB629                                        
160600           MOVE JA TO FL-UPD                                              
160700        END-IF                                                            
160800     ELSE                                                                 
160900        IF MID-NEW-IDPERSON-BUY NUMERIC                                   
161000           MOVE MED-3              TO MOD-TEMFSFEL                        
161100        END-IF                                                            
161101     END-IF                                                               
161102     .                                                                    
161103     EJECT                                                                
161104 HD-DELETE-LOCAL-BUYERS SECTION.                                          
161105     MOVE 'HD-DELETE  ' TO CURRENT-SECTION                                
161106                                                                          
161107**DELETE WDB619,WDB629                                                    
161108     MOVE +1                  TO  IX-RAD                                  
161109     PERFORM UNTIL IX-RAD      > RAD-MAX                                  
161110        IF MID-SELECT (IX-RAD) = 'D'                                      
161111           IF MID-IDPERSON-BUY (IX-RAD) > 40 AND                          
161112              MID-IDPERSON-BUY (IX-RAD) < 100                             
161121                                                                          
161122              MOVE MID-IDPERSON-BUY (IX-RAD) TO W-IDPERSON-BUY            
161123              PERFORM IMS-GHU-WDB619                                      
161124              PERFORM IMS-DLET-WDB619                                     
161125              MOVE JA TO FL-UPD                                           
161126           ELSE                                                           
161127              MOVE MED-4                     TO MOD-TEMFSFEL              
161128           END-IF                                                         
161129        ELSE                                                              
161130           IF (NOT (MID-SELECT (IX-RAD) = ALL '+' OR SPACES))             
161140              MOVE MED-1                     TO MOD-TEMFSFEL              
161141           END-IF                                                         
161142        END-IF                                                            
161143        ADD +1                 TO IX-RAD                                  
161144     END-PERFORM                                                          
161145     .                                                                    
161146     EJECT                                                                
161147 HE-COPY-DC SECTION.                                                      
161148     MOVE 'HE-COPY-DC ' TO CURRENT-SECTION                                
161149                                                                          
161150     MOVE MID-COPY-IDDC TO W-IDDC-COPY                                    
161160     PERFORM IMS-GU-WDB601-B                                              
161170                                                                          
161180     IF SEGMENT-FINNS                                                     
161190        MOVE +1               TO IX-RAD                                   
161200        PERFORM IMS-GNP-WDB619                                            
161300        PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                      
161400          OR IX-RAD  > 200                                                
161500          IF SEGMENT-FINNS AND                                            
161600*TO EXCLUDE LOCAL BUYERS                                                  
161700             (                                                            
161800              (BUYT-IDPERSON-BUY > 0  AND BUYT-IDPERSON-BUY < 41 )        
161900              OR                                                          
162000               BUYT-IDPERSON-BUY > 99                                     
162001             )                                                            
162002*                                                                         
162003             MOVE BUYT-IDPERSON-BUY TO W-COPY-IDPERSON-BUY(IX-RAD)        
162004             MOVE BUYT-IDREFTAB-LF  TO                                    
162005                                     W-COPY-IDREFTAB-LF-UT(IX-RAD)        
162006             MOVE BUYT-IDREFTAB-HF  TO                                    
162100                                     W-COPY-IDREFTAB-HF-UT(IX-RAD)        
162400             ADD +1              TO IX-RAD                                
162401          END-IF                                                          
162402          PERFORM IMS-GNP-WDB619                                          
162500        END-PERFORM                                                       
162600     END-IF                                                               
162700                                                                          
164000                                                                          
164001     MOVE +1               TO IX-RAD                                      
164002     PERFORM UNTIL W-COPY-IDREFTAB-LF-UT(IX-RAD) = SPACE                  
164003       OR IX-RAD  > 200                                                   
164004       MOVE W-COPY-IDPERSON-BUY(IX-RAD)   TO BUYT-IDPERSON-BUY            
164005       MOVE W-COPY-IDREFTAB-LF-UT(IX-RAD) TO BUYT-IDREFTAB-LF             
164006       MOVE W-COPY-IDREFTAB-HF-UT(IX-RAD) TO BUYT-IDREFTAB-HF             
164007                                                                          
164008       PERFORM IMS-ISRT-WDB619                                            
164009       MOVE JA               TO FL-UPD                                    
164010                                                                          
164011       ADD +1              TO IX-RAD                                      
164012     END-PERFORM                                                          
164013     .                                                                    
164100 MFS-RENSA-FAELT-IN SECTION.                                              
164200                                                                          
164300     MOVE MFS-RENSA-FAELT        TO MOD-IDDC-IN                           
STTEST     MOVE MFS-RENSA-FAELT        TO MOD-KVOT-IN                           
STTEST     MOVE MFS-RENSA-FAELT        TO MOD-KVPB-LIM-LF-IN                    
STTEST     MOVE MFS-RENSA-FAELT        TO MOD-KVPB-LIM-HF-IN                    
164700                                                                          
164800     MOVE MFS-RENSA-FAELT        TO MOD-NEW-IDPERSON-BUY                  
164900     MOVE MFS-RENSA-FAELT        TO MOD-NEW-BEBUYER                       
165000     MOVE MFS-RENSA-FAELT        TO MOD-NEW-IDREFTAB-LF                   
165100     MOVE MFS-RENSA-FAELT        TO MOD-NEW-IDREFTAB-HF                   
165200     .                                                                    
165201     EJECT                                                                
165202 MFS-RENSA-FAELT-UT  SECTION.                                             
165203*    --- ALLA UTDATA-FÄLT                                                 
165204     MOVE MFS-RENSA-FAELT        TO MOD-KVOT-UT                           
165205                                    MOD-KVPB-LIM-HF-UT                    
165206                                    MOD-KVPB-LIM-LF-UT                    
165207                                                                          
165208     MOVE +1 TO IX-RAD                                                    
165209     PERFORM UNTIL IX-RAD   > RAD-MAX                                     
165210       MOVE MFS-ERASE-FIELD      TO MOD-IDPERSON-BUY   (IX-RAD)           
165211       MOVE MFS-ERASE-FIELD      TO MOD-BEBUYER        (IX-RAD)           
165212       MOVE MFS-ERASE-FIELD      TO MOD-IDREFTAB-LF-UT (IX-RAD)           
165213       MOVE MFS-ERASE-FIELD      TO MOD-IDREFTAB-HF-UT (IX-RAD)           
165214       ADD +1 TO IX-RAD                                                   
165215     END-PERFORM                                                          
165216     .                                                                    
165217                                                                          
165218 MFS-CLOSE-FIELD-IN SECTION.                                              
165219*    --- ALLA INDATA-FÄLT                                                 
165220     MOVE MFS-CLOSE-FIELD TO MOD-COPY-IDDC-ATTR                           
165221                             MOD-KVOT-IN-ATTR                             
165222                             MOD-KVPB-LIM-HF-IN-ATTR                      
165223                             MOD-KVPB-LIM-LF-IN-ATTR                      
165224                             MOD-NEW-IDPERSON-BUY-ATTR                    
165225                             MOD-NEW-BEBUYER-ATTR                         
165226                             MOD-NEW-IDREFTAB-LF-ATTR                     
165227                             MOD-NEW-IDREFTAB-HF-ATTR                     
165228     MOVE +1 TO IX-RAD                                                    
165229     PERFORM UNTIL IX-RAD   > RAD-MAX                                     
165230       MOVE MFS-CLOSE-FIELD TO MOD-IDREFTAB-LF-IN-ATTR (IX-RAD)           
165231       MOVE MFS-CLOSE-FIELD TO MOD-IDREFTAB-HF-IN-ATTR (IX-RAD)           
165232       ADD +1 TO IX-RAD                                                   
165233     END-PERFORM                                                          
165234     .                                                                    
165235                                                                          
165236* --- IMS SEKTIONER ---                                                   
165237     SKIP3                                                                
165238 IMS-GET-MSG SECTION.                                                     
165239                                                                          
165240     MOVE '  QC' TO GODK-STATUSKODER                                      
165241     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
165242     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
165243     PERFORM IMS-STATUSKONTROLL                                           
165244     .                                                                    
165245     SKIP3                                                                
165246 IMS-INSERT-MSG SECTION.                                                  
165247                                                                          
165248     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
165249     MOVE SPACE TO GODK-STATUSKODER                                       
165250     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
165251     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
165252     PERFORM IMS-STATUSKONTROLL                                           
165253     .                                                                    
165254     EJECT                                                                
165255 IMS-GU-WDB601    SECTION.                                                
165256     MOVE 'GU-WDB601' TO CURRENT-IMS-SECTION                              
165257                                                                          
165258     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
165259          DELIMITED BY SIZE INTO SSA1                                     
165260     MOVE '  GE' TO GODK-STATUSKODER                                      
165270     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
165280     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
165290     PERFORM IMS-STATUSKONTROLL                                           
165300     .                                                                    
165400     EJECT                                                                
165500 IMS-GU-WDB601-B  SECTION.                                                
165600     MOVE 'GU-WDB601-B' TO CURRENT-IMS-SECTION                            
165700                                                                          
165800     STRING 'WDB601  (IDDC     =' W-IDDC-COPY-X ')'                       
165900          DELIMITED BY SIZE INTO SSA1                                     
166000     MOVE '  GE' TO GODK-STATUSKODER                                      
166100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
166200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
166300     PERFORM IMS-STATUSKONTROLL                                           
166400     .                                                                    
166500     EJECT                                                                
166600 IMS-GHU-WDB601 SECTION.                                                  
166700     MOVE 'GHU-WDB601' TO CURRENT-IMS-SECTION                             
166800                                                                          
166900     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
167000          DELIMITED BY SIZE INTO SSA1                                     
167100     MOVE '  GE' TO GODK-STATUSKODER                                      
167200     CALL CBLTDLI USING GHU WDB6-PCB DLI-IO-WDB601 SSA1                   
167300     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
167400     PERFORM IMS-STATUSKONTROLL                                           
167500     .                                                                    
167600                                                                          
167700 IMS-REPL-WDB601 SECTION.                                                 
167800     MOVE 'REPL-WDB601' TO CURRENT-IMS-SECTION                            
167900                                                                          
168000     MOVE '  ' TO GODK-STATUSKODER                                        
168100     CALL CBLTDLI USING REPL WDB6-PCB DLI-IO-WDB601                       
168200     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
168300     PERFORM IMS-STATUSKONTROLL                                           
168301     .                                                                    
168302 IMS-GNP-WDB615 SECTION.                                                  
168303     MOVE 'GNP-WDB615' TO CURRENT-IMS-SECTION                             
168304                                                                          
168305     STRING 'WDB615  (IDTRANS  =' W-IDTRANS-B6-X ')'                      
168306     DELIMITED BY SIZE INTO SSA1                                          
168307     MOVE '  GE' TO GODK-STATUSKODER                                      
168308     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB615 SSA1                   
168309     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
168310     PERFORM IMS-STATUSKONTROLL                                           
168311     .                                                                    
168312     EJECT                                                                
168313 IMS-GHU-WDB615 SECTION.                                                  
168314     MOVE 'GHU-WDB615' TO CURRENT-IMS-SECTION                             
168315                                                                          
168316     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
168317          DELIMITED BY SIZE INTO SSA1                                     
168318     STRING 'WDB615  (IDTRANS  =' W-IDTRANS-B6-X ')'                      
168319          DELIMITED BY SIZE INTO SSA2                                     
168320     MOVE '  GE' TO GODK-STATUSKODER                                      
168321     CALL CBLTDLI USING GHU WDB6-PCB DLI-IO-WDB615 SSA1 SSA2              
168322     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
168323     PERFORM IMS-STATUSKONTROLL                                           
168324     .                                                                    
168325     EJECT                                                                
168326 IMS-REPL-WDB615 SECTION.                                                 
168327     MOVE 'REPL-WDB615' TO CURRENT-IMS-SECTION                            
168328                                                                          
168329     MOVE '  ' TO GODK-STATUSKODER                                        
168330     CALL CBLTDLI USING REPL WDB6-PCB DLI-IO-WDB615                       
168331     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
168332     PERFORM IMS-STATUSKONTROLL                                           
168333     .                                                                    
168334     EJECT                                                                
168335 IMS-ISRT-WDB615 SECTION.                                                 
168336     MOVE 'ISRT-WDB615' TO CURRENT-IMS-SECTION                            
168337                                                                          
168338     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
168339          DELIMITED BY SIZE INTO SSA1                                     
168340     MOVE 'WDB615  ' TO SSA2                                              
168341     MOVE '    ' TO GODK-STATUSKODER                                      
168342     CALL CBLTDLI USING ISRT WDB6-PCB DLI-IO-WDB615 SSA1 SSA2             
168343     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
168344     PERFORM IMS-STATUSKONTROLL                                           
168345     .                                                                    
168346     EJECT                                                                
168347 IMS-GNP-WDB619-FIRST SECTION.                                            
168348     MOVE 'GNP-WDB619' TO CURRENT-IMS-SECTION                             
168349                                                                          
168350     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
168360          DELIMITED BY SIZE INTO SSA1                                     
168370     STRING 'WDB619  (IDPERSBU =' W-IDPERSON-BUY-X                        
168380                    '!IDPERSBU >' W-IDPERSON-BUY-X ')'                    
168390          DELIMITED BY SIZE INTO SSA2                                     
168400     MOVE '  GE' TO GODK-STATUSKODER                                      
168500     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB619 SSA1 SSA2              
168600     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
168700     PERFORM IMS-STATUSKONTROLL                                           
168800     .                                                                    
168900 IMS-GNP-WDB619 SECTION.                                                  
169000     MOVE 'GNP-WDB619' TO CURRENT-IMS-SECTION                             
169100                                                                          
169200     MOVE 'WDB619  '       TO SSA1                                        
169300     MOVE '  GE' TO GODK-STATUSKODER                                      
169400     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB619 SSA1                   
169500     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
169600     PERFORM IMS-STATUSKONTROLL                                           
169700     .                                                                    
169800                                                                          
169900 IMS-GHU-WDB619 SECTION.                                                  
170000     MOVE 'GHU-WDB619' TO CURRENT-IMS-SECTION                             
170100                                                                          
170200     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
170300          DELIMITED BY SIZE INTO SSA1                                     
170400     STRING 'WDB619  (IDPERSBU =' W-IDPERSON-BUY-X ')'                    
170500          DELIMITED BY SIZE INTO SSA2                                     
170600     MOVE '  ' TO GODK-STATUSKODER                                        
170700     CALL CBLTDLI USING GHU WDB6-PCB DLI-IO-WDB619 SSA1 SSA2              
170800     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
170900     PERFORM IMS-STATUSKONTROLL                                           
171000     .                                                                    
171100 IMS-ISRT-WDB619 SECTION.                                                 
171200     MOVE 'ISRT-WDB619' TO CURRENT-IMS-SECTION                            
171300                                                                          
171400     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
171500          DELIMITED BY SIZE INTO SSA1                                     
171600     MOVE 'WDB619 ' TO SSA2                                               
171700     MOVE '  II' TO GODK-STATUSKODER                                      
171800     CALL CBLTDLI USING ISRT WDB6-PCB DLI-IO-WDB619 SSA1 SSA2             
171900     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
172000     PERFORM IMS-STATUSKONTROLL                                           
172100     .                                                                    
172200                                                                          
172300 IMS-REPL-WDB619 SECTION.                                                 
172400     MOVE 'REPL-WDB619' TO CURRENT-IMS-SECTION                            
172500                                                                          
172600     MOVE '  ' TO GODK-STATUSKODER                                        
172700     CALL CBLTDLI USING REPL WDB6-PCB DLI-IO-WDB619                       
172800     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
172900     PERFORM IMS-STATUSKONTROLL                                           
173000     .                                                                    
173001                                                                          
173002 IMS-DLET-WDB619 SECTION.                                                 
173003     MOVE 'DLET-WDB619' TO CURRENT-IMS-SECTION                            
173004                                                                          
173005     MOVE '  ' TO GODK-STATUSKODER                                        
173006     CALL CBLTDLI USING DLET WDB6-PCB DLI-IO-WDB619                       
173007     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
173008     PERFORM IMS-STATUSKONTROLL                                           
173009     .                                                                    
173010     EJECT                                                                
173020 IMS-GNP-WDB629 SECTION.                                                  
173030     MOVE 'GNP-WDB629' TO CURRENT-IMS-SECTION                             
173040                                                                          
173050     MOVE 'WDB629  '       TO SSA1                                        
173060     MOVE '  GE' TO GODK-STATUSKODER                                      
173070     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB629 SSA1                   
173080     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
173090     PERFORM IMS-STATUSKONTROLL                                           
173100     .                                                                    
173200                                                                          
173300 IMS-ISRT-WDB629 SECTION.                                                 
173400     MOVE 'ISRT-WDB629' TO CURRENT-IMS-SECTION                            
173500                                                                          
173600                                                                          
173700     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
173800          DELIMITED BY SIZE INTO SSA1                                     
173900     STRING 'WDB619  (IDPERSBU =' W-IDPERSON-BUY-X ')'                    
174000          DELIMITED BY SIZE INTO SSA2                                     
174100     MOVE 'WDB629 ' TO SSA3                                               
174200     MOVE '  II' TO GODK-STATUSKODER                                      
174300     CALL CBLTDLI USING                                                   
174400                  ISRT WDB6-PCB DLI-IO-WDB629 SSA1 SSA2 SSA3              
174500     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
174600     PERFORM IMS-STATUSKONTROLL                                           
174601     .                                                                    
174602 IMS-STATUSKONTROLL SECTION.                                              
174603                                                                          
174604     SET STATUS-IX TO 1                                                   
174605     SEARCH GODK-STATUS                                                   
174606       AT END                                                             
174607         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
174608         DELIMITED BY SIZE INTO FELTEXT                                   
174609         CALL FELLOG                                                      
174610       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
174611         CONTINUE                                                         
174612     END-SEARCH                                                           
174613     .                                                                    
