000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6012600.                                                
000300 AUTHOR.         EVA LUNDELL / KJELL ANDRÉ                                
000400 DATE-WRITTEN.   96/05/14    / 2011-03-04                                 
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*        THIS IS A DRIVER PGM FOR TRANSACTIONS W6T126.           .        
000900*                                                                         
001000*        IT TAKES CARE OF ALL TECHNICAL DETAILS RELATED TO WHELP          
001100*        AND 3270 FORMATS AND CALLS SUBPROGRAM W6012610 WHICH             
001200*        CONTAINS ALL BUSINESS LOGIC FOR THESE TRANSACTIONS.              
001300*                                                                         
001400*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM THE WEB            
001500*        EXISTS - W6W12600 (TRANSACTION W6W126T)                          
001600*                                                                         
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     EJECT                                                                
002000 DATA DIVISION.                                                           
002100 WORKING-STORAGE SECTION.                                                 
002200                                                                          
002300 77  HHMMSSTT                    PIC 9(8).                                
002400 77  IDPGM                       PIC X(08)   VALUE 'W6012600'.            
002500                                                                          
002600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
002700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
002800                                                                          
002900 77  JA                          PIC X       VALUE 'J'.                   
003000 77  NEJ                         PIC X       VALUE 'N'.                   
003100                                                                          
003200*    --- INDEX FÖR BLÄDDRINGSRADER                                        
003300 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003400 77  MAX-KVRADER                 PIC S9(4)  VALUE +14   COMP SYNC.        
003500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
003600                                                                          
003700                                                                          
003800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
003900     88  NYCKLAR-OK                          VALUE 'J'.                   
004000     88  NYCKLAR-FEL                         VALUE 'N'.                   
004100                                                                          
004200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004300     88  EGEN-MID                            VALUE '6126'.                
004400     88  GODK-MID                            VALUE '6121' '6122'          
004500                                                   '6123' '6124'          
004600                                                   '6125' '6126'          
004700                                                   '6127' '6128'          
004800                                                   '6129'.                
004900     88  HELP-MID                            VALUE '0551'.                
005000     EJECT                                                                
005100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005200 01  GENERELLA-SUBPROGRAM.                                                
005300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
005400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
005500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005700     03  W6012610                PIC X(8)    VALUE 'W6012610'.            
005800     EJECT                                                                
005900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
006000*01 -COPY WMEDAREA                                                        
006100     SKIP3                                                                
006200 01  MESSAGE-CODES-MEDKONV.                                               
006300     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
006400     03  INF-SISTA-SIDAN         PIC X(3)    VALUE '106'.                 
006500     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
006600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
006700     03  ERR-INFO-MISSING        PIC X(3)    VALUE '413'.                 
006800                                                                          
006900 01  MESSAGE-CODES-2610.                                                  
007000     03  RESP-FIRST-PAGE         PIC X(3)    VALUE '010'.                 
007100     03  RESP-SISTA-SIDAN        PIC X(3)    VALUE '012'.                 
007200     03  RESP-MORE-INFO-EXISTS   PIC X(3)    VALUE '011'.                 
007300     03  RESP-WRONG-KEY          PIC X(3)    VALUE '022'.                 
007400     03  RESP-INFO-MISSING       PIC X(3)    VALUE '027'.                 
007500     EJECT                                                                
007600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
007700*                                                                         
007800 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
007900     SKIP3                                                                
008000*01 -COPY WMSGINIT                                                        
008100     SKIP3                                                                
008200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008300*                                                                         
008400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008500     SKIP3                                                                
008600*01  MID -COPY W6I12601                                                   
008700     EJECT                                                                
008800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
008900     SKIP3                                                                
009000*01  -COPY WMSGAREA                                                       
009100     EJECT                                                                
009200     03  MOD REDEFINES MSG-AREA.                                          
009300*      05  -COPY W6O12601                                                 
009400     EJECT                                                                
009500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
009600     SKIP3                                                                
009700*01  -COPY WMFSAREA                                                       
009800     EJECT                                                                
009900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010000*                                                                         
010100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010200     SKIP3                                                                
010300 01  SPAR-AREA-TILL-BLAEDDRING.                                           
010400     03  W-MINKEY-W6L201.                                                 
010500         05  W-MINKEY01-IDLOPNRM PIC S9(9)   VALUE ZERO COMP-3.           
010600         05  W-MINKEY01-IDRADNR  PIC S9(5)   VALUE ZERO COMP-3.           
010700         05  W-MINKEY01-DAREGDAT PIC  9(8)   VALUE ZERO.                  
010800         05  W-MINKEY01-TIKLOCK  PIC S9(9)   VALUE ZERO COMP-3.           
010900     SKIP3                                                                
011000     03  W-W6L201KY-NEXT.                                                 
011100         05  W-IDLOPNRM-NEXT     PIC S9(9)   VALUE ZERO COMP-3.           
011200         05  W-IDRADNR-NEXT      PIC S9(5)   VALUE ZERO COMP-3.           
011300         05  W-DAREGDAT-NEXT     PIC  9(8)   VALUE ZERO.                  
011400         05  W-TIKLOCK-NEXT      PIC S9(9)   VALUE ZERO COMP-3.           
011500     SKIP3                                                                
011600*    --- STATUS-KOD FRÅN IMS                                              
011700 01  STATUS-WS                   PIC XX.                                  
011800     88  SEGMENT-FINNS                       VALUE '  '.                  
011900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012100     SKIP2                                                                
012200 01  GODK-STATUSKODER.                                                    
012300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012400     EJECT                                                                
012500*    --- IMS FUNKTIONSKODER                                               
012600*01  -COPY W0003                                                          
012700     EJECT                                                                
012800*    ---  PARAMETRAR TILL W6012620                                        
012900 01  REQU-AREA.                                                           
013000*    03 -COPY WZ01REQU                                                    
013100*    03 -COPY W60126I1                                                    
013200     SKIP3                                                                
013300 01  RESP-AREA.                                                           
013400*    03 -COPY WZ01RESP                                                    
013500*    03 -COPY W60126O1                                                    
013600     EJECT                                                                
013700 LINKAGE SECTION.                                                         
013800*01  -COPY W0009   -PRE MSG-                                              
013900*01  -COPY W0008   -PRE USEA-                                             
014000     05  FILLER                  PIC X.                                   
014100     EJECT                                                                
014200*01  -COPY W0008  -PRE UPFB-                                              
014300     05  FILLER                  PIC X.                                   
014400     EJECT                                                                
014500 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB UPFB-PCB.                     
014600 MAIN SECTION.                                                            
014700     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB UPFB-PCB.                     
014800                                                                          
014900     PERFORM IMS-GET-MSG                                                  
015000     IF SEGMENT-FINNS                                                     
015100       PERFORM A-INIT                                                     
015200       PERFORM B-KOLLA-NYCKLAR                                            
015300       IF NYCKLAR-OK                                                      
015400         IF MFS-FIRST                                                     
015500           PERFORM C-FOERSTA-SIDA                                         
015600         ELSE                                                             
015700           IF MFS-NEXT                                                    
015800             PERFORM D-NAESTA-SIDA                                        
015900           END-IF                                                         
016000         END-IF                                                           
016100         PERFORM F-LAES-VISA-INFO                                         
016200         IF RESP-IDMSG-INFO NOT = SPACE                                   
016300         OR RESP-IDMSG-ERROR NOT = SPACE                                  
016400           PERFORM G-SET-MSG                                              
016500         END-IF                                                           
016600       END-IF                                                             
016700       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O12601-CTX + 4                  
016800       PERFORM IMS-INSERT-MSG                                             
016900     END-IF                                                               
017000                                                                          
017100     MOVE ZERO TO RETURN-CODE                                             
017200     GOBACK                                                               
017300     .                                                                    
017400     EJECT                                                                
017500 A-INIT SECTION.                                                          
017600                                                                          
017700     IF MSG-DUBBLA-TRANSKODER                                             
017800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I12601-CTX             
017900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
018000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
018100     ELSE                                                                 
018200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I12601-CTX              
018300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
018400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018500     END-IF                                                               
018600                                                                          
018700     MOVE MSG-KDTRTYP      TO MFS-KDTRTYP                                 
018800     MOVE MSG-IDPFK        TO MFS-IDPFK                                   
018900     MOVE MFS-IDTRANS      TO W-IDTRANS                                   
019000                                                                          
019100     MOVE LOW-VALUE        TO MSG-AREA                                    
019200     MOVE 'W6O126N1'       TO MFS-IDMOD                                   
019300     MOVE '6126'           TO MOD-IDTRANS                                 
019400     MOVE MFS-RENSA-FAELT  TO MOD-TEMFSFEL MOD-TEMFSINF                   
019500                                                                          
019600     MOVE ZERO             TO REQU-IDLOPNRM-KEY                           
019700                              REQU-IDRADNR-START                          
019800                              REQU-DAREGDAT-START                         
019900                              REQU-TIKLOCK-START                          
020000                                                                          
020100     IF EGEN-MID OR HELP-MID                                              
020200       CONTINUE                                                           
020300     ELSE                                                                 
020400       MOVE SPACE TO MFS-KDTRTYP                                          
020500       MOVE '7' TO MFS-IDPFK                                              
020600     END-IF                                                               
020700     .                                                                    
020800     EJECT                                                                
020900 B-KOLLA-NYCKLAR SECTION.                                                 
021000                                                                          
021100     MOVE ALL '+'           TO MSGI-WMSGINIT                              
021200     MOVE '001'             TO MSGI-KDCALL                                
021300     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
021400     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
021500     MOVE '6126'            TO MSGI-IDTRANS                               
021600     IF GODK-MID                                                          
021700         IF EGEN-MID                                                      
021800             MOVE MID-IDLOPNRM-IN TO MSGI-IDLOPNRM                        
021900             IF MID-IDLOPNRM-IN NOT = ALL '+'                             
022000                 IF MID-IDRADNR-IN = ALL '+'                              
022100*------------                                            NY NYCKEL        
022200                     MOVE SPACE TO MID-IDRADNR-IN                         
022300                 END-IF                                                   
022400             END-IF                                                       
022500             MOVE MID-IDRADNR-IN  TO MSGI-IDRADNR                         
022600         ELSE                                                             
022700             MOVE MID-IDLOPNRM-IN     TO MSGI-IDLOPNRM                    
022800             MOVE MID-IDRADNR-IN      TO MSGI-IDRADNR                     
022900         END-IF                                                           
023000     END-IF                                                               
023100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
023200                                                                          
023300                                                                          
023400     MOVE MSGI-IDLAND-SPR        TO MED-IDSKYLT                           
023410     IF MSGI-IDLAND-SPR = 'SE'                                            
023420       MOVE 'SV'                 TO REQU-IDSPRAK                          
023430     ELSE                                                                 
023440       MOVE 'EN'                 TO REQU-IDSPRAK                          
023450     END-IF                                                               
023500                                                                          
023600     MOVE JA TO NYCKLAR-SW                                                
023700                                                                          
023800                                                                          
023900*    -- KONTROLL AV IDLOPNRM                                              
024000     MOVE MFS-RENSA-FAELT TO MOD-IDLOPNRM-IN                              
024100                                                                          
024200     IF MID-IDLOPNRM-IN NOT = ALL '+'                                     
024300       MOVE '7'         TO MFS-IDPFK                                      
024400       MOVE SPACE       TO MFS-KDTRTYP                                    
024500     END-IF                                                               
024600     INSPECT MSGI-IDLOPNRM REPLACING LEADING SPACE BY ZERO                
024700     IF MSGI-IDLOPNRM NUMERIC                                             
024800       MOVE MSGI-IDLOPNRM TO REQU-IDLOPNRM-KEY                            
024900     ELSE                                                                 
025000       MOVE NEJ TO NYCKLAR-SW                                             
025100     END-IF                                                               
025200                                                                          
025300*    -- KONTROLL AV IDRADNR                                               
025400     MOVE MFS-RENSA-FAELT TO MOD-IDRADNR-IN                               
025500                                                                          
025600     IF MID-IDRADNR-IN NOT = ALL '+'                                      
025700         MOVE '7'         TO MFS-IDPFK                                    
025800         MOVE SPACE       TO MFS-KDTRTYP                                  
025900     END-IF                                                               
026000                                                                          
026100     INSPECT MSGI-IDRADNR  REPLACING LEADING SPACE BY ZERO                
026200     IF MSGI-IDRADNR  NUMERIC                                             
026300       MOVE MSGI-IDRADNR  TO REQU-IDRADNR-KEY                             
026400     ELSE                                                                 
026500       MOVE NEJ TO NYCKLAR-SW                                             
026600     END-IF                                                               
026700                                                                          
026800     IF GODK-MID OR NYCKLAR-OK                                            
026900       MOVE MSGI-IDLOPNRM        TO MOD-IDLOPNRM-UT                       
027000       MOVE MSGI-IDRADNR         TO MOD-IDRADNR-UT                        
027100       INSPECT MOD-IDRADNR-UT REPLACING LEADING ZEROS BY SPACE            
027200     ELSE                                                                 
027300       MOVE MFS-RENSA-FAELT TO MOD-IDLOPNRM-UT                            
027400                               MOD-IDRADNR-UT                             
027500     END-IF                                                               
027600                                                                          
027700     IF NYCKLAR-FEL                                                       
027800       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
027900       CALL WMEDKONV USING MED-WMEDAREA                                   
028000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
028100       PERFORM MFS-RENSA-FAELT-IN                                         
028200       PERFORM MFS-RENSA-FAELT-UT                                         
028300     END-IF                                                               
028400     .                                                                    
028500     EJECT                                                                
028600 C-FOERSTA-SIDA SECTION.                                                  
028700                                                                          
028800     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
028900     CALL WMEDKONV USING MED-WMEDAREA                                     
029000     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
029100                                                                          
029200     PERFORM MFS-RENSA-FAELT-IN                                           
029300                                                                          
029400     MOVE ZERO        TO REQU-IDRADNR-START                               
029500                         REQU-DAREGDAT-START                              
029600                         REQU-TIKLOCK-START                               
029700     .                                                                    
029800     EJECT                                                                
029900 D-NAESTA-SIDA SECTION.                                                   
030000                                                                          
030100     IF MFS-IDTRANS   = '6126'                                            
030200       MOVE MSGI-SPAR-AREA    TO SPAR-AREA-TILL-BLAEDDRING                
030300       MOVE W-IDLOPNRM-NEXT   TO REQU-IDLOPNRM-KEY                        
030400       MOVE W-IDRADNR-NEXT    TO REQU-IDRADNR-START                       
030500       MOVE W-DAREGDAT-NEXT   TO REQU-DAREGDAT-START                      
030600       MOVE W-TIKLOCK-NEXT    TO REQU-TIKLOCK-START                       
030700     ELSE                                                                 
030800       IF MSGI-IDLOPNRM NUMERIC                                           
030900           MOVE MSGI-IDLOPNRM        TO REQU-IDLOPNRM-KEY                 
031000       END-IF                                                             
031100                                                                          
031200       IF MSGI-IDRADNR NUMERIC                                            
031300           MOVE MSGI-IDRADNR        TO REQU-IDRADNR-KEY                   
031400       END-IF                                                             
031500                                                                          
031600       PERFORM MFS-RENSA-FAELT-IN                                         
031700     END-IF                                                               
031800     .                                                                    
031900     EJECT                                                                
032000 E-SAMMA-SIDA SECTION.                                                    
032100*                                                                         
032200     IF EGEN-MID OR HELP-MID                                              
032300       MOVE W-MINKEY01-IDLOPNRM TO REQU-IDLOPNRM-KEY                      
032400       MOVE W-MINKEY01-IDRADNR  TO REQU-IDRADNR-START                     
032500       MOVE W-MINKEY01-DAREGDAT TO REQU-DAREGDAT-START                    
032600       MOVE W-MINKEY01-TIKLOCK  TO REQU-TIKLOCK-START                     
032700     ELSE                                                                 
032800       MOVE MSGI-IDLOPNRM       TO REQU-IDLOPNRM-KEY                      
032900       MOVE MSGI-IDRADNR        TO REQU-IDRADNR-START                     
033000       MOVE ZERO                TO REQU-DAREGDAT-START                    
033100       MOVE ZERO                TO REQU-TIKLOCK-START                     
033200     END-IF                                                               
033300     .                                                                    
033400                                                                          
033500     EJECT                                                                
033600 F-LAES-VISA-INFO SECTION.                                                
033700     SKIP2                                                                
033800                                                                          
033900*    KEY REQUEST FIELDS ALREADY SET. NO OTHER INPUT FIELDS                
034000                                                                          
034100     CALL W6012610 USING                                                  
034200          MSG-PCB UPFB-PCB MAX-KVRADER                                    
034300          REQU-AREA  RESP-AREA                                            
034400                                                                          
034500     MOVE 1 TO INDX                                                       
034600     PERFORM UNTIL INDX > MAX-KVRADER                                     
034700       IF RESP-IDRADNR (INDX) NOT = ALL '+'                               
034800         PERFORM FA-LAES-VISA-RADDATA                                     
034900       ELSE                                                               
035000         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
035100       END-IF                                                             
035200       ADD 1 TO INDX                                                      
035300     END-PERFORM                                                          
035400                                                                          
035500     MOVE REQU-IDLOPNRM-KEY     TO W-MINKEY01-IDLOPNRM                    
035600     MOVE RESP-IDRADNR-START    TO W-MINKEY01-IDRADNR                     
035700     MOVE RESP-DAREGDAT-START   TO W-MINKEY01-DAREGDAT                    
035800     MOVE RESP-TIKLOCK-START    TO W-MINKEY01-TIKLOCK                     
035900                                                                          
036000     MOVE REQU-IDLOPNRM-KEY     TO W-IDLOPNRM-NEXT                        
036100     MOVE RESP-IDRADNR-NEXT     TO W-IDRADNR-NEXT                         
036200     MOVE RESP-DAREGDAT-NEXT    TO W-DAREGDAT-NEXT                        
036300     MOVE RESP-TIKLOCK-NEXT     TO W-TIKLOCK-NEXT                         
036400                                                                          
036500     MOVE SPAR-AREA-TILL-BLAEDDRING TO MSGI-SPAR-AREA                     
036600                                                                          
036700     MOVE '002'             TO MSGI-KDCALL                                
036800     MOVE '6126'            TO MSGI-IDTRANS                               
036900     CALL W005INIT    USING MSGI-WMSGINIT  USEA-PCB                       
037000     .                                                                    
037100     EJECT                                                                
037200                                                                          
037300 FA-LAES-VISA-RADDATA SECTION.                                            
037400     SKIP2                                                                
037500     MOVE RESP-IDRADNR (INDX)      TO MOD-IDRADNR (INDX)                  
037600     MOVE RESP-TIREGDAT (INDX)     TO MOD-TIREGDAT (INDX)                 
037700     MOVE RESP-TIHHMM (INDX)       TO MOD-TIHHMM (INDX)                   
037800     MOVE RESP-KDINLSTA (INDX)     TO MOD-KDINLSTA (INDX)                 
037900     MOVE RESP-ADINLOMR (INDX)     TO MOD-ADINLOMR (INDX)                 
038000     MOVE RESP-ADINLOMR-NXT (INDX) TO MOD-ADINLOMR-NXT (INDX)             
038100     MOVE RESP-KVINLART (INDX)     TO MOD-KVINLART (INDX)                 
038200     MOVE RESP-IDUSER (INDX)       TO MOD-IDUSER (INDX)                   
038300     .                                                                    
038400                                                                          
038500     EJECT                                                                
038600                                                                          
038700 G-SET-MSG    SECTION.                                                    
038800                                                                          
038900     MOVE RESP-IDMSG-INFO  TO MOD-TEMFSINF                                
039000     MOVE RESP-IDMSG-ERROR TO MOD-TEMFSFEL                                
039100                                                                          
039200     IF RESP-IDMSG-INFO = RESP-FIRST-PAGE                                 
039300       MOVE INF-FIRST-PAGE       TO MED-IDMFSINF                          
039400       CALL WMEDKONV USING MED-WMEDAREA                                   
039500       MOVE MED-MFSINF           TO MOD-TEMFSINF                          
039600     END-IF                                                               
039700     IF RESP-IDMSG-INFO = RESP-SISTA-SIDAN                                
039800       MOVE INF-SISTA-SIDAN      TO MED-IDMFSINF                          
039900       CALL WMEDKONV USING MED-WMEDAREA                                   
040000       MOVE MED-MFSINF           TO MOD-TEMFSINF                          
040100     END-IF                                                               
040200     IF RESP-IDMSG-INFO = RESP-MORE-INFO-EXISTS                           
040300       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
040400       CALL WMEDKONV USING MED-WMEDAREA                                   
040500       MOVE MED-MFSINF           TO MOD-TEMFSINF                          
040600     END-IF                                                               
040700                                                                          
040800     IF RESP-IDMSG-ERROR = RESP-WRONG-KEY                                 
040900       MOVE ERR-WRONG-KEY        TO MED-IDMFSFEL                          
041000       CALL WMEDKONV USING MED-WMEDAREA                                   
041100       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
041200     END-IF                                                               
041300                                                                          
041400     IF RESP-IDMSG-ERROR = RESP-INFO-MISSING                              
041500       MOVE ERR-INFO-MISSING     TO MED-IDMFSFEL                          
041600       CALL WMEDKONV USING MED-WMEDAREA                                   
041700       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
041800     END-IF                                                               
041900     .                                                                    
042000     EJECT                                                                
042100                                                                          
042200 MFS-RENSA-FAELT-UT SECTION.                                              
042300                                                                          
042400*    --- ALLA UTDATA-FÄLT                                                 
042500*    --- INKL. BLÄDDRINGSNYCKLAR                                          
042600     MOVE MFS-RENSA-FAELT TO MOD-IDLOPNRM-UT                              
042700                             MOD-IDRADNR-UT                               
042800     .                                                                    
042900     SKIP3                                                                
043000 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
043100                                                                          
043200*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
043300     MOVE MFS-RENSA-FAELT TO MOD-IDRADNR (INDX)                           
043400                             MOD-TIREGDAT (INDX)                          
043500                             MOD-TIHHMM (INDX)                            
043600                             MOD-KDINLSTA (INDX)                          
043700                             MOD-ADINLOMR (INDX)                          
043800                             MOD-ADINLOMR-NXT (INDX)                      
043900                             MOD-KVINLART (INDX)                          
044000                             MOD-IDUSER (INDX)                            
044100     .                                                                    
044200     SKIP3                                                                
044300 MFS-RENSA-FAELT-IN SECTION.                                              
044400                                                                          
044500*    --- ALLA INDATA-FÄLT                                                 
044600     MOVE MFS-RENSA-FAELT TO MOD-IDLOPNRM-IN                              
044700                             MOD-IDRADNR-IN                               
044800     .                                                                    
044900     EJECT                                                                
045000* --- IMS SEKTIONER ---                                                   
045100     SKIP3                                                                
045200 IMS-GET-MSG SECTION.                                                     
045300                                                                          
045400     MOVE '  QC' TO GODK-STATUSKODER                                      
045500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
045600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
045700     PERFORM IMS-STATUSKONTROLL                                           
045800     .                                                                    
045900     SKIP3                                                                
046000 IMS-INSERT-MSG SECTION.                                                  
046100                                                                          
046200     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
046300       MOVE '0' TO MFS-KDHUVOMR                                           
046400     END-IF                                                               
046500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
046600     MOVE SPACE TO GODK-STATUSKODER                                       
046700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
046800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
046900     PERFORM IMS-STATUSKONTROLL                                           
047000     .                                                                    
047100     EJECT                                                                
047200 IMS-STATUSKONTROLL SECTION.                                              
047300                                                                          
047400     SET STATUS-IX TO 1                                                   
047500     SEARCH GODK-STATUS                                                   
047600       AT END                                                             
047700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
047800         DELIMITED BY SIZE INTO FELTEXT                                   
047900         CALL FELLOG                                                      
048000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
048100         CONTINUE                                                         
048200     END-SEARCH                                                           
048300     .                                                                    
