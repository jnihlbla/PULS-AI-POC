000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6011800.                                                
000300 AUTHOR.         KJELL                                                    
000400 DATE-WRITTEN.   2011-12-15.                                              
000500 DATE-COMPILED.                                                           
000600*                                                                         
000700*    FUNKTION:                                                            
000800*        BACKOUT PART OF A GOODS RECEIPT                                  
000900*                                                                         
001000*        THIS IS A DRIVER PGM FOR TRANSACTIONS W6T118 AND W6T118U.        
001100*                                                                         
001200*        IT TAKES CARE OF ALL TECHNICAL DETAILS RELATED TO WHELP          
001300*        AND 3270 FORMATS AND CALLS SUBPROGRAM W6011810 WHICH             
001400*        CONTAINS ALL BUSINESS LOGIC FOR THESE TRANSACTIONS.              
001500*                                                                         
001600*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM THE WEB            
001700*        EXISTS - W6W11800 (TRANSACTIONS W6W118T AND W6W118U)             
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400 WORKING-STORAGE SECTION.                                                 
002500                                                                          
002600*    -- CHECKED BY WY2000                                                 
002700 77  IDPGM                       PIC X(08)   VALUE 'W6011800'.            
002800                                                                          
002900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003100                                                                          
003200 77  JA                          PIC X       VALUE 'J'.                   
003300 77  NEJ                         PIC X       VALUE 'N'.                   
003400                                                                          
003500*    --- INDEX FÖR BLÄDDRINGSRADER                                        
003600 77  INDX                        PIC S9(4)  VALUE +0    COMP.             
003700 77  MAX-KVRADER                 PIC S9(4)  VALUE +12   COMP.             
003800 77  OUT-KVRADER                 PIC S9(4)              COMP.             
003900 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP.             
004000                                                                          
004100                                                                          
004200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004300     88  EGEN-MID                            VALUE '6118'.                
004400     88  GODK-MID                            VALUE '6111' '6112'          
004500                                                   '6113' '6114'          
004600                                                   '6115' '6116'          
004700                                                   '6118' '6119'.         
004800     88  HELP-MID                            VALUE '0551'.                
004900     EJECT                                                                
005000*      --- VALID IDDC CODES                                               
005100*01    -COPY WWDC99                                                       
005200     EJECT                                                                
005300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005400 01  GENERELLA-SUBPROGRAM.                                                
005500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
005600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
005800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005900     03  W6011810                PIC X(8)    VALUE 'W6011810'.            
006000     EJECT                                                                
006100*01 -COPY WMSGINIT                                                        
006200     SKIP3                                                                
006300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
006400*01 -COPY WMEDAREA                                                        
006500     SKIP3                                                                
006600 01  MESSAGE-CODES.                                                       
006700     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
006800     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
006900     03  ERR-007-OTILLATEN-UPPD  PIC X(3)    VALUE '007'.                 
007000     03  ERR-010-NOT-IN-REG      PIC X(3)    VALUE '010'.                 
007100     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007200     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007300     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007400     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007600     03  ERR-MAN-CHG-NEEDED      PIC X(3)    VALUE '232'.                 
007700     03  ERR-IR-ALREADY-REGISTRED PIC X(3)   VALUE '165'.                 
007800     03  ERR-IR-REMAINS-ADM-REP   PIC X(3)   VALUE '293'.                 
007900                                                                          
008000*    --- WEBB-ANPASSADE FELKODER FRÅN SUBPGM W6011810                     
008100 01  W-IDMSG-ERROR               PIC X(3).                                
008200     88   INVALID-UPDATE                     VALUE '007'.                 
008300     88   MORE-INFO-EXISTS                   VALUE '011'.                 
008400     88   EXEC-AND-NO-DATA                   VALUE '014'.                 
008500     88   CORR-MARKED-FLDS                   VALUE '020'.                 
008600     88   WRONG-KEY                          VALUE '022'.                 
008700     88   NOT-FOUND                          VALUE '025'.                 
008800     88   ALREADY-REGISTRED                  VALUE '030'.                 
008900     88   PRESS-EXEC                         VALUE '276'.                 
009000     88   MAN-CHG-NEEDED                     VALUE '335'.                 
009100     88   IR-REMAINS-ADM-REP                 VALUE '348'.                 
009200     88   FIRST-PAGE                         VALUE '010'.                 
009300                                                                          
009400 01  W-IDMSG-INFO                PIC X(3).                                
009500     88   UPDATE-DONE                        VALUE '001'.                 
009600                                                                          
009700     EJECT                                                                
009800*    -- TEMPORÄRA ARBETSFÄLT FÖR NYCKLAR                                  
009900 01  W-IDLOPNRM                  PIC X(8).                                
010000 01  W-IDDC                      PIC X(2).                                
010100 01  W-IDRADNR                   PIC 9(4).                                
010200                                                                          
010300     EJECT                                                                
010400*    -- PARAMETRAR TILL SUBPGM W6011810                                   
010500 01  REQU-AREA.                                                           
010600*    03 -COPY WZ01REQU                                                    
010700*    03 -COPY W60118I1                                                    
010800                                                                          
010900 01  RESP-AREA.                                                           
011000*    03 -COPY WZ01RESP                                                    
011100*    03 -COPY W60118O1                                                    
011200     EJECT                                                                
011300                                                                          
011400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
011500*                                                                         
011600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011700     SKIP3                                                                
011800*01  MID -COPY W6I11801                                                   
011900     EJECT                                                                
012000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012100     SKIP3                                                                
012200*01  -COPY WMSGAREA                                                       
012300     EJECT                                                                
012400     03  MOD REDEFINES MSG-AREA.                                          
012500*      05  -COPY W6O11801                                                 
012600     EJECT                                                                
012700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012800     SKIP3                                                                
012900*01  -COPY WMFSAREA                                                       
013000     EJECT                                                                
013100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013200*                                                                         
013300     SKIP3                                                                
013400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013500*    --- STATUS-KOD FRÅN IMS                                              
013600 01  STATUS-WS                   PIC XX.                                  
013700     88  SEGMENT-FINNS                       VALUE '  '.                  
013800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014000     SKIP2                                                                
014100 01  GODK-STATUSKODER.                                                    
014200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014300     EJECT                                                                
014400*    --- IMS FUNKTIONSKODER                                               
014500*01  -COPY W0003                                                          
014600     EJECT                                                                
014700 LINKAGE SECTION.                                                         
014800                                                                          
014900*01  -COPY W0009   -PRE MSG-                                              
015000     EJECT                                                                
015100*01  -COPY W0009   -PRE 6202-                                             
015200     EJECT                                                                
015300*01  -COPY W0008  -PRE USEA-                                              
015400     05  FILLER                  PIC X.                                   
015500     EJECT                                                                
015600*01  -COPY W0008  -PRE INLA-                                              
015700     05  FILLER                  PIC X.                                   
015800     EJECT                                                                
015900*01  -COPY W0008  -PRE SEQB-                                              
016000     05  FILLER                  PIC X.                                   
016100     EJECT                                                                
016200*01  -COPY W0008  -PRE PLAA-                                              
016300     05  FILLER                  PIC X.                                   
016400     EJECT                                                                
016500*01  -COPY W0008  -PRE ARTC-                                              
016600     05  FILLER                  PIC X.                                   
016700     EJECT                                                                
016800*01  -COPY W0008  -PRE INLE-                                              
016900     05  FILLER                  PIC X.                                   
017000     EJECT                                                                
017100*01  -COPY W0008  -PRE KVAE-                                              
017200     05  FILLER                  PIC X.                                   
017300     EJECT                                                                
017400*01  -COPY W0008  -PRE WDK7-                                              
017500     05  FILLER                  PIC X.                                   
017600*01  -COPY W0008  -PRE INLC-                                              
017700     05  FILLER                  PIC X.                                   
017800     EJECT                                                                
017900*01  -COPY W0008  -PRE LOGA-                                              
018000     05  FILLER                  PIC X.                                   
018100     EJECT                                                                
018200*01  -COPY W0008  -PRE UPFA-                                              
018300     05  FILLER                  PIC X.                                   
018400     EJECT                                                                
018500*01  -COPY W0008  -PRE WDD3-                                              
018600     05  FILLER                  PIC X.                                   
018700     EJECT                                                                
018706 01  9305-AVG-PCB               PIC X.                                    
018707     EJECT                                                                
018708 01  AVG-WDB6-PCB               PIC X.                                    
018709     EJECT                                                                
018710*01  -COPY W0008  -PRE WDB6-                                              
018711     05  FILLER                  PIC X.                                   
018712     EJECT                                                                
018713*01  -COPY W0008 -PRE LEV-                                                
018714     05  FILLER                  PIC X(5).                                
018720     EJECT                                                                
018760*01  -COPY W0008  -PRE 9305-                                              
018770     05  FILLER                  PIC X.                                   
018780                                                                          
018800 PROCEDURE DIVISION  USING MSG-PCB 6202-PCB USEA-PCB INLA-PCB             
018900                           SEQB-PCB PLAA-PCB                              
019000                           ARTC-PCB INLE-PCB KVAE-PCB WDK7-PCB            
019100                           INLC-PCB LOGA-PCB UPFA-PCB                     
019200                           WDD3-PCB 9305-AVG-PCB                          
019201                           AVG-WDB6-PCB                                   
019210                           WDB6-PCB LEV-PCB 9305-PCB.                     
019300 MAIN SECTION.                                                            
019400     ENTRY 'DLITCBL' USING MSG-PCB 6202-PCB USEA-PCB INLA-PCB             
019500                           SEQB-PCB PLAA-PCB                              
019600                           ARTC-PCB INLE-PCB KVAE-PCB WDK7-PCB            
019700                           INLC-PCB LOGA-PCB UPFA-PCB                     
019800                           WDD3-PCB 9305-AVG-PCB                          
019801                           AVG-WDB6-PCB                                   
019810                           WDB6-PCB LEV-PCB 9305-PCB.                     
019900                                                                          
020000     PERFORM IMS-GET-MSG                                                  
020100     IF SEGMENT-FINNS                                                     
020200       PERFORM A-INIT                                                     
020300       PERFORM B-INIT-KEYS-AND-MOD                                        
020400       IF MFS-UPDATE                                                      
020500         SET REQU-UPDATE TO TRUE                                          
020600       ELSE                                                               
020700         IF MFS-FIRST                                                     
020800           SET REQU-FIRST  TO TRUE                                        
020900           PERFORM C-FOERSTA-SIDA                                         
021000         ELSE                                                             
021100           IF MFS-NEXT                                                    
021200             SET REQU-NEXT  TO TRUE                                       
021300             PERFORM D-NAESTA-SIDA                                        
021400           ELSE                                                           
021500             SET REQU-QUERY  TO TRUE                                      
021600             PERFORM E-SAMMA-SIDA                                         
021700           END-IF                                                         
021800         END-IF                                                           
021900       END-IF                                                             
022000       PERFORM F-CALL-BIZ-LOGIC                                           
022100                                                                          
022200       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O11801 + 4                      
022300       PERFORM IMS-INSERT-MSG                                             
022400     END-IF                                                               
022500                                                                          
022600     MOVE ZERO TO RETURN-CODE                                             
022700     GOBACK                                                               
022800     .                                                                    
022900     EJECT                                                                
023000 A-INIT SECTION.                                                          
023100                                                                          
023200     IF MSG-DUBBLA-TRANSKODER                                             
023300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I11801                 
023400       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
023500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
023600     ELSE                                                                 
023700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I11801                  
023800       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
023900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
024000     END-IF                                                               
024100                                                                          
024200     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
024300     MOVE MSG-IDPFK TO MFS-IDPFK                                          
024400     MOVE MFS-IDTRANS TO W-IDTRANS                                        
024500                                                                          
024600     MOVE LOW-VALUE TO MSG-AREA                                           
024700     MOVE 'W6O118N1' TO MFS-IDMOD                                         
024800     MOVE '6118' TO MOD-IDTRANS                                           
024900     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
025000                                                                          
025100     IF EGEN-MID OR HELP-MID                                              
025200       CONTINUE                                                           
025300     ELSE                                                                 
025400       MOVE SPACE TO MFS-KDTRTYP                                          
025500       MOVE '7' TO MFS-IDPFK                                              
025600     END-IF                                                               
025700                                                                          
025800     PERFORM MFS-FORM-ATTR                                                
025900                                                                          
026000     PERFORM AA-INIT-NYCKLAR                                              
026100                                                                          
026200     IF MSGI-IDLAND-SPR = 'GB'                                            
026300     OR ENGLISH-TEXT                                                      
026400       MOVE +2                 TO SPRAK-IX                                
026500       MOVE 'GB '              TO MED-IDSKYLT                             
026600     ELSE                                                                 
026700       MOVE +1                 TO SPRAK-IX                                
026800       MOVE 'S  '              TO MED-IDSKYLT                             
026900     END-IF                                                               
027000     .                                                                    
027100     EJECT                                                                
027200 AA-INIT-NYCKLAR SECTION.                                                 
027300                                                                          
027400     MOVE ALL '+' TO MSGI-WMSGINIT                                        
027500     MOVE '001'                  TO MSGI-KDCALL                           
027600     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
027700     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
027800     MOVE '6118'                 TO MSGI-IDTRANS                          
027900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
028000     .                                                                    
028100                                                                          
028200     EJECT                                                                
028300 B-INIT-KEYS-AND-MOD     SECTION.                                         
028400                                                                          
028500*    -- TO GET A VALID VALUE WHEN UPDATE                                  
028600     MOVE MID-IDRADNR-ENTER TO W-IDRADNR                                  
028700                                                                          
028800     PERFORM BA-KONTROLL-AV-IDLOPNRM                                      
028900     PERFORM BB-KONTROLL-AV-IDDC                                          
029000                                                                          
029100     PERFORM BC-FLYTTA-OEVRIGA-NYCKLAR                                    
029200                                                                          
029300     IF GODK-MID                                                          
029400       MOVE W-IDLOPNRM      TO MOD-IDLOPNRM-UT                            
029500                    INSPECT    MOD-IDLOPNRM-UT                            
029600                    REPLACING LEADING ZERO BY SPACE                       
029700                                                                          
029800       MOVE W-IDDC          TO MOD-IDDC-UT                                
029900                                                                          
030000     ELSE                                                                 
030100       MOVE MFS-RENSA-FAELT TO MOD-IDLOPNRM-UT                            
030200                               MOD-IDDC-UT                                
030300*      + ÖVRIGA SPAR-NYCKLAR                                              
030400                               MOD-IDLEVNR-UT                             
030500                               MOD-IDFS-UT                                
030600                               MOD-TIAVIDAT-UT                            
030700                               MOD-ADINLOMR-PRT-UT                        
030800                               MOD-KDRT-UT                                
030900                               MOD-IDLBBET-UT                             
031000                               MOD-FLKLIVIS-UT                            
031100     END-IF                                                               
031200     .                                                                    
031300                                                                          
031400     EJECT                                                                
031500 BA-KONTROLL-AV-IDLOPNRM   SECTION.                                       
031600                                                                          
031700     MOVE MFS-RENSA-FAELT TO MOD-IDLOPNRM-IN                              
031800                                                                          
031900     IF MID-IDLOPNRM-IN = ALL '+'                                         
032000       MOVE MID-IDLOPNRM-UT     TO W-IDLOPNRM                             
032100                           INSPECT W-IDLOPNRM                             
032200                           REPLACING LEADING SPACE BY ZERO                
032300     ELSE                                                                 
032400       MOVE MID-IDLOPNRM-IN     TO W-IDLOPNRM                             
032500       MOVE     '7'             TO MFS-IDPFK                              
032600       MOVE    SPACE            TO MFS-KDTRTYP                            
032700     END-IF                                                               
032800     .                                                                    
032900     EJECT                                                                
033000 BB-KONTROLL-AV-IDDC    SECTION.                                          
033100                                                                          
033200     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
033300     MOVE     SPACE       TO W-IDDC                                       
033400                                                                          
033500     IF EGEN-MID OR HELP-MID                                              
033600       IF MID-IDDC-IN = ALL '+'                                           
033700         MOVE MSGI-IDDC   TO W-IDDC                                       
033800       ELSE                                                               
033900         MOVE MID-IDDC-IN TO W-IDDC                                       
034000         MOVE    SPACE    TO MFS-KDTRTYP                                  
034100         MOVE     '7'     TO MFS-IDPFK                                    
034200       END-IF                                                             
034300     ELSE                                                                 
034400       IF GODK-MID                                                        
034500         MOVE MSGI-IDDC   TO W-IDDC                                       
034600       END-IF                                                             
034700     END-IF                                                               
034800                                                                          
034900*NDC                                                                      
035000     MOVE W-IDDC       TO WS-IDDC                                         
035100     IF CDC OR NDC                                                        
035200       CONTINUE                                                           
035300     ELSE                                                                 
035400       MOVE   SPACE    TO W-IDDC                                          
035500     END-IF                                                               
035600     .                                                                    
035700     EJECT                                                                
035800 BC-FLYTTA-OEVRIGA-NYCKLAR  SECTION.                                      
035900                                                                          
036000*    -- THESE KEYS ARE NOT USED BY THE W6011810 SUBPROGRAM.               
036100*    -- THEY AR JUST MOVED AROUND IF YOU USE THE CLASSIC IMS              
036200*    -- INTERFACE.                                                        
036300     MOVE MFS-RENSA-FAELT      TO MOD-IDLEVNR-IN                          
036400                                  MOD-IDFS-IN                             
036500                                  MOD-TIAVIDAT-IN                         
036600                                  MOD-ADINLOMR-PRT-IN                     
036700                                  MOD-KDRT-IN                             
036800                                  MOD-IDLBBET-IN                          
036900                                  MOD-FLKLIVIS-IN                         
037000                                                                          
037100     IF MID-IDLEVNR-IN    = ALL '+'                                       
037200         MOVE MID-IDLEVNR-UT   TO MOD-IDLEVNR-UT                          
037300     ELSE                                                                 
037400         MOVE MID-IDLEVNR-IN   TO MOD-IDLEVNR-UT                          
037500     END-IF                                                               
037600                                                                          
037700     IF MID-IDFS-IN       = ALL '+'                                       
037800         MOVE MID-IDFS-UT      TO MOD-IDFS-UT                             
037900     ELSE                                                                 
038000         MOVE MID-IDFS-IN      TO MOD-IDFS-UT                             
038100     END-IF                                                               
038200                                                                          
038300                                                                          
038400     IF MID-TIAVIDAT-IN   = ALL '+'                                       
038500         MOVE MID-TIAVIDAT-UT  TO MOD-TIAVIDAT-UT                         
038600     ELSE                                                                 
038700         MOVE MID-TIAVIDAT-IN  TO MOD-TIAVIDAT-UT                         
038800     END-IF                                                               
038900                                                                          
039000     IF MID-ADINLOMR-PRT-IN = ALL '+'                                     
039100         MOVE MID-ADINLOMR-PRT-UT TO MOD-ADINLOMR-PRT-UT                  
039200     ELSE                                                                 
039300         MOVE MID-ADINLOMR-PRT-IN TO MOD-ADINLOMR-PRT-UT                  
039400     END-IF                                                               
039500                                                                          
039600     IF MID-KDRT-IN       = ALL '+'                                       
039700         MOVE MID-KDRT-UT      TO MOD-KDRT-UT                             
039800     ELSE                                                                 
039900         MOVE MID-KDRT-IN      TO MOD-KDRT-UT                             
040000     END-IF                                                               
040100                                                                          
040200     IF MID-IDLBBET-IN    = ALL '+'                                       
040300         MOVE MID-IDLBBET-UT   TO MOD-IDLBBET-UT                          
040400     ELSE                                                                 
040500         MOVE MID-IDLBBET-IN   TO MOD-IDLBBET-UT                          
040600     END-IF                                                               
040700                                                                          
040800     IF MID-FLKLIVIS-IN   = ALL '+'                                       
040900         MOVE MID-FLKLIVIS-UT  TO MOD-FLKLIVIS-UT                         
041000     ELSE                                                                 
041100         MOVE MID-FLKLIVIS-IN  TO MOD-FLKLIVIS-UT                         
041200     END-IF                                                               
041300     .                                                                    
041400                                                                          
041500     EJECT                                                                
041600 C-FOERSTA-SIDA SECTION.                                                  
041700                                                                          
041800*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
041900     MOVE ZERO  TO W-IDRADNR                                              
042000     PERFORM MFS-RENSA-FAELT-IN                                           
042100     .                                                                    
042200     EJECT                                                                
042300 D-NAESTA-SIDA SECTION.                                                   
042400                                                                          
042500     MOVE MID-IDRADNR-NEXT TO W-IDRADNR                                   
042600     PERFORM MFS-RENSA-FAELT-IN                                           
042700     .                                                                    
042800     EJECT                                                                
042900 E-SAMMA-SIDA SECTION.                                                    
043000                                                                          
043100     IF EGEN-MID OR HELP-MID                                              
043200       MOVE MID-IDRADNR-ENTER TO W-IDRADNR                                
043300     ELSE                                                                 
043400       PERFORM MFS-RENSA-FAELT-IN                                         
043500     END-IF                                                               
043600     .                                                                    
043700     EJECT                                                                
043800 F-CALL-BIZ-LOGIC        SECTION.                                         
043900                                                                          
044000     PERFORM FA-INIT-REQU                                                 
044100                                                                          
044200     CALL W6011810 USING REQU-AREA RESP-AREA MAX-KVRADER                  
044300                         MSG-PCB 6202-PCB          INLA-PCB               
044400                         SEQB-PCB PLAA-PCB                                
044500                         ARTC-PCB INLE-PCB KVAE-PCB WDK7-PCB              
044600                         INLC-PCB LOGA-PCB UPFA-PCB                       
044700                         WDD3-PCB 9305-AVG-PCB                            
044701                         AVG-WDB6-PCB                                     
044710                         WDB6-PCB LEV-PCB                                 
044800                         9305-PCB                                         
044900     PERFORM FB-SET-MSG                                                   
045000     PERFORM FC-INIT-MOD                                                  
045100     .                                                                    
045200                                                                          
045300     EJECT                                                                
045400 FA-INIT-REQU  SECTION.                                                   
045500                                                                          
045600     MOVE MAX-KVRADER         TO REQU-KVRADER                             
045700     MOVE W-IDLOPNRM          TO REQU-IDLOPNRM-KEY                        
045800     MOVE W-IDDC              TO REQU-IDDC-KEY                            
045900                                                                          
046000     IF MSGI-IDLAND-SPR = 'SE'                                            
046100       MOVE 'SV'              TO REQU-IDSPRAK                             
046200     ELSE                                                                 
046300       MOVE 'EN'              TO REQU-IDSPRAK                             
046400     END-IF                                                               
046500                                                                          
046600     MOVE W-IDRADNR           TO REQU-IDRADNR-START                       
046700                                                                          
046800     MOVE 1 TO INDX                                                       
046900     PERFORM UNTIL INDX > MAX-KVRADER                                     
047000                                                                          
047100       MOVE MID-ADINLOMR-UPD (INDX)                                       
047200                               TO REQU-ADINLOMR-UPD-LINE (INDX)           
047300       MOVE MID-IDRADNR (INDX)     TO REQU-IDRADNR-LINE (INDX)            
047400       MOVE MID-KDINLSTA (INDX)    TO REQU-KDINLSTA-LINE (INDX)           
047500                                                                          
047600       ADD 1 TO INDX                                                      
047700     END-PERFORM                                                          
047800     .                                                                    
047900                                                                          
048000     EJECT                                                                
048100 FB-SET-MSG SECTION.                                                      
048200                                                                          
048300     MOVE RESP-IDMSG-ERROR TO W-IDMSG-ERROR                               
048400     MOVE RESP-IDMSG-INFO  TO W-IDMSG-INFO                                
048500                                                                          
048600     IF WRONG-KEY                                                         
048700       MOVE ERR-WRONG-KEY          TO MED-IDMFSFEL                        
048800       PERFORM MFS-RENSA-FAELT-IN                                         
048900       PERFORM MFS-RENSA-FAELT-UT                                         
049000     ELSE                                                                 
049100       IF MORE-INFO-EXISTS                                                
049200         MOVE INF-MORE-INFO-EXISTS  TO MED-IDMFSINF                       
049300       END-IF                                                             
049400       IF UPDATE-DONE                                                     
049500         MOVE INF-UPDATE-DONE       TO MED-IDMFSINF                       
049600       END-IF                                                             
049700                                                                          
049800       IF FIRST-PAGE                                                      
049900         MOVE INF-FIRST-PAGE        TO MED-IDMFSFEL                       
050000       END-IF                                                             
050100       IF PRESS-EXEC                                                      
050200         MOVE INF-PRESS-PF11        TO MED-IDMFSFEL                       
050300       END-IF                                                             
050400       IF CORR-MARKED-FLDS                                                
050500         MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                       
050600       END-IF                                                             
050700       IF INVALID-UPDATE                                                  
050800         MOVE ERR-007-OTILLATEN-UPPD TO MED-IDMFSFEL                      
050900       END-IF                                                             
051000       IF NOT-FOUND                                                       
051100*        IDELMT = IDRADNR/ADINLOMR-U/IDLOPNRM IGNORED                     
051200*        ONE MESSAGE FITS ALL                                             
051300         MOVE ERR-010-NOT-IN-REG    TO MED-IDMFSFEL                       
051400       END-IF                                                             
051500       IF EXEC-AND-NO-DATA                                                
051600         MOVE ERR-PF11-AND-NO-DATA  TO MED-IDMFSFEL                       
051700       END-IF                                                             
051800       IF MAN-CHG-NEEDED                                                  
051900         MOVE ERR-MAN-CHG-NEEDED    TO MED-IDMFSFEL                       
052000       END-IF                                                             
052100       IF ALREADY-REGISTRED AND RESP-IDELMT-ERROR = 'IDKR'                
052200         MOVE ERR-IR-ALREADY-REGISTRED TO MED-IDMFSFEL                    
052300       END-IF                                                             
052400       IF IR-REMAINS-ADM-REP                                              
052500         MOVE ERR-IR-REMAINS-ADM-REP TO MED-IDMFSFEL                      
052600       END-IF                                                             
052700     END-IF                                                               
052800                                                                          
052900     CALL WMEDKONV USING MED-WMEDAREA                                     
053000     MOVE MED-MFSFEL         TO MOD-TEMFSFEL                              
053100     MOVE MED-MFSINF         TO MOD-TEMFSINF                              
053200     .                                                                    
053300                                                                          
053400     EJECT                                                                
053500 FC-INIT-MOD SECTION.                                                     
053600                                                                          
053700     MOVE RESP-IDRADNR-START   TO MOD-IDRADNR-ENTER                       
053800     MOVE RESP-IDRADNR-NEXT    TO MOD-IDRADNR-NEXT                        
053900                                                                          
054000     IF RESP-IDARTNR = ALL '+'                                            
054100       MOVE MFS-ROER-EJ-FAELT  TO MOD-IDARTNR                             
054200     ELSE IF RESP-IDARTNR = SPACE                                         
054300       MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR                             
054400     ELSE                                                                 
054500       MOVE RESP-IDARTNR       TO MOD-IDARTNR                             
054600     END-IF                                                               
054700     END-IF                                                               
054800                                                                          
054900     IF RESP-KVAVIS  = ALL '+'                                            
055000       MOVE MFS-ROER-EJ-FAELT  TO MOD-KVAVIS                              
055100     ELSE IF RESP-KVAVIS  = SPACE                                         
055200       MOVE MFS-RENSA-FAELT    TO MOD-KVAVIS                              
055300     ELSE                                                                 
055400       MOVE RESP-KVAVIS        TO MOD-KVAVIS                              
055500     END-IF                                                               
055600     END-IF                                                               
055700                                                                          
055800     IF RESP-BEART   = ALL '+'                                            
055900        MOVE MFS-ROER-EJ-FAELT  TO MOD-BEART                              
056000     ELSE                                                                 
056100       MOVE RESP-BEART           TO MOD-BEART                             
056200     END-IF                                                               
056300                                                                          
056400     IF RESP-KDSORT  = ALL '+'                                            
056500        MOVE MFS-ROER-EJ-FAELT  TO MOD-KDSORT                             
056600     ELSE                                                                 
056700       MOVE RESP-KDSORT          TO MOD-KDSORT                            
056800     END-IF                                                               
056900                                                                          
057000     IF RESP-BEFT    = ALL '+'                                            
057100       MOVE MFS-ROER-EJ-FAELT  TO MOD-BEFT                                
057200     ELSE IF RESP-KVRAPP  = SPACE                                         
057300       MOVE MFS-RENSA-FAELT    TO MOD-BEFT                                
057400     ELSE                                                                 
057500       MOVE RESP-BEFT          TO MOD-BEFT                                
057600     END-IF                                                               
057700     END-IF                                                               
057800                                                                          
057900     IF RESP-KVRAPP  = ALL '+'                                            
058000       MOVE MFS-ROER-EJ-FAELT  TO MOD-KVRAPP                              
058100     ELSE IF RESP-KVRAPP  = SPACE                                         
058200       MOVE MFS-RENSA-FAELT    TO MOD-KVRAPP                              
058300     ELSE                                                                 
058400       MOVE RESP-KVRAPP        TO MOD-KVRAPP                              
058500     END-IF                                                               
058600     END-IF                                                               
058700                                                                          
058800     MOVE 1 TO INDX                                                       
058900     MOVE RESP-KVRADER TO OUT-KVRADER                                     
059000     PERFORM UNTIL INDX  >  OUT-KVRADER                                   
059100                                                                          
059200       MOVE RESP-ADINLOMR-UPD-LINE-ATTR (INDX)                            
059300                                  TO MOD-ADINLOMR-UPD-ATTR (INDX)         
059400       IF RESP-ADINLOMR-UPD-LINE (INDX) = ALL '+'                         
059500         MOVE MFS-ROER-EJ-FAELT       TO MOD-ADINLOMR-UPD (INDX)          
059600       ELSE                                                               
059700         MOVE RESP-ADINLOMR-UPD-LINE (INDX)                               
059800                                      TO MOD-ADINLOMR-UPD (INDX)          
059900       END-IF                                                             
060000                                                                          
060100       IF RESP-IDRADNR-LINE (INDX) = ALL '+'                              
060200         MOVE MFS-ROER-EJ-FAELT       TO MOD-IDRADNR (INDX)               
060300       ELSE IF RESP-IDRADNR-LINE (INDX) = SPACE                           
060400         MOVE MFS-RENSA-FAELT         TO MOD-IDRADNR (INDX)               
060500       ELSE                                                               
060600         MOVE RESP-IDRADNR-LINE (INDX) TO MOD-IDRADNR (INDX)              
060700       END-IF                                                             
060800       END-IF                                                             
060900                                                                          
061000       IF RESP-KVINLART-LINE (INDX) = ALL '+'                             
061100         MOVE MFS-ROER-EJ-FAELT       TO MOD-KVINLART (INDX)              
061200       ELSE IF RESP-KVINLART-LINE (INDX) = SPACE                          
061300         MOVE MFS-RENSA-FAELT         TO MOD-KVINLART (INDX)              
061400       ELSE                                                               
061500         MOVE RESP-KVINLART-LINE (INDX) TO MOD-KVINLART (INDX)            
061600       END-IF                                                             
061700       END-IF                                                             
061800                                                                          
061900       IF RESP-ADINLOMR-LINE (INDX) = ALL '+'                             
062000         MOVE MFS-ROER-EJ-FAELT       TO MOD-ADINLOMR (INDX)              
062100       ELSE                                                               
062200         MOVE RESP-ADINLOMR-LINE (INDX) TO MOD-ADINLOMR (INDX)            
062300       END-IF                                                             
062400                                                                          
062500       IF RESP-KDINLSTA-LINE (INDX) = ALL '+'                             
062600         MOVE MFS-ROER-EJ-FAELT       TO MOD-KDINLSTA (INDX)              
062700       ELSE                                                               
062800         MOVE RESP-KDINLSTA-LINE (INDX) TO MOD-KDINLSTA (INDX)            
062900       END-IF                                                             
063000                                                                          
063100       IF RESP-IDLEVNR-KOLLI-LINE (INDX) = ALL '+'                        
063200         MOVE MFS-ROER-EJ-FAELT       TO MOD-IDLEVNR-KOLLI (INDX)         
063300       ELSE                                                               
063400         MOVE RESP-IDLEVNR-KOLLI-LINE (INDX)                              
063500                                      TO MOD-IDLEVNR-KOLLI (INDX)         
063600       END-IF                                                             
063700                                                                          
063800       IF RESP-IDOKOLLI-LINE (INDX) = ALL '+'                             
063900         MOVE MFS-ROER-EJ-FAELT       TO MOD-IDOKOLLI (INDX)              
064000       ELSE IF RESP-IDOKOLLI-LINE (INDX) = SPACE                          
064100         MOVE MFS-RENSA-FAELT         TO MOD-IDOKOLLI (INDX)              
064200       ELSE                                                               
064300         MOVE RESP-IDOKOLLI-LINE (INDX) TO MOD-IDOKOLLI (INDX)            
064400       END-IF                                                             
064500       END-IF                                                             
064600                                                                          
064700       IF RESP-STATUS-TEXT-LINE (INDX) = ALL '+'                          
064800         MOVE MFS-ROER-EJ-FAELT       TO MOD-STATUS-TEXT (INDX)           
064900       ELSE                                                               
065000         MOVE RESP-STATUS-TEXT-LINE (INDX)                                
065100                                      TO MOD-STATUS-TEXT (INDX)           
065200       END-IF                                                             
065300                                                                          
065400       ADD 1 TO INDX                                                      
065500     END-PERFORM                                                          
065600                                                                          
065700*    -- CLEAR AND LOCK ANY REMAINING LINES                                
065800     PERFORM UNTIL INDX  >  MAX-KVRADER                                   
065900       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
066000       MOVE MFS-STAENG-FAELT TO MOD-ADINLOMR-UPD-ATTR (INDX)              
066100       ADD 1 TO INDX                                                      
066200     END-PERFORM                                                          
066300                                                                          
066400     .                                                                    
066500                                                                          
066600     EJECT                                                                
066700 MFS-RENSA-FAELT-UT SECTION.                                              
066800                                                                          
066900*    --- ALLA UTDATA-FÄLT                                                 
067000     PERFORM MFS-RENSA-FAELT-UT-BLAD                                      
067100     PERFORM MFS-RENSA-FAELT-UT-HUV                                       
067200                                                                          
067300     MOVE +1 TO INDX                                                      
067400     PERFORM UNTIL INDX > MAX-KVRADER                                     
067500       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
067600       ADD +1 TO INDX                                                     
067700     END-PERFORM                                                          
067800     .                                                                    
067900     SKIP2                                                                
068000 MFS-RENSA-FAELT-UT-BLAD SECTION.                                         
068100                                                                          
068200*    --- BLÄDDRINGSNYCKLAR                                                
068300     MOVE MFS-RENSA-FAELT TO MOD-IDRADNR-ENTER                            
068400                             MOD-IDRADNR-NEXT                             
068500     .                                                                    
068600     SKIP2                                                                
068700 MFS-RENSA-FAELT-UT-HUV SECTION.                                          
068800                                                                          
068900*    --- UTDATA-FÄLT I BILD-HUVUD                                         
069000     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR                                  
069100                             MOD-KVAVIS                                   
069200                             MOD-BEART                                    
069300                             MOD-KDSORT                                   
069400                             MOD-BEFT                                     
069500                             MOD-KVRAPP                                   
069600                             MOD-BEFARLIG                                 
069700     .                                                                    
069800     SKIP2                                                                
069900 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
070000                                                                          
070100*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
070200     MOVE MFS-RENSA-FAELT TO MOD-IDRADNR  (INDX)                          
070300                             MOD-KVINLART (INDX)                          
070400                             MOD-ADINLOMR (INDX)                          
070500                             MOD-KDINLSTA (INDX)                          
070600                             MOD-IDLEVNR-KOLLI (INDX)                     
070700                             MOD-IDOKOLLI (INDX)                          
070800     .                                                                    
070900     SKIP2                                                                
071000 MFS-RENSA-FAELT-IN SECTION.                                              
071100                                                                          
071200*    --- ALLA INDATA-FÄLT                                                 
071300     MOVE +1 TO INDX                                                      
071400     PERFORM UNTIL INDX > MAX-KVRADER                                     
071500       MOVE MFS-RENSA-FAELT TO MOD-ADINLOMR-UPD (INDX)                    
071600       ADD +1 TO INDX                                                     
071700     END-PERFORM                                                          
071800     .                                                                    
071900     EJECT                                                                
072000     SKIP2                                                                
072100 MFS-FORM-ATTR SECTION.                                                   
072200                                                                          
072300*    --- ALLA INDATA-FÄLT                                                 
072400     MOVE +1 TO INDX                                                      
072500     PERFORM UNTIL INDX > MAX-KVRADER                                     
072600       MOVE MFS-FORMATETS-ATTR TO MOD-ADINLOMR-UPD-ATTR (INDX)            
072700       ADD +1 TO INDX                                                     
072800     END-PERFORM                                                          
072900     .                                                                    
073000     EJECT                                                                
073100* --- IMS SEKTIONER ---                                                   
073200     SKIP3                                                                
073300 IMS-GET-MSG SECTION.                                                     
073400                                                                          
073500     MOVE '  QC' TO GODK-STATUSKODER                                      
073600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
073700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
073800     PERFORM IMS-STATUSKONTROLL                                           
073900     .                                                                    
074000     SKIP3                                                                
074100 IMS-INSERT-MSG SECTION.                                                  
074200                                                                          
074300     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
074400     AND NOT ENGLISH-TEXT                                                 
074500       MOVE '0' TO MFS-KDHUVOMR                                           
074600     END-IF                                                               
074700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
074800     MOVE SPACE TO GODK-STATUSKODER                                       
074900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
075000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
075100     PERFORM IMS-STATUSKONTROLL                                           
075200     .                                                                    
075300     EJECT                                                                
075400 IMS-STATUSKONTROLL SECTION.                                              
075500                                                                          
075600     SET STATUS-IX TO 1                                                   
075700     SEARCH GODK-STATUS                                                   
075800       AT END                                                             
075900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
076000         DELIMITED BY SIZE INTO FELTEXT                                   
076100         CALL FELLOG                                                      
076200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
076300         CONTINUE                                                         
076400     END-SEARCH                                                           
076500     .                                                                    
