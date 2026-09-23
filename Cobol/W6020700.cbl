000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6020700.                                                
000300 AUTHOR.         TOMMIE JIVARP/RAHUL REDDY.                               
000400 DATE-WRITTEN.   98/01/26 / JUNE 2012.                                    
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET LÄSER OCH UPPDATERAR W6KVAE (W6H7)                    
000900*                                                                         
001000*        THIS PROGRAM HANDLES MFS PORTION OF SCREEN 6207                  
001100*        AND INVOKES W6020710 CONTAINING BUSINESS LOGIC.                  
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W6T207                                              
001500*        MID:         W6I20701                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         W6O20701                                            
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400 WORKING-STORAGE SECTION.                                                 
002500                                                                          
002600*    -- CHECKED BY WY2000                                                 
002700 77  IDPGM                       PIC X(08)   VALUE 'W6020700'.            
002800                                                                          
002900*    -- INDEX FÖR BILDENS FRITEXTRADER                                    
003000 77  INDX                        PIC S9(4)   VALUE +0                     
003100                                                   COMP SYNC.             
003200 77  MAX-INDX                    PIC S9(4)   VALUE +15                    
003300                                                   COMP SYNC.             
003400 77  MAX-KVRADER                 PIC S9(4)   COMP VALUE +15.              
003500*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003600 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003700                                                                          
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004000                                                                          
004100*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004200                                                                          
004300 77  WS-IDKR                     PIC X(5)    VALUE SPACE.                 
004400                                                                          
004500                                                                          
004600 77  LAES-VISA-INFO-SW           PIC X       VALUE 'J'.                   
004700     88  LAES-VISA-OK                        VALUE 'J'.                   
004800     88  LAES-VISA-EJ-OK                     VALUE 'N'.                   
004900                                                                          
005000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005100     88  INDATA-OK                           VALUE 'J'.                   
005200     88  INDATA-FEL                          VALUE 'N'.                   
005300                                                                          
005400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005500     88  NYCKLAR-OK                          VALUE 'J'.                   
005600     88  NYCKLAR-FEL                         VALUE 'N'.                   
005700                                                                          
005800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005900     88  EGEN-MID                            VALUE '6207'.                
006000     88  GODK-MID                            VALUE '6201' '6202'          
006100                                                   '6203' '6204'          
006200                                                   '6205' '6206'          
006300                                                   '6207' '6208'          
006400                                                   '6209'.                
006500     88  HELP-MID                            VALUE '0551'.                
006600     EJECT                                                                
006700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006800 01  GENERELLA-SUBPROGRAM.                                                
006900     03  WL01MCNV                PIC X(8)    VALUE 'WL01MCNV'.            
007000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007300     03  W6020710                PIC X(8)    VALUE 'W6020710'.            
007400     EJECT                                                                
007500 01  MESSAGE-CODES.                                                       
007600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007700     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007800     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007900     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008100     03  ERR-REPORT-MISSING      PIC X(3)    VALUE '210'.                 
008200     EJECT                                                                
008300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008400*                                                                         
008500 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008600     SKIP3                                                                
008700*01 -COPY WMSGINIT                                                        
008800     EJECT                                                                
008900*    ---  COPYTEXT FÖR TRANS TILL WL01MCNV                                
009000*01  -COPY WL01MCNV                                                       
009100     EJECT                                                                
009200 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
009300 01  REQU-AREA.                                                           
009400*    03 -COPY WZ01REQU                                                    
009500*    03 -COPY W60207I1                                                    
009600     EJECT                                                                
009700 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
009800 01  RESP-AREA.                                                           
009900*    03 -COPY WZ01RESP                                                    
010000*    03 -COPY W60207O1                                                    
010100     EJECT                                                                
010200                                                                          
010300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
010400*                                                                         
010500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010600     SKIP3                                                                
010700*01  MID -COPY W6I20701                                                   
010800     EJECT                                                                
010900 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
011000     SKIP3                                                                
011100*01  -COPY WMSGAREA                                                       
011200     EJECT                                                                
011300     03  MOD REDEFINES MSG-AREA.                                          
011400*      05  -COPY W6O20701                                                 
011500     EJECT                                                                
011600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011700     SKIP3                                                                
011800*01  -COPY WMFSAREA                                                       
011900     EJECT                                                                
012000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012100*                                                                         
012200     EJECT                                                                
012300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012400     SKIP3                                                                
012500*    --- STATUS-KOD FRÅN IMS                                              
012600 01  STATUS-WS                   PIC XX.                                  
012700     88  SEGMENT-FINNS                       VALUE '  '.                  
012800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013000     SKIP2                                                                
013100 01  GODK-STATUSKODER.                                                    
013200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013300     SKIP3                                                                
013400 01  SSA1                        PIC X(64).                               
013500 01  SSA2                        PIC X(64).                               
013600     EJECT                                                                
013700*    --- IMS FUNKTIONSKODER                                               
013800*01  -COPY W0003                                                          
013900     EJECT                                                                
014000 LINKAGE SECTION.                                                         
014100*01  -COPY W0009   -PRE MSG-                                              
014200 01  USEA-PCB                    PIC X.                                   
014300 01  KVAE-PCB                    PIC X.                                   
014400     EJECT                                                                
014500 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB KVAE-PCB.                     
014600 MAIN SECTION.                                                            
014700     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB KVAE-PCB.                     
014800                                                                          
014900     PERFORM IMS-GET-MSG                                                  
015000     IF SEGMENT-FINNS                                                     
015100       PERFORM A-INIT                                                     
015200       PERFORM B-INIT-KEYS                                                
015300       PERFORM C-INIT-REQU                                                
015400       IF MFS-UPDATE                                                      
015500         SET REQU-UPDATE         TO TRUE                                  
015600         PERFORM E-SAMMA-SIDA                                             
015700       ELSE                                                               
015800         IF MFS-FIRST                                                     
015900           SET REQU-FIRST        TO TRUE                                  
016000           PERFORM MFS-RENSA-FAELT-IN                                     
016100           MOVE MFS-RENSA-FAELT  TO MOD-FLAGGA-DEL                        
016200         ELSE                                                             
016300           SET REQU-QUERY        TO TRUE                                  
016400           PERFORM E-SAMMA-SIDA                                           
016500         END-IF                                                           
016600       END-IF                                                             
016700       PERFORM F-CALL-BIZ-LOGIC-W6020710                                  
016800       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O20701 + 4                      
016900       PERFORM IMS-INSERT-MSG                                             
017000     END-IF                                                               
017100                                                                          
017200     MOVE ZERO                   TO RETURN-CODE                           
017300     GOBACK                                                               
017400     .                                                                    
017500     EJECT                                                                
017600                                                                          
017700 A-INIT SECTION.                                                          
017800                                                                          
017900     IF MSG-DUBBLA-TRANSKODER                                             
018000       MOVE MSG-INDATA-MINUS-2-TRANSKODER                                 
018100                                 TO MID-W6I20701                          
018200       MOVE MSG-IDTRANS-2        TO MFS-IDTRANS                           
018300       MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                          
018400     ELSE                                                                 
018500       MOVE MSG-INDATA-MINUS-1-TRANSKOD                                   
018600                                 TO MID-W6I20701                          
018700       MOVE MSG-IDTRANS-1        TO MFS-IDTRANS                           
018800       MOVE MSG-KDMFSFOR-1       TO MFS-KDMFSFOR                          
018900     END-IF                                                               
019000                                                                          
019100     MOVE MSG-KDTRTYP            TO MFS-KDTRTYP                           
019200     MOVE MSG-IDPFK              TO MFS-IDPFK                             
019300     MOVE MFS-IDTRANS            TO W-IDTRANS                             
019400                                                                          
019500     MOVE LOW-VALUE              TO MSG-AREA                              
019600     MOVE 'W6O207N1'             TO MFS-IDMOD                             
019700     MOVE '6207'                 TO MOD-IDTRANS                           
019800     MOVE MFS-RENSA-FAELT        TO MOD-TEMFSFEL MOD-TEMFSINF             
019900                                                                          
020000     IF EGEN-MID OR HELP-MID                                              
020100       CONTINUE                                                           
020200     ELSE                                                                 
020300       MOVE SPACE                TO MFS-KDTRTYP                           
020400       MOVE '7'                  TO MFS-IDPFK                             
020500     END-IF                                                               
020600     .                                                                    
020700     EJECT                                                                
020800                                                                          
020900 B-INIT-KEYS SECTION.                                                     
021000                                                                          
021100     MOVE ALL '+'                TO MSGI-WMSGINIT                         
021200     MOVE '001'                  TO MSGI-KDCALL                           
021300     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
021400     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
021500     MOVE '6207'                 TO MSGI-IDTRANS                          
021600     CALL W005INIT            USING MSGI-WMSGINIT USEA-PCB                
021700                                                                          
021800     MOVE MSGI-IDSPRAK           TO MCNV-IDSPRAK                          
021900                                                                          
022000     PERFORM MFS-FORM-ATTR                                                
022100     MOVE '101'                  TO REQU-IDMSGVER                         
022200                                                                          
022300     MOVE MFS-RENSA-FAELT        TO MOD-IDKR-IN                           
022400                                                                          
022500     IF MID-IDKR-IN = ALL '+'                                             
022600       MOVE MID-IDKR-UT          TO WS-IDKR                               
022700       INSPECT WS-IDKR REPLACING LEADING SPACE BY ZERO                    
022800     ELSE                                                                 
022900       MOVE MID-IDKR-IN          TO WS-IDKR                               
023000       MOVE '7'                  TO MFS-IDPFK                             
023100       MOVE SPACE                TO MFS-KDTRTYP                           
023200     END-IF                                                               
023300                                                                          
023400     MOVE WS-IDKR                TO REQU-IDKR-KEY                         
023500                                                                          
023600     IF GODK-MID                                                          
023700       MOVE WS-IDKR              TO MOD-IDKR-UT                           
023800       INSPECT MOD-IDKR-UT REPLACING LEADING ZERO BY SPACE                
023900     ELSE                                                                 
024000       MOVE MFS-RENSA-FAELT      TO MOD-IDKR-UT                           
024100     END-IF                                                               
024200     .                                                                    
024300     EJECT                                                                
024400                                                                          
024500 C-INIT-REQU SECTION.                                                     
024600                                                                          
024700     MOVE MID-FLAGGA-DEL         TO REQU-FLAGGA-DEL-UPD                   
024800     PERFORM                                                              
024900     VARYING INDX FROM +1 BY +1                                           
025000       UNTIL INDX > MAX-KVRADER                                           
025100       MOVE MID-TEKRFEL (INDX)   TO REQU-TEKRFEL-LINE (INDX)              
025200     END-PERFORM                                                          
025300     .                                                                    
025400     EJECT                                                                
025500                                                                          
025600 E-SAMMA-SIDA SECTION.                                                    
025700                                                                          
025800     IF EGEN-MID OR HELP-MID                                              
025900       CONTINUE                                                           
026000     ELSE                                                                 
026100       PERFORM MFS-RENSA-FAELT-IN                                         
026200     END-IF                                                               
026300     .                                                                    
026400     EJECT                                                                
026500                                                                          
026600 F-CALL-BIZ-LOGIC-W6020710 SECTION.                                       
026700                                                                          
026800     CALL W6020710            USING REQU-AREA RESP-AREA                   
026900                                    MAX-KVRADER KVAE-PCB                  
027000                                                                          
027100     IF RESP-IDMSG-ERROR NOT = SPACE OR                                   
027200        RESP-IDMSG-INFO  NOT = SPACE                                      
027300       PERFORM FA-SET-MSG-AND-HILIGHT                                     
027400     END-IF                                                               
027500     PERFORM FB-MOVE-RESP-TO-MOD                                          
027600     .                                                                    
027700     EJECT                                                                
027800 FA-SET-MSG-AND-HILIGHT SECTION.                                          
027900                                                                          
028000     MOVE RESP-IDMSG-ERROR       TO MCNV-IDMSG-ERROR                      
028100     MOVE RESP-IDMSG-INFO        TO MCNV-IDMSG-INFO                       
028200     MOVE RESP-IDELMT-ERROR      TO MCNV-IDELMT-ERROR                     
028300                                                                          
028400     CALL WL01MCNV            USING MCNV-AREA                             
028500     MOVE MCNV-MFSFEL            TO MOD-TEMFSFEL                          
028600     MOVE MCNV-MFSINF            TO MOD-TEMFSINF                          
028700     .                                                                    
028800     EJECT                                                                
028900                                                                          
029000 FB-MOVE-RESP-TO-MOD SECTION.                                             
029100                                                                          
029200     MOVE RESP-FLAGGA-DEL-UPD-ATTR                                        
029300                                 TO MOD-FLAGGA-DEL-ATTR                   
029400     IF RESP-FLAGGA-DEL-UPD = SPACE                                       
029500       MOVE MFS-RENSA-FAELT      TO MOD-FLAGGA-DEL                        
029600     ELSE                                                                 
029700       IF RESP-FLAGGA-DEL-UPD = ALL '+'                                   
029800         MOVE MFS-ROER-EJ-FAELT  TO MOD-FLAGGA-DEL                        
029900       ELSE                                                               
030000         MOVE RESP-FLAGGA-DEL-UPD                                         
030100                                 TO MOD-FLAGGA-DEL                        
030200       END-IF                                                             
030300     END-IF                                                               
030400     PERFORM                                                              
030500     VARYING INDX FROM +1 BY +1                                           
030600       UNTIL INDX > MAX-KVRADER                                           
030700       MOVE RESP-TEKRFEL-LINE-ATTR (INDX)                                 
030800                                 TO MOD-TEKRFEL-ATTR (INDX)               
030900       IF RESP-TEKRFEL-LINE (INDX) = SPACE                                
031000         MOVE MFS-RENSA-FAELT    TO MOD-TEKRFEL (INDX)                    
031100       ELSE                                                               
031200         IF RESP-TEKRFEL-LINE (INDX) = ALL '+'                            
031300           MOVE MFS-ROER-EJ-FAELT                                         
031400                                 TO MOD-TEKRFEL (INDX)                    
031500         ELSE                                                             
031600           MOVE RESP-TEKRFEL-LINE (INDX)                                  
031700                                 TO MOD-TEKRFEL (INDX)                    
031800         END-IF                                                           
031900       END-IF                                                             
032000     END-PERFORM                                                          
032100     PERFORM                                                              
032200     VARYING INDX FROM INDX BY +1                                         
032300       UNTIL INDX > MAX-INDX                                              
032400       MOVE MFS-RENSA-FAELT      TO                                       
032500                                    MOD-TEKRFEL (INDX)                    
032600     END-PERFORM                                                          
032700     .                                                                    
032800     EJECT                                                                
032900                                                                          
033000 MFS-RENSA-FAELT-IN SECTION.                                              
033100                                                                          
033200     MOVE MFS-RENSA-FAELT        TO MOD-IDKR-IN                           
033300     MOVE +1                     TO INDX                                  
033400     PERFORM                                                              
033500       UNTIL INDX > MAX-INDX                                              
033600       MOVE MFS-RENSA-FAELT      TO MOD-TEKRFEL(INDX)                     
033700       ADD +1                    TO INDX                                  
033800     END-PERFORM                                                          
033900     .                                                                    
034000     EJECT                                                                
034100                                                                          
034200 MFS-FORM-ATTR SECTION.                                                   
034300                                                                          
034400*    --- ALLA INDATA-FÄLT                                                 
034500     MOVE MFS-FORMATETS-ATTR     TO MOD-FLAGGA-DEL-ATTR                   
034600     MOVE +1                     TO INDX                                  
034700     PERFORM                                                              
034800       UNTIL INDX > MAX-INDX                                              
034900       MOVE MFS-FORMATETS-ATTR   TO MOD-TEKRFEL-ATTR (INDX)               
035000       ADD +1                    TO INDX                                  
035100     END-PERFORM                                                          
035200     .                                                                    
035300     SKIP2                                                                
035400                                                                          
035500* --- IMS SEKTIONER ---                                                   
035600     SKIP3                                                                
035700                                                                          
035800 IMS-GET-MSG SECTION.                                                     
035900                                                                          
036000     MOVE '  QC'                 TO GODK-STATUSKODER                      
036100     CALL CBLTDLI             USING GU MSG-PCB MSG-IO-AREA                
036200     MOVE MSG-STATUS-CODE        TO STATUS-WS                             
036300     PERFORM IMS-STATUSKONTROLL                                           
036400     .                                                                    
036500     SKIP3                                                                
036600                                                                          
036700 IMS-INSERT-MSG SECTION.                                                  
036800                                                                          
036900     MOVE LOW-VALUE              TO MSG-KDZ1 MSG-KDZ2                     
037000     MOVE SPACE                  TO GODK-STATUSKODER                      
037100     CALL CBLTDLI             USING ISRT MSG-PCB                          
037200                                    MSG-IO-AREA MFS-IDMOD                 
037300     MOVE MSG-STATUS-CODE        TO STATUS-WS                             
037400     PERFORM IMS-STATUSKONTROLL                                           
037500     .                                                                    
037600     EJECT                                                                
037700                                                                          
037800 IMS-STATUSKONTROLL SECTION.                                              
037900                                                                          
038000     SET STATUS-IX               TO 1                                     
038100     SEARCH GODK-STATUS                                                   
038200       AT END                                                             
038300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
038400           DELIMITED BY SIZE INTO FELTEXT                                 
038500         CALL FELLOG                                                      
038600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
038700         CONTINUE                                                         
038800     END-SEARCH                                                           
038900     .                                                                    
