000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4034700.                                                
000300 AUTHOR.         ABRAHAMSON SHARON.                                       
000400 DATE-WRITTEN.   96/11/15.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THIS PROGRAM ALLOWS THE USER TO CHANGE THE TRANSPORT             
000900*        NUMBER FOR ALL OR CHOSEN CASES WITHIN AN ORDER.                  
001000*                                                                         
001100*        THE PROGRAM READS     WDE4A                                      
001200*        THE PROGRAM READS     WDE6                                       
001300*        THE PROGRAM UPDATES   WDE6                                       
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W4T347                                              
001700*        MID:         W4I34701                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W4O34701                                            
002100*    SKIP3                                                                
002200*                                                                         
002300* CHANGE LOG:                                                             
002400*                                                                         
002500*    OCT,2021 - LOVISH KANSAL   - MOVED THE BUSINESS LOGIC TO             
002600*                                 NEW PROGRAM W4034710 AND                
002700*                                 CREATED W4W34700 FOR WEB                
002800*                                                                         
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP3                                                                
003100 DATA DIVISION.                                                           
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400*    -- CHECKED BY WY2000                                                 
003500 77  IDPGM                       PIC X(08)   VALUE 'W4034700'.            
003600                                                                          
003700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003800 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003900                                                                          
004000 77  YES                         PIC X       VALUE 'Y'.                   
004100 77  NOO                         PIC X       VALUE 'N'.                   
004200                                                                          
004300*    --- INDEX FOR SCROLL LINES                                           
004400 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004500 77  MAX-KVRADER                 PIC S9(4)  VALUE +12   COMP SYNC.        
004600*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004700*                                                                         
004800 01  W-MSGI-IDKUNDRF.                                                     
004900     03  FILLER                  PIC X(2)    VALUE '00'.                  
005000     03  W-MSGI-IDKUNDRF-5       PIC X(5).                                
005100     03  FILLER                  PIC X(3)    VALUE SPACE.                 
005200*                                                                         
005300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005400     88  OWN-MID                             VALUE '4347'.                
005500     88  GOOD-MID                            VALUE '4341' '4342'          
005600                                                   '4343' '4344'          
005700                                                   '4345' '4346'          
005800                                                   '4347' '4348'          
005900                                                   '4349'.                
006000     88  HELP-MID                            VALUE '0551'.                
006100     EJECT                                                                
006200*                                                                         
006300 01  ALL-SPACE.                                                           
006400     03  FILLER                  PIC X(80)  VALUE SPACE.                  
006500 01  ALL-PLUS.                                                            
006600     03  FILLER                  PIC X(80)  VALUE ALL '+'.                
006700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006800 01  GENERAL-SUBPROGRAM.                                                  
006900     03  W4034710                PIC X(8)    VALUE 'W4034710'.            
007000     03  WL01MCNV                PIC X(8)    VALUE 'WL01MCNV'.            
007100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007400     EJECT                                                                
007500*    --- PARAMETERS FOR SUB PROGRAMS                                      
007600 01  FILLER                      PIC X(16)  VALUE 'WMSGINIT'.             
007700     SKIP3                                                                
007800*01 -COPY WMSGINIT                                                        
007900     EJECT                                                                
008000 01  FILLER                      PIC X(16)  VALUE 'WL01MCNV '.            
008100*01 -COPY WL01MCNV                                                        
008200     SKIP2                                                                
008300*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
008400 01  FILLER                     PIC X(16)   VALUE 'SAVE-AREA'.            
008500 01  SAVE-AREA.                                                           
008600     03  SAVE-IDTRANS           PIC X(8)    VALUE '4347'.                 
008700     03  SAVE-IDPRODNR-START    PIC S9(7)         COMP-3.                 
008800     03  SAVE-IDKOLLI-ENTER     PIC S9(5)         COMP-3.                 
008900     03  SAVE-IDKOLLI-NEXT      PIC S9(5)         COMP-3.                 
009000     EJECT                                                                
009100 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
009200 01  REQU-AREA.                                                           
009300*    03 -COPY WZ01REQU                                                    
009400*    03 -COPY W40347I1                                                    
009500     EJECT                                                                
009600 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
009700 01  RESP-AREA.                                                           
009800*    03 -COPY WZ01RESP                                                    
009900*    03 -COPY W40347O1                                                    
010000     EJECT                                                                
010100*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
010200*                                                                         
010300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010400     SKIP3                                                                
010500*01  MID -COPY W4I34701                                                   
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010800     SKIP3                                                                
010900*01  -COPY WMSGAREA                                                       
011000     EJECT                                                                
011100     03  MOD REDEFINES MSG-AREA.                                          
011200*      05  -COPY W4O34701                                                 
011300     EJECT                                                                
011400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011500     SKIP3                                                                
011600*01  -COPY WMFSAREA                                                       
011700     EJECT                                                                
011800*    --- STATUS-KOD FRÅN IMS                                              
011900 01  STATUS-WS                   PIC XX.                                  
012000     88  SEGMENT-FOUND                       VALUE '  '.                  
012100     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
012200     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012300     SKIP2                                                                
012400 01  GOOD-STATUSCODES.                                                    
012500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012600     SKIP3                                                                
012700 01  SSA1                        PIC X(96).                               
012800 01  SSA2                        PIC X(96).                               
012900     EJECT                                                                
013000*    --- IMS FUNCTION CODES                                               
013100*01  -COPY W0003                                                          
013200     EJECT                                                                
013300                                                                          
013400 LINKAGE SECTION.                                                         
013500     SKIP3                                                                
013600*01  -COPY W0009     -PRE MSG-                                            
013700     EJECT                                                                
013800*01  -COPY W0008     -PRE USEA-                                           
013900     05  FILLER                  PIC X.                                   
014000     EJECT                                                                
014100 01  WDE4A-PCB       PIC X.                                               
014200 01  WDE43-PCB       PIC X.                                               
014300 01  WDE6-PCB        PIC X.                                               
014310 01  XXDM-PCB        PIC X.                                               
014400                                                                          
014500 PROCEDURE DIVISION USING MSG-PCB USEA-PCB WDE4A-PCB WDE43-PCB            
014600                                           WDE6-PCB XXDM-PCB.             
014700 MAIN SECTION.                                                            
014800     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDE4A-PCB WDE43-PCB           
014900                                            WDE6-PCB XXDM-PCB.            
015000                                                                          
015100     PERFORM IMS-GET-MSG                                                  
015200     IF SEGMENT-FOUND                                                     
015300       PERFORM A-INIT                                                     
015400       PERFORM B-INIT-KEYS                                                
015500       PERFORM C-INIT-REQU                                                
015600       IF MFS-UPDATE                                                      
015700         SET REQU-UPDATE      TO TRUE                                     
015800         PERFORM E-SAME-SIDE                                              
015900       ELSE                                                               
016000         IF MFS-FIRST                                                     
016100           SET REQU-FIRST     TO TRUE                                     
016200           PERFORM MFS-ERASE-FIELD-IN                                     
016300         ELSE                                                             
016400           IF MFS-NEXT                                                    
016500             SET REQU-NEXT TO TRUE                                        
016600             PERFORM D-NEXT-SIDE                                          
016700           ELSE                                                           
016800             SET REQU-QUERY TO TRUE                                       
016900             PERFORM E-SAME-SIDE                                          
017000           END-IF                                                         
017100         END-IF                                                           
017200       END-IF                                                             
017300                                                                          
017400       PERFORM F-CALL-BIZ-LOGIC-W4034710                                  
017500       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O34701 + 4                      
017600       PERFORM IMS-INSERT-MSG                                             
017700     END-IF                                                               
017800     MOVE ZERO TO RETURN-CODE                                             
017900     GOBACK                                                               
018000     .                                                                    
018100     EJECT                                                                
018200 A-INIT SECTION.                                                          
018300                                                                          
018400     IF MSG-DUBBLA-TRANSKODER                                             
018500       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W4I34701                 
018600       MOVE MSG-IDTRANS-2      TO MFS-IDTRANS                             
018700       MOVE MSG-KDMFSFOR-2     TO MFS-KDMFSFOR                            
018800     ELSE                                                                 
018900       MOVE MSG-INDATA-MINUS-1-TRANSACT   TO MID-W4I34701                 
019000       MOVE MSG-IDTRANS-1      TO MFS-IDTRANS                             
019100       MOVE MSG-KDMFSFOR-1     TO MFS-KDMFSFOR                            
019200     END-IF                                                               
019300                                                                          
019400     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
019500     MOVE MSG-IDPFK            TO MFS-IDPFK                               
019600     MOVE MFS-IDTRANS          TO W-IDTRANS                               
019700                                                                          
019800     MOVE LOW-VALUE            TO MSG-AREA                                
019900     MOVE 'W4O347N1'           TO MFS-IDMOD                               
020000     MOVE '4347'               TO MOD-IDTRANS                             
020100     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL                            
020200                                  MOD-TEMFSINF                            
020300                                                                          
020400     IF OWN-MID OR HELP-MID                                               
020500        CONTINUE                                                          
020600     ELSE                                                                 
020700        MOVE SPACE             TO MFS-KDTRTYP                             
020800        MOVE '7'               TO MFS-IDPFK                               
020900        PERFORM AA-MOVE-PLUS-TO-INFIELDS                                  
021000     END-IF                                                               
021100     .                                                                    
021200     EJECT                                                                
021300 AA-MOVE-PLUS-TO-INFIELDS SECTION.                                        
021400                                                                          
021500     MOVE  +1        TO INDX                                              
021600     MOVE  '+'       TO MID-FLTRPTCHG                                     
021700     MOVE  '+++'     TO MID-IDTRPTNR                                      
021800     MOVE  '++'      TO MID-KDFRAKT                                       
021900     PERFORM UNTIL INDX > MAX-KVRADER                                     
022000       MOVE '+'      TO MID-VALFLAGGA   (INDX)                            
022100       MOVE '+++'    TO MID-IDTRPTNR-IN (INDX)                            
022200       MOVE '++'     TO MID-KDFRAKT-IN (INDX)                             
022300       ADD +1        TO INDX                                              
022400     END-PERFORM                                                          
022500     .                                                                    
022600     EJECT                                                                
022700                                                                          
022800 B-INIT-KEYS SECTION.                                                     
022900                                                                          
023000     MOVE ALL '+'           TO MSGI-WMSGINIT                              
023100     MOVE '001'             TO MSGI-KDCALL                                
023200     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
023300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
023400     MOVE '4347'            TO MSGI-IDTRANS                               
023500                                                                          
023600     IF OWN-MID                                                           
023700        MOVE MID-IDDISTR-IN   TO MSGI-IDDISTR                             
023800        MOVE MID-IDKUNDNR-IN  TO MSGI-IDKUNDNR                            
023900        IF MID-IDORDNR-IN = ALL '+'                                       
024000          MOVE '++++++++++' TO MSGI-IDKUNDRF                              
024100        ELSE                                                              
024200          MOVE MID-IDORDNR-IN   TO W-MSGI-IDKUNDRF-5                      
024300          MOVE W-MSGI-IDKUNDRF  TO MSGI-IDKUNDRF                          
024400        END-IF                                                            
024500        MOVE MID-IDPRODNR-IN  TO MSGI-IDPRODNR                            
024600     END-IF                                                               
024700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
024800     MOVE MSGI-SPAR-AREA    TO SAVE-AREA                                  
024900     MOVE MSGI-IDSPRAK      TO MCNV-IDSPRAK                               
025000     MOVE MSGI-IDDC         TO MOD-IDDC-UT                                
025100     MOVE '101'             TO REQU-IDMSGVER                              
025200     MOVE MSGI-IDSPRAK      TO MCNV-IDSPRAK                               
025300*****                                                                     
025400     MOVE MFS-ERASE-FIELD   TO MOD-IDDISTR-IN                             
025500                               MOD-IDKUNDNR-IN                            
025600                               MOD-IDORDNR-IN                             
025700                               MOD-IDPRODNR-IN                            
025800                                                                          
025900     IF MID-IDDISTR-IN NOT = ALL '+'                                      
026000        MOVE '7'       TO MFS-IDPFK                                       
026100        MOVE SPACE     TO MFS-KDTRTYP                                     
026200     END-IF                                                               
026300                                                                          
026400     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
026500        MOVE '7'       TO MFS-IDPFK                                       
026600        MOVE SPACE     TO MFS-KDTRTYP                                     
026700     END-IF                                                               
026800                                                                          
026900     IF MID-IDORDNR-IN NOT = ALL '+'                                      
027000        MOVE '7'       TO MFS-IDPFK                                       
027100        MOVE SPACE     TO MFS-KDTRTYP                                     
027200     END-IF                                                               
027300*                                                                         
027400     IF MID-IDPRODNR-IN NOT = ALL '+'                                     
027500        MOVE '7'       TO MFS-IDPFK                                       
027600        MOVE SPACE     TO MFS-KDTRTYP                                     
027700     END-IF                                                               
027800*                                                                         
027900     MOVE MSGI-IDDISTR        TO REQU-IDDISTR-KEY                         
028000     MOVE MSGI-IDKUNDNR       TO REQU-IDKUNDNR-KEY                        
028100     MOVE MSGI-IDORDNR7 (3:5) TO REQU-IDORDNR-KEY                         
028200     MOVE MSGI-IDDC           TO REQU-IDDC-KEY                            
028300                                                                          
028400     IF MID-IDDISTR-IN = ALL '+'                                          
028500        AND MID-IDKUNDNR-IN  = ALL '+'                                    
028600        AND MID-IDORDNR-IN  = ALL '+'                                     
028700        MOVE MSGI-IDPRODNR    TO REQU-IDPRODNR-KEY                        
028800     END-IF                                                               
028900                                                                          
029000     IF GOOD-MID                                                          
029100        MOVE MSGI-IDDISTR        TO MOD-IDDISTR-UT                        
029200        MOVE MSGI-IDKUNDNR       TO MOD-IDKUNDNR-UT                       
029300        MOVE MSGI-IDORDNR7 (3:5) TO MOD-IDORDNR-UT                        
029400        MOVE MSGI-IDPRODNR       TO MOD-IDPRODNR-UT                       
029500        MOVE MSGI-IDDC           TO MOD-IDDC-UT                           
029600        INSPECT MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE           
029700        INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE           
029800        INSPECT MOD-IDORDNR-UT  REPLACING LEADING ZERO BY SPACE           
029900        INSPECT MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE           
030000     ELSE                                                                 
030100        MOVE MFS-ERASE-FIELD     TO MOD-IDDISTR-UT                        
030200        MOVE MFS-ERASE-FIELD     TO MOD-IDKUNDNR-UT                       
030300        MOVE MFS-ERASE-FIELD     TO MOD-IDORDNR-UT                        
030400        MOVE MFS-ERASE-FIELD     TO MOD-IDPRODNR-UT                       
030500        MOVE MFS-ERASE-FIELD     TO MOD-IDDC-UT                           
030600     END-IF                                                               
030700     .                                                                    
030800     EJECT                                                                
030900 C-INIT-REQU SECTION.                                                     
031000                                                                          
031100     MOVE MAX-KVRADER            TO REQU-KVRADER                          
031200     IF MID-FLTRPTCHG = ALL '+'                                           
031300       MOVE ALL-PLUS             TO REQU-FLTRPTCHG                        
031400     ELSE                                                                 
031500       MOVE MID-FLTRPTCHG        TO REQU-FLTRPTCHG                        
031600     END-IF                                                               
031700                                                                          
031800     IF MID-IDTRPTNR = ALL '+'                                            
031900       MOVE ALL-PLUS             TO REQU-IDTRPTNR                         
032000     ELSE                                                                 
032100       MOVE MID-IDTRPTNR         TO REQU-IDTRPTNR                         
032200     END-IF                                                               
032300                                                                          
032400     IF MID-KDFRAKT = ALL '+'                                             
032500       MOVE ALL-PLUS             TO REQU-KDFRAKT                          
032600     ELSE                                                                 
032700       MOVE MID-KDFRAKT          TO REQU-KDFRAKT                          
032800     END-IF                                                               
032900                                                                          
033000     MOVE +1 TO INDX                                                      
033100     PERFORM UNTIL INDX > MAX-KVRADER                                     
033200       MOVE MID-VALFLAGGA(INDX)   TO REQU-VALFLAGGA-LINE (INDX)           
033300       MOVE MID-IDKOLLI-IN(INDX)  TO REQU-IDKOLLI-LINE (INDX)             
033400       MOVE MID-IDTRPTNR-IN(INDX) TO REQU-IDTRPTNR-LINE (INDX)            
033500       MOVE MID-KDFRAKT-IN (INDX) TO REQU-KDFRAKT-LINE (INDX)             
033600       ADD  +1 TO INDX                                                    
033700     END-PERFORM                                                          
033800                                                                          
033900     .                                                                    
034000     EJECT                                                                
034100 D-NEXT-SIDE SECTION.                                                     
034200                                                                          
034300     IF SAVE-IDTRANS = '4347'                                             
034400       MOVE SAVE-IDPRODNR-START TO REQU-IDPRODNR-START                    
034500       MOVE SAVE-IDKOLLI-NEXT   TO REQU-IDKOLLI-START                     
034600       PERFORM MFS-ERASE-FIELD-IN                                         
034700     ELSE                                                                 
034800       PERFORM MFS-ERASE-FIELD-IN                                         
034900     END-IF                                                               
035000     .                                                                    
035100     EJECT                                                                
035200 E-SAME-SIDE SECTION.                                                     
035300                                                                          
035400     IF SAVE-IDTRANS = '4347' OR '0551'                                   
035500       MOVE SAVE-IDPRODNR-START TO REQU-IDPRODNR-START                    
035600       MOVE SAVE-IDKOLLI-ENTER  TO REQU-IDKOLLI-START                     
035700     ELSE                                                                 
035800       PERFORM MFS-ERASE-FIELD-IN                                         
035900     END-IF                                                               
036000     .                                                                    
036100     EJECT                                                                
036200 F-CALL-BIZ-LOGIC-W4034710 SECTION.                                       
036300                                                                          
036400     CALL W4034710 USING REQU-AREA RESP-AREA MAX-KVRADER                  
036500                         WDE4A-PCB WDE43-PCB WDE6-PCB XXDM-PCB.           
036600                                                                          
036700     IF RESP-IDMSG-ERROR NOT = SPACE OR                                   
036800        RESP-IDMSG-INFO  NOT = SPACE                                      
036900       PERFORM FA-SET-MSG-AND-HILIGHT                                     
037000     END-IF                                                               
037100     PERFORM FB-MOVE-RESP-TO-MOD                                          
037200       MOVE RESP-IDPRODNR-START TO MSGI-IDPRODNR                          
037300       MOVE '001'             TO MSGI-KDCALL                              
037400       MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                              
037500       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
037600*SAVE SCROLL KEYS                                                         
037700     MOVE RESP-IDPRODNR-START  TO SAVE-IDPRODNR-START                     
037800     MOVE RESP-IDKOLLI-START   TO SAVE-IDKOLLI-ENTER                      
037900     MOVE RESP-IDKOLLI-NEXT    TO SAVE-IDKOLLI-NEXT                       
038000     MOVE '002'         TO MSGI-KDCALL                                    
038100     MOVE '4347'        TO MSGI-IDTRANS                                   
038200     MOVE '4347'        TO SAVE-IDTRANS                                   
038300     MOVE SAVE-AREA     TO MSGI-SPAR-AREA                                 
038400     CALL W005INIT   USING MSGI-WMSGINIT USEA-PCB                         
038500     .                                                                    
038600     EJECT                                                                
038700 FA-SET-MSG-AND-HILIGHT SECTION.                                          
038800                                                                          
038900     MOVE RESP-IDMSG-ERROR       TO MCNV-IDMSG-ERROR                      
039000     MOVE RESP-IDMSG-INFO        TO MCNV-IDMSG-INFO                       
039100     MOVE RESP-IDELMT-ERROR      TO MCNV-IDELMT-ERROR                     
039200                                                                          
039300     CALL WL01MCNV            USING MCNV-AREA                             
039400     MOVE MCNV-MFSINF            TO MOD-TEMFSINF                          
039500     MOVE MCNV-MFSFEL            TO MOD-TEMFSFEL                          
039600     .                                                                    
039700     EJECT                                                                
039800 FB-MOVE-RESP-TO-MOD SECTION.                                             
039900                                                                          
040000     MOVE RESP-FLTRPTCHG-ATTRIB  TO MOD-FLTRPTCHG-ATTRIB                  
040100     IF RESP-FLTRPTCHG = SPACE                                            
040200       MOVE MFS-RENSA-FAELT      TO MOD-FLTRPTCHG                         
040300     ELSE                                                                 
040400       IF RESP-FLTRPTCHG = ALL '+'                                        
040500         MOVE MFS-ROER-EJ-FAELT                                           
040600                                 TO MOD-FLTRPTCHG                         
040700       ELSE                                                               
040800         MOVE RESP-FLTRPTCHG                                              
040900                                 TO MOD-FLTRPTCHG                         
041000       END-IF                                                             
041100     END-IF                                                               
041200                                                                          
041300     MOVE RESP-IDTRPTNR-ATTRIB   TO MOD-IDTRPTNR-ATTRIB                   
041400     IF RESP-IDTRPTNR = SPACE                                             
041500       MOVE MFS-RENSA-FAELT      TO MOD-IDTRPTNR                          
041600     ELSE                                                                 
041700       IF RESP-IDTRPTNR = ALL '+'                                         
041800         MOVE MFS-ROER-EJ-FAELT                                           
041900                                 TO MOD-IDTRPTNR                          
042000       ELSE                                                               
042100         MOVE RESP-IDTRPTNR                                               
042200                                 TO MOD-IDTRPTNR                          
042300       END-IF                                                             
042400     END-IF                                                               
042500                                                                          
042600     MOVE RESP-KDFRAKT-ATTRIB    TO MOD-KDFRAKT-ATTRIB                    
042700     IF RESP-KDFRAKT = SPACE                                              
042800       MOVE MFS-RENSA-FAELT      TO MOD-KDFRAKT                           
042900     ELSE                                                                 
043000       IF RESP-KDFRAKT  = ALL '+'                                         
043100         MOVE MFS-ROER-EJ-FAELT                                           
043200                                 TO MOD-KDFRAKT                           
043300       ELSE                                                               
043400         MOVE RESP-KDFRAKT                                                
043500                                 TO MOD-KDFRAKT                           
043600       END-IF                                                             
043700     END-IF                                                               
043800                                                                          
043900     MOVE RESP-IDDISTR-UT      TO MOD-IDDISTR-UT                          
044000     MOVE RESP-IDKUNDNR-UT     TO MOD-IDKUNDNR-UT                         
044100     MOVE RESP-IDORDNR-UT      TO MOD-IDORDNR-UT                          
044200     MOVE RESP-IDPRODNR-UT     TO MOD-IDPRODNR-UT                         
044300                                                                          
044400     INSPECT MOD-IDDISTR-UT    REPLACING LEADING ZERO BY SPACE            
044500     INSPECT MOD-IDKUNDNR-UT   REPLACING LEADING ZERO BY SPACE            
044600     INSPECT MOD-IDORDNR-UT    REPLACING LEADING ZERO BY SPACE            
044700     INSPECT MOD-IDPRODNR-UT   REPLACING LEADING ZERO BY SPACE            
044800                                                                          
044900     MOVE +1       TO INDX                                                
045000                                                                          
045100     PERFORM UNTIL INDX > RESP-KVRADER                                    
045200       MOVE RESP-VALFLAGGA-LINE-ATTR (INDX) TO                            
045300                                     MOD-VALFLAGGA-ATTRIB (INDX)          
045400       IF RESP-VALFLAGGA-LINE (INDX) = SPACE                              
045500         MOVE MFS-RENSA-FAELT    TO MOD-VALFLAGGA (INDX)                  
045600       ELSE                                                               
045700         IF RESP-VALFLAGGA-LINE (INDX) = ALL '+'                          
045800           MOVE MFS-ROER-EJ-FAELT                                         
045900                                 TO MOD-VALFLAGGA (INDX)                  
046000         ELSE                                                             
046100           MOVE RESP-VALFLAGGA-LINE (INDX)                                
046200                                 TO MOD-VALFLAGGA (INDX)                  
046300         END-IF                                                           
046400       END-IF                                                             
046500                                                                          
046600       MOVE RESP-IDKOLLI-LINE-ATTR (INDX) TO                              
046700                                   MOD-IDKOLLI-ATTRIB (INDX)              
046800       IF RESP-IDKOLLI-LINE (INDX) = SPACE                                
046900         MOVE MFS-RENSA-FAELT    TO MOD-IDKOLLI (INDX)                    
047000       ELSE                                                               
047100         IF RESP-IDKOLLI-LINE (INDX) = ALL '+'                            
047200           MOVE MFS-ROER-EJ-FAELT                                         
047300                                 TO MOD-IDKOLLI (INDX)                    
047400         ELSE                                                             
047500           MOVE RESP-IDKOLLI-LINE (INDX)                                  
047600                                 TO MOD-IDKOLLI (INDX)                    
047700         END-IF                                                           
047800       END-IF                                                             
047900                                                                          
048000       MOVE RESP-IDTRPTNR-OLD-ATTR (INDX) TO                              
048100                                   MOD-IDTRPTNR-OLD-ATTRIB (INDX)         
048200       IF RESP-IDTRPTNR-OLD  (INDX) = SPACE                               
048300         MOVE MFS-RENSA-FAELT    TO MOD-IDTRPTNR-OLD (INDX)               
048400       ELSE                                                               
048500         IF RESP-IDTRPTNR-OLD  (INDX) = ALL '+'                           
048600           MOVE MFS-ROER-EJ-FAELT                                         
048700                                 TO MOD-IDTRPTNR-OLD (INDX)               
048800         ELSE                                                             
048900           MOVE RESP-IDTRPTNR-OLD  (INDX)                                 
049000                                 TO MOD-IDTRPTNR-OLD (INDX)               
049100         END-IF                                                           
049200       END-IF                                                             
049300                                                                          
049400       MOVE RESP-KDFRAKT-OLD-ATTR (INDX) TO                               
049500                                   MOD-KDFRAKT-OLD-ATTRIB (INDX)          
049600       IF RESP-KDFRAKT-OLD  (INDX) = SPACE                                
049700         MOVE MFS-RENSA-FAELT    TO MOD-KDFRAKT-OLD (INDX)                
049800       ELSE                                                               
049900         IF RESP-KDFRAKT-OLD  (INDX) = ALL '+'                            
050000           MOVE MFS-ROER-EJ-FAELT                                         
050100                                 TO MOD-KDFRAKT-OLD (INDX)                
050200         ELSE                                                             
050300           MOVE RESP-KDFRAKT-OLD  (INDX)                                  
050400                                 TO MOD-KDFRAKT-OLD (INDX)                
050500         END-IF                                                           
050600       END-IF                                                             
050700                                                                          
050800       MOVE RESP-IDTRPTNR-LINE-ATTR (INDX) TO                             
050900                                MOD-IDTRPTNR-IN-ATTRIB (INDX)             
051000       IF RESP-IDTRPTNR-LINE (INDX) = SPACE                               
051100         MOVE MFS-RENSA-FAELT    TO MOD-IDTRPTNR-IN (INDX)                
051200       ELSE                                                               
051300         IF RESP-IDTRPTNR-LINE (INDX) =  ALL '+'                          
051400           MOVE MFS-ROER-EJ-FAELT                                         
051500                                 TO MOD-IDTRPTNR-IN (INDX)                
051600         ELSE                                                             
051700           MOVE RESP-IDTRPTNR-LINE (INDX)                                 
051800                                 TO MOD-IDTRPTNR-IN (INDX)                
051900         END-IF                                                           
052000       END-IF                                                             
052100                                                                          
052200       MOVE RESP-KDFRAKT-LINE-ATTR (INDX) TO                              
052300                                MOD-KDFRAKT-IN-ATTRIB (INDX)              
052400       IF RESP-KDFRAKT-LINE (INDX) = SPACE                                
052500         MOVE MFS-RENSA-FAELT    TO MOD-KDFRAKT-IN (INDX)                 
052600       ELSE                                                               
052700         IF RESP-KDFRAKT-LINE (INDX) =  ALL '+'                           
052800           MOVE MFS-ROER-EJ-FAELT                                         
052900                                 TO MOD-KDFRAKT-IN (INDX)                 
053000         ELSE                                                             
053100           MOVE RESP-KDFRAKT-LINE (INDX)                                  
053200                                 TO MOD-KDFRAKT-IN (INDX)                 
053300         END-IF                                                           
053400       END-IF                                                             
053500                                                                          
053600       ADD +1  TO INDX                                                    
053700     END-PERFORM                                                          
053800                                                                          
053900*    PERFORM STARTS WITH PREVIOUS INDX VALUE                              
054000     PERFORM UNTIL INDX > MAX-KVRADER                                     
054100       MOVE MFS-RENSA-FAELT      TO MOD-VALFLAGGA    (INDX)               
054200                                    MOD-IDKOLLI      (INDX)               
054300                                    MOD-IDTRPTNR-OLD (INDX)               
054400                                    MOD-IDTRPTNR-IN  (INDX)               
054500                                    MOD-KDFRAKT-OLD (INDX)                
054600                                    MOD-KDFRAKT-IN  (INDX)                
054700       ADD  +1 TO INDX                                                    
054800     END-PERFORM                                                          
054900                                                                          
055000     .                                                                    
055100     EJECT                                                                
055200 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
055300                                                                          
055400*    --- OUTDATA-FIELD ON SCROLL KEYS                                     
055500     MOVE MFS-ERASE-FIELD TO MOD-VALFLAGGA (INDX)                         
055600                             MOD-IDKOLLI   (INDX)                         
055700     .                                                                    
055800     SKIP3                                                                
055900 MFS-ERASE-FIELD-IN SECTION.                                              
056000                                                                          
056100*    --- ALLA INDATA-FÄLT                                                 
056200     MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-IN                               
056300                             MOD-IDKUNDNR-IN                              
056400                             MOD-IDORDNR-IN                               
056500                             MOD-IDPRODNR-IN                              
056600                             MOD-FLTRPTCHG                                
056700                             MOD-IDTRPTNR                                 
056800                             MOD-KDFRAKT                                  
056900     MOVE +1 TO INDX                                                      
057000     PERFORM UNTIL INDX > MAX-KVRADER                                     
057100       PERFORM MFS-ERASE-LINE-FIELD-IN                                    
057200       ADD +1 TO INDX                                                     
057300     END-PERFORM                                                          
057400     .                                                                    
057500     EJECT                                                                
057600 MFS-ERASE-LINE-FIELD-IN SECTION.                                         
057700                                                                          
057800*    --- INPUT-FIELD ON SCROLL KEYS                                       
057900     MOVE MFS-ERASE-FIELD TO MOD-VALFLAGGA    (INDX)                      
058000                             MOD-IDKOLLI      (INDX)                      
058100                             MOD-IDTRPTNR-OLD (INDX)                      
058200                             MOD-IDTRPTNR-IN  (INDX)                      
058300                             MOD-KDFRAKT-OLD (INDX)                       
058400                             MOD-KDFRAKT-IN  (INDX)                       
058500     .                                                                    
058600     SKIP3                                                                
058700* IMS SEKTIONER                                                           
058800     SKIP3                                                                
058900                                                                          
059000 IMS-GET-MSG SECTION.                                                     
059100     MOVE '  QC' TO GOOD-STATUSCODES                                      
059200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
059300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
059400     PERFORM IMS-STATUSKONTROLL                                           
059500     SKIP3                                                                
059600                                                                          
059700     .                                                                    
059800 IMS-INSERT-MSG SECTION.                                                  
059900                                                                          
060000*    IF MSGI-IDLAND-SPR NOT = 'GB'                                        
060100         MOVE '0' TO MFS-KDHUVOMR                                         
060200*    END-IF                                                               
060300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
060400     MOVE SPACE TO GOOD-STATUSCODES                                       
060500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
060600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
060700     PERFORM IMS-STATUSKONTROLL                                           
060800                                                                          
060900     .                                                                    
061000     EJECT                                                                
061100 IMS-STATUSKONTROLL SECTION.                                              
061200     SET STATUS-IX TO 1                                                   
061300     SEARCH GOOD-STATUS                                                   
061400       AT END                                                             
061500         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
061600         DELIMITED BY SIZE INTO ERROR-TEXT                                
061700         CALL FELLOG                                                      
061800       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
061900         CONTINUE                                                         
062000     END-SEARCH                                                           
062100     .                                                                    
062200     EJECT                                                                
