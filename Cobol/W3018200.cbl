000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3018200.                                                
000300 AUTHOR.         GAVIN SMITH.                                             
000400 DATE-WRITTEN.   98/12/13.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700****************************************************************          
000800*    FUNKTION:                                                            
000900*        THIS PGM IS USED TO GENERATE CLEARING TRANSACTIONS               
001000*        BETWEEN DIFFERENT SDC'S FOR EXCHANGE PARTS.                      
001100*                                                                         
001200*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001300*        PROGRAMMET UPPDATERAR WDK7                                       
001400*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001500*        PROGRAMMET UPPDATERAR WL3171 (WDGX) PÅ WDR4                      
001600*        PROGRAMMET UPPDATERAR WL3169 (WDGX) (KOLLI-LÖPNR/DC)             
001700*        PROGRAMMET UPPDATERAR WLXXLD (WDGX) PÅ WDR1                      
001900*        PROGRAMMET UPPDATERAR WLLOGA (WDL9)                              
002000*                                                                         
002100*                                                                         
002200*    INDATA.                                                              
002300*        TRANSAKTION: W3T182                                              
002400*        MID:         W3I182N1                                            
002500*                                                                         
002600*    UTDATA.                                                              
002700*        MOD:         W3O182N1                                            
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W3018200'.            
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300 01  ALL-SPACE.                                                           
004400     03 FILLER                   PIC X(80)   VALUE SPACE.                 
004500 01  ALL-PLUS.                                                            
004600     03 FILLER                   PIC X(80)   VALUE ALL '+'.               
004700                                                                          
004800*    --- ARBETSFÄLT FÖR DIVERSE INDEX                                     
004900 77  INDX                        PIC 999     VALUE ZERO.                  
005000 77  MAX-KVRADER                 PIC S9(4)  COMP VALUE +10.               
005100*                                                                         
005200     EJECT                                                                
005300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005400                                                                          
005500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005600     88  EGEN-MID                            VALUE '3182'.                
005700     88  GODK-MID                            VALUE '3181' '3182'          
005800                                                   '3183' '3184'          
005900                                                   '3185' '3186'          
006000                                                   '3187' '3188'          
006100                                                   '3192'.                
006200     88  HELP-MID                            VALUE '0551'.                
006300     EJECT                                                                
006400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006500 01  GENERELLA-SUBPROGRAM.                                                
006600     03  WL01MCNV                PIC X(8)    VALUE 'WL01MCNV'.            
006700     03  W3018210                PIC X(8)    VALUE 'W3018210'.            
006800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI'.             
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG'.              
007100     EJECT                                                                
007200*    --- PARAMETRAR TILL SUBPROGRAM WL01MCNV                              
007300*01 -COPY WL01MCNV                                                        
007400     SKIP3                                                                
007500*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
007600*                                                                         
007700 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
007800     SKIP3                                                                
007900*01 -COPY WMSGINIT                                                        
008000*                                                                         
008100 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
008200 01  REQU-AREA.                                                           
008300*    03 -COPY WZ01REQU                                                    
008400*    03 -COPY W30182I1                                                    
008500     EJECT                                                                
008600 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
008700 01  RESP-AREA.                                                           
008800*    03 -COPY WZ01RESP                                                    
008900*    03 -COPY W30182O1                                                    
009000     EJECT                                                                
009100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009200*                                                                         
009300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009400     SKIP3                                                                
009500*01  MID -COPY W3I18201                                                   
009600     EJECT                                                                
009700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009800     SKIP3                                                                
009900*01  -COPY WMSGAREA                                                       
010000     EJECT                                                                
010100     03  MOD REDEFINES MSG-AREA.                                          
010200*      05  -COPY W3O18201                                                 
010300     EJECT                                                                
010400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010500     SKIP3                                                                
010600*01  -COPY WMFSAREA                                                       
010700     EJECT                                                                
010800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010900*                                                                         
011000     EJECT                                                                
011100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011200     SKIP3                                                                
011300*    --- STATUS-KOD FRÅN IMS                                              
011400 01  STATUS-WS                   PIC XX.                                  
011500     88  SEGMENT-FINNS                       VALUE '  '.                  
011600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011800     88  END-OF-DATABASE                     VALUE 'GB'.                  
011900     SKIP2                                                                
012000 01  GODK-STATUSKODER.                                                    
012100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012200     SKIP3                                                                
012300 01  SSA1                        PIC X(64).                               
012400 01  SSA2                        PIC X(64).                               
012500 01  SSA3                        PIC X(64).                               
012600 01  SSA4                        PIC X(64).                               
012700     EJECT                                                                
012800*    --- IMS FUNKTIONSKODER                                               
012900*01  -COPY W0003                                                          
013000     EJECT                                                                
013100*    ---  DLI INPUT-OUTPUT AREA                                           
013200                                                                          
013300     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500*01  -COPY W0009  -PRE MSG-                                               
013600*01  -COPY W0009  -PRE ALT-                                               
013700                                                                          
013800*01  -COPY W0008  -PRE USEA-                                              
013900     05  FILLER                  PIC X.                                   
014000                                                                          
014100*01  -COPY W0008  -PRE ARTC-                                              
014200     05  FILLER                  PIC X.                                   
014300                                                                          
014400*01  -COPY W0008  -PRE WDK7-                                              
014500     05  FILLER                  PIC X.                                   
014600                                                                          
014700*01  -COPY W0008  -PRE BENA-                                              
014800     05  FILLER                  PIC X.                                   
014900                                                                          
015000*01  -COPY W0008  -PRE 3171-                                              
015100     05  FILLER                  PIC X.                                   
015200                                                                          
015300*01  -COPY W0008  -PRE XXLD-                                              
015400     05  FILLER                  PIC X.                                   
015500                                                                          
015900*01  -COPY W0008  -PRE LOGA-                                              
016000     05  FILLER                  PIC X.                                   
016100*01  -COPY W0008  -PRE 3169-                                              
016200     05  FILLER                  PIC X.                                   
016300*01  -COPY W0008  -PRE WDB6-                                              
016400     05  FILLER                  PIC X.                                   
016800     EJECT                                                                
016900 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB ARTC-PCB              
017000     WDK7-PCB BENA-PCB 3171-PCB XXLD-PCB LOGA-PCB                         
017100     3169-PCB WDB6-PCB.                                                   
017200 MAIN SECTION.                                                            
017300     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB ARTC-PCB              
017400     WDK7-PCB BENA-PCB 3171-PCB XXLD-PCB LOGA-PCB                         
017500     3169-PCB WDB6-PCB.                                                   
017600                                                                          
017700                                                                          
017800     PERFORM IMS-GET-MSG                                                  
017900     IF SEGMENT-FINNS                                                     
018000       PERFORM A-INIT                                                     
018100       PERFORM B-INIT-KEYS                                                
018200       PERFORM C-INIT-REQU                                                
018300       IF MFS-UPDATE                                                      
018400         SET REQU-UPDATE         TO TRUE                                  
018500         PERFORM E-SAMMA-SIDA                                             
018600       ELSE                                                               
018700         IF MFS-FIRST                                                     
018800           SET REQU-FIRST        TO TRUE                                  
018900         ELSE                                                             
019000           IF MFS-NEXT                                                    
019100             SET REQU-NEXT       TO TRUE                                  
019200             PERFORM D-NAESTA-SIDA                                        
019300           ELSE                                                           
019400             IF MFS-SPLIT                                                 
019500               SET REQU-SPLIT    TO TRUE                                  
019600             ELSE                                                         
019700               SET REQU-QUERY    TO TRUE                                  
019800               PERFORM E-SAMMA-SIDA                                       
019900             END-IF                                                       
020000           END-IF                                                         
020100         END-IF                                                           
020200       END-IF                                                             
020300       PERFORM F-CALL-BIZ-LOGIC-W3018210                                  
020400       COMPUTE MSG-KVLL = LENGTH OF MOD-W3O18201 + 4                      
020500       PERFORM IMS-INSERT-MSG                                             
020600     END-IF                                                               
020700                                                                          
020800     MOVE ZERO TO RETURN-CODE                                             
020900     GOBACK                                                               
021000     .                                                                    
021100     EJECT                                                                
021200 A-INIT SECTION.                                                          
021300                                                                          
021400     IF MSG-DUBBLA-TRANSKODER                                             
021500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I18201                 
021600       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
021700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
021800     ELSE                                                                 
021900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W3I18201                  
022000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
022100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
022200     END-IF                                                               
022300                                                                          
022400     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
022500     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
022600     MOVE MFS-IDTRANS TO W-IDTRANS                                        
022700                                                                          
022800     MOVE LOW-VALUE TO MSG-AREA                                           
022900     MOVE 'W3O182N1' TO MFS-IDMOD                                         
023000     MOVE '3182' TO MOD-IDTRANS                                           
023100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
023200                                                                          
023300     IF EGEN-MID OR HELP-MID                                              
023400       CONTINUE                                                           
023500     ELSE                                                                 
023600       MOVE SPACE TO MFS-KDTRTYP                                          
023700       MOVE '7' TO MFS-IDPFK                                              
023800     END-IF                                                               
023900                                                                          
024000     MOVE ALL '+'           TO MSGI-WMSGINIT                              
024100     MOVE '001'             TO MSGI-KDCALL                                
024200     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
024300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
024400     MOVE '3182'            TO MSGI-IDTRANS                               
024500                                                                          
024600     MOVE MID-IDKOLLI-IN    TO MSGI-IDKOLLI                               
024700                                                                          
024800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
024900                                                                          
025000     MOVE MSGI-IDSPRAK      TO MCNV-IDSPRAK                               
025100                               REQU-IDSPRAK                               
025200     MOVE '101'             TO REQU-IDMSGVER                              
025300     .                                                                    
025400     EJECT                                                                
025500 B-INIT-KEYS SECTION.                                                     
025600                                                                          
025700     MOVE MSGI-IDUSER       TO REQU-IDUSER                                
025800     MOVE MSGI-IDDC         TO REQU-IDDC-KEY                              
025900                                                                          
026000     MOVE MID-IDKOLLI-IN    TO REQU-IDKOLLI-KEY                           
026100     IF MSGI-IDKOLLI NUMERIC                                              
026200       MOVE MSGI-IDKOLLI      TO MOD-IDKOLLI-UT                           
026300     ELSE                                                                 
026400       MOVE MFS-RENSA-FAELT   TO MOD-IDKOLLI-UT                           
026500     END-IF                                                               
026600                                                                          
026700     IF MID-IDKOLLI-IN NOT = ALL '+'                                      
026800        SET REQU-FIRST      TO TRUE                                       
026900     END-IF                                                               
027000     .                                                                    
027100     EJECT                                                                
027200 C-INIT-REQU SECTION.                                                     
027300                                                                          
027400     MOVE MAX-KVRADER            TO REQU-KVRADER                          
027500                                                                          
027600     MOVE MID-SKAPA-NY-KOLLI     TO REQU-NYTT-KOLLI-UPD                   
027700     MOVE MID-VIKT-KOLLI-IN      TO REQU-KOLLI-VIKT-UPD                   
027800     MOVE MID-VOL-KOLLI-IN       TO REQU-KOLLI-VOL-UPD                    
027900     MOVE MID-VIKT-TOT-IN        TO REQU-TOT-VIKT-UPD                     
028000     MOVE MID-VOL-TOT-IN         TO REQU-TOT-VOL-UPD                      
028100     MOVE MID-SKAPA-PROFORMA     TO REQU-SKAPA-PROFORMA                   
028200     MOVE MID-DEL-IDARTNO        TO REQU-DEL-IDARTNR-OBJ                  
028300     IF MID-DEL-KVANTAL = ALL '+'                                         
028400       MOVE ALL-PLUS             TO REQU-DEL-KVANTAL                      
028500     ELSE                                                                 
028600       MOVE MID-DEL-KVANTAL      TO REQU-DEL-KVANTAL                      
028700     END-IF                                                               
028800     MOVE MID-DEL-IDKOLLI        TO REQU-DEL-IDKOLLI                      
028900                                                                          
029000     PERFORM                                                              
029100     VARYING INDX FROM +1 BY +1                                           
029200       UNTIL INDX > MAX-KVRADER                                           
029300       MOVE MID-IDARTNR-OBJ (INDX)                                        
029400                                 TO REQU-IDARTNR-OBJ (INDX)               
029500       MOVE MID-KVCLEAR (INDX)   TO REQU-KVANTAL (INDX)                   
029600     END-PERFORM                                                          
029700                                                                          
029800     .                                                                    
029900     EJECT                                                                
030000 D-NAESTA-SIDA SECTION.                                                   
030100                                                                          
030200     CONTINUE                                                             
030300     .                                                                    
030400     EJECT                                                                
030500 E-SAMMA-SIDA SECTION.                                                    
030600                                                                          
030700     IF EGEN-MID OR HELP-MID                                              
030800       CONTINUE                                                           
030900     ELSE                                                                 
031000       PERFORM MFS-RENSA-FAELT-IN                                         
031100     END-IF                                                               
031200     .                                                                    
031300     EJECT                                                                
031400 F-CALL-BIZ-LOGIC-W3018210 SECTION.                                       
031500                                                                          
031600     CALL W3018210 USING REQU-AREA RESP-AREA MAX-KVRADER                  
031700                                 ALT-PCB USEA-PCB ARTC-PCB                
031800                         WDK7-PCB BENA-PCB 3171-PCB                       
031900                         XXLD-PCB LOGA-PCB                                
032000                         3169-PCB WDB6-PCB                                
032100                                                                          
032200     IF RESP-IDMSG-ERROR NOT = SPACE OR                                   
032300        RESP-IDMSG-INFO  NOT = SPACE                                      
032400       PERFORM FA-SET-MSG-AND-HILIGHT                                     
032500     END-IF                                                               
032600     PERFORM FB-MOVE-RESP-TO-MOD                                          
032700     .                                                                    
032800     EJECT                                                                
032900 FA-SET-MSG-AND-HILIGHT SECTION.                                          
033000                                                                          
033100     MOVE RESP-IDMSG-ERROR       TO MCNV-IDMSG-ERROR                      
033200     MOVE RESP-IDMSG-INFO        TO MCNV-IDMSG-INFO                       
033300     MOVE RESP-IDELMT-ERROR      TO MCNV-IDELMT-ERROR                     
033400                                                                          
033500     CALL WL01MCNV USING MCNV-AREA                                        
033600     MOVE MCNV-MFSFEL            TO MOD-TEMFSFEL                          
033700     MOVE MCNV-MFSINF            TO MOD-TEMFSINF                          
033800                                                                          
033900     .                                                                    
034000     EJECT                                                                
034100 FB-MOVE-RESP-TO-MOD SECTION.                                             
034200                                                                          
034300*    -- DO SOME SOFITICATED MANUPULATION TO HANDLE                        
034400*    -- TWO KEY FIELDS (-UN AND -UT) IN THE FORMAT                        
034500*    -- BUT ONLY ONE FIELD IN RESPONSE (-KEY)                             
034600     MOVE RESP-IDKOLLI-KEY-ATTR  TO MOD-IDKOLLI-IN-ATTR                   
034700     IF RESP-IDKOLLI-KEY = SPACE                                          
034800       MOVE MFS-RENSA-FAELT      TO MOD-IDKOLLI-UT                        
034900                                    MOD-IDKOLLI-IN                        
035000     ELSE                                                                 
035100       IF RESP-IDKOLLI-KEY = ALL '+'                                      
035200         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDKOLLI-UT                        
035300                                    MOD-IDKOLLI-IN                        
035400       ELSE                                                               
035500         MOVE RESP-IDKOLLI-KEY   TO MOD-IDKOLLI-UT                        
035600         MOVE MFS-RENSA-FAELT    TO MOD-IDKOLLI-IN                        
035700       END-IF                                                             
035800     END-IF                                                               
035900                                                                          
036000     IF RESP-IDFAKT = SPACE                                               
036100       MOVE MFS-RENSA-FAELT      TO MOD-IDFAKT-UT                         
036200     ELSE                                                                 
036300       IF RESP-IDFAKT = ALL '+'                                           
036400         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDFAKT-UT                         
036500       ELSE                                                               
036600         MOVE RESP-IDFAKT        TO MOD-IDFAKT-UT                         
036700       END-IF                                                             
036800     END-IF                                                               
036900                                                                          
037000     MOVE RESP-NYTT-KOLLI-UPD-ATTR                                        
037100                                 TO MOD-NYTT-KOLLI-IN-ATTR                
037200     IF RESP-NYTT-KOLLI-UPD = SPACE                                       
037300       MOVE MFS-RENSA-FAELT      TO MOD-NYTT-KOLLI-IN                     
037400     ELSE                                                                 
037500       IF RESP-NYTT-KOLLI-UPD = ALL '+'                                   
037600         MOVE MFS-ROER-EJ-FAELT  TO MOD-NYTT-KOLLI-IN                     
037700       ELSE                                                               
037800         MOVE RESP-NYTT-KOLLI-UPD                                         
037900                                 TO MOD-NYTT-KOLLI-IN                     
038000       END-IF                                                             
038100     END-IF                                                               
038200                                                                          
038300     MOVE RESP-KOLLI-VIKT-UPD-ATTR                                        
038400                                 TO MOD-KOLLI-VIKT-IN-ATTR                
038500     IF RESP-KOLLI-VIKT-UPD = SPACE                                       
038600       MOVE MFS-RENSA-FAELT      TO MOD-KOLLI-VIKT-IN                     
038700     ELSE                                                                 
038800       IF RESP-KOLLI-VIKT-UPD = ALL '+'                                   
038900         MOVE MFS-ROER-EJ-FAELT  TO MOD-KOLLI-VIKT-IN                     
039000       ELSE                                                               
039100         MOVE RESP-KOLLI-VIKT-UPD                                         
039200                                 TO MOD-KOLLI-VIKT-IN                     
039300       END-IF                                                             
039400     END-IF                                                               
039500                                                                          
039600     MOVE RESP-KOLLI-VOL-UPD-ATTR                                         
039700                                 TO MOD-KOLLI-VOL-IN-ATTR                 
039800     IF RESP-KOLLI-VOL-UPD = SPACE                                        
039900       MOVE MFS-RENSA-FAELT      TO MOD-KOLLI-VOL-IN                      
040000     ELSE                                                                 
040100       IF RESP-KOLLI-VOL-UPD = ALL '+'                                    
040200         MOVE MFS-ROER-EJ-FAELT  TO MOD-KOLLI-VOL-IN                      
040300       ELSE                                                               
040400         MOVE RESP-KOLLI-VOL-UPD TO MOD-KOLLI-VOL-IN                      
040500       END-IF                                                             
040600     END-IF                                                               
040700                                                                          
040800     IF RESP-KOLLI-VIKT-UT = SPACE                                        
040900       MOVE MFS-RENSA-FAELT      TO MOD-KOLLI-VIKT-UT                     
041000     ELSE                                                                 
041100       IF RESP-KOLLI-VIKT-UT = ALL '+'                                    
041200         MOVE MFS-ROER-EJ-FAELT  TO MOD-KOLLI-VIKT-UT                     
041300       ELSE                                                               
041400         MOVE RESP-KOLLI-VIKT-UT TO MOD-KOLLI-VIKT-UT                     
041500       END-IF                                                             
041600     END-IF                                                               
041700                                                                          
041800     IF RESP-KOLLI-VOL-UT = SPACE                                         
041900       MOVE MFS-RENSA-FAELT      TO MOD-KOLLI-VOL-UT                      
042000     ELSE                                                                 
042100       IF RESP-KOLLI-VOL-UT = ALL '+'                                     
042200         MOVE MFS-ROER-EJ-FAELT  TO MOD-KOLLI-VOL-UT                      
042300       ELSE                                                               
042400         MOVE RESP-KOLLI-VOL-UT  TO MOD-KOLLI-VOL-UT                      
042500       END-IF                                                             
042600     END-IF                                                               
042700                                                                          
042800     MOVE RESP-TOT-VIKT-UPD-ATTR TO MOD-TOT-VIKT-IN-ATTR                  
042900     IF RESP-TOT-VIKT-UPD = SPACE                                         
043000       MOVE MFS-RENSA-FAELT      TO MOD-TOT-VIKT-IN                       
043100     ELSE                                                                 
043200       IF RESP-TOT-VIKT-UPD = ALL '+'                                     
043300         MOVE MFS-ROER-EJ-FAELT  TO MOD-TOT-VIKT-IN                       
043400       ELSE                                                               
043500         MOVE RESP-TOT-VIKT-UPD  TO MOD-TOT-VIKT-IN                       
043600       END-IF                                                             
043700     END-IF                                                               
043800                                                                          
043900     MOVE RESP-TOT-VOL-UPD-ATTR  TO MOD-TOT-VOL-IN-ATTR                   
044000     IF RESP-TOT-VOL-UPD = SPACE                                          
044100       MOVE MFS-RENSA-FAELT      TO MOD-TOT-VOL-IN                        
044200     ELSE                                                                 
044300       IF RESP-TOT-VOL-UPD = ALL '+'                                      
044400         MOVE MFS-ROER-EJ-FAELT  TO MOD-TOT-VOL-IN                        
044500       ELSE                                                               
044600         MOVE RESP-TOT-VOL-UPD   TO MOD-TOT-VOL-IN                        
044700       END-IF                                                             
044800     END-IF                                                               
044900                                                                          
045000     MOVE RESP-SKAPA-PROFORMA-ATTR TO MOD-CRE-INV-ATTR                    
045100     IF RESP-SKAPA-PROFORMA = SPACE                                       
045200       MOVE MFS-RENSA-FAELT      TO MOD-CRE-INV                           
045300     ELSE                                                                 
045400       IF RESP-SKAPA-PROFORMA = ALL '+'                                   
045500         MOVE MFS-ROER-EJ-FAELT  TO MOD-CRE-INV                           
045600       ELSE                                                               
045700         MOVE RESP-SKAPA-PROFORMA TO MOD-CRE-INV                          
045800       END-IF                                                             
045900     END-IF                                                               
046000                                                                          
046100     MOVE RESP-DEL-IDARTNR-OBJ-ATTR                                       
046200                                 TO MOD-DEL-ARTNR-IN-ATTR                 
046300     IF RESP-DEL-IDARTNR-OBJ = SPACE                                      
046400       MOVE MFS-RENSA-FAELT      TO MOD-DEL-ARTNR-IN                      
046500     ELSE                                                                 
046600       IF RESP-DEL-IDARTNR-OBJ = ALL '+'                                  
046700         MOVE MFS-ROER-EJ-FAELT  TO MOD-DEL-ARTNR-IN                      
046800       ELSE                                                               
046900         MOVE RESP-DEL-IDARTNR-OBJ TO MOD-DEL-ARTNR-IN                    
047000       END-IF                                                             
047100     END-IF                                                               
047200                                                                          
047300     MOVE RESP-DEL-KVANTAL-ATTR                                           
047400                                 TO MOD-DEL-KVANT-IN-ATTR                 
047500     IF RESP-DEL-KVANTAL = SPACE                                          
047600       MOVE MFS-RENSA-FAELT      TO MOD-DEL-KVANT-IN                      
047700     ELSE                                                                 
047800       IF RESP-DEL-KVANTAL = ALL '+'                                      
047900         MOVE MFS-ROER-EJ-FAELT  TO MOD-DEL-KVANT-IN                      
048000       ELSE                                                               
048100         MOVE RESP-DEL-KVANTAL TO MOD-DEL-KVANT-IN                        
048200       END-IF                                                             
048300     END-IF                                                               
048400                                                                          
048500     MOVE RESP-DEL-IDKOLLI-ATTR                                           
048600                                 TO MOD-DEL-KOLLI-IN-ATTR                 
048700     IF RESP-DEL-IDKOLLI = SPACE                                          
048800       MOVE MFS-RENSA-FAELT      TO MOD-DEL-KOLLI-IN                      
048900     ELSE                                                                 
049000       IF RESP-DEL-IDKOLLI = ALL '+'                                      
049100         MOVE MFS-ROER-EJ-FAELT  TO MOD-DEL-KOLLI-IN                      
049200       ELSE                                                               
049300         MOVE RESP-DEL-IDKOLLI TO MOD-DEL-KOLLI-IN                        
049400       END-IF                                                             
049500     END-IF                                                               
049600                                                                          
049700     IF RESP-TOT-VIKT-UT = SPACE                                          
049800       MOVE MFS-RENSA-FAELT      TO MOD-TOT-VIKT-UT                       
049900     ELSE                                                                 
050000       IF RESP-TOT-VIKT-UT = ALL '+'                                      
050100         MOVE MFS-ROER-EJ-FAELT  TO MOD-TOT-VIKT-UT                       
050200       ELSE                                                               
050300         MOVE RESP-TOT-VIKT-UT   TO MOD-TOT-VIKT-UT                       
050400       END-IF                                                             
050500     END-IF                                                               
050600                                                                          
050700     IF RESP-TOT-VOL-UT = SPACE                                           
050800       MOVE MFS-RENSA-FAELT      TO MOD-TOT-VOL-UT                        
050900     ELSE                                                                 
051000       IF RESP-TOT-VOL-UT = ALL '+'                                       
051100         MOVE MFS-ROER-EJ-FAELT  TO MOD-TOT-VOL-UT                        
051200       ELSE                                                               
051300         MOVE RESP-TOT-VOL-UT    TO MOD-TOT-VOL-UT                        
051400       END-IF                                                             
051500     END-IF                                                               
051600                                                                          
051700     PERFORM                                                              
051800     VARYING INDX FROM +1 BY +1                                           
051900       UNTIL INDX > MAX-KVRADER                                           
052000                                                                          
052100       MOVE RESP-IDARTNR-OBJ-ATTR (INDX)                                  
052200                                 TO MOD-IDARTNR-OBJ-ATTR (INDX)           
052300       IF RESP-IDARTNR-OBJ (INDX) = SPACE                                 
052400         MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR-OBJ (INDX)                
052500       ELSE                                                               
052600         IF RESP-IDARTNR-OBJ (INDX) = ALL '+'                             
052700           MOVE MFS-ROER-EJ-FAELT                                         
052800                                 TO MOD-IDARTNR-OBJ (INDX)                
052900         ELSE                                                             
053000           MOVE RESP-IDARTNR-OBJ (INDX)                                   
053100                                 TO MOD-IDARTNR-OBJ (INDX)                
053200         END-IF                                                           
053300       END-IF                                                             
053400                                                                          
053500       MOVE RESP-KVANTAL-ATTR (INDX)                                      
053600                                 TO MOD-KVCLEAR-ATTR (INDX)               
053700       IF RESP-KVANTAL (INDX) = SPACE                                     
053800         MOVE MFS-RENSA-FAELT    TO MOD-KVCLEAR (INDX)                    
053900       ELSE                                                               
054000         IF RESP-KVANTAL (INDX) = ALL '+'                                 
054100           MOVE MFS-ROER-EJ-FAELT                                         
054200                                 TO MOD-KVCLEAR (INDX)                    
054300         ELSE                                                             
054400           MOVE RESP-KVANTAL (INDX)                                       
054500                                 TO MOD-KVCLEAR (INDX)                    
054600         END-IF                                                           
054700       END-IF                                                             
054800                                                                          
054900       IF RESP-BEART (INDX) = SPACE                                       
055000       OR RESP-IDARTNR-OBJ (INDX) = SPACE                                 
055100         MOVE MFS-RENSA-FAELT    TO MOD-BEART (INDX)                      
055200       ELSE                                                               
055300         IF RESP-BEART (INDX) = ALL '+'                                   
055400           MOVE MFS-ROER-EJ-FAELT                                         
055500                                 TO MOD-BEART (INDX)                      
055600         ELSE                                                             
055700           MOVE RESP-BEART (INDX)                                         
055800                                 TO MOD-BEART (INDX)                      
055900         END-IF                                                           
056000       END-IF                                                             
056100                                                                          
056200       IF RESP-KVLS (INDX) = SPACE                                        
056300         MOVE MFS-RENSA-FAELT    TO MOD-KVLS (INDX)                       
056400       ELSE                                                               
056500         IF RESP-KVLS (INDX) = ALL '+'                                    
056600           MOVE MFS-ROER-EJ-FAELT                                         
056700                                 TO MOD-KVLS (INDX)                       
056800         ELSE                                                             
056900           MOVE RESP-KVLS (INDX) TO MOD-KVLS (INDX)                       
057000         END-IF                                                           
057100       END-IF                                                             
057200                                                                          
057300     END-PERFORM                                                          
057400                                                                          
057500     PERFORM                                                              
057600     VARYING INDX FROM INDX BY +1                                         
057700       UNTIL INDX > MAX-KVRADER                                           
057800       MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR-OBJ (INDX)                
057900                                    MOD-KVCLEAR  (INDX)                   
058000                                    MOD-BEART    (INDX)                   
058100                                    MOD-KVLS     (INDX)                   
058200     END-PERFORM                                                          
058300                                                                          
058400     .                                                                    
058500     EJECT                                                                
058600 MFS-RENSA-FAELT-IN SECTION.                                              
058700                                                                          
058800* --- ALLA INDATA-FÄLT                                                    
058900     MOVE MFS-RENSA-FAELT  TO                                             
059000*                            MOD-IDKOLLI-IN                               
059100*                            MOD-NYTT-KOLLI-IN                            
059200                             MOD-KOLLI-VIKT-IN                            
059300                             MOD-KOLLI-VOL-IN                             
059400                             MOD-TOT-VIKT-IN                              
059500                             MOD-TOT-VOL-IN                               
059600                             MOD-CRE-INV                                  
059700                             MOD-DEL-ARTNR-IN                             
059800                             MOD-DEL-KVANT-IN                             
059900                             MOD-DEL-KOLLI-IN                             
060000     PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > MAX-KVRADER            
060100       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-OBJ (INDX)                     
060200                               MOD-KVCLEAR (INDX)                         
060300     END-PERFORM                                                          
060400     .                                                                    
060500     EJECT                                                                
060600* --- IMS SEKTIONER                                                       
060700     SKIP3                                                                
060800 IMS-GET-MSG SECTION.                                                     
060900                                                                          
061000     MOVE '  QC' TO GODK-STATUSKODER                                      
061100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
061200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
061300     PERFORM IMS-STATUSKONTROLL                                           
061400     .                                                                    
061500     SKIP3                                                                
061600 IMS-INSERT-MSG SECTION.                                                  
061700                                                                          
061800     IF MSGI-IDLAND-SPR = 'SE'                                            
061900       MOVE '0' TO MFS-KDHUVOMR                                           
062000     END-IF                                                               
062100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
062200     MOVE SPACE TO GODK-STATUSKODER                                       
062300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
062400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
062500     PERFORM IMS-STATUSKONTROLL                                           
062600     .                                                                    
062700     EJECT                                                                
062800 IMS-STATUSKONTROLL SECTION.                                              
062900                                                                          
063000     SET STATUS-IX TO 1                                                   
063100     SEARCH GODK-STATUS                                                   
063200       AT END                                                             
063300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
063400         DELIMITED BY SIZE INTO FELTEXT                                   
063500         CALL FELLOG                                                      
063600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
063700         CONTINUE                                                         
063800     END-SEARCH                                                           
063900     .                                                                    
