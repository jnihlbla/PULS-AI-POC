000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6020400.                                                
000400*AUTHOR.         ELAINE CURTSSON / RAHUL REDDY.                           
000500*DATE-WRITTEN.   AUGUSTI 91 / APRIL 2012                                  
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        KONTROLLRAPPORT PACKNING                                         
001100*        THIS PROGRAM HANDLES MFS PORTION OF SCREEN 6204                  
001200*        AND INVOKES W6020410 CONTAINING BUSINESS LOGIC.                  
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W6T204                                              
001600*        MID:         W6I20401                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W6O20401                                            
002000                                                                          
002100     SKIP3                                                                
002200                                                                          
002300 ENVIRONMENT DIVISION.                                                    
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'W6020400'.            
003000                                                                          
003100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003300                                                                          
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600                                                                          
003700*    --- INDEX FÖR BLÄDDRINGSRADER                                        
003800 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003900 77  MAX-INDX                    PIC S9(4)  VALUE +07   COMP SYNC.        
004000 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004100 77  MAX-KVRADER                 PIC S9(4)  COMP VALUE +7.                
004200 01  ALL-SPACE.                                                           
004300     03  FILLER                  PIC X(50)  VALUE SPACE.                  
004400 01  ALL-PLUS.                                                            
004500     03  FILLER                  PIC X(50)  VALUE ALL '+'.                
004600                                                                          
004700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004800 77  WS-IDKR                     PIC X(5)    VALUE SPACE.                 
004900 77  WS-IDKOLLINR                PIC X(5)    VALUE SPACE.                 
005000                                                                          
005100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005200     88  EGEN-MID                            VALUE '6204'.                
005300     88  GODK-MID                            VALUE '6202' '6203'          
005400                                                   '6204' '6205'          
005500                                                   '6206' '6207'          
005600                                                   '6208' '6209'.         
005700     88  HELP-MID                            VALUE '0551'.                
005800     EJECT                                                                
005900                                                                          
006000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006100 01  GENERELLA-SUBPROGRAM.                                                
006200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006500     03  W6020410                PIC X(8)    VALUE 'W6020410'.            
006600     03  WL01MCNV                PIC X(8)    VALUE 'WL01MCNV'.            
006700     EJECT                                                                
006800*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
006900*01 -COPY WMSGINIT                                                        
007000     SKIP3                                                                
007100     EJECT                                                                
007200*    ---  COPYTEXT FÖR TRANS TILL WL01MCNV                                
007300*01  -COPY WL01MCNV                                                       
007400     EJECT                                                                
007500 01  FILLER                      PIC X(16)  VALUE 'W6020410'.             
007600 01  REQU-AREA.                                                           
007700*    03 -COPY WZ01REQU                                                    
007800*    03 -COPY W60204I1                                                    
007900     EJECT                                                                
008000 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
008100 01  RESP-AREA.                                                           
008200*    03 -COPY WZ01RESP                                                    
008300*    03 -COPY W60204O1                                                    
008400     EJECT                                                                
008500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008600*                                                                         
008700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008800     SKIP3                                                                
008900*01  MID -COPY W6I20401                                                   
009000     EJECT                                                                
009100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009200     SKIP3                                                                
009300*01  -COPY WMSGAREA                                                       
009400     EJECT                                                                
009500     03  MOD REDEFINES MSG-AREA.                                          
009600*      05  -COPY W6O20401                                                 
009700     EJECT                                                                
009800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
009900     SKIP3                                                                
010000*01  -COPY WMFSAREA                                                       
010100     EJECT                                                                
010200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010300*                                                                         
010400     EJECT                                                                
010500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010600     SKIP3                                                                
010700*    --- STATUS-KOD FRÅN IMS                                              
010800 01  STATUS-WS                   PIC XX.                                  
010900     88  SEGMENT-FINNS                       VALUE '  '.                  
011000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011200     SKIP2                                                                
011300 01  GODK-STATUSKODER.                                                    
011400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011500     SKIP3                                                                
011600 01  SSA1                        PIC X(64).                               
011700 01  SSA2                        PIC X(64).                               
011800     EJECT                                                                
011900*    --- IMS FUNKTIONSKODER                                               
012000*01  -COPY W0003                                                          
012100     EJECT                                                                
012200 LINKAGE SECTION.                                                         
012300                                                                          
012400*01  -COPY W0009   -PRE MSG-                                              
012500     EJECT                                                                
012600 01  USEA-PCB                    PIC X.                                   
012700 01  KVAE-PCB                    PIC X.                                   
012800 01  BENA-PCB                    PIC X.                                   
012900 01  LEVA-PCB                    PIC X.                                   
013000 01  EMBB-PCB                    PIC X.                                   
013100 01  WDP3-PCB                    PIC X.                                   
013200 01  ARTC-PCB                    PIC X.                                   
013200 01  WDB6-PCB                    PIC X.                                   
013300 01  W6H7-PCB                    PIC X.                                   
013400 01  9305-PCB                    PIC X.                                   
013500 01  WDK6-PCB                    PIC X.                                   
013600 01  LOPB-PCB                    PIC X.                                   
013700 01  FILC-PCB                    PIC X.                                   
013800 01  WDK7-PCB                    PIC X.                                   
013810 01  KRUP-WDB6-PCB               PIC X.                                   
013900     EJECT                                                                
014000 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB KVAE-PCB BENA-PCB             
014100     LEVA-PCB EMBB-PCB WDP3-PCB ARTC-PCB WDB6-PCB                         
014200     W6H7-PCB 9305-PCB WDK6-PCB LOPB-PCB FILC-PCB                         
014300     WDK7-PCB KRUP-WDB6-PCB.                                              
014400     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB KVAE-PCB BENA-PCB             
014500     LEVA-PCB EMBB-PCB WDP3-PCB ARTC-PCB WDB6-PCB                         
014600     W6H7-PCB 9305-PCB WDK6-PCB LOPB-PCB FILC-PCB                         
014700     WDK7-PCB KRUP-WDB6-PCB.                                              
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
016100         ELSE                                                             
016200           IF MFS-NEXT                                                    
016300             SET REQU-NEXT       TO TRUE                                  
016400             PERFORM D-NAESTA-SIDA                                        
016500           ELSE                                                           
016600             SET REQU-QUERY      TO TRUE                                  
016700             PERFORM E-SAMMA-SIDA                                         
016800           END-IF                                                         
016900         END-IF                                                           
017000       END-IF                                                             
017100       PERFORM F-CALL-BIZ-LOGIC-W6020410                                  
017200       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O20401 + 4                      
017300       PERFORM IMS-INSERT-MSG                                             
017400     END-IF                                                               
017500                                                                          
017600     MOVE ZERO                   TO RETURN-CODE                           
017700     GOBACK                                                               
017800     .                                                                    
017900     EJECT                                                                
018000 A-INIT SECTION.                                                          
018100     IF MSG-DUBBLA-TRANSKODER                                             
018200       MOVE MSG-INDATA-MINUS-2-TRANSKODER                                 
018300                                 TO MID-W6I20401                          
018400       MOVE MSG-IDTRANS-2        TO MFS-IDTRANS                           
018500       MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                          
018600     ELSE                                                                 
018700       MOVE MSG-INDATA-MINUS-1-TRANSKOD                                   
018800                                 TO MID-W6I20401                          
018900       MOVE MSG-IDTRANS-1        TO MFS-IDTRANS                           
019000       MOVE MSG-KDMFSFOR-1       TO MFS-KDMFSFOR                          
019100     END-IF                                                               
019200                                                                          
019300     MOVE MSG-KDTRTYP            TO MFS-KDTRTYP                           
019400     MOVE MSG-IDPFK              TO MFS-IDPFK                             
019500     MOVE MFS-IDTRANS            TO W-IDTRANS                             
019600                                                                          
019700     MOVE LOW-VALUE              TO MSG-AREA                              
019800     MOVE 'W6O204N1'             TO MFS-IDMOD                             
019900     MOVE '6204'                 TO MOD-IDTRANS                           
020000     MOVE MFS-RENSA-FAELT        TO MOD-TEMFSFEL                          
020100                                    MOD-TEMFSINF                          
020200     IF EGEN-MID OR HELP-MID                                              
020300        CONTINUE                                                          
020400     ELSE                                                                 
020500        MOVE SPACE               TO MFS-KDTRTYP                           
020600        MOVE '7'                 TO MFS-IDPFK                             
020700     END-IF                                                               
020800                                                                          
020900     MOVE ALL '+'                TO MSGI-WMSGINIT                         
021000     MOVE '001'                  TO MSGI-KDCALL                           
021100     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
021200     CALL W005INIT            USING MSGI-WMSGINIT                         
021300                                    USEA-PCB                              
021400                                                                          
021500     MOVE MSGI-IDSPRAK           TO MCNV-IDSPRAK                          
021600                                    REQU-IDSPRAK                          
021700                                                                          
021800     PERFORM MFS-FORM-ATTR                                                
021900     MOVE '101'                  TO REQU-IDMSGVER                         
022000     MOVE MSGI-IDUSER            TO REQU-IDUSER                           
022100     .                                                                    
022200     EJECT                                                                
022300 B-INIT-KEYS SECTION.                                                     
022400                                                                          
022500     MOVE MFS-RENSA-FAELT        TO MOD-IDKR-IN                           
022600                                                                          
022700     IF MID-IDKR-IN = ALL '+'                                             
022800       MOVE MID-IDKR-UT          TO WS-IDKR                               
022900       INSPECT WS-IDKR REPLACING LEADING SPACE BY ZERO                    
023000     ELSE                                                                 
023100       MOVE MID-IDKR-IN          TO WS-IDKR                               
023200       MOVE '7'                  TO MFS-IDPFK                             
023300       MOVE SPACE                TO MFS-KDTRTYP                           
023400     END-IF                                                               
023500     MOVE WS-IDKR                TO REQU-IDKR-KEY                         
023600                                                                          
023700     MOVE MFS-RENSA-FAELT        TO MOD-IDKOLLINR-IN                      
023800                                                                          
023900     IF NOT EGEN-MID                                                      
024000        MOVE SPACE               TO MID-IDKOLLINR-UT                      
024100        MOVE ALL '+'             TO MID-IDKOLLINR-IN                      
024200     END-IF                                                               
024300                                                                          
024400     IF MID-IDKOLLINR-IN = ALL '+'                                        
024500        MOVE MID-IDKOLLINR-UT    TO WS-IDKOLLINR                          
024600        INSPECT WS-IDKOLLINR  REPLACING LEADING SPACE BY ZERO             
024700     ELSE                                                                 
024800        MOVE MID-IDKOLLINR-IN    TO WS-IDKOLLINR                          
024900        MOVE SPACE               TO MFS-KDTRTYP                           
025000     END-IF                                                               
025100                                                                          
025200     MOVE WS-IDKOLLINR           TO REQU-IDKOLLINR-KEY                    
025300                                                                          
025400     IF GODK-MID                                                          
025500       MOVE WS-IDKR              TO MOD-IDKR-UT                           
025600       INSPECT MOD-IDKR-UT REPLACING LEADING ZERO BY SPACE                
025700       MOVE WS-IDKOLLINR         TO MOD-IDKOLLINR-UT                      
025800       INSPECT MOD-IDKOLLINR-UT REPLACING LEADING ZERO BY SPACE           
025900     ELSE                                                                 
026000       MOVE MFS-RENSA-FAELT      TO MOD-IDKR-UT                           
026100       MOVE MFS-RENSA-FAELT      TO MOD-IDKOLLINR-UT                      
026200     END-IF                                                               
026300                                                                          
026400     .                                                                    
026500     EJECT                                                                
026600 C-INIT-REQU SECTION.                                                     
026700                                                                          
026800     MOVE MAX-KVRADER            TO REQU-KVRADER                          
026900     MOVE MID-KDKOLLI            TO REQU-KDKOLLI-UPD                      
027000     IF MID-IDKOLLI-IN = ALL '+'                                          
027100       MOVE ALL-PLUS             TO REQU-IDKOLLI-UPD                      
027200     ELSE                                                                 
027300       MOVE MID-IDKOLLI-IN       TO REQU-IDKOLLI-UPD                      
027400     END-IF                                                               
027500     MOVE MID-VKKOLLIB-IN        TO REQU-VKKOLLIB-UPD                     
027600     IF MID-DIKOLLIL-IN = ALL '+'                                         
027700       MOVE ALL-PLUS             TO REQU-DIKOLLIL-UPD                     
027800     ELSE                                                                 
027900       MOVE MID-DIKOLLIL-IN      TO REQU-DIKOLLIL-UPD                     
028000     END-IF                                                               
028100     IF MID-DIKOLLIB-IN = ALL '+'                                         
028200       MOVE ALL-PLUS             TO REQU-DIKOLLIB-UPD                     
028300     ELSE                                                                 
028400       MOVE MID-DIKOLLIB-IN      TO REQU-DIKOLLIB-UPD                     
028500     END-IF                                                               
028600     IF MID-DIKOLLIH-IN = ALL '+'                                         
028700       MOVE ALL-PLUS             TO REQU-DIKOLLIH-UPD                     
028800     ELSE                                                                 
028900       MOVE MID-DIKOLLIH-IN      TO REQU-DIKOLLIH-UPD                     
029000     END-IF                                                               
029100     MOVE MID-KDCMD-IN           TO REQU-KDCMD-UPD                        
029200     IF MID-KDPERSON = ALL '+'                                            
029300       MOVE ALL-PLUS             TO REQU-KDPERSON-UPD                     
029400     ELSE                                                                 
029500       MOVE MID-KDPERSON         TO REQU-KDPERSON-UPD                     
029600     END-IF                                                               
029700     MOVE MID-BEKRPACK           TO REQU-BEKRPACK-UPD                     
029800     MOVE MID-KVKRPACK-IN        TO REQU-KVKRPACK-UPD                     
029900     .                                                                    
030000     EJECT                                                                
030100 D-NAESTA-SIDA SECTION.                                                   
030200     MOVE MID-IDKOLLI-NEXT       TO REQU-IDKOLLI-START                    
030300     .                                                                    
030400     EJECT                                                                
030500 E-SAMMA-SIDA SECTION.                                                    
030600     IF EGEN-MID OR HELP-MID                                              
030700       MOVE MID-IDKOLLI-ENTER    TO REQU-IDKOLLI-START                    
030800     ELSE                                                                 
030900       PERFORM MFS-RENSA-FAELT-IN                                         
031000     END-IF                                                               
031100     .                                                                    
031200     EJECT                                                                
031300 F-CALL-BIZ-LOGIC-W6020410 SECTION.                                       
031400                                                                          
031500     CALL W6020410 USING REQU-AREA RESP-AREA MAX-KVRADER MSG-PCB          
031600                         KVAE-PCB BENA-PCB LEVA-PCB EMBB-PCB              
031700                         WDP3-PCB ARTC-PCB WDB6-PCB                       
031700                         W6H7-PCB 9305-PCB                                
031800                         WDK6-PCB LOPB-PCB FILC-PCB                       
031900                         WDK7-PCB KRUP-WDB6-PCB                           
032000                                                                          
032100     IF RESP-IDMSG-ERROR NOT = SPACE OR                                   
032200        RESP-IDMSG-INFO  NOT = SPACE                                      
032300       PERFORM FA-SET-MSG-AND-HILIGHT                                     
032400     END-IF                                                               
032500     PERFORM FB-MOVE-RESP-TO-MOD                                          
032600     .                                                                    
032700     EJECT                                                                
032800 FA-SET-MSG-AND-HILIGHT SECTION.                                          
032900                                                                          
033000     MOVE RESP-IDMSG-ERROR       TO MCNV-IDMSG-ERROR                      
033100     MOVE RESP-IDMSG-INFO        TO MCNV-IDMSG-INFO                       
033200     MOVE RESP-IDELMT-ERROR      TO MCNV-IDELMT-ERROR                     
033300                                                                          
033400     CALL WL01MCNV USING MCNV-AREA                                        
033500     MOVE MCNV-MFSFEL            TO MOD-TEMFSFEL                          
033600     MOVE MCNV-MFSINF            TO MOD-TEMFSINF                          
033700     .                                                                    
033800     EJECT                                                                
033900 FB-MOVE-RESP-TO-MOD SECTION.                                             
034000                                                                          
034100     IF RESP-IDKOLLI-START = SPACE                                        
034200       MOVE MFS-RENSA-FAELT      TO MOD-IDKOLLI-ENTER                     
034300     ELSE                                                                 
034400       IF RESP-IDKOLLI-START = ALL '+'                                    
034500         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDKOLLI-ENTER                     
034600       ELSE                                                               
034700         MOVE RESP-IDKOLLI-START TO MOD-IDKOLLI-ENTER                     
034800       END-IF                                                             
034900     END-IF                                                               
035000                                                                          
035100     IF RESP-IDKOLLI-NEXT = SPACE                                         
035200       MOVE MFS-RENSA-FAELT      TO MOD-IDKOLLI-NEXT                      
035300     ELSE                                                                 
035400       IF RESP-IDKOLLI-NEXT = ALL '+'                                     
035500         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDKOLLI-NEXT                      
035600       ELSE                                                               
035700         MOVE RESP-IDKOLLI-NEXT  TO MOD-IDKOLLI-NEXT                      
035800       END-IF                                                             
035900     END-IF                                                               
036000                                                                          
036100     IF RESP-IDARTNR = SPACE                                              
036200       MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR                           
036300     ELSE                                                                 
036400       IF RESP-IDARTNR = ALL '+'                                          
036500         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDARTNR                           
036600       ELSE                                                               
036700         MOVE RESP-IDARTNR       TO MOD-IDARTNR                           
036800       END-IF                                                             
036900     END-IF                                                               
037000                                                                          
037100     IF RESP-BEART = SPACE                                                
037200       MOVE MFS-RENSA-FAELT      TO MOD-BEART                             
037300     ELSE                                                                 
037400       IF RESP-BEART = ALL '+'                                            
037500         MOVE MFS-ROER-EJ-FAELT  TO MOD-BEART                             
037600       ELSE                                                               
037700         MOVE RESP-BEART         TO MOD-BEART                             
037800       END-IF                                                             
037900     END-IF                                                               
038000                                                                          
038100     IF RESP-KDKRSTA = SPACE                                              
038200       MOVE MFS-RENSA-FAELT      TO MOD-KDKRSTA                           
038300     ELSE                                                                 
038400       IF RESP-KDKRSTA = ALL '+'                                          
038500         MOVE MFS-ROER-EJ-FAELT  TO MOD-KDKRSTA                           
038600       ELSE                                                               
038700         MOVE RESP-KDKRSTA       TO MOD-KDKRSTA                           
038800       END-IF                                                             
038900     END-IF                                                               
039000                                                                          
039100     IF RESP-TIREGDAT = SPACE                                             
039200       MOVE MFS-RENSA-FAELT      TO MOD-TIREGDAT                          
039300     ELSE                                                                 
039400       IF RESP-TIREGDAT = ALL '+'                                         
039500         MOVE MFS-ROER-EJ-FAELT  TO MOD-TIREGDAT                          
039600       ELSE                                                               
039700         MOVE RESP-TIREGDAT      TO MOD-TIREGDAT                          
039800       END-IF                                                             
039900     END-IF                                                               
040000                                                                          
040100     IF RESP-IDLEVNR = SPACE                                              
040200       MOVE MFS-RENSA-FAELT      TO MOD-IDLEVNR                           
040300     ELSE                                                                 
040400       IF RESP-IDLEVNR = ALL '+'                                          
040500         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDLEVNR                           
040600       ELSE                                                               
040700         MOVE RESP-IDLEVNR       TO MOD-IDLEVNR                           
040800       END-IF                                                             
040900     END-IF                                                               
041000                                                                          
041100     IF RESP-IDLEVG = SPACE                                               
041200       MOVE MFS-RENSA-FAELT      TO MOD-IDLEVG                            
041300     ELSE                                                                 
041400       IF RESP-IDLEVG = ALL '+'                                           
041500         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDLEVG                            
041600       ELSE                                                               
041700         MOVE RESP-IDLEVG        TO MOD-IDLEVG                            
041800       END-IF                                                             
041900     END-IF                                                               
042000                                                                          
042100     IF RESP-BELEV = SPACE                                                
042200       MOVE MFS-RENSA-FAELT      TO MOD-BELEV                             
042300     ELSE                                                                 
042400       IF RESP-BELEV = ALL '+'                                            
042500         MOVE MFS-ROER-EJ-FAELT  TO MOD-BELEV                             
042600       ELSE                                                               
042700         MOVE RESP-BELEV         TO MOD-BELEV                             
042800       END-IF                                                             
042900     END-IF                                                               
043000                                                                          
043100     MOVE RESP-KDKOLLI-UPD-ATTR  TO MOD-KDKOLLI-ATTR                      
043200     IF RESP-KDKOLLI-UPD = SPACE                                          
043300       MOVE MFS-RENSA-FAELT      TO MOD-KDKOLLI                           
043400     ELSE                                                                 
043500       IF RESP-KDKOLLI-UPD = ALL '+'                                      
043600         MOVE MFS-ROER-EJ-FAELT  TO MOD-KDKOLLI                           
043700       ELSE                                                               
043800         MOVE RESP-KDKOLLI-UPD   TO MOD-KDKOLLI                           
043900       END-IF                                                             
044000     END-IF                                                               
044100                                                                          
044200     MOVE RESP-IDKOLLI-UPD-ATTR  TO MOD-IDKOLLI-IN-ATTR                   
044300     IF RESP-IDKOLLI-UPD = SPACE                                          
044400       MOVE MFS-RENSA-FAELT      TO MOD-IDKOLLI-IN                        
044500     ELSE                                                                 
044600       IF RESP-IDKOLLI-UPD = ALL '+'                                      
044700         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDKOLLI-IN                        
044800       ELSE                                                               
044900         MOVE RESP-IDKOLLI-UPD   TO MOD-IDKOLLI-IN                        
045000       END-IF                                                             
045100     END-IF                                                               
045200                                                                          
045300     MOVE RESP-VKKOLLIB-UPD-ATTR TO MOD-VKKOLLIB-IN-ATTR                  
045400     IF RESP-VKKOLLIB-UPD = SPACE                                         
045500       MOVE MFS-RENSA-FAELT      TO MOD-VKKOLLIB-IN                       
045600     ELSE                                                                 
045700       IF RESP-VKKOLLIB-UPD = ALL '+'                                     
045800         MOVE MFS-ROER-EJ-FAELT  TO MOD-VKKOLLIB-IN                       
045900       ELSE                                                               
046000         MOVE RESP-VKKOLLIB-UPD  TO MOD-VKKOLLIB-IN                       
046100       END-IF                                                             
046200     END-IF                                                               
046300                                                                          
046400     MOVE RESP-DIKOLLIL-UPD-ATTR TO MOD-DIKOLLIL-IN-ATTR                  
046500     IF RESP-DIKOLLIL-UPD = SPACE                                         
046600       MOVE MFS-RENSA-FAELT      TO MOD-DIKOLLIL-IN                       
046700     ELSE                                                                 
046800       IF RESP-DIKOLLIL-UPD = ALL '+'                                     
046900         MOVE MFS-ROER-EJ-FAELT  TO MOD-DIKOLLIL-IN                       
047000       ELSE                                                               
047100         MOVE RESP-DIKOLLIL-UPD  TO MOD-DIKOLLIL-IN                       
047200       END-IF                                                             
047300     END-IF                                                               
047400                                                                          
047500     MOVE RESP-DIKOLLIB-UPD-ATTR TO MOD-DIKOLLIB-IN-ATTR                  
047600     IF RESP-DIKOLLIB-UPD = SPACE                                         
047700       MOVE MFS-RENSA-FAELT      TO MOD-DIKOLLIB-IN                       
047800     ELSE                                                                 
047900       IF RESP-DIKOLLIB-UPD = ALL '+'                                     
048000         MOVE MFS-ROER-EJ-FAELT  TO MOD-DIKOLLIB-IN                       
048100       ELSE                                                               
048200         MOVE RESP-DIKOLLIB-UPD  TO MOD-DIKOLLIB-IN                       
048300       END-IF                                                             
048400     END-IF                                                               
048500                                                                          
048600     MOVE RESP-DIKOLLIH-UPD-ATTR TO MOD-DIKOLLIH-IN-ATTR                  
048700     IF RESP-DIKOLLIH-UPD = SPACE                                         
048800       MOVE MFS-RENSA-FAELT      TO MOD-DIKOLLIH-IN                       
048900     ELSE                                                                 
049000       IF RESP-DIKOLLIH-UPD = ALL '+'                                     
049100         MOVE MFS-ROER-EJ-FAELT  TO MOD-DIKOLLIH-IN                       
049200       ELSE                                                               
049300         MOVE RESP-DIKOLLIH-UPD  TO MOD-DIKOLLIH-IN                       
049400       END-IF                                                             
049500     END-IF                                                               
049600                                                                          
049700     MOVE RESP-KDCMD-UPD-ATTR    TO MOD-KDCMD-IN-ATTR                     
049800     IF RESP-KDCMD-UPD = SPACE                                            
049900       MOVE MFS-RENSA-FAELT      TO MOD-KDCMD-IN                          
050000     ELSE                                                                 
050100       IF RESP-KDCMD-UPD = ALL '+'                                        
050200         MOVE MFS-ROER-EJ-FAELT  TO MOD-KDCMD-IN                          
050300       ELSE                                                               
050400         MOVE RESP-KDCMD-UPD     TO MOD-KDCMD-IN                          
050500       END-IF                                                             
050600     END-IF                                                               
050700                                                                          
050800     MOVE RESP-KDPERSON-UPD-ATTR TO MOD-KDPERSON-ATTR                     
050900     IF RESP-KDPERSON-UPD = SPACE                                         
051000       MOVE MFS-RENSA-FAELT      TO MOD-KDPERSON                          
051100     ELSE                                                                 
051200       IF RESP-KDPERSON-UPD = ALL '+'                                     
051300         MOVE MFS-ROER-EJ-FAELT  TO MOD-KDPERSON                          
051400       ELSE                                                               
051500         MOVE RESP-KDPERSON-UPD  TO MOD-KDPERSON                          
051600       END-IF                                                             
051700     END-IF                                                               
051800                                                                          
051900     MOVE RESP-BEKRPACK-UPD-ATTR TO MOD-BEKRPACK-ATTR                     
052000     IF RESP-BEKRPACK-UPD = SPACE                                         
052100       MOVE MFS-RENSA-FAELT      TO MOD-BEKRPACK                          
052200     ELSE                                                                 
052300       IF RESP-BEKRPACK-UPD = ALL '+'                                     
052400         MOVE MFS-ROER-EJ-FAELT  TO MOD-BEKRPACK                          
052500       ELSE                                                               
052600         MOVE RESP-BEKRPACK-UPD  TO MOD-BEKRPACK                          
052700       END-IF                                                             
052800     END-IF                                                               
052900                                                                          
053000     MOVE RESP-KVKRPACK-UPD-ATTR TO MOD-KVKRPACK-IN-ATTR                  
053100     IF RESP-KVKRPACK-UPD = SPACE                                         
053200       MOVE MFS-RENSA-FAELT      TO MOD-KVKRPACK-IN                       
053300     ELSE                                                                 
053400       IF RESP-KVKRPACK-UPD = ALL '+'                                     
053500         MOVE MFS-ROER-EJ-FAELT  TO MOD-KVKRPACK-IN                       
053600       ELSE                                                               
053700         MOVE RESP-KVKRPACK-UPD  TO MOD-KVKRPACK-IN                       
053800       END-IF                                                             
053900     END-IF                                                               
054000                                                                          
054100     IF RESP-KVKRPACK-UT = SPACE                                          
054200       MOVE MFS-RENSA-FAELT      TO MOD-KVKRPACK-UT                       
054300     ELSE                                                                 
054400       IF RESP-KVKRPACK-UT = ALL '+'                                      
054500         MOVE MFS-ROER-EJ-FAELT  TO MOD-KVKRPACK-UT                       
054600       ELSE                                                               
054700         MOVE RESP-KVKRPACK-UT   TO MOD-KVKRPACK-UT                       
054800       END-IF                                                             
054900     END-IF                                                               
055000                                                                          
055100     IF RESP-TIKRPACK = SPACE                                             
055200       MOVE MFS-RENSA-FAELT      TO MOD-TIKRPACK                          
055300     ELSE                                                                 
055400       IF RESP-TIKRPACK = ALL '+'                                         
055500         MOVE MFS-ROER-EJ-FAELT  TO MOD-TIKRPACK                          
055600       ELSE                                                               
055700         MOVE RESP-TIKRPACK      TO MOD-TIKRPACK                          
055800       END-IF                                                             
055900     END-IF                                                               
056000     PERFORM                                                              
056100     VARYING INDX FROM +1 BY +1                                           
056200       UNTIL INDX > RESP-KVRADER                                          
056300       IF RESP-IDKOLLI-LINE (INDX) = SPACE                                
056400         MOVE MFS-RENSA-FAELT    TO MOD-IDKOLLI (INDX)                    
056500       ELSE                                                               
056600         IF RESP-IDKOLLI-LINE (INDX) = ALL '+'                            
056700           MOVE MFS-ROER-EJ-FAELT                                         
056800                                 TO MOD-IDKOLLI (INDX)                    
056900         ELSE                                                             
057000           MOVE RESP-IDKOLLI-LINE (INDX)                                  
057100                                 TO MOD-IDKOLLI (INDX)                    
057200         END-IF                                                           
057300       END-IF                                                             
057400       IF RESP-VKKOLLIB-LINE (INDX) = SPACE                               
057500         MOVE MFS-RENSA-FAELT    TO MOD-VKKOLLIB (INDX)                   
057600       ELSE                                                               
057700         IF RESP-VKKOLLIB-LINE (INDX) = ALL '+'                           
057800           MOVE MFS-ROER-EJ-FAELT                                         
057900                                 TO MOD-VKKOLLIB (INDX)                   
058000         ELSE                                                             
058100           MOVE RESP-VKKOLLIB-LINE (INDX)                                 
058200                                 TO MOD-VKKOLLIB (INDX)                   
058300         END-IF                                                           
058400       END-IF                                                             
058500       IF RESP-DIKOLLIL-LINE (INDX) = SPACE                               
058600         MOVE MFS-RENSA-FAELT    TO MOD-DIKOLLIL (INDX)                   
058700       ELSE                                                               
058800         IF RESP-DIKOLLIL-LINE (INDX) = ALL '+'                           
058900           MOVE MFS-ROER-EJ-FAELT                                         
059000                                 TO MOD-DIKOLLIL (INDX)                   
059100         ELSE                                                             
059200           MOVE RESP-DIKOLLIL-LINE (INDX)                                 
059300                                 TO MOD-DIKOLLIL (INDX)                   
059400         END-IF                                                           
059500       END-IF                                                             
059600       IF RESP-DIKOLLIB-LINE (INDX) = SPACE                               
059700         MOVE MFS-RENSA-FAELT    TO MOD-DIKOLLIB (INDX)                   
059800       ELSE                                                               
059900         IF RESP-DIKOLLIB-LINE (INDX) = ALL '+'                           
060000           MOVE MFS-ROER-EJ-FAELT                                         
060100                                 TO MOD-DIKOLLIB (INDX)                   
060200         ELSE                                                             
060300           MOVE RESP-DIKOLLIB-LINE (INDX)                                 
060400                                 TO MOD-DIKOLLIB (INDX)                   
060500         END-IF                                                           
060600       END-IF                                                             
060700       IF RESP-DIKOLLIH-LINE (INDX) = SPACE                               
060800         MOVE MFS-RENSA-FAELT    TO MOD-DIKOLLIH (INDX)                   
060900       ELSE                                                               
061000         IF RESP-DIKOLLIH-LINE (INDX) = ALL '+'                           
061100           MOVE MFS-ROER-EJ-FAELT                                         
061200                                 TO MOD-DIKOLLIH (INDX)                   
061300         ELSE                                                             
061400           MOVE RESP-DIKOLLIH-LINE (INDX)                                 
061500                                 TO MOD-DIKOLLIH (INDX)                   
061600         END-IF                                                           
061700       END-IF                                                             
061800     END-PERFORM                                                          
061900                                                                          
062000     PERFORM                                                              
062100     VARYING INDX FROM INDX BY +1                                         
062200       UNTIL INDX > MAX-INDX                                              
062300       MOVE MFS-RENSA-FAELT      TO MOD-IDKOLLI  (INDX)                   
062400                                    MOD-VKKOLLIB (INDX)                   
062500                                    MOD-DIKOLLIL (INDX)                   
062600                                    MOD-DIKOLLIB (INDX)                   
062700                                    MOD-DIKOLLIH (INDX)                   
062800     END-PERFORM                                                          
062900                                                                          
063000     .                                                                    
063100     EJECT                                                                
063200 MFS-RENSA-FAELT-IN SECTION.                                              
063300                                                                          
063400     MOVE MFS-RENSA-FAELT        TO MOD-KDKOLLI                           
063500                                    MOD-IDKOLLI-IN                        
063600                                    MOD-VKKOLLIB-IN                       
063700                                    MOD-DIKOLLIL-IN                       
063800                                    MOD-DIKOLLIB-IN                       
063900                                    MOD-DIKOLLIH-IN                       
064000                                    MOD-KDPERSON                          
064100                                    MOD-KVKRPACK-IN                       
064200                                    MOD-KDCMD-IN                          
064300     .                                                                    
064400     EJECT                                                                
064500 MFS-FORM-ATTR      SECTION.                                              
064600                                                                          
064700     MOVE MFS-FORMATETS-ATTR     TO MOD-KDKOLLI-ATTR                      
064800                                    MOD-IDKOLLI-IN-ATTR                   
064900                                    MOD-VKKOLLIB-IN-ATTR                  
065000                                    MOD-DIKOLLIL-IN-ATTR                  
065100                                    MOD-DIKOLLIB-IN-ATTR                  
065200                                    MOD-DIKOLLIH-IN-ATTR                  
065300                                    MOD-KDPERSON-ATTR                     
065400                                    MOD-KVKRPACK-IN-ATTR                  
065500                                    MOD-KDCMD-IN-ATTR                     
065600                                    MOD-BEKRPACK-ATTR                     
065700     .                                                                    
065800     EJECT                                                                
065900* --- IMS SEKTIONER ---                                                   
066000                                                                          
066100 IMS-GET-MSG SECTION.                                                     
066200     MOVE '  QC'                 TO GODK-STATUSKODER                      
066300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
066400     MOVE MSG-STATUS-CODE        TO STATUS-WS                             
066500     PERFORM IMS-STATUSKONTROLL                                           
066600     .                                                                    
066700     SKIP3                                                                
066800 IMS-INSERT-MSG SECTION.                                                  
066900     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
067000       MOVE '0'                  TO MFS-KDHUVOMR                          
067100     END-IF                                                               
067200     MOVE LOW-VALUE              TO MSG-KDZ1 MSG-KDZ2                     
067300     MOVE SPACE                  TO GODK-STATUSKODER                      
067400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
067500     MOVE MSG-STATUS-CODE        TO STATUS-WS                             
067600     PERFORM IMS-STATUSKONTROLL                                           
067700     .                                                                    
067800     EJECT                                                                
067900 IMS-STATUSKONTROLL SECTION.                                              
068000     SET STATUS-IX               TO 1                                     
068100     SEARCH GODK-STATUS                                                   
068200       AT END                                                             
068300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
068400         DELIMITED BY SIZE INTO FELTEXT                                   
068500         CALL FELLOG                                                      
068600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
068700     END-SEARCH                                                           
068800     .                                                                    
