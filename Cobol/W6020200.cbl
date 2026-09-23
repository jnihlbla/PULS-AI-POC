000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6020200.                                                
000400*AUTHOR.         INGER NILSSON / RAHUL REDDY.                             
000500*DATE-WRITTEN.   JUNE 2012.                                               
000600                                                                          
000700**   REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        KONTROLLRAPPORT REGISTRERA FEL.                                  
001100*                                                                         
001200*        PROGRAMMET LÄSER     WLLEVA (WDF1)                               
001300*        PROGRAMMET LÄSER     WLARTC (WDK6)                               
001400*        PROGRAMMET LÄSER     WLINLE (WDL2)                               
001500*        PROGRAMMET LÄSER     WLXXLA (WDG7)                               
001600*        PROGRAMMET LÄSER     WDP3                                        
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W6T202                                              
002000*        MID:         W6I20201                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        MOD:         W6O20201                                            
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100*    -- CHECKED BY WY2000                                                 
003200 77  IDPGM                       PIC X(08)   VALUE 'W6020200'.            
003300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003400                                                                          
003500 01  ALL-SPACE.                                                           
003600     03  FILLER                  PIC X(70)  VALUE SPACE.                  
003700 01  ALL-PLUS.                                                            
003800     03  FILLER                  PIC X(70)  VALUE ALL '+'.                
003900                                                                          
004000 01  W-MFSINF.                                                            
004100     03 FILLER                   PIC  X(30).                              
004200     03 W-MFSINF-IDKR            PIC  Z(05).                              
004300                                                                          
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  YES                         PIC X       VALUE 'Y'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700                                                                          
004800*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004900 77  WS-IDKR                     PIC 9(5)    VALUE ZERO.                  
005000                                                                          
005100 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005200     88  INDATA-OK                           VALUE 'J'.                   
005300     88  INDATA-FEL                          VALUE 'N'.                   
005400                                                                          
005500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005600     88  EGEN-MID                            VALUE '6202'.                
005700     88  GODK-MID                            VALUE '6202' '6203'          
005800                                                   '6204' '6205'          
005900                                                   '6206' '6207'          
006000                                                   '6208'                 
006100                                                   '2171'.                
006200     88  HELP-MID                            VALUE '0551'.                
006300 01  RKOD-ABEND-MED-DUMP         PIC S9(4)   VALUE +1000 COMP.            
006400     EJECT                                                                
006500 01  MESSAGE-CODES.                                                       
006600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '020'.                 
006700     EJECT                                                                
006800                                                                          
006900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007000 01  GENERELLA-SUBPROGRAM.                                                
007100     03  WL01MCNV                PIC X(8)    VALUE 'WL01MCNV'.            
007200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007600     03  W6020210                PIC X(8)    VALUE 'W6020210'.            
007700     EJECT                                                                
007800*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
007900*01 -COPY WMSGINIT                                                        
008000     SKIP3                                                                
008100     EJECT                                                                
008200*    ---  COPYTEXT FÖR TRANS TILL WL01MCNV                                
008300*01  -COPY WL01MCNV                                                       
008400     EJECT                                                                
008500 01  FILLER                      PIC X(16)  VALUE 'W6020210'.             
008600 01  REQU-AREA.                                                           
008700*    03 -COPY WZ01REQU                                                    
008800*    03 -COPY W60202I1                                                    
008900     EJECT                                                                
009000 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
009100 01  RESP-AREA.                                                           
009200*    03 -COPY WZ01RESP                                                    
009300*    03 -COPY W60202O1                                                    
009400     EJECT                                                                
009500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009600*                                                                         
009700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009800     SKIP3                                                                
009900*01  MID -COPY W6I20201                                                   
010000     EJECT                                                                
010100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010200     SKIP3                                                                
010300*01  -COPY WMSGAREA                                                       
010400     EJECT                                                                
010500     03  MOD REDEFINES MSG-AREA.                                          
010600*      05  -COPY W6O20201                                                 
010700     EJECT                                                                
010800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010900     SKIP3                                                                
011000*01  -COPY WMFSAREA                                                       
011100     EJECT                                                                
011200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011300*                                                                         
011400     EJECT                                                                
011500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011600     SKIP3                                                                
011700*    --- STATUS-KOD FRÅN IMS                                              
011800 01  STATUS-WS                   PIC XX.                                  
011900     88  SEGMENT-FINNS                       VALUE '  '.                  
012000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012200     SKIP2                                                                
012300 01  GODK-STATUSKODER.                                                    
012400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012500     SKIP3                                                                
012600 01  SSA1                        PIC X(128).                              
012700 01  SSA2                        PIC X(64).                               
012800 01  SSA3                        PIC X(64).                               
012900     EJECT                                                                
013000*    --- IMS FUNKTIONSKODER                                               
013100*01  -COPY W0003                                                          
013200     EJECT                                                                
013300 LINKAGE SECTION.                                                         
013400                                                                          
013500*01  -COPY W0009   -PRE MSG-                                              
013600     EJECT                                                                
013700 01  USEA-PCB                    PIC X.                                   
013800 01  KVAE-PCB                    PIC X.                                   
013900 01  KVAI-PCB                    PIC X.                                   
014000 01  INLC-PCB                    PIC X.                                   
014100 01  BENA-PCB                    PIC X.                                   
014200 01  LEVA-PCB                    PIC X.                                   
014300 01  ARTC-PCB                    PIC X.                                   
014400 01  XXLA-PCB                    PIC X.                                   
014500 01  WDP3-PCB                    PIC X.                                   
014600 01  LOPB-PCB                    PIC X.                                   
014700 01  INLE-PCB                    PIC X.                                   
014800 01  W6F1-PCB                    PIC X.                                   
014900 01  W6INLA-PCB                  PIC X.                                   
015000 01  UPFA-PCB                    PIC X.                                   
015100 01  WDB6-PCB                    PIC X.                                   
015110 01  WDK7-PCB                    PIC X.                                   
015120 01  WDL6-PCB                    PIC X.                                   
015200     EJECT                                                                
015300 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB KVAE-PCB KVAI-PCB             
015400     INLC-PCB BENA-PCB LEVA-PCB ARTC-PCB XXLA-PCB WDP3-PCB                
015500     LOPB-PCB INLE-PCB W6F1-PCB W6INLA-PCB UPFA-PCB WDB6-PCB              
015600     WDK7-PCB WDL6-PCB.                                                   
015700     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB KVAE-PCB KVAI-PCB             
015800     INLC-PCB BENA-PCB LEVA-PCB ARTC-PCB XXLA-PCB WDP3-PCB                
015900     LOPB-PCB INLE-PCB W6F1-PCB W6INLA-PCB UPFA-PCB WDB6-PCB              
015910     WDK7-PCB WDL6-PCB.                                                   
016000                                                                          
016100     PERFORM IMS-GET-MSG                                                  
016200     IF SEGMENT-FINNS                                                     
016300       PERFORM A-INIT                                                     
016400       PERFORM B-INIT-KEYS                                                
016500       PERFORM C-INIT-REQU                                                
016600       IF MFS-UPDATE OR MFS-UPD-X OR MFS-UPD-V                            
016700         IF MFS-UPDATE                                                    
016800           SET REQU-UPDATE       TO TRUE                                  
016900         ELSE                                                             
017000           IF MFS-UPD-X                                                   
017100             SET REQU-UPD-X      TO TRUE                                  
017200           ELSE                                                           
017300             IF MFS-UPD-V                                                 
017400               SET REQU-UPD-V    TO TRUE                                  
017500             END-IF                                                       
017600           END-IF                                                         
017700         END-IF                                                           
017800         IF MID-IDLOPNRM NOT = ALL '+'                                    
017900           IF MID-KVANTMOT = ALL '+' OR MID-KDKRUTF = '4'                 
018000             IF EGEN-MID                                                  
018100               MOVE NEJ          TO INDATA-SW                             
018200               MOVE MFS-NUM-FAELT-FEL                                     
018300                                 TO MOD-IDLOPNRM-ATTR                     
018400               MOVE ERR-CORR-HILITE-FLDS                                  
018500                                 TO RESP-IDMSG-ERROR                      
018600               MOVE SPACE        TO RESP-IDMSG-INFO                       
018700               MOVE SPACE        TO RESP-IDELMT-ERROR                     
018800               PERFORM MFS-ROER-EJ-FAELT-IN-UT                            
018900             END-IF                                                       
019000           END-IF                                                         
019100         END-IF                                                           
019200       ELSE                                                               
019300         IF MFS-FIRST                                                     
019400           SET REQU-FIRST        TO TRUE                                  
019500         ELSE                                                             
019600           SET REQU-QUERY        TO TRUE                                  
019700           PERFORM E-SAMMA-SIDA                                           
019800         END-IF                                                           
019900       END-IF                                                             
020000       IF INDATA-OK                                                       
020100         PERFORM F-CALL-BIZ-LOGIC-W6020210                                
020200       ELSE                                                               
020300         PERFORM FA-SET-MSG-AND-HILIGHT                                   
020400       END-IF                                                             
020500       IF NOT MFS-UPD-X                                                   
020600         COMPUTE MSG-KVLL = LENGTH OF MOD-W6O20201 + 4                    
020700         PERFORM IMS-INSERT-MSG                                           
020800       ELSE                                                               
020900         IF INDATA-OK                                                     
021000           CONTINUE                                                       
021100         ELSE                                                             
021200           CALL ABEND USING RKOD-ABEND-MED-DUMP                           
021300         END-IF                                                           
021400       END-IF                                                             
021500     END-IF                                                               
021600                                                                          
021700     MOVE ZERO                   TO RETURN-CODE                           
021800     GOBACK                                                               
021900     .                                                                    
022000     EJECT                                                                
022100 A-INIT SECTION.                                                          
022200                                                                          
022300     IF MSG-DUBBLA-TRANSKODER                                             
022400       MOVE MSG-INDATA-MINUS-2-TRANSKODER                                 
022500                                 TO MID-W6I20201                          
022600       MOVE MSG-IDTRANS-2        TO MFS-IDTRANS                           
022700       MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                          
022800     ELSE                                                                 
022900       MOVE MSG-INDATA-MINUS-1-TRANSKOD                                   
023000                                 TO MID-W6I20201                          
023100       MOVE MSG-IDTRANS-1        TO MFS-IDTRANS                           
023200       MOVE MSG-KDMFSFOR-1       TO MFS-KDMFSFOR                          
023300     END-IF                                                               
023400                                                                          
023500     MOVE MSG-KDTRTYP            TO MFS-KDTRTYP                           
023600     MOVE MSG-IDPFK              TO MFS-IDPFK                             
023700     MOVE MFS-IDTRANS            TO W-IDTRANS                             
023800                                                                          
023900     MOVE LOW-VALUE              TO MSG-AREA                              
024000     MOVE 'W6O202N1'             TO MFS-IDMOD                             
024100     MOVE '6202'                 TO MOD-IDTRANS                           
024200     MOVE MFS-RENSA-FAELT        TO MOD-TEMFSFEL MOD-TEMFSINF             
024300                                                                          
024400     IF EGEN-MID OR HELP-MID OR MFS-UPD-X                                 
024500       CONTINUE                                                           
024600     ELSE                                                                 
024700       MOVE SPACE                TO MFS-KDTRTYP                           
024800       MOVE '7'                  TO MFS-IDPFK                             
024900     END-IF                                                               
025000                                                                          
025100     PERFORM MFS-FORM-ATTR                                                
025200     PERFORM AA-INIT-NYCKLAR                                              
025300                                                                          
025400     MOVE '101'                  TO REQU-IDMSGVER                         
025500     MOVE MSGI-IDUSER            TO REQU-IDUSER                           
025600                                                                          
025700     MOVE MSGI-IDSPRAK           TO MCNV-IDSPRAK                          
025800                                    REQU-IDSPRAK                          
025900     .                                                                    
026000     EJECT                                                                
026100 AA-INIT-NYCKLAR SECTION.                                                 
026200                                                                          
026300     MOVE ALL '+'                TO MSGI-WMSGINIT                         
026400     MOVE '001'                  TO MSGI-KDCALL                           
026500     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
026600     CALL W005INIT            USING MSGI-WMSGINIT USEA-PCB                
026700     .                                                                    
026800     EJECT                                                                
026900 B-INIT-KEYS SECTION.                                                     
027000*    -- KONTROLL AV IDKR                                                  
027100     MOVE MFS-RENSA-FAELT        TO MOD-IDKR-IN                           
027200                                                                          
027300     IF MID-IDKR-IN = ALL '+'                                             
027400       MOVE MID-IDKR-UT          TO WS-IDKR                               
027500       INSPECT WS-IDKR REPLACING LEADING SPACE BY ZERO                    
027600     ELSE                                                                 
027700       MOVE MID-IDKR-IN          TO WS-IDKR                               
027800       MOVE '7'                  TO MFS-IDPFK                             
027900       MOVE SPACE                TO MFS-KDTRTYP                           
028000     END-IF                                                               
028100                                                                          
028200     MOVE WS-IDKR                TO REQU-IDKR-KEY                         
028300                                                                          
028400     MOVE MSGI-IDDC              TO REQU-IDDC-KEY                         
028500     IF GODK-MID                                                          
028600       MOVE WS-IDKR              TO MOD-IDKR-UT                           
028700       INSPECT MOD-IDKR-UT REPLACING LEADING ZERO BY SPACE                
028800     END-IF                                                               
028900     .                                                                    
029000     EJECT                                                                
029100 C-INIT-REQU SECTION.                                                     
029200                                                                          
029300     IF MID-IDLOPNRM = ALL '+'                                            
029400       MOVE ALL-PLUS             TO REQU-IDLOPNRM-UPD                     
029500     ELSE                                                                 
029600       MOVE MID-IDLOPNRM         TO REQU-IDLOPNRM-UPD                     
029700     END-IF                                                               
029800                                                                          
029900     IF MID-TIAVSDAT = ALL '+'                                            
030000       MOVE ALL-PLUS             TO REQU-TIAVSDAT-UPD                     
030100     ELSE                                                                 
030200       MOVE MID-TIAVSDAT         TO REQU-TIAVSDAT-UPD                     
030300     END-IF                                                               
030400                                                                          
030500     IF MID-IDARTNR = ALL '+'                                             
030600       MOVE ALL-PLUS             TO REQU-IDARTNR-UPD                      
030700     ELSE                                                                 
030800       MOVE MID-IDARTNR          TO REQU-IDARTNR-UPD                      
030900     END-IF                                                               
031000                                                                          
031100     IF MID-IDLEVNR = ALL '+'                                             
031200       MOVE ALL-PLUS             TO REQU-IDLEVNR-UPD                      
031300     ELSE                                                                 
031400       MOVE MID-IDLEVNR          TO REQU-IDLEVNR-UPD                      
031500     END-IF                                                               
031600                                                                          
031700     IF MID-IDLEVG = ALL '+'                                              
031800       MOVE ALL-PLUS             TO REQU-IDLEVG-UPD                       
031900     ELSE                                                                 
032000       MOVE MID-IDLEVG           TO REQU-IDLEVG-UPD                       
032100     END-IF                                                               
032200                                                                          
032300     IF MID-KVANTMOT = ALL '+'                                            
032400       MOVE ALL-PLUS             TO REQU-KVANTMOT-UPD                     
032500     ELSE                                                                 
032600       MOVE MID-KVANTMOT         TO REQU-KVANTMOT-UPD                     
032700     END-IF                                                               
032800                                                                          
032900     IF MID-KVART-RET = ALL '+'                                           
033000       MOVE ALL-PLUS             TO REQU-KVART-RET-UPD                    
033100     ELSE                                                                 
033200       MOVE MID-KVART-RET        TO REQU-KVART-RET-UPD                    
033300     END-IF                                                               
033400                                                                          
033500     IF MID-KVART-SKROT = ALL '+'                                         
033600       MOVE ALL-PLUS             TO REQU-KVART-SKROT-UPD                  
033700     ELSE                                                                 
033800       MOVE MID-KVART-SKROT      TO REQU-KVART-SKROT-UPD                  
033900     END-IF                                                               
034000                                                                          
034100     IF MID-KVART-SKROT-LDC = ALL '+'                                     
034200       MOVE ALL-PLUS             TO REQU-KVART-SKROT-LDC-UPD              
034300     ELSE                                                                 
034400       MOVE MID-KVART-SKROT-LDC  TO REQU-KVART-SKROT-LDC-UPD              
034500     END-IF                                                               
034600                                                                          
034700     IF MID-KVART-KONTR = ALL '+'                                         
034800       MOVE ALL-PLUS             TO REQU-KVART-KONTR-UPD                  
034900     ELSE                                                                 
035000       MOVE MID-KVART-KONTR      TO REQU-KVART-KONTR-UPD                  
035100     END-IF                                                               
035200                                                                          
035300     IF MID-KVART-KJUST = ALL '+'                                         
035400       MOVE ALL-PLUS             TO REQU-KVART-KJUST-UPD                  
035500     ELSE                                                                 
035600       MOVE MID-KVART-KJUST      TO REQU-KVART-KJUST-UPD                  
035700     END-IF                                                               
035800                                                                          
035900     IF MID-KVART-EJ-GODK = ALL '+'                                       
036000       MOVE ALL-PLUS             TO REQU-KVART-EJ-GODK-UPD                
036100     ELSE                                                                 
036200       MOVE MID-KVART-EJ-GODK    TO REQU-KVART-EJ-GODK-UPD                
036300     END-IF                                                               
036400                                                                          
036500     IF MID-KVART-BEH = ALL '+'                                           
036600       MOVE ALL-PLUS             TO REQU-KVART-BEH-UPD                    
036700     ELSE                                                                 
036800       MOVE MID-KVART-BEH        TO REQU-KVART-BEH-UPD                    
036900     END-IF                                                               
037000                                                                          
037100     IF MID-KVART-SJUST = ALL '+'                                         
037200       MOVE ALL-PLUS             TO REQU-KVART-SJUST-UPD                  
037300     ELSE                                                                 
037400       MOVE MID-KVART-SJUST      TO REQU-KVART-SJUST-UPD                  
037500     END-IF                                                               
037600                                                                          
037700     IF MID-IDKRFEL = ALL '+'                                             
037800       MOVE ALL-PLUS             TO REQU-IDKRFEL-UPD                      
037900     ELSE                                                                 
038000       MOVE MID-IDKRFEL          TO REQU-IDKRFEL-UPD                      
038100     END-IF                                                               
038200                                                                          
038300     IF MID-FLKVALSP = ALL '+'                                            
038400       MOVE ALL-PLUS             TO REQU-FLKVALSP-UPD                     
038500     ELSE                                                                 
038600       MOVE MID-FLKVALSP         TO REQU-FLKVALSP-UPD                     
038700     END-IF                                                               
038800                                                                          
038900     IF MID-KDDISP = ALL '+'                                              
039000       MOVE ALL-PLUS             TO REQU-KDDISP-UPD                       
039100     ELSE                                                                 
039200       MOVE MID-KDDISP           TO REQU-KDDISP-UPD                       
039300     END-IF                                                               
039400                                                                          
039500     IF MID-FLBUFJUS = ALL '+'                                            
039600       MOVE ALL-PLUS             TO REQU-FLBUFJUS-UPD                     
039700     ELSE                                                                 
039800       MOVE MID-FLBUFJUS         TO REQU-FLBUFJUS-UPD                     
039900     END-IF                                                               
040000                                                                          
040100     IF MID-KDHANDCO = ALL '+'                                            
040200       MOVE ALL-PLUS             TO REQU-KDHANDCO-UPD                     
040300     ELSE                                                                 
040400       MOVE MID-KDHANDCO         TO REQU-KDHANDCO-UPD                     
040500     END-IF                                                               
040600                                                                          
040700     IF MID-KDPERSON = ALL '+'                                            
040800       MOVE ALL-PLUS             TO REQU-KDPERSON-UPD                     
040900     ELSE                                                                 
041000       MOVE MID-KDPERSON         TO REQU-KDPERSON-UPD                     
041100     END-IF                                                               
041200                                                                          
041300     IF MID-BEKRBEH = ALL '+'                                             
041400       MOVE ALL-PLUS             TO REQU-BEKRBEH-UPD                      
041500     ELSE                                                                 
041600       MOVE MID-BEKRBEH          TO REQU-BEKRBEH-UPD                      
041700     END-IF                                                               
041800                                                                          
041900     IF MID-KVKRBEH = ALL '+'                                             
042000       MOVE ALL-PLUS             TO REQU-KVKRBEH-UPD                      
042100     ELSE                                                                 
042200       MOVE MID-KVKRBEH          TO REQU-KVKRBEH-UPD                      
042300     END-IF                                                               
042400                                                                          
042500     IF MID-TEKRPLT = ALL '+'                                             
042600       MOVE ALL-PLUS             TO REQU-TEKRPLT-UPD                      
042700     ELSE                                                                 
042800       MOVE MID-TEKRPLT          TO REQU-TEKRPLT-UPD                      
042900     END-IF                                                               
043000                                                                          
043100     IF MID-KDKRUTF = ALL '+'                                             
043200       MOVE ALL-PLUS             TO REQU-KDKRUTF-UPD                      
043300     ELSE                                                                 
043400       MOVE MID-KDKRUTF          TO REQU-KDKRUTF-UPD                      
043500     END-IF                                                               
043600                                                                          
043700     .                                                                    
043800     EJECT                                                                
043900 E-SAMMA-SIDA SECTION.                                                    
044000                                                                          
044100     IF EGEN-MID OR HELP-MID                                              
044200       CONTINUE                                                           
044300     ELSE                                                                 
044400       PERFORM MFS-RENSA-FAELT-IN                                         
044500     END-IF                                                               
044600     .                                                                    
044700     EJECT                                                                
044800 F-CALL-BIZ-LOGIC-W6020210 SECTION.                                       
044900                                                                          
045000     CALL W6020210 USING REQU-AREA RESP-AREA                              
045100                         KVAE-PCB KVAI-PCB INLC-PCB BENA-PCB              
045200                         LEVA-PCB ARTC-PCB XXLA-PCB WDP3-PCB              
045300                         LOPB-PCB INLE-PCB W6F1-PCB W6INLA-PCB            
045400                         UPFA-PCB WDB6-PCB WDK7-PCB WDL6-PCB              
045500                                                                          
045600     IF RESP-IDMSG-ERROR NOT = SPACE OR                                   
045700        RESP-IDMSG-INFO  NOT = SPACE                                      
045800       PERFORM FA-SET-MSG-AND-HILIGHT                                     
045900     END-IF                                                               
046000     PERFORM FB-MOVE-RESP-TO-MOD                                          
046100     .                                                                    
046200     EJECT                                                                
046300 FA-SET-MSG-AND-HILIGHT SECTION.                                          
046400                                                                          
046500     MOVE RESP-IDMSG-ERROR       TO MCNV-IDMSG-ERROR                      
046600     MOVE RESP-IDMSG-INFO        TO MCNV-IDMSG-INFO                       
046700     MOVE RESP-IDELMT-ERROR      TO MCNV-IDELMT-ERROR                     
046800                                                                          
046900     CALL WL01MCNV USING MCNV-AREA                                        
047000     IF RESP-IDMSG-INFO = '351' OR '352'                                  
047100       MOVE MCNV-MFSINF          TO W-MFSINF                              
047200       MOVE RESP-IDKR-INFO-MSG   TO W-MFSINF-IDKR                         
047300       MOVE W-MFSINF             TO MOD-TEMFSINF                          
047400     ELSE                                                                 
047500       MOVE MCNV-MFSINF          TO MOD-TEMFSINF                          
047600     END-IF                                                               
047700     MOVE MCNV-MFSFEL            TO MOD-TEMFSFEL                          
047800     .                                                                    
047900     EJECT                                                                
048000 FB-MOVE-RESP-TO-MOD SECTION.                                             
048100                                                                          
048200     IF RESP-IDKR-KEY = SPACE                                             
048300       MOVE MFS-RENSA-FAELT      TO MOD-IDKR-UT                           
048400     ELSE                                                                 
048500       IF RESP-IDKR-KEY = ALL '+'                                         
048600         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDKR-UT                           
048700       ELSE                                                               
048800         MOVE RESP-IDKR-KEY      TO MOD-IDKR-UT                           
048900       END-IF                                                             
049000     END-IF                                                               
049100     INSPECT MOD-IDKR-UT REPLACING LEADING ZERO BY SPACE                  
049200                                                                          
049300     MOVE RESP-IDLOPNRM-UPD-ATTR TO MOD-IDLOPNRM-ATTR                     
049400                                                                          
049500     IF RESP-IDLOPNRM-UPD = SPACE                                         
049600       MOVE MFS-RENSA-FAELT      TO MOD-IDLOPNRM-IN                       
049700     ELSE                                                                 
049800       IF RESP-IDLOPNRM-UPD = ALL '+'                                     
049900         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDLOPNRM-IN                       
050000       ELSE                                                               
050100         MOVE RESP-IDLOPNRM-UPD  TO MOD-IDLOPNRM-IN                       
050200       END-IF                                                             
050300     END-IF                                                               
050400                                                                          
050500     IF RESP-IDLOPNRM-UT = SPACE                                          
050600       MOVE MFS-RENSA-FAELT      TO MOD-IDLOPNRM-UT                       
050700     ELSE                                                                 
050800       IF RESP-IDLOPNRM-UT = ALL '+'                                      
050900         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDLOPNRM-UT                       
051000       ELSE                                                               
051100         MOVE RESP-IDLOPNRM-UT   TO MOD-IDLOPNRM-UT                       
051200       END-IF                                                             
051300     END-IF                                                               
051400                                                                          
051500     IF RESP-IDAVINR = SPACE                                              
051600       MOVE MFS-RENSA-FAELT      TO MOD-IDAVINR                           
051700     ELSE                                                                 
051800       IF RESP-IDAVINR = ALL '+'                                          
051900         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDAVINR                           
052000       ELSE                                                               
052100         MOVE RESP-IDAVINR       TO MOD-IDAVINR                           
052200       END-IF                                                             
052300     END-IF                                                               
052400                                                                          
052500     IF RESP-KVAVIS = SPACE                                               
052600       MOVE MFS-RENSA-FAELT      TO MOD-KVAVIS                            
052700     ELSE                                                                 
052800       IF RESP-KVAVIS = ALL '+'                                           
052900         MOVE MFS-ROER-EJ-FAELT  TO MOD-KVAVIS                            
053000       ELSE                                                               
053100         MOVE RESP-KVAVIS        TO MOD-KVAVIS                            
053200       END-IF                                                             
053300     END-IF                                                               
053400                                                                          
053500     IF RESP-TIAVSDAT-UPD = SPACE                                         
053600       MOVE MFS-RENSA-FAELT      TO MOD-TIAVSDAT                          
053700     ELSE                                                                 
053800       IF RESP-TIAVSDAT-UPD = ALL '+'                                     
053900         MOVE MFS-ROER-EJ-FAELT  TO MOD-TIAVSDAT                          
054000       ELSE                                                               
054100         MOVE RESP-TIAVSDAT-UPD  TO MOD-TIAVSDAT                          
054200       END-IF                                                             
054300     END-IF                                                               
054400                                                                          
054500     MOVE RESP-IDARTNR-UPD-ATTR  TO MOD-IDARTNR-ATTR                      
054600                                                                          
054700     IF RESP-IDARTNR-UPD = SPACE                                          
054800       MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR-IN                        
054900     ELSE                                                                 
055000       IF RESP-IDARTNR-UPD = ALL '+'                                      
055100         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDARTNR-IN                        
055200       ELSE                                                               
055300         MOVE RESP-IDARTNR-UPD   TO MOD-IDARTNR-IN                        
055400       END-IF                                                             
055500     END-IF                                                               
055600                                                                          
055700     IF RESP-IDARTNR-UT = SPACE                                           
055800       MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR-UT                        
055900     ELSE                                                                 
056000       IF RESP-IDARTNR-UT = ALL '+'                                       
056100         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDARTNR-UT                        
056200       ELSE                                                               
056300         MOVE RESP-IDARTNR-UT    TO MOD-IDARTNR-UT                        
056400       END-IF                                                             
056500     END-IF                                                               
056600                                                                          
056700     IF RESP-BEART = SPACE                                                
056800       MOVE MFS-RENSA-FAELT      TO MOD-BEART                             
056900     ELSE                                                                 
057000       IF RESP-BEART = ALL '+'                                            
057100         MOVE MFS-ROER-EJ-FAELT  TO MOD-BEART                             
057200       ELSE                                                               
057300         MOVE RESP-BEART         TO MOD-BEART                             
057400       END-IF                                                             
057500     END-IF                                                               
057600                                                                          
057700     IF RESP-TIREGDAT = SPACE                                             
057800       MOVE MFS-RENSA-FAELT      TO MOD-TIREGDAT                          
057900     ELSE                                                                 
058000       IF RESP-TIREGDAT = ALL '+'                                         
058100         MOVE MFS-ROER-EJ-FAELT  TO MOD-TIREGDAT                          
058200       ELSE                                                               
058300         MOVE RESP-TIREGDAT      TO MOD-TIREGDAT                          
058400       END-IF                                                             
058500     END-IF                                                               
058600                                                                          
058700     MOVE RESP-IDLEVNR-UPD-ATTR  TO MOD-IDLEVNR-ATTR                      
058800                                                                          
058900     IF RESP-IDLEVNR-UPD = SPACE                                          
059000       MOVE MFS-RENSA-FAELT      TO MOD-IDLEVNR-IN                        
059100     ELSE                                                                 
059200       IF RESP-IDLEVNR-UPD = ALL '+'                                      
059300         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDLEVNR-IN                        
059400       ELSE                                                               
059500         MOVE RESP-IDLEVNR-UPD   TO MOD-IDLEVNR-IN                        
059600       END-IF                                                             
059700     END-IF                                                               
059800                                                                          
059900     IF RESP-IDLEVNR-UT = SPACE                                           
060000       MOVE MFS-RENSA-FAELT      TO MOD-IDLEVNR-UT                        
060100     ELSE                                                                 
060200       IF RESP-IDLEVNR-UT = ALL '+'                                       
060300         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDLEVNR-UT                        
060400       ELSE                                                               
060500         MOVE RESP-IDLEVNR-UT    TO MOD-IDLEVNR-UT                        
060600       END-IF                                                             
060700     END-IF                                                               
060800                                                                          
060900     MOVE RESP-IDLEVG-UPD-ATTR   TO MOD-IDLEVG-ATTR                       
061000                                                                          
061100     IF RESP-IDLEVG-UPD = SPACE                                           
061200       MOVE MFS-RENSA-FAELT      TO MOD-IDLEVG-IN                         
061300     ELSE                                                                 
061400       IF RESP-IDLEVG-UPD = ALL '+'                                       
061500         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDLEVG-IN                         
061600       ELSE                                                               
061700         MOVE RESP-IDLEVG-UPD    TO MOD-IDLEVG-IN                         
061800       END-IF                                                             
061900     END-IF                                                               
062000                                                                          
062100     IF RESP-IDLEVG-UT = SPACE                                            
062200       MOVE MFS-RENSA-FAELT      TO MOD-IDLEVG-UT                         
062300     ELSE                                                                 
062400       IF RESP-IDLEVG-UT = ALL '+'                                        
062500         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDLEVG-UT                         
062600       ELSE                                                               
062700         MOVE RESP-IDLEVG-UT     TO MOD-IDLEVG-UT                         
062800       END-IF                                                             
062900     END-IF                                                               
063000                                                                          
063100     IF RESP-BELEV = SPACE                                                
063200       MOVE MFS-RENSA-FAELT      TO MOD-BELEV                             
063300     ELSE                                                                 
063400       IF RESP-BELEV = ALL '+'                                            
063500         MOVE MFS-ROER-EJ-FAELT  TO MOD-BELEV                             
063600       ELSE                                                               
063700         MOVE RESP-BELEV         TO MOD-BELEV                             
063800       END-IF                                                             
063900     END-IF                                                               
064000                                                                          
064100     IF RESP-KDKRSTA = SPACE                                              
064200       MOVE MFS-RENSA-FAELT      TO MOD-KDKRSTA                           
064300     ELSE                                                                 
064400       IF RESP-KDKRSTA = ALL '+'                                          
064500         MOVE MFS-ROER-EJ-FAELT  TO MOD-KDKRSTA                           
064600       ELSE                                                               
064700         MOVE RESP-KDKRSTA       TO MOD-KDKRSTA                           
064800       END-IF                                                             
064900     END-IF                                                               
065000                                                                          
065100     MOVE RESP-KVANTMOT-UPD-ATTR TO MOD-KVANTMOT-ATTR                     
065200                                                                          
065300     IF RESP-KVANTMOT-UPD = SPACE                                         
065400       MOVE MFS-RENSA-FAELT      TO MOD-KVANTMOT-IN                       
065500     ELSE                                                                 
065600       IF RESP-KVANTMOT-UPD = ALL '+'                                     
065700         MOVE MFS-ROER-EJ-FAELT  TO MOD-KVANTMOT-IN                       
065800       ELSE                                                               
065900         MOVE RESP-KVANTMOT-UPD  TO MOD-KVANTMOT-IN                       
066000       END-IF                                                             
066100     END-IF                                                               
066200                                                                          
066300     IF RESP-KVANTMOT-UT = SPACE                                          
066400       MOVE MFS-RENSA-FAELT      TO MOD-KVANTMOT-UT                       
066500     ELSE                                                                 
066600       IF RESP-KVANTMOT-UT = ALL '+'                                      
066700         MOVE MFS-ROER-EJ-FAELT  TO MOD-KVANTMOT-UT                       
066800       ELSE                                                               
066900         MOVE RESP-KVANTMOT-UT   TO MOD-KVANTMOT-UT                       
067000       END-IF                                                             
067100     END-IF                                                               
067200                                                                          
067300     MOVE RESP-KVART-RET-UPD-ATTR                                         
067400                                 TO MOD-KVART-RET-ATTR                    
067500                                                                          
067600     IF RESP-KVART-RET-UPD = SPACE                                        
067700       MOVE MFS-RENSA-FAELT      TO MOD-KVART-RET-IN                      
067800     ELSE                                                                 
067900       IF RESP-KVART-RET-UPD = ALL '+'                                    
068000         MOVE MFS-ROER-EJ-FAELT  TO MOD-KVART-RET-IN                      
068100       ELSE                                                               
068200         MOVE RESP-KVART-RET-UPD TO MOD-KVART-RET-IN                      
068300       END-IF                                                             
068400     END-IF                                                               
068500                                                                          
068600     IF RESP-KVART-RET-UT = SPACE                                         
068700       MOVE MFS-RENSA-FAELT      TO MOD-KVART-RET-UT                      
068800     ELSE                                                                 
068900       IF RESP-KVART-RET-UT = ALL '+'                                     
069000         MOVE MFS-ROER-EJ-FAELT  TO MOD-KVART-RET-UT                      
069100       ELSE                                                               
069200         MOVE RESP-KVART-RET-UT  TO MOD-KVART-RET-UT                      
069300       END-IF                                                             
069400     END-IF                                                               
069500                                                                          
069600     IF RESP-KVART-AAVV = SPACE                                           
069700       MOVE MFS-RENSA-FAELT      TO MOD-KVART-AAVV-UT                     
069800     ELSE                                                                 
069900       IF RESP-KVART-AAVV = ALL '+'                                       
070000         MOVE MFS-ROER-EJ-FAELT  TO MOD-KVART-AAVV-UT                     
070100       ELSE                                                               
070200         MOVE RESP-KVART-AAVV    TO MOD-KVART-AAVV-UT                     
070300       END-IF                                                             
070400     END-IF                                                               
070500                                                                          
070600     MOVE RESP-KVART-SKROT-UPD-ATTR                                       
070700                                 TO MOD-KVART-SKROT-ATTR                  
070800                                                                          
070900     IF RESP-KVART-SKROT-UPD = SPACE                                      
071000       MOVE MFS-RENSA-FAELT      TO MOD-KVART-SKROT-IN                    
071100     ELSE                                                                 
071200       IF RESP-KVART-SKROT-UPD = ALL '+'                                  
071300         MOVE MFS-ROER-EJ-FAELT  TO MOD-KVART-SKROT-IN                    
071400       ELSE                                                               
071500         MOVE RESP-KVART-SKROT-UPD                                        
071600                                 TO MOD-KVART-SKROT-IN                    
071700       END-IF                                                             
071800     END-IF                                                               
071900                                                                          
072000     IF RESP-KVART-SKROT-UT = SPACE                                       
072100       MOVE MFS-RENSA-FAELT      TO MOD-KVART-SKROT-UT                    
072200     ELSE                                                                 
072300       IF RESP-KVART-SKROT-UT = ALL '+'                                   
072400         MOVE MFS-ROER-EJ-FAELT  TO MOD-KVART-SKROT-UT                    
072500       ELSE                                                               
072600         MOVE RESP-KVART-SKROT-UT                                         
072700                                 TO MOD-KVART-SKROT-UT                    
072800       END-IF                                                             
072900     END-IF                                                               
073000                                                                          
073100     MOVE RESP-KVART-SKROT-LDC-UPD-ATTR                                   
073200                                 TO MOD-KVART-SKROT-LDC-ATTR              
073300                                                                          
073400     IF RESP-KVART-SKROT-LDC-UPD = SPACE                                  
073500       MOVE MFS-RENSA-FAELT      TO MOD-KVART-SKROT-LDC-IN                
073600     ELSE                                                                 
073700       IF RESP-KVART-SKROT-LDC-UPD = ALL '+'                              
073800         MOVE MFS-ROER-EJ-FAELT  TO MOD-KVART-SKROT-LDC-IN                
073900       ELSE                                                               
074000         MOVE RESP-KVART-SKROT-LDC-UPD                                    
074100                                 TO MOD-KVART-SKROT-LDC-IN                
074200       END-IF                                                             
074300     END-IF                                                               
074400                                                                          
074500     IF RESP-KVART-SKROT-LDC-UT = SPACE                                   
074600       MOVE MFS-RENSA-FAELT      TO MOD-KVART-SKROT-LDC-UT                
074700     ELSE                                                                 
074800       IF RESP-KVART-SKROT-LDC-UT = ALL '+'                               
074900         MOVE MFS-ROER-EJ-FAELT  TO MOD-KVART-SKROT-LDC-UT                
075000       ELSE                                                               
075100         MOVE RESP-KVART-SKROT-LDC-UT                                     
075200                                 TO MOD-KVART-SKROT-LDC-UT                
075300       END-IF                                                             
075400     END-IF                                                               
075500                                                                          
075600     MOVE RESP-KVART-KJUST-UPD-ATTR                                       
075700                                 TO MOD-KVART-KJUST-ATTR                  
075800                                                                          
075900     IF RESP-KVART-KJUST-UPD = SPACE                                      
076000       MOVE MFS-RENSA-FAELT      TO MOD-KVART-KJUST-IN                    
076100     ELSE                                                                 
076200       IF RESP-KVART-KJUST-UPD = ALL '+'                                  
076300         MOVE MFS-ROER-EJ-FAELT  TO MOD-KVART-KJUST-IN                    
076400       ELSE                                                               
076500         MOVE RESP-KVART-KJUST-UPD                                        
076600                                 TO MOD-KVART-KJUST-IN                    
076700       END-IF                                                             
076800     END-IF                                                               
076900                                                                          
077000     IF RESP-KVART-KJUST-UT = SPACE                                       
077100       MOVE MFS-RENSA-FAELT      TO MOD-KVART-KJUST-UT                    
077200     ELSE                                                                 
077300       IF RESP-KVART-KJUST-UT = ALL '+'                                   
077400         MOVE MFS-ROER-EJ-FAELT  TO MOD-KVART-KJUST-UT                    
077500       ELSE                                                               
077600         MOVE RESP-KVART-KJUST-UT                                         
077700                                 TO MOD-KVART-KJUST-UT                    
077800       END-IF                                                             
077900     END-IF                                                               
078000                                                                          
078100     MOVE RESP-KVART-KONTR-UPD-ATTR                                       
078200                                 TO MOD-KVART-KONTR-ATTR                  
078300                                                                          
078400     IF RESP-KVART-KONTR-UPD = SPACE                                      
078500       MOVE MFS-RENSA-FAELT      TO MOD-KVART-KONTR-IN                    
078600     ELSE                                                                 
078700       IF RESP-KVART-KONTR-UPD = ALL '+'                                  
078800         MOVE MFS-ROER-EJ-FAELT  TO MOD-KVART-KONTR-IN                    
078900       ELSE                                                               
079000         MOVE RESP-KVART-KONTR-UPD                                        
079100                                 TO MOD-KVART-KONTR-IN                    
079200       END-IF                                                             
079300     END-IF                                                               
079400                                                                          
079500     IF RESP-KVART-KONTR-UT = SPACE                                       
079600       MOVE MFS-RENSA-FAELT      TO MOD-KVART-KONTR-UT                    
079700     ELSE                                                                 
079800       IF RESP-KVART-KONTR-UT = ALL '+'                                   
079900         MOVE MFS-ROER-EJ-FAELT  TO MOD-KVART-KONTR-UT                    
080000       ELSE                                                               
080100         MOVE RESP-KVART-KONTR-UT                                         
080200                                 TO MOD-KVART-KONTR-UT                    
080300       END-IF                                                             
080400     END-IF                                                               
080500                                                                          
080600     MOVE RESP-KVART-BEH-UPD-ATTR                                         
080700                                 TO MOD-KVART-BEH-ATTR                    
080800                                                                          
080900     IF RESP-KVART-BEH-UPD = SPACE                                        
081000       MOVE MFS-RENSA-FAELT      TO MOD-KVART-BEH-IN                      
081100     ELSE                                                                 
081200       IF RESP-KVART-BEH-UPD = ALL '+'                                    
081300         MOVE MFS-ROER-EJ-FAELT  TO MOD-KVART-BEH-IN                      
081400       ELSE                                                               
081500         MOVE RESP-KVART-BEH-UPD TO MOD-KVART-BEH-IN                      
081600       END-IF                                                             
081700     END-IF                                                               
081800                                                                          
081900     IF RESP-KVART-BEH-UT = SPACE                                         
082000       MOVE MFS-RENSA-FAELT      TO MOD-KVART-BEH-UT                      
082100     ELSE                                                                 
082200       IF RESP-KVART-BEH-UT = ALL '+'                                     
082300         MOVE MFS-ROER-EJ-FAELT  TO MOD-KVART-BEH-UT                      
082400       ELSE                                                               
082500         MOVE RESP-KVART-BEH-UT  TO MOD-KVART-BEH-UT                      
082600       END-IF                                                             
082700     END-IF                                                               
082800                                                                          
082900     MOVE RESP-KVART-EJ-GODK-UPD-ATTR                                     
083000                                 TO MOD-KVART-EJ-GODK-ATTR                
083100                                                                          
083200     IF RESP-KVART-EJ-GODK-UPD = SPACE                                    
083300       MOVE MFS-RENSA-FAELT      TO MOD-KVART-EJ-GODK-IN                  
083400     ELSE                                                                 
083500       IF RESP-KVART-EJ-GODK-UPD = ALL '+'                                
083600         MOVE MFS-ROER-EJ-FAELT  TO MOD-KVART-EJ-GODK-IN                  
083700       ELSE                                                               
083800         MOVE RESP-KVART-EJ-GODK-UPD                                      
083900                                 TO MOD-KVART-EJ-GODK-IN                  
084000       END-IF                                                             
084100     END-IF                                                               
084200                                                                          
084300     IF RESP-KVART-EJ-GODK-UT = SPACE                                     
084400       MOVE MFS-RENSA-FAELT      TO MOD-KVART-EJ-GODK-UT                  
084500     ELSE                                                                 
084600       IF RESP-KVART-EJ-GODK-UT = ALL '+'                                 
084700         MOVE MFS-ROER-EJ-FAELT  TO MOD-KVART-EJ-GODK-UT                  
084800       ELSE                                                               
084900         MOVE RESP-KVART-EJ-GODK-UT                                       
085000                                 TO MOD-KVART-EJ-GODK-UT                  
085100       END-IF                                                             
085200     END-IF                                                               
085300                                                                          
085400     MOVE RESP-KVART-SJUST-UPD-ATTR                                       
085500                                 TO MOD-KVART-SJUST-ATTR                  
085600                                                                          
085700     IF RESP-KVART-SJUST-UPD = SPACE                                      
085800       MOVE MFS-RENSA-FAELT      TO MOD-KVART-SJUST-IN                    
085900     ELSE                                                                 
086000       IF RESP-KVART-SJUST-UPD = ALL '+'                                  
086100         MOVE MFS-ROER-EJ-FAELT  TO MOD-KVART-SJUST-IN                    
086200       ELSE                                                               
086300         MOVE RESP-KVART-SJUST-UPD                                        
086400                                 TO MOD-KVART-SJUST-IN                    
086500       END-IF                                                             
086600     END-IF                                                               
086700                                                                          
086800     IF RESP-KVART-SJUST-UT = SPACE                                       
086900       MOVE MFS-RENSA-FAELT      TO MOD-KVART-SJUST-UT                    
087000     ELSE                                                                 
087100       IF RESP-KVART-SJUST-UT = ALL '+'                                   
087200         MOVE MFS-ROER-EJ-FAELT  TO MOD-KVART-SJUST-UT                    
087300       ELSE                                                               
087400         MOVE RESP-KVART-SJUST-UT                                         
087500                                 TO MOD-KVART-SJUST-UT                    
087600       END-IF                                                             
087700     END-IF                                                               
087800                                                                          
087900     MOVE RESP-IDKRFEL-UPD-ATTR                                           
088000                                 TO MOD-IDKRFEL-ATTR                      
088100                                                                          
088200     IF RESP-IDKRFEL-UPD = SPACE                                          
088300       MOVE MFS-RENSA-FAELT      TO MOD-IDKRFEL                           
088400     ELSE                                                                 
088500       IF RESP-IDKRFEL-UPD = ALL '+'                                      
088600         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDKRFEL                           
088700       ELSE                                                               
088800         MOVE RESP-IDKRFEL-UPD   TO MOD-IDKRFEL                           
088900       END-IF                                                             
089000     END-IF                                                               
089100                                                                          
089200     IF RESP-BEKRFEL = SPACE                                              
089300       MOVE MFS-RENSA-FAELT      TO MOD-BEKRFEL                           
089400     ELSE                                                                 
089500       IF RESP-BEKRFEL = ALL '+'                                          
089600         MOVE MFS-ROER-EJ-FAELT  TO MOD-BEKRFEL                           
089700       ELSE                                                               
089800         MOVE RESP-BEKRFEL       TO MOD-BEKRFEL                           
089900       END-IF                                                             
090000     END-IF                                                               
090100                                                                          
090200     MOVE RESP-FLKVALSP-UPD-ATTR                                          
090300                                 TO MOD-FLKVALSP-ATTR                     
090400                                                                          
090500     IF RESP-FLKVALSP-UPD = SPACE                                         
090600       MOVE MFS-RENSA-FAELT      TO MOD-FLKVALSP-IN                       
090700     ELSE                                                                 
090800       IF RESP-FLKVALSP-UPD = ALL '+'                                     
090900         MOVE MFS-ROER-EJ-FAELT  TO MOD-FLKVALSP-IN                       
091000       ELSE                                                               
091100         MOVE RESP-FLKVALSP-UPD  TO MOD-FLKVALSP-IN                       
091200       END-IF                                                             
091300     END-IF                                                               
091400                                                                          
091500     IF RESP-FLKVALSP-UT = SPACE                                          
091600       MOVE MFS-RENSA-FAELT      TO MOD-FLKVALSP-UT                       
091700     ELSE                                                                 
091800       IF RESP-FLKVALSP-UT = ALL '+'                                      
091900         MOVE MFS-ROER-EJ-FAELT  TO MOD-FLKVALSP-UT                       
092000       ELSE                                                               
092100         MOVE RESP-FLKVALSP-UT   TO MOD-FLKVALSP-UT                       
092200       END-IF                                                             
092300     END-IF                                                               
092400                                                                          
092500     MOVE RESP-KDDISP-UPD-ATTR                                            
092600                                 TO MOD-KDDISP-ATTR                       
092700                                                                          
092800     IF RESP-KDDISP-UPD = SPACE                                           
092900       MOVE MFS-RENSA-FAELT      TO MOD-KDDISP                            
093000     ELSE                                                                 
093100       IF RESP-KDDISP-UPD = ALL '+'                                       
093200         MOVE MFS-ROER-EJ-FAELT  TO MOD-KDDISP                            
093300       ELSE                                                               
093400         MOVE RESP-KDDISP-UPD    TO MOD-KDDISP                            
093500       END-IF                                                             
093600     END-IF                                                               
093700                                                                          
093800     MOVE RESP-FLBUFJUS-UPD-ATTR                                          
093900                                 TO MOD-FLBUFJUS-ATTR                     
094000                                                                          
094100     IF RESP-FLBUFJUS-UPD = SPACE                                         
094200       MOVE MFS-RENSA-FAELT      TO MOD-FLBUFJUS-IN                       
094300     ELSE                                                                 
094400       IF RESP-FLBUFJUS-UPD = ALL '+'                                     
094500         MOVE MFS-ROER-EJ-FAELT  TO MOD-FLBUFJUS-IN                       
094600       ELSE                                                               
094700         MOVE RESP-FLBUFJUS-UPD  TO MOD-FLBUFJUS-IN                       
094800       END-IF                                                             
094900     END-IF                                                               
095000                                                                          
095100     IF RESP-FLBUFJUS-UT = SPACE                                          
095200       MOVE MFS-RENSA-FAELT      TO MOD-FLBUFJUS-UT                       
095300     ELSE                                                                 
095400       IF RESP-FLBUFJUS-UT = ALL '+'                                      
095500         MOVE MFS-ROER-EJ-FAELT  TO MOD-FLBUFJUS-UT                       
095600       ELSE                                                               
095700         MOVE RESP-FLBUFJUS-UT   TO MOD-FLBUFJUS-UT                       
095800       END-IF                                                             
095900     END-IF                                                               
096000                                                                          
096100     IF RESP-TEDISP = SPACE                                               
096200       MOVE MFS-RENSA-FAELT      TO MOD-TEDISP                            
096300     ELSE                                                                 
096400       IF RESP-TEDISP = ALL '+'                                           
096500         MOVE MFS-ROER-EJ-FAELT  TO MOD-TEDISP                            
096600       ELSE                                                               
096700         MOVE RESP-TEDISP        TO MOD-TEDISP                            
096800       END-IF                                                             
096900     END-IF                                                               
097000                                                                          
097100     MOVE RESP-KDHANDCO-UPD-ATTR                                          
097200                                 TO MOD-KDHANDCO-ATTR                     
097300                                                                          
097400     IF RESP-KDHANDCO-UPD = SPACE                                         
097500       MOVE MFS-RENSA-FAELT      TO MOD-KDHANDCO                          
097600     ELSE                                                                 
097700       IF RESP-KDHANDCO-UPD = ALL '+'                                     
097800         MOVE MFS-ROER-EJ-FAELT  TO MOD-KDHANDCO                          
097900       ELSE                                                               
098000         MOVE RESP-KDHANDCO-UPD  TO MOD-KDHANDCO                          
098100       END-IF                                                             
098200     END-IF                                                               
098300                                                                          
098400     IF RESP-TEHANDCO = SPACE                                             
098500       MOVE MFS-RENSA-FAELT      TO MOD-TEHANDCO                          
098600     ELSE                                                                 
098700       IF RESP-TEHANDCO = ALL '+'                                         
098800         MOVE MFS-ROER-EJ-FAELT  TO MOD-TEHANDCO                          
098900       ELSE                                                               
099000         MOVE RESP-TEHANDCO      TO MOD-TEHANDCO                          
099100       END-IF                                                             
099200     END-IF                                                               
099300                                                                          
099400     IF RESP-FLAGGA-FELTEXT = SPACE                                       
099500       MOVE MFS-RENSA-FAELT      TO MOD-FLAGGA-FELTEXT                    
099600     ELSE                                                                 
099700       IF RESP-FLAGGA-FELTEXT = ALL '+'                                   
099800         MOVE MFS-ROER-EJ-FAELT  TO MOD-FLAGGA-FELTEXT                    
099900       ELSE                                                               
100000         MOVE RESP-FLAGGA-FELTEXT                                         
100100                                 TO MOD-FLAGGA-FELTEXT                    
100200       END-IF                                                             
100300     END-IF                                                               
100400                                                                          
100500     MOVE RESP-KDPERSON-UPD-ATTR                                          
100600                                 TO MOD-KDPERSON-ATTR                     
100700                                                                          
100800     IF RESP-KDPERSON-UPD = SPACE                                         
100900       MOVE MFS-RENSA-FAELT      TO MOD-KDPERSON                          
101000     ELSE                                                                 
101100       IF RESP-KDPERSON-UPD = ALL '+'                                     
101200         MOVE MFS-ROER-EJ-FAELT  TO MOD-KDPERSON                          
101300       ELSE                                                               
101400         MOVE RESP-KDPERSON-UPD  TO MOD-KDPERSON                          
101500       END-IF                                                             
101600     END-IF                                                               
101700                                                                          
101800     MOVE RESP-BEKRBEH-UPD-ATTR                                           
101900                                 TO MOD-BEKRBEH-ATTR                      
102000                                                                          
102100     IF RESP-BEKRBEH-UPD = SPACE                                          
102200       MOVE MFS-RENSA-FAELT      TO MOD-BEKRBEH                           
102300     ELSE                                                                 
102400       IF RESP-BEKRBEH-UPD = ALL '+'                                      
102500         MOVE MFS-ROER-EJ-FAELT  TO MOD-BEKRBEH                           
102600       ELSE                                                               
102700         MOVE RESP-BEKRBEH-UPD   TO MOD-BEKRBEH                           
102800       END-IF                                                             
102900     END-IF                                                               
103000                                                                          
103100     MOVE RESP-KVKRBEH-UPD-ATTR                                           
103200                                 TO MOD-KVKRBEH-ATTR                      
103300                                                                          
103400     IF RESP-KVKRBEH-UPD = SPACE                                          
103500       MOVE MFS-RENSA-FAELT      TO MOD-KVKRBEH-IN                        
103600     ELSE                                                                 
103700       IF RESP-KVKRBEH-UPD = ALL '+'                                      
103800         MOVE MFS-ROER-EJ-FAELT  TO MOD-KVKRBEH-IN                        
103900       ELSE                                                               
104000         MOVE RESP-KVKRBEH-UPD   TO MOD-KVKRBEH-IN                        
104100       END-IF                                                             
104200     END-IF                                                               
104300                                                                          
104400     IF RESP-KVKRBEH-UT = SPACE                                           
104500       MOVE MFS-RENSA-FAELT      TO MOD-KVKRBEH-UT                        
104600     ELSE                                                                 
104700       IF RESP-KVKRBEH-UT = ALL '+'                                       
104800         MOVE MFS-ROER-EJ-FAELT  TO MOD-KVKRBEH-UT                        
104900       ELSE                                                               
105000         MOVE RESP-KVKRBEH-UT    TO MOD-KVKRBEH-UT                        
105100       END-IF                                                             
105200     END-IF                                                               
105300                                                                          
105400     MOVE RESP-TEKRPLT-UPD-ATTR                                           
105500                                 TO MOD-TEKRPLT-ATTR                      
105600                                                                          
105700     IF RESP-TEKRPLT-UPD = SPACE                                          
105800       MOVE MFS-RENSA-FAELT      TO MOD-TEKRPLT                           
105900     ELSE                                                                 
106000       IF RESP-TEKRPLT-UPD = ALL '+'                                      
106100         MOVE MFS-ROER-EJ-FAELT  TO MOD-TEKRPLT                           
106200       ELSE                                                               
106300         MOVE RESP-TEKRPLT-UPD   TO MOD-TEKRPLT                           
106400       END-IF                                                             
106500     END-IF                                                               
106600                                                                          
106700     .                                                                    
106800     EJECT                                                                
106900 MFS-RENSA-FAELT-IN SECTION.                                              
107000                                                                          
107100*    --- ALLA INDATA-FÄLT                                                 
107200     MOVE MFS-RENSA-FAELT        TO MOD-IDKR-IN                           
107300                                    MOD-IDLOPNRM-IN                       
107400                                    MOD-IDARTNR-IN                        
107500                                    MOD-IDLEVNR-IN                        
107600                                    MOD-IDLEVG-IN                         
107700                                    MOD-KVANTMOT-IN                       
107800                                    MOD-KVART-KONTR-IN                    
107900                                    MOD-KVART-EJ-GODK-IN                  
108000                                    MOD-KVART-RET-IN                      
108100                                    MOD-KVART-SKROT-IN                    
108200                                    MOD-KVART-SKROT-LDC-IN                
108300                                    MOD-FLKVALSP-IN                       
108400                                    MOD-FLBUFJUS-IN                       
108500                                    MOD-KVART-KJUST-IN                    
108600                                    MOD-KVART-BEH-IN                      
108700                                    MOD-KVART-SJUST-IN                    
108800                                    MOD-KVKRBEH-IN                        
108900                                    MOD-KDPERSON                          
109000     .                                                                    
109100     EJECT                                                                
109200 MFS-ROER-EJ-FAELT-IN-UT  SECTION.                                        
109300                                                                          
109400*    --- ALLA UTDATA-FÄLT                                                 
109500*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
109600     MOVE MFS-ROER-EJ-FAELT      TO MOD-IDKR-UT                           
109700                                    MOD-IDLOPNRM-UT                       
109800                                    MOD-IDAVINR                           
109900                                    MOD-KVAVIS                            
110000                                    MOD-TIAVSDAT                          
110100                                    MOD-IDARTNR-UT                        
110200                                    MOD-BEART                             
110300                                    MOD-TIREGDAT                          
110400                                    MOD-IDLEVNR-UT                        
110500                                    MOD-IDLEVG-UT                         
110600                                    MOD-BELEV                             
110700                                    MOD-KDKRSTA                           
110800                                    MOD-KVANTMOT-UT                       
110900                                    MOD-KVART-AAVV-UT                     
111000                                    MOD-KVART-KONTR-UT                    
111100                                    MOD-KVART-EJ-GODK-UT                  
111200                                    MOD-KVART-RET-UT                      
111300                                    MOD-KVART-SKROT-UT                    
111400                                    MOD-KVART-SKROT-LDC-UT                
111500                                    MOD-FLKVALSP-UT                       
111600                                    MOD-FLBUFJUS-UT                       
111700                                    MOD-KVART-KJUST-UT                    
111800                                    MOD-KVART-BEH-UT                      
111900                                    MOD-KVART-SJUST-UT                    
112000                                    MOD-IDKRFEL                           
112100                                    MOD-BEKRFEL                           
112200                                    MOD-KDDISP                            
112300                                    MOD-TEDISP                            
112400                                    MOD-KDHANDCO                          
112500                                    MOD-FLAGGA-FELTEXT                    
112600                                    MOD-KDPERSON                          
112700                                    MOD-BEKRBEH                           
112800                                    MOD-KVKRBEH-UT                        
112900                                    MOD-TEKRPLT                           
113000                                                                          
113100                                    MOD-IDLOPNRM-IN                       
113200                                    MOD-IDARTNR-IN                        
113300                                    MOD-IDLEVNR-IN                        
113400                                    MOD-IDLEVG-IN                         
113500                                    MOD-KVANTMOT-IN                       
113600                                    MOD-KVART-KONTR-IN                    
113700                                    MOD-KVART-EJ-GODK-IN                  
113800                                    MOD-KVART-RET-IN                      
113900                                    MOD-KVART-SKROT-IN                    
114000                                    MOD-KVART-SKROT-LDC-IN                
114100                                    MOD-FLKVALSP-IN                       
114200                                    MOD-FLBUFJUS-IN                       
114300                                    MOD-KVART-KJUST-IN                    
114400                                    MOD-KVART-BEH-IN                      
114500                                    MOD-KVART-SJUST-IN                    
114600                                    MOD-KVKRBEH-IN                        
114700     .                                                                    
114800     EJECT                                                                
114900 MFS-FORM-ATTR SECTION.                                                   
115000                                                                          
115100*    --- ALLA INDATA-FÄLT                                                 
115200     MOVE MFS-FORMATETS-ATTR     TO MOD-IDLOPNRM-ATTR                     
115300                                    MOD-IDARTNR-ATTR                      
115400                                    MOD-IDLEVNR-ATTR                      
115500                                    MOD-IDLEVG-ATTR                       
115600                                    MOD-KVANTMOT-ATTR                     
115700                                    MOD-KVART-KONTR-ATTR                  
115800                                    MOD-KVART-EJ-GODK-ATTR                
115900                                    MOD-KVART-RET-ATTR                    
116000                                    MOD-KVART-SKROT-ATTR                  
116100                                    MOD-KVART-SKROT-LDC-ATTR              
116200                                    MOD-KVART-KJUST-ATTR                  
116300                                    MOD-KVART-BEH-ATTR                    
116400                                    MOD-KVART-SJUST-ATTR                  
116500                                    MOD-IDKRFEL-ATTR                      
116600                                    MOD-KDDISP-ATTR                       
116700                                    MOD-KDHANDCO-ATTR                     
116800                                    MOD-KDPERSON-ATTR                     
116900                                    MOD-BEKRBEH-ATTR                      
117000                                    MOD-KVKRBEH-ATTR                      
117100                                    MOD-TEKRPLT-ATTR                      
117200     .                                                                    
117300     SKIP2                                                                
117400* --- IMS SEKTIONER ---                                                   
117500     SKIP3                                                                
117600 IMS-GET-MSG SECTION.                                                     
117700                                                                          
117800     MOVE '  QC' TO GODK-STATUSKODER                                      
117900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
118000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
118100     PERFORM IMS-STATUSKONTROLL                                           
118200     .                                                                    
118300     SKIP3                                                                
118400 IMS-INSERT-MSG SECTION.                                                  
118500                                                                          
118600     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
118700       MOVE '0' TO MFS-KDHUVOMR                                           
118800     END-IF                                                               
118900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
119000     MOVE SPACE TO GODK-STATUSKODER                                       
119100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
119200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
119300     PERFORM IMS-STATUSKONTROLL                                           
119400     .                                                                    
119500     EJECT                                                                
119600 IMS-STATUSKONTROLL SECTION.                                              
119700                                                                          
119800     SET STATUS-IX TO 1                                                   
119900     SEARCH GODK-STATUS                                                   
120000       AT END                                                             
120100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
120200         DELIMITED BY SIZE INTO FELTEXT                                   
120300         CALL FELLOG                                                      
120400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
120500     END-SEARCH                                                           
120600     .                                                                    
