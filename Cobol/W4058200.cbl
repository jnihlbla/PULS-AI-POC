000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4058200.                                                
000400 AUTHOR.         BERT ANDERSSON.                                          
000500 DATE-WRITTEN.   90/03/27.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET ÄR ETT FRÅGEPROGRAM MOT TRANSPORTREGISTRET.           
001100*        PROGRAMMET LÄSER MED NYCKEL TRANSPORTID ELLER DESTINATION        
001200*        FRÅN DB(WLXXKA) OCH TRANSPORTAVGÅNG FRÅN DB.(WLXXKB).            
001300*        DET FINNS MÖJLIGHET ATT BLÄDDRA OM INTE SAMTLIGA                 
001400*        TRANSPORTID/AVGÅNGAR FÖR EN SÖKNING FÅR PLATS PÅ SKÄRMEN.        
001500*        I PROGRAMMET FINNS MÖJLIGHET ATT:                                
001600*        - SÖKA TRANSPORTID ELLER DESTINATION.                            
001700*        - BLÄDDRA GENOM TRANSPORTID/AVGÅNGAR.                            
001800*        PROGRAMMET ÄR ETT FRÅGE-MPP.                                     
001900*        PROGRAMMET LÄSER WLXXKA (WDR1).                                  
002000*        PROGRAMMET LÄSER WLXXKB (WDR1).                                  
002010*        PROGRAMMET LÄSER WDB6.                                           
002100*    INDATA.                                                              
002200*        TRANSAKTION: W4T582                                              
002300*        MID:         W4I58201                                            
002400*    UTDATA.                                                              
002500*        MOD:         W4O58201                                            
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000 WORKING-STORAGE SECTION.                                                 
003001                                                                          
003010*    -- CHECKED BY WY2000                                                 
003100 77  IDPGM                       PIC X(08)   VALUE 'W4058200'.            
003200 77  JA                          PIC X       VALUE 'J'.                   
003300 77  NEJ                         PIC X       VALUE 'N'.                   
003400 77  INDX                        PIC S9(4)   VALUE +0   COMP SYNC.        
003500 77  MAX-INDX                    PIC S9(4)   VALUE +14  COMP SYNC.        
003700 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
003800 77  WS-IDTRP                    PIC X(5)    VALUE SPACE.                 
003900 77  WS-TRPDST                   PIC X(15)   VALUE SPACE.                 
003920 77  WS-IDTIDZON                 PIC X(2)    VALUE SPACE.                 
004000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004100     88  NYCKLAR-OK                          VALUE 'J'.                   
004200     88  NYCKLAR-FEL                         VALUE 'N'.                   
004300                                                                          
004400 77  IDTRP-NYCKEL-SW             PIC X       VALUE 'N'.                   
004500     88  TRPID-AER-NYCKEL                    VALUE 'J'.                   
004600                                                                          
004700 77  TRPDST-NYCKEL-SW            PIC X       VALUE 'N'.                   
004800     88  TRPDST-AER-NYCKEL                   VALUE 'J'.                   
004900                                                                          
005000 77  IDTRP-SW                    PIC X       VALUE 'N'.                   
005100     88  IDTRP-FINNS                         VALUE 'J'.                   
005200                                                                          
005300 77  TRPAVG-SW                   PIC X       VALUE 'N'.                   
005400     88  TRPAVG-FINNS                        VALUE 'J'.                   
005500                                                                          
005600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005700     88  EGEN-MID                            VALUE '4582'.                
005800     88  GODK-MID                   VALUE '4581' '4582' '4583'.           
005900                                                                          
005940       EJECT                                                              
006000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006100 01  GENERELLA-SUBPROGRAM.                                                
006200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006401     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006407*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
006410*01 -COPY WMSGINIT                                                        
006520     EJECT                                                                
006600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
006700*   -COPY WMEDAREA                                                        
006900     EJECT                                                                
007000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
007100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
007200     SKIP3                                                                
007300*01  MID -COPY W4I58201                                                   
007500     EJECT                                                                
007600 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
007700     SKIP3                                                                
007800*01  -COPY WMSGAREA                                                       
008000     EJECT                                                                
008100*    03  MOD -COPY W4O58201   -RED MSG-AREA.                              
008300     EJECT                                                                
008400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
008500     SKIP3                                                                
008600*01  -COPY WMFSAREA                                                       
008800     EJECT                                                                
008900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009000*                                                                         
009100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009200     SKIP3                                                                
009300 01  NYCKLAR-TILL-DLI.                                                    
009400     03  W-WDGXKEY-4431-X.                                                
009500         05  W-4431-IDHTYP       PIC X(4)    VALUE '4431'.                
009600         05  W-4431-IDDC         PIC X(2).                                
009700         05  W-4431-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.             
009800                                                                          
009900     03  W-WDGXKEY-4432-X.                                                
010000         05  W-4432-IDTRP        PIC X(5)    VALUE SPACE.                 
010100     03  W-SOEKFAELT-4432-X.                                              
010200         05  W-4432-BETRPDST     PIC X(15)   VALUE SPACE.                 
010300                                                                          
010400     03  W-WDGXKEY-4433-X.                                                
010500         05  W-4433-IDHTYP       PIC X(4)    VALUE '4433'.                
010600         05  W-4433-IDDC         PIC X(2).                                
010700         05  W-4433-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.             
010800                                                                          
010900     03  W-WDGXKEY-4434-X.                                                
011000         05  W-4434-IDTRP        PIC X(5)    VALUE SPACE.                 
011100         05  W-4434-TITRPAVG     PIC S9(7)   COMP-3.                      
011200         05  W-4434-LOW-VALUE    PIC X       VALUE LOW-VALUE.             
011210                                                                          
011220     03  W-IDDC-B6-X.                                                     
011230         05 W-IDDC-B6                  PIC X(2).                          
011240                                                                          
011300*    --- STATUS-KOD FRÅN IMS                                              
011400 01  STATUS-WS                   PIC XX.                                  
011500     88  SEGMENT-FINNS                       VALUE '  '.                  
011600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011800     SKIP2                                                                
011900 01  GODK-STATUSKODER.                                                    
012000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012100     SKIP3                                                                
012200 01  SSA1                        PIC X(64).                               
012300 01  SSA2                        PIC X(64).                               
012400     EJECT                                                                
012500*    --- IMS FUNKTIONSKODER                                               
012600*01  -COPY W0003                                                          
012800     EJECT                                                                
012900*    ---  DLI INPUT-OUTPUT AREA                                           
013000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013100     SKIP3                                                                
013200 01  DLI-IO-AREA.                                                         
013300     03  IO-AREA                 PIC X(30)  VALUE SPACE.                  
013400     SKIP3                                                                
013500*    03  WLXXKA01 -COPY WDGX4431   -RED IO-AREA.                          
013700     EJECT                                                                
013800*    03  WLXXKA11 -COPY WDGX4432   -RED IO-AREA.                          
014000     EJECT                                                                
014100*    03  WLXXKB01 -COPY WDGX4433   -RED IO-AREA.                          
014300     EJECT                                                                
014400*    03  WLXXKB11 -COPY WDGX4434   -RED IO-AREA.                          
014500                                                                          
014510 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
014520 01   DLI-IO-AREA-B601.                                                   
014530*     03  -COPY WDB601                                                    
014540                                                                          
014600     EJECT                                                                
014700 LINKAGE SECTION.                                                         
014800                                                                          
014900*01  -COPY W0009      -PRE MSG-                                           
015100     EJECT                                                                
015200*01  -COPY W0008      -PRE USEA-                                          
015400     05  FILLER                  PIC X.                                   
015500     EJECT                                                                
015510*01  -COPY W0008      -PRE XXKA-                                          
015520     05  FILLER                  PIC X.                                   
015530     EJECT                                                                
015600*01  -COPY W0008      -PRE XXKB-                                          
015800     05  FILLER                  PIC X.                                   
015900     EJECT                                                                
015910*01  -COPY W0008      -PRE WDB6-                                          
015920     05  FILLER                  PIC X.                                   
015930     EJECT                                                                
016000 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB XXKA-PCB XXKB-PCB             
016010                           WDB6-PCB.                                      
016100     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB XXKA-PCB XXKB-PCB             
016110                           WDB6-PCB.                                      
016200                                                                          
016300     PERFORM IMS-GET-MSG                                                  
016400     IF SEGMENT-FINNS                                                     
016500       PERFORM A-INIT                                                     
016600       PERFORM B-KOLLA-NYCKLAR                                            
016700       IF NYCKLAR-OK                                                      
016800         IF MFS-FIRST                                                     
016900           PERFORM C-FOERSTA-SIDA                                         
017000         ELSE                                                             
017100           IF MFS-NEXT                                                    
017200             PERFORM D-NAESTA-SIDA                                        
017300           ELSE                                                           
017400             PERFORM E-SAMMA-SIDA                                         
017500           END-IF                                                         
017600         END-IF                                                           
017700           PERFORM F-LAES-VISA-BILD                                       
017800       END-IF                                                             
017810       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O58201 + 4                      
018000       PERFORM IMS-INSERT-MSG                                             
018100     END-IF                                                               
018200                                                                          
018300     MOVE ZERO TO RETURN-CODE                                             
018400     GOBACK                                                               
018500     .                                                                    
018600     EJECT                                                                
018700 A-INIT SECTION.                                                          
018800                                                                          
018900     IF MSG-DUBBLA-TRANSKODER                                             
019000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I58201                 
019100       MOVE MSG-IDTRANS-2       TO MFS-IDTRANS                            
019200       MOVE MSG-KDMFSFOR-2      TO MFS-KDMFSFOR                           
019300     ELSE                                                                 
019400       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I58201                 
019500       MOVE MSG-IDTRANS-1       TO MFS-IDTRANS                            
019600       MOVE MSG-KDMFSFOR-1      TO MFS-KDMFSFOR                           
019700     END-IF                                                               
019800                                                                          
019900     MOVE MSG-KDTRTYP           TO MFS-KDTRTYP                            
020000     MOVE MSG-IDPFK TO MFS-IDPFK                                          
020100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
020200                                                                          
020300     MOVE LOW-VALUE TO MSG-AREA                                           
020400     MOVE 'W4O582N1' TO MFS-IDMOD                                         
020500     MOVE '4582' TO MOD-IDTRANS                                           
020600     MOVE MFS-RENSA-FAELT       TO MOD-TEMFSFEL MOD-TEMFSINF              
020700                                                                          
020800     IF NOT EGEN-MID                                                      
020900       MOVE SPACE               TO MFS-KDTRTYP                            
021000       MOVE '7'                 TO MFS-IDPFK                              
021100     END-IF                                                               
021200                                                                          
022400     .                                                                    
022500     EJECT                                                                
022600 B-KOLLA-NYCKLAR SECTION.                                                 
022700                                                                          
022701     MOVE ALL '+'           TO MSGI-WMSGINIT                              
022702     MOVE '001'             TO MSGI-KDCALL                                
022703     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
022704     MOVE '4582'            TO MSGI-IDTRANS                               
022705     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
022709     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
022710     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
022711     MOVE MSGI-IDTIDZON     TO WS-IDTIDZON                                
022720                                                                          
022800     MOVE JA                      TO NYCKLAR-SW                           
022900     MOVE NEJ                     TO IDTRP-NYCKEL-SW TRPAVG-SW            
023000                                     TRPDST-NYCKEL-SW IDTRP-SW            
023100     MOVE MFS-RENSA-FAELT         TO MOD-IDTRP-IN                         
023200                                     MOD-IDDC-IN                          
023210                                     MOD-BETRPDST-IN                      
023300     IF MID-IDTRP-IN = ALL '+'                                            
023400       IF MID-BETRPDST-IN = ALL '+'                                       
023500         IF MID-IDTRP-UT = SPACE                                          
023600           IF MID-BETRPDST-UT = SPACE                                     
023700             MOVE NEJ             TO NYCKLAR-SW                           
023800           ELSE                                                           
023900             MOVE MID-BETRPDST-UT TO WS-TRPDST                            
024000                                     W-4432-BETRPDST                      
024100             MOVE JA              TO TRPDST-NYCKEL-SW                     
024200           END-IF                                                         
024300         ELSE                                                             
024400           MOVE MID-IDTRP-UT      TO WS-IDTRP                             
024500           MOVE JA                TO IDTRP-NYCKEL-SW                      
024600           MOVE SPACE             TO MFS-KDTRTYP                          
024700           IF MID-BETRPDST-IN = ALL '+'                                   
024800             IF MID-BETRPDST-UT = SPACE                                   
024900               CONTINUE                                                   
025000             ELSE                                                         
025100               MOVE MID-BETRPDST-UT TO WS-TRPDST                          
025200             END-IF                                                       
025300           ELSE                                                           
025400             MOVE MID-BETRPDST-IN TO WS-TRPDST                            
025500           END-IF                                                         
025600         END-IF                                                           
025700       ELSE                                                               
025800         MOVE MID-BETRPDST-IN     TO WS-TRPDST W-4432-BETRPDST            
025900         MOVE JA                  TO TRPDST-NYCKEL-SW                     
026000       END-IF                                                             
026100     ELSE                                                                 
026200       MOVE MID-IDTRP-IN          TO WS-IDTRP                             
026300       MOVE JA                    TO IDTRP-NYCKEL-SW                      
026400       MOVE SPACE                 TO MFS-KDTRTYP                          
026500       IF MID-BETRPDST-IN = ALL '+'                                       
026600         IF MID-BETRPDST-UT = SPACE                                       
026700           CONTINUE                                                       
026800         ELSE                                                             
026900           MOVE MID-BETRPDST-UT TO WS-TRPDST                              
027000         END-IF                                                           
027100       ELSE                                                               
027200         MOVE MID-BETRPDST-IN     TO WS-TRPDST                            
027300       END-IF                                                             
027400     END-IF                                                               
027500                                                                          
027600     IF TRPID-AER-NYCKEL                                                  
027700       IF WS-IDTRP NUMERIC AND WS-IDTRP IS > ZERO                         
027800         INSPECT WS-IDTRP REPLACING LEADING ZERO BY SPACE                 
027900       ELSE                                                               
028000         MOVE NEJ               TO NYCKLAR-SW                             
028100       END-IF                                                             
028200     ELSE                                                                 
028300       IF TRPDST-AER-NYCKEL                                               
028400         IF WS-TRPDST = SPACE                                             
028500           MOVE NEJ             TO NYCKLAR-SW                             
028600         END-IF                                                           
028700       ELSE                                                               
028800         MOVE NEJ               TO NYCKLAR-SW                             
028900       END-IF                                                             
029000     END-IF                                                               
029001                                                                          
029002     IF MID-IDDC-IN             = ALL '+'                                 
029003       IF MID-IDDC-UT           = SPACE                                   
029004         MOVE MSGI-IDDC         TO W-IDDC-B6                              
029005       ELSE                                                               
029006         MOVE MID-IDDC-UT       TO W-IDDC-B6                              
029007       END-IF                                                             
029008     ELSE                                                                 
029009       MOVE MID-IDDC-IN         TO W-IDDC-B6                              
029010       MOVE '7'                 TO MFS-IDPFK                              
029011       MOVE SPACE               TO MFS-KDTRTYP                            
029013     END-IF                                                               
029014     PERFORM IMS-GU-WDB601                                                
029015                                                                          
029016     IF DCS-KDDC = SPACE OR DCS-CDC-TR OR DCS-DDC                         
029017       MOVE MSGI-IDDC           TO W-4431-IDDC                            
029018                                   W-4433-IDDC                            
029019                                   W-IDDC-B6                              
029020       PERFORM IMS-GU-WDB601                                              
029021     ELSE                                                                 
029022       MOVE DCS-IDDC            TO W-4431-IDDC                            
029023                                   W-4433-IDDC                            
029026     END-IF                                                               
029100                                                                          
029110     MOVE DCS-IDDC              TO MOD-IDDC-UT                            
029200     IF GODK-MID                                                          
029300       MOVE WS-IDTRP            TO MOD-IDTRP-UT                           
029400       MOVE WS-TRPDST           TO MOD-BETRPDST-UT                        
029500       MOVE MFS-RENSA-FAELT     TO MOD-IDTRP-IN MOD-BETRPDST-IN           
029510                                   MOD-IDDC-IN                            
029600     ELSE                                                                 
029700       MOVE MFS-RENSA-FAELT     TO MOD-IDTRP-UT MOD-BETRPDST-UT           
029800     END-IF                                                               
029900                                                                          
030000     IF NYCKLAR-FEL                                                       
030100       MOVE '401'               TO MED-IDMFSFEL                           
030200       PERFORM S01-FEL-RUTIN                                              
030300     END-IF                                                               
030400     .                                                                    
030500     EJECT                                                                
030600 C-FOERSTA-SIDA SECTION.                                                  
030700                                                                          
030800     IF TRPID-AER-NYCKEL                                                  
030900       MOVE WS-IDTRP                 TO W-4432-IDTRP W-4434-IDTRP         
031000       PERFORM IMS-GU-WLXXKA01                                            
031010       IF SEGMENT-FINNS                                                   
031100         PERFORM IMS-GNP-WLXXKA11                                         
031110       END-IF                                                             
031200     ELSE                                                                 
031300       PERFORM IMS-GU-WLXXKA01                                            
031320       IF SEGMENT-FINNS                                                   
031400         PERFORM IMS-GNP-SOEKFAELT-WLXXKA11                               
031410       END-IF                                                             
031500     END-IF                                                               
031600     IF SEGMENT-FINNS                                                     
031700       MOVE JA                       TO IDTRP-SW                          
031800       MOVE ZERO                     TO W-4434-TITRPAVG                   
031900       MOVE '006'                    TO MED-IDMFSINF                      
032000       PERFORM S03-INF-RUTIN                                              
032100     ELSE                                                                 
032200       MOVE NEJ                      TO IDTRP-SW                          
032300     END-IF                                                               
032400     .                                                                    
032500     EJECT                                                                
032600 D-NAESTA-SIDA SECTION.                                                   
032700                                                                          
032800     IF MID-IDTRP-NEXT NUMERIC                                            
032900       MOVE MID-IDTRP-NEXT           TO W-4432-IDTRP W-4434-IDTRP         
033000       MOVE MID-TITRPAVG-NEXT        TO W-4434-TITRPAVG                   
033100     ELSE                                                                 
033200       MOVE WS-IDTRP                 TO W-4432-IDTRP W-4434-IDTRP         
033300     END-IF                                                               
033400     PERFORM IMS-GU-WLXXKA01                                              
033410     IF SEGMENT-FINNS                                                     
033500       PERFORM IMS-GNP-WLXXKA11                                           
033510     END-IF                                                               
033600     MOVE MID-BETRPDST-UT            TO WS-TRPDST                         
033700     IF SEGMENT-FINNS                                                     
033800       MOVE JA                       TO IDTRP-SW                          
033900     ELSE                                                                 
034000       MOVE NEJ                      TO IDTRP-SW                          
034100     END-IF                                                               
034200     .                                                                    
034300 E-SAMMA-SIDA SECTION.                                                    
034400                                                                          
034500     MOVE MID-IDTRP-ENTER            TO W-4432-IDTRP W-4434-IDTRP         
034600     MOVE MID-TITRPAVG-ENTER         TO W-4434-TITRPAVG                   
034700     IF TRPID-AER-NYCKEL                                                  
034800       MOVE WS-IDTRP                 TO W-4432-IDTRP W-4434-IDTRP         
034900       MOVE ZERO                     TO W-4434-TITRPAVG                   
035000       PERFORM IMS-GU-WLXXKA01                                            
035010       IF SEGMENT-FINNS                                                   
035100         PERFORM IMS-GNP-WLXXKA11                                         
035110       END-IF                                                             
035200     ELSE                                                                 
035300       IF MID-BETRPDST-IN = ALL '+'                                       
035400         PERFORM IMS-GU-WLXXKA01                                          
035410         IF SEGMENT-FINNS                                                 
035500           PERFORM IMS-GNP-WLXXKA11                                       
035510         END-IF                                                           
035600       ELSE                                                               
035700         PERFORM IMS-GU-SOEKFAELT-WLXXKA01                                
035710         IF SEGMENT-FINNS                                                 
035800           PERFORM IMS-GNP-SOEKFAELT-WLXXKA11                             
035900           MOVE ZERO                 TO W-4432-IDTRP W-4434-IDTRP         
036000                                        W-4434-TITRPAVG                   
036010         END-IF                                                           
036100       END-IF                                                             
036200     END-IF                                                               
036300     IF SEGMENT-FINNS                                                     
036400       MOVE JA                       TO IDTRP-SW                          
036500     ELSE                                                                 
036600       MOVE NEJ                      TO IDTRP-SW                          
036700     END-IF                                                               
036800     .                                                                    
036900       EJECT                                                              
037000 F-LAES-VISA-BILD SECTION.                                                
037100                                                                          
037200     MOVE +1                         TO INDX                              
037300     MOVE LOW-VALUE                  TO W-4434-LOW-VALUE                  
037400                                                                          
037500     IF IDTRP-FINNS                                                       
037600       IF TRPID-AER-NYCKEL                                                
037700         PERFORM FA-LAES-MED-TRPID                                        
037800       ELSE                                                               
037900         PERFORM FB-LAES-MED-BETRPDST                                     
038000       END-IF                                                             
038100         PERFORM S06-MFS-KONTROLL                                         
038200     ELSE                                                                 
038300       MOVE '005'                  TO MED-IDMFSFEL                        
038400       PERFORM S01-FEL-RUTIN                                              
038500     END-IF                                                               
038600     .                                                                    
038700     EJECT                                                                
038800 FA-LAES-MED-TRPID SECTION.                                               
038900                                                                          
039000       PERFORM S04-MOVE-4432-TILL-MOD                                     
039100       PERFORM IMS-GU-WLXXKB01                                            
039200         IF SEGMENT-FINNS                                                 
039300           PERFORM IMS-GNP-FIRST-WLXXKB11                                 
039400                                                                          
039500           PERFORM UNTIL INDX > MAX-INDX                                  
039600             IF SEGMENT-FINNS AND                                         
039700               W-4434-IDTRP = 4434-IDTRP                                  
039800               IF INDX > +1                                               
039900                 PERFORM MFS-RENSA-FAELT-IDTRP                            
040000               END-IF                                                     
040100               PERFORM S05-MOVE-4434-TILL-MOD                             
040200               PERFORM IMS-GNP-WLXXKB11                                   
040300               MOVE JA                   TO TRPAVG-SW                     
040400             ELSE                                                         
040500               MOVE NEJ                  TO TRPAVG-SW                     
040600               IF INDX = +1                                               
040700                 MOVE ZERO               TO MOD-TITRPAVG-ENTER            
040800                 PERFORM MFS-RENSA-FAELT-TRPAVG                           
040900               ELSE                                                       
041000                 PERFORM MFS-RENSA-FAELT-IDTRP                            
041100                 PERFORM MFS-RENSA-FAELT-TRPAVG                           
041200               END-IF                                                     
041300             END-IF                                                       
041400             ADD +1                      TO INDX                          
041500           END-PERFORM                                                    
041600         END-IF                                                           
041700                                                                          
041800       IF TRPAVG-FINNS                                                    
041900         MOVE 4434-IDTRP             TO MOD-IDTRP-NEXT                    
042000         MOVE 4434-TITRPAVG          TO MOD-TITRPAVG-NEXT                 
042100         MOVE '105'                  TO MED-IDMFSINF                      
042200         PERFORM S03-INF-RUTIN                                            
042300       ELSE                                                               
042400         MOVE MOD-IDTRP-ENTER        TO MOD-IDTRP-NEXT                    
042500         MOVE MOD-TITRPAVG-ENTER     TO MOD-TITRPAVG-NEXT                 
042600       END-IF                                                             
042700     .                                                                    
042800       EJECT                                                              
042900 FB-LAES-MED-BETRPDST SECTION.                                            
043000                                                                          
043100       PERFORM UNTIL INDX > MAX-INDX                                      
043200         IF SEGMENT-FINNS                                                 
043300           IF WS-IDTRP = 4432-IDTRP                                       
043400             IF INDX > +1                                                 
043500               PERFORM MFS-RENSA-FAELT-IDTRP                              
043600             END-IF                                                       
043700           ELSE                                                           
043800             PERFORM S04-MOVE-4432-TILL-MOD                               
043900             MOVE 4432-IDTRP       TO WS-IDTRP  MOD-IDTRP-NEXT            
044000                                      W-4432-IDTRP  W-4434-IDTRP          
044100           END-IF                                                         
044200           PERFORM FBA-LAES-TRPAVG                                        
044300           PERFORM IMS-GNP-SOEKFAELT-WLXXKA11                             
044400           IF MFS-IDPFK = 8                                               
044500             IF NOT TRPAVG-FINNS                                          
044600               MOVE 4432-IDTRP         TO MOD-IDTRP-NEXT                  
044700               MOVE ZERO               TO MOD-TITRPAVG-NEXT               
044800             END-IF                                                       
044900           END-IF                                                         
045000         ELSE                                                             
045100           IF INDX > +1                                                   
045200             PERFORM MFS-RENSA-FAELT-IDTRP                                
045300           END-IF                                                         
045400           PERFORM MFS-RENSA-FAELT-TRPAVG                                 
045500         END-IF                                                           
045600         ADD +1                        TO INDX                            
045700       END-PERFORM                                                        
045800                                                                          
045900       IF SEGMENT-FINNS                                                   
046000         MOVE JA                       TO TRPAVG-SW                       
046100       END-IF                                                             
046200       IF TRPAVG-FINNS                                                    
046300         MOVE '105'                    TO MED-IDMFSINF                    
046400         PERFORM S03-INF-RUTIN                                            
046500       END-IF                                                             
046600       .                                                                  
046700 FBA-LAES-TRPAVG SECTION.                                                 
046800                                                                          
046900         PERFORM IMS-GU-WLXXKB01                                          
047000         IF SEGMENT-FINNS                                                 
047100           PERFORM IMS-GNP-FIRST-WLXXKB11                                 
047200           PERFORM UNTIL (WS-IDTRP NOT = 4434-IDTRP) OR                   
047300                         (INDX > MAX-INDX)                                
047400             IF SEGMENT-FINNS                                             
047500               PERFORM S05-MOVE-4434-TILL-MOD                             
047600               PERFORM IMS-GNP-WLXXKB11                                   
047700               MOVE JA                   TO TRPAVG-SW                     
047800             ELSE                                                         
047900               MOVE NEJ                  TO TRPAVG-SW                     
048000               MOVE ZERO                 TO WS-IDTRP                      
048100               PERFORM MFS-RENSA-FAELT-TRPAVG                             
048200             END-IF                                                       
048300             ADD +1                      TO INDX                          
048400           END-PERFORM                                                    
048500         END-IF                                                           
048600         MOVE ZERO                       TO W-4434-TITRPAVG               
048700                                                                          
048800         IF WS-IDTRP = 4434-IDTRP                                         
048900           MOVE 4434-IDTRP               TO MOD-IDTRP-NEXT                
049000           MOVE 4434-TITRPAVG            TO MOD-TITRPAVG-NEXT             
049100         ELSE                                                             
049200           COMPUTE INDX = INDX - 1                                        
049300           MOVE NEJ                      TO TRPAVG-SW                     
049400           MOVE MOD-IDTRP-ENTER          TO MOD-IDTRP-NEXT                
049500           MOVE MOD-TITRPAVG-ENTER       TO MOD-TITRPAVG-NEXT             
049600         END-IF                                                           
049700         .                                                                
049800 S01-FEL-RUTIN SECTION.                                                   
049900       CALL WMEDKONV USING MED-WMEDAREA                                   
050000       MOVE MED-TEMFSFEL             TO MOD-TEMFSFEL                      
050100       .                                                                  
050200 S03-INF-RUTIN SECTION.                                                   
050300       CALL WMEDKONV USING MED-WMEDAREA                                   
050400       MOVE MED-TEMFSINF             TO MOD-TEMFSINF                      
050500       .                                                                  
050600 S04-MOVE-4432-TILL-MOD SECTION.                                          
050700       MOVE 4432-IDTRP               TO MOD-IDTRP-RAD(INDX)               
050800                                        W-4432-IDTRP                      
050900                                        W-4434-IDTRP                      
051000       MOVE 4432-KVLASTTI            TO MOD-KVLASTTI-RAD(INDX)            
051100       MOVE 4432-KVADMFL             TO MOD-KVADMFL-RAD(INDX)             
051200       MOVE 4432-KVADMEL             TO MOD-KVADMEL-RAD(INDX)             
051300       IF INDX = +1                                                       
051400         MOVE 4432-IDTRP             TO MOD-IDTRP-ENTER                   
051500         MOVE 4432-BETRPDST          TO MOD-BETRPDST-UT                   
051600       END-IF                                                             
051700       .                                                                  
051800 S05-MOVE-4434-TILL-MOD SECTION.                                          
051900                                                                          
052000       MOVE 4434-TITRPAVG            TO MOD-TITRPAVG-RAD(INDX)            
052100       MOVE 4434-BETRPFIR            TO MOD-BETRPFIR-RAD(INDX)            
052200       MOVE 4434-KDFARLIG            TO MOD-KDFARLIG-RAD(INDX)            
052300       INSPECT MOD-KDFARLIG-RAD(INDX) REPLACING                           
052400                                      LEADING ZERO BY SPACE               
052500       MOVE 4434-VLTRPMIN            TO MOD-VLTRPMIN-RAD(INDX)            
052600       INSPECT MOD-VLTRPMIN-RAD(INDX) REPLACING                           
052700                                      LEADING ZERO BY SPACE               
052800       IF INDX = +1                                                       
052900         MOVE 4434-IDTRP             TO MOD-IDTRP-ENTER                   
053000         MOVE 4434-TITRPAVG          TO MOD-TITRPAVG-ENTER                
053100       END-IF                                                             
053200       .                                                                  
053298       EJECT                                                              
053300 S06-MFS-KONTROLL SECTION.                                                
053400                                                                          
053500         IF MFS-FIRST                                                     
053600           MOVE '006'                TO MED-IDMFSINF                      
053700           PERFORM S03-INF-RUTIN                                          
053800         END-IF                                                           
053900         IF TRPAVG-FINNS                                                  
054000           CONTINUE                                                       
054100         ELSE                                                             
054200           IF MFS-NEXT                                                    
054300             IF SEGMENT-SAKNAS                                            
054400               MOVE '106'            TO MED-IDMFSINF                      
054500               PERFORM S03-INF-RUTIN                                      
054600             END-IF                                                       
054700           END-IF                                                         
054800         END-IF                                                           
054900       .                                                                  
055000     EJECT                                                                
055100* --- MFS SEKTIONER ---                                                   
055200     SKIP2                                                                
055300 MFS-RENSA-FAELT-IDTRP SECTION.                                           
055400                                                                          
055500     MOVE MFS-RENSA-FAELT TO MOD-IDTRP-RAD(INDX)                          
055600                             MOD-KVLASTTI-RAD(INDX)                       
055700                             MOD-KVADMFL-RAD(INDX)                        
055800                             MOD-KVADMEL-RAD(INDX)                        
055900     .                                                                    
056000 MFS-RENSA-FAELT-TRPAVG SECTION.                                          
056100                                                                          
056200     MOVE MFS-RENSA-FAELT TO MOD-TITRPAVG-RAD(INDX)                       
056300                             MOD-KDFARLIG-RAD(INDX)                       
056400                             MOD-VLTRPMIN-RAD(INDX)                       
056500                             MOD-BETRPFIR-RAD(INDX)                       
056600     .                                                                    
056700     SKIP2                                                                
056800* --- IMS SEKTIONER ---                                                   
056900     SKIP2                                                                
057000 IMS-GET-MSG SECTION.                                                     
057100                                                                          
057200     MOVE '  QC' TO GODK-STATUSKODER                                      
057300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
057400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
057500     PERFORM IMS-STATUSKONTROLL                                           
057600     .                                                                    
057700     SKIP2                                                                
057800 IMS-INSERT-MSG SECTION.                                                  
057900                                                                          
058000     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
058100       MOVE '0' TO MFS-KDHUVOMR                                           
058200     END-IF                                                               
058300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
058400     MOVE SPACE TO GODK-STATUSKODER                                       
058500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
058600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
058700     PERFORM IMS-STATUSKONTROLL                                           
058800     .                                                                    
058900     EJECT                                                                
059000 IMS-GU-WLXXKA01 SECTION.                                                 
059100                                                                          
059200     STRING 'WLXXKA01(WDGXKEY  =' W-WDGXKEY-4431-X ')'                    
059300          DELIMITED BY SIZE INTO SSA1                                     
059400     MOVE '  GE' TO GODK-STATUSKODER                                      
059500     CALL CBLTDLI USING GU XXKA-PCB DLI-IO-AREA SSA1                      
059600     MOVE XXKA-STATUS-CODE TO STATUS-WS                                   
059700     PERFORM IMS-STATUSKONTROLL                                           
059800     .                                                                    
059900     EJECT                                                                
060000 IMS-GNP-WLXXKA11 SECTION.                                                
060100                                                                          
060200     STRING 'WLXXKA11(WDGXKEY  =' W-WDGXKEY-4432-X ')'                    
060300          DELIMITED BY SIZE INTO SSA1                                     
060400     MOVE '  GE' TO GODK-STATUSKODER                                      
060500     CALL CBLTDLI USING GNP XXKA-PCB DLI-IO-AREA SSA1                     
060600     MOVE XXKA-STATUS-CODE TO STATUS-WS                                   
060700     PERFORM IMS-STATUSKONTROLL                                           
060800     .                                                                    
060900 IMS-GU-SOEKFAELT-WLXXKA01 SECTION.                                       
061000                                                                          
061100     STRING 'WLXXKA01(WDGXKEY  =' W-WDGXKEY-4431-X ')'                    
061200          DELIMITED BY SIZE INTO SSA1                                     
061300     MOVE '  GE' TO GODK-STATUSKODER                                      
061400     CALL CBLTDLI USING GU XXKA-PCB DLI-IO-AREA SSA1                      
061500     MOVE XXKA-STATUS-CODE TO STATUS-WS                                   
061600     PERFORM IMS-STATUSKONTROLL                                           
061700     .                                                                    
061800     SKIP2                                                                
061900 IMS-GNP-SOEKFAELT-WLXXKA11 SECTION.                                      
062000                                                                          
062100     STRING 'WLXXKA11(BETRPDST =' W-SOEKFAELT-4432-X ')'                  
062200          DELIMITED BY SIZE INTO SSA1                                     
062300     MOVE '  GE' TO GODK-STATUSKODER                                      
062400     CALL CBLTDLI USING GNP XXKA-PCB DLI-IO-AREA SSA1                     
062500     MOVE XXKA-STATUS-CODE TO STATUS-WS                                   
062600     PERFORM IMS-STATUSKONTROLL                                           
062700     .                                                                    
062800     EJECT                                                                
062900 IMS-GU-WLXXKB01 SECTION.                                                 
063000                                                                          
063100     STRING 'WLXXKB01(WDGXKEY  =' W-WDGXKEY-4433-X ')'                    
063200          DELIMITED BY SIZE INTO SSA1                                     
063300     MOVE '  GE' TO GODK-STATUSKODER                                      
063400     CALL CBLTDLI USING GU XXKB-PCB DLI-IO-AREA SSA1                      
063500     MOVE XXKB-STATUS-CODE TO STATUS-WS                                   
063600     PERFORM IMS-STATUSKONTROLL                                           
063700     .                                                                    
063800     SKIP2                                                                
063900 IMS-GNP-WLXXKB11 SECTION.                                                
064000                                                                          
064100     STRING 'WLXXKB11(WDGXKEY =>' W-WDGXKEY-4434-X ')'                    
064200          DELIMITED BY SIZE INTO SSA1                                     
064300     MOVE '  GE' TO GODK-STATUSKODER                                      
064400     CALL CBLTDLI USING GNP XXKB-PCB DLI-IO-AREA SSA1                     
064500     MOVE XXKB-STATUS-CODE TO STATUS-WS                                   
064600     PERFORM IMS-STATUSKONTROLL                                           
064700     .                                                                    
064800     SKIP2                                                                
064900 IMS-GNP-FIRST-WLXXKB11 SECTION.                                          
065000                                                                          
065100     STRING 'WLXXKB11*F(WDGXKEY =>' W-WDGXKEY-4434-X ')'                  
065200          DELIMITED BY SIZE INTO SSA1                                     
065300     MOVE '  GE' TO GODK-STATUSKODER                                      
065400     CALL CBLTDLI USING GNP XXKB-PCB DLI-IO-AREA SSA1                     
065500     MOVE XXKB-STATUS-CODE TO STATUS-WS                                   
065600     PERFORM IMS-STATUSKONTROLL                                           
065700     .                                                                    
065800     SKIP2                                                                
065810 IMS-GU-WDB601    SECTION.                                                
065820     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
065830          DELIMITED BY SIZE INTO SSA1                                     
065840     MOVE '  GE' TO GODK-STATUSKODER                                      
065850     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
065860     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
065870     PERFORM IMS-STATUSKONTROLL                                           
065880     IF SEGMENT-SAKNAS                                                    
065890         MOVE SPACE TO DCS-KDDC                                           
065891     END-IF                                                               
065892     .                                                                    
065900 IMS-STATUSKONTROLL SECTION.                                              
066000                                                                          
066100     SET STATUS-IX TO 1                                                   
066200     SEARCH GODK-STATUS                                                   
066300       AT END CALL FELLOG                                                 
066400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
066500     END-SEARCH                                                           
066600     .                                                                    
