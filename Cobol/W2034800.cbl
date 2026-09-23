000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2034800.                                                
000400 AUTHOR.         STEFAN ÅSGÅRDEN.                                         
000500 DATE-WRITTEN.   NOV 2002.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*                                                                         
001000*        BESTÄLLNINGSBILD FÖR ATT FÅ UT ARTIKLAR PÅ DC-NIVÅ               
001100*        UTIFRÅN VALDA PARAMETRAR                                         
001200*        OM RESULTATET ÖVERSTIGER 300 ARTIKLAR SÄNDS BARA                 
001300*        ETT MEDDELANDE TILL BESTÄLLAREN VIA MAIL                         
001400*        ANNARS KOMMER RESULTATET PÅ MAIL                                 
001500*                                                                         
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: W2T348                                              
001900*        MID:         W2I34801                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        MOD:         W2O34801                                            
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003000*    -- CHECKED BY WY2000                                                 
003100 77  IDPGM                       PIC X(08)   VALUE 'W2034800'.            
003200                                                                          
003300*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003400 01  FILLER                      PIC X(16)   VALUE 'FELTEXT'.             
003500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003600                                                                          
003700 77  JA                          PIC X       VALUE 'J'.                   
003800 77  YES                         PIC X       VALUE 'Y'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004000 77  STOPP                       PIC X       VALUE 'S'.                   
004100                                                                          
004200 01  FILLER                      PIC X(16)   VALUE 'IX'.                  
004300 77  IX                          PIC S9(9)  VALUE +0    COMP SYNC.        
004400                                                                          
004500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004600                                                                          
004700                                                                          
004800 01  FILLER                      PIC X(16)   VALUE 'INDATA-SW'.           
004900 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005000     88  INDATA-OK                           VALUE 'J'.                   
005100     88  INDATA-FEL                          VALUE 'N'.                   
005200                                                                          
005300 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005400     88  ALLT-OK                             VALUE 'J'.                   
005500                                                                          
005600 01  FILLER                      PIC X(16)   VALUE 'W-IDTRANS'.           
005700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005800     88  EGEN-MID                            VALUE '2348'.                
005900     SKIP2                                                                
006000 01  FILLER                      PIC X(16)   VALUE 'ARBETSAREOR'.         
006100 01  ARBETS-AREOR.                                                        
006200     03 WS-KDERS                 PIC 9(3)    VALUE ZERO.                  
006300     03 WS-RED-KVPB-REF          PIC Z(4)9.9 VALUE ZERO.                  
006400     03 WS-KVPB-REF-FOM          PIC 9(5)V9  VALUE ZERO.                  
006500     03 WS-KVPB-REF-TOM          PIC 9(5)V9  VALUE ZERO.                  
006600     EJECT                                                                
006700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006800 01  GENERELLA-SUBPROGRAM.                                                
006900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007300     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
007400     EJECT                                                                
007500*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
007600*01 -COPY WMSGINIT                                                        
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'WMEDAREA'.            
007900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008000*01 -COPY WMEDAREA                                                        
008100     EJECT                                                                
008200 01  FILLER                      PIC X(16)   VALUE 'WDECEDIT'.            
008300*    --- COPYTEXT TILL SUBPROGRAM WDECEDIT                                
008400*01  -COPY WDECAREA                                                       
008500     SKIP3                                                                
008600 01  FILLER                      PIC X(16)   VALUE 'NYCKLAR'.             
008700 01  NYCKLAR-TILL-DLI.                                                    
008800   03    W-KDARBTYP-X.                                                    
008900     05    W-KDARBTYP             PIC X(8)    VALUE SPACE.                
009000                                                                          
009100   03    W-IDPERSON-X.                                                    
009200     05    W-IDPERSON             PIC S9(3)   COMP-3 VALUE +0.            
009300                                                                          
009400   03  W-IDDC-B6-X.                                                       
009500       05 W-IDDC-B6                  PIC X(2).                            
009600                                                                          
009700 01  FILLER                      PIC X(16)   VALUE                        
009800                                             'MESSAGE-CODES'.             
009900 01  MESSAGE-CODES.                                                       
010000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010100     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
010200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010300     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
010400     SKIP2                                                                
010500 01  FILLER.                                                              
010600     03  MAILSEND.                                                        
010700         05  FILLER              PIC X(13)   VALUE                        
010800                                 'MAIL ORDERED '.                         
010900*                                'MAIL BESTÄLLD'.                         
011000     EJECT                                                                
011100 01  PROG-TO-PROG-SW.                                                     
011200*    03  -COPY WMSGSOP                                                    
011300     EJECT                                                                
011400 01  FILLER                      PIC X(16) VALUE                          
011500                                           'WS-PARAMETRAR'.               
011600 01  WS-PARAMETRAR.                                                       
011700     03  WS-URVAL1.                                                       
011800         05  IDLAND-SPR          PIC X(2)  VALUE SPACE.                   
011900         05  IDDC                PIC X(2)  VALUE SPACE.                   
012000         05  IDPERSON-BUY-FOM    PIC X(3)  VALUE SPACE.                   
012100         05  IDPERSON-BUY-TOM    PIC X(3)  VALUE SPACE.                   
012200         05  IDPERSON-BUY2       PIC X(3)  VALUE SPACE.                   
012300         05  IDPERSON-BUY3       PIC X(3)  VALUE SPACE.                   
012400         05  IDPERSON-BUY4       PIC X(3)  VALUE SPACE.                   
012500         05  IDPROJ-GRP OCCURS 3.                                         
012600             07  IDPROJ          PIC X(4)  VALUE SPACE.                   
012700         05  IDFKNGRP-FOM        PIC X(4)  VALUE SPACE.                   
012800         05  IDFKNGRP-TOM        PIC X(4)  VALUE SPACE.                   
012900         05  KVPB-FOM            PIC X(7)  VALUE SPACE.                   
013000         05  KVPB-TOM            PIC X(7)  VALUE SPACE.                   
013100         05  ADLAGOMR            PIC X(2)  VALUE SPACE.                   
013200         05  ADGANG              PIC X(2)  VALUE SPACE.                   
013300         05  ADPLATS-FOM         PIC X(5)  VALUE SPACE.                   
013400         05  KDERS               PIC X(3)  VALUE SPACE.                   
013500         05  KVLS-TKN            PIC X(1)  VALUE SPACE.                   
013600         05  KVLS                PIC X(7)  VALUE SPACE.                   
013700         05  TIREFEFT-TKN        PIC X(1)  VALUE SPACE.                   
013800         05  TIREFEFT            PIC X(6)  VALUE SPACE.                   
013900     03  WS-URVAL2.                                                       
014000         05  BEART               PIC X(25) VALUE SPACE.                   
014100         05  KDPSLLOC            PIC X(2)  VALUE SPACE.                   
014200     03  WS-URVAL3.                                                       
014300         05  PRISRAD             PIC X(2)  VALUE SPACE.                   
014400         05  PBRAD               PIC X(1)  VALUE SPACE.                   
014500         05  IDREFTAB            PIC X(1)  VALUE SPACE.                   
014600         05  FLKVROS             PIC X(1)  VALUE SPACE.                   
014700         05  FLONORDER           PIC X(1)  VALUE SPACE.                   
014800         05  FLAK-DC             PIC X(1)  VALUE SPACE.                   
014900         05  FLASEAS             PIC X(1)  VALUE SPACE.                   
015000         05  IDLEVNR-CDC         PIC X(5)  VALUE SPACE.                   
015100         05  IDLEVNR-DC          PIC X(5)  VALUE SPACE.                   
015200         05  FLREFILL            PIC X(1)  VALUE SPACE.                   
015300         05  FLREFBEO            PIC X(1)  VALUE SPACE.                   
015400         05  VKART-TKN           PIC X(1)  VALUE SPACE.                   
015500         05  VKART               PIC X(7)  VALUE SPACE.                   
015600         05  VLARTNTO-TKN        PIC X(1)  VALUE SPACE.                   
015700         05  VLARTNTO            PIC X(9)  VALUE SPACE.                   
015800         05  ADPLATS-TOM         PIC X(5)  VALUE SPACE.                   
015900         05  PRARTSTD-TKN        PIC X(1)  VALUE SPACE.                   
016000         05  PRARTSTD            PIC X(9)  VALUE SPACE.                   
016100         05  TIFINLV-TKN         PIC X(1)  VALUE SPACE.                   
016200         05  TIFINLV             PIC X(5)  VALUE SPACE.                   
016300         05  BEMODELL            PIC X(15) VALUE SPACE.                   
016400         05  KDREFSTA            PIC X(1)  VALUE SPACE.                   
016500         05  SUPERWEEK-TKN       PIC X(1)  VALUE SPACE.                   
016600         05  SUPERWEEK           PIC X(3)  VALUE SPACE.                   
016700         05  FLFLYG              PIC X(1)  VALUE SPACE.                   
016800     03  WS-IDMAIL.                                                       
016900         05  IDMAIL              PIC X(58) VALUE SPACE.                   
017000                                                                          
017100     EJECT                                                                
017200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
017300*                                                                         
017400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
017500     SKIP3                                                                
017600*01  MID -COPY W2I34801                                                   
017700     EJECT                                                                
017800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
017900     SKIP3                                                                
018000*01  -COPY WMSGAREA                                                       
018100     EJECT                                                                
018200     03  MOD REDEFINES MSG-AREA.                                          
018300*      05  -COPY W2O34801                                                 
018400     EJECT                                                                
018500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
018600     SKIP3                                                                
018700*01  -COPY WMFSAREA                                                       
018800     EJECT                                                                
018900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019000     SKIP3                                                                
019100*    --- STATUS-KOD FRÅN IMS                                              
019200 01  STATUS-WS                   PIC XX.                                  
019300     88  SEGMENT-FINNS                       VALUE '  '.                  
019400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
019500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
019600     SKIP2                                                                
019700 01  GODK-STATUSKODER.                                                    
019800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019900     EJECT                                                                
020000*- - - - - - - - - - - - - - -DLI INPUT-OUTPUT AREA                       
020100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-P3'.           
020200     SKIP3                                                                
020300 01  DLI-IO-P3.                                                           
020400*    03  -COPY WDP311                                                     
020500                                                                          
020600 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
020700 01   DLI-IO-AREA-B601.                                                   
020800*     03  -COPY WDB601                                                    
020900     EJECT                                                                
021000 01  SSA1                        PIC X(64).                               
021100 01  SSA2                        PIC X(64).                               
021200*    --- IMS FUNKTIONSKODER                                               
021300*01  -COPY W0003                                                          
021400     EJECT                                                                
021500 LINKAGE SECTION.                                                         
021600*01  -COPY W0009   -PRE MSG-                                              
021700                                                                          
021800*01  -COPY W0009   -PRE ALT-                                              
021900                                                                          
022000*01  -COPY W0008   -PRE WDP7-                                             
022100     05  FILLER                  PIC X.                                   
022200                                                                          
022300*01  -COPY W0008   -PRE WDP3-                                             
022400     05  FILLER                  PIC X.                                   
022500                                                                          
022600*01  -COPY W0008   -PRE WDB6-                                             
022700     05  FILLER                  PIC X.                                   
022800     EJECT                                                                
022900 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB WDP7-PCB WDP3-PCB              
023000                           WDB6-PCB.                                      
023100 MAIN SECTION.                                                            
023200     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDP7-PCB WDP3-PCB              
023300                           WDB6-PCB.                                      
023400                                                                          
023500     PERFORM IMS-GET-MSG                                                  
023600     IF SEGMENT-FINNS                                                     
023700       PERFORM A-INIT                                                     
023800       IF MFS-UPDATE                                                      
023900          PERFORM B-KOLLA-INPUT                                           
024000          IF INDATA-OK                                                    
024100             PERFORM H-UPPDATERA                                          
024200             PERFORM MFS-RENSA-FAELT-IN                                   
024300          END-IF                                                          
024400       ELSE                                                               
024500          IF MFS-FIRST                                                    
024600            PERFORM C-FOERSTA-SIDA                                        
024700          ELSE                                                            
024800            PERFORM E-SAMMA-SIDA                                          
024900          END-IF                                                          
025000       END-IF                                                             
025100       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O34801 + 4                      
025200       PERFORM IMS-INSERT-MSG                                             
025300     END-IF                                                               
025400                                                                          
025500     MOVE ZERO TO RETURN-CODE                                             
025600     GOBACK                                                               
025700     .                                                                    
025800     EJECT                                                                
025900 A-INIT SECTION.                                                          
026000                                                                          
026100     IF MSG-DUBBLA-TRANSKODER                                             
026200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I34801                 
026300       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
026400       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
026500     ELSE                                                                 
026600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I34801                  
026700       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
026800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
026900     END-IF                                                               
027000                                                                          
027100     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
027200     MOVE MSG-IDPFK TO MFS-IDPFK                                          
027300     MOVE MFS-IDTRANS TO W-IDTRANS                                        
027400                                                                          
027500     MOVE LOW-VALUE TO MSG-AREA                                           
027600     MOVE 'W2O348N1' TO MFS-IDMOD                                         
027700     MOVE '2348' TO MOD-IDTRANS                                           
027800     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
027900                                                                          
028000     IF NOT EGEN-MID                                                      
028100       MOVE SPACE TO MFS-KDTRTYP                                          
028200       MOVE '7' TO MFS-IDPFK                                              
028300     END-IF                                                               
028400     .                                                                    
028500     EJECT                                                                
028600 B-KOLLA-INPUT   SECTION.                                                 
028700                                                                          
028800     MOVE ALL '+'            TO MSGI-WMSGINIT                             
028900     MOVE '013'              TO MSGI-KDCALL                               
029000     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
029100     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
029200     MOVE '2348'             TO MSGI-IDTRANS                              
029300     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
029400     MOVE MSGI-IDLAND-SPR    TO IDLAND-SPR                                
029500                                                                          
029600     MOVE JA TO INDATA-SW                                                 
029700                                                                          
029800     IF MID-W2I34801 = ALL '+'                                            
029900                                                                          
030000        MOVE ERR-PF11-AND-NO-DATA                                         
030100                             TO MED-IDMFSINF                              
030200        CALL WMEDKONV USING MED-WMEDAREA                                  
030300        MOVE MED-TEMFSINF    TO MOD-TEMFSINF                              
030400        PERFORM MFS-RENSA-FAELT-IN                                        
030500        MOVE NEJ             TO INDATA-SW                                 
030600     ELSE                                                                 
030700                                                                          
030800        PERFORM BA-KONTROLL-URVAL                                         
030900                                                                          
031000        PERFORM BB-KONTROLL-OVR                                           
031100                                                                          
031200        IF INDATA-OK                                                      
031300           PERFORM BC-KONTROLL-FOM-TOM                                    
031400        END-IF                                                            
031500        IF INDATA-FEL                                                     
031600           MOVE ERR-CORR-HILITE-FLDS                                      
031700                             TO MED-IDMFSFEL                              
031800           CALL WMEDKONV USING MED-WMEDAREA                               
031900           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
032000          PERFORM MFS-ROER-EJ-FAELT-IN                                    
032100        END-IF                                                            
032200     END-IF                                                               
032300     .                                                                    
032400     EJECT                                                                
032500 BA-KONTROLL-URVAL SECTION.                                               
032600                                                                          
032700     IF  MID-IDDC NOT = ALL '+'                                           
032800     AND MID-IDDC NOT = SPACE                                             
032900       MOVE MID-IDDC         TO W-IDDC-B6                                 
033000       PERFORM IMS-GU-WDB601                                              
033100       IF DCS-KDDC = SPACE OR DCS-DDC                                     
033200         MOVE MFS-ALFA-FAELT-FEL                                          
033300                             TO MOD-IDDC-ATTR                             
033400         MOVE NEJ            TO INDATA-SW                                 
033500       ELSE                                                               
033600         MOVE MFS-ALFA-FAELT-RAETT                                        
033700                             TO MOD-IDDC-ATTR                             
033800         MOVE MID-IDDC       TO IDDC                                      
033900       END-IF                                                             
034000     END-IF                                                               
034100                                                                          
034200     IF  MID-FLKVROS NOT = ALL '+'                                        
034300     AND MID-FLKVROS NOT = SPACE                                          
034400       IF MID-FLKVROS = JA                                                
034500       OR MID-FLKVROS = YES                                               
034600       OR MID-FLKVROS = NEJ                                               
034700         MOVE MFS-ALFA-FAELT-RAETT                                        
034800                             TO MOD-FLKVROS-ATTR                          
034900         MOVE MID-FLKVROS    TO FLKVROS                                   
035000       ELSE                                                               
035100         MOVE MFS-ALFA-FAELT-FEL                                          
035200                             TO MOD-FLKVROS-ATTR                          
035300         MOVE NEJ            TO INDATA-SW                                 
035400       END-IF                                                             
035500     END-IF                                                               
035600                                                                          
035700     IF MID-IDPERSON-BUY-FOM NOT = ALL '+'                                
035800        IF MID-IDPERSON-BUY-FOM NOT NUMERIC                               
035900           MOVE MFS-NUM-FAELT-FEL                                         
036000                             TO MOD-IDPERSON-BUY-FOM-ATTR                 
036100           MOVE NEJ          TO INDATA-SW                                 
036200        ELSE                                                              
036300           MOVE MFS-NUM-FAELT-RAETT                                       
036400                             TO MOD-IDPERSON-BUY-FOM-ATTR                 
036500           MOVE MID-IDPERSON-BUY-FOM                                      
036600                             TO IDPERSON-BUY-FOM                          
036700        END-IF                                                            
036800     END-IF                                                               
036900                                                                          
037000     IF MID-IDPERSON-BUY-TOM NOT = ALL '+'                                
037100        IF MID-IDPERSON-BUY-TOM NOT NUMERIC                               
037200           MOVE MFS-NUM-FAELT-FEL                                         
037300                             TO MOD-IDPERSON-BUY-TOM-ATTR                 
037400           MOVE NEJ          TO INDATA-SW                                 
037500        ELSE                                                              
037600           MOVE MFS-NUM-FAELT-RAETT                                       
037700                             TO MOD-IDPERSON-BUY-TOM-ATTR                 
037800           MOVE MID-IDPERSON-BUY-TOM                                      
037900                             TO IDPERSON-BUY-TOM                          
038000        END-IF                                                            
038100     END-IF                                                               
038200     IF MID-IDPERSON-BUY2 NOT = ALL '+'                                   
038300        IF MID-IDPERSON-BUY2 NOT NUMERIC                                  
038400           MOVE MFS-NUM-FAELT-FEL                                         
038500                             TO MOD-IDPERSON-BUY2-ATTR                    
038600           MOVE NEJ          TO INDATA-SW                                 
038700        ELSE                                                              
038800           IF MID-IDPERSON-BUY-FOM = ALL '+'                              
038900           AND MID-IDPERSON-BUY-TOM = ALL '+'                             
039000             MOVE MFS-NUM-FAELT-RAETT                                     
039100                               TO MOD-IDPERSON-BUY2-ATTR                  
039200             MOVE MID-IDPERSON-BUY2                                       
039300                               TO IDPERSON-BUY2                           
039400           ELSE                                                           
039500             MOVE MFS-NUM-FAELT-FEL                                       
039600                               TO MOD-IDPERSON-BUY2-ATTR                  
039700             MOVE NEJ        TO INDATA-SW                                 
039800           END-IF                                                         
039900        END-IF                                                            
040000     END-IF                                                               
040100     IF MID-IDPERSON-BUY3 NOT = ALL '+'                                   
040200        IF MID-IDPERSON-BUY3 NOT NUMERIC                                  
040300           MOVE MFS-NUM-FAELT-FEL                                         
040400                             TO MOD-IDPERSON-BUY3-ATTR                    
040500           MOVE NEJ          TO INDATA-SW                                 
040600        ELSE                                                              
040700           IF MID-IDPERSON-BUY-FOM = ALL '+'                              
040800           AND MID-IDPERSON-BUY-TOM = ALL '+'                             
040900             MOVE MFS-NUM-FAELT-RAETT                                     
041000                               TO MOD-IDPERSON-BUY3-ATTR                  
041100             MOVE MID-IDPERSON-BUY3                                       
041200                               TO IDPERSON-BUY3                           
041300           ELSE                                                           
041400             MOVE MFS-NUM-FAELT-FEL                                       
041500                               TO MOD-IDPERSON-BUY3-ATTR                  
041600             MOVE NEJ        TO INDATA-SW                                 
041700           END-IF                                                         
041800        END-IF                                                            
041900     END-IF                                                               
042000     IF MID-IDPERSON-BUY4 NOT = ALL '+'                                   
042100        IF MID-IDPERSON-BUY4 NOT NUMERIC                                  
042200           MOVE MFS-NUM-FAELT-FEL                                         
042300                             TO MOD-IDPERSON-BUY4-ATTR                    
042400           MOVE NEJ          TO INDATA-SW                                 
042500        ELSE                                                              
042600           IF MID-IDPERSON-BUY-FOM = ALL '+'                              
042700           AND MID-IDPERSON-BUY-TOM = ALL '+'                             
042800             MOVE MFS-NUM-FAELT-RAETT                                     
042900                               TO MOD-IDPERSON-BUY4-ATTR                  
043000             MOVE MID-IDPERSON-BUY4                                       
043100                               TO IDPERSON-BUY4                           
043200           ELSE                                                           
043300             MOVE MFS-NUM-FAELT-FEL                                       
043400                               TO MOD-IDPERSON-BUY4-ATTR                  
043500             MOVE NEJ        TO INDATA-SW                                 
043600           END-IF                                                         
043700        END-IF                                                            
043800     END-IF                                                               
043900                                                                          
044000     IF  MID-FLONORDER NOT = ALL '+'                                      
044100     AND MID-FLONORDER NOT = SPACE                                        
044200       IF MID-FLONORDER = JA                                              
044300       OR MID-FLONORDER = YES                                             
044400       OR MID-FLONORDER = NEJ                                             
044500         MOVE MFS-ALFA-FAELT-RAETT                                        
044600                             TO MOD-FLONORDER-ATTR                        
044700         MOVE MID-FLONORDER  TO FLONORDER                                 
044800       ELSE                                                               
044900         MOVE MFS-ALFA-FAELT-FEL                                          
045000                             TO MOD-FLONORDER-ATTR                        
045100         MOVE NEJ            TO INDATA-SW                                 
045200       END-IF                                                             
045300     END-IF                                                               
045400                                                                          
045500     MOVE +1 TO IX                                                        
045600     PERFORM UNTIL IX > 3                                                 
045700       IF  MID-IDPROJ(IX) NOT = ALL '+'                                   
045800       AND MID-IDPROJ(IX) NOT = SPACE                                     
045900         MOVE MFS-ALFA-FAELT-RAETT                                        
046000                             TO MOD-IDPROJ-ATTR (IX)                      
046100         MOVE MID-IDPROJ (IX)                                             
046200                             TO IDPROJ (IX)                               
046300       END-IF                                                             
046400       ADD +1 TO IX                                                       
046500     END-PERFORM                                                          
046600                                                                          
046700     IF  MID-FLAK-DC NOT = ALL '+'                                        
046800     AND MID-FLAK-DC NOT = SPACE                                          
046900       IF MID-FLAK-DC = JA                                                
047000       OR MID-FLAK-DC = YES                                               
047100       OR MID-FLAK-DC = NEJ                                               
047200         MOVE MFS-ALFA-FAELT-RAETT                                        
047300                             TO MOD-FLAK-DC-ATTR                          
047400         MOVE MID-FLAK-DC    TO FLAK-DC                                   
047500       ELSE                                                               
047600         MOVE MFS-ALFA-FAELT-FEL                                          
047700                             TO MOD-FLAK-DC-ATTR                          
047800         MOVE NEJ            TO INDATA-SW                                 
047900       END-IF                                                             
048000     END-IF                                                               
048100                                                                          
048200     IF  MID-FLASEAS NOT = ALL '+'                                        
048300     AND MID-FLASEAS NOT = SPACE                                          
048400       IF MID-FLASEAS = JA                                                
048500       OR MID-FLASEAS = YES                                               
048600       OR MID-FLASEAS = NEJ                                               
048700         MOVE MFS-ALFA-FAELT-RAETT                                        
048800                             TO MOD-FLASEAS-ATTR                          
048900         MOVE MID-FLASEAS     TO FLASEAS                                  
049000       ELSE                                                               
049100         MOVE MFS-ALFA-FAELT-FEL                                          
049200                             TO MOD-FLASEAS-ATTR                          
049300         MOVE NEJ            TO INDATA-SW                                 
049400       END-IF                                                             
049500     END-IF                                                               
049600                                                                          
049700     IF  MID-IDLEVNR-CDC NOT = ALL '+'                                    
049800     AND MID-IDLEVNR-CDC NOT = SPACE                                      
049900         MOVE MFS-ALFA-FAELT-RAETT                                        
050000                             TO MOD-IDLEVNR-CDC-ATTR                      
050100         MOVE MID-IDLEVNR-CDC                                             
050200                             TO IDLEVNR-CDC                               
050300     END-IF                                                               
050400                                                                          
050500     IF MID-KDPSLLOC NOT = ALL '+'                                        
050600        IF MID-KDPSLLOC NOT NUMERIC                                       
050700           MOVE MFS-NUM-FAELT-FEL                                         
050800                             TO MOD-KDPSLLOC-ATTR                         
050900           MOVE NEJ          TO INDATA-SW                                 
051000        ELSE                                                              
051100           MOVE MFS-NUM-FAELT-RAETT                                       
051200                             TO MOD-KDPSLLOC-ATTR                         
051300           MOVE MID-KDPSLLOC                                              
051400                             TO KDPSLLOC                                  
051500        END-IF                                                            
051600     END-IF                                                               
051700                                                                          
051800     IF  MID-IDLEVNR-DC NOT = ALL '+'                                     
051900     AND MID-IDLEVNR-DC NOT = SPACE                                       
052000         MOVE MFS-ALFA-FAELT-RAETT                                        
052100                             TO MOD-IDLEVNR-DC-ATTR                       
052200         MOVE MID-IDLEVNR-DC TO IDLEVNR-DC                                
052300     END-IF                                                               
052400                                                                          
052500     IF MID-IDFKNGRP-FOM NOT = ALL '+'                                    
052600        IF MID-IDFKNGRP-FOM NOT NUMERIC                                   
052700          MOVE MFS-NUM-FAELT-FEL                                          
052800                             TO MOD-IDFKNGRP-FOM-ATTR                     
052900          MOVE NEJ           TO INDATA-SW                                 
053000        ELSE                                                              
053100          MOVE MFS-NUM-FAELT-RAETT                                        
053200                             TO MOD-IDFKNGRP-FOM-ATTR                     
053300          MOVE MID-IDFKNGRP-FOM                                           
053400                             TO IDFKNGRP-FOM                              
053500          MOVE ZERO          TO IDFKNGRP-TOM                              
053600        END-IF                                                            
053700     END-IF                                                               
053800                                                                          
053900     IF MID-IDFKNGRP-TOM NOT = ALL '+'                                    
054000        IF MID-IDFKNGRP-TOM NOT NUMERIC                                   
054100          MOVE MFS-NUM-FAELT-FEL                                          
054200                             TO MOD-IDFKNGRP-TOM-ATTR                     
054300          MOVE NEJ           TO INDATA-SW                                 
054400        ELSE                                                              
054500          MOVE MFS-NUM-FAELT-RAETT                                        
054600                             TO MOD-IDFKNGRP-TOM-ATTR                     
054700          MOVE MID-IDFKNGRP-TOM                                           
054800                             TO IDFKNGRP-TOM                              
054900        END-IF                                                            
055000     END-IF                                                               
055100                                                                          
055200     IF  MID-FLREFILL NOT = ALL '+'                                       
055300     AND MID-FLREFILL NOT = SPACE                                         
055400       IF MID-FLREFILL = JA                                               
055500       OR MID-FLREFILL = YES                                              
055600       OR MID-FLREFILL = NEJ                                              
055700       OR MID-FLREFILL = 'O'                                              
055800****MID-FLREFILL = 'O' ÄR OM ARTIKEL FINNS PÅ WDK7 OCH EJ WDL7            
055900         MOVE MFS-ALFA-FAELT-RAETT                                        
056000                             TO MOD-FLREFILL-ATTR                         
056100         MOVE MID-FLREFILL   TO FLREFILL                                  
056200       ELSE                                                               
056300         MOVE MFS-ALFA-FAELT-FEL                                          
056400                             TO MOD-FLREFILL-ATTR                         
056500         MOVE NEJ            TO INDATA-SW                                 
056600       END-IF                                                             
056700     END-IF                                                               
056800                                                                          
056900     IF MID-PRISRAD NOT = ALL '+'                                         
057000        IF MID-PRISRAD NOT NUMERIC                                        
057100           MOVE MFS-NUM-FAELT-FEL                                         
057200                             TO MOD-PRISRAD-ATTR                          
057300           MOVE NEJ          TO INDATA-SW                                 
057400        ELSE                                                              
057500           MOVE MFS-NUM-FAELT-RAETT                                       
057600                             TO MOD-PRISRAD-ATTR                          
057700           MOVE MID-PRISRAD  TO PRISRAD                                   
057800        END-IF                                                            
057900     END-IF                                                               
058000                                                                          
058100     IF  MID-PBRAD NOT = ALL '+'                                          
058200     AND MID-PBRAD NOT = SPACE                                            
058300       MOVE MFS-ALFA-FAELT-RAETT                                          
058400                             TO MOD-PBRAD-ATTR                            
058500       MOVE MID-PBRAD        TO PBRAD                                     
058600     END-IF                                                               
058700                                                                          
058800     IF  MID-FLREFBEO NOT = ALL '+'                                       
058900     AND MID-FLREFBEO NOT = SPACE                                         
059000       IF MID-FLREFBEO = JA                                               
059100       OR MID-FLREFBEO = YES                                              
059200       OR MID-FLREFBEO = NEJ                                              
059300       OR MID-FLREFBEO = STOPP                                            
059400         MOVE MFS-ALFA-FAELT-RAETT                                        
059500                             TO MOD-FLREFBEO-ATTR                         
059600         MOVE MID-FLREFBEO   TO FLREFBEO                                  
059700       ELSE                                                               
059800         MOVE MFS-ALFA-FAELT-FEL                                          
059900                             TO MOD-FLREFBEO-ATTR                         
060000         MOVE NEJ            TO INDATA-SW                                 
060100       END-IF                                                             
060200     END-IF                                                               
060300                                                                          
060400     IF MID-IDREFTAB NOT = ALL '+'                                        
060500        IF MID-IDREFTAB NOT NUMERIC                                       
060600           MOVE MFS-NUM-FAELT-FEL                                         
060700                             TO MOD-IDREFTAB-ATTR                         
060800           MOVE NEJ          TO INDATA-SW                                 
060900        ELSE                                                              
061000           MOVE MFS-NUM-FAELT-RAETT                                       
061100                             TO MOD-IDREFTAB-ATTR                         
061200           MOVE MID-IDREFTAB TO IDREFTAB                                  
061300        END-IF                                                            
061400     END-IF                                                               
061500                                                                          
061600     IF  MID-VKART-TKN NOT = ALL '+'                                      
061700     AND MID-VKART-TKN NOT = SPACE                                        
061800       IF MID-VKART-TKN = '='                                             
061900       OR MID-VKART-TKN = '>'                                             
062000       OR MID-VKART-TKN = '<'                                             
062100         MOVE MFS-ALFA-FAELT-RAETT                                        
062200                             TO MOD-VKART-TKN-ATTR                        
062300         MOVE MID-VKART-TKN  TO VKART-TKN                                 
062400       ELSE                                                               
062500         MOVE MFS-ALFA-FAELT-FEL                                          
062600                             TO MOD-VKART-TKN-ATTR                        
062700         MOVE NEJ            TO INDATA-SW                                 
062800       END-IF                                                             
062900     END-IF                                                               
063000                                                                          
063100     IF MID-VKART NOT = ALL '+'                                           
063200        IF MID-VKART NOT NUMERIC                                          
063300           MOVE MFS-NUM-FAELT-FEL                                         
063400                             TO MOD-VKART-ATTR                            
063500           MOVE NEJ          TO INDATA-SW                                 
063600        ELSE                                                              
063700           MOVE MFS-NUM-FAELT-RAETT                                       
063800                             TO MOD-VKART-ATTR                            
063900           MOVE MID-VKART    TO VKART                                     
064000        END-IF                                                            
064100     END-IF                                                               
064200                                                                          
064300     IF MID-KVPB-FOM NOT = ALL '+'                                        
064400        MOVE MID-KVPB-FOM    TO DEC-IDFRIDATA                             
064500        MOVE 6               TO DEC-KVHELTAL                              
064600        MOVE 1               TO DEC-KVDECIMAL                             
064700        CALL WDECEDIT USING DEC-WDECAREA                                  
064800        IF DEC-KDSVAR-OK                                                  
064900          MOVE MFS-ADD-LAES-IN-FAELT                                      
065000                             TO MOD-KVPB-FOM-ATTR                         
065100          MOVE DEC-IDEDITDATA                                             
065200                             TO WS-RED-KVPB-REF                           
065300                                WS-KVPB-REF-FOM                           
065400          MOVE WS-RED-KVPB-REF                                            
065500                             TO MOD-KVPB-FOM                              
065600                                KVPB-FOM                                  
065700          MOVE ZERO          TO KVPB-TOM                                  
065800                                WS-KVPB-REF-TOM                           
065900        ELSE                                                              
066000          MOVE MFS-ALFA-FAELT-FEL                                         
066100                             TO MOD-KVPB-FOM-ATTR                         
066200          MOVE NEJ           TO INDATA-SW                                 
066300        END-IF                                                            
066400     END-IF                                                               
066500                                                                          
066600     IF MID-KVPB-TOM NOT = ALL '+'                                        
066700        MOVE MID-KVPB-TOM    TO DEC-IDFRIDATA                             
066800        MOVE 6               TO DEC-KVHELTAL                              
066900        MOVE 1               TO DEC-KVDECIMAL                             
067000        CALL WDECEDIT USING DEC-WDECAREA                                  
067100        IF DEC-KDSVAR-OK                                                  
067200          MOVE MFS-ADD-LAES-IN-FAELT                                      
067300                             TO MOD-KVPB-TOM-ATTR                         
067400          MOVE DEC-IDEDITDATA                                             
067500                             TO WS-RED-KVPB-REF                           
067600                                WS-KVPB-REF-TOM                           
067700          MOVE WS-RED-KVPB-REF                                            
067800                             TO MOD-KVPB-TOM                              
067900                                KVPB-TOM                                  
068000        ELSE                                                              
068100          MOVE MFS-ALFA-FAELT-FEL                                         
068200                             TO MOD-KVPB-TOM-ATTR                         
068300          MOVE NEJ           TO INDATA-SW                                 
068400        END-IF                                                            
068500     END-IF                                                               
068600                                                                          
068700     IF  MID-VLARTNTO-TKN NOT = ALL '+'                                   
068800     AND MID-VLARTNTO-TKN NOT = SPACE                                     
068900       IF MID-VLARTNTO-TKN = '='                                          
069000       OR MID-VLARTNTO-TKN = '>'                                          
069100       OR MID-VLARTNTO-TKN = '<'                                          
069200         MOVE MFS-ALFA-FAELT-RAETT                                        
069300                             TO MOD-VLARTNTO-TKN-ATTR                     
069400         MOVE MID-VLARTNTO-TKN TO VLARTNTO-TKN                            
069500       ELSE                                                               
069600         MOVE MFS-ALFA-FAELT-FEL                                          
069700                             TO MOD-VLARTNTO-TKN-ATTR                     
069800         MOVE NEJ            TO INDATA-SW                                 
069900       END-IF                                                             
070000     END-IF                                                               
070100                                                                          
070200     IF MID-VLARTNTO NOT = ALL '+'                                        
070300        IF MID-VLARTNTO NOT NUMERIC                                       
070400           MOVE MFS-NUM-FAELT-FEL                                         
070500                             TO MOD-VLARTNTO-ATTR                         
070600           MOVE NEJ          TO INDATA-SW                                 
070700        ELSE                                                              
070800           MOVE MFS-NUM-FAELT-RAETT                                       
070900                             TO MOD-VLARTNTO-ATTR                         
071000           MOVE MID-VLARTNTO TO VLARTNTO                                  
071100        END-IF                                                            
071200     END-IF                                                               
071300                                                                          
071400     IF MID-ADLAGOMR NOT = ALL '+'                                        
071500        IF MID-ADLAGOMR NOT NUMERIC                                       
071600           MOVE MFS-NUM-FAELT-FEL                                         
071700                             TO MOD-ADLAGOMR-ATTR                         
071800           MOVE NEJ          TO INDATA-SW                                 
071900        ELSE                                                              
072000           MOVE MFS-NUM-FAELT-RAETT                                       
072100                             TO MOD-ADLAGOMR-ATTR                         
072200           MOVE MID-ADLAGOMR TO ADLAGOMR                                  
072300        END-IF                                                            
072400     END-IF                                                               
072500                                                                          
072600     IF MID-ADGANG NOT = ALL '+'                                          
072700        IF MID-ADGANG NOT NUMERIC                                         
072800           MOVE MFS-NUM-FAELT-FEL                                         
072900                             TO MOD-ADGANG-ATTR                           
073000           MOVE NEJ          TO INDATA-SW                                 
073100        ELSE                                                              
073200           MOVE MFS-NUM-FAELT-RAETT                                       
073300                             TO MOD-ADGANG-ATTR                           
073400           MOVE MID-ADGANG   TO ADGANG                                    
073500        END-IF                                                            
073600     END-IF                                                               
073700                                                                          
073800     IF MID-ADPLATS-FOM NOT = ALL '+'                                     
073900        IF MID-ADPLATS-FOM NOT NUMERIC                                    
074000           MOVE MFS-NUM-FAELT-FEL                                         
074100                             TO MOD-ADPLATS-FOM-ATTR                      
074200           MOVE NEJ          TO INDATA-SW                                 
074300        ELSE                                                              
074400           MOVE MFS-NUM-FAELT-RAETT                                       
074500                             TO MOD-ADPLATS-FOM-ATTR                      
074600           MOVE MID-ADPLATS-FOM                                           
074700                             TO ADPLATS-FOM                               
074800           MOVE ZERO         TO ADPLATS-TOM                               
074900        END-IF                                                            
075000     END-IF                                                               
075100                                                                          
075200     IF MID-ADPLATS-TOM NOT = ALL '+'                                     
075300        IF MID-ADPLATS-TOM NOT NUMERIC                                    
075400           MOVE MFS-NUM-FAELT-FEL                                         
075500                             TO MOD-ADPLATS-TOM-ATTR                      
075600           MOVE NEJ          TO INDATA-SW                                 
075700        ELSE                                                              
075800           MOVE MFS-NUM-FAELT-RAETT                                       
075900                             TO MOD-ADPLATS-TOM-ATTR                      
076000           MOVE MID-ADPLATS-TOM TO ADPLATS-TOM                            
076100        END-IF                                                            
076200     END-IF                                                               
076300                                                                          
076400     IF MID-KDERS NOT = ALL '+'                                           
076500        IF MID-KDERS NOT NUMERIC                                          
076600           MOVE MFS-NUM-FAELT-FEL                                         
076700                             TO MOD-KDERS-ATTR                            
076800           MOVE NEJ          TO INDATA-SW                                 
076900        ELSE                                                              
077000           MOVE MFS-NUM-FAELT-RAETT                                       
077100                             TO MOD-KDERS-ATTR                            
077200           MOVE MID-KDERS    TO WS-KDERS                                  
077300           MOVE WS-KDERS     TO KDERS                                     
077400        END-IF                                                            
077500     END-IF                                                               
077600                                                                          
077700     IF  MID-PRARTSTD-TKN NOT = ALL '+'                                   
077800     AND MID-PRARTSTD-TKN NOT = SPACE                                     
077900       IF MID-PRARTSTD-TKN = '='                                          
078000       OR MID-PRARTSTD-TKN = '>'                                          
078100       OR MID-PRARTSTD-TKN = '<'                                          
078200         MOVE MFS-ALFA-FAELT-RAETT                                        
078300                             TO MOD-PRARTSTD-TKN-ATTR                     
078400         MOVE MID-PRARTSTD-TKN TO PRARTSTD-TKN                            
078500       ELSE                                                               
078600         MOVE MFS-ALFA-FAELT-FEL                                          
078700                             TO MOD-PRARTSTD-TKN-ATTR                     
078800         MOVE NEJ            TO INDATA-SW                                 
078900       END-IF                                                             
079000     END-IF                                                               
079100                                                                          
079200     IF MID-PRARTSTD NOT = ALL '+'                                        
079300        IF MID-PRARTSTD NOT NUMERIC                                       
079400           MOVE MFS-NUM-FAELT-FEL                                         
079500                             TO MOD-PRARTSTD-ATTR                         
079600           MOVE NEJ          TO INDATA-SW                                 
079700        ELSE                                                              
079800           MOVE MFS-NUM-FAELT-RAETT                                       
079900                             TO MOD-PRARTSTD-ATTR                         
080000           MOVE MID-PRARTSTD TO PRARTSTD                                  
080100        END-IF                                                            
080200     END-IF                                                               
080300                                                                          
080400     IF  MID-KVLS-TKN NOT = ALL '+'                                       
080500     AND MID-KVLS-TKN NOT = SPACE                                         
080600       IF MID-KVLS-TKN = '='                                              
080700       OR MID-KVLS-TKN = '>'                                              
080800       OR MID-KVLS-TKN = '<'                                              
080900         MOVE MFS-ALFA-FAELT-RAETT                                        
081000                             TO MOD-KVLS-TKN-ATTR                         
081100         MOVE MID-KVLS-TKN   TO KVLS-TKN                                  
081200       ELSE                                                               
081300         MOVE MFS-ALFA-FAELT-FEL                                          
081400                             TO MOD-KVLS-TKN-ATTR                         
081500         MOVE NEJ            TO INDATA-SW                                 
081600       END-IF                                                             
081700     END-IF                                                               
081800                                                                          
081900     IF MID-KVLS NOT = ALL '+'                                            
082000        IF MID-KVLS NOT NUMERIC                                           
082100           MOVE MFS-NUM-FAELT-FEL                                         
082200                             TO MOD-KVLS-ATTR                             
082300           MOVE NEJ          TO INDATA-SW                                 
082400        ELSE                                                              
082500           MOVE MFS-NUM-FAELT-RAETT                                       
082600                             TO MOD-KVLS-ATTR                             
082700           MOVE MID-KVLS     TO KVLS                                      
082800        END-IF                                                            
082900     END-IF                                                               
083000                                                                          
083100     IF  MID-TIFINLV-TKN NOT = ALL '+'                                    
083200     AND MID-TIFINLV-TKN NOT = SPACE                                      
083300       IF MID-TIFINLV-TKN = '='                                           
083400       OR MID-TIFINLV-TKN = '>'                                           
083500       OR MID-TIFINLV-TKN = '<'                                           
083600         MOVE MFS-ALFA-FAELT-RAETT                                        
083700                             TO MOD-TIFINLV-TKN-ATTR                      
083800         MOVE MID-TIFINLV-TKN TO TIFINLV-TKN                              
083900       ELSE                                                               
084000         MOVE MFS-ALFA-FAELT-FEL                                          
084100                             TO MOD-TIFINLV-TKN-ATTR                      
084200         MOVE NEJ            TO INDATA-SW                                 
084300       END-IF                                                             
084400     END-IF                                                               
084500                                                                          
084600     IF MID-TIFINLV NOT = ALL '+'                                         
084700        IF MID-TIFINLV NOT NUMERIC                                        
084800           MOVE MFS-NUM-FAELT-FEL                                         
084900                             TO MOD-TIFINLV-ATTR                          
085000           MOVE NEJ          TO INDATA-SW                                 
085100        ELSE                                                              
085200           MOVE MFS-NUM-FAELT-RAETT                                       
085300                             TO MOD-TIFINLV-ATTR                          
085400           MOVE MID-TIFINLV  TO TIFINLV                                   
085500        END-IF                                                            
085600     END-IF                                                               
085700                                                                          
085800     IF  MID-TIREFEFT-TKN NOT = ALL '+'                                   
085900     AND MID-TIREFEFT-TKN NOT = SPACE                                     
086000       IF MID-TIREFEFT-TKN = '='                                          
086100       OR MID-TIREFEFT-TKN = '>'                                          
086200       OR MID-TIREFEFT-TKN = '<'                                          
086300         MOVE MFS-ALFA-FAELT-RAETT                                        
086400                             TO MOD-TIREFEFT-TKN-ATTR                     
086500         MOVE MID-TIREFEFT-TKN                                            
086600                             TO TIREFEFT-TKN                              
086700       ELSE                                                               
086800         MOVE MFS-ALFA-FAELT-FEL                                          
086900                             TO MOD-TIREFEFT-TKN-ATTR                     
087000         MOVE NEJ            TO INDATA-SW                                 
087100       END-IF                                                             
087200     END-IF                                                               
087300                                                                          
087400     IF MID-TIREFEFT NOT = ALL '+'                                        
087500        IF MID-TIREFEFT NOT NUMERIC                                       
087600           MOVE MFS-NUM-FAELT-FEL                                         
087700                             TO MOD-TIREFEFT-ATTR                         
087800           MOVE NEJ          TO INDATA-SW                                 
087900        ELSE                                                              
088000           MOVE MFS-NUM-FAELT-RAETT                                       
088100                             TO MOD-TIREFEFT-ATTR                         
088200           MOVE MID-TIREFEFT TO TIREFEFT                                  
088300        END-IF                                                            
088400     END-IF                                                               
088500                                                                          
088600     IF  MID-BEART NOT = ALL '+'                                          
088700     AND MID-BEART NOT = SPACE                                            
088800       MOVE MFS-ALFA-FAELT-RAETT                                          
088900                             TO MOD-BEART-ATTR                            
089000       MOVE MID-BEART        TO BEART                                     
089100     END-IF                                                               
089200                                                                          
089300     IF  MID-BEMODELL NOT = ALL '+'                                       
089400     AND MID-BEMODELL NOT = SPACE                                         
089500       MOVE MFS-ALFA-FAELT-RAETT                                          
089600                             TO MOD-BEMODELL-ATTR                         
089700       MOVE MID-BEMODELL     TO BEMODELL                                  
089800     END-IF                                                               
089900                                                                          
090000     IF  MID-KDREFSTA NOT = ALL '+'                                       
090100     AND MID-KDREFSTA NOT = SPACE                                         
090200       IF MID-KDREFSTA = 'A'                                              
090300       OR MID-KDREFSTA = 'P'                                              
090400         MOVE MFS-ALFA-FAELT-RAETT                                        
090500                             TO MOD-KDREFSTA-ATTR                         
090600         MOVE MID-KDREFSTA   TO KDREFSTA                                  
090700       ELSE                                                               
090800         MOVE MFS-ALFA-FAELT-FEL                                          
090900                             TO MOD-KDREFSTA-ATTR                         
091000         MOVE NEJ            TO INDATA-SW                                 
091100       END-IF                                                             
091200     END-IF                                                               
091300                                                                          
091400     IF  MID-SUPERWEEK-TKN NOT = ALL '+'                                  
091500     AND MID-SUPERWEEK-TKN NOT = SPACE                                    
091600       IF MID-SUPERWEEK-TKN = '='                                         
091700       OR MID-SUPERWEEK-TKN = '>'                                         
091800       OR MID-SUPERWEEK-TKN = '<'                                         
091900         MOVE MFS-ALFA-FAELT-RAETT                                        
092000                             TO MOD-SUPERWEEK-TKN-ATTR                    
092100         MOVE MID-SUPERWEEK-TKN                                           
092200                             TO SUPERWEEK-TKN                             
092300       ELSE                                                               
092400         MOVE MFS-ALFA-FAELT-FEL                                          
092500                             TO MOD-SUPERWEEK-TKN-ATTR                    
092600         MOVE NEJ            TO INDATA-SW                                 
092700       END-IF                                                             
092800     END-IF                                                               
092900                                                                          
093000     IF MID-SUPERWEEK NOT = ALL '+'                                       
093100        IF MID-SUPERWEEK NOT NUMERIC                                      
093200           MOVE MFS-NUM-FAELT-FEL                                         
093300                             TO MOD-SUPERWEEK-ATTR                        
093400           MOVE NEJ          TO INDATA-SW                                 
093500        ELSE                                                              
093600           MOVE MFS-NUM-FAELT-RAETT                                       
093700                             TO MOD-SUPERWEEK-ATTR                        
093800           MOVE MID-SUPERWEEK TO SUPERWEEK                                
093900        END-IF                                                            
094000     END-IF                                                               
094100                                                                          
094200     IF  MID-FLFLYG NOT = ALL '+'                                         
094300     AND MID-FLFLYG NOT = SPACE                                           
094400       IF MID-FLFLYG = JA                                                 
094500       OR MID-FLFLYG = YES                                                
094600       OR MID-FLFLYG = NEJ                                                
094700       OR MID-FLFLYG = 'S'                                                
094800         MOVE MFS-ALFA-FAELT-RAETT                                        
094900                             TO MOD-FLFLYG-ATTR                           
095000         MOVE MID-FLFLYG     TO FLFLYG                                    
095100       ELSE                                                               
095200         MOVE MFS-ALFA-FAELT-FEL                                          
095300                             TO MOD-FLFLYG-ATTR                           
095400         MOVE NEJ            TO INDATA-SW                                 
095500       END-IF                                                             
095600     END-IF                                                               
095700                                                                          
095800     .                                                                    
095900     EJECT                                                                
096000 BB-KONTROLL-OVR   SECTION.                                               
096100                                                                          
096200     IF MID-KDARBTYP NOT = ALL '+' AND                                    
096300       MID-IDPERSON NOT = ALL '+'                                         
096400       IF MID-IDPERSON NUMERIC                                            
096500         MOVE MID-KDARBTYP   TO W-KDARBTYP                                
096600         MOVE MID-IDPERSON   TO W-IDPERSON                                
096700         PERFORM IMS-GU-WDP311                                            
096800         IF SEGMENT-FINNS                                                 
096900           MOVE PERS-IDMAIL  TO IDMAIL                                    
097000           MOVE MFS-ALFA-FAELT-RAETT                                      
097100                             TO MOD-KDARBTYP-ATTR                         
097200                                MOD-IDPERSON-ATTR                         
097300         ELSE                                                             
097400           MOVE MFS-ALFA-FAELT-FEL                                        
097500                             TO MOD-KDARBTYP-ATTR                         
097600                                MOD-IDPERSON-ATTR                         
097700           MOVE NEJ          TO INDATA-SW                                 
097800         END-IF                                                           
097900       ELSE                                                               
098000         MOVE MFS-ALFA-FAELT-FEL                                          
098100                             TO MOD-IDPERSON-ATTR                         
098200         MOVE NEJ            TO INDATA-SW                                 
098300       END-IF                                                             
098400     ELSE                                                                 
098500       MOVE MFS-ALFA-FAELT-FEL                                            
098600                             TO MOD-KDARBTYP-ATTR                         
098700                                MOD-IDPERSON-ATTR                         
098800       MOVE NEJ              TO INDATA-SW                                 
098900     END-IF                                                               
099000     .                                                                    
099100     EJECT                                                                
099200 BC-KONTROLL-FOM-TOM SECTION.                                             
099300                                                                          
099400     IF IDFKNGRP-FOM  > ZERO AND                                          
099500        IDFKNGRP-TOM  = ZERO                                              
099600        MOVE IDFKNGRP-FOM   TO IDFKNGRP-TOM                               
099700     END-IF                                                               
099800                                                                          
099900     IF IDFKNGRP-FOM > IDFKNGRP-TOM                                       
100000        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFKNGRP-FOM-ATTR                 
100100        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFKNGRP-TOM-ATTR                 
100200        MOVE NEJ TO INDATA-SW                                             
100300     END-IF                                                               
100400                                                                          
100500     IF IDPERSON-BUY-FOM > ZERO AND                                       
100600        IDPERSON-BUY-TOM = ZERO                                           
100700        MOVE IDPERSON-BUY-FOM TO IDPERSON-BUY-TOM                         
100800     END-IF                                                               
100900                                                                          
101000     IF IDPERSON-BUY-FOM > IDPERSON-BUY-TOM                               
101100        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPERSON-BUY-FOM-ATTR             
101200        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPERSON-BUY-TOM-ATTR             
101300        MOVE NEJ TO INDATA-SW                                             
101400     END-IF                                                               
101500                                                                          
101600     IF  WS-KVPB-REF-FOM > ZERO                                           
101700     AND WS-KVPB-REF-TOM = ZERO                                           
101800        MOVE WS-KVPB-REF-FOM                                              
101900                            TO WS-KVPB-REF-TOM                            
102000        MOVE KVPB-FOM       TO KVPB-TOM                                   
102100     END-IF                                                               
102200*                                                                         
102300     IF WS-KVPB-REF-FOM > WS-KVPB-REF-TOM                                 
102400        MOVE MFS-NUM-FAELT-FEL                                            
102500                             TO MOD-KVPB-FOM-ATTR                         
102600        MOVE MFS-NUM-FAELT-FEL                                            
102700                             TO MOD-KVPB-TOM-ATTR                         
102800        MOVE NEJ TO INDATA-SW                                             
102900     END-IF                                                               
103000                                                                          
103100     IF ADPLATS-FOM   > ZERO AND                                          
103200        ADPLATS-TOM   = ZERO                                              
103300        MOVE ADPLATS-FOM    TO ADPLATS-TOM                                
103400     END-IF                                                               
103500                                                                          
103600     IF ADPLATS-FOM > ADPLATS-TOM                                         
103700        MOVE MFS-NUM-FAELT-FEL                                            
103800                             TO MOD-ADPLATS-FOM-ATTR                      
103900        MOVE MFS-NUM-FAELT-FEL                                            
104000                             TO MOD-ADPLATS-TOM-ATTR                      
104100        MOVE NEJ TO INDATA-SW                                             
104200     END-IF                                                               
104300     .                                                                    
104400     EJECT                                                                
104500 C-FOERSTA-SIDA SECTION.                                                  
104600                                                                          
104700     MOVE INF-PRESS-PF11     TO MED-IDMFSINF                              
104800     CALL WMEDKONV USING MED-WMEDAREA                                     
104900     MOVE MED-TEMFSINF       TO MOD-TEMFSINF                              
105000                                                                          
105100     PERFORM MFS-RENSA-FAELT-IN                                           
105200     .                                                                    
105300     EJECT                                                                
105400 E-SAMMA-SIDA      SECTION.                                               
105500                                                                          
105600     MOVE INF-PRESS-PF11     TO MED-IDMFSINF                              
105700     CALL WMEDKONV USING MED-WMEDAREA                                     
105800     MOVE MED-TEMFSINF       TO MOD-TEMFSINF                              
105900                                                                          
106000     PERFORM MFS-LAES-IN-IGEN                                             
106100     PERFORM MFS-ROER-EJ-FAELT-IN                                         
106200     .                                                                    
106300     EJECT                                                                
106400 H-UPPDATERA      SECTION.                                                
106500*                                                                         
106600     MOVE '2348'   TO MSGSOP-IDTRANS                                      
106700     MOVE '1'      TO MSGSOP-KDMFSFOR                                     
106800     MOVE 'O'      TO MSGSOP-KDSOPFUNK                                    
106900     MOVE 'W271B2' TO MSGSOP-IDPROCESS                                    
107000*                                                                         
107100* MAIL                                                                    
107200     STRING 'URVAL1(' WS-URVAL1 ')URVAL2('                                
107300            WS-URVAL2 ')URVAL3('                                          
107400            WS-URVAL3 ')MAIL(' WS-IDMAIL ')'                              
107500            DELIMITED BY SIZE INTO MSGSOP-TESYMBV                         
107600                                                                          
107700            MOVE MAILSEND    TO MOD-TEMFSINF                              
107800                                                                          
107900     PERFORM IMS-INSERT-ALTMSG                                            
108000                                                                          
108100                                                                          
108200     .                                                                    
108300     EJECT                                                                
108400 MFS-RENSA-FAELT-IN SECTION.                                              
108500                                                                          
108600*                                                                         
108700*    --- ALLA INDATA-FÄLT                                                 
108800*                                                                         
108900     MOVE MFS-RENSA-FAELT TO MOD-KDARBTYP                                 
109000                             MOD-IDPERSON                                 
109100                             MOD-IDDC                                     
109200                             MOD-FLKVROS                                  
109300                             MOD-IDPERSON-BUY-FOM                         
109400                             MOD-IDPERSON-BUY-TOM                         
109500                             MOD-IDPERSON-BUY2                            
109600                             MOD-IDPERSON-BUY3                            
109700                             MOD-IDPERSON-BUY4                            
109800                             MOD-FLONORDER                                
109900                             MOD-FLAK-DC                                  
110000                             MOD-FLASEAS                                  
110100                             MOD-IDLEVNR-CDC                              
110200                             MOD-KDPSLLOC                                 
110300                             MOD-IDLEVNR-DC                               
110400                             MOD-IDFKNGRP-FOM                             
110500                             MOD-IDFKNGRP-TOM                             
110600                             MOD-FLREFILL                                 
110700                             MOD-PRISRAD                                  
110800                             MOD-PBRAD                                    
110900                             MOD-FLREFBEO                                 
111000                             MOD-IDREFTAB                                 
111100                             MOD-VKART-TKN                                
111200                             MOD-VKART                                    
111300                             MOD-KVPB-FOM                                 
111400                             MOD-KVPB-TOM                                 
111500                             MOD-VLARTNTO-TKN                             
111600                             MOD-VLARTNTO                                 
111700                             MOD-ADLAGOMR                                 
111800                             MOD-ADGANG                                   
111900                             MOD-ADPLATS-FOM                              
112000                             MOD-ADPLATS-TOM                              
112100                             MOD-KDERS                                    
112200                             MOD-PRARTSTD-TKN                             
112300                             MOD-PRARTSTD                                 
112400                             MOD-KVLS-TKN                                 
112500                             MOD-KVLS                                     
112600                             MOD-TIFINLV-TKN                              
112700                             MOD-TIFINLV                                  
112800                             MOD-TIREFEFT-TKN                             
112900                             MOD-TIREFEFT                                 
113000                             MOD-BEART                                    
113100                             MOD-BEMODELL                                 
113200                             MOD-KDREFSTA                                 
113300                             MOD-SUPERWEEK-TKN                            
113400                             MOD-SUPERWEEK                                
113500                             MOD-FLFLYG                                   
113600                                                                          
113700     MOVE +1 TO IX                                                        
113800     PERFORM UNTIL IX > 3                                                 
113900       MOVE MFS-RENSA-FAELT  TO MOD-IDPROJ (IX)                           
114000       ADD +1 TO IX                                                       
114100     END-PERFORM                                                          
114200     .                                                                    
114300     EJECT                                                                
114400 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
114500                                                                          
114600*                                                                         
114700*    --- ALLA INDATA-FÄLT                                                 
114800*                                                                         
114900     MOVE MFS-ROER-EJ-FAELT                                               
115000                          TO MOD-KDARBTYP                                 
115100                             MOD-IDPERSON                                 
115200                             MOD-IDDC                                     
115300                             MOD-FLKVROS                                  
115400                             MOD-IDPERSON-BUY-FOM                         
115500                             MOD-IDPERSON-BUY-TOM                         
115600                             MOD-IDPERSON-BUY2                            
115700                             MOD-IDPERSON-BUY3                            
115800                             MOD-IDPERSON-BUY4                            
115900                             MOD-FLONORDER                                
116000                             MOD-FLAK-DC                                  
116100                             MOD-IDLEVNR-CDC                              
116200                             MOD-KDPSLLOC                                 
116300                             MOD-IDLEVNR-DC                               
116400                             MOD-IDFKNGRP-FOM                             
116500                             MOD-IDFKNGRP-TOM                             
116600                             MOD-FLREFILL                                 
116700                             MOD-PRISRAD                                  
116800                             MOD-PBRAD                                    
116900                             MOD-FLREFBEO                                 
117000                             MOD-IDREFTAB                                 
117100                             MOD-VKART-TKN                                
117200                             MOD-VKART                                    
117300                             MOD-KVPB-FOM                                 
117400                             MOD-KVPB-TOM                                 
117500                             MOD-VLARTNTO-TKN                             
117600                             MOD-VLARTNTO                                 
117700                             MOD-ADLAGOMR                                 
117800                             MOD-ADGANG                                   
117900                             MOD-ADPLATS-FOM                              
118000                             MOD-ADPLATS-TOM                              
118100                             MOD-KDERS                                    
118200                             MOD-PRARTSTD-TKN                             
118300                             MOD-PRARTSTD                                 
118400                             MOD-KVLS-TKN                                 
118500                             MOD-KVLS                                     
118600                             MOD-TIFINLV-TKN                              
118700                             MOD-TIFINLV                                  
118800                             MOD-TIREFEFT-TKN                             
118900                             MOD-TIREFEFT                                 
119000                             MOD-BEART                                    
119100                             MOD-BEMODELL                                 
119200                             MOD-KDREFSTA                                 
119300                             MOD-SUPERWEEK-TKN                            
119400                             MOD-SUPERWEEK                                
119500                             MOD-FLFLYG                                   
119600                                                                          
119700     MOVE +1 TO IX                                                        
119800     PERFORM UNTIL IX > 3                                                 
119900       MOVE MFS-ROER-EJ-FAELT                                             
120000                             TO MOD-IDPROJ (IX)                           
120100       ADD +1 TO IX                                                       
120200     END-PERFORM                                                          
120300     .                                                                    
120400     EJECT                                                                
120500 MFS-LAES-IN-IGEN SECTION.                                                
120600                                                                          
120700*    --- ALLA INDATA-FÄLT                                                 
120800     MOVE MFS-ADD-LAES-IN-FAELT                                           
120900                          TO MOD-KDARBTYP-ATTR                            
121000                             MOD-IDPERSON-ATTR                            
121100                             MOD-IDDC-ATTR                                
121200                             MOD-FLKVROS-ATTR                             
121300                             MOD-IDPERSON-BUY-FOM-ATTR                    
121400                             MOD-IDPERSON-BUY-TOM-ATTR                    
121500                             MOD-IDPERSON-BUY2-ATTR                       
121600                             MOD-IDPERSON-BUY3-ATTR                       
121700                             MOD-IDPERSON-BUY4-ATTR                       
121800                             MOD-FLONORDER-ATTR                           
121900                             MOD-FLAK-DC-ATTR                             
122000                             MOD-FLASEAS-ATTR                             
122100                             MOD-IDLEVNR-CDC-ATTR                         
122200                             MOD-KDPSLLOC-ATTR                            
122300                             MOD-IDLEVNR-DC-ATTR                          
122400                             MOD-IDFKNGRP-FOM-ATTR                        
122500                             MOD-IDFKNGRP-TOM-ATTR                        
122600                             MOD-FLREFILL-ATTR                            
122700                             MOD-PRISRAD-ATTR                             
122800                             MOD-PBRAD-ATTR                               
122900                             MOD-FLREFBEO-ATTR                            
123000                             MOD-IDREFTAB-ATTR                            
123100                             MOD-VKART-TKN-ATTR                           
123200                             MOD-VKART-ATTR                               
123300                             MOD-KVPB-FOM-ATTR                            
123400                             MOD-KVPB-TOM-ATTR                            
123500                             MOD-VLARTNTO-TKN-ATTR                        
123600                             MOD-VLARTNTO-ATTR                            
123700                             MOD-ADLAGOMR-ATTR                            
123800                             MOD-ADGANG-ATTR                              
123900                             MOD-ADPLATS-FOM-ATTR                         
124000                             MOD-ADPLATS-TOM-ATTR                         
124100                             MOD-KDERS-ATTR                               
124200                             MOD-PRARTSTD-TKN-ATTR                        
124300                             MOD-PRARTSTD-ATTR                            
124400                             MOD-KVLS-TKN-ATTR                            
124500                             MOD-KVLS-ATTR                                
124600                             MOD-TIFINLV-TKN-ATTR                         
124700                             MOD-TIFINLV-ATTR                             
124800                             MOD-TIREFEFT-TKN-ATTR                        
124900                             MOD-TIREFEFT-ATTR                            
125000                             MOD-BEART-ATTR                               
125100                             MOD-BEMODELL-ATTR                            
125200                             MOD-KDREFSTA-ATTR                            
125300                             MOD-SUPERWEEK-TKN-ATTR                       
125400                             MOD-SUPERWEEK-ATTR                           
125500                             MOD-FLFLYG-ATTR                              
125600                                                                          
125700     MOVE +1 TO IX                                                        
125800     PERFORM UNTIL IX > 3                                                 
125900       MOVE MFS-ROER-EJ-FAELT                                             
126000                             TO MOD-IDPROJ-ATTR (IX)                      
126100       ADD +1 TO IX                                                       
126200     END-PERFORM                                                          
126300     .                                                                    
126400     EJECT                                                                
126500* --- IMS SEKTIONER ---                                                   
126600     SKIP3                                                                
126700 IMS-GET-MSG SECTION.                                                     
126800                                                                          
126900     MOVE '  QC' TO GODK-STATUSKODER                                      
127000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
127100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
127200     PERFORM IMS-STATUSKONTROLL                                           
127300     .                                                                    
127400     SKIP3                                                                
127500 IMS-INSERT-MSG SECTION.                                                  
127600                                                                          
127700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
127800     MOVE SPACE TO GODK-STATUSKODER                                       
127900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
128000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
128100     PERFORM IMS-STATUSKONTROLL                                           
128200     .                                                                    
128300     SKIP3                                                                
128400 IMS-INSERT-ALTMSG SECTION.                                               
128500                                                                          
128600     MOVE SPACE TO GODK-STATUSKODER                                       
128700     CALL CBLTDLI USING ISRT ALT-PCB PROG-TO-PROG-SW                      
128800     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
128900     PERFORM IMS-STATUSKONTROLL                                           
129000     .                                                                    
129100     EJECT                                                                
129200 IMS-GU-WDP311 SECTION.                                                   
129300     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
129400            DELIMITED BY SIZE INTO SSA1                                   
129500     STRING 'WDP311  (IDPERSON ='  W-IDPERSON-X ')'                       
129600            DELIMITED BY SIZE INTO SSA2                                   
129700     MOVE '  GE' TO GODK-STATUSKODER                                      
129800     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-P3 SSA1 SSA2                   
129900     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
130000     PERFORM IMS-STATUSKONTROLL                                           
130100     .                                                                    
130200     SKIP3                                                                
130300 IMS-GU-WDB601    SECTION.                                                
130400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
130500          DELIMITED BY SIZE INTO SSA1                                     
130600     MOVE '  GE' TO GODK-STATUSKODER                                      
130700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
130800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
130900     PERFORM IMS-STATUSKONTROLL                                           
131000     IF SEGMENT-SAKNAS                                                    
131100         MOVE SPACE TO DCS-KDDC                                           
131200     END-IF                                                               
131300     .                                                                    
131400 IMS-STATUSKONTROLL SECTION.                                              
131500                                                                          
131600     SET STATUS-IX TO 1                                                   
131700     SEARCH GODK-STATUS                                                   
131800       AT END                                                             
131900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
132000         DELIMITED BY SIZE INTO FELTEXT                                   
132100         CALL FELLOG                                                      
132200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
132300     END-SEARCH                                                           
132400     .                                                                    
