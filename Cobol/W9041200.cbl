000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W9041200.                                                
000400*AUTHOR.         BODIL LINDAHL.                                           
000500*DATE-WRITTEN.   93/11/16.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900* UPDATES:    PGM. COPIED FROM W9041200 FOR USE BY SPIE2.                 
001000*             PGM. HAS BEEN REDUCED TO ONLY SHOW NESSECARY                
001100*             INFORMATION FOR SPIE2.                                      
001200*             CHANGE MADE 040405 BY JOHAN NIHLBLAD                        
001300*                                                                         
001400*    FUNKTION:                                                            
001500*        REGISTRERING OCH UPPDATERING AV ARTIKLAR KEMI                    
001600*        - ARTIKLAR MED KDFARLIG 4 ELLER 6.                               
001700*                                                                         
001800*        PROGRAMMET UPPDATERAR WLARTN (WDD5)                              
001900*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
002000*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
002100*        PROGRAMMET LÄSER      W6KVAH (W6D2)                              
002200*        PROGRAMMET LÄSER      W6KVAG (W6H7B)                             
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W9T412                                              
002600*                     W9T412U                                             
002700*        MID:         W90412I1                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W90412O1                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800*    -- CHECKED BY WY2000                                                 
003900 77  IDPGM                       PIC X(08)   VALUE 'W9041200'.            
004000 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
004100 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400 77  ARTIKEL-SW                  PIC X       VALUE SPACE.                 
004500 77  WS-TENOTE1-IFYLLT           PIC X       VALUE SPACE.                 
004600 77  WS-TENOTE2-IFYLLT           PIC X       VALUE SPACE.                 
004700 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004800 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
004900 77  WS-KVFLAMP                  PIC S9(3) COMP-3.                        
005000 77  WS-KVFLAMP-NEG              PIC S9(3) COMP-3.                        
005100 77  WS-KVNTOFG                  PIC S9(2)V9(3) COMP-3.                   
005200 77  WS-KVVOC                    PIC S9(2)V9(3) COMP-3.                   
005300 77  WS-REBLYPRO                 PIC S9(2)V9(3) COMP-3.                   
005400 77  WS-SUEQFG                   PIC S9(3)V9(4) COMP-3.                   
005500 77  WS-VKFORSFG                 PIC S9(4)V9(3) COMP-3.                   
005600 77  WS-VLFG                     PIC S9(4)V9(3) COMP-3.                   
005700 77  WS-VKART-FG                 PIC S9(7) COMP-3.                        
005800 77  WS-IDARTNR-RECEPT           PIC 9(9)    VALUE ZERO.                  
005900 77  WS-KDFGPRIO                 PIC 9(3)    VALUE ZERO.                  
006000 77  WS-IDANMNR                  PIC 9(6)    VALUE ZERO.                  
006100 77  WS-REKSIFFR                 PIC 9(1)    VALUE ZERO.                  
006200                                                                          
006300 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006400     88  INDATA-OK                           VALUE 'J'.                   
006500     88  INDATA-FEL                          VALUE 'N'.                   
006600                                                                          
006700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006800     88  NYCKLAR-OK                          VALUE 'J'.                   
006900     88  NYCKLAR-FEL                         VALUE 'N'.                   
007000                                                                          
007100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007200     88  EGEN-MID                            VALUE '9412'.                
007300     88  HELP-MID                            VALUE '0551'.                
007400                                                                          
007500 01  WS-KDARTHNT                 PIC 9(6).                                
007600 01  FILLER REDEFINES WS-KDARTHNT.                                        
007700     03  WS-KDARTHNT-V           PIC 9(3).                                
007800     03  WS-KDARTHNT-H           PIC 9(3).                                
007900     EJECT                                                                
008000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008100 01  GENERELLA-SUBPROGRAM.                                                
008200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008500     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
008600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008700     EJECT                                                                
008800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008900*01 -COPY WMEDAREA                                                        
009000     SKIP3                                                                
009100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
009200*01 -COPY WMSGINIT                                                        
009300     SKIP3                                                                
009400 01  MESSAGE-CODES.                                                       
009500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009600     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
009700     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
009800     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
009900     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
010000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010100     EJECT                                                                
010200*01  -COPY WDECAREA                                                       
010300     EJECT                                                                
010400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
010500*                                                                         
010600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010700     SKIP3                                                                
010800*01  MID -COPY W90412I1                                                   
010900     EJECT                                                                
011000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011100     SKIP3                                                                
011200*01  -COPY WMSGAREA                                                       
011300     EJECT                                                                
011400     03  MOD REDEFINES MSG-AREA.                                          
011500*      05  -COPY W90412O1                                                 
011600     EJECT                                                                
011700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011800     SKIP3                                                                
011900*01  -COPY WMFSAREA                                                       
012000     EJECT                                                                
012100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012200                                                                          
012300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012400                                                                          
012500 01  NYCKLAR-TILL-DLI.                                                    
012600     03  W-IDARTNR-X.                                                     
012700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
012800     03  W-IDSKYLT-X.                                                     
012900         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
013000     03  W-W6H7B1KY-MAX-X.                                                
013100         05  W-IDARTNR-MAX       PIC S9(9)   VALUE ZERO COMP-3.           
013200         05  W-DAREGDAT-9KOMPL-MAX                                        
013300                                 PIC 9(8)    VALUE ZERO.                  
013400         05  W-IDLEVNR-MAX       PIC X(5)    VALUE SPACE.                 
013500         05  W-KVKRKNTR-MAX      PIC S9(1)   VALUE ZERO COMP-3.           
013600         05  W-IDKR-MAX          PIC 9(5)    VALUE ZERO.                  
013700     03  W-W6H7B1KY-MIN-X.                                                
013800         05  W-IDARTNR-MIN       PIC S9(9)   VALUE ZERO COMP-3.           
013900         05  W-DAREGDAT-9KOMPL-MIN                                        
014000                                 PIC 9(8)    VALUE ZERO.                  
014100         05  W-IDLEVNR-MIN       PIC X(5)    VALUE SPACE.                 
014200         05  W-KVKRKNTR-MIN      PIC S9(1)   VALUE ZERO COMP-3.           
014300         05  W-IDKR-MIN          PIC 9(5)    VALUE ZERO.                  
014400     SKIP2                                                                
014500*    --- STATUS-KOD FRÅN IMS                                              
014600 01  STATUS-WS                   PIC XX.                                  
014700     88  SEGMENT-FINNS                       VALUE '  '.                  
014800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014900     SKIP2                                                                
015000 01  GODK-STATUSKODER.                                                    
015100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015200                                                                          
015300 01  SSA1                        PIC X(96).                               
015400 01  SSA2                        PIC X(64).                               
015500     EJECT                                                                
015600*    --- IMS FUNKTIONSKODER                                               
015700*01  -COPY W0003                                                          
015800     EJECT                                                                
015900*    ---  DLI INPUT-OUTPUT AREA                                           
016000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
016100     SKIP3                                                                
016200 01  DLI-IO-AREA.                                                         
016300     03  IO-AREA                 PIC X(1200)  VALUE SPACE.                
016400     SKIP3                                                                
016500     03  WLARTC01 REDEFINES IO-AREA.                                      
016600*        05  -COPY WDK601                                                 
016700     EJECT                                                                
016800     03  WLARTC11 REDEFINES IO-AREA.                                      
016900*        05  -COPY WDK611                                                 
017000     EJECT                                                                
017100     03  WLBENA11 REDEFINES IO-AREA.                                      
017200*        05  -COPY WDD311  -PRE BENA11-                                   
017300     EJECT                                                                
017400 01  DLI-IO-AREA2.                                                        
017500     03  IO-AREA2                PIC X(200)  VALUE SPACE.                 
017600     SKIP3                                                                
017700     03  WLARTN01 REDEFINES IO-AREA2.                                     
017800*        05  -COPY WDD501  -PRE ARTN01-                                   
017900     EJECT                                                                
018000 LINKAGE SECTION.                                                         
018100                                                                          
018200*01  -COPY W0009  -PRE MSG-                                               
018300     EJECT                                                                
018400*01  -COPY W0008  -PRE USEA-                                              
018500     05  FILLER                  PIC X.                                   
018600     EJECT                                                                
018700*01  -COPY W0008  -PRE ARTN-                                              
018800     05  FILLER                  PIC X.                                   
018900     EJECT                                                                
019000*01  -COPY W0008  -PRE ARTC-                                              
019100     05  FILLER                  PIC X.                                   
019200     EJECT                                                                
019300*01  -COPY W0008  -PRE BENA-                                              
019400     05  FILLER                  PIC X.                                   
019500     EJECT                                                                
019600*01  -COPY W0008  -PRE KVAH-                                              
019700     05  FILLER                  PIC X.                                   
019800     EJECT                                                                
019900*01  -COPY W0008  -PRE KVAG-                                              
020000     05  FILLER                  PIC X.                                   
020100     EJECT                                                                
020200 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
020300                                   ARTN-PCB ARTC-PCB                      
020400     BENA-PCB KVAH-PCB KVAG-PCB.                                          
020500     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
020600                                   ARTN-PCB ARTC-PCB                      
020700     BENA-PCB KVAH-PCB KVAG-PCB.                                          
020800                                                                          
020900     PERFORM IMS-GET-MSG                                                  
021000     IF SEGMENT-FINNS                                                     
021100       PERFORM A-INIT                                                     
021200       PERFORM B-KOLLA-NYCKLAR                                            
021300       IF NYCKLAR-OK                                                      
021400         IF MFS-UPDATE                                                    
021500           PERFORM G-KOLLA-INPUT                                          
021600           IF INDATA-OK                                                   
021700             PERFORM H-UPPDATERA                                          
021800           END-IF                                                         
021900         ELSE                                                             
022000           IF MFS-FIRST                                                   
022100              PERFORM C-FOERSTA-SIDA                                      
022200           ELSE                                                           
022300              PERFORM E-SAMMA-SIDA                                        
022400           END-IF                                                         
022500         END-IF                                                           
022600         PERFORM F-LAES-VISA-INFO                                         
022700       END-IF                                                             
022800       COMPUTE MSG-KVLL = LENGTH OF MOD-W90412O1-CTX + 4                  
022900       PERFORM IMS-INSERT-MSG                                             
023000     END-IF                                                               
023100                                                                          
023200     MOVE ZERO TO RETURN-CODE                                             
023300     GOBACK                                                               
023400     .                                                                    
023500     EJECT                                                                
023600 A-INIT SECTION.                                                          
023700                                                                          
023800     IF MSG-DUBBLA-TRANSKODER                                             
023900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W90412I1-CTX             
024000       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
024100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
024200     ELSE                                                                 
024300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W90412I1-CTX              
024400       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
024500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
024600     END-IF                                                               
024700                                                                          
024800     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
024900     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
025000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
025100                                                                          
025200     MOVE LOW-VALUE TO MSG-AREA                                           
025300     MOVE 'W90412O1' TO MFS-IDMOD                                         
025400     MOVE '9412' TO MOD-IDTRANS                                           
025500     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
025600                                                                          
025700     IF EGEN-MID OR HELP-MID                                              
025800       CONTINUE                                                           
025900     ELSE                                                                 
026000       MOVE SPACE TO MFS-KDTRTYP                                          
026100       MOVE '7' TO MFS-IDPFK                                              
026200     END-IF                                                               
026300                                                                          
026400     ACCEPT DAGENS-DATUM FROM DATE                                        
026500     .                                                                    
026600     EJECT                                                                
026700 B-KOLLA-NYCKLAR SECTION.                                                 
026800                                                                          
026900     MOVE JA TO NYCKLAR-SW                                                
027000     MOVE LOW-VALUE  TO  W-W6H7B1KY-MIN-X                                 
027100     MOVE HIGH-VALUE TO  W-W6H7B1KY-MAX-X                                 
027200*    MOVE MFS-RENSA-FAELT TO                                              
027300*                            MOD-IDARTNR-IN                               
027400*                            MOD-IDPSN-DOLT                               
027500                                                                          
027600     MOVE ALL '+' TO MSGI-WMSGINIT                                        
027700     MOVE '001'             TO MSGI-KDCALL                                
027800     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
027900                               MSGI-IDLTERM-USER                          
028000     MOVE '9412'            TO MSGI-IDTRANS                               
028100     IF MFS-IDTRANS = '9412'                                              
028200     OR (MID-IDARTNR-IN NUMERIC                                           
028300     AND MID-IDARTNR-IN > ZERO)                                           
028400         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
028500     END-IF                                                               
028600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
028700     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
028800     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
028900                                                                          
029000     IF MID-IDARTNR-IN = ALL '+'                                          
029100       CONTINUE                                                           
029200     ELSE                                                                 
029300       MOVE '7'         TO MFS-IDPFK                                      
029400       MOVE SPACE       TO MFS-KDTRTYP                                    
029500     END-IF                                                               
029600                                                                          
029700     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
029800       MOVE +1 TO SPRAK-IX                                                
029900       MOVE 'S  ' TO MED-IDSKYLT                                          
030000     ELSE                                                                 
030100       MOVE +2 TO SPRAK-IX                                                
030200       MOVE 'GB ' TO MED-IDSKYLT                                          
030300     END-IF                                                               
030400                                                                          
030500     IF WS-IDARTNR NUMERIC AND WS-IDARTNR > ZERO                          
030600       MOVE WS-IDARTNR TO W-IDARTNR                                       
030700                          W-IDARTNR-MAX                                   
030800                          W-IDARTNR-MIN                                   
030900     ELSE                                                                 
031000       MOVE NEJ TO NYCKLAR-SW                                             
031100     END-IF                                                               
031200                                                                          
031300*    IF NYCKLAR-OK                                                        
031400*      MOVE WS-IDARTNR TO MOD-IDARTNR-UT                                  
031500*      INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
031600*    ELSE                                                                 
031700*      MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
031800*    END-IF                                                               
031900                                                                          
032000     IF NYCKLAR-FEL                                                       
032100       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
032200       CALL WMEDKONV USING MED-WMEDAREA                                   
032300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
032400       PERFORM MFS-RENSA-FAELT-IN                                         
032500       PERFORM MFS-RENSA-FAELT-UT                                         
032600       MOVE MFS-RENSA-FAELT TO MOD-TENOTE(1)                              
032700                               MOD-TENOTE(2)                              
032800     END-IF                                                               
032900     .                                                                    
033000     EJECT                                                                
033100 C-FOERSTA-SIDA SECTION.                                                  
033200                                                                          
033300     PERFORM MFS-RENSA-FAELT-IN                                           
033400     MOVE MFS-RENSA-FAELT TO MOD-TENOTE(1)                                
033500                             MOD-TENOTE(2)                                
033600     .                                                                    
033700     EJECT                                                                
033800 E-SAMMA-SIDA SECTION.                                                    
033900                                                                          
034000     IF EGEN-MID OR HELP-MID                                              
034100       IF MID-INPUT = ALL '+' AND MID-FLBORT = ALL '+'                    
034200         PERFORM MFS-RENSA-FAELT-IN                                       
034300         MOVE MFS-RENSA-FAELT TO MOD-TENOTE(1)                            
034400                                 MOD-TENOTE(2)                            
034500       ELSE                                                               
034600         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
034700         CALL WMEDKONV USING MED-WMEDAREA                                 
034800         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
034900         PERFORM EA-MID-INDATA-TILL-MOD                                   
035000       END-IF                                                             
035100     ELSE                                                                 
035200       PERFORM MFS-RENSA-FAELT-IN                                         
035300       MOVE MFS-RENSA-FAELT TO MOD-TENOTE(1)                              
035400                               MOD-TENOTE(2)                              
035500     END-IF                                                               
035600     .                                                                    
035700     EJECT                                                                
035800 EA-MID-INDATA-TILL-MOD SECTION.                                          
035900                                                                          
036000     MOVE NEJ TO WS-TENOTE1-IFYLLT                                        
036100                 WS-TENOTE2-IFYLLT                                        
036200                                                                          
036300     IF MID-FLBORT = ALL '+'                                              
036400        MOVE MFS-RENSA-FAELT TO MOD-FLBORT                                
036500     ELSE                                                                 
036600        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLBORT-ATTR                     
036700        MOVE MFS-ROER-EJ-FAELT TO MOD-FLBORT                              
036800     END-IF                                                               
036900                                                                          
037000     IF MID-KDFARG = ALL '+'                                              
037100*       MOVE MFS-RENSA-FAELT TO MOD-KDFARG-IN                             
037200        CONTINUE                                                          
037300     ELSE                                                                 
037400        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDFARG-ATTR                     
037500*       MOVE MFS-ROER-EJ-FAELT TO MOD-KDFARG-IN                           
037600     END-IF                                                               
037700                                                                          
037800     IF MID-KDLACK = ALL '+'                                              
037900*       MOVE MFS-RENSA-FAELT TO MOD-KDLACK-IN                             
038000        CONTINUE                                                          
038100     ELSE                                                                 
038200        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDLACK-ATTR                     
038300*       MOVE MFS-ROER-EJ-FAELT TO MOD-KDLACK-IN                           
038400     END-IF                                                               
038500                                                                          
038600     IF MID-IDARTNR-RECEPT = ALL '+'                                      
038700*       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-RECEPT-IN                     
038800        CONTINUE                                                          
038900     ELSE                                                                 
039000        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDARTNR-RECEPT-ATTR             
039100*       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-RECEPT-IN                   
039200     END-IF                                                               
039300                                                                          
039400     IF MID-VKFORSFG = ALL '+'                                            
039500*       MOVE MFS-RENSA-FAELT TO MOD-VKFORSFG-IN                           
039600        CONTINUE                                                          
039700     ELSE                                                                 
039800        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-VKFORSFG-ATTR                   
039900*       MOVE MFS-ROER-EJ-FAELT TO MOD-VKFORSFG-IN                         
040000     END-IF                                                               
040100                                                                          
040200     IF MID-FLVARINF = ALL '+'                                            
040300*       MOVE MFS-RENSA-FAELT TO MOD-FLVARINF-IN                           
040400        CONTINUE                                                          
040500     ELSE                                                                 
040600        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLVARINF-ATTR                   
040700*       MOVE MFS-ROER-EJ-FAELT TO MOD-FLVARINF-IN                         
040800     END-IF                                                               
040900                                                                          
041000     IF MID-FLVARINF-SDS = ALL '+'                                        
041100*       MOVE MFS-RENSA-FAELT TO MOD-FLVARINF-SDS-IN                       
041200        CONTINUE                                                          
041300     ELSE                                                                 
041400        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLVARINF-SDS-ATTR               
041500*       MOVE MFS-ROER-EJ-FAELT TO MOD-FLVARINF-SDS-IN                     
041600     END-IF                                                               
041700                                                                          
041800     IF MID-IDVARINF = ALL '+'                                            
041900*       MOVE MFS-RENSA-FAELT TO MOD-IDVARINF-IN                           
042000        CONTINUE                                                          
042100     ELSE                                                                 
042200        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDVARINF-ATTR                   
042300*       MOVE MFS-ROER-EJ-FAELT TO MOD-IDVARINF-IN                         
042400     END-IF                                                               
042500                                                                          
042600     IF MID-IDVARINF-SDS = ALL '+'                                        
042700*       MOVE MFS-RENSA-FAELT TO MOD-IDVARINF-SDS-IN                       
042800        CONTINUE                                                          
042900     ELSE                                                                 
043000        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDVARINF-SDS-ATTR               
043100*       MOVE MFS-ROER-EJ-FAELT TO MOD-IDVARINF-SDS-IN                     
043200     END-IF                                                               
043300                                                                          
043400     IF MID-KVNTOFG = ALL '+'                                             
043500*       MOVE MFS-RENSA-FAELT TO MOD-KVNTOFG-IN                            
043600        CONTINUE                                                          
043700     ELSE                                                                 
043800        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVNTOFG-ATTR                    
043900*       MOVE MFS-ROER-EJ-FAELT TO MOD-KVNTOFG-IN                          
044000     END-IF                                                               
044100                                                                          
044200     IF MID-KDSORT-KVNTOFG = ALL '+'                                      
044300        MOVE MFS-RENSA-FAELT TO MOD-KDSORT-KVNTOFG-IN                     
044400     ELSE                                                                 
044500        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDSORT-KVNTOFG-ATTR             
044600*       MOVE MFS-ROER-EJ-FAELT TO MOD-KDSORT-KVNTOFG-IN                   
044700     END-IF                                                               
044800                                                                          
044900     IF MID-KVVOC = ALL '+'                                               
045000*       MOVE MFS-RENSA-FAELT TO MOD-KVVOC-IN                              
045100        CONTINUE                                                          
045200     ELSE                                                                 
045300        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVVOC-ATTR                      
045400*       MOVE MFS-ROER-EJ-FAELT TO MOD-KVVOC-IN                            
045500     END-IF                                                               
045600                                                                          
045700     IF MID-SUEQFG = ALL '+'                                              
045800*       MOVE MFS-RENSA-FAELT TO MOD-SUEQFG-IN                             
045900        CONTINUE                                                          
046000     ELSE                                                                 
046100        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-SUEQFG-ATTR                     
046200*       MOVE MFS-ROER-EJ-FAELT TO MOD-SUEQFG-IN                           
046300     END-IF                                                               
046400                                                                          
046500     IF MID-VLFG = ALL '+'                                                
046600*       MOVE MFS-RENSA-FAELT TO MOD-VLFG-IN                               
046700        CONTINUE                                                          
046800     ELSE                                                                 
046900        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-VLFG-ATTR                       
047000*       MOVE MFS-ROER-EJ-FAELT TO MOD-VLFG-IN                             
047100     END-IF                                                               
047200                                                                          
047300     IF MID-KDSORT-VLFG = ALL '+'                                         
047400*       MOVE MFS-RENSA-FAELT TO MOD-KDSORT-VLFG-IN                        
047500        CONTINUE                                                          
047600     ELSE                                                                 
047700        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDSORT-VLFG-ATTR                
047800*       MOVE MFS-ROER-EJ-FAELT TO MOD-KDSORT-VLFG-IN                      
047900     END-IF                                                               
048000                                                                          
048100     IF MID-FLNEG = ALL '+'                                               
048200*       MOVE MFS-RENSA-FAELT TO MOD-FLNEG-IN                              
048300        CONTINUE                                                          
048400     ELSE                                                                 
048500        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLNEG-ATTR                      
048600*       MOVE MFS-ROER-EJ-FAELT TO MOD-FLNEG-IN                            
048700     END-IF                                                               
048800                                                                          
048900     IF MID-KVFLAMP = ALL '+'                                             
049000*       MOVE MFS-RENSA-FAELT TO MOD-KVFLAMP-IN                            
049100        CONTINUE                                                          
049200     ELSE                                                                 
049300        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVFLAMP-ATTR                    
049400*       MOVE MFS-ROER-EJ-FAELT TO MOD-KVFLAMP-IN                          
049500     END-IF                                                               
049600                                                                          
049700     IF MID-FLFROST = ALL '+'                                             
049800*       MOVE MFS-RENSA-FAELT TO MOD-FLFROST-IN                            
049900        CONTINUE                                                          
050000     ELSE                                                                 
050100        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLFROST-ATTR                    
050200*       MOVE MFS-ROER-EJ-FAELT TO MOD-FLFROST-IN                          
050300     END-IF                                                               
050400                                                                          
050500     IF MID-FLTACTIL = ALL '+'                                            
050600*       MOVE MFS-RENSA-FAELT TO MOD-FLTACTIL-IN                           
050700        CONTINUE                                                          
050800     ELSE                                                                 
050900        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLTACTIL-ATTR                   
051000*       MOVE MFS-ROER-EJ-FAELT TO MOD-FLTACTIL-IN                         
051100     END-IF                                                               
051200                                                                          
051300     IF MID-IDPSN-IN = ALL '+'                                            
051400*         MOVE MFS-RENSA-FAELT TO MOD-IDPSN-IN                            
051500          CONTINUE                                                        
051600     ELSE                                                                 
051700        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPSN-IN-ATTR                   
051800*       MOVE MFS-ROER-EJ-FAELT TO MOD-IDPSN-IN                            
051900     END-IF                                                               
052000                                                                          
052100     IF MID-IDAO-FG = ALL '+'                                             
052200*       MOVE MFS-RENSA-FAELT TO MOD-IDAO-FG-IN                            
052300        CONTINUE                                                          
052400     ELSE                                                                 
052500        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDAO-FG-ATTR                    
052600*       MOVE MFS-ROER-EJ-FAELT TO MOD-IDAO-FG-IN                          
052700     END-IF                                                               
052800                                                                          
052900     IF MID-KDFGPRIO = ALL '+'                                            
053000*       MOVE MFS-RENSA-FAELT TO MOD-KDFGPRIO-IN                           
053100        CONTINUE                                                          
053200     ELSE                                                                 
053300        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDFGPRIO-ATTR                   
053400*       MOVE MFS-ROER-EJ-FAELT TO MOD-KDFGPRIO-IN                         
053500     END-IF                                                               
053600                                                                          
053700     IF MID-BEEMBMAT = ALL '+'                                            
053800*       MOVE MFS-RENSA-FAELT TO MOD-BEEMBMAT-IN                           
053900        CONTINUE                                                          
054000     ELSE                                                                 
054100        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEEMBMAT-ATTR                   
054200*       MOVE MFS-ROER-EJ-FAELT TO MOD-BEEMBMAT-IN                         
054300     END-IF                                                               
054400                                                                          
054500     IF MID-IDANMNR = ALL '+'                                             
054600*       MOVE MFS-RENSA-FAELT TO MOD-IDANMNR-IN                            
054700        CONTINUE                                                          
054800     ELSE                                                                 
054900        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDANMNR-ATTR                    
055000*       MOVE MFS-ROER-EJ-FAELT TO MOD-IDANMNR-IN                          
055100     END-IF                                                               
055200                                                                          
055300     IF MID-REKSIFFR-ANMNR = ALL '+'                                      
055400*       MOVE MFS-RENSA-FAELT TO MOD-REKSIFFR-IN                           
055500        CONTINUE                                                          
055600     ELSE                                                                 
055700        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-REKSIFFR-ATTR                   
055800*       MOVE MFS-ROER-EJ-FAELT TO MOD-REKSIFFR-IN                         
055900     END-IF                                                               
056000                                                                          
056100     IF MID-VKART-FG = ALL '+'                                            
056200        CONTINUE                                                          
056300     ELSE                                                                 
056400        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-VKART-FG-ATTR                   
056500*       MOVE MFS-ROER-EJ-FAELT TO MOD-VKART-FG-IN                         
056600     END-IF                                                               
056700                                                                          
056800     IF MID-TENOTE(1)= ALL '+'                                            
056900        CONTINUE                                                          
057000     ELSE                                                                 
057100        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TENOTE-ATTR(1)                  
057200        MOVE MFS-ROER-EJ-FAELT TO MOD-TENOTE(1)                           
057300        MOVE JA TO WS-TENOTE1-IFYLLT                                      
057400     END-IF                                                               
057500                                                                          
057600     IF MID-TENOTE(2)= ALL '+'                                            
057700        CONTINUE                                                          
057800     ELSE                                                                 
057900        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TENOTE-ATTR(2)                  
058000        MOVE MFS-ROER-EJ-FAELT TO MOD-TENOTE(2)                           
058100        MOVE JA TO WS-TENOTE2-IFYLLT                                      
058200     END-IF                                                               
058300     .                                                                    
058400     EJECT                                                                
058500 F-LAES-VISA-INFO SECTION.                                                
058600                                                                          
058700     PERFORM FA-LAES-GRUNDDATA                                            
058800                                                                          
058900     IF SEGMENT-SAKNAS                                                    
059000        MOVE ARTIKEL-SAKNAS TO MED-IDMFSFEL                               
059100        CALL WMEDKONV USING MED-WMEDAREA                                  
059200        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
059300     END-IF                                                               
059400                                                                          
059500     PERFORM FB-LAES-ARTIKELREG                                           
059600     .                                                                    
059700     EJECT                                                                
059800 FA-LAES-GRUNDDATA SECTION.                                               
059900                                                                          
060000     PERFORM IMS-GET-ARTN01                                               
060100     IF SEGMENT-FINNS                                                     
060200        MOVE ARTN01-ART-BEEMBMAT        TO MOD-BEEMBMAT-UT                
060300        MOVE ARTN01-ART-FLFROST         TO MOD-FLFROST-UT                 
060400        MOVE ARTN01-ART-FLTACTIL        TO MOD-FLTACTIL-UT                
060500        MOVE ARTN01-ART-FLVARINF        TO MOD-FLVARINF-UT                
060600        MOVE ARTN01-ART-FLVARINF-SDS    TO MOD-FLVARINF-SDS-UT            
060700        MOVE ARTN01-ART-IDAO-FG         TO MOD-IDAO-FG-UT                 
060800        MOVE ARTN01-ART-IDVARINF        TO MOD-IDVARINF-UT                
060900        MOVE ARTN01-ART-IDVARINF-SDS    TO MOD-IDVARINF-SDS-UT            
061000        MOVE ARTN01-ART-KDFARG          TO MOD-KDFARG-UT                  
061100        MOVE ARTN01-ART-KDFGPRIO        TO MOD-KDFGPRIO-UT                
061200        MOVE ARTN01-ART-KDLACK          TO MOD-KDLACK-UT                  
061300        MOVE ARTN01-ART-KDSORT-KVNTOFG  TO MOD-KDSORT-KVNTOFG-UT          
061400        MOVE ARTN01-ART-KDSORT-VLFG     TO MOD-KDSORT-VLFG-UT             
061500        MOVE ARTN01-ART-KVFLAMP         TO MOD-KVFLAMP-UT                 
061600        MOVE ARTN01-ART-KVNTOFG         TO MOD-KVNTOFG-UT                 
061700        MOVE ARTN01-ART-KVVOC           TO MOD-KVVOC-UT                   
061800        MOVE ARTN01-ART-IDARTNR-RECEPT  TO MOD-IDARTNR-RECEPT-UT          
061900        MOVE ARTN01-ART-SUEQFG          TO MOD-SUEQFG-UT                  
062000        MOVE ARTN01-ART-VKFORSFG        TO MOD-VKFORSFG-UT                
062100        MOVE ARTN01-ART-VLFG            TO MOD-VLFG-UT                    
062200        MOVE ARTN01-ART-VKART-FG        TO MOD-VKART-FG-UT                
062300        MOVE ARTN01-ART-IDANMNR         TO MOD-IDANMNR-UT                 
062400        IF ARTN01-ART-IDANMNR = ZERO                                      
062500           MOVE MFS-RENSA-FAELT          TO MOD-REKSIFFR-UT               
062600        ELSE                                                              
062700           MOVE ARTN01-ART-REKSIFFR-ANMNR TO MOD-REKSIFFR-UT              
062800        END-IF                                                            
062900        IF WS-TENOTE1-IFYLLT = JA                                         
063000           CONTINUE                                                       
063100        ELSE                                                              
063200           MOVE ARTN01-ART-TENOTE(1)    TO MOD-TENOTE(1)                  
063300        END-IF                                                            
063400        IF WS-TENOTE2-IFYLLT = JA                                         
063500           CONTINUE                                                       
063600        ELSE                                                              
063700           MOVE ARTN01-ART-TENOTE(2)    TO MOD-TENOTE(2)                  
063800        END-IF                                                            
063900     ELSE                                                                 
064000        MOVE MFS-RENSA-FAELT            TO MOD-BEEMBMAT-UT                
064100                                           MOD-FLFROST-UT                 
064200                                           MOD-FLTACTIL-UT                
064300                                           MOD-FLVARINF-UT                
064400                                           MOD-FLVARINF-SDS-UT            
064500                                           MOD-IDAO-FG-UT                 
064600                                           MOD-IDVARINF-UT                
064700                                           MOD-IDVARINF-SDS-UT            
064800                                           MOD-KDFARG-UT                  
064900                                           MOD-KDLACK-UT                  
065000                                           MOD-KDFGPRIO-UT                
065100                                           MOD-KDSORT-KVNTOFG-UT          
065200                                           MOD-KDSORT-VLFG-UT             
065300                                           MOD-KVFLAMP-UT                 
065400                                           MOD-KVNTOFG-UT                 
065500                                           MOD-KVVOC-UT                   
065600                                           MOD-IDARTNR-RECEPT-UT          
065700                                           MOD-SUEQFG-UT                  
065800                                           MOD-VKFORSFG-UT                
065900                                           MOD-VLFG-UT                    
066000        IF WS-TENOTE1-IFYLLT = JA                                         
066100           CONTINUE                                                       
066200        ELSE                                                              
066300           MOVE MFS-RENSA-FAELT         TO MOD-TENOTE(1)                  
066400        END-IF                                                            
066500        IF WS-TENOTE2-IFYLLT = JA                                         
066600           CONTINUE                                                       
066700        ELSE                                                              
066800           MOVE MFS-RENSA-FAELT         TO MOD-TENOTE(2)                  
066900        END-IF                                                            
067000                                                                          
067100     END-IF                                                               
067200     .                                                                    
067300     EJECT                                                                
067400 FB-LAES-ARTIKELREG SECTION.                                              
067500                                                                          
067600                                                                          
067700     PERFORM IMS-GET-ARTC01                                               
067800     IF SEGMENT-FINNS                                                     
067900*       MOVE ART-FLIART                 TO MOD-FLIART                     
068000        PERFORM IMS-GET-ARTC11                                            
068100        IF SEGMENT-FINNS                                                  
068200           MOVE CLAG-IDPSN              TO MOD-IDPSN-UT                   
068300*                                          MOD-IDPSN-DOLT                 
068400*          MOVE CLAG-IDRITN             TO MOD-IDRITN                     
068500*          MOVE CLAG-KDFARLIG           TO MOD-KDFARLIG                   
068600*          MOVE CLAG-KDERS              TO MOD-KDERS                      
068700           MOVE CLAG-KDARTHNT           TO WS-KDARTHNT                    
068800           MOVE WS-KDARTHNT-H           TO MOD-KDARTHNT-H                 
068900        ELSE                                                              
069000           MOVE MFS-RENSA-FAELT                                           
069100                                        TO MOD-IDPSN-UT                   
069200*                                          MOD-IDRITN                     
069300*                                          MOD-KDFARLIG                   
069400*                                          MOD-KDERS                      
069500                                           MOD-KDARTHNT-H                 
069600           CONTINUE                                                       
069700        END-IF                                                            
069800     ELSE                                                                 
069900        MOVE MFS-RENSA-FAELT                                              
070000                                        TO MOD-IDPSN-UT                   
070100*                                          MOD-FLIART                     
070200*                                          MOD-KDERS                      
070300*                                          MOD-IDRITN                     
070400                                           MOD-KDARTHNT-H                 
070500*                                          MOD-KDFARLIG                   
070600        CONTINUE                                                          
070700     END-IF                                                               
070800                                                                          
070900     PERFORM IMS-GET-BENA01                                               
071000     IF SEGMENT-FINNS                                                     
071100        MOVE 'GB ' TO W-IDSKYLT                                           
071200        PERFORM IMS-GET-BENA11                                            
071300*       IF SEGMENT-FINNS                                                  
071400*          MOVE BENA11-TEXT-BEART       TO MOD-BEART-GB                   
071500*       ELSE                                                              
071600*          MOVE MFS-RENSA-FAELT         TO MOD-BEART-GB                   
071700*       END-IF                                                            
071800        MOVE 'S  ' TO W-IDSKYLT                                           
071900        PERFORM IMS-GET-BENA11                                            
072000*       IF SEGMENT-FINNS                                                  
072100*          MOVE BENA11-TEXT-BEART       TO MOD-BEART-S                    
072200*       ELSE                                                              
072300*          MOVE MFS-RENSA-FAELT         TO MOD-BEART-S                    
072400*       END-IF                                                            
072500*    ELSE                                                                 
072600*       MOVE MFS-RENSA-FAELT            TO MOD-BEART-S                    
072700*                                          MOD-BEART-GB                   
072800     END-IF                                                               
072900                                                                          
073000     PERFORM IMS-GET-W6KVAH01                                             
073100     IF SEGMENT-FINNS                                                     
073200        PERFORM IMS-GET-W6KVAH11                                          
073300*       IF SEGMENT-FINNS                                                  
073400*          MOVE JA                      TO MOD-FL-KH                      
073500*       ELSE                                                              
073600*          MOVE MFS-RENSA-FAELT         TO MOD-FL-KH                      
073700*       END-IF                                                            
073800     ELSE                                                                 
073900*       MOVE MFS-RENSA-FAELT            TO MOD-FL-KH                      
074000        CONTINUE                                                          
074100     END-IF                                                               
074200                                                                          
074300     PERFORM IMS-GET-W6KVAG01                                             
074400*    IF SEGMENT-FINNS                                                     
074500*       MOVE JA                         TO MOD-FL-KR                      
074600*    ELSE                                                                 
074700*       MOVE NEJ                        TO MOD-FL-KR                      
074800*    END-IF                                                               
074900     .                                                                    
075000     EJECT                                                                
075100 G-KOLLA-INPUT SECTION.                                                   
075200******************************************************************        
075300* KONTROLL    - NYUPPLÄGG/BORTTAG/UPPDATERING AV ARTIKEL ARTN01  *        
075400*             - UPPDATERING AV IDPSN ARTC11                      *        
075500******************************************************************        
075600                                                                          
075700     MOVE JA TO INDATA-SW                                                 
075800                                                                          
075900     IF MID-INPUT = ALL '+' AND MID-FLBORT = ALL '+'                      
076000        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSINF                         
076100        CALL WMEDKONV USING MED-WMEDAREA                                  
076200        MOVE MED-MFSINF TO MOD-TEMFSINF                                   
076300*       PERFORM MFS-ROER-EJ-FAELT-IN                                      
076400        PERFORM MFS-ROER-EJ-FAELT-UT                                      
076500        PERFORM MFS-ROER-EJ-FAELT-IN-UT                                   
076600        MOVE NEJ TO INDATA-SW                                             
076700     ELSE                                                                 
076800                                                                          
076900        PERFORM IMS-GET-ARTN01                                            
077000        IF SEGMENT-FINNS                                                  
077100           MOVE JA TO ARTIKEL-SW                                          
077200           PERFORM GB-KOLLA-INPUT-REPLACE                                 
077300        ELSE                                                              
077400           MOVE NEJ TO ARTIKEL-SW                                         
077500           PERFORM GA-KOLLA-INPUT-NYUPPLAGG                               
077600        END-IF                                                            
077700                                                                          
077800        IF MID-FLBORT = ALL '+'                                           
077900           MOVE MFS-RENSA-FAELT TO MOD-FLBORT                             
078000        ELSE                                                              
078100           IF MID-INPUT = ALL '+'                                         
078200              IF MID-FLBORT = JA                                          
078300                 IF ARTIKEL-SW = JA                                       
078400                    MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLBORT-ATTR          
078500                 ELSE                                                     
078600                    MOVE MFS-ALFA-FAELT-FEL TO MOD-FLBORT-ATTR            
078700                    MOVE NEJ TO INDATA-SW                                 
078800                 END-IF                                                   
078900              ELSE                                                        
079000                 MOVE MFS-ALFA-FAELT-FEL TO MOD-FLBORT-ATTR               
079100                 MOVE NEJ TO INDATA-SW                                    
079200              END-IF                                                      
079300           ELSE                                                           
079400              MOVE MFS-ALFA-FAELT-FEL TO MOD-FLBORT-ATTR                  
079500              MOVE NEJ TO INDATA-SW                                       
079600           END-IF                                                         
079700        END-IF                                                            
079800                                                                          
079900        IF INDATA-FEL                                                     
080000           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSINF                      
080100           CALL WMEDKONV USING MED-WMEDAREA                               
080200           MOVE MED-MFSINF TO MOD-TEMFSINF                                
080300           PERFORM MFS-ROER-EJ-FAELT-UT                                   
080400*          PERFORM MFS-ROER-EJ-FAELT-IN                                   
080500           PERFORM MFS-ROER-EJ-FAELT-IN-UT                                
080600        END-IF                                                            
080700     END-IF                                                               
080800     .                                                                    
080900     EJECT                                                                
081000 GA-KOLLA-INPUT-NYUPPLAGG SECTION.                                        
081100                                                                          
081200     PERFORM S01-KOLLA-GEMENSAM-INPUT                                     
081300                                                                          
081400     IF MID-KVNTOFG = ALL '+'                                             
081500*       MOVE MFS-RENSA-FAELT TO MOD-KVNTOFG-IN                            
081600        IF MID-KDSORT-KVNTOFG = ALL '+'                                   
081700           MOVE MFS-RENSA-FAELT TO MOD-KDSORT-KVNTOFG-IN                  
081800        ELSE                                                              
081900           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-KVNTOFG-ATTR             
082000           MOVE NEJ TO INDATA-SW                                          
082100        END-IF                                                            
082200     ELSE                                                                 
082300        MOVE MID-KVNTOFG  TO DEC-IDFRIDATA                                
082400        MOVE 2            TO DEC-KVHELTAL                                 
082500        MOVE 3            TO DEC-KVDECIMAL                                
082600                                                                          
082700        CALL WDECEDIT USING DEC-WDECAREA                                  
082800                                                                          
082900        IF DEC-KDSVAR-OK                                                  
083000           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVNTOFG-ATTR                  
083100           MOVE DEC-IDEDITDATA       TO WS-KVNTOFG                        
083200        ELSE                                                              
083300           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVNTOFG-ATTR                    
083400           MOVE NEJ TO INDATA-SW                                          
083500        END-IF                                                            
083600                                                                          
083700        IF MID-KDSORT-KVNTOFG = ALL '+'                                   
083800           MOVE MFS-RENSA-FAELT    TO MOD-KDSORT-KVNTOFG-IN               
083900           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-KVNTOFG-ATTR             
084000           MOVE NEJ TO INDATA-SW                                          
084100        ELSE                                                              
084200           IF MID-KDSORT-KVNTOFG = 'KG' OR 'M ' OR 'L '                   
084300              MOVE MFS-ALFA-FAELT-RAETT TO                                
084400                                MOD-KDSORT-KVNTOFG-ATTR                   
084500           ELSE                                                           
084600              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-KVNTOFG-ATTR          
084700              MOVE NEJ TO INDATA-SW                                       
084800           END-IF                                                         
084900        END-IF                                                            
085000     END-IF                                                               
085100                                                                          
085200     IF MID-VLFG = ALL '+'                                                
085300*       MOVE MFS-RENSA-FAELT TO MOD-VLFG-IN                               
085400        IF MID-KDSORT-VLFG = ALL '+'                                      
085500*          MOVE MFS-RENSA-FAELT TO MOD-KDSORT-VLFG-IN                     
085600           CONTINUE                                                       
085700        ELSE                                                              
085800           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-VLFG-ATTR                
085900           MOVE NEJ TO INDATA-SW                                          
086000        END-IF                                                            
086100     ELSE                                                                 
086200        MOVE MID-VLFG TO DEC-IDFRIDATA                                    
086300        MOVE 4        TO DEC-KVHELTAL                                     
086400        MOVE 3        TO DEC-KVDECIMAL                                    
086500                                                                          
086600        CALL WDECEDIT USING DEC-WDECAREA                                  
086700                                                                          
086800        IF DEC-KDSVAR-OK                                                  
086900           MOVE MFS-ALFA-FAELT-RAETT TO MOD-VLFG-ATTR                     
087000           MOVE DEC-IDEDITDATA       TO WS-VLFG                           
087100        ELSE                                                              
087200           MOVE MFS-ALFA-FAELT-FEL TO MOD-VLFG-ATTR                       
087300           MOVE NEJ TO INDATA-SW                                          
087400        END-IF                                                            
087500                                                                          
087600        IF MID-KDSORT-VLFG = ALL '+'                                      
087700*          MOVE MFS-RENSA-FAELT TO MOD-KDSORT-VLFG-IN                     
087800           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-VLFG-ATTR                
087900           MOVE NEJ TO INDATA-SW                                          
088000        ELSE                                                              
088100           IF MID-KDSORT-VLFG = 'KG  ' OR 'L   ' OR 'KG G'                
088200              MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSORT-VLFG-ATTR           
088300           ELSE                                                           
088400              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-VLFG-ATTR             
088500              MOVE NEJ TO INDATA-SW                                       
088600           END-IF                                                         
088700        END-IF                                                            
088800     END-IF                                                               
088900                                                                          
089000     .                                                                    
089100     EJECT                                                                
089200 GB-KOLLA-INPUT-REPLACE SECTION.                                          
089300                                                                          
089400     PERFORM S01-KOLLA-GEMENSAM-INPUT                                     
089500                                                                          
089600     IF MID-KVNTOFG = ALL '+'                                             
089700*       MOVE MFS-RENSA-FAELT TO MOD-KVNTOFG-IN                            
089800        IF MID-KDSORT-KVNTOFG = ALL '+'                                   
089900           MOVE MFS-RENSA-FAELT TO MOD-KDSORT-KVNTOFG-IN                  
090000        ELSE                                                              
090100           IF MID-KDSORT-KVNTOFG = 'KG' OR 'M ' OR 'L '                   
090200              MOVE MFS-ALFA-FAELT-RAETT TO                                
090300                                MOD-KDSORT-KVNTOFG-ATTR                   
090400           ELSE                                                           
090500              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-KVNTOFG-ATTR          
090600              MOVE NEJ TO INDATA-SW                                       
090700           END-IF                                                         
090800        END-IF                                                            
090900     ELSE                                                                 
091000        MOVE MID-KVNTOFG  TO DEC-IDFRIDATA                                
091100        MOVE 2            TO DEC-KVHELTAL                                 
091200        MOVE 3            TO DEC-KVDECIMAL                                
091300                                                                          
091400        CALL WDECEDIT USING DEC-WDECAREA                                  
091500                                                                          
091600        IF DEC-KDSVAR-OK                                                  
091700           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVNTOFG-ATTR                  
091800           MOVE DEC-IDEDITDATA       TO WS-KVNTOFG                        
091900        ELSE                                                              
092000           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVNTOFG-ATTR                    
092100           MOVE NEJ TO INDATA-SW                                          
092200        END-IF                                                            
092300                                                                          
092400        IF MID-KDSORT-KVNTOFG = ALL '+'                                   
092500           MOVE MFS-RENSA-FAELT TO MOD-KDSORT-KVNTOFG-IN                  
092600           IF ARTN01-ART-KDSORT-KVNTOFG = SPACE                           
092700             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-KVNTOFG-ATTR           
092800             MOVE NEJ TO INDATA-SW                                        
092900           END-IF                                                         
093000        ELSE                                                              
093100           IF MID-KDSORT-KVNTOFG = 'KG' OR 'M ' OR 'L '                   
093200              MOVE MFS-ALFA-FAELT-RAETT TO                                
093300                                MOD-KDSORT-KVNTOFG-ATTR                   
093400           ELSE                                                           
093500              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-KVNTOFG-ATTR          
093600              MOVE NEJ TO INDATA-SW                                       
093700           END-IF                                                         
093800        END-IF                                                            
093900     END-IF                                                               
094000                                                                          
094100     IF MID-VLFG = ALL '+'                                                
094200*       MOVE MFS-RENSA-FAELT TO MOD-VLFG-IN                               
094300        IF MID-KDSORT-VLFG = ALL '+'                                      
094400*          MOVE MFS-RENSA-FAELT TO MOD-KDSORT-VLFG-IN                     
094500           CONTINUE                                                       
094600        ELSE                                                              
094700           IF MID-KDSORT-VLFG = 'KG  ' OR 'L   ' OR 'KG G'                
094800              MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSORT-VLFG-ATTR           
094900           ELSE                                                           
095000              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-VLFG-ATTR             
095100              MOVE NEJ TO INDATA-SW                                       
095200           END-IF                                                         
095300        END-IF                                                            
095400     ELSE                                                                 
095500        MOVE MID-VLFG TO DEC-IDFRIDATA                                    
095600        MOVE 4        TO DEC-KVHELTAL                                     
095700        MOVE 3        TO DEC-KVDECIMAL                                    
095800                                                                          
095900        CALL WDECEDIT USING DEC-WDECAREA                                  
096000                                                                          
096100        IF DEC-KDSVAR-OK                                                  
096200           MOVE MFS-ALFA-FAELT-RAETT TO MOD-VLFG-ATTR                     
096300           MOVE DEC-IDEDITDATA       TO WS-VLFG                           
096400        ELSE                                                              
096500           MOVE MFS-ALFA-FAELT-FEL TO MOD-VLFG-ATTR                       
096600           MOVE NEJ TO INDATA-SW                                          
096700        END-IF                                                            
096800                                                                          
096900        IF MID-KDSORT-VLFG = ALL '+'                                      
097000*          MOVE MFS-RENSA-FAELT TO MOD-KDSORT-VLFG-IN                     
097100           IF ARTN01-ART-KDSORT-VLFG = SPACE                              
097200             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-VLFG-ATTR              
097300             MOVE NEJ TO INDATA-SW                                        
097400           END-IF                                                         
097500        ELSE                                                              
097600           IF MID-KDSORT-VLFG = 'KG  ' OR 'L   ' OR 'KG G'                
097700              MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSORT-VLFG-ATTR           
097800           ELSE                                                           
097900              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-VLFG-ATTR             
098000              MOVE NEJ TO INDATA-SW                                       
098100           END-IF                                                         
098200        END-IF                                                            
098300     END-IF                                                               
098400     .                                                                    
098500     EJECT                                                                
098600 H-UPPDATERA SECTION.                                                     
098700******************************************************************        
098800* FUNKTIONER  - NYUPPLÄGG/BORTTAG/UPPDATERING AV ARTIKEL ARTN01  *        
098900*             - UPPDATERING AV IDPSN ARTC11                      *        
099000******************************************************************        
099100                                                                          
099200     PERFORM IMS-GET-ARTN01                                               
099300     IF SEGMENT-FINNS                                                     
099400        IF MID-FLBORT = JA                                                
099500           PERFORM IMS-DLET-ARTN                                          
099600        ELSE                                                              
099700           PERFORM HB-UPPDATERING-ARTIKEL                                 
099800        END-IF                                                            
099900     ELSE                                                                 
100000        PERFORM HA-NYUPPLAGG-ARTIKEL                                      
100100     END-IF                                                               
100200                                                                          
100300     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
100400     CALL WMEDKONV USING MED-WMEDAREA                                     
100500     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
100600     PERFORM MFS-FORM-ATTR-IN                                             
100700     PERFORM MFS-RENSA-FAELT-IN                                           
100800     MOVE NEJ TO WS-TENOTE1-IFYLLT                                        
100900                 WS-TENOTE2-IFYLLT                                        
101000     .                                                                    
101100     EJECT                                                                
101200 HA-NYUPPLAGG-ARTIKEL SECTION.                                            
101300                                                                          
101400     MOVE W-IDARTNR                TO ARTN01-ART-IDARTNR                  
101500     MOVE DAGENS-DATUM             TO ARTN01-ART-TIREGDAT                 
101600                                                                          
101700     IF MID-BEEMBMAT = ALL '+'                                            
101800        MOVE SPACE                 TO ARTN01-ART-BEEMBMAT                 
101900     ELSE                                                                 
102000        MOVE MID-BEEMBMAT          TO ARTN01-ART-BEEMBMAT                 
102100        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BEEMBMAT-UT-ATTR                
102200     END-IF                                                               
102300                                                                          
102400     IF MID-IDANMNR = ALL '+'                                             
102500        MOVE ZERO                  TO ARTN01-ART-IDANMNR                  
102600     ELSE                                                                 
102700        MOVE WS-IDANMNR            TO ARTN01-ART-IDANMNR                  
102800        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDANMNR-UT-ATTR                 
102900     END-IF                                                               
103000                                                                          
103100     IF MID-REKSIFFR-ANMNR = ALL '+'                                      
103200        MOVE ZERO                  TO ARTN01-ART-REKSIFFR-ANMNR           
103300     ELSE                                                                 
103400        MOVE WS-REKSIFFR           TO ARTN01-ART-REKSIFFR-ANMNR           
103500*       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-REKSIFFR-UT-ATTR                
103600     END-IF                                                               
103700                                                                          
103800     IF MID-VKART-FG = ALL '+'                                            
103900        MOVE ZERO                  TO ARTN01-ART-VKART-FG                 
104000     ELSE                                                                 
104100        MOVE WS-VKART-FG           TO ARTN01-ART-VKART-FG                 
104200        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-VKART-FG-UT-ATTR                
104300     END-IF                                                               
104400                                                                          
104500     IF MID-FLFROST = ALL '+'                                             
104600        MOVE NEJ                   TO ARTN01-ART-FLFROST                  
104700     ELSE                                                                 
104800        MOVE MID-FLFROST           TO ARTN01-ART-FLFROST                  
104900     END-IF                                                               
105000     MOVE MFS-ADD-LYS-UPP-FAELT    TO MOD-FLFROST-UT-ATTR                 
105100                                                                          
105200     IF MID-FLTACTIL = ALL '+'                                            
105300        MOVE NEJ                   TO ARTN01-ART-FLTACTIL                 
105400     ELSE                                                                 
105500        MOVE MID-FLTACTIL          TO ARTN01-ART-FLTACTIL                 
105600     END-IF                                                               
105700     MOVE MFS-ADD-LYS-UPP-FAELT    TO MOD-FLTACTIL-UT-ATTR                
105800                                                                          
105900     IF MID-FLVARINF = ALL '+'                                            
106000        MOVE SPACE                 TO ARTN01-ART-FLVARINF                 
106100     ELSE                                                                 
106200        MOVE MID-FLVARINF          TO ARTN01-ART-FLVARINF                 
106300     END-IF                                                               
106400     MOVE MFS-ADD-LYS-UPP-FAELT    TO MOD-FLVARINF-UT-ATTR                
106500                                                                          
106600     IF MID-FLVARINF-SDS = ALL '+'                                        
106700        MOVE SPACE                 TO ARTN01-ART-FLVARINF-SDS             
106800     ELSE                                                                 
106900        MOVE MID-FLVARINF-SDS      TO ARTN01-ART-FLVARINF-SDS             
107000     END-IF                                                               
107100     MOVE MFS-ADD-LYS-UPP-FAELT    TO MOD-FLVARINF-SDS-UT-ATTR            
107200                                                                          
107300     IF MID-IDAO-FG = ALL '+'                                             
107400        MOVE SPACE                 TO ARTN01-ART-IDAO-FG                  
107500     ELSE                                                                 
107600        MOVE MID-IDAO-FG           TO ARTN01-ART-IDAO-FG                  
107700        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDAO-FG-UT-ATTR                 
107800     END-IF                                                               
107900                                                                          
108000     IF MID-IDVARINF = ALL '+'                                            
108100        MOVE SPACE                 TO ARTN01-ART-IDVARINF                 
108200     ELSE                                                                 
108300        MOVE MID-IDVARINF          TO ARTN01-ART-IDVARINF                 
108400*       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDVARINF-UT-ATTR                
108500     END-IF                                                               
108600                                                                          
108700     IF MID-IDVARINF-SDS = ALL '+'                                        
108800        MOVE SPACE                 TO ARTN01-ART-IDVARINF-SDS             
108900     ELSE                                                                 
109000        MOVE MID-IDVARINF-SDS      TO ARTN01-ART-IDVARINF-SDS             
109100        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDVARINF-SDS-UT-ATTR            
109200     END-IF                                                               
109300                                                                          
109400     IF MID-KDFARG = ALL '+'                                              
109500        MOVE ZERO                  TO ARTN01-ART-KDFARG                   
109600     ELSE                                                                 
109700        MOVE MID-KDFARG            TO ARTN01-ART-KDFARG                   
109800        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDFARG-UT-ATTR                  
109900     END-IF                                                               
110000                                                                          
110100     IF MID-KDFGPRIO = ALL '+'                                            
110200        MOVE ZERO                  TO ARTN01-ART-KDFGPRIO                 
110300     ELSE                                                                 
110400        MOVE WS-KDFGPRIO           TO ARTN01-ART-KDFGPRIO                 
110500        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDFGPRIO-UT-ATTR                
110600     END-IF                                                               
110700                                                                          
110800     IF MID-KDLACK = ALL '+'                                              
110900        MOVE SPACE                 TO ARTN01-ART-KDLACK                   
111000     ELSE                                                                 
111100        MOVE MID-KDLACK            TO ARTN01-ART-KDLACK                   
111200        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDLACK-UT-ATTR                  
111300     END-IF                                                               
111400                                                                          
111500     IF MID-IDARTNR-RECEPT = ALL '+'                                      
111600        MOVE ZERO                  TO ARTN01-ART-IDARTNR-RECEPT           
111700     ELSE                                                                 
111800        MOVE WS-IDARTNR-RECEPT     TO ARTN01-ART-IDARTNR-RECEPT           
111900        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDARTNR-RECEPT-UT-ATTR          
112000     END-IF                                                               
112100                                                                          
112200     IF MID-VKFORSFG = ALL '+'                                            
112300        MOVE ZERO                  TO ARTN01-ART-VKFORSFG                 
112400     ELSE                                                                 
112500        MOVE WS-VKFORSFG           TO ARTN01-ART-VKFORSFG                 
112600        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-VKFORSFG-UT-ATTR                
112700     END-IF                                                               
112800                                                                          
112900     IF MID-VLFG = ALL '+'                                                
113000        MOVE ZERO                  TO ARTN01-ART-VLFG                     
113100        MOVE SPACE                 TO ARTN01-ART-KDSORT-VLFG              
113200     ELSE                                                                 
113300        MOVE WS-VLFG               TO ARTN01-ART-VLFG                     
113400        MOVE MID-KDSORT-VLFG       TO ARTN01-ART-KDSORT-VLFG              
113500        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-VLFG-UT-ATTR                    
113600*                                     MOD-KDSORT-VLFG-UT-ATTR             
113700     END-IF                                                               
113800                                                                          
113900     IF MID-KVFLAMP = ALL '+'                                             
114000        MOVE ZERO                  TO ARTN01-ART-KVFLAMP                  
114100     ELSE                                                                 
114200        IF MID-FLNEG = 'M'                                                
114300           COMPUTE WS-KVFLAMP-NEG = WS-KVFLAMP * -1.0                     
114400           MOVE WS-KVFLAMP-NEG     TO ARTN01-ART-KVFLAMP                  
114500        ELSE                                                              
114600           MOVE WS-KVFLAMP         TO ARTN01-ART-KVFLAMP                  
114700        END-IF                                                            
114800        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVFLAMP-UT-ATTR                 
114900     END-IF                                                               
115000                                                                          
115100     IF MID-KVNTOFG = ALL '+'                                             
115200        MOVE ZERO                  TO ARTN01-ART-KVNTOFG                  
115300        MOVE SPACE                 TO ARTN01-ART-KDSORT-KVNTOFG           
115400     ELSE                                                                 
115500        MOVE WS-KVNTOFG            TO ARTN01-ART-KVNTOFG                  
115600        MOVE MID-KDSORT-KVNTOFG    TO ARTN01-ART-KDSORT-KVNTOFG           
115700        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVNTOFG-UT-ATTR                 
115800                                      MOD-KDSORT-KVNTOFG-UT-ATTR          
115900     END-IF                                                               
116000                                                                          
116100     IF MID-KVVOC = ALL '+'                                               
116200        MOVE ZERO                  TO ARTN01-ART-KVVOC                    
116300     ELSE                                                                 
116400        MOVE WS-KVVOC              TO ARTN01-ART-KVVOC                    
116500        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVVOC-UT-ATTR                   
116600     END-IF                                                               
116700                                                                          
116800     IF MID-SUEQFG = ALL '+'                                              
116900        MOVE ZERO                  TO ARTN01-ART-SUEQFG                   
117000     ELSE                                                                 
117100        MOVE WS-SUEQFG             TO ARTN01-ART-SUEQFG                   
117200        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-SUEQFG-UT-ATTR                  
117300     END-IF                                                               
117400                                                                          
117500     IF MID-TENOTE(1) = ALL '+'                                           
117600        MOVE SPACE                 TO ARTN01-ART-TENOTE(1)                
117700     ELSE                                                                 
117800        MOVE MID-TENOTE(1)         TO ARTN01-ART-TENOTE(1)                
117900        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TENOTE-ATTR(1)                  
118000     END-IF                                                               
118100                                                                          
118200     IF MID-TENOTE(2) = ALL '+'                                           
118300        MOVE SPACE                 TO ARTN01-ART-TENOTE(2)                
118400     ELSE                                                                 
118500        MOVE MID-TENOTE(2)         TO ARTN01-ART-TENOTE(2)                
118600        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TENOTE-ATTR(2)                  
118700     END-IF                                                               
118800                                                                          
118900     PERFORM IMS-ISRT-ARTN01                                              
119000     PERFORM S02-UPPDATERA-ARTC11                                         
119100     .                                                                    
119200     EJECT                                                                
119300 HB-UPPDATERING-ARTIKEL SECTION.                                          
119400                                                                          
119500     IF MID-BEEMBMAT = ALL '+'                                            
119600        CONTINUE                                                          
119700     ELSE                                                                 
119800        MOVE MID-BEEMBMAT          TO ARTN01-ART-BEEMBMAT                 
119900        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BEEMBMAT-UT-ATTR                
120000     END-IF                                                               
120100                                                                          
120200     IF MID-IDANMNR = ALL '+'                                             
120300        CONTINUE                                                          
120400     ELSE                                                                 
120500        MOVE WS-IDANMNR            TO ARTN01-ART-IDANMNR                  
120600        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDANMNR-UT-ATTR                 
120700     END-IF                                                               
120800                                                                          
120900     IF MID-REKSIFFR-ANMNR = ALL '+'                                      
121000        CONTINUE                                                          
121100     ELSE                                                                 
121200        MOVE WS-REKSIFFR           TO ARTN01-ART-REKSIFFR-ANMNR           
121300*       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-REKSIFFR-UT-ATTR                
121400     END-IF                                                               
121500                                                                          
121600     IF MID-VKART-FG = ALL '+'                                            
121700        CONTINUE                                                          
121800     ELSE                                                                 
121900        MOVE WS-VKART-FG           TO ARTN01-ART-VKART-FG                 
122000        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-VKART-FG-UT-ATTR                
122100     END-IF                                                               
122200                                                                          
122300     IF MID-FLFROST = ALL '+'                                             
122400        CONTINUE                                                          
122500     ELSE                                                                 
122600        MOVE MID-FLFROST           TO ARTN01-ART-FLFROST                  
122700        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLFROST-UT-ATTR                 
122800     END-IF                                                               
122900                                                                          
123000     IF MID-FLTACTIL = ALL '+'                                            
123100        CONTINUE                                                          
123200     ELSE                                                                 
123300        MOVE MID-FLTACTIL          TO ARTN01-ART-FLTACTIL                 
123400        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLTACTIL-UT-ATTR                
123500     END-IF                                                               
123600                                                                          
123700     IF MID-FLVARINF = ALL '+'                                            
123800        CONTINUE                                                          
123900     ELSE                                                                 
124000        MOVE MID-FLVARINF          TO ARTN01-ART-FLVARINF                 
124100        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLVARINF-UT-ATTR                
124200     END-IF                                                               
124300                                                                          
124400     IF MID-FLVARINF-SDS = ALL '+'                                        
124500        CONTINUE                                                          
124600     ELSE                                                                 
124700        MOVE MID-FLVARINF-SDS      TO ARTN01-ART-FLVARINF-SDS             
124800        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLVARINF-SDS-UT-ATTR            
124900     END-IF                                                               
125000                                                                          
125100     IF MID-IDAO-FG = ALL '+'                                             
125200        CONTINUE                                                          
125300     ELSE                                                                 
125400        MOVE MID-IDAO-FG           TO ARTN01-ART-IDAO-FG                  
125500        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDAO-FG-UT-ATTR                 
125600     END-IF                                                               
125700                                                                          
125800     IF MID-IDVARINF = ALL '+'                                            
125900        CONTINUE                                                          
126000     ELSE                                                                 
126100        MOVE MID-IDVARINF          TO ARTN01-ART-IDVARINF                 
126200*       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDVARINF-UT-ATTR                
126300     END-IF                                                               
126400                                                                          
126500     IF MID-IDVARINF-SDS = ALL '+'                                        
126600        CONTINUE                                                          
126700     ELSE                                                                 
126800        MOVE MID-IDVARINF-SDS      TO ARTN01-ART-IDVARINF-SDS             
126900        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDVARINF-SDS-UT-ATTR            
127000     END-IF                                                               
127100                                                                          
127200     IF MID-KDFARG = ALL '+'                                              
127300        CONTINUE                                                          
127400     ELSE                                                                 
127500        MOVE MID-KDFARG            TO ARTN01-ART-KDFARG                   
127600        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDFARG-UT-ATTR                  
127700     END-IF                                                               
127800                                                                          
127900     IF MID-KDSORT-KVNTOFG = ALL '+'                                      
128000        CONTINUE                                                          
128100     ELSE                                                                 
128200        MOVE MID-KDSORT-KVNTOFG    TO ARTN01-ART-KDSORT-KVNTOFG           
128300        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDSORT-KVNTOFG-UT-ATTR          
128400     END-IF                                                               
128500                                                                          
128600     IF MID-KDSORT-VLFG = ALL '+'                                         
128700        CONTINUE                                                          
128800     ELSE                                                                 
128900        MOVE MID-KDSORT-VLFG       TO ARTN01-ART-KDSORT-VLFG              
129000*       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDSORT-VLFG-UT-ATTR             
129100     END-IF                                                               
129200                                                                          
129300     IF MID-KDFGPRIO = ALL '+'                                            
129400        CONTINUE                                                          
129500     ELSE                                                                 
129600        MOVE WS-KDFGPRIO           TO ARTN01-ART-KDFGPRIO                 
129700        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDFGPRIO-UT-ATTR                
129800     END-IF                                                               
129900                                                                          
130000     IF MID-KDLACK = ALL '+'                                              
130100        CONTINUE                                                          
130200     ELSE                                                                 
130300        MOVE MID-KDLACK            TO ARTN01-ART-KDLACK                   
130400        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDLACK-UT-ATTR                  
130500     END-IF                                                               
130600                                                                          
130700     IF MID-IDARTNR-RECEPT = ALL '+'                                      
130800        CONTINUE                                                          
130900     ELSE                                                                 
131000        MOVE WS-IDARTNR-RECEPT     TO ARTN01-ART-IDARTNR-RECEPT           
131100        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDARTNR-RECEPT-UT-ATTR          
131200     END-IF                                                               
131300                                                                          
131400     IF MID-VKFORSFG = ALL '+'                                            
131500        CONTINUE                                                          
131600     ELSE                                                                 
131700        MOVE WS-VKFORSFG           TO ARTN01-ART-VKFORSFG                 
131800        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-VKFORSFG-UT-ATTR                
131900     END-IF                                                               
132000                                                                          
132100     IF MID-VLFG = ALL '+'                                                
132200        CONTINUE                                                          
132300     ELSE                                                                 
132400        MOVE WS-VLFG               TO ARTN01-ART-VLFG                     
132500        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-VLFG-UT-ATTR                    
132600     END-IF                                                               
132700                                                                          
132800     IF MID-KVFLAMP = ALL '+'                                             
132900        CONTINUE                                                          
133000     ELSE                                                                 
133100        IF MID-FLNEG = 'M'                                                
133200           COMPUTE WS-KVFLAMP-NEG = WS-KVFLAMP * -1.0                     
133300           MOVE WS-KVFLAMP-NEG     TO ARTN01-ART-KVFLAMP                  
133400        ELSE                                                              
133500           MOVE WS-KVFLAMP         TO ARTN01-ART-KVFLAMP                  
133600        END-IF                                                            
133700        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVFLAMP-UT-ATTR                 
133800     END-IF                                                               
133900                                                                          
134000     IF MID-KVNTOFG = ALL '+'                                             
134100        CONTINUE                                                          
134200     ELSE                                                                 
134300        MOVE WS-KVNTOFG            TO ARTN01-ART-KVNTOFG                  
134400        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVNTOFG-UT-ATTR                 
134500     END-IF                                                               
134600                                                                          
134700     IF MID-KVVOC = ALL '+'                                               
134800        CONTINUE                                                          
134900     ELSE                                                                 
135000        MOVE WS-KVVOC              TO ARTN01-ART-KVVOC                    
135100        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVVOC-UT-ATTR                   
135200     END-IF                                                               
135300                                                                          
135400     IF MID-SUEQFG = ALL '+'                                              
135500        CONTINUE                                                          
135600     ELSE                                                                 
135700        MOVE WS-SUEQFG             TO ARTN01-ART-SUEQFG                   
135800        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-SUEQFG-UT-ATTR                  
135900     END-IF                                                               
136000                                                                          
136100     IF MID-TENOTE(1) = ALL '+'                                           
136200        CONTINUE                                                          
136300     ELSE                                                                 
136400        MOVE MID-TENOTE(1)         TO ARTN01-ART-TENOTE(1)                
136500        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TENOTE-ATTR(1)                  
136600     END-IF                                                               
136700                                                                          
136800     IF MID-TENOTE(2) = ALL '+'                                           
136900        CONTINUE                                                          
137000     ELSE                                                                 
137100        MOVE MID-TENOTE(2)         TO ARTN01-ART-TENOTE(2)                
137200        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TENOTE-ATTR(2)                  
137300     END-IF                                                               
137400                                                                          
137500     PERFORM IMS-REPL-ARTN                                                
137600     PERFORM S02-UPPDATERA-ARTC11                                         
137700     .                                                                    
137800     EJECT                                                                
137900 S01-KOLLA-GEMENSAM-INPUT SECTION.                                        
138000                                                                          
138100     MOVE NEJ TO WS-TENOTE1-IFYLLT                                        
138200                 WS-TENOTE2-IFYLLT                                        
138300                                                                          
138400     IF MID-KDFARG = ALL '+'                                              
138500*       MOVE MFS-RENSA-FAELT TO MOD-KDFARG-IN                             
138600        CONTINUE                                                          
138700     ELSE                                                                 
138800        IF MID-KDFARG NUMERIC                                             
138900           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDFARG-ATTR                    
139000        ELSE                                                              
139100           MOVE MFS-NUM-FAELT-FEL TO MOD-KDFARG-ATTR                      
139200           MOVE NEJ TO INDATA-SW                                          
139300        END-IF                                                            
139400     END-IF                                                               
139500                                                                          
139600     IF MID-KDLACK = ALL '+'                                              
139700*       MOVE MFS-RENSA-FAELT TO MOD-KDLACK-IN                             
139800        CONTINUE                                                          
139900     ELSE                                                                 
140000        MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDLACK-ATTR                      
140100     END-IF                                                               
140200                                                                          
140300     IF MID-IDARTNR-RECEPT = ALL '+'                                      
140400*       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-RECEPT-IN                     
140500        CONTINUE                                                          
140600     ELSE                                                                 
140700        IF MID-IDARTNR-RECEPT NUMERIC                                     
140800           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-RECEPT-ATTR            
140900           MOVE MID-IDARTNR-RECEPT TO WS-IDARTNR-RECEPT                   
141000        ELSE                                                              
141100           MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-RECEPT-ATTR              
141200           MOVE NEJ TO INDATA-SW                                          
141300        END-IF                                                            
141400     END-IF                                                               
141500                                                                          
141600     IF MID-VKFORSFG = ALL '+'                                            
141700*       MOVE MFS-RENSA-FAELT TO MOD-VKFORSFG-IN                           
141800        CONTINUE                                                          
141900     ELSE                                                                 
142000        MOVE MID-VKFORSFG  TO DEC-IDFRIDATA                               
142100        MOVE 4             TO DEC-KVHELTAL                                
142200        MOVE 3             TO DEC-KVDECIMAL                               
142300                                                                          
142400        CALL WDECEDIT USING DEC-WDECAREA                                  
142500                                                                          
142600        IF DEC-KDSVAR-OK                                                  
142700           MOVE MFS-ALFA-FAELT-RAETT TO MOD-VKFORSFG-ATTR                 
142800           MOVE DEC-IDEDITDATA       TO WS-VKFORSFG                       
142900        ELSE                                                              
143000           MOVE MFS-ALFA-FAELT-FEL TO MOD-VKFORSFG-ATTR                   
143100           MOVE NEJ TO INDATA-SW                                          
143200        END-IF                                                            
143300     END-IF                                                               
143400                                                                          
143500     IF MID-FLVARINF = ALL '+'                                            
143600*       MOVE MFS-RENSA-FAELT TO MOD-FLVARINF-IN                           
143700        CONTINUE                                                          
143800     ELSE                                                                 
143900        IF MID-FLVARINF = JA OR NEJ OR SPACE                              
144000           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLVARINF-ATTR                 
144100        ELSE                                                              
144200           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLVARINF-ATTR                   
144300           MOVE NEJ TO INDATA-SW                                          
144400        END-IF                                                            
144500     END-IF                                                               
144600                                                                          
144700     IF MID-FLVARINF-SDS = ALL '+'                                        
144800*       MOVE MFS-RENSA-FAELT TO MOD-FLVARINF-SDS-IN                       
144900        CONTINUE                                                          
145000     ELSE                                                                 
145100        IF MID-FLVARINF-SDS = JA OR NEJ OR SPACE                          
145200           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLVARINF-SDS-ATTR             
145300        ELSE                                                              
145400           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLVARINF-SDS-ATTR               
145500           MOVE NEJ TO INDATA-SW                                          
145600        END-IF                                                            
145700     END-IF                                                               
145800                                                                          
145900     IF MID-IDVARINF = ALL '+'                                            
146000*       MOVE MFS-RENSA-FAELT TO MOD-IDVARINF-IN                           
146100        CONTINUE                                                          
146200     ELSE                                                                 
146300        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDVARINF-ATTR                    
146400     END-IF                                                               
146500                                                                          
146600     IF MID-IDVARINF-SDS = ALL '+'                                        
146700*       MOVE MFS-RENSA-FAELT TO MOD-IDVARINF-SDS-IN                       
146800        CONTINUE                                                          
146900     ELSE                                                                 
147000        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDVARINF-SDS-ATTR                
147100     END-IF                                                               
147200                                                                          
147300     IF MID-KVVOC = ALL '+'                                               
147400*       MOVE MFS-RENSA-FAELT TO MOD-KVVOC-IN                              
147500        CONTINUE                                                          
147600     ELSE                                                                 
147700        MOVE MID-KVVOC  TO DEC-IDFRIDATA                                  
147800        MOVE 2          TO DEC-KVHELTAL                                   
147900        MOVE 3          TO DEC-KVDECIMAL                                  
148000                                                                          
148100        CALL WDECEDIT USING DEC-WDECAREA                                  
148200                                                                          
148300        IF DEC-KDSVAR-OK                                                  
148400           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVVOC-ATTR                    
148500           MOVE DEC-IDEDITDATA       TO WS-KVVOC                          
148600        ELSE                                                              
148700           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVVOC-ATTR                      
148800           MOVE NEJ TO INDATA-SW                                          
148900        END-IF                                                            
149000     END-IF                                                               
149100                                                                          
149200     IF MID-SUEQFG = ALL '+'                                              
149300*       MOVE MFS-RENSA-FAELT TO MOD-SUEQFG-IN                             
149400        CONTINUE                                                          
149500     ELSE                                                                 
149600        MOVE MID-SUEQFG TO DEC-IDFRIDATA                                  
149700        MOVE 3          TO DEC-KVHELTAL                                   
149800        MOVE 4          TO DEC-KVDECIMAL                                  
149900                                                                          
150000        CALL WDECEDIT USING DEC-WDECAREA                                  
150100                                                                          
150200        IF DEC-KDSVAR-OK                                                  
150300           MOVE MFS-ALFA-FAELT-RAETT TO MOD-SUEQFG-ATTR                   
150400           MOVE DEC-IDEDITDATA       TO WS-SUEQFG                         
150500        ELSE                                                              
150600           MOVE MFS-ALFA-FAELT-FEL TO MOD-SUEQFG-ATTR                     
150700           MOVE NEJ TO INDATA-SW                                          
150800        END-IF                                                            
150900     END-IF                                                               
151000                                                                          
151100     IF MID-FLNEG = ALL '+'                                               
151200*       MOVE MFS-RENSA-FAELT TO MOD-FLNEG-IN                              
151300        CONTINUE                                                          
151400     ELSE                                                                 
151500        IF MID-FLNEG = 'P' OR 'M'                                         
151600           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLNEG-ATTR                    
151700        ELSE                                                              
151800           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLNEG-ATTR                      
151900           MOVE NEJ TO INDATA-SW                                          
152000        END-IF                                                            
152100     END-IF                                                               
152200                                                                          
152300     IF MID-KVFLAMP = ALL '+'                                             
152400*       MOVE MFS-RENSA-FAELT TO MOD-KVFLAMP-IN                            
152500        IF MID-FLNEG = ALL '+'                                            
152600*          MOVE MFS-RENSA-FAELT TO MOD-FLNEG-IN                           
152700           CONTINUE                                                       
152800        ELSE                                                              
152900           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLNEG-ATTR                      
153000           MOVE NEJ TO INDATA-SW                                          
153100        END-IF                                                            
153200     ELSE                                                                 
153300        IF MID-KVFLAMP NUMERIC                                            
153400           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVFLAMP-ATTR                   
153500           MOVE MID-KVFLAMP TO WS-KVFLAMP                                 
153600        ELSE                                                              
153700           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVFLAMP-ATTR                    
153800           MOVE NEJ TO INDATA-SW                                          
153900        END-IF                                                            
154000     END-IF                                                               
154100                                                                          
154200     IF MID-FLFROST = ALL '+'                                             
154300*       MOVE MFS-RENSA-FAELT TO MOD-FLFROST-IN                            
154400        CONTINUE                                                          
154500     ELSE                                                                 
154600        IF MID-FLFROST = JA OR NEJ                                        
154700           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLFROST-ATTR                  
154800        ELSE                                                              
154900           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLFROST-ATTR                    
155000           MOVE NEJ TO INDATA-SW                                          
155100        END-IF                                                            
155200     END-IF                                                               
155300                                                                          
155400     IF MID-FLTACTIL = ALL '+'                                            
155500*       MOVE MFS-RENSA-FAELT TO MOD-FLTACTIL-IN                           
155600        CONTINUE                                                          
155700     ELSE                                                                 
155800        IF MID-FLTACTIL = JA OR NEJ                                       
155900           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLTACTIL-ATTR                 
156000        ELSE                                                              
156100           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLTACTIL-ATTR                   
156200           MOVE NEJ TO INDATA-SW                                          
156300        END-IF                                                            
156400     END-IF                                                               
156500                                                                          
156600     IF MID-IDPSN-IN = ALL '+'                                            
156700*       MOVE MFS-RENSA-FAELT TO MOD-IDPSN-IN                              
156800        CONTINUE                                                          
156900     ELSE                                                                 
157000        IF MID-IDPSN-IN NUMERIC                                           
157100           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPSN-IN-ATTR                  
157200        ELSE                                                              
157300           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPSN-IN-ATTR                  
157400           MOVE NEJ TO INDATA-SW                                          
157500        END-IF                                                            
157600     END-IF                                                               
157700                                                                          
157800     IF MID-IDAO-FG = ALL '+'                                             
157900*       MOVE MFS-RENSA-FAELT TO MOD-IDAO-FG-IN                            
158000        CONTINUE                                                          
158100     ELSE                                                                 
158200        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-FG-ATTR                     
158300     END-IF                                                               
158400                                                                          
158500     IF MID-KDFGPRIO = ALL '+'                                            
158600*       MOVE MFS-RENSA-FAELT TO MOD-KDFGPRIO-IN                           
158700        CONTINUE                                                          
158800     ELSE                                                                 
158900        IF MID-KDFGPRIO NUMERIC                                           
159000           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDFGPRIO-ATTR                  
159100           MOVE MID-KDFGPRIO TO WS-KDFGPRIO                               
159200        ELSE                                                              
159300           MOVE MFS-NUM-FAELT-FEL TO MOD-KDFGPRIO-ATTR                    
159400           MOVE NEJ TO INDATA-SW                                          
159500        END-IF                                                            
159600     END-IF                                                               
159700                                                                          
159800     IF MID-IDANMNR = ALL '+'                                             
159900*       MOVE MFS-RENSA-FAELT TO MOD-IDANMNR-IN                            
160000        CONTINUE                                                          
160100     ELSE                                                                 
160200        IF MID-IDANMNR NUMERIC                                            
160300           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDANMNR-ATTR                   
160400           MOVE MID-IDANMNR  TO WS-IDANMNR                                
160500        ELSE                                                              
160600           MOVE MFS-NUM-FAELT-FEL TO MOD-IDANMNR-ATTR                     
160700           MOVE NEJ TO INDATA-SW                                          
160800        END-IF                                                            
160900     END-IF                                                               
161000                                                                          
161100     IF MID-REKSIFFR-ANMNR = ALL '+'                                      
161200*       MOVE MFS-RENSA-FAELT TO MOD-REKSIFFR-IN                           
161300        CONTINUE                                                          
161400     ELSE                                                                 
161500        IF MID-REKSIFFR-ANMNR NUMERIC                                     
161600           MOVE MFS-NUM-FAELT-RAETT TO MOD-REKSIFFR-ATTR                  
161700           MOVE MID-REKSIFFR-ANMNR TO WS-REKSIFFR                         
161800        ELSE                                                              
161900           MOVE MFS-NUM-FAELT-FEL TO MOD-REKSIFFR-ATTR                    
162000           MOVE NEJ TO INDATA-SW                                          
162100        END-IF                                                            
162200     END-IF                                                               
162300                                                                          
162400     IF MID-VKART-FG = ALL '+'                                            
162500*       MOVE MFS-RENSA-FAELT TO MOD-VKART-FG-IN                           
162600        CONTINUE                                                          
162700     ELSE                                                                 
162800        IF MID-VKART-FG NUMERIC                                           
162900           MOVE MFS-NUM-FAELT-RAETT TO MOD-VKART-FG-ATTR                  
163000           MOVE MID-VKART-FG TO WS-VKART-FG                               
163100        ELSE                                                              
163200           MOVE MFS-NUM-FAELT-FEL TO MOD-VKART-FG-ATTR                    
163300           MOVE NEJ TO INDATA-SW                                          
163400        END-IF                                                            
163500     END-IF                                                               
163600                                                                          
163700     IF MID-BEEMBMAT = ALL '+'                                            
163800*       MOVE MFS-RENSA-FAELT TO MOD-BEEMBMAT-IN                           
163900        CONTINUE                                                          
164000     ELSE                                                                 
164100        MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEEMBMAT-ATTR                    
164200     END-IF                                                               
164300                                                                          
164400     IF MID-TENOTE(1)= ALL '+'                                            
164500        MOVE MFS-RENSA-FAELT TO MOD-TENOTE(1)                             
164600     ELSE                                                                 
164700        MOVE MFS-ALFA-FAELT-RAETT TO MOD-TENOTE-ATTR(1)                   
164800        MOVE MFS-ROER-EJ-FAELT TO MOD-TENOTE(1)                           
164900        MOVE JA TO WS-TENOTE1-IFYLLT                                      
165000     END-IF                                                               
165100                                                                          
165200     IF MID-TENOTE(2)= ALL '+'                                            
165300        MOVE MFS-RENSA-FAELT TO MOD-TENOTE(2)                             
165400     ELSE                                                                 
165500        MOVE MFS-ROER-EJ-FAELT TO MOD-TENOTE(2)                           
165600        MOVE MFS-ALFA-FAELT-RAETT TO MOD-TENOTE-ATTR(2)                   
165700        MOVE JA TO WS-TENOTE2-IFYLLT                                      
165800     END-IF                                                               
165900     .                                                                    
166000     EJECT                                                                
166100 S02-UPPDATERA-ARTC11 SECTION.                                            
166200*                                                                         
166300     IF MID-IDPSN-IN = ALL '+'                                            
166400        CONTINUE                                                          
166500     ELSE                                                                 
166600        PERFORM IMS-GET-ARTC01                                            
166700        IF SEGMENT-FINNS                                                  
166800           PERFORM IMS-GET-ARTC11                                         
166900           IF SEGMENT-FINNS                                               
167000              MOVE MID-IDPSN-IN TO CLAG-IDPSN                             
167100              PERFORM IMS-REPL-ARTC                                       
167200              MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDPSN-UT-ATTR             
167300           END-IF                                                         
167400        END-IF                                                            
167500     END-IF                                                               
167600     .                                                                    
167700     EJECT                                                                
167800 MFS-RENSA-FAELT-UT SECTION.                                              
167900                                                                          
168000     MOVE MFS-RENSA-FAELT TO                                              
168100*                            MOD-BEART-S                                  
168200*                            MOD-BEART-GB                                 
168300                             MOD-KDARTHNT-H                               
168400*                            MOD-KDFARLIG                                 
168500*                            MOD-FLIART                                   
168600*                            MOD-KDERS                                    
168700*                            MOD-FL-KH                                    
168800*                            MOD-FL-KR                                    
168900*                            MOD-IDRITN                                   
169000                             MOD-IDPSN-UT                                 
169100                             MOD-BEEMBMAT-UT                              
169200                             MOD-FLFROST-UT                               
169300                             MOD-FLTACTIL-UT                              
169400                             MOD-FLVARINF-UT                              
169500                             MOD-FLVARINF-SDS-UT                          
169600                             MOD-IDVARINF-UT                              
169700                             MOD-IDVARINF-SDS-UT                          
169800                             MOD-KDFARG-UT                                
169900                             MOD-KDFGPRIO-UT                              
170000                             MOD-KDLACK-UT                                
170100                             MOD-KDSORT-KVNTOFG-UT                        
170200                             MOD-KDSORT-VLFG-UT                           
170300                             MOD-KVFLAMP-UT                               
170400                             MOD-KVNTOFG-UT                               
170500                             MOD-KVVOC-UT                                 
170600                             MOD-IDARTNR-RECEPT-UT                        
170700                             MOD-SUEQFG-UT                                
170800                             MOD-VKFORSFG-UT                              
170900                             MOD-VLFG-UT                                  
171000                             MOD-IDAO-FG-UT                               
171100                             MOD-VKART-FG-UT                              
171200                             MOD-IDANMNR-UT                               
171300                             MOD-REKSIFFR-UT                              
171400     .                                                                    
171500     EJECT                                                                
171600 MFS-RENSA-FAELT-IN SECTION.                                              
171700                                                                          
171800     MOVE MFS-RENSA-FAELT TO                                              
171900*                            MOD-IDPSN-IN                                 
172000*                            MOD-BEEMBMAT-IN                              
172100*                            MOD-FLFROST-IN                               
172200*                            MOD-FLTACTIL-IN                              
172300*                            MOD-FLVARINF-IN                              
172400*                            MOD-FLVARINF-SDS-IN                          
172500*                            MOD-IDVARINF-IN                              
172600*                            MOD-IDVARINF-SDS-IN                          
172700*                            MOD-KDFARG-IN                                
172800*                            MOD-KDFGPRIO-IN                              
172900*                            MOD-KDLACK-IN                                
173000                             MOD-KDSORT-KVNTOFG-IN                        
173100*                            MOD-KDSORT-VLFG-IN                           
173200*                            MOD-FLNEG-IN                                 
173300*                            MOD-KVFLAMP-IN                               
173400*                            MOD-KVNTOFG-IN                               
173500*                            MOD-KVVOC-IN                                 
173600*                            MOD-IDARTNR-RECEPT-IN                        
173700*                            MOD-SUEQFG-IN                                
173800*                            MOD-VKFORSFG-IN                              
173900*                            MOD-VLFG-IN                                  
174000*                            MOD-IDAO-FG-IN                               
174100                             MOD-FLBORT                                   
174200                             MOD-TENOTE(1)                                
174300                             MOD-TENOTE(2)                                
174400*                            MOD-VKART-FG-IN                              
174500*                            MOD-IDANMNR-IN                               
174600*                            MOD-REKSIFFR-IN                              
174700     .                                                                    
174800     EJECT                                                                
174900 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
175000                                                                          
175100     MOVE MFS-ROER-EJ-FAELT TO                                            
175200*                              MOD-BEART-S                                
175300*                              MOD-BEART-GB                               
175400                               MOD-KDARTHNT-H                             
175500*                              MOD-KDFARLIG                               
175600*                              MOD-FLIART                                 
175700*                              MOD-KDERS                                  
175800*                              MOD-FL-KH                                  
175900*                              MOD-FL-KR                                  
176000*                              MOD-IDRITN                                 
176100                               MOD-IDPSN-UT                               
176200                               MOD-BEEMBMAT-UT                            
176300                               MOD-FLFROST-UT                             
176400                               MOD-FLTACTIL-UT                            
176500                               MOD-FLVARINF-UT                            
176600                               MOD-FLVARINF-SDS-UT                        
176700                               MOD-IDVARINF-UT                            
176800                               MOD-IDVARINF-SDS-UT                        
176900                               MOD-KDFARG-UT                              
177000                               MOD-KDFGPRIO-UT                            
177100                               MOD-KDLACK-UT                              
177200                               MOD-KDSORT-KVNTOFG-UT                      
177300                               MOD-KDSORT-VLFG-UT                         
177400                               MOD-KVFLAMP-UT                             
177500                               MOD-KVNTOFG-UT                             
177600                               MOD-KVVOC-UT                               
177700                               MOD-IDARTNR-RECEPT-UT                      
177800                               MOD-SUEQFG-UT                              
177900                               MOD-VKFORSFG-UT                            
178000                               MOD-VLFG-UT                                
178100                               MOD-IDAO-FG-UT                             
178200                               MOD-VKART-FG-UT                            
178300                               MOD-IDANMNR-UT                             
178400                               MOD-REKSIFFR-UT                            
178500     .                                                                    
178600     EJECT                                                                
178700*MFS-ROER-EJ-FAELT-IN  SECTION.                                           
178800*                                                                         
178900*    MOVE MFS-ROER-EJ-FAELT TO                                            
179000*                              MOD-IDPSN-IN                               
179100*                              MOD-BEEMBMAT-IN                            
179200*                              MOD-FLFROST-IN                             
179300*                              MOD-FLTACTIL-IN                            
179400*                              MOD-FLVARINF-IN                            
179500*                              MOD-FLVARINF-SDS-IN                        
179600*                              MOD-IDVARINF-IN                            
179700*                              MOD-IDVARINF-SDS-IN                        
179800*                              MOD-KDFARG-IN                              
179900*                              MOD-KDFGPRIO-IN                            
180000*                              MOD-KDLACK-IN                              
180100*                              MOD-KDSORT-KVNTOFG-IN                      
180200*                              MOD-KDSORT-VLFG-IN                         
180300*                              MOD-FLNEG-IN                               
180400*                              MOD-KVFLAMP-IN                             
180500*                              MOD-KVNTOFG-IN                             
180600*                              MOD-KVVOC-IN                               
180700*                              MOD-IDARTNR-RECEPT-IN                      
180800*                              MOD-SUEQFG-IN                              
180900*                              MOD-VKFORSFG-IN                            
181000*                              MOD-VLFG-IN                                
181100*                              MOD-IDAO-FG-IN                             
181200*                              MOD-VKART-FG-IN                            
181300*                              MOD-IDANMNR-IN                             
181400*                              MOD-REKSIFFR-IN                            
181500*    .                                                                    
181600     EJECT                                                                
181700 MFS-ROER-EJ-FAELT-IN-UT SECTION.                                         
181800                                                                          
181900     IF MID-TENOTE(1) = ALL '+'                                           
182000        CONTINUE                                                          
182100     ELSE                                                                 
182200        MOVE MFS-ROER-EJ-FAELT TO MOD-TENOTE(1)                           
182300        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TENOTE-ATTR(1)                  
182400        MOVE JA TO WS-TENOTE1-IFYLLT                                      
182500     END-IF                                                               
182600                                                                          
182700     IF MID-TENOTE(2) = ALL '+'                                           
182800        CONTINUE                                                          
182900     ELSE                                                                 
183000        MOVE MFS-ROER-EJ-FAELT TO MOD-TENOTE(2)                           
183100        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TENOTE-ATTR(2)                  
183200        MOVE JA TO WS-TENOTE2-IFYLLT                                      
183300     END-IF                                                               
183400                                                                          
183500     IF MID-FLBORT = ALL '+'                                              
183600        CONTINUE                                                          
183700     ELSE                                                                 
183800        MOVE MFS-ROER-EJ-FAELT TO MOD-FLBORT                              
183900     END-IF                                                               
184000     .                                                                    
184100     EJECT                                                                
184200 MFS-FORM-ATTR-IN SECTION.                                                
184300                                                                          
184400     MOVE MFS-FORMATETS-ATTR TO                                           
184500                                MOD-IDPSN-IN-ATTR                         
184600                                MOD-BEEMBMAT-ATTR                         
184700                                MOD-FLFROST-ATTR                          
184800                                MOD-FLTACTIL-ATTR                         
184900                                MOD-FLVARINF-ATTR                         
185000                                MOD-FLVARINF-SDS-ATTR                     
185100                                MOD-IDAO-FG-ATTR                          
185200                                MOD-IDVARINF-ATTR                         
185300                                MOD-IDVARINF-SDS-ATTR                     
185400                                MOD-KDFARG-ATTR                           
185500                                MOD-KDLACK-ATTR                           
185600                                MOD-KDFGPRIO-ATTR                         
185700                                MOD-KDSORT-KVNTOFG-ATTR                   
185800                                MOD-KDSORT-VLFG-ATTR                      
185900                                MOD-KVFLAMP-ATTR                          
186000                                MOD-KVNTOFG-ATTR                          
186100                                MOD-KVVOC-ATTR                            
186200                                MOD-IDARTNR-RECEPT-ATTR                   
186300                                MOD-SUEQFG-ATTR                           
186400                                MOD-VKFORSFG-ATTR                         
186500                                MOD-VLFG-ATTR                             
186600                                MOD-FLBORT-ATTR                           
186700                                MOD-VKART-FG-ATTR                         
186800                                MOD-IDANMNR-ATTR                          
186900                                MOD-REKSIFFR-ATTR                         
187000     .                                                                    
187100     EJECT                                                                
187200* --- IMS SEKTIONER ---                                                   
187300     SKIP3                                                                
187400 IMS-GET-MSG SECTION.                                                     
187500     MOVE '  QC' TO GODK-STATUSKODER                                      
187600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
187700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
187800     PERFORM IMS-STATUSKONTROLL                                           
187900     .                                                                    
188000     SKIP3                                                                
188100 IMS-INSERT-MSG SECTION.                                                  
188200*    IF MSGI-IDLAND-SPR NOT = 'GB'                                        
188300*       MOVE '0' TO MFS-KDHUVOMR                                          
188400*    END-IF                                                               
188500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
188600     MOVE SPACE TO GODK-STATUSKODER                                       
188700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
188800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
188900     PERFORM IMS-STATUSKONTROLL                                           
189000     .                                                                    
189100     EJECT                                                                
189200 IMS-GET-ARTN01 SECTION.                                                  
189300     STRING 'WLARTN01(IDARTNR  =' W-IDARTNR-X ')'                         
189400          DELIMITED BY SIZE INTO SSA1                                     
189500     MOVE '  GE' TO GODK-STATUSKODER                                      
189600     CALL CBLTDLI USING GHU ARTN-PCB DLI-IO-AREA2 SSA1                    
189700     MOVE ARTN-STATUS-CODE TO STATUS-WS                                   
189800     PERFORM IMS-STATUSKONTROLL                                           
189900     .                                                                    
190000     SKIP3                                                                
190100 IMS-ISRT-ARTN01 SECTION.                                                 
190200     MOVE 'WLARTN01 ' TO SSA1                                             
190300     MOVE '  ' TO GODK-STATUSKODER                                        
190400     CALL CBLTDLI USING ISRT ARTN-PCB DLI-IO-AREA2 SSA1                   
190500     MOVE ARTN-STATUS-CODE TO STATUS-WS                                   
190600     PERFORM IMS-STATUSKONTROLL                                           
190700     .                                                                    
190800     SKIP3                                                                
190900 IMS-REPL-ARTN SECTION.                                                   
191000     MOVE '  ' TO GODK-STATUSKODER                                        
191100     CALL CBLTDLI USING REPL ARTN-PCB DLI-IO-AREA2                        
191200     MOVE ARTN-STATUS-CODE TO STATUS-WS                                   
191300     PERFORM IMS-STATUSKONTROLL                                           
191400     .                                                                    
191500     SKIP3                                                                
191600 IMS-DLET-ARTN SECTION.                                                   
191700     MOVE '  ' TO GODK-STATUSKODER                                        
191800     CALL CBLTDLI USING DLET ARTN-PCB DLI-IO-AREA2                        
191900     MOVE ARTN-STATUS-CODE TO STATUS-WS                                   
192000     PERFORM IMS-STATUSKONTROLL                                           
192100     .                                                                    
192200     EJECT                                                                
192300 IMS-GET-ARTC01 SECTION.                                                  
192400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
192500          DELIMITED BY SIZE INTO SSA1                                     
192600     MOVE '  GE' TO GODK-STATUSKODER                                      
192700     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
192800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
192900     PERFORM IMS-STATUSKONTROLL                                           
193000     .                                                                    
193100     SKIP3                                                                
193200 IMS-GET-ARTC11 SECTION.                                                  
193300     MOVE 'WLARTC11 ' TO SSA1                                             
193400     MOVE '  GE' TO GODK-STATUSKODER                                      
193500     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA SSA1                    
193600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
193700     PERFORM IMS-STATUSKONTROLL                                           
193800     .                                                                    
193900     SKIP3                                                                
194000 IMS-REPL-ARTC SECTION.                                                   
194100     MOVE '  ' TO GODK-STATUSKODER                                        
194200     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA                         
194300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
194400     PERFORM IMS-STATUSKONTROLL                                           
194500     .                                                                    
194600     EJECT                                                                
194700 IMS-GET-BENA01 SECTION.                                                  
194800     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
194900          DELIMITED BY SIZE INTO SSA1                                     
195000     MOVE '  GE' TO GODK-STATUSKODER                                      
195100     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1                      
195200     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
195300     PERFORM IMS-STATUSKONTROLL                                           
195400     .                                                                    
195500     SKIP3                                                                
195600 IMS-GET-BENA11 SECTION.                                                  
195700     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
195800          DELIMITED BY SIZE INTO SSA1                                     
195900     MOVE '  GE' TO GODK-STATUSKODER                                      
196000     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA SSA1                     
196100     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
196200     PERFORM IMS-STATUSKONTROLL                                           
196300     .                                                                    
196400     EJECT                                                                
196500 IMS-GET-W6KVAH01 SECTION.                                                
196600     STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X ')'                         
196700          DELIMITED BY SIZE INTO SSA1                                     
196800     MOVE '  GE' TO GODK-STATUSKODER                                      
196900     CALL CBLTDLI USING GU KVAH-PCB DLI-IO-AREA SSA1                      
197000     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
197100     PERFORM IMS-STATUSKONTROLL                                           
197200     .                                                                    
197300     SKIP3                                                                
197400 IMS-GET-W6KVAH11 SECTION.                                                
197500     MOVE 'W6KVAH11 ' TO SSA1                                             
197600     MOVE '  GE' TO GODK-STATUSKODER                                      
197700     CALL CBLTDLI USING GNP KVAH-PCB DLI-IO-AREA SSA1                     
197800     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
197900     PERFORM IMS-STATUSKONTROLL                                           
198000     .                                                                    
198100     SKIP3                                                                
198200 IMS-GET-W6KVAG01 SECTION.                                                
198300     STRING 'W6KVAG01(W6H7B1KY>=' W-W6H7B1KY-MIN-X                        
198400                    '&W6H7B1KY<=' W-W6H7B1KY-MAX-X ')'                    
198500          DELIMITED BY SIZE INTO SSA1                                     
198600     MOVE '  GE' TO GODK-STATUSKODER                                      
198700     CALL CBLTDLI USING GU KVAG-PCB DLI-IO-AREA SSA1                      
198800     MOVE KVAG-STATUS-CODE TO STATUS-WS                                   
198900     PERFORM IMS-STATUSKONTROLL                                           
199000     .                                                                    
199100     EJECT                                                                
199200 IMS-STATUSKONTROLL SECTION.                                              
199300                                                                          
199400     SET STATUS-IX TO 1                                                   
199500     SEARCH GODK-STATUS                                                   
199600       AT END                                                             
199700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
199800         DELIMITED BY SIZE INTO FELTEXT                                   
199900         CALL FELLOG                                                      
200000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
200100         CONTINUE                                                         
200200     END-SEARCH                                                           
200300     .                                                                    
