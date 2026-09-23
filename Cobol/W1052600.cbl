000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1052600.                                                
000300 AUTHOR.         CONNY EGHOLT.                                            
000400 DATE-WRITTEN.   98/06/23.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET ADMINISTRERAR BENÄMNINGSNUMMER PÅ                     
000900*        HÄNDELSEREGISTER WL1219.                                         
001000*        DE BENÄMNINGSNUMMER SOM FINNS PÅ WL121921 KOMMER                 
001100*        ATT SKICKAS FÖR ÖVERSÄTTNING, DÅ DET FINNS ETT                   
001200*        IDORDER-BEN (>ZERO) UPPLAGT PÅ WL121911.                         
001300*                                                                         
001400*        BESTÄLLNING SKER I RUTIN W152B2, SOM OCKSÅ TÖMMER                
001500*        ALLA WL121921-SEGMENT OCH NOLLAR IDORDER-BEN.                    
001600*                                                                         
001700*        PROGRAMMET ADMINISTRERAR OCKSÅ DATAELEMENTET IDORDER-LEX,        
001800*        SOM ANVÄNDES FÖR ORDER AV ÖVERSÄTTNING AV KATALOGENS             
001900*        LEXIKON-TEXTER RUBRIK,FOTNOT OCH TILLÄGGSTEXTER. (W152B2)        
002000*                                                                         
002100*        PROGRAMMET UPPDATERAR WL1219 (WDGX)                              
002200*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W1T526 + U                                          
002600*        MID:         W1I52601                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W1O52601                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W1052600'.            
003900                                                                          
004000*    --- arbetsfält för felmeddelanden vid call abend/fellog              
004100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004200                                                                          
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500 77  WS-KONTROLL-IDAVD           PIC X(5)    VALUE '91232'.               
004600 77  WS-IDAVD                    PIC X(5)    VALUE SPACE.                 
004700*    --- booleanska operatorer i ssa'er                                   
004800 77  OCH                         PIC X       VALUE '&'.                   
004900 77  ELLER                       PIC X       VALUE '!'.                   
005000*    --- kommando kod i ssa'er                                            
005100 77  CMD-KOD                     PIC X       VALUE '-'.                   
005200 77  LAES-FIRST                  PIC X       VALUE 'F'.                   
005300 77  LAES-VANLIGT                PIC X       VALUE '-'.                   
005400                                                                          
005500*    --- index för bläddringsrader                                        
005600 77  INDX                        PIC S9(9)  VALUE +0    COMP-3.           
005700 77  MAX-INDX                    PIC S9(9)  VALUE +28   COMP-3.           
005800*    --- index för språktabell                                            
005900 77  IX                          PIC S9(9)  VALUE +0    COMP-3.           
006000 77  MAX-IX                      PIC S9(9)  VALUE +17   COMP-3.           
006100*    --- arbetsfält för aktuella nyckelvärden från skärmen                
006200                                                                          
006300 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006400     88  INDATA-OK                           VALUE 'J'.                   
006500     88  INDATA-FEL                          VALUE 'N'.                   
006600                                                                          
006700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006800     88  NYCKLAR-OK                          VALUE 'J'.                   
006900     88  NYCKLAR-FEL                         VALUE 'N'.                   
007000                                                                          
007100 77  OVERSATT-SW                 PIC X       VALUE 'N'.                   
007200     88  OVERSATTNING-BEHOVS                 VALUE 'J'.                   
007300     88  OVERSATTNING-BEHOVS-EJ              VALUE 'N'.                   
007400                                                                          
007500 77  GRUNDSPRAK-SW               PIC X       VALUE 'N'.                   
007600     88  GRUNDSPRAK-SAKNAS                   VALUE 'J'.                   
007700     88  GRUNDSPRAK-FINNS                    VALUE 'N'.                   
007800                                                                          
007900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008000     88  EGEN-MID                            VALUE '1526'.                
008100     88  GODK-MID                            VALUE '1521' '1522'          
008200                                                   '1523' '1524'          
008300                                                   '1525' '1526'.         
008400     88  HELP-MID                            VALUE '0551'.                
008500     EJECT                                                                
008600 01  FILLER                      PIC X(16) VALUE 'DATUM-OCH-TID'.         
008700 01  WS-CURRENT-DATE             PIC X(21).                               
008800 01  FILLER REDEFINES WS-CURRENT-DATE.                                    
008900     03  FILLER                  PIC X(2).                                
009000     03  DAGENS-6-DATUM          PIC 9(6).                                
009100     03  FILLER                  PIC X(13).                               
009200 01  FILLER REDEFINES WS-CURRENT-DATE.                                    
009300     03  DAGENS-8-DATUM          PIC 9(8).                                
009400     03  DAGENS-HHMMSSTH         PIC 9(8).                                
009500     03  FILLER                  PIC X(5).                                
009600                                                                          
009700*    --- AREA FÖR 9KOMPL-NYCKLAR FÖR WL1221                               
009800 01  WS-TITIDLOP.                                                         
009900     03  WS-TITIDLOP-HHMMSSTH    PIC 9(8)    VALUE ZERO.                  
010000     03  WS-TITIDLOP-LOPNR       PIC 9       VALUE ZERO.                  
010100 01  FILLER REDEFINES WS-TITIDLOP.                                        
010200     03  WS-TITIDLOP-KY          PIC 9(9).                                
010300                                                                          
010400 01  WS-KY1221-AREA.                                                      
010500     03 WS-TITIREGD-9KOMPL       PIC S9(9)   COMP-3 VALUE ZERO.           
010600     03 WS-TITIDLOP-9KOMPL       PIC S9(9)   COMP-3 VALUE ZERO.           
010700                                                                          
010800     EJECT                                                                
010900*    --- subprogram och parameterareor                                    
011000 01  GENERELLA-SUBPROGRAM.                                                
011100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
011300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011500     EJECT                                                                
011600 01  SPRAK-TABELLAREA.                                                    
011700     03  TABELL-DATA.                                                     
011800         05  FILLER              PIC XXX     VALUE 'D  '.                 
011900         05  FILLER              PIC XXX     VALUE 'E  '.                 
012000         05  FILLER              PIC XXX     VALUE 'F  '.                 
012100         05  FILLER              PIC XXX     VALUE 'I  '.                 
012200         05  FILLER              PIC XXX     VALUE 'J  '.                 
012300         05  FILLER              PIC XXX     VALUE 'KOR'.                 
012400         05  FILLER              PIC XXX     VALUE 'MAL'.                 
012500         05  FILLER              PIC XXX     VALUE 'NL '.                 
012600         05  FILLER              PIC XXX     VALUE 'P  '.                 
012700         05  FILLER              PIC XXX     VALUE 'RC '.                 
012800         05  FILLER              PIC XXX     VALUE 'RCN'.                 
012900         05  FILLER              PIC XXX     VALUE 'RUS'.                 
013000         05  FILLER              PIC XXX     VALUE 'SF '.                 
013100         05  FILLER              PIC XXX     VALUE 'T  '.                 
013200         05  FILLER              PIC XXX     VALUE 'TR '.                 
013300         05  FILLER              PIC XXX     VALUE 'USA'.                 
013310         05  FILLER              PIC XXX     VALUE 'PL '.                 
013400     03  TABELL REDEFINES TABELL-DATA.                                    
013500         05  WS-IDSKYLT          PIC XXX  OCCURS 17.                      
013600                                                                          
013700     EJECT                                                                
013800*    --- parametrar till subprogram wmedkonv                              
013900*01 -COPY WMEDAREA                                                        
014000     SKIP3                                                                
014100 01  MESSAGE-CODES.                                                       
014200     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
014300     03  ERR-NOT-REGISTERED      PIC X(3)    VALUE '010'.                 
014400     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
014500     03  ERR-NO-SOURCEDATA       PIC X(3)    VALUE '268'.                 
014600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
014700                                                                          
014800     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
014900     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
015000     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
015100     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
015200     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
015300     03  INF-ALREADY-TRANSLATED  PIC X(3)    VALUE '266'.                 
015400     03  INF-EMPTY-REGISTER      PIC X(3)    VALUE '267'.                 
015500     EJECT                                                                
015600*    --- parametrar till subprogram w005init                              
015700*                                                                         
015800 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
015900     SKIP3                                                                
016000*01 -COPY WMSGINIT                                                        
016100     EJECT                                                                
016200*    --- area med data som ska sparas mellan dialogstegen                 
016300*                                                                         
016400 01  SPAR-AREA.                                                           
016500     03  SPAR-IDTRANS             PIC X(4)    VALUE '1526'.               
016600     03  SPAR-IDBENNR-ENTER       PIC S9(7)        COMP-3.                
016700     03  SPAR-IDBENNR-NEXT        PIC S9(7)        COMP-3.                
016800     03  SPAR-BEART-GB-SV.                                                
016900       05  SPAR-BEART-GB           PIC X(25)   VALUE SPACE.               
017000       05  SPAR-BEART-SV           PIC X(25)   VALUE SPACE.               
017100     EJECT                                                                
017200*    --- areor för mfs och skärmhantering                                 
017300*                                                                         
017400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
017500     SKIP3                                                                
017600*01  MID -COPY W1I52601                                                   
017700     EJECT                                                                
017800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
017900     SKIP3                                                                
018000*01  -COPY WMSGAREA                                                       
018100     EJECT                                                                
018200     03  MOD REDEFINES MSG-AREA.                                          
018300*      05  -COPY W1O52601                                                 
018400     EJECT                                                                
018500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
018600     SKIP3                                                                
018700*01  -COPY WMFSAREA                                                       
018800     EJECT                                                                
018900*    --- arbets-areor till ims-sektionerna                                
019000*                                                                         
019100     EJECT                                                                
019200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019300     SKIP3                                                                
019400 01  NYCKLAR-TILL-DLI.                                                    
019500*    --- värde på bläddringsnyckel för första raden på skärmen            
019600     03  W-KY1219-X.                                                      
019700         05 W-1219-IDHTYP        PIC X(4)    VALUE '1219'.                
019800         05 FILLER               PIC X(26)   VALUE LOW-VALUE.             
019900                                                                          
020000     03  W-KDSEGKEY-X.                                                    
020100         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
020200                                                                          
020300     03  W-IDBENNR-X.                                                     
020400         05  W-IDBENNR           PIC S9(7)   VALUE ZERO COMP-3.           
020500                                                                          
020600     03  W-IDSKYLT-X.                                                     
020700         05  W-IDSKYLT           PIC X(3)    VALUE 'S  '.                 
020800     SKIP2                                                                
020900*    --- status-kod från ims                                              
021000 01  STATUS-WS                   PIC XX.                                  
021100     88  SEGMENT-FINNS                       VALUE '  '.                  
021200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
021300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
021400     SKIP2                                                                
021500 01  GODK-STATUSKODER.                                                    
021600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021700     SKIP3                                                                
021800 01  SSA1                        PIC X(96).                               
021900 01  SSA2                        PIC X(96).                               
022000 01  SSA3                        PIC X(64).                               
022100     EJECT                                                                
022200*    --- ims funktionskoder                                               
022300*01  -COPY W0003                                                          
022400     EJECT                                                                
022500*    ---  DLI INPUT-OUTPUT AREA                                           
022600                                                                          
022700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WL121911'.                    
022800 01  DLI-IO-WL121911.                                                     
022900*    03  -COPY WDGX1220 -PRE 1219-                                        
023000     EJECT                                                                
023100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WL121921'.                    
023200 01  DLI-IO-WL121921.                                                     
023300*    03  -COPY WDGX1221 -PRE 1219-                                        
023400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBENA01'.                    
023500 01  DLI-IO-WLBENA01.                                                     
023600*    03  -COPY WDD301  -PRE BENA-                                         
023700     EJECT                                                                
023800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBENA11'.                    
023900 01  DLI-IO-WLBENA11.                                                     
024000*    03  -COPY WDD311  -PRE BENA-                                         
024100     EJECT                                                                
024200 LINKAGE SECTION.                                                         
024300*01  -COPY W0009   -PRE MSG-                                              
024400*01  -COPY W0008   -PRE USEA-                                             
024500     05  FILLER                  PIC X.                                   
024600                                                                          
024700*01  -COPY W0008  -PRE 1219-                                              
024800     05  FILLER                  PIC X.                                   
024900                                                                          
025000*01  -COPY W0008  -PRE BENA-                                              
025100     05  FILLER                  PIC X.                                   
025200     EJECT                                                                
025300 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB 1219-PCB BENA-PCB.            
025400 MAIN SECTION.                                                            
025500     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB 1219-PCB BENA-PCB.            
025600                                                                          
025700     PERFORM IMS-GET-MSG                                                  
025800     IF SEGMENT-FINNS                                                     
025900       PERFORM A-INIT                                                     
026000       PERFORM B-KOLLA-NYCKLAR                                            
026100       IF NYCKLAR-OK                                                      
026200         IF MFS-UPDATE                                                    
026300           PERFORM G-KOLLA-INPUT                                          
026400           IF INDATA-OK                                                   
026500             PERFORM H-UPPDATERA                                          
026600           END-IF                                                         
026700         ELSE                                                             
026800           IF MFS-FIRST                                                   
026900             PERFORM C-FOERSTA-SIDA                                       
027000           ELSE                                                           
027100             IF MFS-NEXT                                                  
027200               PERFORM D-NAESTA-SIDA                                      
027300             ELSE                                                         
027400               PERFORM E-SAMMA-SIDA                                       
027500             END-IF                                                       
027600           END-IF                                                         
027700         END-IF                                                           
027800         PERFORM F-LAES-VISA-INFO                                         
027900       END-IF                                                             
028000*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
028100*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
028200       COMPUTE MSG-KVLL = LENGTH OF MOD-W1O52601 + 4                      
028300       PERFORM IMS-INSERT-MSG                                             
028400     END-IF                                                               
028500                                                                          
028600     MOVE ZERO TO RETURN-CODE                                             
028700     GOBACK                                                               
028800     .                                                                    
028900     EJECT                                                                
029000 A-INIT SECTION.                                                          
029100                                                                          
029200     IF MSG-DUBBLA-TRANSKODER                                             
029300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I52601                 
029400       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
029500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
029600     ELSE                                                                 
029700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I52601                  
029800       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
029900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
030000     END-IF                                                               
030100                                                                          
030200     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
030300     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
030400     MOVE MFS-IDTRANS TO W-IDTRANS                                        
030500                                                                          
030600     MOVE LOW-VALUE TO MSG-AREA                                           
030700     MOVE 'W1O526N1' TO MFS-IDMOD                                         
030800     MOVE '1526' TO MOD-IDTRANS                                           
030900     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
031000                                                                          
031100     IF EGEN-MID OR HELP-MID                                              
031200       CONTINUE                                                           
031300     ELSE                                                                 
031400       MOVE SPACE TO MFS-KDTRTYP                                          
031500       MOVE '7' TO MFS-IDPFK                                              
031600     END-IF                                                               
031700                                                                          
031800     MOVE FUNCTION CURRENT-DATE    TO WS-CURRENT-DATE                     
031900     .                                                                    
032000     EJECT                                                                
032100 B-KOLLA-NYCKLAR SECTION.                                                 
032200                                                                          
032300     MOVE ALL '+'           TO MSGI-WMSGINIT                              
032400     MOVE '001'             TO MSGI-KDCALL                                
032500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
032600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
032700     MOVE '1526'            TO MSGI-IDTRANS                               
032800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
032900                                                                          
033000     MOVE JA TO NYCKLAR-SW                                                
033100                                                                          
033200*    -- kontroll av idavd-in                                              
033300     MOVE MFS-RENSA-FAELT TO MOD-IDAVD-IN                                 
033400     MOVE '00000' TO WS-IDAVD                                             
033500     IF EGEN-MID                                                          
033600       IF MID-IDAVD-IN NOT = ALL '+'                                      
033700         MOVE MID-IDAVD-IN TO WS-IDAVD                                    
033800       ELSE                                                               
033900         MOVE MID-IDAVD-UT TO WS-IDAVD                                    
034000       END-IF                                                             
034100       INSPECT WS-IDAVD REPLACING LEADING SPACE BY ZERO                   
034200     END-IF                                                               
034300     MOVE WS-IDAVD TO MOD-IDAVD-UT                                        
034400                                                                          
034500     IF WS-IDAVD NOT = WS-KONTROLL-IDAVD                                  
034600       MOVE NEJ TO NYCKLAR-SW                                             
034700     END-IF                                                               
034800                                                                          
034900     IF GODK-MID OR NYCKLAR-OK                                            
035000       IF EGEN-MID                                                        
035100*        --- flytta bläddringsnycklar                                     
035200         MOVE MSGI-SPAR-AREA TO SPAR-AREA                                 
035300       END-IF                                                             
035400     ELSE                                                                 
035500       MOVE MFS-RENSA-FAELT TO MOD-IDAVD-UT                               
035600     END-IF                                                               
035700                                                                          
035800     IF NYCKLAR-FEL                                                       
035900       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
036000       CALL WMEDKONV USING MED-WMEDAREA                                   
036100       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
036200       PERFORM MFS-RENSA-FAELT-IN                                         
036300       PERFORM MFS-RENSA-FAELT-UT                                         
036400     END-IF                                                               
036500     .                                                                    
036600     EJECT                                                                
036700 C-FOERSTA-SIDA SECTION.                                                  
036800     SKIP2                                                                
036900     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
037000     CALL WMEDKONV USING MED-WMEDAREA                                     
037100     MOVE MED-TEMFSINF TO MOD-TEMFSFEL                                    
037200                                                                          
037300     PERFORM MFS-RENSA-FAELT-IN                                           
037400*    --- förbered visning och positionera pcb-pekaren                     
037500     PERFORM XA-LAES-GRUNDDATA                                            
037600     PERFORM IMS-GNP-1219-1221                                            
037700*    --- står nu på första bennr, om segment finns                        
037800     .                                                                    
037900     EJECT                                                                
038000 D-NAESTA-SIDA SECTION.                                                   
038100     SKIP2                                                                
038200     IF SPAR-IDTRANS NOT = '1526'                                         
038300       PERFORM MFS-RENSA-FAELT-IN                                         
038400     END-IF                                                               
038500*    --- förbered visning och positionera pcb-pekaren                     
038600     PERFORM XA-LAES-GRUNDDATA                                            
038700     PERFORM IMS-GNP-1219-1221                                            
038800     IF SEGMENT-FINNS                                                     
038900       IF SPAR-IDBENNR-NEXT > ZERO                                        
039000         PERFORM UNTIL SEGMENT-SAKNAS                                     
039100                 OR 1219-1221-IDBENNR = SPAR-IDBENNR-NEXT                 
039200           PERFORM IMS-GNP-1219-1221                                      
039300         END-PERFORM                                                      
039400*        --- om segment finns står pcb-pekaren nu på nästa bennr          
039500         IF SEGMENT-SAKNAS                                                
039600           PERFORM IMS-GNP-1219-1221-FIRST                                
039700*          --- pcb-pekaren står nu på första bennr istället               
039800         END-IF                                                           
039900       ELSE                                                               
040000           CONTINUE                                                       
040100*          --- här står pekaren på det första bennr:et                    
040200       END-IF                                                             
040300     END-IF                                                               
040400     .                                                                    
040500     EJECT                                                                
040600 E-SAMMA-SIDA SECTION.                                                    
040700     SKIP2                                                                
040800     IF SPAR-IDTRANS = '1526' OR '0551'                                   
040900       IF  MID-IDBENNR-UPD = ALL '+'                                      
041000       AND MID-IDORDER-BEN = ALL '+'                                      
041100       AND MID-IDORDER-LEX = ALL '+'                                      
041200         PERFORM MFS-RENSA-FAELT-IN                                       
041300       ELSE                                                               
041400         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
041500         CALL WMEDKONV USING MED-WMEDAREA                                 
041600         MOVE MED-TEMFSINF TO MOD-TEMFSFEL                                
041700         PERFORM EA-MID-INDATA-TILL-MOD                                   
041800       END-IF                                                             
041900     ELSE                                                                 
042000       PERFORM MFS-RENSA-FAELT-IN                                         
042100     END-IF                                                               
042200*    --- förbered visning och positionera pcb-pekaren                     
042300     IF MID-IDAVD-IN NOT = ALL '+'                                        
042400       MOVE ZERO TO SPAR-IDBENNR-ENTER                                    
042500*      --- läser från början vid nystart                                  
042600     END-IF                                                               
042700     PERFORM XA-LAES-GRUNDDATA                                            
042800     PERFORM IMS-GNP-1219-1221                                            
042900     IF SEGMENT-FINNS                                                     
043000       IF SPAR-IDBENNR-ENTER > ZERO                                       
043100         PERFORM UNTIL SEGMENT-SAKNAS                                     
043200                 OR 1219-1221-IDBENNR = SPAR-IDBENNR-ENTER                
043300           PERFORM IMS-GNP-1219-1221                                      
043400         END-PERFORM                                                      
043500*        --- om segment finns står pcb-pekaren nu på rätt bennr           
043600         IF SEGMENT-SAKNAS                                                
043700           PERFORM IMS-GNP-1219-1221-FIRST                                
043800*          --- pcb-pekaren står nu på första bennr istället               
043900         END-IF                                                           
044000       ELSE                                                               
044100           CONTINUE                                                       
044200*          --- här står pekaren på det första bennr:et                    
044300       END-IF                                                             
044400     END-IF                                                               
044500     .                                                                    
044600     EJECT                                                                
044700 EA-MID-INDATA-TILL-MOD SECTION.                                          
044800     SKIP2                                                                
044900     IF MID-IDAVD-IN    NOT = ALL '+'                                     
045000       MOVE MID-IDAVD-IN          TO MOD-IDAVD-IN                         
045100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDAVD-IN-ATTR                    
045200     ELSE                                                                 
045300       MOVE MFS-RENSA-FAELT       TO MOD-IDAVD-IN                         
045400     END-IF                                                               
045500                                                                          
045600     MOVE MID-IDAVD-UT  TO MOD-IDAVD-UT                                   
045700                                                                          
045800     IF MID-IDBENNR-UPD NOT = ALL '+'                                     
045900       MOVE MID-IDBENNR-UPD       TO MOD-IDBENNR-UPD                      
046000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDBENNR-UPD-ATTR                 
046100     ELSE                                                                 
046200       MOVE MFS-RENSA-FAELT       TO MOD-IDBENNR-UPD                      
046300     END-IF                                                               
046400                                                                          
046500     IF MID-IDORDER-BEN NOT = ALL '+'                                     
046600       MOVE MID-IDORDER-BEN       TO MOD-IDORDER-BEN                      
046700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDORDER-BEN-ATTR                 
046800     ELSE                                                                 
046900       MOVE MFS-RENSA-FAELT       TO MOD-IDORDER-BEN                      
047000     END-IF                                                               
047100                                                                          
047200     IF MID-IDORDER-LEX NOT = ALL '+'                                     
047300       MOVE MID-IDORDER-LEX       TO MOD-IDORDER-LEX                      
047400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDORDER-LEX-ATTR                 
047500     ELSE                                                                 
047600       MOVE MFS-RENSA-FAELT       TO MOD-IDORDER-LEX                      
047700     END-IF                                                               
047800     .                                                                    
047900     EJECT                                                                
048000 F-LAES-VISA-INFO SECTION.                                                
048100     SKIP2                                                                
048200     IF MFS-UPDATE                                                        
048300       PERFORM XA-LAES-GRUNDDATA                                          
048400       PERFORM IMS-GNP-1219-1221                                          
048500     ELSE                                                                 
048600       CONTINUE                                                           
048700*     --- i 1219-1221-arean ligger nu första bennr som skall visas        
048800*     --- om segment-fanns i c- d- eller e- sektionen.                    
048900     END-IF                                                               
049000     MOVE +1 TO INDX                                                      
049100     IF SEGMENT-FINNS                                                     
049200       MOVE 1219-1221-IDBENNR TO SPAR-IDBENNR-ENTER                       
049300     ELSE                                                                 
049400       MOVE ZERO TO SPAR-IDBENNR-ENTER                                    
049500     END-IF                                                               
049600                                                                          
049700     IF MSGI-IDLAND-SPR = 'SE'                                            
049800       MOVE 'S  ' TO W-IDSKYLT                                            
049900     ELSE                                                                 
050000       MOVE 'GB ' TO W-IDSKYLT                                            
050100     END-IF                                                               
050200     PERFORM UNTIL INDX > MAX-INDX                                        
050300       IF SEGMENT-FINNS                                                   
050400*        --- lägg ut läst benämningsnummer                                
050500         MOVE 1219-1221-IDBENNR TO MOD-LIST-IDBENNR (INDX)                
050600                                   W-IDBENNR                              
050700*        --- hämta och lägg ut text                                       
050800         PERFORM IMS-GU-BENA11                                            
050900         IF SEGMENT-FINNS                                                 
051000           MOVE BENA-TEXT-BEART TO MOD-LIST-BEART (INDX)                  
051100         END-IF                                                           
051200*        --- hämta nästa benämningsnummer                                 
051300         PERFORM IMS-GNP-1219-1221                                        
051400       ELSE                                                               
051500*        --- rensa kvarvarande tomma rader                                
051600         MOVE MFS-RENSA-FAELT TO MOD-LIST-IDBENNR (INDX)                  
051700                                 MOD-LIST-BEART (INDX)                    
051800       END-IF                                                             
051900       ADD +1 TO INDX                                                     
052000     END-PERFORM                                                          
052100                                                                          
052200     IF SEGMENT-FINNS                                                     
052300       MOVE 1219-1221-IDBENNR TO SPAR-IDBENNR-NEXT                        
052400       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
052500       CALL WMEDKONV USING MED-WMEDAREA                                   
052600       MOVE MED-TEMFSINF TO MOD-TEMFSINF                                  
052700     ELSE                                                                 
052800       MOVE ZERO                TO SPAR-IDBENNR-NEXT                      
052900       IF MOD-LIST-IDBENNR(1) = MFS-RENSA-FAELT                           
053000*        --- finns inga bennr att visa                                    
053100         MOVE INF-EMPTY-REGISTER  TO MED-IDMFSINF                         
053200       ELSE                                                               
053300         MOVE INF-LAST-PAGE      TO MED-IDMFSINF                          
053400       END-IF                                                             
053500       CALL WMEDKONV USING MED-WMEDAREA                                   
053600       MOVE MED-TEMFSINF TO MOD-TEMFSINF                                  
053700     END-IF                                                               
053800                                                                          
053900     MOVE '002'        TO MSGI-KDCALL                                     
054000     MOVE '1526'     TO SPAR-IDTRANS                                      
054100     MOVE SPAR-AREA    TO MSGI-SPAR-AREA                                  
054200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
054300     .                                                                    
054400     EJECT                                                                
054500 XA-LAES-GRUNDDATA SECTION.                                               
054600     SKIP2                                                                
054700     PERFORM IMS-GHU-1219-1220                                            
054800*    --- segment skall finnas, annars abend                               
054900     IF SEGMENT-SAKNAS                                                    
055000        MOVE 'WL121911 SAKNAS. (WDGX1220 - WDR2) SKALL FINNAS'            
055100                                TO  FELTEXT                               
055200        CALL FELLOG                                                       
055300     ELSE                                                                 
055400       MOVE 1219-1220-IDORDER-VCTS-BEN   TO MOD-IDORDER-BEN               
055500       MOVE 1219-1220-IDORDER-VCTS-LEXIKON TO MOD-IDORDER-LEX             
055600       MOVE 1219-1220-TIUPPDAT-LEXIKON   TO MOD-TIUPPDAT-LEX              
055700     END-IF                                                               
055800     .                                                                    
055900     EJECT                                                                
056000 G-KOLLA-INPUT SECTION.                                                   
056100     SKIP2                                                                
056200     MOVE JA  TO INDATA-SW                                                
056300     IF  MID-IDBENNR-UPD = ALL '+'                                        
056400     AND MID-IDORDER-BEN = ALL '+'                                        
056500     AND MID-IDORDER-LEX = ALL '+'                                        
056600       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
056700       CALL WMEDKONV USING MED-WMEDAREA                                   
056800       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
056900       PERFORM MFS-ROER-EJ-FAELT-IN                                       
057000       PERFORM MFS-ROER-EJ-FAELT-UT                                       
057100       MOVE NEJ TO INDATA-SW                                              
057200     ELSE                                                                 
057300       IF MID-IDBENNR-UPD NOT = ALL '+'                                   
057400         IF MID-IDBENNR-UPD NOT NUMERIC                                   
057500           MOVE MFS-NUM-FAELT-FEL TO MOD-IDBENNR-UPD-ATTR                 
057600           MOVE NEJ TO INDATA-SW                                          
057700         ELSE                                                             
057800           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDBENNR-UPD-ATTR               
057900         END-IF                                                           
058000       END-IF                                                             
058100                                                                          
058200       IF MID-IDORDER-BEN NOT = ALL '+'                                   
058300         IF MID-IDORDER-BEN NOT NUMERIC                                   
058400           MOVE MFS-NUM-FAELT-FEL TO MOD-IDORDER-BEN-ATTR                 
058500           MOVE NEJ TO INDATA-SW                                          
058600         ELSE                                                             
058700           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDORDER-BEN-ATTR               
058800         END-IF                                                           
058900       END-IF                                                             
059000                                                                          
059100       IF MID-IDORDER-LEX NOT = ALL '+'                                   
059200         IF MID-IDORDER-LEX NOT NUMERIC                                   
059300           MOVE MFS-NUM-FAELT-FEL TO MOD-IDORDER-LEX-ATTR                 
059400           MOVE NEJ TO INDATA-SW                                          
059500         ELSE                                                             
059600           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDORDER-LEX-ATTR               
059700         END-IF                                                           
059800       END-IF                                                             
059900                                                                          
060000       IF INDATA-FEL                                                      
060100         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
060200         CALL WMEDKONV USING MED-WMEDAREA                                 
060300         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
060400         PERFORM MFS-ROER-EJ-FAELT-UT                                     
060500         PERFORM MFS-ROER-EJ-FAELT-IN                                     
060600       ELSE                                                               
060700         IF MID-IDBENNR-UPD NOT = ALL '+'                                 
060800           MOVE MID-IDBENNR-UPD TO W-IDBENNR                              
060900           PERFORM IMS-GU-BENA01                                          
061000*          --- läs in databassegment för att kolla inmatningsfält         
061100           IF SEGMENT-FINNS                                               
061200             PERFORM GA-KOLLA-TEXTSEGMENTEN                               
061300             IF GRUNDSPRAK-SAKNAS                                         
061400*              --- benämningen finns inte ens på grundspråken !!          
061500               MOVE NEJ TO INDATA-SW                                      
061600               MOVE MFS-ALFA-FAELT-FEL TO MOD-IDBENNR-UPD-ATTR            
061700               MOVE ERR-NO-SOURCEDATA TO MED-IDMFSFEL                     
061800               CALL WMEDKONV USING MED-WMEDAREA                           
061900               MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                          
062000               PERFORM MFS-RENSA-FAELT-IN                                 
062100               PERFORM MFS-RENSA-FAELT-UT                                 
062200             ELSE                                                         
062300               IF OVERSATTNING-BEHOVS-EJ                                  
062400*                --- benämningen har alla översättningar redan            
062500                 MOVE NEJ TO INDATA-SW                                    
062600                 MOVE MFS-ALFA-FAELT-FEL TO MOD-IDBENNR-UPD-ATTR          
062700                 MOVE INF-ALREADY-TRANSLATED TO MED-IDMFSFEL              
062800                 CALL WMEDKONV USING MED-WMEDAREA                         
062900                 MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                        
063000                 PERFORM MFS-RENSA-FAELT-IN                               
063100                 PERFORM MFS-ROER-EJ-FAELT-UT                             
063200               END-IF                                                     
063300             END-IF                                                       
063400           ELSE                                                           
063500             MOVE NEJ TO INDATA-SW                                        
063600             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDBENNR-UPD-ATTR              
063700             MOVE ERR-NOT-REGISTERED TO MED-IDMFSFEL                      
063800             CALL WMEDKONV USING MED-WMEDAREA                             
063900             MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                            
064000             PERFORM MFS-RENSA-FAELT-IN                                   
064100             PERFORM MFS-RENSA-FAELT-UT                                   
064200           END-IF                                                         
064300         END-IF                                                           
064400       END-IF                                                             
064500     END-IF                                                               
064600     .                                                                    
064700     EJECT                                                                
064800 GA-KOLLA-TEXTSEGMENTEN   SECTION.                                        
064900     SKIP2                                                                
065000*    --- undersök ifall det finns textsegment att översätta               
065100     SET OVERSATTNING-BEHOVS-EJ TO TRUE                                   
065200     SET GRUNDSPRAK-FINNS       TO TRUE                                   
065300     MOVE SPACE TO SPAR-BEART-GB-SV                                       
065400                                                                          
065500     MOVE 'S  '          TO W-IDSKYLT                                     
065600*    --- läser svenskan för att kolla ihop med engelskan                  
065700     PERFORM IMS-GNP-BENA11                                               
065800     IF SEGMENT-FINNS                                                     
065900       MOVE BENA-TEXT-BEART TO SPAR-BEART-SV                              
066000     END-IF                                                               
066100     MOVE 'GB '          TO W-IDSKYLT                                     
066200*    --- läser engelskan för att jämföra övriga fält                      
066300     PERFORM IMS-GNP-BENA11                                               
066400     IF SEGMENT-FINNS                                                     
066500       MOVE BENA-TEXT-BEART TO SPAR-BEART-GB                              
066600     END-IF                                                               
066700                                                                          
066800     MOVE +1  TO  IX                                                      
066900     MOVE WS-IDSKYLT(IX) TO W-IDSKYLT                                     
067000                                                                          
067100     MOVE LAES-FIRST TO CMD-KOD                                           
067200*    --- läser första språk i tabellen                                    
067300     PERFORM IMS-GNP-BENA11                                               
067400                                                                          
067500     PERFORM UNTIL SEGMENT-SAKNAS                                         
067600     OR IX > MAX-IX                                                       
067700     OR OVERSATTNING-BEHOVS                                               
067800     OR GRUNDSPRAK-SAKNAS                                                 
067900       IF  BENA-TEXT-BEART = SPACE                                        
068000       AND BENA-TEXT-BEART = SPAR-BEART-GB                                
068100*         --- om GB finns på övriga språk är det ej översatt              
068200         SET OVERSATTNING-BEHOVS TO TRUE                                  
068300       END-IF                                                             
068400       IF SPAR-BEART-GB-SV = SPACE                                        
068500*        --- benämningen finns inte på något av grundspråken !!           
068600         SET GRUNDSPRAK-SAKNAS TO TRUE                                    
068700       END-IF                                                             
068800       ADD +1 TO IX                                                       
068900       IF IX <= MAX-IX                                                    
069000         MOVE WS-IDSKYLT(IX) TO W-IDSKYLT                                 
069100         MOVE LAES-VANLIGT TO CMD-KOD                                     
069200         PERFORM IMS-GNP-BENA11                                           
069210       END-IF                                                             
069300     END-PERFORM                                                          
069400                                                                          
069500*    IF SEGMENT-SAKNAS                                                    
069600       SET OVERSATTNING-BEHOVS TO TRUE                                    
069700*    END-IF                                                               
069800     .                                                                    
069900     EJECT                                                                
070000 H-UPPDATERA SECTION.                                                     
070100     SKIP3                                                                
070200     PERFORM IMS-GHU-1219-1220                                            
070300     IF SEGMENT-FINNS                                                     
070400       IF MID-IDORDER-BEN NOT = ALL '+'                                   
070500         MOVE MID-IDORDER-BEN TO 1219-1220-IDORDER-VCTS-BEN               
070600                                 MOD-IDORDER-BEN                          
070700         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDORDER-BEN-ATTR               
070800       ELSE                                                               
070900         MOVE MFS-ROER-EJ-FAELT TO MOD-IDORDER-BEN                        
071000       END-IF                                                             
071100                                                                          
071200       IF MID-IDORDER-LEX NOT = ALL '+'                                   
071300         MOVE MID-IDORDER-LEX TO 1219-1220-IDORDER-VCTS-LEXIKON           
071400                                 MOD-IDORDER-LEX                          
071500         MOVE DAGENS-6-DATUM  TO 1219-1220-TIUPPDAT-LEXIKON               
071600         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDORDER-LEX-ATTR               
071700       ELSE                                                               
071800         MOVE MFS-ROER-EJ-FAELT TO MOD-IDORDER-LEX                        
071900       END-IF                                                             
072000                                                                          
072100       PERFORM IMS-REPL-1219-1220                                         
072200       IF MID-IDBENNR-UPD NOT = ALL '+'                                   
072300         MOVE MID-IDBENNR-UPD TO W-IDBENNR                                
072400         PERFORM IMS-GHU-1219-1221                                        
072500         IF SEGMENT-FINNS                                                 
072600*          --- delete IDBENNR från 1219-1221-segment                      
072700           PERFORM IMS-DLET-1219-1221                                     
072800         ELSE                                                             
072900*          --- insert IDBENNR på   1219-1221-segment                      
073000           MOVE MID-IDBENNR-UPD TO 1219-1221-IDBENNR                      
073100*          -- räkna fram nyckeldel datum                                  
073200           COMPUTE WS-TITIREGD-9KOMPL = 999999999 - DAGENS-8-DATUM        
073300           MOVE WS-TITIREGD-9KOMPL TO 1219-1221-TITIREGD-9KOMPL           
073400*          -- räkna fram nyckeldel tid                                    
073500           MOVE DAGENS-HHMMSSTH TO WS-TITIDLOP-HHMMSSTH                   
073600           COMPUTE WS-TITIDLOP-9KOMPL = 999999999 - WS-TITIDLOP-KY        
073700           MOVE WS-TITIDLOP-9KOMPL TO 1219-1221-TITIDLOP-9KOMPL           
073800                                                                          
073900           PERFORM IMS-ISRT-1219-1221                                     
074000         END-IF                                                           
074100       END-IF                                                             
074200                                                                          
074300       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
074400       CALL WMEDKONV USING MED-WMEDAREA                                   
074500       MOVE MED-TEMFSINF TO MOD-TEMFSFEL                                  
074600*      --- Meddelande till felfältet då infofältet blir upptaget          
074700*      --- av läsningsinfo i slutet av F-sektionen.                       
074800       PERFORM MFS-FORM-ATTR                                              
074900       PERFORM MFS-RENSA-FAELT-IN                                         
075000     END-IF                                                               
075100     .                                                                    
075200     EJECT                                                                
075300 MFS-RENSA-FAELT-UT SECTION.                                              
075400                                                                          
075500*    --- alla utdata-fält                                                 
075600*    --- inkl. bläddringsnycklar                                          
075700                                                                          
075800     MOVE MFS-RENSA-FAELT TO MOD-TIUPPDAT-LEX                             
075900     MOVE +1 TO INDX                                                      
076000     PERFORM UNTIL INDX > MAX-INDX                                        
076100*      --- utdata-fält på bläddringsrader                                 
076200       MOVE MFS-RENSA-FAELT TO MOD-LIST-IDBENNR (INDX)                    
076300                               MOD-LIST-BEART  (INDX)                     
076400       ADD +1 TO INDX                                                     
076500     END-PERFORM                                                          
076600     .                                                                    
076700     SKIP3                                                                
076800 MFS-RENSA-FAELT-IN SECTION.                                              
076900                                                                          
077000*    --- alla indata-fält                                                 
077100     MOVE MFS-RENSA-FAELT TO MOD-IDAVD-IN                                 
077200                             MOD-IDBENNR-UPD                              
077300                             MOD-IDORDER-BEN                              
077400                             MOD-IDORDER-LEX                              
077500     .                                                                    
077600     EJECT                                                                
077700 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
077800                                                                          
077900*    --- alla utdata-fält                                                 
078000*    --- inkl bläddringsnycklar och rad-data                              
078100     MOVE MFS-ROER-EJ-FAELT TO MOD-TIUPPDAT-LEX                           
078200     MOVE +1 TO INDX                                                      
078300     PERFORM UNTIL INDX > MAX-INDX                                        
078400*      --- utdata-fält på bläddringsrader                                 
078500       MOVE MFS-ROER-EJ-FAELT TO MOD-LIST-IDBENNR (INDX)                  
078600                                 MOD-LIST-BEART  (INDX)                   
078700       ADD +1 TO INDX                                                     
078800     END-PERFORM                                                          
078900     .                                                                    
079000     SKIP2                                                                
079100 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
079200                                                                          
079300*    --- alla indata-fält                                                 
079400     MOVE MFS-ROER-EJ-FAELT TO MOD-IDAVD-IN                               
079500                               MOD-IDBENNR-UPD                            
079600                               MOD-IDORDER-BEN                            
079700                               MOD-IDORDER-LEX                            
079800     .                                                                    
079900     EJECT                                                                
080000 MFS-FORM-ATTR  SECTION.                                                  
080100                                                                          
080200*    --- alla attribut-fält                                               
080300     MOVE MFS-FORMATETS-ATTR TO MOD-IDAVD-IN-ATTR                         
080400                                MOD-IDBENNR-UPD-ATTR                      
080500                                MOD-IDORDER-BEN-ATTR                      
080600                                MOD-IDORDER-LEX-ATTR                      
080700     .                                                                    
080800     EJECT                                                                
080900* --- ims sektioner ---                                                   
081000     SKIP3                                                                
081100 IMS-GET-MSG SECTION.                                                     
081200                                                                          
081300     MOVE '  QC' TO GODK-STATUSKODER                                      
081400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
081500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
081600     PERFORM IMS-STATUSKONTROLL                                           
081700     .                                                                    
081800     SKIP3                                                                
081900 IMS-INSERT-MSG SECTION.                                                  
082000     IF MSGI-IDLAND-SPR = 'SE'                                            
082100       MOVE '0' TO MFS-KDHUVOMR                                           
082200     END-IF                                                               
082300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
082400     MOVE SPACE TO GODK-STATUSKODER                                       
082500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
082600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
082700     PERFORM IMS-STATUSKONTROLL                                           
082800     .                                                                    
082900     EJECT                                                                
083000 IMS-GHU-1219-1220 SECTION.                                               
083100     STRING 'WL121901(WDGXKEY  =' W-KY1219-X ')'                          
083200          DELIMITED BY SIZE INTO SSA1                                     
083300     STRING 'WL121911(KDSEGKEY =' W-KDSEGKEY-X ')'                        
083400          DELIMITED BY SIZE INTO SSA2                                     
083500     MOVE '  GE' TO GODK-STATUSKODER                                      
083600     CALL CBLTDLI USING GHU 1219-PCB DLI-IO-WL121911 SSA1 SSA2            
083700     MOVE 1219-STATUS-CODE TO STATUS-WS                                   
083800     PERFORM IMS-STATUSKONTROLL                                           
083900     .                                                                    
084000     SKIP3                                                                
084100 IMS-REPL-1219-1220 SECTION.                                              
084200     MOVE '  ' TO GODK-STATUSKODER                                        
084300     CALL CBLTDLI USING REPL 1219-PCB DLI-IO-WL121911                     
084400     MOVE 1219-STATUS-CODE TO STATUS-WS                                   
084500     PERFORM IMS-STATUSKONTROLL                                           
084600     .                                                                    
084700     SKIP3                                                                
084800 IMS-GNP-1219-1221 SECTION.                                               
084900     MOVE   'WL121921 '  TO SSA1                                          
085000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
085100     CALL CBLTDLI USING GNP 1219-PCB DLI-IO-WL121921 SSA1                 
085200     MOVE 1219-STATUS-CODE TO STATUS-WS                                   
085300     PERFORM IMS-STATUSKONTROLL                                           
085400     .                                                                    
085500     SKIP3                                                                
085600 IMS-GNP-1219-1221-FIRST SECTION.                                         
085700     MOVE   'WL121921*F'  TO SSA1                                         
085800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
085900     CALL CBLTDLI USING GNP 1219-PCB DLI-IO-WL121921 SSA1                 
086000     MOVE 1219-STATUS-CODE TO STATUS-WS                                   
086100     PERFORM IMS-STATUSKONTROLL                                           
086200     .                                                                    
086300     SKIP3                                                                
086400 IMS-GHU-1219-1221 SECTION.                                               
086500     STRING 'WL121901(WDGXKEY  =' W-KY1219-X ')'                          
086600          DELIMITED BY SIZE INTO SSA1                                     
086700     STRING 'WL121911(KDSEGKEY =' W-KDSEGKEY-X ')'                        
086800          DELIMITED BY SIZE INTO SSA2                                     
086900     STRING 'WL121921(IDBENNR  =' W-IDBENNR-X ')'                         
087000          DELIMITED BY SIZE INTO SSA3                                     
087100     MOVE '  GE' TO GODK-STATUSKODER                                      
087200     CALL CBLTDLI USING GHU  1219-PCB DLI-IO-WL121921                     
087300                             SSA1  SSA2  SSA3                             
087400     MOVE 1219-STATUS-CODE TO STATUS-WS                                   
087500     PERFORM IMS-STATUSKONTROLL                                           
087600     .                                                                    
087700     SKIP3                                                                
087800 IMS-ISRT-1219-1221 SECTION.                                              
087900     STRING 'WL121901(WDGXKEY  =' W-KY1219-X ')'                          
088000          DELIMITED BY SIZE INTO SSA1                                     
088100     STRING 'WL121911(KDSEGKEY =' W-KDSEGKEY-X ')'                        
088200          DELIMITED BY SIZE INTO SSA2                                     
088300     MOVE 'WL121921 ' TO SSA3                                             
088400     MOVE '  ' TO GODK-STATUSKODER                                        
088500     CALL CBLTDLI USING ISRT 1219-PCB                                     
088600                             DLI-IO-WL121921                              
088700                             SSA1 SSA2 SSA3                               
088800     MOVE 1219-STATUS-CODE TO STATUS-WS                                   
088900     PERFORM IMS-STATUSKONTROLL                                           
089000     .                                                                    
089100     SKIP3                                                                
089200 IMS-DLET-1219-1221 SECTION.                                              
089300                                                                          
089400     MOVE '  ' TO GODK-STATUSKODER                                        
089500     CALL CBLTDLI USING DLET 1219-PCB DLI-IO-WL121921                     
089600     MOVE 1219-STATUS-CODE TO STATUS-WS                                   
089700     PERFORM IMS-STATUSKONTROLL                                           
089800     .                                                                    
089900     EJECT                                                                
090000 IMS-GU-BENA01 SECTION.                                                   
090100     STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-X ')'                         
090200          DELIMITED BY SIZE INTO SSA1                                     
090300     MOVE '  GE' TO GODK-STATUSKODER                                      
090400     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WLBENA01 SSA1                  
090500     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
090600     PERFORM IMS-STATUSKONTROLL                                           
090700     .                                                                    
090800     EJECT                                                                
090900 IMS-GNP-BENA11 SECTION.                                                  
091000     STRING 'WLBENA11*' CMD-KOD '(IDSKYLT  =' W-IDSKYLT-X ')'             
091100          DELIMITED BY SIZE INTO SSA1                                     
091200     MOVE '  GE' TO GODK-STATUSKODER                                      
091300     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-WLBENA11 SSA1                 
091400     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
091500     PERFORM IMS-STATUSKONTROLL                                           
091600     .                                                                    
091700     EJECT                                                                
091800 IMS-GU-BENA11 SECTION.                                                   
091900     STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-X ')'                         
092000          DELIMITED BY SIZE INTO SSA1                                     
092100     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
092200          DELIMITED BY SIZE INTO SSA2                                     
092300     MOVE '  GE' TO GODK-STATUSKODER                                      
092400     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WLBENA11 SSA1 SSA2             
092500     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
092600     PERFORM IMS-STATUSKONTROLL                                           
092700     .                                                                    
092800     EJECT                                                                
092900 IMS-STATUSKONTROLL SECTION.                                              
093000     SET STATUS-IX TO 1                                                   
093100     SEARCH GODK-STATUS                                                   
093200       AT END                                                             
093300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
093400         DELIMITED BY SIZE INTO FELTEXT                                   
093500         CALL FELLOG                                                      
093600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
093700         CONTINUE                                                         
093800     END-SEARCH                                                           
093900     .                                                                    
