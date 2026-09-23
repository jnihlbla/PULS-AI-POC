000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4032600.                                                
000300 AUTHOR.         SUSANNE OLSSON.                                          
000400 DATE-WRITTEN.   98/06/24.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        FRÅGE- OCH UPPDATERINGSBILD.                                     
000900*        VISAR RETAILERNR OCH TILL VILKET DC-TEAM DELIVERY NOTE           
001000*        SKALL STYRAS.MAN KAN LÄGGA TILL/TA BORT RETAILER SAMT            
001100*        BYTA TEAM PÅ BEFINTLIG RETAILER.                                 
001200*                                                                         
001300*        PROGRAMMET LÄSER      WLGMTA (WDB2)                              
001310*        PROGRAMMET LÄSER      WDB6                                       
001400*        PROGRAMMET UPPDATERAR WL4459 (WDGX) FYS.BAS WDR1                 
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W4T326                                              
001800*        MID:         W4I32601                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        MOD:         W4O32601                                            
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900*    -- CHECKED BY WY2000                                                 
003000 77  IDPGM                       PIC X(08)   VALUE 'W4032600'.            
003100                                                                          
003200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003400                                                                          
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003700 77  WS-KDCMD                    PIC X       VALUE SPACE.                 
003800                                                                          
003900 77  PRINTER-SW                  PIC X       VALUE 'J'.                   
004000     88  PRINTER-FINNS                       VALUE 'J'.                   
004100     88  PRINTER-SAKNAS                      VALUE 'N'.                   
004200                                                                          
004300                                                                          
004400*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004500 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004600 77  MAX-INDX                    PIC S9(4)  VALUE +39   COMP SYNC.        
004700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004800                                                                          
004900 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005000     88  INDATA-OK                           VALUE 'J'.                   
005100     88  INDATA-FEL                          VALUE 'N'.                   
005200                                                                          
005300 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005400     88  NYCKLAR-OK                          VALUE 'J'.                   
005500     88  NYCKLAR-FEL                         VALUE 'N'.                   
005600                                                                          
005700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005800     88  EGEN-MID                            VALUE '4326'.                
005900     88  GODK-MID                            VALUE '4321' '4322'          
006000                                                   '4323' '4324'          
006100                                                   '4325' '4326'          
006200                                                   '4327' '4328'          
006300                                                   '4329'.                
006400     88  HELP-MID                            VALUE '0551'.                
006500     EJECT                                                                
006600                                                                          
006610 77  WS-REDIGERA-IDPRT           PIC X(3)    VALUE SPACE.                 
006700 01  WS-IDPRTLST.                                                         
006800     03 WS-NR                    PIC X(1)    VALUE '4'.                   
006900     03 WS-LISTTYP               PIC X(2)    VALUE 'DN'.                  
007100     03 WS-IDDC-PR               PIC X(2).                                
007110     03 WS-IDPRT                 PIC X(3).                                
007200                                                                          
007300     EJECT                                                                
007400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007500 01  GENERELLA-SUBPROGRAM.                                                
007600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008000     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
008100     EJECT                                                                
008200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008300*01 -COPY WMEDAREA                                                        
008400     EJECT                                                                
008500*    --- PARAMETRAR TILL SUBPROGRAM W006PRT                               
008600*   -COPY W006PRT                                                         
008700     EJECT                                                                
008800     SKIP3                                                                
008900 01  MESSAGE-CODES.                                                       
009000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009100     03  INF-KONFLIKT            PIC X(3)    VALUE '002'.                 
009110     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
009200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
009300     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
009400     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
009500     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
009600     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
009610     03  INF-KUND-FINNS-REDAN    PIC X(3)    VALUE '070'.                 
009700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009800     03  ERR-KEY-MISSING         PIC X(3)    VALUE '005'.                 
009900     03  ERR-WRONG-PRINTER       PIC X(3)    VALUE '772'.                 
010000     EJECT                                                                
010100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
010200*                                                                         
010300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
010400     SKIP3                                                                
010500*01 -COPY WMSGINIT                                                        
010600     EJECT                                                                
010700*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
010800*                                                                         
010900 01  SPAR-AREA.                                                           
011000     03  SPAR-IDTRANS            PIC X(4)   VALUE '4326'.                 
011100     03  SPAR-IDDISTR-ENTER      PIC S9(4)  VALUE ZERO COMP-3.            
011200     03  SPAR-IDDISTR-NEXT       PIC S9(4)  VALUE ZERO COMP-3.            
011300     03  SPAR-IDKUNDNR-ENTER     PIC S9(6)  VALUE ZERO COMP-3.            
011400     03  SPAR-IDKUNDNR-NEXT      PIC S9(6)  VALUE ZERO COMP-3.            
011500     EJECT                                                                
011600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
011700*                                                                         
011800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011900     SKIP3                                                                
012000*01  MID -COPY W4I32601                                                   
012100     EJECT                                                                
012200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012300     SKIP3                                                                
012400*01  -COPY WMSGAREA                                                       
012500     EJECT                                                                
012600     03  MOD REDEFINES MSG-AREA.                                          
012700*      05  -COPY W4O32601                                                 
012800     EJECT                                                                
012900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013000     SKIP3                                                                
013100*01  -COPY WMFSAREA                                                       
013200     EJECT                                                                
013300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013400*                                                                         
013500     EJECT                                                                
013600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013700     SKIP3                                                                
013800 01  NYCKLAR-TILL-DLI.                                                    
013900*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
014200                                                                          
014600     03  W-IDGMT-X.                                                       
014700         05  W-GMT-IDDISTR        PIC S9(5)   VALUE ZERO COMP-3.          
014800         05  W-GMT-IDKUNDNR       PIC S9(7)   VALUE ZERO COMP-3.          
014900                                                                          
015000     03  W-WDGX4459-X.                                                    
015100         05  W-4459-IDHTYP       PIC X(4)     VALUE '4459'.               
015200         05  W-4459-IDDC         PIC X(2).                                
015300         05  W-4459-LOW-VALUE    PIC X(24)    VALUE LOW-VALUE.            
015400                                                                          
015500     03  W-WDGX4460-X.                                                    
015600         05  W-4460-IDDISTR      PIC S9(5)    VALUE ZERO COMP-3.          
015700         05  W-4460-IDKUNDNR     PIC S9(7)    VALUE ZERO COMP-3.          
015800                                                                          
015900     03  W-WDGX4460-MIN-X.                                                
016000         05  W-4460-IDDISTR-MIN  PIC S9(5)    VALUE ZERO COMP-3.          
016100         05  W-4460-IDKUNDNR-MIN PIC S9(7)    VALUE ZERO COMP-3.          
016200                                                                          
016300     03  W-WDGX4460-MAX-X.                                                
016400         05  W-4460-IDDISTR-MAX  PIC S9(5)    VALUE ZERO COMP-3.          
016500         05  W-4460-IDKUNDNR-MAX PIC S9(7)    VALUE ZERO COMP-3.          
016510                                                                          
016520     03  W-IDDC-B6-X.                                                     
016530         05 W-IDDC-B6                  PIC X(2).                          
016540                                                                          
016600     SKIP2                                                                
016700*    --- STATUS-KOD FRÅN IMS                                              
016800 01  STATUS-WS                   PIC XX.                                  
016900     88  SEGMENT-FINNS                       VALUE '  '.                  
017000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017200     SKIP2                                                                
017300 01  GODK-STATUSKODER.                                                    
017400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017500     SKIP3                                                                
017600 01  SSA1                        PIC X(64).                               
017700 01  SSA2                        PIC X(64).                               
017800     EJECT                                                                
017900*    --- IMS FUNKTIONSKODER                                               
018000*01  -COPY W0003                                                          
018100     EJECT                                                                
018200*    ---  DLI INPUT-OUTPUT AREA                                           
018300                                                                          
018400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLGMTA01'.                    
018500 01  DLI-IO-WLGMTA01.                                                     
018600*    03  -COPY WDB201                                                     
018700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WL445901'.                    
018800 01  DLI-IO-WL445901.                                                     
018900*    03  -COPY WDGX4459                                                   
019000     EJECT                                                                
019100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WL445911'.                    
019200 01  DLI-IO-WL445911.                                                     
019300*    03  -COPY WDGX4460                                                   
019310                                                                          
019320 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
019330 01   DLI-IO-AREA-B601.                                                   
019340*     03  -COPY WDB601                                                    
019350                                                                          
019400     EJECT                                                                
019500 LINKAGE SECTION.                                                         
019600*01  -COPY W0009   -PRE MSG-                                              
019700*01  -COPY W0008   -PRE USEA-                                             
019800     05  FILLER                  PIC X.                                   
019900                                                                          
020000*01  -COPY W0008  -PRE GMTA-                                              
020100     05  FILLER                  PIC X.                                   
020200                                                                          
020300*01  -COPY W0008  -PRE 4459-                                              
020400     05  FILLER                  PIC X.                                   
020410                                                                          
020420*01  -COPY W0008  -PRE WDB6-                                              
020430     05  FILLER                  PIC X.                                   
020500     EJECT                                                                
020600 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB GMTA-PCB 4459-PCB             
020610                           WDB6-PCB.                                      
020700 MAIN SECTION.                                                            
020800     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB GMTA-PCB 4459-PCB             
020810                           WDB6-PCB.                                      
020900                                                                          
021000     PERFORM IMS-GET-MSG                                                  
021100     IF SEGMENT-FINNS                                                     
021200       PERFORM A-INIT                                                     
021300       PERFORM B-KOLLA-NYCKLAR                                            
021400       IF NYCKLAR-OK                                                      
021500         IF MFS-UPDATE                                                    
021600           PERFORM G-KOLLA-INPUT                                          
021700           IF INDATA-OK                                                   
021800             PERFORM H-UPPDATERA                                          
021900           END-IF                                                         
022000         ELSE                                                             
022100           IF MFS-FIRST                                                   
022200             PERFORM C-FOERSTA-SIDA                                       
022300           ELSE                                                           
022400             IF MFS-NEXT                                                  
022500               PERFORM D-NAESTA-SIDA                                      
022600             ELSE                                                         
022700               PERFORM E-SAMMA-SIDA                                       
022800             END-IF                                                       
022900           END-IF                                                         
023000         END-IF                                                           
023010           IF INDATA-OK                                                   
023100             PERFORM F-LAES-VISA-INFO                                     
023200           END-IF                                                         
023210       END-IF                                                             
023300       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O32601 + 4                      
023400       PERFORM IMS-INSERT-MSG                                             
023500     END-IF                                                               
023600                                                                          
023700     MOVE ZERO TO RETURN-CODE                                             
023800     GOBACK                                                               
023900     .                                                                    
024000     EJECT                                                                
024100 A-INIT SECTION.                                                          
024200                                                                          
024300     IF MSG-DUBBLA-TRANSKODER                                             
024400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I32601                 
024500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
024600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
024700     ELSE                                                                 
024800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I32601                  
024900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
025000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
025100     END-IF                                                               
025200                                                                          
025300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
025400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
025500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
025600                                                                          
025700     MOVE LOW-VALUE TO MSG-AREA                                           
025800     MOVE 'W4O326N1' TO MFS-IDMOD                                         
025900     MOVE '4326' TO MOD-IDTRANS                                           
026000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
026100                                                                          
026200     MOVE 9999999         TO W-4460-IDKUNDNR-MAX                          
026300                                                                          
026400     IF EGEN-MID OR HELP-MID                                              
026500       CONTINUE                                                           
026600     ELSE                                                                 
026700       MOVE SPACE TO MFS-KDTRTYP                                          
026800       MOVE '7' TO MFS-IDPFK                                              
026900     END-IF                                                               
027000     .                                                                    
027100     EJECT                                                                
027200 B-KOLLA-NYCKLAR SECTION.                                                 
027300                                                                          
027400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
027500     MOVE '001'             TO MSGI-KDCALL                                
027600     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
027700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
027800     MOVE '4326'            TO MSGI-IDTRANS                               
027900     IF EGEN-MID                                                          
028000       MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                            
028100       MOVE MID-IDDC-IN        TO MSGI-IDDC-KEY                           
028200     END-IF                                                               
028300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
028400     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
028500                                                                          
028600*    MOVE 'GB'              TO MED-IDSKYLT                                
028700     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
028800     MOVE '2'               TO MFS-KDMFSFOR                               
028900                                                                          
029000     MOVE JA TO NYCKLAR-SW                                                
029100                                                                          
029200                                                                          
029300*    -- KONTROLL AV IDDISTR                                               
029400     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
029500                                                                          
029600     IF MID-IDDISTR-IN NOT = ALL '+'                                      
029700       MOVE '7'         TO MFS-IDPFK                                      
029800       MOVE SPACE       TO MFS-KDTRTYP                                    
029900     END-IF                                                               
030000     INSPECT MSGI-IDDISTR REPLACING LEADING SPACE BY ZERO                 
030100     IF MSGI-IDDISTR NUMERIC                                              
030200       MOVE MSGI-IDDISTR TO W-GMT-IDDISTR                                 
030300                            W-4460-IDDISTR-MIN                            
030400                            W-4460-IDDISTR-MAX                            
030500                            W-4460-IDDISTR                                
030600     ELSE                                                                 
030700       MOVE NEJ TO NYCKLAR-SW                                             
030800     END-IF                                                               
030900                                                                          
031000*    -- KONTROLL AV IDDC                                                  
031100     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
031200                                                                          
031300     IF MID-IDDC-IN NOT = ALL '+'                                         
031400       MOVE '7'         TO MFS-IDPFK                                      
031500       MOVE SPACE       TO MFS-KDTRTYP                                    
031600     END-IF                                                               
031700*    INSPECT MSGI-IDDC-KEY REPLACING LEADING SPACE BY ZERO                
031710     MOVE MSGI-IDDC-KEY      TO W-IDDC-B6                                 
031720     PERFORM IMS-GU-WDB601                                                
031800     IF DCS-KDDC = SPACE OR DCS-DDC                                       
031810       MOVE NEJ TO NYCKLAR-SW                                             
032000     ELSE                                                                 
032100       MOVE MSGI-IDDC-KEY TO W-4459-IDDC                                  
032200     END-IF                                                               
032300                                                                          
032400     IF GODK-MID OR NYCKLAR-OK                                            
032500       MOVE MSGI-IDDISTR     TO MOD-IDDISTR-UT                            
032600       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
032700       MOVE MSGI-IDDC-KEY    TO MOD-IDDC-UT                               
032800     ELSE                                                                 
032900       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                             
033000                               MOD-IDDC-UT                                
033100     END-IF                                                               
033200                                                                          
033300     IF NYCKLAR-FEL                                                       
033400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
033500       CALL WMEDKONV USING MED-WMEDAREA                                   
033600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
033700       PERFORM MFS-RENSA-FAELT-IN                                         
033800       PERFORM MFS-RENSA-FAELT-UT                                         
033900     END-IF                                                               
034000     .                                                                    
034100     EJECT                                                                
034200 C-FOERSTA-SIDA SECTION.                                                  
034300                                                                          
034400     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
034500     CALL WMEDKONV USING MED-WMEDAREA                                     
034600     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
034700                                                                          
034800     PERFORM MFS-RENSA-FAELT-IN                                           
034900     .                                                                    
035000     EJECT                                                                
035100 D-NAESTA-SIDA SECTION.                                                   
035200                                                                          
035300     IF SPAR-IDTRANS = '4326'                                             
035400       MOVE SPAR-IDDISTR-NEXT TO W-4460-IDDISTR-MIN                       
035500                                 W-4460-IDDISTR-MAX                       
035600       MOVE SPAR-IDKUNDNR-NEXT TO W-4460-IDKUNDNR-MIN                     
035610       PERFORM MFS-RENSA-FAELT-IN                                         
035700     ELSE                                                                 
035800       PERFORM MFS-RENSA-FAELT-IN                                         
035900     END-IF                                                               
036000     .                                                                    
036100     EJECT                                                                
036200 E-SAMMA-SIDA SECTION.                                                    
036300                                                                          
036400     IF SPAR-IDTRANS = '4326' OR '0551'                                   
036500       MOVE SPAR-IDDISTR-ENTER TO W-4460-IDDISTR-MIN                      
036600                                  W-4460-IDDISTR-MAX                      
036700       MOVE SPAR-IDKUNDNR-ENTER TO W-4460-IDKUNDNR-MIN                    
036800*    ELSE                                                                 
036900*      PERFORM MFS-RENSA-FAELT-IN                                         
037000     END-IF                                                               
037100                                                                          
037210     IF MID-INPUT = SPACE   AND  MID-IDKUNDNR-IN = ALL '+'                
037300       AND  MID-IDPRT = ALL '+'                                           
037400       PERFORM MFS-RENSA-FAELT-IN                                         
037500     ELSE                                                                 
037600       MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                
037700       CALL WMEDKONV USING MED-WMEDAREA                                   
037800       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
037900       PERFORM EA-MID-INDATA-TILL-MOD                                     
038000     END-IF                                                               
038100     .                                                                    
038200     EJECT                                                                
038300 EA-MID-INDATA-TILL-MOD SECTION.                                          
038400                                                                          
039000     MOVE +1 TO INDX                                                      
039100     PERFORM UNTIL INDX > MAX-INDX                                        
039210       IF MID-KDCMD (INDX)  =  SPACE                                      
039300         MOVE SPACE TO MOD-KDCMD (INDX)                                   
039400       ELSE                                                               
039700         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-ATTR (INDX)              
039800         MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD (INDX)                       
040600       END-IF                                                             
040700       ADD +1 TO INDX                                                     
040800     END-PERFORM                                                          
040900                                                                          
041000     IF MID-IDKUNDNR-IN = ALL '+'                                         
041100       MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                            
041200     ELSE                                                                 
041300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKUNDNR-IN-ATTR                 
041400       MOVE MFS-ROER-EJ-FAELT TO MOD-IDKUNDNR-IN                          
041500     END-IF                                                               
041600                                                                          
041700     IF MID-IDPRT = ALL '+'                                               
041800       MOVE MFS-RENSA-FAELT TO MOD-IDPRT-IN                               
041900     ELSE                                                                 
042000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPRT-IN-ATTR                    
042100       MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRT-IN                             
042200     END-IF                                                               
042300     .                                                                    
042400     EJECT                                                                
042500 F-LAES-VISA-INFO SECTION.                                                
042600                                                                          
042700     PERFORM IMS-GU-4459-ROT                                              
042800                                                                          
042900     IF SEGMENT-SAKNAS                                                    
043000       MOVE ERR-KEY-MISSING TO MED-IDMFSFEL                               
043100       CALL WMEDKONV USING MED-WMEDAREA                                   
043200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
043400       PERFORM MFS-RENSA-FAELT-UT                                         
043500     ELSE                                                                 
043600       PERFORM IMS-GNP-4459-BARN                                          
043700                                                                          
043800       IF SEGMENT-SAKNAS                                                  
043900         IF MED-IDMFSFEL = '003' OR '011' OR '001' OR '101'               
044100           CONTINUE                                                       
044200         ELSE                                                             
044300          MOVE ERR-KEY-MISSING TO MED-IDMFSFEL                            
044400          CALL WMEDKONV USING MED-WMEDAREA                                
044500          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
044600          PERFORM MFS-RENSA-FAELT-UT                                      
044700          MOVE W-4460-IDDISTR-MIN  TO SPAR-IDDISTR-ENTER                  
044800          MOVE W-4460-IDKUNDNR-MIN TO SPAR-IDKUNDNR-ENTER                 
044900         END-IF                                                           
045000       ELSE                                                               
045100         MOVE 4460-IDDISTR  TO SPAR-IDDISTR-ENTER                         
045200         MOVE 4460-IDKUNDNR TO SPAR-IDKUNDNR-ENTER                        
045300         PERFORM  FA-VISA-RADER                                           
045400       END-IF                                                             
045500     END-IF                                                               
045600     .                                                                    
045700     EJECT                                                                
045800 FA-VISA-RADER SECTION.                                                   
045900                                                                          
046600     MOVE +1 TO INDX                                                      
046700     PERFORM UNTIL INDX > MAX-INDX                                        
046800       IF SEGMENT-FINNS                                                   
046900         MOVE 4460-IDKUNDNR        TO MOD-IDKUNDNR (INDX)                 
047000         MOVE 4460-IDPRTLST (6:3)  TO WS-REDIGERA-IDPRT                   
047002         INSPECT WS-REDIGERA-IDPRT REPLACING LEADING ZERO BY SPACE        
047010         MOVE WS-REDIGERA-IDPRT    TO MOD-IDPRT (INDX)                    
047100       ELSE                                                               
047200         MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR (INDX)                      
047300                                 MOD-IDPRT    (INDX)                      
047400       END-IF                                                             
047500       PERFORM IMS-GNP-4459-BARN                                          
047600       ADD +1 TO INDX                                                     
047700     END-PERFORM                                                          
047800                                                                          
047900**** OM DET FINNS ETT 14:E SEGMENT.                                       
048000                                                                          
048100     IF SEGMENT-FINNS                                                     
048200       MOVE 4460-IDDISTR   TO SPAR-IDDISTR-NEXT                           
048300       MOVE 4460-IDKUNDNR  TO SPAR-IDKUNDNR-NEXT                          
048310       IF MED-IDMFSINF = '003' OR '101' OR '772' OR '002'                 
048320         CONTINUE                                                         
048330       ELSE                                                               
048400         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
048410       END-IF                                                             
048500     ELSE                                                                 
048600       MOVE 4460-IDDISTR   TO SPAR-IDDISTR-NEXT                           
048700       MOVE 4460-IDKUNDNR  TO SPAR-IDKUNDNR-NEXT                          
048710       IF MED-IDMFSINF = '003' OR '101' OR '772'                          
048730         CONTINUE                                                         
048740       ELSE                                                               
048750         MOVE INF-LAST-PAGE  TO MED-IDMFSINF                              
048901       END-IF                                                             
048910     END-IF                                                               
049000     CALL WMEDKONV USING MED-WMEDAREA                                     
049100     MOVE MED-MFSINF     TO MOD-TEMFSINF                                  
049200                                                                          
049300     MOVE '002'      TO MSGI-KDCALL                                       
049400     MOVE '4326'     TO SPAR-IDTRANS                                      
049500     MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                    
049600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
049700                                                                          
049800     .                                                                    
049900     EJECT                                                                
050000 G-KOLLA-INPUT SECTION.                                                   
050100                                                                          
050200     MOVE SPACE TO WS-KDCMD                                               
050210     MOVE JA    TO INDATA-SW                                              
050310     IF MID-INPUT = SPACE   AND MID-IDKUNDNR-IN = ALL '+'                 
050400                            AND MID-IDPRT = ALL '+'                       
050500       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
050600       CALL WMEDKONV USING MED-WMEDAREA                                   
050700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
050800       PERFORM MFS-ROER-EJ-FAELT-IN                                       
050900       PERFORM MFS-ROER-EJ-FAELT-UT                                       
051000       MOVE NEJ TO INDATA-SW                                              
051100     ELSE                                                                 
051200                                                                          
051300******* FÖR INMATN.FÄLT : MID-KDCMD (INDX) ********************           
051400                                                                          
051500       MOVE +1 TO INDX                                                    
051600       PERFORM UNTIL INDX > MAX-INDX                                      
051800         IF MID-KDCMD (INDX)  = SPACE                                     
052100           MOVE SPACE TO MOD-KDCMD (INDX)                                 
052200         ELSE                                                             
052300           IF MID-KDCMD (INDX) = 'R' OR 'D'                               
052400             IF WS-KDCMD = SPACE                                          
052500               MOVE MID-KDCMD (INDX) TO WS-KDCMD                          
052600             END-IF                                                       
052700                                                                          
052800             IF WS-KDCMD = MID-KDCMD (INDX)                               
052900               MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR (INDX)         
052910             ELSE                                                         
052920               MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-ATTR (INDX)         
052930               MOVE MFS-ROER-EJ-FAELT    TO MOD-KDCMD (INDX)              
052940               MOVE NEJ TO INDATA-SW                                      
052950             END-IF                                                       
053000                                                                          
053100             IF MID-KDCMD (INDX) = 'R' AND                                
053200               ( MID-IDPRT NOT = ALL '+' ) AND                            
053300               ( MID-IDKUNDNR-IN = ALL '+' )                              
053400               MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR (INDX)         
053600             ELSE                                                         
053610               IF MID-KDCMD (INDX) = 'D' AND                              
053611                 MID-IDPRT = ALL '+' AND                                  
053612                 MID-IDKUNDNR-IN = ALL '+'                                
053620                 MOVE MFS-ALFA-FAELT-RAETT TO                             
053621                                    MOD-KDCMD-ATTR (INDX)                 
053630               ELSE                                                       
053640                 MOVE INF-KONFLIKT TO MED-IDMFSINF                        
053650                 CALL WMEDKONV USING MED-WMEDAREA                         
053660                 MOVE MED-MFSINF TO MOD-TEMFSINF                          
053700                 MOVE MFS-ALFA-FAELT-FEL   TO                             
053710                                    MOD-KDCMD-ATTR (INDX)                 
053800                 MOVE MFS-ROER-EJ-FAELT    TO MOD-KDCMD (INDX)            
053900                 MOVE NEJ TO INDATA-SW                                    
054000               END-IF                                                     
054100             END-IF                                                       
054600           ELSE                                                           
054700             MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDCMD-ATTR (INDX)            
054800             MOVE MFS-ROER-EJ-FAELT   TO MOD-KDCMD (INDX)                 
054900             MOVE NEJ TO INDATA-SW                                        
055000           END-IF                                                         
055100         END-IF                                                           
055200         ADD +1 TO INDX                                                   
055300       END-PERFORM                                                        
055400                                                                          
055500******* FÖR INMATN.FÄLT : MID-IDKUNDNR-IN ************************        
055600                                                                          
055700       IF MID-IDKUNDNR-IN NOT = ALL '+' AND MID-IDPRT = ALL '+'           
055800           MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-IN-ATTR                 
055900           MOVE NEJ TO INDATA-SW                                          
056000       ELSE                                                               
056100         IF MID-IDKUNDNR-IN NOT = ALL '+'                                 
056200           IF MID-IDKUNDNR-IN NOT NUMERIC                                 
056300             MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-IN-ATTR               
056400             MOVE NEJ TO INDATA-SW                                        
056500           ELSE                                                           
056600             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-IN-ATTR             
056700           END-IF                                                         
056800         END-IF                                                           
056900       END-IF                                                             
057000                                                                          
057100******* FÖR INMATN.FÄLT : MID-IDPRT ***************************           
057200                                                                          
057300       IF MID-IDPRT NOT = ALL '+'                                         
057400         MOVE MID-IDPRT     TO WS-IDPRT                                   
057500         MOVE MSGI-IDDC-KEY TO WS-IDDC-PR                                 
057600                                                                          
057700         PERFORM S02-KOLLA-PRINTER-FINNS                                  
057800         IF PRINTER-SAKNAS                                                
057900           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPRT-IN-ATTR                   
058000           MOVE ERR-WRONG-PRINTER  TO MED-IDMFSFEL                        
058010           CALL WMEDKONV USING MED-WMEDAREA                               
058020           MOVE MED-MFSINF TO MOD-TEMFSINF                                
058100           MOVE NEJ TO INDATA-SW                                          
058200         ELSE                                                             
058210           IF MID-INPUT = SPACE AND MID-IDKUNDNR-IN = ALL '+'             
058220             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPRT-IN-ATTR                 
058230             CALL WMEDKONV USING MED-WMEDAREA                             
058240             MOVE MED-MFSINF TO MOD-TEMFSINF                              
058250             MOVE NEJ TO INDATA-SW                                        
058260           ELSE                                                           
058300             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPRT-IN-ATTR               
058400           END-IF                                                         
058410         END-IF                                                           
058500       END-IF                                                             
058600                                                                          
058700       IF INDATA-FEL                                                      
059100*        MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
059200         CALL WMEDKONV USING MED-WMEDAREA                                 
059300         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
059400         PERFORM MFS-ROER-EJ-FAELT-UT                                     
059500         PERFORM MFS-ROER-EJ-FAELT-IN                                     
059700       ELSE                                                               
059800*LÄS IN DATABASSEGMENT FÖR ATT KOLLA INMATNINGSFÄLT                       
059900                                                                          
060000         IF MID-IDKUNDNR-IN NOT = ALL '+'                                 
060100           MOVE MID-IDKUNDNR-IN TO W-GMT-IDKUNDNR                         
060200           PERFORM IMS-GET-GMTA-KUND                                      
060300           IF SEGMENT-FINNS                                               
060400             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDKUNDNR-IN-ATTR            
060500           ELSE                                                           
060600             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDKUNDNR-IN-ATTR              
060700             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
060800             CALL WMEDKONV USING MED-WMEDAREA                             
060900             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
061000             PERFORM MFS-ROER-EJ-FAELT-UT                                 
061100             PERFORM MFS-ROER-EJ-FAELT-IN                                 
061200             MOVE NEJ TO INDATA-SW                                        
061300           END-IF                                                         
061400         END-IF                                                           
061500       END-IF                                                             
061600     END-IF                                                               
061700     .                                                                    
061800     EJECT                                                                
061900 H-UPPDATERA SECTION.                                                     
062000                                                                          
062110     IF MID-INPUT NOT = SPACE                                             
062200       PERFORM IMS-GU-4459-ROT                                            
062300       MOVE +1   TO INDX                                                  
062400       PERFORM UNTIL INDX > MAX-INDX                                      
062500         MOVE MID-IDKUNDNR (INDX) TO W-4460-IDKUNDNR                      
062600         IF MID-KDCMD (INDX) = 'D'                                        
062800           PERFORM IMS-GHNP-4459-BARN                                     
062900           IF SEGMENT-FINNS                                               
063000             PERFORM IMS-DLET-4459-BARN                                   
063100           END-IF                                                         
063200         ELSE                                                             
063300           IF MID-KDCMD (INDX) = 'R'                                      
063400             PERFORM HA-REPL-BARN                                         
063500           END-IF                                                         
063600         END-IF                                                           
063700         ADD +1 TO INDX                                                   
063800       END-PERFORM                                                        
063900                                                                          
064000       PERFORM S01-INFO-UPDATE                                            
064100     END-IF                                                               
064200                                                                          
064310     IF MID-INPUT = SPACE                                                 
064400       IF MID-IDKUNDNR-IN NOT = ALL '+'                                   
064500                                                                          
064600         MOVE MSGI-IDDISTR    TO 4460-IDDISTR                             
064700         MOVE MID-IDKUNDNR-IN TO 4460-IDKUNDNR                            
064800         MOVE WS-IDPRTLST     TO 4460-IDPRTLST                            
064900                                                                          
065000         PERFORM IMS-ISRT-4459-BARN                                       
065001                                                                          
065010         IF SEGMENT-FINNS-REDAN                                           
065011           MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDKUNDNR-IN-ATTR              
065012           MOVE INF-KUND-FINNS-REDAN TO MED-IDMFSINF                      
065013           CALL WMEDKONV USING MED-WMEDAREA                               
065014           MOVE MED-MFSINF TO MOD-TEMFSFEL                                
065015           PERFORM MFS-ROER-EJ-FAELT-UT                                   
065016           PERFORM MFS-ROER-EJ-FAELT-IN                                   
065020         ELSE                                                             
065100           PERFORM S01-INFO-UPDATE                                        
065200         END-IF                                                           
065210       END-IF                                                             
065300     END-IF                                                               
065400     .                                                                    
065500     EJECT                                                                
065600 HA-REPL-BARN  SECTION.                                                   
065700                                                                          
065900     PERFORM IMS-GHNP-4459-BARN                                           
066000                                                                          
066100     IF SEGMENT-FINNS                                                     
066200       MOVE WS-IDPRTLST TO 4460-IDPRTLST                                  
066300       PERFORM IMS-REPL-4459-BARN                                         
066400     END-IF                                                               
066500     .                                                                    
066600     EJECT                                                                
066700 S01-INFO-UPDATE SECTION.                                                 
066800                                                                          
066900     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
067000     CALL WMEDKONV USING MED-WMEDAREA                                     
067100     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
067200     PERFORM MFS-FORM-ATTR                                                
067300     PERFORM MFS-RENSA-FAELT-IN                                           
067500                                                                          
067600     .                                                                    
067700     EJECT                                                                
067800 S02-KOLLA-PRINTER-FINNS SECTION.                                         
067900                                                                          
068000     MOVE JA TO PRINTER-SW                                                
068700                                                                          
068800       MOVE 1               TO PRT-KDCALL                                 
068900       MOVE WS-IDPRTLST     TO PRT-IDPRTLST                               
069000       CALL W006PRT      USING PRT-W006PRT                                
069100                                                                          
069200       IF PRT-IDLTERM = 'SAKNAS  '                                        
069300         MOVE NEJ TO PRINTER-SW                                           
069400       END-IF                                                             
069500                                                                          
069700     .                                                                    
069800     EJECT                                                                
069900 MFS-RENSA-FAELT-UT SECTION.                                              
070000                                                                          
070100*    --- ALLA UTDATA-FÄLT                                                 
070200*    --- INKL. BLÄDDRINGSNYCKLAR                                          
070300     MOVE +1 TO INDX                                                      
070400     PERFORM UNTIL INDX > MAX-INDX                                        
070500       MOVE MFS-RENSA-FAELT TO MOD-KDCMD    (INDX)                        
070600                               MOD-IDKUNDNR (INDX)                        
070700                               MOD-IDPRT    (INDX)                        
070800       ADD +1 TO INDX                                                     
070900     END-PERFORM                                                          
071000     .                                                                    
071100     SKIP3                                                                
071900 MFS-RENSA-FAELT-IN SECTION.                                              
072000                                                                          
072100*    --- ALLA INDATA-FÄLT                                                 
072200     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
072300                             MOD-IDPRT-IN                                 
072400     MOVE +1 TO INDX                                                      
072500     PERFORM UNTIL INDX > MAX-INDX                                        
072600       MOVE MFS-RENSA-FAELT TO MOD-KDCMD (INDX)                           
072700       ADD +1 TO INDX                                                     
072800     END-PERFORM                                                          
072900     .                                                                    
073000     EJECT                                                                
073100 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
073200                                                                          
073300*    --- ALLA UTDATA-FÄLT                                                 
073400*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
073500                                                                          
073600     MOVE +1 TO INDX                                                      
073700     PERFORM UNTIL INDX > MAX-INDX                                        
073800       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
073900       ADD +1 TO INDX                                                     
074000     END-PERFORM                                                          
074100     .                                                                    
074200     SKIP2                                                                
074300 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
074400                                                                          
074500*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
074600     MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD    (INDX)                        
074700                               MOD-IDKUNDNR (INDX)                        
074800                               MOD-IDPRT    (INDX)                        
074900     .                                                                    
075000     SKIP3                                                                
075100 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
075200                                                                          
075300*    --- ALLA INDATA-FÄLT                                                 
075400     MOVE MFS-ROER-EJ-FAELT TO MOD-IDKUNDNR-IN                            
075500                               MOD-IDPRT-IN                               
075600                                                                          
075700     MOVE +1 TO INDX                                                      
075800     PERFORM UNTIL INDX > MAX-INDX                                        
075900       MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD (INDX)                         
076000       ADD +1 TO INDX                                                     
076100     END-PERFORM                                                          
076200     .                                                                    
076300     EJECT                                                                
076400 MFS-FORM-ATTR SECTION.                                                   
076500                                                                          
076600*    --- ALLA INDATA-FÄLT                                                 
076700     MOVE MFS-FORMATETS-ATTR TO MOD-IDKUNDNR-IN-ATTR                      
076800                                MOD-IDPRT-IN-ATTR                         
076900                                                                          
077000     MOVE +1 TO INDX                                                      
077100     PERFORM UNTIL INDX > MAX-INDX                                        
077200       MOVE MFS-FORMATETS-ATTR TO MOD-KDCMD (INDX)                        
077300       ADD +1 TO INDX                                                     
077400     END-PERFORM                                                          
077500     .                                                                    
077600     SKIP2                                                                
079000* --- IMS SEKTIONER ---                                                   
079100     SKIP3                                                                
079200 IMS-GET-MSG SECTION.                                                     
079300                                                                          
079400     MOVE '  QC' TO GODK-STATUSKODER                                      
079500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
079600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
079700     PERFORM IMS-STATUSKONTROLL                                           
079800     .                                                                    
079900     SKIP3                                                                
080000 IMS-INSERT-MSG SECTION.                                                  
080100                                                                          
080500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
080600     MOVE SPACE TO GODK-STATUSKODER                                       
080700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
080800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
080900     PERFORM IMS-STATUSKONTROLL                                           
081000     .                                                                    
081100     EJECT                                                                
081200 IMS-GET-GMTA-KUND SECTION.                                               
081300                                                                          
081400     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
081500          DELIMITED BY SIZE INTO SSA1                                     
081600     MOVE '  GE' TO GODK-STATUSKODER                                      
081700     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-WLGMTA01 SSA1                  
081800     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
081900     PERFORM IMS-STATUSKONTROLL                                           
082000     .                                                                    
082100     EJECT                                                                
082200 IMS-GU-4459-ROT SECTION.                                                 
082300                                                                          
082400     STRING 'WL445901(WDGXKEY  =' W-WDGX4459-X ')'                        
082500          DELIMITED BY SIZE INTO SSA1                                     
082600     MOVE '  GE' TO GODK-STATUSKODER                                      
082700     CALL CBLTDLI USING GU 4459-PCB DLI-IO-WL445901 SSA1                  
082800     MOVE 4459-STATUS-CODE TO STATUS-WS                                   
082900     PERFORM IMS-STATUSKONTROLL                                           
083000     .                                                                    
083100     EJECT                                                                
083200 IMS-GNP-4459-BARN SECTION.                                               
083300                                                                          
083400     STRING 'WL445911(KY4460  >=' W-WDGX4460-MIN-X                        
083500                    '&KY4460  <=' W-WDGX4460-MAX-X ')'                    
083600          DELIMITED BY SIZE INTO SSA1                                     
083700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
083800     CALL CBLTDLI USING GNP 4459-PCB DLI-IO-WL445911 SSA1                 
083900     MOVE 4459-STATUS-CODE TO STATUS-WS                                   
084000     PERFORM IMS-STATUSKONTROLL                                           
084100     .                                                                    
084200     SKIP3                                                                
084300 IMS-GHNP-4459-BARN SECTION.                                              
084400                                                                          
084500     STRING 'WL445911*F(KY4460   =' W-WDGX4460-X ')'                      
084600          DELIMITED BY SIZE INTO SSA1                                     
084700     MOVE '  GE' TO GODK-STATUSKODER                                      
084800     CALL CBLTDLI USING GHNP 4459-PCB DLI-IO-WL445911 SSA1                
084900     MOVE 4459-STATUS-CODE TO STATUS-WS                                   
085000     PERFORM IMS-STATUSKONTROLL                                           
085100     .                                                                    
085200     SKIP3                                                                
086500 IMS-ISRT-4459-BARN SECTION.                                              
086600                                                                          
086700     STRING 'WL445901(WDGXKEY  =' W-WDGX4459-X ')'                        
086800          DELIMITED BY SIZE INTO SSA1                                     
086900     MOVE 'WL445911 ' TO SSA2                                             
087000     MOVE '  II' TO GODK-STATUSKODER                                      
087100     CALL CBLTDLI USING ISRT 4459-PCB DLI-IO-WL445911 SSA1 SSA2           
087200     MOVE 4459-STATUS-CODE TO STATUS-WS                                   
087300     PERFORM IMS-STATUSKONTROLL                                           
087400     .                                                                    
087500     SKIP3                                                                
087600 IMS-REPL-4459-BARN SECTION.                                              
087700                                                                          
087800     MOVE '  ' TO GODK-STATUSKODER                                        
087900     CALL CBLTDLI USING REPL 4459-PCB DLI-IO-WL445911                     
088000     MOVE 4459-STATUS-CODE TO STATUS-WS                                   
088100     PERFORM IMS-STATUSKONTROLL                                           
088200     .                                                                    
088300     SKIP3                                                                
088400 IMS-DLET-4459-BARN SECTION.                                              
088500                                                                          
088600     MOVE '  ' TO GODK-STATUSKODER                                        
088700     CALL CBLTDLI USING DLET 4459-PCB DLI-IO-WL445911                     
088800     MOVE 4459-STATUS-CODE TO STATUS-WS                                   
088900     PERFORM IMS-STATUSKONTROLL                                           
089000     .                                                                    
089100     EJECT                                                                
089110 IMS-GU-WDB601    SECTION.                                                
089120     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
089130          DELIMITED BY SIZE INTO SSA1                                     
089140     MOVE '  GE' TO GODK-STATUSKODER                                      
089150     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
089160     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
089170     PERFORM IMS-STATUSKONTROLL                                           
089180     IF SEGMENT-SAKNAS                                                    
089190         MOVE SPACE TO DCS-KDDC                                           
089191     END-IF                                                               
089192     .                                                                    
089200 IMS-STATUSKONTROLL SECTION.                                              
089300                                                                          
089400     SET STATUS-IX TO 1                                                   
089500     SEARCH GODK-STATUS                                                   
089600       AT END                                                             
089700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
089800         DELIMITED BY SIZE INTO FELTEXT                                   
089900         CALL FELLOG                                                      
090000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
090100         CONTINUE                                                         
090200     END-SEARCH                                                           
090300     .                                                                    
