000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2032300.                                                
000400 AUTHOR.         PER-ANDERS HELGEGREN.                                    
000500 DATE-WRITTEN.   92/03/19.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        BESTÄLLNINGS PROGRAM                                             
001000*        FÖR LEVERANTÖRBEDÖMNINGS LISTA                                   
001100*        (OCH EV URVAL)                                                   
001200*                                                                         
001300*        LISTPROGRAMMET BESTÄLLES VIA SOP MED                             
001400*        DETTA PGMS INPUT SOM EV PARAMETRAR                               
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W2T323                                              
001800*        MID:         W2I32301                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        MOD:         W2O32301                                            
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900*    -- CHECKED BY WY2000                                                 
003000 77  IDPGM                       PIC X(08)   VALUE 'W2032300'.            
003100                                                                          
003200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003400                                                                          
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003700                                                                          
003800*   OM SVAR TILL SKÄRM: MAX-MOD-LAENGD = MOD-LÄNGD + 4                    
003900*   OM PROGRAM-TILL-PROGRAM-SWITCH:    = MOD-LÄNGD + 17                   
004000 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +165  COMP SYNC.        
004100                                                                          
004200*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004300                                                                          
004400                                                                          
004500 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004600     88  INDATA-OK                           VALUE 'J'.                   
004700     88  INDATA-FEL                          VALUE 'N'.                   
004800                                                                          
004900 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005000     88  ALLT-OK                             VALUE 'J'.                   
005100                                                                          
005200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005300     88  EGEN-MID                            VALUE '2323'.                
005400     88  GODK-MID                            VALUE '2321' '2322'          
005500                                                   '2323' '2324'          
005600                                                   '2325' '2326'          
005700                                                   '2327' '2328'          
005800                                                   '2329'.                
005900     88  HELP-MID                            VALUE '0551'.                
006000     EJECT                                                                
006100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006200 01  GENERELLA-SUBPROGRAM.                                                
006300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006600     SKIP3                                                                
006700 01  PARAMETRAR.                                                          
006800     03  WS-IDFTG.                                                        
006900         05  IDFTG               PIC 9(2)    VALUE 57.                    
007000     03  WS-IDLEVNR.                                                      
007100         05  IDLEVNR             PIC X(5)    VALUE SPACE.                 
007200     03  WS-KDPRODSL.                                                     
007300         05  KDPRODSL            PIC 9(2)    VALUE ZERO.                  
007400     03  WS-KDARBTYP.                                                     
007500         05  KDARBTYP            PIC X(4)    VALUE SPACE.                 
007600     03  WS-IDPERSON.                                                     
007700         05  IDPERSON            PIC 9(3)    VALUE ZERO.                  
007800     03  WS-IDANSK-FOM.                                                   
007900         05  IDANSK-FOM          PIC 9(3)    VALUE ZERO.                  
008000     03  WS-IDANSK-TOM.                                                   
008100         05  IDANSK-TOM          PIC 9(3)    VALUE ZERO.                  
008200     03  WS-IDLEVNR-2.                                                    
008300         05  IDLEVNR-2           PIC X(5)    VALUE SPACE.                 
008400     03  WS-AVROP-VV.                                                     
008500         05  AVROP-VV            PIC 9(2)    VALUE ZERO.                  
008600     03  WS-FLSLAP.                                                       
008700         05  FLSLAP              PIC X(1)    VALUE SPACE.                 
008800     03  WS-FLLEVBESK.                                                    
008900         05  FLLEVBESK           PIC X(1)    VALUE SPACE.                 
009000     03  WS-IDMAIL.                                                       
009100         05  IDMAIL              PIC X(57) VALUE SPACE.                   
009200     EJECT                                                                
009300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009400*01 -COPY WMEDAREA                                                        
009500     SKIP3                                                                
009600 01  MESSAGE-CODES.                                                       
009700     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009800     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
009900     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
010100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010200     SKIP2                                                                
010300 01  FILLER.                                                              
010400     03  LISTANATT.                                                       
010500         05  FILLER              PIC X(30)   VALUE                        
010600                                 'LIST ORDERED               '.           
010700     EJECT                                                                
010800 01  NYCKLAR-TILL-DLI.                                                    
010900   03    W-KDARBTYP-X.                                                    
011000     05    W-KDARBTYP             PIC X(8)    VALUE SPACE.                
011100                                                                          
011200   03    W-IDPERSON-X.                                                    
011300     05    W-IDPERSON             PIC S9(3)   COMP-3 VALUE +0.            
011400                                                                          
011500 01  PROG-TO-PROG-SW.                                                     
011600*    03  -COPY WMSGSOP                                                    
011700     EJECT                                                                
011800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
011900*                                                                         
012000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012100     SKIP3                                                                
012200*01  MID -COPY W2I32301                                                   
012300     EJECT                                                                
012400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012500     SKIP3                                                                
012600*01  -COPY WMSGAREA                                                       
012700     EJECT                                                                
012800     03  MOD REDEFINES MSG-AREA.                                          
012900*      05  -COPY W2O32301                                                 
013000     EJECT                                                                
013100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013200     SKIP3                                                                
013300*01  -COPY WMFSAREA                                                       
013400     EJECT                                                                
013500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013600*                                                                         
013700     SKIP2                                                                
013800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013900     SKIP3                                                                
014000*    --- STATUS-KOD FRÅN IMS                                              
014100 01  STATUS-WS                   PIC XX.                                  
014200     88  SEGMENT-FINNS                       VALUE '  '.                  
014300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014500     SKIP2                                                                
014600 01  GODK-STATUSKODER.                                                    
014700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014800     EJECT                                                                
014900*    --- IMS FUNKTIONSKODER                                               
015000*01  -COPY W0003                                                          
015100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-P3'.           
015200     SKIP3                                                                
015300 01  DLI-IO-P3.                                                           
015400*    03  -COPY WDP311                                                     
015500     EJECT                                                                
015600 01  SSA1                        PIC X(64).                               
015700 01  SSA2                        PIC X(64).                               
015800 LINKAGE SECTION.                                                         
015900*01  -COPY W0009   -PRE MSG-                                              
016000                                                                          
016100*01  -COPY W0009   -PRE ALT-                                              
016200                                                                          
016300*01  -COPY W0008   -PRE WDP3-                                             
016400     05  FILLER                  PIC X.                                   
016500                                                                          
016600     EJECT                                                                
016700 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB WDP3-PCB.                      
016800 MAIN SECTION.                                                            
016900     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDP3-PCB.                      
017000                                                                          
017100     PERFORM IMS-GET-MSG                                                  
017200     IF SEGMENT-FINNS                                                     
017300       PERFORM A-INIT                                                     
017400       IF MFS-UPDATE                                                      
017500          PERFORM B-KOLLA-INPUT                                           
017600          IF INDATA-OK                                                    
017700             PERFORM C-UPPDATERA                                          
017800             PERFORM MFS-RENSA-FAELT-X                                    
017900          END-IF                                                          
018000       ELSE                                                               
018100          IF EGEN-MID                                                     
018200             PERFORM D-SAMMA-SIDA                                         
018300          ELSE                                                            
018400             PERFORM MFS-RENSA-FAELT-X                                    
018500          END-IF                                                          
018600       END-IF                                                             
018700       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
018800       PERFORM IMS-INSERT-MSG                                             
018900     END-IF                                                               
019000                                                                          
019100     MOVE ZERO TO RETURN-CODE                                             
019200     GOBACK                                                               
019300     .                                                                    
019400     EJECT                                                                
019500 A-INIT SECTION.                                                          
019600                                                                          
019700     IF MSG-DUBBLA-TRANSKODER                                             
019800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I32301-CTX             
019900       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
020000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
020100     ELSE                                                                 
020200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I32301-CTX              
020300       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
020400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
020500     END-IF                                                               
020600                                                                          
020700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
020800     MOVE MSG-IDPFK TO MFS-IDPFK                                          
020900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
021000                                                                          
021100     MOVE LOW-VALUE TO MSG-AREA                                           
021200     MOVE 'W2O323N1' TO MFS-IDMOD                                         
021300     MOVE '2323' TO MOD-IDTRANS                                           
021400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
021500                                                                          
021600     IF NOT EGEN-MID AND NOT HELP-MID                                     
021700*      MOVE NEJ   TO MID-FLSLAP                                           
021800*                    MOD-FLSLAP                                           
021900       MOVE SPACE TO MFS-KDTRTYP                                          
022000       MOVE '7' TO MFS-IDPFK                                              
022100     END-IF                                                               
022200                                                                          
022400     MOVE 'GB ' TO MED-IDSKYLT                                            
022800     .                                                                    
022900     EJECT                                                                
023000 B-KOLLA-INPUT   SECTION.                                                 
023100                                                                          
023200     MOVE JA TO INDATA-SW                                                 
023300                                                                          
023400     IF MID-W2I32301-CTX = ALL '+'                                        
023500        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
023600        CALL WMEDKONV USING MED-WMEDAREA                                  
023700        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
023800        PERFORM MFS-RENSA-FAELT-X                                         
023900        MOVE NEJ TO INDATA-SW                                             
024000     ELSE                                                                 
024100                                                                          
024200        IF MID-IDLEVNR NOT = ALL '+'                                      
024300          PERFORM BA-KONTROLL-URVAL                                       
024400        END-IF                                                            
024500                                                                          
024600        IF MID-IDLEVNR-2 NOT = ALL '+'                                    
024700          PERFORM BB-KONTROLL-URVAL                                       
024800        END-IF                                                            
024900                                                                          
025000        IF INDATA-FEL                                                     
025100          IF MOD-TEMFSFEL NOT > SPACE                                     
025200             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
025300             CALL WMEDKONV USING MED-WMEDAREA                             
025400             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
025500          END-IF                                                          
025600          PERFORM MFS-ROER-EJ-FAELT-X                                     
025700        END-IF                                                            
025800     END-IF                                                               
025900     .                                                                    
026000     EJECT                                                                
026100 BA-KONTROLL-URVAL SECTION.                                               
026200                                                                          
026210     IF MID-IDLEVNR    NOT = ALL '+'                                      
026211        IF MID-IDLEVNR = SPACE                                            
026213           MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDLEVNR-ATTR                  
026214           MOVE NEJ TO INDATA-SW                                          
026240        ELSE                                                              
026250           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-ATTR                  
026260           MOVE MID-IDLEVNR          TO WS-IDLEVNR                        
026270        END-IF                                                            
026286     END-IF                                                               
026290                                                                          
026800     IF MID-KDPRODSL   NOT = ALL '+'                                      
026900        IF MID-KDPRODSL   NOT NUMERIC                                     
027000           MOVE MFS-NUM-FAELT-FEL   TO MOD-KDPRODSL-ATTR                  
027100           MOVE NEJ TO INDATA-SW                                          
027200        ELSE                                                              
027300           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDPRODSL-ATTR                  
027400           MOVE MID-KDPRODSL        TO WS-KDPRODSL                        
027500        END-IF                                                            
027600     END-IF                                                               
027601                                                                          
027602     IF INDATA-OK                                                         
027610        PERFORM S01-KONTROLL-MAIL                                         
027620     END-IF                                                               
027700     .                                                                    
027800     EJECT                                                                
027900 BB-KONTROLL-URVAL SECTION.                                               
028000                                                                          
028100     PERFORM S01-KONTROLL-MAIL                                            
028200     IF MID-IDANSK-FOM NOT = ALL '+'                                      
028300        IF MID-IDANSK-FOM NOT NUMERIC                                     
028400           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDANSK-FOM-ATTR                
028500           MOVE NEJ TO INDATA-SW                                          
028600        ELSE                                                              
028700           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDANSK-FOM-ATTR                
028800           MOVE MID-IDANSK-FOM      TO WS-IDANSK-FOM                      
028900        END-IF                                                            
029000     ELSE                                                                 
029100        MOVE MFS-NUM-FAELT-FEL      TO MOD-IDANSK-FOM-ATTR                
029200        MOVE NEJ TO INDATA-SW                                             
029300     END-IF                                                               
029400                                                                          
029500     IF MID-IDANSK-TOM NOT = ALL '+'                                      
029600        IF MID-IDANSK-TOM NOT NUMERIC                                     
029700           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDANSK-TOM-ATTR                
029800           MOVE NEJ TO INDATA-SW                                          
029900        ELSE                                                              
030000           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDANSK-TOM-ATTR                
030100           MOVE MID-IDANSK-TOM      TO WS-IDANSK-TOM                      
030200        END-IF                                                            
030300     ELSE                                                                 
030400        MOVE MFS-NUM-FAELT-FEL      TO MOD-IDANSK-TOM-ATTR                
030500        MOVE NEJ TO INDATA-SW                                             
030600     END-IF                                                               
030700                                                                          
030800     IF MID-IDLEVNR-2  NOT = ALL '+'                                      
030900        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-2-ATTR                   
031000        MOVE MID-IDLEVNR-2        TO WS-IDLEVNR-2                         
031100     ELSE                                                                 
031200        MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDLEVNR-2-ATTR                   
031300        MOVE NEJ TO INDATA-SW                                             
031400     END-IF                                                               
031500                                                                          
031600     IF MID-AVROP-VV NOT = ALL '+'                                        
031700        IF MID-AVROP-VV NOT NUMERIC                                       
031800           MOVE MFS-NUM-FAELT-FEL   TO MOD-AVROP-VV-ATTR                  
031900           MOVE NEJ TO INDATA-SW                                          
032000        ELSE                                                              
032100           MOVE MFS-NUM-FAELT-RAETT TO MOD-AVROP-VV-ATTR                  
032200           MOVE MID-AVROP-VV        TO WS-AVROP-VV                        
032300        END-IF                                                            
032400     ELSE                                                                 
032500        MOVE MFS-NUM-FAELT-FEL      TO MOD-AVROP-VV-ATTR                  
032600        MOVE NEJ TO INDATA-SW                                             
032700     END-IF                                                               
032800                                                                          
032900     IF MID-FLLEVBESK  NOT = ALL '+'                                      
033000        IF MID-FLLEVBESK = 'J' OR 'N'                                     
033100          MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLLEVBESK-ATTR                 
033200          MOVE MID-FLLEVBESK      TO WS-FLLEVBESK                         
033300        ELSE                                                              
033400          MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLLEVBESK-ATTR                 
033500          MOVE NEJ TO INDATA-SW                                           
033600        END-IF                                                            
033700     ELSE                                                                 
033800        MOVE MFS-ALFA-FAELT-RAETT   TO MOD-FLLEVBESK-ATTR                 
033900        MOVE 'N'                  TO WS-FLLEVBESK                         
034000     END-IF                                                               
034100                                                                          
034200     IF MID-FLSLAP     NOT = ALL '+'                                      
034300        IF MID-FLSLAP    = 'J' OR 'N'                                     
034400          MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLSLAP-ATTR                    
034500          MOVE MID-FLSLAP         TO WS-FLSLAP                            
034600        ELSE                                                              
034700          MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLSLAP-ATTR                    
034800          MOVE NEJ TO INDATA-SW                                           
034900        END-IF                                                            
035000     ELSE                                                                 
035100        MOVE MFS-ALFA-FAELT-FEL     TO MOD-FLSLAP-ATTR                    
035200        MOVE NEJ TO INDATA-SW                                             
035300     END-IF                                                               
035400                                                                          
035500     .                                                                    
035600     EJECT                                                                
035700                                                                          
038700 C-UPPDATERA      SECTION.                                                
038800                                                                          
038900        IF MID-IDLEVNR NOT = ALL '+'                                      
039000          MOVE '2323' TO MSGSOP-IDTRANS                                   
039100          MOVE '1'    TO MSGSOP-KDMFSFOR                                  
039200          MOVE 'W235B1' TO MSGSOP-IDPROCESS                               
039300          MOVE 'O'    TO MSGSOP-KDSOPFUNK                                 
039400                                                                          
039500          STRING 'IDFTG('   WS-IDFTG                                      
039600                 ')IDLEVNR(' WS-IDLEVNR                                   
039700                 ')KDPRODSL(' WS-KDPRODSL                                 
039800                 ')MAIL(' WS-IDMAIL                                       
039900                 ')IDUSER(' MSG-SIGNON-USERID ')'                         
040000                 DELIMITED BY SIZE INTO MSGSOP-TESYMBV                    
040100                                                                          
040200          PERFORM IMS-INSERT-ALTMSG                                       
040300                                                                          
040400          MOVE LISTANATT  TO MOD-TEMFSINF                                 
040500        END-IF                                                            
040600        IF MID-IDLEVNR-2 NOT = ALL '+'                                    
040700          MOVE '2323' TO MSGSOP-IDTRANS                                   
040800          MOVE '1'    TO MSGSOP-KDMFSFOR                                  
040900          MOVE 'W235B2' TO MSGSOP-IDPROCESS                               
041000*         MOVE 'W217S3' TO MSGSOP-IDPROCESS                               
041100          MOVE 'O'    TO MSGSOP-KDSOPFUNK                                 
041200                                                                          
041300          STRING 'IDANSK-FOM(' WS-IDANSK-FOM                              
041400                 ')IDANSK-TOM(' WS-IDANSK-TOM                             
041500                 ')IDLEVNR(' WS-IDLEVNR-2                                 
041600                 ')AVR-VV(' WS-AVROP-VV                                   
041700                 ')FLSLAP(' WS-FLSLAP                                     
041800                 ')FLLEVBE(' WS-FLLEVBESK                                 
041900                 ')MAIL(' WS-IDMAIL                                       
042000                 ')IDUSER(' MSG-SIGNON-USERID ')'                         
042100                 DELIMITED BY SIZE INTO MSGSOP-TESYMBV                    
042200                                                                          
042300          PERFORM IMS-INSERT-ALTMSG                                       
042400                                                                          
042500          MOVE LISTANATT  TO MOD-TEMFSINF                                 
042600        END-IF                                                            
042700     .                                                                    
042800     EJECT                                                                
042900 D-SAMMA-SIDA      SECTION.                                               
043000                                                                          
043100     MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                  
043200     CALL WMEDKONV USING MED-WMEDAREA                                     
043300     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
043400                                                                          
043500     PERFORM MFS-LAES-IN-IGEN                                             
043600     PERFORM MFS-ROER-EJ-FAELT-X                                          
043700     .                                                                    
043800     EJECT                                                                
043810 S01-KONTROLL-MAIL SECTION.                                               
043820                                                                          
043830       IF  MID-KDARBTYP NOT = ALL '+'                                     
043840       AND MID-IDPERSON NOT = ALL '+'                                     
043850         IF MID-IDPERSON NUMERIC                                          
043860           MOVE MID-KDARBTYP             TO W-KDARBTYP                    
043870           MOVE MID-IDPERSON             TO W-IDPERSON                    
043880           PERFORM IMS-GU-WDP311                                          
043890           IF SEGMENT-FINNS                                               
043892             IF PERS-IDMAIL = SPACE                                       
043894               MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDARBTYP-ATTR             
043895               MOVE MFS-NUM-FAELT-FEL    TO MOD-IDPERSON-ATTR             
043896               MOVE NEJ                  TO INDATA-SW                     
043900             ELSE                                                         
043901               MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDARBTYP-ATTR             
043902               MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDPERSON-ATTR             
043903               MOVE PERS-IDMAIL          TO IDMAIL                        
043904             END-IF                                                       
043905           ELSE                                                           
043906             MOVE MFS-ALFA-FAELT-FEL     TO MOD-KDARBTYP-ATTR             
043907             MOVE MFS-NUM-FAELT-FEL      TO MOD-IDPERSON-ATTR             
043908             MOVE NEJ                    TO INDATA-SW                     
043909           END-IF                                                         
043910         ELSE                                                             
043911           MOVE MFS-NUM-FAELT-FEL        TO MOD-IDPERSON-ATTR             
043912           MOVE NEJ                      TO INDATA-SW                     
043913         END-IF                                                           
043914       ELSE                                                               
043915         MOVE MFS-ALFA-FAELT-FEL         TO MOD-KDARBTYP-ATTR             
043916         MOVE MFS-NUM-FAELT-FEL          TO MOD-IDPERSON-ATTR             
043917         MOVE NEJ                        TO INDATA-SW                     
043918       END-IF                                                             
043919       .                                                                  
043920       EJECT                                                              
043921                                                                          
043930 MFS-RENSA-FAELT-X  SECTION.                                              
044000                                                                          
044100*    --- ALLA        FÄLT                                                 
044200     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR                                  
044300                             MOD-KDPRODSL                                 
044400                             MOD-KDARBTYP                                 
044500                             MOD-IDPERSON                                 
044600                             MOD-IDANSK-FOM                               
044700                             MOD-IDANSK-TOM                               
044800                             MOD-IDLEVNR-2                                
044900                             MOD-AVROP-VV                                 
045000                             MOD-FLLEVBESK                                
045100                             MOD-FLSLAP                                   
045200*    MOVE NEJ             TO MOD-FLSLAP                                   
045300     .                                                                    
045400     EJECT                                                                
045500 MFS-ROER-EJ-FAELT-X   SECTION.                                           
045600                                                                          
045700*    --- ALLA        FÄLT                                                 
045800     MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR                                
045900                               MOD-KDPRODSL                               
046000                               MOD-KDARBTYP                               
046100                               MOD-IDPERSON                               
046200                               MOD-IDANSK-FOM                             
046300                               MOD-IDANSK-TOM                             
046400                               MOD-IDLEVNR-2                              
046500                               MOD-AVROP-VV                               
046600                               MOD-FLLEVBESK                              
046700                               MOD-FLSLAP                                 
046800     .                                                                    
046900     EJECT                                                                
047000 MFS-LAES-IN-IGEN SECTION.                                                
047100                                                                          
047200*    --- ALLA INDATA-FÄLT                                                 
047300     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVNR-ATTR                       
047400                                   MOD-KDPRODSL-ATTR                      
047500                                   MOD-KDARBTYP-ATTR                      
047600                                   MOD-IDPERSON-ATTR                      
047700                                   MOD-IDANSK-FOM-ATTR                    
047800                                   MOD-IDANSK-TOM-ATTR                    
047900                                   MOD-IDLEVNR-2-ATTR                     
048000                                   MOD-AVROP-VV-ATTR                      
048100                                   MOD-FLSLAP-ATTR                        
048200                                   MOD-FLLEVBESK-ATTR                     
048300     .                                                                    
048400     EJECT                                                                
048500* --- IMS SEKTIONER ---                                                   
048600     SKIP3                                                                
048700 IMS-GET-MSG SECTION.                                                     
048800                                                                          
048900     MOVE '  QC' TO GODK-STATUSKODER                                      
049000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
049100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
049200     PERFORM IMS-STATUSKONTROLL                                           
049300     .                                                                    
049400     SKIP3                                                                
049500 IMS-INSERT-MSG SECTION.                                                  
049600                                                                          
049700     IF ENGLISH-TEXT                                                      
049800       MOVE 'N' TO MFS-KDHUVOMR                                           
049900     END-IF                                                               
050000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
050100     MOVE SPACE TO GODK-STATUSKODER                                       
050200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
050300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
050400     PERFORM IMS-STATUSKONTROLL                                           
050500     .                                                                    
050600     SKIP3                                                                
050700 IMS-INSERT-ALTMSG SECTION.                                               
050800                                                                          
050900     MOVE SPACE TO GODK-STATUSKODER                                       
051000     CALL CBLTDLI USING ISRT ALT-PCB PROG-TO-PROG-SW                      
051100     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
051200     PERFORM IMS-STATUSKONTROLL                                           
051300     .                                                                    
051400     EJECT                                                                
051500 IMS-GU-WDP311 SECTION.                                                   
051600     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
051700            DELIMITED BY SIZE INTO SSA1                                   
051800     STRING 'WDP311  (IDPERSON ='  W-IDPERSON-X ')'                       
051900            DELIMITED BY SIZE INTO SSA2                                   
052000     MOVE '  GE' TO GODK-STATUSKODER                                      
052100     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-P3 SSA1 SSA2                   
052200     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
052300     PERFORM IMS-STATUSKONTROLL                                           
052400     .                                                                    
052500     SKIP3                                                                
052600 IMS-STATUSKONTROLL SECTION.                                              
052700                                                                          
052800     SET STATUS-IX TO 1                                                   
052900     SEARCH GODK-STATUS                                                   
053000       AT END                                                             
053100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
053200         DELIMITED BY SIZE INTO FELTEXT                                   
053300         CALL FELLOG                                                      
053400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
053500         CONTINUE                                                         
053600     END-SEARCH                                                           
053700     .                                                                    
