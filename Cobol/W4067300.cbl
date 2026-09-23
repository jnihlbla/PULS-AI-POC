000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4067300.                                                
000400 AUTHOR.         MARGARETA GABRIELSSON.                                   
000500 DATE-WRITTEN.   1998-07-31.                                              
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PROGRAM FÖR INMATNING AV KOLLIN SOM SKALL VÄLJAS FÖR             
001000*        FAKTURERING/LASTNING. PROGRAMMET ÄR ETT ALTERNATIV TILL          
001100*        W4066300. MAN SKANNAR KOLLINA 'MANUELLT' SAMT TRYCKER            
001200*        PF11 FÖR ATT UPPDATERA..                                         
001300*                                                                         
001400*        MAN KAN ÄVEN SKANNA IN SAMLINGSKOLLIN, FOM DEC. '16, CO.         
001500*                                                                         
001600*        PROGRAMMET UPPDATERAR WDE6                                       
001700*        PROGRAMMET UPPDATERAR WDR4-4495                                  
001800*        PROGRAMMET LÄSER      WDE7                                       
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: W4T673U                                             
002200*        MID:         W4I67301                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        MOD:         W4O67301                                            
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003300*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(08)   VALUE 'W4067300'.            
003500                                                                          
003600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003800                                                                          
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  YES                         PIC X       VALUE 'Y'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200 77  MIXED                       PIC X       VALUE 'M'.                   
004300                                                                          
004400*    --- INDEX FÖR UPPDATERINGSRADER                                      
004500 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004600 77  MAX-INDX                    PIC S9(4)  VALUE +26   COMP SYNC.        
004700 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004800*    --- DET RÄTTA VÄRDET PÅ NEDANSTÅENDE FÄLT SÄTTS I A-INIT             
004900                                                                          
005000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005100 77  WS-IDTRPTNR                 PIC X(3)    VALUE SPACE.                 
005200 77  WS-IDLBBET                  PIC X(12)   VALUE SPACE.                 
005300 77  WS-FLFARLIG                 PIC X(1)    VALUE SPACE.                 
005400                                                                          
005500*      --- VALID IDDC CODES                                               
005600*                                                                         
005700*01    -COPY WWDC99                                                       
005800       EJECT                                                              
005900                                                                          
006000 01  WS-IDTIDZON                 PIC 9(2).                                
006100 01  WS-KDMATT                   PIC X.                                   
006200                                                                          
006300 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006400     88  INDATA-OK                           VALUE 'J'.                   
006500     88  INDATA-FEL                          VALUE 'N'.                   
006600                                                                          
006700 77  INPUT-FINNS-SW              PIC X       VALUE 'N'.                   
006800     88  INPUT-FINNS                         VALUE 'J'.                   
006900     88  INPUT-SAKNAS                        VALUE 'N'.                   
007000                                                                          
007100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007200     88  NYCKLAR-OK                          VALUE 'J'.                   
007300     88  NYCKLAR-FEL                         VALUE 'N'.                   
007400                                                                          
007500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007600     88  EGEN-MID                            VALUE '4673'.                
007700     88  GODK-MID                            VALUE '4663' '4664'          
007800                                                   '4665' '4673'.         
007900     88  HELP-MID                            VALUE '0551'.                
008000 77  SW-TRAEFF               PIC  X(1)        VALUE 'N'.                  
008100     88 TRAEFF                                VALUE 'J'.                  
008200     88 EJ-TRAEFF                             VALUE 'N'.                  
008300     EJECT                                                                
008400 01  FILLER                  PIC X(16) VALUE 'SWITCHAR'.                  
008500 77  TRAFF-SW                PIC  X(1)        VALUE 'N'.                  
008600     88  TRAFF                                VALUE 'J'.                  
008700                                                                          
008800 77  TRAFF-IDDC-SW           PIC  X(1)        VALUE 'N'.                  
008900     88  TRAFF-IDDC                           VALUE 'J'.                  
009000                                                                          
009100 77  TRAFF-IDKOLLI-SW        PIC  X(1)        VALUE 'N'.                  
009200     88  TRAFF-IDKOLLI                        VALUE 'J'.                  
009300     88  EJ-TRAFF-IDKOLLI                     VALUE 'N'.                  
009400                                                                          
009500 77  SAMKOLLI-SW             PIC  X(1)        VALUE 'N'.                  
009600     88  SAMKOLLI-FINNS                       VALUE 'J'.                  
009700     88  SAMKOLLI-FINNS-EJ                    VALUE 'N'.                  
009800 EJECT                                                                    
009900                                                                          
010000 01  TEST-IDDISTR            PIC  9(5) COMP-3 VALUE ZERO.                 
010100*01  FILLER  -COPY WWDIST79 -RED TEST-IDDISTR.                            
010200     EJECT                                                                
010300 01  FILLER                  PIC X(16) VALUE 'KONSTANTER'.                
010400 01  KONSTANTER.                                                          
010500     03  KLI-PACK            PIC S9(1) COMP-3 VALUE +1.                   
010600     03  KLI-PACK-FAKT       PIC S9(1) COMP-3 VALUE +6.                   
010700     03  SK-CLOSED           PIC  X(1)        VALUE 'C'.                  
010800     03  SK-SHIPPED          PIC  X(1)        VALUE 'S'.                  
010900                                                                          
011000 01      TIDSUPPGIFTER.                                                   
011100   03    DAGENS-DATUM        PIC 9(6).                                    
011200   03    TIAAMMDD            PIC 9(6).                                    
011300   03    TIKLOCK             PIC 9(8).                                    
011400   03    FILLER              REDEFINES TIKLOCK.                           
011500     05  TIKLOCK-MIN         PIC 9(4).                                    
011600     05  FILLER              PIC X(4).                                    
011700                                                                          
011800*    LOKAL TID                                                            
011900 01  WS-TILASTID                 PIC 9(6)    VALUE ZERO.                  
012000 01  WS-TILASTID-GRP             REDEFINES WS-TILASTID.                   
012100     03 WS-TILASTID-HHMM         PIC 9(4).                                
012200     03 WS-TILASTID-SS           PIC 9(2).                                
012300                                                                          
012400 77  LNG-P-TO-P-PREFIX           PIC S9(4)   VALUE +17  COMP SYNC.        
012500 77  W-KVKOLLI                   PIC S9(3)   VALUE ZERO COMP-3.           
012600 77  W-VLORDBTO            PIC S9(4)V9(3)    VALUE ZERO COMP-3.           
012700 77  W-VKORDBTO            PIC S9(6)V9(1)    VALUE ZERO COMP-3.           
012800 77  W-SUORDV              PIC S9(9)V9(2)    VALUE ZERO COMP-3.           
012900 77  W-SPAR-IDDISTR              PIC S9(5)   VALUE ZERO COMP-3.           
013000 77  W-SPAR-IDKOLLI-SAMP         PIC S9(5)   VALUE ZERO COMP-3.           
013100 77  W-IDKUNDNR-NUM              PIC S9(6)   VALUE ZERO COMP-3.           
013200 77  W-IDPSN-NUM                 PIC  9(3)   VALUE ZERO.                  
013300 77  W-MED-IDMFSFEL              PIC  X(3)   VALUE SPACE.                 
013400                                                                          
013500 77  WS-VLORDBTO           PIC S9(4)V9(3)    VALUE ZERO COMP-3.           
013600 77  WS-VKORDBTO           PIC S9(6)V9(1)    VALUE ZERO COMP-3.           
013700 77  WS-IDDC-IN                  PIC  X(2)   VALUE SPACE.                 
013800                                                                          
013900***     TABELL FÖR ATT LAGRA PRODUKTIONSNR ELLER                          
014000***     SAMLINGSKOLLIN PER RAD                                            
014100 01  FILLER.                                                              
014200   03  IDPRODNR-TABELL OCCURS  26.                                        
014300     05  TABELL-IDPRODNR         PIC S9(7) COMP-3.                        
014400     05  TABELL-IDKOLLI-SAMP     PIC S9(5) COMP-3.                        
014500     EJECT                                                                
014600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
014700 01  GENERELLA-SUBPROGRAM.                                                
014800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
014900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
015000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
015200     EJECT                                                                
015300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
015400*01 -COPY WMSGINIT                                                        
015500     EJECT                                                                
015600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
015700*01 -COPY WMEDAREA                                                        
015800     EJECT                                                                
015900*    --- PARAMETRAR TILL COPYTEXT   WWOMVAND                              
016000*01 -COPY WWOMVAND                                                        
016100     SKIP3                                                                
016200 01  MESSAGE-CODES.                                                       
016300     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
016400     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
016500     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
016600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
016700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
016800                                                                          
016900     03  W-FEL-795               PIC  X(3)   VALUE '795'.                 
017000     03  W-FEL-758               PIC  X(3)   VALUE '758'.                 
018000     EJECT                                                                
019000 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
020000 01      P-TO-P-SW.                                                       
021000                                                                          
021100  02     P-TO-P-KVLL             PIC S9(4)           COMP SYNC.           
021200  02     P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
021300  02     P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
021400  02     P-TO-P-KDTRANS          PIC X(8).                                
021500  02     P-TO-P-IDTRANS          PIC X(4).                                
021600  02     P-TO-P-KDMFSFOR         PIC X(1).                                
021700  02     P-TO-P-DATA             PIC X(1000).                             
021800     EJECT                                                                
021900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
022000*                                                                         
022100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
022200     SKIP3                                                                
022300*01  MID -COPY W4I67301                                                   
022400     EJECT                                                                
022500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
022600     SKIP3                                                                
022700*01  -COPY WMSGAREA                                                       
022800     EJECT                                                                
022900     03  MOD REDEFINES MSG-AREA.                                          
023000*      05  -COPY W4O67301     -PRE MOD-                                   
023100     EJECT                                                                
023200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
023300     SKIP3                                                                
023400*01  -COPY WMFSAREA                                                       
023500     EJECT                                                                
023600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
023700*                                                                         
023800     EJECT                                                                
023900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
024000     SKIP3                                                                
024100 01  NYCKLAR-TILL-DLI.                                                    
024200   03  W-IDDC-X.                                                          
024300     05  W-IDDC                  PIC  X(2)   VALUE SPACE.                 
024400*                                                                         
024500   03  W-IDPRODNR-X.                                                      
024600     05  W-IDPRODNR              PIC S9(7)   VALUE ZERO  COMP-3.          
024700*                                                                         
024800   03  W-IDKOLLI-X.                                                       
024900     05  W-IDKOLLI               PIC S9(5)   VALUE ZERO  COMP-3.          
025000*                                                                         
025100   03  W-WDGXKEY-X.                                                       
025200     05  W-IDHTYP                PIC  X(4)   VALUE '4495'.                
025300     05  W-IDDC-4495             PIC  X(2)   VALUE SPACE.                 
025400     05  W-IDTRPTNR              PIC S9(3)   VALUE ZERO  COMP-3.          
025500     05  W-IDLBBET               PIC X(12)   VALUE SPACE.                 
025600     05  FILLER                  PIC X(10)   VALUE LOW-VALUE.             
025700*                                                                         
025800   03  W-WDE4ASEQ-X.                                                      
025900     05  W-4A1-IDDISTR           PIC S9(5)   VALUE ZERO  COMP-3.          
026000     05  W-4A1-IDKUNDNR          PIC S9(7)   VALUE ZERO  COMP-3.          
026100     05  W-4A1-IDKUNDRF.                                                  
026200       07  W-4A1-IDORDNR         PIC X(5).                                
026300       07  FILLER                PIC X(5).                                
026400*                                                                         
026500   03  W-WDE7ASEQ-X.                                                      
026600     05  W-IDDC-E7               PIC  X(2)   VALUE SPACE.                 
026700     05  W-IDKOLLIS              PIC S9(5)   VALUE ZERO  COMP-3.          
026800*    --- STATUS-KOD FRÅN IMS                                              
026900 01  STATUS-WS                   PIC XX.                                  
027000     88  SEGMENT-FINNS                       VALUE '  '.                  
027100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
027200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
027300     88  BASEN-SLUT                          VALUE 'GB'.                  
027400     SKIP2                                                                
027500 01  GODK-STATUSKODER.                                                    
027600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
027700     SKIP3                                                                
027800 01  SSA1                        PIC X(128).                              
027900 01  SSA2                        PIC X(64).                               
028000 01  SSA3                        PIC X(64).                               
028100     EJECT                                                                
028200*    --- IMS FUNKTIONSKODER                                               
028300*01  -COPY W0003                                                          
028400     EJECT                                                                
028500*    ---  DLI INPUT-OUTPUT AREOR                                          
028600 01  FILLER         PIC X(16) VALUE 'DLI-IO-E601'.                        
028700 01  DLI-IO-E601.                                                         
028800*    03  -COPY WDE601                                                     
028900     EJECT                                                                
029000 01  FILLER         PIC X(16) VALUE 'DLI-IO-E611'.                        
029100 01  DLI-IO-E611.                                                         
029200*    03  -COPY WDE611                                                     
029300     EJECT                                                                
029400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4495'.                    
029500 01  DLI-IO-WDGX4495.                                                     
029600*    03  -COPY WDGX4495                                                   
029700     EJECT                                                                
029800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4496'.                    
029900 01  DLI-IO-WDGX4496.                                                     
030000*    03  -COPY WDGX4496                                                   
030100     EJECT                                                                
030200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4497'.                    
030300 01  DLI-IO-WDGX4497.                                                     
030400*    03  -COPY WDGX4497                                                   
030500     EJECT                                                                
030600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4498'.                    
030700 01  DLI-IO-WDGX4498.                                                     
030800*    03  -COPY WDGX4498                                                   
030900     EJECT                                                                
031000 01  FILLER         PIC X(16) VALUE 'DLI-IO-E401'.                        
031100 01  DLI-IO-E401.                                                         
031200*    03  -COPY WDE401                                                     
031300     EJECT                                                                
031400 01  FILLER         PIC X(16) VALUE 'DLI-IO-E711'.                        
031500 01  DLI-IO-E711.                                                         
031600*    03  -COPY WDE711                                                     
031700     EJECT                                                                
031800 01  FILLER         PIC X(16) VALUE 'DLI-IO-E721'.                        
031900 01  DLI-IO-E721.                                                         
032000*    03  -COPY WDE721                                                     
032100     EJECT                                                                
032200 LINKAGE SECTION.                                                         
032300                                                                          
032400*01  -COPY W0009   -PRE MSG-                                              
032500     EJECT                                                                
032600*01  -COPY W0008  -PRE USEA-                                              
032700     05  FILLER                  PIC X.                                   
032800     EJECT                                                                
032900*01  -COPY W0008  -PRE WDE6-                                              
033000     05  FILLER                  PIC X.                                   
033100     EJECT                                                                
033200*01  -COPY W0008  -PRE WDE4-                                              
033300     05  FILLER                  PIC X.                                   
033400     EJECT                                                                
033500*01  -COPY W0008  -PRE WDE7-                                              
033600     05  FILLER                  PIC X.                                   
033700     EJECT                                                                
033800*01  -COPY W0008  -PRE 4495-                                              
033900     05  FILLER                  PIC X.                                   
034000     EJECT                                                                
034100 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
034200                           WDE6-PCB WDE4-PCB                              
034300                           WDE7-PCB 4495-PCB.                             
034400     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
034500                           WDE6-PCB WDE4-PCB                              
034600                           WDE7-PCB 4495-PCB.                             
034700                                                                          
034800     PERFORM IMS-GET-MSG                                                  
034900     IF SEGMENT-FINNS                                                     
035000       PERFORM A-INIT                                                     
035100       PERFORM B-KOLLA-NYCKLAR                                            
035200       IF NYCKLAR-OK                                                      
035300         IF MFS-UPDATE                                                    
035400           PERFORM D-KOLLA-INPUT                                          
035500           IF INDATA-OK                                                   
035600             PERFORM E-UPPDATERA                                          
035700           END-IF                                                         
035800         ELSE                                                             
035900           PERFORM F-KOLLA-OM-FELTRYCK                                    
036000         END-IF                                                           
036100         IF INDATA-OK                                                     
036200           PERFORM C-VISA-LASTAT-HITTILLS                                 
036300         END-IF                                                           
036400       END-IF                                                             
036500       COMPUTE MSG-KVLL         = LENGTH OF MOD-W4O67301 + 4              
036600       PERFORM IMS-INSERT-MSG                                             
036700     END-IF                                                               
036800                                                                          
036900     MOVE ZERO TO RETURN-CODE                                             
037000     GOBACK                                                               
037100     .                                                                    
037200     EJECT                                                                
037300 A-INIT SECTION.                                                          
037400                                                                          
037500     IF MSG-DUBBLA-TRANSKODER                                             
037600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I67301                 
037700       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
037800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
037900     ELSE                                                                 
038000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I67301                  
038100       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
038200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
038300     END-IF                                                               
038400                                                                          
038500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
038600     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
038700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
038800                                                                          
038900     MOVE LOW-VALUE TO MSG-AREA                                           
039000     MOVE 'W4O673N1' TO MFS-IDMOD                                         
039100     MOVE '4673' TO MOD-IDTRANS                                           
039200     MOVE MFS-RENSA-FAELT  TO MOD-TEMFSFEL MOD-TEMFSINF                   
039300                                                                          
039500     IF EGEN-MID OR HELP-MID                                              
039610       CONTINUE                                                           
039700     ELSE                                                                 
039800       MOVE SPACE TO MFS-KDTRTYP                                          
039900     END-IF                                                               
040000     PERFORM MFS-RENSA-FAELT-IN                                           
040100                                                                          
040200     MOVE 1 TO INDX                                                       
040300     PERFORM UNTIL INDX > MAX-INDX                                        
040400       MOVE ZERO           TO TABELL-IDPRODNR(INDX)                       
040500                              TABELL-IDKOLLI-SAMP(INDX)                   
040600       ADD +1              TO INDX                                        
040700     END-PERFORM                                                          
040800     .                                                                    
040900     EJECT                                                                
041000 B-KOLLA-NYCKLAR SECTION.                                                 
041100                                                                          
041200     MOVE ALL '+'           TO MSGI-WMSGINIT                              
041300     MOVE '013'             TO MSGI-KDCALL                                
041400     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
041500     MOVE '4673'            TO MSGI-IDTRANS                               
041600     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
041700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
041800                                                                          
041900     IF MSGI-IDLAND-SPR = 'GB'                                            
042000       MOVE +2 TO SPRAK-IX                                                
042100     ELSE                                                                 
042200       MOVE +1 TO SPRAK-IX                                                
042300     END-IF                                                               
042400                                                                          
042500     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
042600     MOVE MSGI-IDTIDZON     TO WS-IDTIDZON                                
042700     MOVE MSGI-KDMATT       TO WS-KDMATT                                  
042800                                                                          
042900     MOVE JA TO NYCKLAR-SW                                                
043000                                                                          
043100*    -- KONTROLL AV IDTRPTNR                                              
043200     MOVE MFS-RENSA-FAELT   TO MOD-IDTRPTNR-IN                            
043300                               MOD-IDLBBET-IN                             
043400                               MOD-FLFARLIG-IN                            
043500                               MOD-IDDC-IN                                
043600                                                                          
043700     IF MID-IDTRPTNR-IN     =  ALL '+'                                    
043800       MOVE MID-IDTRPTNR-UT TO WS-IDTRPTNR                                
043900       INSPECT WS-IDTRPTNR REPLACING LEADING SPACE BY ZERO                
044000     ELSE                                                                 
044100       MOVE MID-IDTRPTNR-IN TO WS-IDTRPTNR                                
044200       MOVE SPACE           TO MFS-KDTRTYP                                
044300     END-IF                                                               
044400                                                                          
044500     IF WS-IDTRPTNR NUMERIC AND WS-IDTRPTNR > ZERO                        
044600       MOVE WS-IDTRPTNR     TO W-IDTRPTNR                                 
044700     ELSE                                                                 
044800       MOVE NEJ             TO NYCKLAR-SW                                 
044900     END-IF                                                               
045000                                                                          
045100     IF MID-IDLBBET-IN      = ALL '+'                                     
045200       MOVE MID-IDLBBET-UT  TO WS-IDLBBET                                 
045300     ELSE                                                                 
045400       MOVE MID-IDLBBET-IN  TO WS-IDLBBET                                 
045500       MOVE SPACE           TO MFS-KDTRTYP                                
045600     END-IF                                                               
045700     IF WS-IDLBBET          NOT = SPACE                                   
045800       MOVE WS-IDLBBET      TO W-IDLBBET                                  
045900     ELSE                                                                 
046000       MOVE NEJ             TO NYCKLAR-SW                                 
046100     END-IF                                                               
046200                                                                          
046300     IF MID-FLFARLIG-IN     = ALL '+'                                     
046400       MOVE MID-FLFARLIG-UT  TO WS-FLFARLIG                               
046500     ELSE                                                                 
046600       MOVE MID-FLFARLIG-IN TO WS-FLFARLIG                                
046700       MOVE SPACE           TO MFS-KDTRTYP                                
046800     END-IF                                                               
046900     IF WS-FLFARLIG         = JA OR YES OR NEJ OR MIXED                   
047000       IF WS-FLFARLIG = YES                                               
047100         MOVE JA TO WS-FLFARLIG                                           
047200       END-IF                                                             
047300     ELSE                                                                 
047400       MOVE NEJ             TO NYCKLAR-SW                                 
047500     END-IF                                                               
047600                                                                          
047700     MOVE MSGI-IDDC         TO WS-IDDC                                    
047800     IF CDC-SE                                                            
047900        IF MID-IDDC-IN NOT = ALL '+'                                      
048000          MOVE MID-IDDC-IN  TO WS-IDDC                                    
048100          IF DDC-SE                                                       
048200             MOVE MID-IDDC-IN   TO WS-IDDC-IN                             
048300             MOVE SPACE     TO MFS-KDTRTYP                                
048400          ELSE                                                            
048500             MOVE MSGI-IDDC TO WS-IDDC-IN                                 
048600          END-IF                                                          
048700        ELSE                                                              
048800          MOVE MID-IDDC-UT  TO WS-IDDC                                    
048900          IF DDC-SE                                                       
049000             MOVE MID-IDDC-UT   TO WS-IDDC-IN                             
049100          ELSE                                                            
049200             MOVE MSGI-IDDC TO WS-IDDC-IN                                 
049300          END-IF                                                          
049400        END-IF                                                            
049500     ELSE                                                                 
049600        MOVE MSGI-IDDC      TO WS-IDDC-IN                                 
049700     END-IF                                                               
049800                                                                          
049900     MOVE WS-IDDC-IN        TO W-IDDC                                     
050000                               W-IDDC-4495                                
050100                               W-IDDC-E7                                  
050200                               MOD-IDDC-UT                                
050300                                                                          
050400     IF GODK-MID OR NYCKLAR-OK                                            
050500       MOVE WS-IDTRPTNR     TO MOD-IDTRPTNR-UT                            
050600       INSPECT MOD-IDTRPTNR-UT REPLACING LEADING ZERO BY SPACE            
050700       MOVE WS-IDLBBET      TO MOD-IDLBBET-UT                             
050800       MOVE WS-FLFARLIG     TO MOD-FLFARLIG-UT                            
050900     ELSE                                                                 
051000       MOVE MFS-RENSA-FAELT TO MOD-IDTRPTNR-UT                            
051100                                MOD-FLFARLIG-UT                           
051200                                MOD-IDLBBET-UT                            
051300     END-IF                                                               
051400                                                                          
051500     IF NYCKLAR-FEL                                                       
051600       MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                               
051700       CALL WMEDKONV USING MED-WMEDAREA                                   
051800       MOVE MED-MFSFEL      TO MOD-TEMFSFEL                               
051900       PERFORM MFS-RENSA-FAELT-IN                                         
052000       PERFORM MFS-RENSA-FAELT-UT                                         
052100       PERFORM MFS-CLOSE-FAELT-ATTR                                       
052200     ELSE                                                                 
052300       IF MFS-QUERY                                                       
052400         MOVE MFS-ADD-SAETT-CURSOR TO MOD-IDDISTR-UPD-ATTR(1)             
052500       END-IF                                                             
052600     END-IF                                                               
052700     .                                                                    
052800     EJECT                                                                
052900 C-VISA-LASTAT-HITTILLS SECTION.                                          
053000                                                                          
053100     PERFORM IMS-GHU-WDGX4496                                             
053200     IF SEGMENT-FINNS                                                     
053300       MOVE 4496-KVKOLLI-LAST      TO MOD-KVKOLLI-VALD                    
053400                                                                          
053500       IF WS-KDMATT = 'U'                                                 
053600         COMPUTE WS-VKORDBTO         =                                    
053700                 4496-VKORDBTO-LASTB *                                    
053800                 CONV-KG-TO-LB                                            
053900         COMPUTE WS-VLORDBTO         =                                    
054000                 4496-VLORDBTO-LASTB *                                    
054100                 CONV-M3-TO-YD3                                           
054200         MOVE WS-VKORDBTO          TO MOD-VKORDBTO-VALD                   
054300         MOVE WS-VLORDBTO          TO MOD-VLORDBTO-VALD                   
054400                                                                          
054500       ELSE                                                               
054600         MOVE 4496-VLORDBTO-LASTB  TO MOD-VLORDBTO-VALD                   
054700         MOVE 4496-VKORDBTO-LASTB  TO MOD-VKORDBTO-VALD                   
054800       END-IF                                                             
054900                                                                          
055000     ELSE                                                                 
055100       MOVE ZERO                   TO MOD-KVKOLLI-VALD                    
055200                                      MOD-VLORDBTO-VALD                   
055300                                      MOD-VKORDBTO-VALD                   
055400     END-IF                                                               
055500     .                                                                    
055600     EJECT                                                                
055700 D-KOLLA-INPUT SECTION.                                                   
055800                                                                          
055900     MOVE JA                         TO INDATA-SW                         
056000     MOVE NEJ                        TO SW-TRAEFF                         
056100     MOVE +1                         TO INDX                              
056200     MOVE SPACE                      TO W-MED-IDMFSFEL                    
056300                                                                          
056400     PERFORM UNTIL INDX > MAX-INDX                                        
056500                                                                          
056600       IF MID-IDDISTR-UPD(INDX) = ALL '+' OR SPACE                        
056700          CONTINUE                                                        
056800       ELSE                                                               
056900         MOVE JA                     TO SW-TRAEFF                         
057000         IF MID-IDDISTR-UPD(INDX) NOT NUMERIC                             
057100           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDDISTR-UPD-ATTR(INDX)        
057200           MOVE NEJ                  TO INDATA-SW                         
057300         ELSE                                                             
057400           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDDISTR-UPD-ATTR(INDX)        
057500           MOVE MID-IDDISTR-UPD(INDX) TO W-4A1-IDDISTR                    
057600           PERFORM DA-KOLLA-REG-RAD                                       
057700         END-IF                                                           
057800       END-IF                                                             
057900                                                                          
058000       ADD +1                         TO INDX                             
059000     END-PERFORM                                                          
060000                                                                          
060100     IF EJ-TRAEFF                                                         
060200       MOVE ERR-PF11-AND-NO-DATA      TO MED-IDMFSFEL                     
060300       CALL WMEDKONV USING MED-WMEDAREA                                   
060400       MOVE MED-MFSFEL                TO MOD-TEMFSFEL                     
060500       PERFORM MFS-ROER-EJ-FAELT-IN                                       
060600       PERFORM MFS-ROER-EJ-FAELT-UT                                       
060700       MOVE NEJ                       TO INDATA-SW                        
060800     ELSE                                                                 
060900       IF INDATA-FEL                                                      
061000         IF W-MED-IDMFSFEL            = SPACE                             
062000            MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                     
063000         ELSE                                                             
063100            MOVE W-MED-IDMFSFEL       TO MED-IDMFSFEL                     
063200         END-IF                                                           
063300         CALL WMEDKONV USING MED-WMEDAREA                                 
063400         MOVE MED-MFSFEL              TO MOD-TEMFSFEL                     
063500         PERFORM MFS-ROER-EJ-FAELT-UT                                     
063600         PERFORM MFS-ROER-EJ-FAELT-IN                                     
063700       END-IF                                                             
063800     END-IF                                                               
063900     .                                                                    
064000     EJECT                                                                
064100 DA-KOLLA-REG-RAD   SECTION.                                              
064200                                                                          
064300     IF MID-IDDISTR-UPD(INDX) > ZERO                                      
064400       IF MID-IDKUNDNR-UPD(INDX) NOT = ALL '+'                            
064500         IF MID-IDKUNDNR-UPD(INDX) NOT NUMERIC                            
064600          MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-UPD-ATTR(INDX)           
064700           MOVE NEJ              TO INDATA-SW                             
064800         ELSE                                                             
064900           MOVE MFS-NUM-FAELT-RAETT                                       
065000                                 TO MOD-IDKUNDNR-UPD-ATTR(INDX)           
065100           MOVE MID-IDKUNDNR-UPD(INDX)                                    
065200                                 TO W-4A1-IDKUNDNR                        
065300         END-IF                                                           
065400       ELSE                                                               
065500           MOVE MFS-NUM-FAELT-RAETT                                       
065600                                 TO MOD-IDKUNDNR-UPD-ATTR(INDX)           
065700           MOVE ZERO             TO MID-IDKUNDNR-UPD(INDX)                
065800           MOVE MID-IDKUNDNR-UPD(INDX) TO W-4A1-IDKUNDNR                  
065900       END-IF                                                             
066000                                                                          
066100       IF MID-IDORDNR7-UPD(INDX)    NOT = ALL '+'                         
066200         IF MID-IDORDNR7-UPD(INDX)  NOT NUMERIC                           
066300           MOVE MFS-NUM-FAELT-FEL                                         
066400                                 TO MOD-IDORDNR7-UPD-ATTR(INDX)           
066500           MOVE NEJ              TO INDATA-SW                             
066600         ELSE                                                             
066700           IF MID-IDORDNR7-UPD(INDX)(1:2) NOT = ZERO                      
066800             MOVE MFS-NUM-FAELT-FEL                                       
066900                          TO MOD-IDORDNR7-UPD-ATTR(INDX)                  
067000             MOVE NEJ   TO INDATA-SW                                      
067100           ELSE                                                           
067200             MOVE MFS-NUM-FAELT-RAETT                                     
067300                          TO MOD-IDORDNR7-UPD-ATTR(INDX)                  
067400             MOVE SPACE TO W-4A1-IDKUNDRF                                 
067500             MOVE MID-IDORDNR7-UPD(INDX)(3:5)                             
067600                          TO W-4A1-IDORDNR                                
067700           END-IF                                                         
067800         END-IF                                                           
067900       END-IF                                                             
068000                                                                          
068100       IF MID-IDKOLLI-UPD(INDX)  NOT = ALL '+'                            
068200         IF MID-IDKOLLI-UPD(INDX) NOT NUMERIC                             
068300           MOVE MFS-NUM-FAELT-FEL TO MOD-IDKOLLI-UPD-ATTR(INDX)           
068400           MOVE NEJ              TO INDATA-SW                             
068500         ELSE                                                             
068600           MOVE MFS-NUM-FAELT-RAETT                                       
068700                                 TO MOD-IDKOLLI-UPD-ATTR(INDX)            
068800           MOVE MID-IDKOLLI-UPD(INDX)                                     
068900                                 TO W-IDKOLLI                             
069000         END-IF                                                           
069100       END-IF                                                             
069200     ELSE                                                                 
069300       IF MID-IDDISTR-UPD(INDX)  = ZERO AND                               
069400          MID-IDKUNDNR-UPD(INDX) = ZERO AND                               
069500          MID-IDORDNR7-UPD(INDX) = ZERO AND                               
069600         (MID-IDKOLLI-UPD(INDX) NUMERIC AND                               
069700          MID-IDKOLLI-UPD(INDX)  > ZERO)                                  
069800         MOVE MFS-NUM-FAELT-RAETT                                         
069900                                 TO MOD-IDKOLLI-UPD-ATTR(INDX)            
070000         MOVE JA                 TO SAMKOLLI-SW                           
071000       ELSE                                                               
071100         MOVE MFS-NUM-FAELT-FEL  TO MOD-IDKOLLI-UPD-ATTR(INDX)            
071200         MOVE NEJ                TO INDATA-SW                             
071300       END-IF                                                             
071400     END-IF                                                               
071500                                                                          
071600     IF INDATA-OK                                                         
071700       IF SAMKOLLI-FINNS                                                  
071800         PERFORM DAA-KOLLA-INDATA-SK                                      
071900       ELSE                                                               
072000         PERFORM DAB-KOLLA-INDATA-KOLLI                                   
073000       END-IF                                                             
074000     END-IF                                                               
075000     .                                                                    
076000     EJECT                                                                
077000 DAA-KOLLA-INDATA-SK SECTION.                                             
078000                                                                          
078100     MOVE MID-IDKOLLI-UPD(INDX) TO W-IDKOLLIS                             
078200     PERFORM IMS-GU-WDE711-ASEQ                                           
078300     IF SEGMENT-FINNS                                                     
078400       IF SKLI-KDSTASKLI = SK-CLOSED   AND                                
078500          SKLI-IDTRPTNR  = W-IDTRPTNR                                     
078600                                                                          
078700         IF ((SKLI-FLFARLIG  = JA  AND (WS-FLFARLIG = JA      OR          
078800                                        WS-FLFARLIG = MIXED)) OR          
078900             (SKLI-FLFARLIG  = NEJ AND WS-FLFARLIG = NEJ))    OR          
079000             (SKLI-FLFARLIG  = NEJ AND WS-FLFARLIG = MIXED)               
079100                                                                          
079200           MOVE MID-IDKOLLI-UPD (INDX)                                    
079300                                  TO TABELL-IDKOLLI-SAMP(INDX)            
079400           MOVE ZERO            TO TABELL-IDPRODNR(INDX)                  
079500           MOVE NEJ             TO SAMKOLLI-SW                            
079600         ELSE                                                             
079700           MOVE W-FEL-795       TO W-MED-IDMFSFEL                         
079800           MOVE MFS-ADD-SAETT-CURSOR                                      
079900                                TO MOD-IDDISTR-UPD-ATTR(INDX)             
080000                                   MOD-IDKUNDNR-UPD-ATTR(INDX)            
080100                                   MOD-IDORDNR7-UPD-ATTR(INDX)            
080200                                   MOD-IDKOLLI-UPD-ATTR(INDX)             
080300           MOVE NEJ             TO INDATA-SW                              
080400         END-IF                                                           
080500       ELSE                                                               
080600         MOVE W-FEL-795         TO W-MED-IDMFSFEL                         
080700         MOVE MFS-ADD-SAETT-CURSOR                                        
080800                                TO MOD-IDDISTR-UPD-ATTR(INDX)             
080900                                   MOD-IDKUNDNR-UPD-ATTR(INDX)            
081000                                   MOD-IDORDNR7-UPD-ATTR(INDX)            
081100                                   MOD-IDKOLLI-UPD-ATTR(INDX)             
081200         MOVE NEJ               TO INDATA-SW                              
081300       END-IF                                                             
081400     ELSE                                                                 
081500       MOVE W-FEL-758           TO W-MED-IDMFSFEL                         
081600       MOVE MFS-ADD-SAETT-CURSOR                                          
081700                                TO MOD-IDDISTR-UPD-ATTR(INDX)             
081800                                   MOD-IDKUNDNR-UPD-ATTR(INDX)            
081900                                   MOD-IDORDNR7-UPD-ATTR(INDX)            
082000                                   MOD-IDKOLLI-UPD-ATTR(INDX)             
082100       MOVE NEJ                 TO INDATA-SW                              
082200     END-IF                                                               
082300     .                                                                    
082400     EJECT                                                                
082500 DAB-KOLLA-INDATA-KOLLI SECTION.                                          
082600                                                                          
082700     PERFORM IMS-GU-WDE401-ASEK                                           
082800                                                                          
082900     IF SEGMENT-FINNS                                                     
083000       MOVE NEJ       TO TRAFF-IDDC-SW                                    
083100       MOVE NEJ       TO TRAFF-IDKOLLI-SW                                 
083200                                                                          
083300       PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR                      
083400                     TRAFF-IDKOLLI OR INDATA-FEL                          
083500         MOVE WS-IDDC-IN TO WS-IDDC                                       
083600         MOVE NEJ     TO TRAFF-IDDC-SW                                    
083700         IF GOOD-DDC                                                      
083800           IF KORD-IDDC = W-IDDC     AND                                  
083900              KORD-KVORDRAD = ZERO                                        
084000             MOVE JA       TO TRAFF-IDDC-SW                               
084100           END-IF                                                         
084200         ELSE                                                             
084300           IF GOOD-DC                                                     
084400              IF KORD-IDDC = W-IDDC AND                                   
084500                 KORD-KVORDRAD-LEVPL = ZERO                               
084600                MOVE JA    TO TRAFF-IDDC-SW                               
084700              END-IF                                                      
084800           END-IF                                                         
084900         END-IF                                                           
085000         IF TRAFF-IDDC                                                    
085100           MOVE NEJ        TO TRAFF-IDKOLLI-SW                            
085200           MOVE KORD-IDPRODNR TO W-IDPRODNR                               
085300                                 TABELL-IDPRODNR(INDX)                    
085400           MOVE ZERO       TO TABELL-IDKOLLI-SAMP(INDX)                   
085500                                                                          
085600           PERFORM IMS-GU-WDE611                                          
085700           IF SEGMENT-FINNS                                               
085800                                                                          
085900             IF KOLLI-KDKOLSTA NOT         = KLI-PACK                     
086000             OR KOLLI-IDTRPTNR NOT         = W-IDTRPTNR                   
086100             OR KOLLI-FLUTLAST NOT         = JA                           
086200             OR (WS-FLFARLIG = JA  AND KOLLI-IDPSN(1) = ZERO)             
086300             OR (WS-FLFARLIG = NEJ AND KOLLI-IDPSN(1) > ZERO)             
086400                                                                          
086500               MOVE W-FEL-795              TO W-MED-IDMFSFEL              
086600               MOVE MFS-ADD-SAETT-CURSOR                                  
086700                           TO MOD-IDDISTR-UPD-ATTR(INDX)                  
086800                              MOD-IDKUNDNR-UPD-ATTR(INDX)                 
086900                              MOD-IDORDNR7-UPD-ATTR(INDX)                 
087000                              MOD-IDKOLLI-UPD-ATTR(INDX)                  
087100               MOVE NEJ    TO INDATA-SW                                   
087200             ELSE                                                         
087300               MOVE JA     TO TRAFF-IDKOLLI-SW                            
087400             END-IF                                                       
087500           ELSE                                                           
087600*            INGET 611-SEGM HITTAT                                        
087700             PERFORM IMS-GN-WDE401-ASEK                                   
087800                                                                          
087900           END-IF                                                         
088000         ELSE                                                             
088100           PERFORM IMS-GN-WDE401-ASEK                                     
088200         END-IF                                                           
088300       END-PERFORM                                                        
088400       IF EJ-TRAFF-IDKOLLI                                                
088500         MOVE W-FEL-758                   TO W-MED-IDMFSFEL               
088600         MOVE MFS-ADD-SAETT-CURSOR                                        
088700                              TO MOD-IDDISTR-UPD-ATTR(INDX)               
088800         MOVE NEJ             TO INDATA-SW                                
088900       END-IF                                                             
089000     ELSE                                                                 
089100       MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-UPD-ATTR(INDX)               
089200                                 MOD-IDKUNDNR-UPD-ATTR(INDX)              
089300                                 MOD-IDORDNR7-UPD-ATTR(INDX)              
089400                                 MOD-IDKOLLI-UPD-ATTR(INDX)               
089500       MOVE NEJ               TO INDATA-SW                                
089600     END-IF                                                               
089700     .                                                                    
089800     EJECT                                                                
089900 E-UPPDATERA SECTION.                                                     
090000                                                                          
090100     MOVE ZERO             TO W-KVKOLLI                                   
090200                              W-VKORDBTO                                  
090300                              W-VLORDBTO                                  
090400                              W-SUORDV                                    
090500                                                                          
090600     PERFORM IMS-GU-WDGX4495                                              
090700     IF SEGMENT-SAKNAS                                                    
090800       MOVE LOW-VALUE      TO 4495-LOW-VALUE                              
090900       MOVE '4495'         TO 4495-IDHTYP                                 
091000       MOVE W-IDDC         TO 4495-IDDC                                   
091100       MOVE W-IDTRPTNR     TO 4495-IDTRPTNR                               
091200       MOVE W-IDLBBET      TO 4495-IDLBBET                                
091300                                                                          
091400       PERFORM IMS-ISRT-WDGX4495                                          
091500                                                                          
091600     END-IF                                                               
091700                                                                          
091800                                                                          
091900     PERFORM EA-UPPDAT-REG-RADER                                          
092000                                                                          
092800     MOVE INF-UPDATE-DONE  TO MED-IDMFSINF                                
092900     CALL WMEDKONV USING MED-WMEDAREA                                     
093000     MOVE MED-MFSINF       TO MOD-TEMFSINF                                
093100     PERFORM MFS-FORM-ATTR                                                
093200     PERFORM MFS-RENSA-FAELT-IN                                           
093210     MOVE MFS-ADD-SAETT-CURSOR TO MOD-IDDISTR-UPD-ATTR (1)                
093300     .                                                                    
093400     EJECT                                                                
093500 EA-UPPDAT-REG-RADER  SECTION.                                            
093600                                                                          
093700     MOVE +1                           TO INDX                            
093800     PERFORM UNTIL INDX  >  MAX-INDX                                      
093900       IF TABELL-IDKOLLI-SAMP (INDX) > ZERO                               
094000*        SAMLINGSKOLLI FINNS                                              
094100         PERFORM EAA-UPPDAT-WDE6-ARB                                      
094200       ELSE                                                               
094300         IF MID-IDDISTR-UPD(INDX) NUMERIC                                 
094400           MOVE TABELL-IDPRODNR(INDX)  TO W-IDPRODNR                      
094500           MOVE MID-IDKOLLI-UPD(INDX)  TO W-IDKOLLI                       
094600           MOVE MID-IDDISTR-UPD(INDX)  TO TEST-IDDISTR                    
094700                                                                          
094800           PERFORM EAB-UPPDATERA-WDE6                                     
094900                                                                          
095000           MOVE MID-IDDISTR-UPD(INDX)  TO 4498-IDDISTR                    
095100           MOVE MID-IDKUNDNR-UPD(INDX) TO 4498-IDKUNDNR                   
095200                                          4498-IDDEALER                   
095300           MOVE SPACE                  TO 4498-IDKUNDRF                   
095400           INSPECT MID-IDORDNR7-UPD(INDX)                                 
095500           REPLACING LEADING SPACE BY ZERO                                
095600           MOVE MID-IDORDNR7-UPD(INDX) TO 4498-IDORDNR7                   
095700           MOVE TABELL-IDPRODNR(INDX)  TO 4498-IDPRODNR                   
095800           PERFORM EAC-UPPDATERA-ARBREG-RAD                               
095900         END-IF                                                           
096000       END-IF                                                             
096100       ADD +1                          TO INDX                            
096200     END-PERFORM                                                          
096300     PERFORM EAD-UPPDATERA-ARBREG-TOT                                     
096400     .                                                                    
096500     EJECT                                                                
096600 EAA-UPPDAT-WDE6-ARB   SECTION.                                           
096700                                                                          
096800     MOVE TABELL-IDKOLLI-SAMP(INDX) TO W-IDKOLLIS                         
096900     PERFORM IMS-GHU-WDE711-ASEQ-2                                        
097000     ADD SKLI-VKKOLLIB-SAMP         TO W-VKORDBTO                         
097100     ADD SKLI-VLKOLLIB-SAMP         TO W-VLORDBTO                         
097200     MOVE SK-SHIPPED                TO SKLI-KDSTASKLI                     
097300     PERFORM IMS-REPL-WDE711                                              
097400*    ETT KVKOLLI / SAMLINGSKOLLI                                          
097500     ADD 1                          TO W-KVKOLLI                          
097600     MOVE SKLI-IDKOLLI-SAMP         TO 4497-IDKOLLI-SAMP                  
097700     MOVE SKLI-VKKOLLIB-SAMP        TO 4497-VKKOLLIB-SAMP                 
097800     MOVE SKLI-VLKOLLIB-SAMP        TO 4497-VLKOLLIB-SAMP                 
097900     MOVE SKLI-FLFARLIG             TO 4497-FLFARLIG                      
098000     MOVE SKLI-KDKOLLI-SAMP         TO 4497-KDKOLLI-SAMP                  
098100     PERFORM IMS-ISRT-WDGX4497                                            
098200                                                                          
098300     PERFORM IMS-GNP-WDE721                                               
098400     PERFORM UNTIL SEGMENT-SAKNAS                                         
098500       PERFORM EAAA-UPPDAT-WDE6                                           
098600       PERFORM EAAB-UPPDAT-4498                                           
098700                                                                          
098800       PERFORM IMS-GNP-WDE721                                             
098900     END-PERFORM                                                          
099000     .                                                                    
099100     EJECT                                                                
099200 EAAA-UPPDAT-WDE6 SECTION.                                                
099300                                                                          
099400     MOVE SKOR-IDPRODNR           TO W-IDPRODNR                           
099500     MOVE SKOR-IDKOLLI            TO W-IDKOLLI                            
099600     PERFORM IMS-GHU-WDE611                                               
099700                                                                          
100000     MOVE W-IDLBBET               TO KOLLI-IDLBBET                        
101000     MOVE KLI-PACK-FAKT           TO KOLLI-KDKOLSTA                       
102000     MOVE SK-SHIPPED              TO KOLLI-KDSTASKLI                      
103000     MOVE MSGI-TILOKDAT           TO KOLLI-TILASTN                        
103100                                                                          
103200     MOVE MSGI-TILOKTID           TO WS-TILASTID-HHMM                     
103300     MOVE FUNCTION CURRENT-DATE (13:2)                                    
103400                                  TO WS-TILASTID-SS                       
103500     MOVE WS-TILASTID             TO KOLLI-TILASTID                       
103600                                                                          
103700     PERFORM IMS-REPL-WDE611                                              
103800                                                                          
103900     PERFORM IMS-GHU-WDE601                                               
104000     ADD +1                       TO VORD-KVKOLLI-FL                      
104100     MOVE SKOR-IDDISTR            TO TEST-IDDISTR                         
104200     IF DIST79-DEALER-PRICE                                               
104300        ADD KOLLI-SUORDV-LOC      TO VORD-SUORDV-FL-LOC                   
104400        ADD KOLLI-SUORDV-LOCPREL  TO VORD-SUORDV-FL-LOCPREL               
104500                                                                          
104600        ADD KOLLI-SUORDV-LOC      TO W-SUORDV                             
104700        ADD KOLLI-SUORDV-LOCPREL  TO W-SUORDV                             
104710     ELSE                                                                 
104712       IF DIST79-ECOM-PRICE                                               
104720          ADD KOLLI-SUORDV-LOC    TO VORD-SUORDV-FL-LOC                   
104730                                     W-SUORDV                             
104800       ELSE                                                               
104900          ADD KOLLI-SUORDV-KOLLI  TO VORD-SUORDV-FL                       
105000                                     W-SUORDV                             
105100       END-IF                                                             
105110     END-IF                                                               
105200     ADD KOLLI-VKORDBTO-KOLLI     TO VORD-VKORDBTO-FL                     
105300     ADD KOLLI-VLORDBTO-KOLLI     TO VORD-VLORDBTO-FL                     
105400     PERFORM IMS-REPL-WDE601                                              
105500     .                                                                    
105600     EJECT                                                                
105700 EAAB-UPPDAT-4498 SECTION.                                                
105800                                                                          
105900     MOVE SKOR-IDDISTR            TO 4498-IDDISTR                         
106000     MOVE SKOR-IDKUNDNR           TO 4498-IDKUNDNR                        
106100     MOVE SKOR-IDKUNDNR           TO 4498-IDDEALER                        
106200     MOVE SKOR-IDORDNR7           TO 4498-IDKUNDRF                        
106300     MOVE SKOR-IDPRODNR           TO 4498-IDPRODNR                        
106400     MOVE VORD-KDFAKTYP           TO 4498-KDFAKTYP                        
106500     MOVE KOLLI-IDKOLLI           TO 4498-IDKOLLI                         
106600     MOVE KOLLI-IDKOLLI-SAMP      TO 4498-IDKOLLI-SAMP                    
106700     MOVE KOLLI-IDPSN(1)          TO 4498-IDPSN(1)                        
106800     MOVE KOLLI-IDPSN(2)          TO 4498-IDPSN(2)                        
106900     MOVE KOLLI-KDKOLLI           TO 4498-KDKOLLI                         
107000     MOVE KOLLI-DARFS (3:10)      TO 4498-TIRFS                           
107100     MOVE KOLLI-VLORDBTO-KOLLI    TO 4498-VLORDBTO                        
107200     MOVE KOLLI-VKORDBTO-KOLLI    TO 4498-VKORDBTO                        
107210     MOVE NEJ                     TO 4498-FLCROSS                         
107300                                                                          
107400     PERFORM IMS-ISRT-WDGX4498                                            
107500     .                                                                    
107600     EJECT                                                                
107700 EAB-UPPDATERA-WDE6    SECTION.                                           
107800                                                                          
107900     PERFORM IMS-GHU-WDE601                                               
108000     ADD  +1                      TO VORD-KVKOLLI-FL                      
108100     IF DIST79-DEALER-PRICE                                               
108200        ADD KOLLI-SUORDV-LOC      TO VORD-SUORDV-FL-LOC                   
108300        ADD KOLLI-SUORDV-LOCPREL  TO VORD-SUORDV-FL-LOCPREL               
108400                                                                          
108500        ADD KOLLI-SUORDV-LOC      TO W-SUORDV                             
108600        ADD KOLLI-SUORDV-LOCPREL  TO W-SUORDV                             
108610     ELSE                                                                 
108612       IF DIST79-ECOM-PRICE                                               
108620          ADD KOLLI-SUORDV-LOC    TO VORD-SUORDV-FL-LOC                   
108630                                     W-SUORDV                             
108700       ELSE                                                               
108800          ADD KOLLI-SUORDV-KOLLI  TO VORD-SUORDV-FL                       
108900                                     W-SUORDV                             
109000       END-IF                                                             
109010     END-IF                                                               
109100     ADD  KOLLI-VKORDBTO-KOLLI    TO VORD-VKORDBTO-FL                     
109200     ADD  KOLLI-VLORDBTO-KOLLI    TO VORD-VLORDBTO-FL                     
109300     PERFORM IMS-REPL-WDE601                                              
109400                                                                          
109500     PERFORM IMS-GHNP-WDE611                                              
109600     ADD 1                        TO W-KVKOLLI                            
109700     ADD KOLLI-VKORDBTO-KOLLI     TO W-VKORDBTO                           
109800     ADD KOLLI-VLORDBTO-KOLLI     TO W-VLORDBTO                           
109900                                                                          
110000     MOVE NEJ                     TO KOLLI-FLUTLAST                       
110100     MOVE W-IDLBBET               TO KOLLI-IDLBBET                        
110200     MOVE KLI-PACK-FAKT           TO KOLLI-KDKOLSTA                       
110300     MOVE KOLLI-IDKOLLI-SAMP      TO W-SPAR-IDKOLLI-SAMP                  
110400     MOVE ZERO                    TO KOLLI-IDKOLLI-SAMP                   
110500     MOVE MSGI-TILOKDAT           TO KOLLI-TILASTN                        
110600                                                                          
110700     MOVE MSGI-TILOKTID           TO WS-TILASTID-HHMM                     
110800     MOVE FUNCTION CURRENT-DATE (13:2)                                    
110900                                 TO WS-TILASTID-SS                        
111000     MOVE WS-TILASTID            TO KOLLI-TILASTID                        
111100                                                                          
111200     PERFORM IMS-REPL-WDE611                                              
111300     .                                                                    
111400     EJECT                                                                
111500 EAC-UPPDATERA-ARBREG-RAD  SECTION.                                       
111600                                                                          
111700     MOVE VORD-KDFAKTYP          TO 4498-KDFAKTYP                         
111800     MOVE KOLLI-IDKOLLI          TO 4498-IDKOLLI                          
111900     MOVE KOLLI-IDKOLLI-SAMP     TO 4498-IDKOLLI-SAMP                     
112000     MOVE KOLLI-IDPSN(1)         TO 4498-IDPSN(1)                         
112100     MOVE KOLLI-IDPSN(2)         TO 4498-IDPSN(2)                         
112200     MOVE KOLLI-KDKOLLI          TO 4498-KDKOLLI                          
112300     MOVE KOLLI-DARFS (3:10)     TO 4498-TIRFS                            
112400     MOVE KOLLI-VLORDBTO-KOLLI   TO 4498-VLORDBTO                         
112500     MOVE KOLLI-VKORDBTO-KOLLI   TO 4498-VKORDBTO                         
112510     MOVE NEJ                    TO 4498-FLCROSS                          
112600                                                                          
112700     PERFORM IMS-ISRT-WDGX4498                                            
112800     .                                                                    
112900     EJECT                                                                
113000 EAD-UPPDATERA-ARBREG-TOT  SECTION.                                       
113100                                                                          
113200     PERFORM IMS-GHU-WDGX4496                                             
113300                                                                          
113400     IF SEGMENT-FINNS                                                     
113500        ADD W-KVKOLLI        TO 4496-KVKOLLI-LAST                         
113600        ADD W-VKORDBTO       TO 4496-VKORDBTO-LASTB                       
113700        ADD W-VLORDBTO       TO 4496-VLORDBTO-LASTB                       
113800        ADD W-SUORDV         TO 4496-SUORDV-LASTB                         
113900                                                                          
114000        PERFORM IMS-REPL-WDGX4496                                         
114100     ELSE                                                                 
114200        MOVE '1'             TO 4496-KDSEGKEY                             
114300        MOVE W-KVKOLLI       TO 4496-KVKOLLI-LAST                         
114400        MOVE W-VKORDBTO      TO 4496-VKORDBTO-LASTB                       
114500        MOVE W-VLORDBTO      TO 4496-VLORDBTO-LASTB                       
114600        MOVE W-SUORDV        TO 4496-SUORDV-LASTB                         
114700                                                                          
114800        PERFORM IMS-ISRT-WDGX4496                                         
114900     END-IF                                                               
115000     .                                                                    
115100     EJECT                                                                
115200 F-KOLLA-OM-FELTRYCK SECTION.                                             
115300                                                                          
115400     IF EGEN-MID OR HELP-MID                                              
115500       MOVE +1 TO INDX                                                    
115600       MOVE NEJ TO SW-TRAEFF                                              
115700       PERFORM UNTIL INDX > MAX-INDX                                      
115800         IF MID-UPD(INDX) = ALL '+'                                       
115900           CONTINUE                                                       
116000         ELSE                                                             
116100           MOVE JA TO SW-TRAEFF                                           
116200         END-IF                                                           
116300         ADD +1 TO INDX                                                   
116400       END-PERFORM                                                        
116500       IF TRAEFF                                                          
116600         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
116700         CALL WMEDKONV USING MED-WMEDAREA                                 
116800         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
116900         PERFORM FA-MID-INDATA-TILL-MOD                                   
117000       END-IF                                                             
117100     ELSE                                                                 
117200       PERFORM MFS-RENSA-FAELT-IN                                         
117300       PERFORM MFS-RENSA-FAELT-MID                                        
117400     END-IF                                                               
117500                                                                          
117600     .                                                                    
117700     EJECT                                                                
117800 FA-MID-INDATA-TILL-MOD SECTION.                                          
117900                                                                          
118000     MOVE +1                        TO INDX                               
118100                                                                          
118200     PERFORM UNTIL INDX > MAX-INDX                                        
118300                                                                          
118400       IF  MID-IDDISTR-UPD(INDX)    =  ALL '+'                            
118500         MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR-UPD(INDX)              
118600       ELSE                                                               
118700         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDISTR-UPD-ATTR(INDX)         
118800         MOVE MID-IDDISTR-UPD(INDX) TO MOD-IDDISTR-UPD(INDX)              
118900       END-IF                                                             
119000                                                                          
119100       IF  MID-IDKUNDNR-UPD(INDX)   =  ALL '+'                            
119200         MOVE MFS-RENSA-FAELT       TO MOD-IDKUNDNR-UPD(INDX)             
119300       ELSE                                                               
119400         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKUNDNR-UPD-ATTR(INDX)        
119500         MOVE MID-IDKUNDNR-UPD(INDX) TO MOD-IDKUNDNR-UPD(INDX)            
119600       END-IF                                                             
119700                                                                          
119800       IF  MID-IDORDNR7-UPD(INDX)   =  ALL '+'                            
119900         MOVE MFS-RENSA-FAELT       TO MOD-IDORDNR7-UPD(INDX)             
120000       ELSE                                                               
120100         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDORDNR7-UPD-ATTR(INDX)        
120200         MOVE MID-IDORDNR7-UPD(INDX) TO MOD-IDORDNR7-UPD(INDX)            
120300       END-IF                                                             
120400                                                                          
120500       IF  MID-IDKOLLI-UPD(INDX)    =  ALL '+'                            
120600         MOVE MFS-RENSA-FAELT       TO MOD-IDKOLLI-UPD(INDX)              
120700       ELSE                                                               
120800         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKOLLI-UPD-ATTR(INDX)         
120900         MOVE MID-IDKOLLI-UPD(INDX) TO MOD-IDKOLLI-UPD(INDX)              
121000       END-IF                                                             
121100                                                                          
121200       ADD +1                       TO INDX                               
121300     END-PERFORM                                                          
121400                                                                          
121500     .                                                                    
121600     EJECT                                                                
121700 MFS-RENSA-FAELT-UT SECTION.                                              
121800                                                                          
121900*    --- ALLA UTDATA-FÄLT                                                 
122000     MOVE MFS-RENSA-FAELT     TO MOD-KVKOLLI-VALD                         
122100                                 MOD-VKORDBTO-VALD                        
122200                                 MOD-VLORDBTO-VALD                        
122300     .                                                                    
122400     SKIP3                                                                
122500 MFS-RENSA-FAELT-IN SECTION.                                              
122600                                                                          
122700*    --- ALLA INDATA-FÄLT                                                 
122800     MOVE +1                  TO INDX                                     
122900     PERFORM  UNTIL INDX  > MAX-INDX                                      
123000       MOVE MFS-RENSA-FAELT   TO MOD-IDDISTR-UPD(INDX)                    
123100                                 MOD-IDKUNDNR-UPD(INDX)                   
123200                                 MOD-IDORDNR7-UPD(INDX)                   
123300                                 MOD-IDKOLLI-UPD(INDX)                    
123400       ADD +1                 TO INDX                                     
123500     END-PERFORM                                                          
123600     .                                                                    
123700     EJECT                                                                
123800 MFS-CLOSE-FAELT-ATTR SECTION.                                            
123900                                                                          
124000*    --- ALLA INDATA-FÄLT                                                 
124100     MOVE +1                  TO INDX                                     
124200     PERFORM  UNTIL INDX  > MAX-INDX                                      
124300       MOVE MFS-CLOSE-FIELD   TO MOD-IDDISTR-UPD-ATTR(INDX)               
124400                                 MOD-IDKUNDNR-UPD-ATTR(INDX)              
124500                                 MOD-IDORDNR7-UPD-ATTR(INDX)              
124600                                 MOD-IDKOLLI-UPD-ATTR(INDX)               
124700       ADD +1                 TO INDX                                     
124800     END-PERFORM                                                          
124900     .                                                                    
125000     EJECT                                                                
125100 MFS-OPEN-FAELT-ATTR SECTION.                                             
125200                                                                          
125300*    --- ALLA INDATA-FÄLT                                                 
125400     MOVE +1                  TO INDX                                     
125500     PERFORM  UNTIL INDX  > MAX-INDX                                      
125600       MOVE MFS-OPEN-NUM-FIELD TO MOD-IDDISTR-UPD-ATTR(INDX)              
125700                                  MOD-IDKUNDNR-UPD-ATTR(INDX)             
125800                                  MOD-IDORDNR7-UPD-ATTR(INDX)             
125900                                  MOD-IDKOLLI-UPD-ATTR(INDX)              
126000       ADD +1                  TO INDX                                    
126100     END-PERFORM                                                          
126200     .                                                                    
126300     EJECT                                                                
126400 MFS-RENSA-FAELT-MID SECTION.                                             
126500                                                                          
126600*    --- ALLA MID-FÄLT                                                    
126700     MOVE +1                   TO INDX                                    
126800     PERFORM  UNTIL INDX  > MAX-INDX                                      
126900       MOVE MFS-RENSA-FAELT    TO MID-IDDISTR-UPD(INDX)                   
127000                                  MID-IDKUNDNR-UPD(INDX)                  
127100                                  MID-IDORDNR7-UPD(INDX)                  
127200                                  MID-IDKOLLI-UPD(INDX)                   
127300       ADD +1                  TO INDX                                    
127400     END-PERFORM                                                          
127500     .                                                                    
127600     EJECT                                                                
127700 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
127800                                                                          
127900*    --- ALLA UTDATA-FÄLT                                                 
128000     MOVE MFS-ROER-EJ-FAELT    TO MOD-KVKOLLI-VALD                        
128100                                  MOD-VKORDBTO-VALD                       
128200                                  MOD-VLORDBTO-VALD                       
128300                                                                          
128400     MOVE +1                   TO INDX                                    
128500     .                                                                    
128600     SKIP2                                                                
128700 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
128800                                                                          
128900*    --- ALLA INDATA-FÄLT                                                 
129000     MOVE +1                  TO INDX                                     
129100     PERFORM  UNTIL INDX > MAX-INDX                                       
129200       MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR-UPD(INDX)                    
129300                                 MOD-IDKUNDNR-UPD(INDX)                   
129400                                 MOD-IDORDNR7-UPD(INDX)                   
129500                                 MOD-IDKOLLI-UPD(INDX)                    
129600       ADD +1                 TO INDX                                     
129700     END-PERFORM                                                          
129800     .                                                                    
129900     EJECT                                                                
130000 MFS-FORM-ATTR SECTION.                                                   
130100                                                                          
130200*    --- ALLA INDATA-FÄLT                                                 
130300     MOVE +1                   TO INDX                                    
130400     PERFORM  UNTIL INDX  > MAX-INDX                                      
130500       MOVE MFS-FORMATETS-ATTR TO MOD-IDDISTR-UPD-ATTR(INDX)              
130600                                  MOD-IDKUNDNR-UPD-ATTR(INDX)             
130700                                  MOD-IDORDNR7-UPD-ATTR(INDX)             
130800                                  MOD-IDKOLLI-UPD-ATTR(INDX)              
130900       ADD +1                  TO INDX                                    
131000     END-PERFORM                                                          
131100     .                                                                    
131200     EJECT                                                                
131300* --- IMS SEKTIONER ---                                                   
131400     SKIP3                                                                
131500 IMS-GET-MSG SECTION.                                                     
131600                                                                          
131700     MOVE '  QC' TO GODK-STATUSKODER                                      
131800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
131900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
132000     PERFORM IMS-STATUSKONTROLL                                           
132100     .                                                                    
132200     SKIP3                                                                
132300 IMS-INSERT-MSG SECTION.                                                  
132400                                                                          
132500     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
132600       MOVE '0' TO MFS-KDHUVOMR                                           
132700     END-IF                                                               
132800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
132900     MOVE SPACE TO GODK-STATUSKODER                                       
133000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
133100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
133200     PERFORM IMS-STATUSKONTROLL                                           
133300     .                                                                    
133400     EJECT                                                                
133500 IMS-GU-WDE711-ASEQ SECTION.                                              
133600                                                                          
133700     STRING 'WDE711  (WDE7ASEQ =' W-WDE7ASEQ-X ')'                        
133800            DELIMITED BY SIZE INTO SSA1                                   
133900     MOVE '  GE' TO GODK-STATUSKODER                                      
134000     CALL CBLTDLI USING GU WDE7-PCB DLI-IO-E711 SSA1                      
134100     MOVE WDE7-STATUS-CODE TO STATUS-WS                                   
134200                                                                          
134300     PERFORM IMS-STATUSKONTROLL                                           
134400     .                                                                    
134500     SKIP2                                                                
134600 IMS-GHU-WDE711-ASEQ-2 SECTION.                                           
134700                                                                          
134800     STRING 'WDE711  (WDE7ASEQ =' W-WDE7ASEQ-X ')'                        
134900            DELIMITED BY SIZE INTO SSA1                                   
135000     MOVE '  ' TO GODK-STATUSKODER                                        
135100     CALL CBLTDLI USING GHU WDE7-PCB DLI-IO-E711 SSA1                     
135200     MOVE WDE7-STATUS-CODE TO STATUS-WS                                   
135300                                                                          
135400     PERFORM IMS-STATUSKONTROLL                                           
135500     .                                                                    
135600     SKIP2                                                                
135700 IMS-REPL-WDE711 SECTION.                                                 
135800                                                                          
135900     MOVE '  ' TO GODK-STATUSKODER                                        
136000     CALL CBLTDLI USING REPL WDE7-PCB DLI-IO-E711                         
136100     MOVE WDE7-STATUS-CODE TO STATUS-WS                                   
136200     PERFORM IMS-STATUSKONTROLL                                           
136300     .                                                                    
136400     SKIP2                                                                
136500 IMS-GNP-WDE721 SECTION.                                                  
136600                                                                          
136700     MOVE 'WDE721 ' TO SSA1                                               
136800     MOVE '  GE' TO GODK-STATUSKODER                                      
136900     CALL CBLTDLI USING GNP WDE7-PCB DLI-IO-E721 SSA1                     
137000     MOVE WDE7-STATUS-CODE TO STATUS-WS                                   
137100     PERFORM IMS-STATUSKONTROLL                                           
137200     .                                                                    
137300     EJECT                                                                
137400 IMS-GHU-WDE601 SECTION.                                                  
137500                                                                          
137600     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
137700            DELIMITED BY SIZE INTO SSA1                                   
137800     MOVE '  ' TO GODK-STATUSKODER                                        
137900     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-E601 SSA1                     
138000     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
138100                                                                          
138200     PERFORM IMS-STATUSKONTROLL                                           
138300     .                                                                    
138400     SKIP2                                                                
138500 IMS-GHNP-WDE611 SECTION.                                                 
138600                                                                          
138700     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
138800            DELIMITED BY SIZE INTO SSA1                                   
138900     MOVE '  ' TO GODK-STATUSKODER                                        
139000     CALL CBLTDLI USING GHNP WDE6-PCB DLI-IO-E611 SSA1                    
139100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
139200     PERFORM IMS-STATUSKONTROLL                                           
139300     .                                                                    
139400     SKIP2                                                                
139500 IMS-REPL-WDE601 SECTION.                                                 
139600                                                                          
139700     MOVE '  ' TO GODK-STATUSKODER                                        
139800     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-E601                         
139900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
140000     PERFORM IMS-STATUSKONTROLL                                           
140100     .                                                                    
140200     SKIP2                                                                
140300 IMS-REPL-WDE611 SECTION.                                                 
140400                                                                          
140500     MOVE '  ' TO GODK-STATUSKODER                                        
140600     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-E611                         
140700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
140800     PERFORM IMS-STATUSKONTROLL                                           
140900     .                                                                    
141000     EJECT                                                                
141100 IMS-GU-WDE401-ASEK       SECTION.                                        
141200                                                                          
141300     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
141400     DELIMITED BY SIZE INTO SSA1                                          
141500     MOVE '  GE' TO GODK-STATUSKODER                                      
141600     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-E401 SSA1                      
141700     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
141800     PERFORM IMS-STATUSKONTROLL                                           
141900     .                                                                    
142000     SKIP2                                                                
142100 IMS-GN-WDE401-ASEK       SECTION.                                        
142200                                                                          
142300     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
142400     DELIMITED BY SIZE INTO SSA1                                          
142500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
142600     CALL CBLTDLI USING GN WDE4-PCB DLI-IO-E401 SSA1                      
142700     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
142800     PERFORM IMS-STATUSKONTROLL                                           
142900     .                                                                    
143000     EJECT                                                                
143100 IMS-GU-WDE611 SECTION.                                                   
143200                                                                          
143300     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
143400                      DELIMITED BY SIZE INTO SSA1                         
143500     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
143600                      DELIMITED BY SIZE INTO SSA2                         
143700     MOVE '  GE' TO GODK-STATUSKODER                                      
143800     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-E611 SSA1 SSA2                 
143900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
144000     PERFORM IMS-STATUSKONTROLL                                           
144100     .                                                                    
144200     EJECT                                                                
144300 IMS-GHU-WDE611 SECTION.                                                  
144400                                                                          
144500     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
144600                      DELIMITED BY SIZE INTO SSA1                         
144700     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
144800                      DELIMITED BY SIZE INTO SSA2                         
144900     MOVE '  GE' TO GODK-STATUSKODER                                      
145000     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-E611 SSA1 SSA2                
145100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
145200     PERFORM IMS-STATUSKONTROLL                                           
145300     .                                                                    
145400     EJECT                                                                
145500 IMS-GU-WDGX4495  SECTION.                                                
145600     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
145700                      DELIMITED BY SIZE INTO SSA1                         
145800     MOVE '  GE' TO GODK-STATUSKODER                                      
145900     CALL CBLTDLI USING GU 4495-PCB DLI-IO-WDGX4495 SSA1                  
146000     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
146100     PERFORM IMS-STATUSKONTROLL                                           
146200     .                                                                    
146300     SKIP2                                                                
146400 IMS-ISRT-WDGX4495    SECTION.                                            
146500     MOVE  'WDR401 ' TO SSA1                                              
146600     MOVE '  ' TO GODK-STATUSKODER                                        
146700     CALL CBLTDLI USING ISRT 4495-PCB DLI-IO-WDGX4495 SSA1                
146800     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
146900     PERFORM IMS-STATUSKONTROLL                                           
147000     .                                                                    
147100     SKIP2                                                                
147200 IMS-GHU-WDGX4496  SECTION.                                               
147300     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
147400                      DELIMITED BY SIZE INTO SSA1                         
147500     MOVE  'WDGX4496 ' TO SSA2                                            
147600     MOVE '  GE' TO GODK-STATUSKODER                                      
147700     CALL CBLTDLI USING GHU 4495-PCB DLI-IO-WDGX4496 SSA1 SSA2            
147800     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
147900     PERFORM IMS-STATUSKONTROLL                                           
148000     .                                                                    
148100     SKIP2                                                                
148200 IMS-ISRT-WDGX4496  SECTION.                                              
148300     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
148400                      DELIMITED BY SIZE INTO SSA1                         
148500     MOVE  'WDGX4496 ' TO SSA2                                            
148600     MOVE '  ' TO GODK-STATUSKODER                                        
148700     CALL CBLTDLI USING ISRT 4495-PCB DLI-IO-WDGX4496 SSA1 SSA2           
148800     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
148900     PERFORM IMS-STATUSKONTROLL                                           
149000     .                                                                    
149100     SKIP2                                                                
149200 IMS-REPL-WDGX4496  SECTION.                                              
149300     MOVE '  ' TO GODK-STATUSKODER                                        
149400     CALL CBLTDLI USING REPL 4495-PCB DLI-IO-WDGX4496                     
149500     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
149600     PERFORM IMS-STATUSKONTROLL                                           
149700     .                                                                    
149800     SKIP2                                                                
149900 IMS-ISRT-WDGX4498    SECTION.                                            
150000     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
150100                      DELIMITED BY SIZE INTO SSA1                         
150200     MOVE  'WDGX4498 ' TO SSA2                                            
150300     MOVE '  II' TO GODK-STATUSKODER                                      
150400     CALL CBLTDLI USING ISRT 4495-PCB DLI-IO-WDGX4498 SSA1 SSA2           
150500     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
150600     PERFORM IMS-STATUSKONTROLL                                           
150700     .                                                                    
150800     SKIP2                                                                
150900 IMS-ISRT-WDGX4497    SECTION.                                            
151000     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
151100                      DELIMITED BY SIZE INTO SSA1                         
151200     MOVE  'WDGX4497 ' TO SSA2                                            
151300     MOVE '  II' TO GODK-STATUSKODER                                      
151400     CALL CBLTDLI USING ISRT 4495-PCB DLI-IO-WDGX4497 SSA1 SSA2           
151500     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
151600     PERFORM IMS-STATUSKONTROLL                                           
151700     .                                                                    
151800     SKIP2                                                                
151900 IMS-STATUSKONTROLL SECTION.                                              
152000                                                                          
152100     SET STATUS-IX TO 1                                                   
152200     SEARCH GODK-STATUS                                                   
152300       AT END                                                             
152400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
152500         DELIMITED BY SIZE INTO FELTEXT                                   
152600         CALL FELLOG                                                      
152700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
152800         CONTINUE                                                         
152900     END-SEARCH                                                           
153000     .                                                                    
